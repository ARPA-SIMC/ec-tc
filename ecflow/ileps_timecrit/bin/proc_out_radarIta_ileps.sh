#!/bin/bash
#SBATCH --output=./proc_out_radarIta_ileps.log
#SBATCH --threads-per-core=1
#SBATCH --export=ALL,STHOST=ws1
#SBATCH --account=cosmo
#SBATCH --qos=np
#SBATCH --time=23:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=128
#SBATCH --mem-bind=local

#-------------------------------------------------------------------------------------------
# Extract a sub-domain of Icon-LEPS precipitation; produces precipitation percentiles
# Available input data:
# - from 20250614 to 20250813 (61 days)
# - from 20251116 to 20251230 (45 days)
#
# Templates for fieldextra use at SIMC: /ind2/meteo/a07smr03/lami/prod/icon_2I/fcens/fxtr
# Further semplifications / generalisations
# - single file for all timeranges in input to fieldextra
# - simplify fieldextra input (generatingProcessIdentifier=65, rain variables)
#-------------------------------------------------------------------------------------------

# Path, environment
module load apptainer
module load ecmwf-toolbox

fldx_bin=/ec/res4/hpcperm/chcosmo/projects/fieldextra/v16_0_0/bin/fieldextra_gnu_opt_omp
BIN_DIR=/ec/ws1/tc/zcl2/tcwork/bin
SIMC_TOOLS="singularity exec -B $TCWORK $BIN_DIR/simctools_simc_tools_r8.sif"

in_root=/ec/ws1/tc/zcl2/tcwork/arc/output/surf
proc_root=/ec/ws1/tc/zcl2/tcwork/tmp/verification/ileps

# User modifications
m_ini=1
m_end=20
step=3
last=126
suba="--ilon=6.5 --ilat=36.5 --flon=18.5 --flat=47."

# grib_copy filter
list=$(seq 0 $(($step*60)) $((${last}*60)) )
gc_list=$(echo $list|sed 's/\ /\//g')

# Command line
date1=$1  # yyyymmdd
rth=$2    # 18
ndays=$3

if [ $# -ne 3 ] ; then
  echo "Usage: date1 rth ndays "
  exit
fi

#-------------------------------------------------------------------------------------------
# Processing

#set -x
mkdir -p ${proc_root}/run
cd ${proc_root}/run

rm -f lotr.filt
cat <<EOF > lotr.filt
write "tmp_[lengthOfTimeRange].grib";
EOF

cnt=0
while [ $cnt -lt $ndays ] ; do
  datac=$(date -d "$date1 + ${cnt}day" +%Y%m%d)
  echo "Processing "$datac
  in_dir=${in_root}/${datac}${rth}00
  mkdir -p ${proc_root}/${datac}${rth}00/fldx ${proc_root}/out
  for mem in $(seq $m_ini $m_end) ; do
    m2=$(printf "%02d" $mem)
    echo "  processing member "$m2
    out_dir=${proc_root}/${datac}${rth}00/mem.${m2}
    mkdir -p $out_dir
    filein=${in_dir}/${datac}${rth}00.${m2}.TOT_PREC.grib 

#   Filter input timeranges
    rm -f tmp1.grib tmp2.grib tmp_*.grib
    if [ $step -gt 1 ] ; then
      grib_copy -w lengthOfTimeRange=${gc_list} $filein tmp1.grib
    else
      ln -s $filein tmp1.grib
    fi

# Cut subarea
    rm -f libsim.log
    $SIMC_TOOLS vg6d_transform --trans-mode=s $suba \
      --trans-type=zoom --sub-type=coord tmp1.grib tmp2.grib >> libsim.log 2>&1

# Split and rename output, according to Cosmo conventions
    grib_filter lotr.filt tmp2.grib
    for lotr in $(seq 0 $step $last) ; do
      h3=$(printf "%03d" $lotr)
      fileout=${datac}${rth}00+${h3}.grib
      mv tmp_${lotr}.grib ${out_dir}/${fileout}
    done      # Timeranges

  done        # Ens members

# Run fieldextra
    rm -f fldx.nml fldx.log
    cat << EOFA > fldx.nml
&RunSpecification
 strict_nl_parsing  = .true.
 n_ompthread_total  = 1
 verbosity          = "high"
 diagnostic_length  = 0
 strict_usage = .false.
/

&GlobalResource
 dictionary           = "/ec/res4/hpcperm/chcosmo/projects/fieldextra/v16_0_0/resources/dictionary_icon.txt",
 grib_definition_path = "/ec/res4/hpcperm/chcosmo/projects/fieldextra/v16_0_0/resources/eccodes_definitions_cosmo", "/ec/res4/hpcperm/chcosmo/projects/fieldextra/v16_0_0/resources/eccodes_definitions_vendor"
 grib2_sample         = "/ec/res4/hpcperm/chcosmo/projects/fieldextra/v16_0_0/resources/eccodes_samples/COSMO_GRIB2_default.tmpl"
/

&GlobalSettings
 default_model_name   = "icon"
 originating_center   = "roma"
/

&ModelSpecification
 model_name         = "icon"
/

&Process
  in_file="${proc_root}/${datac}${rth}00/mem.<mm>/${datac}${rth}00+<HHH>.grib"
  epsstart=${m_ini}, epsstop=${m_end}, epsincr=1
  tstart=0, tstop=${last}, tincr=${step}, tlag=-3, out_tstart = 3, out_tincr=3
  out_file="${proc_root}/${datac}${rth}00/fldx/tpp03h_${datac}${rth}00+<HHH>.grib"
  out_type="GRIB2", out_mode_weighted_member=.FALSE., out_type_packing= 'simple,16'
  out_type_alternate_code=.true.
/
&Process in_field = "TOT_PREC", toper="delta,3,hour" /
&Process out_field="TOT_PREC", product_category='eps_quantile', quantile=50,
 set_trange_type='difference_(end-start)'
/
&Process out_field="TOT_PREC", product_category='eps_quantile', quantile=75,
 set_trange_type='difference_(end-start)'
/
&Process out_field="TOT_PREC", product_category='eps_quantile', quantile=90,
 set_trange_type='difference_(end-start)'
/
EOFA

  $fldx_bin fldx.nml > fldx.log

# Write a file for each percentile, with all timeranges
  rm -f pct.filt
  cat << EOFB > pct.filt
write "${proc_root}/out/ileps_tpp03h_${datac}${rth}00_pct[percentileValue].grib";
EOFB
  grib_filter pct.filt ${proc_root}/${datac}${rth}00/fldx/tpp03h_${datac}${rth}00+*.grib

# Aggregate precipitation from the beginning of the run
  rm -f ft.filt
  cat <<EOFC > ft.filt
write "st_[forecastTime].grib";
EOFC
  for filein in $(ls ${proc_root}/out/ileps_tpp03h_${datac}${rth}00_pct??.grib) ; do
    filein_root=$(echo ${filein%.*})
    fileout=${filein_root}_cum.grib
    rm -f st_*.grib $fileout cum.grib tmp?.grib

    grib_filter ft.filt $filein
    ln -s  st_0.grib tmp2.grib 
    grib_set -s productDefinitionTemplateNumber=11 tmp2.grib tmp3.grib
    mv tmp2.grib cum.grib
    mv tmp3.grib $fileout

    lastms=$(($last-$step))
    for ft_in in $(seq $step $step $lastms) ; do
      rm -f tmp?.grib
      ft_out=$(($ft_in+$step))
      $SIMC_TOOLS /usr/libexec/ma_utils/math_grib.exe 1. st_${ft_in}.grib 1. cum.grib tmp2.grib sum -check=nil
      grib_set -s lengthOfTimeRange=$ft_out,forecastTime=0,productDefinitionTemplateNumber=11 tmp2.grib tmp3.grib
      mv tmp2.grib cum.grib
      cat tmp3.grib >> $fileout
    done      # Forecast times

  done        # Percentiles
  cnt=$(($cnt+1))
done          # Reftimes

exit


