#!/bin/bash
#SBATCH --output=./proc_out_radarIta_ifsens.log
#SBATCH --threads-per-core=1
#SBATCH --export=ALL,STHOST=ws1
#SBATCH --account=cosmo
#SBATCH --qos=np
#SBATCH --time=23:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=128
#SBATCH --mem-bind=local

#-------------------------------------------------------------------------------------------
# Process IFS-ENS precipitation forecasts, extracted from MARS by mars_get_ifsens.sh
# to produce precipitation percentiles.
# Evaluate 3-hourly percentiles, then aggregate to have a single "virtual model"
#
# Templates for fieldextra use at SIMC: /ind2/meteo/a07smr03/lami/prod/icon_2I/fcens/fxtr
# Further semplifications / generalisations
# - single file for all timeranges in input to fieldextra
#-------------------------------------------------------------------------------------------

# Path, environment
module load apptainer
module load ecmwf-toolbox

fldx_bin=/ec/res4/hpcperm/chcosmo/projects/fieldextra/v16_0_0/bin/fieldextra_gnu_opt_omp
BIN_DIR=/ec/ws1/tc/zcl2/tcwork/bin
SIMC_TOOLS="singularity exec -B $TCWORK $BIN_DIR/simctools_simc_tools_r8.sif"

in_dir=/ec/ws1/tc/zcl2/tcwork/tmp/verification/ifsens/mars
proc_dir=/ec/ws1/tc/zcl2/tcwork/tmp/verification/ifsens/run
out_dir=/ec/ws1/tc/zcl2/tcwork/tmp/verification/ifsens/out

# User modifications
m_ini=1
m_end=50
step=3
last=126

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
cd $proc_dir

rm -f split.filt
cat <<EOF > split.filt
write "ifs_[P1]_[perturbationNumber].grib";
EOF

cnt=0
while [ $cnt -lt $ndays ] ; do
  datac=$(date -d "$date1 + ${cnt}day" +%Y%m%d)
  echo "Processing "$datac

# Split and rename output
  rm -f ifs_*_*.grib
  grib_filter split.filt ${in_dir}/ifsens_${datac}${rth}.grb

  for m in $(seq $m_ini $m_end) ; do
  for ft in $(seq 0 $step $last) ; do
    ft3=$(printf "%03d" $ft)
    m2=$(printf "%02d" $m)
    if [ $ft3 != $ft -o $m2 != $m ] ; then
      mv ifs_${ft}_${m}.grib ifs_${ft3}_${m2}.grib
    fi
  done
  done

# Run fieldextra
  echo "  Run fieldextra"
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
 dictionary           = "/ec/res4/hpcperm/chcosmo/projects/fieldextra/v16_0_0/resources/dictionary_ifs.txt",
 grib_definition_path = "/ec/res4/hpcperm/chcosmo/projects/fieldextra/v16_0_0/resources/eccodes_definitions_vendor"
 grib2_sample         = "/ec/res4/hpcperm/chcosmo/projects/fieldextra/v16_0_0/resources/eccodes_samples/GRIB2.tmpl"
/

&GlobalSettings
 default_model_name   = "ifs-ens"
/

&ModelSpecification
 model_name         = "ifs-ens"
/
&Process
  in_file="${proc_dir}/ifs_<HHH>_<mm>.grib"
  epsstart=${m_ini}, epsstop=${m_end}, epsincr=1
  tstart=0, tstop=${last}, tincr=${step}, tlag=-3, out_tstart = 3, out_tincr=3
  out_mode_mismemb_limit=90
  out_file="${proc_dir}/tpp03h_${datac}${rth}00+<HHH>.grib"
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
write "${out_dir}/ifsens_tpp03h_${datac}${rth}00_pct[percentileValue].grib";
EOFB
  grib_filter pct.filt ${proc_dir}/tpp03h_${datac}${rth}00+*.grib

# Aggregate precipitation from the beginning of the run
  rm -f ft.filt
  cat <<EOFC > ft.filt
write "st_[forecastTime].grib";
EOFC
  for filein in $(ls ${out_dir}/ifsens_tpp03h_${datac}${rth}00_pct??.grib) ; do
    filein_root=$(echo ${filein%.*})
    fileout=${filein_root}_cum.grib
    rm -f st_*.grib $fileout cum.grib tmp?.grib
    grib_filter ft.filt $filein

    $SIMC_TOOLS /usr/libexec/ma_utils/grib_set_scanning.exe st_0.grib tmp1.grib 010
    $SIMC_TOOLS /usr/libexec/ma_utils/math_grib.exe 1000. tmp1.grib 0. "dummy" tmp2.grib lin -check=nil
    grib_set -s productDefinitionTemplateNumber=11 tmp2.grib tmp3.grib
    mv tmp2.grib cum.grib
    mv tmp3.grib $fileout

    lastms=$(($last-$step))
    for ft_in in $(seq $step $step $lastms) ; do
      rm -f tmp?.grib
      ft_out=$(($ft_in+$step))
      $SIMC_TOOLS /usr/libexec/ma_utils/grib_set_scanning.exe st_${ft_in}.grib tmp1.grib 010
      $SIMC_TOOLS /usr/libexec/ma_utils/math_grib.exe 1000. tmp1.grib 1. cum.grib tmp2.grib sum -check=nil
      grib_set -s lengthOfTimeRange=$ft_out,forecastTime=0,productDefinitionTemplateNumber=11 tmp2.grib tmp3.grib
      mv tmp2.grib cum.grib
      cat tmp3.grib >> $fileout
    done      # Forecast times

  done        # Percentiles
  cnt=$(($cnt+1))
done          # Reftimes

# Process section, to produce "de-cumulated" precipitaton
# &Process
#   in_file="${proc_dir}/ifs_<HHH>_<mm>.grib"
#   epsstart=${m_ini}, epsstop=${m_end}, epsincr=1
#   tstart=0, tstop=${last}, tincr=${step}, tlag=-3, out_tstart = 3, out_tincr=3
#   out_mode_mismemb_limit=90
#   out_file="${proc_dir}/tpp03h_${datac}${rth}00+<HHH>.grib"
#   out_type="GRIB2", out_mode_weighted_member=.FALSE., out_type_packing= 'simple,10'
#   out_type_alternate_code=.true.
# /
# &Process in_field = "TOT_PREC", toper="delta,3,hour" /
# &Process out_field="TOT_PREC", product_category='eps_quantile', quantile=50,
#   set_trange_type='difference_(end-start)'
# /
# &Process out_field="TOT_PREC", product_category='eps_quantile', quantile=75,
#   set_trange_type='difference_(end-start)'
# /
# &Process out_field="TOT_PREC", product_category='eps_quantile', quantile=90,
#   set_trange_type='difference_(end-start)'
# /

# &Process
#   in_file="${proc_dir}/ifs_<HHH>_<mm>.grib"
#   epsstart=${m_ini}, epsstop=${m_end}, epsincr=1
#   tstart=0, tstop=${last}, tincr=${step}, out_tstart = 3, out_tincr=3
#   out_mode_mismemb_limit=90
#   out_file="${proc_dir}/tpp03h_${datac}${rth}00+<HHH>.grib"
#   out_type="GRIB2", out_mode_weighted_member=.FALSE., out_type_packing= 'simple,10'
#   out_type_alternate_code=.true.
# /
# &Process in_field = "TOT_PREC" /
# &Process out_field="TOT_PREC", product_category='eps_quantile', quantile=50/
# &Process out_field="TOT_PREC", product_category='eps_quantile', quantile=75/
# &Process out_field="TOT_PREC", product_category='eps_quantile', quantile=90/

