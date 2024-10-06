#!/bin/bash
#SBATCH --qos=nf
#SBATCH --account=spitrasp
#SBATCH --job-name=get_raw_output_multi
#SBATCH --output=get_raw_output_multi.log
#SBATCH --error=get_raw_output_multi.log
#SBATCH --time=23:59:00
#SBATCH --mail-type=FAIL

#-------------------------------------------------------------------------------
# Extract one or more parameteres from the ouputs of multiple Icon Test Suite
# expoermints. Derived from get_raw_output.sh.
# The list of parameters must be manually edited in the "grib_filter" section. 
#
#-------------------------------------------------------------------------------
set -x
set -e

ndays=31

# Constants
scratch_root=/ec/res4/scratch/itic/nwptsH   # Root of NWP-TS working dir
ecf_root=/itic/nwp_suite_HindcastMode       # Root dir of NWP-TS archive on ECFS

run_dir=/ec/res4/scratch/itic/tmp/ver_NWPTS # Working dir (temporary)
out_dir=/ec/res4/hpcperm/itic/ver_NWPTS     # Root dir of permanente outputs

# Environment
module load ecmwf-toolbox

export DWDPERM=/perm/dwg/
export ECCODES_DEFINITION_PATH=${DWDPERM}/grib_api_def/ec-2.23.0/definitions.edzw:${DWDPERM}/grib_api_def/ec-2.23.0/definitions
export ECCODES_SAMPLES_PATH=${DWDPERM}/grib_api_def/ec-2.23.0/samples-19.1

cd $run_dir

# Main loop (experiments)
exp_list=3
for exp in $exp_list ; do

  if [ $exp = 1 ] ; then
    project="icon261"
    data_ini=20171201
    src="ecf"
  elif [ $exp = 2 ] ; then
    project="icon261"
    data_ini=20171201
    src="ecf"
  elif [ $exp = 3 ] ; then
    project="icon265"
    data_ini=20170701
    src="ecf"
  elif [ $exp = 4 ] ; then
    project="icon265"
    data_ini=20171201
    src="ecf"
  elif [ $exp = 5 ] ; then
    project="S23icon265"
    data_ini=20210701
    src="wrk"
  elif [ $exp = 6 ] ; then
    project="S23icon265"
    data_ini=20211201
    src="wrk"
  elif [ $exp = 7 ] ; then
    project="S23icon261"
    data_ini=20210701
    src="wrk"
  elif [ $exp = 8 ] ; then
    project="S23icon261"
    data_ini=20211201
    src="wrk"
  fi

  echo "## Processing: $project $data_ini"
  tmp_dir=$SCRATCH/tmp/${project}_${data_ini}
  mkdir -p $tmp_dir
  cd $tmp_dir
  rm -f *

# Build grib filter (user modification)
  var_list="T_2M TD_2M TOT_PREC CLCT VMAX_10M PS"
  rm -f its.filt
  for var in $var_list ; do
    cat << EOF > its.filt
if (shortName is "${var}") {
  append "[shortName].grib";
}
EOF
  done

  exit
# Secondary loop (required days)
  cnt=1
  while [ $cnt -le $ndays ] ; do
    incr=$(($cnt-1))
    datac=$(date -d "${data_ini} + ${incr}day" +%Y%m%d)
    echo "# Processing date "$datac" "$(date)
  
#   Find the names of input directories  
    YM=${datac:0:6}
    dd=${datac:6:2}
    d=$(printf %01g $dd)
    DD=$(( ($d-1)/5*5+1 ))
    DD2=$(printf %02g $DD)
    wrk_dir=${scratch_root}/${project}/bc_ic_00/${YM}${DD2}00_test_2p8
    ecf_dir=ec:${ecf_root}/${project}/${YM}
  
#   Find the names of input files
    dm1=$(($d-1))
    tr_day1=$(printf %02g $dm1)
    tr_day2=$(printf %02g $d)
  
    if [ $src = "wrk" ] ; then
#     If processing the first day of the month, process also the initial conditions
      if [ $dm1 -eq 0 ] ; then
        file_list="ICON-LAM_Testsuite_00000000.grb"
      else
        file_list=""
      fi
      for tr_hh in $(seq -f "%02g" 23) ; do 
        file_list=${file_list}" ICON-LAM_Testsuite_${tr_day1}${tr_hh}0000.grb"
      done
      file_list=${file_list}" ICON-LAM_Testsuite_${tr_day2}000000.grb"
 
    elif [ $src = "ecf" ] ; then
      file_list=ITS_${project}_${datac}_basic.grb

    fi

#   Process input files
    for file in $file_list ; do
      echo "Processing file "$file
      if [ $src = "wrk" ] ; then
        grib_filter its.filt ${wrk_dir}/$file
      elif [ $src = "ecf" ] ; then
        echo "## Start ecp "$(date)
        ecp ${ecf_dir}/$file .
	echo "## Start filter "$(date)
	grib_filter its.filt $file
	echo "## End filter "$(date)
      fi
    done
  
    cnt=$(($cnt+1))
  done
  echo "Outputs are in $tmp_dir"

done  
