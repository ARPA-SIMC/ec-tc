#!/bin/bash
#SBATCH --qos=nf
#SBATCH --account=spitrasp
#SBATCH --job-name=archive_offline
#SBATCH --output=get_raw_output.log
#SBATCH --error=get_raw_output.log
#SBATCH --time=12:00:00
#SBATCH --mail-type=FAIL

#-------------------------------------------------------------------------------
# Extract one or more parameteres from the ouputs of Icon Test Suite
# the list of parameters must be manually edited in the "grib_filter" section. 
#
#-------------------------------------------------------------------------------
set -x

# Command line
if [ $# -ne 4 ] ; then
  echo "Usage: [sbatch] get_raw_output.sh project data_ini ndays src"
  echo "  src: wrk or ecf"
  exit
fi
project=$1                  # ITS project ($VERSION in nwptsICON.def)
data_ini=$2                 # First day to process
ndays=$3                    # Number of days to process
src=$4                      # Source of data: working directories or archive on ecf

if [ $src != "wrk" ] ; then
  echo "input source $src not yet implemented"
  exit
fi

# Constants
scratch_root=/ec/res4/scratch/itic/nwptsH   # Root of ITS working dir
ecf_root=/itic/nwp_suite_HindcastMode       # Root dir of ITS archive on ECFS

# Environment
module load ecmwf-toolbox

export DWDPERM=/perm/dwg/
export ECCODES_DEFINITION_PATH=${DWDPERM}/grib_api_def/ec-2.23.0/definitions.edzw:${DWDPERM}/grib_api_def/ec-2.23.0/definitions
export ECCODES_SAMPLES_PATH=${DWDPERM}/grib_api_def/ec-2.23.0/samples-19.1

cd $SCRATCH/tmp

# Build grib filter (user modification)

var_list="T_2M VMAX_10M TOT_PREC"
cat << EOF > its.filt
if (shortName is "T_2M" || shortName is "VMAX_10M" || shortName is "TOT_PREC") {
  append "[shortName].grib";
}
EOF

# Main loop (required days)
cnt=1
while [ $cnt -le $ndays ] ; do
  incr=$(($cnt-1))
  datac=$(date -d "${data_ini} + ${incr}day" +%Y%m%d)
  echo "Processing date "$datac

# Find the names of input directories  
  YM=${datac:0:6}
  dd=${datac:6:2}
  d=$(printf %01g $dd)
  DD=$(( ($d-1)/5*5+1 ))
  DD2=$(printf %02g $DD)
  wrk_dir=${scratch_root}/${project}/bc_ic_00/${YM}${DD2}00_test_2p8
  ecf_dir=${ecf_root}/${label}/${YM}

# Find the names of input files
  dm1=$(($d-1))
  tr_day1=$(printf %02g $dm1)
  tr_day2=$(printf %02g $d)

# I processing the first day of the month, process also the initial conditions
  if [ $dm1 -eq 0 ] ; then
    file_list="ICON-LAM_Testsuite_00000000.grb"
  else
    file_list=""
  fi
  for tr_hh in $(seq -f "%02g" 23) ; do 
    file_list=${file_list}" ICON-LAM_Testsuite_${tr_day1}${tr_hh}0000.grb"
  done
  file_list=${file_list}" ICON-LAM_Testsuite_${tr_day2}000000.grb"

# Process input files
  for var in $var_list ; do
    rm -f $var.grib
  done
  for file in $file_list ; do
    echo "Processing file "$file
    grib_filter its.filt ${wrk_dir}/$file
  done

cnt=$(($cnt+1))
done
echo "Outputs are in $SCRATCH/tmp"
