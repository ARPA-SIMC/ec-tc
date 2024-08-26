#!/bin/bash
#SBATCH --qos=nf
#SBATCH --account=spitrasp
#SBATCH --job-name=archive_offline
#SBATCH --output=archive_offline.log
#SBATCH --error=archive_offline.log
#SBATCH --time=12:00:00
#SBATCH --mail-type=FAIL

### #SBATCH --account=spitconv
#-------------------------------------------------------------------------------
# Process and copy to ECFS the outputs of Icon Test Suite
# It is assumed that ICON runs last 31 days, starting the first day of the month
# This script can process the whole run or a subset of days
# Outputs are split in "basic" and "extra"; one file is produced for each 
# forecast day (from 01Z to 24Z)
#
#-------------------------------------------------------------------------------
#set -x

# Command line
if [ $# -ne 4 ] ; then
  echo "Usage: sbatch archive_offline.sh project data_ini ndays label"
  exit
fi
project=$1                  # ITS project ($VERSION in nwptsICON.def)
data_ini=$2                 # First day to process
ndays=$3                    # Number of days to process
label=$4                    # Label for output files on ECFS (may be equal to $project)

# Constants
scratch_root=/ec/res4/scratch/itic/nwptsH   # Root of ITS working dir
#ecf_root=ec:/itic/nwp_suite_HindcastMode   # Root dir of ITS archive on ECFS
ecf_root=ec:/mck/NWP-TS                     # Root dir of ITS archive on ECFS
centre=80
igen=116

# Environment
module load ecmwf-toolbox

export DWDPERM=/perm/dwg/
export ECCODES_DEFINITION_PATH=${DWDPERM}/grib_api_def/ec-2.23.0/definitions.edzw:${DWDPERM}/grib_api_def/ec-2.23.0/definitions
export ECCODES_SAMPLES_PATH=${DWDPERM}/grib_api_def/ec-2.23.0/samples-19.1

cd $SCRATCHDIR

# Build grib filter
# The original script included the instruction: "set setLocalDefinition = 1", that does not seem
# to be needed anymore (in fact, it has no effect..)

rm -f its.filt
cat << EOF > its.filt
set centre = ${centre};
set generatingProcessIdentifier = ${igen};
if (shortName is "QC" || shortName is "QI" || shortName is "QR" || shortName is "QS" ||
    typeOfLevel is "generalVertical" || typeOfLevel is "depthBelowLandLayer" || typeOfLevel is "depthBelowLand") {
  append "extra.grb";
}
else
{
  append "basic.grb";
}
EOF

# Main loop (required days)
cnt=1
while [ $cnt -le $ndays ] ; do
  incr=$(($cnt-1))
  datac=$(date -d "${data_ini} + ${incr}day" +%Y%m%d)
  echo "Processing date "$datac

# Find the names of input and output directories  
  YM=${datac:0:6}
  dd=${datac:6:2}
  d=$(printf %01g $dd)
  DD=$(( ($d-1)/5*5+1 ))
  DD2=$(printf %02g $DD)
  input_dir=${scratch_root}/${project}/bc_ic_00/${YM}${DD2}00_test_2p8
  ecf_dir=${ecf_root}/${label}/${YM}
  emkdir -p ${ecf_dir}

# If processing the first required day, copy to ECFS the constant fields
  if [ $cnt -eq 1 ] ; then
    cfile_in="ICON-LAM_Testsuite_00000000c.grb"
    cfile_out="ITS_"${label}"_constant.grb"
    echo "Saving constant file "$cfile_in
    ls -l ${input_dir}/${cfile_in}
    ecp -o ${input_dir}/${cfile_in} ${ecf_dir}/${cfile_out}
    echmod 644 ${ecf_dir}/${cfile_out}
  fi

# Find the names of input files
  dm1=$(($d-1))
  tr_day1=$(printf %02g $dm1)
  tr_day2=$(printf %02g $d)

# If processing the first day of the month, process also the initial conditions
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
  rm -f basic.grb extra.grb
  for file in $file_list ; do
    echo "Processing file "$file
    ls -l ${input_dir}/$file
    grib_filter its.filt ${input_dir}/$file
  done

# Copy to ECFS
  echo "Save to "${ecf_dir}/ITS_${label}_${datac}_basic.grb
  ecp -o basic.grb ${ecf_dir}/ITS_${label}_${datac}_basic.grb 
  echmod 644 ${ecf_dir}/ITS_${label}_${datac}_basic.grb

  echo "Save to "${ecf_dir}/ITS_${label}_${datac}_extra.grb
  ecp -o extra.grb ${ecf_dir}/ITS_${label}_${datac}_extra.grb 
  echmod 644 ${ecf_dir}/ITS_${label}_${datac}_extra.grb

cnt=$(($cnt+1))
done




