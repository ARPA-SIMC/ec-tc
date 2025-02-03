#!/bin/bash
#SBATCH --qos=nf
#SBATCH --account=spitrasp
#SBATCH --job-name=scan_unstr
#SBATCH --output=scan_unstr.log
#SBATCH --error=scan_unstr.log
#SBATCH --time=23:59:00
#SBATCH --mem=16G

#--------------------------------------------------------------------------------------------------------
# Semi-automatic script, to scan the ouptuts of a NWP-TS experiment.
#  Written to test the "restart bug" of icon.2024-10
#--------------------------------------------------------------------------------------------------------

# User modifications
proj_lst="S24icon202410"
#mm_lst="07 12"
mm_lst="07"

# Path
nwpts_root=/ec/res4/scratch/mck/nwptsH/
proc_dir=/ec/res4/scratch/mck/nwptsH/bug_restart

# Environment
# module load apptainer/1.3.0 cdo/2.4.0 nco/5.1.9 prgenv/intel intel/2021.4.0 eclib/1.1.0 netcdf4/4.9.2
module load ecmwf-toolbox/2024.04.0.0
export ZCLPERM=/perm/zcl/
export ECCODES_DEFINITION_PATH=${ZCLPERM}/grib_api_def/ec-2.35.0/definitions.edzw:${ZCLPERM}/grib_api_def/ec-2.35.0/definitions
export ECCODES_SAMPLES_PATH=${ZCLPERM}/grib_api_def/ec-2.35.0/samples-19.1

#set -x
set -e
module list
codes_info

#--------------------------------------------------------------------------------------------------------
cd $proc_dir

for mm in $mm_lst ; do
for proj in $proj_lst ; do
  echo "### Start processing: $proj $mm"
  rm -f tmpi?.gls *_${proj}_${mm}.gls
  input_lst=${nwpts_root}/${proj}/bc_ic_00/2021${mm}??00/${proj}_????????.grb
  grib_ls -p identifier,forecastTime,lengthOfTimeRange,stepRange,shortName,average -w productDefinitionTemplateNumber=8 $input_lst > tmp1.gls
  grep ^GRIB tmp1.gls > tmp2.gls
  var_lst=$(awk '{print $5}' tmp2.gls | sort | uniq)
  for var in $var_lst ; do
    grep $var tmp2.gls > ${var}_${proj}_${mm}.gls
  done
done
done

