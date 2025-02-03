#!/bin/bash
#SBATCH --qos=nf
#SBATCH --account=spitrasp
#SBATCH --job-name=proc_str
#SBATCH --output=proc_str.log
#SBATCH --error=proc_str.log
#SBATCH --time=23:59:00
#SBATCH --mem=16G

#--------------------------------------------------------------------------------------------------------
# Semi-automatic script, to process the regular-grid output of a NWP-TS experiment; the final goal is to 
# to produce a set of diagnostic maps.
# This is the first step, and must be run on ATOS; scripts 2 and 3 run on SIMC servers.
#
#                                                                        Version 1.0.0, Enrico 01/02/2025
#--------------------------------------------------------------------------------------------------------
# - read from filesystem hourly/daily output, re-arrange to a file per parameter

# User modifications
run_arc="Y"             # copy "structured" ouputs from $nwpts_root to $arc_root
run_split="Y"           # copy selected parameters to $proc_dir
proj_lst="S24icon202410"
var_lst="TOT_PREC CLCT PMSL T_2M RELHUM_2M V_10M U_10M"
#mm_lst="07 12"
mm_lst="07"

# Path
nwpts_root=/ec/res4/scratch/mck/nwptsH/
arc_root=/ec/res4/hpcperm/mck/nwp-ts
proc_dir=/ec/res4/scratch/mck/nwptsH/diagno

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
# 1) Copy from $nwpts_root to $arc_root

for mm in $mm_lst ; do
  for proj in $proj_lst ; do
  echo "### Save to HPCPERM: $proj $mm"
  mkdir -p ${arc_root}/${proj}/2021${mm}
  cd ${nwpts_root}/${proj}/bc_ic_00
  rsync -auv 2021${mm}??00/*_str_*.grb ${arc_root}/${proj}/2021${mm}
done
done

#--------------------------------------------------------------------------------------------------------
# 2) Re-arrange to a file per parameter, copy to temporary dir

mkdir -p $proc_dir
cd $proc_dir
rm -f fields.filt
echo "write \"tmp_[shortName].grb\";" > fields.filt

for mm in $mm_lst ; do
  echo "----------------------------------------------------------------------------------"
  echo "### Filter grib $proj $mm"
  for proj in $proj_lst ; do
    if [ $proj = S23icon265 ] ; then
      d1=1
    else
      d1=0
    fi
    rm -f ts_$proj_2021${mm}_*.grb
    for dd in $(seq -f "%02g" ${d1} 31) ; do
      echo "Processing day $dd"
      rm -f tmp_*.grb 
      in_files=$(ls ${arc_root}/${proj}/2021${mm}/${proj}_str_surf_${dd}??0000.grb)
      grib_filter fields.filt $in_files

      for var in $var_lst ; do
        cat tmp_${var}.grb >> ts_${proj}_2021${mm}_${var}.grb
      done
    done
  done
done
