#!/usr/bin/bash
# Used by tasks that do not access MARS or ECFS.

#SBATCH --qos=nf
#SBATCH --account=spitrasp
#SBATCH --job-name=%TASK%
#SBATCH --output=%ECF_JOBOUT%
#SBATCH --error=%ECF_JOBOUT%
#SBATCH --mail-type=FAIL
#SBATCH --time=01:00:00
### #SBATCH --mem=16G

source /etc/profile
if [[ $LOADL_STEP_ID != NOT_SET ]] ; then
   ECF_RID=$(echo $LOADL_STEP_ID | cut -f2 -d.)
   JOB_ID=$LOADL_STEP_ID
fi

# Modules required for scalar tasks
module load eclib
module load apptainer
module load cdo
module load nco

# Moduls required for icontools
module load prgenv/intel
module load intel/2021.4.0
module load netcdf4/4.9.2

module list

