#!/usr/bin/bash
# Used by tasks that access MARS (*retrieve*) or ECFS

#SBATCH --qos=nf
#SBATCH --account=spitrasp
#SBATCH --job-name=%TASK%
#SBATCH --output=%ECF_JOBOUT%
#SBATCH --error=%ECF_JOBOUT%
#SBATCH --mail-type=FAIL
#SBATCH --time=23:59:00
#SBATCH --mem=16G

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

module list

