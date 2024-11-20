#!/bin/sh
#SBATCH --qos np
#SBATCH --account=spitrasp
#SBATCH -J %TASK%
#SBATCH --ntasks=576
#SBATCH --output=%ECF_JOBOUT%
#SBATCH --error=%ECF_JOBOUT%
#SBATCH --mail-type=FAIL
#SBATCH --cpus-per-task=1
#SBATCH --hint=nomultithread
#SBATCH --time=12:00:00
#SBATCH --mem-bind=local

#export SLURM_EXPORT_ENV=ALL
#export SLURM_MPI_TYPE=pmix

source /etc/profile

set -xa
module load ecmwf-toolbox/2022.05.0.0
module load eclib

SUITE=%SUITE%
FAMILY=%FAMILY%
TASK=%TASK%
YEAR=%YYYY%
MONTH=%MM%
DAY=%DD%
YMD=%YMD%
ECF_PASS=%ECF_PASS%
ECF_HOST=%ECF_HOST%
ECF_NAME=%ECF_NAME%
ECF_PORT=%ECF_PORT%
ECF_JOBOUT=%ECF_JOBOUT%

#
SCHOST=%SCHOST%
WSHOST=%WSHOST%
MAINDIR=%MAINDIR%
RUNDIR=%RUNDIR%
PERM=%PERM%
DOPOCORSA=%DOPOCORSA%
MYMARSDIR=%MYMARSDIR%
EXECDIR=%EXECDIR%
SORGENTE=%SORGENTE%
FORTRAN=%FORTRAN%
NMEMBER=%NMEMBER%
FCLENGTH=%FCLENGTH%
VERSION=%VERSION%
starthour=%starthour%
# hincbd_ec=%hincbd_ec%
# hincbd_7p0=%hincbd_7p0%
# hincbd_2p8=%hincbd_2p8%
TCHOME=%TCHOME%
RESCUE=%RESCUE%
EXEC=%EXEC%
USOHOME=%USOHOME%
WSCRATCH=%WSCRATCH%
#==========================================================================
#  Definitions
#==========================================================================

if [[ $LOADL_STEP_ID != NOT_SET ]] ; then
   ECF_RID=$(echo $LOADL_STEP_ID | cut -f2 -d.)
   JOB_ID=$LOADL_STEP_ID
fi

