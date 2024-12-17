#!/usr/bin/ksh
#SBATCH --qos=nf
#SBATCH --account=spitrasp
#SBATCH --job-name=%TASK%
#SBATCH --output=%ECF_JOBOUT%
#SBATCH --error=%ECF_JOBOUT%
#SBATCH --time=04:00:00
#SBATCH --mem=16G
#SBATCH --mail-type=FAIL
source /etc/profile

set -xa

# module load ecmwf-toolbox/2022.05.0.0
module load ecmwf-toolbox/2024.04.0.0
module load eclib
module load apptainer

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
WSHOST=%WSHOST%
MAINDIR=%MAINDIR%
RUNDIR=%RUNDIR%
DOPOCORSA=%DOPOCORSA%
MYMARSDIR=%MYMARSDIR%
EXECDIR=%EXECDIR%
SORGENTE=%SORGENTE%
FORTRAN=%FORTRAN%
NMEMBER=%NMEMBER%
FCLENGTH=%FCLENGTH%
VERSION=%VERSION%
starthour=%starthour%
hincbd_ec=%hincbd_ec%
hincbd_7p0=%hincbd_7p0%
hincbd_2p8=%hincbd_2p8%
TCHOME=%TCHOME%
RESCUE=%RESCUE%
USOHOME=%USOHOME%
WSCRATCH=%WSCRATCH%

#==========================================================================
#  Definitions
#==========================================================================

if [[ $LOADL_STEP_ID != NOT_SET ]] ; then
   ECF_RID=$(echo $LOADL_STEP_ID | cut -f2 -d.)
   JOB_ID=$LOADL_STEP_ID
fi

set -ex

