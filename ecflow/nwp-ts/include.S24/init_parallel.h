#!/bin/bash
#SBATCH --qos=np
#SBATCH --account=spitrasp
#SBATCH --job-name=%TASK%
#SBATCH --output=%ECF_JOBOUT%
#SBATCH --error=%ECF_JOBOUT%
#SBATCH --mail-type=FAIL
#SBATCH --ntasks=576
#SBATCH --cpus-per-task=1
#SBATCH --hint=nomultithread
#SBATCH --time=12:00:00
#SBATCH --mem-bind=local

#export SLURM_EXPORT_ENV=ALL
#export SLURM_MPI_TYPE=pmix
#export LANG=en_US.UTF-8

source /etc/profile
if [[ $LOADL_STEP_ID != NOT_SET ]] ; then
   ECF_RID=$(echo $LOADL_STEP_ID | cut -f2 -d.)
   JOB_ID=$LOADL_STEP_ID
fi

# Modules required to run Icon on Atos
# - module hpcx-openmpi could be replaced with intel-mpi

module load eclib
module load prgenv/intel
module load intel/2021.4.0
module load hpcx-openmpi/2.9.0
module load hdf5/1.14.3
module load netcdf4/4.9.2
module load intel-mkl/19.0.5
module load aec/1.1.2

module list
