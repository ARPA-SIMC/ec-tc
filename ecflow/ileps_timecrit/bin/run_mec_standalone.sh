#!/bin/bash
#SBATCH --output run_par.log
#SBATCH --error run_par.log
#SBATCH --account=cosmo
#SBATCH --qos=np
#SBATCH --nodes=8
#SBATCH --ntasks-per-node=8
#SBATCH --hint=nomultithread
#SBATCH --export=ALL,STHOST

. /home/zcl2/ec-tc/ecflow/ileps_timecrit/include/mod_mec.h

ECTC_BIN=/ec/ws1/tc/zcl2/tcwork/bin
set -x

cd /ec/ws1/tc/zcl2/tcwork/work/ileps_ver/mec.0/run
srun ${ECTC_BIN}/mec
