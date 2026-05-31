#!/bin/bash
#SBATCH --output %ECF_JOBOUT%
#SBATCH --error %ECF_JOBOUT%
#SBATCH --threads-per-core=1
#SBATCH --export=ALL,STHOST=ws1
%include <auto/sched_accounting.h>
