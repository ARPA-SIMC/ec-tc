#!/bin/bash
#SBATCH --output %ECF_JOBOUT%
#SBATCH --error %ECF_JOBOUT%
#SBATCH --threads-per-core=1
%include <%ECTC_JOB_TYPE%.h>
%include <sched_accounting.h>
%include <head.h>
