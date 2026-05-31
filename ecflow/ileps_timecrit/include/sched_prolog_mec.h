#!/bin/bash
#SBATCH --output %ECF_JOBOUT%
#SBATCH --error %ECF_JOBOUT%
%include <auto/sched_accounting.h>
