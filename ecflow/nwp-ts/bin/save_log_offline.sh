#!/bin/bash
#
# Offline version of task save_log.ecf
#

if [ $# -ne 2 ] ; then
  echo "Usage: save_log_offline.sh project YMD"
  echo "Project: eg. icon261_1stepNest, .."
  echo "YMD: 20170701, ..."
  exit
fi

set -x

VERSION=$1
YMD=$2
label=$VERSION

# Suite path (hardcoded in nwptshICON.def and exported by init_serial.h)
RUNDIR=/ec/res4/scratch/itic/nwptsH/${VERSION}/bc_ic_00
DOPOCORSA=/ec/res4/scratch/itic/nwptsH/${VERSION}/post_00
ECF_OUT=/ec/res4/scratch/itic/nwptsH/${VERSION}/log

# Archive path in ECFS (hardcoded in the task definition in nwptshICON.def) 
BASEDIR=/itic/nwp_suite_HindcastMode/

log_root=${ECF_OUT}/nwptsICON/nwp_suite_00
cd $DOPOCORSA

# List of tasks for which the logfile must be archived
tasks=( \
  retrieve_MARS_det_ana_day1 \
  retrieve_MARS_det_ana_day2 \
  retrieve_MARS_det_ana_day3 \
  retrieve_MARS_det_ana_day4 \
  retrieve_MARS_det_ana_day5 \
  retrieve_MARS_det_fc_day1 \
  retrieve_MARS_det_fc_day2 \
  retrieve_MARS_det_fc_day3 \
  retrieve_MARS_det_fc_day4 \
  retrieve_MARS_det_fc_day5 \
  retrieve_ICON \
  run_ICON_LAM \
  remap_ICON \
  remap_IFS_init \
  remap_IFS_lbc \
  remap_SST_FRICE \
  copy_2_ecfs \
  )

subdirs=( \
  get_bc_ic \
  get_bc_ic \
  get_bc_ic \
  get_bc_ic \
  get_bc_ic \
  get_bc_ic \
  get_bc_ic \
  get_bc_ic \
  get_bc_ic \
  get_bc_ic \
  get_bc_ic \
  model \
  model/remap \
  model/remap \
  model/remap \
  model/remap \
  copy_2_ecfs \
  )

ntasks=${#tasks[*]}

# Tranche of the run being processed
YM=${YMD:0:6}
DD=${YMD:6:2}

# List of icon logfiles to archive
input_dir=${RUNDIR}/${YMD}00_test_2p8
files="${input_dir}/icon_master.namelist \
  ${input_dir}/nml.atmo.log \
  ${input_dir}/NAMELIST_ICON-LAM_Testsuite \
  ${input_dir}/NAMELIST_ICON_output_atm"

# List of task logfiles to archive 
cnt=0
while [ $cnt -lt $ntasks ] ; do
  task=${tasks[$cnt]}
  subdir=${subdirs[$cnt]}
  set +e; file=$(ls -t ${log_root}/${subdir}/${task}.*| head -n 1) ; set -e
  files=${files}" "${file}
  cnt=$(($cnt+1))
done

# Build tar archive
tar_file=${label}_${YMD}_log.tar.gz
tar -czf $tar_file $files

# Save to ecfs
# ecf_dir=ec:${BASEDIR}/${label}/${YM}
# emkdir -p $ecf_dir
# echmod 755 ec:${BASEDIR}/${label}
# echmod 755 ec:${BASEDIR}/${label}/${YM}
# 
# ecp $tar_file $ecf_dir
# echmod 644 ${ecf_dir}/${tar_file}

