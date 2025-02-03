#!/bin/bash
#--------------------------------------------------------------------------------------------------------
# Semi-automatic script, to process the regular-grid output of a NWP-TS experiment; the final goal is to 
# to produce a set of diagnostic maps.
# This is the third step, and must be run on SIMC servers; script 1 is run on ATOS, script 2 on SIMC
#
# Produce le mappe
#
#                                                                        Version 1.0.0, Enrico 01/02/2025
#--------------------------------------------------------------------------------------------------------

set -x
g2g=~/enr/util/grads/sh/g2g.sh

root_dir=/home/eminguzzi@ARPA.EMR.NET/enr/icon/tmp/nwpts
gs1=${root_dir}/sh/plot_map_nwpTS_ave.gs
gs2=${root_dir}/sh/plot_map_nwpTS_rmd.gs
gs3=${root_dir}/sh/plot_map_series.gs

exp1="S23icon265"
exp2="S23icon202410"
exp3="S24icon202410"

cd ${root_dir}/run2

for mm in 07 ; do
# for mm in 07 12 ; do
#for var in CLCT PMSL RELHUM_2M TMAX_2M TMIN_2M TOT_PREC FF_10M ; do
for var in TOT_PREC ; do
  if [ $var = TOT_PREC ] ; then
    stat=sum
  else
    stat=ave
  fi
  for exp in $exp1 $exp2 $exp3 ; do
#    file_root=${var}_${mm}_${stat}_${exp}
#    $g2g ${root_dir}/grb/${file_root}.grb ana
#    grads -clb 'run '$gs1' '${file_root}.ctl' '$file_root

    if [ $var = TOT_PREC ] ; then
      file_root=${var}24h_${mm}_${stat}_${exp}
      $g2g ${root_dir}/grb/${file_root}.grb for
      grads -clb 'run '$gs3' '${file_root}'.ctl TOT_PREC24h_'${exp}' times'
    fi
  done
    
#   for dexp in "${exp3}-${exp2}" "${exp3}-${exp1}" "${exp2}-${exp1}" ; do
#     file_root=${var}_${mm}_rmd_${dexp}
#     $g2g ${root_dir}/grb/${file_root}.grb ana
#     grads -clb 'run '$gs2' '${file_root}.ctl' '$file_root
#   done
  
done
done
