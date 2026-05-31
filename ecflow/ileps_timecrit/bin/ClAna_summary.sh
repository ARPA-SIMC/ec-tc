#!/bin/bash
#set -x

# Script to parses and process cluster analysis results for Icon_LEPS

# log_arc_dir=/ec/ws1/tc/zcl2/tcwork/arc/log/
arc_dir=$TCWORK/arc/input/IFS
work_dir=/ec/ws1/tc/zcl2/tcwork/tmp/cla

date1=$1
ndays=$2
hhr=$3

cd $work_dir
cnt=0
while [ $cnt -lt $ndays ] ; do
  datac=$(date -d "$date1 + ${cnt}day" +%Y%m%d)
  out_file=cla_${datac}${hhr}00.log
  rm -f $out_file
  echo "mem,refd,reft,lag,elem" > $out_file
  for mem in $(seq 1 20) ; do
    echo "Process day "$datac" member "$mem
    cla_arc=${arc_dir}/${datac}${hhr}00/clust_ana_result_${datac}${hhr}00.tar      
    tar -xf $cla_arc
    in_file=ec/ws1/tc/zcl2/tcwork/work/ileps_tc_2.5km/clust_ana/data/clust_ana_result_${datac}${hhr}00.${mem}.sh
    refd=$(grep refdate $in_file | cut -d = -f 2)
    reft=$(grep reftime $in_file | cut -d = -f 2)
    lag=$(grep lag $in_file | cut -d = -f 2)
    elem=$(grep elem $in_file | cut -d = -f 2)
    echo $mem,$refd,$reft,$lag,$elem >> $out_file
  done
  cnt=$(($cnt+1))
done

# log_arc=${log_arc_dir}/ileps.${datc}1800.tar.gz

