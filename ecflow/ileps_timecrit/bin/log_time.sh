#!/bin/bash
#set -x

log_arc_dir=/ec/ws1/tc/zcl2/tcwork/arc/log/
# arc_dir=$TCWORK/arc/input/IFS
work_dir=/ec/ws1/tc/zcl2/tcwork/tmp/log_time

date1=$1
ndays=$2
hhr=$3

out_file=time.log

cd $work_dir
rm -f $out_file
echo "reft,start,end,delta" > $out_file

cnt=0
while [ $cnt -lt $ndays ] ; do
  datac=$(date -d "$date1 + ${cnt}day" +%Y%m%d)
  echo "Process day "$datac

  tar_file=${log_arc_dir}/ileps.${datac}${hhr}00.tar.gz
  rm -f ./iconrun/eps_member_20/icon.1 ./iconsoil/remap_iconsoil.1
  tar -xzf $tar_file ./iconrun/eps_member_20/icon.1 ./iconsoil/remap_iconsoil.1

  start=$(ls -l --time-style=+%Y%m%d%H%M iconrun/eps_member_20/icon.1 |awk '{print $6}')
  end=$(ls -l --time-style=+%Y%m%d%H%M iconsoil/remap_iconsoil.1 |awk '{print $6}')
  t2=$(ls -l --time-style=+%s iconrun/eps_member_20/icon.1 |awk '{print $6}')
  t1=$(ls -l --time-style=+%s iconsoil/remap_iconsoil.1 |awk '{print $6}')
  delta=$(( ($t2-$t1)/60 ))
  echo ${datac}${hhr},$start,$end,$delta >> $out_file

  cnt=$(($cnt+1))
done

# log_arc=${log_arc_dir}/ileps.${datc}1800.tar.gz

