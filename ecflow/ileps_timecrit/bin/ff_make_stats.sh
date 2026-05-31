#!/bin/bash
#set -x

ffilecheck=/home/dwo/bin/ffilecheck
run_len=120

vdate=20250620
var_list="T2M FF RR GUST TMAX TMIN"
in_dir=$(pwd)

module load netcdf4/4.7.4 R
export LD_LIBRARY_PATH=/usr/local/apps/netcdf4/4.7.4/GNU/8.5/lib

for var in $var_list ; do
  rm -f ${var}.log 
done

### Run
for vth in $(seq 0 3 21) ; do
  vth2=$(printf "%02d" $vth)
  ft=$vth
  if [ $ft -eq 0 ] ; then
    ftz=$ft
  else
    ftz=${ft}00
  fi
  while [ $ft -le $run_len ] ; do
    ft2=$(printf "%02d" $ft)
    echo "Process: $vdate $vth2 ft: $ft2"
    rm -f tmp.log
    $ffilecheck ${in_dir}/vepSYNOP.nc.${vdate}${vth2}Z $ftz > tmp.log
    for var in $var_list ; do
      str=$(grep "ensemble mean in observation space" tmp.log | grep $var | cut -d \| -f 7-10)
      echo "$vdate $vth2 $ft2 $str" >> ${var}.log
    done
    ft=$(($ft+24))
  done
done
