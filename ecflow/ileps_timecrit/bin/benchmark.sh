#!/bin/bash

# Evaluate execution times of an Icon-LEPS run
echo "Evaluate run in $TCWORK"

cd $TCWORK/work/ileps_tc_2.5km

# Start pre
pre_start=$(ls -ldtr --time-style=+"%H:%M" retrieve_ic_bc.*|head -n 1|cut -d \  -f 6)

# End post
post_end=$(ls -lt --time-style=+"%H:%M" post/*.grib |head -n 1|cut -d \  -f 6)

# Inquire dates of  key files
for m in $(seq 0 20); do 
  echo "Inquire member "$m

# Remap
  rm_start_sec[$m]=$(ls -lrt --time-style=+"%s" remap.${m}.*/*/*.nml | head -n 1 |cut -d \  -f 6)
  rm_end_sec[$m]=$(ls -lrt --time-style=+"%s" remap.${m}/data/*.nc | head -n 1 |cut -d \  -f 6)
  rm_delta[$m]=$(( ${rm_end_sec[$m]}-${rm_start_sec[$m]} ))
  rm_start_hum[$m]=$(ls -lrt --time-style=+"%H:%M" remap.${m}.*/*/*.nml | head -n 1 |cut -d \  -f 6)
  rm_end_hum[$m]=$(ls -lrt --time-style=+"%H:%M" remap.${m}/data/*.nc | head -n 1 |cut -d \  -f 6)

# Icon run
  ic_start_sec[$m]=$(ls -l --time-style=+"%s" icon.${m}/icon.nml|cut -d \  -f 6)
  ic_end_sec[$m]=$(ls -l --time-style=+"%s" icon.${m}/finish.status|cut -d \  -f 6)
  ic_delta[$m]=$(( (${ic_end_sec[$m]}-${ic_start_sec[$m]})/60 ))
  ic_start_hum[$m]=$(ls -l --time-style=+"%H:%M" icon.${m}/icon.nml|cut -d \  -f 6)
  ic_end_hum[$m]=$(ls -l --time-style=+"%H:%M" icon.${m}/finish.status|cut -d \  -f 6)

done

rm_start_first=$(ls -ltr --time-style=+"%H:%M" remap.*.*/*/*.nml |head -n 1 | cut -d \  -f 6)
rm_end_last=$(ls -lt --time-style=+"%H:%M" remap.*/data/*.nc | head -n 1 | cut -d \  -f 6)

ic_start_first=$(ls -ltr --time-style=+"%H:%M" icon.*/icon.nml|head -n 1 | cut -d \  -f 6)
ic_end_last=$(ls -lt --time-style=+"%H:%M" icon.*/finish.status|head -n 1 | cut -d \  -f 6)

# Output (grouped by process)
echo ""
echo "Start pre: "$pre_start
echo "End post : "$post_end

echo ""
echo "Remap"
echo "mm start   end sec"
for m in $(seq 0 20); do 
  if [ $m -le 9 ] ; then
    m2="0$m"
  else
    m2=$m
  fi
  echo "$m2 ${rm_start_hum[$m]} ${rm_end_hum[$m]} ${rm_delta[$m]}"
done 
echo "   $rm_start_first $rm_end_last"

echo ""
echo "Icon run"
echo "mm start   end min"
for m in $(seq 0 20); do 
  if [ $m -le 9 ] ; then
    m2="0$m"
  else
    m2=$m
  fi
  echo "$m2 ${ic_start_hum[$m]} ${ic_end_hum[$m]} ${ic_delta[$m]}"
done 
echo "   $ic_start_first $ic_end_last"


