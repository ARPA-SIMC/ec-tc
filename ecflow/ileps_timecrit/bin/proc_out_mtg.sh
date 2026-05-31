#!/bin/bash
#
# Process raw LEPS output to produce input for meteograms
#

nmembers=20
work_root=${TCWORK}/work/ileps_tc_2.5km
datac=$1  # yyyymmddhh

# set environment
. /home/zcl2/ec-tc/ecflow/ileps_timecrit/include/mod_icon.h
module load nco

#set -x

out_dir=${work_root}/output/mtg/${datac}
mkdir -p $out_dir
chmod o+rx $out_dir
cd $out_dir 

for mem in $(seq 0 $nmembers) ; do
  echo "Process member "$mem
  m2=$(printf "%02d" $mem)
  in_dir=${work_root}/icon.${mem}/data

# Patch: split nc output at initial time, that contains 2 timestamps
  cd $in_dir
  in_file=icon_${datac}0000_mtg_unstr_+00000000.nc
  if [ ! -f ${in_file}.org ] ; then   # not yet processed
    mv $in_file ${in_file}.org
    ncks -d time,0,0 ${in_file}.org icon_${datac}0000_mtg_unstr_+00000000.nc
    ncks -d time,1,1 ${in_file}.org icon_${datac}0000_mtg_unstr_+00010000.nc
  fi
  cd $out_dir 
# End of patch
  
# Copy nc output
  cp ${in_dir}/icon_${datac}0000_mtg_unstr_+????0000.nc .
  rename icon m${m2}_icon icon_${datac}0000_mtg_unstr_+????0000.nc

# Process grib2 output: re-arrange to one file per variable
  rm -f mtg.filt
  cat > mtg.filt << EOF
write "m${m2}_icon_${datac}_unstr_[shortName].grib";
EOF
  grib_filter mtg.filt ${in_dir}/icon_${datac}0000_mtg_unstr_+????0000.grb

done

chmod o+r *.grib *.nc

