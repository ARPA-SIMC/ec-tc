#!/bin/bash
#--------------------------------------------------------------------------------------------------------
# Semi-automatic script, to process the regular-grid output of a NWP-TS experiment; the final goal is to 
# to produce a set of diagnostic maps.
# This is the second step, and must be run on SIMC servers; script 1 is run on ATOS
#
# Process a list of esperiments: download data from Atos, process wind speed and Tmax/Tmin, calculate
# average and RMS differences
#
#                                                                        Version 1.1.0, Enrico 17/07/2025
#--------------------------------------------------------------------------------------------------------
# set -e
set -x
grib_rms_diff=/home/eminguzzi@ARPA.EMR.NET/enr/git/ma_utils/util/grib/src/grib_rms_diff.exe
grib_uv2ffdd=/home/eminguzzi@ARPA.EMR.NET/enr/git/ma_utils/util/grib/src/grib_uv2ffdd.exe
grib_stat=/home/eminguzzi@ARPA.EMR.NET/enr/git/ma_utils/util/grib/src/grib_stat.exe
root_dir=/home/eminguzzi@ARPA.EMR.NET/enr/icon/tmp/nwpts
grib_set_tranges=${root_dir}/sh/grib_set_tranges.exe

exp1="S24icon202410"
exp2="S24icon202504"
exp_lst="$exp1 $exp2"
nexp=2

mkdir -p ${root_dir}/run ${root_dir}/grb
cd ${root_dir}/run

# for mm in 07 12 ; do
for mm in 12 ; do
for var in CLCT PMSL RELHUM_2M TMAX_2M TMIN_2M TOT_PREC FF_10M ; do

# Download from ATOS
  for exp in $exp_lst ; do
    if [ $var = FF_10M ] ; then
      if [ ! -s ts_${exp}_2021${mm}_U_10M.grb.org ] ; then
        scp mck@ecs-login:/ec/res4/scratch/mck/nwptsH/diagno/ts_${exp}_2021${mm}_U_10M.grb ./ts_${exp}_2021${mm}_U_10M.grb.org
      fi
      if [ ! -s ts_${exp}_2021${mm}_V_10M.grb.org ] ; then
        scp mck@ecs-login:/ec/res4/scratch/mck/nwptsH/diagno/ts_${exp}_2021${mm}_V_10M.grb ./ts_${exp}_2021${mm}_V_10M.grb.org
      fi
    elif [ $var = TMAX_2M ] ; then
      if [ ! -s ts_${exp}_2021${mm}_T_2M.grb.org ] ; then
        scp mck@ecs-login:/ec/res4/scratch/mck/nwptsH/diagno/ts_${exp}_2021${mm}_T_2M.grb ./ts_${exp}_2021${mm}_T_2M.grb.org
      fi
    elif [ $var = TMIN_2M ] ; then
      echo "Only retrieve for TMAX"
    else
      if [ ! -s ts_${exp}_2021${mm}_${var}.grb.org ] ; then
        scp mck@ecs-login:/ec/res4/scratch/mck/nwptsH/diagno/ts_${exp}_2021${mm}_${var}.grb ./ts_${exp}_2021${mm}_${var}.grb.org
      fi
    fi
  done
    
# Pre-processing
  for exp in $exp_lst ; do
    if [ $var = TOT_PREC ] ; then
      file=ts_${exp}_2021${mm}_${var}.grb
      rm -f tmp.grb

# Bug fix (2025/02/02): in icon-202410, at restart forecastTime of processed timeranges is updated
#  to restart time, but values are not re-set to zero
      if [ $exp = S24icon202410 ] ; then
        $grib_set_tranges ${file}.org tmp.grb
      else
        ln -s ${file}.org tmp.grb
      fi
      
      vg6d_transform --trans-mode=p --comp-stat-proc=1 --comp-step='0 01' tmp.grb $file

      file24=${var}24h_${mm}_sum_${exp}.grb
      vg6d_transform --trans-mode=p --comp-stat-proc=1 --comp-step='1 00' --comp-full-steps --comp-frac-valid=0 tmp.grb $file24
      mv $file24 ${root_dir}/grb
      
    elif [ $var = FF_10M ] ; then
      $grib_uv2ffdd ts_${exp}_2021${mm}_U_10M.grb.org ts_${exp}_2021${mm}_V_10M.grb.org
      mv ff.grib ts_${exp}_2021${mm}_FF_10M.grb

    elif [ $var = TMAX_2M ] ; then
      vg6d_transform --trans-mode=p --comp-stat-proc=254:2 --comp-step='1 00' --comp-full-steps --comp-frac-valid=0 ts_${exp}_2021${mm}_T_2M.grb.org ts_${exp}_2021${mm}_TMAX_2M.grb

    elif [ $var = TMIN_2M ] ; then
      vg6d_transform --trans-mode=p --comp-stat-proc=254:3 --comp-step='1 00' --comp-full-steps --comp-frac-valid=0 ts_${exp}_2021${mm}_T_2M.grb.org ts_${exp}_2021${mm}_TMIN_2M.grb

    else
      file=ts_${exp}_2021${mm}_${var}.grb
      ln -s ${file}.org $file
    fi
  done
  
# Calculate RMS difference
  if [ $nexp -ge 2 ] ; then
    $grib_rms_diff ts_${exp2}_2021${mm}_${var}.grb ts_${exp1}_2021${mm}_${var}.grb -check=all
    mv rms_diff.grib ${root_dir}/grb/${var}_${mm}_rmd_${exp2}-${exp1}.grb
  fi
  if [ $nexp -ge 3 ] ; then
    $grib_rms_diff ts_${exp3}_2021${mm}_${var}.grb ts_${exp2}_2021${mm}_${var}.grb -check=all
    mv rms_diff.grib ${root_dir}/grb/${var}_${mm}_rmd_${exp3}-${exp2}.grb
    $grib_rms_diff ts_${exp3}_2021${mm}_${var}.grb ts_${exp1}_2021${mm}_${var}.grb -check=all
    mv rms_diff.grib ${root_dir}/grb/${var}_${mm}_rmd_${exp3}-${exp1}.grb
  fi

# Calculate average fields
  for exp in $exp_lst ; do
    if [ $var = TOT_PREC ] ; then
      $grib_stat ts_${exp}_2021${mm}_${var}.grb sum
      mv sum.grib ${root_dir}/grb/${var}_${mm}_sum_${exp}.grb
    else
      $grib_stat ts_${exp}_2021${mm}_${var}.grb ave
      mv ave.grib ${root_dir}/grb/${var}_${mm}_ave_${exp}.grb
    fi
  done

# Delete original files
#  rm -f *${mm}_${var}.grb
#  if [ $var = TOT_PREC ] ; then
#    rm -f tmp.grb
#  elif [ $var = FF_10M ] ; then
#    rm -f *${mm}_U_10M.grb
#    rm -f *${mm}_V_10M.grb
#  elif [ $var = TMAX_2M -o $var = TMIN_2M ] ; then
#    rm -f *${mm}_T_2M.grb
#  fi

done
done
