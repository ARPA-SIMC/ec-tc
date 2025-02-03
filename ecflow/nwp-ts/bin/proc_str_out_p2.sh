#!/bin/bash
#--------------------------------------------------------------------------------------------------------
# Semi-automatic script, to process the regular-grid output of a NWP-TS experiment; the final goal is to 
# to produce a set of diagnostic maps.
# This is the second step, and must be run on SIMC servers; script 1 is run on ATOS
#
# Elabora una lista di esperimenti: scarica i dati da Atos, elabora vento e tmax/tmin, calcola ave e rmsd
#
#                                                                        Version 1.0.0, Enrico 02/02/2025
#--------------------------------------------------------------------------------------------------------

set -x
grib_rms_diff=/home/eminguzzi@ARPA.EMR.NET/enr/git/ma_utils/util/grib/src/grib_rms_diff.exe
grib_uv2ffdd=/home/eminguzzi@ARPA.EMR.NET/enr/git/ma_utils/util/grib/src/grib_uv2ffdd.exe
grib_stat=/home/eminguzzi@ARPA.EMR.NET/enr/git/ma_utils/util/grib/src/grib_stat.exe
root_dir=/home/eminguzzi@ARPA.EMR.NET/enr/icon/tmp/nwpts
grib_set_tranges=${root_dir}/sh/grib_set_tranges.exe

exp1="S23icon265"
exp2="S23icon202410"
exp3="S24icon202410"
exp_lst="$exp1 $exp2 $exp3"

cd ${root_dir}/run

# for mm in 07 12 ; do
for mm in 07 ; do
# for var in CLCT PMSL RELHUM_2M TMAX_2M TMIN_2M TOT_PREC FF_10M ; do
for var in TOT_PREC ; do

# Download da ATOS
for exp in $exp_lst ; do
  if [ $var = FF_10M ] ; then
    scp mck@ecs-login:/ec/res4/scratch/mck/nwptsH/diagno/ts_${exp}_2021${mm}_U_10M.grb .
    scp mck@ecs-login:/ec/res4/scratch/mck/nwptsH/diagno/ts_${exp}_2021${mm}_V_10M.grb .
  elif [ $var = TMAX_2M ] ; then
    scp mck@ecs-login:/ec/res4/scratch/mck/nwptsH/diagno/ts_${exp}_2021${mm}_T_2M.grb .
  elif [ $var = TMIN_2M ] ; then
    echo "Only retrieve for TMAX"
  else
    scp mck@ecs-login:/ec/res4/scratch/mck/nwptsH/diagno/ts_${exp}_2021${mm}_${var}.grb .
  fi
done
    
# Pre-processing
  for exp in $exp_lst ; do
    if [ $var = TOT_PREC ] ; then
      file=ts_${exp}_2021${mm}_${var}.grb
      rm -f tmp.grb

# Bug fix (2025/02/02): in icon-202410, at restart forecastTime of processed timeranges is updated
#  to restart time, but values are not re-set to zero
#     mv $file tmp.grb
      $grib_set_tranges $file tmp.grb
      rm $file

      vg6d_transform --trans-mode=p --comp-stat-proc=1 --comp-step='0 01' tmp.grb $file

      file24=${var}24h_${mm}_sum_${exp}.grb
      vg6d_transform --trans-mode=p --comp-stat-proc=1 --comp-step='1 00' --comp-full-steps --comp-frac-valid=0 tmp.grb $file24
      mv $file24 ${root_dir}/grb
      
    elif [ $var = FF_10M ] ; then
      $grib_uv2ffdd ts_${exp}_2021${mm}_U_10M.grb ts_${exp}_2021${mm}_V_10M.grb
      mv ff.grib ts_${exp}_2021${mm}_FF_10M.grb

    elif [ $var = TMAX_2M ] ; then
      vg6d_transform --trans-mode=p --comp-stat-proc=254:2 --comp-step='1 00' --comp-full-steps --comp-frac-valid=0 ts_${exp}_2021${mm}_T_2M.grb ts_${exp}_2021${mm}_TMAX_2M.grb

    elif [ $var = TMIN_2M ] ; then
      vg6d_transform --trans-mode=p --comp-stat-proc=254:3 --comp-step='1 00' --comp-full-steps --comp-frac-valid=0 ts_${exp}_2021${mm}_T_2M.grb ts_${exp}_2021${mm}_TMIN_2M.grb

    fi
  done
exit  
# Calcolo RMS difference
  $grib_rms_diff ts_${exp3}_2021${mm}_${var}.grb ts_${exp2}_2021${mm}_${var}.grb -check=all
  mv rms_diff.grib ${root_dir}/grb/${var}_${mm}_rmd_${exp3}-${exp2}.grb
  $grib_rms_diff ts_${exp2}_2021${mm}_${var}.grb ts_${exp1}_2021${mm}_${var}.grb -check=all
  mv rms_diff.grib ${root_dir}/grb/${var}_${mm}_rmd_${exp2}-${exp1}.grb
  $grib_rms_diff ts_${exp3}_2021${mm}_${var}.grb ts_${exp1}_2021${mm}_${var}.grb -check=all
  mv rms_diff.grib ${root_dir}/grb/${var}_${mm}_rmd_${exp3}-${exp1}.grb
  
# Calcolo campi medi
  for exp in $exp_lst ; do
    if [ $var = TOT_PREC ] ; then
      $grib_stat ts_${exp}_2021${mm}_${var}.grb sum
      mv sum.grib ${root_dir}/grb/${var}_${mm}_sum_${exp}.grb
    else
      $grib_stat ts_${exp}_2021${mm}_${var}.grb ave
      mv ave.grib ${root_dir}/grb/${var}_${mm}_ave_${exp}.grb
    fi
  done

# Cancello i files originari
  rm -f *${mm}_${var}.grb
  if [ $var = TOT_PREC ] ; then
    rm -f tmp.grb
  elif [ $var = FF_10M ] ; then
    rm -f *${mm}_U_10M.grb
    rm -f *${mm}_V_10M.grb
  elif [ $var = TMAX_2M -o $var = TMIN_2M ] ; then
    rm -f *${mm}_T_2M.grb
  fi
done
done
