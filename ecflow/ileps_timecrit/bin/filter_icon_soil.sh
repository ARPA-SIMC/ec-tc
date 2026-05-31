#!/bin/bash
datah=$1

. /home/zcl2/ec-tc/ecflow/ileps_timecrit/include/mod_icon.h
cd /ec/ws1/tc/zcl2/tcwork/tmp/iconsoil

rm -f template.grb2 FR_ICE.grb2 icon-eu_soil.grb2
grib_copy -w shortName=W_SO/T_SO/T_G/T_ICE/H_ICE/QV_S/FR_ICE/W_I/T_SO/T_SNOW/W_SNOW/RHO_SNOW/H_SNOW/FRESHSNW/Z0/T_MNW_LK/T_BOT_LK/H_ML_LK/T_WML_LK/C_T_LK/DEPTH_LK \
  /ec/ws2/tc/zcl/tcwork/lb/cleps_timecrit/0001/bc_ic_00/${datah}_icon_dwd/icon-eu_to_cleps_${datah}.grb2 icon-eu_soil_${datah}00.grb2
grib_copy -w shortName=Z0 icon-eu_soil_${datah}00.grb2 template.grb2
grib_set -s discipline=10,parameterCategory=2,parameterNumber=0 -d 0. template.grb2 FR_ICE.grb2
cat FR_ICE.grb2 >> icon-eu_soil_${datah}00.grb2
