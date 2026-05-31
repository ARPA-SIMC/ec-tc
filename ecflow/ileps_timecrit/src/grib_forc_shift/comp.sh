#!/bin/bash
#module load ecmwf-toolbox
# module purge
. /home/zcl2/ec-tc/ecflow/ileps_timecrit/include/mod_mec.h

gfortran -o grib_forc_shift.exe $ECCODES_INCLUDE $ECCODES_LIB \
  kinds.F90 missing_values.f90 io_units.F90 optional_values.f90 \
  char_utilities.F90 err_handling.f90 datetime_class.F90 grib_forc_shift.f90
