#!/bin/bash
#
# Script to build the Mars query that reproduce LEPS operational dissemination for DET
#

rt="20241120 00"
area_geo="65.0/-13.0/27.0/40.0"
area_gauss="66.0/-14.0/26.0/41.0"

file_rt=$(date -d "$rt" +"%m%d%H")
mars_date=$(date -d "$rt" +"%Y%m%d")
mars_time=$(date -d "$rt" +"%H")

if [ $mars_time -eq 0 -o $mars_time -eq 12 ] ; then
  stream="oper"
elif [ $mars_time -eq 6 -o $mars_time -eq 18 ] ; then
  stream="scda"
fi

# Constant fields
target=ITD${file_rt}00${file_rt}001c
qfile=ITD_constant.query
cat << EOF1 > $qfile
# Regular grid, surface
RETRIEVE,
CLASS=OD,
STREAM=${stream},
TYPE=an,
DATE=${mars_date},
TIME=${mars_time},
LEVTYPE=SFC,
PARAM=Z/LSM/SR/SLT,
AREA=${area_geo},
GRID=0.1/0.1,
TARGET=${target}

# Regular grid, level 1
RETRIEVE,
LEVTYPE=ML,
LEVELIST=1,
PARAM=Z,
AREA=${area_geo},
GRID=0.1/0.1,
TARGET=${target}

# Gaussian grid
RETRIEVE,
LEVTYPE=SFC,
PARAM=LSM/SLT,
AREA=${area_gauss},
GRID=AV,
TARGET=${target}
EOF1

# Deterministic forecast
for step in $(seq -w 0 3 144) ; do

vt=$(date -d "$rt +${step}hours" +"%Y%m%d %H")
file_vt=$(date -d "$vt" +"%m%d%H")
target=ITD${file_rt}00${file_vt}001
qfile=ITD_fc.query.${step}
cat << EOF2 > $qfile
RETRIEVE,
CLASS=OD,
STREAM=${stream},
TYPE=fc,
DATE=${mars_date},
TIME=${mars_time},
STEP=${step},
LEVTYPE=ML,
LEVELIST=31/TO/137,
PARAM=T/U/V/W/Q/CLWC/CIWC/CRWC/CSWC,
AREA=${area_geo},
GRID=0.1/0.1,
ACCURACY=12,
PACKING=CCSDS,
TARGET=${target}

RETRIEVE,
LEVTYPE=ML,
LEVELIST=1,
PARAM=LNSP,
AREA=${area_geo},
GRID=0.1/0.1,
ACCURACY=OFF,
PACKING=CCSDS,
TARGET=${target}

RETRIEVE,
LEVTYPE=SFC,
PARAM=SRC/TSN/SKT/STL1/STL2/STL3/STL4/SD/RSN/ASN/CI,
AREA=${area_geo},
GRID=0.1/0.1,
ACCURACY=OFF,
PACKING=SIMPLE,
TARGET=${target}

RETRIEVE,
LEVTYPE=SFC,
PARAM=SST/SWVL1/SWVL2/SWVL3/SWVL4,
AREA=${area_gauss},
GRID=AV,
ACCURACY=OFF,
PACKING=SIMPLE,
TARGET=${target}
EOF2

done
