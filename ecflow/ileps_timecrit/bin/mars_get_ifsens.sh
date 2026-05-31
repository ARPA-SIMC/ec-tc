#! /bin/bash
#SBATCH --output /ec/ws1/tc/zcl2/tcwork/tmp/verification/ifsens/mars_get_ifsens.log
#SBATCH --mail-type=FAIL
#SBATCH --qos=nf
#SBATCH --time=23:59:00
#SBATCH --export=ALL,STHOST=ws1

# #SBATCH --account=cosmo
# #SBATCH --mem=16G

#--------------------------------------------------------------------------------
# Script to extract IFS-ENS ouptut (tot_prec) from MARS
# Usage: mars_get_ifsens.sh data_ini (YYYYMMDDHH) ndays
#--------------------------------------------------------------------------------
#set -x

# Command line
if [ $1 = "-h" -o $1 = "--help" ] ; then
  echo "Usage: mars_get_ifsens.sh data_ini hh ndays"
  echo "data_ini: YYYYMMDD"
  echo "hh: mutliple values allowed (eg. \"00 12 \")"
  exit 1
fi
if [ $# -ne 3 ] ; then
  echo "Usage: mars_get_ifsens.sh data_ini hh ndays"
  echo "data_ini: YYYYMMDD"
  echo "hh: mutliple values allowed (eg. \"00 12 \")"
  exit 1
fi

module load ecmwf-toolbox

data_ini=$1
hh_list=$2
ndays=$3

grid="0.025/0.02"
area="47.0/6.5/36.5/18.5"     # Italy
stride=4

run_root=/ec/ws1/tc/zcl2/tcwork/tmp/verification/ifsens2
cd $run_root

ngrp=$((($ndays-1)/5+1))

rm -f rt.filt
cat << EOF > rt.filt
write "ifsens_[dataDate][hour]_p2.grb";
EOF

# Loop on days
cnt=0
while [ $cnt -lt $ngrp ] ; do
  dlt1=$(($cnt*$stride))
  dlt2=$((($cnt+1)*$stride-1))
  if [ $dlt2 -ge $ndays ] ; then
    dlt2=$(($ndays-1))
  fi
  data1=$(date -d "$data_ini + ${dlt1}day" +%Y%m%d)
  data2=$(date -d "$data_ini + ${dlt2}day" +%Y%m%d)
  for hhc in $hh_list ; do
    echo "Process run "$data1" "$data2" "$hhc

    fileout=${run_root}/ifsens_${data1}${hhc}_p2.grb
    rm -f $fileout mars_req.ifsens.${data1}${hhc}

    cat <<EOFA > mars_req.ifsens.${data1}${hhc}
retrieve,
  STREAM     = ENFO,
  CLASS      = OD,
  TYPE       = PF,
  EXPVER     = 1,
  DATE       = ${data1}/TO/${data2}/BY/1,
  TIME       = ${hhc},
  STEP       = 93/TO/132/BY/3,
  LEVTYPE    = SFC,
  PARAM      = 228.128,
  NUMBER     = 1/TO/50,
  GRID       = ${grid},
  AREA       = ${area},
  TARGET     = "${fileout}"
EOFA

    mars mars_req.ifsens.${data1}${hhc}

    mv ${fileout} ${fileout}.org
    grib_filter -f rt.filt ${fileout}.org
  done
  cnt=$(($cnt+1))
done

exit

#-----------------------------------------------------------------
# Templates for complete query

# STEP       = 0/TO/90/BY/3,     # First ft, on a single tape
# STEP       = 93/TO/132/BY/3
# STEP       = 0/TO/132/BY/3,    # all ft, on two tapes

