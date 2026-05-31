#! /bin/bash
#SBATCH --output /ec/ws1/tc/zcl2/tcwork/tmp/verification/cleps/mars_get_cleps.log
#SBATCH --mail-type=FAIL
#SBATCH --qos=nf
#SBATCH --time=23:59:00
#SBATCH --export=ALL,STHOST=ws1

# #SBATCH --account=cosmo
# #SBATCH --mem=16G

#--------------------------------------------------------------------------------
# Script to extract Cleps ouptut (tot_prec) from MARS
# Usage: mars_get_cleps.sh data_ini (YYYYMMDDHH) ndays
#--------------------------------------------------------------------------------
set -x

# Command line
if [ $1 = "-h" -o $1 = "--help" ] ; then
  echo "Usage: mars_get_cleps.sh data1 hh ndays"
  echo "data1: YYYYMMDD"
  echo "hh: mutliple values allowed (eg. \"00 12 \")"
  exit 1
fi
if [ $# -ne 3 ] ; then
  echo "Usage: mars_get_cleps.sh reftime ndays"
  echo "data1: YYYYMMDD"
  echo "hh: mutliple values allowed (eg. \"00 12 \")"
  exit 1
fi

data1=$1
hh_list=$2
ndays=$3

grid="0.025/0.02"
area="47.0/6.5/36.5/18.5"     # Italy

run_root=/ec/ws1/tc/zcl2/tcwork/tmp/verification/cleps
cd $run_root

# Loop on days
cnt=0
while [ $cnt -lt $ndays ] ; do
  datac=$(date -d "$data1 + ${cnt}day" +%Y%m%d)
  for hhc in $hh_list ; do
    echo "Process run "$datac" "$hhc

    fileout=${run_root}/cleps_${datac}${hhc}.grb
    rm -f $fileout mars_req.cleps.${datac}${hhc}

    cat <<EOFA > mars_req.cleps.${datac}${hhc}
retrieve,
  STREAM     = ENFO,
  CLASS      = CO,
  TYPE       = PF,
  EXPVER     = 0001,
  DATE       = ${datac},
  TIME       = ${hhc},
  STEP       = 0/TO/132/BY/3,
  LEVTYPE    = SFC,
  PARAM      = 61.2,
  NUMBER     = 1/TO/20,
  GRID       = ${grid},
  AREA       = ${area},
  TARGET     = "${fileout}"
EOFA

    mars mars_req.cleps.${datac}${hhc}

  done
  cnt=$(($cnt+1))
done

exit

#-----------------------------------------------------------------
# Templates for complete query

# Surface variables
#   param      = 1.2/2.2/11.2/15.2/16.2/17.2/33.2/34.2/61.2/62.2/66.2/71.2/74.2/73.2/75.2/78.2/79.2/111.2/84.2/121.2/122.2/22.201/23.201/84.201/85.201/102.201/112.2/113.201/187.201/241.201/149.203/146.201/,
#   LEVTYPE    = SFC,

# Variablaes on first model levels
#   param      = 33.2/34.2/1.2/11.2,
#   LEVTYPE    = ML,
#   LEVELIST   = 35/36/37/38/39/40,



