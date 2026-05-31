#!/bin/bash
#SBATCH --output ./run_iconsub.log
#SBATCH --error ./run_iconsub.log
#SBATCH --threads-per-core=1
#SBATCH --export=ALL,STHOST
#SBATCH --account=cosmo
#SBATCH --qos=nf
#SBATCH --time=23:00:00
#SBATCH --ntasks=1

# This script runs iconsub to remove the nudging zone from Icon-LEPS test outputs
# from 20250614 to 20250709. 
# TODO: add strie, to process only 3 hourly files (for Carma dataset)
# Possibile accounts:
# - user mck, --account=spitrasp
# - user zcl, --account=cosmo

set -x

# User modifications
arc_root_in=/ec/res4/scratch/mck/arc/proc_ff
arc_root_out=/ec/res4/scratch/mck/arc/proc_ff_sub
run_dir=/ec/res4/scratch/mck/tmp/run

date1=20250614
ndays=10
nmem=2

cd $run_dir
ln -sf /ec/ws1/tc/zcl2/tcwork/bin/iconsub .
ln -sf /ec/ws1/tc/zcl2/tcwork/const/icon-leps-2.5km/ileps_2.5km_DOM01.nc
ln -sf /home/zcl2/ec-tc/ecflow/ileps_timecrit/include/mod_icon.h .
ln -sf /ec/res4/scratch/mck/tmp/iconsub_store.nc .

. ./mod_icon.h

cnt=0
while [ $cnt -lt $ndays ] ; do
  datac=$(date -d "$date1 + ${cnt}day" +%Y%m%d)
  for mem in $(seq $nmem) ; do
    in_dir=${arc_root_in}/${datac}1800/mem_${mem}
    out_dir=${arc_root_out}/${datac}1800/mem_${mem}
    mkdir -p $out_dir
# TODO: build list of files, with ft stride (3 hours)
    for in_file in $(ls ${in_dir}/*.grb) ; do
      bfile=$(basename $in_file)
      out_file=${out_dir}/${bfile}
      rm -f iconsub.nml
      cat > iconsub.nml << EOF
&iconsub_nml
  data_filename = "${in_file}"
  grid_filename = "ileps_2.5km_DOM01.nc"
  ndiv = 150
  output_type = 2
  lwrite_grid = .FALSE.
  ncstorage_file = iconsub_store.nc
/
&subarea_nml
  ORDER = "${out_file}"
  startlon = -10.
  endlon = 36.5
  startlat = 29.2
  endlat = 58.7
  pollon = -180.0
  pollat = 90.0
/
EOF
      echo "### iconsub: process day $datac mem $mem file $in_file"
      ./iconsub --nml=iconsub.nml
    done
  done
  cnt=$(($cnt+1))
done

