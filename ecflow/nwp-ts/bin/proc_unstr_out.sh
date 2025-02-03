#!/bin/bash
#SBATCH --qos=nf
#SBATCH --account=spitrasp
#SBATCH --job-name=proc_unst
#SBATCH --output=proc_unst.out
#SBATCH --error=proc_unst.err
#SBATCH --time=23:59:00
#SBATCH --mem=16G

# Process the output of an old NWP-TS experiment, to produce diagnostic maps
# - read from ECFS output on unstructured grid
# - interpolate the useful outputs on regular grid for further processing
# - save in $HPCPERM

module load apptainer/1.3.0 cdo/2.4.0 nco/5.1.9 prgenv/intel intel/2021.4.0 eclib/1.1.0 netcdf4/4.9.2
module load ecmwf-toolbox/2024.04.0.0
export ZCLPERM=/perm/zcl/
export ECCODES_DEFINITION_PATH=${ZCLPERM}/grib_api_def/ec-2.35.0/definitions.edzw:${ZCLPERM}/grib_api_def/ec-2.35.0/definitions
export ECCODES_SAMPLES_PATH=${ZCLPERM}/grib_api_def/ec-2.35.0/samples-19.1

set -x
set -e
module list
codes_info

in_grid=/perm/mck/rescue/icon-lam/ICON-domain/icon-leps_grid_0/grid_0_DOM01.nc
proj=S23icon265
ecf_root=ec:/itic/nwp_suite_HindcastMode
arc_root=/ec/res4/hpcperm/mck/nwp-ts
work_dir=/ec/res4/scratch/mck/nwptsH/${proj}/tmp
iconremap=/ec/ws1/tc/zcl2/tcwork/bin/iconremap

field_lst="TOT_PREC CLCT ASOB_S ATHB_S ASHFL_S ALHFL_S PMSL T_2M RELHUM_2M U_10M V_10M VMAX_10M"
filt_lst=$(echo $field_lst | sed 's/ /\//g')

cd $work_dir
rm -f fields.nml

for field in $field_lst ; do
cat >> fields.nml << EOFA
&input_field_nml
 inputname      = "${field}"
 outputname     = "${field}"
 intp_method    = 3
/
EOFA
done

for mm in 07 12 ; do
mkdir -p ${arc_root}/2021${mm}

if [ $mm = "07" ] ; then
  dd1=28
else
  dd1=1
fi

for dd in $(seq -f "%02g" $dd1 31) ; do
  echo "----------------------------------------------------------------------------------"
  echo "##### Processing $mm $dd"
  rm -f surf_uns.grb surf_geo.grb iconremap.nml
  in_file=ITS_S23icon265_2021${mm}${dd}_basic.grb
  out_file=${proj}_str_surf_${dd}000000.grb
  if [ ! -s $in_file ] ; then
    ecp ${ecf_root}/${proj}/2021${mm}/${in_file} .
  fi
  grib_copy -w shortName=${filt_lst} $in_file surf_uns.grb

  cat > iconremap.nml << EOFB
&remap_nml
  in_grid_filename  = '$in_grid' 
  in_filename       = '$in_file'
  in_type           = 2
  lsynthetic_grid   = .TRUE.
  nxpoints          = 1861
  nypoints          = 1516
  corner1           = -10.,28.9
  corner2           = 36.5,59.2
  out_filename      = '$out_file'
  out_type          = 3
  out_filetype      = 2
  ncstorage_file    = "ncstorage.tmp"
/
EOFB

  $iconremap --remap_nml iconremap.nml --input_field_nml fields.nml
  mv $out_file ${arc_root}/${proj}/2021${mm}
  rm $in_file  
done
done

