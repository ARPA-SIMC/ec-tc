#!/bin/bash
#SBATCH --output=./remap.log
#SBATCH --threads-per-core=1
#SBATCH --export=ALL,STHOST=ws1
#SBATCH --account=cosmo
#SBATCH --qos=np
#SBATCH --time=05:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=128
#SBATCH --mem-bind=local

#
# extract a lat-lon subarea from raw LEPS output for MEC
#

BIN_DIR=/ec/ws1/tc/zcl2/tcwork/bin
nmembers=20
in_root=/ec/ws1/tc/zcl2/tcwork/arc
out_root=/ec/ws2/tc/zcl2/tcwork/arc
datac=$1  # yyyymmddhh

# set environment
. /home/zcl2/ec-tc/ecflow/ileps_timecrit/include/mod_icon.h
#module load nco

# set -x

out_dir=${out_root}/proc_ff_Carmens/${datac}
mkdir -p $out_dir
chmod o+rx $out_dir
cd $out_dir 

in_dir=${in_root}/proc_ff/${datac}1800
if [ ! -d $in_dir ] ; then
  echo "Retreive from tapes (to be done)"
  exit
fi

for mem in $(seq 0 $nmembers) ; do
  echo "Process member "$mem" scad +"$sca
  m2=$(printf "%02d" $mem)
#  for sc_dd in $(seq 0 1) ; do
#  for sc_hh in $(seq 0 3 21) ; do
  for sc_dd in 2 ; do
  for sc_hh in 0 ; do
    echo "Process scad: "$sc_dd" "$sc_hh
    sc_hh2=$(printf "%02d" $sc_hh)
    out_file=${out_dir}/lfff0${sc_dd}${sc_hh2}0000.${datac}18.pf0${m2}

  rm -f iconremap.nml
cat <<EOF > iconremap.nml
&remap_nml
 in_grid_filename  = "/ec/ws1/tc/zcl2/tcwork/const/icon-leps-2.5km/ileps_2.5km_DOM01.nc"
 in_filename       = "${in_dir}/mem_${mem}/icon_${datac}180000_mlevSurf_unstr_+0${sc_dd}${sc_hh2}0000.grb"
 in_type           = 2
! out_grid_filename = ""
 lsynthetic_grid   = .TRUE.
 nxpoints          = 881
 nypoints          = 416
 corner1           = 4.,43.
 corner2           = 26.,51.3
 pole              = -180.,90.
 out_filename      = "${out_file}"
 out_type          = 3
 out_filetype      = 2
 l_have3dbuffer    = .false.
 ncstorage_file    = "${out_root}/proc_ff_Carmens/ncstorage.nc"
/
!
! 3D fiuelds
!
&input_field_nml  ! horiz. wind comp. u
 inputname      = "U"
 outputname     = "U"
 intp_method = 3
/
&input_field_nml  ! horiz. wind comp. v
 inputname      = "V"
 outputname     = "V"
 intp_method = 3
/
&input_field_nml  ! temperature
 inputname      = "T"
 outputname     = "T"
 intp_method = 3
/
&input_field_nml  ! specific humidity
 inputname      = "QV"
 outputname     = "QV"
 intp_method = 3
/
&input_field_nml  ! pressure
 inputname      = "P"
 outputname     = "P" 
 intp_method = 3
/
!
! Surface and soil parameters (grib2 or grib1)
!
&input_field_nml  ! 
 inputname      = "U_10M"
 outputname     = "U_10M" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "V_10M"
 outputname     = "V_10M" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "VMAX_10M"
 outputname     = "VMAX_10M" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "T_2M"
 outputname     = "T_2M" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "TD_2M"
 outputname     = "TD_2M" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "TMAX_2M"
 outputname     = "TMAX_2M" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "TMIN_2M"
 outputname     = "TMIN_2M" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "CLCL"
 outputname     = "CLCL" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "CLCM"
 outputname     = "CLCM" 
 intp_method = 3
/
&input_field_nml  !
 inputname      = "CLCH"
 outputname     = "CLCH" 
 intp_method = 3
/
&input_field_nml  !
 inputname      = "CLCT"
 outputname     = "CLCT" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "ASWDIFD_S"
 outputname     = "ASWDIFD_S" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "ASWDIR_S"
 outputname     = "ASWDIR_S" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "T_G"
 outputname     = "T_G" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "H_SNOW"
 outputname     = "H_SNOW" 
 intp_method = 3
/
&input_field_nml  !
 inputname      = "PS"
 outputname     = "PS" 
 intp_method = 3
/
&input_field_nml  ! 
 inputname      = "TOT_PREC"
 outputname     = "TOT_PREC" 
 intp_method = 3
/
EOF

    srun $BIN_DIR/iconremap --remap_nml=iconremap.nml > iconremap.log 2>&1
#    $BIN_DIR/iconremap -vvv --remap_nml=iconremap.nml > iconremap.log 2>&1
    chmod o+r $out_file

  done  # scad (hours)
  done  # scad (days)
done    # members
