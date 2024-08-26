#!/bin/bash
#
# Script semi-autmomatico per interpolare su griglia regolare un file con grib
# su griglia triangolare Icon
# Uso: offline_remap.sh in_grid in_file out_grid out_file
# Richiede in input:
# - griglia di input (*DOM01.nc)
# - griglia di output (un file grib sulla griglia richiesta in output)

if [ $# -ne 4 ] ; then
  echo "Uso: offline_remap.sh in_grid in_file out_grid out_file"
  exit 1
fi

in_grid=$1
in_file=$2
out_grid=$3
out_file=$4

# Input grid for NWP-TS on ATOS
# in_grid=/perm/itic/rescue/icon-lam/ICON-domain/*/*_DOM01.nc

# Assegnazioni perlanciare iconremap su Atos
module purge
module load prgenv/intel
module load intel/2021.4.0
module load hpcx-openmpi/2.9.0
module load hdf5/1.10.6
module load netcdf4/4.7.4
module load ecmwf-toolbox/2021.12.0.0
module load aec
module load intel-mkl/19.0.5

# Eseguibilie iconremap
REMAP=/perm/itic/eseguibili/icontools/iconremap 

# Trovo la lista dei paraemtri presenti nel file
echo "Build list of input variables"
var_list=$(grib_get -p shortName $in_file | sort | uniq)
echo $var_list

# Creo le namelist
rm -f remap.nm
cat << EOFa > remap.nml
&remap_nml
 in_grid_filename  = "${in_grid}"
 in_filename       = "${in_file}"
 in_type           = 2
 out_grid_filename = "${out_grid}"
 out_filename      = "${out_file}"
 out_type          = 3
 out_filetype      = 2
 l_have3dbuffer    = .false.
 ncstorage_file    = "ncstorage.tmp"
/
EOFa

rm -f fields.nml
for var in $var_list ; do
cat << EOFb >> fields.nml
&input_field_nml
 inputname      = "$var"         
 outputname     = "$var"          
 intp_method    = 3
/
EOFb
done

echo "Run remap"
rm -f ncstorage.tmp* $out_file
$REMAP --remap_nml remap.nml --input_field_nml fields.nml

