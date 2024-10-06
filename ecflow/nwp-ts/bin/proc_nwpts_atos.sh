#!/bin/bash
#
# Semi-automatic script, to plot a set of maps from the output of a run of NWP-TS
# Input data must be preliminary gathered with get_raw_output.sh or 
# get_raw_output_multi.sh, that must be run batch (squeue)
# This script can be run interactively, from the dir with icon output "TOT_PREC.grib"
#

# User modifications
offline_remap="/home/itic/nwptsH_ecflow/nwptsICON/bin/offline_remap.sh"
# in_grid=/perm/itic/rescue/icon-lam/ICON-domain/icon-leps_grid_0/grid_0_DOM01.nc  # new domain (S23)
in_grid=/perm/itic/rescue/icon-lam/ICON-domain/icon-leps_MED_2.5km/icon-leps-2.5km_DOM01.nc # old domain
in_file="TOT_PREC.grib"
out_grid=/home/itic/nwptsH_ecflow/nwptsICON/bin/outGrid_regular.grib

module load ecmwf-toolbox

# Build the grib filter for tp
# tp is archived as "cumulated from the beginning of each 5 days run section"
rm -f tp_total.filt
cat << EOF > tp_total.filt
if (lengthOfTimeRange == 7200 ) {
  append "tpt_unstr.grib";
}
if (forecastTime == 43200 && lengthOfTimeRange == 1440 ) {
  append "tpt_unstr.grib";
}
EOF

echo "grib filter"
rm -f tpt_unstr.grib 
grib_filter tp_total.filt $in_file

# Interpolate on regular grid
echo "remap on regular grid"
rm -f tpt_regular.grib
$offline_remap $in_grid tpt_unstr.grib $out_grid tpt_regular.grib 

