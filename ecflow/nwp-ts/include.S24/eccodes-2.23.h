# Modules and environment for eccodes library
# eccodes version must strictly correspond to definitions; both must be the same used to compile Icon
# Definitions 2.23 should nevertheless be compatible at least with eccodes 2.24 (ecmwf-toolbox/2021.12.0.0)

module load ecmwf-toolbox/2021.08.3.0

export DWDPERM=/perm/dwg/
export ECCODES_DEFINITION_PATH=${DWDPERM}/grib_api_def/ec-2.23.0/definitions.edzw:${DWDPERM}/grib_api_def/ec-2.23.0/definitions
export ECCODES_SAMPLES_PATH=${DWDPERM}/grib_api_def/ec-2.23.0/samples-19.1

