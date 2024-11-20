# Modules and environment for eccodes library
# eccodes version must strictly correspond to definitions; both must be the same used to compile Icon

module load ecmwf-toolbox/2024.04.0.0

export ZCLPERM=/perm/zcl/
export ECCODES_DEFINITION_PATH=${ZCLPERM}/grib_api_def/ec-2.35.0/definitions.edzw:${ZCLPERM}/grib_api_def/ec-2.35.0/definitions
export ECCODES_SAMPLES_PATH=${ZCLPERM}/grib_api_def/ec-2.35.0/samples-19.1

# export DWDPERM=/perm/dwg/
# export ECCODES_DEFINITION_PATH=${DWDPERM}/grib_api_def/ec-2.35.0/definitions.edzw:${DWDPERM}/grib_api_def/ec-2.35.0/definitions
# export ECCODES_SAMPLES_PATH=${DWDPERM}/grib_api_def/ec-2.35.0/samples-19.1
