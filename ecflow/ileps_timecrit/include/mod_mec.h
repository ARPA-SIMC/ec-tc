# August 2025: module versions required to run mec version 2.25
# (built by DWD; binary file /perm/dfv/dace/gnu/bin/mec)
module load prgenv/gnu
module load gcc
module load hdf5 netcdf4 openblas hpcx-openmpi
module load ecmwf-toolbox/2023.01.0.0     # with eccodes 2.28

export ZCLPERM=/perm/zcl/
export ECCODES_DEFINITION_PATH=/perm/zcl//grib_api_def/ec-2.28.0/definitions.edzw:/usr/local/apps/ecmwf-toolbox/2023.01.0.0/GNU/8.4/share/eccodes/definitions
export ECCODES_SAMPLES_PATH=/usr/local/apps/ecmwf-toolbox/2023.01.0.0/GNU/8.4/share/eccodes/samples
