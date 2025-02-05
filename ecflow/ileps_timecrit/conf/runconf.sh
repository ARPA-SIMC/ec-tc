# this script is sourced with set -a (automatic export), so there is
# no need to export every single variable

MODEL_STOP_00=120
MODEL_STOP_06=126
MODEL_STOP_12=120
MODEL_STOP_18=126
ENS_TOTAL_MEMB=20 # this must coincide with the value in suite definition until there will be a mechanism to dynamically reduce the number of members
GRIB_CENTER=80
GRIB_SUBCENTER=255
MODEL_FCENS_GP=131
MODEL_FCAST_GP=132

ICONSOIL_DIR=$ECTC_WORK/iconsoil
RETRIEVE_CLA_DIR=$ECTC_WORK/retrieve_cla_pl
CLUST_ANA_DIR=$ECTC_WORK/clust_ana
RETRIEVE_IC_BC_DIR=$ECTC_WORK/retrieve_ic_bc.$ENS_MEMB.$RETRIEVE_STOP
RETRIEVE_IC_BC_DIR_COMMON=$ECTC_WORK/retrieve_ic_bc.$ENS_MEMB
REMAP_DIR=$ECTC_WORK/remap.$ENS_MEMB
REMAP_DIR_DAY=$ECTC_WORK/remap.$ENS_MEMB.$RETRIEVE_STOP
MODEL_DIR=$ECTC_WORK/icon.$ENS_MEMB
REGRIB_DIR=$ECTC_WORK/regrib.$ENS_MEMB
POST_DIR=$ECTC_WORK/post
SEND_DIR=$ECTC_WORK/send
ARCHIVE_DIR=$ECTC_WORK/archive
PREOPE_OUT=$TCWORK/ileps_preope
PRIVATE=$TCWORK/lb/private
BIN_DIR=$ECTC_WORKBASE/bin
CONST_DIR=$ECTC_WORKBASE/const
SIMC_TOOLS="singularity exec -B $ECTC_WORK $BIN_DIR/simctools_simc_tools_r8.sif"

# input data configuration
CLA_AREA=60.0/-10.0/30.0/30.0
CLA_GRID=0.2/0.2
CLA_EXPID=1
CLA_HINCBD=3

EPS_NLEV=137
EPS_AREA=65.0/-13.0/27.0/40.0
EPS_GRID=0.1/0.1
EPS_EXPID=1
EPS_HINCBD=3

DET_NLEV=137
DET_AREA=65.0/-13.0/27.0/40.0
DET_GRID=0.1/0.1
DET_EXPID=1
DET_HINCBD=3

# ensemble configuration
NECEPSMEMB=50
NLEPSMEMB=20
#deb NECEPSMEMB=5
#deb NLEPSMEMB=4
CLA_LAG=(0 6)
#old CLA_LAG=(0 12)
#deb CLA_LAG=(0)
CLA_STEP=(96 120)

CLA_NENS=${#CLA_LAG[*]}
CLA_NSTEP=${#CLA_STEP[*]}


## @fn safe_rm_rf()
## @brief Remove recursively directory tree(s) with some security checks.
## @details This function removes the requested directories and all
## their content recursively (`rm -rf`) performing some preliminary
## checks to prevent removing undesired files due, e.g., to wrong
## environmental variable assignment. Each argument provided must
## match the following conditions:
## 
##  * being non null
##  * being not equal to /
##  * being a directory
##  * being owned or writable by the user
## 
## otherwise the removal for the current argument is canceled and the
## following argument is examined.
## 
## @param $* directories to be removed
safe_rm_rf() {
    for dir in "$@"; do
        if [ -n "$dir" -a "$dir" != "/" ]; then
            if [ -d "$dir" ]; then
                if [ -O "$dir" -o -w "$dir" ]; then
                    rm -rf "$dir"
                fi
            fi
        fi
    done

}

conf_template() {
  $ECTC_BIN/ac_templater.py $1.in > $1
}

