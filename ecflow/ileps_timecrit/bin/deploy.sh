#!/bin/bash

mkdir -p $TCWORK/const $TCWORK/bin
mkdir -p $TCWORK/const/ecrad

# build cluster analysis code
(
    cd src/cleps_cluster_analysis
    module load ecmwf-toolbox
    make clean; make; make install
)

# build icon code (update ecmwf-toolbox)
(
    cd $TCWORK/src
    git clone git@gitlab.dkrz.de:icon/icon.git
    cd icon
    git checkout icon-2.6.6
    sed -e 's?ecmwf-toolbox/2021.12.0.0?ecmwf-toolbox/2023.10.1.0?g' \
	config/ecmwf/atos2020.intel-2021.4 > config/ecmwf/ileps
    sh -c config/ecmwf/ileps
    make -j 4
    cp -p bin/icon $TCWORK/bin
)

# build icontools code (update ecmwf-toolbox)
(
    cd $TCWORK/src
    git clone git@gitlab.dkrz.de:dwd-sw/dwd_icon_tools.git
    cd dwd_icon_tools
    git checkout icontools-2.6.0
    sed -e 's?ecmwf-toolbox/2021......0?ecmwf-toolbox/2023.10.1.0?g' \
	do_configure.sh > ileps.sh
    sh -c ileps.sh
    make -j 4
    for file in icondelaunay icongpi icongridgen iconremap iconsub; do
	cp -p icontools/$file $TCWORK/bin
    done
)

# download container for SIMC tools
(
    module load singularity
    cd $TCWORK/bin
    singularity pull library://dcesari/default/simctools:simc_tools_r8
)

# copy constant data from icon sources to const dir
rsync -a $TCWORK/src/icon/externals/ecrad/data $TCWORK/const/ecrad
cp -p $TCWORK/src/icon/ECHAM6_CldOptProps.nc $TCWORK/src/icon/rrtmg_lw.nc $TCWORK/const

# clone suite source files
#cd $HOME
#git clone https://github.com/ARPA-SIMC/ec-tc.git

# sync to other storage host
$ECTC_BASE/bin/sthost_sync.sh
