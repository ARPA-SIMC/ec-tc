GRID_CONST_DIR=$CONST_DIR/icon-leps-7km
MODEL_GRID=$GRID_CONST_DIR/ileps_7km_DOM01.nc
MODEL_GRID_PARENT=$GRID_CONST_DIR/ileps_7km_DOM01.parent.nc
MODEL_GRID_EXTERNAL=$GRID_CONST_DIR/external_parameter_icon_ileps_7km_DOM01_tiles.nc
MODEL_FCAST_GP=125
TIME_STEP=60
if [ "$TRYNO" -gt 1 ]; then
  TIME_STEP=$(($TIME_STEP/2))
fi

