# Path of constant files for verification
GRID_CONST_DIR=$CONST_DIR/icon-leps-2.5km
MODEL_GRID=$GRID_CONST_DIR/ileps_2.5km_DOM01.nc
MODEL_INVAR=$GRID_CONST_DIR/ileps_2.5km_static_unstr.grb
GEOID_GRID=$GRID_CONST_DIR/ww15mgh.grd

#sub# MODEL_GRID=/ec/res4/scratch/mck/tmp/run/ileps_2.5kmsub_DOM01.nc
#sub# MODEL_INVAR=/ec/res4/scratch/mck/tmp/run/leps_2.5kmsub_static_unstr.grb

# List of Ileps starting hours
#hh_run_list="00 12"
hh_run_list="00"

# Lenght of Ileps runs
run_len=120

# Time (days) allowd to cellect late observations (1 = process yesterday observations; for Carmens, use at least 4)
obs_delay=5

# Lenght (minutes) of the time interval for which MARS observations are extracted 
# Usually "60" (ie. extract observations for the previuos hour), could be "180" (3 hours - NOT TESTED)
mars_time_window=60

# First and last available Icon refernce times (to limit timeranges search to available forecasts)
# rt_db_first=2025061418
# rt_db_last=2025081318
rt_db_first=2025111618
rt_db_last=2025123018

# Number of ensemble members to be processed
nens=20
#nens=2

# Frequency of mec instances: this is the frequncy of ouptut "feedback files", and it is usullay equal
# to the frequency of observations (1h for MARS, 3h for Carmens). Check "obssrc" in suiteconf.toml!
mec_stride=3

# Shift of Icon reference time (hours; can't be <0)
# Used to process forecasts "as if" they had a later reference time
# To process 18Z forecasts as if they were 00Z forecasts: set rt_shift=6, hh_run_list=00, run_len=120
rt_shift=0

# List of non-instanteous varibles to be processed (shortNames, eccodes definitions defined in mod_mec.h)
statProc_vars="TOT_PREC VMAX_10M ASWDIR_S"

# List of "lengthOfTimeRange" to process for non-instanteous varibles (hours)
# Observations are assumed to be defined at the end of the processing interval.
# Data from previous forecast ranges are needed to evaluate the corresponding model values: 
#   they will be appended to model outputs at verification time. 
statProc_ranges="1 3 6 12"
