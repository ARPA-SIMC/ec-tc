

export ECF_PORT=%ECF_PORT%    # The server port number
export ECF_HOST=%ECF_HOST%    # The host name where the server is running
export ECF_NAME=%ECF_NAME%    # The name of this current task
export ECF_PASS=%ECF_PASS%    # A unique password
export ECF_TRYNO=%ECF_TRYNO%  # Current try number of the task
export ECF_DENIED=%ECF_DENIED:% # Optional, if set, ecflow_client exits when connection with server fails

export ECTC_CONF=%ECTC_CONF%
export ECTC_WORK=%ECTC_WORK%

export DATE=%YMD:%
export TIME=%TIME:00%
export RETRIEVE_START=%RETRIEVE_START:%
export RETRIEVE_STOP=%RETRIEVE_STOP:%

# set ensemble member from ecflow if available
if [ -z "$ENS_MEMB" ]; then
    export ENS_MEMB=%ECF_ENS_MEMB:0%
fi
#if [ "$ENS_MEMB" = 0 ]; then
#    unset ENS_MEMB
#fi
# >0 ensemble member
# 0 deterministic
# -1 control
# -2 loop on all members from 0 to $ENS_TOTAL_MEMB

# source conf
. $ECTC_CONF/%SUITE%
