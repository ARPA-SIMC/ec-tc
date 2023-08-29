
export ECTC_CONF=%ECTC_CONF%
export ECTC_WORK=%ECTC_WORK%

export DATE=%YMD:%
export TIME=%TIME:00%00
export RETRIEVE_START=%RETRIEVE_START:%
export RETRIEVE_STOP=%RETRIEVE_STOP:%

# a non-null $DISPLAY (from ecflow env) may interfere with some graphical processes
# hopefully nobody needs it intentionally
unset DISPLAY

# set ensemble member from ecflow if available
if [ -z "$ENS_MEMB" ]; then
    export ENS_MEMB=%ECTC_ENS_MEMB:0%
fi
#if [ "$ENS_MEMB" = 0 ]; then
#    unset ENS_MEMB
#fi
# >0 ensemble member
# 0 deterministic
# -1 control
# -2 loop on all members from 0 to $ENS_TOTAL_MEMB

# source conf with autoexport
set -a
. $ECTC_CONF/%SUITE%
set +a
