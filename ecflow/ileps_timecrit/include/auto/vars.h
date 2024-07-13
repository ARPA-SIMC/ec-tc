# enable autoexport
set -a
ECTC_BASE=%ECTC_BASE%
ECTC_CONF=%ECTC_CONF%
ECTC_WORKBASE=%ECTC_WORKBASE%
ECTC_WORK=%ECTC_WORK%

DATE=%YMD:%
TIME=%TIME:00%00
RETRIEVE_START=%RETRIEVE_START:%
RETRIEVE_STOP=%RETRIEVE_STOP:%
EC_DISS=%EC_DISS%

# a non-null $DISPLAY (from ecflow env) may interfere with some graphical processes
# hopefully nobody needs it intentionally
unset DISPLAY

# set ensemble member from ecflow if available
if [ -z "$ENS_MEMB" ]; then
    ENS_MEMB=%ECTC_ENS_MEMB:0%
fi
#if [ "$ENS_MEMB" = 0 ]; then
#    unset ENS_MEMB
#fi
# >0 ensemble member
# 0 deterministic
# -1 control
# -2 loop on all members from 0 to $ENS_TOTAL_MEMB

ECTC_CONFDIRLIST="$ECTC_CONF $ECTC_CONF/%SUITE%"
for dir in $ECTC_CONFDIRLIST; do
  . $dir/runconf.sh
done
set +a
