set -e          # stop the shell on first error
#set -u          # fail when using an undefined variable
set -o pipefail # fail if last(rightmost) command exits with a non-zero status
 
# Defines the variables that are needed for any communication with ECF
export ECF_PORT=%ECF_PORT%    # The server port number
export ECF_HOST=%ECF_HOST%    # The host name where the server is running
export ECF_NAME=%ECF_NAME%    # The name of this current task
export ECF_PASS=%ECF_PASS%    # A unique password
export ECF_TRYNO=%ECF_TRYNO%  # Current try number of the task
export ECF_DENIED=%ECF_DENIED:% # Optional, if set, ecflow_client exits when connection with server fails

# record the process id. Also used for zombie detection
if [ -n "$PBS_JOBID" ]; then
    export ECF_RID=${PBS_JOBID%%.*}
elif [ -n "$SLURM_JOB_ID" ]; then
    export ECF_RID=$SLURM_JOB_ID
else
    export ECF_RID=$$
fi

# Define error and exit handlers
ERROR() {
    set +e                      # Clear -e flag, so we don't fail
    wait                        # wait for background process to stop
    if [ "%NO_FAIL:%" = "TRUE" ]; then
	ecflow_client --msg="Forgiving failure of %ECF_NAME%"
	ecflow_client --complete   # Notify ecFlow of a normal end
    else
	ecflow_client --abort="trap" # Notify ecFlow that something went wrong
    fi
    trap - EXIT ERR             # Remove the trap
    exit 0                      # End the script, was exit 1, set to 0 to avoid double failure of interactive jobs
}

CLEANEXIT() {
    [ "$?" = "0" ] || ERROR   # required for `exit 1` to call ERROR function
    wait                      # wait for background process to stop
    ecflow_client --complete # Notify ecFlow of a normal end
    trap - EXIT               # Remove all traps
    exit 0                    # End the script
}

# Trap any signal that may cause the script to fail
trap '{ CLEANEXIT ; }' EXIT
trap '{ echo "Exiting with error"; ERROR ; }' ERR
trap '{ echo "Killed by a signal"; ERROR ; }' 1 2 3 4 5 6 7 8 10 11 13 24

[ "$(hostname)" != ${ECF_HOST} ] && module load ecflow/%ECF_VERSION%

# Tell ecFlow we have started
ecflow_client --init=$ECF_RID
