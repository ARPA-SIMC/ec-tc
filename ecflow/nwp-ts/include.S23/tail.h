wait

ecflow_client --complete    # Notify ecFlow of a normal end

# On the Cray HPC, remove the link to the PBS running output
if [[ $HOST = @(cc*) ]]; then
  [[ -L $_running_output ]] && rm -f $_running_output
fi

trap 0                 # Remove all traps
exit 0                 # End the shell
