# ec-tc
Ecflow scripts for a time critical suite on ECMWF HPC systems.

This repository contains the python script for defining the new
ICON-LEPS time-critical EcFlow suite at ECMWF and a draft of the
scripts defining the suite jobs.

The prototype suite can be tested on a generic system with ecflow and
ecflow-python installed, with the following steps:

```
cd ecflow/ileps_timecrit
# setup environment and start an ecflow server on port 3140
. bin/start-local
# create and load the suite
./ectc.py
# start ecflow_ui and play with the suite
ecflow_ui
# stop the server and restore the environment
. bin/stop-local
```




