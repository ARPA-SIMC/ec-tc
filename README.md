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

At the moment the suite can already run all the jobs from the beginning up to and included CLEPS data
retrieval on the ECMWF system. The cluster analysis works as well. The procedure is the following:

```
cd ecflow/ileps_timecrit
# setup environment and modules
. bin/start-atos
# create and load the suite (answer `y` to the questions)
./ectc.py
# start ecflow_ui and play with the suite
ecflow_ui
```



