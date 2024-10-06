# ec-tc
Ecflow scripts for suites on ECMWF HPC systems.

This repository presently contains the software for two independent EcFlow suites:
- the new ICON-LEPS time-critical suite (ileps)
- the Icon offline test suite (nwp-ts)
It includes the python scripts for defining the suite, a part of the suite jobs and
configuration files for suite definition and suite runtime.

At the moment, the ileps suite can already run all the jobs from the
beginning up to ICON LEPS ensemble run, including data retireval and
cluster analysis.

More detailed information can be found in the specific
[README](ecflow/ileps_timecrit/README.md) in the ileps_timecrit
directory.

The suite generation and the job configuration systems are based on
[nwpconf](https://github.com/ARPA-SIMC/nwpconf) and
[nwprun](https://github.com/ARPA-SIMC/nwprun) packages, but in a
simplified and incompatible way.
