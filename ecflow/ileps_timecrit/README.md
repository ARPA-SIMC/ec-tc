## ileps timecritical suite

This directory of the package contains all the necessary, except big
static data and model sources, for setting up and running the
ICON-LEPS time-critical suite(s) on Atos.

 * `src/` source files for cluster analysis procedure
 * `bin/` utility scripts
 * `conf/` suites' configuration
 * `jobs/` suite jobs
 * `include/` job include files

Additionally the following directories are created and/or populated
runtime:

 * `ecflow/` jobs and logs
 * `const/` link to the constant data directory on timecritical storage.

### Creating the suites' definitions

The `ectc.py` script has to be run in order to create the suites
definitions and upload them to the user ecflow server. The script can
create different suites according to the configurations contained in
the subdirectories of `conf/` directory, still following the general
ileps suite structure. The script has to be prefixed by `bin/ec_wrap`
command in order to set up the environment and suffixed by the suite
name (a directory under `conf/`, see next section) as first argument:

```
bin/ec_wrap ./ectc.py ileps_tc_7km
```

According to the current configuration (see next sections), the only
environmental vriables which have to be set at the time of generating
the suite are `$ECTC_BASE`, pointing to the `ileps_timecrit` directory
of this package and set by the user's profile, and `$TCWORK`, pointing
to the time critical filesystem to be used and set by system login
scripts.

### Suite configuration

Every suite has its own configuration in the `conf` and
`conf/<suitename>` directory. The configuration consists of a suite
configuration file, runtime configuration file(s) and template files.

#### suite configuration file

Its name is `suiteconf.toml`, there is only one such file per suite in
`conf/<suitename>`, it is a simple text file in
[toml](https://toml.io/en/) format, a variant of the so-called "ini
file" format.

It has two sections, the first called `ecfvars` containing the ecflow
variables to be included in the suite at suite level, the second
`suiteconf` containing the configuration of the suite as expected by
`ectc.py`.

The `ecfvars` section can contain environmental variables with a
shell-like symtax (e.g. `$HOME` or `${HOME}`) which will be expanded
at suite generation time, this is not a feature of toml-format, it is
implemented in `ectc.py` using python library function
`os.path.expandvars`.  Here is an example of the `ecfvars` section:
```
[ecfvars]

SCHOST = "hpc"
WSHOST = "hpc"
STHOST = "ws1"
STHOST_BKUP = "ws2"
ECF_FILES = "$ECTC_BASE/jobs"
ECF_INCLUDE = "$ECTC_BASE/include"
ECF_HOME = "$ECTC_BASE/ecflow"
ECF_STATUS_CMD = "STHOST=%STHOST% troika monitor %SCHOST% %ECF_JOB%"
ECF_KILL_CMD = "STHOST=%STHOST% troika kill %SCHOST% %ECF_JOB%"
ECF_JOB_CMD = "STHOST=%STHOST% troika submit -o %ECF_JOBOUT% %SCHOST% %ECF_JOB%"
ECF_TRIES = "2"
ECTC_BASE = "$ECTC_BASE" # from user profile, repeated here because it could be lost in the suite env
ECTC_ENS_MEMB = "0"
ECTC_CONF = "$ECTC_BASE/conf" # shell conf files to be sourced
ECTC_WORK = "$TCWORK/work/%SUITE%" # "/ec/%STHOST%/tc/$USER/tcwork/%SUITE%" ?
```

Notice that these are not real environmental variables but internal
ecflow server variables. They can be used in the ecflow `.ecf` scripts
using the `%<var>%` syntax and its variants, or converted to
shell environmental variables individually as it is done in the
[`vars.h`](https://github.com/ARPA-SIMC/ec-tc/blob/e6ce374a3ba590dc35c8fa739db0e3af9db2c237/ecflow/ileps_timecrit/include/auto/vars.h#L3)
script header file.

The section `suiteconf` looks like:
```
[suiteconf]

# suitestart = 2024-01-01 00:00:00Z
suiteback = 5
suitestop = 2035-01-01 00:00:00Z
# suiteduration = 365
hours = "0-12:12" # "0" to run only at 00
forecastrange = 132
subsuites = ["ana", "eps", "det"]
pretypes = ["mars", "diss"]
predefault = "mars"
splitretrieve = true
membrange = "0-4"
timecrit = true
```

The list of supported variables may change. This is the explanation of
nost of them:

 * `suitestart` (datetime) first day of suite cycling
 * `suiteback` (int) alternative to the previous, how many days before today the suite should start cycling
 * `suitestop` (datetime) last day of suite cycling
 * `suiteduration` (int) alternative to the previous, for how many days the suite should cycle
 * `hours` (range string in format `1,3,6-9,10-20:2`) hours of day (UTC) at which suite should run, e.g. `0-21:3` means every 3 hours from 0 to 21 UTC
 * `forecastrange` (int) maximum forecast range for leps run in hours
 * `subsuites` (array of strings) pre-configured types of model runs to be added to the suite, possible values "ana", "eps", "det"
 * `iconsoil` to be included in subsuites?
 * `pretypes` (array of strings) pre-configured types of data access for the suite, possible values "mars", "diss"
 * `predefault` default data access method among `pretypes`, the other one(s) will be set to "defstatus complete" in the suite definition
 * `splitretrieve` (boolean) retrieve BC with a task for each 24 hour interval (default, true) or with a single task (false)
 * `membrange` (range string) range of ensemble members to be run (0=deterministic)
 * `timecrit` (boolean) the suite is realtime/timecritical, thus it has an "emergency" family and it is started by real-time dissemination events (true) or it is a continuous reforecast non realtime suite (default, false).

#### Runtime configuration files

These are pure bash shell scripts files, called `runconf.sh`, one is
in the `conf/` directory, and one is in each suite-specific
configuration subdirectory. They are sourced by each suite job in the
order (`conf/runconf.sh`, `conf/<suitename>/runconf.sh`), so the first
holds common settings, while the second ones hold suite-specific
settings. In principle they can contain any bash command, but it is
recommended to limit them to environmental variable assignment and
utility functions. They are sourced with `set -a`, so every assigned
variable is implicitly exported without the need to use `export`
command.

#### Text file templates

The configuration directories can also contain templates, with the
suffix `.in`, for creating runtime configuration files
(e.g. namelists); these files can be used in the suite jobs with the command
`ac_templater <filename>` in order to create in the current directory a
file based on a suite-specific template `<filename>.in`, where
sequences like `@VAR@` are replaced by the corresponding environmental
variable `$VAR`, and `@INCLUDE <includefile>@` are replaced by the
contents of file `<includefile>` searched for in the suite
configuration tree.

These files are first searched in the generic configuration directory
and then in the suite-specific directory, the last one found is
used. In case of ensemble runs (`$ENS_MEMB` > 0), a file with the
`.in.ens` or a file with the `.in.$ENS_MEMB` suffix, if found, is used
instead of the file with `.in` extension.

### Runtime directories

Additionally the following directories are used and/or populated
runtime:

 * `$ECTC/ecflow/` jobs and logs
 * `$ECTC_WORKBASE/const` constant data directory on timecritical storage
 * `$ECTC_WORKBASE/bin` executable directory on timecritical storage.
