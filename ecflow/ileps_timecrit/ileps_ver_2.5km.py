#!/usr/bin/env python3

# Version 4.0

import ecflow
import math
import os, sys
import datetime
import tomllib

suitename="ileps_ver_2.5km"
defs = ecflow.Defs()
suite = defs.add_suite(suitename)


# Get ecflow variables from .toml configuration file

script_dir = os.path.dirname(os.path.abspath(__file__))
os.environ["ECTC_BASE"] = script_dir

with open(os.path.join(script_dir, "conf", suitename, "suiteconf.toml"), "rb") as fd:
    conf = tomllib.load(fd)

for var in conf["ecfvars"]:
    varx = os.path.expandvars(conf["ecfvars"][var])
    suite.add_variable(var, varx)

mode=conf["suiteconf"]["mode"]
obssrc=conf["suiteconf"]["obssrc"]
suite.add_variable("mode",mode)
suite.add_variable("obssrc",obssrc)

# Build the verifcation suite: "cron" mode
if mode == "cron":
    famname = "day"
    fam = suite.add_family(famname)
    fam.add_variable("YMDf","nil")
    fam.add_variable("stride",1)
    fam.add_variable("block",0)

    hhver=int(conf["suiteconf"]["hhver"])
    cron = ecflow.Cron(("%02d" % hhver) + ':30')
    fam.add_cron(cron)

    fam.add_task("v_start").add_meter("YMDc",1,25000000)
    tsk = fam.add_task("v_get_obs_mars").add_trigger("v_start == complete")
    if obssrc != "mars" : tsk.add_defstatus(ecflow.Defstatus("complete"))
    tsk = fam.add_task("v_get_obs_carm").add_trigger("v_start == complete")
    if obssrc != "carm" : tsk.add_defstatus(ecflow.Defstatus("complete"))
    fam.add_task("v_mec").add_trigger("v_get_obs_mars == complete and v_get_obs_carm == complete")
    fam.add_task("v_arc").add_trigger("v_mec == complete")

elif mode == "free":
    famname = "day"
    fam = suite.add_family(famname)
    fam.add_variable("YMDc","nil")

    start = conf["suiteconf"]["suitestart"]
    stop = conf["suiteconf"]["suitestop"]
    stride = int(conf["suiteconf"]["stride"])
    fam.add_repeat(
            ecflow.RepeatDate("YMDf", int(start.strftime("%Y%m%d")), int(stop.strftime("%Y%m%d")), stride ))
    fam.add_variable("stride",stride)
  
    for block in range(0,stride):
        famname = "block_" + str(block)
        blk = fam.add_family(famname)
        blk.add_variable("block",block)
        blk.add_task("v_start").add_defstatus(ecflow.Defstatus("complete"))
 
        tsk = blk.add_task("v_get_obs_mars").add_trigger("v_start == complete")
        if obssrc != "mars" : tsk.add_defstatus(ecflow.Defstatus("complete"))
        tsk = blk.add_task("v_get_obs_carm").add_trigger("v_start == complete")
        if obssrc != "carm" : tsk.add_defstatus(ecflow.Defstatus("complete"))

        blk.add_task("v_mec").add_trigger("v_get_obs_mars == complete and v_get_obs_carm == complete")
        blk.add_task("v_arc").add_trigger("v_mec == complete")

# Write suite to a .def file
defs.save_as_defs("ileps_ver_2.5km.def")

# Update the suite on ecflow server
client = ecflow.Client() # connect using environment
client.replace("/ileps_ver_2.5km", defs)
print("Suite ileps_ver replaced on server")

