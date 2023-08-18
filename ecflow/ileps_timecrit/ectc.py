#!/usr/bin/python

import ecflow
import math
import os
import datetime

# some utility routines:

# from https://rosettacode.org/wiki/Range_expansion%23Python&sa=D&ust=1517307153586000&usg=AFQjCNHTzzU1Kk2XfryWWIoWwKTWNWItiw
def rangeexpand(txt):
    lst = []
    for r in txt.split(','):
        if '-' in r[1:]:
            r0, r1 = r[1:].split('-', 1)
            lst += range(int(r[0] + r0), int(r1) + 1)
        else:
            lst.append(int(r))
    return lst

# incremental or for trigger syntax
def expr_or(expr, orexpr):
    if expr == "":
        return orexpr
    elif orexpr == "":
        return expr
    else:
        return expr+" || "+orexpr


def ask_confirm(msg=""):
    ans = input(msg+" (y/n)? ")
    return ans.startswith("y")


def daily_cron(step):
    if step > 60 or step < 0:
        return None
    time_series = ecflow.TimeSeries(ecflow.TimeSlot(0, 0),
                                    ecflow.TimeSlot(23, 60-step),
                                    ecflow.TimeSlot(0,step), False)
    cron = ecflow.Cron()
    cron.set_time_series(time_series)
    return cron


def cron_set_time(time):
    cron = ecflow.Cron()
    cron.set_time_series(time)
    return cron


# convert a member number to a member name for naming families
def membname(eps_memb, memb_pref=""):
    if eps_memb == -1:
        return "control" # control
    elif eps_memb == 0:
        return "deterministic" # deterministic
    else:
        return f"{memb_pref}{eps_memb}"


# class for creating and managing a suite (check, write to a def file,
# update on ecflow server)
class TcSuite():
    def __init__(self, name):
        self.name = name
        self.defs = ecflow.Defs()
        self.suite = self.defs.add_suite(name)
        self.checked = False


    def check(self):
        # check syntax
        result = self.defs.check()
        if result != "":
            print("Error in "+self.name+" suite definition:")
            print(result)
        else:
            # check job tree
            result = self.defs.check_job_creation()
            if result != "":
                print("Error in "+self.name+" suite job creation:")
                print(result)
            else: 
                self.checked = True


    def write(self, interactive=True):
        if not self.checked:
            print("suite "+self.name+" has not been checked, refusing to write")
            return
        name = self.name+".def"
        if interactive:
            if os.path.exists(name):
                if not ask_confirm("Definition file "+name+" exists, replace"):
                    return
        self.defs.save_as_defs(name)
        print("Suite saved in "+name)


    def replace(self, interactive=True):
        if not self.checked:
            print("suite "+self.name+" has not been checked, refusing to replace")
            return
        if interactive:
            if not ask_confirm("Replace suite "+self.name+" on server"):
                return
        client = ecflow.Client() # connect using environment
        client.replace("/"+self.name, self.defs)
        print("Suite "+self.name+" replaced on server")


# base class for adding a family to a suite node, for not repeating
# common code
class TcFamily:
    def __init__(self, conf={}):
        self.conf = {}
        self.conf.update(conf)


# Add the root family for looping on days and hours (loop on hours not
# implemented yet)
class TcSuiteTime(TcFamily):
    def add_to(self, node):
        day = node.add_family("day").add_repeat(
            ecflow.RepeatDate("YMD",
                              int((datetime.datetime.now()-datetime.timedelta(days=self.conf["deltaday"])).strftime("%Y%m%d")),
                              int((datetime.datetime.now()+datetime.timedelta(days=5000)).strftime("%Y%m%d"))))
        return day # day is the new root of the suite. it must be returned


# Add a family for starting suite 
class TcStartSuite(TcFamily):
    def add_to(self, node):
        for sub in self.conf["subsuites"]: # ana, eps, det, 
            fam = node.add_family("start_suite_"+sub)
            fam.add_task("startileps_tc_"+sub)
            fam.add_variable("ECF_DUMMY_TASK", "Y")


# Add a pre family (data retrieval for cluster analysis, cluster
# analysis, data retrieval for each ensemble member), data retrieval
# can be in different flavours (mars, dissemination...)
class TcPre(TcFamily):
    def add_to(self, node):
        for pre in self.conf["pretypes"]: # mars, diss
            fam = node.add_family(f"pre_{pre}")
            fam.add_late(ecflow.Late(active="09:30", complete="11:59")) # shift 00 or 12?
            if self.conf["predefault"] != pre: fam.add_defstatus(ecflow.Defstatus("complete"))
            fam.add_trigger("./start_suite_eps == complete")
            fam.add_task(f"get_pl_{pre}")
            fam.add_task("cluster_analysis").add_trigger(f"./get_pl_{pre} == complete")
            # configure task should be deleted and its tasks distributed
            fam.add_task("configure").add_trigger("./cluster_analysis == complete")
            get = fam.add_family("get_ml") # inlimit /ileps_timecrit:get_ml_limit
            for eps_memb in self.conf["membrange"]:
                fname = membname(eps_memb, "eps_member_")
                if eps_memb > 0: # eps membs depend on cluster analysis, det does not
                    trig = "../configure == complete" # rather "../cluster_analysis == complete"
                elif eps_memb == 0:
                    trig = "../../start_suite_det == complete"
                memfam = get.add_family(fname).add_variable("ECF_ENS_MEMB", str(eps_memb)).add_trigger(trig)
                if self.conf.get("splitretrieve", None) is not None:
                    # add an analysis family
                    memfam.add_family(f"retrieve_ana").add_task(f"retrieve_ec_bc_day_{pre}").add_variable("RETRIEVE_START", "0").add_variable("RETRIEVE_STOP", "0")
                    # add a family for each forecast day
                    nh = self.conf["forecastrange"]
                    for d in range(math.ceil(nh/24)):
                        memfam.add_family(f"retrieve_day_{d}").add_task(f"retrieve_ec_bc_day_{pre}").add_variable("RETRIEVE_START", f"{d*24}").add_variable("RETRIEVE_STOP", f"{min((d+1)*24, nh)}")
                else:
                    memfam.add_task(f"retrieve_ec_bc_{pre}")


# Add a run family for retrieving (DWD) icon soil
class TcIconSoil(TcFamily):
    def add_to(self, node):
        fam = node.add_family("iconsoil")
        if self.conf.get("iconsoil", None) is None:
            fam.add_defstatus(ecflow.Defstatus("complete"))
        fam.add_trigger("./start_suite_ana == complete")
        fam.add_task("setup_iconsoil")
        fam.add_task("get_iconsoil").add_trigger("./setup_iconsoil == complete")
        fam.add_task("iconsoil_to_leps").add_trigger("./get_iconsoil == complete")

# Add a run family for every ensemble member
class TcRun(TcFamily):
    def add_to(self, node):
        fam = node.add_family("iconrun") # was "lokal", task setup_lokal_det deleted!
        for eps_memb in self.conf["membrange"]:
            fname = membname(eps_memb, "eps_member_")
            run = fam.add_family(fname)
            run.add_variable("ECF_ENS_MEMB", str(eps_memb))
            trig = ""
            for pre in self.conf["pretypes"]: # mars, diss
                trig = expr_or(trig, f"../../pre_{pre}/get_ml/{fname} == complete")
            run.add_task("remap").add_trigger(trig)
            run.add_task("icon").add_trigger("./icon == complete && ../../iconsoil == complete")


# Add a family for regribbing for cluster analysis and writing to fdb for each member
class TcRegribFdb(TcFamily):
    def add_to(self, node):
        fam = node.add_family("regrib_and_fdb") # task regrib_setup deleted!
        # add clst_info family
        for eps_memb in self.conf["membrange"]:
            fname = membname(eps_memb, "eps_member_")
            raf = fam.add_family(fname)
            raf.add_variable("ECF_ENS_MEMB", str(eps_memb))
            raf.add_trigger(f"../iconrun/{fname} == complete")
            # copio tutto bovinamente, comprese le asimmetrie dei trigger
            raf.add_task("get_dataoutput") # ha senso spostare i dati?
            raf.add_task("mlev_regrib") # non fa quasi niente
            raf.add_task("plev_regrib").add_trigger("get_dataoutput == complete")
            raf.add_task("surf_regrib").add_trigger("get_dataoutput == complete")
            raf.add_task("plev_to_fdb").add_trigger("plev_regrib == complete")
            raf.add_task("surf_to_fdb").add_trigger("surf_regrib == complete")


if __name__ == '__main__':
    interactive = True
    # vars and configuration management to be improved    
    ecf_vars={"SCHOST": "hpc-login",
              "ECF_FILES": os.path.join(os.getcwd(), "ecffiles"),
              "ECF_INCLUDE": os.path.join(os.getcwd(), "include"),
              "ECF_HOME": os.path.join(os.getcwd(), "work"),
              "ECF_STATUS_CMD": "/opt/troika/bin/troika monitor %SCHOST% %ECF_JOB%",
              "ECF_KILL_CMD": "/opt/troika/bin/troika kill %SCHOST% %ECF_JOB%",
              "ECF_JOB_CMD": "/opt/troika/bin/troika submit -o %ECF_JOBOUT% %SCHOST% %ECF_JOB%",
              "ECF_TRIES": "2",
    }
    conf={"deltaday": 1,
          "hours": range(0, 24, 12), # (0,24,24) to run only at 00
          "forecastrange": 132,
          "subsuites": ("ana", "eps", "det"),
          "pretypes": ("mars", "diss"),
          "predefault": "mars",
          "splitretrieve": "Y",
          "iconsoil": None,
          "membrange": range(5)}

    # create a suite, ileps.suite will be the root node of the suite
    ileps = TcSuite("ileps_timecrit")
    # add configure ECF vars, this should be moved to a special subroutine
    for var in ecf_vars:
        ileps.suite.add_variable(var, ecf_vars[var])
    # add timing loop
    newroot = TcSuiteTime(conf).add_to(ileps.suite)
    # add the rest of the suite elements to the new root of the suite
    TcStartSuite(conf).add_to(newroot) # short for f=TcStartSuite(conf); f.add_to(newroot)
    TcIconSoil(conf).add_to(newroot)
    TcPre(conf).add_to(newroot)
    TcRun(conf).add_to(newroot)
    TcRegribFdb(conf).add_to(newroot)
    # check the suite (syntax, triggers, jobs)
    ileps.check()
    #ileps.checked=True
    # write suite to a .def file
    ileps.write(interactive=interactive)
    # upload/replace on server (with confirmation if interactive)
    #ileps.replace(interactive=interactive)
