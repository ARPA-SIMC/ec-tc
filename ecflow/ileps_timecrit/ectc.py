#!/usr/bin/env python3

import ecflow
import math
import os, sys
import datetime
import tomllib

# some utility routines:

# from https://rosettacode.org/wiki/Range_expansion%23Python&sa=D&ust=1517307153586000&usg=AFQjCNHTzzU1Kk2XfryWWIoWwKTWNWItiw
def rangeexpand(txt):
    lst = []
    for r in txt.split(','):
        if '-' in r[1:]:
            step = 1
            if ':' in r:
                r, step = r.split(':')
            r0, r1 = r[1:].split('-', 1)
            lst += range(int(r[0] + r0), int(r1) + 1, int(step))
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

# incremental and for trigger syntax
def expr_and(expr, andexpr):
    if expr == "":
        return andexpr
    elif andexpr == "":
        return expr
    else:
        return expr+" && "+andexpr


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


    def addlimit(self, name, size, nodere):
        import re
        noderec = re.compile(nodere)
#        for suite in self.defs.suites:
        self.suite.add_limit(name, size) # add limit at suite root level
        self.__loopnodes(self.suite, name, noderec)


    def __loopnodes(self, suite, name, noderec):
        for node in suite.nodes:
            if isinstance(node, ecflow.Task): # task, stop here
                if noderec.search(node.name()):
                    node.add_inlimit(name, "", 1)
            else: # family, go deeper
                if noderec.search(node.name()):
                    node.add_inlimit(name, "", 1)
                else: # do not go deeper if the limit applies to the family
                    self.__loopnodes(node, name, noderec)


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


# Add the basic ECF_ variables (not really a family)
class TcEcfVars(TcFamily):
    def add_to(self, node):
        for var in conf["ecfvars"]:
            node.add_variable(var, conf["ecfvars"][var])


# Add the root family for looping on days and hours
class TcSuiteTime(TcFamily):
    def add_to(self, node):
        hour = []
        if self.conf.get("suitestart", None) is None:
            start = datetime.datetime.now() - datetime.timedelta(days=self.conf.get("suiteback", 5))
        else:
            start = self.conf["suitestart"]
        if self.conf.get("suitestop", None) is None:
            stop = start + datetime.timedelta(days=self.conf.get("suiteduration", 365))
        else:
            stop = self.conf["suitestop"]

        day = node.add_family("day").add_repeat(
            ecflow.RepeatDate("YMD", int(start.strftime("%Y%m%d")), int(stop.strftime("%Y%m%d"))))
        for h in rangeexpand(self.conf["hours"]):
            famname = "hour_" + ("%02d" % h)
            hour.append(day.add_family(famname).add_variable("TIME", "%02d" % h))
        return hour # further on the user has to loop on hour for filling the suite

    # return an updated conf dictionary including hour of current node
    def updatedconf(self, node):
        if node.name().startswith("hour_"):
            hour = int(node.name().removeprefix("hour_").removeprefix("0"))
            newconf = self.conf.copy()
            newconf["time"] = hour
            return newconf

# Add an inactive family for emergency tasks
class TcEmergency(TcFamily):
    def add_to(self, node):
        fam = node.add_family("emergency")
        fam.add_defstatus(ecflow.Defstatus("complete"))
        fam.add_task("emergency_start_hpcf")
        fam.add_task("switch_STHOST").add_label("STHOST", "")


# Add a family for starting suite 
class TcStartSuite(TcFamily):
    def add_to(self, node):
        for sub in self.conf["subsuites"]: # ana, eps, det, 
            fam = node.add_family("start_suite_"+sub)
            fam.add_task("startileps_tc_"+sub)
            fam.add_variable("ECF_DUMMY_TASK", "Y")
            if not self.conf.get("timecrit", False):
                fam.add_defstatus(ecflow.Defstatus("complete"))



# Add a pre family (data retrieval for cluster analysis, cluster
# analysis, data retrieval for each ensemble member), data retrieval
# can be in different flavours (mars, dissemination...)
class TcPre(TcFamily):
    def add_to(self, node):
        for pre in self.conf["pretypes"]: # mars, diss
            fam = node.add_family(f"pre_{pre}")
            ddt = datetime.datetime(1970, 1, 1, self.conf["time"])
            ltact = (ddt + datetime.timedelta(hours=9, minutes=30)).time().isoformat(timespec="minutes")
            lcmpl = (ddt + datetime.timedelta(hours=11, minutes=59)).time().isoformat(timespec="minutes")
            fam.add_late(ecflow.Late(active=ltact, complete=lcmpl))
            if self.conf["predefault"] != pre: fam.add_defstatus(ecflow.Defstatus("complete"))
            if "eps" in self.conf["subsuites"]: # homogeneize condition, either subsuite "eps" or membrange
                fam.add_task(f"retrieve_cla_pl_{pre}").add_trigger("../start_suite_eps == complete")
                fam.add_task("cluster_analysis").add_trigger(f"./retrieve_cla_pl_{pre} == complete")
            get = fam.add_family("retrieve_ic_bc") # inlimit /ileps_timecrit:get_ml_limit
            for eps_memb in rangeexpand(self.conf["membrange"]):
                fname = membname(eps_memb, "eps_member_")
                if eps_memb > 0: # eps membs depend on cluster analysis, det does not
                    trig = "../cluster_analysis == complete" # rather "../cluster_analysis == complete"
                elif eps_memb == 0:
                    trig = "../../start_suite_det == complete"
                memfam = get.add_family(fname).add_variable("ECTC_ENS_MEMB", str(eps_memb)).add_trigger(trig)
                if self.conf.get("splitretrieve", False):
                    memfam.add_task("setup_retrieve")
                    # add an analysis family
                    memfam.add_family(f"retrieve_ana").add_task(f"retrieve_ic_bc_day_{pre}").add_variable("RETRIEVE_START", "0").add_variable("RETRIEVE_STOP", "0").add_trigger("../setup_retrieve == complete")
                    # add a family for each forecast day
                    nh = self.conf["forecastrange"]
                    for d in range(math.ceil(nh/24)):
                        memfam.add_family(f"retrieve_day_{d}").add_task(f"retrieve_ic_bc_day_{pre}").add_variable("RETRIEVE_START", f"{d*24}").add_variable("RETRIEVE_STOP", f"{min((d+1)*24, nh)}").add_trigger("../setup_retrieve == complete")
                else:
                    memfam.add_task(f"retrieve_ic_bc_{pre}")


# Add a run family for retrieving (DWD) icon soil
class TcIconSoil(TcFamily):
    def add_to(self, node):
        fam = node.add_family("iconsoil")
        if self.conf.get("iconsoil", None) is None:
            fam.add_defstatus(ecflow.Defstatus("complete"))
        if "ana" in self.conf["subsuites"]: # is this trigger necessary?
            fam.add_trigger("./start_suite_ana == complete")
        fam.add_task("get_iconsoil")
        fam.add_task("remap_iconsoil").add_trigger("get_iconsoil == complete")

# Add a run family for every ensemble member
class TcRun(TcFamily):
    def add_to(self, node):
        fam = node.add_family("iconrun") # was "lokal", task setup_lokal_det deleted!
        for eps_memb in rangeexpand(self.conf["membrange"]):
            fname = membname(eps_memb, "eps_member_")
            run = fam.add_family(fname)
            run.add_variable("ECTC_ENS_MEMB", str(eps_memb))
            
            trig = ""
            trig_day = ""
            for pre in self.conf["pretypes"]: # mars, diss
                trig = expr_and(trig, f"../../pre_{pre}/retrieve_ic_bc/{fname} == complete")
                trig_day = expr_and(trig_day, f"../../../pre_{pre}/retrieve_ic_bc/{fname} == complete")

            rmd=run.add_family("remap_days")
            rmd.add_task("remap_clean").add_trigger(trig_day)
            rmd.add_family("remap_ana").add_task("remap_day").add_variable("RETRIEVE_START", "0").add_variable("RETRIEVE_STOP", "0").add_trigger("../remap_clean == complete")
            nh = self.conf["forecastrange"]
            for d in range(math.ceil(nh/24)):
                rmd.add_family(f"remap_day_{d}").add_task("remap_day").add_variable("RETRIEVE_START", f"{d*24}").add_variable("RETRIEVE_STOP", f"{min((d+1)*24, nh)}").add_trigger("../remap_clean == complete")

            run.add_task("remap").add_trigger(trig).add_defstatus(ecflow.Defstatus("complete"))
            run.add_task("icon").add_trigger("./remap == complete && ./remap_days == complete && ../../iconsoil == complete")

# Add a family for post-processing
class TcPost(TcFamily):
    def add_to(self, node):
        fam = node.add_family("post").add_trigger("iconrun == complete") 
        fam.add_task("post_proc")

# Add a family for data dissemination
class TcSend(TcFamily):
    def add_to(self, node):
        fam = node.add_family("send").add_trigger("post == complete") 
        fam.add_task("send_to_efas")
        
# Add a family for archiving LEPS ouputs
class TcArchive(TcFamily):
    def add_to(self, node):
        fam = node.add_family("archive").add_trigger("send == complete") 
        fam.add_task("archive_data")
        fam.add_task("archive_log")
        
# Add a family for regribbing for cluster analysis and writing to fdb for each member
class TcRegribFdb(TcFamily):
    def add_to(self, node):
        fam = node.add_family("regrib_and_fdb") # task regrib_setup deleted!
        # add clst_info family
        for eps_memb in rangeexpand(self.conf["membrange"]):
            fname = membname(eps_memb, "eps_member_")
            raf = fam.add_family(fname)
            raf.add_variable("ECTC_ENS_MEMB", str(eps_memb))
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
    suitename = sys.argv[1]
    # vars and configuration management to be improved    
    # reset $ECTC_BASE to script directory for relocating in tc directories
    script_dir = os.path.dirname(os.path.abspath(__file__))
    if script_dir.startswith(os.environ["STHOST"]):
        os.environ["ECTC_BASE"] = script_dir.replace(os.environ["STHOST"], "%STHOST%")
        print("Setting up the suite from a TIME CRITICAL storage", script_dir)
    else:
        os.environ["ECTC_BASE"] = script_dir
        print("Setting up the suite from a NON TIME CRITICAL storage", script_dir)
    with open(os.path.join(script_dir, "conf", suitename, "suiteconf.toml"), "rb") as fd:
        suiteconf = tomllib.load(fd)
    conf = suiteconf["suiteconf"] # suiteconf section becomes main conf
    conf["ecfvars"] = {} # add an empty dictionary for ecflow variables
    for k in suiteconf["ecfvars"]: # add ecflow variables expanding environment
        conf["ecfvars"][k] = os.path.expandvars(suiteconf["ecfvars"][k])

    # create a suite, ileps.suite will be the root node of the suite
    ileps = TcSuite(suitename)
    # add defined ECF vars
    TcEcfVars(conf).add_to(ileps.suite)
    if conf.get("timecrit", False):
        TcEmergency(conf).add_to(ileps.suite)
    # add timing loop
    suitetime = TcSuiteTime(conf)
    for timeloop in suitetime.add_to(ileps.suite):
        # get a new conf including current time
        conft = suitetime.updatedconf(timeloop)
        # add the rest of the suite elements to the new root of the suite
        TcStartSuite(conft).add_to(timeloop) # short for f=TcStartSuite(conf); f.add_to(timeloop)
        TcIconSoil(conft).add_to(timeloop)
        TcPre(conft).add_to(timeloop)
        TcRun(conft).add_to(timeloop)
#       TcRegribFdb(conft).add_to(timeloop)
        TcPost(conft).add_to(timeloop)
        TcSend(conft).add_to(timeloop)
        TcArchive(conft).add_to(timeloop)
    # add anything to be added outside the time loop .add_to(ileps.suite)
    # check the suite (syntax, triggers, jobs)
    if "mars" in conf.get("pretypes", []) and conf.get("splitretrieve", False):
        ileps.addlimit("mars_retrieve", 10, "retrieve_ic_bc_day_mars")
    ileps.check()
    #ileps.checked=True
    # write suite to a .def file
    ileps.write(interactive=interactive)
    # upload/replace on server (with confirmation if interactive)
    ileps.replace(interactive=interactive)
