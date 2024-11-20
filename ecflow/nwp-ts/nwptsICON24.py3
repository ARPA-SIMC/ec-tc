#!/usr/bin/env python3                                                                        
# NOTICE: this file was originally created with def2def.py                                    
import sys                                                                                  
# from ecf import * # 25-05-2022 for now use local ecf.py (latest version) from the same directo
# import ecf as ecf # this will become part of the ecflow module later                        
                  # then we should change the call to "from ecflow.ecf import *"              
                  # to use ecf.py from the official module  

from ecf import *

# ecf.ECF_MODE = 'sms'
suite0 = Suite('nwptsICON24').add(
   Repeat(kind='date', name='YMD', start=20211201, end=20211231, step=5),
   Variables(
      ECF_TRIES=1,
      BC_STEP="1",
      TOP_NUD="ON",
      RUN_LEN="744",
      FCLENGTH= '120',
      GRID= 'grid_0',
      DWD_DS= 'e',
      SUITE_NAME= 'nwptsICON24',
      ECF_HOME= '/home/mck/ec-tc/ecflow/nwp-ts',
      ECF_FILES='/home/mck/ec-tc/ecflow/nwp-ts/ecffiles.S24',
      ECF_INCLUDE= '/home/mck/ec-tc/ecflow/nwp-ts/include.S24',
      BASEDIR= '/mck/nwp-ts',
      SCHOST= 'hpc',
      SCHOST_BKUP= 'hpc',
      WSHOST= 'hpc',
      USER= 'mck',
      starthour= '00',
      NMEMBER= '0',
      WSCRATCH= '/ec/res4/scratch/mck',
      MARS_FDB_NO_SPECIAL_FP= '1',
      VERSION= 'S24icon202410',),

   (Family('nwp_suite_00').add(
      Variables(
         ECF_JOB_CMD='troika submit -o %ECF_JOBOUT% %SCHOST% %ECF_JOB% ',
         ECF_KILL_CMD='troika kill %SCHOST% %ECF_JOB%',
         ECF_STATUS_CMD='troika monitor %SCHOST% %ECF_JOB%',
         starthour= '00',
         DOPOCORSA= '/ec/res4/scratch/mck/nwptsH/%VERSION%/post_00',
         EXECDIR= '/ec/res4/scratch/mck/nwptsH/%VERSION%/exec_00',
         MYMARSDIR= '/ec/res4/scratch/mck/nwptsH/%VERSION%/archive_00',
         RUNDIR= '/ec/res4/scratch/mck/nwptsH/%VERSION%/bc_ic_00',
         ECF_OUT= '/ec/res4/scratch/mck/nwptsH/%VERSION%/log',
         ECF_LOGHOST= 'hpc-log',
         ECF_LOGPORT= '39046',
         MAINDIR= '/ec/res4/scratch/mck/nwptsH/%VERSION%',
         TCHOME= '/home/mck',
         PERM= '/perm/mck',
         SORGENTE= '/home/mck/src/ged',
         FORTRAN= '/home/mck/src/f77/prg_timecrit_7km',
         RESCUE= '/perm/mck/rescue',
         USOHOME= '/home/mck/grib_stuff',
         EXEC= '/home/mck/ec-tc/ecflow/nwp-ts/bin',
         NLEVDET= '137',),
   
      Family('pre').add(
      
         Task('configure'),
       #  Task('configure_SP').add(
       #     Defstatus('complete'),
       #  ),
      ), # endfamily pre      
      Family('get_bc_ic').add(
         Trigger('./pre eq complete'),
         Variables(
            ML_EXPID= '0001',
            DET_AREA= '60.4/-12.5/27.6/39.0', 
            DET_GRID= '0.1/0.1',),
      
         Family('retrieve_MARS_day1').add(
             Task('retrieve_MARS_ana').add(Variables( EXTRACT_DAY = "0",),),
             Task('retrieve_MARS_fc').add(Variables( EXTRACT_DAY = "0",),)
         ),

         Family('retrieve_MARS_day2').add(
             Task('retrieve_MARS_ana').add(Variables( EXTRACT_DAY = "1",),),
             Task('retrieve_MARS_fc').add(Variables( EXTRACT_DAY = "1",),)
         ),

         Family('retrieve_MARS_day3').add(
             Task('retrieve_MARS_ana').add(Variables( EXTRACT_DAY = "2",),),
             Task('retrieve_MARS_fc').add(Variables( EXTRACT_DAY = "2",),)
         ),

         Family('retrieve_MARS_day4').add(
             Task('retrieve_MARS_ana').add(Variables( EXTRACT_DAY = "3",),),
             Task('retrieve_MARS_fc').add(Variables( EXTRACT_DAY = "3",),)
         ),

         Family('retrieve_MARS_day5').add(
             Task('retrieve_MARS_ana').add(Variables( EXTRACT_DAY = "4",),),
             Task('retrieve_MARS_fc').add(Variables( EXTRACT_DAY = "4",),)
         ),

         Task('retrieve_ICON'),

      ), # endfamily get_bc_ic      

      Family('model').add(
         Trigger('./get_bc_ic eq complete'),
         Variables(
            hincout= '1',),
      
         Family('remap').add(
           Task('remap_IFS_init'),
           Task('remap_ICON').add(Trigger('./remap_IFS_init eq complete'),),
           Task('remap_SST_FRICE').add(Trigger('./remap_IFS_init eq complete'),),

           Family('remap_IFS_lbc').add(
               Family('remap_01').add(Task('remap_lbc').add(Variables( TRANGES = "13", TRANGE_INI = "0",))),
               Family('remap_02').add(Task('remap_lbc').add(Variables( TRANGES = "12", TRANGE_INI = "13",))),
               Family('remap_03').add(Task('remap_lbc').add(Variables( TRANGES = "12", TRANGE_INI = "25",))),
               Family('remap_04').add(Task('remap_lbc').add(Variables( TRANGES = "12", TRANGE_INI = "37",))),
               Family('remap_05').add(Task('remap_lbc').add(Variables( TRANGES = "12", TRANGE_INI = "49",))),
               Family('remap_06').add(Task('remap_lbc').add(Variables( TRANGES = "12", TRANGE_INI = "61",))),
               Family('remap_07').add(Task('remap_lbc').add(Variables( TRANGES = "12", TRANGE_INI = "73",))),
               Family('remap_08').add(Task('remap_lbc').add(Variables( TRANGES = "12", TRANGE_INI = "85",))),
               Family('remap_09').add(Task('remap_lbc').add(Variables( TRANGES = "12", TRANGE_INI = "97",))),
               Family('remap_10').add(Task('remap_lbc').add(Variables( TRANGES = "12", TRANGE_INI = "109",))),
           )
         ),
         
         Task('run_ICON_LAM').add(
            Trigger('./remap eq complete'),
            Variables(
                  ICONLAM_TASK4NODE= '36',
                  ICONLAMTASK= '576',
                  NLEV= '65',
                  TSTEP= '24',),
            ),
      ), # endfamily model      
      Family('copy_2_ecfs').add(
         Trigger('./model eq complete'),
         Task('copy_2_ecfs').add(),
         Task('save_log').add(
            Trigger('./copy_2_ecfs eq complete'),
         ),
      ), # endfamily copy_2_ecfs     
   ),), # endfamily nwp_suite_00   
)

if __name__ == '__main__':
    defs = Defs()
    defs.add(suite0); 
    defs.auto_add_externs(True)
    if 0: 
      import cli_proc, ecflow
      cli_proc.process(ie.Seed(defs), compare=False)
    else:
        #ECF_PORT = 3141
        #ECF_HOST = (os.environ['HOST'])
        #print_note = "loading on {}@{}".format(ECF_HOST, ECF_PORT)
        #print (print_note)
        print ("prova")
        #client = ecf.Client(ECF_HOST, ECF_PORT)
        #client.replace("/%s" % suite0.name(), defs)

# 2018.08 also save a def file
print(defs)

print("Checking job creation: .ecf -> .job0")
print(defs.check_job_creation())

print("Saving definition to file '" + "%s" % suite0.name() + ".def'")
defs.save_as_defs("%s" % suite0.name() + ".def")


