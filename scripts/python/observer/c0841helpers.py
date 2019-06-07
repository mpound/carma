import sys
import device
import time
from subarrayCommands import *
# import subarrayCommands as SAC
import obsdef3 as od
import traceback
import subprocess
import shlex
lr=device.getLoberotator()

# minutes until time UTstring
def deltaUT( UTstring ) :
  [hr,min,sec] = str.rsplit( UTstring, ":" )
  [nowYr, nowMon, nowDay, nowHr, nowMin, nowSec, aa, bb, cc] = time.gmtime()
  targetMins = 60.*float(hr) + float(min) + float(sec)/60.
  nowMins = 60.*nowHr + nowMin + nowSec/60.
  return (targetMins - nowMins)

# restore comparison antenna to its normal state
def antrestore( ant ) :
  pswitchon( ant )                    # turn on phase switching
  lr.enableFringeTracking( ant,True )     # turn on fringe tracking
  lr.setOffsetRate( ant, 0. )       # set offset freq to 0.
  lr.setOffsetControl( ant, False )     # disable offset freq

def getNextScan( lastUT, schedfile='/home/obs/plambeck/vlbi/2012/schedules/sched.081' ) :
  fin = open( schedfile, "r" )
  buf = []
  chosenLine = -1 
  UT1 = "55:55"
  UT2 = "55:55"
  source = "None"
  type = 9
  intsecs = 10.

  for line in fin:
    if (not line.startswith("#")) : 
      a = line.split()
      UTstop = a[2]

      # all lines are written to the buffer; first viable line is
      # chosen as the active line, indicated by arrow " > "
   
      # cannot be active line because it is past, because there are
      #  < 2 minutes before UTstop, or because it matches lastUT
      if ( (deltaUT( UTstop) < 2.) or (UTstop == lastUT) ) :
        buf.append("    " + line.rstrip() )

      # first line not meeting above criteria is chosen as active line
      elif (chosenLine < 0) :
        buf.append("  > " + line.rstrip()  )
        source = a[0]
        UT1 = a[1]
        UT2 = a[2]
        intsecs = int(a[3])
        type = int(a[4])
        chosenLine = len(buf)

      # write remaining lines to the buffer
      else :
        buf.append("    " + line.rstrip() )

  fin.close()
  if (chosenLine > -1) :
    istart = (chosenLine - 3)
    if (istart < 0) :
      istart = 0
    istop = (chosenLine + 2)
    if (istop > len(buf)) : 
      istop = len(buf)
    print " "
    for i in range(istart,istop) :
      print buf[i]
    print " "
  return [source,UT1,UT2,intsecs,type]

def restoreOpticalPointing(pointing):
   # Optical pointing values
   badants = s.getScriptString(od.INDX_STR_OPT_BADANTS)
   sopt    = s.getScriptString(od.INDX_STR_OPT_RES)
   if sopt != "":
       # Bad antennas
       if not pointing.has_key('badOptAnts'):
           pointing['badOptAnts'] = list()
           if badants != '': pointing['badOptAnts'] = od.utils.makeListObsdef(badants)

       # Optical pointing results
       if not pointing.has_key('optRes'):
           pointing['optRes'] = dict()
           z = sopt.split()
           for i in range(0,len(z),3):
              ant = int(z[i])
              d_az = float(z[i+1])
              d_el = float(z[i+2])
              pointing['optRes'][ant] = [d_az, d_el]

def gridsOut( ) :
  print "commanding grids to move out of 10-m beams"
  for a in range(1,7) :
    subprocess.Popen(shlex.split('canpacket overip=t host=c%d.carma.pvt canbus=1 api=72 msgid=0x082 format=%%c values=0x01'%(a))).wait()


