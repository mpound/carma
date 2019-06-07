import sys
import device
import time
from subarrayCommands import * 
import obsdef3 as od
import obsdefUtils as utils
import traceback
import c0841helpers as hlp
import radioPoint as rp
import runCommand as rc
lr=device.getLoberotator()

# --- begin helper routines --- # 
# minutes until time UTstring
#def deltaUT( UTstring ) :
#  [hr,min,sec] = str.rsplit( UTstring, ":" )
#  [nowYr, nowMon, nowDay, nowHr, nowMin, nowSec, aa, bb, cc] = time.gmtime()
#  targetMins = 60.*float(hr) + float(min) + float(sec)/60.
#  nowMins = 60.*nowHr + nowMin + nowSec/60.
#  return (targetMins - nowMins)
#
## restore comparison antenna to its normal state
#def antrestore( ant ) :
#  pswitchon( ant )                    # turn on phase switching
#  lr.enableFringeTracking( ant,True )     # turn on fringe tracking
#  lr.setOffsetRate( ant, 0. )       # set offset freq to 0.
#  lr.setOffsetControl( ant, False )     # disable offset freq
#
#def getNextScan( lastUT, schedfile='/home/obs/plambeck/vlbi/2012/schedules/sched.075pol' ) :
#  fin = open( schedfile, "r" )
#  buf = []
#  chosenLine = -1 
#  UT1 = "55:55"
#  UT2 = "55:55"
#  source = "None"
#  type = 9
#  intsecs = 10.
#
#  for line in fin:
#    if (not line.startswith("#")) : 
#      a = line.split()
#      UTstop = a[2]
#
#      # all lines are written to the buffer; first viable line is
#      # chosen as the active line, indicated by arrow " > "
#   
#      # cannot be active line because it is past, because there are
#      #  < 2 minutes before UTstop, or because it matches lastUT
#      if ( (deltaUT( UTstop) < 2.) or (UTstop == lastUT) ) :
#        buf.append("    " + line.rstrip() )
#
#      # first line not meeting above criteria is chosen as active line
#      elif (chosenLine < 0) :
#        buf.append("  > " + line.rstrip()  )
#        source = a[0]
#        UT1 = a[1]
#        UT2 = a[2]
#        intsecs = int(a[3])
#        type = int(a[4])
#        chosenLine = len(buf)
#
#      # write remaining lines to the buffer
#      else :
#        buf.append("    " + line.rstrip() )
#
#  fin.close()
#  if (chosenLine > -1) :
#    istart = (chosenLine - 3)
#    if (istart < 0) :
#      istart = 0
#    istop = (chosenLine + 2)
#    if (istop > len(buf)) : 
#      istop = len(buf)
#    print " "
#    for i in range(istart,istop) :
#      print buf[i]
#    print " "
#  return [source,UT1,UT2,intsecs,type]

#def restoreOpticalPointing(pointing):
#   # Optical pointing values
#   badants = s.getScriptString(od.INDX_STR_OPT_BADANTS)
#   sopt    = s.getScriptString(od.INDX_STR_OPT_RES)
#   if sopt != "":
#       # Bad antennas
#       if not pointing.has_key('badOptAnts'):
#           pointing['badOptAnts'] = list()
#           if badants != '': pointing['badOptAnts'] = od.utils.makeListObsdef(badants)
#
#       # Optical pointing results
#       if not pointing.has_key('optRes'):
#           pointing['optRes'] = dict()
#           z = sopt.split()
#           for i in range(0,len(z),3):
#              ant = int(z[i])
#              d_az = float(z[i+1])
#              d_el = float(z[i+2])
#              pointing['optRes'][ant] = [d_az, d_el]
#      
# --- end helper routines, begin main loop --- #

#-------------------------------------------------------------------------------#
# parse any input parameters
#-------------------------------------------------------------------------------#

p = rc.Params()
p.add("vlbiAnt1", type=int, noneAllowed=True, default=1,
      description="vlbi comparison ant with 114 Hz offset")
p.add("tune", type=bool, noneAllowed=True, default=True,
      description="tune receivers (to 226.321 GHz), set up correlator")
p.add("restart", type=bool, noneAllowed=True, default=False,
      description="restarting script - restoreOpticalPointing")
p.processInputParameters(inputParams=scriptKeyVals)

newProject('c0841', '1C_230SgrA', '', False)
ucat('c0015.cat')

pointing = od.createPointingDictionary()
pointing[ "doOptPoint" ] =  True  # If True, then perform optical instead of radio pointing
pointing[ "intervalDay" ] = 0.01
pointing[ "intervalNight" ] = 0.01
pointing[ "intervalOpt" ] = 0.01
pointing[ "minsepsun" ] = 40.
od.initializeOpticalPointing()
doOptPoint(True)

if (p.restart) :
  hlp.restoreOpticalPointing(pointing)
  hlp.gridsOut()
else :
  controlVariablesClear()
    # ... to avoid optical pointing crash
   

  # ---------------- frequency setup --------------------- 
  #  synth = 1197.7830692
  #  Xband = (9 * synth - 10) = 10770.0476228
  #  mmosc = (7 * xband + 50) = 75440.3333596
  #  LO1 = 3 * mmosc = 226321.0000788
  #  correlator band 5 = vlbi IF0 covers sky freq (228.849 - 229.329)
  #  correlator band 7 = vlbi IF1 covers sky freq (229.361 - 229.841)

if (p.tune):
  lo1 = 226.3210000788
  freq( lo1, USB, 0., None )
  clearastroband(0)
  configastroband( 1, "FULLSTOKES", BW500, lo1 + 6.0, AUTO, bits=CORR_2BIT, decimate=True )
  configastroband( 3, "FULLSTOKES", BW500, lo1 + 7.5, AUTO, bits=CORR_2BIT, decimate=True )
  configastroband( 5, "FULLSTOKES", BW500, lo1 + 2.750, USB, bits=CORR_2BIT, decimate=True )
  configastroband( 7, "FULLSTOKES", BW500, lo1 + 3.298, LSB, bits=CORR_2BIT, decimate=True )
  optimizeThresholds()
  flattenPhases()
  tsys(ifsetup=True)

# indicate script start in logfile
logfile = open('/home/obs/plambeck/vlbi/2012/vlbiLog', 'a')
ut = getMiriadUTStamp()
logfile.write("\n**********************************************************\n")
logfile.write( str(ut) + " begin with comparison ant = " + str(p.vlbiAnt1) + "\n\n")
logfile.flush()

# read through schedule file, retrieve next viable scan (> 2 minutes until UTstop)
lastUT = "99:99"
[source, UTstart, UTstop, intsecs, action] = hlp.getNextScan( lastUT )

# loop through schedule; action=9 signifies end of file
while (action < 9) :

    # it is guaranteed that UTstop is at least 2 minutes in the future
    hlp.antrestore( p.vlbiAnt1 )
    ut = getMiriadUTStamp()
    intent( source, 'O', True )       
    logfile.write("\n%s %8s [ %8s %8s %2d %d ]" % (ut, source, UTstart, UTstop, intsecs, action))
 
    if (action == 2) :    # optical pointing
      logfile.write(" optical pointing\n" )
      od.doPoint( pointing, ref=source )

    elif (action == 3) :   # radio pointing
      logfile.write(" radio pointing\n" )
      rp.radioPoint( source, type='triangle', timeLimit=5 )

    elif (action == 4) :   # xyphase calibration using grids; blank sky OK
      logfile.write(" xyphase calibration\n" )
      logfile.flush()
      intent( source, 'P', True)       # 'P' = polarization calibration
      utils.observePol(0.5, intsecs) 

    elif (action == 1) :   # normal calibration scheme
      logfile.write(" calibration scan\n" )
      logfile.flush()
      track( source, waiton=ALL )
      minutes_left = hlp.deltaUT( UTstop )  
      ut = getMiriadUTStamp()
      if (minutes_left > 1) :
        print "%s ... on source; measure Tsys" % ut 
        logfile.write("%s ... on source; measure Tsys\n" % ut )
        tsys()
        ut = getMiriadUTStamp()
        print "%s ... begin regular integration" % ut 
        logfile.write("%s ... begin regular integration\n" % ut )
        logfile.flush()
        minutes_left = hlp.deltaUT( UTstop )   
        integrate(intsecs,int(60.*minutes_left/intsecs) )
      else:
        print "%s ... on source; abort because < 1 minute remains" % ut 
        logfile.write("%s ... on source; abort because < 1 minute remains\n" % ut )
        logfile.flush()

    else:   # vlbi scan
      logfile.write(" vlbi scan\n" )
      logfile.flush()
      track( source, waiton=ALL )
      intent( source, 'S', True)       # 'S' = VLBI scan

      # record when we reach the source, measure tsys
      ut = getMiriadUTStamp()
      print "%s ... on source; measure Tsys" % ut 
      logfile.write("%s ... on source; measure Tsys\n" % ut )
      tsys()
      minutes_left = hlp.deltaUT( UTstart )   

      # do regular observation if there are more than 2 minutes left before UTstart
      if (minutes_left > 2) :
        print "%s ... begin regular integration" % ut 
        logfile.write("%s ... begin regular integration\n" % ut )
        logfile.flush()
        integrate(intsecs,int((60.*minutes_left-30.)/intsecs) )

      # begin VLBI scan; turn off fringe tracking for vlbi ants              
      minutes_left = hlp.deltaUT( UTstop )
      if ( minutes_left > 0 ) :
        ut = getMiriadUTStamp()
        print "%s ... begin vlbi integration, Walsh off" % ut 
        logfile.write("%s ... begin vlbi integration, Walsh off\n" % ut )
        logfile.flush()
        pswitchoff(p. vlbiAnt1 )
        lr.enableFringeTracking( p.vlbiAnt1,False )
        lr.setOffsetRate( p.vlbiAnt1, 114. )
        lr.setOffsetControl( p.vlbiAnt1, True )  
        intent( source, 'S', True)       # 'S' = VLBI scan
        integrate(intsecs,int((60.*minutes_left+30.)/intsecs) )

    # retrieve next viable scan
    lastUT = UTstop
    [source, UTstart, UTstop, intsecs, action] = hlp.getNextScan( lastUT )

# normal end
hlp.antrestore( p.vlbiAnt1)

print "***********"
print " FINISHED! "
print "***********"

