""" flux calibration script for sci2
"""
# import vaious libraries whose functions are used in this script
import radioPoint as rp
import obsdefUtils as utils
import subarrayCommands as sc
import obsdef3 as od
import source as src

# Make sure user has not added any command line options
import runCommand as rc
p = rc.Params()

p.add("freq", type=float, noneAllowed=True, default=35.938,
      description="1st LO frequency")
p.add("pnttype", type=str, noneAllowed=True, default='cross',
      description="Type of pointing shape to use; cross (default) or triangle")
p.add("list", type=str, noneAllowed=True, default='normal',
      description="List to use for fluxcal - bright sources only ('normal', default) or full list ('full')");
p.add("tintCross", type=float, noneAllowed=True, default=20,
      description="integration time in seconds for each step in pointing cross when pointing up (20s = default)")
p.add("tintSrc", type=float, noneAllowed=True, default=20,
      description="integration time in seconds for fluxcal integration (20s = default)")
p.add("nrepSrc", type=int, noneAllowed=True, default=15,
      description="number of repetitions for fluxcal integration (15 = default)")
p.add("nreps", type=int, noneAllowed=True, default=10,
      description="number of repetitions through flux calibrator list (10 = default)")
p.add("endtrack",type=str, noneAllowed=True, default='None',
      description="LST at which the track should end (default is None)")
p.add("forceStart", type=bool, noneAllowed=True, default='False',
      description="permit start of script even if no planets are up (default is False)")      
p.add("abstest", type=bool, noneAllowed=True, default='True',
      description="observe the abstest sources at the beginning and end if they are up.")      
p.add("alarm",         type=bool, default=True, description="Alarm on when it finishes? True/False")
p.add("lowcoh",        type=str, noneAllowed=True, default=None, description="List of antennas in which to ignore for max coherence alarm")

p.processInputParameters(inputParams=scriptKeyVals)

# set elevation limit
elevationLimit = 25.0

# set minimum separation of source from sun - should always be 30degrees
minSepSun = 20;


#-----------------------------------------------------------------------#
# list of primary calibrators
#-----------------------------------------------------------------------#
primaryCalibrators=src.primaryCalibrators()


#-----------------------------------------------------------------------#
# List of sources to observe
#-----------------------------------------------------------------------#
if p.list == "full":
  print "Using extended 3.5m catalog"
  sourceList=src.fullList()
else:
  print "Using bright 3.5m catalog"
  sourceList=src.brightList()


#-----------------------------------------------------------------------#
# Track info.
#-----------------------------------------------------------------------#
trackFilename='fluxcal_sci2.py'
projectCode='flux'
PI='Tom Plagge'
email='tplagge@kicp.uchicago.edu,ap@astro.caltech.edu'
sendMsg(projectCode,PI,email,trackFilename,start=1)
newProject('flux', 'fluxcal_sci2', '', False)

#-----------------------------------------------------------------------#
# Track start up and tuning.
#-----------------------------------------------------------------------#
radioInit()
zeroMountOffsets()
applyFlux(True)
applyTsys(True)
trackThreshold(0.3,0)
# turn off drive blanking, don't trust it
driveErrorPreference(PREF_BLANK)

#-----------------------------------------------------------------------#
# tune receivers and set sideband
# CARMA8  - 1cm (LSB) or 3mm (USB)
#-----------------------------------------------------------------------#
ants=currentAntennaNumbers();
# configure all WB corr bands
if(p.freq < 50):
    print "Tuning to %2.2f; using LSB data" % p.freq
    # tune the receivers
    freq(p.freq,LSB,0,'none')

if(p.freq > 50):
    print "Tuning to %2.2f; using USB data" % p.freq
    # tune the receivers
    freq(p.freq,USB,0,'none')

print "Configuring wideband correlator for SZA mode"
configwideastroband()
#+++
#astrobandOnline(23,False,subarray='DEFAULT') #for the time when b23 was down
#astrobandOnline(24,False,subarray='DEFAULT') #for the time when b24 was down
#+++
print "Checking correlator bands"
checkbands()

# Coherence alarm
utils.disableCoherenceAlarm()
lowcoh=None
if lowcoh != None: sc.s.setScriptString(utils.INDX_STR_LOWCOH, LOWcoh)
utils.enableCoherenceAlarm()


#-----------------------------------------------------------------------------------------#
# Track each primary flux calibrator, point up on it, and integrate
#-----------------------------------------------------------------------------------------#
# Define source intents.
k=0
nabs=0

abstestTargets=['mars','uranus','neptune','jupiter','mwc349','3c84']

for src in abstestTargets:
  print "Source %s is at el = %2.2f degrees, and the limit is %2.2f degrees" % (src, utils.getSourceElevation(src), elevationLimit)
  print "Source %s is %2.2f degrees from the sun, and the limit is %2.2f degrees" % (src, utils.getDistance('sun',src), minSepSun)
  if utils.getSourceElevation(src) > elevationLimit:
    if utils.getDistance('sun',src) > minSepSun:
       print "Source %s satisfies both sun separation and elevation criteria" % src
       nabs = 1
       k = k + 1   
       print "Integrating on noise source"
       # integrate on noise source twice for 2 seconds
       od.utils.observeNoise(2,2,verbose=True)

       # do tsys
       tsys()
      
       print "Abstest flux calibrator " + src + " is up; integrating for 5 min"
       # intent for abscal sources should be 'BF'
       intent(src,'BF',True,False)

       # point up on the source, apply offset
       rp.radioPoint(src, apply=True, type=p.pnttype, intTime=p.tintCross, timeLimit=10)
       # integrate
       integrate(p.tintSrc,p.nrepSrc,0)
       
       # remove mount offsets found for this source
       zeroMountOffsets()


for src in primaryCalibrators:
  if src.lower() in [i.lower() for i in abstestTargets]:
    continue
  print "Source %s is at el = %2.2f degrees, and the limit is %2.2f degrees" % (src, utils.getSourceElevation(src), elevationLimit)
  print "Source %s is %2.2f degrees from the sun, and the limit is %2.2f degrees" % (src, utils.getDistance('sun',src), minSepSun)
  if utils.getSourceElevation(src) > elevationLimit:
    if utils.getDistance('sun',src) > minSepSun:
       print "Source %s satisfies both sun separation and elevation criteria" % src
       nabs = 1
       k = k + 1   
       print "Integrating on noise source"
       # integrate on noise source twice for 2 seconds
       od.utils.observeNoise(2,2,verbose=True)

       # do tsys
       tsys()
      
       print "Absolute flux calibrator " + src + " is up; integrating for 5 min"
       # intent for abscal sources should be 'BF'
       intent(src,'BF',True,False)

       # point up on the source, apply offset
       rp.radioPoint(src, apply=True, type=p.pnttype, intTime=p.tintCross, timeLimit=10)
       # integrate
       integrate(p.tintSrc,p.nrepSrc,0)
       
       # remove mount offsets found for this source
       zeroMountOffsets()

if k == 0 and p.forceStart==False:
    sys.exit("No primary calibrators were up or available")

#-----------------------------------------------------------------------#
# do radio pointing on each source. point up first so we are centered
# and then integrate
#-----------------------------------------------------------------------#
# loop so that we keep checking to see if sources are up

brsw=0
for k in range(p.nreps):
  if brsw==1:
    break
  
  for src in sourceList:

    # lst check
    if(p.endtrack == 'None'):
      print "No endtrack specified"
    else:
      x=p.endtrack.split(':')
      x=float(x[0])+float(x[1])/60
      if(float(sc.lst()) > x and abs(x-sc.lst())<12):
        print "LST is %2.2f; endtrack LST is %2.2f" % (float(sc.lst()),x)
        print "LST endtrack limit reached; exiting"
        brsw=1
        break

    print "---------------------------------------------------------------------------"
    print "Source is %s" % src
    print "Source %s is at el = %2.2f degrees, and the limit is %2.2f degrees" % (src, utils.getSourceElevation(src), elevationLimit)
    print "Source %s is %2.2f degrees from the sun, and the limit is %2.2f degrees" % (src, utils.getDistance('sun',src), minSepSun)
    if utils.getSourceElevation(src) > elevationLimit:
      if utils.getDistance('sun',src) > minSepSun:
        print "Source " + src + " satisfies both sun separation and elevation criteria"

        utils.relockReceivers()
        # integrate on noise source
        od.utils.observeNoise(2,2,verbose=True)

        # do tsys
        tsys()
      
        # point up on the source, apply offset
        rp.radioPoint(src, apply=True, type=p.pnttype, intTime=p.tintCross, timeLimit=6)

        # now integrate on source
        integrate(p.tintSrc,p.nrepSrc,0)

        # remove mount offsets found for this source
        zeroMountOffsets()

        # remove source from catalog so we don't observe it again
        #del sourceList[sourceList.index(src)]

      else:
        print "Source " + src + " does not satisfy sun separation criterion"
    else:
      print "Source " + src + " does not satisfy elevation criterion"

#-----------------------------------------------------------------------#
# Track each primary flux calibrator, point up on it, and integrate
#-----------------------------------------------------------------------#
# Define source intents.

for src in abstestTargets:
  if brsw: break

  # lst check
  if(p.endtrack == 'None'):
    print "No endtrack specified"
  else:
    x=p.endtrack.split(':')
    x=float(x[0])+float(x[1])/60
    if(float(sc.lst()) > x and abs(x-sc.lst())<12):
      print "LST is %2.2f; endtrack LST is %2.2f" % (float(sc.lst()),x)
      print "LST endtrack limit reached; exiting"
      brsw=1
      break

  print "Source %s is at el = %2.2f degrees, and the limit is %2.2f degrees" % (src, utils.getSourceElevation(src), elevationLimit)
  print "Source %s is %2.2f degrees from the sun, and the limit is %2.2f degrees" % (src, utils.getDistance('sun',src), minSepSun)
  if utils.getSourceElevation(src) > elevationLimit:
    if utils.getDistance('sun',src) > minSepSun:
       print "Source %s satisfies both sun separation and elevation criteria" % src
       nabs = 1
       k = k + 1   
       print "Integrating on noise source"
       # integrate on noise source twice for 2 seconds
       od.utils.observeNoise(2,2,verbose=True)

       # do tsys
       tsys()
      
       print "Abstest flux calibrator " + src + " is up; integrating for 10 min"
       # intent for abscal sources should be 'BF'
       intent(src,'BF',True,False)

       # point up on the source, apply offset
       rp.radioPoint(src, apply=True, type=p.pnttype, intTime=p.tintCross, timeLimit=10)
       # integrate
       integrate(p.tintSrc,p.nrepSrc,0)
       
       # remove mount offsets found for this source
       zeroMountOffsets()

if nabs<1:
  for src in primaryCalibrators:
    if src.lower() in [i.lower() for i in abstestTargets]:
      continue
    print "Source %s is at el = %2.2f degrees, and the limit is %2.2f degrees" % (src, utils.getSourceElevation(src), elevationLimit)
    print "Source %s is %2.2f degrees from the sun, and the limit is %2.2f degrees" % (src, utils.getDistance('sun',src), minSepSun)
    if utils.getSourceElevation(src) > elevationLimit:
      if utils.getDistance('sun',src) > minSepSun:
        print "Source " + src + " satisfies both sun separation and elevation criteria"
        print "Integrating on noise source"
        # integrate on noise source
        od.utils.observeNoise(2,2,verbose=True)

        # do tsys
        tsys()

        print "Absolute flux calibrator " + src + " is up; integrating for 10 min"
        # intent for mars should be 'BF'
        intent(src,'BF',True,False)
        # point up on the source, apply offset
        rp.radioPoint(src, apply=True, type=p.pnttype, intTime=p.tintCross, timeLimit=10)
        # integrate for 10min
        integrate(p.tintSrc,p.nrepSrc,0)

        # remove mount offsets found for this source
        zeroMountOffsets()

#-----------------------------------------------------------------------#
# Ends main script
#
# turn tsys back on, resets project, sends email
#
#-----------------------------------------------------------------------#
# remove mount offsets found for this source
zeroMountOffsets()
applyTsys(True)
applyFlux(True)
resetProject()
sendMsg(projectCode,PI,email,trackFilename)
if p.alarm: sc.alarmon()
