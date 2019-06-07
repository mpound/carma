""" radio_pointing script

    Generalised to run on CARMA23, CARMA15/CARMA8 mode
"""
# import various libraries whose functions are used in this script
import radioPoint as rp
import obsdefUtils as utils
import subarrayCommands as sc
import source as src

# Make sure user has not added any command line options
import runCommand as rc
p = rc.Params()

p.add("freq", type=float, noneAllowed=True, default=85.8286,
      description="1st LO frequency")
p.add("pnttype", type=str, noneAllowed=True, default='triangle',
      description="Type of pointing shape to use; cross or triangle (default)")
p.add("tint", type=float, noneAllowed=True, default=20,
      description="integration time in seconds (20s = default)")
# Sci2: tint=5s is good enough at 1cm, tint=20s at 3mm (same as old schedlib)
p.add("endtrack",type=str, noneAllowed=True, default='None',
      description="LST at which the track should end (default is None)")

p.processInputParameters(inputParams=scriptKeyVals)

#-----------------------------------------------------------------------#
# Track info.
#-----------------------------------------------------------------------#
trackFilename='radio_pointing.py'
projectCode='rpnt'
PI='Katherine Lee'
email='volgenau@mmarray.org,demerese@astro.umd.edu,ijlee9@astro.umd.edu'
sendMsg(projectCode,PI,email,trackFilename,start=1)
# TLC - added 4/15/2011 
#newProject(projectCode, 'rpnt', '', False)
newProject('ct013', 'rpnt', '', False)

# get current antenna numberse
ants=currentAntennaNumbers();

ovrobima=False
sza=False
for k in ants:
    if k in range(1,15):
      ovrobima=True
    elif k in range(16,23):
      sza=True

#-----------------------------------------------------------------------#
#
# Track start up and tuning.
#
#-----------------------------------------------------------------------#

radioInit()
# zero the mount offsets
zeroMountOffsets()
# if we are working at 1cm, zero out the aperture offsets as well
#if p.freq < 50:
#  print "1cm observations: zeroing aperture offsets"
#  for k in ants:
#    radio1cmPointingConstants(0,0,0,k)

applyFlux(True)
applyTsys(True)
trackThreshold(0.3,0)

resetProject()

#-----------------------------------------------------------------------#
# write out current sci2 pointing model parameter values when the script is
# run, and write to a file with the same name as the miriad file
#-----------------------------------------------------------------------#

# Sza1.Tracker.encoder_off1
#s1 ="Sza%i.Tracker.flexure1" % k
#s2 ="Sza%i.Tracker.flexure2" % k
#s3 ="Sza%i.Tracker.tilts1" % k
#s4 ="Sza%i.Tracker.tilts2" % k
#s5 ="Sza%i.Tracker.tilts3" % k
#s6 ="Sza%i.Tracker.collimation1" % k
#s7 ="Sza%i.Tracker.collimation2" % k
#s8 ="Sza%i.Tracker.encoder_off1" % k
#s9 ="Sza%i.Tracker.encoder_off2" % k
#s10="Sza%i.Tracker.encoder_off3" % k
#v1,v2,v3,v4,v5,v6,v7,v8,v9,v10=queryMpValues([s1,s2,s3,s4,s5,s6,s7,s8,s9,s10)


# turn off drive blank/flag, don't trust it
driveErrorPreference(PREF_NONE)


#-----------------------------------------------------------------------#
# tune receivers and set sideband
# cases:
# CARMA23 - 3mm only, USB
# CARMA15 - 3mm or 1mm, DSB
# CARMA8  - 1cm (LSB) or 3mm (USB)
#-----------------------------------------------------------------------#

# configure all corr bands to wideband mode
# CARMA 23 mode
if (sza & ovrobima):
    # tune the receivers
    freq(p.freq,USB,0,'none')
    print "Configuring spectral line correlator for CARMA23 mode"
    configwideastroband(conf='CARMA23')
# SZA mode
elif (sza & (not ovrobima)):
    if(sza & (p.freq < 50) ):
        print "Tuning to %2.2f; using LSB data" % p.freq
        # tune the receivers
        # XXX This should probably be hard-wired to 35.938 GHz, rather than p.freq.
        freq(p.freq,LSB,0,'none')
    if(sza & (p.freq > 50) ):
        print "Tuning to %2.2f; using USB data" % p.freq
        # tune the receivers
        freq(p.freq,USB,0,'none')

    print "Configuring wideband correlator for SZA mode"
    configwideastroband()

# CARMA 15 mode
elif ((not sza) & ovrobima):
    # tune the receivers
    if (p.freq < 50):
      freq(p.freq,LSB,0,'none')
    else:
      freq(p.freq,AUTO,0,'none')
    print "Configuring wideband correlator for CARMA15 mode"
    configwideastroband()

checkbands()

#
# reps is the number of times to go through the source list.
#
reps      = 25
elevationLimit = 20.0
#-----------------------------------------------------------------------#
#
# Source set up. 
# Average radio-optical separation: 4.95 degrees (not including planets)
# Be pristine, if adding source, make them upper case.
#
#-----------------------------------------------------------------------#
if (sza & ovrobima):
  srcList=src.fullList()
elif (sza & (not ovrobima)):
  # SZA mode
  if (p.freq < 50):
    print "Using extended 3.5m catalog"
    srcList=src.fullList()
  else:
    print "Using bright 3.5m catalog"
    srcList=src.brightList()
elif ((not sza) & ovrobima):
  # CARMA 15 mode
  srcList=src.brightList()

print srcList

# set endtrack
if p.endtrack.find('None') <0:
  sc.setLstEndTrack(p.endtrack)

#-----------------------------------------------------------------------#
#
# do radio pointing on each source
#
#-----------------------------------------------------------------------#
brsw=0
for i in range(reps) :
  if brsw==1:
    print i
    print reps
    print "breaking"
    break
  else:
    sourceList = srcList

    for name in sourceList:
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
        
      print "Radio pointing: observing %s" % name
      # zero out the mount offsets - shouldn't need to do this, but err on
      # side of caution
      zeroMountOffsets()
      utils.relockReceivers()
      if utils.getSourceElevation(name) > elevationLimit:
        if sza:
          print "Observing source %s with the 3.5m telescopes - default timeLimit=7 min" % name
          rp.radioPoint(name, apply=False, type=p.pnttype, intTime=p.tint, timeLimit=7, antwait=-1)

        elif (p.freq > 50 & (not sza)):
          rp.radioPoint(name, apply=False, type=p.pnttype, intTime=p.tint, timeLimit=10, antwait=-1)


#-----------------------------------------------------------------------#
# Ends main script
# turn tsys back on, resets project, sends email
#-----------------------------------------------------------------------#
applyTsys(True)
applyFlux(True)
resetProject()
sendMsg(projectCode,PI,email,trackFilename)

