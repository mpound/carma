#
# 22-Apr-06: S. Corder: added LST handling
# 08-Jun-06: S. Corder: Added reload of stars within the loop
#            to force redefining of ostars_.
# 12-Jun-06: S. Scott: Added elmin and ignoreSun options.
# 18-Jun-06: S. Corder: Added telescope type for sorting azimuth.
# 01-Feb-07: S. Corder: Added north-south sorting options so we can
#                       optimize for Ovro slew times.  Also added
#                       options to reverse order on multiple reps
#                       to eliminate long slews.
# 18-Apr-07: S. Corder: Adding a full wrapper that allows for windows to
#                       spawn and execute.
# 21-Mar-12: N. Volgenau: Option to run tilt before opt pointing.
#
# $Id: runOPoint.py,v 1.40 2013/12/17 18:28:55 scott Exp $
#
from subarrayCommands import *
from cameraControl import *
import fileIOPython as fIOP
import time,os,sys
from etrack import IS_E_CONFIGURATION, getInnerAntennas, getOuterAntennas
import carmaHelpers as helpers

sys.path.append('/tmp')
#s = Subarray.getSubarray()
isEArray = IS_E_CONFIGURATION


def convertLst(lstVal) :
    if str in [type(lstVal)] :
        lstTemp = lstVal.split(':')
        if len(lstTemp) == 3 : lstTime = float(lstTemp[0])+float(lstTemp[1])/60.0+float(lstTemp[2])/3600.0
        elif len(lstTemp) == 2 : lstTime = float(lstTemp[0])+float(lstTemp[1])/60.0
        elif len(lstTemp) == 1 : lstTime = float(lstTemp[0])
        else : return 'Improper time format'
    elif ((int in [type(lstVal)]) or (float in [type(lstVal)])) :
        lstTime = float(lstVal)
    else : return 'Improper time format'
    return lstTime
def waitLst(lstVal) :
    lstTime = convertLst(lstVal)
    timeWait = (lstTime-lst())
    if timeWait > 12.0 : timeWait = timeWait-24.0
    elif timeWait < -12.0 : timeWait = timeWait+24.0
    if timeWait > 0.0 : 
       print "Waiting %s minutes before starting." % str(timeWait*60.0)
       wait(tmo=timeWait*3600.0)    
def stopLst(lstVal) : 
    lstTime = convertLst(lstVal)
    timeDiff = (lstTime-lst())
    if timeDiff > 12.0 : timeDiff = timeDiff-24.0
    elif timeDiff < -12.0 : timeDiff = timeDiff+24.0
    if timeDiff < 0.0 : 
        print "LST time cutoff has been reached."
        return 1 # Implies you are past the stop time
    else : return 0 # Implies you are in the proper time range.
def getBrightOptical(elmax):
    infoObjList = getBrightest(elMax=elmax, getOptical=True)
    return infoObjList[0].name
    
def flapCheck(ants=0,elMax=80) :
    print 'This program opens all the optical cameras,'
    print 'then tracks a bright optical source that is up.'
    print 'You should view each camera and once on source,'
    print 'determine if the flap is open.'
    print 'The flap is closed if the star appears either'
    print 'EXTREMELY faint and you are looking at VEGA or'
    print 'Sirius, or not at all...or very very faint'
    print 'if you are looking at another source.'
    print 'Note these and go try to hit the black flap reset'
    print 'button if you need to on bima.'
    print 'You should also hit the applycentroid button'
    print 'on camera images that are not well centered.'
    print 'Close the camera window when done to go to the next'
    print 'telescope.'
    antVec=makeAntList(ants)
    camera(ON,antVec)
    track(getBrightOptical(elMax))
    #[ovroAnts,bimaAnts,szaAnts] = short.separateAntType(antVec)
    #for i in bimaAnts : Camera(Carma(i)).on()
    #for i in ovroAnts : Camera(Carma(i)).on()
    for a in makeAntList(ants) : opticalSystem(a,)

def runOuterOPoint(oreps=2,breps=None,isDaytime=False) :
    ants = getOuterAntennas()
    if len(ants) == 0: 
        raise Exception, "There are no outer antennas in this subarray"
    runAllOPoint(ants,oreps,breps,isDaytime)

def runInnerOPoint(oreps=2,isDaytime=False) :
    ants = getInnerAntennas()
    if len(ants) == 0: 
        raise Exception, "There are no inner antennas in this subarray"
    runOPoint(ants,oreps,isDaytime=isDaytime,effectFlap=True,pointMode=0,isInner=True)

def runAllOPoint(ants=0,oreps=2,breps=None,isDaytime=False,doTilt=True) :
    if ants==0 or ants == [0] : ants=currentAntennaNumbers()
    ants = helpers.makeList(ants)
    if breps == None : breps = 2*oreps-1
    arrayName = saName
    if doTilt :
        print 'Starting optical pointing with a tilt ... '
        tilt(ants=0,direction=0,samples=12,delay=6.0,doplot=False)
        print 'tilt complete.'
    else : print 'Starting optical pointing WITHOUT a tilt.'
    for i in ants :
        if i < 7 : reps = oreps
        elif i < 16 : reps = breps
        else : reps = 0
        makeOPointGo(i,reps,arrayName,isDaytime)
    print 'When all pointing is done, press cntrl-c in this window'
    print 'if the spawned windows do not go away on their own.'

def makeOPointGo(ant,param,arrayName,isDaytime) :
    fname = "/tmp/opntAnt%i.py" % ant
    f     = open(fname,'w')
    f.write("sleep(1.0)\n")
    f.write("import runOPoint,stars\n")
    f.write("runOPoint.runOPoint(%d,%d,3.0,effectFlap=True,isDaytime=%i)\n" % (ant,param,int(isDaytime)) )
    f.write("print 'I am finished.'\n")
    f.close()
    cmd = "xterm -fg white -bg black -geometry 80x15 -T opnt-%i -e '%s < %s' &" % (ant,arrayName,fname)
    print cmd
    os.system(cmd)

def runOPoint(ant,repetitions,zoomVal=4.5,magMax=99.99,\
        effectFlap=False,doGui=False,zeroOffsets=True, \
        elmin=20, lstStart='None',lstStop='None',cameraOn=True, \
        ignoreSun=True,pointMode=1,isInner=False,isDaytime=False) :
    import stars
    #sendMsg('opnt','Andrea Isella','isella@astro.caltech.edu','junk.txt')
    #sendMsg('opnt','Nikolaus Volgenau','volgenau@ovro.caltech.edu','opnt')
    ant=helpers.makeList(ant)
    
    # Set the tracking threshold to 2 arcseconds
    trackThresholdOptical(2.0, ant)
    
    for i in ant : s.mountOffset(0.0,0.0,i)
    if cameraOn : camera(ON,ant)
    if (lstStart <> 'None') : aitLst(lstStart)
    for i in range(repetitions) :
        if (lstStop <> 'None') :
            if (stopLst(lstStop)) : break
        reload(stars)
        stars.stars(magmax=magMax,binSize=25.0,northSouth='north')
        northStars = stars.stars_[:]
        stars.stars(magmax=magMax,binSize=25.0,northSouth='south')
        southStars = stars.stars_[:]
        stars.stars_ = northStars+southStars
        if (not ignoreSun) and s.isUp('sun') :
            print """The sun is up!! **Exiting this script just
                  in case one of flap protection circuits fails**"""
            if not isEArray : stow(ant, waiton=NONE)
            return 1
        if i%2 == 1 : stars.stars_.reverse()
        if not isDaytime : stars.point(ant,gui=doGui,zoom=zoomVal,elmin=elmin, flap=effectFlap,mode=pointMode,isInner=isInner)
        else : stars.point(ant,gui=doGui,zoom=zoomVal,elmin=elmin,flap=effectFlap,mode=pointMode,isInner=isInner,subtractBackground=True,ncoadd=40,dazbkg=2.0,centroidLoopMax=4)
        stow( ants=ant )

def checkNewOpticalCounts(ants=0) :
    date = time.asctime(time.gmtime()).split()
    if int(date[2]) < 10 : date[2] = str('0'+str(date[2]))
    dateString = '%s%s%s' % (date[2],date[1],date[4][2:])
    antVec = makeAntList(ants)
    [ovroAnts,bimaAnts,szaAnts] = antennasByType(antVec)
    for i in ovroAnts :
        os.system('rm -rf /array/rt/scripts/ovroJunk.txt')
        os.system("grep %s /home/obs/optical.ovro.data | grep MM%i | nl >  /array/rt/scripts/ovroJunk.txt" % (dateString,i))
        table = fIOP.fileToTable('/array/rt/scripts/ovroJunk.txt')        
        if len(table) > 0 : print 'Carma %i has %i stars to this point' % (i,int(table[-1][0]))
    for i in bimaAnts :
        os.system('rm -rf /array/rt/scripts/bimaJunk.txt')
        os.system('grep %s /home/obs/optical.bima.data | grep " %i " | nl >  /array/rt/scripts/bimaJunk.txt' % (dateString,(i-6)))
        table = fIOP.fileToTable('/array/rt/scripts/bimaJunk.txt')
        if len(table) > 0 : print 'Carma %i has %i stars to this point' % (i,int(table[-1][0]))
