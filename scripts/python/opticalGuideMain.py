#
# Stuartt Corder
# 626-345-4457 (cell)
#
# Daytime:
# HP024608-->AAur: 3c111,    13.6 degrees
# HP027989-->AOri: 0530+135, 8.54 degrees ****winner****
# HP037279-->ACMi: 0854+201, 28.9 degrees
# HP037826-->BGem: 0854+201, 19.1 degrees
# ABoo------>ABoo: 3c273,    31.2 degrees
#
from subarrayCommands import *
#import short
import sys,stars,os
import time
import obsdefIndex as odi
import carmaHelpers as helpers
#import radioPoint
from cameraControl import Camera
sys.path.append('/tmp')

#
# Typical use:
#  call opticalInit() which spawns lots of xterms
#  to initiate optical pointing, call subOptical for a single (key) antenna
#   the other ants will do an optical pointing when they
#   see the refraction model change for this antenna.
# This is an incredible ugly kludge and it should be completely reworked.

#
# Needed improvements: 
# o dictionary value defining success with something
#    other than non-zero values, which zero can be fine....unlikely failure
#    mode but still possible.
# o use script variables to set up coordination.
#   This will allow removal of the key antenna and more transparent code.
#

VERBOSE = False

# Control functions: Directly executable by external script.

def opticalInit(antKey=None,antVec=0) :
    """
    Must be run before starting parallel opoint loops.  Sets the
    apertures as they need to be and gets the flaps open but
    with proper aperture in.  Does radioInit, opens flaps, forces
    apertures to radio. Replaces radioInit for opticalRadio offset
    observations.
    """
    if VERBOSE:
        scriptlog("opticalInit: antKey="+str(antKey)+" antVec="+str(antVec))
    antVec = makeAntList(antVec)
    if antKey == None : antKey = min(antVec)
    camera(ON, antVec)
    sleep(2.0)
    changeToRadio(antVec)
    sleep(1.0)
    antOther = antVec[:]
    antOther.remove(antKey)
    runOpticalReference(antOther)  # Launch parallel optical pointing
    return [antVec, antKey]

def runOpticalReference(ants) :
    if VERBOSE:
        scriptlog("runOpticalReference: ants="+str(ants))
    ants = helpers.makeList(ants)
    for i in ants :
        makeReferenceGo(i, saName)
        sleep(0.5)
    print ''
    print '*******************************************'
    print 'Do not kill the windows that just opened!!!'
    print 'These windows run optical pointing on each'
    print 'of the antennas.'
    print '*******************************************'
    print ''

def makeReferenceGo(ant, arrayName) :
    fname = "/tmp/referenceAnt%i.py" % ant
    f     = open(fname,'w')
    f.write("sleep(0.5)\n")
    f.write("import opticalGuideMain as ogmE\n")
    f.write("rm -rf "+fname+"\n")
    f.write("ogmE.runParallelOPoint(%i)\n" % ant)
    f.write("print 'I am finished.'\n")
    f.close()

    # Change the file permissions so that any user can overwrite it
    try:
       os.chmod(fname, 0666)
    except:
       # print 'WARNING: Could not change permissions for file ',fname,'.'
       pass
    cmd  = "xterm -fg white -bg black -geometry 80x15 -T opticalRef-%i" %ant
    cmd += " -e '%s < %s' &" %(arrayName, fname)
    os.system(cmd)

def makeOptRes(ants=None):
    """Helper for debugging: makes an optRes dictionary containing all the
    current antennas in the subarray"""
    if ants == None: ants = currentAntennaNumbers()
    ores = dict()
    for a in ants:
        ores[a] = [a/60., -a/60.0]
    return ores
        
def subOptical(optRes, opticalSource, antKey=None, zoomVal=6.0,badOptical=[]):
    """
    This measures the new optical offset position and corrects
    the mount model according to optical offset vector, which is given.
    Parameters:
     optRes: a dictionary of optical pointing results with ant#'s as the keys.
     opticalSource:
     antKey: ant# to be used for optical pointing. Default (None) is to use
      lowest value in optRes.
    """
    antVec = optRes.keys()
    cur = currentAntennaNumbers()
    if VERBOSE:
        scriptlog("subOptical: antVec="+str(antVec)+" currentAnts="+str(cur)
                  +" antKey="+str(antKey))
    for i in antVec :
        if i not in cur : antVec.remove(i)
    if antKey == None    : antKey = min(antVec)
    optRadDiffAz = []
    optRadDiffEl = []
    for i in antVec :
        optRadDiffAz.append(optRes[i][0])
        optRadDiffEl.append(optRes[i][1])
    [azVec,elVec] = runKeyOpticalPeakUp(opticalSource,antKey,antVec,
                            True,optRadDiffAz,optRadDiffEl,zoomVal=zoomVal)
    [azOff,elOff] = optRadDiff(azVec,elVec,optRadDiffAz,optRadDiffEl,antVec)
    applyAzOff = [] ; applyElOff = [] ; applyAntVec = []
    [azValTemp,elValTemp] = azel(opticalSource)
    for i in range(len(antVec)) :
        if antVec[i] not in badOptical :
            applyAntVec.append(antVec[i])
            applyAzOff.append(azOff[i])
            applyElOff.append(elOff[i])
    incmountoffset(applyAzOff, applyElOff, applyAntVec)
    offset(0.0, 0.0)

    # Write results to file
    f=open("/home/obs/pointing/subOpticalInfo.txt",'a')
    timeStamp = getMiriadUTStamp()
    for i in range(len(applyAntVec)) :
        f.write("%20s %2i %10.3f %10.3f %10.3f %10.3f %10s\n " \
            %(timeStamp,applyAntVec[i],azValTemp,elValTemp,
              applyAzOff[i],applyElOff[i],opticalSource) )
    f.close()

    # Done
    return [azVec,azOff,elVec,elOff,applyAzOff,applyElOff,applyAntVec]

def deriveOptRadDiff(opticalSource, radioSource, antKey, 
                     antVec=None, applyRadio=True, zoomVal=6.0,
                     tune95=False, antwait=-2,
                     fakeRadio=False, antennas=None,
                     timeLimit=7, pntwait=2, waitCycles=3):
    """
    This derives the optical to radio offset vector.  In the case of 
    applyRadio=True, the standard default, the mount offsets are centered on 
    the radio source and this is the initiation of optical reference pointing.  
    If applyRadio=False, the offsets are calculated but not applied on the 
    radio side.
    """
    # Set up
    if VERBOSE:
        scriptlog("deriveOptRadDiff: antKey="+str(antKey)+" antVec="+str(antVec))
    goodRadio   = False
    goodOptical = False
    maxRepsOpt  = 1
    maxRepsRad  = 1
    countO=0
    countR=0
    if antVec == None : antVec = currentAntennaNumbers()
    antVec = helpers.makeList(antVec)
    antOther = getAntOther(antVec, antKey) # antVec with antKey removed
    # zeroMountOffsets()
    # Either peak up and apply the radio correction or just calculate it.
    while (not goodRadio) and (countR < maxRepsRad) :
        if not fakeRadio :
            if applyRadio : 
                refRes = radioPoint(radioSource,tune95=tune95,antwait=antwait,antennas=antennas, timeLimit=timeLimit, pntwait=pntwait, waitCycles=waitCycles)
                if refRes == None:
                    refRes = dict()
                    iWasNoneOnce=True
                else : iWasNoneOnce=False
            else :  
                scriptlog("WARNING: Call to white.multimap removed")
            sleep(1.1)
        else :
            print 'Using fake radio pointing'
            refRes=dict()
            for i in antVec : refRes[i]=[1,1,1,1]
            iWasNoneOnce = False
            sleep(1.1)
    # Get values of radio center, then get optical positions RELATIVE to those radio positions.
        print 'grabbing mount offsets'
        azVecR = dict()
        elVecR = dict()
        for k in refRes.keys() :
            azVecR[k] = refRes[k][2]
            elVecR[k] = refRes[k][3]
        countR=countR+1
        badValsR = []
        print 'constructing bad antennas.'
#
# This loop does not work for dictionary....refRes is good only!
# 
#        for i in refRes.keys() :
#            if refRes[i][0] == 0.0 and refRes[i][1] == 0.0 : badValsR.append(i)
        for i in currentAntennaNumbers() :
            if i not in refRes.keys() : badValsR.append(i)
#        badValsR = successOrNot(azVecR,elVecR,antVec)
        if len(badValsR) == 0 : goodRadio = True
        else : goodRadio = False
    print 'getting ready for key optical...'
    while (not goodOptical) and (countO < maxRepsOpt) :
        [optRadDiffAz,optRadDiffEl]=runKeyOpticalPeakUp(opticalSource,
                antKey,zoomVal=zoomVal)
        badValsO = successOrNot(optRadDiffAz,optRadDiffEl,antVec)
        countO = countO+1
        if len(badValsO) == 0 : goodOptical = True        
        else :
            m = 'Check telescopes ' + str(badValsO) + ' for flap problems.'
            print m
            scriptlog(m)
            goodOptical = False
    successVal = [goodRadio,goodOptical]
    if not goodRadio :
        print 'Telescopes ',badValsR,' had trouble in the radio.'
        print """Consider tuning to a lower frequency before 
             the call to deriveOptRadDiff and then retuning afterwards."""
    if not goodOptical :
        print 'Telescopes ',badValsO,' had trouble in the optical.'
        m  = "Consider checking for clouds or closed camera flaps " 
        m += "on the 6m antennas. "
        m += "Failures may be associated with the sun recently setting; " 
        m += "wait a bit and try again."
        print m
    f=open("/home/obs/pointing/optRadDiffInfo.txt",'a')
    timeStamp = getMiriadUTStamp()
    [azValTemp,elValTemp] = azel(opticalSource)
    [azT,elT] = azel(radioSource)
    #dist = short.distSky(opticalSource, radioSource)
    dist = distSky(opticalSource, radioSource)
    distCoord = ((azValTemp-azT)**2.0+(elValTemp-elT)**2.0)**0.5
    badValsT = badValsO+badValsR
    badVals  = []
    for i in badValsT :
        if i not in badVals and i <> 0 : badVals.append(i)
    for i in range(len(antVec)) :
        if antVec[i] not in badVals : 
            f.write("%20s %2i %10.3f %10.3f %10.3f %10.3f %5.1f %5.1f %10s %10s %10.3f %10.3f\n " % (timeStamp,antVec[i],azValTemp,elValTemp,optRadDiffAz[i],optRadDiffEl[i],dist,distCoord,opticalSource,radioSource,azVecR[antVec[i]],elVecR[antVec[i]]) )
    f.close()
    optRes = dict()
    for i in range(len(antVec)) :
        optRes[antVec[i]] = [optRadDiffAz[i],optRadDiffEl[i]]
    if iWasNoneOnce : refRes = None
    return [optRes,badVals,refRes]

def runParallelOPoint(antRef,antKey=None,reps=10000,zoomVal=6.0) :
    """This is the command that is run in the standalone xterms.
    It does an optical pointing and then waits for the next optical pointing to
    be signalled by the antKey antenna refraction/aperture going to radio.
    Parameters: 
     antRef: ant# to do optical pointing on"""
    if antKey==None : antKey = min(currentAntennaNumbers())
    myOpnt = s.getScriptString(odi.INDX_STR_OPOINT_KEY)
    scriptlog('Starting runParallelOPoint for CARMA %d' % antRef)
    if not amIHere(antRef) :
        scriptlog('runParallel: Quiting (1) optical pointing on CARMA %d' % antRef)
        return
    # Usually a semi-endless loop that waits/opoints 
    for i in range(reps) :
        scriptlog('runParallel: Starting  cycle %5d for CARMA %d' % (i, antRef))
        # Wait for antKey antenna to go to optical refract, then do an
        # optical pointing.
        goOptical(antRef, antKey, zoomVal=zoomVal, myOpnt=myOpnt)
        goRadio = 0
        while goRadio == 0 :
            if myOpnt <> s.getScriptString(odi.INDX_STR_OPOINT_KEY) :
                scriptlog('runParallel: Exiting optical pointing on CARMA %d' % antRef)
                return
            if not amIHere(antRef) : 
                scriptlog('runParallel: Quiting (2) optical pointing on CARMA %d' % antRef)
                return
            sleep(2.1)
            goRadio = checkRadio(antKey)
        scriptlog('runParallel: Completed cycle %5d for CARMA %d' % (i, antRef))
    scriptlog('Exiting runParallelOPoint for CARMA %d' % antRef)

#===========================================================
#====================== Helper code ========================
#===========================================================

def vectorDiff(vec1,vec2) :
    diffVec = []
    vec1=helpers.makeList(vec1)
    vec2=helpers.makeList(vec2)
    for i in range(len(vec1)) : diffVec.append(vec1[i]-vec2[i])
    return diffVec
    
def grabOffsets(antVec) :
    antVec = makeAntList(antVec)
    azVec = []
    elVec = []
    for i in antVec :
        if i < 7 :
            azVec.append(queryDouble('Ovro%i.AntennaCommon.Drive.Point.offsetAz' % i,24))
            elVec.append(queryDouble('Ovro%i.AntennaCommon.Drive.Point.offsetEl' % i,24))
        elif i > 15 :
            azVec.append(queryDouble('Sza%i.AntennaCommon.Drive.Point.offsetAz' % (i-15),24))
            elVec.append(queryDouble('Sza%i.AntennaCommon.Drive.Point.offsetEl' % (i-15),24))
        else :
            azVec.append(queryDouble('Bima%i.AntennaCommon.Drive.Point.offsetAz' % (i-6),24))
            elVec.append(queryDouble('Bima%i.AntennaCommon.Drive.Point.offsetEl' % (i-6),24))
    return [azVec,elVec]

def optRadDiff(azVec,elVec,azDiff,elDiff,antVec) :
    applyAz = vectorDiff(azVec,azDiff)
    applyEl = vectorDiff(elVec,elDiff)
    return [applyAz,applyEl]

def getAntOther(antVec, antKey) :
    """Return a new list that is antVec with antKey removed.
    Parameters:
     antVec: vector of antenna numbers; 0 gives all in subarray
     antKey: a single antenna number"""
    antVec   = makeAntList(antVec)
    antOther = antVec[:]
    antOther.remove(antKey)
    return antOther

def runKeyOpticalPeakUp(opticalSource, antKey=None, antVec=None,
        initOffset=False, optRadDiffAz=None, optRadDiffEl=None, 
        zoomVal=6.0):
    """Runs optical pointing on a single antenna: antKey, unless it is None,
    in which case the lowest value in antVec is used."""
        
    # Starting optical pointing
    print 'key optical starting'
    antVecIn = antVec
    antKeyIn = antKey
    if antVec == None : antVec = currentAntennaNumbers()
    else :              antVec = makeAntList(antVec)
    if antKey == None : antKey = min(antVec)
    antOther = getAntOther(antVec, antKey) #antVec with antKey removed
    if VERBOSE:
        scriptlog("runKeyOpticalPeakUp: antVec="+str(antVec)
                + "  antKey="+str(antKey)
                + " antOther="+str(antOther)
                + " antVecIn="+str(antVecIn)
                + " antKeyIn="+str(antKeyIn))
    for a in antOther:
        s.setScriptBool(odi.INDX_BOOL_OPNT_CARMA1 + a - 1, False)

    # Track source -- make sure we wait for antennas here,
    # so checksafe has a chance to cancel if necessary (E array only)
    track(opticalSource, waiton=ALL)
    print "TRACK issued on all antennas"
    if initOffset : 
        offset(optRadDiffAz,optRadDiffEl,antVec,10,ALL) 
        print "Issued optRadDiff OFFSETS for all antennas because initOffset=T"
 
    # Change aperture to optical from radio
    sleep(1.1)
    changeToOptical(antKey)
    s.setScriptString(odi.INDX_STR_OPNT_SOURCE, opticalSource)
    
    [ncoadd, subtractBackground, centroidLoopMax, minsamples] = \
        getOpticalPointParameters()
    #slewEst = short.slewSim(opticalSource,telescope=antKey)*60.0
    slewEst = slewEstimate(opticalSource, ant=antKey)*60.0
    slewTmo = max([slewEst*1.40,60.0])
    str = "  **debug: C%d slewEst=%.1f, slewTmo=%.1f" %(antKey,slewEst,slewTmo)
    scriptlog(str)
    # OK, a second track command! Probably to see if it completes sucessfully.
    # The track command always zeros out the offset()'s
    trackRes = track(opticalSource,antKey,tmo=slewTmo,waiton=ALL)
    print "TRACK on C%d" %antKey
    trackOk = (antKey in trackRes.ready)
    if trackOk:
        scriptlog('track command completed for CARMA %d' % antKey)
    else:
        trackRes = list()
        scriptlog('track command failed for CARMA %d' % antKey)
    if initOffset : 
        offset(optRadDiffAz,optRadDiffEl,antVec,10,ALL) 
        print "Issued optRadDiff OFFSETS for all antennas because initOffset=T"
        for i in range(len(antVec)):
            print "  offset(%.2f, %.2f, %d)" %(optRadDiffAz[i], optRadDiffEl[i], antVec[i]) 
    sleep(1.1)                                               
    if trackOk:
        centroidReturn = opticalSystem(ant=antKey, 
             auto=True,object=opticalSource,repeat=6,
             zoom=zoomVal,showGUI=False,dontClose=True,brightness=50,
             ncoadd=ncoadd, subtractBackground=subtractBackground,
             centroidLoopMax=centroidLoopMax, minsamples=minsamples)
        if centroidReturn[4] == False:
            print "Optical centroid failed on C%d" %antKey
    # Wait for completion of optical pointing on other antennas
    monitorRadio(antOther)
    s.setScriptString(odi.INDX_STR_OPNT_SOURCE, "")
    sleep(1.1)
    [azVec,elVec] = grabOffsets(antVec)
    sleep(1.1)
    changeToRadio(antKey)
    #print azVec,elVec
    print "set OFFSETS(0,0) for all ants"
    offset(0.0,0.0,antVec)
    return [azVec,elVec]

def monitorRadio(antVec) :
    """ Loop and wait for optical pointing to complete on all ants in antVec.
    """
    # Set wait time for optical pointing to converge
    # slew time + 3 minutes. The maximum allowed time is 10 minutes.
    twait = 120.0
    if azel('sun')[1] > 0.0: twait += 60.0
    tstart = time.time()

    # Initialize
    stillPointing = True
    badopt = list()
    tprint = 20
    tsleep = 2.0
    t = 0

    # Wait for script state for opoint of other antennas to be updated
    sleep(tsleep)
    # Start loop
    print ''
    print 'Waiting for the other antennas to finish.'
    print 'Will wait for maximum of ',twait,' seconds (',time.asctime(),').'
    while time.time() - tstart < twait and stillPointing:
        # Check all indices
        stillPointing = False

        # Check to see if antennas are still performing optical pointing
        antVec = makeAntList(antVec)
        badopt = []
        for i in antVec : 
            j = odi.INDX_BOOL_OPNT_CARMA1 + i - 1
            if s.getScriptBool(j) and i <= 15: badopt.append(i)
        stillPointing = (len(badopt) > 0)

        # Pause for a bit
        if stillPointing: 
            if t == 0: print '   ... waiting for CARMA ', str(badopt)
            sleep(tsleep)
            t += tsleep
            if t >= tprint: t = 0
    if not stillPointing: print 'Optical pointing completed'

    # If still in optical, then one of the antennas may be stuck
    for i in badopt : 
        trackMessage('ERROR: Optical pointing did not converge on CARMA %d' % i)
        changeToRadio(i)

def goOptical(antRef,antKey,zoomVal=6.0,myOpnt=None):
    """Used by antennas that are doing optical pointing in parallel with the
    key antenna. First waits for the antKey antenna to go to optical refraction,
    then tracks the optical source, then does an optical pointing."""
    goOpt = 0
    while goOpt == 0 :
        sleep(2.1)
        goOpt = checkOptical(antKey)
        if myOpnt <> s.getScriptString(odi.INDX_STR_OPOINT_KEY) : 
            scriptlog('goOptical1: Exiting optical pointing on CARMA %d' % antRef)
            return
        if not amIHere(antRef) : 
            scriptlog('goOptical1: Quiting optical pointing on CARMA %d' % antRef)
            return
    scriptlog('Changing to optical aperture for CARMA %d' % antRef)
    changeToOptical(antRef)
    sleep(4.1)
    if antKey < 7 :    opticalSource = queryString('Ovro%i.AntennaCommon.Drive.sourcename' % antKey,24)
    elif antKey < 16 : opticalSource = queryString('Bima%i.AntennaCommon.Drive.sourcename' % (antKey-6),24)
    countcountcount=0

    while opticalSource.lower() <> s.getScriptString(odi.INDX_STR_OPNT_SOURCE).lower():
        if myOpnt <> s.getScriptString(odi.INDX_STR_OPOINT_KEY) : 
            scriptlog('goOptical2: Exiting optical pointing on CARMA %d' % antRef)
            return
        if not amIHere(antRef) : 
            scriptlog('goOptical2: Quiting optical pointing on CARMA %d' % antRef)
            return
        sleep(0.5)
        if antKey < 7 : opticalSource = queryString('Ovro%i.AntennaCommon.Drive.sourcename' % antKey,24)
        elif antKey < 16 : opticalSource = queryString('Bima%i.AntennaCommon.Drive.sourcename' % (antKey-6),24)
        countcountcount=countcountcount+1
    [ncoadd, subtractBackground, centroidLoopMax, minsamples] = \
        getOpticalPointParameters()
    slewEst = slewEstimate(opticalSource, ant=antRef)*60.0
    slewTmo = max([slewEst*1.40,60.0])
    str = "  **debug: C%d slewEst=%.1f, slewTmo=%.1f" %(antRef,slewEst,slewTmo)
    scriptlog(str)
    scriptlog('Grabbing existing offsets for CARMA %d' % antRef)
    # Grab the offsets before the track command zero's them out...
    print 'Ant ',antRef,' slewing to source ',opticalSource,' with TMO = ',slewTmo,' seconds.'
    optRadDiffAzRef=queryCommonDouble('Drive.Point.offsetAz', antRef, 6)    
    optRadDiffElRef=queryCommonDouble('Drive.Point.offsetEl', antRef, 6)  
      
    scriptlog('Tracking optical source for CARMA %d' % antRef)
    trackRes = track(opticalSource, antRef, tmo=slewTmo, waiton=ALL)
    scriptlog('track command return for CARMA %d' % antRef)
    print "TRACK on C%d" %antRef
    trackOk = (antRef in trackRes.ready)
    if trackOk:
        scriptlog('track command completed for CARMA %d' % antRef)
    else:
        trackRes = list()
        scriptlog('track command failed for CARMA %d' % antRef)
    # Put offsets back into the antenna
    offset(optRadDiffAzRef, optRadDiffElRef, ants=antRef)
    print "OFFSET(%.2f, %.2f) on C%d" %(optRadDiffAzRef, optRadDiffElRef,antRef)
    if trackOk:
        scriptlog('Starting optical pointing for CARMA %d' % antRef)
        print 'Starting optical pointing on Ant ',antRef
        centroidReturn = opticalSystem(ant=antRef,
            auto=True,object=opticalSource,repeat=6,zoom=zoomVal,
            showGUI=False,dontClose=True,brightness=50,
            ncoadd=ncoadd, subtractBackground=subtractBackground,
            centroidLoopMax=centroidLoopMax, minsamples=minsamples)
        print 'Completed optical pointing on CARMA ',antRef
        scriptlog('Completed optical pointing for CARMA %d' % antRef)
        if centroidReturn[4] == False:
            m = "Optical centroid failed on C%d" %antRef
            print m
    else:
        print 'track() command not successful for Ant ',antRef
        scriptlog('track() command not successful for CARMA %d' % antRef)
        scriptlog('Changing to radio aperture for CARMA %d' % antRef)
    # Change to radio aperture and signal completion to 
    #  monitorRadio() => runKeyOpticalPointing() => subOptical()
    changeToRadio(antRef)
    scriptlog('Completed goOptical for CARMA %d' % antRef)

def getOpticalPointParameters():
    """ Returns optical pointing parameters stored in script variables """

    ncoadd = s.getScriptInt(odi.INDX_INT_NCOADD)
    centroidLoopMax = s.getScriptInt(odi.INDX_INT_CENTROID_LOOP_MAX)
    subtractBackground = s.getScriptBool(odi.INDX_BOOL_SUBTRACT_BKG)
    minsamples = s.getScriptInt(odi.INDX_INT_MINSAMPLES)
    if minsamples == 0: minsamples = None

    if ncoadd == 0: ncoadd = 1
    if centroidLoopMax == 0: centroidLoopMax = 16

    return [ncoadd, subtractBackground, centroidLoopMax, minsamples]

def changeToOptical(antRef) :
    antRef = makeAntList(antRef)
    for a in antRef:
        i = odi.INDX_BOOL_OPNT_CARMA1 + a - 1
        s.setScriptBool(i, True)
    radioAperture(False,antRef)

def changeToRadio(antRef) :
    """This changes to the radio aperture and also signifies that optical
    pointing has completed by setting the OPNT script variable to False."""
    antRef = makeAntList(antRef)
    for a in antRef:
        i = odi.INDX_BOOL_OPNT_CARMA1 + a - 1
        s.setScriptBool(i, False)
    radioAperture(True, antRef)
    camera(OFF,antRef)

def checkOptical(antKey) :
    if antKey < 7 : keyString = ('Ovro%i.Drive.Point.refractionModel' % antKey)
    elif antKey < 16 : keyString = ('Bima%i.AntennaCommon.Drive.Point.Constants.selectedApert' % (antKey-6))
    aperUsed = queryInt('%s' % keyString,20)
    if aperUsed == 0 : return 1
    else : return 0

def checkRadio(antKey) :
    if antKey < 7 : keyString = ('Ovro%i.Drive.Point.refractionModel' %antKey)
    elif antKey < 16 : keyString = ('Bima%i.AntennaCommon.Drive.Point.Constants.selectedApert' % (antKey-6))
    aperUsed = queryInt('%s' % keyString,20)
    if aperUsed == 0 : return 0
    else : return 1

def successOrNot(vec1,vec2,antVec) :
    vec1=helpers.makeList(vec1) 
    vec2=helpers.makeList(vec2) 
    antVec=helpers.makeList(antVec)
    badList = []
    for i in range(len(vec1)) :
        if (vec1[i] == 0.0) and (vec2[i]==0.0) :
            badList.append(antVec[i])
    return badList
    
def amIHere(antref,checkList=None) :
    """Checks if antref is in checkList or current array.
    Parameters
     antref: antenna to check
     checkList: list or dictionary keys of antenna numbers
      default=None => ants in current array
    Returns True if antref is in checkList, False if not.
    """
    if checkList==None : checkList=currentAntennaNumbers()
    if list not in [type(checkList)] :
        if dict not in [type(checkList)] : checkList=[checkList]
        else : checkList=checkList.keys()
    if antref in checkList : return True
    else : return False

