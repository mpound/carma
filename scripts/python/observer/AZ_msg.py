"""
This file contains shortcut commands that can be used in sac.  If you
are confused because this looks different, check short.prejan22.py
as many changes were made to this file.  

"""

import carma
import Subarray
import math as m
from device import *
import time
import os,sys
import glob
from subarrayCommands import *
import carmaHelpers as helpers
import fileIOPython as fIOP
reload(fIOP)
s = Subarray.getSubarray()


"""
These early commands are largely just alias commands for more extensive
lists of commands.  For example, rpointing() contains the optical-radio
offsets.  These in principle can be added in to 3mm aperture coefficients
in subarrayInit.py but until we verify that they are the best values we
maintain these offsets.

11-30-2005: S. Corder: Someone feeling particularly interested could find
a reasonable monitor point for testing to see if the calwheel is moving
and sleeping the system while it is moving.  The exception currently handles
timeouts.

12-16-2005: A. Beard: There currently is no such monitor point due to the
fact that we don't have an optics module yet.  I believe the only way to tell
if the calibration wheel is being moved or not is to monitor power levels
from the downconverters.

1-22-2006: S. Corder: Major overhaul to utilize the changes to
/opt/rt/scripts/python/shortcuts.py.  ALOT changes here so see the pre
jan22 version if you want something that was in here a long time ago.
You'll notice no tobsSource, no tintegrateWait, and a great deal of
shorter code! As I have removed some indications of credit, this file
had contributions from A. Baker, P. Teuben, A. Beard and K. Sheth.
I have now tested them all and they work fine except for incOffset which
needs some work.

2-3-2006: incOffset won't ever work again.  It's utility is basically gone.

2-6-2006: S. Corder: Added writeTsys and waitRise functions as they
will be used by other programs so utility is more suited to short.

4-18-2006: S. Corder: Began overhaul of calls to eliminate definitions which
have become more or less empty wrappers to things in subarrayCommands.  Sniff,
sniff.  If you expect a wrapper to be here, check for a similar one in 
subarrayCommands.py. Add a beam offset command as well.

5-17-2006: S. White Added incrementTrial, ymdObsblock

6-7-2006: S. Corder Added radioInit

6-20-2006: S. Corder Added a telescopes not ready yet function for use
in several places.

9-21-2006: S. Corder Added multiIntent to do quick intent labelling of
multiple sources.  Marc is going to put this in subarrayCommads eventually
but he is busy so you get me, deal with it :)

This first set of commands are basically wrappers for sets of setting
commands which are often used together.  You can obviously just enter
these directly but they are sometimes time saving.
"""

def radioInit() :
    """This command is useful for getting ready for radio observations.
    It takes no arguments.  It does the following for all antennas in the
    subarray:
    1. Turns on Tsys and flux calibration.
    2. Resets the antenna tracking thresholds to the default.
    3. Resets the elevation limit to 15 degrees.
    4. Takes the calibrator wheel out of the optical path.
    5. Turns the noise source off.
    6. Zeros out the offsets (BUT NOT MOUNT OFFSETS).
    7. Changes the aperture to radio.
    8. Unsets the doppler source.
    9. Unloads the user catalog.
    """
    print "Beginning radioInit"
    applyTsys(True)
    applyFlux(True)
    trackThreshold(0.1,0)
    elevlimit(15)
    sky()
    noiseoff()
    zeroMountOffsets()
    offset(0.0,0.0)
    camera(OFF)
    ucat('NONE')
    print "radioInit finished."

def multiIntent(sourceList,intentFlags='O',selfCal=False,fastSwitch=False) :
    """Makes a list of sources have the same intent and flags."""
    sourceList=helpers.makeList(sourceList) # verifies list nature of sourceList
    for i in sourceList : intent(i,intentFlags,selfCal,fastSwitch)
    

def sendMsg(pC,principalInvestigator,email,trackFileName,start=0) :
    startlst=lst()
    lstsym=str(int(startlst))+":"+str(int(60*(startlst-int(startlst))))
    date=str(yearMonthDay())
    msg  = 'To: '+email+'\n'
    msg += 'From: obs@mmarray.org\n'

    if start == 0:
        msg += 'Subject: Your track has been run.\n\n'
        msg += 'Attn:  '+principalInvestigator+'\n\n'
        msg += 'Project '+pC+' ended at '+lstsym+'LST.\n'
        msg += 'Date: '+date+'\n\n'
        msg += 'The edited file that the observers ran is: '+trackFileName+'\n'
        msg += '*****This email was automatically generated and sent at the completion of your track.*****\n'
    else:
        msg += 'Subject: Your track has just started running.\n\n'
        msg += 'Attn:  '+principalInvestigator+'\n\n'
        msg += 'Project '+pC+' started at '+lstsym+'LST.\n'
        msg += 'Date: '+date+'\n\n'
        msg += 'The edited file that the observers ran is: '+trackFileName+'\n\n'
        msg += 'Send observers feedback at obs@mmarray.org\n\n'
        msg += '*****This email was automatically generated and sent '
        msg += 'at the beginning of your track.*****\n'
    # /usr/bin/sendmail
    MAIL = "/usr/sbin/sendmail"
    if (False ) :  # for debugging
        f = open("test.txt",'w')
        f.write(msg)
        f.close()
    p = os.popen("%s -t" % MAIL, 'w')
    p.write(msg)
    exitcode = p.close()
    if exitcode: print "Exit code from sendmail: %s" % exitcode

def oInt(inttime=10,nints=1,source=None,track=True,type='BFG',quickInt=False):
    if source <> None : track(source)
    else:
        source=queryString('Control.Subarray1.Commands.Track.sourcename')
        if source == None :
            return 'Source name not already defined for control and no source given.  Provide source name.'
    newProject('fringe', 'dwc', yearMonthDay() , 1 )
    intent(source, type, True )
    constraints()
    if quickInt : qinteg(inttime,nints)
    else : integrate(inttime,nints)
    resetProject()
    
def qoInt(inttime=10,nints=1,source=None,track=True,type='BFG'):
    oInt(inttime,nints,source,track,type,True)
    
def quickTest():
    newProject('fringe', 'dwc', yearMonthDay() , 1 )
    source=getPointSource()
    intent(source, 'BFG', True )
    intent('NOISE', 'BFG', True )
    constraints()
    freq(95,source)
    run('tune95')
    run('tune95')
    track(source)
    integrate(10,12)
    tnoiseInt(2.0,15)
    resetProject()
    
def newObsblock(projectName,valObsblock,valSubObsblock='',valTrial=1) :
    """This program is a short cut to making new observing blocks in
    a single line.  It sets the full obsblock and takes these inputs:
    projectName: Usually a ct or cx number (sometimes the date). No default.
    valObsblock: Usually a source name or commissioning id.  No default.
    valSubObsBlock: Often the date or some other qualifier. Default is blank.
    valTrial: Must be an integer.  Default is 1
    """
    s.project(projectName)
    s.obsblock(valObsblock)
    s.subObsblock(valSubObsblock)
    s.trial(valTrial)

def incrementTrial(projectName,valObsblock,valSubObsblock='',allSubs=False) :
    """Usage:   short.incrementTrial(projectName,obsBlock,subObsblock)
    or:    short.incrementTrial(projectName,obsBlock)
    or:    short.incrementTrial(projectName,obsBlock,allSubs=True)
    Looks for XML files named projectName.obsBlock.subObsblock.trial.xml* in the
       usual directory: if no such file, returns 1, otherwise returns trial+1 to 
       increment trial number.
    Examples assume that you have loaded the short module as 'import short'; if 
       you use 'from short import *', then you can drop the 'short.'.
    Set allSubs=True if you want to increment trial number regardles of subObsblock label.convertAntPosFile('antpos.061225.s.try1')
    Used by ymdObsblock to set yyyymmdd.obs.sub.trial name, see below."""

    # handle various subObsblock options via a variable label, index for trial location
    if (allSubs):
       subLabel=''
       argTrial=3
    elif (len(valSubObsblock) == 0):
       subLabel=''
       argTrial=2
    else:
       subLabel=valSubObsblock
       argTrial=3

    # first make sure there is a file: if not, we are done
    if (len(glob.glob('/opt/sdp/astroheader/SLCorrelIntegrated/astrohdr_'+projectName+'.'+valObsblock+'.'+subLabel+'*.xml*')) == 0 ) :
       trial=1
    else:
       # list the files via Peter's favored popen
       flst=os.popen('ls /opt/sdp/astroheader/SLCorrelIntegrated/astrohdr_'+projectName+'.'+valObsblock+'.'+subLabel+'*.xml*')
       # convert the object to a list
       alst=flst.readlines()
       # now that we have the list, close the pipe as early as possible
       flst.close()
       # need an array to hold existing trial numbers
       nf=len(alst)
       valTrials=list([0])
       # use the list append method: only append values that convert to integers
       # use try to make sure trial converts to a valid integer
       for files in alst:
          try: int(str(files).strip('\n').strip('/opt/sdp/astroheader/SLCorrelIntegrated/astrohdr_').split('.')[argTrial])
          except ValueError: valTrials.append(0)
          else: valTrials.append(int(str(files).strip('\n').strip('/opt/sdp/astroheader/SLCorrelIntegrated/astrohdr_').split('.')[argTrial]))
       trial=max(valTrials)+1
    return trial

def ymdObsblock(valObsblock,valSubObsblock='') :
    s.project(yearMonthDay())
    s.obsblock(valObsblock)
    s.subObsblock(valSubObsblock)
    s.trial(incrementTrial(yearMonthDay(),valObsblock,valSubObsblock))

def focusXYZ(focX,focY,focZ,antref) :
    s.focusX(focX,CarmaAnt().getName(antref))
    s.focusY(focY,CarmaAnt().getName(antref))
    s.focusZ(focZ,CarmaAnt().getName(antref))

def focusInit(antref) :
    if antref == None : antref = currentAntennaNumbers()
    antref = checkAntVectorType(antref)
    focusVals = getInitFocus()
    focusToPass = []
    for i in antref : focusToPass.append(focusVals[i-1])
    focusWait(focusToPass,antref)
    
def getInitFocus() :
    os.system("rm tempFocus")
    os.system("grep 's.focusZ(' /array/rt/scripts/subarrayInit.py > tempFocus")
    focusInfo = fileIOPython.fileToTable("tempFocus")
    focusVals = [0.0]*15
    for i in range(len(focusInfo)) :
        print focusInfo[i][1][0:-1],focusInfo[i][2][0:-1]
        try: 
           focusTemp = float(focusInfo[i][1][0:-1])
           antTemp = int(focusInfo[i][2][0:-1])
           focusVals[antTemp-1] = focusTemp
        except: print "Invalid line format.  Line not processed."
    return focusVals
        
def focusWait(focusVals,antref,maxCount=10000) :
    antref = checkAntVectorType(antref)
    focusVals = helpers.makeList(focusVals)
    for i in range(len(antref)) : s.focusZ(focusVals[i],antref[i])
    sleep(2)
    counter = 1
    waitFocus = [1]*len(antref)
    while ((max(waitFocus) > 0) and (counter < maxCount)):
        sleep(1)   
        if not counter%30 :
            for i in range(len(antref)) : s.focusZ(focusVals[i],antref[i])
        waitFocus = getFocusState(antref)
        counter=counter+1

def getFocusVals(antref) :
    antref = checkAntVectorType(antref)
    focusVals = []
    for i in antref : 
        if i < 7 :  queryVal = "Ovro%i.Secondary.zPosition" % i
        elif i<16 : queryVal = "Bima%i.AntennaCommon.Secondary.focusZ" % (i-6)
        else :      queryVal = "Sza%i.AntennaCommon.Secondary.focusZ"  % (i-15)
        focusVals.append(queryDouble(queryVal,20))
    return focusVals

def getFocusState(antref) :
    antref = checkAntVectorType(antref)
    focusState = []
    for i in antref :
        if i < 7  : queryVal = "Ovro%i.Secondary.zStatus" % i
        elif i<16 : queryVal = "Bima%i.AntennaCommon.Secondary.focusState" % (i-6)
        else : queryVal = "Sza%i.AntennaCommon.Secondary.focusState" % (i-15)
        focusState.append(queryInt(queryVal,20))
    return focusState

def convertAntPosFile(fileName) :
    """Usage:
    Filename contains a miriad antenna position file which is converted
    to the format needed for direct input into either the control system
    (sac/sci1/ etc) or subarrayInit.py (if it is still around).

    Output: Text of the form padOffset(X,Y,Z,ant#)

    Authors: J. Koda, S. Corder
    """
    antPosVals = fIOP.fileToTable(fileName)
    antAssign  = s.getAntennaAssignments()
    antVec = range(len(antAssign))
    for j in antVec :
        i = antAssign[antVec[j]].carmaAntennaNo
        baseOff = s.convertBaseline(float(antPosVals[j][0]),float(antPosVals[j][1]),float(antPosVals[j][2]),i)
        print ('s.padOffset(%0.2f,%9.2f,%9.2f,%3i)' % (baseOff.east,baseOff.north,baseOff.up,i) )
        
def displayDelay(slopeUsb,slopeLsb,antNb,applyDelay=False) :
    """Usage:
    Input: slopeUsb and slopeLsb are the slopes of a given baseline from
    a cdv plot.
           antNb is the antenna number in question.
           applyDelay: If True, sends new delays to control system.  Default
           is False.
    Output: New delay is displayed and return to the user.
    Notes: Make sure you reference you slopes to Ovro 1 the delay reference
    for the array.  Also, watch for wrapping across the band which will
    pollute your measurement.  If you have one sideband which wraps, you can
    simply put minus 1 times the value in for the other side band with
    little negative consequences.

    Authors: M. Hamidouche, S. Corder
    """
    if (antNb < 24 ) : 
        sign= abs(slopeUsb)/slopeUsb
        delta = sign*((abs(slopeUsb)+abs(slopeLsb))/2.0)*15.0/0.5/360.0
        oldDelay = queryDouble('DelayEngine.DelayData%i.delayOffset'%antNb,20)
        if (oldDelay >= 0) :
            print 'Old Delay = %7.3f ns' % oldDelay
            delay = oldDelay + delta
            print 'New Delay = %7.3f ns ; Delta = %7.3f ns' % (delay,delta)
            if applyDelay :
                s.delay(delay,antNb)
                return 'New delay set.'
            else : return
        else : return 'Can not get the old delay for antenna',antNb,' ; (Delta = ',delta,' ns)'
    else : return ' ERROR: Antenna Number must be less than 24 !'
    
""" Here we have telescope pointing motions, to point to specific
locations or account for specific pointing offsets that have yet to be
commited to the control system (as rpointing is). Rpointing will soon
be antiquated as coefficients are moved into the radio aperture
coefficients. As you see, rpointing() now contains all zero entries.
The values have been ported to aperture coefficients in subarrayInit.
The new command mountOffset is maintained to track small scale changes
in collimation (much like those below) while their behavior is determined
to be transient or requires inclusion in the pointing model.

Obstone is reasonable locations for the transmitter on the microwave hill
Stowall takes all the telescopes to stow...as sometimes the bima
dishes do not respect stow.

As for the snow commands, there are two.  If you can, you should use
snowtrack.  It takes the number of hours you wish to execute snow
tracking.  Two hours is the time scale over which the observer should
be getting up to check the system for snow accumulation so 2 hours is
a good number (thus it is the default...).  It basically reads the
wind direction from the weather station every five minutes and adjusts
the directions accordingly.  IF THE WEATHER STATION IS NOT WORKING
YOU SHOULD USE snowstow.  Here you must estimate the direction the wind
is out of and give it as input.  This does NOT actively update
thus snowtrack is better.
"""

def rpointing() :
    radioOff = [[ 0.00, 0.00,1 ],[ 0.00, 0.00,2 ],\
               [  0.00, 0.00,3 ],[ 0.00, 0.00,4 ],\
               [  0.00, 0.00,6 ],[ 0.00, 0.00,8 ],\
               [  0.00, 0.00,9 ],[ 0.00, 0.00,13],\
               [  0.00, 0.00,15]               ]
    antsInSub = currentAntennaNumbers()
    for i in range(len(radioOff)) :
        if radioOff[i][2] in antsInSub : s.offset(radioOff[i][0],radioOff[i][1],[radioOff[i][2]])

def obstone(goOptical=True) :
    """The TONE was moved Sept 2006 from Carma Hill to Microwave Link Hill
    Positions of all but C5,C9 and C13 have been optimized Oct 7 by
    finding the tone box in the optical, offseting apertures and then
    peaking up.  
    """
# THE TONE HAS MOVED - as of when?  sep 2006 ? !!!
# C5  obstructed by C1
# C9  obstructed by C10
# C13 obstructed by C8
# C4  obstructed by C8, but it can be moved to az+90 and is fine
#
    toneLocOpt = [[160.0,  3.35,  1],
                 [160.360,3.335, 2],
                 [159.756,3.300, 3],
                 [160.210,3.330, 4],
                 [160.0,  6,     5],
                 [160.060,3.300, 6],
                 [160.140,3.350, 7],
                 [160.087,3.350, 8],
                 [160.0,  6,     9],
                 [159.895,3.360,10],
                 [159.794,3.360,11],
                 [159.730,3.340,12],
                 [160.0,  6,    13],
                 [160.193,3.342,14],
                 [160.322,3.345,15]]
    toneLocOff = [[0.00,  0.00,  1],
                  [ 0.00,  0.00,  2],
                  [ 0.00,  0.00,  3],
                  [ 0.00,  0.00,  4],
                  [ 0.00,  0.00,  5],
                  [-0.85,  0.00,  6],
                  [ 0.00,  0.00,  7],
                  [-0.30,  1.05,  8],
                  [ 0.00,  0.00,  9],
                  [ 0.00,  0.00, 10],
                  [ 0.15,  0.15, 11],
                  [ 0.25, -1.00, 12],
                  [ 0.00,  0.00, 13],
                  [ 1.00, -0.25, 14],
                  [ 1.05,  0.50, 15]
                  ]
    print "qMoving to the TONE (transmitter) at the microwave link position"
    print "at around AZ=160 EL=3"
    antsInSub = currentAntennaNumbers()
    move(160.0,5.0)
    for i in range(len(toneLocOpt)) :
        if toneLocOpt[i][2] in antsInSub:
            if goOptical:
               qmove(toneLocOpt[i][0],toneLocOpt[i][1],[toneLocOpt[i][2]])
            else :
               qmove(toneLocOpt[i][0]+toneLocOff[i][0]/60.0,toneLocOpt[i][1]+toneLocOff[i][1]/60.0,[toneLocOpt[i][2]])

def snowstow(windOutOf) :
    """In this situation, make sure you have ALL the antennas capable of moving in the 
    subarray used to execute this command.  If you must, move telescopes by hand to
    the appropriate direction but generally, all antennas should be made part of this
    subarray!
    The input should be the direction the wind is out of, not the direction it is blowing into!
    """
    print snowstow.__doc__
    
    [ovroAnts,bimaAnts,szaAnts] = separateAntType()
    ovroStowPosT =windOutOf+60.0
    if windOutOf < 180.0 : windDirection=windOutOf+180.0
    else : windDirection=windOutOf-180.0
    if ovroStowPosT < 180.0 : ovroStowPos = ovroStowPosT+180.0
    else : ovroStowPos = ovroStowPosT-180.0
    qmove(ovroStowPos,20.0,ovroAnts)
    qmove(windDirection,20.0,bimaAnts)

def windstow(windOutOf) :
    """In this situation, make sure you have ALL the antennas capable of moving in the 
    subarray used to execute this command.  If you must, move telescopes by hand to
    the appropriate direction but generally, all antennas should be made part of this
    subarray!
    The input should be the direction the wind is out of, not the direction it is blowing into!
    """
    print windstow.__doc__
    
    [ovroAnts,bimaAnts,szaAnts] = separateAntType()
    if windOutOf < 180.0 : windDirection=windOutOf+180.0
    else : windDirection=windOutOf-180.0
    stow(ovroAnts)
    qmove(windDirection,10.0,bimaAnts)

def snowtrack(lengthTime=4.0) :
    """In this situation, make sure you have ALL the antennas capable of moving in the 
    subarray used to execute this command.  If you must, move telescopes by hand to
    the appropriate direction but generally, all antennas should be made part of this
    subarray!"""
    print snowtrack.__doc__
    
    [ovroAnts,bimaAnts,szaAnts] = separateAntType()
    lengthTimeSec = lengthTime*60.0*60.0
    repCheck = int(lengthTimeSec/300.0)
    for i in range(repCheck) :
        try: windOutOf = queryDouble('Weather.windDirection',24)
        except:
            alarmon()
            return 'Script failed due to 24 consequtive bad frames.'
        ovroStowPosT = windOutOf+60.0
        if windOutOf < 180.0 : windDirection=windOutOf+180.0
        else : windDirection = windOutOf-180.0
        if ovroStowPosT < 180.0 : ovroStowPos = ovroStowPosT+180.0
        else : ovroStowPos = ovroStowPosT-180.0
        qmove(ovroStowPos,20.0,ovroAnts)
        qmove(windDirection,20.0,bimaAnts)
        sleep(300)

def windtrack(lengthTime=4.0) :
    """In this situation, make sure you have ALL the antennas capable of moving in the 
    subarray used to execute this command.  If you must, move telescopes by hand to
    the appropriate direction but generally, all antennas should be made part of this
    subarray!"""
    print windtrack.__doc__
    
    [ovroAnts,bimaAnts,szaAnts] = separateAntType()
    lengthTimeSec = lengthTime*60.0*60.0
    repCheck = int(lengthTimeSec/300.0)
    for i in range(repCheck) :
        try: windOutOf = queryDouble('Weather.windDirection',24)
        except:
            alarmon()
            return 'Script failed due to 24 consequtive bad frames.'
        if windOutOf < 180.0 : windDirection=windOutOf+180.0
        else : windDirection = windOutOf-180.0
        stow(ovroAnts)
        qmove(windDirection,20.0,bimaAnts)



"""
These two routines deal with sorting through antenna types.
checkAntVectorType was moved to shortcuts by Steve as makeList, but I
keep checkAntVectorType as it has the additional function that it
actually generates a vector 1 to 15 for all antennas instead of
allowing 0 for input.  Steve's version is more appropriate for
the implementation of subarrays as 0 can have a specific meaning
for a subarray.  Be cautious of the difference between
checkAntVectorType and makeList as some operations do very special
things for lists of antennas verses specifically 0
(e.g. subarrayCommands.track and s.equatOffset)
"""

def checkAntVectorType(antref) :
    if antref==0 or antref==[0] : antref = currentAntennaNumbers()
    if list in [type(antref)] : antrefVector = antref
    else : antrefVector = [antref]
    return antrefVector

def separateAntType(antref=0) :
    ovroAnts = []
    bimaAnts = []
    szaAnts = []
    antref = checkAntVectorType(antref)
    for i in antref :
        if i < 7 : ovroAnts.append(i)
        elif (i < 16) : bimaAnts.append(i)
        elif (i < 24)  : szaAnts.append(i)
        else : print '%s is a bogus antenna number!' % i
    return [ovroAnts,bimaAnts,szaAnts]

def removeAntNotInSubarray(antList=0) :
    """Takes an antenna list and removes antennas from the list that are
    not in the current subarray and returns the abbreviated list.
    """
    antList = checkAntVectorType(antList)
    subList = []
    for i in antList :
        if i not in currentAntennaNumbers() : subList.append(i)
    removeAntenna(subList)
    return subList

def addAntennaNotInSubarray(antList=0) :
    antList = checkAntVectorType(antList)
    subList = []
    for i in antList :
        if i in currentAntennaNumbers() : subList.append(i)
    addAntenna(subList)
    return subList

def giveNotReady(antNotReady,task) :
    if len(antNotReady) == 1: return ('Telescope %s did not complete %s.' % (antNotReady[0],taskList(task)))
    elif len(antNotReady) > 1 : return ('Telescopes %s did not complete %s.' % (antNotReady,taskList(task)))
    else : return ('All telescopes completed %s.' % taskList(task))

def updateTsys(antref) :
    antVec = helpers.makeList(antref)
    resVec = amb(antVec,tmo=30)
    giveNotReady(resVec.notready,'cal')
    sleep(3.1)
    resVec = sky(antVec,tmo=30)
    giveNotReady(resVec.notready,'cal')

def taskList(task) :
    if task == 'cal' : return 'calibrator wheel move'
    else : return 'INVALID'

def vectorDiff(vec1,vec2) :
    diffVec = []
    vec1=helpers.makeList(vec1)
    vec2=helpers.makeList(vec2)
    for i in range(len(vec1)) : diffVec.append(vec1[i]-vec2[i])
    return diffVec

def vectorAdd(vec1,vec2) :
    addVec = []
    vec1=helpers.makeList(vec1)
    vec2=helpers.makeList(vec2)
    for i in range(len(vec1)) : addVec.append(vec1[i]+vec2[i])
    return addVec

def checkConverge(vec1,vec2,convergeCrit=0.05) :
    vec1=helpers.makeList(vec1)
    vec2=helpers.makeList(vec2)
    diffVec = vectorDiff(vec1,vec2)
    for i in range(len(diffVec)) :
        if diffVec[i] < 0 : diffVec[i]=-diffVec[i]
    if abs(max(diffVec)) < convergeCrit : return True
    else : return False

def getFluxSource(elMax=82.0) :
    fluxSource = ['uranus','mars','neptune']
    elVal = []
    sourceUp = []
    for i in fluxSource :
        elVal.append(azel(i)[1])
        sourceUp.append(isup(i))
    if max(sourceUp) > 0 : return getMaxSourceEl(fluxSource,elVal,elMax)
    else : return 'None'

def getBrightOptical(elMax=82.0):
    brightOptical = ['BPER','AAUR','SIRIUS','AGEM','ALEO','AVIR','ASCO','VEGA','ACYG','APSA']
    elVal = []
    for i in brightOptical : elVal.append(azel(i)[1])
    sourceUse = getMaxSourceEl(brightOptical,elVal,elMax)
    return sourceUse

def getPointSource(elMax=80.0,useExtended=True) :
    pointPoint = ['3c84','0530+135','2232+117','3c454.3','3c279','3c273','3c345','1733-130']
    pointExtend = ['uranus','mars','venus','neptune']
    if useExtended : pointSource=pointPoint+pointExtend
    else : pointSource = pointPoint
    maserSource = ['ocet','orimsr','rleo']
    LOVal = queryDouble('Control.Subarray%s.oscFreq' % (s.getSubarrayNo()),24)
    if pointSiO(LOVal) : pointSource = pointSource+maserSource
    elVal = []
    sourceUp = []
    for i in pointSource : elVal.append(azel(i)[1])
    sourceUse = getMaxSourceEl(pointSource,elVal,elMax)
    return sourceUse

def pointSiO(LOVal,minDiff=1.5,maxDiff=2.5) :
    SiOFreq=86.24
    diffVal = abs(LOVal-SiOFreq)
    if (diffVal < maxDiff) and (diffVal>minDiff) : return 1
    else : return 0

def getMaxSourceEl(sourceVec,valVec,elMax=82.0) :
    valVecSort = valVec[:]
    valVecSort.sort()
    valTest = 90.0
    i=-1
    while valTest > elMax and abs(i) <= len(valVec) :
        valTest = valVecSort[i]
        i-=1
    whereVal = valVec.index(valTest)
    return sourceVec[whereVal]
    
"""
getTamb, getPsys, and getAntName are basically wrappers to call the
monitor points needed for tsys calculations.  They take a single
telescope (scalar but this is dealt with effectively below) and return
either ambient temperature, power or telescope name. We also get dish
diameter and Nyquist step in arcmin.  There is an important distinctino,
getTamb and getPsys get only a single value!  They don't act as vector
wrapers.
"""

def getAntName(antref) :
    return CarmaAnt().getName(antref).capitalize()

def getTamb(antNum) :
    nameTele = getAntName(antNum)
    if antNum < 7 : tamb = queryDouble("%s.Drive.Weather.ambientTemp" % nameTele,20 )
    if 6 < antNum < 16 : tamb = queryDouble("%s.BimaSpecific.CalPlate.tempAmb" % nameTele,20)
    return tamb

def getTout() :
    return queryDouble("Weather.WeatherStation.ambientTemperature",20)+273.15

def getRa(source,unitType='hours') :
    sourceLoc     = s.info(source).split().index(source.upper())
    sourceRa      = s.info(source).split()[sourceLoc+1]
    sourceHours   = float(sourceRa.split(':')[0])
    sourceMinutes = float(sourceRa.split(':')[1])
    sourceSeconds = float(sourceRa.split(':')[2])
    if unitType == 'hours' :
        return (sourceHours+sourceMinutes/60.0+sourceSeconds/3600.0)
    elif unitType == 'degrees' :
        return 15.0*(sourceHours+sourceMinutes/60.0+sourceSeconds/3600.0)
    elif unitType == 'dd:mm:ss'  : 
        return sourceRa
    else : return "Unknown unit type, use hours, degrees or dd:mm:ss."

def getDec(source,unitType='degrees') :
    sourceLoc = s.info(source).split().index(source.upper()) 
    sourceDec = s.info(source).split()[sourceLoc+2]
    if sourceDec[0] == '-' : signVal = False
    else : signVal = True
    sourceDeg = float(sourceDec.split(':')[0])
    sourceMin = float(sourceDec.split(':')[1])
    sourceSec = float(sourceDec.split(':')[2])
    if unitType == 'degrees' :
        if signVal : returnVal = (sourceDeg+sourceMin/60.0+sourceSec/3600.0)
        else : returnVal = sourceDeg-sourceMin/60.0-sourceSec/3600.0
        return returnVal
    elif unitType == 'dd:am:as' :
        return 'sourceDec'
    else : return "Unknown unit type, use degrees or dd:am:as."

def getRaDec(source) :
    decVal = getDec(source)
    raVal  = getRa(source)
    return [raVal,decVal]

def getHa(source) :
    sRa = getRa(source.upper())
    sHa = lst()-sRa
    if sHa < -12.0 : sHa=sHa+24.0
    if sHa >  12.0 : sHa=sHa-24.0
    return sHa

def getPsys(antref,band) :
    inputQuery = "Sldc.Band%s.Input%s.psys" %(str(band),str(antref))
    psys = queryDouble(inputQuery,20)
    return psys

def getPamPower(antref) :
    nameTele = getAntName(antref)
    pamPower = queryDouble("%s.AntennaIFContainer.AntennaIF.ifOutTotalPower" % nameTele,3)
    return pamPower

def getAlias(source) :
    compareList = []
    aliasList = []
    tableVals = fIOP.fileToTable('/opt/rt/conf/catalogs/SystemSource.cat')
    [junk,lineVals] = fIOP.getRestrictTable(tableVals,5,'LSR')
    for i in range(len(junk)) : compareList.append(junk[i][0])
    for i in compareList :
        if distSky(source.upper(),i.upper()) < 10.0/3600.0 : aliasList.append(i)
    return aliasList

def getNearestN(source,numberNearest,compareList=None,ignoreNorthSouth=False,getOptical=False,coordSys='azel',onSky=True,fromCatalog=True,optMagLimit=99.0,radFluxLimit=0.0,radFreq=95.0,extraExclude=[]) :
    sourceNames  = []
    sourceDist   = []
    sourceBright = []
    excludeList = extraExclude
    for i in range(numberNearest) :
        [nearSource,nearDist,nearBright] = getNearest(source,compareList,ignoreNorthSouth,getOptical,coordSys,onSky,fromCatalog,optMagLimit,radFluxLimit,radFreq,excludeList)
        print [nearSource,nearDist,nearBright]
        excludeList.append(nearSource)
        sourceNames.append(nearSource)
        sourceDist.append(nearDist)
        sourceBright.append(nearBright)
    return [sourceNames,sourceDist,sourceBright]

def sourceUpList(sourceList) :
    upList = []
    for i in sourceList :
        if isup(i) : upList.append(i.upper())
    return upList

def sourceOrderedList(source=None,compareList=None) :
    if source == None : source=queryString('Control.Subarray%i.Commands.Track.sourcename' % s.getSubarrayNo(),24)
    orderedList = []
    upList = sourceUpList(compareList)
    [nearSource,timeSource,brightSource] = getNearest(source,upList,ignoreNorthSouth=True,coordSys='time')
    orderedList.append(nearSource)
    upList.remove(nearSource)
    while len(upList) <> 0 :
        [nearSource,timeSource,brightSource] = getNearest(nearSource,upList,ignoreNorthSouth=True,coordSys='time')
        upList.remove(nearSource)
        orderedList.append(nearSource)
    return orderedList
    
def getNearest(source=None,compareList=None,ignoreNorthSouth=False,getOptical=False,coordSys='azel',onSky=False,fromCatalog=False,optMagLimit=99.0,radFluxLimit=0.0,radFreq=95.0,excludeList=[]) :
    """Distance is returned in degrees unless time is choosen for coordinates, in which minutes is the unit."""
    localLat   = 37.0+16.0/60.0+49.37285/3600.0
    lstVal     = lst()
    if source==None : source=queryString('Control.Subarray%i.Commands.Track.sourcename' % s.getSubarrayNo(),24)
    [sRa,sDec] = getRaDec(source.upper())
    [sAz,sEl]  = azel(source.upper())
    minDist    = 180.0
    minSource  = ''
    coord1     = 180.0
    coord2     = 180.0
    totDist    = 180.0
    if compareList == None :
        compareList = []
        if fromCatalog :
            tableVals = fIOP.fileToTable('/opt/rt/conf/catalogs/SystemSource.cat')
            [restrictAll,lineVals] = fIOP.getRestrictTable(tableVals,5,'LSR')
            if getOptical : [junk,junkLine] = fIOP.getRestrictTable(restrictAll,10,'OPTICAL')
            else : [junk,junkLine] = fIOP.getRestrictTable(restrictAll,10,'RADIO')
            for i in range(len(junk)) :
                try: compareList.append(junk[i][0].upper())
                except: continue
        else :
            if getOptical : junk = s.whazUpOptical().split('\n')[2:-1]
            else : junk = s.whazUp().split('\n')[2:-1]
            for i in range(len(junk)) :
                try: compareList.append(junk[i].split()[0].upper())
                except: continue
    for j in ['NORTHPOL',source.upper(),'USER'] :
        try: compareList.remove(j)
        except: continue
    for i in excludeList :
        try: compareList.remove(i.upper())
        except: continue
    for i in compareList :
        [tRa,tDec] = getRaDec(i.upper())
        [tAz,tEl]  = azel(i.upper())
        if ignoreNorthSouth == False :
            if (((sDec > localLat) and (tDec < localLat)) or ((sDec < localLat) and (tDec > localLat))) : continue
        if ((abs(sRa-tRa) < (0.1/3600.0)) and (abs(sDec-tDec) < (0.2/3600.0))) : continue
        if (getOptical and (s.queryMag(i.upper()) > optMagLimit)) : continue
        if ((not getOptical) and (s.queryFlux(i.upper(),radFreq,20.0,0) < radFluxLimit)) : continue
        if onSky :
            totDist = distSky(source.upper(),i.upper())
        else :
            if coordSys == 'azel' :
                coord1 = sAz-tAz
                if coord1 >  180.0 : coord1=coord1-360.0
                if coord1 < -180.0 : coord1=coord1+360.0
                coord2 = sEl-tEl
                totDist = (coord1**2.0+coord2**2.0)**0.5
            elif coordSys == 'radec' :
                coord1 = (sRa-tRa)*15.0
                coord2 = (sDec-tDec)
                totDist = (coord1**2.0+coord2**2.0)**0.5
            elif coordSys == 'time' :
                totDist = slewSim(source.upper(),i.upper(),1)
            else : return "coordSys not recognized.  coordSys can be radec, azel or time."
        if totDist < minDist : 
            minSource = i
            minDist   = totDist
    if getOptical : minBright = s.queryMag(minSource.upper())
    else : minBright = s.queryFlux(minSource.upper(),radFreq,20.0,0)
    return [minSource,minDist,minBright]


def getNyquistStep(antref=0,scalingFactor=1.0,userFreq=None) :
    """Returns Nyquist step in arcminutes
       scalingFactor allows the observer to
       set a slightly smaller step if desired.
       One might consider this for continuum
       observations if the high frequency end
       of your bandwidth is more than a few percent
       above your LO.  This factor was previously
       set to 0.9 in expectation of continuum mosaics
       with full bandwidth (upper end would differ
       by ~5%, ~8% in the future).  Now we default to the
       more reasonable 1.0 allowing the user to fine tune.
    """
    antVec = checkAntVectorType(antref)
    nyquistStepVec = []
    if userFreq=='lo' : freq = queryDouble('Control.Subarray1.loFreq',24)
    if userFreq==None : freq = queryDouble('Control.Subarray1.skyFreq',24)
    else : freq=userFreq
    wavelength = 3.0e11/(freq*1.0e9)
    diameterVec = getDishDiameter(antVec)
    for i in diameterVec :
       if not i : nyquistStep = 0.0
       else : nyquistStep = (wavelength/i)*3437.75/2.0
       nyquistStepVec.append(scalingFactor*nyquistStep)
    return nyquistStepVec

def getDishDiameter(antref=0) :
    """Returns dish diameter in millimeters"""
    antVec = checkAntVectorType(antref)
    diameterVec = []
    for i in antVec :
       if i < 7 : diameter= 10400.0
       elif i < 16 : diameter = 6100.0
       elif i < 24 : diameter = 3500.0
       else : diameter = 0
       diameterVec.append(diameter)
    return diameterVec

def getConfigCorrT(slcBandsT=[1,2,3]) : 
    allConfigT = []
    baseStringT = 'Control.SpectralLineCorrelator.SlcBand'
    for i in slcBandsT :
        bandConfigT = []
        bandStringT = ('%s%i.ControlBandPoints' % (baseStringT,i) )
        bandWidthMT = queryDouble('%s.bandwidth' % bandStringT,24)
        bandSBT     = queryString('%s.lo2Sideband' % bandStringT,24)
        frest       = queryDouble('%s.restFreq' % bandStringT,24)
        imagefrest  = queryDouble('%s.imageRestFreq' % bandStringT,24)
        if bandSBT   =='LOWER' : bandSBT = LSB
        elif bandSBT =='UPPER' : bandSBT = USB
        bandNumT    = queryInt('%s.number' % bandStringT,24)
        if   abs(bandWidthMT - 500)  < 1 : BWT = BW500
        elif abs(bandWidthMT - 62.5) < 1 : BWT = BW62
        elif abs(bandWidthMT - 31.3) < 1 : BWT = BW31
        elif abs(bandWidthMT - 7.8)  < 1 : BWT = BW8
        elif abs(bandWidthMT - 2.0)  < 1 : BWT = BW2
        bandConfigT = [bandNumT, BWT, frest, bandSBT, imagefrest]
        allConfigT.append(bandConfigT)
    return allConfigT

def compareConfigCorr(bandConfigsT) :
    bandConfigsT = makeNestedList(bandConfigsT)
    bandsToTry = [0]*len(bandConfigsT)
    for i in range(len(bandConfigsT)) :
        bandsToTry[i] = bandConfigsT[i][0]
    if appearsTwice(bandsToTry) :
        print 'Multiple band configurations issued; clean up your act and issue unique bands!'
        return None
    currentConfigCorr = getConfigCorrT()
    bandsToChange = []
    for i in range(len(bandConfigsT)) :
        changeBand = [True]*4
        specBand = bandConfigsT[i][0]-1
        for j in range(4) :
            if bandConfigsT[i][j] <> currentConfigCorr[specBand][j] :
                changeBand[j] = False
        if min(changeBand) <> True :
           bandsToChange.append(bandConfigsT[i])
    return bandsToChange

def changeConfigCorr(bandConfigsT) :
    """Changes correlator configuration for whatever bands are input.
    Important notes:
        Input takes the form [[bandNum,BWXXX,frest,LSB,imagefrest],
                              [bandNum,BWXXX,frest,LSB,imagefrest],
                              ...]
        Single entries can be entered as [bandNum,BWXXX,frest,LSB,imagefrest].
        DO NOT enter the same band more than once.  The last band 
        set-up which differs from the current setup will persist, i.e.
        if the current setup is BW500,2.5,USB,1 and you give BW62,2.5,USB,1
        and BW500,2.5,USB,1...then the BW62 will persist, if you give
        BW62,2.5,USB,1 and BW500,2.0,LSB,1...then the BW500,2.0,LSB,1
        command will persist.  Basically, DO NOT issue two configs
        for the same band!  This is done to save you time in changing
        configCorrs that don't need to be change...the expense is that
        you don't give the same band twice!  In fact...it will throw
        and exception!
    Function:
        Eventually (with optical module..if we have it before this
        command is gone), the cal wheel will start to move into place
        the correlator bands will be changed only if the request is
        different from the current setup (band comparison done in
        setAllConfigCorrT/compareConfigCorr).
        Ambient load then moved in with wait (with optics module,
        load may already be there...once the first amb() goes in
        to effect)
    Return: Nothing"""
#    amb(waiton=NONE)
    setAllConfigCorrT(bandConfigsT)
    postConfigCorr()

def postConfigCorr() :
    amb(tmo=20)
    psysPreset()
    sleep(3.1)
    sky(tmo=15)

def setAllConfigCorrT(bandConfigsT) :
    bandsToChange = compareConfigCorr(bandConfigsT)
    for i in range(len(bandConfigsT)) :
        configband(bandConfigsT[i][0],bandConfigsT[i][1],\
                   bandConfigsT[i][2],bandConfigsT[i][3],bandConfigsT[i][4])
#       qconfigCorr(bandConfigsT[i][0],bandConfigsT[i][1],bandConfigsT[i][2],bandConfigsT[i][3])

"""
These routines involve manipulating the subarray.  The top two
functions simply return lists and are simply wrappers which
destructure the output of getAntennaAssignments.  The rest have to do
with filling the subarray, emptying it and defining new subarrays.
assignSubarray is simply a wrapper to limitSubarray as both have
existed for awhile and may be in use though their function is identical.
"""

def limitSubarray(antref) :
    antVec = checkAntVectorType(antref)
    removeAntenna(0,True)
    addAntenna(antVec,True)

def checkThreeAgree(source,optionA,optionB) :
    source = helpers.makeList(source)
    optionA = helpers.makeList(optionA)
    optionB = helpers.makeList(optionB)
    if len(optionA) <> len(source) : optionA = len(source)*optionA
    if len(optionB) <> len(source) : optionB = len(source)*optionB
    if (len(optionA) <> len(source)) or (len(optionB) <> len(source)) :
       return [0,0,0,0]
    else : return [1,source,optionA,optionB]

def makeAllVector(antref,source,intTime,intRep) :
    antref  = checkAntVectorType(antref)
    source  = helpers.makeList(source)
    intTime = helpers.makeList(intTime)
    intRep  = helpers.makeList(intRep)
    return [antref,source,intTime,intRep]

"""
These are independent programs that are of general utility and
may not be associated with a specific observing task.

I kept tnoiseInt as it is a special case of integrate noise and it is
useful for some types of integration cycles. It basically turns on the
noise source, takes intRep_ integrations of intTime_ in length and then
turns off the noise source.

tTsys is a temporary Tsys calculation. You can give it a scalar
escope number or a vector of one or several elements.  The last
major improvement would be to, instead of time.sleep(10), have a check
to see if the calWheel is moving, then sleep.  This would make it more
efficient as 10 seconds is likely too long. I think I have multiple
bands worked in now.  It returns a n by m array with n telescopes and
m bands.  I will test this when I have some system time.

setBlockName and fillBlockName are Stephen White creations.  fillBlockName
is now outdated because sdpFiller automatically generates miriad data after
the closing of an obsblock.  The setBlockName is a very useful command that
sets up and looks for files which include the most recent obsblock for a
given commissioning task.  It returns the next trial number for that task
and returns the block name as well as sending the block name to the control
system.  I have broken off the function which creates the 2006jan03 or
whatever the date may be because this is a helpful function for other
tasks.

incOffset basically allows you to enter az and el offsets that are
relative to the offsets already in the system.  This is useful for
manual optical pointing but it is now no longer supported as it does
not mess well with the construction of subarrays.
"""
def tnoiseInt(intTime,intRep=1,moveCalWheel=True,affectRFAmp=False) :
    if affectRFAmp : s.rfPower(False)
    noiseon()
    if moveCalWheel : amb(0)
    else : sleep(1.6)
    integrate(intTime,intRep)
    noiseoff()
    if affectRFAmp :
        s.rfPower(True)
        sleep(1.6)
        psysPreset()
        sleep(3.1)
    if moveCalWheel : sky(0)
    else : sleep(1.6)
    print 'Noise source integrations are finished.'
    
def waitRise(source) :
    timeToSleep = s.whenUp(source)*60.0
    if ((timeToSleep > 0.0) and (timeToSleep/(3600.0) < 2.0)) :
       print 'Waiting %s minutes for %s to rise.' % (round(timeToSleep/6.0)/10.0,source)
       sleep(timeToSleep+1.0)
       return 1
    else :
       print 'Source %s has either set or has a restrictively long rise time.' % source
       return 0

def waitLst(lstVal) :
    lstTime = convertLst(lstVal)
    timeWait = (lstTime-s.lst())
    if timeWait > 12.0 : timeWait = timeWait-24.0
    elif timeWait < -12.0 : timeWait = timeWait+24.0
    if timeWait > 0.0 : 
       print "Waiting %s minutes before starting." % str(timeWait*60.0)
       sleep(timeWait*3600.0)
    
def stopLst(lstVal) : 
    lstTime = convertLst(lstVal)
    timeDiff = (lstTime-s.lst())
    if timeDiff > 12.0 : timeDiff = timeDiff-24.0
    elif timeDiff < -12.0 : timeDiff = timeDiff+24.0
    if timeDiff < 0.0 : 
        print "LST time cutoff has been reached."
        return 1 # Implies you are past the stop time
    else : return 0 # Implies you are in the proper time range.

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

def setBlockName(obsType,subType="") :
    """Usage: setBlockName(obsType)
    Set the obsblock name mmmdd.obsType.trial, where trial is a
    number and obsType is e.g. 'rpoint', 'cross', 'base'. Automatically
    increments the trial number using last value stored in a disk file."""
    oType = obsType.lower()
    if (obsType != oType) :
        print 'Note: observation type converted to a lower-case string ...'
    # set project name, etc - time format is ['thu','jan','12','08:38:56','2006']
    projectName = yearMonthDay()
    s.project(projectName)
    s.obsblock(oType)
    s.subObsblock(subType)
    #Now increment the trial number if needed: open file storing name of last run.
    s.trial(1)
    trial = 1
    lastPointFile = '/home/obs/commissioningData/.last_'+oType+'_project'
    if (os.path.exists(lastPointFile)):
       fd = open(lastPointFile)
       lastProject = fd.read().strip('\n').split('.')
       fd.close()
       print lastProject
       if (lastProject[0] == projectName):
          s.trial(int(lastProject[3])+1)
          trial = int(lastProject[3])+1
    blockName = '.'.join([projectName,oType,subType,str(trial)])
    # Store the name of the project in a file in /home/obs/radioPointData
    fd = open(lastPointFile,'w')
    fd.write(blockName)
    fd.close()
    return blockName

def yearMonthDay() :
    timeDateValue = time.asctime(time.gmtime()).lower().split()
#   timeDateValue = time.asctime().lower().split()
    if int(timeDateValue[2]) < 10 : timeDateValue[2] = str('0'+str(timeDateValue[2]))
    return '%s%s%s' % (timeDateValue[4],timeDateValue[1],timeDateValue[2])

def getMiriadUTStamp() :
    calendar = { '1' : 'jan' , '2' : 'feb' ,  '3' : 'mar' , '4' : 'apr' ,  '5' : 'may' , '6' : 'jun' ,  '7' : 'jul' , '8' : 'aug' ,  '9' : 'sep' , '10' : 'oct' ,  '11' : 'nov' , '12' : 'dec' }
    utStamp = time.gmtime()
    utYear  = str(utStamp[0])[2:]
    utMon   = str(utStamp[1])
    utMonU  = calendar[utMon]
    utDay   = maybeAddAZero(utStamp[2])
    utHour  = maybeAddAZero(utStamp[3])
    utMin   = maybeAddAZero(utStamp[4])
    utSec   = maybeAddAZero(utStamp[5])
    return ('%s%s%s:%s:%s:%s' % (utYear,utMonU,utDay,utHour,utMin,utSec) )

def maybeAddAZero(xS) :
    if (xS < 10) : return '0'+str(xS)
    else : return str(xS)

def avgLinear(data,type) :
    rmsVal = 0.0
    reps = len(data)
    if type == 'median' :
        midPoint = int(reps/2)
        checkMid = reps%2
        data.sort()
        if ((checkMid) and (reps > 1)) : return1 = (data[midPoint]+data[midPoint+1])/2.0
        else : return1 = data[midPoint]
    elif type == 'mean' :
        return1 = 0.0
        for k in data : return1 = return1+(i/reps)
    else : return 'Either median or mean are supported, not %s' % type
    for i in data : rmsVal = rmsVal+(return1-i)**2.0
    if reps > 1: return2 = (rmsVal/(float(reps-1)))**0.5
    else: return2 = 0.0
    return [return1,return2]

def weightedLinearAverage(data,errors) :
    """It would be nice if someone added weighted mean deviation but
    I don't have time to now.  I would be really easy.  We could then replace
    calls to avgLinear in avgDbM to this function"""
    weightedMeanDeviation = 0.0
    weightsDe = 0.0
    weightVals = 0.0
    for i in range(len(data)) :
        weightVals = weightVals+data[i]/(errors[i]**2.0)
        weightsDe = weightsDe+1/(errors[i]**2.0)
    return1 = weightVals/weightsDe
    return2 = weightedMeanDeviation
    return [return1,return2]

def avgDbM(data,type) :
    rmsVal = 0.0
    reps = len(data)
    dataLinear = []
    for i in data : dataLinear.append(10.0**(i/10.0))
    [return1,return2] = avgLinear(dataLinear,type)
    return1New = 10.0*m.log10(return1)
    if return2 == 0.0 : return2New='err'
    else : return2New = 10.0*m.log10(return2)
    return [return1New,return2New]

    

"""
This program is no longer in use because the sdpFiller automatically converts obsblock data files to miriad format
after a block is closed.

def fillBlockName(blockName) :
    if not (blockName == str(blockName)) :
        print 'Sorry: blockName must be a string. Program fillBlockName terminating.'
        return    
    xmlFile = '/misc/longterm2/sdp/astroheader/SLCorrelIntegrated/astrohdr_'+blockName+'.xml'
    # if this xml file does not exist, assume it is still in .write state
    if not (os.path.exists(xmlFile)):
       xmlFile = '/misc/longterm2/sdp/astroheader/SLCorrelIntegrated/astrohdr_'+blockName+'.xml.write'
    # now fill the data IF the file exists
    if (os.path.exists(xmlFile)):
       os.system('sdpFiller infile='+xmlFile+' outfile=/home/obs/vis_data/'+blockName+'.mir') 
       # os.system('sdpFiller infile=/misc/longterm2/sdp/astroheader/SLCorrelIntegrated/astrohdr_%s.xml outfile=/home/obs/vis_data/%s.mir' % (blockName,blockName))
    else:
       print 'Sorry: no such XML file found. Exiting ...'

This program is no longer in use.  The offsets are carried in their proper location,
aperture coefficients.  Temporary pointing offsets are now to be carried in the
mountOffset

def incOffset(incAz,incEl,antref) :
    antVec = checkAntVectorType(antref)
    for i in antVec :
       queryCall= %s.AntennaCommon.Drives % (getAntName(i))
       prevOffAz = queryDouble(queryCall+.offsetAz,2)
       prevOffEl = queryDouble(queryCall+.offsetEl,2)
       try: offset(prevOffAz+incAz,prevOffEl+incEl,i)
       except: writeShortLogInfo('Drive timeout error, from offset')
       
Finally, the short logging program sends errors to shortError.log.
and a beam width offset command.
"""

def offsetBeamWidth(azFracOff,elFracOff,antref=0) :
    antVec = checkAntVectorType(antref)
    step   = getNyquistStep(antVec)
    azOff  = []
    elOff  = []
    for i in range(len(step)) :
        azOff.append(2.0*step[i]*azFracOff)
        elOff.append(2.0*step[i]*elFracOff)
    offset(azOff,elOff,antVec)

def applyMountOffsets(azVec,elVec,antsInUse) :
    antsInUse = checkAntVectorType(antsInUse)
    for i in range(len(antsInUse)) : s.mountOffset(azVec[i],elVec[i],antsInUse[i])

def zeroMountOffsets() :
    for iant in currentAntennaNumbers(): s.mountOffset(0.0, 0.0, iant)

def incrementMountOffsets(azVec,elVec,antsInUse) :
    antsInUse = checkAntVectorType(antsInUse)
    [oldAz,oldEl] = grabMountOffsets(antsInUse)
    for i in range(len(antsInUse)) : s.mountOffset(azVec[i]+oldAz[i],elVec[i]+oldEl[i],antsInUse[i])

def grabOffsets(antVec) :
    antVec = checkAntVectorType(antVec)
    azVec = []
    elVec = []
    for i in antVec :
        if i < 7 :
            azVec.append(queryDouble('Ovro%i.AntennaCommon.Drive.Point.offsetAz' % i,24))
            elVec.append(queryDouble('Ovro%i.AntennaCommon.Drive.Point.offsetEl' % i,24))
        else :
            azVec.append(queryDouble('Bima%i.AntennaCommon.Drive.Point.offsetAz' % (i-6),24))
            elVec.append(queryDouble('Bima%i.AntennaCommon.Drive.Point.offsetEl' % (i-6),24))
    return [azVec,elVec]

def grabMountOffsets(antVec) :
    antVec = checkAntVectorType(antVec)
    azVec = []
    elVec = []
    for i in antVec :
        if i < 7 :
            azVec.append(queryDouble('Ovro%i.AntennaCommon.Drive.Point.mountOffsetAz' % i,24))
            elVec.append(queryDouble('Ovro%i.AntennaCommon.Drive.Point.mountOffsetEl' % i,24))
        else :
            azVec.append(queryDouble('Bima%i.AntennaCommon.Drive.Point.mountOffsetAz' % (i-6),24))
            elVec.append(queryDouble('Bima%i.AntennaCommon.Drive.Point.mountOffsetEl' % (i-6),24))
    return [azVec,elVec]

def makeNestedList(x) :
    if list not in [type(x[0])] : x=[x]
    return x

def appearsTwice(x) :
    if len(x) > 1 :
        returnFault = [False]*(len(x)-1)
        x.sort()
        for i in range(len(x)-1) :
            if ((x[i]-x[i+1]) == 0) : returnFault[i]=True
        return max(returnFault)
    else : return False

def makePause() :
    print "Press return to continue."
    sys.stdin.readline()
        
def writeShortLogInfo(message,fileName='/array/rt/scripts/shortError.log') :
    fd = open(fileName,'a')
    fd.write("%s %s \n" % (time.asctime(),message) )
    fd.close()
#
# This is a slew simulator and includes the wrap logic
# and drive rates.
#
def totalSlewTime(starList,telescope=1) :
    clockTime = 0.0
    for i in range(len(starList)-1) :
        s1 = starList[i]
        s2 = starList[i+1]
        if list in [type(s1)] : s1 = s1[0]
        if list in [type(s2)] : s2 = s2[0]
        if tuple in [type(s1)] : s1 = s1[0]
        if tuple in [type(s2)] : s2 = s2[0]
        clockTime=clockTime+slewSim(s1,s2,telescope)
    return clockTime
    
def slewSim(source1,source2,telescope) :
    localLat = 37.0+16.0/60.0+49.37285/3600.0
    [sRa1,sDec1] = getRaDec(source1.upper())    
    [sRa2,sDec2] = getRaDec(source2.upper())
    [az1,el1] = azel(source1)
    [az2,el2] = azel(source2)    
    lstVal = lst()
    if telescope < 7 : 
       ha1 = getHa(source1)
       ha2 = getHa(source2)
       if ((sDec1>localLat) and (ha1 > 0.0)) : 
           az1=az1-360.0
       if ((sDec2>localLat) and (ha2 > 0.0)) :
           az2=az2-360.0
       azRate = 60.0
       elRate = 30.0
       settleTime = 5.0
    elif telescope < 16 :
       if ((az2 < 45.0) and (az1 > 225.0)) : 
           az2=az2+360.0
       elif ((az2 > 315.0) and (az1 < 135.0)) :
           az2=az2-360.0
       azRate = 120.0
       elRate = 90.0   
       settleTime = 10.0
    azDist = abs(az1-az2)
    elDist = abs(el1-el2)
    driveTime = max(azDist/azRate,elDist/elRate)+settleTime/60.0
    return driveTime # minutes

def distSky(source,ref):
    """Return angular distance between a ref and source"""
    srcAzEl = azel(source)
    refAzEl = azel(ref)
    deg2rad = 180.0/m.pi
    srcAz   = srcAzEl[0]/deg2rad
    srcEl   = srcAzEl[1]/deg2rad
    refAz   = refAzEl[0]/deg2rad
    refEl   = refAzEl[1]/deg2rad
    cosDist =  m.sin(refEl)*m.sin(srcEl)+m.cos(refEl)*m.cos(srcEl)*m.cos(refAz-srcAz)
    dist    = math.acos(cosDist)*deg2rad
    return  float(dist)

def getMyCal(source) :
    fluxes = fIOP.fileToTable('/array/rt/pointing/fluxTable.txt',ignoreEmpty=True)
    tracks = []
    for i in fluxes :
        for j in i :
            if j.upper() == source.upper() : tracks.append(i)
    return tracks
  

def getTracksWithFlux(addLastN=1000) :
    fileNames = []
    pFluxCals = ['URANUS','NEPTUNE','MWC349','MARS'] 
    fluxCals  = ['3C273','3C454.3','3C279','3C345','0530+135','3C111']
    fluxFile = '/array/rt/pointing/fluxTable.txt'
    os.system('ls -lrtd /opt/sdp/sciencedata/*.mir > fluxFile.info')
    fileTable = fIOP.fileToTable('fluxFile.info',ignoreEmpty=True)
    for i in range(len(fileTable)) : fileNames.append(fileTable[i][8])
    numFiles = len(fileNames)
    for i in fileNames[numFiles-addLastN:] :
        os.system("uvindex vis=%s > uvindexFile.txt" % i)
        uvindexTable  = fIOP.fileToTable('uvindexFile.txt',ignoreEmpty=True)
        sourceList    = fIOP.getUnique(uvindexTable,1)
        if "URANUS" not in sourceList and "NEPTUNE" not in sourceList and "MARS" not in sourceList and "MWC349" not in sourceList: continue
        os.system("listobs vis=%s > listobsFile.txt" % i)
        listobsTable  = fIOP.fileToTable('listobsFile.txt',':',ignoreEmpty=True)
        [freqVal,locos]= fIOP.getRestrictTable(listobsTable,0,'Velo Code')
        keyIndex1     = sourceList.index('Source')
        keyIndex2     = sourceList.index('Total')
        fd=open(fluxFile,'a')
        fd.write("%s %s " % (i,freqVal[0][-1][:-1]))
        for j in sourceList[keyIndex1+2:keyIndex2] : 
            if j.upper() in pFluxCals : fd.write("%s " % j)
        for j in sourceList[keyIndex1+2:keyIndex2] :
            if j.upper() in fluxCals : fd.write("%s " % j)
        fd.write('\n')
        fd.close()
