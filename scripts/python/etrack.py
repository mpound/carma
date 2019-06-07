# etrack.py
#
# @author John Carpenter
# $Id: etrack.py,v 1.43 2015/01/06 16:14:05 control Exp $
#
#   History
#   2007-Aug-11  JMC    Created stand alone version

import string
import subarrayCommands as commands
import carmaHelpers as helper
import math
import obsdefUtils as utils

# Variables needed for E-array
IS_E_CONFIGURATION       = False  # Are we in E-array?
MAXIMUM_AZSLEW           = 30.0   # Degrees
SAFE_ELEVATION_E_CONFIG  = 85.0   # Degrees
NANTENNAS                = 23 
ELOG                     = False  # Print diagnostic messages to log
SLEEPTIME = 3                     # Drive recheck interval in seconds

# ***Dangerous*** but useful for testing; should normally be an empty list
ignoreAntennas           = []

def getSlewParameters(antenna, source1, source2=None):
    """ Returns slew information to move antenna to source1

        Inputs: antenna - antenna number (1-15)
                source1 - the source to slew to
                source2 - the source source to slew from. If None, then
                          use the current antenna position

        Output: a list containing
                  1) azimuth slew distance in degrees
                  2) elevation slew distance in degrees
                  3) desired azimuth in degrees
                  4) desired elevation in degrees

    """
    # Get RA/DEC and az/el of source1
    ra1  = helper.convertHms(utils.getSourceRa(source1))
    dec1 = helper.convertHms(utils.getSourceDec(source1))
    az1, el1 = commands.azel(source1)

    # Get RA/DEC and az/el of source2, or if source2 = None, then
    # use the current antenna position
    if source2 == None:
        mp = utils.getAntennaMp(antenna)
        root = "Drive"
        if antenna > 6: root = 'AntennaCommon.Drive'
        az2  = commands.queryDouble('%s.%s.Track.actualAzimuth' % (mp, root))
        el2  = commands.queryDouble('%s.%s.Track.actualElevation' % (mp, root))
        ra2  = commands.queryDouble('%s.%s.rightAscension' % (mp, root))
        dec2 = commands.queryDouble('%s.%s.declination' % (mp, root))
    else:
        ra2  = helper.convertHms(utils.getSourceRa(source2))
        dec2 = helper.convertHms(utils.getSourceDec(source2))
        az2, el2 = commands.azel(source2)

    # Adjust az/el for wraps
    if antenna <= 6 : 
        ha1 = utils.getHa(source1)
        if source2 <> None : 
            ha2 = utils.getHa(source2)
        else :
            ha2 = commands.lst() - ra2
        if ha2 < -12.0 : ha2 +=  24.0
        if ha2 >  12.0 : ha2 += -24.0
        if dec1 > utils.OBSERVATORY_LATITUDE and ha1 > 0.0: 
            az1 += -360.0
        if dec2 > utils.OBSERVATORY_LATITUDE and ha2 > 0.0 and source2 <> None:
            az2 += -360.0
    elif antenna < 16 :
        if az2 < 45.0 and az1 > 225.0: 
            az2 +=  360.0
        elif az2 > 315.0 and az1 < 135.0:
            az2 += -360.0

    # Compute distance
    azDist = abs(az1 - az2)
    elDist = abs(el1 - el2)

    # Done
    return azDist, elDist, az1, el1


def isAntennaMoveable(antenna):
    """ Returns True/False/None if antenna is moveable by computer control 

        True  -> Antenna can be moved remotely
        False -> Antenna cannot be moved remotely
        None  -> Unknown if the antenna can be moved (HW/SW limit)
    """
    # Get drive state
    state = getDriveState(antenna)

    # Determine if the antenna is moveable.
    # True : Antennas is moveable since the current drive state is
    #        TRACK, CLOSE, SLEW, STOW, SNOW, STOP
    # False: Antennas is not moveable since the current drive state is
    #        DISABLE or LOCAL
    # None : Antenna status is unknown since the current drive state
    #        HWLIMIT, SWLIMIT, or ERROR
    isMoveableTrue    = ['TRACK', 'CLOSE', 'SLEW', 'STOW', 'SNOW', 'STOP']
    isMoveableFalse   = ['DISABLE', 'LOCAL']
    isMoveableUnknown = ['HWLIMIT', 'SWLIMIT', 'ERROR']

    # Is antenna moveable?
    try:
        isMoveableTrue.index(state)
        isMoveable = True
    except:
        try:
            isMoveableFalse.index(state)
            isMoveable = False
        except:
            try:
                isMoveableUnknown.index(state)
                isMoveable = None
            except:
                commands.trackMessage('ERROR: Unknown antenna state : ' + state, 
                      printLog=ELOG)
                raise Exception,'Aborting track'

    # Done
    return isMoveable


def getDriveState(antenna):
    """ Returns drive state of specified antenna """
    mp = utils.getAntennaMp(antenna) + '.AntennaCommon.Drive.state'
    return commands.queryString(mp)


def isAntennaMoving(antenna):
    """ Returns True/False if antenna is moving.

        'Moving' in this context moving in a way that cannot be easily
        predicted such as 'SLEW' or 'SNOW'. A tracking state will 
        return false since the telescope movements are small.

        The function should perhaps be renamed.
    """
    # Initialize
    isMovingTrue = ['SLEW', 'SNOW']
    state = getDriveState(antenna)

    # Is antenna moving?
    try:
        isMovingTrue.index(state)
        isMoving = True
    except:
        isMoving = False

    # Done
    return isMoving


def isSafePosition(antenna):
    """ Returns antenna state.

        Input: Antenna number (1-15)

        Output: 2 element list
                  1) Is the antenna in a safe position AND cannot be moved?
                  2) Is the antenna in the inner array (and therefore
                     can potentially collide with another antenna)?

        Example: safe, inner = isSafePosition(7)
    """
    # Fake safe for the ignored antennas
    if ignoreAntennas.count(antenna) > 0: return True, False
        
    # Get pad position
    pad = commands.queryInt('Control.Antenna%s.padNumber' % antenna)

    # Setup for monitor point variables
    anttype = utils.getAntennaMp(antenna)

    # Loop over pad positions that may collide
    isInner = True
    isSafe = False
    if pad == 32 or \
       pad == 46 or \
       pad == 48 or \
       pad == 61 or \
       pad == 62 or \
       pad == 63 or \
       pad == 64:
        mp = anttype + '.AntennaCommon.Drive.safeState'
        isSafe = (commands.queryInt(mp) == 0)
    else:
        isInner = False
        isSafe = True

    # Done
    return isSafe, isInner

def getInnerAntennas() : 	 
    innerAnt = [] 	 
    antVec = commands.currentAntennaNumbers() 	 
    for i in antVec : 	 
        if isSafePosition(i)[1] : innerAnt.append(i) 	 
    return innerAnt 	 
	  	 
def getOuterAntennas() : 	 
    outerAnt = [] 	 
    antVec = commands.currentAntennaNumbers() 	 
    for i in antVec : 	 
        if not isSafePosition(i)[1]  : outerAnt.append(i) 	 
    return outerAnt
	        
def getAntennaInformation(source=None):
    """ Returns current position/status of all antennas.
        Used to coordinate antenna slews in E-array

        Input:  Name of source to slew to  (optional)

        Output: dictionary with 1 entry for each antenna.
                The "key" is the antenna number (1, 2, 3, ....)
                The value is a dictionary containing:
                     az          : current azimuth   of the antenna in degrees
                     el          : current elevation of the antenna in degrees
                     azRequested : the requested azimuth in degrees for the
                                   specified source.
                     elRequested : the requested elevation in degrees for the
                                   specified source.
                     deltaAz     : requested azimuth slew on the sky in degrees
                                   to move from current antenna position to 
                                   requested source.
                     inner       : If True, antenna is in the inner array and
                                   could potentially collide. If False, antenna
                                   will never collide.23186
                     safe        : antenna is currently in a safe position
                     state       : Current drive state
    """
    # Initialize
    antennas = dict()

    # Loop over antennas
    for i in range(1,NANTENNAS+1):
        # Initialize dictionary for this antenna
        info = dict()

        # Get antenna status
        info['safe'], info['inner'] = isSafePosition(i)

        # If outer array, then we are done
        if not info['inner']:
           # Save in dictionary
           antennas[i] = info.copy()
           continue

        # *******************************************************
        # *   START CHECK FOR ERROR CONDITIONS FOR INNER ARRAY  *
        # *******************************************************
        if info['inner']:
            # If antenna is in another subarray, it must be in a safe position
            if not utils.inSubarray(i) and not info['safe']:
                commands.trackMessage('ERROR: Antenna ' + str(i) + 
                             ' is in an unsafe position in another subarray.',
                             printLog=ELOG)
                commands.trackMessage('Move antenna to a safe position before re-starting script',
                      printLog=ELOG)
                commands.stop()
                raise Exception, 'Aborting script: antenna in unsafe position'

            # If antenna cannot be moved, then it must be in a safe position
            if isAntennaMoveable(i) != True and not info['safe']:
                # Double check drive state
                commands.sleep(SLEEPTIME)
                if isAntennaMoveable(i) != True and not info['safe']:
                    commands.trackMessage('ERROR: Antenna ' + str(i) + 
                          ' is in an unsafe position and cannot be moved', printLog=ELOG)
                    commands.trackMessage('Move antenna to a safe position before re-starting script', printLog=ELOG)
                    commands.stop()
                    raise Exception, 'Aborting script: antenna in unsafe position'

            # Antenna should not be moving
            if isAntennaMoving(i):
                if utils.inSubarray(i):
                    commands.stop(i)
                    commands.sleep(1) # Make sure monitor point is updated
                else:
                    commands.trackMessage('ERROR: Antenna ' + str(i) + ' is moving in another subarray', printLog=ELOG)
                    commands.stop()
                    raise Exception, 'Aborting script: another antenna moving'

            # If antenna is at a limit, then abort script.
            if isAntennaMoveable(i) == None:
                # Double check drive state
                commands.sleep(SLEEPTIME)
                if isAntennaMoveable(i) == None:
                    commands.trackMessage('ERROR: Drive state for antenna ' + str(i) + \
                             ' is at a limit or in error', printLog=ELOG)
                    commands.trackMessage('Fix drive-state or disable antenna in safe position before re-starting script', printLog=ELOG)
                    commands.stop()
                    raise Exception, 'Aborting script: error in drive state'

            # *****************************************************
            # *   END CHECK FOR ERROR CONDITIONS FOR INNER ARRAY  *
            # *****************************************************

            # Get drive state
            info['state'] = getDriveState(i)

            # Get azimuth/elevation
            mp = ['Track.actualAzimuth', 'Track.actualElevation']
            code = ['az', 'el']
            for j in range(len(mp)):
                var = utils.getAntennaMp(i) + '.AntennaCommon.Drive.' + mp[j]
                info[code[j]] = commands.queryDouble(var)

            # Get slew information
            if source <> None: 
                slew = getSlewParameters(i,source)
                info['deltaAz'] = slew[0]
                info['azRequested'] = slew[2]
                info['elRequested'] = slew[3]

        # Save antenna
        antennas[i] = info.copy()

    # Return antenna information
    return antennas.copy()


def doubleCheckAntennas(ants=[], targAz=None, targEl=None, tol=0.25):
    ### WARNING: the doubleCheck command assumes we are only dealing with BIMAs
    azmpStr = "Bima%d.AntennaCommon.Drive.Track.actualAzimuth"
    elmpStr = "Bima%d.AntennaCommon.Drive.Track.actualElevation"
    for ant in ants:
        if targAz is not None:
            kmp = str( azmpStr % (int(ant)-6) )
            az  = commands.queryDouble(kmp)
            if math.fabs(az - targAz) > tol:
                return False
        if targEl is not None:
            kmp = str( elmpStr % (int(ant)-6) )
            el  = commands.queryDouble(kmp)
            if math.fabs(el - targEl) > tol:
                return False
    return True


def moveAntennasSafely(antennas, azCurrent, elCurrent, 
                       azRequested, elRequested):
    """ Move antennas safely (hopefully) from current position to az,el.

        The move is done in the following steps:
             1) move antennas to a safe elevation at fixed azimuth (azCurrent);
             2) move to azRequested at the safe elevation, 
        and 
             3) move to azRequested,elRequested moving at a fixed 
                azimuth (azRequested).
    """ 
    # Convert input antennas to a list
    antlist = helper.makeList(antennas)

    m = "moveAntSafely(%s, cur=%.0f/%.0f, req=%.0f/%.0f" \
         %(str(antlist), azCurrent, elCurrent, azRequested, elRequested)
    print m


    ### WARNING: the doubleCheck command assumes we are only dealing with BIMAs
    
    # STEP 1) Move antennas to a safe elevation at constant azimuth
    # Rarely, the move command will return a None value; so we retry here
    while True:
        legitReturnValue = False
        while (legitReturnValue == False):
            result = commands.move(azCurrent, SAFE_ELEVATION_E_CONFIG, ants=antlist, waiton=commands.ALL)
            if result != None: legitReturnValue = True
            else: print "Move command returned None value; retrying..."
        if len(result.notready) > 0:
            m  = 'ERROR: Could not move all antennas to safe elevation:'
            m += utils.list2string(result.notready) 
            commands.trackMessage(m, printLog=ELOG)
            raise Exception, 'Stopping script'
        if doubleCheckAntennas(antlist, targEl=SAFE_ELEVATION_E_CONFIG):
            break
        else:
            commands.trackMessage('moveAntennasSafely did not achieve correct elevation ('+str(SAFE_ELEVATION_E_CONFIG)+'). Trying again...', printLog=ELOG)

    # STEP 2) Move the antennas in azimuth. 
    # This is a kludge since because of antenna wraps, the azimuth could be 
    # different for each antennas. I am assuming that only the BIMA antennas 
    # could collide and therefore this will not be the case in practice.
    while True:
        result = commands.move(azRequested, SAFE_ELEVATION_E_CONFIG, ants=antlist, waiton=commands.ALL)
        if len(result.notready) > 0:
            commands.trackMessage('ERROR: Could not move all antennas to new azimuth:' + 
                         utils.list2string(result.notready), printLog=ELOG)
            raise Exception, 'Stopping script'
        if doubleCheckAntennas(antlist, targAz=azRequested):
            break
        else:
            commands.trackMessage('moveAntennasSafely did not achieve correct azimuth ('+str(azRequested)+'). Trying again...', printLog=ELOG)

    # STEP 3) Now move the antennas to desired elevaiton.
    while True:
        result = commands.move(azRequested, elRequested, ants=antlist)
        if len(result.notready) > 0:
            commands.trackMessage('ERROR: Could not move all antennas to new elevation:' + 
                         utils.list2string(result.notready), printLog=ELOG)
            raise Exception, 'Stopping script'
        if doubleCheckAntennas(antlist, targEl=elRequested):
            break
        else:
            commands.trackMessage('moveAntennasSafely did not achieve correct elevation ('+str(elRequested)+'). Trying again...', printLog=ELOG)


def basicEtrack(source, waiton=None, tmo=500, ants=0):
    """ Special track command for E array 

        Inputs have their same meaning as for the track() command.
        Does not return ready/not ready lists
    """

    # Check the list of antennas to be moved.
    # If the antenna list consists *only* of outer antennas, 
    # then we can just use the track command. If it consists of both 
    # inner and outer antennas, then we need to be more careful.
    antlist = helper.makeList(ants)
    if ants == 0: antlist = commands.currentAntennaNumbers()
    outerAntennasOnly = True
    for a in antlist:
        # Get antenna status
        isSafe, isInner = isSafePosition(a)

        # Check for any Inner antennas
        if isInner:
           outerAntennasOnly = False
           break
    if outerAntennasOnly:
        commands.s.track(source, antlist, True, commands.TRACKTIME, 25.0, False)
        return

    # "ants" argument not accepted for rest of etrack
    if ants <> None and ants != 0:
        commands.trackMessage("WARNING: ignoring 'ants' argument for etrack()",
              printLog=ELOG)

    # Get information for ALL antennas even if they are not in the subarray.
    # All error handling conditions are made in this function, including
    # stopping moving antennas and aborting script if antennas are not in a
    # safe position.
    antennas = getAntennaInformation(source)

    # An antenna is listed in only ONE of the following lists:
    # antlistOuter() contains the outer antennas that are moveable
    # antlistInner() contains the inner antennas that are moveable
    antlistOuter = list()
    antlistInner = list()
    for ant, info in antennas.items():
        if utils.inSubarray(ant): 
            if not info['inner']:
                antlistOuter.append(ant)
            elif info['inner'] and isAntennaMoveable(ant) == True:
                antlistInner.append(ant)

    # Make sure there are antennas to move!
    if len(antlistOuter) + len(antlistInner) == 0:
        commands.trackMessage('Uh oh - there are no more antennas to move', 
              printLog=ELOG)
        raise Exception,'Aborting script: no antennas to move'

    # Move antennas in outer array immediately
    if len(antlistOuter) > 0: 
        commands.trackMessage('Moving antennas in outer array :' + 
                     utils.list2string(antlistOuter), printLog=ELOG)
        commands.s.track(source, antlistOuter, True, commands.TRACKTIME, 25.0, False)

    # Make sure all inner-array antennas are pointed in the same direction and
    # the same wrap. Do this by computing the maximum separation in degrees. 
    # Also, compute:
    #     a) the maximum slew distance in degrees
    #     b) minimum elevation of antennas
    maxsepAntennas = 0.0
    maxsepSlew = 0.0
    minimumCurrentElevation = 90.0
    for i in antlistInner:
        # Compute maximum azimuth slew distance to next source
        maxsepSlew = max(maxsepSlew, antennas[i]['deltaAz'])

        # Set minimum current elevation
        minimumCurrentElevation = min(minimumCurrentElevation, antennas[i]['el'])

        # Compare against other antennas
        for j in antlistInner:
            # Compute separation
            dist = math.sqrt((antennas[i]['az'] - antennas[j]['az'])**2 + \
                             (antennas[i]['el'] - antennas[j]['el'])**2)

            # Compute maximum separation between antennas
            maxsepAntennas = max(maxsepAntennas, dist)

    # Move the inner antennas. If the maximum antennas separation plus
    # the maximum slew distance is more than 30 degrees, then move one 
    # antenna at a time in safe-mode. Otherwise, we can move them all at once.
    if len(antlistInner) == 0:
        # No antennas to move
        commands.trackMessage('No inner-array antennas to move', printLog=ELOG)
    elif minimumCurrentElevation >= SAFE_ELEVATION_E_CONFIG and \
         antennas[antlistInner[0]]['elRequested'] >= SAFE_ELEVATION_E_CONFIG:
        # All inner antennas are above safe elevation. Use track command()
        commands.trackMessage('Moving antennas in inner array at high elevation:' + 
                     utils.list2string(antlistInner), printLog=ELOG)
        result = commands.s.track(source, antlistInner, True, commands.TRACKTIME, 25.0, False)
        #if len(result.notready) > 0:
        #    commands.trackMessage('Error moving antennas with track command: ' + \
        #                 utils.list2string(result.notready), printLog=ELOG)
        #    raise Exception, 'Abort script'
    elif maxsepAntennas + maxsepSlew <= MAXIMUM_AZSLEW:
        # Slew distance is OK. Use s.track() command.
        commands.trackMessage('Moving antennas in inner array (small az slew):' + 
                     utils.list2string(antlistInner), printLog=ELOG)
        result = commands.s.track(source, antlistInner, True, commands.TRACKTIME, 25.0, False)
        #if len(result.notready) > 0:
        #    commands.trackMessage('Error moving antennas with track command: ' + \
        #                 utils.list2string(result.notread), printLog=ELOG)
        #    raise Exception, 'Abort script'
    elif maxsepAntennas <= MAXIMUM_AZSLEW:
        # Too long of a combined slew (maxsepAntennas + maxsepSlew), but 
        # antennas are pointed in the same direction. Use commands.move() command 
        # to slew antennas at the same time.
        commands.trackMessage('Moving antennas in inner array in safe mode:' + 
                     utils.list2string(antlistInner), printLog=ELOG)
        j = antlistInner[0]
        print 'Maximum separation between antennas =',maxsepAntennas,' degrees.' # XXX
        moveAntennasSafely(antlistInner, antennas[j]['az'], antennas[j]['el'], 
                      antennas[j]['azRequested'], antennas[j]['elRequested'])
    else:
        # Antennas not pointed in same direction are too far apart. 
        # Use commands.move() command one antennas at a time.
        commands.trackMessage('Antennas are not pointed in same direction.', 
              printLog=ELOG)
        commands.trackMessage('Will move antennas one at a time in safe mode', 
              printLog=ELOG)

        # Get requested azimuth/elevation. This should be the same for
        # all antennas. antlistMoved() lists the antennas that are already
        # at a safe high elevation and do not need to be moved further.
        antlistMoved = list()
        azRequested = antennas[antlistInner[0]]['azRequested']
        elRequested = antennas[antlistInner[0]]['elRequested']
        for a in antlistInner:
            if abs(azRequested - antennas[a]['azRequested']) > 0.1:
                 raise Exception, 'Error slewing to azimuth'
            if antennas[a]['el'] >= SAFE_ELEVATION_E_CONFIG: 
                antlistMoved.append(a)

        # We need to move all antennas to a high elevation. We will do this
        # by moving the antennas at high elevation first. The variable
        # "movedAntenna" should not be necessary, but I added it just in
        # case to prevent an infinite loop.
        movedAntenna = True
        while movedAntenna and len(antlistMoved) < len(antlistInner):
            # Get highest elevation antenna remaining to be moved
            antHighest = None
            movedAntenna = False
            for a in antlistInner:
                try:
                    antlistMoved.index(a)
                except:
                    if (antHighest == None or 
                        antennas[a]['el'] > antennas[antHighest]['el']):
                        antHighest = a
                    
            # Move antenna
            if antHighest <> None:
                commands.trackMessage('Moving antenna ' + str(antHighest), printLog=ELOG)
                moveAntennasSafely(antHighest,
                    antennas[antHighest]['az'], antennas[antHighest]['el'], \
                    antennas[antHighest]['az'], SAFE_ELEVATION_E_CONFIG)
                movedAntenna = True
                antlistMoved.append(antHighest)

        # Slew in azimuth at high elevation
        commands.trackMessage('Moving antennas to desired azimuth', printLog=ELOG)
        result = commands.move(azRequested, SAFE_ELEVATION_E_CONFIG, antlistInner)
        if len(result.notready) > 0:
            commands.trackMessage('Error moving antennas with move command: ' + \
                         utils.list2string(result.notready), printLog=ELOG)
            raise Exception, 'Abort script because etrack failed'

        # Slew to desired az/el
        commands.trackMessage('Moving antennas to desired elevation', printLog=ELOG)
        result = commands.move(azRequested, elRequested, antlistInner)
        if len(result.notready) > 0:
            commands.trackMessage('Error moving antennas with move command: ' + \
                         utils.list2string(result.notready), printLog=ELOG)
            raise Exception, 'Aborting script because etrack failed'

