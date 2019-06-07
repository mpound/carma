# obsdef.py
#
# @author John Carpenter
# $Id: obsdef1.py,v 1.19 2012/10/05 03:23:57 abeard Exp $
#
#   History
#   2006-Nov-30  JMC    Original
#   2007-Jan-07  JMC    Added weather statistics to log output
#   2007-Jan-08  JMC    Added function to change correlator bands to 
#                       recommended continuum configuration.
#   2007-Jan-11  JMC    Enabled ability to observe hybrid correlator modes,
#                       and multiple sources per mosaic.
#   2007-Jan-16  JMC    Added option to support Doppler tracking for each 
#                       source in the "sources" list.
#   2007-Jan-17  JMC    Increased storage registers for script variables.
#                       Add function for to check/report which antennas 
#                       are online.
#   2007-Jan-18  JMC    Cleaned up elevation limits such that elevlimit()
#                       is never called in the script.
#   2007-Jan-26  JMC    Added parameters that enable multiple flux calibrators
#                       to be observed in a track in a straight foward manner
#   2007-Feb-22  JMC    Added parameters to observe secondary flux calibrators
#   2007-Jun-20  MWP    Updated to conform to project database use.
#

import string
import time
import os
import short
import copy
import printFunctions
from   subarrayCommands import *
from   carmaHelpers     import *
import refPoint
import fileIOPython
from obsdefIndex import *
import etrack

# For sendEmailMessage()
import smtplib
import email
import email.MIMEMultipart
import email.MIMEText

import math
import string
import sys

CORR_BW500    = 1
CORR_BW500LO  = 2
CORR_BW500LO6 = 3
TRECORD_NOISE = 10.0   # Default record length for noise integration

def minimumIntegrationTime():
    """ Returns minimum integration time in seconds allowed by the control system"""
    return 2.0


def overhead():
    """ Adopted overhead factor for observing.

        Inputs: None

        Output: Overhead factor (currently 1.3)

        Example: x = overhead()
    """
    return 1.3


def getWeather(formatString=False):
    """ Return the ambient temperature (C), water vapor (mm), 
        wind speed (mph), and phase RMS (um).

        Inputs: formatString - if true, format return varaibles as 
                               string variables instead of floating point.

        Example: tamb, pmv, wind, phaseRms = getWeather()
    """ 
    # Ambient temperature
    try:
        tamb = queryDouble('Weather.ambientTemperature')
        tamb = 32.0 + 1.8 * tamb
        if formatString: tamb = str('%.1f' % tamb)
    except Exception, ex:
        tamb = '?'

    # Water vapor
    try:
        pwv = queryDouble('Weather.precipWater')
        if formatString: pwv = str('%.1f' % pwv)
    except Exception, ex:
        pwv = '?'

    # Wind speed
    try:
        wind = queryDouble('Weather.windSpeed')
        if formatString: wind = str('%.1f' % wind)
    except Exception, ex:
        wind = '?'

    # phase RMS
    try:
        phaseRms = queryDouble('PhaseMonitor.skyRMS')
        if formatString: phaseRms = str('%d' % int(phaseRms))
    except Exception, ex:
        phaseRms = '?'

    # Done
    return tamb, pwv, wind, phaseRms


def printWeather(timestamp=True, printLog=True, indent=1):
    """ Prints current weather conditions to the screen and script log"""

    # Get weather conditions
    tamb, pwv, wind, phaseRms = getWeather(formatString=True)

    # Print weather
    msg = 'Tamb=' + str('%5s' % tamb) + ' F   ' + \
          'pwv=' + str('%4s' % pwv) + ' mm   ' + \
          'wind=' + str('%4s' % wind) + ' mph  ' + \
          'phaseRMS=' + str('%4s' % phaseRms) + ' um';
    trackMessage(msg,indent=indent)


def lastcal():
    """ End source/phase-calibrator cycles and observe calibrator one last time.

        The current integration will complete before going to the last 
        calibrator. Also, if there are multiple phase calibrators in the
        track, lastcal() will only end the current phase calibrator cycle.
    """
    s.setScriptBool(INDX_BOOL_LASTCAL,True)
    trackMessage('lastcal() command issued')


def stopAfterNextCal():
    """ End source/phase-calibrator cycles after observing next 
        observation of the phase calibrator.

        If there are multiple phase calibrators in the track, 
        stopAfterNextCal() will only end the current phase calibrator cycle.
    """
    s.setScriptBool(INDX_BOOL_STOPNEXTCAL,True)
    trackMessage('stopAfterNextCal() command issued')


def endtrack(lstend=None):
    """ Set ending LST time for track.

        Inputs: lstend - ending LST time in HH:MM:SS or hours.
                         If not set, then end track after current integration
                         is complete.
    """
    if lstend == None:
        s.setScriptBool(INDX_BOOL_ENDTRACK,True)
        trackMessage('endtrack() command issued')
    else:
        setLstEndTrack(lstend)
        s.setScriptBool(INDX_BOOL_ENDTRACK,False)
        trackMessage('Setting endtrack time to ' + convertHmsString(lstend) +
                     ' LST')


def setTmax(t, verbose=True):
    """ reset maximum track length (hours) """
    if t == None:
        s.setScriptDouble(INDX_DBL_TMAX,0.0)
        s.setScriptBool(INDX_BOOL_TMAX,False)
        if verbose:
            trackMessage('Unsetting maximum track length')
    else:
        s.setScriptDouble(INDX_DBL_TMAX,convertHms(t))
        s.setScriptBool(INDX_BOOL_TMAX,True)
        if verbose:
            trackMessage('Setting maximum track length to '+dtString(getTmax()))


def setLstStartCycle(t, verbose=True):
    """ reset LST start time (hh:mm:ss or hours) for the source/phaseCal cycle """
    if t == None:
        s.setScriptDouble(INDX_DBL_LST_START_CYCLE,0.0)
        s.setScriptBool(INDX_BOOL_LST_START_CYCLE,False)
        if verbose: trackMessage('Unsetting LST start cycle')
    else:
        s.setScriptDouble(INDX_DBL_LST_START_CYCLE,convertHms(t))
        s.setScriptBool(INDX_BOOL_LST_START_CYCLE,True)
        if verbose: 
            trackMessage('Setting LST start cycle to ' + 
                  convertHmsString(getLstStartCycle()))


def setLstStopCycle(t, verbose=True):
    """ reset LST stop time (hh:mm:ss or hours) for the source/phaseCal cycle """
    if t == None:
        s.setScriptDouble(INDX_DBL_LST_STOP_CYCLE,0.0)
        s.setScriptBool(INDX_BOOL_LST_STOP_CYCLE,False)
        if verbose: trackMessage('Unsetting LST stop cycle')
    else:
        s.setScriptDouble(INDX_DBL_LST_STOP_CYCLE,convertHms(t))
        s.setScriptBool(INDX_BOOL_LST_STOP_CYCLE,True)
        if verbose: 
            trackMessage('Setting LST stop cycle to ' + 
                  convertHmsString(getLstStopCycle()))


def setLstLastPoint(t):
    """ Stores LST time (hh:mm:ss or hours) of last radio pointing observation """
    if t == None:
        s.setScriptDouble(INDX_DBL_LASTPOINT,0.0)
        s.setScriptBool(INDX_BOOL_LASTPOINT,False)
    else:
        s.setScriptDouble(INDX_DBL_LASTPOINT,convertHms(t))
        s.setScriptBool(INDX_BOOL_LASTPOINT,True)


def setLstLastFluxcal(t):
    """ Stores LST time (hh:mm:ss or hours) of last flux cal observation """
    if t == None:
        s.setScriptDouble(INDX_DBL_LASTFLUX,0.0)
        s.setScriptBool(INDX_BOOL_LASTFLUX,False)
    else:
        s.setScriptDouble(INDX_DBL_LASTFLUX,convertHms(t))
        s.setScriptBool(INDX_BOOL_LASTFLUX,True)


def setLastMosaicPosition(lastpos):
    """ Set last observed mosaic position that was observed"""
    if lastpos <= 0:
        s.setScriptBool(INDX_BOOL_MOSPOS,0)
    else:
        s.setScriptInt(INDX_INT_MOSPOS,lastpos)


def addSourceTime(dt):
    """ Add dt hours to time spent in source/phase-cal cycle  """
    t = s.getScriptDouble(INDX_DBL_TSOURCE) + dt
    s.setScriptDouble(INDX_DBL_TSOURCE,t)


def getLstStartTrack():
    """ Return LST starting time (hours) for the current track """
    t = None
    if s.getScriptBool(INDX_BOOL_LST_START_TRACK):
        t = s.getScriptDouble(INDX_DBL_LST_START_TRACK)
    return t


def getTmax():
    """ Returns maximum track length (hours) for the current track """
    t = None
    if s.getScriptBool(INDX_BOOL_TMAX):
        t = s.getScriptDouble(INDX_DBL_TMAX)
    return t


def getLstStartCycle(string=False, default=None):
    """ Returns LST start time (hours) for the current track """
    t = default
    if s.getScriptBool(INDX_BOOL_LST_START_CYCLE):
        t = s.getScriptDouble(INDX_DBL_LST_START_CYCLE)
    if string == True: t = convertHmsString(t)
    return t


def getLstStopCycle(default=None, string=False):
    """ Returns LST stop time (hours) for the current track """
    t = default
    if s.getScriptBool(INDX_BOOL_LST_STOP_CYCLE):
        t = s.getScriptDouble(INDX_DBL_LST_STOP_CYCLE)
    if string == True: t = convertHmsString(t)
    return t


def getLstEndTrack(string=False):
    """ Returns LST end time (hours) for the current track """
    t = None
    if s.getScriptBool(INDX_BOOL_LST_END_TRACK):
        t = s.getScriptDouble(INDX_DBL_LST_END_TRACK)
    if string == True: t = convertHmsString(t)
    return t


def getLstLastPoint(string=False):
    """ Returns LST time (hours) of last radio pointing observation """
    t = None
    if s.getScriptBool(INDX_BOOL_LASTPOINT):
        t = s.getScriptDouble(INDX_DBL_LASTPOINT)
    if string == True: t = convertHmsString(t)
    return t


def getLstLastFluxcal(string=False):
    """ Returns LST time (hours) of flux cal observation """
    t = None
    if s.getScriptBool(INDX_BOOL_LASTFLUX):
        t = s.getScriptDouble(INDX_DBL_LASTFLUX)
    if string == True: t = convertHmsString(t)
    return t


def getLastMosaicPosition(startpos=1):
    """ Returns last observed mosaic position """
    lastpos = s.getScriptInt(INDX_INT_MOSPOS)
    if lastpos < 1: lastpos = startpos - 1
    return lastpos


def appendHistory(msg, delim='\n'):
    """ Append comment to script history """

    # If log is full, then there is nothing to do
    if s.getScriptBool(INDX_BOOL_HISTORYFULL): return

    # Get current indx
    indx = max(MIN_INDX_HISTORY, s.getScriptInt(INDX_INT_HISTORY))

    # Finite number of registers
    logfull = 'History file truncated'
    maxRegisters = 100

    # Get size of current message
    l = len(s.getScriptString(indx)) 
    maxlen = 4999 - len(msg)
    if indx >= maxRegisters: maxlen -= len(logfull)

    # Increase size of registers if needed
    message = msg
    if l >= maxlen: indx += 1

    # Make sure log is not full
    if indx > maxRegisters:
        s.setScriptBool(INDX_BOOL_HISTORYFULL,True)
        message = logfull
        indx = maxRegisters
        trackMessage(logfull,printLog=False)
    else:
        s.setScriptInt(INDX_INT_HISTORY,indx)

    # Append message
    addScriptString(indx, message)


def addScriptString(indx, svar, bindx=None, delim='\n', allowDup=True, 
                    append=True):
    """ Appends string to system script variable.

        Inputs: indx  - index value to setScriptString()
                svar  - string to append to system string
                bindx - if set, then set setScriptBool(bindx) to True
                append    - If True, append to current string
                delim - deliminator for appending strings
                allowDup  - If False, do not allow duplicate entries in string

    """
    # Nothing to do if svar is empty
    if svar == None or svar == "": return

    # Add string
    if append:
        # add delimiter
        msgAppend = svar
        msgOrig = s.getScriptString(indx)
        if msgOrig != '' and msgOrig != None: msgAppend = delim + svar

        # If dup=False, make sure svar has not already been entered
        if not allowDup and msgOrig <> None:
            t = msgOrig.split(delim)
            if svar in t: return

        # Append string
        s.appendScriptString(indx, msgAppend)
    else:
        msg = svar
        if msg == None: msg = ''
        s.setScriptString(indx,msg)
    if bindx != None: s.setScriptBool(bindx, True)


def getTrackHistory():
    """ Returns track history """
    msg = ''
    for i in range(MIN_INDX_HISTORY, s.getScriptInt(INDX_INT_HISTORY)+1):
        if i > MIN_INDX_HISTORY: msg += '\n'
        msg += s.getScriptString(i)
    if msg == "": msg = None
    return msg


def getLocalTime():
    """ Returns current time/date as a string """

    now = time.localtime(time.time())
    return time.asctime(now)


def checkAntennas(indent=None):
    # Get the current antennas in the subarray
    antennas = currentAntennaNumbers()

    # Convert antennas list to a string
    antStr = ''
    for a in antennas:
        if antStr != "": antStr += ' '
        antStr += str("%d" %  a)

    # Get old antenna string list
    antStrOld = s.getScriptString(INDX_STR_ANTENNAS)

    # If the old list is empty, then just print the current antennas on
    # line. If it is not empty, check to see if any antennas have been
    # added or removed
    if antStrOld == "":
        offline = ''
        for a in range(1,16):
             try :
                 antennas.index(a)
             except Exception:
                 if offline == '': offline += ''
                 offline += str(' %s' % a)
        trackMessage(str(len(antennas)) + ' antennas online',indent=indent)
        if offline != '':
            trackMessage('antennas offline : ' + offline,indent=indent)
    elif antStr == antStrOld:
        # Nothing to do
        pass
    else:
        # Determine which antennas were on online
        antennasOldList = antStrOld.split()
        antennasNewList = antStr.split()

        # Find antennas removed from subarray
        for a in antennasOldList:
             try :
                 antennasNewList.index(a)
             except Exception:
                 trackMessage('Antenna ' + a + ' was taken offline', indent=indent)

        # Find antennas added from subarray
        for a in antennasNewList:
             try :
                 antennasOldList.index(a)
             except Exception:
                 trackMessage('Antenna ' + a + ' is now online', indent=indent)

    # Store new antennas in memory
    s.setScriptString(INDX_STR_ANTENNAS, antStr)


def endOfTrack():
    """ Returns True/False if end of track has been issued """
    return s.getScriptBool(INDX_BOOL_ENDTRACK)


def isLastcal():
    """ Returns True/False if lastcal() has been issued """
    return s.getScriptBool(INDX_BOOL_LASTCAL)


def isStopAfterNextCal():
    """ Returns True/False if stopAfterNextCal() has been issued """
    return s.getScriptBool(INDX_BOOL_STOPNEXTCAL)


def finishedPassband():
    """ Returns True/False if a flux calibrator has been observed """
    return s.getScriptBool(INDX_BOOL_PASSBAND)


def finishedFluxcal():
    """ Returns True/False if a flux calibrator has been observed """
    return (s.getScriptInt(INDX_INT_NOBS_FLUXPRI) >= 
            s.getScriptInt(INDX_INT_NFLUXTRACK))


def getNfluxRemaining(fluxParams):
    nfluxCycle = 1
    if fluxParams <> None: 
        if fluxParams['nfluxCycle'] <> None:
             nfluxCycle = fluxParams['nfluxCycle']

    nflux = s.getScriptInt(INDX_INT_NFLUXTRACK) - \
            s.getScriptInt(INDX_INT_NOBS_FLUXPRI)
    if nflux < 0: nflux = 0

    return min(nflux, nfluxCycle)


def resetFluxcal():
    """ Sets number of observed flux calibrators to zero """
    return s.setScriptInt(INDX_INT_NOBS_FLUXPRI, 0)

def dtString(dt, ndec=1):
    """ Reports the time interval in hours, unless dt < 1.0,
        when it reports it in minutes, or seconds if dt < 1/60.

        ndec = number of decimal points to print dt
    """
    unit = 'hours'
    format = "%." + str(ndec) + "f"
    if abs(dt) < 1.0:
        dt *= 60.0
        unit = 'min'
        if abs(dt) < 1.0:
            dt *= 60.0
            unit = 'sec'
            if ndec == 1:
                format = "%d"
                dt = round(dt)
    tmp = str(format % dt) + ' ' + unit
    return tmp


def readRestartVariables(var, mosaic):
    """ Reads system variables upon restarting track 

        Inputs:  var    - variable dictionary
                 mosaic - mosaic structure
    """

    # Print message
    trackMessage("Restarting observing script")

    # Options
    tmp = s.getScriptString(INDX_STR_OPTIONS)
    if tmp != "" and tmp <> None:
        trackMessage('    reading previous command line options : ' + tmp)
        readCommandLineOptions(tmp, var, mosaic=mosaic, verbose=False)

    # Check status of calibrations
    if s.getScriptBool(INDX_BOOL_PASSBAND):
        trackMessage('    passband calibrator (' +
              s.getScriptString(INDX_STR_PASSBAND) + 
              ') has already been observed')
    if s.getScriptBool(INDX_BOOL_FLUXPRIMARY):
        trackMessage('    primary flux calibrator (' +
              s.getScriptString(INDX_STR_FLUXPRIMARY) + 
              ') has already been observed')
    if s.getScriptBool(INDX_BOOL_FLUXSECONDARY):
        trackMessage('    secondary flux calibrator (' +
              s.getScriptString(INDX_STR_FLUXSECONDARY) + 
              ') has already been observed')

    # Time stamps
    if s.getScriptBool(INDX_BOOL_LST_START_TRACK):
        trackMessage('    track originally started at ' +
                     convertHmsString(getLstStartTrack()) + ' LST')
    if s.getScriptBool(INDX_BOOL_TMAX):
        t = s.getScriptDouble(INDX_DBL_TMAX)
        trackMessage('    maximum time for track is ' +
              str("%.1f" % t) + ' hours')
    if s.getScriptBool(INDX_BOOL_LASTPOINT):
        trackMessage('    pointing last performed at ' +
              convertHmsString(getLstLastPoint()) + ' LST')
    if s.getScriptBool(INDX_BOOL_LST_START_CYCLE):
        lststart = s.getScriptDouble(INDX_DBL_LSTSTART)
        trackMessage('    setting cycle start time to ' +
              convertHmsString(lststart) + ' LST')
    if s.getScriptBool(INDX_BOOL_LST_STOP_CYCLE):
        lststop = s.getScriptDouble(INDX_DBL_LST_STOP_CYCLE)
        trackMessage('    setting cycle stop time to ' +
              convertHmsString(lststop) + ' LST')


    # Starting position in mosaic
    lastpos = getLastMosaicPosition()
    if lastpos > 0:
        trackMessage('    last observed position in mosaic was position ' +
                     str(lastpos) + '')

    # Reset stop cycle
    s.setScriptBool(INDX_BOOL_STOPNEXTCAL,False)
    s.setScriptBool(INDX_BOOL_LASTCAL,False)
    s.setScriptBool(INDX_BOOL_ENDTRACK,False)


def trackError(msg, timestamp=False, printLog=True, indent=0, alarm=False):
    """ Prints a  message to the screen.

        Inputs: msg       - message to print 
                timestamp - if True, print LST time stamp
                printLog  - in addition to printing the message to the 
                            screen, print the message to the control log
                indent    - indent the message by indent*4 spaces before
                            printing msg.
                alarm     - if True, trigger the alarm
    """
    s = ''
    if timestamp: s = convertHmsString(lst()) + ' '
    for i in range(indent): s = s + '    '
    s += msg
    printFunctions.printError(s,alarm=alarm)
    s = 'ERROR: ' + s
    scriptlog(s)
    appendHistory(s)


def makeDictionary(sources):
    """ Convert the variable "sources" into a dictionary.

        Input : sources - a variable, list, or dictionary

        Output: Return value is a dictionary.
                If input is a single value, then output is {sources: true}
                If input is a list, each list value is returned as {list:True}
                If input is a dictionary, the result is copied

        Examples:
            (1) d = makeDictionary('3c273')
            (2) d = makeDictionary(['3c273','3c279'])
            (3) d1 = {'3c273':True, '3c279': False}
                d = makeDictionary(d1)
    """
    if sources == None: return None
    d = dict()
    if dict in [type(sources)]:
        d = sources.copy()
    else:
        l = makeList(sources)
        for name in l:
            d[name] = True
    return d.copy()


def makeList(sources):
    """ Convert the variable "sources" into a list.

        Input : sources - a variable, list, or dictionary (key=>boolean)
        Output: Return value is a list.
                If input is a single value, then output is [sources]
                If input is a list, the result is copied
                If input is a dictionary, all of the True values are returned

        Examples:
             (1) l = makeList('3c273')
             (2) l = makeList(['3c273','3c279'])
             (3) l1 = {'3c273':True, '3c279': False}
                 l = makeDictionary(l1)
    """
    if sources == None: return None
    l = list()
    t = [type(sources)]
    if dict in t:
        for name in sources:
            if sources[name]: l.append(name)
    elif list in t:
        l = sources[:]
    else:
        l = [sources]
    if len(l) == 0: return None
    return l[:]


def isSourceUp(source, el=None):
    """ Returns True or False if source above an elevation limit of el degrees.

        If el is not specified, the current system elevation limit is used.
    """

    isUp = (isup(source) != 0)
    if el <> None:
        isUp = (getSourceElevation(source) >= el)
    return isUp


def getSourceInfo(source, el=None):
    """ Get source info from the check source command """

    # it is faster to use the s.info command if elevation is not specified, 
    if el == None:
        result = s.info(source)
    else:
        # Start the command
        cmd = "/opt/rt/bin/checksource source=" + source + \
              " elevlim=" + str("%.2f" % el)

        # Add catalog
        try :
            ucat = queryString('Control.Subarray%d.userCatalog' % s.getSubarrayNo())
            if string.upper(ucat) <> "NONE":
                cmd += " catalog=/array/rt/catalogs/" + ucat
        except Exception:
            pass

        # Execute command
        mycmd = os.popen(cmd)
        result = mycmd.read()
        mycmd.close()

    # Return result
    return result


def getElevationLimit():
    """ Return the current elevation limit in degrees for the subarray """
    el_limit_old = s.getScriptDouble(INDX_DBL_EL_LIMIT)
    try :
       el_limit = queryDouble('Control.Subarray%i.elevLimit' % s.getSubarrayNo() )
       s.setScriptDouble(INDX_DBL_EL_LIMIT, el_limit)
    except Exception:
       trackMessage('Error reading elevation limit - using previous value')
       el_limit = el_limit_old
    return el_limit


def getSourceRa(source):
    """ Return right ascention as a string for 'source' in the current catalog"""
    return getSourceInfo(source).split('\n')[1].split()[1]


def getSourceDec(source):
    """ Return declination as a string for 'source' in the current catalog"""
    return getSourceInfo(source).split('\n')[1].split()[2]


def getSourceElevation(source):
    """ Return elevation as a floating point for 'source' in the current catalog"""
    return azel(source)[1]


def getLstRise(source, el=None):
    """ Return LST rise time as a string for 'source' in the current catalog

        Input : source - name of source in current catalog
                el     - If set, use specified elevation limit instead of 
                         current system elevation.

        Output: return rising LST as a string
    """
    # Get rising lst
    lst = None
    info = getSourceInfo(source,el=el).split('\n')[1]
    if info.find('NEVER RISES') >= 0: 
        lst = None
    elif info.find('NEVER SETS') < 0: 
        lst = info.split()[5]

    # Done
    return lst


def getLstSet(source, el=None):
    """ Return LST set time as a string for 'source' in the current catalog

        Input : source - name of source in current catalog
                el     - If set, use specified elevation limit instead of 
                         current system elevation.

        Output: return setting LST as a string
    """
    # Get setting lst
    lst = None
    info = getSourceInfo(source, el=el).split('\n')[1]
    if info.find('NEVER RISES') >= 0: 
        lst = None
    elif info.find('NEVER SETS') < 0: 
        lst = info.split()[6]

    # Done
    return lst


def getBrightSources(fluxLimit=4.0):
    """ Retrieves source from the flux catalog brighter than the
        specified flux limit in Janskys.
    """

    # Read the flux file. If files does not exist, then return list of
    # hardcoded codes
    fluxFile = '/opt/rt/conf/catalogs/FluxSource.cat'
    if not os.path.exists(fluxFile):
        return ['3c279', '3c273', '1924-292', '3c84', '3c454.3', '3c345',
                '1751+096', '0927+390', '0359+509', '3c446', '1058+015',
                '0530+135', '2148+069', '0423-013']
    a = fileIOPython.fileToTable(fluxFile,ignoreEmpty=True)

    # Read sources in system catalog
    sourceFile = '/opt/rt/conf/catalogs/SystemSource.cat'
    sourcecat = fileIOPython.fileToTable(sourceFile,ignoreEmpty=True)
    sources = list()
    nsources = len(sourcecat)
    for i in range(nsources):
        sources.append(string.upper(sourcecat[i][0]))
    
    # Select only recent epoch data
    brightSources = list()
    n   = 0
    sum = 0.0
    sourceName = ''
    for z in a:
        # Skip comments ("#")
        if z[0] == '#' or len(z) == 1: continue

        # If ##, this is a new source
        if z[0] == '##':
            if n > 0 and sourceName != '':
                if sum / n >= fluxLimit: brightSources.append(sourceName)
            n   = 0
            sum = 0.0
            sourceName = z[1]

            # Read aliases, and find which alias is actually in the catalog
            if z[0] == '##':
                aliases = z[1:]
                for name in aliases:
                    try:
                        sources.index(string.upper(name))
                        sourceName = name
                        break
                    except:
                        # Source not in catalog
                        pass
        else:
            # Read frequency/observatory
            if len(z) != 6: continue
            freq = string.atof(z[2])
            obs  = z[5]

            # Must be 3mm, and must be carma
            if freq < 80.0 or freq > 117.0 or obs.find('CARMA') != 0: continue

            # Sum
            sum += string.atof(z[3])
            n += 1

    # Add last source
    if n > 0 and sourceName != '':
        if sum / n >= fluxLimit: brightSources.append(sourceName)

    # Return list
    return brightSources


def getSourceFlux(source):
    """ Returns source flux in Janskys """

    # Read the flux file. If files does not exist, then return list of
    # hardcoded codes
    fluxFile = '/opt/rt/conf/catalogs/FluxSource.cat'
    if not os.path.exists(fluxFile): return 0.0
    a = fileIOPython.fileToTable(fluxFile,ignoreEmpty=True)

    # Select only recent epoch data
    brightSources = list()
    n   = 0
    sum = 0.0
    sourceName = ''
    sourceUpper = string.upper(source)
    aliases = list()
    for z in a:
        # Skip comments ("#")
        if z[0] == '#' or len(z) == 1: continue

        # If '##', then read aliases
        if z[0] == '##':
            aliases = z[1:]
            continue

        # Is this the correct source?
        sourceName = string.upper(z[0])
        ok = (sourceName == sourceUpper)
        if not ok:
            for t in aliases:
                if string.upper(t) == sourceUpper: ok = True

        # Read source name
        if ok:
            # Read frequency/observatory
            if len(z) != 6: continue
            freq = string.atof(z[2])
            obs  = z[5]

            # Must be 3mm, and must be carma
            if freq < 80.0 or freq > 117.0 or obs.find('CARMA') != 0: continue

            # Sum
            sum += string.atof(z[3])
            n += 1

    # Add last source
    flux = 0.0
    if n > 0: flux = sum / n

    # Return flux
    return flux


def getDistance( source, ref ) :
    """Return angular distance between a source and reference."""
    srcAzEl = getSourceElevation(source)
    refAzEl = getSourceElevation(ref)
    deg2rad = 180.0 / math.pi;
    srcAz   = srcAzEl[0] / deg2rad;
    srcEl   = srcAzEl[1] / deg2rad;
    refAz   = refAzEl[0] / deg2rad;
    refEl   = refAzEl[1] / deg2rad;
    cosDist = math.sin( refEl ) * math.sin( srcEl ) + \
              math.cos( refEl ) * math.cos( srcEl ) * math.cos(refAz - srcAz);
    dist    = math.acos( cosDist )*deg2rad;
    return  float(dist);

def setCorrCal(mode, configCorr = None) :
    """ Set correlator configuration to continuum mode for calibrator observations.

        Inputs: mode  - If mode = 1 = CORR_BW500, change all bands to
                        500 MHz and keep LO2 the same.
                        If mode = 2 = CORR_BWLO500, change all bands to
                        500 MHz and change LO2 to have 3 non-overlapping bands.
                        If mode = 3 = CORR_BWLO5006, change all bands to
                        500 MHz and change LO2 to have 3 non-overlapping bands
                        within the BIMA IF.
                configCorr - template correlator configuration. If None, then
                             the current correlator configuration is used.

        Output: new correlator configuration

        Example: configNew = setCorrcal(CORR_BW500)
    """

    # Get configuration
    corrConfigOld = configCorr
    if corrConfigOld == None: corrConfigOld = short.getConfigCorrT()

    # Change configuration
    if mode == CORR_BW500LO:
        # Change IF so bands to not overlap in BIMA IF.
        wideBand = configCorrWidebandCarma(corrConfigOld)
    elif mode == CORR_BW500LO6:
        # Change IF so bands to not overlap in BIMA IF.
        wideBand = configCorrWidebandBima(corrConfigOld)
    elif mode == CORR_BW500:
        # change each band to 500 MHz, but bands may overlap
        wideBand = corrConfigOld[:]
        for i in range(len(wideBand)):
            wideBand[i][1] = BW500
    else:
        raise Exception, 'Error reading correlator configuration: ' + str(mode)

    return wideBand[:]


def configCorrWidebandCarma(configCorr) :
    """ Returns  correlator configuration to wideband continuum.

        Two of the bands are optimized within the BIMA IF, and the third
        is optimized for the OVRO IF.

        Inputs: configCorr - the current correaltor configuration
    """
    # Function is applicable only for <= 3 bands
    if len(configCorr) > 3:
        raise Exception, 'Can only re-configure 3 correlator bands'

    # Initialize
    wideConfigP = []

    # Get frequency setup of current correlator configuration
    fsetup = freqSetup()
    frest  = fsetup[0]
    fif    = fsetup[1]
    flo    = fsetup[2]

    # Determine sideband for rest frequency
    sidebandLO = USB
    if flo > frest: sidebandLO = LSB

    # Set frequency offsets from frest. The nominal frequencies are
    # for a IF frequency of 2.15 GHz and upper sideband. We need
    # to allow for other setups for these parameters.
    dfreq = [-0.5, 0.0, 1.1]
    for i in range(len(dfreq)):
       # Correct for different IF frequency
       df = dfreq[i] - fif + 2.15

       # Correct for sideband
       if sidebandLO == LSB: df *= -1.0

       # Set frequency
       dfreq[i] = df

    # Sideband baseband for downconverter
    sbBaseband = LSB

    # Cycle over correlator bands
    for i in range(len(configCorr)):
        # Get correlator band
        z = configCorr[i]

        # Compute new frequency. Set baseband sideband.
        z[1] = BW500
        z[2] = frest + dfreq[i]
        z[3] = sbBaseband

        # put them all in the lower sideband with 500 MHz bandwidth
        wideConfigP.append(z)
    return wideConfigP


def configCorrWidebandBima(configCorr) :
    """ Returns  correlator configuration to wideband continuum that falls
        within the 6-m IF bandpass.

        Inputs: configCorr - the current correaltor configuration

        Adopted from Stephen White's refPoint routine.
    """
    # Initialize
    wideConfigP = []

    # Get frequency setup of current correlator configuration
    fsetup = freqSetup()
    flo = fsetup[2]
    fif = fsetup[1]

    # Determine sideband
    sbFacLo = 1   # USB
    if (fsetup[2] > fsetup[0]) : sbFacLo = -1 # LSB

    # Set parameters for new correlator configuration
    sbBaseband = LSB
    sbFacBaseband = 1
    if sbBaseband == LSB: sbFacBaseband = -1
    fifNew = 1.79

    # Cycle over correlator bands
    for i in range(len(configCorr)):
        # Band number (1, 2, 3, ...)
        fbn = configCorr[i][0]

        # offset frequency by 460 MHz from center of sideband
        # now try to get all bands in the BIMA LO2 range from 1.1-2.5
        fbb = flo + sbFacLo*(fifNew + sbFacBaseband*(2.-configCorr[i][0])*0.46)
        fbrest = configCorr[i][4]

        # put them all in the lower sideband with 500 MHz bandwidth
        tempConfig = [fbn, BW500, fbb, sbBaseband, fbrest]
        wideConfigP.append(tempConfig)
    return wideConfigP


def getTimeToRise(sourceName, el=None):
    """ Returns time in hours before the source rises again for the current
        elevation limit.

        Inputs: sourceName - source name (must be in the current catalog)
                el - elevation limit in degrees. If None, the current
                     system elevation limit is used.
    """
    # Get LST rise time
    tlst = getLstRise(sourceName, el=el)

    # Compute time difference
    dt = None
    if tlst == None: 
        dt = 0.0
    else:
        dt = timeDifference(tlst, lst())
        if dt < 0.0 and not isSourceUp(sourceName, el=el): dt += 24.0
        if dt > 0.0 and     isSourceUp(sourceName, el=el): dt -= 24.0

    return dt


def getTimeToSet(sourceName, el=None):
    """ Returns time in hours before the source rises again for the current
        elevation limit.

        Inputs: sourceName - source name (must be in the current catalog)
                el - elevation limit in degrees. If None, the current
                     system elevation limit is used.
    """
    # Get LST set time
    tlst = getLstSet(sourceName, el=el)

    # Compute time difference
    dt = None
    if tlst <> None: 
        dt = timeDifference(tlst, lst())
        if dt < 0.0 and     isSourceUp(sourceName, el=el): dt += 24.0
        if dt > 0.0 and not isSourceUp(sourceName, el=el): dt -= 24.0

    return dt


def timeRemaining(source=None, phase=None, lsttime=None,
                  useTime=False, default=999., toRise=False, el=None):
    """ Computes the time in hours remaining for the specified observations.

        If toRise=False, compute the maximum time.
        If toRise=Rise,  compute the minimum time.

        The time remaining in the track in hours is computed as the
        following (assuming they are set on the command line:
           (1) If useTime=True, tmax - lst(current) + lst(start)
           (2) If useTime=True, lst(endtrack) - lst(start)
           (3) If toRise = True:
                  lst(current) - first common rise time of (source+phase)
                  or lst - lst(current) , which ever is longer
           (4) If toRise=False, 
                  last common set time of (source+phase) - lst(current)
                  or lst - lst(current) , which ever is shorter

        Inputs : source   - source name, which must be in current catalog
                 phase    - phase calibrator, which must be in current catalog
                 lsttime  - LST time
                 default  - return value if no variables are set
                 useTime  - if True, use tmax/tstart script variables
                 toRise   - if True, compute rising times, else setting times
        Output : remaining time in hours

        Example: dt = timeRemaining(source='3c273',useTime=False)
    """
    # Initialize
    tremaining = None
    currentLst = lst()

    # time
    if useTime:
        # tmax
        if getTmax() <> None:
            dt = getTmax() - timeDifference(currentLst, getLstStartTrack(), pos=True)
            if tremaining <> None: 
                tremaining = min(tremaining, dt) 
            else: 
                tremaining = dt

        # endtrack
        if getLstEndTrack() <> None:
            dt = timeDifference(getLstEndTrack(), currentLst)
            if tremaining <> None: 
                tremaining = min(tremaining, dt) 
            else: 
                tremaining = dt

    # source and phase calibrator
    if source <> None or phase <> None:
        # Phase calibrator. Could be matrix of sources/times. 
        # pick only first source
        dtphase = None
        isUpPhase = False
        if phase <> None:
            phaseName = makeList(makeList(phase)[0])[0]
            isUpPhase = isSourceUp(phaseName, el=el)
            if toRise:
                dtphase = getTimeToRise(phaseName, el=el)
            else:
                dtphase = getTimeToSet(phaseName, el=el)

        # Sources (could be a list)
        dtsource = None
        isUpSource = False
        if source <> None:
            names = makeList(source)
            for n in names:
                # Time difference for this source
                dt = None
                isUpSource = isUpSource or isSourceUp(n, el=el)
                if toRise:
                    dt = getTimeToRise(n, el=el)
                else:
                    dt = getTimeToSet(n, el=el)

                # Time difference for source list combined
                if dtsource == None:
                    dtsource = dt
                elif toRise:
                    dtsource = min(dt, dtsource)
                else:
                    dtsource = max(dt, dtsource)

        # Set combined source+phasecal time
        tcombined = None
        if dtsource <> None or dtphase <> None:
            if dtsource == None:
                tcombined = dtphase
            elif dtphase == None:
                tcombined = dtsource
            elif toRise:
                tcombined = max(dtphase,dtsource)
            else:
                tcombined = min(dtphase,dtsource)

        # If time to set, and either the phase calibrator or sources are set,
        # then time remaining is zero
        if source <> None and phase <> None:
            if toRise and isUpPhase and isUpSource:
                tcombined = 0.0
            elif not toRise and (not isUpPhase or not isUpSource): 
                tcombined = 0.0

        # Finally, combined with tremaining
        if tremaining <> None: 
            tremaining = min(tremaining, tcombined) 
        else: 
            tremaining = tcombined
       
    # LST time
    if lsttime <> None:
        dt = timeDifference(convertHms(lsttime), currentLst)
        if tremaining <> None: 
            if toRise:
                tremaining = max(tremaining, dt) 
            else:
                tremaining = min(tremaining, dt) 
        else: 
            tremaining = dt

    # Done
    if tremaining == None: tremaining = default
    return tremaining


def checkSources(sources, key=True, indent=None, phase=False):
    """ Determines if existing sources can be read by existing catalogs.

        Inputs: sources - a scalar, list, or dictionary of source names
                key     - If True and sources is a dictionary, the source 
                          names are assumed to be the keyword of the
                          dictionary (map[key] = value). If False, the source
                          names are assumed to be the value.
                phase   - If True, then "sources" are phase calibrator lists.
                          This is necessary since phase calibrators could be
                          a list containing LST start/stop times.
    """

    # If there are no sources, then there is nothing to do
    if sources == None: return

    # Convert to list or dictionary
    z = 0
    if dict in [type(sources)]: 
        z = sources.copy()
        if key: z = makeList(z)   # Removes sources with value=False
    else:
        z = makeList(sources)

    # Loop over sources
    for t in z:
        # Get source name. name could be a list
        names = t
        if dict in [type(z)] and not key: names = z[names]
        names = makeList(names)
        if phase: 
            getPhaseCal(names)  # This checks LST ranges
            names = [names[0]]

        # Check source is up. I do not care if the source is up, but this
        # will exercise the catalogs
        if names <> None:
           for n in names: 
               trackMessage('checking ' + n, indent=indent)
               isSourceUp(n)


def setSourceTint(sources, tint, isMosaic=False):
    """ Converts source and integration-times into lists of the same size.

        Inputs: sources - A string variable or a string list
                tint    - Integration time in minutes for each source. If only
                          one integration time is given, it is assumed all
                          sources will have this integration time.
                isMosaic  - True if source is for a mosaic observation
        Output: Return value is sourceList,tintList where both are lists.
                On return, sourceList and tintList have the same number of
                elements.

        Examples:
             (1) sources = ['S255', 'S254', 'S258']
                 tint = 5.0
                 souList,tintList = setSourceTint(sources,tint)
                 (souList and tintList both have 3 elements)

             (2) sources = ['S255', 'S254', 'S258']
                 tint = [5.0, 3.0, 10.0]
                 souList,tintList = setSourceTint(sources,tint)
                 (souList and tintList both have 3 elements)
    """
    # Convert to lists
    sou = makeList(sources)
    t   = makeList(tint)

    # Set tint to be a vector with the same length as sources.
    # For mosaics, only the first element matters
    if isMosaic:
        if len(sou) > 1:
            trackError('Only one source allowed for mosaics')
            raise Exception, 'Error entering mosaic parameters'
        sou = [sou[0]]
        t   = [t[0]]
    elif len(sou) != len(t):
        if len(t) != 1:
            msg = 'Different number of sources and integration times.\n' + \
                  'Number of sources = ' + str(len(sou)) + '\n' + \
                  'Number of integration times = ' + str(len(t)) + '\n'
            trackError(msg)
            raise Exception, 'Exiting setSourceTint'
        t = t * len(sou) # Since "t" is a list, * expands the number of elements

    # Done
    return sou[:],t[:]


def readOptions(options, keyval):
    """ Read command line arguments in the global variable 'OPTIONS'
        and store options in the keyval dictionary.

        Inputs: options - string of options and values
                keyval  - variable of type "dictionary"
        Output: Command line options are stored in keyval

        Example:
             keyval = dict()
             options = "point=False tmax=4.0 flux=True"
             readOptions(options, keyval)
    """
    if options == None: return True
    fields = options.split()

    # Initialize
    err = False

    # Loop over command line arguments
    for arg in fields:
        # Parse string by the =
        i=string.find(arg,"=")
        if i < 0:
            trackError('Argument ' + arg + ' not understood')
            err = True

        # Parse string into keyword/value pair
        key = arg[0:i]
        val = arg[i+1:]

        # Make sure keyword exists
        if not keyval.has_key(key):
            trackError('Invalid keyword: '+key)
            err = True
        else:
            # Save keyword/value in list. Preserve current variable type
            vtype = [type(keyval[key])]
            if int in vtype:
                try:
                    keyval[key] = string.atoi(val)
                except ValueError:
                    trackError("Cannot convert '"+val+"' to an integer")
                    err = True
            elif str in vtype or keyval[key] == None:
                if val == 'None': keyval[key] = None
                else :            keyval[key] = val
            elif float in vtype:
                try:
                    keyval[key] = string.atof(val)
                except ValueError:
                    trackError("Cannot convert '"+val+"' to a floating point")
                    err = True
            elif bool in vtype:
                if string.upper(val) == 'TRUE':
                    keyval[key] = True
                elif string.upper(val) == 'FALSE':
                    keyval[key] = False
                else:
                    trackError("Cannot convert '"+val+"' to boolean")
                    err = True
            else:
                trackError("Cannot set variable for argument '" + arg + "'")
                err = True

    # Check for errors
    if err: raise Exception, 'Error entering command line arguments'


def initializeTrack(var, sources, phaseCal, time, mosaic, 
                    projectCode, sourceCatalog, trackingThreshold, 
                    preferred, elcal, restart=False, scriptName=None, 
                    scriptOptions=None, obsblock=None, subObsblock=None,
                    maxPointingSep=None, fluxParams=None, version=None):
    """ Initialize various settings when starting a track.
       
        The initialization is as follows:
           1) If restart=False, then
                  a) clear script variables
                  b) call radioInit()
                  c) set tracking threshold
           2) Set new project
           3) Load user catalog
           4) Set intent for sources and phase calibrator
           5) Initialize constraints
           6) Read system variables if restarting track
           7) Read command line options

        Inputs:
           var         --- Stores command line options
           sources     --- list of science sources to be observed
           phaseCal    --- list of phase calibrators
           time        --- time parameters
           mosaic      --- mosaic parameters
           projectCode --- assigned project code for the track
           sourceCatalog --- name of user source catalog
           trackingThreshold --- tracking threshold in beamwidths
           preferred   --- list of preferred calibrators
           elcal       --- elevation limit in degrees for calibrators
           restart     --- if True, restart system from previous track
           scriptName  --- Name of the observing script
           scriptOptions --- command line options passed to the script
           obsblock      --- obsblock name
           subObsblock   --- subobsblock name
           maxPointingSep --- maximum pointing separation from source in degrees
           fluxParams     --- flux cal options
    """
    # Clear system variables if not restarting track
    if not restart: 
        controlVariablesClear()
        radioInit()
        trackThreshold(trackingThreshold)

    # Messages
    trackMessage("Initializing track")
    if scriptName <> None: 
        trackMessage('Script name    : ' + scriptName, indent=1)
    if scriptOptions == None or scriptOptions == "":
        trackMessage('Script options : None', indent=1)
    else:
        trackMessage('Script options : ' + scriptOptions, indent=1)
    addScriptString(INDX_STR_SCRIPTNAME, scriptName, append=False)
    trackMessage('Elevation limit = ' + str('%.1f' % getElevationLimit()), indent=1)
    checkAntennas(indent=1)

    # Check project code is a string and a scalar
    if projectCode == None or str not in [type(projectCode)]:
        raise Exception, 'Error specifying project code'

    # Add options to give observers flexibility on executing the script
    p = preferred
    var['alarm']    = True          # Turn alarm on when script is finished
    var['elcal']    = elcal         # Elevation limit for passband/flux calibrators [degrees]
    var['fluxname'] = p['fluxcal']  # Name of preferred flux calibrator
    var['fluxstart']= True          # Observe flux calibrator at start of track
    var['pbname']   = p['passband'] # Name of preferred passband source
    var['pbstart']  = True          # Measure passband at start of track
    var['pointing'] = time['pointing'] # Time between radio pointing [hours]
    var['pntname']  = p['pointing'] # Name of preferred pointing source
    var['pntsep']   = maxPointingSep # Maximum pointing sepeation in degrees
    var['pntstart'] = True          # Point at start of track
    var['reset']    = True          # Reset project when script is finished
    var['tune']     = True          # Tune the receivers
    if mosaic <> None:              # Starting position in mosaic
        var['startpos'] = mosaic['startpos'] 
    var['fluxsec'] = None           # Preferred secondary flux calibrator
    if p.has_key('fluxsec'): var['fluxsec'] = p['fluxsec']

    # Variables monitored in the system variables, but stored in "var"
    # so that they can be read in as command line arguments.
    # These will be removed from "var" in readCommandLineOptions.
    var['lststart'] = getLstStartCycle(string=True) 
    var['lststop']  = getLstStopCycle(string=True)  
    var['tmax']     = getTmax()          
    var['endtrack'] = getLstEndTrack(string=True)

    # Set "hidden" variables for tracking timeouts and record lengths
    if not time.has_key('recNoise'):  time['recNoise']  = TRECORD_NOISE
    if not time.has_key('recMosaic'): time['recMosaic'] = time['record']
    if not time.has_key('tmoTrack'):  time['tmoTrack']  = None
    if not time.has_key('tmoMosaic'): time['tmoMosaic'] = None

    # Read restart variables
    if restart: 
        readRestartVariables(var, mosaic)
        var['tune'] = False

    # Read command line options
    readCommandLineOptions(scriptOptions, var, mosaic=mosaic)

    # Set number of flux calibrators to observe per track
    nfluxTrack = 1
    if fluxParams <> None: 
        nfluxTrack = fluxParams['nfluxTrack']
        if nfluxTrack == None or nfluxTrack <= 0: nfluxTrack = 0
    s.setScriptInt(INDX_INT_NFLUXTRACK, nfluxTrack)

    # Set obsblock
    tmpObsblock = obsblock

    # Set subObsblock. Remove any spaces and '.'
    tmpSubobsblock = subObsblock
    if tmpSubobsblock == None: tmpSubobsblock = ''
    tmpSubobsblock = tmpSubobsblock.split('.')[0]
    tmpSubobsblock = ''.join(tmpSubobsblock.split()) # Removes spaces

    # Validate obsblock and conditionally initialize project
    newProject(projectCode, tmpObsblock, tmpSubobsblock )
    # Trial number is now assigned and set in newProject
    trial = s.getScriptInt(INDX_INT_OBSTRIAL)
    comment("Project validation and trial number assignment succeeded. Trial = "+str(trial) )

    # Load user catalog
    if sourceCatalog <> None: ucat(sourceCatalog)

    # Set intent for source
    #sou = makeList(sources)
    #for name in sou: intent(name,'S',True)
    #intent('noise','B',True)

    # Set intent for phase calibrators
    #if phaseCal <> None:
    #    for l in makeList(phaseCal): 
    #        if list in [type(l)]:
    #           name = l[0]
    #        else:
    #           name = l
    #        intent(name,'BF',True)

    # Constraints
    constraints()

    # Reset integration time to the same size as sources
    sources, time['source'] = setSourceTint(sources, time['source'])

    # Set time stamps if needed
    if not s.getScriptBool(INDX_BOOL_DATE):
        addScriptString(INDX_STR_DATE, getLocalTime(), bindx=INDX_BOOL_DATE)
    if not s.getScriptBool(INDX_BOOL_LST_START_TRACK):
        s.setScriptBool(INDX_BOOL_LST_START_TRACK, True)
        s.setScriptDouble(INDX_DBL_LST_START_TRACK, lst())

    # If tuning, then move antennas to a high elevation
    if var['tune']:
        # Get list of current antennas
        ants = currentAntennaNumbers()

        # Get minimum elevation
        el = list()
        minel = 90.0
        for a in ants:
            # Set monitor point
            m = 'ovro' + str(a)
            if a > 6: m = 'bima' + str(a-6)
            m = m + '.AntennaCommon.Drive.Track.actualElevation'

            # Get elevation
            el.append(queryDouble(m))
        if len(el) > 0: minel = min(el)

        # Move antennas?
        if minel < 35:
            trackMessage('Moving antennas to 35 deg elevation for tuning')
            move(el=35)

    # Run postconfig corr
    trackMessage('running postConfigCorr()', indent=1)
    short.postConfigCorr()


def readCommandLineOptions(options, var, mosaic=None, verbose=True):
    """ Defines command line options that can be used by the observer, and then
        reads/parses the options input variable to override any
        of the default options.

        Inputs: options - string containing options and values
                var - variable of type dictionary
        Output: var is updated with the new variables and options.

        The following command line options and default values are listed below.
        Many of the default values are defined in the preamble in the
        template script.

        Keyword    Default Value   Type  Result
        --------   --------------- ----- -------------------------------------
        alarm      True            bool  Turn alarm on when script is finished
        elcal      30.0            flt   Elevation limit for flux calibrators
        endtrack   None            str   End track before this lst time in hours
        flux       True            bool  Measure flux calibrator
        fluxname   None            str   Preferred flux calibration source
        fluxstart  True            bool  Observe flux calibrator at beginning of track
        lststart   None            str   Starting LST for source/phasecal cycle
        lststop    None            str   Stopping LST for source/phasecal cycle
        pb         True            bool  Observe passband
        pbname     None            str   Preferred passband source
        pbstart    True            bool  Observe passband at beginning of track
        point      True            bool  Point during track
        pntname    None            str   Preferred pointing source
        pntstart   True            bool  Point at beginning of track
        reset      True            bool  Reset project when script is finished
        startpos   1               int   Starting position in mosaic file
        tmax       None            flt   Maximum time in hours for track to run
        tune       True            bool  Tune the receivers
    """

    # Message
    if verbose: trackMessage('Setting script options')

    # A handful of critical options critical for restarting tracks are 
    # stored in CARMA system variables. Add these variables to var temporarily,
    # but remove them below.
    var['endtrack'] = getLstEndTrack(string=True)
    var['lststart'] = getLstStartCycle(string=True) 
    var['lststop']  = getLstStopCycle(string=True)  
    var['tmax']     = getTmax()          
    if mosaic <> None:
        var['startpos'] = mosaic['startpos']

    # Read command line options
    readOptions(options, var)
    if s.getScriptString(INDX_STR_OPTIONS) != options:
        addScriptString(INDX_STR_OPTIONS, options, delim=' ')

    # Store 'var' back in control variables for some options
    setLstEndTrack(var['endtrack'])
    setLstStartCycle(var['lststart'], verbose=False)
    setLstStopCycle(var['lststop'], verbose=False)
    setTmax(var['tmax'], verbose=False)
    del var['endtrack']
    del var['lststart']
    del var['lststop']
    del var['tmax']

    # mosaic.startPos
    if mosaic <> None:
        mosaic['startpos'] = var['startpos']
        del var['startpos']

    # Summarize main options
    if getLstStartCycle() <> None: trackMessage('LST start cycle = ' + 
        convertHmsString(getLstStartCycle()), indent=1)
    if getLstStopCycle() <> None: trackMessage('LST stop  cycle = ' + 
        convertHmsString(getLstStopCycle()), indent=1)
    if getTmax() <> None: 
        trackMessage('Maximum track length = ' + dtString(getTmax()), indent=1)


def timeDifference(t2, t1, pos=False):
    """ Computes the time difference in hours between time stamps t2 and t1.
        Time difference = t2 - t1, but factors in time stamps that cross
        time=24.0 hours.

        Inputs: t2  - Ending   time start in decimal hours
                t1  - Starting time start in decimal hours
                pos - If True, then time difference is known to be positive
        Output: Difference (t2 - t1) in hours

        Examples:
             (1) print timeDifference(5.0,3.0)  # =  2.0 hours
             (2) print timeDifference(1.0,23.0) # =  2.0 hours
             (3) print timeDifference(2.0,22.0) # = -4.0 hours

        Bugs: Cannot compute time differences more than 12 hours.

    """
    # Compute the difference between t2 and t1 (t2 - t1), taking into
    # account the ambiguity of 24 hours.
    dt = convertHms(t2) - convertHms(t1)
    if pos :
        if dt < 0.0: dt += 24.0
    elif dt < 0.0 and abs(dt + 24.0) < abs(dt):
        dt += 24.0
    elif dt > 0.0 and abs(dt - 24.0) < abs(dt):
        dt -= 24.0

    # Done
    return dt


def getNreps(tintMinutes, trecordSeconds, tsysMinutes=None, tintSinceLastTsys=None):
    """ Determine number of cycles and repeats needed to observe a source for
        tintMinutes, with a record time of trecordSeconds, and measuring 
        the system temperature everyone tsysMinutes.

        Inputs: tintMinutes    - total desired integration time in minutes
                trecordSeconds - record time in seconds
                tsysMinutes    - how frequently to measure tsys
                tintSinceLastTsys - integration time in minutes since tsys was last measured

        Output: The number of repeats per cycle. 
        
                If tsysMinutes=None, then getNreps is an integer, else it is a integer 
                list with one value per cycle.

                Number of repeats are rounded to the nearest integer.

                It tintMinutes > 0, then there will be a minimum of one repeat.

                It tsysMinutes < 0 and tintSinceLastTsys > abs(tsysMinutes),
                the first nrep is set to zero.

        Examples:
              (1) print getNreps(10.0,30.0)      # 20 repeats, 1 cycle
              (3) print getNreps(10.0,30.0,5.0)  # 10 repeats, 2 cycles
              (2) print getNreps(0.75,30.0)      #  2 repeats, 1 cycle
              (3) print getNreps(0.70,30.0)      #  1 repeats, 1 cycle
    """
    # Tminutes must be positive
    if tintMinutes == None or tintMinutes <= 0.0: return 0

    # set number of cycles and integration time per cycle
    # If tsys time is not specified, integration is done in one cycle
    ncycles = 1
    tintCycle = [float(tintMinutes)]
    if tsysMinutes <> None and tsysMinutes != 0.0:
        # Tsys time cannot be smaller than the record time
        t = max(abs(tsysMinutes), trecordSeconds/60.0)

        # tpart contains the integration time for two parts:
        #     Part 1: Integration time before a tsys is needed. 
        #             This is != None only if tsysMinutes < 0 and tlastTsys != None
        #     Part 2: Integration time after the non-tsys data is obtained.
        tpart = [None, tintCycle[0]]
        if tsysMinutes < 0.0 and tintSinceLastTsys <> None:
            timeBeforeNextTsys = t - tintSinceLastTsys
            tpart[0] = max(0.0, timeBeforeNextTsys)
            tpart[1] = max(0.0, tintCycle[0] - tpart[0])

        # Number of cycles for each part
        ncycles = [None, None]
        for i in range(len(tpart)):
            if tpart[i] <> None:
                rcycles = float(tpart[i]) / t
                ncycles[i] = int(rcycles)
                if rcycles != ncycles[i]: ncycles[i] += 1

        # Set integration time per cycle
        tintCycle = [t] * ncycles[1]
        if ncycles[0] <> None: tintCycle.insert(0,tpart[0])
        tintCycle[-1] = tintMinutes - sum(tintCycle[:-1])

    # Set number of reps per cycle
    nreps = list()
    for t in tintCycle:
        n = max(1, round(t * 60.0 / trecordSeconds))
        if t <= 0: n = 0
        nreps.append(int(n))

    # Because nreps is set to a minimum value of 1, we may have exceeded the
    # total integration time. Check that here and make any adjustments
    textra = tintMinutes - sum(nreps) * trecordSeconds / 60.0
    if textra > trecordSeconds:
        nextra = int(textra / trecordSeconds)
        nreps[-1] -= nextra

    # If tsys was set to none, then return a scalar
    if tsysMinutes == None: nreps = nreps[0]

    # Return number of repetitions
    return nreps

def inList(slist, name, caseSensitive=False):
    """ Return True/False if name is list slist 

        Inputs: slist - list of source names
                name  - scalar string 
                caseSensitive  - If true, search is case sensitive
    """
    # No match if slist is not set
    if slist == None or len(slist) == 0: return False

    # Makecopy of list
    t = makeList(slist)
    n = name

    # Convert to upper case
    if not caseSensitive:
        n = string.upper(n)
        for i in range(len(t)):
            t[i] = string.upper(t[i])

    # Match
    found = False
    try :
        i = t.index(n)
        found = True
    except:
        pass

    # Done
    return found


def getDistance(source,ref):
    """Return angular distance between a ref and source"""
    srcAzEl = azel(source,0.0)
    refAzEl = azel(ref,0.0)
    deg2rad = 180.0/math.pi;
    srcAz   = srcAzEl[0]/deg2rad
    srcEl   = srcAzEl[1]/deg2rad
    refAz   = refAzEl[0]/deg2rad
    refEl   = refAzEl[1]/deg2rad
    cosDist =  math.sin(refEl)*math.sin(srcEl) + math.cos(refEl)*math.cos(srcEl)*math.cos(refAz-srcAz)
    dist    = math.acos(cosDist)*deg2rad
    return  float(dist)


def getSource(sources, preferred=None, el=None, elmax=80.0, ref=None,
              crossTerminus=True, maxsep=None, isPreferred=False,
              exclude=None):
    """ Given a list of "sources, getSource() will return either :
             (1) if "ref" is set, the nearest source on the sky to the
                 source "ref" that is between the minimum elevations and elmax".
             OR
             (2) if "ref" is not set, the highest elevation source between the
                 minimum elevation and "elmax".
        The sources will be first find a suitable source in "preferred", and
        if no suitable sources are found, "sources" will be searched.

        Inputs: sources   - a string variable, list, or dictionary
                preferred - a string variable, list, or dictionary containing
                            the preferred list of sources
                el        - Minimum elevation that overrides system elevation
                elmax     - Maximum elevation
                ref       - reference source
                crossTerminus - If False, pointing source cannot cross north/south
                            terminun if half hour before transit
                maxsep    - Maximum separation in degrees for source
                isPreferred - If True, then the objects listed in "sources"
                              are preferred calibrators. This option is added
                              to allow recursive calls.
                exclude   - List of any sources to exclude
        Output: string variable containing the source name meeting the above
                criteria.
    """
    # Check the preferred source list first to see if one is available
    if preferred <> None:
        # Make list of preferred sources
        sou = makeList(preferred)

        # Check preferred sources
        for n in sou:
            name = getSource(n, el=el, elmax=elmax, ref=ref, 
                             crossTerminus=crossTerminus, maxsep=maxsep,
                             isPreferred=True)
            if name <> None: return name

    # Convert sourceDict into list or dictionary if needed
    # I do not automatically convert it to a dictionary since I assume
    # any lists are in priority order.
    d = sources
    if list not in [type(sources)] and dict not in [type(sources)]:
        d = makeList(sources)

    # Get the reference source
    refSource = makeList(ref)
    refEl = None
    hemisRefSouth = True
    obslat = 37.280358
    afterTransitRef = False
    if refSource <> None: 
        refSource = refSource[0]
        refEl = getSourceElevation(refSource)
        if convertHms(getSourceDec(refSource)) >= obslat: hemisRefSouth = False
        if timeDifference(lst(), getSourceRa(refSource)) > -0.5:
            afterTransitRef = True

    # Initialize
    if el == None: el = getElevationLimit()
    sepChosenSource  = 1e35
    elChosenSource   = el
    nameChosenSource = None

    # Check source list for either:
    #     1) source nearest in time if ref!=None
    # OR
    #     2) highest elevation source if there is no reference source
    isDictionary = dict in [type(d)]
    for name in d:
        # If "d" is a dictionary, see if we should use this source
        use = True
        if isDictionary: use = d[name]
        if use:
            # Get current source elevation
            elSource = getSourceElevation(name)

            # Check elevation limits
            if elSource >= el and (elmax == None or elSource <= elmax):
                if refSource <> None:
                    # Compute separation
                    sep = getDistance(name,refSource)
                    if maxsep <> None and sep > maxsep: continue
                    if sep > sepChosenSource: continue

                    # Make sure sources is not in excluded list
                    if inList(exclude,name): continue

                    # Sources must be same hemisphere if past transit.
                    # If this is not a preferred calibrator, this is 
                    # strictly enforced. If it is a preferred calibrator,
                    # it is enforced before transit.
                    if not crossTerminus:
                        # Set hemisphere
                        hemisSourceSouth = True
                        if convertHms(getSourceDec(name)) >= obslat: 
                            hemisSourceSouth = False

                        # Set before/after transit
                        afterTransitSource = False
                        if timeDifference(lst(), getSourceRa(refSource)) > -0.5:
                            afterTransitSource = True

                        # OK source?
                        if hemisSourceSouth != hemisRefSouth:
                            if not isPreferred or  \
                               (isPreferred and afterTransitSource):
                               continue

                    # Source must be ok
                    sepChosenSource = sep
                    nameChosenSource = name
                else:
                    # Highest elevation source
                    if elSource > elChosenSource:
                        elChosenSource = elSource
                        nameChosenSource = name
    
    # Done
    return nameChosenSource


def doPoint(preferred=None, time=None, elmax=80, ref=None, reconfig=None,
            minFlux=4.0, el=30.0, crossTerminus=False, maxsep=50.0,
            printHeader=True):
    """ Perform radio pointing on a source with an elevation greater than the
        system limit and below elmax. The pointing source is first
        searched in "preferred", and if no suitable sources are found, then in
        sourceList. The observed pointing source is chosen based upon:
           1) If ref is set, the source closest to "ref"
           2) If ref is not set, the sources highest in the sky.

        sourceList is currently set to the bright quasars
         3c84, 0530+135, 2232+117, 3c454.3, 3c279, 3c273, 3c345, 1733-130

        Inputs: time        - time dictionary containing integration times
                preferred   - List of sources to search for first
                elmax       - Maximum elevation to observe source
                ref         - Name of reference source
                reconfig    - If set, reconfigure correlator
                minFlux     - Minimum flux in Janskys to select a pointing 
                              source if "preferred" is not set.
                printHeader - Print message with indent=0
        Output: None
    """
    # Message
    tpoint = getLstLastPoint()
    if printHeader: trackMessage("Checking radio pointing")

    # Check if pointing needs to be done time wise
    if time <> None and tpoint <> None: 
        dtLastPoint = timeDifference(lst(), tpoint, pos=True)
        dtNextPoint = time['pointing'] - dtLastPoint
        if dtNextPoint > 0.0: 
            trackMessage("radio pointing in " + dtString(dtNextPoint), indent=1)
            return False
        else:
            trackMessage("radio pointing last done " + 
                         dtString(abs(dtLastPoint)) + " ago", indent=1)

    # Get closest pointing source on the sky
    sourceList = getBrightSources(minFlux)
    source = getSource(sourceList, preferred=preferred, ref=ref, el=el, 
                       elmax=elmax, crossTerminus=crossTerminus, maxsep=maxsep)
    if source == None:
        trackMessage("no pointing source available", indent=1)
        return False
    else:
        trackMessage("pointing on " + source, indent=1)

    # Change correlator if needed
    #config = short.getConfigCorrT()
    #if reconfig <> None: 
    #    trackMessage("setting correlator for calibrator observations", indent=1)
    #    short.changeConfigCorr(reconfig)

    # Point
    etrack.etrack(source)
    refPoint.refPoint(source)
    setLstLastPoint(lst())
    addScriptString(INDX_STR_POINTNAME, source,
                  bindx=INDX_BOOL_POINTNAME, delim=', ', allowDup=False)
    trackMessage("pointing completed", indent=1)

    # Change correlator back if needed
    #if reconfig <> None: 
    #    trackMessage("setting correlator for science observations", indent=1)
    #    short.changeConfigCorr(config)

    # Done
    return True


def doPassband(time, lststop=None, preferred=None, el=30.0, elmax=80.0, 
               reconfig=None, hybrid=None, ref=None, 
               maximumTime=None, minFlux=4.0):
    """ Observe a passband calibrator with an elevation greater than the
        system limit and below elmax. The passband calibrator is first
        searched in "preferred", and if no suitable sources are found, then in
        sourceList. The observed passband source is chosen based upon:
           1) If ref is set, the source closest to "ref"
           2) If ref is not set, the sources highest in the sky.

        Inputs: time        - time dictionary containing integration times
                lststop     - repeat observations until lststop is reached.
                              This is a string variable of the form HH:MM:SS.
                              If lststop is not set, then observe source once.
                preferred   - List of sources to search for first
                el          - Minimum elevation limit to observe calibrators
                              If el=None, the current system elevation is used
                elmax       - Maximum elevation to observe source
                ref         - Name of reference source
                reconfig    - Correlator configuration list for calibrator
                              observations
                hybrid      - list of correlator configurations that will be
                              observed to calibrate band-to-band offsets
                minFlux     - minimum flux for the passband calibrator if the
                              preferred list is not set
         Outputs: True or False, indicating whether or not a passband
                  calibrator was observed.
    """
    # Initialize
    ok = False
    trackMessage("Passband calibration")

    # Get passband calibrator
    sourceList = getBrightSources(minFlux)
    passbandCal = getSource(sourceList, preferred=preferred, ref=ref,
                            el=el, elmax=elmax)
    if passbandCal == None: 
        trackMessage("No passband calibrator available", indent=1)
        return ok
    else:
        printWeather()
        trackMessage("slewing to " + passbandCal, indent=1)

    # Set intent
    intent(passbandCal,'BF',True)

    # Observe passband calibrator in current correlator configuration
    intentObject = 'B'
    selfcal = True
    ok = observeSource(passbandCal, time['pb'], time['record'],
                       tsysTime=time['tsys'], lststop=lststop, 
                       maximumTime=maximumTime, tnoise=time['noise'],
                       trecnoise=time['recNoise'], tmo=time['tmoTrack'],
                       el=el, intentObject=intentObject, selfcal=selfcal)

    # If performing hybrid observations, we do no want to reset the
    # correlator between observations. This improves the observing efficiency.
    # This behavior is controlled through origConfig.
    origConfig = None
    if hybrid <> None:
        origConfig = short.getConfigCorrT()

    # Observe passband in reconfigured correlator
    if ok and reconfig <> None:
        ok = ok and observeSource(passbandCal, time['pb'], time['record'],
                               tsysTime=time['tsys'], lststop=lststop, 
                               maximumTime=maximumTime, reconfig=reconfig,
                               tnoise=time['noise'], trecnoise=time['recNoise'],
                               tmo=time['tmoTrack'], origConfig=origConfig,
                               el=el, 
                               intentObject=intentObject, selfcal=selfcal)

    # Observe hybrid passbnad modes
    if ok and hybrid <> None:
        for h in hybrid:
            ok = ok and observeSource(passbandCal, time['hybrid'], 
                          time['record'], tsysTime=time['tsys'], 
                          lststop=lststop, maximumTime=maximumTime, 
                          reconfig=reconfig, hybrid=h,
                          tnoise=time['noise'], trecnoise=time['recNoise'],
                          tmo=time['tmoTrack'], origConfig=origConfig,
                          el=el,
                          intentObject=intentObject, selfcal=selfcal)

    # Reset correlator
    if origConfig <> None:
        trackMessage("setting correlator for science observations", indent=1)
        short.changeConfigCorr(origConfig)

    # Done. "ok" indicates if passband calibrator was observed.
    if ok:
        # trackMessage("passband observations completed successfully", indent=1)
        addScriptString(INDX_STR_PASSBAND, passbandCal,
                      bindx=INDX_BOOL_PASSBAND, delim=', ', allowDup=False)
    else:
        if endOfTrack():
            trackMessage('Ending observations based on endtrack()')
        elif isLastcal(): 
            trackMessage('Ending cycle based on lastcal()')
        elif isStopAfterNextCal(): 
            trackMessage('Ending cycle based on stopAfterNextCal()')
        else:
            trackMessage("WARNING: Passband observations may have failed", indent=1)
    return ok


def doFluxcal(fluxSources, time, preferred=None, el=30.0, elmax=80.0, 
              ref=None, reconfig=None, lststop=None, fluxParams=None,
              printHeader=True, preferredSecondary=None):
    """ Observe a flux calibrator with an elevation greater than the
        system limit and below elmax. The flux calibrator is first
        searched in "preferred", and if no suitable sources are found, then in
        fluxSources. The observed flux calibrator is chosen based upon:
           1) If ref is set, the source closest to "ref"
           2) If ref is not set, the sources highest in the sky.

        Inputs: fluxSources - string variable, list, or dictionary of possible
                              flux calibrators
                time        - time dictionary containing integration times
                preferred   - List of sources to search for first
                preferredSecondary - list of preferred secondary calibrators
                el          - Minimum elevation limit to observe calibrators
                              If el=None, the current system elevation is used
                elmax       - Maximum elevation to observe source
                ref         - Name of reference source
                reconfig    - If set, reconfigure correlator for continuum
                fluxParams  - Dictionary that specified various flux cal 
                              observations
                printHeader - Print message with indent=0

         Output: True or False, indicating whether or not a flux
                  calibrator was observed.
    """
    # Set number of primary flux calibrators to observe this cycle
    nfluxCycle = 1
    if fluxParams <> None: nfluxCycle = fluxParams['nfluxCycle']

    # Adjust for the total number of primary calibrators per track
    nfluxRemaining = s.getScriptInt(INDX_INT_NFLUXTRACK) - \
                     s.getScriptInt(INDX_INT_NOBS_FLUXPRI)
    if nfluxCycle == None or nfluxCycle == 0:
        nfluxCycle = nfluxRemaining
    else:
        nfluxCycle = min(nfluxCycle, nfluxRemaining)

    # Determine if we need to do primary calibration
    doPrimary = not finishedFluxcal() and nfluxCycle > 0

    # Determine if we need to observe secondary calibration
    doSecondary = (s.getScriptInt(INDX_INT_NOBS_FLUXSEC) == 0)
    doBothPrimarySecondary = False
    if fluxParams <> None:
        if fluxParams.has_key('doSecondary'): 
            if fluxParams['doSecondary']:
                if fluxParams['doBoth']: 
                    doSecondary = True;
                    doBothPrimarySecondary = True
            else:
                doSecondary = False;

    # Do we need to observe a flux calibrator?
    if not doPrimary and not doSecondary: return False

    # Message
    tfluxcal = getLstLastFluxcal()
    if printHeader: trackMessage("Flux calibration")

    # Check if primary flux cal needs to be done time wise
    if time <> None and tfluxcal <> None: 
        if time.has_key('fluxcal'):
            dtLastFluxcal = timeDifference(lst(), tfluxcal, pos=True)
            dtNextFluxcal = time['fluxcal'] - dtLastFluxcal
            if dtNextFluxcal > 0.0: 
                trackMessage("flux calibration in " + dtString(dtNextFluxcal), indent=1)
                return False
            else:
                trackMessage("flux calibration last done " + 
                             dtString(abs(dtLastFluxcal)) + " ago", indent=1)

    # Are duplicates allowed?
    allowDup = False
    if fluxParams <> None: allowDup = fluxParams['dupFluxcal']

    # Loop over number of primary flux calibrators to observe
    ok = False
    triedSource = False
    observedOnce = list()
    if not doPrimary: nfluxCycle = 0
    for n in range(nfluxCycle):
        # If dups are not allowed, get list of calibrators 
        # that cannot be observed.
        excludeSources = None
        if not allowDup:
            excludeSources = s.getScriptString(INDX_STR_FLUXPRIMARY)
            if excludeSources == "":
                excludeSources = None
            else:
                excludeSources = excludeSources.split()
        if observedOnce <> None:
            if excludeSources == None: excludeSources = list()
            excludeSources.extend(observedOnce)

        # Get available flux calibrator
        sourceName = getSource(fluxSources, preferred=preferred, el=el,
                               ref=ref, elmax=elmax, exclude=excludeSources)
        if sourceName <> None: 
            # Messages
            if not triedSource:
                if printHeader: printWeather()
                trackMessage('Primary flux calibration', indent=1)
                triedSource = True
            trackMessage("slewing to " + sourceName, indent=2)

            # Observe source
            okSource = observeSource(sourceName, time['flux'], time['record'],
                               tsysTime=time['tsys'], reconfig=reconfig,
                               lststop=lststop, tnoise=time['noise'], 
                               trecnoise=time['recNoise'], tmo=time['tmoTrack'],
                               el=el, indent=2, intentObject='BF', selfcal=True)
            if okSource:
                setLstLastFluxcal(lst())
                ok = okSource
                if not doBothPrimarySecondary: doSecondary = False
                observedOnce.append(sourceName)
                addScriptString(INDX_STR_FLUXPRIMARY, sourceName, delim=' ', 
                                allowDup=False, bindx=INDX_BOOL_FLUXPRIMARY)
                nn = s.getScriptInt(INDX_INT_NOBS_FLUXPRI) 
                s.setScriptInt(INDX_INT_NOBS_FLUXPRI, nn + 1)
    if doPrimary and not triedSource:
        trackMessage("No primary flux calibrator available", indent=1)

    # Do secondary calibrator
    if doSecondary: 
        # Set second flux calibrator list
        #secondaryCalibrators = ['3c84', '0530+135', '0854+201', '0927+390', 
        #                        '3c273', '3c279', '3c345', '1751+096',
        #                        '2148+069', '3c446', '3c454.3']
        secondaryCalibrators = ['3c84', '3c273']

        # Make list of sources observed already
        excludeSources = s.getScriptString(INDX_STR_FLUXPRIMARY)
        if not allowDup:
            excludeSources += ' ' + s.getScriptString(INDX_STR_FLUXSECONDARY)
        if excludeSources == "" or excludeSources == ' ':
            excludeSources = None
        else:
            excludeSources = excludeSources.split()

        # Get calibrator
        sourceName = getSource(secondaryCalibrators, 
                               preferred=preferredSecondary, el=el,
                               ref=ref, elmax=elmax, exclude=excludeSources)
        if sourceName <> None: 
            # Messages
            trackMessage("Secondary flux calibration", indent=1)
            trackMessage("slewing to " + sourceName, indent=2)

            # Observe source
            triedSource = True
            okSource = observeSource(sourceName, time['flux'], time['record'],
                               tsysTime=time['tsys'], reconfig=reconfig,
                               lststop=lststop, tnoise=time['noise'], 
                               trecnoise=time['recNoise'], tmo=time['tmoTrack'],
                               el=el, indent=2, intentObject='BFG', selfcal=True)
            if okSource:
                ok = okSource
                addScriptString(INDX_STR_FLUXSECONDARY, sourceName, delim=' ', 
                                allowDup=False, bindx=INDX_BOOL_FLUXSECONDARY)
                nn = s.getScriptInt(INDX_INT_NOBS_FLUXSEC) 
                s.setScriptInt(INDX_INT_NOBS_FLUXSEC, nn + 1)
            else:
                trackMessage("No secondary flux calibrator available", indent=2)

    # Done
    if triedSource and not ok:
        if endOfTrack():
            trackMessage('Ending observations based on endtrack()')
        elif isLastcal():
            trackMessage('Ending cycle based on lastcal()')
        elif isStopAfterNextCal():
            trackMessage('Ending cycle based on stopAfterNextCal()')
        else:
            trackMessage("Flux calibrator observations may have failed", indent=1)
    return ok


def trackSummary():
    """ Summarizes the track in terms of total time, which calibrations
        were completed, and time spent in source/phaseCal cycle.
    """

    # Summary
    trackMessage('\nTrack summary', timestamp=False)

    # Total time
    dt = timeDifference(lst(), getLstStartTrack(), pos=True)
    tmp = '    track length = ' + dtString(dt)
    trackMessage(tmp, timestamp=False)

    # Source time
    t = s.getScriptDouble(INDX_DBL_TSOURCE)
    tmp = 'source  time = ' + dtString(t)
    trackMessage(tmp, indent=1, timestamp=False)

    # Primary flux calibrator
    tmp = 'observed primary   flux cal   : '
    if s.getScriptInt(INDX_INT_NOBS_FLUXPRI) > 0:
        trackMessage(tmp + s.getScriptString(INDX_STR_FLUXPRIMARY), indent=1, timestamp=False)
    else:
        trackMessage(tmp + 'None', indent=1, timestamp=False)

    # Secondary flux calibrator
    tmp = 'observed secondary flux cal   : '
    if s.getScriptInt(INDX_INT_NOBS_FLUXSEC) > 0:
        trackMessage(tmp + s.getScriptString(INDX_STR_FLUXSECONDARY), indent=1, timestamp=False)
    else:
        trackMessage(tmp + 'None', indent=1, timestamp=False)

    # Passband calibration
    tmp = 'observed passband calibrators : '
    if finishedPassband():
        trackMessage(tmp + s.getScriptString(INDX_STR_PASSBAND), 
                     indent=1, timestamp=False)
    else:
        trackMessage(tmp + 'None', indent=1, timestamp=False)

    return

def sendEmailMessage(sendTo, sendFrom, msg, subject = None, cc=None, 
              bcc=None, attachments=None, scriptLog=None):
    # Make complete list of recipients
    recipients = list()

    # Convert "To" to list and string
    toStr = sendTo
    if list not in [type(toStr)]:
        toStr  = ','.join(sendTo.replace(',', ' ').split())
    toList = toStr.split(',')
    recipients.extend(toList)

    # Convert "From" to list and string
    fromStr  = ','.join(sendFrom.replace(',', ' ').split())
    # recipients.extend(fromList)

    # Convert "cc" to list and string
    ccStr  = None
    ccList = None
    if cc <> None:
        ccStr = cc
        if list not in [type(ccStr)]:
            ccStr  = ','.join(cc.replace(',', ' ').split())
        ccList = ccStr.split(',')
        recipients.extend(ccList)

    # Convert "bcc" to list and string
    bccStr  = None
    bccList = None
    if bcc <> None:
        bccStr = bcc
        if list not in [type(ccStr)]:
            bccStr  = ','.join(bcc.replace(',', ' ').split())
        bccList = bccStr.split(',')
        recipients.extend(bccList)

    # Set message
    message = None
    if attachments <> None or scriptLog <> None:
        # Initialize message
        message = email.MIMEMultipart.MIMEMultipart()
        message.attach(email.MIMEText.MIMEText(msg))

        # Attachments
        if attachments <> None:
            for file in makeList(attachments):
                 part = email.MIMEBase.MIMEBase('text', "plain")
                 part.set_payload( open(file,"rb").read() )
                 # email.Encoders.encode_base64(part)
                 part.add_header('Content-Disposition', 'attachment; filename="%s"'
                                 % os.path.basename(file))
                 message.attach(part)

        # Attach script log
        if scriptLog <> None:
            s = email.MIMEText.MIMEText(scriptLog)
            s.add_header('Content-Disposition', 'attachment; filename="Scriptlog"')
            message.attach(s)
    else:
        message = email.MIMEText.MIMEText(msg)
    message['To'] = toStr
    message['From'] = fromStr
    if cc  <> None: message['Cc'] = ccStr
    if bcc <> None: message['Bcc'] = bccStr
    if subject <> None: message['Subject'] = subject

    # Send message
    hosts = ['mail.ovro.caltech.edu', 'localhost']
    smtp = None
    connected = False
    for h in hosts:
        try :
            smtp = smtplib.SMTP(h)
            connected = True
        except Exception: 
            pass
        if connected: break
    if connected:
        trackMessage('Sending email to ' + toStr)
        smtp.sendmail(fromStr,recipients,message.as_string())
        smtp.close()
    else:
        trackMessage('Script email not sent. Error opening smtp connection')


def sendEmail(pc, email, scriptName, scriptOptions, start = False, msg=None,
              restart=False):
    """Sends a message to the PI with information about their track.

       This function should be used to send a message from a schedule script.
       It has separate messages for the start and end of the schedule, 
       depending on the value of 'start'. If there is an error sending the 
       message, an error message is printed on the terminal.

       Inputs: pc            - Program code (e.g., 'CX006')
               email         - Comma separated list of recipient e-mail addresses
               scriptName    - Name of the track that is being run
               scriptOptions - Any options passed to the script
               start         - 'True' if email is sent at the beginning of track
               msg           - Message to be included in email
               restart       - if True, send message indicating track 
                               has been restarted.

       Output: None

       Examples:
            (1) To send the start message:

                scriptName = os.path.split(__file__)[1]
                projectCode = scriptName[0 : 5]
                pi = 'Arthur Itis'
                email = 'ai@mmarray.org,obs@mmarray.org'

                sendEmail(projectCode, pi, email, scriptName, True) :

            (2) To send the finish message:

                sendEmail(projectCode, pi, email, scriptName) :
    """

    # If the email address is not set, then return
    if email == None: return
    if restart == True: return

    # Get obsblock name
    subarrayNumber = s.getSubarrayNo()
    obsblockName = 'Unknown'
    if subarrayNumber < 3: 
        obsblockName = queryString('Control.Subarray%s.obsBlockId' % subarrayNumber)


    # Body of message
    msgBody = ''
    if pc <> None and pc <> '': 
        msgBody += 'Project code  : ' + pc + '\n'
    else:
        msgBody += 'Project code  : Unknown\n'
    msgBody += 'Starting time : ' + getLocalTime() + '\n'
    msgBody += 'LST time      : ' + convertHmsString(lst()) + '\n'
    if scriptName <> None and scriptName <> '': 
        msgBody += 'Name of script: ' + scriptName + '\n'
    else:
        msgBody += 'Name of script: Unknown\n'
    if scriptOptions <> None and scriptOptions <> '': 
        msgBody += 'Script options: ' + scriptOptions + '\n'
    else:
        msgBody += 'Script options: None\n'
    msgBody += 'Obsblock name : ' + obsblockName + '\n'
    msgBody += '\n'

    # Create the Subject line and e-mail message for the start or end 
    # of schedule as appropriate
    subject = 'Project ' + pc + ': '
    if restart == True:
        subject += 'CARMA track restarted'
    elif start == True:
        subject += 'CARMA track started'
    else:
        subject += 'CARMA track finished'
        msgBody += 'Please go to \n' + \
                   '      http://cedarflat.mmarray.org/observing\nfor instructions on how to retrieve your data.\n\n'

    # Attachments
    if msg <> None or scriptName <> None:
        nattachments = 0
        msgBody += 'Attachments:'
        if scriptName <> None: 
            nattachments += 1
            msgBody += '\n    ' + str(nattachments) + ') observing script'
        if msg <> None       : 
            nattachments += 1
            msgBody += '\n    ' + str(nattachments) + ') script log'
        if nattachments == 0: msgBody += ' None'
        msgBody += '\n\n\n'
    msgBody += 'Send observers feedback at obs@mmarray.org' 

    # Create script log
    scriptLog = None
    if msg <> None:
        scriptLog = '                          Script log for project ' + pc + '\n'
        scriptLog += 'LST      Comments\n'
        scriptLog += '-------- ----------------------------------------------------------------------\n'
        for n in makeList(msg):
            if n <> None: scriptLog += n + '\n'

    # Send the message and check for errors
    sendFrom = 'obs@mmarray.org'
    bcc  = None
    cc   = 'obs@mmarray.org'
    attachments = scriptName
    sendEmailMessage(email, sendFrom, msgBody, subject=subject, bcc=bcc, cc=cc,
                     attachments=attachments, scriptLog=scriptLog)


def continueObservations(isScience=False):
    """ Indicates if observations should continue based on keyboard inputs 

        Inputs: isScience - current object is science target

        Observations will be halted if endtrack() is set, or lastcal() is
        set and this is a science source
    """
    cont = True
    if endOfTrack() or (isLastcal() and isScience): cont = False
    return cont


def getSourceName(sources, n=None):
    """ Retrieve the nth source name from sources.

        Function takes into account whether sources is a scalar or list.
        The first source corresponds to n = 1.
    """

    l = makeList(sources)
    if n <> None and n > len(l):
        raise Exception, 'Error trying to read source name'
    if n == None: n = 1
    return l[n-1]


def getPhaseCal(phaseCal, n=None):
    """ Returns source name, starting lst, and ending LST for phase calibrator.

        Inputs: phaseCal - scalar or list of phase calibrators
                n        - The number of the calibrator to retrieve.
                           The first calibrator is n=1.

        Output  phaseName, startlst, stoplst
    """
    # If there is no phase calibrator, return None
    if phaseCal == None: return None, None, None

    # Initialize
    pname    = None
    lststart = None
    lststop  = None
    if n == None or n <= 0: n = 1

    # If phaseCal is scalar, then only source names are specified
    if str in [type(phaseCal)]:
        pname = phaseCal
    elif list in [type(phaseCal)]:
        # Get first element
        pname = phaseCal[0]
        if list in [type(pname)] and len(phaseCal) < n:
            trackError('Error entering phase calibrator names')
            raise Exception, phaseCal

        # Read info
        t = phaseCal[n-1]
        if str in [type(t)]:
            pname = t
            if len(phaseCal) >= 2: lststart = phaseCal[1]
            if len(phaseCal) >= 3: lststop  = phaseCal[2]
        else:
            pname = t[0]
            if len(t) >= 2: lststart = t[1]
            if len(t) >= 3: lststop  = t[2]

        # Check LST start/stop time
        if lststop <> None:
            dt = timeDifference(lststop, lststart)
            if dt <= 0.0 or dt > 12.0:
                raise Exception,'LST range for phase calibrator ' + pname + \
                                ' must be between 0 and 12 hours'
    else:
        trackError('Error entering phase calibrator names')
        raise Exception, phaseCal

    # Done
    return pname, lststart, lststop


def doSourcesPhasecal(var, sources, phaseCal, time, fluxSources=None,
                      doFlux=False, mosaic=None, reconfig=False,
                      minFluxPointing=4.0, hybrid=None,
                      resetDoppler=False, fluxParams=None):
    """ Cycle between phaseCal and sources.

        This function will cycle between phase calibrator and sources.
        phaseCal can be a list() that contains multiple phase calibrators
        and various start/stop times. This routine loops over the specified
        phase calibrators, and calls doSources for each calibrator in turn.

        Inputs: var        - dictionary containing command line options
                sources    - string variable or list of science sources
                phaseCal   - string variable or list of phase calibrators
                time       - dictionary containing integration time variables
                fluxSources- list of possible flux calibration sources
                doFlux     - observe flux calibrators if possible
                mosaic     - mosaic options
                reconfig   - If True, reconfigure correlator for continuum
                             before observing phase/flux calibrators.
                hybrid     - hybrid correlator modes, used here only for 
                             estimating time needed to complete track
                minFluxPointing: minimum flux in Jansky's for radio pointing
                resetDoppler   : If true, then set the doppler command
                                 for each source in "sources"
                fluxParams - dictionary that specified various flux cal
                             options
         Notes of phaseCal:
            phaseCal is either a scalar containing a single source name,
            of a list containing source names and start/stop lst times.
            Examples:
                 Single phase calibrator:
                     phaseCal = '0530+135'
                     In this example, 0530+135 is the phase calibrator 
                     for the entire track.

                 Multiple phase calibrators
                     phasecal = [  ['3c111',    '02:00:00', '04:30:00'],
                                   ['0530+135', '04:30:00', '07:00:00']
                                ]
                     In this example, 3c111 is the phase calibrator 
                     between 2:: and 4:30 LST, and 0530+135 is the phase
                     calibrator between 4:30 and 7:00 LST.

         Output: time in hours spent in function
    """
    # Convert phase cal to list
    phase = makeList(phaseCal)
    if phase == None: phase = [None]

    # Loop over phase calibrators
    for p in phase:
        # Read phase calibrator name, and optionally start/stop time
        pname, pstart, pstop = getPhaseCal(p)

        # Ready to go
        if timeRemaining(source=sources, phase=pname, lsttime=pstart, toRise=True) <= 0.0:
            doCycles(var, sources, pname, time,
                     fluxSources=fluxSources, doFlux=doFlux, mosaic=mosaic, 
                     reconfig=reconfig, hybrid=hybrid, pstop=pstop, 
                     minFluxPointing=minFluxPointing, 
                     resetDoppler=resetDoppler, fluxParams=fluxParams)


def doCycles(var, sources, phaseCal, time, fluxSources=None, doFlux=False, 
             mosaic=None, reconfig=None, hybrid=None, pstop=None,
             minFluxPointing=4.0, resetDoppler=False, fluxParams=None):
    """ Perform one cycle between phaseCal and sources.

        Inputs: var        - dictionary containing command line options
                sources    - string variable or list containing name of
                             science sources
                phaseCal   - name of the phase calibrator [scalar only]
                time       - dictionary containing integration time variables
                fluxSources- list of possible flux calibration sources
                doFlux     - observe flux calibrators if possible
                mosaic     - mosaic options
                reconfig   - If set, reconfigure correlator for continuum
                             before observing phase/flux calibrators.
                hybrid     - If set, contains list of hybrid correlator
                             configurations to observe in passband calibration.
                             This is needed only to estimate time needed to
                             finish track.
                pstop      - ending LST time for this phase calibrator
                minFluxPointing : minimum flux in Janskys for radio pointing
                resetDoppler : If true, re-issue doppler command for each source
                fluxParams   : Dictionary that controls various flux cal options

         Output: time in hours spent in function
    """
    # See if this is a mosaic observation
    isMosaic = mosaic <> None;
    if isMosaic: isMosaic = mosaic['mosaic']

    # Compute time in hours for required observations
    foverhead = overhead() / 60.0        # 60.0 converts minutes to hours
    # Nominal passband
    timePb = foverhead * time['pb']
    # Add passband time if observing two correlator configurations
    if reconfig <> None: timePb *= 2.0   
    # Add passband time if observing hybrid configurations
    if hybrid <> None: timePb += foverhead * time['hybrid'] * len(hybrid)
    # Primary flux calibrator
    timeFluxPrimary = foverhead * time['flux']
    # Secondary flux calibrator
    timeFluxSecondary = 0.0
    observeSecondary = (s.getScriptInt(INDX_INT_NOBS_FLUXSEC) == 0)
    doBothPrimarySecondary = False;
    if var['flux']:
        if fluxParams <> None:
            if fluxParams.has_key('doSecondary'):
                if fluxParams['doSecondary']:
                    if fluxParams['doBoth']: 
                        observeSecondary = True;
                        doBothPrimarySecondary = True
                else:
                    observeSecondary = False;
        if observeSecondary: timeFluxSecondary = foverhead * time['flux']
    doSecondary = var['flux'] and (observeSecondary or \
                  (not finishedFluxcal() and doBothPrimarySecondary))

    # Last calibrator
    timeLastcal = foverhead * time['phase']
    # Source observations
    sou,t = setSourceTint(sources,time['source'])
    if isMosaic:
        timeCycle = timeLastcal + \
                       foverhead * (t[0] * mosaic['nPhase'] + sum(t[1:]))
    else:
        timeCycle = timeLastcal + foverhead * sum(t)

    # Initialize cycles
    okSource  = False
    continueCycles = not endOfTrack()
    s.setScriptBool(INDX_BOOL_LASTCAL,False)
    s.setScriptBool(INDX_BOOL_STOPNEXTCAL,False)
    ncycle = s.getScriptInt(INDX_INT_LASTCYCLE)

    # Start cycles
    while continueCycles :
        # Compute time needed to complete remaining critical observations
        tneed = 0.0
        if var['pb']   and not finishedPassband() : tneed += timePb
        if var['flux']:
            if not finishedFluxcal() :
                tneed += timeFluxPrimary * getNfluxRemaining(fluxParams)
                if doSecondary : tneed += timeFluxSecondary

        # Message
        ncycle += 1
        tremaining = timeRemaining(source=sou, phase=phaseCal, useTime=True,
                                   lsttime=getLstStopCycle(default=pstop))
        dt = tremaining - tneed
        if dt < 0.0: dt = 0.0
        msg = 'Cycle ' + str('%3d' % ncycle) + '     ' + \
              dtString(dt) + ' remaining'
        trackMessage(msg)
        s.setScriptInt(INDX_INT_LASTCYCLE, ncycle)
        if tremaining <= tneed:
            trackMessage('time limit reached on source cycles', indent=1)
            continueCycles = False
            continue
        printWeather()

        # Observe phase calibrator
        okSource = False
        if phaseCal == None:
            okCal = True
        elif continueObservations() and tremaining > tneed:
            okCal = observeSource(phaseCal, time['phase'], time['record'],
                               lststop=getLstStopCycle(default=pstop),
                               tsysTime=time['tsys'], reconfig=reconfig,
                               tnoise=time['noise'], trecnoise=time['recNoise'],
                               tmo=time['tmoTrack'], intentObject='G', 
                               selfcal=True)
        if isStopAfterNextCal():
            continueCycles = False
            continue

        # Check pointing
        pointed  = False
        if var['point'] and continueObservations(isScience=True) and \
           timeRemaining(useTime=True, lsttime=getLstStopCycle(default=pstop)) > 0.0:
            pointed = doPoint(time=time, preferred=var['pntname'], ref=sources,
                              reconfig=reconfig, minFlux=minFluxPointing,
                              el=var['elcal'], maxsep=var['pntsep'],
                              printHeader=False)

        # Flux calibrator
        observedFluxcal = False
        if var['flux'] and not finishedFluxcal() and doFlux and \
           continueObservations(isScience=True) and \
           timeRemaining(useTime=True, lsttime=getLstStopCycle(default=pstop)) > 0.0:
            observedFluxcal = doFluxcal(fluxSources, time, 
                  preferred=var['fluxname'], el=var['elcal'], ref=sources, 
                  reconfig=reconfig, lststop=pstop, fluxParams=fluxParams,
                  printHeader=False)
            doSecondary = (observeSecondary or \
                          (not finishedFluxcal() and doBothPrimarySecondary))

        # Observe source if we did not point and did not observe flux cal
        tneed = 0.0
        if phaseCal <> None : tneed += timeLastcal
        if var['pb']   and not finishedPassband() : tneed += timePb
        if var['flux'] and not finishedFluxcal()  : 
            tneed += timeFluxPrimary * getNfluxRemaining(fluxParams)
            if doSecondary: tneed += timeFluxSecondary
        okSource = False
        tremaining = timeRemaining(source=sou, phase=phaseCal, useTime=True,
                              lsttime=getLstStopCycle(default=pstop))
        if not pointed and not observedFluxcal and \
           continueObservations(isScience=True) and tremaining > tneed:
            tremaining -= tneed
            mstart = lst()
            if isMosaic:
                okSource = observeMosaic(time, sou, mosaic, 
                              maximumTime=tremaining, 
                              lststop=getLstStopCycle(default=pstop))
            else:
                okSource = observeSource(sources, time['source'], 
                              time['record'], isScience=True,
                              lststop=getLstStopCycle(default=pstop),
                              maximumTime=tremaining, tsysTime=time['tsys'],
                              trecnoise=time['recNoise'], tmo=time['tmoTrack'],
                              resetDoppler=resetDoppler, intentObject='S',
                              selfcal=True)
            addSourceTime(timeDifference(lst(), mstart, pos=True))

        # Stop if nothing was observed or we have reached the time limit.
        # Also stop if we do not have time to finish remaining observations.
        tremaining = timeRemaining(source=sou, phase=phaseCal, useTime=True,
                                   lsttime=getLstStopCycle(default=pstop))
        if tremaining <= tneed:
            if tneed > 0.0:
                msg = 'Stopping phase-cal cycle to finish remaining observations'
            else: 
                msg = 'Reached time limit on source/phase-calibrator'
            trackMessage(msg)
            continueCycles = False
        elif not continueObservations(isScience=True):
            continueCycles = False
        elif not okCal and not okSource and not pointed and not observedFluxcal:
            trackMessage('No sources observed - exiting cycle')
            continueCycles = False

    # End cycle on phase calibrator
    if okSource and continueObservations() and phaseCal <> None:
        trackMessage('Observing ' + phaseCal + ' one last time')
        observeSource(phaseCal, time['phase'], time['record'],
                      tsysTime=time['tsys'], reconfig=reconfig,
                      tnoise=time['noise'], trecnoise=time['recNoise'],
                      tmo=time['tmoTrack'], intentObject='G', selfcal=True)

    # See why we are here
    if endOfTrack():
        trackMessage('Ending observations based on endtrack()')
    elif isLastcal(): 
        trackMessage('Ending cycle based on lastcal()')
    elif isStopAfterNextCal():
        trackMessage('Ending cycle based on stopAfterNextCal()')


def observeNoise(tintMinutes, trecordSeconds, verbose=True, indent=None):
    """ Observe the noise for tintMinutes with a record time of
        trecordSeconds. The number of repeats of trecordSeconds is given
        by tintMinutes/trecordSeconds rounded to the nearest integer with
        a minimum of one repeat.

        The observing sequence for the noise source is:
           (1) noiseon()
           (2) integrate(trecordSeconds,nrepeats)
           (3) noiseoff())
    """
    nrep = getNreps(tintMinutes,trecordSeconds)
    if nrep > 0: 
        if verbose:
            tmp = str("%-10s" % 'noise') + '   tint=' + \
                str("%9s" % dtString(trecordSeconds * nrep / 3600.0))
            trackMessage(tmp, indent=indent)
        noiseon()
        wait(item=TRACK, waiton=-2) # Get most of the antennas
        intent('noise','B',True)
        integrate(trecordSeconds, nrep, antwait=ALL, tmo=5)
        noiseoff()


def observeSource(sources, tint, trecord, lststop=None,
                  tsysTime=None, maximumTime=None, isScience=False, 
                  reconfig=None, tnoise=None, trecnoise=TRECORD_NOISE,
                  tmo=None, hybrid=None, origConfig=None, resetDoppler=False,
                  el=None, indent=1, intentObject="O", selfcal=False):
    """ Observe "sources" for integration time "tint" minutes with a
        record time of "trecord" seconds. 
                                          
        Data are obtained only 
           1) the current lst is before lstStop
           2) source is above the elevation limit stored in the CARMA
              control system.
        The maximum integration time is maximumTime. 
        
        If tnoise != None,
        a noise source integration is obtained while slewing to the source.

        The system temperature is measured at the beginning of the first
        integration and every tsysTime minutes afterwards.

       Inputs: sources    - string or string list of "sources" to observe
               tint       - integration time in minutes for the sources. If
                            a single value is given, then it applies to each
                            element in the "sources" list. Otherwise, the number
                            of elements in "tint" must be the same in "sources".
               trecord    - record length in seconds for the integrations
               lstStop    - stopping LST time in HH:MM:SS for the observations
               tsysTime   - Measure tsys every tsysTime minutes. Tsys is always
                            measured when moving to a source for the first time.
               maximumTime- Maximum time in hours to spend observing the sources
               reconfig   - If set, reconfigure correlator for continuum
               isScience  - If true, then observing science target. This is 
                            needed to makr lastcal() and stopAfterNextCal()
                            have their intended affect.
               tnoise     - Integration time in minutes for the noise source
               trecnoise  - Record time in seconds for the source source
               hybrid     - List of hybrid bands to use instead of current
                            correlator configuration.
               origConfig - original correlator configuration. If set, then
                            the correlator configuration is not set or checked
                            at the beginning of the observing sequence. This
                            is useful if observing multiple correlator 
                            configurations in sequence.
               el         - minimum elevation limit for the observations
               resetDoppler - If True, re-issue doppler command for each source
                              in the "sources" list.
               intentObject- intent for the source(s)
               selfcal     - Is the source self-calibratable? (True/False)
 
        Output: Boolean value indicating if source was observed

        Note: Since tint, trecord, and tsysTime may not be divisible into
              integers, tsysTime is only approximate.
    """
    # Check which antennas are online
    checkAntennas(indent=indent)

    # Keep track if a source was observed in this cycle
    tstart = lst()
    observedSource = False
    if not continueObservations(isScience=isScience): return observedSource

    # Put inputs in a list
    sou, integTime = setSourceTint(sources, tint)
 
    # Set original correlator configuration, if needed.
    # I tried using copy.deepcopy(), but that did not work in terms of perserving 
    # the BW and LSB/USB types.
    if origConfig == None:
        specConfig = short.getConfigCorrT()
    else:
        specConfig = list()
        for i in origConfig:
            specConfig.append(i[:])

    # Reset correlator configuration. Only copy the parameters here, and
    # set it after looking at hybrid
    # I tried using copy.deepcopy(), but that did not work in terms of 
    # perserving the BW and LSB/USB types.
    contConfig = None
    if reconfig <> None: 
        contConfig = list()
        for i in reconfig:
            contConfig.append(i[:])

    # Change band width for hybrid correlator mode if needed
    # The IF frequency is determined as follows:
    #      1) if BW=500, IF frequency given by reconfig (if set)
    #      2) if BW<500, IF frequency given by config
    if hybrid <> None:
        # Loop over the bands in the hybrid configuration
        for i in range(len(hybrid)):
            # Get configuration for this band to set IF frequency
            hconfig = specConfig[i][:]
            if hybrid[i] == BW500 and contConfig <> None: 
                hconfig = contConfig[i][:]

            # Set hybrid bandwidth
            hconfig[1] = hybrid[i]

            # Copy to final hybrid configuration
            contConfig[i] = hconfig[:]

    # Change correlator
    if reconfig <> None or hybrid <> None:
        msg = "setting correlator for calibrator observations"
        if hybrid <> None:
            msg = "setting correlator for hybrid observations"
        trackMessage(msg, indent=indent)
        short.changeConfigCorr(contConfig)

    # Loop over sources
    tint_actual  = 0.0
    tint_maximum = 0.0
    tintSinceLastTsys = None
    for isource in range(len(sou)):
        # Check to see if source is up and whether this is the noise source
        isUp = False
        isNoiseSource = False
        name = sou[isource]
        if string.lower(name) == 'noise': 
            isUp = True
            isNoiseSource = True
        else:
            isUp = isSourceUp(name, el=el)

        # Compute remaining time for this cycle
        tremaining = timeRemaining(lsttime=lststop, source=sou, el=el)
        if maximumTime <> None: 
            tremaining = min(maximumTime - lst() + tstart, tremaining)

        # Stop if ENDTRACK or LASTCAL is set
        if endOfTrack() or (isLastcal() and isScience): continue

        # Observe source if it is up
        if not isUp:
            trackMessage(name + ' has set', indent=indent)
        elif tremaining == None or tremaining > 0.0:
            # Integration time should not exceed time remaining in track
            integrationTime = integTime[isource]
            if tremaining <> None:
                integrationTime = min(integrationTime, tremaining * 60.0)
            if integrationTime < minimumIntegrationTime() / 60.0: 
                integrationTime = 0.0
            if integrationTime < 1.0 and \
               integrationTime <= 0.5*integTime[isource]: continue

            # Set number of repetitions and cycles to observe this source
            if isNoiseSource:
                nreps = getNreps(integrationTime, trecord)
            else:
                nreps = getNreps(integrationTime, trecord, tsysMinutes=tsysTime,
                                 tintSinceLastTsys=tintSinceLastTsys)

            # Track source. Observe noise source first if tnoise != None
            if not isNoiseSource: 
                # Reset doppler
                if resetDoppler: 
                    t += ' doppler reset'
                    doppler(sou[isource])

                # Noise source
                if tnoise <> None:
                    etrack.etrack(sou[isource])
                    observeNoise(tnoise, trecnoise, verbose=True, indent=indent)

                # Track source
                intent(sou[isource], intentObject, selfcal)
                if tmo == None:
                    etrack.etrack(sou[isource])
                else: 
                    etrack.etrack(sou[isource], tmo=tmo)

            # Loop over cycles
            nrepsList = makeList(nreps)
            firstSource = False
            for j in range(len(nrepsList)):
                n = nrepsList[j]
                if n > 0 and continueObservations(isScience=isScience):
                    # Print message on how long we are going to integrate
                    ttot = n * trecord / 60.0
                    t = str("%-10s" % sou[isource]) + '   tint=' + \
                        str("%9s" % dtString(ttot/60.0)) + '  el=' + \
                        str("%4.1f" % getSourceElevation(sou[isource]))

                    # Indicate if we will measure tsys
                    # If tsysTime < 0, then nreps[0] contains number if 
                    # integrations to perform before next tsys
                    measureTsys = True
                    if tsysTime == None or tsysTime == 0 or isNoiseSource or \
                       (j == 0 and tsysTime < 0.0 and tintSinceLastTsys <> None):
                        t += ' (no tsys)'
                        measureTsys = False
                    else:
                        t += ' (tsys)'

                    # Message
                    trackMessage(t, indent=indent)

                    # Measure tsys
                    if measureTsys:
                        tsys()
                        tintSinceLastTsys = 0.0

                    # Integrate
                    if isNoiseSource:
                        observeNoise(trecord*n, trecnoise, verbose=True)
                    else:
                        integrate(trecord,n)
                        tint_actual += trecord * n
                        tint_maximum += integTime[isource]
                        tintSinceLastTsys += (trecord * n) / 60.0

    # Change correlator back if reconfig/hybrid was set AND origConfg was not.
    if contConfig <> None and origConfig == None:
        trackMessage("setting correlator for science observations", 
                     indent=indent)
        short.changeConfigCorr(specConfig)

    # Did we integrate long enough to call this done?
    # if tint_actual >= 0.8 * tint_maxumum * 60.0: observedSource = True
    if tint_actual > 0.0: observedSource = True
 
    # Reset doppler command
    if resetDoppler:
        trackMessage('re-setting doppler tracking for source '+sou[0], 
                     indent=indent)
        doppler(sou[0])

    # Done
    return observedSource


def observeMosaic(time, source, mosaic, lststop=None, maximumTime=None):
    """ Make mosaic

        Inputs: time        - integration time per position
                source      - source name
                mosaic      - mosaic parameters
                lststop     - stopping LST time for mosaic
                maximumTime - [hours] Maximum time to spend observing the mosaic

        Output: True if at least one positions were observed, False if not
    """
    # Initialize
    tstart = lst()
    observedSource = False

    # Sou and integration time can only be scalar values
    sou, integTime = setSourceTint(source, time['source'])
    souMosaic = sou[0]
    integTimeMosaic = integTime[0]

    # Compute how much time is needed to complete test calibrator sources
    tneed = sum(integTime[1:]) / 60.0 * overhead()

    # Set mosaic offsets once. Offsets are stored in arcminutes 
    # within mosaic['offsetsArcmin'] keyword
    if not mosaic.has_key('offsetsArcmin'):
        # Read offsets
        if mosaic['offsets'] <> None:
            trackMessage('setting mosaic offsets from input list in script',indent=1)
            mosaic['offsetsArcmin'] = mosaic['offsets']
        elif mosaic['offsetFile'] <> None:
            inputFile = string.strip(mosaic['offsetFile'])
            if inputFile.find('/') != 0: 
                inputFile = '/array/rt/mosaics/' + inputFile
            trackMessage('reading mosaic offsets from '+inputFile,indent=1)
            mosaic['offsetsArcmin'] = fileIOPython.fileToTable(inputFile,ignoreEmpty=True)
        else:
            raise Exception, 'Error specifying mosaic offsets'
        trackMessage('mosaic contains ' + str(len(mosaic['offsetsArcmin'])) + 
              ' positions', indent=1)

        # Make sure offsets are a list
        if list not in [type(mosaic['offsetsArcmin'])]:
            raise Exception, 'Mosaic offsets must be a list'

        # Convert offsets into arcminutes, and change string to floating point
        zoff = mosaic['offsetsArcmin']
        stepSize = 1.0
        if not mosaic['arcminUnits']:
            stepSize = short.getNyquistStep(min(currentAntennaNumbers()))[0]
        for i in range(len(zoff)):
             x = float(zoff[i][0]) * stepSize
             y = float(zoff[i][1]) * stepSize
             zoff[i] = [x,y]
        mosaic['offsetsArcmin'] = zoff

    # Copy pointing offsets
    offsets = mosaic['offsetsArcmin']
    npointingCenters = len(offsets)

    # Set number of pointing centers to observe per cycle
    nposCycle = mosaic['nPhase']
    if nposCycle <= 0: nposCycle = npointingCenters

    # Continue if source is up
    if timeRemaining(source=sou, lsttime=lststop) > 0:
        # Track source
        trackMessage('slewing to ' + souMosaic, indent=1)
        intent(souMosaic, 'S', False)
        if time['tmoTrack'] == None:
            etrack.etrack(souMosaic)
        else:
            etrack.etrack(souMosaic, tmo=time['tmoTrack'])

        # Initialize cycle
        tintSinceLastTsys = None  # Integration time [minutes] since last tsys 
        nposObserved = 0          # Number of positions that have been observed
        observedSourceInCycle = True

        # Loop over mosaic positions. Measure tsys every tsysTime minutes
        while ((maximumTime == None or 
                (maximumTime - lst() + tstart - tneed) > 0) and 
                nposObserved < nposCycle and observedSourceInCycle and
                continueObservations(isScience=True)):
            # Initialize
            observedSourceInCycle = False

            # Tsys measurement
            if tintSinceLastTsys == None or \
               (time['tsys'] <> None and 
                 tintSinceLastTsys >= abs(time['tsys'])) :
                trackMessage('Measuring tsys', indent=1)
                tsys()
                tintSinceLastTsys = 0.0

            # Set integration time without exceeding time or elevation limits
            tint = min(integTimeMosaic, 
                       timeRemaining(source=souMosaic, lsttime=lststop) * 60)
            if maximumTime <> None: 
                tint = min(tint, (maximumTime - lst() + tstart) * 60.0)
            if tint < minimumIntegrationTime() / 60.0: 
                tint = 0.0
            if tint < 1.0 and tint <= 0.5*integTimeMosaic: continue

            # Set mosaic position
            pos = max(1, getLastMosaicPosition(mosaic['startpos']) + 1)
            if pos > npointingCenters: pos = 1

            # Print message
            t = str("%-10s" % souMosaic) + '   tint=' + \
                str("%9s" % dtString(tint/60.0)) + '  el=' + \
                str("%4.1f" % getSourceElevation(souMosaic)) + \
                '  Pos=' + str("%3d" % pos) + '  ' + \
                str("%6.3f" % offsets[pos-1][0]) + "' " + \
                str("%6.3f" % offsets[pos-1][1]) + "'"
            trackMessage(t, indent=1)

            # Offset telescopes. pos starts at 1, while offsets are index=0.
            if time['tmoMosaic'] == None:
                equatOffset(offsets[pos-1][0], offsets[pos-1][1])
            else:
                equatOffset(offsets[pos-1][0], offsets[pos-1][1], tmo=time['tmoMosaic'])

            # Integrate
            trecordMosaic = time['recMosaic']
            nreps = getNreps(tint, trecordMosaic)
            if nreps > 0:
                integrate(trecordMosaic, nreps)
                observedSource = True
                tintSinceLastTsys += nreps * trecordMosaic / 60.0
                if nreps * trecordMosaic / 60.0 >= 0.5 * integTimeMosaic:
                    observedSourceInCycle = True
                    setLastMosaicPosition(pos)
                    nposObserved += 1

    # Mosaic observations are finished, so now observe any extra sources
    for i in range(1,len(sou)):
        ok = observeSource(sou[i], integTime[i], time['recMosaic'],
                       tsysTime=time['tsys'], lststop=lststop, 
                       maximumTime=maximumTime, tmo=time['tmoTrack'],
                       intentObject='O', selfcal=false)

    return observedSource
