# obsdef2Utils.py
#
# @author John Carpenter
# $Id: obsdefUtils.py,v 1.132 2015/03/16 17:07:03 jmc Exp $
#
#   History
#   2007-Aug-13  JMC    Separated some utility functions from obsdef2


import subarrayCommands as commands
import math
import os
import string
import time
import numpy as np
import copy
import projectManage as pm

import carmaHelpers as helpers
from obsdefIndex import *
import fileIOPython
import printFunctions
import subprocess
import shlex
import sys
import re

# from subarrayCommands import *

# For sendEmailMessage() function
import smtplib
import email
import email.MIMEMultipart
import email.MIMEText
import email.utils

DIRECTORY_MOSAICS    = '/array/rt/mosaics/' 
OBSERVATORY_LATITUDE = 37.280358  # Degrees
DT_TRANSITION        = 2.0        # +/- transition time in hours around sunrise/set

CORR_BW500    = 1
CORR_BW500LO  = 2
CORR_BW500LO6 = 3
CORR_CUSTOM   = 4


SUNSTATE_NIGHT = 1
SUNSTATE_DAY   = 2
SUNSTATE_TRANS = 3

def minimumIntegrationTime():
    """ Returns minimum integration time in seconds allowed 
        by the control system
    """
    return 0.5


def overhead():
    """ Adopted overhead factor for observing.

        Inputs: None

        Output: Overhead factor (currently 1.3)

        Example: x = overhead()
    """
    return 1.3

def disableCoherenceAlarm():
   commands.alarmMpdisable(['Astro.Antenna%d.MaxCoherence'%ant for ant in commands.currentAntennaNumbers()])

def enableCoherenceAlarm():
   # Get list of antennas in subarray
   ants = commands.currentAntennaNumbers()

   # Get list of antennas with known bad coherence and remove them from list
   lowcoh = commands.s.getScriptString(INDX_STR_LOWCOH).replace("["," ").replace("]"," ").replace(",", " ").split()
   for a in lowcoh:
      if a != "" and ants.count(int(a)) > 0:
         ants.remove(int(a))
      elif a == '0':
         ants = []

   # Restore alarms
   commands.alarmMpenable(['Astro.Antenna%d.MaxCoherence'%a for a in ants])

def parseObsblock(obsblock):
    """ Splits obsblock name into project, obsblock, subobsblock, and trial """

    tokens = obsblock.split('.')
    if len(tokens) < 3:
       raise Exception,'Invalid obsblock name : %s' % obsblock
    result = dict()
    result['project'] = tokens[0]
    result['obsblock'] = tokens[1]
    result['trial'] = int(tokens[-1:][0])
    result['subobsblock'] = None
    if len(tokens) == 4: result['subobsblock'] = tokens[2]

    return result

def resetObsblockName(subarray, obsblock):
    """ Manually set the project, obsblock, subobsblock, and trial
        given a full obsblock name.

        Inputs: subarray (s1 or s2)
                obsblock (e.g. test.1A_30object.2)
    """

    # I inserted sleep commands since otherwise, the updates do not always occur
    tokens = obsblock.split('.')
    subarray.project(tokens[0])
    commands.sleep(1)
    subarray.obsblock(tokens[1])
    commands.sleep(1)
    if len(tokens) == 4 :
       subarray.subObsblock(tokens[2])
       commands.sleep(1)
       subarray.trial(int(tokens[3]))
    elif len(tokens) == 3 :
       subarray.subObsblock('')
       subarray.trial(int(tokens[2]))
    commands.sleep(1)


def isShadowed(sourceName, ants=None, lstHours=-1.0, diameterFraction=0.0):
    """ Returns True or False if antenna(s) in the subarray for 
        sourceName will be shadowed.

        Arguments lstHours, and diameterFraction are the
        same as for isShadowedSource(). The optional argument
        ants indicates which antennas to check. If None, then
        all antennas in the current subarray are checked.

        ants : List of antennas to check for shadowing. If None,
               then all antennas in the current subarray are checked.
        lstHours: Compute shadowing for LST time lstHours.
                  default of lst < 0 uses the current LST
        diameterFraction: Amount of shadowing to tolerate
    """

    # Set lst time
    xlst = lstHours
    if xlst < 0.0: xlst = commands.lst()
    if xlst > 24.0: xlst = xlst % 24.0

    # Get dec/HA
    dec = helpers.convertHms(getSourceDec(sourceName))
    ra = helpers.convertHms(getSourceRa(sourceName))
    ha = xlst - ra
    if ha > 12.0: ha -= 24.0
    if ha < -12.0: ha += 24.0

    # Set antennas
    antennas = makeListObsdef(ants)
    if antennas == None: antennas = commands.currentAntennaNumbers()

    # Loop over antennas
    shadowed = False
    for a in antennas:
#      if commands.isShadowedSource(a, sourceName, lstHours=xlst, shadowingType=shadowingType, diameterFraction=diameterFraction):
       if commands.isShadowedHaDec(a, ha, dec, shadowingType=commands.SHADOW_INTERNAL, diameterFraction=diameterFraction) or \
          commands.isShadowedHaDec(a, ha, dec, shadowingType=commands.SHADOW_SWEPTVOLUME, diameterFraction=diameterFraction):

          shadowed = True
          break

    # Done
    return shadowed


def getOfficialObsblock(projectCode, obsblock, config=None):
    """ Given the projectCode and the expected obsblock, returns
        the obsblock actually stored in the database.

        Inputs:
           projectCode - project code in the database
           obsblock    - obsblock name or number on proposal cover sheet
           config      - Array configuration. If not specified, then the
                         current configuration is used.
    """

    # Get list of obsblocks
    projectSeq = commands.getProjectSequence( projectCode )
    obSeq      = commands.OBDescSequenceFromProjectSequence( projectSeq )

    # Get array configuration
    if config == None:
        config = commands.queryString(_controlMPprefix() + 'configName')

    # Convert obsblock to a string
    obsblock = str(obsblock)

    # Get the entered obsblock number, if not exists. This refers to the 
    # line on the proposal coversheet. There is no obsblock number for 
    # shared risk projects. Therefore, if the obsblock number is not a
    # digit, set the obsblock number to None.
    obsblockNumber = makeListObsdef(obsblock.upper().split(config))
    if len(obsblockNumber) != 1 or not obsblockNumber[0].isdigit(): 
        obsblockNumber = None
    else: 
        obsblockNumber = int(obsblockNumber[0])

    # Initialize
    oblock = None

    # Loop over obsblocks in database
    for o in obSeq:
        # Must match array configuration
        if o._arrayconfig.upper() != config.upper(): continue

        # Get official obsblock name from pid
        officialName = o._pid.split('.')[1]

        # See if name matches exactly. If so then exit loop
        if officialName.upper() == obsblock.upper(): 
            oblock = officialName
            break

        # Get official obsblock number from the obsblock.
        # This is the source number on the proposal cover sheet. 
        # The assumed format is <number><config>_<freq><name>
        tokens = officialName.split("_")  # tokens[0] = <number><config>
        officialNumber = makeListObsdef(tokens[0].upper().split(o._arrayconfig))[0]
        if officialNumber.isdigit():
            officialNumber = int(officialNumber)
        else:
            officialNumber = None

        # If the entered obsblock is a number, then it refers to the obslock
        # number on the cover sheet. Check for a match here.
        if obsblockNumber <> None and officialNumber <> None and obsblockNumber == officialNumber:
            oblock = officialName
            break

    # Finished
    if oblock == None:
        print 'Did not find official obsblock for %s.%s... continuing anyway' % (projectCode, obsblock)
        oblock = obsblock

    # Done
    return oblock


def makeObsblock(projectCode, source, freq=None, config=None, 
                 subarray=None, time=4, force=False, create=True):
    """ Create a new obsblock name for a project
        Inputs:
           projectCode - project code in the database
           source      - Name of the source
           config      - Array configuration. If not specified, then the
                         current configuration is used.
           subarray    - Either "1" or "2". If none specified, then the
                         current subarray is used.
           freq        - Frequency (GHz) of the observations, which will be 
                         used in the obsblock name.
           time        - Amount of time to allocate
           force       - Force a new obsblock even it one already exists.
           create      - If False, then do not create a new obsblock.
    """

    # Get list of obsblocks
    projectSeq = commands.getProjectSequence( projectCode )
    obSeq      = commands.OBDescSequenceFromProjectSequence( projectSeq )

    # Get subarray
    if subarray == None:
        subarray = commands.subarrayNo
    if subarray not in [1,2]:
       raise Exception,'Subarray must be either 1 or 2'

    # Get array configuration
    if config == None:
        config = commands.queryString(_controlMPprefix(subarray) + 'configName')

    # Set frequency
    if freq == None:
       if subarray == 1:
          freq = 95
       elif subarray == 2:
          freq = 30
       else:
          raise Exception,'Unknown subarray number'

    # Set the desired obsblock name, expect for the sequence number.
    # For the obsblock name, truncate the source name at 6 characters
    obsblock_suffix = '%s_%d%s' % (config, int(freq), source[0:6])

    # Initialize
    obsblock = None
    obsblockFirst = None
    match = False
    lastObsblockNumber = 0

    # Loop over obsblocks in database
    for o in obSeq:
        # Get official obsblock name from pid
        officialName = o._pid.split('.')[1]
        print 'Found obsblock ', officialName

        # Copy the first obsblock name
        if obsblockFirst is None:
            obsblockFirst = officialName

        # Must match array configuration
#       if o._arrayconfig.upper() != config.upper(): continue

        # Get obsblock number from the obsblock.
        # The assumed format is <number><config>_<freq><name>
        # This will NOT work if the configuration contains a number
        tokens = officialName.split("_")  # tokens[0] = <number><config>
        result = re.findall('\d+|\D+', tokens[0])
        if len(result) != 2:
           continue
#          print ''
#          print 'Obsblock = %s' % officialName
#          print 'Prefix   = %s' % tokens[0]
#          print 'Result   = %s' % result
#          raise Exception, 'Error splitting obsblock name'
        officialNumber = result[0]
        if officialNumber.isdigit():
           officialNumber = int(officialNumber)
        else:
#          print ''
#          print 'Obsblock = %s' % officialName
#          print 'Prefix   = %s' % tokens[0]
#          print 'Result   = %s' % result
#          print 'WARNING: The obsblock should contain a number'
           continue
        lastObsblockNumber = max(lastObsblockNumber, officialNumber)

        # See if we have match the obsblock name. 
        # If Force == False, then we are fone
        tmpobsblock = '%d%s' % (officialNumber, obsblock_suffix)
        match = (tmpobsblock.lower() == officialName.lower())
        if match:
            print '   ... found matching obsblock: %s' % officialName
            if force:
               print '   ... but since Force=True, will not use it.'
            else:
               obsblock = officialName
               break

    # If we do not have an obsblock, then create one
    print ''
    if obsblock is None:
        obsblock = '%d%s' % (lastObsblockNumber+1, obsblock_suffix)
        if create:
           print 'Creating obsblock %s' % obsblock
           pm.replicateObsblock(projectCode, obsblockFirst, obsblock)
           pm.changeItem(projectCode, obsblock, 'arrayConfiguration', config)
           print 'Allocating %g hours to obsblock' % time
           pm.changeAllocation(projectCode, obsblock, time)
        else:
           print 'Not creating obsblock %s since create=False.' % obsblock
    else:
        print 'Using obsblock %s.  No time was added' % obsblock

    # Done
    return obsblock


def getDopplerSource():
    """ Returns name of the source being doppler tracked """

    mp = _controlMPprefix() + 'dopplerSource' 
    return commands.queryString(mp)


def getTmax():
    """ Returns maximum track length (hours) for the current track """
    t = None
    if commands.s.getScriptBool(INDX_BOOL_TMAX):
        t = commands.s.getScriptDouble(INDX_DBL_TMAX)
    return t


def inSubarray(antenna):
    """ Returns True/False if antennas is in current subarray """
    in_subarray = (commands.currentAntennaNumbers().count(antenna) > 0)

    return in_subarray


def isSunUp(elmin=0.0):
    """ Return True/False if the sun is above elmin degrees elevation """
    return getSourceElevation('sun') > elmin


def isSourceUp(source, elmin=None):
    """ Returns True or False if source above an elevation limit of el degrees.

        If el is not specified, the current system elevation limit is used.
    """
    if elmin <> None:
        isUp = (getSourceElevation(source) >= elmin)
    else:
        isUp = (commands.isup(source) != 0)
    return isUp


def getHa(source) :
    """ Return source hour angle """
    ra = helpers.convertHms(getSourceRa(source))
    ha = commands.lst() - ra
    if ha < -12.0 : ha +=  24.0
    if ha >  12.0 : ha += -24.0
    return ha


def getAntennaMp(antenna):
    """ Get OVRO/BIMA/SZA monitor point name """
    if antenna <= 6:
        name = 'Ovro' + str(antenna)
    elif antenna <= 15:
        name = 'Bima' + str(antenna-6)
    elif antenna <= 23:
        name = 'Sza' + str(antenna-15)
    else:
        raise Exception, 'Invalid antenna number (' + str(antenna) + \
                         ') for monitor point'
    return name


def getSourceInfo(sources, elmin=None):
    """ Get source info from the check source command """

    # it is faster to use the s.info command if elevation is not specified, 
    source = makeListObsdef(sources)[0]
    if elmin == None:
        result = commands.s.info(source)
    else:
        # Start the command
        cmd = "/opt/rt/bin/checksource flux=f source=" + source + \
              " elevlim=" + str("%.2f" % elmin)

        # Add catalog
        try :
            mp = _controlMPprefix() + 'userCatalog' 
            ucat = commands.queryString(mp)
            if string.upper(ucat) <> "NONE":
                cmd += " catalog=/array/rt/catalogs/" + ucat
        except Exception:
            pass

        # Execute command
        mycmd = os.popen(cmd)
        result = mycmd.read()
        mycmd.close()

    # If results start with "isEphem:...", then remove first part of entry
    if result.find('isEphem::') == 0 and result.find('\nSOURCE')>0:
       result = result[result.find('\nSOURCE')+1:]

    # Done
    return result


def getSourceRa(source):
    """ Return right ascention as a string for 'source' in the current catalog"""
    info = getSourceInfo(source)
    return info.split('\n')[1].split()[1]


def getSourceDec(source):
    """ Return declination as a string for 'source' in the current catalog"""
    return getSourceInfo(source).split('\n')[1].split()[2]


def getSourceElevation(source):
    """ Return elevation as a floating point for 'source' in the current catalog"""
    return commands.azel(source)[1]


def getSourceVelocity(source):
    """ Return velocity as a floating point for 'source' in the current catalog"""
    t = getSourceInfo(source).split('\n')[1].split()
    vlsr = t[len(t)-2]
    return float(vlsr)


def getTimeToRise(sourceName, elmin=None, isup=None):
    """ Returns time in hours before the source rises again for the current
        elevation limit.

        Inputs: sourceName - source name (must be in the current catalog)
                el - elevation limit in degrees. If None, the current
                     system elevation limit is used.
                isup - Boolean that indicates if the source is up or not.
                       If None, then the function will determine that based
                       on the source elevation. It is useful to specify this
                       option to save a disk read.
    """
    # Get LST rise time
    tlst = getLstRise(sourceName, elmin=elmin)

    # Compute time difference
    dt = None
    if tlst == None: 
        dt = 0.0
    else:
        if isup == None: isup = isSourceUp(sourceName, elmin=elmin)
        dt = timeDifference(tlst, commands.lst())
        if dt < 0.0 and not isup: dt += 24.0
        if dt > 0.0 and     isup: dt -= 24.0

    return dt


def getTimeToSet(sourceName, elmin=None, isup=None):
    """ Returns time in hours before the source rises again for the current
        elevation limit.

        Inputs: sourceName - source name (must be in the current catalog)
                el - elevation limit in degrees. If None, the current
                     system elevation limit is used.
                isup - Boolean that indicates if the source is up or not.
                       If None, then the function will determine that based
                       on the source elevation. It is useful to specify this
                       option to save a disk read.
    """
    # Get LST set time
    tlst = getLstSet(sourceName, elmin=elmin)

    # Compute time difference
    dt = None
    if tlst <> None: 
        dt = timeDifference(tlst, commands.lst())
        if isup == None: isup = isSourceUp(sourceName, elmin=elmin)
        if dt < 0.0 and     isup: dt += 24.0
        if dt > 0.0 and not isup: dt -= 24.0

    return dt


def getHighestFrequency(subarray=None):
   """ Returns the highest sky frequency in GHz in the correlator setup """

   # Set subarray number
   if subarray == None: subarray = commands.subarrayNo

   # Set prefixes based on subarray
   # @FIXME This incorrectly assumes that Sci1 = SPECTRAL and Sci2=WIDEBAND.
   #        Need to use assignedCorrelator(subarrayNumber)
   #corrname = 'SpectralLine' 
   #bandname = 'Slc'
   #nbands = 8
   #sb = [1,2]  - unused!
   #if subarray == 2: 

   corDes = commands.assignedCorrelator(subArrayNo);
   corrname = [] 
   bandname = []
   nbands = []
   # by using 'XX' in corDes not using elif we 
   # automatically include 'SPECTRAL+WIDEBAND')
   if ( 'SPECTRAL' in corDes ):
      corrname.append('SpectralLine') 
      bandname.append('Slc')
      nbands.append(8)
   if ( 'WIDEBAND' in corDes ) :
      corrname.append('Wideband')
      bandname.append('Wbc')
      nbands.append(16)
   #@todo if ('C3GMAX8' in corDes) etc.
   else :
      raise Exception,'No correlator or unrecognized correlator associated with this subarray: %s' % corDes


   # Get LO frequency
   lofreq = commands.queryDouble('Control.Subarray%d.lofreq' % subarray)

   # Set sidebands based on LO frequency
   sidebands = [1,2]
   if lofreq < 45.0:
      sidebands = [2]
   #  This is still wrong.  Want one sideband we have 3.5m ants in array not
   #  sci# or correlator dependent.
   elif lofreq > 45.0 and subarray == 2:
      sidebands = [1]

   # Get frequencies in each band
   freq = []
   for j in range(0,len(corrname)) :
       for i in range(1,nbands[j]+1):
          mpValid = 'Control.%sCorrelator.%sBand%d.ControlBandPoints.online' % (corrname[j], bandname[j], i)
          if commands.queryBool(mpValid) == 1:
              for sb in sidebands:
                  mpFreq  = 'Control.%sCorrelator.%sBand%d.ControlBandPoints.Sideband%d.BandCenter.skyfreq' % (corrname[j], bandname[j], i, sb)
                  freq.append(commands.queryDouble(mpFreq))

   # Return maximum frequency
   return max(freq)

def inList(slist, name, caseSensitive=False):
    """ Return True/False if name is list slist 

        Inputs: slist - list of source names
                name  - scalar string 
                caseSensitive  - If true, search is case sensitive
    """
    # No match if slist is not set
    if slist == None or len(slist) == 0: return False

    # Makecopy of list
    t = makeListObsdef(slist)
    n = name

    # Convert to upper case
    if not caseSensitive:
        n = string.upper(n)
        for i in range(len(t)):
            t[i] = string.upper(t[i])

    # Match?
    found = n in t

    # Done
    return found


def list2string(mylist):
    """ Converts a list to string """
    slist = ''
    for l in mylist:
        if slist != '':  slist += ' '
        slist += str(l)
    return slist


def getLocalTime():
    """ Returns current time/date as a string """

    now = time.localtime(time.time())
    return time.asctime(now)


def getUTDay():
    """ Returns current UT day """

    ut = time.gmtime()
    ut = str(ut[0]) + '-' + str(ut[1]) + '-' + str(ut[2])
    return ut


def getUT(timestamp=False, sza=False, pointing=False):
    """ Returns current universal time

        If timestamp=True, then returns year-month-day UT (useful for snapshot)
        If sza=True,       then returns day-month-year:UT (useful for pacs)
        If pointing=True,  then returns daymonyy UT       (useful for pointing scripts)
        If all are false, then returns floating point value
    """


    x = time.gmtime()
    if timestamp:
        ut = time.strftime('%Y-%m-%d %H:%M:%S', x)
    elif sza:
        ut = time.strftime('%d-%b-%Y:%H:%M:%S', x).lower()
    elif pointing:
        z = float(x[3]) + float(x[4])/60.0 + float(x[5])/3600.0
        ut = time.strftime('%d%b%y', x).lower()
        ut = '%s %.6f' % (ut, z)
    else:
        ut = float(x[3]) + float(x[4])/60.0 + float(x[5])/3600.0

    return ut


def setPacsUT():
    """ Store starting pacs UT file """
    commands.s.setScriptString(INDX_STR_PACS_UT, getUT(sza=True))


def getLstRise(source, elmin=None):
    """ Return LST rise time as a string for 'source' in the current catalog

        Input : source - name of source in current catalog
                el     - If set, use specified elevation limit instead of 
                         current system elevation.

        Output: return rising LST as a string
    """
    # Get rising lst
    lst = None
    info = getSourceInfo(source,elmin=elmin).split('\n')[1]
    if info.find('NEVER RISES') >= 0: 
        lst = None
    elif info.find('NEVER SETS') < 0: 
        lst = info.split()[5]

    # Done
    return lst


def getLstSet(source, elmin=None):
    """ Return LST set time as a string for 'source' in the current catalog

        Input : source - name of source in current catalog
                elmin  - If set, use specified elevation limit instead of 
                         current system elevation.

        Output: return setting LST as a string
    """
    # Get setting lst
    lst = None
    info = getSourceInfo(source, elmin=elmin).split('\n')[1]
    if info.find('NEVER RISES') >= 0: 
        lst = None
    elif info.find('NEVER SETS') < 0: 
        lst = info.split()[6]

    # Done
    return lst


def isMosaic(mosaic):
    """ Indicates True/False if "mosaic" list is anywhere true"""

    # Make it into a list
    l = makeListObsdef(mosaic)

    # Is true present?
    ntrue = l.count(True)

    # Done
    return ntrue > 0

def makeListObsdef(sources, parse=True):
    """ Convert the variable "sources" into a list.

        Input : sources - a variable, list, or dictionary (key=>boolean)
                parse   - if True, then parse elements as comma/spaced strings
        Output: Return value is a list.
                If input is a single value, then output is [sources]
                If input is a list, the result is copied
                If input is a dictionary, all of the True values are returned

        Examples:
             (1) l = makeListObsdef('3c273')
             (2) l = makeListObsdef(['3c273','3c279'])
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

    # Parse string
    if parse:
        t = list()
        for z in l: 
             if str in [type(z)]: 
                 t.extend(z.replace(',', ' ').split())
             else:
                 t.append(z)
        l = t[:]
    return l[:]


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
        l = makeListObsdef(sources)
        for name in l:
            d[name] = True
    return d.copy()


def expandList(origList, refList):
    """ Expands origList to the same length as refList

        Inputs:
           origList: original list
           refList : reference list

        If the size of origList is less than refList, then origList
        is padded until the sizes match. The last argument in
        origList is repeated as necessary.
    """

    newList = makeListObsdef(origList)
    if refList <> None and len(newList) < len(refList):
        n = len(refList) - len(newList)
        newList.extend([newList[len(newList)-1]] * n )

    return newList[:]



def verifyCarmaSetup():
    """ Returns True/False if correlator and antennas are setup for CARMA 23 mode """

    # Check antennas
    n35 = np.sum(np.array(commands.currentAntennaNumbers()) > 15)
    isCarma23Antennas = (n35 > 0)

    # Are we in 23 correlator mode?
    isCarma23Correlator = isCarma23Mode()

    return (isCarma23Antennas and isCarma23Correlator) or (not isCarma23Antennas and not isCarma23Correlator)


def isCarma23Mode():
    """ Returns True/False if _correlator_ is setup for CARMA 23 mode """

    return getCorrelatorMode()[0]

def isDualPol( astroband ):
    """ Determine if this astroband is dual pol or not.
        This routine uses the signal path mapping to 
        programatically determine if we are in a dual pol configuration.
        It does not use configuration name.
    """
    mpBaseStr = "SignalPath.Mapping.Astroband%d"%astroband
    nInputs = commands.queryInt( "%s.nInput"%mpBaseStr )
    polMpStrs  = [ "%s.Input%d.polarization"%(mpBaseStr,input) for input in range( 1, nInputs + 1 ) ]
    pols = set()
    for polMpStr in polMpStrs:
        pols.add( commands.queryString( polMpStr ) )
    pols -= set( ['UNKNOWN'] )
    if len( pols ) >= 2:
        return True
    else:
        return False
   
def getCurrentAstrobands():
    """ Get current astrobands for this subarray. """
    spmSaNoMpName = "SignalPath.Mapping.Astroband%d.subarrayNo"
    spmConfMpName = "SignalPath.Mapping.Astroband%d.confName"
    spmSaNoMpList = [ spmSaNoMpName%astroband for astroband in range(1,41) ]
    spmConfMpList = [ spmConfMpName%astroband for astroband in range(1,41) ]
    spmSaNoMps = commands.queryMonitorPoint( spmSaNoMpList )
    spmConfMps = [commands.queryString(spmConfMp,6) for spmConfMp in spmConfMpList ]
    astrobands = [abi + 1 for abi in range(0,40) if spmSaNoMps[abi].isValid() 
                  and spmSaNoMps[ abi ].value() == commands.subarrayNo
                  and spmConfMps[ abi ] != "NONE" ]
    return astrobands[:]


def getCorrelatorMode():
    """ Returns correlator configuration state.
            The first return value is True/False if any astroband is CARMA23 mode.
            The second return value is the list of online astrobands for this subarray.
    """

    # Set bands depending on subarray number
    astrobands = getCurrentAstrobands()

    # See if signal path for any band is CARMA23
    isCarma23 = False
    for ab in astrobands:
        # Construct monitor point
        mp = 'SignalPath.Mapping.Astroband%d.confName' % ab
        mode = commands.queryString(mp)
        if mode.find('CARMA23') > 0:
           isCarma23 = True
           break

    # Done
    return isCarma23, astrobands[:]


def getConfigBand(slcbands=None):
    """ Deprecated function """
    return getConfigband(slcbands)

def setConfigBand(allConfigP):
    """ Deprecated function """
    setConfigband(allConfigP)


def getConfigband(slcbands=None):
    """Read the current correlator configuration for later restoration."""

    # Initialize
    allConfigP = []

    # Set bands and monitor points
    offset = 0
    if commands.subarrayNo == 1:
       baseString = 'Control.SpectralLineCorrelator.SlcBand'
       if slcbands == None: 
          slcbands = range(1,9)
          if isCarma23Mode(): slcbands = [1, 3, 5, 7]
    elif commands.subarrayNo == 2:
       baseString = 'Control.WidebandCorrelator.Wbcband'
       if slcbands == None: 
           offset = 8
           slcbands = range(1,17)
    else:
       raise Exception, 'Invalid subarray number (%d)' % commands.subarrayNo

    # Loop over bands
    for i in slcbands :
        # Set monitor point names
        bandConfig = []
        bandString = ('%s%i.ControlBandPoints' % (baseString,i) )

        # Read monitor points
        bw          = commands.queryDouble('%s.bandwidth' % bandString)
        sb2Str      = commands.queryString('%s.lo2Sideband' % bandString)
        fcenter     = commands.queryDouble('%s.centerFreq' % bandString)
        frest       = commands.queryDouble('%s.restFreq' % bandString)
        imagefrest  = commands.queryDouble('%s.imageRestFreq' % bandString)
        corrbits    = commands.queryInt('%s.corrBits' % bandString)
        fpgamode    = commands.queryInt('%s.fpgaMode' % bandString)
        pol         = commands.queryInt('%s.blockDCpolarization' % bandString)
        decimate    = commands.queryBool('%s.decimation' %bandString)
        confName    = commands.queryString('SignalPath.Mapping.Astroband%d.confName' % (i+offset))

        # Set correlator bits
        if   corrbits == 0: corrbits = commands.CORR_2BIT
        elif corrbits == 1: corrbits = commands.CORR_3BIT
        elif corrbits == 2: corrbits = commands.CORR_4BIT
        else:
            raise Exception, 'Cannot read correlator bits in refpoint'

        # USB, BW500, etc are "instances", effectively variables, not strings
        if sb2Str   == 'LOWER' :
           sb2 = commands.LSB
        elif sb2Str == 'UPPER' :
           sb2 = commands.USB
        elif sb2Str == 'AUTO' :
           sb2 = commands.AUTO
        else:
           raise Exception, 'Error reading sideband string in getConfigband'

        # Set bandwidth
        if   abs(bw - 500)  < 1 : bw = commands.BW500
        elif abs(bw - 250.) < 1 : bw = commands.BW250
        elif abs(bw - 125.) < 1 : bw = commands.BW125
        elif abs(bw - 62.5) < 1 : bw = commands.BW62
        elif abs(bw - 31.3) < 1 : bw = commands.BW31
        elif abs(bw - 7.8)  < 1 : bw = commands.BW8
        elif abs(bw - 2.0)  < 1 : bw = commands.BW2
        else:
            raise Exception, 'Cannot read correlator bandwidth in refpoint: bw=%.2f' % bw

        # Save
        bandConfig = [i, confName, bw, fcenter, sb2, frest, imagefrest, \
                corrbits, decimate]
        allConfigP.append(bandConfig)

    # Done
    return allConfigP

def getConfigAstroband(astrobands=None):
    """Read the current astroband configuration for later restoration.
    This routine varies from getConfigband since it does not assume a single
    correlator per subarray."""
    
    retries = 3  # For queries

    # Initialize
    allConfigP = []

    if astrobands == None:
        astrobands = getCurrentAstrobands()

    # Loop over bands
    for ab in astrobands:

        corrDesStr = commands.astrobandCorrelator(ab);

        # Set bands and monitor points
        if corrDesStr == 'SPECTRAL':
           baseString = 'Control.SpectralLineCorrelator.SlcBand'
           corrband = ab 
        elif corrDesStr == 'WIDEBAND':
           baseString = 'Control.WidebandCorrelator.Wbcband'
           corrband = ab - 8
        elif corrDesStr == 'C3GMAX8':
           baseString = 'Control.C3gMax8Correlator.C3gMax8Band'
           corrband = ab - 24
        elif corrDesStr == 'C3GMAX23':
           baseString = 'Control.C3gMax23Correlator.C3gMax23Band'
           corrband = ab - 32
        else:
           raise Exception, 'Invalid astroband (%d) correlator designation (%s)'%(ab, corrDesStr)


        # Set monitor point names
        astroBandConfig = []
        bandString = ('%s%i.ControlBandPoints' % (baseString,corrband) )

        # Read monitor points
        bw          = commands.queryDouble('%s.bandwidth' %bandString, retries)
        sb2Str      = commands.queryString('%s.lo2Sideband' %bandString, retries)
        fcenter     = commands.queryDouble('%s.centerFreq' %bandString, retries)
        frest       = commands.queryDouble('%s.restFreq' %bandString, retries)
        imagefrest  = commands.queryDouble('%s.imageRestFreq' %bandString, retries)
        corrbits    = commands.queryInt('%s.corrBits' %bandString, retries)
        fpgamode    = commands.queryInt('%s.fpgaMode' %bandString, retries)
        pol         = commands.queryInt('%s.blockDCpolarization' %bandString, retries)
        decimate    = commands.queryBool('%s.decimation' %bandString, retries)
        confNameMP  = 'SignalPath.Mapping.Astroband%d.confName' % ab
        confName    = commands.queryString(confNameMP, retries)

        # Set correlator bits
        if   corrbits == 0: corrbits = commands.CORR_2BIT
        elif corrbits == 1: corrbits = commands.CORR_3BIT
        elif corrbits == 2: corrbits = commands.CORR_4BIT
        else:
            raise Exception, 'Cannot read correlator bits in getConfigAstroband'

        # USB, BW500, etc are "instances", effectively variables, not strings
        if sb2Str   == 'LOWER' :
           sb2 = commands.LSB
        elif sb2Str == 'UPPER' :
           sb2 = commands.USB
        elif sb2Str == 'AUTO' :
           sb2 = commands.AUTO
        else:
           raise Exception, 'Error reading sideband string in getConfigband'

        # Set bandwidth
        if   abs(bw - 500)  < 1 : bw = commands.BW500
        elif abs(bw - 250.) < 1 : bw = commands.BW250
        elif abs(bw - 125.) < 1 : bw = commands.BW125
        elif abs(bw - 62.5) < 1 : bw = commands.BW62
        elif abs(bw - 31.3) < 1 : bw = commands.BW31
        elif abs(bw - 7.8)  < 1 : bw = commands.BW8
        elif abs(bw - 2.0)  < 1 : bw = commands.BW2
        else:
            raise Exception, 'Cannot read correlator bandwidth in refpoint: bw=%.2f' % bw

        # Save
        astroBandConfig = [ab, confName, bw, fcenter, sb2, frest, \
            imagefrest, corrbits, decimate]
        allConfigP.append(astroBandConfig)

    # Done
    return allConfigP


def setConfigband(allConfigP):
   """Call configband to set the correlator to a stored configuration."""

   # Clear bands
   commands.clearastroband(0)

   # Configure bands
   for i in range(len(allConfigP)):
       b = allConfigP[i]
       if b[1] != 'NONE':
           commands.configastroband( b[0], b[1], b[2], b[3], sb2=b[4], \
                                     frest=b[5], imagefrest=b[6], \
                                     bits=b[7], decimate=b[8])
   commands.tsys(ifsetup=True)
   commands.optimizeThresholds()
   commands.flattenPhases()


def getDistance(source, ref):
    """Return angular distance in degrees in the coordinates between a ref and source"""

    sref = makeListObsdef(ref)[0]
    deg2rad = 180.0/math.pi;

    srcRa   = helpers.convertHms(getSourceRa(source))*15.0/deg2rad;
    srcDec  = helpers.convertHms(getSourceDec(source))/deg2rad;

    refRa   = helpers.convertHms(getSourceRa(sref))*15.0/deg2rad;
    refDec  = helpers.convertHms(getSourceDec(sref))/deg2rad;

    cosDist = math.sin(refDec)*math.sin(srcDec) + math.cos(refDec)*math.cos(srcDec)*math.cos(refRa-srcRa);
    cosDist = min(cosDist,  1.0)  # Checks for roundoff error
    cosDist = max(cosDist, -1.0)  # Checks for roundoff error
    dist    = math.acos(cosDist)*deg2rad;
    return  float(dist);


def getDistanceSky(source, ref, azo=None, elo=None):
    """Return angular distance in degrees between a ref and source.

       If azo and elo are specified, then they are used in placed of ref.

       The distance is "on the sky" and does dot include any cos(el) term.
    """

    deg2rad = 180.0/math.pi;

    [srcAz, srcEl] = commands.azel(source)
    if azo == None or elo == None:
       sref = makeListObsdef(ref)[0]
       [refAz, refEl] = commands.azel(sref)
    else:
       [refAz, refEl] = [azo, elo]

    deltaAz = srcAz - refAz
    deltaEl = srcEl - refEl
    if deltaAz >= 360.0 : deltaAz -= 360.0
    dist = math.sqrt(deltaAz*deltaAz + deltaEl*deltaEl)

    return  float(dist);


def getElevationLimit():
    """ Return the current elevation limit in degrees for the subarray """
    el_limit_old = commands.s.getScriptDouble(INDX_DBL_EL_LIMIT)
    try:
       el_limit = commands.queryDouble(_controlMPprefix() + 'elevLimit')
       commands.s.setScriptDouble(INDX_DBL_EL_LIMIT, el_limit)
    except Exception:
       commands.trackMessage('Error reading elevation limit - using previous value')
       el_limit = el_limit_old
    return el_limit


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
    dt = helpers.convertHms(t2) - helpers.convertHms(t1)
    if pos :
        if dt < 0.0: dt += 24.0
    elif dt < 0.0 and abs(dt + 24.0) < abs(dt):
        dt += 24.0
    elif dt > 0.0 and abs(dt - 24.0) < abs(dt):
        dt -= 24.0

    # Done
    return dt


def getBrightSources(fluxLimit=4.0, freq=95.0, slope=-0.40, returnFluxes=False):
    """ Retrieves sources from the flux catalog brighter than the
        specified flux limit in Janskys.

        Inputs: fluxLimit - flux limit in Janskys
                freq      - scale fluxes to specified frequency in GHz
                slope     - assumed spectral slope (Flux ~ nu^slope)
                returnFluxes - If True, then return source name and flux names.

        Output: A python list of source names.
                If returnFluxes = True, then a dictionary is returned 
                where the keys are the source names, and the values are
                the fluxes in Janskys.
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
    brightSourcesDict = dict()
    sourceName = ''
    sumflux = dict()
    n       = dict()
    for i in [1,3]:
        sumflux[i] = 0.0
        n[i]       = 0
    for z in a:
        # Skip comments ("#")
        if z[0] == '#' or z[0] == "#|" or len(z) == 1: continue

        # If ##, this is a new source
        if z[0] == '##':
            fluxFrom1mm = 0.0
            fluxFrom3mm = 0.0
            if n[3] > 0: fluxFrom3mm = sumflux[3] / n[3]
            if n[1] > 0: fluxFrom1mm = sumflux[1] / n[1]
            flux = max(fluxFrom3mm, fluxFrom1mm)
            if freq > 150.0 and n[1] > 0:
                flux = fluxFrom1mm
            elif freq < 150.0 and n[3] > 0:
                flux = fluxFrom3mm
            if flux > 0 and sourceName != '' and flux >= fluxLimit:
                brightSources.append(sourceName)
                brightSourcesDict[sourceName] = flux
            for i in sumflux:
               n[i]       = 0
               sumflux[i] = 0.0
            sourceName = z[1]

            # Read aliases, and find which alias is actually in the catalog
            if z[0] == '##':
                aliases = z[1:]
                aliasesGood = list()
                for name in aliases:
                    if string.upper(name) in sources: 
                        sourceName = name
                        aliasesGood.append(name)
        else:
            # Read frequency/observatory
            if len(z) < 6: continue
            ofreq = float(z[2])
            obs  = z[5]

            # Determine time (in days) since current date
            tobs = time.strptime(z[1][0:11], '%Y-%b-%d')

            # Is this a valid flux?
            if ofreq < 60.0 or ofreq > 300.0 or tobs[0] < 2006: continue

            # Sum
            ifreq = 3
            if ofreq > 150.0: ifreq = 1
            flux = float(z[3]) * (freq / ofreq) ** slope
            # Sum fluxes
            # sumflux[ifreq] += flux
            # n[ifreq] += 1
            sumflux[ifreq] = flux
            n[ifreq] = 1

            # Save most recent source name
            if string.upper(z[0]) in aliasesGood: sourceName = z[0]

    # Add last source
    fluxFrom1mm = 0.0
    fluxFrom3mm = 0.0
    if n[3] > 0: fluxFrom3mm = sumflux[3] / n[3]
    if n[1] > 0: fluxFrom1mm = sumflux[1] / n[1]
    flux = max(fluxFrom3mm, fluxFrom1mm)
    if freq > 150.0 and n[1] > 0:
        flux = fluxFrom1mm
    elif freq < 150.0 and n[3] > 0:
        flux = fluxFrom3mm
    if flux > 0 and sourceName != '' and flux >= fluxLimit:
        brightSources.append(sourceName)
        brightSourcesDict[sourceName] = flux

    # Return list
    if returnFluxes:
       return brightSourcesDict
    else:
       return brightSources[:]


def getSourceFlux(source, freq=95.0, slope=-0.40):
    """ Returns source flux in Janskys 

        Inputs: source - source name
                freq   - frequency in GHz
                slope  - change in source flux with frequency

        This function assumes the flux varies with frequency as nu^slope.
    """

    # Read the flux file. If files does not exist, then return 0.0
    fluxFile = '/opt/rt/conf/catalogs/FluxSource.cat'
    if not os.path.exists(fluxFile): return 0.0
    a = fileIOPython.fileToTable(fluxFile,ignoreEmpty=True)

    # Get current date
    tcurrent = time.strptime(time.ctime())

    # Select only recent epoch data
    fluxband = dict()
    dtobs = dict()
    for i in [1,3]: fluxband[i] = 0.0
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
            if len(z) < 6: continue
            ofreq = float(z[2])
            obs  = z[5]

            # Determine time (in days) since current date
            tobs = time.strptime(z[1][0:11], '%Y-%b-%d')
            dt = abs(time.mktime(tcurrent) - time.mktime(tobs)) / 86400.0

            # Must be 3mm or 1mm, and must be recent
            #if ofreq < 60.0 or ofreq > 300.0 or obs.find('CARMA') != 0: continue
            if ofreq < 60.0 or ofreq > 300.0 or tobs[0] < 2006: continue

            # Store most recent flux
            ifreq = 3
            if ofreq > 150.0: ifreq = 1
            flux = float(z[3]) * (freq / ofreq) ** slope
            save = (not dtobs.has_key(ifreq)) or (dt <= dtobs[ifreq])
            if save: 
                fluxband[ifreq] = flux
                dtobs[ifreq] = dt

    # Get appropriate flux
    flux = max(fluxband[1], fluxband[3])
    if freq < 150.0 and fluxband[3] > 0:
        flux = fluxband[3]
    elif freq > 150.0 and fluxband[1] > 0:
        flux = fluxband[1]

    # Return flux
    return flux


def getSource(sources, preferred=None, elmin=None, elmax=None, ref=None,
              maxsep=None, exclude=None, excludeExpref=None, 
              timeAvailable=None, minsepsun=None, sky=True, 
              allowShadowing=True, allowShadowingPreferred=None,
              diameterFraction=0.0):
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
                elmin     - Minimum elevation that overrides system elevation
                elmax     - Maximum elevation
                ref       - reference source
                maxsep    - Maximum separation in degrees for source
                exclude   - List of any sources to exclude
                excludeExpref  - List of any sources to exclude UNLESS they
                                 are specified in the preferred list
                timeAvailable  - minimum time (in minutes) the source must be
                                 available
                minsepsun - minimum allow separation from sun in degrees.
                            Only works for sci2.
                sky       - Compute separation based on az/el differences.
                            Otherwise, compute based on angular separation
                            on the sky. Default = True.
                allowShadowing - If True, then permit the source to be shadowed
                allowShadowingPreferred - If True, then permit the preferred sources to be shadowed
                                          Default value is None, which means use the same value as
                                          allowShadowing.
                diameterFraction - Amount of shadowing to tolerate in terms
                                   of the fraction of the diameter of an
                                   antenna.
 
        Output: string variable containing the source name meeting the above
                criteria.
    """
    # Check the preferred source list first to see if one is available
    if preferred <> None:
        # Make list of preferred sources
        sou = makeListObsdef(preferred)

        # Check preferred sources
        asp = allowShadowingPreferred
        if asp == None: asp = allowShadowing
        for n in sou:
            name = getSource(n, elmin=elmin, elmax=elmax, ref=ref, 
                             maxsep=maxsep, exclude=exclude, 
                             timeAvailable=timeAvailable, minsepsun=minsepsun,
                             allowShadowing=asp,
                             diameterFraction=diameterFraction)
            if name <> None: return name

    # Convert sourceDict into list or dictionary if needed
    # I do not automatically convert it to a dictionary since I assume
    # any lists are in priority order.
    d = sources
    if list not in [type(sources)] and dict not in [type(sources)]:
        d = makeListObsdef(sources)

    # Get the reference source
    refSource = makeListObsdef(ref)
    if refSource <> None: refSource = refSource[0]

    # Initialize
    if elmin == None: elmin = getElevationLimit()
    sepChosenSource  = 1e35
    elChosenSource   = elmin
    nameChosenSource = None

    # Make combined exclude list
    excludeAll = exclude
    if excludeExpref <> None:
        if excludeAll == None:
            excludeAll = excludeExpref
        else:
            excludeAll.extend(excludeExpref)
        
    # Check source list for either:
    #     1) source nearest in time if ref!=None
    # OR
    #     2) highest elevation source if there is no reference source
    # The order of the different critera are deliberate. The most inexpesive calculations
    # are done first, and the most expensive calculations last
    isDictionary = dict in [type(d)]
    for name in d:
        # If "d" is a dictionary, see if we should use this source
        if isDictionary and not d[name]: continue

        # Get current source elevation (and check if source is in catalog)
        # This is a time consuming step, but it is unavoidable.
        try:
           elSource = getSourceElevation(name)
        except:
           continue

        # Make sure source is not in exclude list
        if inList(excludeAll,name): continue

        # Check elevation limits
        if elSource < elmin or (elmax != None and elSource > elmax): continue

        # Check reference source
        if refSource <> None:
            # Compute separation
            if sky:
               sep = getDistanceSky(name,refSource)
            else:
               sep = getDistance(name,refSource)

            # Check separation
            if maxsep <> None and sep > maxsep: continue
            if sep > sepChosenSource: continue
        elif elSource < elChosenSource:
            continue

        # Check distance from sun
        if minsepsun <> None and minsepsun > 0 and getDistance(name, 'sun') < minsepsun: continue

        # Check time available
        if timeAvailable <> None and getTimeToSet(name, elmin=elmin) < timeAvailable/60.0: continue

        # Check shadowing
        if not allowShadowing and isShadowed(name, lstHours=commands.lst()+5.0/60.0, diameterFraction=diameterFraction): continue

        # This source meets all of the criteria
        nameChosenSource = name
        elChosenSource = elSource
        if ref <> None: sepChosenSource = sep
    
    # Done
    return nameChosenSource


def getObsblockName():
    """ Returns current obsblock name """
    obsblockName = 'Unknown'
    if commands.subarrayNo < 3: 
        obsblockName = commands.queryString(_controlMPprefix() + 'obsBlockId')
    return obsblockName
    

def _controlMPprefix(subarray=None) :
    """Returns a string with the prefix for monitor points in the 
    current control subarray, including the trailing dot."""

    # Set subarray number
    if subarray == None:
       subarray = commands.subarrayNo

    return 'Control.Subarray%s.' % subarray


def getLstEndTrack(string=False):
    """ Returns LST end time (hours) for the current track """
    t = None
    if commands.s.getScriptBool(INDX_BOOL_LST_END_TRACK):
        t = commands.s.getScriptDouble(INDX_DBL_LST_END_TRACK)
    if string : t = helpers.convertHmsString(t)
    return t


def getLstLastPoint(string=False):
    """ Returns LST time (hours) of last radio pointing observation """
    t = None
    if commands.s.getScriptBool(INDX_BOOL_LASTPOINT):
        t = commands.s.getScriptDouble(INDX_DBL_LASTPOINT)
    if string : t = helpers.convertHmsString(t)
    return t


def getLstLastFluxcal(string=False):
    """ Returns LST time (hours) of flux cal observation """
    t = None
    if commands.s.getScriptBool(INDX_BOOL_LASTFLUX):
        t = commands.s.getScriptDouble(INDX_DBL_LASTFLUX)
    if string : t = helpers.convertHmsString(t)
    return t


def getLstLastPolarization(string=False):
    """ Returns LST time (hours) of polarization observation """
    t = None
    if commands.s.getScriptBool(INDX_BOOL_LASTPOLARIZATION):
        t = commands.s.getScriptDouble(INDX_DBL_LASTPOLARIZATION)
    if string : t = helpers.convertHmsString(t)
    return t


def getLstLastPassband(string=False):
    """ Returns LST time (hours) of passband observation """
    t = None
    if commands.s.getScriptBool(INDX_BOOL_LASTPASSBAND):
        t = commands.s.getScriptDouble(INDX_DBL_LASTPASSBAND)
    if string : t = helpers.convertHmsString(t)
    return t


def getLastMosaicPosition(startpos=1):
    """ Returns last observed mosaic position, where 1 is the first
        position in the mosaic list.
    """
    lastpos = commands.s.getScriptInt(INDX_INT_MOSPOS)
    if lastpos < 1: lastpos = startpos - 1
    return lastpos


def addSourceTime(dt):
    """ Add dt hours to time spent in source/phase-cal cycle  """
    t = commands.s.getScriptDouble(INDX_DBL_TSOURCE) + dt
    commands.s.setScriptDouble(INDX_DBL_TSOURCE,t)


def getLstStartTrack():
    """ Return LST starting time (hours) for the current track """
    t = None
    if commands.s.getScriptBool(INDX_BOOL_LST_START_TRACK):
        t = commands.s.getScriptDouble(INDX_DBL_LST_START_TRACK)
    return t


def getLstStartCycle(default=None, string=False):
    """ Returns LST start time (hours) for the current track """
    t = default
    if commands.s.getScriptBool(INDX_BOOL_LST_START_CYCLE):
        t = commands.s.getScriptDouble(INDX_DBL_LST_START_CYCLE)
    if string: t = helpers.convertHmsString(t)
    return t


def getLstStopCycle(default=None, string=False):
    """ Returns LST stop time (hours) for the current track """
    t = default
    if commands.s.getScriptBool(INDX_BOOL_LST_STOP_CYCLE):
        t = commands.s.getScriptDouble(INDX_DBL_LST_STOP_CYCLE)
    if string : t = helpers.convertHmsString(t)
    return t


def timeRemaining(source=None,   phase=None,   lsttime=None,
                  useTime=False, default=999., toRise=False, 
                  elmin=None,    tneed=None):
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
                 tneed    - time in hours to complete remaining calibrator 
                            observations
        Output : remaining time in hours

        Example: dt = timeRemaining(source='3c273',useTime=False)
    """
    # Initialize
    tremaining = None
    currentLst = commands.lst()

    # time
    if useTime:
        # tmax
        tmax = getTmax()
        if tmax is not None:
            dt = tmax - timeDifference(currentLst, getLstStartTrack(), pos=True)
            if tremaining <> None: 
                tremaining = min(tremaining, dt) 
            else: 
                tremaining = dt

        # endtrack
        lend = getLstEndTrack()
        if lend is not None:
            dt = timeDifference(lend, currentLst)
            if tremaining <> None: 
                tremaining = min(tremaining, dt) 
            else: 
                tremaining = dt

        # Subtract time needed
        if tremaining <> None and tneed <> None: tremaining -= tneed

    # source and phase calibrator
    if source <> None or phase <> None:
        # Phase calibrator. Could be matrix of sources/times. 
        # Pick only first source.
        dtphase = None
        isUpPhase = False
        if phase <> None:
            phaseName = makeListObsdef(phase)[0]
            isUpPhase = isSourceUp(phaseName, elmin=elmin)
            if toRise:
                dtphase = getTimeToRise(phaseName, elmin=elmin, isup=isUpPhase)
            else:
                dtphase = getTimeToSet(phaseName, elmin=elmin, isup=isUpPhase)

        # Sources (could be a list)
        dtsource = None
        isUpSource = False
        if source <> None:
            names = makeListObsdef(source)
            for n in names:
                # Time difference for this source
                dt = None
                isUpSource = isUpSource or isSourceUp(n, elmin=elmin)
                if toRise:
                    dt = getTimeToRise(n, elmin=elmin, isup=isUpSource)
                else:
                    dt = getTimeToSet(n, elmin=elmin, isup=isUpSource)

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
        if tcombined <> None:
            if tremaining <> None: 
                tremaining = min(tremaining, tcombined) 
            else: 
                tremaining = tcombined

    # LST time
    if lsttime <> None:
        dt = timeDifference(helpers.convertHms(lsttime), currentLst)
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


def getNreps(tintMinutes, trecordSeconds, tsysMinutes=None, tsampleMinutes=None,
             tintSinceLastTsys=None, tintSinceLastSample=None):
    """ Determine number of cycles and repeats needed to observe a source for
        tintMinutes, with a record time of trecordSeconds, and measuring 
        the system temperature every tsysMinutes, or a sampling time of
        every tsampleMinutes.

        Inputs: tintMinutes    - total desired integration time in minutes
                trecordSeconds - record time in seconds
                tsysMinutes    - how frequently to measure tsys
                tsampleMinutes - Maximum integration time for a rep
                tintSinceLastTsys - integration time in minutes since  
                                    tsys was last measured
                tintSinceLastSample - integration time in minutes since  
                                    sample was last taken

        Output: The number of repeats per cycle. 
        
        Notes:
             1) There are two time scales (tsys and sample), which may
                not both be satisifed. The smaller in absolute value
                of tsysMinutes and sampleMinutes is used.

             2) If tsysMinutes=None and tsampleMinutes=None, then getNreps 
                returns and an integer, else it is a integer list with one 
                value per cycle.

             3) Number of repeats are rounded to the nearest integer.
                It tintMinutes > 0, then there will be a minimum of one 
                repeat per cycle.

             4) It tsysMinutes < 0 and tintSinceLastTsys > abs(tsysMinutes),
                the first nrep is set to zero. Similarly, if tsampleMinutes > 0
                and tsampleMinutes > tintSinceLastMinutes, then first nrep=0.

        Examples:
              (1) print getNreps(10.0,30.0)      # 20 repeats, 1 cycle
              (3) print getNreps(10.0,30.0,5.0)  # 10 repeats, 2 cycles
              (2) print getNreps(0.75,30.0)      #  2 repeats, 1 cycle
              (3) print getNreps(0.70,30.0)      #  1 repeats, 1 cycle
    """
    # Debug message
    commands.scriptlog('DEBUG GETNREPS: tintMinutes=%s trecordSeconds=%s tsysMinutes=%s tsampleMinutes=%s tintSinceLastTsys=%s tintSinceLastSample=%s' % (str(tintMinutes), str(trecordSeconds), str(tsysMinutes), str(tsampleMinutes), str(tintSinceLastTsys), str(tintSinceLastSample)))

    # Tminutes must be positive
    if tintMinutes == None or tintMinutes <= 0.0: return 0

    # set number of cycles and integration time per cycle
    # If tsys time is not specified, integration is done in one cycle
    ncycles = 1
    tintCycle = [float(tintMinutes)]
    tsam = None
    if tsampleMinutes <> None and tsampleMinutes != 0: 
        tsam = tsampleMinutes
    if tsysMinutes <> None and tsysMinutes != 0:
        if tsam == None:
            tsam = abs(tsysMinutes)
        else: 
            tsam = min(tsampleMinutes, abs(tsysMinutes))

    if tsam <> None: 
        # tpart contains the integration time for two parts:
        #     Part 1: Integration time before a tsys or tsample is needed. 
        #             This is != None only if 
        #             (tsysMinutes < 0 and tlastTsys != None) or
        #             (tsampleMinutes <> None and tintSinceLastSample >? None)
        #     Part 2: Integration time after the non-tsys or non-tsample 
        #             data are obtained.
        tsam = max(tsam, trecordSeconds/60.0)
        tpart = [None, tintCycle[0]]
        tref = None
        if tsysMinutes < 0.0 and tintSinceLastTsys <> None:
            tref = tintSinceLastTsys
        if tsampleMinutes <> None and tintSinceLastSample <> None:
            if tref == None:
                tref = tintSinceLastSample
            else:
                tref = min(tref, tintSinceLastSample)
        if tref <> None:
            timeBeforeNext = tsam - tref
            tpart[0] = max(0.0, timeBeforeNext)
            tpart[1] = max(0.0, tintCycle[0] - tpart[0])

        # Number of cycles for each part
        ncycles = [None, None]
        for i in range(len(tpart)):
            if tpart[i] <> None:
                rcycles    = float(tpart[i]) / tsam
                ncycles[i] = int(rcycles)
                if rcycles != ncycles[i]: ncycles[i] += 1

        # Set integration time per cycle
        tintCycle = [tsam] * ncycles[1]
        if ncycles[0] <> None: tintCycle.insert(0,tpart[0])
        tintCycle[-1] = tintMinutes - sum(tintCycle[:-1])

    # Set number of reps per cycle
    nreps = list()
    for t in tintCycle:
        n = max(1, round(t * 60.0 / trecordSeconds))
        if t <= 0: 
           n = 0
        elif t - (trecordSeconds*n/60.0) > 0.01:
           n+=1
        nreps.append(int(n))

    # Because nreps is set to a minimum value of 1, we may have exceeded the
    # total integration time. Check that here and make any adjustments
    textra = tintMinutes - sum(nreps) * trecordSeconds / 60.0
    if textra > trecordSeconds:
        nextra = int(textra / trecordSeconds)
        nreps[-1] -= nextra

    # If tsys and tsample are none, then return a scalar
    if tsampleMinutes == None and tsysMinutes == None: nreps = nreps[0]

    # Return number of repetitions
    return nreps


def getSourceName(sources, n=None, parse=True):
    """ Retrieve the nth source name from sources.

        Function takes into account whether sources is a scalar or list.
        The first source corresponds to n = 1.
    """

    l = makeListObsdef(sources, parse=parse)
    if n <> None and n > len(l):
        raise Exception, 'Error trying to read source name'
    if n == None: n = 1
    return l[n-1]


def getVariable(vdict, key, defaultValue, nonone=False):
    """ Returns the value for "key" in the dictionary vdict if it exists.
        Otherwise, returns defaultValue.

        If nonone == True and vdict[key] = None, then the default value is
        returned as well.
    """
    value = defaultValue
    if vdict.has_key(key): 
        if not (vdict[key] == None and nonone): value = vdict[key]

    return value


def splitPhaseCalList(phase_orig):
    phase_new  = list()
    for i in range(len(phase_orig)):
        # Get start/stop time
        p = phase_orig[i]
        pname, pstart, pstop, pcaltime = \
            getPhaseCal(p, check=False, returnCaltime=True)

        # Check times
        if pstop == None or pstart == None:
           phase_new.append(p)
        else:
            # Compute time difference
            dt = helpers.convertHms(pstop) - helpers.convertHms(pstart)
            if dt < 0.0: dt += 24.0

            # If time difference is more than 12 hours, then split
            if dt <= 12.0:
                phase_new.append(p)
            else:
                # Convert times to real numbers
                t1 = helpers.convertHms(pstart)
                t2 = helpers.convertHms(pstop)

                # If this is the first calibrator, then break off
                # time at the beginning. Use the end for the second
                # calibrator.
                if i == 0:
                    # Set second time
                    tstop2  = t2
                    tstart2 = tstop2 - helpers.convertHms('11:59:50')
                    if tstart2 <= 0.0: tstart2 += 24.0

                    # Set first time
                    tstop1  = tstart2 + 0.5
                    tstart1 = t1
                    if tstop1 >= 24.0: tstop1 -= 24.0
                else:
                    # Set first time
                    tstart1 = t1
                    tstop1  = tstart1 + helpers.convertHms('11:59:50')
                    if tstop1 >= 24.0: tstop1 -= 24.0

                    # Set second time
                    tstart2 = tstop1 - 0.5
                    tstop2  = t2
                    if tstart2 < 0.0: tstart2 += 24.0
                extra = ''
                if pcaltime <> None:
                    extra += ' t=' + str(pcaltime)
                p1 = pname + ' ' + helpers.convertHmsString(tstart1) + ' ' + helpers.convertHmsString(tstop1) + extra
                p2 = pname + ' ' + helpers.convertHmsString(tstart2) + ' ' + helpers.convertHmsString(tstop2) + extra
                p = [p1, p2]
                phase_new.extend(p)
    return phase_new


def getPhaseCal(phaseCal, n=None, check=True, returnCaltime=False, split=False):
    """ Returns source name, starting lst, and ending LST for phase calibrator.

        Inputs: phaseCal - scalar or list of phase calibrators
                n        - The number of the calibrator to retrieve.
                           The first calibrator is n=1.
                check    - If True, then check stop-start time difference
                           is less than 12 hours.
                split    - If True, then split calibrators that are up for
                           more than 12 hours
                returnCaltime - If True, then return phase calibrator name.
                           Default is False for backwards compatability.

        Output  phaseName, startlst, stoplst, caltime
                caltime is in minutes
    """
    # If there is no phase calibrator, return None
    if phaseCal == None: 
       if returnCaltime:
           return None, None, None, None
       else:
           return None, None, None

    # Initialize
    pname    = None
    lststart = None
    lststop  = None
    caltime  = None
    setElmax = False
    if n == None or n <= 0: n = 1

    # If phaseCal is scalar, convert it to list and parse by comma
    pcal = phaseCal
    if str in [type(pcal)]: 
        pcal = makeListObsdef(pcal, parse=False)

    # Get element
    if len(pcal) < n:
        trackError('Error entering phase calibrator names')
        raise Exception, phaseCal

    # Parse phase calibrator
    p = pcal[n-1]
    if split: p = splitPhaseCalList([p])[0]
    t = makeListObsdef(p)
    if len(t) > 3:
        raise Exception,'Error entering phase calibrator : ' + t
    pname = t[0]
    args  = t[1:]

    # Check if this should be a phase calibrator only observation.
    # This can be specified if lststart or calonly is "cal".
    for a in args:
        # Skip if none
        if a == None: continue

        # Process argument
        if a.find('=') < 0:
            if lststart == None: 
                lststart = a
            elif lststop == None: 
                lststop = a
            else:
                raise Exception,'Error entering phase calibrator : ' + t
        else:
            # Parse
            tokens = a.split('=')
            if len(tokens) != 2:
                raise Exception,'Error reading phase calibrator informtion: '+t

            # Read arguments
            action = string.lower(tokens[0])
            value  = string.lower(tokens[1])

            # Parse action
            if action == "elmax":
               trise = getLstRise(pname, float(value))
               if trise <> None: lststop = trise
               setElmax = True
            elif action == "t":
               caltime = float(value)
            elif action == "lststart":
               lststart = value
            elif action == "lststop":
               lststop = value

    # If only lststart or lststop is set, then I need to set the other 
    # parameter. I use the lst time to make an intelligent choice,
    # and add 1 second just to avoid confusion in other parts of the program.
    if lststart == None and lststop <> None:
        if timeDifference(lststop,commands.lst()) > 0:
           lststart = str(commands.lst())
        else:
           lststart = helpers.convertHms(lststop) - 1.0/3600.0
           if lststart < 0: lststart += 24.0
    elif lststart <> None and lststop == None:
        if timeDifference(commands.lst(),lststart) < 0:
           lststop = helpers.convertHms(lststart) + 1.0/3600.0
        else:
           lststop = helpers.convertHms(lststart) + 11.9
        if lststop > 24: lststop -= 24.0

    # Check LST start/stop time
    if lststop <> None:
        dt = timeDifference(lststop, lststart)
        if check and (dt <= 0.0 or dt > 12.0):
            print 'Phase calibrator entry = ',phaseCal
            raise Exception,'LST range for phase calibrator ' + pname + \
                            ' must be between 0 and 12 hours.\n Change the start/stop time for the calibrator in the observing script\nsuch that LST stop - start < 12 hours.'

    # Done
    if returnCaltime:
        return pname, lststart, lststop, caltime
    else:
        return pname, lststart, lststop


def getHybridConfiguration(hybrid, specConfig, contConfig):
    """ Returns the hybrid configuration for the bandwidths in hybrid 

        Inputs:  hybrid - bandwidths for each band
                          e.g. hybrid = [BW500, BW62, BW62]
                 specConfig - complete correlator configuration for
                              science observations
                 contConfig - complete correlator configuration for
                              continuum observations

        Output: returns complete hybrid correlator configuration
    """
    # Loop over the bands in the hybrid configuration
    newConfig = getConfigband()
    for i in range(len(hybrid)):
        # Get configuration for this band to set IF frequency
        hconfig = specConfig[i][:]
        if hybrid[i] == commands.BW500 and contConfig <> None: 
            hconfig = contConfig[i][:]

        # Set hybrid bandwidth
        hconfig[1] = hybrid[i]

        # Copy to final hybrid configuration
        newConfig[i] = hconfig[:]

    # Return correlator configuration
    return newConfig[:]


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
    st = ''
    if timestamp: st = helpers.convertHmsString(commands.lst()) + ' '
    for dummy in range(indent): st = st + '    '
    st += msg
    printFunctions.printError(st, alarm=alarm)
    st = 'ERROR: ' + st
    commands.scriptlog(st)
    if printLog: commands.appendHistory(st)


def setSourceVariable(sources, variable, default):
    """ Converts source and variable into lists of the same size.

        Inputs: sources  - A string variable or a string list
                variable - A variable (e.g. intents, point status). If only
                           a single values is given, it is assumed all
                           sources will have the same value.
                default  - Default value
        Output: Return value is sourceList,varList where both are lists.
                On return, sourceList and varList have the same number of
                elements.

        Examples:
             (1) sources = ['S255', 'S254', 'S258']
                 pointstatus = commands.ONSRC
                 souList,statusList = setSourceVariable(sources,status,commands.ONSRC)
                 (souList and statusList both have 3 elements)
    """

    # Make default intent
    var_def = default
    if variable <> None: var_def = variable

    # Convert to lists
    sou = makeListObsdef(sources)
    v   = makeListObsdef(var_def)

    # If v is none, then set it to vector
    if v == None: v = [v] * len(sou)

    # Set variable to be a vector with the same length as sources.
    if len(sou) != len(v):
        if len(v) != 1:
            msg = 'Different number of sources and variable.\n' + \
                  'Number of sources = ' + str(len(sou)) + '\n' + \
                  'Number of intents = ' + str(len(v)) + '\n'
            trackError(msg)
            raise Exception, 'Exiting setSourceVariable'
        v = v * len(sou) # Since "i" is a list, * expands the number of elements

    # Done
    return sou[:],v[:]


def isEndOfTrack():
    """ Returns True/False if end of track has been issued """
    return commands.s.getScriptBool(INDX_BOOL_ENDTRACK)


def isLastcal():
    """ Returns True/False if lastcal() has been issued """
    return commands.s.getScriptBool(INDX_BOOL_LASTCAL)


def isStopAfterNextCal():
    """ Returns True/False if stopAfterNextCal() has been issued """
    return commands.s.getScriptBool(INDX_BOOL_STOPNEXTCAL)


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


def getTrackHistory():
    """ Returns track history """
    msg = commands.s.getScriptHistory()
    if msg == "":
        return None

    return msg


def getWeather(formatString=False):
    """ Return the ambient temperature (C), 225 GHz opacity, 
        wind speed (mph), and phase RMS (um).

        Inputs: formatString - if true, format return varaibles as 
                               string variables instead of floating point.

        Example: tamb, tau, wind, phaseRms = getWeather()
    """ 
    # Ambient temperature
    try:
        tamb = commands.queryDouble('Weather.ambientTemperature')
        tamb = 32.0 + 1.8 * tamb
        if formatString: tamb = str('%.1f' % tamb)
    except Exception:
        tamb = '?'

    # Tau
    try:
        tau = commands.queryDouble('OpacityMonitor.tau225')
        if formatString: tau = str('%.2f' % tau)
    except Exception :
        tau = '?'

    # Wind speed
    try:
        wind = commands.queryDouble('Weather.windSpeed')
        if formatString: wind = str('%.1f' % wind)
    except Exception :
        wind = '?'

    # phase RMS
    try:
        phaseRms = commands.queryDouble('PhaseMonitor.skyRMS')
        if formatString: phaseRms = str('%d' % int(phaseRms))
    except Exception:
        phaseRms = '?'

    # Done
    return tamb, tau, wind, phaseRms


def printWeather(timestamp=True, printLog=True, indent=1):
    """ Prints current weather conditions to the screen and script log"""

    # Get weather conditions
    tamb, tau, wind, phaseRms = getWeather(formatString=True)

    # Print weather
    msg = 'Tamb=' + str('%5s' % tamb) + ' F   ' + \
          'tau=' + str('%4s' % tau) + '      ' + \
          'wind=' + str('%4s' % wind) + ' mph  ' + \
          'phaseRMS=' + str('%4s' % phaseRms) + ' um';
    commands.trackMessage(msg, indent=indent, printLog=printLog, timestamp=timestamp)

def checkCoherence(fraction=0.5, maxcoh=0.2, indent=None, verbose=False, \
                   nlim=3,       nmaxrefreq=2):
    """ This function checks the coherence on the antennas in the current
        subarray and will relock a receiver on an antenna if the maximum
        coherence is less than maxcoh AND the coherence is less than
        fraction*median coherence.

       indent     : integer indicating number of blocks to indent when printing 
                    messages to script log
       nlim       : The receivers are relocked only if there are <= nlim 
                    antennas with low coherence. No action is taken if more
                    than this since it could be that something else if wrong 
                    with the system.
       nmaxrefreq : Maximum number of times a refreq will be issued for 
                    a receiver in a track
       verbose    : If true, print diagnostic information
    """

    # Get coherence for all antennas in subarray
    ants = commands.currentAntennaNumbers()
    coherence = []
    for a in ants:
        # Read value
        mp = 'astro.antenna%d.framemaxcoherence' % a
        coherence.append(commands.queryDouble(mp))

        # Print message
        if verbose:
            print 'Ant %2d  coherence=%.2f' % (a, coherence[-1:][0])

    # Find median coherence
    medianCoherence = np.median(coherence)
    if verbose:
        print 'Median coherence = %.2f' % medianCoherence

    # Find number of antennas with coherence less than fraction*mediaun
    j = np.where((np.array(coherence) < fraction*medianCoherence) & \
                 (np.array(coherence) <= maxcoh))
    j = j[0]
    nlow = len(j)
    if verbose:
        print '%d antennas appear to have too low of coherence' % nlow

    # Determine number of times receiver has been retuned for coherence
    scoh = commands.s.getScriptString(INDX_STR_COH_REFREQ)
    nrefreq = dict()
    if scoh == '':
       allants = range(1,24)
       for a in allants:
           nrefreq[a] = 0
    else:
       tokens = scoh.split()
       for t in tokens:
          tt = t.split(':')
          a = int(tt[0])
          n = int(tt[1])
          nrefreq[a] = n
    if verbose:
        for a,n in nrefreq.items():
            print 'Antenna %2d   nrefreq=%2d' % (a, n)

    # Relock
    if nlow <= nlim:
        for i in j:
           a = ants[i]
           if nrefreq[a] < nmaxrefreq:
              commands.refreq(a, auto=True)
              commands.trackMessage('low coherence (%.2f) on antenna %2d - relocking receiver' % \
                    (coherence[i], a), indent=indent)
              nrefreq[a] += 1

    # Update script variable
    s = ''
    for a,v in nrefreq.items():
        t = '%d:%d' % (a,v)
        if s != '': s += ' '
        s += t
    commands.s.setScriptString(INDX_STR_COH_REFREQ, s)
  

def relockReceivers(indent=None, pause=None):
    """ Runs refreq() and any receivers that are out of lock 

        Inputs: indent - indent spacing for track messages
                pause  - number of seconds to pause after attempting refreq

        Output: True/False if refreq() was attempted on one or more antennas
    """

    # Get the current antennas in the subarray
    antennas = commands.currentAntennaNumbers()

    # Loop over antennas
    relocked = False
    for a in antennas:
        # Set monitor point
        mp = getAntennaMp(a) + '.AntennaCommon.Receivers.rxState'

        # Get monitor point, but do not choke on this step
        try:
           rxstate = string.upper(commands.queryString(mp))
           if rxstate != 'GOOD' and a != 1:
               commands.refreq(a, auto=True)
               commands.trackMessage('attempting to relock CARMA '+str(a) + ' --- rxstate = '+rxstate, indent=indent)
               relocked = True
        except:
           print 'WARNING: Could not read rxstate for CARMA ',a

    # Pause if necessary
    if relocked and pause <> None and pause > 0: commands.sleep(pause)


def continueObservations(isScience=False):
    """ Indicates if observations should continue based on keyboard inputs 

        Inputs: isScience - current object is science target

        Observations will be halted if endtrack() is set, or lastcal() is
        set and this is a science source
    """
    cont = True
    if isEndOfTrack() or (isLastcal() and isScience): cont = False
    return cont


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
            if keyval[key] == None:
                if val == 'None': 
                    keyval[key] = None
                elif string.upper(val) == 'TRUE':
                    keyval[key] = True
                elif string.upper(val) == 'FALSE':
                    keyval[key] = False
                else:
                    keyval[key] = val
            elif int in vtype:
                try:
                    keyval[key] = float(val)
                except ValueError:
                    trackError("Cannot convert '" + val +
                               "' to an integer for key="+key)
                    err = True
            elif str in vtype:
                keyval[key] = val
            elif float in vtype:
                try:
                    keyval[key] = float(val)
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


def setLstStartCycle(t, verbose=True):
    """ reset LST start time (hh:mm:ss or hours) for the source/phaseCal cycle """
    if t == None:
        commands.s.setScriptDouble(INDX_DBL_LST_START_CYCLE,0.0)
        commands.s.setScriptBool(INDX_BOOL_LST_START_CYCLE,False)
        if verbose: commands.trackMessage('Unsetting LST start cycle')
    else:
        commands.s.setScriptDouble(INDX_DBL_LST_START_CYCLE,helpers.convertHms(t))
        commands.s.setScriptBool(INDX_BOOL_LST_START_CYCLE,True)
        if verbose: 
            commands.trackMessage('Setting LST start cycle to ' + 
                  helpers.convertHmsString(getLstStartCycle()))


def setLstStopCycle(t, verbose=True):
    """ reset LST stop time (hh:mm:ss or hours) for the source/phaseCal cycle """
    if t == None:
        commands.s.setScriptDouble(INDX_DBL_LST_STOP_CYCLE,0.0)
        commands.s.setScriptBool(INDX_BOOL_LST_STOP_CYCLE,False)
        if verbose: commands.trackMessage('Unsetting LST stop cycle')
    else:
        commands.s.setScriptDouble(INDX_DBL_LST_STOP_CYCLE,helpers.convertHms(t))
        commands.s.setScriptBool(INDX_BOOL_LST_STOP_CYCLE,True)
        if verbose: 
            commands.trackMessage('Setting LST stop cycle to ' + 
                  helpers.convertHmsString(getLstStopCycle()))


def setLstLastPoint(t, state_sun):
    """ Stores LST time (hh:mm:ss or hours) of last radio pointing observation """
    if t == None:
        commands.s.setScriptDouble(INDX_DBL_LASTPOINT,0.0)
        commands.s.setScriptBool(INDX_BOOL_LASTPOINT,False)
    else:
        commands.s.setScriptDouble(INDX_DBL_LASTPOINT,helpers.convertHms(t))
        commands.s.setScriptBool(INDX_BOOL_LASTPOINT,True)
        commands.s.setScriptDouble(INDX_DBL_SUNLST_LASTPOINT, getSourceElevation('sun'))
        commands.s.setScriptInt(INDX_INT_SUNSTATE_LASTPOINT, state_sun)


def setLstLastFluxcal(t):
    """ Stores LST time (hh:mm:ss or hours) of last flux cal observation """
    if t == None:
        commands.s.setScriptDouble(INDX_DBL_LASTFLUX,0.0)
        commands.s.setScriptBool(INDX_BOOL_LASTFLUX,False)
    else:
        commands.s.setScriptDouble(INDX_DBL_LASTFLUX,helpers.convertHms(t))
        commands.s.setScriptBool(INDX_BOOL_LASTFLUX,True)


def setLstLastPassband(t):
    """ Stores LST time (hh:mm:ss or hours) of last flux cal observation """
    if t == None:
        commands.s.setScriptDouble(INDX_DBL_LASTPASSBAND,0.0)
        commands.s.setScriptBool(INDX_BOOL_LASTPASSBAND,False)
    else:
        commands.s.setScriptDouble(INDX_DBL_LASTPASSBAND,helpers.convertHms(t))
        commands.s.setScriptBool(INDX_BOOL_LASTPASSBAND,True)


def setLastMosaicPosition(lastpos):
    """ Set last observed mosaic position that was observed"""
    if lastpos <= 0:
       commands.s.setScriptInt(INDX_INT_MOSPOS,0)
    else:
        commands.s.setScriptInt(INDX_INT_MOSPOS,lastpos)


def emailScriptLog(value=None):
    """ Set True/False if email should be sent.
        If value=None, then return True/False current value of variable.
    """
    if value == None:
        return (commands.s.getScriptBool(INDX_BOOL_SENDEMAIL) != 0)
    else:
        commands.s.setScriptBool(INDX_BOOL_SENDEMAIL, value)

def checkTuning(tuning, dopplerSource):
    """ Verify that requested tuning is consistent with tuning dictionary 
        The LO frequency, IF frequency, and sideband are checked.
    """

    # Set which variables to check
    var = ['Commands.Freq.restFreq', 'Commands.Freq.sideband', 'Commands.Freq.ifFreq', 'dopplerSource']
    desc= ['Rest frequency', 'sideband', 'IF frequency', 'dopplerSource']
    vtype = ['float', 'string', 'float', 'string']

    # Get values in tuning dictionary
    sgn = -1.0
    sideband = 'USB'
    if tuning['sideband'] == commands.LSB: 
       sgn = 1.0
       sideband = 'LSB'
    d = dopplerSource
    if d == None: d = 'None'
    tdict = [tuning['restfreq'], sideband, tuning['IFfreq'], d]

    # Check variables
    err   = False
    is1CM = False
    for i in range(len(var)):
       # Get monitor point
       mp = _controlMPprefix() + var[i]
       if vtype[i] == 'float':
          value = commands.queryDouble(mp)
       elif vtype[i] == 'string':
          value = commands.queryString(mp)
       else:
          raise Exception,'Unknown datatype (%s) in checkTuning' % vtype[i]

       if i == 0: is1CM = value < 50.0
       # Check value
       if vtype[i] == 'float':
          diff = value - tdict[i]
          # Don't check the IF freq for 1cm
          if (not (is1CM and i==2)) and (abs(diff) > 0.001):
             if not err: print 'Error in verifying tuning'
             print 'Expected %s = %f, but found %f' % (desc[i], tdict[i], value)
             err = True
       elif vtype[i] == 'string':
          if value.upper() != tdict[i].upper():
             if not err: print 'Error in verifying tuning'
             print 'Expected %s = %s, but found %s' % (desc[i], tdict[i], value)
             err = True
       else:
          raise Exception,'Unknown datatype (%s) in checkTuning' % vtype[i]

    # Done
    if err:
       if d != None: d = "'%s'" % d
       raise Exception,'The values in the tuning dictionary are not the same as the current frequency setup.\n Either manually re-issue the correcting freq command, or re-run the script from scratch.\n The correct freq command is likely:\n freq(%f, %s, %f, %s)' % (tuning['restfreq'], sideband, tuning['IFfreq'], d)


def tuneReceivers():
    """ Return True/False if the receivers need to be tuned """
    return commands.s.getScriptBool(INDX_BOOL_TUNE)


def sunStateLastPoint():
    """ Returns sun state at time of last pointing """
    return commands.s.getScriptInt(INDX_INT_SUNSTATE_LASTPOINT)


def sunState(dt, dtTransition=DT_TRANSITION):
    """ Returns flag indicating location of sun 

        Inputs: dt - time in hours since sunrise or sunset
                dtTransition : the transition time in hours that defines
                               day, night, or transition

        Output: sun state
    """
    state = SUNSTATE_NIGHT
    if isSunUp(): state = SUNSTATE_DAY
    if abs(dt) <= dtTransition: state = SUNSTATE_TRANS

    return state


def checkAntennas(indent=None):
    """ Checks which antennas are in the array, and if that has changed
        since the last time the function was called.

        Inputs: indent - indent spacing to use when printed antenna
                         status to the script log

        Output: None
    """
    # Get the current antennas in the subarray
    antennas = commands.currentAntennaNumbers()

    # Convert antennas list to a string
    antStr = ''
    for a in antennas:
        if antStr != "": antStr += ' '
        antStr += str("%d" %  a)

    # Get old antenna string list
    antStrOld = commands.s.getScriptString(INDX_STR_ANTENNAS)

    # If the old list is empty, then just print the current antennas on
    # line. If it is not empty, check to see if any antennas have been
    # added or removed
    if antStrOld == "":
        offline = ''
        antsCurrent = commands.currentAntennaNumbers()
        if max(antsCurrent) <= 15:
           ants = range(1,16)
        elif min(antsCurrent) >= 16:
           ants = range(16,24)
        else:
           ants = range(1,24)
        for a in ants:
            if not a in antennas:
                if offline == '': offline += ''
                offline += str(' %s' % a)
        commands.trackMessage(str(len(antennas)) + ' antennas online',indent=indent)
        if offline != '':
            commands.trackMessage('antennas offline : ' + offline,indent=indent)
    elif antStr == antStrOld:
        # Nothing to do
        pass
    else:
        # Determine which antennas were on online
        antennasOldList = antStrOld.split()
        antennasNewList = antStr.split()

        # Find antennas removed from subarray
        for a in antennasOldList:
            if a not in antennasNewList:
                commands.trackMessage('Antenna ' + a + ' was taken offline', indent=indent)

        # Find antennas added from subarray
        for a in antennasNewList:
            if a not in antennasOldList:
                commands.trackMessage('Antenna ' + a + ' is now online', indent=indent)

    # Store new antennas in memory
    commands.s.setScriptString(INDX_STR_ANTENNAS, antStr)


def setCorrCal(mode, configCorr=None, custom=None):
    """ Set correlator configuration to continuum mode for calibrator observations.

        Inputs: mode  - If mode = 1 = CORR_BW500, change all bands to
                        500 MHz and keep LO2 the same.
                        If mode = 2 = CORR_BW500LO, change all bands to
                        500 MHz and change LO2 to have 3 non-overlapping bands.
                        If mode = 3 = CORR_BW500LO6, change all bands to
                        500 MHz and change LO2 to have 3 non-overlapping bands
                        within the BIMA IF.
                        if mode = 4 = CORR_CUSTOM, then correlator configuration
                        is set explicitly by the user.

                        If the LO frequency is in the 1mm band, then
                        CORR_BW500LO and CORR_BW500LO6 have the same 
                        functionality.
                configCorr - template correlator configuration. If None, then
                             the current correlator configuration is used.
                custom     - custom correlator configuration for hybrid mode.
                             Only used for mode=od.CORR_CUSTOM.

        Output: new correlator configuration

        Example: configNew = setCorrcal(CORR_BW500)
    """

    # Get configuration
    corrConfigOld = configCorr
    if corrConfigOld == None: corrConfigOld = getConfigband()

    # Change configuration
    is1mmBand = False
    if commands.freqSetup()[2] > 150.0: is1mmBand = True
    if mode == CORR_BW500LO or (is1mmBand and mode == CORR_BW500LO6):
        # Change IF so bands to not overlap in BIMA IF.
        wideBand = configCorrWidebandCarma(corrConfigOld, band1mm=is1mmBand)
    elif mode == CORR_BW500LO6:
        # Change IF so bands do not overlap in BIMA IF.
        wideBand = configCorrWidebandBima(corrConfigOld)
    elif mode == CORR_BW500:
        # change each band to 500 MHz, but bands may overlap
        wideBand = corrConfigOld[:]
        for i in range(len(wideBand)):
            wideBand[i][1] = commands.BW500
    elif mode == CORR_CUSTOM:
        # Wideband must be set already
        if custom == None:
            raise Exception,'configCal must be set in the correlator dictionary if using CORR_CUSTOR'
        wideBand = custom[:]
    else:
        raise Exception, 'Error reading correlator configuration: ' + str(mode)

    # Check 
    if len(wideBand) != len(getConfigband()):
       raise Exception,'Length of hybrid correlator configuration is not the same as the science spectral setup'

    return wideBand[:]


def configCorrWidebandCarma(configCorr, band1mm=False) :
    """ Returns  correlator configuration to wideband continuum.

        Two of the bands are optimized within the BIMA IF, and the third
        is optimized for the OVRO IF.

        Inputs: configCorr - the current correaltor configuration
                band1mm    - If True, then set IF frequencies for 1mm band
    """
    # Function is applicable only for <= 3 bands
    if len(configCorr) > 3:
        raise Exception, 'Can only re-configure 3 correlator bands'

    # Initialize
    wideConfigP = []

    # Get frequency setup of current correlator configuration
    fsetup = commands.freqSetup()
    frest  = fsetup[0]
    fif    = fsetup[1]
    flo    = fsetup[2]

    # Determine sideband for rest frequency
    sidebandLO = commands.USB
    if flo > frest: sidebandLO = commands.LSB

    # Set frequency offsets from frest. The nominal frequencies are
    # for a IF frequency of 2.15 GHz and upper sideband. We need
    # to allow for other setups for these parameters.
    # sbBaseband = sideband baseband for downconverter
    sbBaseband = [commands.LSB, commands.LSB, commands.LSB]
    dfreq = [-0.5, 0.0, 1.1]
    fifnew = 2.15
    if band1mm: 
        dfreq = [-0.5, 0.0, 0.5]
        fifnew = 2.75
        sbBaseband = [commands.LSB, commands.LSB, commands.LSB]
    for i in range(len(dfreq)):
        # Correct for different IF frequency
        df = dfreq[i] - fif + fifnew

        # Correct for sideband
        if sidebandLO == commands.LSB: df *= -1.0

        # Set frequency
        dfreq[i] = df

    # Cycle over correlator bands
    for i in range(len(configCorr)):
        # Get correlator band
        z = configCorr[i]

        # Compute new frequency. Set baseband sideband.
        z[1] = commands.BW500
        z[2] = frest + dfreq[i]
        z[3] = sbBaseband[i]

        # put them all in the lower sideband with 500 MHz bandwidth
        wideConfigP.append(z)
    return wideConfigP


def configCorrWidebandBima(configCorr) :
    """ Returns  correlator configuration to wideband continuum that falls
        within the 6-m IF bandpass.

        Inputs: configCorr - the current correaltor configuration

        Adopted from Stephen White's ref-Point routine.
    """
    # Initialize
    wideConfigP = []

    # Get frequency setup of current correlator configuration
    fsetup = commands.freqSetup()
    flo = fsetup[2]

    # Determine sideband
    sbFacLo = 1   # USB
    if (fsetup[2] > fsetup[0]) : sbFacLo = -1 # LSB

    # Set parameters for new correlator configuration
    sbBaseband = commands.LSB
    sbFacBaseband = 1
    if sbBaseband == commands.LSB: sbFacBaseband = -1
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
        tempConfig = [fbn, commands.BW500, fbb, sbBaseband, fbrest]
        wideConfigP.append(tempConfig)
    return wideConfigP


def setMosaicFile(filename):
    """ Set input mosaic file including path name """
    inputFile = string.strip(filename)
    if inputFile.find('/') != 0: 
        inputFile = DIRECTORY_MOSAICS + inputFile
    return inputFile


def zeroPointingOffsets(ant=None, bima=False, indent=1):
    """ Removes pointing offsets for antennas 

        Inputs: ant    - antenna list
                bima   - If True, then zero bima antennas
                indent - Indent for messages

        Output: None
    """

    # Get antennas
    antList = ant
    if antList == None: antList = commands.currentAntennaNumbers()
    if bima:            antList = commands.antennasByType()[1]
    antList = makeListObsdef(antList)

    # Zero offsets
    if antList == None: antList = list()
    if len(antList) > 0: commands.zeroMountOffsets(antList)

    # Print message
    alist = ''
    for a in antList:
        if alist != '': alist += ' '
        alist += str(a)
    if alist != '':
        commands.trackMessage('initialized mount offsets for antenna ' + alist, 
                     indent=indent)


def trackSummary():
    """ Summarizes the track in terms of total time, which calibrations
        were completed, and time spent in source/phaseCal cycle.
    """

    # Summary
    commands.trackMessage('\nTrack summary', timestamp=False)

    # Total time
    dt = timeDifference(commands.lst(), getLstStartTrack(), pos=True)
    tmp = '    track length = ' + dtString(dt)
    commands.trackMessage(tmp, timestamp=False)

    # Source time
    t = commands.s.getScriptDouble(INDX_DBL_TSOURCE)
    tmp = 'source  time = ' + dtString(t)
    commands.trackMessage(tmp, indent=1, timestamp=False)

    # Primary flux calibrator
    tmp = 'observed primary   flux cal   : '
    if commands.s.getScriptInt(INDX_INT_NOBS_FLUXPRI) > 0:
        commands.trackMessage(tmp + commands.s.getScriptString(INDX_STR_FLUXPRIMARY), indent=1, timestamp=False)
    else:
        commands.trackMessage(tmp + 'None', indent=1, timestamp=False)

    # Secondary flux calibrator
    tmp = 'observed secondary flux cal   : '
    if commands.s.getScriptInt(INDX_INT_NOBS_FLUXSEC) > 0:
        commands.trackMessage(tmp + commands.s.getScriptString(INDX_STR_FLUXSECONDARY), indent=1, timestamp=False)
    else:
        commands.trackMessage(tmp + 'None', indent=1, timestamp=False)

    # Passband calibration
    tmp = 'observed passband calibrators : '
    if commands.s.getScriptBool(INDX_BOOL_PASSBAND) :
        commands.trackMessage(tmp + commands.s.getScriptString(INDX_STR_PASSBAND), 
                     indent=1, timestamp=False)
    else:
        commands.trackMessage(tmp + 'None', indent=1, timestamp=False)

    return


def sendEmailMessage(sendTo,           sendFrom,  msg, 
                     subject=None,     cc=None,   bcc=None, 
                     attachments=None, scriptLog=None,
                     verbose=False):
    """ Send email message.

        Inputs: sendTo   - the recipient(s) as a comma separated string or list
                sendFrom - the sender as a string
                msg      - the body of the message as string
                subject  - the subject
                cc       - cc list
                bcc      - bcc list
                attachments - list of files to send as attachments
                scriptLog - the script log stored as a string that will be
                            sent as an attachment
                verbose   - print message that email was sent
        Output: None
    """
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
            for filename in makeListObsdef(attachments):
                 part = email.MIMEBase.MIMEBase('text', "plain")
                 part.set_payload( open(filename,"rb").read() )
                 # email.Encoders.encode_base64(part)
                 part.add_header('Content-Disposition', 'attachment; filename="%s"'
                                 % os.path.basename(filename))
                 message.attach(part)

        # Attach script log
        if scriptLog <> None:
            st = email.MIMEText.MIMEText(scriptLog)
            st.add_header('Content-Disposition', 'attachment; filename="scriptlog.txt"')
            message.attach(st)
    else:
        message = email.MIMEText.MIMEText(msg)
    message['To'] = toStr
    message['From'] = fromStr
    message['Date'] = email.utils.formatdate(localtime=True)
    message['Message-ID'] = email.utils.make_msgid()
    if cc  <> None: message['Cc'] = ccStr
    if bcc <> None: message['Bcc'] = bccStr
    if subject <> None: message['Subject'] = subject

    # Send message
    hosts = ['mail.ovro.caltech.edu', 'localhost']
    smtp = None
    connected = False
    for h in hosts:
        try:
            smtp = smtplib.SMTP(h)
            connected = True
        except Exception: 
            pass
        if connected: break
    if connected:
        if verbose: commands.trackMessage('Sending email to ' + toStr)
        smtp.sendmail(fromStr,recipients,message.as_string())
        smtp.close()
    else:
        if verbose: commands.trackMessage('Script email not sent. Error opening smtp connection')


def sendEmail(pc,          email,     scriptName,    scriptOptions, 
              start=False, msg=None,  restart=False):
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
    if restart : return

    # Get obsblock name
    obsblockName = getObsblockName()
    tokens = obsblockName.split('.')
    project = tokens[0]
    obsblock = tokens[1]
    trial = tokens[-1:][0]

    # Body of message
    msgBody = ''
    if pc <> None and pc <> '': 
        msgBody += 'Project code  : ' + pc + '\n'
    else:
        msgBody += 'Project code  : Unknown\n'
    msgBody += 'Starting time : ' + getLocalTime() + '\n'
    msgBody += 'LST time      : ' + helpers.convertHmsString(commands.lst()) + '\n'
    msgBody += 'UT  time      : ' + helpers.convertHmsString(getUT()) + '\n'
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
    if restart :
        subject += 'CARMA track restarted'
    elif start :
        subject += 'CARMA track started'
        msgBody += '\n'
        msgBody += 'To monitor your track in real time, you can launch the\n'
        msgBody += 'Real Time Display (RTD) windows at\n'
        msgBody += '    http://cedarflat.mmarray.org/observing/tools/rtd.html\n'
        msgBody += '\n'
        msgBody += "You can monitor the real time reduction of your track in the 'gains page' at\n"
        msgBody += '    http://cedarflat.mmarray.org/gains/index_sci%d.html\n' % commands.subarrayNo
        msgBody += 'This web page is password protected. and can be accessed using\n'
        msgBody += 'the username/password you used to upload the observing script.\n'
        msgBody += '\n'
    else:
        subject += 'CARMA track finished'
        msgBody += 'Important links:\n' + \
                   '    1) Instructions on how to retrieve your data\n' + \
                   '           http://cedarflat.mmarray.org/observing\n\n'
        msgBody += '    2) Real-time reduction from "gains" script\n' + \
                   '           http://cedarflat.mmarray.org/gains/results/%s/%s.%s\n\n' % (project, obsblock, trial)
        msgBody += '    3) Output from the quality script (note: may not be immediately available)\n' + \
                   '           http://cedarflat.mmarray.org/quality/%s/%s.%s\n\n' % (project, obsblock, trial)

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
        scriptLog += 'UT       Comments\n'
        scriptLog += '-------- ----------------------------------------------------------------------\n'
        for n in makeListObsdef(msg, parse=False):
            if n <> None: scriptLog += n + '\n'

    # Send the message and check for errors
    sendFrom = 'obs@mmarray.org'
    bcc  = None
    cc   = 'obs@mmarray.org'
    attachments = scriptName
    sendEmailMessage(email, sendFrom, msgBody, subject=subject, bcc=bcc, cc=cc,
                     attachments=attachments, scriptLog=scriptLog, verbose=True)


def setLastPointing(source, sunStateCurrent):
    """ Sets variables for lst pointing source """
    setLstLastPoint(commands.lst(), sunStateCurrent)
    commands.addScriptString(INDX_STR_POINTNAME, source,
                    bindx=INDX_BOOL_POINTNAME, delim=', ', allowDup=False)
    commands.s.setScriptString(INDX_STR_LASTPOINT_NAME, source)
    azel = commands.azel(source)
    commands.s.setScriptDouble(INDX_DBL_LASTPOINT_AZ, azel[0])
    commands.s.setScriptDouble(INDX_DBL_LASTPOINT_EL, azel[1])


def observeNoise(tintSeconds, trecordSeconds, verbose=True, indent=None,
            pacs=False):
    """ Observe the noise for tintMinutes with a record time of
        trecordSeconds. The number of repeats of trecordSeconds is given
        by tintMinutes/trecordSeconds rounded to the nearest integer with
        a minimum of one repeat.

        The observing sequence for the noise source is:
           (1) noiseon()
           (2) integrate(trecordSeconds,nrepeats)
           (3) noiseoff())
    """
    subarray=commands.DEFAULT
    if pacs: subarray=commands.BOTH
    nrep = getNreps(tintSeconds/60.0,trecordSeconds)
    if nrep > 0: 
        if verbose:
            tmp = str("%-10s" % 'noise') + '   tint=' + \
                str("%9s" % dtString(trecordSeconds * nrep / 3600.0))
            commands.trackMessage(tmp, indent=indent)
        commands.noiseon(subarray=subarray)
        commands.intent('noise', 'B', subarray=subarray)
        commands.integrate(trecordSeconds, nrep, antwait=commands.NONE,
                subarray=subarray)
        commands.noiseoff(subarray=subarray)


def observePol(tintMinutes, trecordSeconds, verbose=True, indent=None):
    """ Observe the LCP-RCP for the 10m antennas for tintMinutes with a record time of
        trecordSeconds. The number of repeats of trecordSeconds is given
        by tintMinutes/trecordSeconds, rounded to the nearest integer with
        a minimum of one repeat. The observations are done at the current telescope
        postion.

        The procedure is as follows:

             (1) Move the polarizers
             (2) Integrate for tintMinutes
             (3) Move the polarizers back

        This program detects when the grids have successfully moved into the 10m beams, for
        XYphase calibration. It gives up after 45 seconds. 

        Notes:

        GRID_POS --> 270 degrees
        RX_1MM_POS --> 180 degrees

        Move grids back to 1mm position        
            subprocess.Popen(shlex.split('canpacket overip=t host=c3.carma.pvt canbus=1 api=72 msgid=0x082 format=%c values=0x01')).wait()
        Move grids back to 3mm position        
            subprocess.Popen(shlex.split('canpacket overip=t host=c3.carma.pvt canbus=1 api=72 msgid=0x082 format=%c values=0x00')).wait()
    """
    subarray = commands.DEFAULT
    nrep = getNreps(tintMinutes,trecordSeconds)
    if nrep > 0: 
        # Print message
        if verbose:
            tmp = str("%-10s" % 'polarization calibration') + '   tint=' + \
                str("%9s" % dtString(trecordSeconds * nrep / 3600.0))
            commands.trackMessage(tmp, indent=indent)

        # Check which 10m antennas are in the array
        ants = np.array(commands.currentAntennaNumbers())
        print "\nThe following 10m antennas are in the array:"
        print ants[ants<7]
        print 'WARNING: Not inserting grids for antennas 2'
        status=np.zeros(len(ants),dtype=str)

        t = 0
        # Insert grids
        print '\nInserting grids...\n'
        for a in range(1,7) :
            if a != 2 and a in commands.currentAntennaNumbers():
                subprocess.Popen(shlex.split('canpacket overip=t host=c%d.carma.pvt canbus=1 api=72 msgid=0x082 format=%%c values=0x02'%(a))).wait()

        print '\nStatus:\n'
        while len( status[ status != 'GRID_POS' ] ) and t<45 :
            status=np.array([])
            for ant in ants[ants<7] :
                if a == 2: continue
                status = np.append( status, commands.queryString( 'ovro%d.optics.mmselectpos' %ant, 20 ) )
            if not t%4 :
                print status
            commands.sleep(1)
            t += 1
        if t<45 :
            print status
            print "\nAll grids are *IN*, after %d seconds.\n"%t
        else :
            print "\nWARNING!  Not all grids are in yet, after %d seconds.\n\nFinal status:\n"%t
            print status
            print '\n'

        # Retrieve source name
        sourceName = commands.queryString("Control.Subarray%d.Source"%commands.subarrayNo)

        # Set intent
        commands.intent(sourceName, 'P', True)

        # Integrate with grids in
        print 'Integrating...'
        commands.integrate(trecordSeconds, nrep)

        t = 0
        # Take the grids out
        print 'Moving the grids out...'
        for a in range(1,7) :
            if a != 2 and a in commands.currentAntennaNumbers():
                subprocess.Popen(shlex.split('canpacket overip=t host=c%d.carma.pvt canbus=1 api=72 msgid=0x082 format=%%c values=0x01'%(a))).wait()

        print '\nStatus:\n'
        while len( status[ status != 'RX_1MM_POS' ] ) and t<45 :
            status=np.array([])
            for ant in ants[ants<7] :
                if a == 2: continue
                status = np.append( status, commands.queryString( 'ovro%d.optics.mmselectpos' %ant, 20 ) )
            if not t%4 :
                print status
            commands.sleep(1)
            t += 1
        if t<45 :
            print status
            print "\nAll grids are *OUT*, after %d seconds.\n"%t
        else :
            print "\nWARNING!  Not all grids are out yet, after %d seconds.\n\nFinal status:\n"%t
            print status
            print '\n'

        # Set variables
        commands.s.setScriptBool(INDX_BOOL_LASTPOLARIZATION, True)
        commands.s.setScriptDouble(INDX_DBL_LASTPOLARIZATION, commands.lst())


def checkSources(sources, key=True, indent=None, isPhase=False, minsepsun=None):
    """ Determines if existing sources can be read by existing catalogs.

        Inputs: sources - a scalar, list, or dictionary of source names
                key     - If True and sources is a dictionary, the source 
                          names are assumed to be the keyword of the
                          dictionary (map[key] = value). If False, the source
                          names are assumed to be the value.
                isPhase - If true, sources is a phase/LST list
                indent  - number of spaces to indent track message 
                minsepsun - Minimum allowed separation from the sun.
    """

    # If there are no sources, then there is nothing to do
    if sources == None: return

    # Convert to list or dictionary
    z = None
    if dict in [type(sources)]: 
        z = sources.copy()
        if key: z = makeListObsdef(z)   # Removes sources with value=False
        if z == None: z = [None]
    else:
        z = makeListObsdef(sources, parse=(not isPhase))

    # Loop over sources
    for t in z:
        # Get source name, which may be a list() or command separated string
        names = t
        if dict in [type(z)] and not key: names = z[names]

        # Check if string or command separated list
        names = makeListObsdef(names)
        if names == None: continue
        if isPhase: 
            getPhaseCal(names)  # This checks LST ranges
            names = [names[0]]

        # Check source is up. I do not care if the source is up, but this
        # will exercise the catalogs
        if names <> None:
           for n in names: 
               # There are reserved source names that should not be checked.
               nu = string.lower(n)
               if nu == "flux" or nu == "fluxcal" or \
                  nu == "passband" or nu == "bandpass" or \
                  nu == "point":
                   continue

               # See if source has already been checked
               slist = commands.s.getScriptString(INDX_STR_CHECKSOURCE)
               docheck = True
               if slist != "":
                   if string.upper(n) in makeListObsdef(slist): 
                       docheck = False

               # Check source
               if docheck:
                   commands.trackMessage('checking ' + n, indent=indent)
                   isSourceUp(n)
                   slist += " " + string.upper(n)
                   commands.s.setScriptString(INDX_STR_CHECKSOURCE, slist)

                   # Check SZ-sun distance
                   if minsepsun <> None and getDistance(nu, 'sun') < minsepsun:
                       msg = '\nThe source %s is %.1f deg from the sun. The closest it can be is %.1f deg,\notherwise the observations will be corrupted. Please run a different track.\nIf you *really* want to run this track, you can run the script with the option minsepsun=0.' % (nu, getDistance(nu, 'sun'), minsepsun)
                       raise Exception,msg
