# Subarray control commands
# To have these available at the keyboard level, include:
#  from subarrayCommands import *
# in your python file
# @author Steve Scott
# $Id: subarrayCommands.py,v 1.781 2014/12/02 19:09:40 scott Exp $


#
# $CarmaCopyright$
#
"""Module: subarrayCommands.py"""

import carma
import carmaIni
import getopt
import inspect
import math
import pacs
import os
import re
import sys
import time
import traceback
import types

import Subarray
import device
import carmaHelpers as helpers
import KeyboardInterruptable
import SubarrayControl_idl

from tabs2spaces import tabs2spaces
from printFunctions import *
import obsdefIndex as odi
from pdbWrappers import *
from monsysDropouts import monsysDropouts
from subarrayControl import s, s1, s2, subarrayNo, saName, initialize
from runCommand import ScriptError, ScriptReturn, ScriptInputError
from stateHistory import *
from queue import *
from fitDelays import *

# script enumerations
RUN       = carma.control.RUNNING
COMPLETED = carma.control.COMPLETED
FAILED    = carma.control.CRASHED

# Script globals
fullScriptName = 'NONE'
scriptName     = 'NONE'
scriptOptions  = 'None'
scriptKeyVals  = dict()

ON  = carma.antenna.common.ON
OFF = carma.antenna.common.OFF

# Receiver type enumerations
RX3MM = carma.antenna.common.RxControl.RX3MM
RX1MM = carma.antenna.common.RxControl.RX1MM
RX1CM = carma.antenna.common.RxControl.RX1CM

# Block downconverter polarization channel enumerations
POL1 = carma.control.BLOCK_DC_POL1
POL2 = carma.control.BLOCK_DC_POL2

# Correlator FPGA Mode
SINGLEPOL = carma.control.SINGLEPOL
DUALPOL   = carma.control.DUALPOL
FULLPOL   = carma.control.FULLPOL
CARMA23   = carma.control.CARMA23

# Ambient load enumerations
AMBIENT = carma.antenna.common.CalibratorControl.AMBIENT
SKY     = carma.antenna.common.CalibratorControl.SKY

# Polarization control state enumerations
VERTICAL   = carma.antenna.common.PolarizationControl.POLV
HORIZONTAL = carma.antenna.common.PolarizationControl.POLH
RCP        = carma.antenna.common.PolarizationControl.POLRCP
LCP        = carma.antenna.common.PolarizationControl.POLLCP

SINGLE        = carma.antenna.common.RxControl.SINGLE
LEFTCIRCULAR  = carma.antenna.common.RxControl.LEFTCIRCULAR
RIGHTCIRCULAR = carma.antenna.common.RxControl.RIGHTCIRCULAR

# Correlator designation enumerations
CORR_NONE     = carma.util.CORR_NONE
CORR_SPECTRAL = carma.util.CORR_SPECTRAL
CORR_WIDEBAND = carma.util.CORR_WIDEBAND
CORR_ALL      = carma.util.CORR_ALL
CORR_C3GMAX8  = carma.util.CORR_C3GMAX8
CORR_C3GMAX23 = carma.util.CORR_C3GMAX23
CORRELATOR_DESIGNATION_MP = carma.control.CORR_DESIGNATION_STRING

# Correlator designation strings
CORR_STRING = {
    CORR_NONE:"CORR_NONE",
    CORR_SPECTRAL:"CORR_SPECTRAL",
    CORR_WIDEBAND:"CORR_WIDEBAND",
    CORR_C3GMAX8:"CORR_C3GMAX8",
    CORR_C3GMAX23:"CORR_C3GMAX23",
    CORR_ALL:"CORR_ALL"
}


#Image fidelity type enumerations
IMG = carma.control.IMG
SNR = carma.control.SNR

# Fault System Preference
PREF_NONE  = carma.fault.PREF_NONE
PREF_BLANK = carma.fault.PREF_BLANK
PREF_FLAG  = carma.fault.PREF_FLAG

#Stow options
ZENITH  = carma.antenna.common.DriveControl.ZENITH
SERVICE = carma.antenna.common.DriveControl.SERVICE
SAFE    = carma.antenna.common.DriveControl.SAFE

# AzWrap modes
TRACKTIME = carma.control.TIME
ADD       = carma.control.ADD
SUB       = carma.control.SUB
ZERO      = carma.control.ZERO

# Singledish point status
ONSRC  = carma.control.ON_SOURCE
OFFSRC = carma.control.OFF_SOURCE

# getNearest actions
EXCLUDE = carma.control.ACTION_EXCLUDE
INCLUDE = carma.control.ACTION_INCLUDE

# getNearest actions
SHADOW_INTERNAL = carma.control.SHADOW_INTERNAL
SHADOW_SWEPTVOLUME = carma.control.SHADOW_SWEPTVOLUME

# subarray selection (a few commands can operate on any subarray)
SCI1    = "SCI1"
SCI2    = "SCI2"
DEFAULT = "DEFAULT"
BOTH    = "SCI1SCI2"

# Different things to wait on (wait command)
INTEG    = carma.control.WAIT_INTEG
TRACK    = carma.control.WAIT_ONSOURCE
TILT     = carma.control.WAIT_TILT
OPTICS   = carma.control.WAIT_OPTICS
CENTROID = carma.control.WAIT_CENTROID
CAL      = carma.control.WAIT_CALIBRATOR
CORR     = carma.control.WAIT_CORRELATOR
TUNED    = carma.control.WAIT_TUNED
TIME     = carma.control.WAIT_INTERVAL
# And how many need to be ready
ALL      = carma.control.WAIT_ALL
ANY      = carma.control.WAIT_SINGLE
COUNT    = carma.control.WAIT_COUNT
NONE     = None
NOCHANGE = "dontChange"

# Antenna references
bima1 = device.Bima(1)
bima2 = device.Bima(2)
bima3 = device.Bima(3)
bima4 = device.Bima(4)
bima5 = device.Bima(5)
bima6 = device.Bima(6)
bima7 = device.Bima(7)
bima8 = device.Bima(8)
bima9 = device.Bima(9)
ovro1 = device.Ovro(1)
ovro2 = device.Ovro(2)
ovro3 = device.Ovro(3)
ovro4 = device.Ovro(4)
ovro5 = device.Ovro(5)
ovro6 = device.Ovro(6)
sza1  = device.Sza(1)
sza2  = device.Sza(2)
sza3  = device.Sza(3)
sza4  = device.Sza(4)
sza5  = device.Sza(5)
sza6  = device.Sza(6)
sza7  = device.Sza(7)
sza8  = device.Sza(8)

carma1  = device.Ovro(1)
carma2  = device.Ovro(2)
carma3  = device.Ovro(3)
carma4  = device.Ovro(4)
carma5  = device.Ovro(5)
carma6  = device.Ovro(6)
carma7  = device.Bima(1)
carma8  = device.Bima(2)
carma9  = device.Bima(3)
carma10 = device.Bima(4)
carma11 = device.Bima(5)
carma12 = device.Bima(6)
carma13 = device.Bima(7)
carma14 = device.Bima(8)
carma15 = device.Bima(9)
carma16 = device.Sza(1)
carma17 = device.Sza(2)
carma18 = device.Sza(3)
carma19 = device.Sza(4)
carma20 = device.Sza(5)
carma21 = device.Sza(6)
carma22 = device.Sza(7)
carma23 = device.Sza(8)


#---------------------------- Utilities ------------------------------------
def makeAntList(ants, subarray=DEFAULT) :
    """Expands input into a list of antenna numbers.
    Parameter:
       ants: If ants is a scalar, returns a single item list.
         If ants is a list it is returned without change.
         If ants is zero, a list of all ants currently in the subarray
         is returned."""
    if list == type(ants) :
        if ants.count(0) > 0: ants = 0
    if ants == 0: return currentAntennaNumbers(subarray)
    else :        return helpers.makeList(ants)

def makeAntString(ants):
    """Helper method to return a string with a label (Antenna(s) and the
    list of antennas.
    Parameter:
      ants: a single or list of antennas. Zero is exanded to list of
        antennas currently in the array."""
    st = "Antenna"
    antlist = makeAntList(ants)
    if len(antlist) > 1: st += "s"
    st += " %s"%helpers.formatAsRanges( antlist )
    return st

def sleep(seconds) :
    """Sleep for a specified amount     raise Exception, "Invalid subarray: " + subarray
 of time
    Parameter:
      seconds: number of seconds to sleep (real number)
    See also: wait  """
    time.sleep(seconds)

def cancel(subarray=DEFAULT) :
    """Cancel any active waits or integrations in the subarray."""
    multiSubarray('cancel', subarray)

def getMiriadUTStamp() :
    """Form a miriad sytle time stamp for the current time"""
    def maybeAddAZero(xS) :
        if (xS < 10) : return '0'+str(xS)
        else : return str(xS)
    calendar = dict()
    calendar['1']  = 'jan'
    calendar['2']  = 'feb'
    calendar['3']  = 'mar'
    calendar['4']  = 'apr'
    calendar['5']  = 'may'
    calendar['6']  = 'jun'
    calendar['7']  = 'jul'
    calendar['8']  = 'aug'
    calendar['9']  = 'sep'
    calendar['10'] = 'oct'
    calendar['11'] = 'nov'
    calendar['12'] = 'dec'
    utStamp = time.gmtime()
    utYear  = str(utStamp[0])[2:]
    utMon   = str(utStamp[1])
    utMonU  = calendar[utMon]
    utDay   = maybeAddAZero(utStamp[2])
    utHour  = maybeAddAZero(utStamp[3])
    utMin   = maybeAddAZero(utStamp[4])
    utSec   = maybeAddAZero(utStamp[5])
    return ('%s%s%s:%s:%s:%s' % (utYear,utMonU,utDay,utHour,utMin,utSec) )

def progress(text):
    "Print out a message indicating init progress"
    if progress.firstCall:
        progress.startS = time.time()
        progress.deltaS = time.time()
        progress.firstCall = False
    else:
        now = time.time()
        print " done in %.1fs." %(now - progress.deltaS)
        progress.deltaS = now
    t=" init:" + text + "..."
    print t,
    sys.stdout.flush()

progress.startS = 0
progress.deltaS = 0
progress.firstCall = True

def done():
    totalTime = time.time() - progress.startS
    endString = "Initialization complete in %.1f seconds" %(totalTime)
    progress( endString )
    progress.firstCall = True
    print ""

def deprecate():
    callingFunctionName = inspect.stack()[1][3]
    printError( "%s is deprecated."%callingFunctionName )

class SuccessfulCancel(BaseException):
    def __init__(self, *args): 
        BaseException.__init__(self, *args)
        
def runKeyboardInterruptable(target, *args, **kwargs ):
    """Run a target command such that it will respond to a keyboard interrupt.
    Target command must be cancellable meaning that it responds to the
    cancel() command.  If it doesn't, this needs to be fixed on the C++ side."""
    kit = KeyboardInterruptable.KeyboardInterruptable(targetcmd=target, 
              cancelException=carma.util.CancelException, args=args, 
              kwargs=kwargs )
    try:
        kit.start()
    except Exception, ex:
        print 'Caught exception on dispatch!!'
        print ex
        done = True
    keyboardInterrupted = False
    done = False
    try:
        while not done:
            try:
                done = kit.doneWaiting()
            except KeyboardInterrupt:
                print "\nCancelling %s..."%target.__name__
                cancel()
                keyboardInterrupted = True
            except Exception, ex:
                print ex
                done = True
        if not kit.cleanFinish:
            print kit.getExceptionInfo()
        return kit.ret
    except Exception, ex:
        print ex
        done = True
    finally:
        kit.join()
        if kit.successfulCancel: 
            print "%s successfully cancelled" %target.__name__
            raise SuccessfulCancel
        if keyboardInterrupted and done:
            # The calling function didn't terminate from cancel
            # so reraise the interrupt.
            raise KeyboardInterrupt
       
#-------------------------Multiple subarray helper-----------------------------
def multiSubarray(methodName, subarray, *args):
    """This is a helper to send commands to multiple subarray controllers.
    Parameters:
     methodName: the subarrayController DO method name
     subarray: SCI1, SCI2, BOTH or DEFAULT. DEFAULT will give the
       subarray of the sac, or sci1 if a sciall sac.
     args: argument list for the method"""
    def _execMethod(subarrayRef, methodName, args):
        "Helper method to actually execute a method on a single subarray" 
        # The argument description is a list of lists containing the types of 
        # all of the arguments for a given method, the return types, and the
        # exception types. The descriptions are found in all the interfaces
        # that are inherited by SubarrayControl (in SubarrayControl.idl).    
        k       = "_d_"+methodName
        root    =   SubarrayControl_idl._0_carma
        #print "Method:", methodName, "args:", args
        if k in root.control.SubarrayControl. __dict__:
            argDesc = root.control.SubarrayControl.__dict__[k]
        # Drives is the first of the inherited interfaces
        elif k in root.control.Drives. __dict__:
            argDesc = root.control.Drives.__dict__[k]
        elif k in root.control.OpticalTelescope. __dict__:
            argDesc = root.control.OpticalTelescope.__dict__[k]
        elif k in root.control.SignalPath. __dict__:
            argDesc = root.control.SignalPath.__dict__[k]
        elif k in root.util.PhaseSwitching. __dict__:
            argDesc = root.util.PhaseSwitching.__dict__[k]
        else :
            m = "Could not get argument descriptor list for \'"+methodName+"\'"
            print m
            raise Exception, m 
        # Check that all the arguments have been passed           
        if len(argDesc[0]) != len(args) :
            plural = ""
            if len(argDesc[0]) > 1: plural = "s"
            m = "%d argument%s required " %(len(argDesc[0]), plural)
            m += "but only %d given for method %s" %(len(args), methodName)
            print "ERROR:", m 
            raise Exception, m
        # Execute the remote method
        return SubarrayControl_idl._omnipy.invoke(subarrayRef, methodName, 
                argDesc, args)

    if subarray == DEFAULT: 
        return _execMethod(s, methodName, args)
    elif subarray == SCI1:
        if s1 == None: raise Exception, "Invalid ref for sci1"
        return _execMethod(s1, methodName, args)
    elif subarray == SCI2:
        if s2 == None: raise Exception, "Invalid ref for sci2"
        return _execMethod(s2, methodName, args)
    elif subarray == BOTH:
        if s1 == None: raise Exception, "Invalid ref for sci1"
        if s2 == None: raise Exception, "Invalid ref for sci2"
        rtn1 = _execMethod(s1, methodName, args)
        rtn2 = _execMethod(s2, methodName, args)
        return rtn2
    else :
        raise Exception, "Invalid subarray: " + str(subarray)
        
#-----------------------------Noise source -------------------------------
def noiseon(delay=2.0, reference=False, subarray=DEFAULT) :
    """Turn on noise source, This also turns off the antenna RF amplifiers
    and will give a correlated signal of about 100%
    Parameters:
       delay - time in seconds to sleep to allow correlator command to 
               complete. Default: 2.0
       reference  - boolean to indicated is a refernece spectrum is about 
                    to be measured.  If True, source name will be changed to
                    NOISEREF, if False source name will be NOISE.
                    Default: False.
    """
    multiSubarray('noiseSource', subarray, True, reference)
    multiSubarray('rfPower', subarray, False)
    sleep(delay)  # Temporary - to allow for delay in correlator

def noiseoff(subarray=DEFAULT) :
    """Turn off noise source. Also turns on the RF amplifiers."""
    multiSubarray('noiseSource', subarray, False, False)
    multiSubarray('rfPower', subarray, True)

def noisePreset() :
    """Set noise source power level to default value stored in EEPROM."""
    s.noisePreset()

def noiseAtten(atten) :
    """Sets the noise source attenuation"""
    s.noiseAtten(atten)

#------------------------ Debugging, engineering --------------------------
def quadmodAtten(atten) :
    """Sets the quadmod attenuation"""
    s.quadmodAtten(atten)

def pswitchon(chan) :
    """Turn on phase switching.
    Parameter:
      chan: loberotator channel number, starting at one"""
    s.phaseSwitching(True, chan)

def pswitchoff(chan) :
    """Turn off phase switching.
    Parameter:
       chan: loberotator channel number, starting at one"""
    s.phaseSwitching(False, chan)

#--------------------- Command completion (wait) ----------------------
def numReady(antReady) :
    """Return number of antennas that are ready"""
    return len(antReady.ready)

def numNotready(antReady) :
    """Return number of antennas that are not ready"""
    return len(antReady.notready)

def allready(antReady) :
    """ReturnTrue if all antennas are ready"""
    return numNotready(antReady) == 0

def wait(item=TIME, ants=0, tmo=0, waiton=-2, precomment=None, postcomment=None,
        subarray=DEFAULT) :
    """Wait for procedure to complete, antennas to be on source or correlator
    bands to be configured.
    A cancel in another sac will break out of the wait.
    A wait for TRACK returns after one monitor cycle if the noise
    source is on.
    Parameters:
     item: An enumeration, such as INTEG, TRACK, etc
      TIME means to wait for a time interval as specified by tmo;
      this is the default.
     ants: A single or list of antennas; zero is all antennas. Bands numbers
      can also be used when waiting on CORR item with the same semantics.
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY) or NONE.
      A specific number of antennas is signified by a positive integer,
      and 'all except' by a negative integer.
      Defaults to -2.
    Return value contains a list of the ready antennas or corr bands and a list
    of the not ready antennas or corr bands. An InvalidMonitorDataException is
    thrown when there are 24 consecutive invalid monitor points (these are used
    internally to see if the wait condition is complete).
    Example usage:
     r = wait(TRACK, waiton=ANY)
     print r.ready
      [3,5] #ants 3 and 5 are ready
     print r.notready
      [1,2,4,6] #ants 1,2,4, and 6 are not ready
     print "Num ants ready::", len(r.ready)
     fini = allready(r)
     To just sleep for 5.4 seconds:
     wait(tmo=5.4)"""
    if tmo > carmaIni.CORBA_CLIENT_CALL_TIMEOUT_S:
        warn = "wait: Timeout (%ds) is greater than max CORBA timeout (%ds)."
        warn += "\n\tThrottling to %ds."
        warn = warn%( tmo, 
                      carmaIni.CORBA_CLIENT_CALL_TIMEOUT_S, 
                      carmaIni.CORBA_CLIENT_CALL_TIMEOUT_S - 10 )
        printWarning( warn )
        tmo = carmaIni.CORBA_CLIENT_CALL_TIMEOUT_S - 10
            
    return runKeyboardInterruptable( _wait, item, ants, tmo, waiton, 
                                     precomment, postcomment, subarray )

def _wait(item, ants, tmo, waiton, precomment, postcomment, subarray) :
    antlist = helpers.makeList(ants)
    if waiton == None:                 
        return carma.control.AntennaReady(antlist, [])
    count = 0
    if type(waiton) == int:
        count = waiton
        waiton = COUNT
        if min(antlist) > 0 and count < 0:
            numants = len(antlist)
            if numants <= abs(count):
                if ( item == CORR ) :
                    m = "The number of astrobands specified (" + str(numants) + ")"
                else :
                    m = "The number of antennas specified (" + str(numants) + ")"
                m += " is less than or equal to the \'all except\'"
                m += " number specified"
                m += " in the waiton argument (" + str(count) + ")"
                m += " which would result in no waiting."
                # We could throw here, but will fix instead
                if ( item == CORR ) :
                    m += " Will change to wait for ANY astrobands."
                else :
                    m += " Will change to wait for ANY antennas."
                commandlog(m)
                print m
                waiton = ANY
    rtdComment(precomment, subarray)
    if (subarray == BOTH) and (tmo > 0):
        t0 = time.time()
        ret = multiSubarray('wait', SCI2, item, antlist, tmo, waiton, count)
        rtdComment(postcomment, subarray=SCI2)
        tmo = tmo - (time.time()-t0)
        if tmo < 0: tmo = 0
        ret = multiSubarray('wait', SCI1, item, antlist, tmo, waiton, count)
        rtdComment(postcomment, subarray=SCI1)
    else:
        ret = multiSubarray('wait', subarray, item, antlist, tmo, waiton, count)
        rtdComment(postcomment, subarray)
    return ret

#------------------------- Calibration, integration ------------------------
def cal(state, ants=0, tmo=15, waiton=ALL, subarray=DEFAULT, setAttens=True) :
    """Set cal wheel position for the requested antennas and wait for position.
    Parameter:
     state: such as carma.antenna.common.CalibratorControl.AMBIENT
     ants: A single or list of antennas; zero is all antennas
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY), or NONE
    Return value contains a list of the ready antennas and a list
    of the not ready antennas.
    See abs and sky for more common usage"""
    #print "cal subarray =", subarray
    antlist = helpers.makeList(ants)
    # This must be started first so that the atten changes don't confuse things
    multiSubarray('cal', subarray, state, antlist)
    is1cm = lofreq(subarray) < 50
    if is1cm:
        # Make sure that the last AMB psys seen by the pipeline is not
        # erroneously affected by changing the atten too quickly.
        # If we are going to the SKY, wait an extra two seconds before 
        # changing the attenuation.
        if state == SKY and setAttens: wait(TIME, tmo=2.0, subarray=subarray)
        # 1cm, need to set attens
        if setAttens :
            for a in makeAntList(antlist, subarray) :
                mp = "Control.Antenna%d.ifAttenAmb1" %a
                try:
                    atten = queryDouble(mp, retries=2)
                except:
                    atten = 8
                    m = "Cannot get valid antenna IF atten on ambient"
                    m += "for C%d from the monitor system" %a
                    print m
                    commandlog(m)
                if state == SKY: 
                    atten -= 8
                    if atten <= 0:
                        if atten < 0:
                            m = "Cannot set sky atten to %.1fdB" %atten
                            m += " for C%d; setting to 0dB" %a 
                            print m
                            commandlog(m)
                        else:
                            m = "Attenuation is too low on C%d," %a
                            m += " indicating a potential low power issue"
                            print m
                            commandlog(m)
                        
                        m = "A possible cause is that the last time"
                        m += " the attenuation was set with a \n"
                        m += "tsys(ifsetup=True) the power level was "
                        m += "too low to use an attenuation > 8 dB.\n"
                        m += "You can try:\n"
                        m += "  - Another tsys(ifsetup=True)\n"
                        m += "  - Reporting it as a hardware error to the "
                        m += "hardware group."
                        print m  
                        atten = 0
                ifnum = 1
                antennaIFatten(atten, ifnum, invalidateTsys=False, ants=a, 
                               subarray=subarray)
                #print "Setting attenation for C%d:" %(a), atten
    return wait(CAL, antlist, tmo, waiton, subarray=subarray)

def amb(ants=0, tmo=15, waiton=ALL, subarray=DEFAULT, setAttens=True) :
    """Put in the absorber for the requested antennas and wait for position.
    Parameter:
     ants: A single or list of antennas; zero is all antennas
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY), or NONE
    Return value contains a list of the ready antennas and a list
    of the not ready antennas.
    """
    antlist = helpers.makeList(ants)
    return cal(carma.antenna.common.CalibratorControl.AMBIENT, antlist, 
        tmo, waiton, subarray=subarray, setAttens=setAttens)

def sky(ants=0, tmo=15, waiton=ALL, subarray=DEFAULT, setAttens=True) :
    """Take out the absorber for the requested antennas and wait for position.
    Parameters:
     ants: A single or list of antennas; zero is all antennas
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY), or NONE
    Return value contains a list of the ready antennas and a list
    of the not ready antennas.
    """
    antlist = helpers.makeList(ants)
    return cal(carma.antenna.common.CalibratorControl.SKY, antlist, tmo, 
                waiton, subarray=subarray, setAttens=setAttens)

def caltimer(ants=0, reps=1, tmo=15) :
    """Moves the ambient load and prints out how long it takes to report that
    it is in position. The command assumes that it is starting from 
    a sky position then puts in the ambient load and then goes back to sky. 
    Multiple repetitions of this loop can be requested.
    Parameters:
     ants: A single or list of antennas; zero is all antennas
     reps: Number of repetitions; default is 1
     tmo: Timeout value in seconds; default is 15
    """
    if ants == 0: ants = currentAntennaNumbers()
    antlist = helpers.makeList(ants)
    def looper(a, t, skypos):
        if skypos : label = "SKY"
        else :      label = "AMBIENT"
        print label + " started"
        t0 = time.time()
        timeout = t0 + t
        if skypos: state = carma.antenna.common.CalibratorControl.SKY
        else:      state = carma.antenna.common.CalibratorControl.AMBIENT
        s.cal(state, a)
        keepGoing = True
        while keepGoing:
            t = time.time()
            res = wait(CAL, a, timeout-t, ANY)
            #print "RES:", res.ready, res.notready
            t = time.time()
            r = res.ready
            a = res.notready
            if len(r) > 0:
                 m = "  %4.2f: " %(t-t0)
                 prefix = ""
                 for i in r:
                      m += prefix + "C%d" %i
                      prefix = ", "
                 print m
            if t >= timeout:
                keepGoing = False
                if len(a) > 0:
                    m = "  did not complete: " 
                    prefix = ""
                    for i in a:
                        m += prefix + "C%d" %i
                        prefix = ", "
                    print m
                    print "  Timeout " + label 
                    return 
            if len(a) == 0: keepGoing = False                   
        print "  Completed " + label     
    for r in range(reps) :
        looper(a=antlist, t=tmo, skypos=False)
        looper(a=antlist, t=tmo, skypos=True)

def integrate(integTime=10, reps=6, ants=0, antwait=-2, tmo=500, 
        gap=0.0, science=True,
        subarray=DEFAULT) :
    """Waits for antennas to be acquired (default), then starts
    an integration and waits for it to complete.
    A cancel in another sac will break out of either wait.
    Parameters:
     integTime: integration time for a single record, in seconds.
      Will be rounded to a multiple of 0.5 seconds. Any integration
      times less than 2 seconds should be pre-approved by Nikolaus Volgenau
      because of the resultant high data rate and volume.
     reps: number of records to repeat, must be >= 1; default=6.
     ants: A single or list of antenna numbers; zero is all antennas
     antwait: All to be complete (ALL), or just the first (ANY) or NONE.
      A specific number of antennas is signified by a positive integer,
      and 'all except' by a negative integer.
      Default is -2.
     tmo: Timeout value for tracking; zero inhibits timeout, default 500sec.
     gap: The gap (in seconds) between consecutive integrations. Default=0.
      Finite values used for OTF mosaicking.
     science: Record science data for this integration (default=True).
      Use False for system functions such as pointing, focus, etc that use
      integration visibilities or selfcal values."""
    if type(reps) != int:
        m = "integrate(integTime, reps): reps must be an integer"
        commandlog(m)
        raise Exception, m
    if reps <= 0:
        m = "Infinite integration not allowed"
        commandlog(m)
        raise Exception, m
    if antwait != NONE:
        c1 = "Waiting for antennas to acquire source"
        c2 = "Antennas acquired"
        wait(TRACK, ants, tmo, antwait, precomment=c1, postcomment=c2,
                subarray=subarray)

    cblist = makeCorrBandList(0)
    # If band is offline, don't wait for it.
    if ( len( cblist ) != 0 ) :
        c1 = "Waiting for previous astroband commands to complete..."
        c2 = "Correlator band configuration complete"
        wait(CORR, cblist, 10, ALL, precomment=c1, postcomment=c2, 
                subarray=subarray)
    rtdComment("Integrating...", subarray=subarray) 
    multiSubarray('integrate', subarray, integTime, reps, gap, science)
    rtn = wait(INTEG, postcomment="Integration complete" ,subarray=subarray)
    #print "Integration complete, itime=", integTime
    return rtn

def qinteg(integTime=10, reps=6,        
           gap=0.0, science=True,
           subarray=DEFAULT) :
    """Quick version of integrate() that does not wait for
    antennas to be acquired or for integration completion.
    Can be used from keyboard; should not be used in scripts.
    Documentation: see integrate, except reps < 0 will integrate forever"""
    multiSubarray('integrate', subarray, integTime, reps, gap, science)

#-------------------------- Move, offsets -------------------------------
def move(az=None, el=None, ants=0, tmo=500, waiton=ALL) :
    """Move antennas to requested az/el and wait for all to acquire.
    All of the parameters have defaults, and it the az or el are not specified
    then they are unchanged. Examples:
        move(10, 30)  # az=10, el=30
        move(44)      # az=44, el unchanged
        move(el=80)   # el=80, az unchanged
    A cancel in another sac will break out of the wait.
    Parameters:
     az: azimuth in degrees
     el: elevation in degrees
     ants: A single or list of antenna numbers; zero is all antennas
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY),
       NONE or an integer.
       When the value is an integer it is used as the number
       of antennas to wait on. A negative value means "all except".
       The default is ALL.
    Return value contains a list of the ready antennas and a list
    of the not ready antennas."""
    antlist = helpers.makeList(ants)
    if az == None and el == None : return
    if   az == None : s.moveEl(el, antlist)
    elif el == None : s.moveAz(az, antlist)
    else :            s.move(az, el, antlist)
    return wait(TRACK, antlist, tmo, waiton)

def qmove(az=None, el=None, ants=0) :
    """Quick version of move() that does not wait for completion.
    Can be used from keyboard; should not be used in scripts.
    Documentation: see move"""
    move(az, el, ants, waiton=NONE)

def offset(az, el, ants=0, tmo=15, waiton=NONE, subarray=DEFAULT) :
    """Offset antennas from nominal source position and wait for
    antennas to acquire. The az and el can be a list of offsets; in this
    case the length of those lists and the antenna list must be the same.
    The azimuth offset is specified as an arc on the sky, not a coordinate
    offset. A cancel in another sac will break out of the wait.
    The offsets are automatically set to zero on the invocation of a track()
    command.
    See also incoffset, track
    Parameters:
     az: azimuth offset in arcminutes
     el: elevation offset in arcminutes
     ants: A single or list of antenna numbers; zero is all antennas
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY) or NONE.
      A specific number of antennas is signified by a positive integer,
      and 'all except' by a negative integer.
      Default is NONE, allowing the wait to typically be done in the
      subsequent integrate command.
    Return value contains a list of the ready antennas and a list
    of the not ready antennas."""
    if subarray == BOTH:
        raise Exception, "Can't do offset on BOTH subarrays"
    antlist = helpers.makeList(ants)
    azlist  = helpers.makeList(az)
    ellist  = helpers.makeList(el)
    antlen  = len(antlist)
    azlen   = len(azlist)
    ellen   = len(ellist)
    sameErr = " must be the same"
    if (azlen != ellen) :
        st = "The number of az(%d) and el(%d)%s" %(azlen, ellen, sameErr)
        raise Exception, st
    if (azlen > 1) and (azlen != antlen) :
        st = "The number of ants(%d)%s and az/el(%d)" %(antlen, sameErr, azlen)
        raise Exception, st
    if azlen == 1 :
        multiSubarray('offset', subarray, azlist[0], ellist[0], antlist)
        #print "offset(%.2f, %.2f, %s)" %(azlist[0], ellist[0], antlist)
    else :
        for i in range(antlen) :
            multiSubarray('offset', subarray, 
                    azlist[i], ellist[i], [antlist[i]])
            #print "offset(%.2f, %.2f, %s)" %(azlist[i], ellist[i], antlist[i])
    return wait(TRACK, antlist, tmo, waiton, subarray=subarray)

def pointstatus( pos, ants=0 ) :
    """Set the single-dish point status.
    Parameters:
     pos:  Pointing position ONSRC means on source, OFFSRC means off
           source (i.e. pointed at emission-free reference position).
     ants: A single or list of antenna numbers; zero is all antennas"""
    antlist = helpers.makeList( ants )
    s.pointStatus( pos, antlist )

def mountoffset(az, el, ant, tmo=15, waiton=ALL, subarray=DEFAULT) :
    """Change the mount offset for a single antenna and wait for
    it to acquire. The azimuth offset is specified as an arc on the sky,
    not a coordinate offset. A cancel in another sac will break out of the wait.
    See also incmountoffset.
    Parameters:
     az: azimuth offset in arcminutes
     el: elevation offset in arcminutes
     ant: An antenna number
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: the condition to wait on (see wait), defaulting to ALL
    Return value contains a list of the ready antenna and a list
    of the not ready antenna."""
    if subarray == BOTH:
        raise Exception, "Can't do mountoffset on BOTH subarrays"
    multiSubarray('mountOffset', subarray, az, el, ant)
    return wait(TRACK, ant, tmo, waiton, subarray=subarray)

def restoreMountoffset(ant, subarray=DEFAULT):
    """Restore mount offsets from state values in monitor system.
    This routine is used for state restoration.
    See also mountoffset.
    Parameters:
     ant: Antenna to restore mount offsets for.
    """
    mpPrefix = "Control.Antenna%d."%ant
    azoff = queryDouble( mpPrefix + "azimuthMountOffset", 24 )
    eloff= queryDouble( mpPrefix + "elevationMountOffset", 24 )
    mountoffset(azoff, eloff, ant, tmo=1, waiton=NONE, subarray=subarray)

def zeroMountOffsets(ants=0, tmo=15, subarray=DEFAULT) :
    """Set the mount offsets to zero and waits for acquisition.
    Parameter:
      ants: an antenna number or list of antenna numbers;
        zero (the default) means use all antennas in current subarray.
      tmo: Timeout value in seconds; zero inhibits timeout"""
    if subarray == BOTH:
        raise Exception, "Can't do zeroMountOffset on BOTH subarrays"
    alist = helpers.makeList(ants)  # Retains zero
    antlist = makeAntList(ants)
    for iant in antlist:
        multiSubarray('mountOffset', subarray, 0.0, 0.0, iant)
    return wait(TRACK, alist, tmo, ALL, subarray=subarray)

def _incoffset(az, el, ants, tmo=15, waiton=ALL, subarray=DEFAULT,
               isMountoffset=False) :
    """This is a helper function for both incoffset and incmountoffset."""
    if isMountoffset: baseFuncName = "mountoffset"
    else:             baseFuncName = "offset"
    funcName = "inc" + baseFuncName
    if subarray == BOTH:
        raise Exception, "Can't do %s on BOTH subarrays" %funcName
    antlist = makeAntList(ants)
    azlist  = helpers.makeList(az)
    ellist  = helpers.makeList(el)
    antlen  = len(antlist)
    azlen   = len(azlist)
    ellen   = len(ellist)
    sameErr = " must be the same"
    if (azlen != ellen) :
        st = "%s: Length of az(%d) and el(%d) offsets%s" \
             %(funcName, azlen, ellen, sameErr)
        raise Exception, st
    if (azlen > 1) and (azlen != antlen) :
        st = "%s: Length of ants(%d)%s as az/el(%d)" \
             %(funcName, antlen, sameErr, azlen)
        st += " or az & el must be a single value."
        raise Exception, st
    elif (azlen == 1) and (antlen > 1):
        azlist = list()
        ellist = list()
        for a in antlist :
            azlist.append(az)
            ellist.append(el)
    for i in range(len(antlist)) :
        a    = antlist[i]
        mp = device.Carma(antlist[i]).getName() + ".AntennaCommon.Drive.Point."
        if (isMountoffset): mp += "mountOffset"
        else:               mp += "offset"
        azo = queryDouble(mp + "Az", 10)
        elo = queryDouble(mp + "El", 10)
        tazo = azo+azlist[i]
        telo = elo+ellist[i]
        if (isMountoffset):
            mountoffset(tazo, telo, a, waiton=NONE, subarray=subarray)
        else:
            offset(tazo, telo, a, waiton=NONE, subarray=subarray)
        #print "Doing: %s(%.2f, %.2f, %s)" %(funcName, tazo, telo, a)
    return wait(TRACK, antlist, tmo, waiton, subarray=subarray)

def _incoffsetHelp(isMountoffset=False) :
    """This is a helper function for building the help (doc string) for
    both incoffset and incmountoffset."""
    if isMountoffset: funcName = "mountoffset"
    else:             funcName = "offset"
    h = \
      """Increment the %ss, then apply to the nominal source position, and then
      wait for antennas to acquire. The az and el can be a list of incremental
      offsets; in  this case the length of those lists and the antenna list must
      be the same. The azimuth offset is specified as an arc on the sky, not a
      coordinate offset. A cancel in another sac will break out of the wait.
      See also %s
      Parameters:
      az: incremental azimuth offset in arcminutes
      el: incremental elevation offset in arcminutes
      ants: A single or list of antenna numbers; zero is all antennas
      tmo: Timeout value in seconds; zero inhibits timeout; default=15s
      waiton: All to be complete (ALL), or just the first (ANY) or NONE.
      A specific number of antennas is signified by a positive integer,
      and 'all except' by a negative integer.
      Default is ALL, causing the command to block until all antennas
      are tracking again.
      Return value contains a list of the ready antennas and a list
      of the not ready antennas.""" %(funcName, funcName)
    return h
 	  	 
def incmountoffset(az, el, ants, tmo=15, waiton=ALL, subarray=DEFAULT) :
    """To be filled in programmatically."""
    return _incoffset(az, el, ants=ants, tmo=tmo, waiton=waiton,
                      subarray=subarray, isMountoffset=True)
# Setup help for incmountoffset command
incmountoffset.__doc__ = _incoffsetHelp(isMountoffset=True)

def incoffset(az, el, ants, tmo=15, waiton=ALL, subarray=DEFAULT) :
    """To be filled in programmatically."""
    return _incoffset(az, el, ants=ants, tmo=tmo, waiton=waiton,
                      subarray=subarray, isMountoffset=False)
# Setup help for incoffset command
incoffset.__doc__ = _incoffsetHelp(isMountoffset=False)

def offsetAz(az, ants=0, tmo=15, waiton=NONE) :
    """Offset antennas in azimuth from nominal source position and wait for
    antennas to acquire.
    A cancel in another sac will break out of the wait.
    Parameters:
     az: azimuth offset in arcminutes
     ants: A single or list of antenna numbers; zero is all antennas
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY),
      a specific number (COUNT) or NONE. Default is COUNT
     waiton: All to be complete (ALL), or just the first (ANY) or NONE.
      A specific number of antennas is signified by a positive integer,
      and 'all except' by a negative integer.
      Default is NONE, allowing the wait to typically be done in the
      subsequent integrate command.
    Return value contains a list of the ready antennas and a list
    of the not ready antennas."""
    if subarray == BOTH:
        raise Exception, "Can't do offsetAz on BOTH subarrays"
    antlist = helpers.makeList(ants)
    s.offsetAz(az, antlist)
    return wait(TRACK, antlist, tmo, waiton)

def offsetEl(el, ants=0, tmo=15, waiton=NONE) :
    """Offset antennas in elevation from nominal source position and wait for
    antennas to acquire.
    A cancel in another sac will break out of the wait.
    Parameters:
     el: elevation offset in arcminutes
     ants: A single or list of antenna numbers; zero is all antennas
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY) or NONE.
      A specific number of antennas is signified by a positive integer,
      and 'all except' by a negative integer.
      Default is NONE, allowing the wait to typically be done in the
      subsequent integrate command.
     count: the number of antennas to wait on when COUNT is used.
      A negative number means wait for "all except".
      Default is -ALL.
    Return value contains a list of the ready antennas and a list
    of the not ready antennas."""
    if subarray == BOTH:
        raise Exception, "Can't do offsetEl on BOTH subarrays"
    antlist = helpers.makeList(ants)
    s.offsetEl(el, antlist)
    return wait(TRACK, antlist, tmo, waiton)

def equatOffset(ra, dec, ants=0, phaseCenter=None, whileIntegrating=False,
        tmo=15, waiton=NONE, subarray=DEFAULT) :
    """Offset antennas in equatorial coordinates (ra/dec) from nominal
    source position and wait for antennas to acquire.
    A cancel in another sac will break out of the wait.
    Parameters:
     ra: ra offset in arcminutes on the sky
     dec: dec offset in arcminutes
     ants: A single or list of antenna numbers; zero is all antennas
     phaseCenter: with default value of None, phase center tracking is determined
         by the ants parameter (0=True, other=False); if this parameter is set
         to True or False then it determines phase center tracking
     whileIntegrating: Allow while integrating; default=False
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY) or NONE.
      A specific number of antennas is signified by a positive integer,
      and 'all except' by a negative integer.
      Default is NONE, allowing the wait to typically be done in the
      subsequent integrate command.
    Return value contains a list of the ready antennas and a list
    of the not ready antennas."""
    if subarray == BOTH:
        raise Exception, "Can't do equatOffset on BOTH subarrays"
    antlist = helpers.makeList(ants)
    if phaseCenter == None :
        if antlist[0] == 0 : pcTracking = True
        else :               pcTracking = False
    else :
        pcTracking = phaseCenter
    multiSubarray('equatOffset', subarray, ra, dec, antlist, 
            pcTracking, whileIntegrating)
    return wait(TRACK, antlist, tmo, waiton, subarray=subarray)

def _getoffsets(isMountoffset):
    """Get offsets for all 23 ants as a 23 element list of two element lists
    of az/el ofsets"""
    mplist = list()
    for i in range(23) :
        a    = i+1
        mp = device.Carma(a).getName() + ".AntennaCommon.Drive.Point."
        if (isMountoffset): mp += "mountOffset"
        else:               mp += "offset"
        mpaz = mp + "Az"
        mpel = mp + "El"
        mps  = [mpaz, mpel]
        mplist.append(mps)
    r = queryMpValues(mplist, nothrow=True)
    if False:
        for i in range(23):
            if r[i][0] == None: astr = " None"
            else :              astr = "%5.2f" %r[i][0]
            if r[i][1] == None: estr = " None"
            else :              estr = "%5.2f" %r[i][1]
            print "%2d: %s  %s" %(i+1, astr, estr)
    return r
def getoffsets():
    """Get offsets for all 23 ants as a 23 element list of two element lists
    of az/el ofsets. A value of None is returned for invalid MPs"""
    r = _getoffsets(isMountoffset=False)
    return r
def getmountoffsets():
    """Get offsets for all 23 ants as a 23 element list of two element lists
    of az/el ofsets. A value of None is returned for invalid MPs"""
    r = _getoffsets(isMountoffset=True)
    return r
 	  	 
def stow(ants=0, tmo=200, waiton=-2, position=ZENITH, subarray=DEFAULT) :
    """Stow the antennas at the given position and wait for all to acquire.
    A cancel in another sac will break out of the wait.
    Parameters:
     ants: A single or list of antenna numbers; zero is all antennas
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY) or NONE.
      A specific number of antennas is signified by a positive integer,
      and 'all except' by a negative integer.
     position:  One of ZENITH, SERVICE, SAFE.  Default: ZENITH
    Return value contains a list of the ready antennas and a list
    of the not ready antennas."""
    antlist = helpers.makeList(ants)
    multiSubarray('stow', subarray, position, antlist)
    return wait(TRACK, antlist, tmo, waiton, subarray=subarray)

def safe(ants=0) :
    """Move the antennas the SAFE position and wait for all to acquire.
    A cancel in another sac will break out of the wait.
    An exception will be thrown if setSafeRange() has not first been called on
    each antenna in the antenna list.
    Parameters:
     ants: A single or list of antenna numbers; zero is all antennas
    Return value contains a list of the ready antennas and a list
    of the not ready antennas."""
    # If in E-configuration, use etrack
#    if (etrack.IS_E_CONFIGURATION):
#        import  etrack
##        return basicEtrack(ants=ants,source=ZENITH)
#    else:
    return stow( ants, 200 , ALL , SAFE );

def service(ants=0, tmo=200, waiton=-2) :
    """Move the antennas the SERVICE position and wait for all to acquire.
    A cancel in another sac will break out of the wait.
    Parameters:
     ants: A single or list of antenna numbers; zero is all antennas
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY) or NONE.
      A specific number of antennas is signified by a positive integer,
      and 'all except' by a negative integer.
    Return value contains a list of the ready antennas and a list
    of the not ready antennas."""
    return stow( ants, tmo, waiton, SERVICE );

def stop(ants=0, subarray=DEFAULT) :
    """Stop the antennas. Note that for the BIMA antennas this resets
    any DISABLED condition, such as those resulting from stalls.
    Parameters:
     ants: A single or list of antenna numbers; zero is all antennas"""
    antlist = helpers.makeList(ants)
    multiSubarray('stop', subarray, antlist)

def setSafeRange( azLow, azHigh, elLow, elHigh, ants, subarray=DEFAULT) :
    """Set the safe azimuth and elevation ranges over which an antenna
    cannot collide with a neighboring antenna (in close-packed array
    configurations).
    Parameters:
     azLow: Low azimuth safe range, in degrees. No default.
     azHigh: High azimuth safe range, in degrees. No default.
     elLow: Low elevation safe range, in degrees. No default.
     elHigh: High elevation safe range, in degrees. No default.
     ants: A single or list of antenna numbers; zero is all antennas
    See also safe().
    """
    antlist = helpers.makeList(ants)
    multiSubarray('setSafeRange', subarray, azLow, azHigh, elLow, elHigh, 
            antlist)

setSafeRange.__argsMpContainer__ = 'Antenna'
setSafeRange.__argsMpNames__ = [ 'azSafeLow', 'azSafeHigh', 
                                 'elSafeLow', 'elSafeHigh' ]
    
def trackThreshold(threshold, ants=0, subarray=DEFAULT) :
    """Set the tracking threshold for a single or group of antennas.
    The tracking threshold is the sky tracking error in beamwidths
    that is used to determine if the antenna state is TRACK
    Parameters:
     threshold: tracking threshold in beamwidths
     ants: A single or list of antenna numbers; zero (default)
     is all antennas
    See also trackThresholdOptical"""
    if subarray == BOTH:
        raise Exception, "Can't do trackThreshold on BOTH subarrays"
    antlist = helpers.makeList(ants)
    multiSubarray('trackThreshold', subarray, threshold, antlist)

def setInvalidationForMosaics(invalidate) :
    s.setInvalidationForMosaics(invalidate) 

def addPointing(opnt=False):
    """ Add another pointing to the current script.
           opnt : If True, redo optical pointing only (default: False)
                  Otherwise, redo radio and optical pointing

        Examples:
           addPointing()          -  redo optical and radio pointing
           addPointing(opnt=True) -  redo optical pointing only
    """

    # Check optical pointing 
    s.setScriptBool(odi.INDX_BOOL_LASTPOINT, False)
    if not opnt:
       s.setScriptBool(odi.INDX_BOOL_OPTRAD_VECTOR, False)
       
def addFluxcal():
    """ Add another primry flux calibrator observation to the observing script """
    # Overall
    i = s.getScriptInt(odi.INDX_INT_NOBS_FLUX) - 1
    if i < 0: i = 0
    s.setScriptInt(odi.INDX_INT_NOBS_FLUX, i)

    # Primary
    i = s.getScriptInt(odi.INDX_INT_NOBS_FLUXPRI) - 1
    if i < 0: i = 0
    s.setScriptInt(odi.INDX_INT_NOBS_FLUXPRI, i)

def addPol():
    """ Add another passband observation to the observing script """
    s.setScriptBool(odi.INDX_BOOL_LASTPOLARIZATION, False) 

def addPassband():
    """ Add another passband observation to the observing script """
    i = s.getScriptInt(odi.INDX_INT_NOBS_PB) - 1
    if i < 0: i = 0
    s.setScriptInt(odi.INDX_INT_NOBS_PB, i) 

def restoreTrackThreshold(ants=0, subarray=DEFAULT) :
    """Retrieve last tracking threshold from the monitor system and set.
    Used for state restoration.
    Parameters:
     ants: List of antennas; zero is the default."""
    subNo = subarrayNo
    if subarray == SCI2: subNo = 2
    toleranceMpName = "Control.Subarray%d.trackTolerance"%subNo
    tolerance = queryDouble(toleranceMpName, 24) # 24 retries (12 seconds)
    trackThreshold(tolerance, ants, subarray=subarray)

def trackThresholdOptical(threshold, ants=0) :
    """Set the tracking threshold for optical pointing
    for a single or group of antennas.
    The tracking threshold is the sky tracking error in arcseconds
    that is used to determine if the antenna state is TRACK
    Parameters:
     threshold: tracking threshold in arcseconds
     ants: A single or list of antenna numbers; zero (default)
           is all antennas
    Return: previous track threshold in beamwidths for last antenna
    See also trackThreshold"""
    SPEED_OF_LIGHT = 299792458.0 # m/s
    antlist = helpers.makeList(ants)
    if antlist[0] == 0: antlist = currentAntennaNumbers()
    flo = lofreq()
    t = 0
    for ant in antlist:
        antmp = "control.antenna%d" %ant
        d = queryDouble(antmp + ".diameter")
        t = queryDouble(antmp + ".trackTolerance")
        tbw = threshold*math.pi/(180*60*60)*flo*1e9*d/SPEED_OF_LIGHT
        #print "Threshold in bw:", tbw
        trackThreshold(tbw, ant)
    return t

# ---------------------------- Pads and Baselines ----------------------------
def pad( padNumber, ant, subarray=DEFAULT):
    """Assign antenna to a pad number.
    Parameters:
     padNumber: The pad number the antenna resides (or will reside) on.
     ant: carma antenna number (starting with 1)"""
    multiSubarray('pad', subarray, padNumber, ant)
pad.__argsMpContainer__ = 'Antenna'
pad.__argsMpNames__ = ['padNumber']


def padOffset(east, north, up, ant, subarray=DEFAULT):
    """Specify offset from pad nominal position for an antenna.
    Parameters:
     east: Easterly offset in mm.
     north: northerly offset in mm.
     up: Vertical offset in mm.
     ant: carma antenna number (starting with 1)"""
    multiSubarray('padOffset', subarray, east, north, up, ant)

padOffset.__argsMpContainer__ = 'Antenna'
padOffset.__argsMpNames__ = [ "PadOffset.%s"%mp for mp in ["east",
                                                           "north",
                                                           "up"] ]

def baselineSolutions(filename):
    """ Apply new baseline solutions from input antpos file to the system.  
    The new solutions will be remembered by the system via the state
    restoration subsystem.  
    Parameters: 
     filename: Name of file containing antenna positions.  May be an 
               absolute or relative path."""
    
    def getOtherSaCurrentAnts( ):
        """ Retrieve ants in other science subarrays. """
        if subarrayNo == 1:
            otherSa = Subarray.getSubarrayRef( 2 )
            otherSaAntAssignments = otherSa.getAntennaAssignments( )
        elif subarrayNo == 2:
            otherSa = Subarray.getSubarrayRef( 1 )
            otherSaAntAssignments = otherSa.getAntennaAssignments( )
        else:
            otherSa1Ants = Subarray.getSubarrayRef(1).getAntennaAssignments()
            otherSa2Ants = Subarray.getSubarrayRef(2).getAntennaAssignments()
            otherSaAntAssignments = otherSa1Ants + otherSa2Ants

        otherSaAnts = [ i.carmaAntennaNo for i in otherSaAntAssignments ]
        return otherSaAnts

    def toosmall( val ):
        if abs( val ) < 0.0001:
            return True
        else:
            return False

    import fileIOPython as fIOP

    # First determine which antennas will receive updates.
    antPosVals = fIOP.fileToTable( filename, ignoreEmpty=True, comment="#" )
    antsInThisSa = currentAntennaNumbers()
    antsInOtherScienceSa = getOtherSaCurrentAnts()
    antNo = 0
    validAnts = []
    invalidAnts = []
    validAntsInOtherScienceSa = []
    validAntsNotInThisSa = []
    for pos in antPosVals:
        antNo += 1
        x,y,z = float(pos[0]), float(pos[1]), float(pos[2])
        if toosmall(x) and toosmall(y) and toosmall(z):
            invalidAnts.append( antNo )
        else:
            if antNo in antsInOtherScienceSa:
                validAntsInOtherScienceSa.append( antNo )
            if antNo not in antsInThisSa:
                validAntsNotInThisSa.append( antNo )
            validAnts.append( antNo )

    if len( validAntsInOtherScienceSa ) > 0:
        rfValidAntsInOtherScienceSa = helpers.formatAsRanges( validAntsInOtherScienceSa )
        error = ""
        if len(rfValidAntsInOtherScienceSa) > 1:
            error += "Ants %s currently belong to other science subarrays. "%(rfValidAntsInOtherScienceSa)
            error += "Please remove them prior to rerunning this command."
        else:
            error += "Ant %s currently belongs to another science subarray. "%(rfValidAntsInOtherScienceSa)
            error += "Please remove it prior to rerunning this command."
        printError( error )
        return

        
    antNo = 0
    for pos in antPosVals:
        antNo += 1
        if antNo in invalidAnts:
            continue
        x,y,z = float(pos[0]), float(pos[1]), float(pos[2])
        baseOff = s.convertBaseline( x, y, z, antNo )

        if ( baseOff.antNo != antNo ): # Sanity check
            raise Exception,"Mismatched ant no."
        
        padOffset( round( baseOff.east, 2 ), 
                   round( baseOff.north, 2 ),
                   round( baseOff.up, 2 ),
                   antNo )

        s.setAntPosFilename( filename )

    rfValidAnts = helpers.formatAsRanges( validAnts )
    if ( rfValidAnts > 1 ):
        print "Solutions applied for ants %s."%(rfValidAnts)
    else:
        print "Solutions applied for ant %s."%(rfValidAnts)
    
    if len( invalidAnts ) > 0:
        rfInvalidAnts = helpers.formatAsRanges( invalidAnts )
        if ( len( rfInvalidAnts ) > 1 ):
            printWarning( "Antpos does NOT contain solutions for ants %s."%(rfInvalidAnts) )
        else:
            printWarning( "Antpos does NOT contain a solution for ant %s."%(rfInvalidAnts) )

    if len( validAntsNotInThisSa ) > 0:
        rfValidAntsNotInThisSa = helpers.formatAsRanges( validAntsNotInThisSa )
        warning = "Solutions were applied for "
        if len(rfValidAntsNotInThisSa) > 1:
            warning += "ants %s which do "%rfValidAntsNotInThisSa
        else:
            warning += "ant %s which does "%rfValidAntsNotInThisSa
        warning += "not belong to this subarray.  The new solutions "
        warning += "may not be reflected in the monitor system until these "
        warning += "ants are added back into a science subarray." 
        printWarning( warning )

def computeENU(filename) :
    """Convert a file containing antenna positions (antpos) to East, North & Up
    padoffsets. 
    Note this routine does NOT apply the new baseline solutions.  
    See also: baselineSolutions
    Parameters:
     filename: Name of file containing antenna positions.  May be an 
               absolute or relative path."""
   
    import fileIOPython as fIOP

    antPosVals = fIOP.fileToTable( filename, comment="#" )
    antNo = 0
    for pos in antPosVals:
        antNo += 1
        x,y,z = float(pos[0]), float(pos[1]), float(pos[2])
        baseOff = s.convertBaseline( x, y, z, antNo ) 
        
        if ( baseOff.antNo != antNo ): # Sanity check
            raise Exception,"Mismatched ant no." 
        
        print ('%3i: (%9.2f,%9.2f,%9.2f),' % (antNo, baseOff.east,
                                                     baseOff.north,
                                                     baseOff.up) )
    print "These values have NOT been applied to the system.  ",
    print "Use baselineSolutions for that purpose."

# --------------------- Pointing constants ---------------------
def tiltzeros(af, lr, ant, subarray=DEFAULT) :
    """Set tiltmeter zeros for a given antenna
    Parameters:
     af: Aft/forward tiltmeter zero, in arcminutes
     lr: Left/right tiltmeter zero, in arcminutes
     ant: carma antenna number (starting with 1)"""
    multiSubarray('tiltZeros', subarray, af, lr, ant)

tiltzeros.__argsMpContainer__ = 'Antenna'
tiltzeros.__argsMpNames__ = ["aftForwardTiltZero", "leftRightTiltZero"]

def focus( x, y, z, ant, subarray=DEFAULT):
    """Set antenna secondary focus values.
    If any value is set to NONE it is ignored, if all
    values are None, this routine is a no-op.
    Parameter:
        x: Focus in mm for x axis (may be None).
        y: Focus in mm for x axis (may be None).
        z: Focus in mm for x axis (may be None).
        ant: carma antenna number (starting with 1)"""
    if x != None:
        multiSubarray('focusX', subarray, x, ant )
    if y != None:
        multiSubarray('focusY', subarray, y, ant )
    if z != None:
        multiSubarray('focusZ', subarray, z, ant )

focus.__argsMpContainer__ = 'Antenna'
focus.__argsMpNames__ = ["focusX", "focusY", "focusZ"]

def opticalPointingConstants( az, el, sag, ant, subarray=DEFAULT):
    """Set pointing constants for the optical aperture.
    These sit directly on top of the mount model.
    Parameters:
        az: Azimuth offset in arcminutes
        el: Elevation offset in arcminutes
        sag: Elevation offset in arcminutes
        ant: carma antenna number (starting with 1)"""
    OPTICAL = carma.control.APERTURE_OPTICAL
    multiSubarray('aperturePointingConstants', subarray, 
                OPTICAL, az, el, sag, ant)

opticalPointingConstants.__argsMpContainer__ = 'Antenna'
opticalPointingConstants.__argsMpNames__ = \
    ["ApertureOptical.PointingConstants." + \
     mp for mp in ["azOffset", "elOffset","sag"]]

def dbugpc(ants=[1,2,4]):
    "Debug pointing constants. Remove after 01/01/2014"
    h = "Ant  %4s %4s %4s  %4s %4s %4s" %("az3","el3","sag3","az1","el1","sag1")
    print h
    for a in ants:
        mp   = "Drive.Point.Constants."
        ap   = "ApertureCoefficients3."
        az3  = queryCommonDouble(mp+ap+"crossElCollErr",a)
        el3  = queryCommonDouble(mp+ap+"elCollErr",a)
        sag3 = queryCommonDouble(mp+ap+"sag",a)
        ap   = "ApertureCoefficients2."
        az1  = queryCommonDouble(mp+ap+"crossElCollErr",a)
        el1  = queryCommonDouble(mp+ap+"elCollErr",a)
        sag1 = queryCommonDouble(mp+ap+"sag",a)
        m = "%3d  %4.1f %4.1f %4.1f  %4.1f %4.1f %4.1f" \
          %(a,az3, el3, sag3, az1, el1, sag1)
        print m

def radio1mmPointingConstants(az, el, sag, ant, subarray=DEFAULT):
    """Set pointing constants for the 1mm radio aperture.
    This sets the constants in the antennas as absolute corrections. 
    Note that these constants will be changed whenever the 3mm constants change
    as the 1mm constants are assumed to be dependent on the 3mm. If this is not
    the case, for example if a 3mm receiver is changed or moved in the dewar,
    then the 1mm constants should be redetermined independently.
    Parameters:
        az: Cross elevation offset in arcminutes
        el: Elevation offset in arcminutes
        sag: Elevation sag term in arcminutes
        ant: carma antenna number (starting with 1)
     See also: ovroMountPointingConstants, bimaMountPointingConstants,
     and szaMountPointingConstants, radio3mmPointingConstants"""

    R1MM    = carma.control.APERTURE_RADIO1MM
    s.aperturePointingConstants(R1MM, az, el, sag, ant)

radio1mmPointingConstants.__argsMpContainer__ = 'Antenna'
radio1mmPointingConstants.__argsMpNames__ = \
    ["Aperture1mm.PointingConstants." + \
     mp for mp in ["azOffset", "elOffset","sag"]]

def radio3mmPointingConstants( az, el, sag, ant, subarray=DEFAULT):
    """Set pointing constants for the 3mm radio aperture.
    This also adjusts the 1mm aperture constants which are
    relative to this 3mm aperture, by the difference from the previous 3mm
    radio aperture terms. When establishing the state of a new system, where the
    previous values may not be valid, restore the 1mm constants after the 3mm.
    Parameters:
        az: Azimuth offset in arcminutes
        el: Elevation offset in arcminutes
        sag: Elevation sag term in arcminutes
        ant: carma antenna number (starting with 1)
     See also: ovroMountPointingConstants, bimaMountPointingConstants,
     szaMountPointingConstants, and radio1mmPointingConstants"""
    
    mproot = "Control.Antenna%d.Aperture3mm.PointingConstants."  %ant
    azValid  = False
    elValid  = False
    sagValid = False
    numInvalid = 0
    try :
        azDelta  = az  - queryDouble(mproot+"azOffset") 
        azValid = True
    except:
        numInvalid += 1
    try :
        elDelta  = el  - queryDouble(mproot+"elOffset") 
        elValid = True
    except:
        numInvalid += 1
    try :
        sagDelta = sag - queryDouble(mproot+"sag") 
        sagValid = True
    except:
        numInvalid += 1
    allValid = azValid and elValid and sagValid
    #print "Deltas:", azDelta, elDelta, sagDelta

    R3MM    = carma.control.APERTURE_RADIO3MM
    multiSubarray('aperturePointingConstants',subarray, R3MM, az, el, sag, ant)

    if ant > 15:  return
    
    # Apply deltas to the 1mm aperture
    prefix = ""
    if not allValid:
        m = "Could not get the initial value"
        if numInvalid > 1: m += "s"
        m += " for the 3mm"
        if not azValid:
            m += prefix + " az"
            prefix = ","
        if not elValid:
            m += prefix + " el"
            prefix = ","
        if not sagValid:
            m += prefix + " sag"
        m += " term"        
        if numInvalid > 1: m += "s"
        m += "."
        print m
        m = "The 3mm offsets have been changed but the relative 1mm offsets\n"
        m += "will not be changed because of the missing initial 3mm values.\n"
        m += "This is a serious error "
        m += "that should be reported to the operations staff!!"
        print m
        return
        
    mproot = "Control.Antenna%d.Aperture1mm.PointingConstants." %ant 
    az1mm  = azDelta +  queryDouble(mproot+"azOffset") 
    el1mm  = elDelta +  queryDouble(mproot+"elOffset") 
    sag1mm = sagDelta + queryDouble(mproot+"sag") 
    R1MM    = carma.control.APERTURE_RADIO1MM
    #print "1mm constants:", az1mm, el1mm, sag1mm
    multiSubarray('aperturePointingConstants', subarray, R1MM, 
                        az1mm, el1mm,sag1mm, ant)

radio3mmPointingConstants.__argsMpContainer__ = 'Antenna'
radio3mmPointingConstants.__argsMpNames__ = \
    ["Aperture3mm.PointingConstants." + \
     mp for mp in ["azOffset", "elOffset", "sag"]]

def radio1cmPointingConstants(az, el, sag, ant, subarray=DEFAULT):
    """Set pointing constants for the 1cm radio aperture.
    This aperture is not relative to any other apertures
    (it sits directly on top of the mount model).
    Parameters:
        az: Azimuth offset in arcminutes
        el: Elevation offset in arcminutes
        sag: Elevation sag term in arcminutes
        ant: carma antenna number (starting with 1)
     See also: ovroMountPointingConstants, bimaMountPointingConstants,
     and szaMountPointingConstants"""

    R1CM    = carma.control.APERTURE_RADIO1CM
    multiSubarray('aperturePointingConstants',subarray, R1CM, az, el, sag, ant)

radio1cmPointingConstants.__argsMpContainer__ = 'Antenna'
radio1cmPointingConstants.__argsMpNames__ = \
    ["Aperture1cm.PointingConstants." + \
     mp for mp in ["azOffset", "elOffset", "sag"]]


def ovroMountPointingConstants( azEncoderOffset, elEncoderOffset,
                                axisNonOrthogonality,
                                axisVerticalityNorthSouth,
                                axisVerticalityEastWest, ant,
                                subarray=DEFAULT ):
    """Set mount pointing constants for 10m dishes. All the coefficients except
    the azEncoderOffset should remain constant within 0.1 arcmin. The 
    azEncoderOffset may change by a few arcmin on a configuration change.
    Parameters:
        azEncoderOffset: Azimuth encoder offset in arcmin.
        elEncoderOffset: Elevation encoder offset in arcmin.
        axisNonOrthogonality: Non-orthogonality of azimuth and elevation
          axes in arcmin.
        axisVerticalityNorthSouth: Azimuth verticality north/south in arcmin.
        axisVerticalityEastWest: Azimuth verticality east/west in arcmin.
        ant: carma antenna number (starting with 1)
        See also: radio1cmPointingConstants, radio3mmPointingConstants,
        radio1mmPointingConstants, and opticalPointingConstants"""
    multiSubarray('setOvroMountPointingConstants', subarray, 
                                    azEncoderOffset, elEncoderOffset,
                                    axisNonOrthogonality,
                                    axisVerticalityNorthSouth,
                                    axisVerticalityEastWest, ant )

ovroMountPointingConstants.__argsMpContainer__ = 'Ovro'
ovroMountPointingConstants.__argsMpNames__ = [ "azEncoderOffset",
    "elEncoderOffset", "axisNonOrthogonality", "northSouthAzAxisVerticality",
    "eastWestAzAxisVerticality"]

def bimaMountPointingConstants(azCoefficients, elCoefficients, ant,
        subarray=DEFAULT):
    """Set mount pointing constants for 6m dishes.
    Parameters:
        azCoefficients: List of azimuth model pointing coefficients.
        elCoefficients: List of elevation model pointing coefficients.
        ant: carma antenna number (starting with 1)"""
    multiSubarray('setBimaMountPointingConstants', subarray,
                     azCoefficients, elCoefficients, ant )

bimaMountPointingConstants.__argsMpContainer__  = 'Bima'
bimaMountPointingConstants.__argsMpNames__ = [
    [ ("apc%d"%(c)) for c in xrange(1, 10) ],
    [ ("epc%d"%(c)) for c in xrange(1, 10) ] ]


def szaMountPointingConstants( 
        azEncoderCountsPerTurn,     elEncoderCountsPerTurn,
        azMinEncoderCount,          azMaxEncoderCount,
        elMinEncoderCount,          elMaxEncoderCount,
        azEncoderZeroDegrees,       elEncoderZeroDegrees,
        haTiltDegrees, latTiltDegrees, elTiltDegrees,
        opticalXCollimationDegrees, opticalYCollimationDegrees,
        opticalFlexureSinDegrees,   opticalFlexureCosDegrees,
        radioXCollimationDegrees,   radioYCollimationDegrees,
        radioFlexureSinDegrees,     radioFlexureCosDegrees, ant,
        subarray=DEFAULT ) :
    """Set mount pointing constants for 3.5m dishes.
    Parameters:
    azEncoderCountsPerTurn     AZ encoder calibration (counts per turn)
    elEncoderCountsPerTurn     AZ encoder calibration (counts per turn)
    azMinEncoderCounts         Minimum AZ limit (encoder counts)
    azMaxEncoderCounts         Maximum AZ limit (encoder counts)
    elMinEncoderCounts         Minimum EL limit (encoder counts)
    elMaxEncoderCounts         Maximum EL limit (encoder counts)
    azEncoderZeroDegrees       AZ encoder zero (degrees)
    elEncoderZeroDegrees       EL encoder zero (degrees)
    haTiltDegrees              Hour-angle tilt (degrees)
    latTiltDegrees             Latitude tilt (degrees)
    elTiltDegrees              Elevation tilt (degrees)
    opticalXCollimationDegrees Optical collimation great-circle (x) angle (degrees)
    opticalYCollimationDegrees Optical collimation great-circle (y) angle (degrees)
    opticalFlexureSinDegrees   Optical flexure sin term (degrees per sin elevation)
    opticalFlexureCosDegrees   Optical flexure cos term (degrees per sin elevation)
    radioXCollimationDegrees   Radio collimation great-circle (x) angle (degrees)
    radioYCollimationDegrees   Radio collimation great-circle (y) angle (degrees)
    radioFlexureSinDegrees     Radio flexure sin term (degrees per sin elevation)
    radioFlexureCosDegrees     Radio flexure cos term (degrees per sin elevation)
        ant: carma antenna number (starting with 1)"""
    multiSubarray('setSzaMountPointingConstants', subarray,
            azEncoderCountsPerTurn,     elEncoderCountsPerTurn,
            azMinEncoderCount,          azMaxEncoderCount,
            elMinEncoderCount,          elMaxEncoderCount,
            azEncoderZeroDegrees,       elEncoderZeroDegrees,
            haTiltDegrees,              latTiltDegrees,         elTiltDegrees,
            opticalXCollimationDegrees, opticalYCollimationDegrees,
            opticalFlexureSinDegrees,   opticalFlexureCosDegrees,
            radioXCollimationDegrees,   radioYCollimationDegrees,
            radioFlexureSinDegrees,     radioFlexureCosDegrees, ant )

# These are function attributes and need to be outside the function scope!!!
szaMountPointingConstants.__argsMpContainer__ = 'Sza'
szaMountPointingConstants.__argsMpNames__ = [ "azEncoderCountsPerTurn", "elEncoderCountsPerTurn", "azMinEncoderCount", "azMaxEncoderCount", "elMinEncoderCount", "elMaxEncoderCount", "azEncoderZero", "elEncoderZero", "haTilt", "latTilt", "elTilt", "opticalXCollimation", "opticalYCollimation", "opticalFlexureSin", "opticalFlexureCos", "radioXCollimation", "radioYCollimation", "radioFlexureSin", "radioFlexureCos"]

def szaEncoderLimits( 
        azMinEncoderCount,          azMaxEncoderCount,
        elMinEncoderCount,          elMaxEncoderCount, ant,
        subarray=DEFAULT ) :
    """Set encoder limits for 3.5m dishes.
    Parameters:
    azMinEncoderCounts         Minimum AZ limit (encoder counts)
    azMaxEncoderCounts         Maximum AZ limit (encoder counts)
    elMinEncoderCounts         Minimum EL limit (encoder counts)
    elMaxEncoderCounts         Maximum EL limit (encoder counts)
        ant: carma antenna number (starting with 1)"""
    multiSubarray('setSzaEncoderLimits', subarray,
            azMinEncoderCount,          azMaxEncoderCount,
            elMinEncoderCount,          elMaxEncoderCount, ant )

# These are function attributes and need to be outside the function scope!!!
szaEncoderLimits.__argsMpContainer__ = 'Sza'
szaEncoderLimits.__argsMpNames__ = [ "azMinEncoderCount", "azMaxEncoderCount", "elMinEncoderCount", "elMaxEncoderCount"]

def szaEncoderZeros( 
        azEncoderZeroDegrees, elEncoderZeroDegrees, ant,
        subarray=DEFAULT ) :
    """Set encoder zeros for 3.5m dishes.
    Parameters:
    azEncoderZeroDegrees       AZ encoder zero (degrees)
    elEncoderZeroDegrees       EL encoder zero (degrees)
        ant: carma antenna number (starting with 1)"""
    multiSubarray('setSzaEncoderZeros', subarray,
            azEncoderZeroDegrees, elEncoderZeroDegrees, ant )

# These are function attributes and need to be outside the function scope!!!
szaEncoderZeros.__argsMpContainer__ = 'Sza'
szaEncoderZeros.__argsMpNames__ = [ "azEncoderZeroDegrees", "elEncoderZeroDegrees"]

def szaTilts( 
        haTiltDegrees, latTiltDegrees, elTiltDegrees, ant,
        subarray=DEFAULT ) :
    """Set tilts for 3.5m dishes.
    Parameters:
    haTiltDegrees              Hour-angle tilt (degrees)
    latTiltDegrees             Latitude tilt (degrees)
    elTiltDegrees              Elevation tilt (degrees)
        ant: carma antenna number (starting with 1)"""
    multiSubarray('setSzaTilts', subarray,
            haTiltDegrees, latTiltDegrees, elTiltDegrees, ant )

# These are function attributes and need to be outside the function scope!!!
szaTilts.__argsMpContainer__ = 'Sza'
szaTilts.__argsMpNames__ = [ "haTiltDegrees", "latTiltDegrees", "elTiltDegrees"]

def setAzEncoderOffset(ant) :
    """Take the current pointing offset and convert it into an encoder offset
    in the system.
    The encoder offset in the mount pointing model is modified based on the 
    current elevation and the azimuth offset value set by the 'offset' command.
    The offset is then set to zero.
    Parameter:
     ant: carma antenna number"""
    
    # Retrieve current azimuth offset (arcmin), elevation (degrees) 
    # and az encoder pointing offset (implementation specific).

    azOffMpName = "Control.Antenna%d.azimuthOffset"%ant
    pointingConstants = pointingSetup( ant )

    if  device.CarmaAnt().isOvro(ant):
        actualElMpName = "Ovro%d.AntennaCommon.Drive.Track.actualElevation"%ant
    elif device.CarmaAnt().isBima(ant): 
        bimaAntNo = ant - 6
        actualElMpName = "Bima%d.AntennaCommon.Drive.Track.actualElevation"%bimaAntNo
        [azOffset,actualEl ] = queryMpValues([azOffMpName, actualElMpName])
    elif device.CarmaAnt().isSza(ant): 
        szaAntNo = ant - 15
        actualElMpName = "Sza%d.AntennaCommon.Drive.Track.actualElevation"%szaAntNo
        [azOffset,actualEl ] = queryMpValues([azOffMpName, actualElMpName])
    else:
        raise Exception, "Invalid ant"

    [azOffset,actualEl ] = queryMpValues([azOffMpName, actualElMpName])
    cosEl = math.cos( actualEl * math.pi / 180.0 )

    if  device.CarmaAnt().isOvro(ant):
        pointingConstants[0] = pointingConstants[0] + azOffset/cosEl
        ovroMountPointingConstants( pointingConstants[0],
                                    pointingConstants[1],
                                    pointingConstants[2],
                                    pointingConstants[3],
                                    pointingConstants[4], ant )
    elif device.CarmaAnt().isBima(ant): 
        pointingConstants[0][0] = pointingConstants[0][0] + azOffset/cosEl
        bimaMountPointingConstants( pointingConstants[0], pointingConstants[1], ant )
    elif device.CarmaAnt().isSza(ant): 
        # For SZA, the az zero (term 7 in the pointing constants) is in degrees 
        pointingConstants[6] += ( ( azOffset/cosEl ) / 60.0 );
        # Avoid having to spell out all 19 arguments by using the special 
        # form '*args' with a list of ordered args.
        args = pointingConstants 
        args.append( ant )
        szaMountPointingConstants( *args )
    else:
        raise Exception, "Invalid ant"

    return offset(0, 0, ant)

def setMountAzOffset(ant) :
    """Deprecated! See setAzEncoderOffset."""
    print setMountAzOffset.__doc__
    return setAzEncoderOffset(ant)

          
# ------------------------- Procedures ------------------------
def doIVcurve(rx, pol=carma.antenna.common.RxControl.LEFTCIRCULAR, ants=0, tmo=0, waiton=ALL) :
    """Initiate an iv curve on a set of antennas and wait for completion.
    Parameter:
     rx: receiver type, e.g. RX3MM or RX1MM
     pol: polarization specifier for dual pol receivers
     ants: A single or list of antenna numbers; zero is all antennas
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY), or NONE
    Return value contains a list of the ready antennas and a list
    of the not ready antennas.
    """
    antlist = helpers.makeList(ants)
    s.doIVcurve(rx, pol, antlist)
    return wait(TUNED, tmo, waiton)

# --------------------- Optics ----------------------
def polarization(state, ants=0, tmo=60, waiton=ALL) :
    """Set polarization state on a set of antennas and wait for completion.
    Parameter:
     state: polarization state, e.g. VERTICAL or RCP
     ants: A single or list of antenna numbers; zero is all antennas
     tmo: Timeout value in seconds; zero inhibits timeout
     waiton: All to be complete (ALL), or just the first (ANY), or NONE
    Return value contains a list of the ready antennas and a list
    of the not ready antennas.
    """
    antlist = helpers.makeList(ants)
    s.polarization(state, antlist)
    return wait(OPTICS, tmo, waiton)

# --------------------- Queries ----------------------
# Internal routines
def raiseNFexception(monitorPoint) :
    raise Exception, "Invalid monitor point name: " + monitorPoint
def raiseMPexception(monitorPoint, ex) :
    m = "Couldn't get valid data for: " + monitorPoint + '\n' + str(ex)
    raise Exception, m
def raiseRetryException(monitorPoint, retries) :
    m = "Couldn't get valid data for " + monitorPoint
    if retries == 0: strretries = "no"
    else :           strretries = str(retries)
    m += ", " + strretries 
    if retries == 1: m +=  " retry"
    else:            m +=  " retries"
    raise Exception, m
class MonitorPoint(object) :
    "Class to contain name, value, etc of a monitor point"
    def __init__(self) :
        name_        = "notInitialized"
        value_       = 0
        stringValue_ = "0"
        valid_       = False
        found_       = True
        type_        = None
    def set(self, name, found, type, value, valid, stringValue) :
        """Set state for a monitor point
         Params:
          name: canonical monitor point name
          found: whether the requested name was found to be an MP
          type: data type (from carma.monitor)
          value: current value
          valid: true if MP has valid data
          stringValue: value expressed as a string"""
        self.name_        = name
        self.found_       = found
        self.type_        = type
        self.value_       = value
        self.valid_       = valid
        self.stringValue_ = stringValue
    def dump(self) :
        """Print out the values for the MP"""
        st = "%s=%s, valid=%d, found=%d, type=%s stringValue=%s"  \
               %(self.name_,  str(self.value_), self.valid_, self.found_, \
               self.type_, self.stringValue_)
        print st
    def isValid(self) :
        "Returns true if the MP is valid"
        return self.found_ and self.valid_
    def value(self) :
        "Returns the value of the monitor point"
        return self.value_
    def stringValue(self) :
        "Returns the value of the monitor point as a string (useful for enums)"
        return self.stringValue_

def queryInt(monitorPoint, retries=1) :
    """Get current value of monitor point. If not valid, go into a loop
    sleeping 0.6sec and retrying until the retry count is reached. If
    data are still not valid then an exception is thrown.
    Parameters:
     monitorPoint monitor point name (case insensitive)
     retries retry count, default is 1"""
    trials = retries + 1
    for i in range(trials) :
        try:
            return s.queryInt(monitorPoint)
        except carma.control.InvalidMonitorDataException:
            if i < retries:
                sleep(0.6)
        except carma.control.MonitorPointNotFoundException:
            raiseNFexception(monitorPoint)
        except Exception, ex:
            raiseMPexception(monitorPoint, ex)
    raiseRetryException(monitorPoint, retries)

def queryDouble(monitorPoint, retries=1) :
    """Get current value of monitor point. If not valid, go into a loop
    sleeping 0.6sec and retrying until the retry count is reached. If
    data is still not valid then an exception is thrown
    Parameters:
     monitorPoint monitor point name (case insensitive)
     retries retry count, default is 1"""
    trials = retries + 1
    for i in range(trials) :
        try:
            return s.queryDouble(monitorPoint)
        except carma.control.InvalidMonitorDataException:
            if i < retries:
                sleep(0.6)
        except carma.control.MonitorPointNotFoundException:
            raiseNFexception(monitorPoint)
        except Exception, ex:
            raiseMPexception(monitorPoint, ex)
    raiseRetryException(monitorPoint, retries)

def queryString(monitorPoint, retries=1) :
    """Get current value of monitor point. This command works on
    enumeration type monitor points as well as string types.
    If not valid, go into a loop sleeping 0.6sec and retrying until
    the retry count is reached. If data is still not valid then an
    exception is thrown
    Parameters:
     monitorPoint monitor point name (case insensitive)
     retries retry count, default is 1"""
    trials = retries + 1
    for i in range(trials) :
        try:
            return s.queryString(monitorPoint)
        except carma.control.InvalidMonitorDataException:
            if i < retries:
                sleep(0.6)
        except carma.control.MonitorPointNotFoundException:
            raiseNFexception(monitorPoint)
        except Exception, ex:
            raiseMPexception(monitorPoint, ex)
    raiseRetryException(monitorPoint, retries)

def queryComplex(monitorPoint, retries=1) :
    """Get current value of monitor point. If not valid, go into a loop
    sleeping 0.6sec and retrying until the retry count is reached. If
    data are still not valid then an exception is thrown
    Parameters:
     monitorPoint monitor point name (case insensitive)
     retries retry count, default is 1"""
    trials = retries + 1
    for i in range(trials) :
        try:
            return s.queryComplex(monitorPoint)
        except carma.control.InvalidMonitorDataException:
            if i < retries:
                sleep(0.6)
        except carma.control.MonitorPointNotFoundException:
            raiseNFexception(monitorPoint)
        except Exception, ex:
            raiseMPexception(monitorPoint, ex)
    raiseRetryException(monitorPoint, retries)

def queryBool(monitorPoint, retries=1) :
    """Get current value of monitor point. If not valid, go into a loop
    sleeping 0.6sec and retrying until the retry count is reached. If
    data is still not valid then an exception is thrown
    Parameters:
     monitorPoint monitor point name (case insensitive)
     retries retry count, default is 1"""
    trials = retries + 1
    for i in range(trials) :
        try:
            return s.queryBool(monitorPoint)
        except carma.control.InvalidMonitorDataException:
            if i < retries:
                sleep(0.6)
        except carma.control.MonitorPointNotFoundException:
            raiseNFexception(monitorPoint)
        except Exception, ex:
            raiseMPexception(monitorPoint, ex)
    raiseRetryException(monitorPoint, retries)

def query(monitorPoint) :
    """Retrieve a monitor point hierarchy. This is for interactive use,
    for programmatic use call queryInt, queryDouble, etc.
    Parameters:
     monitorPoint monitor point name (case insensitive)"""
    return s.query(monitorPoint)
    
def setMonitorPointReal(name, value):
    """Set the value of a control subsystem monitor point.
    The monitor point must be either a double of a float.
    Useful for fixing broken system state (invalid monitor point).
    Params:
     name: hierarchical monitor point name, with or without 
       the initial 'Control.'
     value: value to set the monitor point"""
    c1 = name.split(".")[0].lower()
    if c1 == "carma": name =name[6:]
    c1 = name.split(".")[0].lower()
    if c1 == "control": name =name[8:]
    #print name
    s.setMonitorPointReal(name, value)    

def setMonitorPointInvalid(name):
    """Set a control subsystem monitor point as invalid.
    This is for debugging and testing.
    Params:
     name: hierarchical monitor point name, with or without 
       the initial 'Control.'"""
    c1 = name.split(".")[0].lower()
    if c1 == "carma": name =name[6:]
    c1 = name.split(".")[0].lower()
    if c1 == "control": name =name[8:]
    #print name
    s.setMonitorPointInvalid(name)    

def queryFlux(source, freq=0.0, deltafreq=0.0, daysback=0.0) :
    """Query the flux catalog for a calibrator flux measurement.
        source - The source name, case insensitive.
        freq   - The frequency in GHz, zero means any frequency.
        deltafreq - The half-width frequency range about 'freq' to
          consider as a match in the query.  This parameter is ignored
          if freq = 0.
        daysback - Time limit of how far back to search the catalog, in days
          before today.  Zero means no time limit.

        Return value is a structure containing:
          The source name.
          The source flux in Janskys.
          The rms error on the flux, in Janskys
          The frequency of the measurement in GHz
          The number of days since the flux was measured at the given frequency
          A string representation of date flux was measured"""
    return s.queryFlux(source, freq, deltafreq, daysback)
    
def queryDelay(ants=0) :
    """Get the current values of the delay offset for all antennas and display
    them in the form of the commands to enter the delay.
    Parameters:
     ants: a list or antenna numbers or a single antenna number. A zero,
      the default, gives all antennas in the subarray."""
    antlist=makeAntList(ants)
    print "Current delays"
    for a in antlist :
        mp = "Control.Antenna%d.delayOffset3mmRx" %a
        try :
            print "delay(%7.3f, %2d)" %(queryDouble(mp, retries=0), a)
        except Exception:    
            print "C%d" %a, "does not have a value or is invalid"

def flux(source, freq=0.0, deltafreq=0.0, daysback=0.0) :
    """Query the flux catalog for a calibrator flux measurement.
        source - The source name, case insensitive.
        freq   - The frequency in GHz, zero means any frequency.
        deltaFreq - The half-width frequency range about 'freq' to
          consider as a match in the query.  This parameter is ignored
          if freq = 0.
        daysback - Time limit of how far back to search the catalog, in days
          before today.  Zero means no time limit.

        Return value is the source flux in Janskys."""
    x = queryFlux(source,freq,deltafreq,daysback)
    return x.flux

def queryMonitorPoint(mpnameList, dump=False) :
    if type(mpnameList) != list:
        #print "Not a list"
        mpnameList = [mpnameList]
    r = s.queryMonitorPoint(mpnameList)
    rtn = []
    for i in r:
        m = MonitorPoint()
        m.set(i.name, i.found, i.value._d, i.value._v, i.valid, i.stringValue)
        rtn.append(m)
    if (dump) :
        for m in rtn:
            m.dump()
    return rtn

def queryMpValues( canonicalNames, nothrow=False ):
    """Return a list of values (or list of list of values) for the input
    canonical names. Recursive lists are allowed in which the output structure 
    duplicates the input structure of the canonicalNames argument with the 
    caveat that a list must contain either other lists or names but not both.
    So for example [ ['mp1-1', 'mp1-2'], ['mp2-1', 'mp2-2'] ] is allowed but
    [ 'mp1', ['mp2-1, 'mp2-2']] isn't.  If the nothrow argument is false, an 
    exception is thrown if a point is not found OR is found to be invalid.
    The monitor point names are case insensitive.
    Parameters:
        mpList List (or recursive list) of canonical monitor point names.
        nothrow Mark missing/invalid points with None instead of throwing.
    Returns:
        Return Monitor point values as a (possibly recursive/empty) list.
    Throws:
        Exception if nothrow is false AND ANY monitor point(s) do not exist
        OR are found to be invalid.
    """
    from subarrayCommands import queryMonitorPoint
    def querySingleListOfMpValues( canonicalNameList, nothrow ):
        mps = queryMonitorPoint( canonicalNameList )
        mpValues = []
        for m in mps:
            if m.isValid():
                mpValues.append( m.value() )
            else:
                if nothrow: mpValues.append( None )
                else: raise Exception( m.name_ + " is not valid!" )

        return mpValues # Return as a list

    def recursiveMpListQuery( canonicalNameList, nothrow ):
        output = []
        for i in canonicalNameList:
            if list == type( i ):
                output.append(
                    recursiveMpListQuery(i, nothrow) )
            else:
                output = querySingleListOfMpValues( canonicalNameList,
                                                    nothrow)
                break
        return output

    return recursiveMpListQuery( canonicalNames, nothrow )

def  getNearest(source, elMin=None, elMax=None, sourceList=[], action=INCLUDE, 
                numReturn=1, ignoreNorthSouth=True, coordsys="azel",
                getOptical=True, fluxLimit=99, frequency=95) :
    """Search for nearby neighbors of input source, filtering by
    various criteria and sorted by distance from target.  This is useful
    for, e.g., finding nearby calibrators, optical/radio pointing sources.

    Parameters:
     source - The target source for which to find neighbors. No Default.
              A source name of 'SUBARRAY' (case insensitive) will attempt to
              use the RA/DEC of the current subarray phase center. If there
              is not a phase center, as indicated by a subarray source name
              of 'NONE', then Polaris is used.
     elMin  - minimum elevation filter, degrees. Default: None meaning no filter.
     elMax  - maximum elevation filter, degrees. Default: None meaning no filter.
     sourceList - a list of source names to exclude or include in the
        search, case insensitive.  If you don't want to
        include the target source, put its name here and choose
        EXCLUDE for action.  Default: empty list
     action - either INCLUDE or EXLUDE (enumeration values), to
            include or exclude the input sourceList. Default INCLUDE
     numReturn - the number of neighbor sources to return. Default: 1
     ignoreNorthSouth - If false, then check that the target soruce
            and neighbor source declinations are not on opposite sides
            of the telescope latitude AND that the sources are not close
            to transit.  This will filter out sources that would cause
            a change in azimuth wrap.  Default: False
     coordsys - one of "azel" or "radec".  "azel" means compute
            the distance using the closest in true slewing distance,
            so for instance a large azimuth slew for high elevation
            sources would be avoided.  "radec" means compute the
            spherical geometric distance.  Default: "azel"
     getOptical - if true use optical sources, if false use radio sources
            Default: True
     fluxLimit - lowest flux (or faintest magnitude in the case of
                 optical sources) to include in the return list.
                 Default:99 (magnitude)
     frequency - For radio sources, use this frequency to lookup a recent
          flux measurement. Units are GHz. Anything matching
          frequency+/-55GHz is deemed acceptable -- keeping the
          measurement in the respective waveband.  The lookback time
          is 100 days.  This parameter is ignored for optical sources.
          see queryFlux().  Default:95

    Return: A list of one or more sources close on the sky to the input
          source, also matching an additional criteria specified.
          The return list is sorted in ascending order of distance
          in degrees from the target source.  The list will contain
              sequence of NearestInfo objects.  NearestInfo contains: 
                   name - target source (name of this neighbor)
                   reference  - reference source (the input target source)
                   distance   - the distance in degrees beteen the two sources
                   azimuth    - azimuth of neighbor in degrees.
                   elevation  - elevation of neighbor in degrees.
                   brightness - magnitude if optical or flux if radio of 
                                neighbor 
                   isOptical  - True if optical source, false if radio
                   mjd        - The Modified Julian Date for which the 
                                distance was calculated
               A NearestInfo sequence can be printed with 
               carmaHelpers.printNeighbor( sequence )
     See also getBrightest()          
     """

    if ( elMin == None ) : elMin = -90
    if ( elMax == None ) : elMax = +90        
    return s.getNearest(source,elMin,elMax,sourceList,action,numReturn,
                        ignoreNorthSouth,coordsys,getOptical,fluxLimit,frequency)
                        
def  getBrightest(source=None, elMin=20, elMax=87, sourceList=[], action=INCLUDE, 
                  numReturn=1, ignoreNorthSouth=True, coordsys="azel",
                  getOptical=False, fluxLimit=1.0, frequency=95) :
    """Search for the brightest sources that are currently up, filtering by
    various criteria and sorted by brightness.  This is useful
    for, e.g., finding nearby calibrators, optical/radio pointing sources.

    Parameters:
     source - source to use as reference for distance measurements
              The default is None.
     elMin  - minimum elevation filter, degrees. Default: 20.
     elMax  - maximum elevation filter, degrees. Default: 87.
     sourceList - a list of source names to exclude or include in the
        search, case insensitive.  If you don't want to
        include the target source, put its name here and choose
        EXCLUDE for action.  Default: empty list
     action - either INCLUDE or EXLUDE (enumeration values), to
            include or exclude the input sourceList. Default INCLUDE
     numReturn - the number of neighbor sources to return. Default: 1
     getOptical - if true use optical sources, if false use radio sources
            Default: False (radio)
     fluxLimit - lowest flux in Jy (or faintest magnitude in the case of
                 optical sources) to include in the return list.
                 Default:1 Jy
     frequency - For radio sources, use this frequency to lookup a recent
          flux measurement. Units are GHz. Anything matching
          frequency+/-55GHz is deemed acceptable -- keeping the
          measurement in the respective waveband.  The lookback time
          is 100 days.  This parameter is ignored for optical sources.
          see queryFlux().  Default:95

      @return A list of one or more sources close on the sky to the input
          source, also matching an additional criteria specified.
          The return list is sorted in descending order of flux/brightness
          The list will contain a sequence of NearestInfo objects.  
          NearestInfo contains: 
                   name - target source (name of this neighbor)
                   reference  - reference source (the input target source)
                   distance   - the distance in degrees beteen the two sources
                   azimuth    - azimuth of neighbor in degrees.
                   elevation  - elevation of neighbor in degrees.
                   brightness - magnitude if optical or flux if radio of 
                                neighbor 
                   isOptical  - True if optical source, false if radio
                   mjd        - The Modified Julian Date for which the 
                                distance was calculated
               A NearestInfo sequence can be printed with 
               carmaHelpers.printNeighbor( sequence )
     See also getNearest() as it provides the core functionality of this method""" 
    # If source is None then use Polaris as the source as it is always up
    nsource = source 
    if source == None: 
        nsource = 'aumi'
        ignoreNorthSouth = True  
        coordsys = "azel"         
    # We need to get a bunch and then sort them and truncate after sorting...
    numToGet = 333 
    r = s.getNearest(nsource, elMin, elMax, sourceList, action, numToGet,
                     ignoreNorthSouth, coordsys, getOptical, fluxLimit, frequency)
    print "Number of results before trimming:", len(r)    
    def f(x, y):
        return cmp(y.brightness, x.brightness) 
    r.sort(f)                    
    if source == None: 
        for n in r:
            n.reference = "None"
            n.distance  = 0
            n.azimuth   = 0
            n.elevation = 0
    return r[:numReturn]                 

# ----------------------- Source/catalog info, time ------------------------
def ucat(catalogName, subarray=DEFAULT) :
    """Set a user catalog.  This is an alternate catalog of sources that will
    be searched BEFORE the system catalog. When a valid user catalog is set,
    the requested source is looked up in the user catalog; if not found,
    it is then looked up in the system catalog. The specified catalog must
    exist in /array/rt/catalogs/catalogName. To unset user catalog searching,
    set user catalog to 'none' or an empty string.
    Parameters:
     catalogName  The user catalog name"""
    stripCat = catalogName.strip() 
    if stripCat == "": catalogName = 'NONE'
    else : catalogName = stripCat
    multiSubarray('ucat', subarray, catalogName)

def elevlimit(elevation, subarray=DEFAULT) :
    """This commands sets the elevation limit for other commands 
       like info() and whazup().  

       NOTE: It has no effect on antenna tracking limits.
    
    Parameters:
     elevation: in degrees"""

    multiSubarray('elevLimit', subarray, elevation)

def restoreElevlimit(subarray=DEFAULT):
    """Retrieve last input elevation limit from the monitor system.
    This is used to restore state.
    Parameters: 
     None
    Return:
     None"""
    subNo = subarrayNo
    if subarray == SCI2: subNo = 2
    elevMpName = "Control.Subarray%d.elevLimit"%subNo
    elLimit = queryDouble( elevMpName, 24 ) # 24 retries (12 seconds)
    elevlimit( elLimit, subarray)

def info(sourcename) :
    """Lookup source in catalog and prints out:
    ra, dec, lst, elev, rise/set times
    Uses current elevation limit (see elevLimit).
    Throws an exception if source is not found in catalog.
    See also azel(), isup().
    Parameters:
     sourcename source name"""
    print s.info(sourcename)

def isup(sourcename) :
    """Lookup source in catalog and returns whether or not it is up.
    Uses current elevation limit (see elevLimit).
    Throws an exception if source is not found in catalog.
    Returns True if current source elevation is > elevation limit.
    See also azel(), info().
    Parameter:
     sourcename source name
     """
    return s.isUp(sourcename)

def whendown(sourcename) :
    """Lookup source in catalog and returns when source will set.
    Uses current elevation limit (see elevLimit).
    Throws an exception if source is not found in catalog.
    Returns number of minutes until source sets, 0 if already set

    Parameter:
     sourcename source name
     """
    return s.whenDown(sourcename)

def whenup(sourcename) :
    """Lookup source in catalog and returns when source will rise.
    Uses current elevation limit (see elevLimit).
    Throws an exception if source is not found in catalog.
    Returns number of minutes until source rises, 0 if already up

    Parameter:
     sourcename source name
     """
    return s.whenUp(sourcename)

def whentransit(sourcename) :
    """Returns the number of minutes until/since the source transits.

    Uses current elevation limit (see elevLimit).
    Throws an exception if source is not found in catalog.
    Returns number of minutes since/until source transits,
    positively valued if the source has not yet transitted,
    negatively valued if the source has already transitted.
    Return value is guaranteed to be between -720 and 720,
    inclusive.

    Parameter:
     sourcename source name
    """
    return s.whenTransit(sourcename)

def checktransit(sourcename) :
    transit = s.whenTransit(sourcename)
    if ( transit < 0 ) :
      print "Source %s transitted %.2f minutes ago." % ( sourcename, abs(transit) )
    else :
      print "Source %s transits in %.2f minutes."    % ( sourcename, transit )

def azel(sourcename, timeOffset=0.0) :
    """Lookup source in catalog and return a list (vector)
    containing az and el in degrees. See also info(), isup().
    Parameters:
     sourcename: source name
     timeOffset: time offset from current time in minutes (negative OK),
     defaults to zero
    Throws an exception if source is not found in catalog."""
    return s.azel(sourcename, timeOffset)

def whazup(optical=False) :
    """Show a list of all sources in the catalog that are currently up,.
    Uses current elevation limit (see elevLimit).
    Can use the radio catalog (default) or optical catalog.
    Parameter:
      optical: controls use of optical or radio catalog, default=False"""
    if optical :
        print s.whazUpOptical()
    else :
        print s.whazUp()

def lst() :
    """ Return current LST in hours. """
    return s.lst()

def lstString() :
    """ Return current LST as a sexagesimal string."""
    return s.lstString()

def mjd(offsetSeconds=0) :
    """ Return the current time plus an offset as an MJD in days,
    Parameter:
     offsetSeconds: offset from current time in seconds, default = 0."""
    return s.mjd(offsetSeconds)

#------------------------------Other-----------------------------------
def removeAntenna(ants, ignoreOwnership=True, subarray=DEFAULT) :
    """Remove an antenna or list of antennas from this subarray and
    put them into the maintenance subarray. Zero removes all antennas that are
    currently in this subarray..
    Parameters:
     ants: single or list of antenna numbers; zero is all
     ignoreOwnership: when False, will throw an exception if an attempt is made
        to remove an antenna that is not part of the subarray.
        When True, will ignore antennas that can't be removed because they
        are in another subarray or are already in maintenance, but will print
        out a message about them..
        Default=True.
    Return: list of antennas actually removed
    """
    results = multiSubarray('removeAntenna', subarray,
                helpers.makeList(ants), ignoreOwnership)
    if not ignoreOwnership : return
    alreadyRemoved = results.alreadyInMaint
    inOtherSubarrays = results.ownedByOtherSubarrays
    if len(alreadyRemoved) > 0 :
        st = makeAntString(alreadyRemoved)
        st += " already removed"
        print st
    if len(inOtherSubarrays) > 0 :
        st = makeAntString(inOtherSubarrays)
        st += " can't be removed because they are in another subarray"
        print st
    return results.actuallyRemoved

def addAntenna(ants, ignoreOwnership=True, retune=True, waiton=ALL, tsys=True,
               restore=True, dbg=False, subarray=DEFAULT):
    """Add an antenna or list of antennas to this subarray.
    The antenna(s) to be added must be in the Offline subarray.
    The state of the antenna is restored when it is added to the 
    subarray, including frequency and tracking.
    Note that addAntenna cannot do a linelength initialization because 
    it will change the 1st LO. See the details in help linelengthinit. 

    Parameters:
     ants:  single or list of antenna numbers; zero not allowed
     ignoreOwnership: when False, will throw an exception if an attempt is made
        to add an antenna that is not part of the maintenance subarray.
        If True, will print out if an antenna can't be added because it is
        part of another subarray, or already in this subarray. Default = True.
     retune: If true, retune antenna(s) to the previous frequency.
     waiton: When retuning wait on all to be complete (ALL), just the first
        (ANY), or NONE. Ignored if retune=False.
     tsys: Run a tsys on the added antenna(s). Forces implied waiton=ALL.
        Default is True.
    Return: list of antennas actually added.
    See also removeAntenna, linelengthinit."""

    def setInitialized( initialized, ants, subarray=DEFAULT ):
        """Local helper routine to set antenna initialized flags.  This is
        function local because we don't want people calling it independent
        of antenna restoration (currently our only antenna restoration 
        mechanism).
        """
        antlist = helpers.makeList(ants)
        multiSubarray('antennaInitialized', subarray, initialized, antlist )

    if subarray == BOTH:
        raise Exception, "Can't invoke addAntenna on BOTH subarrays"

    antlist = helpers.makeList(ants)
    
    # Check for ants=0 and throw an exception if found
    try :
        antlist.index(0) # This will throw if not found and skip out
        raise Exception, "ants=0 not allowed"
    except ValueError : pass

    results = multiSubarray('addAntenna', subarray, antlist, ignoreOwnership)

    # Warn if we've skipped antennas owned by others
    nOwnedByOthers = len( results.ownedByOtherSubarrays )
    if nOwnedByOthers == 1:
        warn = "Skipping ant %d since it is owned by another subarray."
        warn = warn%results.ownedByOtherSubarrays[0]
        printWarning( warn )
    elif nOwnedByOthers > 1:
        antString = helpers.formatAsRanges(results.ownedByOtherSubarrays)
        warn = "Skipping ants %s since they are each owned by another subarray."
        warn = warn%antString 
        printWarning( warn )

    # Just inform if we're reinitializing ants which already belong
    nAlreadyInSub = len( results.alreadyInSubarray )
    if nAlreadyInSub == 1:
        info = "Reinitializing ant %d which is already in this subarray."
        info = info%results.alreadyInSubarray[0]
        printInfo( info )
    elif nAlreadyInSub > 1:
        antString = helpers.formatAsRanges(results.alreadyInSubarray)
        info = "Reinitializing ants %s which are already in this subarray."
        info = info%antString
        printInfo( info )

    antsToInitialize = results.actuallyAdded + results.alreadyInSubarray

    if len( antsToInitialize ) == 0:
        return [] # Empty list indicating nothing was added.
        
    antsCopy = antsToInitialize[:]
    actuallyRestored = restoreAntenna(antsToInitialize, retune, waiton, tsys,
            dbg=dbg, subarray=subarray)
    antsNotRestored = [ ant for ant in antsCopy if ant not in actuallyRestored ]

    if len( antsNotRestored ) > 0:
        errorMsg = "Ants %s will not be added to this subarray "%(antsNotRestored)
        errorMsg += "due to the above listed reasons.  Correct these prior to "
        errorMsg += "adding these back into this subarray."
        printError( errorMsg )
        for ant in antsNotRestored:
            removeAntenna(ant, subarray=subarray)

    setInitialized( True, actuallyRestored, subarray=subarray ) 

    return actuallyRestored

def restoreAntCommand(command, ants, errorList=[], dbg=False, subarray=DEFAULT):
    """Using the carma monitor system, lookup arguments for the specified 
    command and run the command with those arguments.  This function relies
    on the function defining two attributes: __argsMpContainer__ which
    describes which ControlSubsystem container the commands arguments reside
    in (e.g. 'Ovro', 'Bima', 'Antenna', 'Sza') and __argsMpNames__ which
    defines the monitor point names in the order they are to be specified
    on the command line (e.g. ["azEncoderOffset", "elEncoderOffset"]).
    Commands must generally have the form command( args, ants ) as well.
    Parameters:
     command: The command to restore.
     ants: List of antennas to restore args for. Note this command REMOVES 
      antennas which fail command restoration from the list.
     errorList: A list to which errors are appended.
    Return:
     List of antennas which successfully completed the restore command.
    Throws:
     AttributeError if command does not define __argsMpContainer__ and 
     __argsMpNames__.
    """
    sa = dict()
    sa['subarray'] = subarray
    try:
        mpCont  = command.__argsMpContainer__
        mpNames = command.__argsMpNames__
        if dbg:
            print "Command:", command.__name__
            print "  mpNames:",     mpNames
    except AttributeError:
        errorMsg  = "Unable to restore command %s; " %(command.__name__)
        errorMsg += "make sure both the "
        errorMsg += "__argsMpContainer__ and __argsMpNames__ function "
        errorMsg += "attributes are defined."
        raise AttributeError( errorMsg )

    failures = []
    for ant in ants:
        if mpCont == 'Bima':
            prefix = "Control.%s%d."%(mpCont,ant - 6)
        elif mpCont == 'Sza':
            prefix = "Control.%s%d."%(mpCont,ant - 15)
        else:
            prefix = "Control.%s%d."%(mpCont,ant)
        canonNameList = helpers.prefixStringList( prefix, mpNames )
        args = queryMpValues( canonNameList )
        args.append( ant )
        try:
            command(*args,**sa) 
        except carma.util.UserException, ex: 
            errorMsg = "    " + ex.errorMsg
            printError( errorMsg )
            failures.append( ant )
        except Exception, ex:
            errorMsg = "    Failed to restore command "
            errorMsg += "%s() for ant %d."%(command.__name__,ant)
            printError( errorMsg + "\n" + str(ex)) 
            failures.append( ant )

    for failure in failures:
        ants.remove(failure)


def restoreAntenna(ants, retune=True, waiton=ALL, dotsys=True, 
        dbg=False, subarray=DEFAULT):
    """Initialize list of input antennas with configuration information
    and restore state. This includes pointing model, optical telescope
    parameters, pad location, safe range, etc.  In addition tuning frequency is
    optionally restored as well as the position and/or track source.
    Parameters:
        ants List of antennas in carma antenna number format.
        retune: If true, retune antenna(s) to the previous frequency.
        waiton: When retuning wait on all to be complete (ALL), just the first
                 (ANY), or NONE. Ignored if retune=False.
        tsys: Run a tsys on the added antenna. Forces implied waiton=ALL.
    Return:
        List of ants which were successfully restored.
    """
    ants = helpers.makeList(ants)

    # Check for ants=0 and throw an exception if found
    if ants.count(0) > 0: raise Exception, "ants=0 not allowed"

    if len(ants) == 0: raise Exception, "Empty antenna list not allowed"

    progress( "Assigning pads" )
    restoreAntCommand(pad, ants, dbg=dbg, subarray=subarray )
  
    progress("Antenna positional offset and axis non-intersection")
    restoreAntCommand( antennaOffset, ants, dbg=dbg, subarray=subarray)
    restoreAntCommand( axisNonIntersection, ants, dbg=dbg, subarray=subarray )
    
    ovroAnts,bimaAnts,szaAnts = antennasByType(ants)

    if len( ovroAnts ) > 0:
        progress("ovro physical parameters")
        restoreAntCommand(ovroMountPointingConstants, ovroAnts, dbg=dbg, subarray=subarray)
        restoreAntCommand(tiltzeros,                  ovroAnts, dbg=dbg, subarray=subarray)
        restoreAntCommand(opticalPointingConstants,   ovroAnts, dbg=dbg, subarray=subarray)
        # Order is important: 3mm before 1mm
        restoreAntCommand(radio3mmPointingConstants,  ovroAnts, dbg=dbg, subarray=subarray)
        restoreAntCommand(radio1mmPointingConstants,  ovroAnts, dbg=dbg, subarray=subarray)
        restoreAntCommand(radio1cmPointingConstants,  ovroAnts, dbg=dbg, subarray=subarray)
    
    if len( bimaAnts ) > 0:
        progress("bima physical parameters")
        restoreAntCommand(bimaMountPointingConstants, bimaAnts, dbg=dbg, subarray=subarray)
        restoreAntCommand(opticalPointingConstants,   bimaAnts, dbg=dbg, subarray=subarray)
        restoreAntCommand(radio3mmPointingConstants,  bimaAnts, dbg=dbg, subarray=subarray)
        restoreAntCommand(radio1cmPointingConstants,  bimaAnts, dbg=dbg, subarray=subarray)

    if len( szaAnts ) > 0:
        progress("sza physical parameters")
        restoreAntCommand(szaMountPointingConstants, szaAnts, dbg=dbg, subarray=subarray)
        restoreAntCommand(tiltzeros,                 szaAnts, dbg=dbg, subarray=subarray)
        restoreAntCommand(opticalPointingConstants,  szaAnts, dbg=dbg, subarray=subarray)
        restoreAntCommand(radio3mmPointingConstants, szaAnts, dbg=dbg, subarray=subarray)
        restoreAntCommand(radio1cmPointingConstants, szaAnts, dbg=dbg, subarray=subarray)


    progress( "Focus" )
    restoreAntCommand( focus, ovroAnts, dbg=dbg, subarray=subarray )
    restoreAntCommand( focus, bimaAnts, dbg=dbg, subarray=subarray )

    progress( "Reference LO attenuation" )
    restoreAntCommand( refatten, ovroAnts, dbg=dbg, subarray=subarray )

    ants=ovroAnts + bimaAnts + szaAnts

    progress( "Pad Offsets" )
    restoreAntCommand( padOffset, ants, dbg=dbg, subarray=subarray )

    progress( "Safe Ranges" )
    restoreAntCommand( setSafeRange, ants, dbg=dbg, subarray=subarray ) 

    progress("Zero offsets and restore mount offsets")
    for ant in ants:
        offset(0.0, 0.0, [ant], subarray=subarray)  # No wait
        restoreMountoffset(ant, subarray=subarray)

    progress("Set optical camera gains and rotations")
    restoreAntCommand(opticalRotationFOV, ants, dbg=dbg, subarray=subarray)

    if len(ants) == 0:
        return ants

    lastDriveCommands = driveSetup()

    if len( lastDriveCommands ) == 0:
        progress("Skipping drive restoration (no state info).")
    else:
        for commandAndArgs in lastDriveCommands:
            command = commandAndArgs[0]
            args = commandAndArgs[1]
            infoString = command.__name__ + '('
            argcount = 1
            numargs = len( args )
            for arg in args:
                infoString += "%s=%s" %(arg, args[arg])
                if argcount < numargs:
                    infoString += ", "
                argcount += 1
            infoString += ")"

            progress("Restoring drive via " + infoString )
            
            doStop   = False
            noSource = False
            if command.__name__ == "track":
                src = args["sourcename"]
                try :
                    srcup = isup(src)
                    if not srcup: doStop = True
                except:
                    noSource = True
                #print "["+src+" isup="+str(srcup)+"]"

            try:
                if doStop: 
                    print "\n    %s is not up, stopping antenna" %src
                    stop(ants)
                elif noSource:
                    m  = "\n ERROR:"
                    m += "source %s not found in current catalogs; " %src
                    m += "stopping antennas"
                    print m
                    stop(ants)    
                else: command(ants=ants, subarray=subarray, **args)
            except :
                printInColor("\n\tWarning: Unable to restore drive state.", 
                             color='yellow', linefeed=False );

    if dotsys: waiton=ALL # Force wait on all antennas if we're running a tsys.

    if retune:
        progmsg = "Retuning receiver"
        if waiton == ALL or waiton == ANY:
            progmsg += " (may take several minutes)"

        progress(progmsg)
        # Relock and retune
        refreq(ants, True, subarray=subarray, logging=False)
        tmo=130 # 130 seconds should suffice.
        wait(TUNED, ants, tmo, waiton, subarray=subarray)
        # restore track threshold only if retuning - this is due to the fact 
        # that this depends on the lo frequency.  No use in restoring this
        # if we'll be changing that in the future.
        restoreTrackThreshold(ants, subarray=subarray) 
    
    if len(ants) == 0:
        return ants

    if dotsys:
        progress("Tsys")
        tsys(ifsetup=True, ants=ants, subarray=subarray) #Blocks until completed

    progress( "Delays" )
    restoreAntCommand( delay, ants, dbg=dbg, subarray=subarray )
    restoreDelayDiff( ants )
    delayDefaults(ants, subarray=subarray)

    return ants

def restorePositionsOfUnownedAntennas() :
    """For antennas that are unowned and uninitialized, set their locations
       so that the MIRIAD file gets all antenna locations regardless of
       initialization state or subarray ownership.  If the subarray is already
       initialized, this command will return immediately (a no-op).
    """
    if ( s.getInitializationFlag() == True ): return
    unownedAnts = subarrayAntSetup( True )
    progress("Setting positions of unowned and uninitialized antennas %s" % helpers.formatAsRanges( unownedAnts) )
    progress("....Pads")
    restoreAntCommand(pad, unownedAnts, subarray=DEFAULT)
    progress( "....Pad Offsets" )
    restoreAntCommand( padOffset, unownedAnts, subarray=DEFAULT )
    progress("....Antenna positional offset and axis non-intersection")
    restoreAntCommand( antennaOffset, unownedAnts, subarray=DEFAULT )
    restoreAntCommand( axisNonIntersection, unownedAnts, subarray=DEFAULT )

def currentAntennaNames(carmaOnly=False) :
    """Return a list of strings describing the antenna composition of
    this subarray.
    Parameters:
     carmaOnly if true, only carma antenna names are output,
       otherwise it will return the default carma antenna name with the
       typed name in parenthesis, e.g. carma8(bima2)
    """
    a=s.getAntennaAssignments()
    namelist = []
    for i in a:
        cname = i.carmaAntennaName
        tname = i.typedAntennaName
        if (carmaOnly) :
            names = i.carmaAntennaName
        else :
            names = "%s(%s)" %(cname,tname)
        namelist.append(names)
    return namelist

def currentAntennaNumbers(subarray=DEFAULT) :
    """Return a list of carma antenna numbers that are members of
    this subarray. If BOTH is requested as the subarray then the list
    of antennas in both SCI1 and SCI2 subarrays is returned."""
    def _helper(a):
        antnums = []
        for i in a:
            num   = i.carmaAntennaNo
            antnums.append(num)
        return antnums
    if type(subarray) != str:
        m = "The subarray input to currentAntennaNumbers(" + str(subarray)
        m += ") is not a string, e.g. 'SCI1' or SCI1"
        raise Exception, m   
    if subarray == BOTH:
        a = _helper(multiSubarray('getAntennaAssignments', SCI1))
        b = _helper(multiSubarray('getAntennaAssignments', SCI2))
        return a+b
    if subarray == DEFAULT:
        return _helper(multiSubarray('getAntennaAssignments', DEFAULT))
    if subarray == SCI1:
        sa = Subarray.getSubarrayRef(1)
        return _helper(sa.getAntennaAssignments())
    if subarray == SCI2:
        sa = Subarray.getSubarrayRef(2)
        return _helper(sa.getAntennaAssignments())
    return _helper(multiSubarray('getAntennaAssignments', subarray))

def typedAntennaNames() :
    """Return a list of strings describing the antenna composition of
    this subarray, using the typed antenna names (e.g. ovroN, bimaN)
    """
    a=s.getAntennaAssignments()
    namelist = []
    for i in a:
        namelist.append( i.typedAntennaName )
    return namelist

def antennasByType(ants=0) :
    """Returns a list with three lists, each containing the antenna numbers
    for each antenna type, ordered by ovro, bima, sza
    Parameter:
     ants: a single antenna number or a list of antenna numbers, zero means all
       antennas currently in the subarray"""
    ovroAnts = []
    bimaAnts = []
    szaAnts = []
    antlist = makeAntList(ants)
    for a in antlist :
        if device.CarmaAnt().isOvro(a) :   ovroAnts.append(a)
        elif device.CarmaAnt().isBima(a) : bimaAnts.append(a)
        elif device.CarmaAnt().isSza(a) :   szaAnts.append(a)
        else : raise Exception, '%s is a bogus antenna number!' %a
    return [ovroAnts,bimaAnts,szaAnts]

def delayDefaults(ants=[0], subarray=DEFAULT) :
    """Set all delay usage booleans to their default values for all antennas
    in this subarray.  These booleans turn on or off different parts of the
    delay calculation. The defaults are:
    useAdjustable:   TRUE
    useGeometric:    TRUE
    useHeight:       TRUE
    useIonospheric:  FALSE
    useThermal:      FALSE
    useTropospheric: TRUE
    See, e.g., s.useAdjustableDelay(...) for more info.
    Parameter:
     ants: a single antenna number or a list of antenna numbers, zero means all
       antennas currently in the subarray"""
    multiSubarray('useAdjustableDelay', subarray, True,ants)
    multiSubarray('useGeometricDelay', subarray, True,ants)
    multiSubarray('useHeightDelay', subarray, True,ants)
    multiSubarray('useIonosphericDelay', subarray, False,ants)
    multiSubarray('useThermalDelay', subarray, False,ants)
    multiSubarray('useTroposphericDelay', subarray, True,ants)
    return

def delaysOff() :
    """Set all delay usage booleans to False and delay offsets to zero.  
       *** FOR TESTING ONLY ***.
    """
    s.useAdjustableDelay(False,[0])
    s.useGeometricDelay(False,[0])
    s.useHeightDelay(False,[0])
    s.useIonosphericDelay(False,[0])
    s.useThermalDelay(False,[0])
    s.useTroposphericDelay(False,[0])
    adjustableDelay(0,0)
    ants = currentAntennaNumbers()
    for myAnt in ants :
        delay( 0, myAnt )
    return

def delay(delay, ant, subarray=DEFAULT) :
    """Set delay offsets for an antenna for 3mm POL1 receiver signal path.
    Parameters:
     delay: delay offset in nanoseconds
     ant: carma antenna number
    See delaydiff and adjustableDelay"""
    if ant < 1 or ant > 23:
        raise Exception, "Antenna number(%d) must be in range [1-23]" %ant
    mp = "Control.Antenna%d.delayOffset3mmRx" %ant
    try :
        oldDelay = queryDouble(mp, retries=0)
        if False: print "Previous value of delay for antenna %d was %7.3f nsec" \
                %(ant,oldDelay)
    except: pass   
    multiSubarray('delay', subarray, delay, ant)

delay.__argsMpContainer__ = 'Antenna'
delay.__argsMpNames__ = [ 'delayOffset3mmRx' ]


def delaydiff(delay, ant, rx=RX1MM, pol=POL1, subarray=DEFAULT) :
    """Set delay difference between 1mm/1cm and 3mm POL1 receiver signal path,
    for each polarization.  The difference is 1mm/1cm path minus 3mm POL1 path.
    Parameters:
     delay: delay offset difference in nanoseconds
     ant: carma antenna number
     rx:  receiver type, e.g. RX3MM, RX1MM, or RX1CM. Default: RX3MM
     pol: Receiver polarization selection. Either POL1 or POL2.  Default: POL1
    See delay"""
    if ( pol == POL1 ) :
       if ( rx == RX3MM ) :
           raise Exception, "The delay difference for 3mm Rx Pol1 is zero by definition."
       elif ( rx == RX1MM ) :
           mp = "Control.Antenna%d.rxDelay1mmPol1"%ant
       elif ( rx == RX1CM ) :
           mp = "Control.Antenna%d.rxDelay1cmPol1"%ant
       else:
           raise Exception, "Unrecognized receiver type"
    elif ( pol == POL2 ) :
       if ( rx == RX3MM ) :
           mp = "Control.Antenna%d.rxDelay3mmPol2"%ant
       elif ( rx == RX1MM ) :
           mp = "Control.Antenna%d.rxDelay1mmPol2"%ant
       elif ( rx == RX1CM ) :
           mp = "Control.Antenna%d.rxDelay1cmPol2"%ant
       else:
           raise Exception, "Unrecognized receiver type"
    else:
       raise Exception, "Unrecognized polarization value"

    try :
        oldDelayDiff = queryDouble(mp, retries=0)
        if False: print "Previous delay diff for antenna %d receiver %d pol %d was %7.3f nsec" \
              %(ant,rx,pol,oldDelayDiff)
    except: pass   
    multiSubarray('delayDifference', subarray, delay, ant, rx, pol)


def restoreDelayDiff(ants) :
    """Restore delay differences for input antennas and all valid
       polarization/receiver pairs
       Parameter:
         ants: List of antennas to restore mapping for. As with 
         restoreAntCommand, this removes ants which fail.
    """
    failedAnts = []
    
    for ant in ants:
       try:
           mp = "Control.Antenna%d.rxDelay3mmPol2"%ant 
           delay = queryDouble(mp)
    #       print "Ant %d, 3mm Pol2 = %f"% (ant,delay)
           s.delayDifference(delay, ant, RX3MM, POL2)

           mp = "Control.Antenna%d.rxDelay1mmPol1"%ant 
           delay = queryDouble(mp)
    #       print "Ant %d, 1mm Pol1 = %f"% (ant,delay)
           s.delayDifference(delay, ant, RX1MM, POL1)

           mp = "Control.Antenna%d.rxDelay1mmPol2"%ant 
           delay = queryDouble(mp)
    #       print "Ant %d, 1mm Pol2 = %f"% (ant,delay)
           s.delayDifference(delay, ant, RX1MM, POL2)

           mp = "Control.Antenna%d.rxDelay1cmPol1"%ant 
           delay = queryDouble(mp)
    #       print "Ant %d, 1cm Pol1 = %f"% (ant,delay)
           s.delayDifference(delay, ant, RX1CM, POL1)

           mp = "Control.Antenna%d.rxDelay1cmPol2"%ant 
           delay = queryDouble(mp)
    #       print "Ant %d, 1cm Pol2 = %f"% (ant,delay)
           s.delayDifference(delay, ant, RX1CM, POL2)
       except carma.util.UserException, ue:
            printError( ue.errorMsg )
            failedAnts.append( ant )
       except:
            errmsg = "Unable to restore delay differences on ant# %d "%(ant)
            printError( errmsg )
            failedAnts.append( ant )

    for failed in failedAnts:
        ants.remove( failed )

def adjustableDelay(delay, ants) :
    """Set the adjustable delay value for a set of antennas that is
       added into the total delay for the given antennas. This component of
       the delay can be turned off on one or more antennas
       with s.useAdjustableDelay(True/False,ants).  The default is 
       to use the adjustable delay value (see boolean values in
       RTD Delay page).
       This is an engineering type command that is not saved as part of the
       system state. On system restart it will be set to zero.
    Parameters:
     delay: delay value in nanoseconds
     ants: A single or list of antenna numbers; zero is all antennas.
    See delay and delayDiff"""
    antlist = helpers.makeList(ants)
    s.adjustableDelay(delay, antlist)

def antennaOffset(east, north, up, ant, subarray=DEFAULT):
    """Set the phase center offset for an antenna.
    Parameters:
     east: Easterly offset in mm.
     north: Northerly offset in mm.
     up: Vertical offset in mm.
     ant: Carma antenna number (starting at 1)."""
    multiSubarray('antennaOffset', subarray, east, north, up, ant)

antennaOffset.__argsMpContainer__ = 'Antenna'
antennaOffset.__argsMpNames__ = ["AntennaOffset.%s"%mp for mp in ["east", 
                                                                  "north", 
                                                                  "up"]] 
def axisNonIntersection(offset, ant, subarray=DEFAULT):
    """Set the non-intersection offset for this antenna.
    Parameters:
     offset: Non-intersection offset in millimeters.
     ant: Carma antenna number (starting at 1)."""
    multiSubarray('axisNonintersection', subarray, offset, ant)

axisNonIntersection.__argsMpContainer__  = 'Antenna'
axisNonIntersection.__argsMpNames__ = ["axisNonIntersection"]


def radioAperture(useRadio, ants, subarray=DEFAULT) :
    """Switch between radio and optical pointing models.
    This is normally done with the freq() and camera() commands,
    but in rare cases finer control is needed.
    Parameters:
     useRadio: boolean, if true, use radio model, otherwise use optical
     ants: single or list of antenna numbers; zero is all"""
    antlist = helpers.makeList(ants)
    multiSubarray('radioAperture', subarray,useRadio, antlist)

def checktime(host="all", status=False, verbose=False):
    """Checks the status of ntp on a given host by using the ntpq command
    and returns a list of strings. The string contents depends on the mode
    in which the program is run. Modes:
    default: returns a zero length string if offset and jitter are within spec;
      returns err message if offset or jitter for preferred clock is large or
      if there is no preferred clock or if there is a communication error.
    status: returns clock name, offset and jitter for the preferred clock
    verbose: returns all ntpq output verbatim. The times listed 
     are in milliseconds.
    Parameters:
     host: hostname to check, defaults to all in the RTS configuration files
     status: True will set to status mode, default is False
     verbose: True will set to verbose mode, default is False"""
    if host == "all" :
        exe = helpers.getCarmaBinaryPath() + "/imrconfig file="
        exe += helpers.getCarmaConfPath() + "/imr/carma.xml hosts=true"
        host = os.popen(exe).read().split()
    else : host = helpers.makeList(host)
    for h in host :
        helpers.printStringSeq(helpers.checkNTP(h, status, verbose))

# ----------------------- Error/fault system ------------------------
def driveErrorPreference(state, subarray=DEFAULT):
    """
Set the effect for antenna drive tracking errors. The pipeline
will perform the requested action when it encounters an antenna
drive tracking error condition.

Parameter:
    state: PREF_NONE, PREF_BLANK, PREF_FLAG

Description of enumeration values:
    PREF_NONE  - ignore all drive tracking errors, all data will be written to the dataset
    PREF_BLANK - while the drive tracking error persists, data will not be written to the dataset
    PREF_FLAG  - while the drive tracking error persists, data will be written to the dataset
                 and the integration will be flagged

Additional notes:
    For convenience, the string values 'NONE', 'BLANK', and 'FLAG'
    are also accepted.
"""
    if type(state) == str:
        try:
            d = dict()
            d['NONE'] = carma.fault.PREF_NONE
            d['BLANK'] = carma.fault.PREF_BLANK
            d['FLAG'] = carma.fault.PREF_FLAG
            state = d[state.upper()]
        except KeyError:
            raise Exception('trackingErrorPreference: state "%s" is invalid' % state)
    elif state not in (carma.fault.PREF_NONE, carma.fault.PREF_BLANK, carma.fault.PREF_FLAG):
        raise Exception('trackingErrorPreference: state "%s" is invalid' % state)

    # TODO: use the subarray parameter
    s.setFaultSystemDriveErrorPreference(state)

def alarmMpdisable(monitorpoints) :
    """Disable alarms on the single or list of monitor points given.
    See alarmMpenable.
    Parameter:
     monitorpoints: a single or list of monitor points to disable alarms on"""

    # Make sure every monitor point specified has perfect
    # capitalization and exists in the monitor system before passing it
    # along to the SubarrayController and then the FaultSystem
    m = []
    for elem in helpers.makeList(monitorpoints):
        m.append(helpers.getAbsoluteMPName(s, elem))

    s.disableFaultSystemAlarms(m)

def alarmDisableAntenna(nums=0):
    """Disables all of the alarms for the given list of antennas

    Parameter:
        - nums: one number or a list of numbers specifying which antennas
                to disable. The "0" wildcard is supported."""

    # support using 0 as a wildcard meaning "all antennas"
    if nums == 0:
        nums = range(1, 24)

    m = []
    for elem in helpers.makeList(nums):
        mpname = device.CarmaAnt().getName(elem) + ".online"
        m.append(mpname)

    alarmMpdisable(m)

def alarmEnableAntenna(nums=0):
    """Enables all of the alarms for the given list of antennas

    Parameter:
        - nums: one number or a list of numbers specifying which antennas
                to disable. The "0" wildcard is supported."""


    # support using 0 as a wildcard meaning "all antennas"
    if nums == 0:
        nums = range(1, 24)

    m = []
    for elem in helpers.makeList(nums):
        mpname = device.CarmaAnt().getName(elem) + ".online"
        m.append(mpname)

    alarmMpenable(m)

def alarmDisableCorrelator(nums=0):
    """Disables all of the alarms for the given list of astrobands

    Parameter:
        - nums: one number or a list of numbers specifying which astrobands
                to disable. The "0" wildcard is supported."""

    # support using 0 as a wildcard meaning "all bands"
    if nums == 0:
        nums = range(1, 25)

    m = [helpers.getAstroBandName(elem) for elem in helpers.makeList(nums)]
    alarmMpdisable(m)

def alarmEnableCorrelator(nums=0):
    """Enables all of the alarms for the given list of astrobands

    Parameter:
        - nums: one number or a list of numbers specifying which astrobands
                to disable. The "0" wildcard is supported."""

    # support using 0 as a wildcard meaning "all bands"
    if nums == 0:
        nums = range(1, 25)

    m = [helpers.getAstroBandName(elem) for elem in helpers.makeList(nums)]
    alarmMpenable(m)

def alarmMpenable(monitorpoints) :
    """Restores alarms on the single or list of monitor points given
    to the state given in the fault system description file,
    normally /opt/rt/conf/fault/carma.fdagml.
    See alarmMpdisable
    Parameter:
     monitorpoints: a single or list of monitor points to disable alarms on"""

    # Make sure every monitor point specified has perfect
    # capitalization and exists in the monitor system before passing it
    # along to the SubarrayController and then the FaultSystem
    m = []
    for elem in helpers.makeList(monitorpoints):
        m.append(helpers.getAbsoluteMPName(s, elem))

    s.restoreFaultSystemAlarms(m)

def alarmMprestore(monitorpoints):
    """Deprecated alias for alarmMpenable()"""

    # print a deprecation message
    msg = ''
    msg += 'NOTICE: alarmMprestore() is deprecated! Please change your '
    msg += 'script to use alarmMpenable() instead'
    print msg

    # run the underlying command anyway
    alarmMpenable(monitorpoints)

_integMP = "IntegratorStageContainer.IntegratorStage.timeSinceLastIntegration"
def alarmIntegdisable(corr=CORR_ALL, subarray=DEFAULT) :
    """Disable alarms on the timeSinceLastIntegration MP for requested
    correlators owned by the requested subarray and max coherence 
   rfor all antennas in the requested subarray.
    Parameter:
      corr: which correlators (pipelines) to disable; this is an enumeration 
            with default=CORR_ALL (all that belong to the requested subarray)
      subarray: 
    See alarmIntegrestore."""
    if   subarray == SCI1:     acorrs = assignedCorrelator(1)
    elif subarray  == SCI2:    acorrs = assignedCorrelator(2)
    elif subarray  == DEFAULT: acorrs = assignedCorrelator(subarrayNo)
    elif subarray  == BOTH:    acorrs = 'SPECTRAL+WIDEBAND'  # BOTH gets ALL correlators!
    hasSpectral = ('SPECTRAL' in acorrs);
    hasWideband = ('WIDEBAND' in acorrs);
    if ((corr == CORR_SPECTRAL or corr == CORR_ALL) and hasSpectral):
           print "Disabling integ alarm for SlPipeline"
           try:
               alarmMpdisable('SlPipeline.' + _integMP)
           except Exception, ex :
                err = "Exception when trying to disable SL integ alarm:\n"
                printInColor(err,color='red')
                print(ex.errorMsg);
              
    if ((corr == CORR_WIDEBAND or corr == CORR_ALL) and hasWideband):
           print "Disabling integ alarm for WbPipeline"
           try:
               alarmMpdisable('WbPipeline.' + _integMP)
           except Exception, ex :
                err = "Exception when trying to disable WB integ alarm:\n"
                printInColor(err,color='red')
                print(ex.errorMsg);

    currentAnts = currentAntennaNumbers(subarray)
    try:
        alarmMpdisable(['Astro.Antenna%d.MaxCoherence'%ant for ant in currentAnts])
    except Exception, ex :
         err = "Exception when trying to disable Max Coherence alarm:\n"
         printInColor(err,color='red')
         print(ex.errorMsg);

def alarmIntegrestore(subarray=DEFAULT) :
    """Restores alarms on the time since last integration
    to the state given in the fault system description file,
    normally /opt/rt/conf/fault/carma.fdagml.
    Does nothing in non-science sac sessions.
    This does *not* enable the max coherence alarms as that is only
    done at the start of a script (in the obsdef code)
    See alarmIntegdisable"""
    def _helper(sub) :
        if sub == SCI1: corrs = assignedCorrelator(1)
        if sub == SCI2: corrs = assignedCorrelator(2)
        hasWideband = ('WIDEBAND' in corrs);
        if 'SPECTRAL' in corrs:
            print "Enabling integ alarm for SlPipeline"
            try:
                alarmMpenable('SlPipeline.' + _integMP)
            except Exception, ex :
                err = "Exception when trying to enable SL integ alarm:\n"
                printInColor(err,color='red')
                print(ex.errorMsg);
        if 'WIDEBAND' in corrs:
            print "Enabling integ alarm for WbPipeline"
            try:
                alarmMpenable('WbPipeline.' + _integMP)
            except Exception, ex :
                err = "Exception when trying to enable WB integ alarm:\n"
                printInColor(err,color='red')
                print(ex.errorMsg);
    
    # To erase any timeout before resetting the alarm
    resetTimeSinceLastIntegration(subarray)
    if subarray == DEFAULT:
        if   subarrayNo == 1: subarray = SCI1
        elif subarrayNo == 2: subarray = SCI2
        else :
            print "alarmIntegrestore ignored; only effective in SCI1 or SCI2" 
            return
    # Max coherence alarms are *not* enabled - leave that to radioInit
    if subarray == SCI1 or subarray == SCI2:
        _helper(subarray)
    if subarray == BOTH:
        _helper(SCI1)
        _helper(SCI2)

def resetTimeSinceLastIntegration(subarray=DEFAULT) :
    """Resets the 'time since the last integration' in the pipeline.
    This is effectively a dummy integration that can be inserted
    in optical pointing, tilts, etc to inhibit the no integration alarm.
    See also alarmIntegdisable and alarmIntegrestore"""
    multiSubarray('resetTimeSinceLastIntegration', subarray)

def alarm1mmdisable() :
    """Disable alarms due to 1mm observing conditions changing.
    (Currently the alarm is only triggered for Sci1 subarray)"""
    mpName = 'Control.Subarray%d.alarm1mm' %subarrayNo
    alarmMpdisable( mpName )

def alarm1mmrestore() :
    """Restore alarms due to 1mm observing conditions changing.
    (Currently the alarm is only triggered for Sci1 subarray)"""
    mpName = 'Control.Subarray%d.alarm1mm' %subarrayNo
    alarmMpenable( mpName )

# ------------------------- Pipeline ---------------------------
def decimation(decimate=False, bandNo=0, keepEndChans=False, subarray=DEFAULT) :
    """Controls decimation of data in the pipeline.
    When data are decimated the lag data are Hann windowed and
    then Fourier transformed, yielding a doubled channel width, after which
    every other channel are dropped, giving independent channels.
    When decimation is turned off there is no Hann windowing and
    the end channels are dropped. Decimation is usually controlled by the
    configastroband command. However this command is useful for the rare cases
    where control of end channels is needed. 
    See configastroband.
    Parameters:
     decimate: boolean, True turns on decimation
     bandNo: band number, one is first band, zero is all (default)
     keepEndChans: default=False. When True, oerrides decimate parameter 
      and provides undecimated data with end channels in place. 
      Should only be set True by expert users for diagnostics"""
    if bool != type(decimate) : 
        raise Exception, "Input decimation must be a boolean"
    DEC_ON  = carma.control.DECIMATION_ON
    DEC_OFF = carma.control.DECIMATION_OFF_DROP_END_CHANNELS
    KEEP    = carma.control.DECIMATION_OFF_KEEP_END_CHANNELS
    if keepEndChans :
        d = KEEP
        print "Keeping end channels with no decimation"
    elif decimate : d = DEC_ON
    else          : d = DEC_OFF
    multiSubarray('setDecimationMode', subarray, d, bandNo)

def applyTsys(tsys, subarray=DEFAULT) :
    """Controls application of tsys to the data in the pipeline.
    No tsys scaling is applied to the noise source.
    Parameter:
     tsys: boolean control flag; if true, tsys scaling is applied"""

    if bool != type(tsys) : raise Exception, "Input must be a boolean"
    multiSubarray('applyTsys', subarray, tsys)

def applyFlux(flux, subarray=DEFAULT) :
    """Controls application of flux scaling to the data in the pipeline.
    If true, sets tsys scaling as well to avoid errors.
    If the flux scaling is applied, the noise source is always scaled
    by 100 to give percent correlation.
    Parameter:
     flux: boolean control flag; if true, apply flux calibration"""

    if bool != type(flux) : raise Exception, "Input must be a boolean"
    if flux: applyTsys(True, subarray=subarray)
    multiSubarray('applyFlux', subarray, flux)

# --------------------- Frequency and tuning -----------------------
def _resolveFreq(freq) :
    """ An internal routine (not for use by observers) to take a string that
    may be either a frequency in GHz or a molecular line name. Returns a tuple
    containing the freq and name. The name is set to 'none' if a numerical
    value is directly entered."""
    err = "The input type(%s) of %s passed to _resolveFreq() is not legitimate" \
             %(type(freq), freq)
    if isinstance(freq, types.BooleanType):  raise Exception, err
    if isinstance(freq, types.FloatType):    return (freq, 'none')
    if isinstance(freq, types.IntType):      return (freq, 'none')
    if isinstance(freq, types.StringType):   return (linefreq(freq), freq)
    raise Exception,  err


def freq(frest, sb1, fif, dopplerSource, quick=False, subarray=DEFAULT) :
    """Sets the first LO frequency, placing a Doppler shifted rest frequency
    at a specific IF frequency. The IF frequency should be chosen to maximize
    receiver sensitivity or avoid image bleed through.
    After the initial setup, continuous Doppler tracking by the control system
    will cause small changes in the IF frequency. The first and second LO 
    frequencies will be set correctly independent of the order of the freq 
    and configastroband commands, but power level setup requires that the freq 
    command be last or alternatively that the configastroband commands be followed
    by a tsys command.
    For engineering work, to set LO1 directly, use
      frest=LO1, sb1=USB or LSB, fif=0, dopplerSource=None.
    For DSB work with lines in both sidebands of a single band, use 
      frest=LO1rest, sb1=USB or LSB and fif=0.
      In many cases, LO1rest is just the average of the upper and
      lower sideband line rest frequencies. This obviously works for only
      one pair (one band) of USB/LSB lines. Lines in other bands will
      be tracked in a single sideband only (the one with the frest specified
      in the configastroband command).
    For reference:
      flo1 = dopplerFactor*(frest - S1*fif/dopplerFactorIni)
    where
      flo1: first LO freq
      frest: rest frequency
      S1 : LO1 sideband factor; USB=1, LSB=-1
      dopplerFactorIni: doppler factor when command is invoked
    This command implements the following sequence:
     o Sets LO reference synthesizer in control building
     o Sends LO freq to correlators
     o Sends LO freq to loberotators
     o Sends Yig and Gunn freq to antennas
     o Tells antennas to tune
     o Waits for antennas to tune (all ants leave amb load in)
     o If quick mode, exits right here, with all amb loads in
     o Puts in ambient load
     o Sets antenna IF power level (PAM)
     o Sets downconverter power to preset level
     o Removes ambient load
    Parameters:
     frest: rest frequency for setup in GHz, OR, a molecular line
       name from the catalog (/opt/rt/conf/catalogs/SpectralLine.cat).
       The line name must be a string, enclosed in quotes, case insensitive.
       See the linefreq() command to get the frequency for a spectral line.
     sb1: first LO sideband for the restfreq: USB or LSB
     fif: IF freq to map rest freq to, in GHz. After the initial setup, 
       the IFfreq will vary slightly due to doppler tracking. This value 
       should be chosen to be within the IF band of the rx (1-9 GHz), 
       taking into account Tsys performance within the IF band. A value of
       around 2.5 GHz is a good default, but see the plots off the observer
       web page. The choice of IF freq will affect the ability to place 
       other lines in the opposite sideband. Note that this is the actual 
       IF frequency, not a fictitious 'rest' IF frequency.
       See web page ref at the end of this help.
     dopplerSource: doppler source name,
       The velocity and position of the source from the catalog are used
       to compute the doppler correction to apply to the first LO rest
       frequency. To not change the doppler source, use a source of NOCHANGE.
       To not use doppler tracking, use a doppler of None or NONE. These
       three names are special and do not require quotes.
    See also: configastroband, qfreq, linefreq, transitionfreq, checkbands, tsys
       http://cedarflat.mmarray.org/observing/doc/correlator.html"""
    return runKeyboardInterruptable(_freq, frest, sb1, fif, dopplerSource,
                                     quick, subarray )

def _freq(frest, sb1, fif, dopplerSource, quick, subarray) :
    if subarray == BOTH:
        raise Exception, "Can't do freq on BOTH subarrays"
    if dopplerSource == NOCHANGE : dopplerSource = "dontChange"
    if dopplerSource == None:      dopplerSource = "None"
    freqline   = _resolveFreq(frest)
    frest      = freqline[0]
    transition = freqline[1]
    
    # 1cm check
    if frest < 50:
        _MAGIC_1CM_FREQ = 35.938
        if dopplerSource == "None":
            if abs((frest - fif) - _MAGIC_1CM_FREQ) > .001:
                print "****************************************"
                print "*************** WARNING ****************"
                print " 1cm LO frequency is fixed at %.3f GHz" %_MAGIC_1CM_FREQ
                print "*************** WARNING ****************"
                print "****************************************"
        else :
            print "******************************************"
            print "**************** WARNING *****************"
            print "Using Doppler shift at 1cm but LO will be"
            print "fixed at %.3f GHz." %_MAGIC_1CM_FREQ
            print "This will work for the spectral correlator"
            print "but the wideband correl bands will not be"
            print "centered (but usually not significant)."
            print "**************** WARNING *****************"
            print "******************************************"

    if quick :
        multiSubarray('qfreq', subarray, frest, sb1, fif, dopplerSource, 
                transition)
    else :
        print "Tuning receivers, can take up to 2 minutes to complete..."
        rtdComment("Tuning receivers...", subarray=subarray)
        t = time.time()
        multiSubarray('freq', subarray,
            frest, sb1, fif, dopplerSource, transition)
        rtdComment("Tuning complete", subarray=subarray)
        tstr = "  FYI, it took %.0f seconds to tune" %(time.time()-t)
        print tstr

def qfreq(frest, sb1, fif, dopplerSource=None, subarray=DEFAULT) :
    """A quick version of freq() that does not request or wait for rx tuning,
    nor set antenna or downconverter power levels. The reference oscillator,
    antenna LOs, and 2nd LOs are all set. See freq().
    Parameters: same as freq()"""
    freq(frest, sb1, fif, dopplerSource, True, subarray=subarray)
    sleep(0.5)

def linefreq(transition) :
    """Get the frequency for a molecular line transition.
    The transitions are listed in /opt/rt/conf/catalogs/SpectralLine.cat
    as aliases near the top of the file.
    If the transition is not found an exception will be thrown
    Parameters:
     transition: a molecular line transition name, case insensitive.
         examples:   'CO', '12CO', '12CO(1-0)', 'HCN', 'C18O(1-0)'
    See also transitionfreq()"""
    return s.lineFreq(transition)

def transitionfreq(molecule, transition) :
    """Gets the frequency for a given molecular line transition.
    The line must be in the carma line catalog; if not, an exception
    is thrown. The catalog is in /opt/rt/conf/catalogs/SpectralLine.cat.
    Examples of molecule,transition parameters:
      'HCN', '1-0,F=2-1'
      'CO', '1-0'
    Parameters:
     molecule: the molecule, case insensitive
     transition: spectral line transition for the molecule, case insensitive
    See also linefreq()"""
    return s.transitionFreq(molecule, transition)

def doppler(dopplerSource, subarray=DEFAULT) :
    """Sets the doppler source and changes the reference oscillator
    accordingly. It does not tell the receivers about the change, so
    it is only appropriate for small frequency changes.
    Returns the doppler frequency. See freq."""
    if dopplerSource == NOCHANGE : dopplerSource = "dontChange"
    if dopplerSource == None:      dopplerSource = "None"
    if subarray == BOTH:
        raise Exception, "Can't do doppler on BOTH subarrays"
    return multiSubarray('doppler', subarray, dopplerSource)

def refreq(ants, retune=False, subarray=DEFAULT, logging=True, auto=False) :
    """Initiates a relocking of the oscillator on the requested 
    antennas. The receivers may be optionally retuned, which takes 
    approximately two minutes on the 6m antennas. This command 
    does not wait for either the relocking or retuning to complete.
    See freq.
    Parameters:
      ants:  single or list of antenna numbers; zero means all
      retune: also retune the receiver (not just relock). Default is False"""
    if subarray == BOTH:
        raise Exception, "Can't do refreq on BOTH subarrays"
    st  = ""
    if auto: st += "auto(script) "
    else:    st += "manual "
    st += "refreq(" + str(ants) + ")"
    if logging: commandlog(st)
    antlist = helpers.makeList(ants)
    multiSubarray('refreq', subarray, antlist, retune)

def refatten(atten, ants, subarray=DEFAULT) :
    """Set the reference LO termination in the antennas.
    Vectors (lists) can be used, but they must be the same size, unless
    only a single attenuation is given, in which case it is sent to all
    antennas in the vector.
    Parameters:
     atten: attenuation value, [0-31] dB; list allowed if it matches
            length of antenna list
     ants:  single or list of antenna numbers; zero not allowed"""
    if subarray == BOTH: 
        raise Exception, "Refatten can't be called on BOTH subarrays"
    antlist   = helpers.makeList(ants)
    attenlist = helpers.makeList(atten)
    antlen    = len(antlist)
    attenlen  = len(attenlist)
    sameErr = " must be the same"
    if (attenlen > 1) and (attenlen != antlen) :
        msg = "Length ants(%d) must be the same as atten(%d)" \
            %(antlen, attenlen)
        raise Exception, msg
    for i in range(antlen) :
        if attenlen > 1 : ai = i
        else :            ai = 0
        multiSubarray('refAtten', subarray, attenlist[ai], antlist[i])

refatten.__argsMpContainer__  = 'Antenna'
refatten.__argsMpNames__ = ["refAtten"]


def vj(vj, pol, ant) :
    """Set the SIS junction voltage (Vj) for the current rx for a specific antenna.
    Debugging/engineering: should not be part of normal observering.
    Parameters:
     vj:  Vj value
     pol: polarization specifier (SINGLE, LEFTCIRCULAR or RIGHTCIRCULAR)
     ant: carma antenna number, beginning with one"""
    s.vj(pol, vj, ant)

def ij(ij, pol, ant) :
    """Set the SIS junction current (Ij) for the current rx for a specific antenna.
    Debugging/engineering: should not be part of normal observering.
    Parameters:
     ij:  Ij value
     pol: polarization specifier (SINGLE, LEFTCIRCULAR or RIGHTCIRCULAR)
     ant: carma antenna number, beginning with one"""
    s.ij(pol, ij, ant)

def vjij(Vj, Ij, pol, ant) :
    """Set the SIS junction voltage (Vj)  and current (Ij) for the
     current rx for a specific antenna.
    Debugging/engineering: should not be part of normal observering.
    The voltage is set first, then a sleep of 0.5 seconds, then the current.
    Parameters:
     Vj:  Vj value
     Ij:  Ij value
     pol: polarization specifier (SINGLE, LEFTCIRCULAR or RIGHTCIRCULAR)
     ant: carma antenna number, beginning with one"""
    s.vj(pol,Vj, ant)
    sleep(0.5)
    s.ij(pol,Ij, ant)

# ----------------------- Power levels and IF --------------------------
def psysPreset(ants=0, band=0, subarray=DEFAULT) :
    """Set downconverter power level to the preset value.
    This should be done when the ambient load is in the beam.
    It is best to do this with the tsys command, which will ensure
    proper power levels from the antenna. The default sets the level
    for all antennas, all bands. See tsys.
    Parameters:
     ants:  single or list of antenna numbers; zero (default) means all
     band: band number, zero (default) means all"""
    antlist = helpers.makeList(ants)
    multiSubarray('psysPreset', subarray, antlist, band)

def antennaIFpower(power, ants=0, subarray=DEFAULT) :
    """Set the IF total power level in the antenna (PAM).
    The ambient load position is not changed as part of this command.
    Debugging command not normally used in routine operations.
    Parameters:
      power: power level in milliwatts
      ants:  single or list of antenna numbers; zero (default) means all"""
    antlist = helpers.makeList(ants)
    multiSubarray('antennaIFpower', subarray, power, antlist)

def antennaIFatten(atten, ifnum=1, invalidateTsys=True, ants=0, subarray=DEFAULT) :
    """Set the IF attenuation in the antenna (PAM).
    The ambient load position is not changed as part of this command.
    Debugging command not normally used in routine operations.
    Parameters:
      atten: attenuation in dB
      ifnum: Specify which IF for dual pol receivers (1=IF1, 2=IF2, 0=both)
      ants:  single or list of antenna numbers; zero (default) means all"""
    antlist = helpers.makeList(ants)
    try:
        multiSubarray('antennaIFatten', subarray, float(atten), ifnum, antlist, invalidateTsys )
    except carma.util.UserException, ue:
        printError( ue.errorMsg )

def antennaIFpresetPower(ants=0, subarray=DEFAULT) :
    """Set the IF total power level in the antenna (PAM) to the preset level.
    Should normally use tsys command instead of this. See tsys.
    Parameters:
      ants:  single or list of antenna numbers; zero (default) means all"""
    antlist = helpers.makeList(ants)
    multiSubarray('antennaIFpresetPower', subarray, antlist)

def tsys(ifsetup=False, ants=0, ifPowerDelay=5.0, subarray=DEFAULT) :
    """Insert ambient load and remove - this will cause a new Tsys value
    to be calculated by the pipeline. This command blocks until complete.
    If ifsetup=True then the IF power levels will be setup. This can
    cause a phase discontinuity because signal phase may be a function of
    attenuation. This command does not wait for the antennas to be tracking
    before it does the measurement.
    Parameters: atten
     ifsetup: when True, puts the ambient load in the beam and sets 
      the IF level in the antenna (PAM) to 0.3 dBm followed by the
      downconverters (psysPreset). 
      Default is False.
     setupstate: ambient load state for power level setup
       Default is AMBIENT.
     ants:  single or list of antenna numbers; zero (default) means all
     ifPowerDelay: delay (in seconds) after the antennas are requested to setup
       the IF power level (only on ifsetup=True). Default=3.0 seconds normally,
       but set to the obscenely large value of 5 seconds for testing.
   Returns list of antennas that completed."""
    if subarray == BOTH:
        raise Exception, "Can't do tsys on BOTH subarrays"
    if type(ifsetup) != bool:
        print "The first argument is 'ifsetup' and it must be a boolean"
        print "Here is the help for the tsys command"
        print tsys.__doc__
        raise Exception, "Input for ifsetup is not a boolean, see help above"
    rtdComment("Measuring system temperature", subarray)
    m = "tsys(ifsetup=%r, ants=%s, ifPowerDelay=%.1f)" \
            %(ifsetup, str(ants), ifPowerDelay)
    commandlog(m)
    antlist = makeAntList(ants)
    is1cm = lofreq(subarray) < 50
    currentPos = "AMBIENT"
    #print "Inserting ambient load into beam"
    pos1ants = amb(ants, subarray=subarray).ready
    if ifsetup :
        targetPowerLevel = 0.3 # In dBm
        #print "Setting up IF levels on %s" %currentPos
        antennaIFpower(targetPowerLevel, ants, subarray=subarray)
        # Wait for ant levels to finish
        wait(tmo=ifPowerDelay, subarray=subarray)
        if is1cm :
            s.storeAntennaIFattenAmbient(antlist)
        # Set the IF levels in the downconverters
        bands = 0  # All bands
        psysPreset(ants, bands, subarray=subarray)
        # Wait for 3secs for psysPreset to complete internally in downconverters 
        wait(tmo=3.0)  
        # And then wait for two seconds to get final psys reading     
        wait(tmo=2.0) 
    else :
        wait(tmo=2.0) # Wait for two seconds to get final psys amb reading
    #print "Moving ambient load out of the beam"
    pos2ants = sky(ants, subarray=subarray).ready
    # Wait for two seconds to get final psys reading
    wait(tmo=2.0, subarray=subarray)
    skyants = pos2ants
    complete = []
    for a in pos1ants :
        if (pos2ants.count(a) > 0) : complete.append(a)
    rtdComment("System temperature measurement complete", subarray)
    return complete
    
def evaluateIFsetup(ants=0, timerange=3.4, deltat=0.1,
            verbose=False, iniAtten=0) :
    """Measures response time for setting IF power in antennas.
    This is a diagnostic routine for experts.
    Currently assumes SZA antenna, and will only work for them.
    Params:
      ants: antenna list or 0 for all
      timerange: length of time to conduct measurements over (secs)
      delta: sampling time interval (secs) for total power and attens
      verbose: when True prints out all samples
      iniAtten: if None, does not change attenuation settings before
        starting measurements (this usually gives fast setup times); 
        otherwise sets attenuators to input value before starting
        measurements. Default = 0 dB."""
    antlist = makeAntList(ants)
    for a in antlist:
        if a < 16:
            print "SZA antennas only can be used - exiting"
            return
    tpMP    = "Ifmod.ifTotalPower"
    attenMP = "Ifmod.totalAtten"
    currentPos = "AMBIENT"
    print "Inserting ambient load into beam"
    rtn = amb(ants)
    pos1ants = rtn.ready
    mplist = []
    for a in pos1ants:
        prefix = "Sza%d." %(a-15)
        mplist.append([prefix+tpMP, prefix+attenMP])
    #print mplist
    if len(rtn.notready) != 0:
        print "Ambient load not inserted for ants:", rtn.notready
    if iniAtten != None :
         print "Setting IF atten to %.1f" %iniAtten
         antennaIFatten(iniAtten, 1, invalidateTsys=False, 
                ants=antlist)
         wait(tmo=2.0)
    t0=time.time()
    antennaIFpower(0.3, ants)
    t1 = time.time()
    cmdtime = t1-t0
    results = []
    times   = []
    nsamps = int(round(timerange/deltat))
    for i in range(nsamps):
        sleep(deltat)
        dt=time.time()-t1
        times.append(dt)
        results.append(queryMpValues(mplist))
    #print results
    print "Command time = %.3f seconds" %cmdtime
    for j in range(len(pos1ants)):
        aname = "Sza%d" %(pos1ants[j]-15)
        if verbose: print "======== %s ========" %aname
        for i in range(nsamps):
            r = results[i][j]
            st = "%s:  %4.1f  %6.3f  %4.1f" %(aname,times[i],r[0],r[1])
            if verbose: print st
    print "\n==== Inferred timing & final atten ====="
    for j in range(len(pos1ants)):
        aname = "Sza%d" %(pos1ants[j]-15)
        finalAtten = results[nsamps-1][j][1]
        timing = times[nsamps-1]
        for i in range(nsamps):
            index = nsamps-1-i
            atten = results[index][j][1]
            if atten != finalAtten: break
            timing = times[index]
        st = " %s:   %4.1f   %4.1f" %(aname,timing, finalAtten)
        print st
       
    #print "Moving ambient load out of the beam"
    pos2ants = sky(ants).ready
    # Wait for two seconds to get final psys reading
    wait(tmo=2.0)
    skyants = pos2ants
    complete = []
    for a in pos1ants :
        if (pos2ants.count(a) > 0) : complete.append(a)
    return complete

def setIFswitch(position, IFnum, ant):
    """Set the IF switch position. This is an engineering command only
    to allow backdoor access for testing the MMIC 3mm receiver. 
    It will only work on the 10m antennas. 
    WARNING: Any kind of frequency command, including a refreq,
    will put the switch back into the normal default state
    corresponding to the requested frequency.
    Parameters:
     position: an integer from 1 through 4. 1=1cm, 2=3mm USB,
               3=1mm, 4=3mm LSB
     IFnum: an integer to select the IF. Values are 1(LCP) or 2(RCP)
     ant: 10m antenna number, 1 through 6"""
    if (position < 1) or (position > 4):
        m = "Input switch position (%d) must be between 1 and 4" %position
        raise Exception, m
    if (IFnum < 1) or (IFnum > 2):
        m = "Input IFnum (%d) must be 1 or 2" %IFnum
        raise Exception, m
    if (ant < 1) or (ant > 6):
        m = "Input ant number (%d) must be between 1 and 6" %ant
        raise Exception, m
    if IFnum == 1:
        IFsel = carma.antenna.common.RxControl.IF1
    else:
        IFsel = carma.antenna.common.RxControl.IF2
    device.Ovro(ant).rxSelect().Rx(RX1MM).IF(IFsel).selectBand(position)

# ------------------------- Correlator ---------------------------
BW500 = carma.util.CORR_BW_500MHZ
BW250 = carma.util.CORR_BW_250MHZ
BW125 = carma.util.CORR_BW_125MHZ
BW62  = carma.util.CORR_BW_62MHZ
BW31  = carma.util.CORR_BW_31MHZ
BW8   = carma.util.CORR_BW_8MHZ
BW2   = carma.util.CORR_BW_2MHZ

USB   = carma.control.SB_UPPER
LSB   = carma.control.SB_LOWER
AUTO  = carma.control.SB_AUTO


CORR_2BIT     = carma.util.CORR_2BIT
CORR_3BIT     = carma.util.CORR_3BIT
CORR_4BIT     = carma.util.CORR_4BIT

def freqSetup(subarray=DEFAULT) :
    """Gets the distilled parameters last input with the freq() command.
    Useful for configastroband() that are relative to the current setup.
    See also: qfreqSetup
    Parameters: none
    Return: [frest, fif, florest]"""        
    if subarray == BOTH:
        raise Exception, "Can't do freqSetup on BOTH subarrays"
    subNo = subarrayNo
    if subarray == SCI1: subNo = 1
    if subarray == SCI2: subNo = 2
    frest = queryDouble("control.subarray%d.commands.freq.restFreq" % subNo, 24)
    fif   = queryDouble("control.subarray%d.commands.freq.ifFreq" % subNo, 24)
    sb    = queryString("control.subarray%d.commands.freq.sideband" % subNo, 24)
    if sb == "USB" : florest = frest - fif
    else :           florest = frest + fif
    return [frest, fif, florest]

def qfreqSetup(subarray=DEFAULT) :
    """Gets the distilled parameters last input with the qfreq() command.
    Used for restoring state.
    See also: freqSetup
    Parameters: none
    Return: [frest, sb, fif]"""
    if subarray == BOTH:
        raise Exception, "Can't do qfreqSetup on BOTH subarrays"
    subNo = subarrayNo
    if subarray == SCI1: subNo = 1
    if subarray == SCI2: subNo = 2
    frest = queryDouble("control.subarray%d.commands.freq.restFreq" % subNo, 24)
    fif   = queryDouble("control.subarray%d.commands.freq.ifFreq" % subNo, 24)
    sb    = queryString("control.subarray%d.commands.freq.sideband" % subNo, 24)
    if sb == "USB" : sb = USB
    else : sb = LSB
    return [frest, sb, fif]

def driveSetup():
    """Retrieves last subarray drive related commands and associate parameters.
    If a track and equatorial offset was applied, both are returned in the list.
    If the last command was a moveAz or moveEl the corresponding elevation or
    azimuth will be the last one set with either moveAz/El or move.  If neither
    is available the current antenna az/el is used.
    Parameters: none
    Return: A list of function, parameter pairs [(func1, params),
           (func2, params)...].  Parameters are specified as a dictionary keyed
           by argument name.  The return list will be time ordered and should
           be forward iterated through.  This enables one to pass the
           parameters dictionary directly into the associated command
           (e.g. ans[0][0]( **ans[0][1] )).
    """
    # Retrieve subarray drive commands and determine which was issued last.

    mpPrefix = "Control.subarray%d.Commands." % Subarray.getSubarrayNo()

    commandTsNameMap = {
        track: "Track.timestamp",
        move: "Move.timestamp",
        stop: "Stop.timestamp",
        stow: "Stow.timestamp"
        }

    mostRecentTs = 1.0
    mostRecentCommand = None
    for command in commandTsNameMap.keys():
        mpName = mpPrefix + commandTsNameMap[command]
        try:
            timestamp = queryDouble( mpName )
            if timestamp > mostRecentTs:
                mostRecentTs = timestamp
                mostRecentCommand = command
        except:
            pass # Stifle

    if mostRecentCommand == None:
        return []

    # Let exceptions bubble at this point as they indicate an invalid state.
    if mostRecentCommand == track:
        answer = []
        sourcename = queryString( mpPrefix + "Track.sourcename")
        args = { 'sourcename': sourcename, 'waiton': NONE }
        answer.append( (mostRecentCommand, args ) )
        try:
            equatOffsetTs = queryDouble( mpPrefix + "EquatOffset.timestamp" )
        except:
            pass # Stifle
        else:
            if equatOffsetTs > mostRecentTs:
                ra = queryDouble( mpPrefix + "EquatOffset.ra" )
                dec = queryDouble( mpPrefix + "EquatOffset.dec" )
                args = { 'ra': ra, 'dec': dec, 'waiton': NONE, 
                         'whileIntegrating': True }
                answer.append( (equatOffset, args) )
        return answer
    elif mostRecentCommand == move:
        try:
            az = queryDouble( mpPrefix + "Move.azimuth" )
            moveAz = True
        except:
            moveAz = False

        try:
            el = queryDouble( mpPrefix + "Move.elevation" )
            moveEl = True
        except:
            moveEl = False

        if moveAz and moveEl:
            return [( move, { 'az': az, 'el': el, 'waiton': NONE } )]
        elif moveAz:
            return [( move, { 'az': az, 'waiton': NONE } )]
        elif moveEl:
            return [( move, { 'el': el, 'waiton': NONE } )]
        else:
            errStr = "No prior state for az or el available."
            raise Exception, errStr

    elif mostRecentCommand == stop:
        return [( stop, {} )]
    elif mostRecentCommand == stow:
        positionStr = queryString( mpPrefix + "Stow.position" )
        if positionStr == 'ZENITH':
            pos = ZENITH
        elif positionStr == 'SERVICE':
            pos = SERVICE
        elif positionStr == 'SAFE':
            pos = SAFE
        else:
            pos = None
        return [( stow, { 'position': pos, 'waiton': NONE }) ]
    
def pointingSetup( ant ):
    """Return mount pointing constants for input antenna.  Note that
    Bima and Ovro antennas use different mount models.  For Ovro antennas a
    single list of values corresponding to m1-m5 will be returned.  For Bima a 
    double list of values containing the 10 apc/epc pointing coefficients will be
    returned.
    Parameters:
     ant Antenna number."""

    if device.CarmaAnt().isOvro(ant): 
        mpPrefix = "Control.Ovro%d."%ant
        mpNames = helpers.prefixStringList(
            mpPrefix,
            ovroMountPointingConstants.__argsMpNames__ )
    elif device.CarmaAnt().isBima(ant):
        mpPrefix = "Control.Bima%d."%(ant-6)
        mpNames = helpers.prefixStringList( 
            mpPrefix, 
            bimaMountPointingConstants.__argsMpNames__ )
    # TLC edits 11/18/2010 - trying to get sza antennas working
    elif device.CarmaAnt().isSza(ant):
        mpPrefix = "Control.Sza%d."%(ant-15)
        mpNames = helpers.prefixStringList( 
            mpPrefix, 
            szaMountPointingConstants.__argsMpNames__ )
    else:
        raise Exception, "%d is an invalid ant #!"%ant

    return queryMpValues( mpNames ) 

def corrBandSetup( aband, corrType ):
    """Return the last known input parameters to configastroband for the
    specified band and corr type.  These values are retrieved from the 
    monitor system.
    We intentionally ignore decimation and keepEndChans.
    Parameters:
         aband - Astroband number 
         corrType - correlator type (CORR_SPECTRAL, CORR_WIDEBAND, etc)
    """

    def bandwidthToEnum( bw ):
        """Convert input numeric bandwidth to a fixed enum."""
        # global BW500, BW250, BW125, BW62, BW31, BW8, BW2
        bandwidths = {
            BW500: 500.0,
            BW250: 500.0/2.0,
            BW125: 500.0/4.0,
            BW62: 500.0/8.0,
            BW31: 500.0/16.0,
            BW8: 500.0/64.0,
            BW2: 500.0/256.0
        }

        for bwk in bandwidths.keys():
            if bw >= 0.9 * bandwidths[bwk] and bw <= 1.1 * bandwidths[bwk]:
                return bwk 
        else:
            raise Exception, "Invalid bandwidth (%f)!"%bw

    def sidebandToEnum( sb ):
        """Convert sideband monitor point enum (integer) to a fixed enum."""
        sidebands = {
            0: USB,
            1: LSB,
            2: AUTO 
        }
        return sidebands[sb]

    def fpgaModeToAbConfig(fpga):
        """Convert fpgaMode enum to astroband configuration string."""
        abconfig= {
            0: "LL",
            1: "DUALPOL",
            2: "FULLSTOKES",
            3: "CARMA23",
            
        }
        return abconfig[fpga]

    def bitModeToEnum(bitMode):
        """Convert correlator bit mode to enum."""
        bits = {
            0: CORR_2BIT,
            1: CORR_3BIT,
            2: CORR_4BIT
        }
        return bits[bitMode]

    # Map of configastroband arguments and associated monitor point name.
    if (corrType == CORR_NONE ):
        raise Exception, "corrBandSetup: No correlator to restore"

    argMpMap = { 
        'bw': 'bandwidth',
        'conf': 'fpgaMode', #note: eventually rename this MP astrobandMode
        'fcenter': 'centerFreq',
        'sb2': 'reqLO2sideband',
        'frest': 'restFreq',
        'imagefrest': 'imageRestFreq',
        'bits':  'corrBits'
    }
    band = aband
    # Rebind monitor point name with canonical mp value.
    if ((corrType & CORR_SPECTRAL) == CORR_SPECTRAL ):
        mpBase = 'Control.SpectralLineCorrelator.SlcBand'
    elif ((corrType & CORR_WIDEBAND) == CORR_WIDEBAND):
        mpBase = 'Control.WidebandCorrelator.WbcBand'
        band = band - 8
    elif ((corrType & CORR_C3GMAX23 ) == CORR_C3GMAX23):
        mpBase = 'Control.C3gMax23Correlator.C3gMax23Band'
        band = band - 24
    elif ((corrType & CORR_C3GMAX8 ) == CORR_C3GMAX8):
        mpBase = 'Control.C3gMax8Correlator.C3gMax8Band'
        band = band - 33
    #elif corrType == (CORR_SPECTRAL|CORR_WIDEBAND):
    #    if(aband < 9) :
    #        mpBase = 'Control.SpectralLineCorrelator.SlcBand'
    #    elif (aband >8 && aband < 25) :
    #        mpBase = 'Control.WidebandCorrelator.WbcBand'
    #    elif (aband >24 && aband < 33) :
    #        mpBase = 'Control.C3gMax23Correlator.C3gMax23Band'
    #    elif (aband >24 && aband < 33) :
    #        mpBase = 'Control.C3gMax8Correlator.C3gMax8Band'
    else :
        raise Exception, "corrBandSetup: Unrecognized correlator type: %d" % corrType
        
    for param in argMpMap.keys():
        canonMpName = mpBase+"%d.ControlBandPoints.%s" %(band,argMpMap[param])
        valList = queryMpValues( canonMpName )
        argMpMap[param] = valList[0]   # Only expect a single value

    # Special case the online mp
    #onlineMp = queryMpValues( monFormat%(band,"online") )
    #if onlineMp[0] == False:
    #    argMpMap['fcenter']="offline"

    # convert bandwidth and sideband to an enum 
    argMpMap['bw'] = bandwidthToEnum( argMpMap['bw'] )
    argMpMap['sb2'] = sidebandToEnum( argMpMap['sb2'] )
    argMpMap['conf'] = fpgaModeToAbConfig( argMpMap['conf'] )
    argMpMap['bits'] = bitModeToEnum( argMpMap['bits'] )
    
    argMpMap['subarray'] = DEFAULT
    argMpMap['decimate'] = False

    return argMpMap

def subarrayAntSetup( unownedAntennas = False ):
    """Retrieve last known list of antennas belonging to this subarray. 
    This command is meant for system/state restoration rather than for 
    querying a running system (use currentAntNames/currentAntNumbers for 
    that purpose).  These values are retrieved from the monitor system.
    Return: Possibly empty list of last known antennas belonging to subarray.
    Parameters:
       unownedAntennas - If True, return only those antennas NOT belonging 
                         to this subarray. Default: False
    """

    # Form lists of MP canonical names for antenna and subarray numbers.
    canonAntNumberMps = []
    canonSubNumberMps = []
    
    numAnts = device.CarmaAnt().getNumAnts()
    for antNo in xrange(1, 1 + numAnts):
        canonAntNumberMps.append( "Control.Antenna%d.carmaAntennaNumber"%antNo )
        canonSubNumberMps.append( "Control.Antenna%d.subarrayNumber"%antNo )

    # Query MP Values in a single pass.  Note that we don't throw on invalid
    # or missing MPs but rather check for None below.
    [antennaNumbers,subarrayNumbers] = queryMpValues( [canonAntNumberMps, 
                                                       canonSubNumberMps ],
                                                      True ) # No throw
        
    # Verify that the returned lists are in step - get out if they aren't.
    if len( antennaNumbers ) != len( subarrayNumbers ):
        raise Exception, "Ant # list size does not match subarray # list size!"

    # Form up our answer.  If somebody knows an easy way to do this with
    # iterators, let me know.
    answer = []
    thisSubarrayNo = Subarray.getSubarrayNo()
    for index in xrange( 0, len( antennaNumbers ) ):
        subNo = subarrayNumbers[index]
        antNo = antennaNumbers[index]
        
        if subNo == None or antNo == None:
            continue

        if ( unownedAntennas == False ) :
            if subNo == thisSubarrayNo:
                answer.append( antNo )
        else :
            if subNo != thisSubarrayNo:
                answer.append( antNo )

    return answer

def lofreq(subarray=DEFAULT) :
    """Gets the current LO frequency, throwing an exception if there is
    no valid monitor point in 12 seconds.
    Parameters: none
    Return: 1st LO frequency in GHz"""
    if subarray == BOTH: 
        raise Exception, "Can't call lofreq on BOTH subarrays"
    subNo = Subarray.getSubarrayNo()
    if subarray == SCI1: subNo = 1
    if subarray == SCI2: subNo = 2
    m = "control.subarray%i.loFreq" %subNo
    return  queryDouble(m, 24)

def makeCorrBandList(astroband, includeOfflineBands = False) :
    """Return correlator band list for input astroband number or list.
       The return value depends on the astroband mode for the 
       input astroband and whether the individual correlator bands
       are online.  By default offline correlator bands will be 
       removed from the return list.
       Parameters:
         astroband - A single or list of astroband numbers. Zero means all.
         includeOfflineBands - Whether or not to include offline correlator
                     bands in the returned list. True means include them.
                     Default: False
    """
    if ( subarrayNo < 1) :
        raise Exception, "Invalid subarray number %d" % subarrayNo
    if ( subarrayNo > 2 ) :
        raise Exception, "No correlator in this subarray"
    ablist = helpers.makeList(astroband)
    # s.getCorrelatorBandNoSeq has to return something or we get
    # CORBA marshalling error.  So if no ABs are active from the 
    # input list, then a sequence = [-1] is returned.  If so,
    # this python method will return an empty sequence.
    cblist = s.getCorrelatorBandNoSeq( ablist, includeOfflineBands )
    if ( len(cblist) == 1 and cblist[0] == -1 ) :
        cblist = []
    return cblist

def isAstrobandOnline(band) :
    """Discover if an astroband is online or offline. If any correlator bands
    that make up the astroband are offline, then the astroband is considered
    offline. 
       Parameters: 
           band - the astroband to query.

    Returns True if online, False if offline or if the astroband is not 
    configured in the signal path map.
    """
    cblist = makeCorrBandList(band,True)

    if ( band < 9 ) :
        monFormat = "Control.SpectralLineCorrelator.SlcBand%d.ControlBandPoints.%s"
    elif ( band < 25 ) :
        monFormat = "Control.WidebandCorrelator.WbcBand%d.ControlBandPoints.%s"
        band = band - 8
    elif ( band < 33 ) :
        monFormat = "Control.C3gMax23Correlator.C3gMax23Band%d.ControlBandPoints.%s"
        band = band - 24
    else :
        monFormat = "Control.C3gMax8Correlator.C3gMax8Band%d.ControlBandPoints.%s"
        band = band - 32 

    # if signal path map has no info, so assume corrband==astroband
    if len(cblist) == 0 : 
          cblist.append(band)
        
    onlineMp = []
    for b in cblist :
        onlineMp.append( monFormat%(b,"online") )
    #print onlineMp
    onlineValues = queryMpValues(onlineMp)
    # if any correlator band in the astroband is offline, then
    # consider the entire astroband offline
    for v in onlineValues :
        if ( v == False ): return False

    return True

def astrobandOnline(band, online, subarray=DEFAULT) :
   """Set an astroband online or offline
   Parameters:
       band: astroband number, starting at one.  Note that astroband 
             numbers 1-8 correspond to bands of the spectral-line correlator, 
             and astroband numbers 9-24 correspond to bands of the wideband 
             correlator
       online: True to set the astroband online, False to set it offline
   """
   try:
      if subarray == BOTH:
          raise Exception, "Can't do astrobandOnline on BOTH subarrays"
      if subarray == DEFAULT and subarrayNo > 2:
          raise Exception, "Must call astrobandOnline only on science subarrays"        
      multiSubarray('astroBandOnline', subarray, band, online)
   except carma.util.UserException, ex:
      print(ex.errorMsg)

def configastroband(band, conf, bw, fcenter, sb2=AUTO, frest='none', 
               imagefrest='none', bits=CORR_2BIT, decimate=False, 
               subarray=DEFAULT, doThrow=True) :
    """Configure an astroband.  
       See the freq command for instructions on tracking lines in both
       both sidebands of a single astroband.
    Parameters:
     band: astroband number, starting at one.  Note that astroband numbers 1-8
       correspond to bands of the spectral-line correlator, and astroband 
       numbers 9-24 correspond to bands of the wideband correlator
     conf: The (string) name of the configuration to assert. Supported 
      configurations are 'LL', 'RR', 'DUALPOL', 'CARMA23', 'FULLSTOKES'. 
      No default. 
     bw: bandwidth selection for the bandwidth in MHz. One of
       BW500, BW250, BW125, BW62, BW31, BW8, BW2
     fcenter: Rest frequency for the center of the band. The frequency
       may be in GHz, OR, a molecular line name from the catalog
       (/opt/rt/conf/catalogs/SpectralLine.cat).
       The line name must be a string, enclosed in quotes, case insensitive.
       See the linefreq() command to get the freq for a spectral line.
       This parameter specifies the rest frequency that will be Doppler
       tracked by the 2nd LO.
       If you are using a line frequency and want the line to not be
       centered in the band, just add an offset to the actual line frequency.
       This can be done programmatically with something like:
        configastroband(2, "LL", BW31, linefreq('myLine')+0.010, frest=linefreq('myLine'))
     sb2:  2nd LO downconversion sideband (USB, LSB, or AUTO), default is AUTO.
       The default will choose the sideband based on the IF frequency -
       those below 3.5 GHz will be LSB, those above will we USB. This will
       keep the 2nd LOs within their range of [1.75 - 4.25] GHz,
       and favors LSB as much as possible because of better performance
       by the downconverters.  
     frest: rest frequency to use in velocity labeling of the signal sideband
       as specified by the fcenter parameter.
       Like the fcenter parameter, a line name may be entered.
       This frequency becomes the miriad header value 'restfreq' for the signal 
       spectral window.  If the default ('none') is used, fcenter will be used.
     imagefrest: rest frequency to use in labeling image sideband. 
       The image sideband is the sideband opposite the sideband of fcenter.
       Like the frest parameter a line name may be entered, and it is also a
       miriad header variable 'restfreq' for the image spectral window.
       If the default ('none') is used, fcenter will be used.
     bits: Number of bits for correlator digitization, one of CORR_2BIT,
       CORR_3BIT, CORR_4BIT, specifying 2-bit,3-bit and 4-bito
       digitization, respectively.   More bits is higher quantization
       efficiency (better sensitivity), but fewer channels. See
       CARMA Memo #46 for details. 
     decimate: Either True/False (default=False).
       When decimation=True, a Hann window is applied to the data prior to
       removal of every other channel.
     doThrow: True/False (default=True).
       When doThrow=True, this function will throw an error if the requested band is currently offline
     The setup status for a band may be seen in RTD->spectralCorrelator->Setup.
     See also freq.
    """
    if subarray == BOTH:
        raise Exception, "Can't do configastroband on BOTH subarrays"
    if subarray == DEFAULT and subarrayNo > 2:
        raise Exception, "Must call configastroband only on science subarrays"        
    # give users a clue that the behavior of this command has changed.
    if ( fcenter == 'offline' ):
        raise Exception, "Astrobands can no longer be set off line via this command. Issue 'astrobandOnline(%d,False)' to turn it offline" % band
        
    # check if the AB is online, throw if offline
    if ( isAstrobandOnline(band) == False ) :
        if(doThrow) :
            raise Exception, "Astroband %d is offline. Issue 'astrobandOnline(%d,True)' to turn it online" % (band, band)
        else:
            printError("Astroband %d is offline. Issue 'astrobandOnline(%d,True)' to turn it online" % (band, band))
            return;

    freqline          = _resolveFreq(fcenter)
    fcenter           = freqline[0]
    fcentertransition = freqline[1]
    if frest == 'none' :
        frest      = fcenter
        transition = fcentertransition
    else :
        freqline   = _resolveFreq(frest)
        frest      = freqline[0]
        transition = freqline[1]
    if imagefrest == 'none' :
        imagefrest      = 0
        imagetransition = "none"
    else :
        freqline        = _resolveFreq(imagefrest)
        imagefrest      = freqline[0]
        imagetransition = freqline[1]
    # Call helper method to return all correlator bands
    # associated with a given astroBand number and then pass that list
    # to wait().  
    cblist = makeCorrBandList(band)

    # If band is offline, don't wait for it.
    c2 = "Configuring astroband " + str(band);
    if ( len( cblist ) != 0 ) :
        c1 = "Waiting for previous commands to complete on astrobands %s" % cblist
        print c1
        rangedCb = helpers.formatAsRanges(cblist)
        c1 = "Waiting for command completion on astrobands %s" % rangedCb
        wait(CORR, cblist, 10, ALL, precomment=c1, postcomment=c2, 
                    subarray=subarray)
    else :
        rtdComment(c2)

    try:
        multiSubarray('configAstroBand', subarray,
            band, conf, bw, fcenter, sb2, frest, imagefrest, True,
            transition, imagetransition, bits)
        decimation(decimate, band, subarray=subarray)
    except carma.util.UserException, ue:
            printError( ue.errorMsg )
            raise Exception, ue.errorMsg
    
    rtdComment("")

#-----------------------------------------------------------------------
# Begin clearastroband command
#-----------------------------------------------------------------------
def clearastroband(band, subarray=DEFAULT) :

    if subarray == BOTH:
        raise Exception, "Can't do clearastroband on BOTH subarrays"
    if subarray == DEFAULT and subarrayNo > 2:
        raise Exception, "Must call clearastroband only on science subarrays"        
# NOTE: We need a helper method to return all bands (i.e. CorrelatorHandles)
# associated with a given astroBand number and then pass THAT list
# to wait().  Until then wait on ALL correlator handles.
    cblist = makeCorrBandList(band)
    # If band is offline, don't wait for it.
    if ( len( cblist ) != 0 ) :
        rangedCb = helpers.formatAsRanges(cblist)
        c1 = "Waiting for command completion on astrobands %s" % rangedCb
#        print c1
        wait(CORR, cblist, 10, ALL, precomment=c1, subarray=subarray)
        # Unlike other python methods where makeCorrBandList is used, here 
        # the subsequent call to subarrayControl is INSIDE the if statement.
        # If cblist is empty, there are no astrobands to clear.
        try:
            multiSubarray('clearAstroBand', subarray, band)
        except carma.util.UserException, ue:
            printError( ue.errorMsg )

#-----------------------------------------------------------------------
# End clearastroband command
#-----------------------------------------------------------------------
def enableCorrelation( band=range(9,25), enable=True ) :     
     """ Enable or disable correlations on COBRA widebands.  
         This command is to disable correlations when the temperature in     
         the correlator room is alarmingly high and re-enable them when  
         the temperature is normal again.   This command     
         only affects the wideband correlator, astrobands 9-24.  
         Parameters:     
            band: astroband number, starting at one.  Note that astroband    
                  numbers 1-8 correspond to bands of the spectral-line   
                  correlator, and astroband numbers 9-24 correspond to   
                  bands of the wideband correlator. Default: 9-24 inclusive.     
          enable: True to enable correlations, False to disable. Default: True   
     """     
     
     if ( not subarrayOwnsCorrelator(subarrayNo , CORR_WIDEBAND) ) :     
         print "This command is only for the Wideband correlation and this subarray does not own the Wideband correlator."   
         return  
     
     try:    
         bandlist = helpers.makeList(band)   
         print bandlist  
         s.enableCorrelation(bandlist , enable )     
     except Exception, ex:   
         print(ex.errorMsg)  
     
def disableCorrelation( band=range(9,25) ) :    
      """Disable correlations on COBRA widebands.    
         This command is to be used when the temperature in  
         the correlator room is alarmingly high.   This command  
         only affects the wideband correlator, astrobands 9-24.  
         Parameters:     
            band: astroband number, starting at one.  Note that astroband    
                  numbers 1-8 correspond to bands of the spectral-line   
                  correlator, and astroband numbers 9-24 correspond to   
                  bands of the wideband correlator. Default: 9-24 inclusive.     
      """    
      enableCorrelation(band, False)

def addCorrelator(corrType) :
    """Add the correlator of the given type to this subarray.
       Parameters:
         corrType: one of CORR_WIDEBAND, CORR_SPECTRAL, CORR_C3GMAX8, 
                   or CORR_C3GMAX23 to add the wideband, spectral, 
                   C3GMAX8, and C3GMAX23 correlators respectively.  If 
                   you try to add a correlator owned by another subarray, 
                   you will get an error message.  Furthermore, you cannot
                   have both C3G correlators in the same subarray.
    """
    try :
      s.addCorrelator(corrType)
    except Exception, ex:
        print(ex.errorMsg)
        e  = "Unable to add the %s correlator. If it is currently\n" % CORR_STRING[corrType]
        e += "owned by another subarray, you will have to issue\n"
        e += "removeCorrelator(%s) in that subarray first." % CORR_STRING[corrType]
        print e 
       

def removeCorrelator(corrType) :
    """Remove the correlator of the given type to this subarray.
       Parameters:
         corrType: one of CORR_WIDEBAND, CORR_SPECTRAL, CORR_C3GMAX8, 
                   or CORR_C3GMAX23 to remove the wideband, spectral, 
                   C3GMAX8, and C3GMAX23 correlators respectively.  If 
                   you try to remove a correlator owned by another subarray, 
                   you will get an error message.  
    """
    try :
      s.removeCorrelator(corrType)
    except Exception, ex:
        print(ex.errorMsg)

def assignedCorrelator(subarrayNo) :
    """Return the type of correlator assigned to a given subarray as a string: 
      'SPECTRAL', 'WIDEBAND','C3GMAX23', 'C3GMAX8', 
       or 'NONE' (not Python None!).  If more than one correlator is 
       assigned, the return value is a combination, e.g., 
       'SPECTRAL+WIDEBAND'
       This is useful for scripts like fringecheck which can't run 
       with 'NONE' or multiple correlators and so must check the 
       assignment before proceeding.
       Parameters: 
           subarrayNo:  The subarray number
     """
    return s.ownedCorrelator(subarrayNo);

def astrobandCorrelator(astroband) :
    """Return the type of correlator assigned to a given astroband as a string: 
      'SPECTRAL', 'WIDEBAND','C3GMAX23', 'C3GMAX8', 
       or 'NONE' (not Python None!).  If more than one correlator is 
       assigned, the return value is a combination, e.g., 
       'SPECTRAL+WIDEBAND'
       This is useful for scripts like fringecheck which can't run 
       with 'NONE' or multiple correlators and so must check the 
       assignment before proceeding.
       Parameters: 
           astroband:  The astroband number
    """
    return s.astrobandCorrelator(astroband);

def subarrayOwnsCorrelator(subNo, corrType) :
    """Return true if subNo owns correlator corrType
       Parameters:
           subNo:  The subarray number
           corrType: one of CORR_WIDEBAND, CORR_SPECTRAL, CORR_C3GMAX8, 
                   or CORR_C3GMAX23 to remove the wideband, spectral, 
                   C3GMAX8, and C3GMAX23 correlators respectively.  
    """
    return s.subarrayOwnsCorrelator(subNo, corrType);

def subarrayOwnsSingleCorrelator(subNo) :
    """Return true if subNo owns a single correlator
       Parameters:
           subNo:  The subarray number
    """
    return s.subarrayOwnsSingleCorrelator(subNo);

def subarrayOwnsMultipleCorrelators(subNo) :
    """Return true if subNo owns multiple correlators
       Parameters:
           subNo:  The subarray number
    """
    return not(s.subarrayOwnsSingleCorrelator(subNo) or s.subarrayOwnsNoCorrelator(subNo));

def subarrayOwnsNoCorrelator(subNo) :
    """Return true if subNo owns no correlator
       Parameters:
           subNo:  The subarray number
    """
    return s.subarrayOwnsNoCorrelator(subNo);

#---------------------------------------------------------------
# Signal Path Switchyard configuration commands
#---------------------------------------------------------------

def loadconfig(file, name , conf, subarray=DEFAULT) :
    """ Load a signal path switchyard configuration 
    Parameters:
        file - name file containing configuration
        name - The name to assign the configuration
        conf - The name of the astroband configuration to associate with this siwtchyard configuration
    """
    # do we really need multiSubarray for this?
    if subarray == BOTH:
        raise Exception, "Can't do loadconfig on BOTH subarrays"
     # is the following true?
    if subarray == DEFAULT and subarrayNo > 2:
        raise Exception, "Must call loadconfig only on science subarrays"        
    # do we need a wait(SIGNALPATH) construct?

    try :
      multiSubarray('loadConfiguration', subarray, file, name, conf)
    except Exception, ex:
        print(ex.errorMsg)

def queryconfig():
    """Query current signal path switchyard configuration """
    print s.queryConfiguration()

def initcablemap(file, subarray=DEFAULT) :
    """ Initialize a cable map, specifying switchyard hardware connections
       Parameter:
        file The name of the file containing the cable map 
    """
    # do we really need multiSubarray for this?
    if subarray == BOTH:
        raise Exception, "Can't do initcablemap on BOTH subarrays"
     # is the following true?
    if subarray == DEFAULT and subarrayNo > 2:
        raise Exception, "Must call initcablemap only on science subarrays"        
    # do we need a wait(SIGNALPATH) construct?

    try :
      multiSubarray('initializeCableMap', subarray, file)
    except Exception, ex:
        print(ex.errorMsg)

#---------------------------------------------------------------
# End Signal Path Switchyard configuration commands
#---------------------------------------------------------------

def configwideastroband(conf="DEFAULT", bits=None, decimate=False, 
            subarray=DEFAULT, dotsys=True, optflat=True):
    """Configure all the correlator bands into a wideband configuration
    at the current LO frequency. After configuring the bands a
    tsys(ifsetup=True) is done.
    Parameter:
      conf: The astroband configuration, one of "LL", "RR", "FULLSTOKES", 
            "CARMA23" Default: "LL" 
      bits: an enumertion for number of correlator bits. Defaults to CORR_3BIT in 
            single crate modes (LL) and CORR_2BIT in dual crate modes (CARMA23). 
            This parameter is ignored in sci2.
      decimate: controls decimation, default=False. 
            When True, Hanning smoothing is applied prior to removing
            every other channel.
      subarray: either SCI1/SCI2/BOTH or DEFEAULT; default=DEFAULT
      dotsys: does a tsys(ifsetup=True); default=True
      optflat: If True, call optimizeThresholds and flattenPhases on spectral bands. This ensures observing readiness.  Default: True
    See also configastroband, freq
    ** NOTE: This command will clear all astroband configurations in the
             current subarray before reconfiguring astrobands.
    """
    if subarray == BOTH:
        raise Exception, "Can't do configwideastroband only on BOTH subarrays"        
    subNo = subarrayNo
    if subarray == SCI1: subNo = 1
    if subarray == SCI2: subNo = 2
    if subNo > 2:
        raise Exception, "Must call configwideastroband on science subarrays"        
    sacmp = 'Control.Subarray%d.' %subNo
    fmp = sacmp + 'Commands.Freq.'
    flo,frest,fif = queryMpValues([sacmp+'loFreq',fmp+'restFreq',fmp+'ifFreq'])
    sb = queryString(fmp+'sideband')
    if sb == 'USB':   fsky1 = flo + fif
    else:             fsky1 = flo - fif
    fsky = 1.0
    try:
        fsky = queryDouble(sacmp+"skyfreq")
    except: 
        pass
    if frest > 1.0:   dopfac = fsky/frest
    else:             dopfac = 1.0
    if dopfac < 0.03: dopfac = 1.0
    if False:
        print "Flo:", flo
        print "Frest:", frest
        print "Fif:", fif
        print "sb:", sb
        print "Fsky:", fsky
        print "Fsky1:", fsky1
        print "dopfac:", dopfac
        print "conf:", conf  

    # clear all astroband configurations in this subarray
    clearastroband(0)

    # if correlator is spectral or any    
    if (subarrayOwnsCorrelator(subNo, CORR_SPECTRAL)) :
        oddBands = [ 1, 3, 5, 7 ]
        evenBands = [ 2, 4, 6, 8 ]
        allBands = oddBands + evenBands
        allBands.sort()

        if ( conf == "DEFAULT" ):
            conf="LL"
            bandlist = allBands
            if bits == None: 
                bits = CORR_3BIT
        elif ( conf == "LL" ):
            bandlist = allBands
            if bits == None: 
                bits = CORR_3BIT
        elif ( conf == "RR" ):
            bandlist = allBands
            if bits == None: 
                bits = CORR_3BIT
        elif ( conf == "MAXSENS_DUALPOL" ):
            bandlist = allBands
            if bits == None: 
                bits = CORR_3BIT
        elif ( conf == "MAXSENS_LL" ):
            bandlist = allBands
            if bits == None: 
                bits = CORR_3BIT
        elif ( conf == "CARMA23" ):
            bandlist = oddBands
            if bits == None:
                bits = CORR_2BIT
        elif ( conf == "MAXSENS_CARMA23_LOWRES" ):
            bandlist = oddBands
            if bits == None: 
                bits = CORR_2BIT
        elif ( conf == "MAXSENS_CARMA23_MEDRES" ):
            bandlist = oddBands
            if bits == None: 
                bits = CORR_2BIT
        elif ( conf == "MAXSENS_CARMA23_HIGHRES" ):
            bandlist = oddBands
            if bits == None: 
                bits = CORR_2BIT
        elif ( conf == "FULLSTOKES" ):
            bandlist = oddBands
            if bits == None: 
                bits = CORR_2BIT
        elif ( conf == "DUALPOL" ):
            bandlist = oddBands
            if bits == None: 
                bits = CORR_3BIT
        else :
            m = "Unrecognized astroband configuration: %s" % conf
            m += "\nValid configs are LL, RR, DUALPOL, CARMA23, FULLSTOKES,"
            m += " MAXSENS_LL, MAXSENS_DUALPOL, MAXSENS_CARMA23_LOWRES,"
            m += "MAXSENS_CARMA23_MEDRES, MAXSENS_CARMA23_HIGHRES"
            raise Exception, m

        # Special case 1cm system to avoid birdies. These commands
        # came from Tom Plagge.
        if ( flo < 50 ) :
            if ( (conf == "CARMA23") | (conf.find('MAXSENS') == 0)) :
                print "Configuring to avoid birdies at 1-cm..."
                print "configastroband(1, \"%s\", BW500, 35.938-3.25-0.25, AUTO)" % conf
                configastroband(1, conf, BW500, (35.938-3.25-0.25)/dopfac, AUTO, doThrow=False)
                print "configastroband(3, \"%s\", BW500, 35.938-4.25-0.25, AUTO)" % conf
                configastroband(3, conf, BW500, (35.938-4.25-0.25/dopfac), AUTO, doThrow=False)
                print "configastroband(5, \"%s\" , BW500, 35.938-6.039-0.25, AUTO)" % conf
                configastroband(5, conf, BW500, (35.938-6.039-0.25)/dopfac, AUTO, doThrow=False)
                print "configastroband(7, \"%s\", BW500, 35.938-7.05-0.25, AUTO)" % conf
                configastroband(7, conf, BW500, (35.938-7.05-0.25)/dopfac, AUTO, doThrow=False)
            elif ( conf == "LL" ) :
                print "Configuring to avoid birdies at 1-cm..."
                print "configastroband(1, \"%s\" , BW500, 35.938-3.25-0.25, AUTO)" % conf
                configastroband(1, conf, BW500, (35.938-3.25-0.25)/dopfac, AUTO, doThrow=False)
                print "configastroband(2, \"%s\" , BW500, 35.938-4.25-0.25, AUTO)" % conf
                configastroband(2, conf, BW500, (35.938-4.25-0.25)/dopfac, AUTO, doThrow=False)
                print "configastroband(3, \"%s\" , BW500, 35.938-5.25-0.25, AUTO)" % conf
                configastroband(3, conf, BW500, (35.938-5.25-0.25)/dopfac, AUTO, doThrow=False)
                print "configastroband(4, \"%s\" , BW500, 35.938-6.05-0.25, AUTO)" % conf
                configastroband(4, conf, BW500, (35.938-6.05-0.25)/dopfac, AUTO, doThrow=False)
                print "configastroband(5, \"%s\" , BW500, 35.938-6.45-0.25, AUTO)" % conf
                configastroband(5, conf, BW500, (35.938-6.45-0.25)/dopfac, AUTO, doThrow=False)
                print "configastroband(6, \"%s\" , BW500, 35.938-7.03-0.25, AUTO)" % conf
                configastroband(6, conf, BW500, (35.938-7.03-0.25)/dopfac, AUTO, doThrow=False)
                print "configastroband(7, \"%s\" , BW500, 35.938-7.46-0.25, AUTO)" % conf
                configastroband(7, conf, BW500, (35.938-7.46-0.25)/dopfac, AUTO, doThrow=False)
                print "configastroband(8, \"%s\" , BW500, 35.938-8.05-0.25, AUTO)" % conf
                configastroband(8, conf, BW500, (35.938-8.05-0.25)/dopfac, AUTO, doThrow=False)
            else : 
                m = "Astroband configuration: %s" % conf
                m += " is not supported for 1-cm observing. "
                m += "\nValid configs are LL and CARMA23"
                raise Exception, m
        else :
            # Center bands within the lower IF band, overlapping each band 
            # by 50 MHz
            fifLow           = 1.0   # IF starts at 1.0 GHz
            fifHigh          = 5.0   # Top of the lower part of the IF band
            bandCenterOffset = 0.250 # 0.25GHz to get to center of a 500MHz band
            beginBandOffset = ((fifHigh-fifLow)-(len(bandlist)*0.450 + 0.050))/2
            for i in range(len(bandlist)) :
                bandNo = bandlist[i]
                fifband = fifLow + beginBandOffset + bandCenterOffset + i*0.45
                if sb == 'USB': f = (flo + fifband)/dopfac
                else:           f = (flo - fifband)/dopfac               
                #print bandNo, "%.3f  %.3f  %.3f " %(flo, fifband, f)
                print "configastroband(%d,\"%s\",BW500,%f,bits=%s,decimate=%s)" \
                        %(bandNo, conf, f, bits , decimate )
                configastroband(bandNo, conf, BW500, f, bits=bits, 
                        decimate=decimate, subarray=subarray, doThrow=False)

    # if correlator is wideband or any, we configure bands 9-24
    if (subarrayOwnsCorrelator(subNo, CORR_WIDEBAND)) :
        if ( conf == "DEFAULT" ):
            conf="LL"
        bits = CORR_2BIT
        is1cm = flo < 50
        for i in range(16):
            lo2sb = USB
            if i < 3 or i > 12: lo2sb = LSB
            fif = 1.0+i*0.5+0.25
            if is1cm: 
                fsky = flo - fif
            else:     
                fsky = flo + fif
            m  = "configastroband(%d,\"%s\",BW500,%f," %(i+9, conf, fsky) 
            m += "carma.control.%s,bits=%s,decimate=%s)"%(lo2sb, bits, decimate)
            print m
            configastroband(i+9, conf, BW500, fsky, lo2sb, bits=bits, 
                    decimate=decimate, subarray=subarray, doThrow=False)

    # for C3G bands,configure them identical to SPECTRAL until we learn otherwise.
    if (subarrayOwnsCorrelator(subNo, CORR_C3GMAX23)) :
        if bits == None:
            bits = CORR_3BIT;
        if (conf == "DEFAULT"):
            conf = "LL";
 
        bandlist=range(25,33)
        # Center bands within the lower IF band, overlapping each band by 50 MHz
        fifLow           = 1.0   # IF starts at 1.0 GHz
        fifHigh          = 5.0   # Top of the lower part of the IF band
        bandCenterOffset = 0.250 # 0.25 GHz to get to center of a 500MHz band
        beginBandOffset = ((fifHigh-fifLow)-(len(bandlist)*0.450 + 0.050))/2
        for i in range(len(bandlist)) :
            bandNo = i+25
            fifband = fifLow + beginBandOffset + bandCenterOffset + i*0.45
            if sb == 'USB': f = (flo + fifband)/dopfac
            else:           f = (flo - fifband)/dopfac               
            #print bandNo, "%.3f  %.3f  %.3f " %(flo, fifband, f)
            print "configastroband(%d,\"%s\",BW500,%f,bits=%s,decimate=%s)" \
                    %(bandNo, conf, f, bits , decimate )
            configastroband(bandNo, conf, BW500, f, bits=bits, 
                    decimate=decimate, subarray=subarray, doThrow=False)

    if (subarrayOwnsCorrelator(subNo, CORR_C3GMAX8)) :
        if bits == None:
            bits = CORR_3BIT;
        if (conf == "DEFAULT"):
            conf = "LL";
 
        bandlist=range(33,41)
        # Center bands within the lower IF band, overlapping each band by 50 MHz
        fifLow           = 1.0   # IF starts at 1.0 GHz
        fifHigh          = 5.0   # Top of the lower part of the IF band
        bandCenterOffset = 0.250 # 0.25 GHz to get to center of a 500MHz band
        beginBandOffset = ((fifHigh-fifLow)-(len(bandlist)*0.450 + 0.050))/2
        for i in range(len(bandlist)) :
            bandNo = i+33
            fifband = fifLow + beginBandOffset + bandCenterOffset + i*0.45
            if sb == 'USB': f = (flo + fifband)/dopfac
            else:           f = (flo - fifband)/dopfac               
            #print bandNo, "%.3f  %.3f  %.3f " %(flo, fifband, f)
            print "configastroband(%d,\"%s\",BW500,%f,bits=%s,decimate=%s)" \
                    %(bandNo, conf, f, bits , decimate )
            configastroband(bandNo, conf, BW500, f, bits=bits, 
                    decimate=decimate, subarray=subarray, doThrow=False)
    elif ( subNo > 2 ) :
        m =  "configwideastroband cannot be done for subarray#%d" %subNo
        raise Exception, m

    if dotsys :
        print "Waiting for correlator commands to complete before doing Tsys"
        wait(CORR,[0],30,ALL)
        print "Doing tsys(ifsetup=True)"
        tsys(ifsetup=True, subarray=subarray)

    if (subarrayOwnsCorrelator(subNo, CORR_SPECTRAL)) :
        if (optflat == True ) :
           print "Optimizing thresholds on spectral bands"
           optimizeThresholds()
           print "Flattening phases on spectral bands"
           flattenPhases()

def optimizeThresholds(band=0, tmo=7) :
    """ Optimize thresholds in CARMA hardware bands (no-op for COBRA bands).
        This command will set a sequence number to be returned
        by the correlator library code when complete.
        Note: This command turns the noise source ON, tells the bands to
        optimize thresholds, waits for completion (or timeout) and
        then turns the noise source OFF.

         Parameters: 
             band: A single or list of Astroband numbers. Zero means all 
                   Astrobands.
             tmo:  timeout for wait on for this command to complete.
                   Default: 7 seconds 
    """
    # Optimize thresholds should take <5 seconds, so set tmo to 7
    bandlist = helpers.makeList(band)
    cblist = makeCorrBandList(band)
    # If band is offline, don't wait for it.
    if ( len( cblist ) != 0 ) :
        rangedCb = helpers.formatAsRanges(cblist)
        c1 = "Waiting for astrobands %s before turning noise on" % rangedCb
    # previous command was probably configastroband, which could take 
    # 30 seconds if loading a new FPGA configuration.
        wait(CORR, cblist, 40, ALL, precomment=c1)

    noiseon()

    if ( band == 0 ) :
        cstr = "Optimizing thresholds on all Astrobands"
    else :
        rangedAb = helpers.formatAsRanges(bandlist)
        cstr = "Optimizing thresholds on Astroband(s) %s " % ( rangedAb )
    rtdComment( cstr )
    s.optimizeThresholds( bandlist );
    if ( len( cblist ) != 0 ) :
        rangedCb = helpers.formatAsRanges(cblist)
        c1 = "Waiting for astrobands %s before turning noise off" % rangedCb
        wait(CORR, cblist, tmo, ALL, precomment=c1)

    noiseoff()

def flattenPhases( band = 0, tmo = 45 ) :
    """ Flatten phases on CARMA hardware bands (no-op for COBRA bands).
        This command will set a sequence number to be returned
        by the correlator library code when complete.
        Note: This command turns the noise source ON, tells the bands
        to flatten phases, waits for completion (or timeout) and
        then turns the noise source OFF.

         Parameters: 
             band: A single or list of AstroBand numbers. Zero means 
                   all AstroBands in this subarray's correlator.
             tmo:  timeout for wait on for this command to complete.
                   Default: 45 seconds 
    """
    bandlist = helpers.makeList(band)
    cblist = makeCorrBandList(band)
    # If band is offline, don't wait for it.
    if ( len( cblist ) != 0 ) :
        rangedCb = helpers.formatAsRanges(cblist)
        c1 = "Waiting for astrobands %s before turning noise on" % rangedCb
    # previous command was probably configastroband, which takes 5 seconds 
        wait(CORR, cblist, 7, ALL, precomment=c1)

    noiseon()

    if ( band == 0 ) :
        cstr = "Flattening phases on all AstroBands"
    else :
        rangedAb = helpers.formatAsRanges(bandlist)
        cstr = "Flattening phases on AstroBand(s) %s " % ( rangedAb )
    rtdComment( cstr )
    s.flattenPhases( bandlist );
    # Flatten phases may take up to 30 seconds in fullstokes or carma23 mode, 
    # so tmo is 45
    if ( len( cblist ) != 0 ) :
        rangedCb = helpers.formatAsRanges(cblist)
        c1 = "Waiting for astrobands %s before turning noise off" % rangedCb
        wait(CORR, cblist, tmo, ALL, precomment=c1)

    noiseoff()

def calibrateSpectra( band = 0, noiseEnable = True, intTime = 10.0, tmo = 45) :
    """Measure spectral calibration on CARMA hardware bands (no-op for 
       COBRA bands).
       This command will set a sequence number to be returned
       by the correlator library code when complete.
       Note: This command turns the noise source to the requested state, 
       tells the bands to calibrate, waits for completion (or timeout) and
       then turns the noise source OFF.
         Parameters:
              band:   A single or list of astroband numbers. Zero means all 
                      astrobands.  Default: 0
              noiseEnable:  Calibration with the noise source on (True) 
                      or off (False).
                      Default: True
              intTime:  Integration time in seconds. Default: 10
              tmo:    timeout for wait on for this command to complete.
                      Default: 45 seconds )
    """
    bandlist = helpers.makeList(band)
    cblist = makeCorrBandList(band)
    # If band is offline, don't wait for it.
    if ( len( cblist ) != 0 ) :
        rangedCb = helpers.formatAsRanges(cblist)
        if ( noiseEnable ) :  noisestate = "on"
        else               :  noisestate = "off"
        c1 = "Waiting for astrobands %s before turning noise %s" % (rangedCb,noisestate)
    # previous command was flattenphases, which can take 30 seconds in
    # fullstokes or carma23 mode.
        wait(CORR, cblist, 45, ALL, precomment=c1)

    if ( noiseEnable ) : noiseon(reference=True)
    else               : noiseoff()
    if ( band == 0 ) :
        cstr = "Performing spectral calibration on all Astrobands"
    else :
        rangedAb = helpers.formatAsRanges(bandlist)
        cstr = "Performing spectral calibration on Astroband(s) %s " % ( rangedAb )
    rtdComment( cstr )

    #@todo: At some point, perhaps, put the double call inside CorrelatorHandle;
    # this is not critical however.
    # Call once to cache the reference spectrum
    cache=True
    enable=False
    s.calibrateSpectra(bandlist,noiseEnable,intTime,cache,enable)
    # now integrate to output the reference spectrum
    cstr = "Integrating on reference spectrum"
    rtdComment( cstr )
    integrate(intTime,1)

    # Call again to enable calibration with just measured reference spectrum.
    # This will cause all subsequent integrations to be complex-divided by
    # reference spectrum.
    cache=False
    enable=True
    cstr = "Enabling on-line passband calibration"
    rtdComment( cstr )
    s.calibrateSpectra(bandlist,noiseEnable,intTime,cache,enable)
    # If band is offline, don't wait for it.
    if ( len( cblist ) != 0 ) :
        rangedCb = helpers.formatAsRanges(cblist)
        c1 = "Waiting for astrobands %s before turning noise off" % rangedCb
        wait(CORR, cblist, tmo, ALL, precomment=c1)
    noiseoff()

def calbands( band = 0, tmo = 30 ) :
    """Performs threshold optimization, phase flattening, and passband calibration on the
       input bands.  See help('optimizeThresholds'), help('flattenPhases'), and
       help('calibrateSpectra') for documentation on the individual commands.

         Parameters: 
             band: A single or list of band numbers. Zero means all bands.
             tmo:  timeout for wait on for each step to complete.
                   Default: 30 seconds 
    """
    optimizeThresholds(band,tmo)
    flattenPhases(band,tmo)
    calibrateSpectra(band=band,tmo=tmo)

def checkbands() :
    """Checks that all correlator bands have valid configurations.
    Throws an exception if any band is online and has an invalid
    configuration (2nd LO out of range). See RTD=>spectralCorrelator=>Setup
    for details on individual bands.
    It is recommended that all observing scripts include this command after
    the spectral setup has been completed (all freq and configastroband commands
    issued).
    See also: freq, configastroband,  RTD=>spectralCorrelator=>Setup"""
    dontThrowException = False   
    success = s.checkConfig(dontThrowException)
    return success

def assertCorrConfig() :
    """Assert correlator configuration. Currently C3G only."""
    # eventually we will want a wait on C3Gmax8 or C3GMax24 astrobands 
    if ( subarrayOwnsCorrelator(subarrayNo,CORR_WIDEBAND) or subarrayOwnsCorrelator(subarrayNo,CORR_SPECTRAL) ) :
       print "This command is currently a no-op for non-C3G correlators"
       return
    if ( subarrayOwnsNoCorrelator( subarrayNo ) ) :
       print "You need to add a C3G correlator to use this command!"
       return
    s.assertCorrelatorConfiguration()

# ------------------------- Project and Obsblock ---------------------------
def restructurePacsObsblock( obsblockID ) :
    """Reorganizes an obsblock name from sci1 to sci2
       Parameters :
            obsblockID: The obsblock name to restructure

       Returns :
            pacs obsblock ID

       e.g. turns 1A_115NGC000 to 1A_30NGC000

    """
    substrings = obsblockID.split("_", 1)
    if( len( substrings ) != 2 ) :
        raise Exception, "Unexpected obsblock name %s" % (obsblockID)
    start = 2
    if( int( substrings[1][0] ) < 3):
        start = 3
    pObsblockID = substrings[0] + "_30" + substrings[1][start:]
    return pObsblockID

def newPacsProject( projectName, obsblockName, subObsblockName='', pdb=True,
                    attempt=1) :
    """Begin observations for a project.
    This command will contact the project database to validate the
    input project.obsblock.  Validation means that
    the project.obsblock already exists in the
    project database or it is a commissioning project (ctNNN), summer school
    project (csNNN), or special project (see below)--which also
    must have an entry in the project database.   If the project
    database is unreachable, the inputs will be considered valid,
    to be sorted out later.
    It also validates the PACS obsblock project.obsblock.PACS. 

    Parameters:
    projectName: Usually a ct or cx number (sometimes the date). No default.

    obsblockName: The obsblock value created for the project or an
                  arbitrary one for a commissioning project.  No default.

    subObsblockName: The subObsblock value created for the project or an
                     arbitrary one for a commissioning project.  No default.

    pdb: use the pdb

    attempt: internal tracking of getting trial numbers

    The observing constraints are set to default values.  No default observing
    intent is set unless projectName is one of 'base', 'fringe', 'flux',
    'test', 'none', 'tilt', 'rpnt', 'opnt'.
    """
    myThrow("Cannot currently run PACS projects")
    if(obsblockName.lower() == "none") :
        m  = obsblockName + " is a protected name and cannot be used "
        m += "as an obsblock name" % (obsblockName)
        raise Exception, m
    if(pdb) :
        rtdComment("Using Project Database", BOTH)
        # first validate the obsblock
        obsblockID = projectName + '.' + obsblockName
        if( subObsblockName != '' ) :
            obsblockID += '.' + subObsblockName
        trialNumber = getCurrentTrial(obsblockID)
        if( trialNumber == -1 ) :
            return newPacsProject(projectName, obsblockName, subObsblockName, 
                pdb=False)
        elif( trialNumber == -2 ) :
            raise Exception, "Could not validate obsblock %s" %obsblockID
        elif( trialNumber == 0 ) :
            # new commissioning project
            pass
        trialNumber = requestNewTrial( obsblockID )
        pObsblockName = restructurePacsObsblock( obsblockName )
        pObsblockID = projectName + "." + pObsblockName
        pSubObsblockName = subObsblockName
        if( pSubObsblockName != '' ) :
            pSubObsblockName += "-PACS"
            pObsblockID += '.' + pSubObsblockName
        else :
            pSubObsblockName = "PACS"
            pObsblockID += ".PACS"
                                         
        pTrialNumber = getCurrentTrial( pObsblockID )
        if( pTrialNumber == -1 ) :
            return newPacsProject(projectName, obsblockName, subObsblockName,
                         pdb=False)
        elif( pTrialNumber == -2 ) :
            piv = carma.observertools.ItemValue("project",projectName)
            oiv = carma.observertools.ItemValue("obsblock",pObsblockName)
            tPrj = subarray.queryProject([piv,oiv])
            if(len(tPrj) < 1) :
                noiv = carma.observertools.ItemValue("newObsblock",pObsblockName)
                success = subarray.projectEdit(projectName,obsblockName,"none",-1,[noiv],carma.observertools.ESTATUS_REPLICATE)
                sleep(0.5)
                if(not success) :
                    raise Exception, "Could not create PACS subObsblock."
            riv = carma.observertools.ItemValue("newSubObsblock","PACS")
            success = subarray.projectEdit(projectName,pObsblockName,"",-1,[riv],carma.observertools.ESTATUS_RENAME)
            if(not success) :
                raise Exception, "Could not create PACS subObsblock."
            pSubObsblockName = "PACS"
            pObsblockID = projectName + "." + pObsblockName + "." + pSubObsblockName
            pTrialNumber = getCurrentTrial( pObsblockID )
            if( pTrialNumber == -1 ) :
                return newPacsProject(projectName, obsblockName, "PACS",
                                      pdb=False)
            elif( pTrialNumber == -2 ) :
                raise Exception, "Could not validate obsblock %s.%s.PACS" % (projectName,pObsblockName)
        pTrialNumber = requestNewTrial( pObsblockID)
        if( trialNumber > pTrialNumber ) :
            while( trialNumber != pTrialNumber ) :
                pTrialNumber = requestNewTrial( pObsblockID )
                time.sleep(1)

            if( trialNumber != pTrialNumber ) :
                if( attempt > 1 ) :
                    raise Exception,"Trial numbers do not match, but should"
                else :
                    return newPacsProject( projectName, obsblockName, subObsblockName, pdb,attempt=2)
        else :
            while( trialNumber != pTrialNumber ) :
                trialNumber = requestNewTrial( obsblockID )
                time.sleep(1)
            if( trialNumber != pTrialNumber ) :
                if( attempt > 1 ) :
                    raise Exception,"Trial numbers do not match, but should"
                else :
                    return newPacsProject( projectName, obsblockName, subObsblockName, pdb,attempt=2)

        rtdComment( "Trial is " + str( trialNumber ), BOTH )
        if ( s.isCommissioning( projectName ) ) :
            projectName = projectName.lower()
        multiSubarray('setObsblock', SCI1, 
            projectName,obsblockName,subObsblockName,trialNumber)
        multiSubarray('setObsblock', SCI2, 
            projectName,pObsblockName,pSubObsblockName,trialNumber)
        constraints(subarray=SCI1)
        constraints(subarray=SCI2)
    
        # failure to contact PDB is not sufficient to stop observing.
        sname = s.getScriptName()
        pnameupper = projectName.upper()
        # don't bother adding script in for fringe or rpnt
        if ( sname.upper() == "NONE" or pnameupper == "RPNT" or pnameupper == "FRINGE" ) :
            trackMessage("No script being added for " + obsblockID)
        else :
            try :
                addScript( obsblockID, script=sname )
            except carma.observertools.ProjectDatabaseException, ex :
                estr = "###  WARNING: unable to add script " + sname \
                 + " to project database. Continuing on anyway.  ###"
                rtdComment( estr,BOTH )
                trackMessage( estr )
            except Exception, ex :
                estr = "###  WARNING: unable to add script " + sname \
                 + " to project database. Continuing on anyway.  ###"
                rtdComment( estr,BOTH )
                trackMessage( estr )

        obsblockID += '.' + str( trialNumber )
        return obsblockID
    
    else :
        rtdComment("Not using Project Database", BOTH)
        trialNo = myIncrementTrial( projectName, obsblockName, subObsblockName, 
                        False)
        rtdComment( "Trial is " + str( trialNo ), BOTH)
        multiSubarray('setObsblock', SCI1,
            projectName, obsblockName, subObsblockName, trialNo)
        multiSubarray('setObsblock', SCI2,
            projectName, obsblockName, "PACS", trialNo)
        obsblockID = projectName + '.' + obsblockName + '.' + str(trialNo)
        multiSubarray('setScriptInt', s1, odi.INDX_INT_OBSTRIAL, trialNo )
        constraints(subarray = BOTH)
        return obsblockID

def newProjectFromPdb(projectName, obsblockName, subObsblockName='', 
        subarray=DEFAULT, isDualCorr = False ) :
    """Begin observations for a project.
    This command will contact the project database to validate the
    input project.obsblock.[subobsblock].  Validation means that
    the project.obsblock.[subobsblock] already exists in the
    project database or it is a commissioning project (ctNNN), summer school
    project (csNNN), or special project (see below)--which also
    must have an entry in the project database.   If the project
    database is unreachable, the inputs will be considered valid,
    to be sorted out later.

    Parameters:
    projectName: Usually a ct or cx number (sometimes the date). No default.

    obsblockName: The obsblock value created for the project or an
                  arbitrary one for a commissioning project.  No default.

    subObsblockName: The subObsblock value created for the project or an
                     arbitrary one for a commissioning project.
                     Default is blank.


    The observing constraints are set to default values.  No default observing
    intent is set unless projectName is one of 'base', 'fringe', 'flux',
    'test', 'none', 'tilt', 'rpnt', 'opnt'.
    """

    # use rtdComment because we don't want to call addComment() for this
    # as the obsblockID is not yet set.
    print "NPFPDBB: %s,%s,%s DUALCORR=%s" % (projectName, obsblockName, subObsblockName,isDualCorr )
    if(obsblockName == "none" or obsblockName == "NONE" 
                              or obsblockName == "None") :
        m = " is a protected name and cannot be used as an obsblock name"
        raise Exception, obsblockName + m
    rtdComment("Using Project Database", subarray)

    if ( s.isCommissioning( projectName ) ) :
          projectName = projectName.lower()
    sname = multiSubarray('getScriptName', subarray)
    pnameupper = projectName.upper()
    catalog = ""
    # don't bother adding script in for fringe or rpnt
    if ( sname.upper() == "NONE" or pnameupper == "RPNT" or pnameupper == "FRINGE" ) :
        sname = ""

    print "requestNewTrial( %s,%s,%s,%s,%s, %s)" % ( projectName, obsblockName, subObsblockName, sname, catalog, isDualCorr )
    trialNo = requestNewTrial( projectName, obsblockName, subObsblockName, sname, catalog, isDualCorr )
    rtdComment( "Trial is " + str( trialNo ), subarray)

    # commissioning projects are all lower case in the PDB!
    obsblockID = projectName + '.' + obsblockName
    if(isDualCorr) :
        sID = subObsblockName
        wID = subObsblockName
        if(subObsblockName != '') :
            sID += "-SL"
            wID += "-WB"
        else :
            sID += "SL"
            wID += "WB"
        obsblockID += "." + sID
        multiSubarray('setAllObsblocks',DEFAULT,projectName,obsblockName,sID,trialNo,projectName,obsblockName,wID,trialNo)
        constraints(subarray=DEFAULT)
    else :
        multiSubarray('setObsblock', subarray, 
                      projectName,obsblockName,subObsblockName,trialNo)
        constraints(subarray=subarray)

    obsblockID += '.' + str( trialNo )
    return obsblockID

def oldnewProject( projectName, obsblockName, subObsblockName='', 
                   subarray=DEFAULT,isDualCorr=False) :
    """ Version of newProject to be used WITHOUT project database.
    This method should be removed when the PDB is deployed.
    """
    if(obsblockName == "none" or obsblockName == "NONE"
                              or obsblockName == "None") :
        m = " is a protected name and cannot be used as an obsblock name"
        raise Exception, obsblockName + m
    rtdComment("Not using Project Database", subarray)
    trialNo = myIncrementTrial( projectName, obsblockName, subObsblockName, False,isDualCorr)
    rtdComment( "Trial is " + str( trialNo ), subarray)
    multiSubarray('setObsblock', subarray,
                  projectName,obsblockName,subObsblockName,trialNo)
    obsblockID = projectName + '.' + obsblockName + '.' + str(trialNo)
    #s.project( projectName )
    #s.obsblock( obsblockName )
    #s.subObsblock( subObsblockName )
    #s.trial( trialNo )
    multiSubarray('setScriptInt', subarray, odi.INDX_INT_OBSTRIAL, trialNo )
    constraints(subarray=subarray)
    return obsblockID                                                                    

def newProject( projectName, obsblockName, subObsblockName='', pacs=False, 
        maxsens=False, subarray=DEFAULT) :
     """Get a newProject command until PDB is fully deployed
     Parameters:
     projectName: Usually a ct or cx number (sometimes the date). No default.

     obsblockName: The obsblock value created for the project or an
                   arbitrary one for a commissioning project.  No default.

     subObsblockName: The subObsblock value created for the project or an
                      arbitrary one for a commissioning project.
                      Default is blank.
     pacs:            True if this is a PACS project.  Default:False
     maxsens:         True if this is a maximum sensitivity project
                      Default is False

     """

     useProjectDatabase = True
     if (useProjectDatabase and "rpnt" != projectName and "ct013" != projectName) :
         aiv = carma.observertools.ItemValue("project",projectName)
         biv = carma.observertools.ItemValue("obsblock",obsblockName)
         ivSeq = [aiv,biv]
         subarrayInstance = Subarray.getSubarray()
         prj = subarrayInstance.queryProject(ivSeq)
         if(len(prj) == 0) :
             print "project length is zero";
             if(s.isCommissioning( projectName )) :
                 pass
             else :
                 raise Exception,"Project not found in PDB"
         else :
             pacs = pacs or (prj[0].obsblock[0].observationType == carma.observertools.TYPE_PACS) or (prj[0].obsblock[0].observationType == carma.observertools.TYPE_PACS_DUALPOL) or (prj[0].obsblock[0].observationType == carma.observertools.TYPE_PACS_FULLPOL)
             # EML removing explicit configuration checks in place of multiple correlator ownership check
             # maxsens = maxsens or (prj[0].obsblock[0].observationType == carma.observertools.TYPE_MAXSENS_DUALPOL) or (prj[0].obsblock[0].observationType == carma.observertools.TYPE_MAXSENS_CARMA23) or (prj[0].obsblock[0].observationType == carma.observertools.TYPE_MAXSENS_LL)
             maxsens = maxsens or subarrayOwnsMultipleCorrelators(subarrayNo)
             print "PACS=%s, MAXSENS=%s"%(pacs,maxsens)

         if (pacs) :
             obsblockID = newPacsProject(projectName, obsblockName,
                                         subObsblockName, pdb=True)
         elif (maxsens) :
             obsblockID = newProjectFromPdb(projectName, obsblockName,
                                            subObsblockName, subarray=subarray,
                                            isDualCorr = True)
         else:
             obsblockID = newProjectFromPdb(projectName, obsblockName,
                                            subObsblockName, subarray=subarray,
                                            isDualCorr = False)
     else :
         if (pacs) :
             obsblockID = newPacsProject(projectName, obsblockName,
                                         subObsblockName, pdb=False)
         elif (maxsens) :
             obsblockID =  oldnewProject( projectName, obsblockName,
                                  subObsblockName, subarray=subarray, isDualCorr = True)
         else :
             obsblockID = oldnewProject( projectName, obsblockName,
                                  subObsblockName, subarray=subarray)             
     alarmIntegrestore(subarray=subarray)
     rtdComment("Integration Alarm enabled.", subarray);
     m = "newProject(project=%s, obsblock=%s, subObsblock=%s "\
             %(projectName, obsblockName, subObsblockName )
     commandlog(m, subarray=subarray)
     return obsblockID
         
def _closeTrial(projectName="NONE", 
        comment="Trial closed, ready for new project", subarray=DEFAULT) :
    """Internal function to close out the current trial by changing 
    the project name
    and taking a 0.5 sec integration.
    There is no effect on or interaction with the PDB.
    Parameters:
     projectName: name for the dummy project, default is NONE.,
       which will generate NONE.STANDBY.1
     comment: string to use for comment; default is
       'Trial closed, ready for new project'"""
    multiSubarray('setObsblock', subarray, projectName,"STANDBY","",1)
    rtdComment(comment, subarray=subarray)
    _pdbIntegration(subarray=subarray)
    
def resetProject(closeTrial=True, subarray=DEFAULT) :
    """Resets project, obsblock, and subObsblock to default values:
    project: NONE, obsblock: STANDBY, subObsblock: [blank].
    Also stops all antennas, and sets user catalog to NONE.
    Takes a dummy 0.5 sec integration."""
    if closeTrial: _closeTrial(subarray=subarray)
    s.resetProjectAndObsblock()
    ucat('NONE', subarray=subarray)
    multiSubarray('setScriptName', subarray, 'NONE')
    commandlog("resetProject()", subarray=subarray)
    
def setTrackStartTime() :
    """Marks the current time as the start of the track for this subarray. 
    This start time is then used to update the monitor point
     Control.SubarrayX.trackRuntime"""
    s.startTrack() 

def _pdbIntegration(subarray=DEFAULT):
    " Performs a short integration on the noise source as required by the PDB"       
    noiseon(subarray=subarray)
    #print "Doing pdb integ"
    integrate(0.5, 1, antwait=NONE, subarray=subarray)
    noiseoff(subarray=subarray)
    
def _closeWeather(subarray=DEFAULT) :
    _pdbIntegration(subarray=subarray)
    _closeTrial(subarray=subarray)    
    alarmIntegrestore(subarray=subarray)
    print "Integration alarm enabled..."
    commandlog("end weather()")

def weather(obsblock, subarray=DEFAULT) :
    """Set project code to WEATHER to indicate the array is shut down
    due to weather.  Turn off the integration alarm. 
    Changes project database to the weather project and does a short
    noise source integration.
    Parameters:
     obsblock: will be used with newProject - try 'snow' or 'wind' """
    _closeTrial("WEATHER",
       "Array stopped due to weather conditions. ", subarray=subarray)
    commandlog("weather()", subarray=subarray)
    newProject("ct017", obsblock, "", False, subarray=subarray)
    intent("noise", "O", subarray=subarray)
    # An integration is required to track the time in the PDB
    _pdbIntegration(subarray=subarray)
    alarmIntegdisable(subarray=subarray)
    print "Integration alarm disabled..."

def maint(subarray=DEFAULT) :
    """Set project code to MAINTENANCE to indicate the array is shut down
    due to maintenance. Turn off the integration alarm.
    No affect/interaction with project database.
    """
    _closeTrial("MAINTENANCE",
        "Array stopped for maintenance. Integration Alarm Disabled.",
       subarray=subarray)
    alarmIntegdisable(subarray=subarray)
    commandlog("maint()", subarray=subarray)


def intent(sourcename, purpose, selfcal=False, fastswitch=False, 
        subarray=DEFAULT) :
   """Defines the intent of the observation of the given source. Inputs:
   sourcename - the name of the source; no default
   purpose    - string indicating purpose of the observation.
    Choices are:
     A - Atmospheric monitoring
     B - Bandpass calibration
     F - Flux calibration
     G - Gain (phase and/or amp) calibration
     P - Polarization calibration
     R - Reference pointing
     S - Science target
     O - Other
    For multi-purpose source, you may concatenate the characters,
    e.g. BFP  (order does not matter). No default.
   selfcal - Is this source self-calibratable?  Default: False
   fastswitch - Is this source part of a fast-switch cycle? Default: False
   NOTE: Your observing script will exit with an exception if you have not
   set an intent for each source in your obsblock.
   """
   # We use BOTH on the noise source for PACS
   multiSubarray('setIntent',subarray, sourcename, purpose, selfcal, fastswitch)

def constraints(imgType=SNR, minAnts=3, calMaxTime=100.0, calMaxRms=100.0, 
                maxTsys=2E5, maxRmsPath=1E4, maxTau=1E5, maxDecor=1, 
                srcRms=1E-5, subarray=DEFAULT) :
   """Defines the observing constraints that must be met for this obsblock to
   be observed. The defaults are meant to be extremes such that any
   observation will meet the default criteria.  As a recommended practice,
   users should override these defaults with values specific to their
   science target.  Inputs:
     imgType    - imaging type. IMG means maximize imaging fidelity, SNR means
                  maximize signal-to-noise. Default: SNR
     minAnts    - minimum number of antennas. Default: 3
     calMaxTime - maximum time in minutes per calibrator-source cycle to
                  spend integrating on the calibrator. Default: 100
     calMaxRms  - maximum rms noise in Jansky per calibrator-source cycle
                  to obtain for the calibrator.  This will correspond to
                  a minimum integration time, based on the system temperature
                  at the time of the observation. Default: 100
     maxTsys    - maxi    print "snowtrack SA:", subarray
mum system temperature allowed for the observation
                  to proceed. Default: 2E5
     maxRmsPath - maximum rms atmospheric path length in microns allowed for
                  the observation to proceed.  This will be compared against
                  the output of the phase monitor.
                  Too small a value may greatly reduce the likelihood of the
                  obsblock being observed.  Default: 1E5
     maxTau     - maximum 225 GHz opacity allowed for the observation to
                  proceed. This will be compared against the output of the
                  opacity monitor (aka tipper), or if the tipper is
                  unavailable to computed tau225 from the weather station.
                  Too small a value may greatly reduce the likelihood of the
                  obsblock being observed. Default: 1E5
     maxDecor   - Maximum signal decorrelation desired for this observation.
                  This is measured by taking the ratio of the scalar average
                  to the vector average of the gain calibrator visibilities.
                  A value of 0 means no decorrelation, a value of 1 means
                  complete decorrelation.  Too small a value may greatly
                  reduce the likelihood of the obsblock being observed.
                  Default: 1
     srcRms     - Desired rms noise in Jansky on the science target.
                  Default: 1E-5"""
    
   if subarray == BOTH:
        raise Exception, "Can't do constraints on BOTH subarrays"
   multiSubarray('setConstraints', subarray, imgType, minAnts, calMaxTime,
                     calMaxRms, maxTsys,maxRmsPath, maxTau, maxDecor, srcRms )

def comment(commentString, subarray=DEFAULT) :
   """Add a comment about the observations that will be placed in the
   project database. Comment will also be archived in the half-second
   database and the script log. No default.  Eventually these should
   be added to the MIRIAD history file.
   """
   if subarray == BOTH:
        raise Exception, "Can't do comment on BOTH subarrays"
   rtdComment(commentString, subarray=subarray)
   # May need to add a subarray arg to trackMessage when it supports it
   trackMessage(commentString)
   # attempt to add the comment to the project database for this
   # obsblock
   try :
       subNo = subarrayNo
       if subarray == SCI1: subNo = 1
       if subarray == SCI2: subNo = 2
       mpName = "Control.Subarray%d.obsBlockId" % subNo

       # split off trial number from obsblockid
       # i really should override addComments to allow
       # trialNo appended to obsblock...
       obid = queryString( mpName )
       parts = obid.split('.')
       trialIdx = len(parts) - 1
       trialNo = int( parts[trialIdx] )
       obsblock = ''
       for i in range(0,len(parts)-1) :
          obsblock += parts[i]
          if ( i != trialIdx-1 ):
               obsblock += '.'
       addComments(obsblock, trialNo, "Observer", commentString)

   except Exception, ex:
       print "Sorry, unable to add comment to project database for current obsblock."
       print "Reason was: %s " % str(ex)
       return

def rtdComment(commentString, subarray=DEFAULT) :
    """Put a comment in the monitor stream.
    This will be displayed on the Array Composite RTD page. Unlike
    'comment()' it does not add the comment to the database
    or the track log."""
    if commentString == None: return
    multiSubarray('comment', subarray, commentString)
    return

# ----------------------------- Alarm --------------------------------

def alarmon(alarmName="") :
    """Turn on the control building alarm"""
    s.alarm(True, alarmName)

def alarmoff() :
    """Turn off the control building alarm"""
    s.alarm(False, "")

def alarmdisable(hardware=False, subarray=DEFAULT):
    """Disable the control building alarm for the current subarray.

    In normal operation (no parameters), this command DOES NOT disable
    the alarm in other subarrays. It also DOES NOT disable the deadman.

    In hardware operation, the output from the alarm computer is
    completely switched off. This DOES disable the alarm for all
    subarrays. It also DOES disable the deadman.

    Parameters:
        hardware - disable the alarm in hardware (default False)
    """
    if type(hardware) != bool:
        raise Exception, "The first parameter, hardware, must be a boolean"
        
    # disable the alarm in hardware
    if (hardware):
        multiSubarray('alarmEnable', subarray, False)

    # disable the fault system subarray-specific alarms
    multiSubarray('setFaultSystemAlarmEnableState', subarray, False)

def alarmenable(hardware=False, subarray=DEFAULT):
    """Enable the control building alarm for the current subarray.

    In normal operation (no parameters), this command DOES NOT enable
    the alarm in other subarrays. If the hardware=True parameter is given,
    the alarm hardware will be switched on as well.

    Parameters:
        hardware - enable the alarm in hardware (default False)"""
        
    # disable the alarm in hardware
    if (hardware):
        multiSubarray('alarmEnable', subarray, False)
        
    # enable the alarm in hardware
    if (hardware):
        multiSubarray('alarmEnable', subarray, True)
        sleep(2)

    # warn if the alarm is disabled in hardware
    try :
        val = queryInt('Alarm.alarmEnable')
        if (val == 0):
            msg = ""
            msg += "WARNING: alarm system hardware switch is disabled!\n"
            msg += "WARNING: please call ``alarmenable(hardware=True)'' "
            msg += "to enable it!\n"
            sys.stderr.write(msg)
    except Exception, ex:
        msg = ""
        msg += "WARNING: cannot get current state of alarm system "
        msg += "hardware switch!\n"        
        sys.stderr.write(msg)

    # enable the fault system subarray-specific alarms
    multiSubarray('setFaultSystemAlarmEnableState', subarray, True)

def alarm1mm(state) :
    """Set condition to trigger the 1mm observing audible alarm or
       shut it off.  This command sets the monitor point
       Control.SubarrayN.alarm1mm to True (ON) or False (OFF).  It is
       up to the Fault System to trigger the audible alarm based on this
       monitor point. This is not an observer command, but one that is used
       by a script checking the weather conditions. For observer commands
       see alarm1mmenbable and alarm1mmrestore.

       Parameters:
           state - either ON or OFF (enumeration, not a string)"""
    if (subarrayNo > 2) :
        raise Exception, "This command is only valid for Science subarrays."
    if ( state == ON ) :
       s.alarm1mm( True );
       return;

    if ( state == OFF ) :
       s.alarm1mm( False );
       return;

    raise Exception, "Bad input. Must be ON or OFF (no quotes)"

def scriptClear(subarray=DEFAULT) :
    """Clears the script name and state monitor points. This will turn off the
    alarm if it is triggering because of a crashed script.  This will not
    affect the observation state when restarting a script with
    run(scriptname, restart=True).
    Parameters: none"""
    multiSubarray('setScriptState', subarray, carma.control.COMPLETED)
    multiSubarray('setScriptName', subarray, "NONE")

def clearScript(subarray=DEFAULT) :
    """Alias for scriptClear() for those who get it backwards."""
    scriptClear(subarray=subarray)

# ----------------------------- Log --------------------------------

def log(entry, subarray=DEFAULT) :
    """Insert an entry into the control log at INFO level.
    The entry will be prefaced with LOG:.
    Parameters:
     entry: the string to enter into the log"""
    multiSubarray('log', subarray, "LOG: " + entry)

def logerror(entry, subarray=DEFAULT) :
    """Insert an entry into the control log at ERROR level.
    The entry will be prefaced with LOG:.
    Parameters:
     entry: the string to enter into the log"""
    multiSubarray('logError', subarray, "LOG: " + entry)

def scriptlog(entry, subarray=DEFAULT) :
    """Insert an entry into the control log. The entry will be prefaced
    with SCRIPT:.
    Parameters:
     entry: the string to enter into the log"""
    multiSubarray('log', subarray, "SCRIPT: " + entry)
    
def commandlog(entry, subarray=DEFAULT) :
    """Insert an entry into the control log. The entry will be prefaced
    with COMMAND:. 
    Parameters:
     entry: the string to enter into the log"""
    multiSubarray('log', subarray, "COMMAND: " + entry)

# ----------------------- Script Control Vars --------------------------

def controlVariablesClear() :
    """Clear all of the script control variables (boolean is set to False).
    There are 4 different types of script control variables,
    each with 100 different values. These can be accessed with:
      s.setScriptBool(index, value)
      s.getScriptBool(index)
      s.setScriptDouble(index, value)
      s.getScriptDouble(index)
      s.setScriptInt(index, value)
      s.getScriptInt(index)
      s.setScriptString(index, value)
      s.appendScriptString(index, value)
      s.getScriptString(index)
    The index runs from 1 to 100, and a 0 sets all 100.
    Strings are limited in length to 5000 chars.
    This command name was chosen to avoid erroneous tab completion confusion
    with the scriptClear command."""
    s.clearScriptAll()

def dumpScriptAll() :
    """Dumps all of the script control variable values."""
    for idx in range(1,101) :
        b = "False"
        if s.getScriptBool(idx): b = "True "
        d  = s.getScriptDouble(idx)
        i  = s.getScriptInt(idx)
        st = s.getScriptString(idx)
        msg = "%2i %s %12.5f  %8i  %s" %(idx,b,d,i,st)
        print msg

def radioInit(clearastrobands=True, subarray=DEFAULT) :
    """This command is useful for getting ready for radio observations.
    It does the following for all antennas in the subarray:
    0. Turns off the prewriter invalidation that is used for OTF mosaics
    1. Turns on Tsys and flux calibration.
    2. Resets the antenna tracking thresholds to the default.
    3. Resets the elevation limit to 15 degrees.
    4. Takes the calibrator wheel out of the optical path.
    5. Turns the noise source off.
    6. Zeros out the offsets (BUT NOT MOUNT OFFSETS).
    7. Turns off the optical cameras and closes the flap/lenscap
    8. Changes the aperture to radio.
    9. Unsets the doppler source.
   10. Unloads the user catalog.
   11. Turn on drive blanking
   12. Sets the user catalog (ucat) to none
   13. Turns on the "time since last integration alarm"
   14. Configures the pipeline to default for:
        keepEndChans=False
        decimation=DEFAULT
   15. Enables max coherence alarms for all ants in array
   16. Optionally clears astrobands
   Parameter:
    clearastrobands: default is True
    """
    print "Beginning radioInit"
    s.setInvalidationForMosaics( False )
    applyTsys(True, subarray=subarray)
    applyFlux(True, subarray=subarray)
    trackThreshold(0.1,0, subarray=subarray)
    elevlimit(15, subarray=subarray)
    sky(subarray=subarray)
    noiseoff(subarray=subarray)
    offset(0.0,0.0, subarray=subarray)
    camera(OFF, subarray=subarray)
    driveErrorPreference(PREF_BLANK, subarray=subarray)
    ucat('NONE', subarray=subarray)

    alarmIntegrestore(subarray=subarray)
    decimation(subarray=subarray)
    if subarray == SCI1 or subarray == SCI2:
        currentAnts = currentAntennaNumbers(subarray)
        alarmMpenable(['Astro.Antenna%d.MaxCoherence'%ant for ant in currentAnts])
    if subarray == BOTH:
        currentAnts = currentAntennaNumbers(SCI1)
        alarmMpenable(['Astro.Antenna%d.MaxCoherence'%ant for ant in currentAnts])
        currentAnts = currentAntennaNumbers(SCI2)
        alarmMpenable(['Astro.Antenna%d.MaxCoherence'%ant for ant in currentAnts])

    if clearastrobands:
        clearastroband(0)
    print "radioInit finished."

def yearMonthDay() :
    """Return the current UT year, month and day number as a string, 
    e.g. '2007mar01'.
    """
    timeDateValue = time.asctime(time.gmtime()).lower().split()
    if int(timeDateValue[2]) < 10 : timeDateValue[2] = str('0'+str(timeDateValue[2]))
    return '%s%s%s' % (timeDateValue[4],timeDateValue[1],timeDateValue[2])

def sendMsg(pC,principalInvestigator,email,trackFileName,start=0) :
    """Send an email message from obs@mmarray.org to the PI with information
    about the track observation. A different message is sent at the start and
    end of the observation.

       pC                    :  project code
       principalInvestigator :  The PI name
       email                 :  The PI email address
       trackFileName         :  The file name of the observing script.
       start                 :  0 indicates this message is being sent at the
                                start of the track; anything else indicates it
                                is being sent at the end of the track.
    """

    startlst=lst()
    lstsym=str(int(startlst))+":"+str(int(60*(startlst-int(startlst))))
    date=str(yearMonthDay())
    msg  = 'To: '+email+'\n'
    msg += 'From: obs@mmarray.org\n'

    if start == 0:
        msg += 'Subject: Your track has been run.\n\n'
        msg += 'Attn:  '+principalInvestigator+'\n\n'
        msg += 'Project '+pC+' ended at '+lstsym+'LST('
        msg += getMiriadUTStamp() + "UT).\n"
        msg += 'Date: '+date+'\n\n'
        msg += 'The edited file that the observers ran is: '+trackFileName+' .\n'
        msg += 'Please go to http://carma-server.ncsa.uiuc.edu:8181\n'
        msg += ' to retrieve your data, including quality script '
        msg += 'output and plots.\n\n'
        msg += 'Send observers feedback at obs@mmarray.org\n\n'
        msg += '*****This email was automatically generated and sent '
        msg += 'at the completion of your track.*****\n'
    else:
        msg += 'Subject: Your track has just started running.\n\n'
        msg += 'Attn:  '+principalInvestigator+'\n\n'
        msg += 'Project '+pC+' started at '+lstsym+'LST('
        msg += getMiriadUTStamp() + "UT).\n"
        msg += 'Date: '+date+'\n\n'
        msg += 'The edited file that the observers ran is: '+trackFileName+'\n\n'
        msg += 'Send observers feedback at obs@mmarray.org\n\n'
        msg += '*****This email was automatically generated and sent '
        msg += 'at the beginning of your track.*****\n'
    MAIL = "/usr/sbin/sendmail"
    if (False) :  # for debugging
        f = open("test.txt",'w')
        f.write(msg)
        f.close()
        return
    p = os.popen("%s -t" %MAIL, 'w')
    p.write(msg)
    exitcode = p.close()
    if exitcode: print "Exit code from sendmail: %s" %exitcode

#---------------------- Special tracking modes -----------------------
def snowstow(windOutOf) :
    """Stow the antennas out of the snow.
    This command should only be used if you cannot use snowtrack().
    There are not many cases when snowtrack() cannot be used, but
    E config is one of them and a weather station failure is another. 
    During snow stow, the 10-m antennas are moved to an azimuth 60 degrees
    away from the wind direction and elevation of 20 degrees.  The 6-m
    antennas are moved to point away from the wind at an elevation of 20
    degrees.
    Make sure you have ALL the antennas capable of moving in the subarray
    used to execute this command, using addAntenna().  If you must,
    move antennas manually to the appropriate direction, but generally
    all antennas should be made part of this subarray.
    Parameter:
      windOutOf - Direction in degrees that the wind is coming from,
        not the direction it is blowing into. The direction the wind
        is coming from is reported by the Weather station and is shown
        on the Weather RTD page as 'Wind Direction'."""
    print snowstow.__doc__

    weather("snow")
    [ovroAnts,bimaAnts,szaAnts] = antennasByType()
    ovroStowPosT =windOutOf+60.0
    if windOutOf < 180.0 : windDirection=windOutOf+180.0
    else : windDirection=windOutOf-180.0
    if ovroStowPosT < 180.0 : ovroStowPos = ovroStowPosT+180.0
    else :                    ovroStowPos = ovroStowPosT-180.0
    qmove(ovroStowPos,20.0,ovroAnts)
    qmove(windDirection,20.0,bimaAnts)

def windstow(windOutOf) :
    """Stow the antennas out of the wind.
    For the 10-m antennas this is a normal stow; for the 6-m antennas,
    they will be pointed with their backs to the wind and at 10 degrees
    elevation.
    Make sure you have ALL the antennas capable of moving in the subarray
    used to execute this command, using addAntenna().  If you must,
    move antennas amnually to the appropriate direction, but generally
    all antennas should be made part of this subarray.
    Parameter:
      windOutOf - Direction in degrees that the wind is coming from,
        not the direction it is blowing into. The direction the wind
        is coming from is reported by the Weather station and is shown
        on the Weather RTD page as 'Wind Direction'."""
    print windstow.__doc__

    weather("wind")
    [ovroAnts,bimaAnts,szaAnts] = antennasByType()
    if windOutOf < 180.0 : windDirection=windOutOf+180.0
    else : windDirection=windOutOf-180.0
    stow(ovroAnts)
    qmove(windDirection, 10.0, bimaAnts)

def snowtrack(lengthTime=2.0, subarray=DEFAULT) :
    """Actively move the antennas to minimize snow accumulation.
    This command is preferred over snowstow(), but can only be used if
    the weather station is working and reporting the wind direction.
    This command reads the wind direction from the weather station every
    five minutes and adjusts the azimuths accordingly. The 10m antennas
    use a elevation of 20 degrees which is designed to minimize the 
    accumulation of snow in the cracks on the backs of the panels.
    The 6m antennas use an elevation of 10 sdegrees.

    Make sure you have ALL the antennas capable of moving in the subarray
    used to execute this command, using addAntenna().  If you must,
    move telescopes by hand to the appropriate direction but generally,
    all antennas should be made part of this subarray. Issue a cancel()
    command in a different sac to exit from snowtrack.
    
    This command cannot be used in E array.

    Parameter:
     lengthTime : The number of hours you want this command to run.  Two hours
                 is the time scale over which the observer should be getting up
                 to check the system for snow accumulation so 2 hours is
                 a good number (thus it is the default...).  """
    print snowtrack.__doc__
    
    import  etrack
    # If in E-configuration, use etrack
    if (subarrayNo == 1) and (etrack.IS_E_CONFIGURATION):
        m = "Snowtrack not allowed in E configuration"
        raise Exception, m
        
    weather("snow", subarray=subarray)
    [ovroAnts,bimaAnts,szaAnts] = antennasByType()
    lengthTimeSec = lengthTime*60.0*60.0
    repCheck = int(lengthTimeSec/300.0)
    if len(szaAnts) > 0: track("bumi", ants=szaAnts, subarray=subarray)
    try :  # Catches cancel, control-C, and other exceptions
        for i in range(repCheck) :
            fiveMinutes = 5*60*2   # in frames
            try: 
                pass
                windOutOf = queryDouble('Weather.windDirection', fiveMinutes)
            except:
                #if subarrayNo <= 2: alarmon()            
                m = 'Failure: 5  minutes of bad weather station data'
                print m
                _closeWeather(subarray=subarray)
                return
            if len(ovroAnts) > 0:
                ovroAz = windOutOf+60.0
                ovroAz = _wrapFix(ovroAz, ovroAnts[0])           
                qmove(ovroAz, 20.0, ovroAnts)
        
            if windOutOf < 180.0 : windDirection = windOutOf+180.0
            else :                 windDirection = windOutOf-180.0
            if len(bimaAnts) > 0: qmove(windDirection, 10.0, bimaAnts)
            wait(tmo=300)
    except SuccessfulCancel, ex:
        print "snowtrack() cancelled"
        return
    except Exception, ex:        
        print "Snowtrack encountered error: " + str(ex)
        _closeWeather(subarray=subarray)
        return
    _closeWeather(subarray=subarray)

def windtrack(lengthTime=4.0, debug=False) :
    """Actively move the antennas to point them out of the wind.

    For the 10-m antennas this is a normal stow; for the 6-m antennas,
    they will be pointed with their backs to the wind and at 20 degrees
    elevation.  This command reads the wind direction from the weather station
    every five minutes and adjusts the azimuths accordingly. Type cancel()
    in another sac to exit this function.
    
    This command cannot be used in E array.

    Parameter:
      lengthTime : The number of hours you want this command to run.  
                   The default is 4 hours.
    """
    print windtrack.__doc__
    
    import  etrack
    # If in E-configuration, use etrack
    if (subarrayNo == 1) and (etrack.IS_E_CONFIGURATION):
        m = "Windtrack not allowed in E configuration"
        raise Exception, m

    if not debug: weather("wind")
    [ovroAnts,bimaAnts,szaAnts] = antennasByType()
    lengthTimeSec = lengthTime*60.0*60.0
    loopTime = 300.0
    if lengthTimeSec < loopTime: loopTime = lengthTimeSec
    repCheck = int(lengthTimeSec/loopTime)
    if not debug: 
        if subarrayNo <= 2: alarmIntegdisable()
        alarmMpdisable("Weather.averageWindSpeed")
    try :  # Catches control-C, cancel, and other exceptions
        for i in range(repCheck) :
            if debug:
                windOutOf = 60.0
            else:
                try: 
                    windOutOf = queryDouble('Weather.windDirection',24)
                except:
                    #if subarrayNo <= 2: alarmon()
                    if subarrayNo <= 2: alarmIntegrestore()
                    _closeWeather()
                    alarmMpenable("Weather.averageWindSpeed")
                    print 'windtrack failed getting wind direction.'
                    return
            # Flip wind direction to get wind vector (still [0,360])
            if windOutOf < 180.0 : windDirection=windOutOf+180.0
            else : windDirection = windOutOf-180.0
            stow(ovroAnts)
            if len(bimaAnts) > 0:
                # Put backs to the wind; az lim = [-70,430] so no prob there
                qmove(windDirection, 20.0, bimaAnts)
            wait(tmo=loopTime)
    except SuccessfulCancel, ex:
        print "windtrack() cancelled"
        if debug: return        
        if subarrayNo <= 2: alarmIntegrestore()
        alarmMpenable("Weather.averageWindSpeed")
        _closeWeather()
        return
    except Exception, ex:        
        print "Error during windtrack: " + str(ex)
        if debug: return
        if subarrayNo <= 2: alarmIntegrestore()
        alarmMpenable("Weather.averageWindSpeed")
        _closeWeather()
        return
    print "Windtrack() completed successfully"
    if debug: return
    if subarrayNo <= 2: alarmIntegrestore()
    alarmMpenable("Weather.averageWindSpeed")
    _closeWeather()

def getNyquistStep(ants=0, scalingFactor=1.0, userFreq=None) :
    """Returns a list with the Nyquist step in arcminutes for each antenna.
    Parameters:
     ants: a single ant#, a list of ant#'s or zero(default) which gives all
       antennas in the subarray.
     scalingFactor: for the step, default=1.0 .
       Set this smaller for continuum observations if the high frequency end
       of the band is more than a few percent above your LO.
     userFreq: frequency in GHz, defaults to sky frequency. A value of 'lo'
       will use the first LO frequency.
    """
    antVec = makeAntList(ants)
    nyquistStepVec = []
    subNo = s.getSubarrayNo()
    if userFreq=='lo' : freq = queryDouble('Control.Subarray%d.loFreq' % subNo,24)
    elif userFreq==None : freq = queryDouble('Control.Subarray%d.skyFreq' % subNo,24)
    else : freq=userFreq
    wavelength = 3.0e11/(freq*1.0e9)
    diameterVec = list()
    for a in antVec :
        diameterVec.append(1000*device.CarmaAnt().getDiameter(a))
    for i in diameterVec :
       if not i : nyquistStep = 0.0
       else : nyquistStep = (wavelength/i)*3437.75/2.0
       nyquistStepVec.append(scalingFactor*nyquistStep)
    return nyquistStepVec

# --------------------- Optical camera/telescope ----------------------

def camera(state, ants=0, subarray=DEFAULT) :
    """Turn cameras on/off (opens/closes) flaps for requested antennas.
    Also sets refraction model to optical/radio.
    Parameters:
     state: Either ON or OFF
     ants: A single or list of antennas; default is zero, all antennas"""
    antlist = helpers.makeList(ants)
    multiSubarray('camera', subarray, state, antlist)

def opticalRotationFOV(rot, azFOV, elFOV, ant, subarray=DEFAULT) :
    """Sets the rotation and field of view for the optical camera for
    a single antenna. The field of view is the full width.
    The nominal FOV ratio of el/az is 1.375.
    Params:
     rot: clockwise rotation of image in degrees
     azFOV: az field of view in arcmin, after rotation
     elFOV: el field of view in arcmin, after rotation
     ant: camra antenna number"""
    multiSubarray('setRotationAndFieldsOfView', subarray, rot, azFOV, elFOV,ant)

opticalRotationFOV.__argsMpContainer__  = 'Antenna'
opticalRotationFOV.__argsMpNames__ = ["cameraRotation", 
                                      "cameraAzFOV", 
                                      "cameraElFOV"] 

# ----------------------- Script Run methods ------------------------

def run(script, restart=False, subscript=True, triggerAlarm=False, **options) :
    """Run a python observing script.
    Observing scripts have a .obs or .py extension and are run in the current
    keyboard context, removing the need for most import statements.
    The search path for the file is the full python path.
    If the script crashes it will usually sound the alarm through the
    monitor point \"control.subarrayX.scriptState\". You can turn off this
    alarm with \"clearScript\". See also clearScript(), queue().
    Parameters:
     script: observing script name (without the .obs or .py). 
     restart: Set this to True if you are restarting a script and want it
       to pick up where it left off.
     subscript: Set this to True if this script is within the main observing
       script. It inhibits the clearing of the script control variables - it you
       don't set this when you should, the main script might get confused. Use
       this for tuning scripts, utility scripts, etc. that are run inside of
       oberving scripts.
     triggerAlarm: Set the alarm for this subarray if a script error occurs.
       For the main science subarray the alarm system is usually configured to
       trigger on the scriptState, so this is an additional alarm trigger for
       that subarray. The engineering subarrays are not configured to trigger
       on scriptState, so this is where this parameter can be useful.
       The default is False.
     Options: The options are keyword=value pairs separated by commas and 
      are the last parameters to the run command. They are general input
      parameters that can be specific for a given script.
      Differnet scripts may have different keywords, but the generated
      standard observing scripts all use the same keywords. To inspect
      the keyword names, data types, and default values just add  'help=True'
      to the run command (the script will not be executed). Example:
        run ('myscript', help=True)
    Return value: True if the script is successfully executed, False otherwise

    Exceptions: Thrown if the script name parameter is not a string or if the
      script file cannot be opened.

    Example:
        run "myFavoriteScript"  # No options
        run "myFavoriteScript, lststop='12:10:00' tune=False"
      will execute myFavoriteScript.obs or myFavoriteScript.py, passing in
      an options of 'lststop='12:10:00' and tune=False
        run "myFavoriteScript, restart=True, lststop='12:34:56' tune=False"
      will restart the script, keeping track of what has already been done

    Tricks:
     o to chain several scripts together try
        run('a') and run('b') and run('c') ...
     o to run one script inside another, do
          if not run('myScript') : raise Exception
       to get proper error handling
    See also queue"""

    global fullScriptName
    global scriptName
    global scriptOptions
    global scriptKeyVals
    # For backward compatibility
    scriptOptions = ""
    
    # Note to the programmer:
    # Scripts that are run by this function can expect the global variable
    # restartScript to be defined.

    # Make sure that the script name has no embedded whitespace.
    if len(script.strip().split()) != 1:
        msg  = "The script name must be a string with no embedded.\n "
        msg += "white space.\n"
        msg += __doc__
        raise Exception, msg

    # Check input parameter type
    if not isinstance(script, str)  :
        raise Exception, "script must be a string"

    # Stash away current script name if one is already running...
    state = s.getScriptState()
    nested = state == RUN
    if nested: previousScript = fullScriptName

    restartScript = restart # restartScript is used externally
    scriptName    = script.split()[0]
    scriptKeyVals = dict(options)
    # This will throw an exception if the script cannot be found
    fullScriptName = helpers.getScriptFullPathname(scriptName)

    s.setScriptName(fullScriptName)
    s.setScriptState(carma.control.RUNNING)
    logmsg = "Beginning " + fullScriptName + " with "
    if len(scriptKeyVals) > 0 :
        logmsg += "options"
        scriptsOptions = ""
        prefix = ' '
        for i in scriptKeyVals.items():
            scriptOptions += prefix + i[0] + "=" + str(i[1])
            prefix = ', '
    else :
        scriptOptions = "no options"
    logmsg += scriptOptions    
    scriptlog(logmsg)
    print logmsg
    if (not restart) and (not subscript): s.clearScriptAll()
    try :
        # Execute the script using identical globals and locals dictionaries.
        # This prevents the current scope type (function scope) from "leaking"
        # into the execution environment of the script.
        #
        # By passing identical dictionaries, global variables will work
        # as expected in the script.
        #
        # In addition, a copy of the globals() + locals() dictionary is used,
        # which avoids "leaking" import statements from the script back into
        # this execution environment.
        d = dict(globals(), **locals())
        execfile(fullScriptName, d, d)
    except ScriptReturn, ex:
        #print "Quick and quiet return from script has been called"
        pass
    except ScriptInputError, ex:
        s.setScriptState(carma.control.CRASHED)
        print "Script input error!!"
        if triggerAlarm: s.alarm(True, "scriptError")
        return False
        # Don't print out the exception because we have already done that
        #print ex
    except ScriptError, ex:
        s.setScriptState(carma.control.CRASHED)
        print "Script error!!"
        print ex
        if triggerAlarm: s.alarm(True, "scriptError")
        cancel()  # Stop any integrations that might be running...
        return False
    except (carma.util.CancelException,KeyboardInterrupt, SuccessfulCancel):
        s.setScriptState(carma.control.CANCELED)
        print "Script canceled."
        rtdComment("Script canceled.")
        scriptlog("Script " + fullScriptName + " has been canceled." )
        cancel()  # Stop any integrations that might be running...
        return False
    except carma.util.UserException, userEx:
        s.setScriptState( carma.control.CRASHED )
        if triggerAlarm: s.alarm( True, "scriptError" )
        errMsg  = "\nScript crashed due to RTS runtime exception: \n"
        errMsg += "\n" + userEx.errorMsg + "."
        printInColor( errMsg, 'red', linefeed=True )
        try:
            exceptionType, exceptionValue, exceptionTraceback = sys.exc_info()
            scriptlog( "Exception: " )
            tbstrs = traceback.format_exception(exceptionType, 
                                                exceptionValue, 
                                                exceptionTraceback)
            for t in tbstrs: scriptlog(t)                                    
            printInColor( "\nDetailed context information has been sent to the logs.\n", 
                          'red', 
                          linefeed=True )
        except: 
            printInColor( "\nUnable to send detailed context information to the logs.", 
                          'red', 
                          linefeed=True )
        return False
    except Exception, ex:
        s.setScriptState(carma.control.CRASHED)
        if triggerAlarm: s.alarm(True, "scriptError")
        print "Script error!!"
        print ex
        exceptionType, exceptionValue, exceptionTraceback = sys.exc_info()
        traceback.print_exception(exceptionType,
                                  exceptionValue, exceptionTraceback)
        scriptlog("Exception: " + str(ex))
        tbstrs = traceback.format_exception(exceptionType, 
                                            exceptionValue, exceptionTraceback)
        for t in tbstrs: scriptlog(t)                                    
        scriptlog("Script " + fullScriptName + " crashed!!")

        cancel()  # Stop any integrations that might be running...
        if s.getScriptBool(odi.INDX_BOOL_PACS): 
             pacs.cleanup(s.getScriptString(odi.INDX_STR_PACS_OBSBLOCK))
        return False
    except :
        # Anything else represents an unknown exception...
        s.setScriptState(carma.control.CRASHED)
        print "Script error!!"
        if triggerAlarm: s.alarm(True, "scriptError")
        scriptlog("Script " + fullScriptName + " crashed!!" )
        cancel()  # Stop any integrations that might be running...
        raise
    scriptlog("Script " + fullScriptName + " completed")
    if nested:
        fullScriptName = previousScript
        s.setScriptName(previousScript)
    else: s.setScriptState(carma.control.COMPLETED)
    return True

def appendHistory(msg, delim='\n'):
    """ Append comment to script history and write to script log on disk """

    # append message to script history if it is not full
    if s.getScriptHistoryFull():
        if not s.getScriptBool(odi.INDX_BOOL_HISTORYFULL):
            trackMessage('History file truncated', printLog=False)

        s.setScriptBool(odi.INDX_BOOL_HISTORYFULL, True)
    else:
        s.addScriptHistory(msg + '\n')

    # Append message to hard disk
    if s.getScriptString(odi.INDX_STR_SCRIPTLOG) <> '':
        try:
            fout = open(s.getScriptString(odi.INDX_STR_SCRIPTLOG), 'a')
            fout.write(msg + '\n')
            fout.close()
        except Exception:
            pass

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

def trackMessage(msg, timestamp=True, printLog=True, indent=0):
    """ Prints a  message to the screen.

        Inputs: msg       - message to print
                timestamp - if True, print LST time stamp
                printLog  - in addition to printing the message to the
                            screen, print the message to the control log
                indent    - indent the message by indent*4 spaces before
                            printing msg.
    """
    stmp = ''
    if timestamp:
        ut = time.gmtime()
        ut = float(ut[3]) + float(ut[4])/60.0 + float(ut[5])/3600.0
        stmp = helpers.convertHmsString(ut) + ' '
    nindent = 0
    if indent <> None: nindent = indent
    for i in range(nindent): stmp += '    '
    stmp += msg
    printMessage(stmp)
    scriptlog(stmp)
    if printLog: appendHistory(stmp)


def trackWarning(msg, timestamp=False, printLog=True, indent=0, alarm=False):
    """ Prints a warning message to the screen.

        Inputs: msg       - message to print
                timestamp - if True, print LST time stamp
                printLog  - in addition to printing the message to the
                            screen, print the message to the control log
                indent    - indent the message by indent*4 spaces before
                            printing msg.
                alarm     - if True, trigger the alarm
    """
    stmp = ''
    if timestamp: stmp = helpers.convertHmsString(lst()) + ' '
    for i in range(indent): stmp += '    '
    stmp += msg
    printWarning(stmp,alarm=alarm)
    stmp = 'WARNING: ' + stmp
    scriptlog(stmp)
    if printLog: appendHistory(stmp)


def restartTrack(triggerAlarm=True, **kw):
    """ Restarts the track based on the system variables

        Inputs: **kw          - keywords to pass to observing script
                triggerAlarm  - trigger the alaram if there is an error
                                while executing the script.
    """
    # Get script name
    scriptName = s.getScriptString(odi.INDX_STR_SCRIPTNAME)
    if scriptName == None or scriptName == "":
        raise Exception, 'Error restarting track: script name not found'

    # Run script
    kw['restart'] = True
    kw['subscript'] = True
    kw['triggerAlarm'] = triggerAlarm
    if not run(scriptName, **kw):
        raise Exception, 'Error restarting track ' + scriptName

    # Success, apparently
    return True

def setLstEndTrack(t=None):
    """ Set ending LST time for track.

        Inputs: lstend - ending LST time in HH:MM:SS or hours.
                         If not set, the track is stopped after the
                         current integration is complete.
    """
    if t == None:
        s.setScriptDouble(odi.INDX_DBL_LST_END_TRACK,0.0)
        s.setScriptBool(odi.INDX_BOOL_LST_END_TRACK,False)
    else:
        s.setScriptDouble(odi.INDX_DBL_LST_END_TRACK,helpers.convertHms(t))
        s.setScriptBool(odi.INDX_BOOL_LST_END_TRACK,True)

def endtrack(lst=None, ut=None):
    """ Set ending LST time for track.

        Inputs: lst - LST ending time in HH:MM:SS or hours.
                ut  - UT ending time in HH:MM:SS or hours.

                If neither lst or ut is set, then the track
                is stopped after the current integration is complete.
    """
    if lst == None and ut == None:
        s.setScriptBool(odi.INDX_BOOL_ENDTRACK,True)
        printInColor("\nWarning: If you are running the queue() program, the next\nscript in the queue file will still be executed unless\nyou comment or remove the remaining scripts!\n\n", color='blue')
        trackMessage('endtrack() command issued')
    else:
        if lst == None:  # UT must be set if lstend = None
            xut = time.gmtime()
            xut = float(xut[3]) + float(xut[4])/60.0 + float(xut[5])/3600.0
            lst = helpers.convertHms(ut) + s.lst() - xut
            if lst > 24.0: lst -= 24.0
            if lst <  0.0: lst += 24.0

        setLstEndTrack(lst)
        s.setScriptBool(odi.INDX_BOOL_ENDTRACK,False)
        trackMessage('Setting endtrack time to ' + helpers.convertHmsString(lst) +
                     ' LST')


def setTmax(t, verbose=True):
    """Reset maximum track length
    Params:
     t: Track length (hours); value of None unsets max track length
     verbose: prints out message about setting/unsetting, default=True"""
    if t == None:
        s.setScriptDouble(odi.INDX_DBL_TMAX,0.0)
        s.setScriptBool(odi.INDX_BOOL_TMAX,False)
        if verbose:
            trackMessage('Unsetting maximum track length')
    else:
        s.setScriptDouble(odi.INDX_DBL_TMAX,helpers.convertHms(t))
        s.setScriptBool(odi.INDX_BOOL_TMAX,True)
        if verbose:
            trackMessage('Setting maximum track length to '+str(t)+ ' hours')

def lastcal():
    """ End source/phase-calibrator cycles and observe calibrator one last time.
        The current integration will complete before going to the last
        calibrator. Also, if there are multiple phase calibrators in the
        track, lastcal() will only end the current phase calibrator cycle."""
    s.setScriptBool(odi.INDX_BOOL_LASTCAL,True)
    printInColor("\nWarning: If you are running the queue() program, the next\nscript in the queue file will still be executed unless\nyou comment or remove the remaining scripts!\n\n", color='blue')
    trackMessage('lastcal() command issued')


def doOptPoint(opnt):
    """ Set whether or not optical pointing should be done
    Param:
     opnt: True  -> use optical pointing
           False -> do not use optical pointing"""
    s.setScriptBool(odi.INDX_BOOL_DO_OPT_POINT, opnt)
    s.setScriptBool(odi.INDX_BOOL_DO_OPT_POINT_SET, True)

def stopAfterNextCal():
    """ End source/phase-calibrator cycles after observing next
        observation of the phase calibrator.

        If there are multiple phase calibrators in the track,
        stopAfterNextCal() will only end the current phase calibrator cycle.
    """
    s.setScriptBool(odi.INDX_BOOL_STOPNEXTCAL,True)
    trackMessage('stopAfterNextCal() command issued')

def endTrackAlarm(value=None):
    """ Set True/False if the alarm should be sound at end of track.
        If value=None, then return True/False current value of variable.
    """
    if value == None:
        return (s.getScriptBool(odi.INDX_BOOL_ALARM) != 0)
    else:
        s.setScriptBool(odi.INDX_BOOL_ALARM, value)

def configName(name):
    """Set the name for the current configuration, e.g E07a.
    Parameter:
     name: current configuration name"""
    s.setConfigName(name)

def restoreConfigName( ):
    """Retrieve the array configuration name from the monitor system and
    set appropriately."""
    subNo = s.getSubarrayNo()
    configMpName = "Control.Subarray%d.configName" %subNo
    retries = 24 
    arrayConfig = queryString( configMpName, retries )
    configName(arrayConfig)

#--------------------------------------------------------------------
# Testing commands
def acqtimer(off1, off2, ants=0, azelmode = True) :
    """Put in offsets and measure acquisition time. An optional parameter
    controls whether azel offsets are used or radec (equatorial) offsets.
    Offsets set to zero on completion.
    Parameters:
      off1: azimuth or ra offset in arcmin (on sky)
      off2: elevation or dec offset in arcmin
      ants: single or list of antenna numbers, 0=all (default)
      azelmode: controls offset vs. equatOffset (default=True)
    Returns a dictionary of acquisition times with antenna numbers as keys  """
    result = dict()
    notdone = True
    t0 = time.time()
    while notdone :
        if azelmode :
            ret = offset(off1, off2, ants, tmo=100, waiton=ANY)
        else:
            ret = equatOffset(off1, off2, ants, tmo=100, waiton=ANY)
        t = time.time()
        for a in ret.ready :
            deltat = t-t0
            result[a] = deltat
        #print ret.ready, ret.notready, numNotready(ret)
        notdone = numNotready(ret)
        ants = ret.notready
    # Print out results
    ants = result.keys()
    ants.sort()
    for a in ants :
        spacer = " "
        if a > 9 : spacer = ""
        st = "  C%d%s: %6.2f" %(a,spacer, result[a])
        print st
    # Set offsets to zero
    print "Setting offsets to zero..."
    if azelmode :
        ret = offset(0, 0, ants, tmo=100, waiton=ANY)
    else:
        ret = equatOffset(0, 0, ants, tmo=100, waiton=ANY)
    offset(0, 0, ants, waiton=ALL)
    # Return results in case someone wants to use them
    return result

# ------------------------Track, eTrack ----------------------------
def track(sourcename, ants=0, phaseCenter=None, tmo=500, waiton=NONE,
    azWrapMode=TRACKTIME, trackTime=25.0, subarray=DEFAULT) :
    """Send antennas to a new source and wait for them to get there.
    Any offsets that have been set with the offset() command are set to zero.
    Parameters:
     sourcename: A source name from the catalog
     ants: A single or list of antenna numbers;
        zero is all antennas plus the phase center
     phaseCenter: with default value of None, phase center tracking is determined
         by the ants parameter (0=True, anything else=False); other accepted
         values are True or False. If True, this source is used as the phase
         center for delay and fringe tracking.
     tmo: Timeout value in seconds; zero inhibits timeout. Default is 500sec.
     waiton: All to be complete (ALL), or just the first (ANY) or NONE.
      A specific number of antennas is signified by a positive integer,
      and 'all except' by a negative integer.
      Default is NONE, allowing the wait to typically be done in the
      subsequent integrate command.
     azWrapMode: how azimuth ambiguity will be resolved.
      TRACKTIME(default): guarantee tracking time before limits
      ADD: add a turn
      SUB: subtract a turn
      ZERO: don't add or subtract anything
     trackTime: amount of guaranteed tracking time when azWrapMode=TIME,
        in minutes
    Return value contains a list of the ready antennas and a list
    of the not ready antennas.
    Examples of usage:
     track('3c273')               # All antenna, no timeout
     track('3c273', tmo=180)      # All antenna, 3 minute timeout
     track('3c273', 3, 120)       # carma3, 2 minute timeout
     track('3c273', [1,3,8], 420) # carma1, 3 and 8 with a 7 minute timeout

    E-array: A special mode to move the antennas without colliding by
     setting the IS_E_CONFIGURATION variable in etrack.py. This variable
     is used because synchronization of the motion mode and miriad header
     variable controlled by the configName command. When in this mode
     the ants argument is ignored and the command applies to all antennas
     in the subarray. The phaseCenter parameter is ignored and set to True.
     The azWrapMode is ignored and set to TRACKTIME of 25 minutes.
    See also: info, whazup, offset, move, stow, integrate
    """
    # The import of etrack here ensures that everything in this module is
    # available to etrack in spite of the circular import using '*'
    import  etrack

    antlist = helpers.makeList(ants)
    antsByType = antennasByType(antlist)
    bimaAnts = antsByType[1]
    # If in E-configuration, use etrack
    #if (subarrayNo == 1) and (etrack.IS_E_CONFIGURATION):
    if ( len ( bimaAnts ) > 0 ) and (etrack.IS_E_CONFIGURATION):
        etrack.basicEtrack(sourcename, waiton=waiton, tmo=tmo, ants=ants)
        # Re-issue track command for all antennas. This is necessary
        # for "source" to appear in rtd window and to get proper wait
        # and return values
        overTheTop = False
        s.track(sourcename,antlist,True,TRACKTIME,25.0,overTheTop)
        try :
            return wait(TRACK, antlist, tmo, waiton)
        except carma.control.TimeoutException :
            return [[],[]]

    # Non E-array mode
    antlist = helpers.makeList(ants)
    if phaseCenter == None :
        if antlist[0] == 0 : pcTracking = True
        else :               pcTracking = False
    else :
        pcTracking = phaseCenter
    # Over the top is not supported so must always be false
    overTheTop = False
    multiSubarray('track', subarray,
            sourcename,antlist,pcTracking,azWrapMode,trackTime,overTheTop)
    try :
        return wait(TRACK, antlist, tmo, waiton, subarray=subarray)
    except carma.control.TimeoutException :
        return [[],[]]

def wtrack(sourcename, ants=0, phaseCenter=None, tmo=500) :
    """Send antennas to a new source and wait for **ALL** of them to get there
    or for a timeout. The 'w' prefix is for 'wait'.
    Parameters:
     sourcename: A source name from the catalog
     ants: A single or list of antenna numbers;
        zero is all antennas plus the phase center
     phaseCenter: with default value of None, phase center tracking is determined
         by the ants parameter (0=True, other=False); if this parameter is set
         to True or False then it determines phase center tracking
     tmo: Timeout value in seconds; zero inhibits timeout. Default is 500sec.
    See track."""
    antlist = helpers.makeList(ants)
    if phaseCenter == None :
        if antlist[0] == 0 : pcTracking = True
        else :               pcTracking = False
    else :
        pcTracking = phaseCenter
    s.track(sourcename, antlist, pcTracking)
    try :
        return wait(TRACK, antlist, tmo, ALL)
    except carma.control.TimeoutException :
        return [[],[]]

def beginrestore( ):
    s.restorationInProgress( True )

def endrestore( ):
    s.restorationInProgress( False )

def prepAntennaForMove( ants, az=None, el=None, shutdown=True ) :
    """Prepare antennas for move during configuration change.
    This routine will remove ants from their current subarray, stow them 
    (unless otherwise specified via az/el options), shutdown all carma 
    processes on the antenna, then shutdown the antenna computer.
    This routine can only be called from an engineering subarray.
    Parameters:
        ants Single antenna number or list of Carma antenna numbers.
        az Desired azimuth (defaults to None)
        el Desired elevation (defaults to None) 
        shutdown Whether to shutdown the antenna computer (Default: True)
    """
    
    antlist = helpers.makeList(ants)

    thisSaNo = Subarray.getSubarrayNo()

    if thisSaNo <= 2:
        warning  = "So as not to interfere with running science tracks, "
        warning += "prepAntennaForMove can only be called from an engineering "
        warning += "subarray.  Please ask observers to remove antennas which "
        warning += "need moved from sci1, sci2 and/or sciall, then add them "
        warning += "to eng1 or eng2 by typing 'addAntenna(%s)' before "%antlist
        warning += "repeating this command."
        printWarning( warning )
        return 

    currentAnts = currentAntennaNumbers()
    antsNotInSa = [ ant for ant in antlist if ant not in currentAnts ]
    if len( antsNotInSa ) != 0:
        warning  = "Ants %s need to be in this subarray before I "%antsNotInSa
        warning += "can prep them for move.  Please remove them from their "
        warning += "current subarray(s), add them to this one and try again."
        printWarning( warning )
        return

    # Ok, whoever finally got it right - stow the antenna(s).
    if az != None or el != None:
        print "Moving ants %s - to input az/el..."%antlist
        move(az, el, ants=antlist)
    else:
        antsByType = antennasByType(antlist)
        nonSzaAnts = antsByType[0] + antsByType[1]
        szaAnts = antsByType[2]
        if len( nonSzaAnts ) > 0:
            print "Stowing 6m and 10m dishes %s..."%nonSzaAnts
            stow(ants=nonSzaAnts, tmo=360, waiton=NONE, position=ZENITH)

        if len( szaAnts ) > 0:
            saz=165
            sel=18
            print "Moving 3.5m dishes %s to az %s, el %s..."%(szaAnts,saz,sel)
            move(saz, sel, ants=szaAnts, tmo=360, waiton=NONE)

        print "    Waiting for antennas to stow/move, this can take several "
        print "    minutes so watch 'em crawl via RTD if you're bored..."
        wait(TRACK, antlist, tmo=360, waiton=ALL)

    # Now remove them from the subarray
    print "Removing ants %s from subarray..."%antlist
    antsRemoved = removeAntenna(antlist)

    if len(antsRemoved) != len(antlist):
        warning  = "Unable to remove all antennas from subarray!  Tried to "
        warning += "remove %s but actually removed %s."%(antlist, antsRemoved)
        warning += "Please post this message to Bug 903 and reopen the bug: "
        warning += "http://www.mmarray.org/bugzilla/show_bug.cgi?id=903. "
        warning += "Continuing despite this and shutting down ants."
        printWarning(warning)

    # Finally ssh to the antenna machine and turn off carma
    print "Connecting to ants %s and disabling carma services..."%antlist
    ssh='ssh -o ServerAliveCountMax=3 -o ServerAliveInterval=5 -o StrictHostKeyChecking="no"'
    for ant in antlist:
        command = "%s control@c%d sudo /sbin/service carma stop"%(ssh,ant)
        print "    %s"%command
        os.system(command) 
    if ( shutdown ) :
        print "Shutting down pxe crates on ants %s..."%antlist
        for ant in antlist:
            command = "%s control@c%d sudo /sbin/shutdown -h now"%(ssh,ant)
            print "    %s"%command
            os.system(command) 

        print "Ants %s are now safe to move..."%antlist
    else :
        print "Ants %s are NOT safe to move - command failed."%antlist


################################################################################
### Process List
################################################################################

# TAO IMR Client
from TaoImrClient import TaoImrClient
imrclient = TaoImrClient(carmaIni.corba.imr_)

def processList(machine=None, tree=False, running=None, stopped=None):
    """List all CORBA processes

    Parameters:
        machine - a specific machine name to list (case insensitive)
        tree    - tree view (all machines)
        running - show running processes
        stopped - show stopped processes

    Examples:
        1) Flat list of all stopped processes (default)
        ==> processList()
        2) Flat list of all processes (both running and stopped)
        ==> processList(running=True)
        3) Tree view of all stopped processes
        ==> processList(tree=True)
        4) All processes on machine acc.carma.pvt
        ==> processList(machine='acc.carma.pvt')
        5) All processes on machine acc (same as above, shorter to type)
        ==> processList(machine='acc')

    """
    imrclient.update_server_info()
    imrclient.process_list(machine, tree, running, stopped)

def processStatus(name, verbose=False):
    """Get the status of a CORBA process

    Parameters:
        name    - the name of the process (case insensitive)
        verbose - print extra information

    Examples:
        1) Get the status of the FaultSystem process
        ==> processStatus('FaultSystem')
        2) Get the status of the FaultSystem process (same as above)
        ==> processStatus('faultsystem')
        3) Get the status of the FaultSystem process with extra info
        ==> processStatus('FaultSystem', verbose=True)

    """
    imrclient.update_server_info()
    imrclient.process_status(name, verbose)

def processStart(name):
    """Start a CORBA process

    Parameters:
        name - the name of the process (case insensitive)

    Examples:
        1) Start the FaultSystem process
        ==> processStart('FaultSystem')
        2) Start the FaultSystem process (same as above)
        ==> processStart('faultsystem')

    """
    imrclient.update_server_info()
    imrclient.process_start(name)

    # FIXME: this is a workaround until the Bima processes are fixed
    # FIXME: such that their initialized monitor points actually work
    m = re.match(r'^\s*(th|dm|rm|mh|if|ot|ch)bima([1-9])\s*$', name)
    if m:
        ant = int(m.group(2)) + 6
        print 'WARNING: You MUST re-initialize C%d by using removeAntenna(%d)' % (ant, ant)
        print 'WARNING: followed by addAntenna(%d) in the correct subarray' % (ant, )

def processStop(name):
    """Stop a CORBA process

    Parameters:
        name - the name of the process (case insensitive)

    Examples:
        1) Stop the FaultSystem process
        ==> processStop('FaultSystem')
        2) Stop the FaultSystem process (same as above)
        ==> processStop('faultsystem')

    """
    imrclient.update_server_info()
    imrclient.process_stop(name)

def processRestart(name):
    """Restart a CORBA process

    Parameters:
        name - the name of the process (case insensitive)

    Examples:
        1) Restart the FaultSystem process
        ==> processRestart('FaultSystem')
        2) Restart the FaultSystem process (same as above)
        ==> processRestart('faultsystem')

    """
    imrclient.update_server_info()
    imrclient.process_restart(name)

    # FIXME: this is a workaround until the Bima processes are fixed
    # FIXME: such that their initialized monitor points actually work
    m = re.match(r'^\s*(th|dm|rm|mh|if|ot|ch)bima([1-9])\s*$', name)
    if m:
        ant = int(m.group(2)) + 6
        print 'WARNING: You MUST re-initialize C%d by using removeAntenna(%d)' % (ant, ant)
        print 'WARNING: followed by addAntenna(%d) in the correct subarray' % (ant, )
#--------------------------------------------------------------------
def linelengthinit() :
    """Initializes the linelength system for all antennas in the subarray.
    This command takes control of the local oscillator reference
    synthesizer and manually steps the frequency to measure the absolute
    line lenghts. The absolute lenghts are required for the correction of
    phase as a function of LO reference frequency. The LO reference frequency
    changes to perform Doppler tracking. This is a secondary effect compared to
    the relative changes in linelength caused by temperature. The relative 
    changes will be measued correctly without an initialization. 
    Running this command during a science script will ruin the data because
    the frequency will be changed. Run this command inbetween tracks.
    The hardware only supports running this command in sci1 or sci2. """
    if subarrayNo < 3: 
        import LineLength
        LineLength.quickinit()
    else :
        m = "The hardware only supports a linelength system for sci1 and sci2" 
        raise Exception, m 
 
#--------------------------------------------------------------------
def addPycheck(pathToAdd=None) :
    "Temporary location until it is installed as part of system"
    if pathToAdd == None:
        pathToAdd = '/home/scott/carma/scripts/python/pycheck/pychecker-0.8.18'
    sys.path.append(pathToAdd)
    print "Now type: import pychecker.checker"

def isShadowedNow(carmaAntNo, shadowingType=SHADOW_INTERNAL, diameterFraction=0.0) :
    return s.isShadowedNow(carmaAntNo, shadowingType, diameterFraction)

def isShadowedHaDec(carmaAntNo, hourAngleHours, decDegrees, shadowingType=SHADOW_INTERNAL, diameterFraction=0.0) :
    return s.isShadowedHaDec(carmaAntNo, hourAngleHours, decDegrees, shadowingType, diameterFraction)

def isShadowedSource(carmaAntNo, sourceName, lstHours=-1.0, shadowingType=SHADOW_INTERNAL, diameterFraction=0.0) :
  try:
    return s.isShadowedSource(carmaAntNo, sourceName, lstHours, shadowingType, diameterFraction)
  except carma.util.UserException, ex: 
    errorMsg = "    " + ex.errorMsg
    printError( errorMsg )
    raise Exception, errorMsg

def checkHardwareState(astroband) :
    try:
        s.checkConfigurationSuccess(astroband)
        print("Hardware state is ok");
    except carma.util.UserException, ue:
        printError( ue.errorMsg )

def scriptChecker(filename):
    """Check a CARMA observing script for errors."""
    if not os.path.exists(filename):
        print 'ERROR: %s does not exist' % filename
        import errno
        return errno.ENOENT

    # The script-checker program is called directly. If we call the code
    # from this python interpreter, any changes to an observing script will
    # not be noticed.
    #
    # This is due to the way python works: a second import statement of the
    # same module does nothing!
    import subprocess
    script = helpers.getCarmaBuildPath() + '/scripts/script-checker'
    cmd = [script, filename]
    ret = subprocess.call(cmd)
    if ret != 0:
        print 'ERROR: script-checker returned status code:', ret

def antCommonPrefix(antnum):
    """Takes a carma antenna number (1-23) and returns a string for the 
    prefix of the antenna common monitor points for that antenna.
    Parameter:
      antnum: carma antenna number [1-23]
      returns ant common MP prefix, e.g. 'Ovro2.AntennaCommon.'"""
    return device.Carma(antnum).getName() + ".AntennaCommon."
    
def antCommonDrivePrefix(antnum):
    """Takes a carma antenna number (1-23) and returns a string for the 
    prefix of the antenna common drive monitor points for that antenna.
    Parameter:
      antnum: carma antenna number [1-23]
      returns ant common MP prefix, e.g. 'Ovro2.AntennaCommon.Drive'"""
    return antCommonPrefix(antnum) + "Drive."
#--------------------------------------------------------------------
# ====== Repetitve tasks ======
# Task indices
REPTASK_TILTSCI1      = 0   
REPTASK_TILTSCI2      = 1
REPTASK_FLUXCALWEEKLY = 2

def _repTaskInit(taskIndex, taskName, interval, remind, auto) :
    s.setRepTaskName(taskIndex, taskName)
    s.setRepTaskInterval(taskIndex, interval)
    s.setRepTaskRemind(taskIndex, remind)
    s.setRepTaskAuto(taskIndex, auto)
    repTaskCompleted(taskIndex)
    
def repTaskInit(taskIndex, name="", interval=1.0, remind=0.8, auto=0.6):
    return _repTaskInit(taskIndex, name, interval, remind, auto)    

def repTaskCompleted(taskIndex) :
    s.setRepTaskCompleted(taskIndex, s.mjd(0))
    
def _repTaskDisplay(taskIndex, skipEmpty = True) :
    r = "Control.RepTask%d." %(taskIndex+1)
    try:
        n = queryString(r+"taskName")
    except Exception:
        if skipEmpty: return
        print "Task (index=%d) has not been initialized" %taskIndex
        return
    if len(n) <= 1 and skipEmpty: return
    #print "*****%s****" %n
    m = "%3d %-20s " %(taskIndex, n)
    m += "%8.2f "  %(queryDouble(r+"timeLastDone"))
    m += "%6.3f "  %(queryDouble(r+"since"))
    m += "%4.2f "  %(queryDouble(r+"repeatInterval"))    
    m += "%4.2f "  %(queryDouble(r+"reminderInterval"))    
    m += "%4.2f "  %(queryDouble(r+"autoInterval"))
    m += "%-5s "    %(queryBool(r+"reminderReady"))
    m += "%-5s "    %(queryBool(r+"autoReady"))
    print m

def repTaskDisplay():
    try:
        e = str(queryInt("Control.repTaskErrors"))
    except Exception:
        e = "?"
    m = "Errors in update loop: %s" %e
    print m
    m = "Idx          Name           Last   Since  rep  rem auto  rem  auto"
    l = "--- -------------------- -------- ------ ---- ---- ---- ----- -----"
    print m
    print l
    for i in range(10):
        _repTaskDisplay(i, skipEmpty=True)
        
def repTaskIsReady(taskIndex) :
    r = "Control.RepTask%d." %(taskIndex+1)
    try:
        n = queryString(r+"taskName")
    except Exception:
        print "Task (index=%d) has not been initialized" %taskIndex
        return
    if len(n) <= 1 : 
        print "Task (index=%d) has not been initialized" %taskIndex
        return
    return queryBool(r+"autoReady")
                   
def rtTILT1display():
    _repTaskDisplay(REPTASK_TILTSCI1)
def rtTILT2display():
    _repTaskDisplay(REPTASK_TILTSCI2)
def rtFLUXWEEKLYdisplay():
    _repTaskDisplay(REPTASK_FLUXCALWEEKLY)
def rtTILT1completed():
    repTaskCompleted(REPTASK_TILTSCI1)
def rtTILT2completed():
    repTaskCompleted(REPTASK_TILTSCI2)
def rtFLUXWEEKLYcompleted():
    repTaskCompleted(REPTASK_FLUXCALWEEKLY)
def rtTILT1isReady():
    return repTaskIsReady(REPTASK_TILTSCI1)
def rtTILT2isReady():
    return repTaskIsReady(REPTASK_TILTSCI2)
def rtFLUXWEEKLYisReady():
    return repTaskIsReady(REPTASK_FLUXCALWEEKLY)
def rtFLUXWEEKLYisReady():
    return repTaskIsReady(REPTASK_FLUXCALWEEKLY)
        
#--------------------------------------------------------------------
# Handy utilities
def slewEstimate(source1,source2=None,ant=1,returnUnit='time') :
    """ Return estimated time of shortest slew or angluar distance of 
    shortest slew.
    Params:
     source1: name of source tp slew to
     source2: name of source to slew from; if None use current position
     ant: antenna number, starting at 1
     returnUnit: a string, with 'time', the default, returning minutes;
       anything else returns XXX"""
    azel1    = azel(source1)
    azel1[0] = _wrapFix(azel1[0], ant)
    if source2 == None: 
        azel2 = currentAzel(ant)
    else:
        azel2 = azel(source2)
        azel2[0] = _wrapFix(azel2[0], ant)
    az1 = [azel1[0], _wrapChoice(azel1[0], ant)]
    az2 = [azel2[0], _wrapChoice(azel2[0], ant)]
    print az1, az2
    deltaAz = 360
    deltaAz = min(deltaAz, abs(az1[0]-az2[0]))
    deltaAz = min(deltaAz, abs(az1[0]-az2[1]))
    deltaAz = min(deltaAz, abs(az1[1]-az2[0]))
    deltaAz = min(deltaAz, abs(az1[1]-az2[1]))
    delta = [deltaAz, abs(azel1[1]-azel2[1])]
    print delta
    if ant < 7:
        azRate = 60.0
        elRate = 30.0
        settleTime = 5.0
    elif ant < 16: 
        azRate = 120.0
        elRate = 60.0
        settleTime = 10.0
    else: 
        azRate = 60.0
        elRate = 30.0
        settleTime = 8.0
    if returnUnit == 'time': 
        return max(delta[0]/azRate,delta[1]/elRate) + settleTime/60.0
    return delta

def _wrapFix(az, ant):
    """Returns a legitimate (within limits) azimuth for a specific az 
    on a given antenna.
    If the input az is within range then it is returned.
    Params:
     az: in degrees
     ant: carma antenna number, starting at 1"""
    azlimpos = queryCommonDouble("Drive.Limit.azHighSwLimitVal", ant)
    azlimneg = queryCommonDouble("Drive.Limit.azLowSwLimitVal",  ant)
    if az > azlimpos: return az-360
    if az < azlimneg: return az+360 
    return az

def _wrapChoice(az, ant):
    """Returns other wrap azimuth choice for a specific az on a given antenna.
    If there is no choice in the other wrap then the input az is returned.
    Params:
     az: in degrees
     ant: carma antenna number, starting at 1"""
    azlimpos = queryCommonDouble("Drive.Limit.azHighSwLimitVal", ant)
    azlimneg = queryCommonDouble("Drive.Limit.azLowSwLimitVal", ant)
    if az+360 < azlimpos: return az+360
    if az-360 > azlimneg: return az-360 
    return az

def currentAzel(ant):
    az = queryCommonDouble("Drive.Track.actualAzimuth", ant)
    el = queryCommonDouble("Drive.Track.actualElevation", ant)
    return [az,el]
        
def queryCommonDouble(mpname, ants, retries=NONE) :
    """Returns value of MP for a single or list of ants (0 gives list of all
    current ants.
    Params:
     mpname: Hierarchical name that is appended to 'Antname#.AntennaCommon.'.
     ants: a single or list of antenna numbers. Zero gives all antennas
           in the current array
     retries: number of times to retry getting a valid MP value. Default (NONE)
           gives the default for queryDouble (currently=1)      
    Return: a single or list of doubles representing the MP values"""
    antlist = makeAntList(ants)
    rtn = []
    for a in antlist:
        mp = device.Carma(a).getName() + ".AntennaCommon."+mpname
        if retries == NONE: v = queryDouble(mp)
        else:               v = queryDouble(mp, retries)
        rtn.append(v)
    if int == type(ants) and (ants != 0): return rtn[0]
    return rtn 
       
def distSky(source,ref):
    """Return angular distance (degrees) between a ref and source.
    Taken directly from short.py.
    Params:
     source: source name
     ref: reference name"""
    srcAzEl = azel(source)
    refAzEl = azel(ref)
    deg2rad = 180.0/math.pi
    srcAz   = srcAzEl[0]/deg2rad
    srcEl   = srcAzEl[1]/deg2rad
    refAz   = refAzEl[0]/deg2rad
    refEl   = refAzEl[1]/deg2rad
    cosDist = math.sin(refEl)*math.sin(srcEl) + \
              math.cos(refEl)*math.cos(srcEl)*math.cos(refAz-srcAz)
    dist    = math.acos(cosDist)*deg2rad
    return  float(dist)

def getRaDec(sourcename) :
    """Return ra/dec for a source. The system and user catalogs are searched.
    Params:
     sourcename:
    Return: a list with the ra and dec in degrees"""
    return s.getRaDec(sourcename)

#--------------------------------------------------------------------
# Utilities to list pointing constants

def _radioPointingSetup( aperture, ants ):
    """Retrieve current radio pointing constants.
    Parameters:
     aperture: One of RX1MM, RX3MM or RX1CM 
     ants: List of antennas or 0 for all antennas in the current array. 
    Return:
     List of aperture coefficients in a list of antennas."""     
    if aperture == RX1MM:
        apString = "Aperture1mm"
    elif aperture == RX3MM:
        apString = "Aperture3mm"
    elif aperture == RX1CM:
        apString = "Aperture1cm"
    else:
        raise Exception, "Invalid aperture."

    ants = makeAntList(ants)
    mpList = []
    for ant in ants:
        prefix =  "Control.Antenna%d.%s.PointingConstants." % (ant, apString)
        mpNames = [ prefix + mp for mp in ["azOffset", "elOffset", "sag"] ]
        mpList.append( mpNames )    
    return queryMpValues( mpList )

def list3mmPointing() :
    """List the current 3mm offsets for all antennas.
    This command can be run from any subarray."""
    p3mm = _radioPointingSetup(RX3MM, range(1,24))
    # output commands for the current 3 mm constants
    for i in range(1,24):
        print "radio3mmPointingConstants(%6.2f, %5.2f, %5.2f, %2i)" \
                %(p3mm[i-1][0], p3mm[i-1][1], p3mm[i-1][2], i)

def list1mmPointing() :
    """list the current 1mm offsets for all antennas.
    This command can be run from any subarray."""
    p1mm = _radioPointingSetup(RX1MM, range(1,16))
    # output commands for the current 1 mm constants
    for i in range(1,16):
        print "radio1mmPointingConstants(%6.2f, %5.2f, %5.2f, %2i)" \
                %(p1mm[i-1][0], p1mm[i-1][1], p1mm[i-1][2], i)

def list1cmPointing() :
    """List the current 1cm offsets for all antennas.
    This command can be run from any subarray."""
    p1cm = _radioPointingSetup(RX1CM, range(1,24))
    # output commands for the current 1 cm constants
    for i in range(1,24):
        print "radio1cmPointingConstants(%6.2f, %5.2f, %5.2f, %2i)" \
                %(p1cm[i-1][0], p1cm[i-1][1], p1cm[i-1][2], i)
     
#--------------------------------------------------------------------
# These need to be at the bottom to pick up all the preceding stuff when
# they circularly import this file using 'from subarrayCommands import *'
from tiltMeasurement import tilt
from tiltMeasurement import tiltDataSummary
from tiltMeasurement import tiltSamplePlot
from tiltMeasurement import tiltZeroPlot
from tiltMeasurement import tiltMagPlot
from tiltMeasurement import tiltDirPlot
from tiltMeasurement import tiltResPlot
from cameraControl import Camera, snapshot, opticalSystem
from ivcurve import ivcurve
from continuousIntegMosaic import Mosaic
from radioPoint import radioPoint

