# Initialization of all carma subarray controllers
# 
# @author Steve Scott
# $Id: subarrayInit.py,v 1.157 2014/06/24 23:01:14 scott Exp $
#
# $CarmaCopyright$
#
#

from device import *
from carmaHelpers import *
from subarrayCommands import *
from printFunctions import *
import Subarray
import time
import LineLength
import sys

def isEven( num ) :
    x = divmod(num,2)
    return ( x[1] == 0 )

def isOdd( num ) :
    return ( not isEven(num) )

def isSinglepol( conf ) :
    return ( conf == "LL" or conf == "RR" )

def isAdvancedConfig( conf ) :
    return ( not isSinglepol( conf ) )

def initSafeRanges( ):
    """Initialize safe ranges for all antennas."""
    progress("Setting SAFE az/el ranges")
    allAnts = list( xrange(1,24) )
    restoreAntCommand( setSafeRange, allAnts )

def initCorrelator( corrType ):
    progress("Configuring correlator(s) to wideband mode")
    if (corrType == CORR_NONE ) :
        emsg = "\n\tNo correlator to restore."
        printInColor(emsg, color='magenta', linefeed=False)
        return
    if ((corrType & CORR_SPECTRAL) == CORR_SPECTRAL ):
          for i in range(1,9) :
             astrobandOnline(i,True)
    if ((corrType & CORR_WIDEBAND) == CORR_WIDEBAND):
          for i in range(9,25) :
             astrobandOnline(i,True)
    if ((corrType & CORR_C3GMAX23) == CORR_C3GMAX23):
          for i in range(25,33) :
             astrobandOnline(i,True)
    if ((corrType & CORR_C3GMAX8) == CORR_C3GMAX8):
          for i in range(33,41) :
             astrobandOnline(i,True)

    print "\n"
    configwideastroband(conf="LL")

#@DEPRECATED - MWP 5/2014
def initCorrelatorOLD( corrType ):
    progress("Correlator config")

    startAstroBand = 1
    bands = 0
    if ( corrType == CORR_NONE ) :
        emsg = "\n\tNo correlator to restore."
        printInColor(emsg, color='magenta', linefeed=False)
        return
    if ( corrType == CORR_SPECTRAL ) :
        startAstroBand = 1
        bands = 8
    elif ( corrType == CORR_WIDEBAND ): 
        startAstroBand = 9
        bands = 16
    elif ( corrType == CORR_C3GMAX23 ):
        startAstroBand = 25
        bands = 8
    elif corrType == CORR_C3GMAX8:
        startAstroBand = 33
        bands = 8
    else :  # multicorrelator mode
#TODO: What happens if e.g. SPECTRAL+C3GMAX8.  Need to skip astrobands in the middle!
        if ( corrType & CORR_SPECTRAL ) :
            bands += 8
        if ( corrType & CORR_WIDEBAND ) :
            bands += 15 
        if ( corrType & CORR_C3GMAX8 ) :
            bands += 8 
        if ( corrType & CORR_C3GMAX23 ) :
            bands += 8 

    onlineAstroBands = list( xrange(startAstroBand, startAstroBand + bands) )
    try:
        progress("   clearing astroband configurations")
        clearastroband(0)
    except Exception: 
        print "Warning: Failed to clear all AstroBands"
    for astroband in onlineAstroBands:
        # skip CARMA23,DUALPOL,FULLPOL for even bands to
        # avoid warning from signalpathMapper
        corrBand = astroband +1 - startAstroBand
        x=corrBandSetup(corrBand,corrType)
        if ( isEven(corrBand) and isAdvancedConfig(x['conf']) ): 
            continue
        progress("   configuring astroband %d for %s"%(astroband, x['conf']))
        try:
            configastroband( astroband, **corrBandSetup( corrBand, corrType ) )
        except Exception as inst:
            print inst
            print "Warning: Failed to restore AstroBand %d" % astroband
            continue
    if ((corrType & CORR_SPECTRAL) == CORR_SPECTRAL ) :
           progress("Optimizing thresholds on spectral bands")
           optimizeThresholds()
           progress("Flattening phases on spectral bands")
           flattenPhases()

def restoreCorrDesig(subA) :
    """Initialize which correlator(s) are attached to which subarray(s)"""
    try:
        subNo = subA.getSubarrayNo()
        # get the correlator designation enumeration (bitmask)
        mp = "Control.Subarray%d." % subNo + CORRELATOR_DESIGNATION_MP
        desig = queryInt(mp)

        if ((desig == 0 ) ) :
            print "No correlators were assigned to this subarray.";
        if ((desig & CORR_WIDEBAND) == CORR_WIDEBAND ) :
            print "Adding WIDEBAND correlator";
            subA.addCorrelator(CORR_WIDEBAND)            
        if ((desig & CORR_SPECTRAL) == CORR_SPECTRAL ) :
            print "Adding SPECTRAL correlator";
            subA.addCorrelator(CORR_SPECTRAL)
        if ((desig & CORR_C3GMAX23 ) == CORR_C3GMAX23 ):
            print "Adding C3GMAX23 correlator";
            subA.addCorrelator(CORR_C3GMAX23)
        if ((desig & CORR_C3GMAX8 ) == CORR_C3GMAX8 ):
            print "Adding C3GMAX8 correlator";
            subA.addCorrelator(CORR_C3GMAX8)
        return desig
    except:
        printInColor("\n\tWARNING: Could not add correlator. This must be done manually",
                     color='magenta',linefeed=False)
        return CORR_NONE

def physInit() :
    """Initializes the physical parts of the system that can be
    in common to all subarrays. This includes the mapping of the
    hardware and the assignment of antennas to subarrays."""

    # Get the global subarray reference
    progress("Getting subarray ref")
    s = Subarray.getSubarray()

    # Set the subarray control to uninitialized state so that
    # 1) commands to antennas not in the subarray will be ignored
    # 2) delays will not be broadcast as each position and pad offset is updated.
    s.setInitializationFlag(False)

    progress("Array configuration")
    restoreConfigName( )

    initSafeRanges()

    #progress("Project")
    #resetProject(closeTrial=False);

    progress("Standard observing")
    # Make sure we're back into regular observing mode
    s.fringeTracking(True)
    pswitchon(0)
    # This is a blackbelt debugging mode that we don't want to use
    s.lrPhaseOffsetMode(False)
    # Turn off the correlated noise source
    noiseoff()
    # Turn off cameras - also forces use of 3mm radio pointing constants
    camera(OFF, 0)

    progress("Freq setup")
    [frest, sb, fif] = qfreqSetup()
    qfreq( frest, sb, fif, None )

    # Added for new noise source and quadmod code. JWL 07-Oct-2007
    progress("Set noise source default attenuation to preset.")
    noisePreset()
    progress("Set quadmod default attenuation to zero")
    quadmodAtten(0)


def init1() :
    "Initialize subarray#1"

    # Get the global subarray reference
    s = Subarray.getSubarray()

    # Alarm management
    alarmoff()
    s.alarm1mm(False)

    # Turn off alarm for engineering subarrays, but don't fail if they don't exist
    try :
        Subarray.getSubarrayRef(3).alarm(False, "")
    except Exception: print "Eng1 alarm not available"
    try :
        Subarray.getSubarrayRef(4).alarm(False, "")
    except Exception: print "Eng2 alarm not available"

    # Enable the alarm (including the hardware enable switch)
    alarmenable(hardware=True)

    progress("Correlator designations")
    corrSetup = restoreCorrDesig(s)

    antsToAdd = subarrayAntSetup()

    if len( antsToAdd ) != 0:
        progress("Add antennas %s"%formatAsRanges( antsToAdd ))
        addAntenna( antsToAdd, ignoreOwnership=True, retune=False, tsys=False )

    restorePositionsOfUnownedAntennas() 

    progress("RefLO assignment")
    # reference LO
    s.assignLO(1)

    # Step synthesizer, compute absolute linelengths
    progress("Fit linelengths")
    try:
        LineLength.quickinit()
    except:
        pass

    physInit() # Includes freq

    initCorrelator(corrSetup);

    # Tsys and flux
    applyTsys( True )
    applyFlux( True )

    # Elev and tracking limits
    restoreElevlimit( )
    restoreTrackThreshold(0)
    driveErrorPreference(PREF_BLANK)

    # Set the script state
    scriptClear()

    # set pointstatus monitor point to onsrc (default for interferometry)
    pointstatus(ONSRC,0)

    # Turn off noise source    
    noiseoff()
    
    progress("Tsys" )
    tsys( ifsetup=True )

def init2() :
    # Get the global subarray reference
    s = Subarray.getSubarray()

    progress("Correlator designations")
    corrSetup = restoreCorrDesig(s)

    antsToAdd = subarrayAntSetup()
    if len( antsToAdd ) != 0:
        progress("Add antennas %s"%formatAsRanges( antsToAdd ) )
        addAntenna( antsToAdd, ignoreOwnership=True, retune=False, tsys=False )

    progress("RefLO assignment")
    # reference LO
    s.assignLO(2)

    # Step synthesizer, compute absolute linelengths
    progress("Fit linelengths")
    try:
        LineLength.quickinit()
    except:
        pass

    physInit() # Includes freq

    initCorrelator(corrSetup);

    # Set the script state
    scriptClear()
    alarmoff()
    s.alarm1mm(False)

    # Turn off noise source    
    noiseoff()

    progress("Tsys" )
    tsys( ifsetup=True )

def init3() :
    # Get the global subarray reference
    s = Subarray.getSubarray()

    antsToAdd = subarrayAntSetup()
    if len( antsToAdd ) != 0:
        progress("Add antennas %s"%formatAsRanges( antsToAdd ))
        addAntenna( antsToAdd, ignoreOwnership=True, retune=False, tsys=False )

    progress("RefLO assignment")
    # reference LO
    s.assignLO(3)

    # Note: the hardware only supports measurement of absolute linelengths
    # in sci1 and sci2!

    physInit()

    # Set the script state
    scriptClear()
    alarmoff()
    s.alarm1mm(False)

    # Elev and tracking limits
    restoreElevlimit( )
    restoreTrackThreshold(0)

def init4() :
    # Get the global subarray reference
    s = Subarray.getSubarray()

    physInit()

    # Set the script state
    scriptClear()
    alarmoff()
    s.alarm1mm(False)
    
    antsToAdd = subarrayAntSetup()
    if len( antsToAdd ) != 0:
        progress("Add antennas %s"%formatAsRanges( antsToAdd ) )
        addAntenna( antsToAdd, ignoreOwnership=True, retune=False, tsys=False )

    # Elev and tracking limits
    restoreElevlimit( )
    restoreTrackThreshold(0)

def init5() :
    physInit()

def subarrayInit() :
    subarrayNo = Subarray.getSubarrayNo()
    print "Initializing subarray#%d" %subarrayNo

    # Set the subarray control to uninitialized state so that
    # 1) commands to antennas not in the subarray will be ignored
    # 2) delays will not be broadcast as each position and pad offset is updated.
    s.setInitializationFlag(False)

    try:
        beginrestore()
        if subarrayNo == 1: init1()
        if subarrayNo == 2: init2()
        if subarrayNo == 3: init3()
        if subarrayNo == 4: init4()
        if subarrayNo == 5: init5()
        endrestore()
    except:
        endrestore()
        raise

    # Record that we are finished
    s.setInitializationFlag(True)
    done()

def selfTest() :
    """Do a few quick tests to verify functionality"""
    saNo = Subarray.getSubarrayNo()
    if saNo <= 2:
        progress("Doing a short test integration")
        resetProject(closeTrial=False);
        # Do a single short integration here just to make sure it works
        if("NONE" in queryString("Control.Subarray%i.CorrelatorDesignation" % (saNo))) :
            print "skipping. (no correlator)"
            return
        intent("NONE","O",False,False);
        integrate(2.0, 1, antwait=NONE,tmo=10)
        print "done."

