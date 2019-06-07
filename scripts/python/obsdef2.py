# obsdef2.py
#
# @author John Carpenter
# $Id: obsdef2.py,v 1.218 2011/12/16 20:50:44 iws Exp $
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
#   2007-Feb-26  JMC    Extensive modifications to reflect new template
#   2007-Mar-19  JMC    Added options to point on flux and passband
#                       calibrators, and pointing at 95 GHz
#   2007-Mar-21  JMC    Added waiton options to track
#   2007-Apr-17  JMC    Now write script log to disk. 
#                       Replaced waiton with antwait.
#   2007-Jun-20  MWP    Updated to conform to project database use.
#   2007-Jul-21  JMC    Add etrack() command and supporting functions.
#   2007-Aug-13  JMC    Separated etrack and utility functions
#   2007-Sep-28  JMC    Added more optical pointing functionality
#   2008-Sep-28  JMC    Removed etrack

import string
import time
import os
import short
import copy
import printFunctions
import fileIOPython
import snapshot as sn
import pacs
import runCommand as rc

# In the process of removing 'from X import *'
import subarrayCommands as commands
import obsdefUtils as utils
import carmaHelpers as helpers

from subarrayCommands import *
from carmaHelpers import *
from obsdefUtils import *

from obsdefIndex import *

import refPoint
import opticalGuideMain as ogm

MINIMUM_ELEVATION_TUNING = 35      # Degrees
EXTRAWAIT                = 10.0    # Seconds. Extra wait time for track command
DB_PROJECTS              = "pdb"   # Name of psql project database

#def integrate(records, nreps, tmo=None, antwait=None):
#   com = 'Integrating for ' + str(records) + ' sec x ' + str(nreps) + ' reps.'
#   #commands.trackMessage(com,indent=1)
#   sleep(records * nreps)
#
#def currentAntennaNumbers():
#    return range(1,16)
#
#def fakeOpticalPointing():
#    import random
#    ants = commands.currentAntennaNumbers()
#
#    optRes = dict()
#    results = dict()
#    badOptAnts = list()
#    for a in ants:
#        z = random.random()
#        if z > 0.2:
#           optRes[a] = [a/60.0, -a/60.0]
#           results[a] = [0.001 * a, -0.001 * a, 0.01 * a, -0.01 * a];
#        else:
#           badOptAnts.append(a)
#
#    return [optRes, badOptAnts, results]
#
#def fakeSubOptical(badants):
#    import random
#
#    applyAntVec = list()
#    applyAzOff = list()
#    applyElOff = list()
#    for a in commands.currentAntennaNumbers():
#        if random.random() > 0.2 and a not in badants:
#            applyAntVec.append(a)
#            applyAzOff.append(-0.5*a/60.0)
#            applyElOff.append(0.5*a/60.0)
#
#    return [applyAntVec, applyAzOff, applyElOff]


def getNfluxCal(fluxcal):
    """ Returns maximum number of flux calibrators that may be observed
        in a flux calibration cycle """

    ntot = fluxcal['ncal']
    if fluxcal['doPrimary'] and fluxcal['doSecondary'] and fluxcal['doBoth']:
        ntot += 1
    if ntot == None or ntot < 0: ntot = 0
    return ntot


def finishedPassband(passband):
    """ Returns True/False if a passband observations have been completed """
    return (not passband['doPassband'] or 
            commands.s.getScriptInt(INDX_INT_NOBS_PB) >= passband['ncal'])


def finishedFluxcal(fluxcal):
    """ Returns True/False if a flux calibrator observations are complete """
    # Initialize
    done = commands.s.getScriptInt(INDX_INT_NOBS_FLUX) >= fluxcal['ncycles'] or \
           (not fluxcal['doPrimary'] and not fluxcal['doSecondary'])

    # Check if primary calibrator has been observed
    if needPrimaryFluxCalibrator(fluxcal): done = False

    # Check if secondary calibrator has been observed
    #if needSecondaryFluxCalibrator(fluxcal): done = False

    return done


def needSecondaryFluxCalibrator(fluxcal):
    """ Returns True/False if secondary flux calibrator still 
        needs to be observed 
    """
    return (fluxcal['doSecondary'] and (fluxcal['doBoth'] or
            commands.s.getScriptInt(INDX_INT_NOBS_FLUXSEC) == 0))


def needPrimaryFluxCalibrator(fluxcal):
    """ Returns True/False if primary flux calibrator still 
        needs to be observed 
    """
    return (fluxcal['doPrimary'] and commands.s.getScriptInt(INDX_INT_NOBS_FLUXPRI) == 0)


def readRestartVariables(var, mosaic, pointing):
    """ Reads system variables upon restarting track 

        Inputs:  var      - variable dictionary
                 mosaic   - mosaic structure
                 pointing - pointing structure
    """

    # Print message
    commands.trackMessage("Restarting observing script")

    # Options
    var['alarm']     = commands.endTrackAlarm()
    var['email']     = utils.emailScriptLog()
    var['endtrack']  = utils.getLstEndTrack(string=True)
    var['flux']      = None
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_FLUX_SET):
        var['flux'] = commands.s.getScriptBool(INDX_BOOL_COMMAND_FLUX)
    var['fluxstart'] = commands.s.getScriptBool(INDX_BOOL_COMMAND_FLUXSTART)
    var['lststart']  = utils.getLstStartCycle(string=True)
    var['lststop']   = utils.getLstStopCycle(string=True)
    var['pb']        = None
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_PB_SET):
        var['pb'] = commands.s.getScriptBool(INDX_BOOL_COMMAND_PB)
    var['pbstart']   = commands.s.getScriptBool(INDX_BOOL_COMMAND_PBSTART)
    var['pnt']       = None
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_PNT_SET):
        var['pnt'] = commands.s.getScriptBool(INDX_BOOL_COMMAND_PNT)
    var['pntstart']  = commands.s.getScriptBool(INDX_BOOL_COMMAND_PNTSTART)
    var['opnt']      = None
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_OPNT_SET):
        var['opnt'] = commands.s.getScriptBool(INDX_BOOL_COMMAND_OPNT)
    var['pacs']      = commands.s.getScriptBool(INDX_BOOL_PACS)
    var['tmax']      = utils.getTmax()
    var['tune']      = commands.s.getScriptBool(INDX_BOOL_TUNE)
    commands.trackMessage('Previous options:', indent=1)
    keys = var.keys()
    keys.sort()
    for k in keys:
        s = ''
        for i in range(len(k),13): s += '.'
        val = var[k]
        # The following is a kludge. I want to print True/False instead of 0/1
        # for the booleans. Since I cannot be certain whether the parameter is
        # an integer or a boolean, I merely check for 0/1.
        if type(val) == int:
           if val == 0:
              val = False
           elif val == 1:
              val = True
        commands.trackMessage(k +  s +  ' ' + str(val), indent=2)

    # Check status of calibrations
    if commands.s.getScriptBool(INDX_BOOL_PASSBAND):
        commands.trackMessage('passband calibrator (' +
              commands.s.getScriptString(INDX_STR_PASSBAND) + 
              ') has already been observed', indent=1)
    if commands.s.getScriptBool(INDX_BOOL_FLUXPRIMARY):
        commands.trackMessage('primary flux calibrator (' +
              commands.s.getScriptString(INDX_STR_FLUXPRIMARY) + 
              ') has already been observed', indent=1)
    if commands.s.getScriptBool(INDX_BOOL_FLUXSECONDARY):
        commands.trackMessage('secondary flux calibrator (' +
              commands.s.getScriptString(INDX_STR_FLUXSECONDARY) + 
              ') has already been observed', indent=1)

    # Time stamps
    if commands.s.getScriptBool(INDX_BOOL_LST_START_TRACK):
        commands.trackMessage('track originally started at ' +
                     helpers.convertHmsString(utils.getLstStartTrack()) + ' LST', indent=1)
    if commands.s.getScriptBool(INDX_BOOL_TMAX):
        t = commands.s.getScriptDouble(INDX_DBL_TMAX)
        commands.trackMessage('maximum time for track is ' +
              str("%.1f" % t) + ' hours', indent=1)
    if commands.s.getScriptBool(INDX_BOOL_LASTPOINT):
        commands.trackMessage('pointing last performed at ' +
              helpers.convertHmsString(utils.getLstLastPoint()) + ' LST', indent=1)
    if commands.s.getScriptBool(INDX_BOOL_LST_START_CYCLE):
        lststart = commands.s.getScriptDouble(INDX_DBL_LST_START_CYCLE)
        commands.trackMessage('setting cycle start time to ' +
              helpers.convertHmsString(lststart) + ' LST', indent=1)
    if commands.s.getScriptBool(INDX_BOOL_LST_STOP_CYCLE):
        lststop = commands.s.getScriptDouble(INDX_DBL_LST_STOP_CYCLE)
        commands.trackMessage('setting cycle stop time to ' +
              helpers.convertHmsString(lststop) + ' LST', indent=1)


    # Starting position in mosaic
    lastpos = utils.getLastMosaicPosition()
    if lastpos > 0:
        commands.trackMessage('last observed position in mosaic was position ' +
                     str(lastpos), indent=1)

    # Reset stop cycle
    commands.s.setScriptBool(INDX_BOOL_STOPNEXTCAL,False)
    commands.s.setScriptBool(INDX_BOOL_LASTCAL,False)
    commands.s.setScriptBool(INDX_BOOL_ENDTRACK,False)

    # Optical pointing values
    badants = commands.s.getScriptString(INDX_STR_OPT_BADANTS)
    sopt    = commands.s.getScriptString(INDX_STR_OPT_RES)
    if sopt != "": 
        # Bad antennas
        if not pointing.has_key('badOptAnts'):
            pointing['badOptAnts'] = list()
            if badants != '': pointing['badOptAnts'] = utils.makeListObsdef(badants)

        # Optical pointing results
        if not pointing.has_key('optRes'):
            pointing['optRes'] = dict()
            z = sopt.split()
            for i in range(0,len(z),3):
               ant = int(z[i])
               d_az = float(z[i+1])
               d_el = float(z[i+2])
               pointing['optRes'][ant] = [d_az, d_el]


def setIFLevels(correlator, donoise=True):
    """ Runs tsys(ifsetup=True) for calibrator correlator configurations """
    # Get current configuration and set iflevels
    resetCorrelator = False
    specConfig = utils.getConfigBand()
    commands.tsys(ifsetup=True)

    # Integrate on noise-source to trigger starting track time for quality
    if donoise: utils.observeNoise(2, 2, verbose=True)

    # Add missing parameters to correlator dictionary.
    if not correlator.has_key('reconfig'):   correlator['reconfig']   = None
    if not correlator.has_key('hybrid'):     correlator['hybrid']     = None
    if not correlator.has_key('tintHybrid'): correlator['tintHybrid'] = 5.0

    # Set correlator for reconfig options
    contConfig = correlator['configCal']
    if correlator['reconfig'] <> None:
        resetCorrelator = True
        utils.setConfigBand(correlator['configCal'])
        commands.tsys(ifsetup=True)

    # Set correlator for hybrid options
    # if correlator['hybrid'] <> None:
    #     resetCorrelator = True
    #     for h in correlator['hybrid']:
    #         hconfig = utils.getHybridConfiguration(h, specConfig, contConfig)
    #         utils.setConfigBand(hconfig)
    #         commands.tsys(ifsetup=True)

    # Set correlator back to original mode
    if resetCorrelator: utils.setConfigBand(specConfig)


def findCalibrator(options=None, returnFluxes=False, verbose=True, 
                   force=False,  sources=None,       correlator=None):
    """ Observes calibrators and finds one bright enough to observe.

        Inputs: options      - "sources" options in template2.obs
                returnFluxes - if True, then return list of fluxes instead
                               of calibrator
                force        - if True, then force the calibrator fluxes to
                               be measured even if they were previously 
                               measured.
                sources      - list of sources that overrides sources
                               in "options".

        Output: adopted calibrator if returnFluxes = False

        Relevant keywords in the options dictionary:
            'callist'   : list of calibrators
            'phaseCal'  : default calibrator if a new one is not found
            'tintlist'  : integration time in seconds for each calibrator
            'tPhaseCalList' : integration time in minutes if a source in 
                              callist is accepted
            'fluxlimit' : minimum acceptable flux in Jy. Can be a single number
                          of a list.
            'usebrightest' : If True, then use the bright source in the list.
    """
    # See if calibrator has already been found. If so, then we are finished.
    if not force and commands.s.getScriptBool(INDX_BOOL_FINDCALIBRATOR):
        stmp  = commands.s.getScriptString(INDX_STR_FINDCALIBRATOR)
        stint = commands.s.getScriptDouble(INDX_DBL_FINDCALIBRATOR_TINT)
        if verbose:
            commands.trackMessage('Measuring calibrators')
            commands.trackMessage('Phase calibrator from previous search: ' + 
                         stmp + ', tint=' + str('%.1f' % stint) + ' min', 
                         indent=1)
        return stmp,stint

    # Set default calibrator
    result = [None, None]
    if options <> None: 
        result = [options['phaseCal'], options['tintPhaseCal']]

    # Get calibrator names
    calibrators = sources
    if options <> None: calibrators = utils.makeListObsdef(options['callist'])
    if calibrators == None: return result

    # Message
    if verbose: commands.trackMessage('Measuring calibrators')

    # Antenna 1 must be on-line
    if not utils.inSubarray(1):
       commands.trackMessage('Antenna 1 is off-line - using default calibrator ' +
                    result[0], indent=1)
       return result

    # Get flux limits
    fluxlimits = None
    if options <> None and options.has_key('fluxlimit'): 
        fluxlimits = utils.makeListObsdef(options['fluxlimit'])
    if fluxlimits <> None:
        if len(calibrators) != len(fluxlimits) and len(fluxlimits) > 1:
            raise Exception, 'callist and fluxlimits have different sizes.'
        if len(calibrators) != len(fluxlimits):
            fluxlimits = [fluxlimits[0]] * len(calibrators)

    # Get phase cal integration times if the source is adopted
    tintPhaseCal = utils.makeListObsdef(result[1])
    if options <> None and options.has_key('tPhaseCalList'):
        if options['tPhaseCalList'] <> None:
            tintPhaseCal = utils.makeListObsdef(options['tPhaseCalList'])
    if tintPhaseCal <> None:
        if len(calibrators) != len(tintPhaseCal) and len(tintPhaseCal) > 1:
            raise Exception, 'callist and tintPhaseCal have different sizes.'
        if len(calibrators) != len(tintPhaseCal):
            tintPhaseCal = [tintPhaseCal[0]] * len(calibrators)

    # Set integration time for observing calibrators in this function
    tint = 60.0
    if options <> None and options.has_key('tintlist'): 
        tint = options['tintlist']

    # Set correlator for calibrator observations
    origConfig = None
    reconfig = None
    if correlator <> None and correlator['configCal'] <> None:
        origConfig = utils.getConfigBand()
        utils.setConfigBand(correlator['configCal'])

    # Determine which bands have 500 MHz band width
    config = utils.getConfigBand()
    bands62 = list()
    bands = list()
    for c in config:
        if c[1] == commands.BW62:  bands62.append(c[0])
        if c[1] == commands.BW500: bands.append(c[0])
    if len(bands) == 0:
        if len(bands62) > 0: 
            bands = bands62[:]
        else:
            commands.trackMessage('Must be at least one 500 MHz or 62 MHz band in configuration', 
                         indent=1)
            if origConfig <> None: utils.setConfigBand(origConfig)
            return result

    # Use brightest source?
    useBrightest = False
    if options <> None and options.has_key('useBrightest'): 
        useBrightest = options['useBrightest']

    # Loop over calibrators
    amp = list()
    for ic in range(len(calibrators)):
        # Proceed?
        if utils.isEndOfTrack(): 
            commands.trackMessage('stopping measurements based on endtrack() command',
                  indent=1)
            return result

        # Check source elevation
        c = calibrators[ic]
        commands.intent(c,"G")
        msg = str('%-10s' % c) + ' : '
        x = None
        if (utils.getSourceElevation(c) < utils.getElevationLimit()):
            msg += ' source at too low of elevation (' + str('%.1f' % utils.getSourceElevation(c))
            amp.append(x)
        else:
            # Observe calibrator
            commands.track(c, waiton=options['antwait'])
            commands.tsys()
            commands.integrate(tint,1)
            commands.sleep(2.0) # Just to make sure monitor point has the time to arrive

            # Loop over antennas and get amplitudes
            antennas = commands.currentAntennaNumbers()
            amplist = list()
            found = False
            for ant in antennas:
                for band in bands:
                    for sb in ['Usb', 'Lsb']:
                        # Make monitor point
                        mp = 'SLPipeline.Input' + str(ant) + '.Band' + str(band)
                        #mp_snr = mp + '.SignalToNoise.IntegratedSNR.' + sb
                        mp_amp = mp + '.VisAverages.Integrated.' + sb

                        # Get value
                        try:
                            xcmp = commands.queryComplex(mp_amp)
                            xamp = (xcmp[0]**2 + xcmp[1]**2) ** 0.5
                            #xsnr = min(commands.queryDouble(mp_snr),5)
                            amplist.append(xamp)
                        except :
                            pass

            # Compute median amplitude
            amplist.sort()
            x = amplist[len(amplist)/2]
            if x == 0.0: x = None
            amp.append(x)
            if x <> None:
                msg += 'flux = ' + str('%6.2f' % x) + ' Jy; limit = '
                if fluxlimits == None:
                    msg += ' None'
                else:
                    msg += str('%6.2f' % fluxlimits[ic]) + ' Jy'
            else:
                msg += 'no flux measured'
        if verbose: commands.trackMessage(msg, indent=1)

        # If source is bright enough, we are done
        if x <> None and fluxlimits <> None and not useBrightest:
            if x >= fluxlimits[ic]:
                found = True
                result = [c, tintPhaseCal[ic]]
                break

    # Reset correlator
    if origConfig <> None: utils.setConfigBand(origConfig)

    # Choose flux calibrator if a calibrator has not been not found already
    if not found:
        # Find best source
        ibest = None
        for i in range(len(amp)):
            # Get amplitude
            a = amp[i]

            # Skip it if amplitude=None
            if a == None: continue

            # Is this the best source?
            isBest = False
            if fluxlimits == None:
                if ibest == None:
                    isBest = True
                elif a > amp[ibest]:
                    isBest = True
            elif a >= fluxlimits[i]:
                if ibest == None:
                    isBest = True
                elif useBrightest and a > amp[ibest]:
                    isBest = True
            if isBest: ibest = i

        # Print message
        if ibest == None:
            if verbose: 
                commands.trackMessage('None of the calibrators are bright enough', 
                             indent=1)
        else:
            result = [calibrators[ibest], tintPhaseCal[ibest]]
            if verbose: 
                commands.trackMessage(result[0] + 
                     ' is the brightest source meeting flux limits', indent=1)

    # Done
    if list in [type(result[0])]:
        if verbose: 
            commands.trackMessage('Using initial phaseCal list as the phase calibrator',
                          indent=1)
    elif verbose:
        commands.trackMessage('Adopted phase calibrator: ' + result[0] + ', integration time = '+str('%.1f' % result[1])+ ' min',
                      indent=1)

    # Return either fluxes or adopted calibrator
    if returnFluxes:
        return amp
    else:
        commands.s.setScriptString(INDX_STR_FINDCALIBRATOR, result[0])
        commands.s.setScriptDouble(INDX_DBL_FINDCALIBRATOR_TINT, result[1])
        commands.s.setScriptBool(INDX_BOOL_FINDCALIBRATOR, True)
        return result


def setHiddenOpticalPointing(pointing):
    """ Set hidden variables for optical pointing """
    pointing['doOptPoint']  = utils.getVariable(pointing,'doOptPoint',False,nonone=True)
    pointing['intervalOpt'] = utils.getVariable(pointing,'intervalOpt',10.0/60.0,nonone=True)
    pointing['ncoadd']      = utils.getVariable(pointing,'ncoadd',None)
    pointing['subtract']    = utils.getVariable(pointing,'subtract',None)
    pointing['maxmag']      = utils.getVariable(pointing,'maxmag',None)
    pointing['maxOptSep']   = utils.getVariable(pointing,'maxOptSep',20.0,nonone=True)
    pointing['nbadAnts']    = utils.getVariable(pointing,'nbadAnts',3,nonone=True)
    pointing['optPointCal'] = utils.getVariable(pointing,'optPointCal',True,nonone=True)
    pointing['optPointTarget'] = utils.getVariable(pointing,'optPointTarget',False,nonone=True)
    pointing['nfailOpt'] = utils.getVariable(pointing,'nfailOpt',3,nonone=True)
    pointing['centroidLoopMax'] = utils.getVariable(pointing,'centroidLoopMax',8,nonone=True)
    pointing['optradPreferred'] = utils.getVariable(pointing,'optradPreferred',None)
    pointing['optradTune95'] = utils.getVariable(pointing,'optradTune95',None)
    pointing['optradElmin'] = utils.getVariable(pointing,'optradElmin',30.0,nonone=True)
    pointing['optradElmax'] = utils.getVariable(pointing,'optradElmax',75.0,nonone=True)
    pointing['optElmin'] = utils.getVariable(pointing,'optElmin',None)
    pointing['optElmax'] = utils.getVariable(pointing,'optElmax',pointing['elmax'],nonone=True)


def setHiddenParameters(limits, fluxcal, mosaic, passband, pointing, sources,
                        noise=None):
    """ Set hidden parameters in the various parameter options """
    # Callist
    if not sources.has_key('callist'): sources['callist'] = None

    # Ambient load
    pn = 'ambient';
    limits[pn]   = utils.getVariable(limits,pn,False,nonone=True)
    mosaic[pn]   = utils.getVariable(mosaic,pn,limits[pn],nonone=True)
    fluxcal[pn]  = utils.getVariable(fluxcal,pn,limits[pn],nonone=True)
    sources[pn]  = utils.getVariable(sources,pn,limits[pn],nonone=True)
    passband[pn] = utils.getVariable(passband,pn,limits[pn],nonone=True)
    pointing[pn] = utils.getVariable(pointing,pn,limits[pn],nonone=True)

    # Elevation
    paramsLimits = ['minElevationCal', 'maxElevationCal']
    paramsNew    = ['elmin', 'elmax']
    for i in range(len(paramsLimits)):
        pl = paramsLimits[i]
        pn = paramsNew[i]
        fluxcal[pn]  = utils.getVariable(fluxcal,pn,limits[pl],nonone=True)
        passband[pn] = utils.getVariable(passband,pn,limits[pl],nonone=True)
        pointing[pn] = utils.getVariable(pointing,pn,limits[pl],nonone=True)

    # Preferred pointing sources
    pn = 'preferredPointing'
    fluxcal[pn]  = utils.getVariable(fluxcal,pn,None,nonone=True)
    passband[pn] = utils.getVariable(passband,pn,None,nonone=True)
    pn = 'intervalPointing'
    fluxcal[pn]  = utils.getVariable(fluxcal,pn,None)
    pn = 'forcePoint'
    fluxcal[pn]  = utils.getVariable(fluxcal,pn,False,nonone=True)
    passband[pn] = utils.getVariable(passband,pn,False,nonone=True)

    # antwait
    limits['antwait'] = utils.getVariable(limits,'antwait',-2,nonone=True)
    params = ['antwait']
    for p in params:
        fluxcal[p]  = utils.getVariable(fluxcal,p,limits[p],nonone=True)
        passband[p] = utils.getVariable(passband,p,limits[p],nonone=True)
        pointing[p] = utils.getVariable(pointing,p,limits[p],nonone=True)
        sources[p]  = utils.getVariable(sources,p,limits[p],nonone=True)
        mosaic[p]   = utils.getVariable(mosaic,p,limits[p],nonone=True)

    # PACS parameters.
    def_combine = int(round(limits['record'] * 2))
    pn = 'combine'
    limits[pn] = utils.getVariable(limits,'combine',def_combine,nonone=True)
    fluxcal[pn]  = limits[pn]
    passband[pn] = limits[pn]
    sources[pn]  = limits[pn]
    mosaic[pn]   = limits[pn]

    # Recordrc.
    paramsLimits = ['record']
    paramsNew    = ['record']
    for i in range(len(paramsLimits)):
        pl = paramsLimits[i]
        pn = paramsNew[i]
        fluxcal[pn]  = utils.getVariable(fluxcal,pn,limits[pl],nonone=True)
        passband[pn] = utils.getVariable(passband,pn,limits[pl],nonone=True)
        sources[pn]  = utils.getVariable(sources,pn,limits[pl],nonone=True)
        mosaic[pn]   = utils.getVariable(mosaic,pn,limits[pl],nonone=True)

    # Track timeout
    paramsLimits = ['tmoTrack']
    paramsNew    = ['tmo']
    for i in range(len(paramsLimits)):
        pl = paramsLimits[i]
        pn = paramsNew[i]
        fluxcal[pn]  = utils.getVariable(fluxcal,pn,limits[pl],nonone=True)
        passband[pn] = utils.getVariable(passband,pn,limits[pl],nonone=True)
        sources[pn] = utils.getVariable(sources,pn,limits[pl],nonone=True)
    mosaic['tmoTrack'] = utils.getVariable(mosaic,'tmoTrack',limits['tmoTrack'],nonone=True)
    mosaic['tmoMosaic'] = utils.getVariable(mosaic,'tmoMosaic',15.0,nonone=True)

    # Tsys
    paramsLimits = ['tsys']
    paramsNew    = ['tsys']
    for i in range(len(paramsLimits)):
        pl = paramsLimits[i]
        pn = paramsNew[i]
        fluxcal[pn]  = utils.getVariable(fluxcal,pn,limits[pl],nonone=True)
        mosaic[pn]   = utils.getVariable(mosaic,pn,limits[pl],nonone=True)
        passband[pn] = utils.getVariable(passband,pn,limits[pl],nonone=True)
        sources[pn]  = utils.getVariable(sources,pn,limits[pl],nonone=True)

    # Pointing
    pointing['nrepCross'] = utils.getVariable(pointing,'nrepCross',1,nonone=True)
    pointing['tune95']    = utils.getVariable(pointing,'tune95',False,nonone=True)
    pointing['tune95Pref'] = utils.getVariable(pointing,'tune95Pref',pointing['tune95'],nonone=True)
    pointing['intervalTran'] = utils.getVariable(pointing,'intervalTran',None)
    pointing['doPointSunRiseSet'] = \
              pointing['doPointNight'] or pointing['doPointDay']
    param = ['doPoint', 'maxsep']
    value = [False, pointing['maxsep']]
    for ip in range(len(param)):
        p = param[ip]
        v = value[ip]
        if not passband.has_key(p): passband[p] = v
        if not fluxcal.has_key(p):  fluxcal[p]  = v

    # Optical pointing
    setHiddenOpticalPointing(pointing)

    # Snapshot
    sources['doSnapshot'] = utils.getVariable(sources,'doSnapshot',False,nonone=True)
    sources['snapProject'] = utils.getVariable(sources,'snapProject',None)
    sources['snapElmax'] = utils.getVariable(sources,'snapElmax',None)
    sources['snapElHa'] = utils.getVariable(sources,'snapElHa',None)
    sources['snapRetune'] = utils.getVariable(sources,'snapRetune',False,nonone=True)
    sources['snapIF'] = utils.getVariable(sources,'snapIF',None)
    sources['snapFreq'] = utils.getVariable(sources,'snapFreq',None)
    sources['snapSB'] = utils.getVariable(sources,'snapSB',None)
    sources['snapTint'] = utils.getVariable(sources,'snapTint',None)
    sources['snapAutoPhase'] = utils.getVariable(sources,'snapAutoPhase',False)
    sources['snapAutoFlux']  = utils.getVariable(sources,'snapAutoFlux',1.0)
    sources['snapAutoSep']   = utils.getVariable(sources,'snapAutoSep',25.0)
    sources['snapAutoTint']   = utils.getVariable(sources,'snapAutoTint',None)
    sources['snapDb']   = utils.getVariable(sources,'snapDb',None)

    # Check to make sure snapshot sources exist
    if sources['doSnapshot']: checkSnapshotSources(sources)

    # PACS parameters
    sources['pacs'] = utils.getVariable(sources,'pacs',None)
    sources['phaseCalPacs'] = utils.getVariable(sources,'phaseCalPacs',None)

    # Sample time for noise source
    if noise <> None:
        defsample = None
        noise['tsample'] = utils.getVariable(noise,'tsample',defsample,nonone=False)


def initializeOpticalPointing():
    """ Launch optical pointing windows and set script variables """
    commands.s.setScriptString(INDX_STR_OPOINT_KEY, time.asctime())
    ogm.opticalInit()
    doOptPoint = True
    # if pointing['doOptPoint'] and commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT_SET):
    if commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT_SET):
       doOptPoint = commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT)
    commands.s.setScriptBool(INDX_BOOL_DO_OPT_POINT, doOptPoint)
    commands.s.setScriptBool(INDX_BOOL_DO_OPT_POINT_SET, True)


def initializeTrack(sources, mosaic,        projectInfo, 
                    limits,  pointing,      passband, 
                    fluxcal, restart=False, scriptName=None, 
                    scriptOptions=None, noise=None):
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
           sources     --- list of science sources and phase calibrators
           mosaic      --- mosaic options
           projectInfo --- information of project
           limits      --- various observing limits
           pointing    --- pointing options
           passband    --- passband options
           fluxcal     --- flux calibration options
           noise       --- noise source options
           restart     --- if True, restart system from previous track
           scriptName  --- Name of the observing script
           scriptOptions --- command line options passed to the script
    """

    # Add options to give observers flexibility on executing the script
    # A few options can override script dictionaries, and therefore their
    # default value is None (=> do not override script dictionary)
    var = dict()
    var['alarm']     = True
    var['email']     = True
    var['endtrack']  = None
    var['flux']      = None  # Can override script dictionaries
    var['fluxstart'] = True
    var['pacs']      = pacs.RUN_PACS
    var['full']      = False # Used for pacs test-tracks only
    var['pb']        = None  # Can override script dictionaries
    var['pbstart']   = True
    var['pnt']       = None  # Can override script dictionaries
    var['pntstart']  = True
    var['opnt']      = None  # Can override script dictionaries
    var['lststart']  = None
    var['lststop']   = None
    var['tmax']      = None
    var['tune']      = True

    # Read restart variables
    if restart: 
        readRestartVariables(var, mosaic, pointing)
        if var['tune']: 
            trackMessage('Setting tune to False for a track restart', indent=1)
            var['tune'] = False

    # Initialize run command options
    p = rc.Params()
    p.add("alarm",    default=var['alarm'],     type=bool,  description="Sound alarm at end of track")
    p.add("email",    default=var['email'],     type=bool,  description="Email scriptlog")
    p.add("endtrack", default=var['endtrack'],  type=str,   \
            noneAllowed=True, description="Ending LST time")
    p.add("flux",     default=var['flux'],      type=bool,  
            noneAllowed=True, description="Observe flux cal")
    p.add("fluxstart",default=var['fluxstart'], type=bool,  description="Observe flux cal at start of track")
    p.add("full",     default=var['full'],      type=bool,  description="Run full list in PACS test track")
    p.add("lststart", default=var['lststart'],  type=str,   
            noneAllowed=True, description="Starting LST time for phase-cal")
    p.add("lststop",  default=var['lststop'],   type=str,  
            noneAllowed=True,  description="Ending LST time for phase-cal")
    p.add("pacs",     default=var['pacs'],      type=bool,  description="Run PACS")
    p.add("pb",       default=var['pb'],        type=bool,  description="Observe passband cal")
    p.add("pbstart",  default=var['pbstart'],   type=bool,  description="Observe passband cal at start")
    p.add("pnt",      default=var['pnt'],       type=bool,  description="Perform radio pointing")
    p.add("pntstart", default=var['pntstart'],  type=bool,  description="Perform radio pointing at start")
    p.add("opnt",     default=var['opnt'],      type=bool,  
            noneAllowed=True, description="Perform optical pointing")
    p.add("tmax",     default=var['tmax'],      type=float, 
            noneAllowed=True, description="Maximum number of hours to run track")
    p.add("tune",     default=var['tune'],      type=bool,  description="Tune the receivers")

    # Read command line options
    p.processInputParameters(inputParams=commands.scriptKeyVals)
    
    # Clear system variables if not restarting track
    if restart: 
        # Initialize history variables
        j = MIN_INDX_HISTORY
        if INDX_INT_HISTORY > 0: j = commands.s.getScriptInt(INDX_INT_HISTORY)
        for i in range(MIN_INDX_HISTORY, j+1): 
            commands.s.setScriptString(i, "")
        commands.s.setScriptInt(INDX_INT_HISTORY, 0)
    else:
        commands.controlVariablesClear()
        commands.radioInit()
        utils.setPacsUT()

    # Reset tracking thresholds and drive blanking
    commands.trackThreshold(limits['trackingThreshold'])
    commands.driveErrorPreference(commands.PREF_BLANK)

    # Messages
    commands.trackMessage("Initializing track")
    commands.trackMessage('Local time      : ' + utils.getLocalTime(), indent=1)
    commands.trackMessage('LST             : ' + helpers.convertHmsString(commands.lst()), indent=1)
    if scriptName <> None: 
        commands.trackMessage('Script name     : ' + scriptName, indent=1)
    if len(commands.scriptKeyVals) == 0:
        commands.trackMessage('Script options  : None', indent=1)
    else:
        commands.trackMessage('Script options  : ', indent=1)
        keys = commands.scriptKeyVals.keys()
        keys.sort()
        for k in keys:
            s = ''
            for i in range(len(k),13): s += '.'
            commands.trackMessage(k +  s +  ' ' + str(commands.scriptKeyVals[k]), indent=2)
    commands.trackMessage('Project code    : ' + projectInfo['code'], indent=1)
    commands.addScriptString(INDX_STR_SCRIPTNAME, scriptName, append=False)
    commands.trackMessage('Elevation limit : ' + str('%.1f' % utils.getElevationLimit()), indent=1)
    utils.checkAntennas(indent=1)

    # Check project code is a string and a scalar
    if projectInfo['code'] == None or str not in [type(projectInfo['code'])]:
        raise Exception, 'Error specifying project code'

    # Set most hidden parameters.
    setHiddenParameters(limits, fluxcal, mosaic, passband, pointing, sources,
                        noise=noise)
    sources['full'] = p.full # this indicates if the full PACS test track should be run

    # Summarize main options
    if utils.getLstStartCycle() <> None: commands.trackMessage('LST start cycle = ' + 
        helpers.convertHmsString(utils.getLstStartCycle()), indent=1)
    if utils.getLstStopCycle() <> None: commands.trackMessage('LST stop  cycle = ' + 
        helpers.convertHmsString(utils.getLstStopCycle()), indent=1)
    if utils.getTmax() <> None: 
        commands.trackMessage('Maximum track length = ' + utils.dtString(utils.getTmax()), indent=1)

    # Store command line options back in variables
    if p.flux <> None: 
        fluxcal['doPrimary']   = p.flux
        fluxcal['doSecondary'] = p.flux
    if p.opnt <> None: 
        pointing['doOptPoint'] = p.opnt
    if p.pnt <> None: 
        pointing['doPointNight'] = p.pnt
        pointing['doPointDay'] = p.pnt
    if p.pb <> None:
        passband['doPassband'] = p.pb
    fluxcal['start']  = p.fluxstart
    passband['start'] = p.pbstart
    pointing['start'] = p.pntstart 

    # Set variable to indicate if we are running the PACS commands
    commands.s.setScriptBool(INDX_BOOL_PACS, p.pacs)

    # If this is a snapshot multi-frequency track, then we must retune.
    # Otherwise, the correlator configuration may get messed up.
    if sources['snapFreq'] <> None: p.tune = True

    # Optical pointing. Launch the optical pointing windows if the
    # keyword is set in the script. But the script variable controls
    # it is actually done.
    if pointing['doOptPoint']: initializeOpticalPointing()

    # Save command line variables in memory in case script is restarted
    commands.s.setScriptBool(INDX_BOOL_TUNE, p.tune)
    commands.endTrackAlarm(p.alarm)
    utils.emailScriptLog(p.email)
    commands.setLstEndTrack(p.endtrack)
    utils.setLstStartCycle(p.lststart, verbose=False)
    utils.setLstStopCycle(p.lststop, verbose=False)
    commands.setTmax(p.tmax, verbose=False)
    commands.s.setScriptBool(INDX_BOOL_COMMAND_FLUXSTART, p.fluxstart)
    commands.s.setScriptBool(INDX_BOOL_COMMAND_PBSTART, p.pbstart)
    commands.s.setScriptBool(INDX_BOOL_COMMAND_PNTSTART, p.pntstart)

    # Some options have None as a default since they can override
    # dictionary options. We therefore need to store two variables:
    # one that indicates if the command option has been set, and
    # a second that stores the value.
    commands.s.setScriptBool(INDX_BOOL_COMMAND_FLUX_SET, (p.flux <> None))
    commands.s.setScriptBool(INDX_BOOL_COMMAND_PB_SET, (p.pb <> None))
    commands.s.setScriptBool(INDX_BOOL_COMMAND_PNT_SET, (p.pnt <> None))
    commands.s.setScriptBool(INDX_BOOL_COMMAND_OPNT_SET, (p.opnt <> None))
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_FLUX_SET):
        commands.s.setScriptBool(INDX_BOOL_COMMAND_FLUX, p.flux)
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_PB_SET):
        commands.s.setScriptBool(INDX_BOOL_COMMAND_PB, p.pb)
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_PNT_SET):
        commands.s.setScriptBool(INDX_BOOL_COMMAND_PNT, p.pnt)
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_OPNT_SET):
        commands.s.setScriptBool(INDX_BOOL_COMMAND_OPNT, p.opnt)

    # Set obsblock
    tmpObsblock = projectInfo['obsblock']

    # Set subObsblock. Remove any spaces and '.'
    tmpSubobsblock = projectInfo['subobsblock']
    if tmpSubobsblock == None: tmpSubobsblock = ''
    tmpSubobsblock = tmpSubobsblock.split('.')[0]
    tmpSubobsblock = ''.join(tmpSubobsblock.split()) # Removes spaces

    # Get official obsblock
    tmpObsblock = utils.getOfficialObsblock(projectInfo['code'], tmpObsblock)

    # Initialize project and trial. However, if we are restarting the script
    # and the obsblock name has not changed, then to not re-initialize 
    # the obsblock.
    obsblockName = utils.getObsblockName()
    if restart and \
       obsblockName != commands.s.getScriptString(INDX_STR_OBSBLOCK_ID):
        raise Exception, 'Cannot restart the script since the obsblock name has changed.\nRe-issue the run() command without the restart option.'
    if not restart or \
       obsblockName != commands.s.getScriptString(INDX_STR_OBSBLOCK_ID):
        obsblockName = commands.newProject(projectInfo['code'], tmpObsblock, tmpSubobsblock)

    # Get the obsblock name. Sometimes the obsblock name is not updated
    # immediately, so I use the obsblock name returned from the newProject 
    # command
    commands.trackMessage('Obsblock name   : ' + obsblockName, indent=1)
    commands.sleep(2) # Wait for monitor stream to catch up
    commands.s.setScriptString(INDX_STR_OBSBLOCK_ID, obsblockName)

    # If PACS, then set SZA obsblock name
    if commands.s.getScriptBool(INDX_BOOL_PACS):
        sza_obsblock = pacs.getObsblock(obsblockName)
        commands.s.setScriptString(INDX_STR_PACS_OBSBLOCK, sza_obsblock)
        pacs.start(sza_obsblock, limits['combine'])
        trackMessage('Setting SZA obsblock to ' + sza_obsblock, indent=1)

    # Get trial number from obsblock
    strial = obsblockName.split('.')
    trial = int(strial[len(strial)-1])

    # Set output directory for script log
    rootDir = '/misc/sdp/quality/' + projectInfo['code']
    outputDir = rootDir + '/' + tmpObsblock 
    if tmpSubobsblock != "": outputDir += "." + tmpSubobsblock
    outputDir += "." + str(trial)
    outputFile = None
    try:
        os.makedirs(outputDir)
        os.chmod(rootDir,0775)
        os.chmod(outputDir,0775)
    except Exception:
        pass
    if os.access(outputDir, os.W_OK): 
        outputFile = outputDir + '/scriptlog.txt'
        try:
            fout = open(outputFile, 'a')
            fout.write(utils.getTrackHistory()+'\n')
            fout.close()
        except Exception, ex:
            outputFile = None
    if outputFile == None:
        commands.trackMessage('WARNING: Could not create output file for script log')
        outputFile = ''
    commands.s.setScriptString(INDX_STR_SCRIPTLOG, outputFile)

    # Load user catalog
    if projectInfo['sourceCatalog'] <> None: commands.ucat(projectInfo['sourceCatalog'])

    # Re-issue doppler command just in case the linelength was re-initialized
    if restart:
        commands.doppler(utils.getSourceName(sources['target'], n=1, parse=True))

    # Constraints
    commands.constraints()

    # Check mosaic file
    if mosaic['doMosaic'] and mosaic['offsetFile'] <> None:
        inputFile = utils.setMosaicFile(mosaic['offsetFile'])
        if not os.path.exists(inputFile):
            raise Exception, 'Cannot find mosaic file '+inputFile

    # Reset integration time to the same size as sources
    sourceIntent = None
    if sources.has_key('intentTarget'): sourceIntent = sources['intentTarget']
    dummy, sources['tintTarget'], sources['intentTarget']  = \
        utils.setSourceTintIntent(sources['target'], sources['tintTarget'], sourceIntent)

    # Set time stamps if needed
    if not commands.s.getScriptBool(INDX_BOOL_DATE):
        commands.addScriptString(INDX_STR_DATE, utils.getLocalTime(), bindx=INDX_BOOL_DATE)
    if not commands.s.getScriptBool(INDX_BOOL_LST_START_TRACK):
        commands.s.setScriptBool(INDX_BOOL_LST_START_TRACK, True)
        commands.s.setScriptDouble(INDX_DBL_LST_START_TRACK, commands.lst())

    # If tuning, then move antennas to a high elevation
    if utils.tuneReceivers():
        # Get list of current antennas
        ants = commands.currentAntennaNumbers()

        # Get list of antennas at low elevation
        antlist = list()
        for a in ants:
            # Move BIMA antennas only
            if a <= 6: continue

            # Set monitor point
            m = utils.getAntennaMp(a) + '.AntennaCommon.Drive.Track.actualElevation'

            # Get elevation
            if commands.queryDouble(m) < MINIMUM_ELEVATION_TUNING:
                antlist.append(a)

        # Move antennas?
        if len(antlist) > 0:
            commands.trackMessage('Moving antennas ' + utils.list2string(antlist) + ' to ' + \
                  str(MINIMUM_ELEVATION_TUNING) + ' deg elevation for tuning')
            commands.move(el=MINIMUM_ELEVATION_TUNING, ants=antlist, waiton=limits['antwait'])


def needPointing(options, ref, intervalDefault, maxsep, force):
    """ Determine if we need to point.

        Input parameters:
            options         : pointing options
            ref             : reference source
            intervalDefault : timing interval (minutes)
            maxsep          : maximum separation between sources (deg)
            force           : True or false, force pointing

        The following parameters are returned:
            doRadioPoint  :  True of False
            doOptPoint    :  True of False
            forceDistance :  Force pointing based on distance of last pointing
            resetMountOffsets : True of False
            sunStateCurrent   : Current position of the sun (Rise/Set/Transition)
    """
    # Initialize
    doRadioPoint = False
    doOptPoint = False
    if options['doOptPoint']: 
        doOptPoint = commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT)
    forceDistance = False
    resetMountOffsets = False

    # Get current position of sun
    dtSunSet  = utils.timeDifference(commands.lst(),utils.getLstSet('sun',elmin=0.0))
    dtSunRise = utils.timeDifference(commands.lst(),utils.getLstRise('sun',elmin=0.0))
    dtSunRiseSet = min(abs(dtSunSet), abs(dtSunRise))
    sunStateCurrent = utils.sunState(dtSunRiseSet)
    timeSunLimit = -dtSunSet
    if abs(dtSunRise) < abs(dtSunSet): timeSunLimit = dtSunRise

    # See if sun is in the same time period as the previous observation
    inTransition = (dtSunRiseSet <= utils.DT_TRANSITION)
    isDay = utils.isSunUp()
    isNight = not isDay
    inSameSunState = True
    if commands.s.getScriptBool(INDX_BOOL_LASTPOINT):
        sunStatePrevious = commands.s.getScriptInt(INDX_INT_SUNSTATE_LASTPOINT)
        inSameSunState = (sunStatePrevious == sunStateCurrent)

    # Set time interval for pointing
    interval = intervalDefault
    if interval == None: 
        # Night time
        if isNight and options['doPointNight']: 
            interval = options['intervalNight']

        # Day time
        if isDay and options['doPointDay']: 
            interval = options['intervalDay']

        # Transition
        if inTransition and options['doPointSunRiseSet'] and \
           (options['doPointNight'] or options['doPointDay']):
            if options['intervalTran'] <> None:
                interval = options['intervalTran']
            else:
                interval = min(interval, \
                               max(utils.DT_TRANSITION, utils.DT_TRANSITION+timeSunLimit))
                if isNight and options['doPointDay']:
                    interval = min(interval, options['intervalDay'])
                if isDay and options['doPointNight']:
                    interval = min(interval, options['intervalNight'])
        elif not inSameSunState and interval <> None:
           if options['doPointSunRiseSet']:
               interval = min(interval, utils.DT_TRANSITION)
               if options['intervalTran'] <> None:
                   interval = min(interval, options['intervalTran'])
               if isNight and options['doPointDay']:
                   interval = min(interval, options['intervalDay'])
               if isDay and options['doPointNight']:
                   interval = min(interval, options['intervalNight'])

        # Optical
        if options['doOptPoint'] and \
           (commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT) != 0) and \
           (commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) != 0):
            interval = options['intervalOpt']

    # If interval is set and there was no prior pointing, then point
    if interval <> None and not commands.s.getScriptBool(INDX_BOOL_LASTPOINT): 
        doRadioPoint = True

    # Check if radio pointing needs to be done based on position.
    # If the last pointing was on a source far away, then we have to repoint.
    if not doRadioPoint and interval <> None and \
       commands.s.getScriptString(INDX_STR_LASTPOINT_NAME) != '' and ref <> None:
        distance = utils.getDistanceSky(commands.s.getScriptString(INDX_STR_LASTPOINT_NAME), ref)
        if distance > maxsep:
            doRadioPoint = True
            forceDistance = True
            msg = 'last pointing source was ' + str('%.1f' % distance) \
                  + ' deg away'
            commands.trackMessage(msg, indent=1)

    # See if we need to point because we crossed transition
    if not doRadioPoint and interval <> None and not inSameSunState and \
       options['doPointSunRiseSet'] and \
       ((isDay and options['doPointDay']) or \
       (isNight and options['doPointNight'])) and \
       commands.s.getScriptBool(INDX_BOOL_LASTPOINT) and \
       utils.timeDifference(commands.lst(), commands.s.getScriptDouble(INDX_DBL_LASTPOINT), pos=True) >= interval:
        doRadioPoint = True
        msg = 'pointing ' + str('%.1f' % dtSunRiseSet) + ' hours'
        if abs(dtSunSet) < abs(dtSunRise):
            if dtSunSet > 0.0: 
                msg += ' after sunset'
            else:
                msg += ' before sunset'
        else:
            if dtSunRise > 0.0: 
                msg += ' after sunrise'
            else:
                msg += ' before sunrise'
        commands.trackMessage(msg,indent=1)

    # Check if pointing needs to be done time wise.
    # Check either agains optical or radio pointing
    tpoint = None
    if commands.s.getScriptBool(INDX_BOOL_LASTPOINT): 
         tpoint = commands.s.getScriptDouble(INDX_DBL_LASTPOINT)
    if not doRadioPoint and interval <> None and tpoint <> None:
        dtLastPoint = utils.timeDifference(commands.lst(), tpoint, pos=True)
        dtNextPoint = helpers.convertHms(interval) - dtLastPoint
        dtEndOfTrack = utils.timeRemaining(useTime=True)
        if dtNextPoint > 0.0: 
            commands.trackMessage("pointing in " + utils.dtString(dtNextPoint), indent=1)
        elif dtEndOfTrack <> None and dtEndOfTrack > 0 and \
             dtEndOfTrack < 0.25 and interval > dtEndOfTrack:
            commands.trackMessage("Skipping pointing since track will end in " + 
                         utils.dtString(dtEndOfTrack), indent=1)
        else:
            doRadioPoint = True
            commands.trackMessage("pointing last done " + 
                         utils.dtString(abs(dtLastPoint)) + " ago", indent=1)

    # Determine if mount offsets need to be reset to zero if pointing fails
    if not inSameSunState and \
       ((isNight and not options['doPointNight']) or
        (isDay and not options['doPointDay'])):
        resetMountOffsets = True

    # If we are doing optical pointing and the optical-radio vector has 
    # been measured, then use optical instead of radio
    if doOptPoint and not force:
        doOptPoint = doRadioPoint
        if commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) != 0: doRadioPoint = False

    # Do we need to force pointing
    if force and not doRadioPoint and not doOptPoint:
        doRadioPoint = True

    # Done
    return doRadioPoint, doOptPoint, forceDistance, resetMountOffsets, sunStateCurrent


def setOptPointOptions(options):
    """ Sets parameters for daytime optical pointing .

        Inputs: 
             options  - pointing dictionary

        Output:
             Returns faintest magnitude to point on.

             Also, the following script variables are set:
             ncoadd             : Number of images to coadd
             subtractBackground : Subtract sky background  (True/False)
             centroidLoopMax    : Maximum number of centroids per repeat
             minsamples         : minimum number of "good" samples required
    """

    # Override defaults
    if dict in [type(options)]:
        ncoadd = options['ncoadd']
        subtractBackground = options['subtract']
        maxmag = options['maxmag']
        centroidLoopMax = options['centroidLoopMax']
        minsamples      = None
    else:
        ncoadd = 1
        subtractBackground = False
        maxmag = 99.0
        centroidLoopMax = 16
        minsamples      = None

    # If daytime, set options
    if utils.isSunUp(-15.0):
        if ncoadd == None: ncoadd = 40
        if subtractBackground == None: subtractBackground = True
        if maxmag == None: maxmag = 3.0
    else:
        if ncoadd == None: ncoadd = 1
        if subtractBackground == None: subtractBackground = False
        if maxmag == None: maxmag = 5.0

    # Set script variables
    commands.s.setScriptInt(INDX_INT_NCOADD, ncoadd)
    commands.s.setScriptInt(INDX_INT_CENTROID_LOOP_MAX, centroidLoopMax)
    commands.s.setScriptInt(INDX_BOOL_SUBTRACT_BKG, subtractBackground)
    t = minsamples
    if t == None: t = 0
    commands.s.setScriptInt(INDX_INT_MINSAMPLES, t)

    # Done
    return maxmag


def measureOptradVector(pointing, correlator, force=False, preferredonly=True):
    """ Measures the optical-radio pointing vector, if needed. 

        pointing  : pointing options
        correlator: correlator options
        force     : If True, then force pointing
    """
    # Measure the vector
    if force or \
       (pointing['doOptPoint'] and commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT) \
        and not commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) and
        (not preferredonly or pointing['optradPreferred'] <> None)):
        print 'Measuring optical-radio pointing vector'
        doPoint(pointing, preferred=pointing['optradPreferred'], 
                tune95Pref=pointing['optradTune95'], correlator=correlator,
                preferredonly=preferredonly)

def createPointingDictionary():
    """ Create pointing dictionary needed for optical pointing """
    # Create
    pointing = dict()

    # Initialize
    pointing = {
           'doPointNight' :  True, # If True, then perform pointing during nightime
           'doPointDay'   :  True, # If True, then perform pointing during daytime
           'doOptPoint'   :  True, # If True, then perform optical instead of radio pointing
           'intervalNight':   4.0, # [hours]  How often to point during nighttime
           'intervalDay'  :   2.0, # [hours]  How often to point during daytime
           'intervalOpt'  :   1.0, # [hours]  How often to perform optical pointing
           'minflux'      :  2.00, # [Jy]     Minimum pointing flux
           'maxsep'       :    50, # [degree] Maximum distance from science target
           'preferred'    :  None, # Preferred pointing source
           'nrepInt'      :     1, # Number of repeat observations per position
           'mapPoints'    :     5, # Make a strip in az/el with mapPoints positions
           'tune95'       : False, # Tune to 95 GHz before pointing
           'doPointSunRiseSet'  :  True, # If True, point immediately after sunrise/sunset
    }

    # Other hidden parameters
    pointint['ambient'] = False
    pointint['elmin']   = 20.0
    pointint['elmax']   = 80.0
    pointint['antwait'] = -2
    pointing['nrepCross'] = utils.getVariable(pointing,'nrepCross',1,nonone=True)
    pointing['tune95']    = utils.getVariable(pointing,'tune95',True,nonone=True)
    pointing['tune95Pref'] = utils.getVariable(pointing,'tune95Pref',pointing['tune95'],nonone=True)
    pointing['intervalTran'] = utils.getVariable(pointing,'intervalTran',None)
    pointing['doPointSunRiseSet'] = pointing['doPointNight'] or pointing['doPointDay']

    # Hidden variables
    setHiddenOpticalPointing(pointing)

    # Done
    return pointing

def optPoint(source, pointing=None, force=True):
    """ Perform optical pointing using default parameters.

        source  : Name of source to point near
        pointing: pointing dictionary
        force   : force pointing regardless when it was done last
    """

    # Define pointing dictionary if it does not exist
    if pointing == None: pointing = createPointingDictionary()

    # Point 
    doPoint(pointing, ref=source, force=force)


def doPoint(options,             ref=None,         correlator=None, 
            crossTerminus=None,  printHeader=True, start=False, 
            force=False,         maxsep=None,      preferred=None,   
            interval=None,       indent=1,         tune95Pref=None,
            elmin=None,          elmax=None,       preferredonly=False):
    """ Perform radio pointing on a source with an elevation greater than the
        system limit and below elmax. The pointing source is first
        searched in "preferred", and if no suitable sources are found, then in
        sourceList. The observed pointing source is chosen based upon:
           1) If ref is set, the source closest to "ref"
           2) If ref is not set, the sources highest in the sky.

        sourceList is currently set to the bright quasars
         3c84, 0530+135, 2232+117, 3c454.3, 3c279, 3c273, 3c345, 1733-130

        The routine then selects a nearby optical source subject to a
        maximum magnitude limit (maxMag) and a maximum source
        separation (maxOptSep).  In general, a source is always
        present within 10 degrees on the sky and this limit does not
        need to be exercised.

        Inputs: options     - pointing options; see template2.obs
                ref         - Name of reference source
                correlator  - correlator options for calibration observations;
                              see template2.obs
                start       - If True, this is the initial pointing observation
                              for the track. This is used only in 
                              conjunction with options['start'].
                crossTerminus - If True, then it is ok to select a pointing
                              source that crosses the north/south terminus.
                              Default value is None, and the value of crossTerminus is
                              taken from the options dictionary. OBSOLETE.
                printHeader - Print message with indent=0
                force       - If true, then force pointing regardless of
                              time limits or other criteria.
                maxsep      - If set, it overrides the value of options['maxsep']
                tune95Pref  - If set, it overrides the value of options['tune95Pref']
                elmin       - If set, it overrides the value of options['elmin']
                elmax       - If set, it overrides the value of options['elmax']
                preferredonly - If True, then only point on source it if is
                                in the preferred list.
        Output: True/False  - If RADIO pointing was attempted
    """
    # Proceed?
    if utils.isEndOfTrack(): return False

    # Set parameters that can override 'options'
    #    1) maxsep        - maximum separation of pointing and science sources
    #    2) preferred     - preferred pointing source
    #    3) checkPointing - does pointing need to be checked at all?
    if maxsep    == None: maxsep    = options['maxsep']
    if preferred == None: preferred = options['preferred']
    isDoPointSet = options['doPointNight'] or options['doPointDay'] or \
                    options['doPointSunRiseSet'] or force

    # See if we need to check radio pointing
    if not force:
        if not isDoPointSet: return False
        if start and not options['start']: return False

    # Message
    if printHeader: commands.trackMessage("Checking pointing")

    # Do we need pointing?
    doRadioPoint, doOptPoint, forceDistance, resetMountOffsets, sunStateCurrent = \
             needPointing(options, ref, interval, maxsep, force)
    if resetMountOffsets: 
        utils.zeroPointingOffsets()
        utils.setLstLastPoint(None, sunStateCurrent)
        commands.s.setScriptString(INDX_STR_LASTPOINT_NAME, '')

    # Pointing sources to exclude unless they are in the preferred list
    excludeExpref = ['W3OH']

    # Set tune95 variable
    lofreq = commands.freqSetup()[2]
    tune95Point = options['tune95']
    if tune95Point: lofreq= 99

    # Get closest radio pointing source on the sky, if needed
    source = None
    if doRadioPoint:
        # Get possible radio pointing sources
        sourceList = utils.getBrightSources(options['minflux'], freq=lofreq)
        if preferredonly: sourceList = preferred

        # Get elevation limits
        xelmin = options['elmin']
        xelmax = options['elmax']
        if doOptPoint: 
            xelmin = options['optradElmin']
            xelmax = options['optradElmax']
        if elmin <> None: xelmin = elmin
        if elmax <> None: xelmax = elmax

        # Get source within elevation limits
        source = utils.getSource(sourceList, preferred=preferred, ref=ref, 
                           elmin=xelmin, elmax=xelmax, 
                           maxsep=maxsep, excludeExpref=excludeExpref)

        # If source is preferred source, check tune95Pref.
        # If no source was found, then print message
        if source == None:
            # Print message
            commands.trackMessage("no radio pointing source available", indent=indent)

            # If pointing was not successful but the previous pointing was on
            # a "bad" part of the sky, we must zero out the pointing offsets
            if forceDistance: utils.zeroPointingOffsets(bima=True)
        elif preferred <> None:
            # Make list for preferred sources
            spref   = utils.makeListObsdef(preferred)
            t95pref = options['tune95Pref']
            if tune95Pref <> None: t95pref = tune95Pref
            t95pref = utils.makeListObsdef(t95pref)

            # Loop over preferred sources
            if t95pref <> None:
                for i in range(len(spref)):
                    if spref[i].upper() == source.upper():
                        tune95Point = t95pref[min(i, len(t95pref)-1)]
                        break

    # Point
    if doRadioPoint and source <> None:
        # Start SZA radio pointing
        runPacs = commands.s.getScriptBool(INDX_BOOL_PACS)
        if runPacs: 
           commands.trackMessage('Starting SZA radio pointing', indent=indent)
           pacs.point()

        # Message
        ae = commands.azel(source)
        if tune95Point: 
            commands.trackMessage('tuning to 3mm for pointing',indent=indent)
        msg = 'pointing on ' + source + ' az=' + str('%.1f' % ae[0]) + \
                  '  el=' + str('%.1f' % ae[1]) + \
                  '  npos= ' + str('%2d' % options['mapPoints']) + \
                  '  nrepInt= ' + str('%2d' % options['nrepInt'])
        commands.trackMessage(msg, indent=indent)

        # Issue track command. Wait a few seconds for all antennas to arrive
        commands.track(source, waiton=options['antwait'])
        commands.track(source, waiton=commands.ALL, tmo=EXTRAWAIT)

        # Loop over crosses
        for i in range(options['nrepCross']):
            # Check receiver tuning
            utils.relockReceivers(indent=indent)

            # Print message
            jindent = indent
            if options['nrepCross'] > 1: 
                jindent += 1
                commands.trackMessage('Cycle ' + str(i+1), indent=indent)

            # Point
            results = None
            if doOptPoint:
                # Get nearest optical pointing source
                maxmag = setOptPointOptions(options)
                sourceNearest = utils.getSourceName(source, n=1, parse=True)
                [optSrc] = commands.getNearest(sourceNearest, 
                              getOptical=True, fluxLimit=maxmag, 
                              elMin=xelmin, elMax=xelmax)
                opticalSource = optSrc.name
                optSep        = optSrc.distance
                optMag        = optSrc.brightness

                # Determine optical-radio pointing vector
                # [optRes, badOptAnts, results] = fakeOpticalPointing()
                [optRes, badOptAnts, results] = \
                    ogm.deriveOptRadDiff(opticalSource, source,
                        min(commands.currentAntennaNumbers()), refPointReps=1,
                        refConverge=0.05, antVec=None, applyRadio=True, 
                        zoomVal=4.0, mapPoints=options['mapPoints'], 
                        intRep=options['nrepInt'], tune95=tune95Point,
                        antwait=options['antwait'])

                # Save results in dictionary for later use
                options['optRes']     = optRes
                options['badOptAnts'] = badOptAnts

                # Save results in script string for efficient restart
                badants = utils.list2string(options['badOptAnts'])
                sopt = ''
                for key in optRes:
                    if key not in badOptAnts:
                        sopt += ' ' + str(key) + ' ' + str(optRes[key][0]) + ' ' + \
                               str(optRes[key][1])
                commands.s.setScriptString(INDX_STR_OPT_BADANTS, badants)
                commands.s.setScriptString(INDX_STR_OPT_RES, sopt.strip())
            else:
                print "Begin refPoint"
                results = refPoint.refPoint(source, mapPoints=options['mapPoints'],
                     intRep=options['nrepInt'], tune95=tune95Point,
                     antwait=options['antwait'])
                print "Radio pointing is complete"
            # Reset IF levels
            if correlator <> None: setIFLevels(correlator, donoise=False)

            # Print pointing results
            if dict not in [type(results)] :
                commands.trackMessage('WARNING: Radio pointing may have failed', 
                      indent=jindent)
            else:
                # Radio pointing results
                commands.trackMessage('Antenna    Pointing Change      Mount offset',indent=jindent)
                commands.trackMessage('              (arcsec)            (arcmin)',indent=jindent)
                commands.trackMessage('              Az      El          Az       El',indent=jindent)
                commands.trackMessage('-------    ---------------    ----------------',indent=jindent)
                ants = commands.currentAntennaNumbers()
                badlist = list()
                for i in ants:
                    msg = str('  %2d    ' % i)
                    if results.has_key(i):
                        msg += '   ' + str('%6.1f' % (results[i][0] * 60.0)) + \
                               '  ' + str('%6.1f' % (results[i][1] * 60.0)) + \
                               '     ' + str('%7.3f' % results[i][2]) + \
                               '  ' + str('%7.3f' % results[i][3])
                    else:
                        msg += '     *** Pointing did not converge ***'
                        if i >= 7: badlist.append(i)
                    commands.trackMessage(msg, indent=jindent)
                if forceDistance and len(badlist) > 0: 
                    utils.zeroPointingOffsets(badlist)

                # Optical pointing results
                if doOptPoint:
                    goodVals = list()
                    msg = 'Optical pointing on %s (V=%.2f mag)' % (opticalSource, optMag)
                    commands.trackMessage(msg, indent=jindent)
                    for i in commands.currentAntennaNumbers() :
                        if i not in options['badOptAnts'] : goodVals.append(i)
                    if len(goodVals) == 0 :
                        commands.trackMessage("WARNING: No valid optical pointing values were reported.",indent=jindent)
                    else : 
                        commands.trackMessage('Antenna    Optical Radio Difference',indent=jindent)
                        commands.trackMessage('                  (arcsec)',indent=jindent)
                        commands.trackMessage('                Az       El',indent=jindent)
                        commands.trackMessage('-------      ---------------',indent=jindent)
                        for i in commands.currentAntennaNumbers():
                            msg = str('  %2d    ' % i)
                            if options['optRes'].has_key(i) and i not in options['badOptAnts']:
                                msg += '   ' + str('%7.1f' % (60.0*options['optRes'][i][0])) + \
                                       '  ' + str('%7.1f' % (60.0*options['optRes'][i][1])) 
                            else:
                                msg += '      *** Optical-radio difference not measured ***'
                            commands.trackMessage(msg,indent=jindent)            

            # If re-tuned to 95 GHz, then must reset IF
            #if tune95Point and correlator <> None: 
            #    setIFLevels(correlator, donoise=False)

            # Store time of last pointing and source name
            utils.setLastPointing(source, sunStateCurrent)
            if doOptPoint:
                doOptPoint = False
                optPointOk = len(options['badOptAnts']) <= options['nbadAnts']
                if optPointOk:
                    commands.trackMessage('Will use optical instead of radio pointing', indent=indent)
                else:
                    commands.trackMessage('Too few antennas converged - will continue with radio pointing', indent=indent)
                commands.s.setScriptInt(INDX_INT_NFAIL_OPT, 0)
                commands.s.setScriptBool(INDX_BOOL_OPTRAD_VECTOR, optPointOk)
                commands.s.setScriptDouble(INDX_DBL_LAST_OPTRAD, commands.lst())
                commands.s.setScriptInt(INDX_INT_NFAIL_OPT, 0)

    # Check optical pointing
    if ref <> None and doOptPoint and \
       commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT) != 0 and \
       commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) != 0:
        stmp = utils.makeListObsdef(ref)[0]
        updatePointingOptical(options, stmp, sunStateCurrent)

    # Done
    return (doRadioPoint and source <> None)


def updatePointingOptical(pointing, source, sunStateCurrent, 
      indent=1) :
    """ Run optical offset pointing/

        pointing       : Pointing options in observing template
        source         : Reference source.
        sunStateCurrent: Current state of the sun. These are hardcoded 
                         integers in obsdef2.py. Possible values are:
                               SUNSTATE_DAY
                               SUNSTATE_NIGHT
                               SUNSTATE_TRANS
    """
    # Set elevation limits
    elmin = utils.getElevationLimit()
    elmax = pointing['elmax']

    # Get pointing source
    maxmag = setOptPointOptions(pointing)
    sourceNearest = utils.getSourceName(source, n=1, parse=True)
    [optSrc] = commands.getNearest(sourceNearest, getOptical=True, 
          fluxLimit=maxmag, elMin=elmin, elMax=elmax)
    opticalSource = optSrc.name
    optSep        = optSrc.distance
    optMag        = optSrc.brightness

    # Determine offsets
    #applyAntVec, applyAzOff,applyElOff = fakeSubOptical(pointing['badOptAnts'])
    [azVec,azOff,elVec,elOff,applyAzOff,applyElOff,applyAntVec] = \
           ogm.subOptical(pointing['optRes'], opticalSource, 
                      badOptical=pointing['badOptAnts'])

    # Print results. Determine number of stars with identically zero offsets.
    # These are bad offsets that will not be applied
    nbad = 0
    sep = utils.getDistanceSky(opticalSource, source)
    msg = 'Pointed on %s (V=%.2f mag) : %.2f deg from %s' % \
             (opticalSource, optMag, sep, source)
    commands.trackMessage(msg, indent=indent)
    commands.trackMessage('Antenna      Pointing Update',indent=indent)
    commands.trackMessage('                  (arcsec)',  indent=indent)
    commands.trackMessage('                Az       El', indent=indent)
    commands.trackMessage('-------      ---------------',indent=indent)
    for i in commands.currentAntennaNumbers():
        if i in applyAntVec:
            k = applyAntVec.index(i)
            if applyAzOff[k] == 0 and applyElOff[k] == 0: 
                nbad += 1
                commands.trackMessage('  %2d       %16s' % (i,"*** Did not converge ***"), indent=indent)
            else:
                commands.trackMessage('  %2d       %7.1f  %7.1f' % (i,applyAzOff[k]*60.0,applyElOff[k]*60.0), indent=indent)
        else:
            commands.trackMessage('  %2d       %16s' % (i,"*** No optical-radio vector ***"), indent=indent)

    # Did pointing succeed?
    nbad_tot = nbad + len(pointing['badOptAnts'])
    if nbad > 0 and nbad_tot >= pointing['nbadAnts']:
        nfail = commands.s.getScriptInt(INDX_INT_NFAIL_OPT) + 1
        commands.s.setScriptInt(INDX_INT_NFAIL_OPT, nfail)
        commands.trackMessage('Optical pointing failed for ' + str(nbad_tot) + ' antennas', indent=indent)
        if nfail >= pointing['nfailOpt']:
            commands.trackMessage('Optical pointing has failed ' + str(nfail) + ' consecutive times.', indent=indent)
            commands.trackMessage('Resuming standard radio pointing mode', indent=indent)
            commands.s.setScriptBool(INDX_BOOL_OPTRAD_VECTOR, False)
    else:
        commands.s.setScriptInt(INDX_INT_NFAIL_OPT, 0)
        utils.setLastPointing(opticalSource, sunStateCurrent)


def doPassband(options,          correlator,      noise, 
               pointing=None,    lststop=None,    ref=None, 
               maximumTime=None, force=False,    printHeader=True, 
               start=False):
    """ Observe a passband calibrator with an elevation greater than the
        system limit and below elmax. The passband calibrator is first
        searched in "preferred", and if no suitable sources are found, then in
        sourceList. The observed passband source is chosen based upon:
           1) If ref is set, the source closest to "ref"
           2) If ref is not set, the sources highest in the sky.

        Inputs: options     - passband options; see template2.obs
                correlator  - correlator options; see template2.obs
                noise       - noise source options; see template2.obs
                pointing    - pointing dictionary
                lststop     - repeat observations until lststop is reached.
                              This is a string variable of the form HH:MM:SS.
                              If lststop is not set, then observe source once.
                ref         - Name of reference source
                start       - If True, this is the initial passband observation
                              for the track. This is used only in 
                              conjunction with options['pbstart'].
                printHeader - Print message with indent=0
                force       - If true, then force passbnad regardless of
                              time limits or other criteria.
                maximumTime - The maximum time in hours to spend on
                              passband observations.
         Outputs: True or False, indicating whether or not a passband
                  calibrator was observed.
    """
    # Proceed?
    if utils.isEndOfTrack(): return False

    # See if we need to observe a passband calibrator
    if force:
        pass
    elif start and not options['start']:
        return False
    elif finishedPassband(options):
        return False

    # Initialize
    ok = False
    if printHeader: commands.trackMessage("Passband calibration")

    # Check if calibrator needs to be done time wise
    tpbcal = utils.getLstLastPassband()
    if not force and tpbcal <> None and options['interval'] <> None:
        dtLastPassband = utils.timeDifference(commands.lst(), tpbcal, pos=True)
        dtNextPassband = helpers.convertHms(options['interval']) - dtLastPassband
        if dtNextPassband > 0.0: 
            commands.trackMessage("passband calibration in " + utils.dtString(dtNextPassband), indent=1)
            return False
        else:
            commands.trackMessage("passband calibration last done " + 
                         utils.dtString(abs(dtLastPassband)) + " ago", indent=1)

    # Passband calibrators to avoid
    # BLLAC/0355+508/0212+735: Contain spectral lines in some velocities. 
    #                 Lucas & Liszt 1994, A&A, 282, L5
    # 0355+508 = 0359+509 = NRA150 
    # 0212+735 = 0212+735
    # W3OH     - not sure why it is even in the catalog
    excludeExpref = ['BLLAC', '2202+422','2200+420',
               '0355+508', 'NRA150', '0359+509', 
               '0212+735', '0217+738', 'W3OH'
               ]

    # Get passband calibrator
    lofreq = commands.freqSetup()[2]
    sourceList  = utils.getBrightSources(options['minflux'], freq=lofreq)
    passbandCal = utils.getSource(sourceList, preferred=options['preferred'], ref=ref,
                            elmin=options['elmin'], elmax=options['elmax'],
                            excludeExpref=excludeExpref, 
                            timeAvailable=options['tint'])
    if passbandCal == None: 
        commands.trackMessage("No passband calibrator available", indent=1)
        return ok
    else:
        if printHeader: utils.printWeather()
        commands.trackMessage("slewing to " + passbandCal + " for passband", indent=1)

    # Point
    if pointing <> None and (options['doPoint'] or options['forcePoint']):
        doPoint(pointing, ref=passbandCal, correlator=correlator, 
                maxsep=options['maxsep'], force=options['forcePoint'], 
                preferred=options['preferredPointing'], printHeader=False)

    # Observe passband calibrator in current correlator configuration
    # Set elevation limit to None since elevation limit was checked above
    # in selecting the source. This way, the observation will not be cut off
    # if it falls below the actual elevation limit during the integration.
    intentObject = 'B'
    selfcal = True
    ok = observeSource(passbandCal, options['tint'], options['record'],
                       tsysTime=options['tsys'], lststop=lststop, 
                       maximumTime=maximumTime, noise=noise,
                       tmo=options['tmo'], elmin=None,
                       antwait=options['antwait'], extraWait=EXTRAWAIT,
                       intentObject=intentObject, selfcal=selfcal,
                       ambient=options['ambient'], runMiriad=True,
                       combine=options['combine'])

    # If performing hybrid observations, we do no want to reset the
    # correlator between observations to improve the observing efficiency.
    # This behavior is controlled through origConfig.
    origConfig = None
    if correlator['hybrid'] <> None: origConfig = utils.getConfigBand()

    # Observe passband in reconfigured correlator
    if ok and correlator['configCal'] <> None:
        ok = ok and observeSource(passbandCal, options['tint'], 
                      options['record'], tsysTime=options['tsys'], 
                      lststop=lststop, maximumTime=maximumTime, 
                      reconfig=correlator['configCal'], noise=noise,
                      tmo=options['tmo'], origConfig=origConfig, 
                      elmin=options['elmin'], antwait=options['antwait'],
                      extraWait=EXTRAWAIT, 
                      intentObject=intentObject, selfcal=selfcal,
                      ambient=options['ambient'],
                      combine=options['combine'])

    # Observe hybrid passband modes
    if ok and correlator['hybrid'] <> None:
        for h in correlator['hybrid']:
            ok = ok and observeSource(passbandCal, correlator['tintHybrid'], 
                          options['record'], tsysTime=options['tsys'], 
                          lststop=lststop, maximumTime=maximumTime, 
                          reconfig=correlator['configCal'], hybrid=h,
                          noise=noise, tmo=options['tmo'], 
                          origConfig=origConfig, elmin=options['elmin'],
                          antwait=options['antwait'], extraWait=EXTRAWAIT,
                          intentObject=intentObject, selfcal=selfcal,
                          ambient=options['ambient'],
                          combine=options['combine'])

    # Reset correlator
    if origConfig <> None:
        commands.trackMessage("setting correlator for science observations", indent=1)
        utils.setConfigBand(origConfig)

    # Done. "ok" indicates if passband calibrator was observed.
    if ok:
        # commands.trackMessage("passband observations completed successfully", indent=1)
        commands.addScriptString(INDX_STR_PASSBAND, passbandCal,
                      bindx=INDX_BOOL_PASSBAND, delim=', ', allowDup=False)
        n = commands.s.getScriptInt(INDX_INT_NOBS_PB)
        commands.s.setScriptInt(INDX_INT_NOBS_PB, n + 1)
        utils.setLstLastPassband(commands.lst())
    else:
        if utils.isEndOfTrack():
            commands.trackMessage('Ending observations based on endtrack()')
        elif utils.isLastcal(): 
            commands.trackMessage('Ending cycle based on lastcal()')
        elif utils.isStopAfterNextCal(): 
            commands.trackMessage('Ending cycle based on stopAfterNextCal()')
        else:
            commands.trackMessage("WARNING: Passband observations may have failed", indent=1)
    return ok


def doFluxcal(options,      primaryCal,       noise, 
              correlator,   pointing=None,    ref=None, 
              lststop=None, printHeader=True, force=False, 
              start=False):
    """ Observe a flux calibrator with the elevation limits in fluxcal.
        The flux calibrator is first searched in "preferred", and if no 
        suitable sources are found, then in fluxSources. The observed flux 
        calibrator is chosen based upon:
           1) If ref is set, the source closest to "ref"
           2) If ref is not set, the sources highest in the sky.

        Inputs: options     - flux calibration options; see template2.obs
                primaryCal  - dictionary of primary flux calibrators;
                              see template2.obs
                ref         - Name of reference source
                correlator  - If set, correlator parameters for flux cal;
                              see template2.obs
                pointing    - pointing structure
                printHeader - Print message with indent=0
                force       - It True, then force calibrator to be observe
                              regardless of time crieria
                start       - If true, then this is the starting flux 
                              calibrator. This is used only in conjunction
                              with options['fluxstart']

         Output: True or False, indicating whether or not a flux
                 calibrator was observed successfully.
    """
    # Proceed?
    if utils.isEndOfTrack(): return False

    # See if we need to observe a flux calibrator
    if force:
        pass
    elif start and not options['start']:
        return False
    elif finishedFluxcal(options):
        return False

    # Set number of primary flux calibrators to observe this cycle
    ncal = options['ncal']
    if ncal == None: ncal = 1
    if ncal <= 0 and force: ncal = 1
    if ncal == 0: return False

    # Determine if we need to do primary calibration
    doPrimary = options['doPrimary']

    # Message
    tfluxcal = utils.getLstLastFluxcal()
    if printHeader: 
        commands.trackMessage("Flux calibration")
        utils.printWeather()

    # Check if calibrator needs to be done time wise
    if not force and tfluxcal <> None and options['interval'] <> None:
        dtLastFluxcal = utils.timeDifference(commands.lst(), tfluxcal, pos=True)
        dtNextFluxcal = helpers.convertHms(options['interval']) - dtLastFluxcal
        if dtNextFluxcal > 0.0: 
            commands.trackMessage("flux calibration in " + utils.dtString(dtNextFluxcal), indent=1)
            return False
        else:
            commands.trackMessage("flux calibration last done " + 
                         utils.dtString(abs(dtLastFluxcal)) + " ago", indent=1)

    # Are duplicates allowed?
    allowDup = True
    # allowDup = fluxParams['dupFluxcal']

    # Observe primary calibrators first
    nprimary = ncal
    if not doPrimary: nprimary = 0
    if force and nprimary == 0: nprimary = 1
    ok1, triedSource1, nobserved1 = False, False, 0
    if nprimary > 0:
        ok1, triedSource1, nobserved1 = observeFluxCalibrators(options, 
              primaryCal, nprimary, allowDup, correlator=correlator, 
              ref=ref, noise=noise, lststop=lststop, pointing=pointing)

    # Determine if we need to observe secondary calibration
    doSecondary = force or options['doSecondary']
    if not force and doSecondary and not options['doBoth']:
        doSecondary = not ok1 and needSecondaryFluxCalibrator(options)

    # Do secondary calibrator
    ok2, triedSource2, nobserved2 = False, False, 0
    if doSecondary: 
        # Set second flux calibrator list
        secondaryCalibrators = ['3c84', '0530+135', '0854+201', '0927+390', 
                                '3c273', '3c279', '3c345', '1751+096',
                                '2148+069', '3c446', '3c454.3']
        secondaryCalibrators = ['3c84', '3c273']

        # Observe secondary calibrators
        nsecondary = ncal - nobserved1
        if force or options['doBoth']: nsecondary = max(1,nsecondary)
        ok2, triedSource2, nobserved2 = observeFluxCalibrators(options, 
              secondaryCalibrators, nsecondary, allowDup, 
              correlator=correlator, ref=ref, noise=noise, lststop=lststop, 
              secondary=True, pointing=pointing)

    # Done
    triedSource = triedSource1 or triedSource2
    ok = ok1 or ok2
    if ok:
        n = commands.s.getScriptInt(INDX_INT_NOBS_FLUX)
        commands.s.setScriptInt(INDX_INT_NOBS_FLUX, n+1)

    # Print message if not successful
    if triedSource and not ok:
        if utils.isEndOfTrack():
            commands.trackMessage('Ending observations based on endtrack()')
        elif utils.isLastcal():
            commands.trackMessage('Ending cycle based on lastcal()')
        else:
            commands.trackMessage("Flux calibrator observations may have failed", indent=1)
    return ok


def observeFluxCalibrators(options,         sourceList,      nsources,    
                           allowDup,        correlator=None, ref=None,
                           noise=None,      secondary=False, lststop=None, 
                           pointing=None):
    """ Observe up to nsources in sourceList. 

        Inputs: options    - flux calibration options; see template2.obs
                sourceList - list of sources that are ok to observe
                nsources   - maximum number of sources to observe
                allowDup   - observe duplicate source from previous observation
                correlator - correlator options; see template2.obs
                ref        - name of reference source
                noise      - noise options; see template2.obs
                secondary  - If True, sourceList contains secondary flux
                             calibrators
                lststop    - ending lst time in hours to spend on flux cal
                pointing   - pointing structure
    """
    # Set indices
    indx_bool_flux = INDX_BOOL_FLUXPRIMARY
    indx_str_flux = INDX_STR_FLUXPRIMARY
    indx_int_nobs = INDX_INT_NOBS_FLUXPRI
    msg1 = 'Primary flux calibration'
    msg2 = 'No primary flux calibrator available'
    preferred = options['preferredPri']
    if secondary:
        indx_bool_flux = INDX_BOOL_FLUXSECONDARY
        indx_str_flux  = INDX_STR_FLUXSECONDARY
        indx_int_nobs  = INDX_INT_NOBS_FLUXSEC
        preferred = options['preferredSec']
        msg1 = 'Secondary flux calibration'
        msg2 = 'No secondary flux calibrator available'

    # Loop over calibrators
    ok = False
    triedSource = False
    observedOnce = list()
    nobserved = 0
    for n in range(nsources):
        # If dups are not allowed, get list of calibrators 
        # that cannot be observed.
        excludeSources = None
        if not allowDup:
            excludeSources = commands.s.getScriptString(indx_str_flux)
            if excludeSources == "":
                excludeSources = None
            else:
                excludeSources = excludeSources.split()
        if observedOnce <> None:
            if excludeSources == None: excludeSources = list()
            excludeSources.extend(observedOnce)
        if secondary:
            tmp = commands.s.getScriptString(INDX_STR_FLUXPRIMARY)
            if tmp != "": excludeSources.extend(tmp.split())

        # Get available flux calibrator
        sourceName = utils.getSource(sourceList, preferred=preferred,
                               elmin=options['elmin'], elmax=options['elmax'],
                               ref=ref, exclude=excludeSources,
                               timeAvailable=options['tint'])
        if sourceName <> None: 
            # Messages
            if not triedSource:
                commands.trackMessage(msg1, indent=1)
                triedSource = True
            commands.trackMessage("slewing to " + sourceName, indent=2)

            # Point
            if (options['doPoint'] or options['forcePoint']) and \
               pointing <> None:
                preferredPointing = options['preferredPointing']
                tmp = string.upper(sourceName)
                if preferredPointing == None and (tmp == "XURANUS" or \
                   tmp == "XMARS" or tmp == "XNEPTUNE"):
                    preferredPointing = sourceName
                doPoint(pointing, ref=sourceName, correlator=correlator, 
                       maxsep=options['maxsep'], force=options['forcePoint'],
                       preferred=preferredPointing, indent=2,
                       printHeader=False, interval=options['intervalPointing'])
                
            # Observe source
            okSource = observeSource(sourceName, options['tint'], 
                           options['record'], noise=noise, 
                           tsysTime=options['tsys'], antwait=options['antwait'],
                           reconfig=correlator['configCal'], lststop=lststop, 
                           tmo=options['tmo'], indent=2, extraWait=EXTRAWAIT,
                           intentObject='BF', selfcal=True,
                           ambient=options['ambient'], runMiriad=True,
                           combine=options['combine'])
            if okSource:
                utils.setLstLastFluxcal(commands.lst())
                ok = okSource
                nobserved += 1
                observedOnce.append(sourceName)
                commands.addScriptString(indx_str_flux, sourceName, delim=' ', 
                                allowDup=False, bindx=indx_bool_flux)
                nn = commands.s.getScriptInt(indx_int_nobs) 
                commands.s.setScriptInt(indx_int_nobs, nn + 1)
    if not triedSource: commands.trackMessage(msg2, indent=1)

    # Done
    return ok, triedSource, nobserved


def doSourcesPhasecal(sources,      fluxcal,   pointing,        passband,
                      correlator,   noise,     primaryCal=None, mosaic=None,
                      resetDoppler=None,       ncyclesMax=None):
    """ Cycle between phaseCal and sources.

        This function will cycle between phase calibrator and sources.
        phaseCal can be a list() that contains multiple phase calibrators
        and various start/stop times. This routine loops over the specified
        phase calibrators, and calls doSources for each calibrator in turn.

        Inputs: sources    - contains science targets and phase calibrators;
                             see template2.obs
                fluxcal    - flux calibrator options; see template2.obs
                pointing   - pointing options; see template2.obs
                passband   - passband options; see template2.obs
                correlator - correlator options; see template2.obs
                noise      - noise source options; see template2.obs
                primaryCal - list of primary flux calibrators; see template2.obs
                mosaic     - mosaic options; see template2.obs
                resetDoppler   - If true, then set the doppler command
                                 for each source in "sources"
                ncyclesMax - Maximum number of phase calibrator cycles to perform
         Notes of phaseCal:
            phaseCal is either a scalar containing a single source name,
            of a list containing source names and start/stop lst times.
            Examples:
                 Single phase calibrator:
                     phaseCal = '0530+135'
                     In this example, 0530+135 is the phase calibrator 
                     for the entire track.

                 Multiple phase calibrators
                     phasecal = [  '3c111, 02:00:00, 04:30:00',
                                   '0530+135, 04:30:00, 07:00:00'
                                ]
                     In this example, 3c111 is the phase calibrator 
                     between 2:: and 4:30 LST, and 0530+135 is the phase
                     calibrator between 4:30 and 7:00 LST.

         Output: time in hours spent in function
    """
    # Convert phase cal to list
    phase = utils.makeListObsdef(sources['phaseCal'], parse=False)
    if phase == None: phase = [None]

    # The obsdef script cannot accept time ranges longer than 12 hours 
    # because it cannot elegantly handle the 24 hour wrap in time.
    # Therefore, if the phase cal stop-start time is more than 12 hours,
    # I break the calibrator into pieces.
    phase_orig = phase
    phase = utils.splitPhaseCalList(phase_orig)

    # Get reference source if we need to observe calibrators
    srefCal = utils.getSourceName(sources['target'], n=1, parse=True)

    # Loop over phase calibrators
    for p in phase:
        # Read phase calibrator name, and optionally start/stop time
        pname, pstart, pstop, pcaltime = utils.getPhaseCal(p, returnCaltime=True)

        # Ready to go
        action = pname
        if action <> None: action = action.upper()
        if action == "BANDPASS" or action == "PASSBAND":
            if not finishedPassband(passband):
                doPassband(passband, correlator, noise, 
                           pointing=pointing, ref=srefCal)
        elif action == "FLUX" or action == "FLUXCAL":
            if not finishedFluxcal(fluxcal):
                doFluxcal(fluxcal, primaryCal, noise, correlator, pointing, ref=srefCal)
        elif action == "POINT":
            doPoint(pointing, ref=srefCal, correlator=correlator, force=True)
        elif pcaltime <> None and pcaltime > 0:
            if plststart == None or utils.timeDifference(lst(),plststart) > 0:
                commands.trackMessage('Observing extra source')
                observeSource(pname, pcaltime,
                   sources['record'], tsysTime=sources['tsys'], 
                   reconfig=correlator['configCal'], noise=noise,
                   lststop=utils.getLstStopCycle(default=pstop),
                   tmo=sources['tmo'], antwait=sources['antwait'],
                   intentObject='G', selfcal=True,
                   ambient=sources['ambient'], runMiriad=False,
                   combine=sources['combine'])
        elif sources['doSnapshot']:
            doSnapshot(sources, pname, pointing, passband, 
                  fluxcal, correlator, noise, primaryCal=primaryCal, 
                  pstop=pstop, resetDoppler=resetDoppler)
        elif utils.timeRemaining(source=sources['target'], phase=pname, 
                           lsttime=pstart, toRise=True) <= 0.0:
            doCycles(sources, pname, pointing, passband, fluxcal, mosaic,
                     correlator, noise, primaryCal=primaryCal, 
                     pstop=pstop, resetDoppler=resetDoppler, 
                     ncyclesMax=ncyclesMax)


def doCycles(sources,         phaseCal,     pointing,     passband, 
             fluxcal,         mosaic,       correlator,   noise, 
             primaryCal=None, pstop=None,   resetDoppler=False,
             ncyclesMax=None):
    """ Perform one cycle between phaseCal and sources.

        Inputs: sources    - contains science targets and phase calibrators;
                             see template2.obs
                phaseCal   - phase calibrator to use for this cycle
                fluxcal    - flux calibrator options; see template2.obs
                pointing   - pointing options; see template2.obs
                passband   - passband options; see template2.obs
                correlator - correlator options; see template2.obs
                noise      - noise source options; see template2.obs
                primaryCal - list of primary flux calibrators; see template2.obs
                mosaic     - mosaic options; see template2.obs
                resetDoppler   : If true, then set the doppler command
                                 for each source in "sources"
                pstop      - ending LST time for this phase calibrator

         Output: None
    """
    # Set overhead factor. 60 converts minutes to hours
    foverhead = utils.overhead() / 60.0

    # Nominal passband
    timePb = foverhead * passband['tint']

    # Add passband time if observing two correlator configurations
    if correlator['configCal'] <> None: timePb *= 2.0   

    # Add passband time if observing hybrid configurations
    if correlator['hybrid'] <> None: 
        timePb += foverhead * correlator['tintHybrid'] * len(correlator['hybrid'])

    # Flux calibrators
    timeFlux = foverhead * fluxcal['tint']

    # Last calibrator
    timeLastcal = foverhead * sources['tintPhaseCal']

    # Source observations
    sourceIntent = None
    if sources.has_key('intentTarget'): sourceIntent = sources['intentTarget']
    sou, t, intents = \
        utils.setSourceTintIntent(sources['target'], sources['tintTarget'], sourceIntent)
    if mosaic['doMosaic']:
        timeCycle = timeLastcal + \
                       foverhead * (t[0] * mosaic['nphase'] + sum(t[1:]))
    else:
        timeCycle = timeLastcal + foverhead * sum(t)

    # PACS sources
    runPacs = commands.s.getScriptBool(INDX_BOOL_PACS)
    souPacs = [None] * len(sou)
    phaseCalPacs = None
    if runPacs:
        pacsSources = sources['pacs']
        phaseCalPacs = sources['phaseCalPacs']
        if pacsSources == None:
           souPacs = sou
        else:
           souPacs = utils.makeListObsdef(pacsSources)
           if len(souPacs) < len(sou):
              sn = souPacs[len(souPacs)-1]
              for i in sou[len(souPacs):]:
                 souPacs.append(sn)

    # Initialize cycles
    okSource  = False
    continueCycles = not utils.isEndOfTrack()
    commands.s.setScriptBool(INDX_BOOL_LASTCAL,False)
    commands.s.setScriptBool(INDX_BOOL_STOPNEXTCAL,False)
    ncycle = commands.s.getScriptInt(INDX_INT_LASTCYCLE)

    # Start cycles
    while continueCycles :
        # Pick reference target to find pointing sources. One reference source 
        # is set for finding the phase calibrator (srefCal), and another for 
        # the target source (srefTarget).
        #
        # For radio pointing, srefCal is the target source, and srefTarget=None.
        #
        # For optical pointing, there can be different results depending on 
        # whether optical pointing is done only on the phase calibrator, or 
        # whether it is done the the phase calibrator and target source.
        srefCal = utils.getSourceName(sources['target'], n=1, parse=True)
        srefTarget = srefCal
        if commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) and phaseCal <> None and \
           pointing['optPointCal'] and pointing['optPointTarget']:
            srefCal = phaseCal

        # Compute time needed to complete remaining critical observations
        tneed = 0.0
        if not finishedPassband(passband) : tneed += timePb
        if not finishedFluxcal(fluxcal) : 
            tneed += timeFlux * getNfluxCal(fluxcal)

        # Message
        ncycle += 1
        tremaining = utils.timeRemaining(source=sou, phase=phaseCal, useTime=True,
                                   lsttime=utils.getLstStopCycle(default=pstop),
                                   tneed=tneed)
        if tremaining <= 0:
            commands.trackMessage('time limit reached on source cycles', indent=1)
            continueCycles = False
            continue
        elif ncyclesMax <> None and ncycle > ncyclesMax:
            commands.trackMessage('reached maximum number of cycles', indent=1)
            continueCycles = False
            continue
        msg = 'Cycle ' + str('%3d' % ncycle) + '     ' + \
              utils.dtString(tremaining) + ' remaining'
        commands.trackMessage(msg)
        commands.s.setScriptInt(INDX_INT_LASTCYCLE, ncycle)
        utils.printWeather()

        # Check receivers are still locked
        utils.relockReceivers(indent=1)

        # Check optical pointing. Radio pointing is bracketed by phase
        # calibrator observations. We do not do this for optical pointing.
        if commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) and \
           commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT) and \
           pointing['optPointCal'] and \
           (pointing['doPointNight'] or pointing['doPointDay']) and \
           utils.continueObservations(isScience=True) and \
           utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0:
           doPoint(pointing, ref=srefCal, 
                   correlator=correlator, printHeader=False)

        # Observe phase calibrator
        if phaseCal == None:
            okCal = True
        elif utils.continueObservations() and tremaining > 0:
            okCal = observeSource(phaseCal, sources['tintPhaseCal'], 
                         sources['record'], tsysTime=sources['tsys'], 
                         reconfig=correlator['configCal'], noise=noise,
                         lststop=utils.getLstStopCycle(default=pstop),
                         tmo=sources['tmo'], antwait=sources['antwait'],
                         intentObject='G', selfcal=True,
                         pacsSources=phaseCalPacs,
                         ambient=sources['ambient'], runMiriad=True,
                         combine=sources['combine'])
        if okCal: okSource = False
        if utils.isStopAfterNextCal():
            continueCycles = False
            continue

        # Check radio pointing. "pointed" only indicates if radio pointing 
        # was performed, not optical pointing.
        pointed = False
        if (not commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) or \
            not commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT)) and \
           (pointing['doPointNight'] or pointing['doPointDay']) and \
           utils.continueObservations(isScience=True) and \
           utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0:
            pointed = doPoint(pointing, ref=srefCal, 
                              correlator=correlator, printHeader=False)

        # Flux calibrator
        observedFluxcal = False
        if not finishedFluxcal(fluxcal) and fluxcal['middle'] and \
           utils.continueObservations(isScience=True) and \
           utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0:
            observedFluxcal = doFluxcal(fluxcal, primaryCal, noise, correlator,
                  pointing, ref=srefCal, lststop=pstop, printHeader=False)

        # Passband calibrator
        observedPassband = False
        if not finishedPassband(passband) and passband['middle'] and \
           utils.continueObservations(isScience=True) and \
           utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0:
            observedPassband = doPassband(passband, correlator, noise,
                pointing=pointing, ref=srefCal, lststop=pstop, 
                printHeader=False)

        # Observe source if we did not point and did not observe flux/pb cal
        #   -- nsources indicates the number of successful source observations
        #      that were completed THIS CYCLE.
        #   -- okSource indicates if a source has been successfull observed
        #      since the LAST completed phase calibrator.
        # This distinction is necessary since if no sources are observed in
        # this cycle, then the loop stops. However, we may need to observe
        # the phase calibrator one last time.
        tneed = 0.0
        if phaseCal <> None : tneed += timeLastcal
        if not finishedPassband(passband) : tneed += timePb
        if not finishedFluxcal(fluxcal) : 
            tneed += timeFlux * getNfluxCal(fluxcal)
        # okSource = False
        nsources = 0
        tremaining = utils.timeRemaining(source=sou, phase=phaseCal, useTime=True,
                                   lsttime=utils.getLstStopCycle(default=pstop),
                                   tneed=tneed)
        if not pointed and not observedFluxcal and not observedPassband and \
           utils.continueObservations(isScience=True) and tremaining > 0.0:
            # Check optical pointing. Keep track of time spent pointing
            # and subtract from tremaining
            t1 = commands.lst()
            if commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) and \
               commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT) and \
               pointing['optPointTarget']:
                xt1 = commands.lst()
                doPoint(pointing, ref=srefTarget, 
                        correlator=correlator, printHeader=False)
            t1 = utils.timeDifference(commands.lst(), t1, pos=True)

            # Observe source
            tremaining -= t1
            mstart = commands.lst()
            if mosaic['doMosaic']:
                # By default, mosaic the first source, but not the second
                mosaic_list = [True, False]
                if mosaic.has_key('mosaic'): 
                    mosaic_list = utils.makeListObsdef(mosaic['mosaic'])
                # okSource = False
                nsources = 0
                for i in range(len(sou)):
                    # Default mosaic_source to the last entry in mosaic_list
                    mosaic_source = mosaic_list[len(mosaic_list)-1]
                    if i < len(mosaic_list): mosaic_source = mosaic_list[i]

                    # Update time remaining
                    tremaining = utils.timeRemaining(source=sou[i], phase=phaseCal,
                           useTime=True, lsttime=utils.getLstStopCycle(default=pstop),
                           tneed=tneed)

                    # Observe source
                    if mosaic_source:
                        ok = observeMosaic(sou[i], t[i], mosaic, 
                             maximumTime=tremaining, 
                             lststop=utils.getLstStopCycle(default=pstop),
                             pacsSources=souPacs[i], noise=noise,
                             combine=mosaic['combine'], intent=intents[i])
                    else:
                        ok = False
                        # Run noise source for pacs observations
                        obsnoise = None
                        if noise.has_key('tsample') and (noise['tsample'] <> None): 
                            obsnoise = noise
                        observeSource(sou[i], t[i], mosaic['record'], 
                            tsysTime=mosaic['tsys'], isScience=True, 
                            lststop=utils.getLstStopCycle(default=pstop),
                            maximumTime=tremaining, tmo=mosaic['tmoTrack'],
                            antwait=mosaic['antwait'], noise=obsnoise,
                            intentObject=intents[i], selfcal=False,
                            ambient=mosaic['ambient'], pacsSources=souPacs[i],
                            combine=mosaic['combine'])
                    okSource = okSource or ok
                    if ok: nsources += 1
            else:
                # Run noise source for pacs observations
                obsnoise = None
                if noise.has_key('tsample') and (noise['tsample'] <> None): 
                    obsnoise = noise
                okSource = observeSource(sources['target'], 
                      sources['tintTarget'], sources['record'],
                      isScience=True, lststop=utils.getLstStopCycle(default=pstop),
                      tsysTime=sources['tsys'], maximumTime=tremaining, 
                      tmo=sources['tmo'], resetDoppler=resetDoppler,
                      antwait=sources['antwait'], noise=obsnoise,
                      intentObject=intents, selfcal=False,
                      ambient=sources['ambient'], pacsSources=souPacs,
                      combine=sources['combine'])
                if okSource: nsources += 1
            utils.addSourceTime(utils.timeDifference(commands.lst(), mstart, pos=True))

        # Stop if nothing was observed or we have reached the time limit.
        # Also stop if we do not have time to finish remaining observations.
        tremaining = utils.timeRemaining(source=sou, phase=phaseCal, useTime=True,
                                   lsttime=utils.getLstStopCycle(default=pstop),
                                   tneed=tneed)
        if tremaining <= 0:
            if tneed > 0.0:
                msg = 'Stopping phase-cal cycle to finish remaining observations'
            else: 
                msg = 'Reached time limit on source/phase-calibrator'
            commands.trackMessage(msg)
            continueCycles = False
        elif not utils.continueObservations(isScience=True):
            continueCycles = False
        elif nsources == 0 and not pointed and \
             not observedPassband and not observedFluxcal:
            commands.trackMessage('No sources observed - exiting cycle')
            continueCycles = False

    # End cycle on phase calibrator
    if okSource and utils.continueObservations() and phaseCal <> None:
        commands.trackMessage('Observing ' + phaseCal + ' one last time')
        utils.relockReceivers(indent=1)
        observeSource(phaseCal, sources['tintPhaseCal'], sources['record'],
                     noise=noise, tsysTime=sources['tsys'], 
                     pacsSources=phaseCalPacs, isScience=False,
                     reconfig=correlator['configCal'], tmo=sources['tmo'],
                     antwait=sources['antwait'], intentObject='G', 
                     selfcal=True, ambient=sources['ambient'], runMiriad=True,
                     combine=sources['combine'])

    # See why we are here
    if utils.isEndOfTrack():
        commands.trackMessage('Ending observations based on endtrack()')
    elif utils.isLastcal(): 
        commands.trackMessage('Ending cycle based on lastcal()')
    elif utils.isStopAfterNextCal():
        commands.trackMessage('Ending cycle based on stopAfterNextCal()')


def observeSource(sources, tint,    trecord,             lststop=None,
                  tsysTime=None,    maximumTime=None,    isScience=False, 
                  reconfig=None,    tmo=None,            hybrid=None,
                  origConfig=None,  resetDoppler=False,  antwait=-2, 
                  elmin=None,       indent=1,            extraWait=0,
                  intentObject="O", selfcal=False,       returnTint=False,
                  ambient=False,    runMiriad=False,     runPacs=None,
                  pacsSources=None, noise=None,          combine=None):
    """ Observe "sources" for integration time "tint" minutes with a
        record time of "trecord" seconds. 
                                          
        Data are obtained only 
           1) the current lst is before lstStop
           2) source is above the elevation limit stored in the CARMA
              control system.
        The minimum integration time is given by obsdefUtils.minimumIntegrationtime().
        
        If noise != None,
        a noise source integration is obtained before integrating on sources

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
               noise      - Noise source dictionary
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
               antwait    - The "antwait" parameter for the integrate() command
               extraWait  - If > 0, then will wait an extra checkAnt seconds
                            to see if any more antennas come online. This
                            parameter is usually set for critical calibrator 
                            observations.
               intentObject- intent for the source(s)
               selfcal     - Is the source self-calibratable? (True/False)
               returnTint  - If true, function returns list actual integration
                             time in seconds
               ambient     - If true, insert the ambient load in the beam
                             before integrating.
               runMiriad   - If true, then run miriad gain check
               runPacs     - True/False : Run PACS commands. If None, then
                             the script variable INDX_BOOL_PACS is used.
               pacsSources - Source to use for PACS. If None, then use sources.
               combine     - Number of 0.5 records to combine for sza
 
        Output: Boolean value indicating if source was observed.
                If returnTint = True, returns instead actual integration
                time in seconds.

        Note: Since tint, trecord, and tsysTime may not be divisible into
              integers, tsysTime is only approximate.
    """
    # Check which antennas are online
    utils.checkAntennas(indent=indent)

    # Keep track if a source was observed in this cycle
    tstart = commands.lst()
    observedSource = False
    if not utils.continueObservations(isScience=isScience): return observedSource

    # Put inputs in a list
    sou, integTime, intents = utils.setSourceTintIntent(sources, tint, intentObject)
 
    # Are we running pacs?
    if runPacs == None: runPacs = commands.s.getScriptBool(INDX_BOOL_PACS)

    # Make a list of pacs sources
    souPacs = [None] * len(sou)
    if runPacs:
        if pacsSources == None:
           souPacs = sou
        else:
           souPacs = utils.makeListObsdef(pacsSources)
           if len(souPacs) < len(sou):
              for s in sou[len(souPacs):]:
                 souPacs.append(s)

    # Set original correlator configuration, if needed.
    # I tried using copy.deepcopy(), but that did not work in terms 
    # of perserving the BW and LSB/USB types.
    if origConfig == None:
        specConfig = utils.getConfigBand()
    else:
        specConfig = list()
        for i in origConfig: specConfig.append(i[:])

    # Reset correlator configuration. Only copy the parameters here, and
    # set it after looking at hybrid
    # I tried using copy.deepcopy(), but that did not work in terms of 
    # perserving the BW and LSB/USB types.
    contConfig = None
    if reconfig <> None: 
        contConfig = list()
        for i in reconfig: contConfig.append(i[:])

    # Change bandwidth for hybrid correlator mode if needed
    # The IF frequency is determined as follows:
    #      1) if BW=500, IF frequency given by reconfig (if set)
    #      2) if BW<500, IF frequency given by config
    if hybrid <> None:
        contConfig = utils.getHybridConfiguration(hybrid, specConfig, contConfig)

    # Change correlator
    if reconfig <> None or hybrid <> None:
        msg = "setting correlator for calibrator observations"
        if hybrid <> None:
            msg = "setting correlator for hybrid observations"
        commands.trackMessage(msg, indent=indent)
        utils.setConfigBand(contConfig)

    # Loop over sources
    tint_actual  = 0.0
    tint_maximum = 0.0
    tintSinceLastTsys = None
    tintSinceLastNoise = None
    sourcePrevious = None
    for isource in range(len(sou)):
        # Check to see if source is up and whether this is the noise source
        isUp = False
        isNoiseSource = False
        name = sou[isource]
        if string.lower(name) == 'noise': 
            isUp = True
            isNoiseSource = True
        else:
            isUp = utils.isSourceUp(name, elmin=elmin)

        # Compute remaining time for this cycle
        tremaining = utils.timeRemaining(lsttime=lststop, source=sou, elmin=elmin)
        if maximumTime <> None: 
            tremaining = min(maximumTime - commands.lst() + tstart, tremaining)

        # Stop if ENDTRACK or LASTCAL is set
        if utils.isEndOfTrack() or (utils.isLastcal() and isScience): continue

        # Observe source if it is up
        if not isUp:
            commands.trackMessage(name + ' is below the elevation limit', indent=indent)
        elif tremaining == None or tremaining > 0.0:
            # Integration time should not exceed time remaining in track
            integrationTime = integTime[isource]
            if tremaining <> None:
                integrationTime = min(integrationTime, tremaining * 60.0)
            if integrationTime < utils.minimumIntegrationTime() / 60.0: 
                integrationTime = 0.0
            if integrationTime < 1.0 and \
               integrationTime <= 0.5*integTime[isource]: continue

            # Set number of repetitions and cycles to observe this source
            if isNoiseSource:
                nreps = utils.getNreps(integrationTime, trecord)
            else:
                tsampleNoise = None
                if noise <> None and noise.has_key('tsample'): 
                    tsampleNoise = noise['tsample']
                nreps = utils.getNreps(integrationTime, trecord, 
                              tsysMinutes=tsysTime, tsampleMinutes=tsampleNoise,
                              tintSinceLastTsys=tintSinceLastTsys,
                              tintSinceLastSample=tintSinceLastNoise)

            # Track astronomical source. 
            # Observe noise source first if noise tint != None.
            if not isNoiseSource: 
                # Reset doppler
                if resetDoppler: commands.doppler(sou[isource])

                # Track source for PACS
                if runPacs: 
                    commands.trackMessage(souPacs[isource] + ' (sza)', indent=indent)
                    intentPacs = string.upper(intents[isource])
                    if intentPacs == 'S' or intentPacs == 'O': intentPacs = 'A'
                    pacs.observe(souPacs[isource], intentPacs)

                # Noise source
                if noise <> None:
                    if runPacs: pacs.noise(combine)
                    commands.track(sou[isource])
                    utils.observeNoise(noise['tint'], noise['record'], 
                                verbose=True, indent=indent)
                    tintSinceLastNoise = 0

                # Track source - and wait for antennas to arrive
                commands.intent(sou[isource], intents[isource], selfcal)
                commands.track(sou[isource], waiton=antwait)
                if extraWait > 0: 
                    commands.track(sou[isource], waiton=commands.ALL, tmo=extraWait)

            # Loop over cycles
            nrepsList = utils.makeListObsdef(nreps)
            for j in range(len(nrepsList)):
                # Stop if ENDTRACK or LASTCAL is set
                if utils.isEndOfTrack() or (utils.isLastcal() and isScience): 
                   break

                n = nrepsList[j]
                if n > 0 and utils.continueObservations(isScience=isScience):
                    # Observe noise source?
                    if tsampleNoise <> None and \
                       tintSinceLastNoise >= tsampleNoise:
                        if runPacs: pacs.noise(combine)
                        utils.observeNoise(noise['tint'], noise['record'], 
                                    verbose=True, indent=indent)
                        tintSinceLastNoise = 0

                    # Print message on how long we are going to integrate
                    ttot = n * trecord / 60.0
                    t = str("%-10s" % sou[isource]) + '   tint=' + \
                        str("%9s" % utils.dtString(ttot/60.0)) + '  el=' + \
                        str("%4.1f" % utils.getSourceElevation(sou[isource]))
                    if resetDoppler: t += ' doppler reset'

                    # Indicate if tsys will be measured.
                    measureTsys = True
                    if tsysTime == None or tsysTime == 0 or isNoiseSource:
                        measureTsys = False
                    elif tsysTime < 0.0 and tintSinceLastTsys <> None and \
                         tintSinceLastTsys < abs(tsysTime):
                        measureTsys = False
                    elif tsampleNoise <> None and tintSinceLastTsys <> None \
                         and tsys > 0 and tintSinceLastTsys < tsysTime and \
                         sou[isource] == sourcePrevious:
                        measureTsys = False
                    if measureTsys:
                        t += ' (tsys)'
                    else:
                        t += ' (no tsys)'

                    # Message
                    commands.trackMessage(t, indent=indent)

                    # Measure tsys
                    if measureTsys:
                        if runPacs: pacs.tsys(combine)
                        commands.tsys()
                        tintSinceLastTsys = 0.0

                    # Integrate
                    sourcePrevious = sou[isource]
                    if ambient: commands.amb();
                    if isNoiseSource:
                        if runPacs: pacs.noise(combine)
                        trecnoise = trecord
                        if noise <> None: trecnoise = noise['record']
                        utils.observeNoise(trecord*n, trecnoise, verbose=True)
                        tintSinceLastNoise = 0
                    else:
                        commands.integrate(trecord, n, antwait=antwait, tmo=tmo)
                        tint_actual += trecord * n
                        tint_maximum += integTime[isource]
                        if tintSinceLastTsys <> None: 
                            tintSinceLastTsys += (trecord * n) / 60.0
                        if tintSinceLastNoise <> None: 
                            tintSinceLastNoise += (trecord * n) / 60.0
                    if ambient: commands.sky();

            # Run miriad
            if runMiriad and len(nrepsList) > 0:
                # Get reference antenna
                [ovroAnts, bimaAnts, szaAnts] = commands.antennasByType();
                refAnt = commands.currentAntennaNumbers()[0]
                if len(bimaAnts) > 0:
                    refAnt = bimaAnts[0]
                elif len(ovroAnts) > 0:
                    refAnt = ovroAnts[0]
                elif len(szaAnts) > 0:
                    refAnt = szaAnts[0]

                # Get miriad file
                miriadFile = '/home/obs/data/' + utils.getObsblockName() + '.mir'

                # Run script
                com = '/array/obs/bin/gainCheck.csh ' + \
                      'vis=' + miriadFile + ' ' + \
                      'refant=' + str(refAnt)
                if runPacs:
                    com += ' pacs=true ' + \
                        'ut1=' + commands.s.getScriptString(utils.INDX_STR_PACS_UT) + ' ' + \
                        'ut2=' + utils.getUT(sza=True) + ' ' + \
                        'config=' + commands.queryString(utils._controlMPprefix() + 'configName')
                tt = os.popen3(com)
                tt[0].close()
                tt[1].close()
                tt[2].close()

    # Change correlator back if reconfig/hybrid was set AND origConfg was not.
    if contConfig <> None and origConfig == None:
        commands.trackMessage("setting correlator for science observations", 
                     indent=indent)
        utils.setConfigBand(specConfig)

    # Did we integrate long enough to call this done?
    # I use 0.01 instead of 0 to avoid round off error
    if tint_actual > 0.01: observedSource = True
 
    # Reset doppler command
    if resetDoppler:
        commands.trackMessage('re-setting doppler tracking for source '+sou[0], 
                     indent=indent)
        commands.doppler(sou[0])

    # Done
    result = observedSource
    if returnTint: result = tint_actual
    return result


def observeMosaic(sources, tint, mosaic, lststop=None, maximumTime=None, 
                  pacsSources=None, noise=None, combine=None, intent='S'):
    """ Make mosaic

        Inputs: sources     - sources to observe
                tint        - integration times
                mosaic      - mosaic parameters
                noise       - noise parameters
                lststop     - stopping LST time for mosaic
                maximumTime - [hours] Maximum time to spend observing the mosaic
                combine     - Number of 0.5 records to combine for SZA

        Output: True if at least one positions were observed, False if not
    """
    # Initialize
    tstart = commands.lst()
    observedSource = False

    # Sou and integration time can only be scalar values
    sou, integTime = utils.setSourceTint(sources, tint)
    souMosaic = sou[0]
    integTimeMosaic = integTime[0]

    # PACS
    runPacs = commands.s.getScriptBool(INDX_BOOL_PACS)
    souPacs = [None] * len(sou)
    if runPacs:
        if pacsSources == None:
           souPacs = sou
        else:
           souPacs = utils.makeListObsdef(pacsSources)
           if len(souPacs) < len(sou):
              for s in sou[len(souPacs):]:
                 souPacs.append(s)

    # Compute how much time is needed to complete test calibrator sources
    tneed = sum(integTime[1:]) / 60.0 * utils.overhead()

    # Set mosaic offsets once. Offsets are stored in arcminutes 
    # within mosaic['offsetsArcmin'] keyword
    if not mosaic.has_key('offsetsArcmin'):
        # Read offsets
        if mosaic['offsets'] <> None:
            commands.trackMessage('setting mosaic offsets from input list in script',indent=1)
            mosaic['offsetsArcmin'] = mosaic['offsets']
        elif mosaic['offsetFile'] <> None:
            inputFile = utils.setMosaicFile(mosaic['offsetFile'])
            commands.trackMessage('reading mosaic offsets from '+inputFile,indent=1)
            mosaic['offsetsArcmin'] = fileIOPython.fileToTable(inputFile,ignoreEmpty=True,comment='#')
        else:
            raise Exception, 'Error specifying mosaic offsets'
        commands.trackMessage('mosaic contains ' + str(len(mosaic['offsetsArcmin'])) + 
              ' positions', indent=1)

        # Make sure offsets are a list
        if list not in [type(mosaic['offsetsArcmin'])]:
            raise Exception, 'Mosaic offsets must be a list'

        # Convert offsets into arcminutes, and change string to floating point
        zoff = mosaic['offsetsArcmin']
        stepSize = 1.0
        if not mosaic['arcminUnits']:
            stepSize = commands.getNyquistStep(min(commands.currentAntennaNumbers()))[0]
        for i in range(len(zoff)):
             x = float(zoff[i][0]) * stepSize
             y = float(zoff[i][1]) * stepSize
             zoff[i] = [x,y]
        mosaic['offsetsArcmin'] = zoff

    # Copy pointing offsets
    offsets = mosaic['offsetsArcmin']
    npointingCenters = len(offsets)

    # Set number of pointing centers to observe per cycle
    nposCycle = mosaic['nphase']
    if nposCycle <= 0: nposCycle = npointingCenters

    # Indicate how often to sample the noise source. Usually
    # only used for PACS observations in observeMosaic
    tsampleNoise = None
    if noise <> None and noise.has_key('tsample'): 
        tsampleNoise = noise['tsample']

    # Continue if source is up
    if utils.timeRemaining(source=sou, lsttime=lststop) > 0:
        # Track source
        if runPacs: pacs.observe(souPacs[0], 'A')
        commands.trackMessage('slewing to ' + souMosaic, indent=1)
        commands.intent(souMosaic, intent, False)
        commands.track(souMosaic)

        # Initialize cycle
        tintSinceLastTsys = None  # Integration time [minutes] since last tsys 
        tintSinceLastNoise = None # Integration time [minutes] since last noise
        nposObserved = 0          # Number of positions that have been observed
        observedSourceInCycle = True

        # Loop over mosaic positions. Measure tsys every tsysTime minutes
        while ((maximumTime == None or 
                (maximumTime - commands.lst() + tstart - tneed) > 0) and 
                nposObserved < nposCycle and observedSourceInCycle and
                utils.continueObservations(isScience=True)):
            # Initialize
            observedSourceInCycle = False

            # Noise measurement
            if tsampleNoise <> None and noise <> None and \
               (tintSinceLastNoise == None or 
                tintSinceLastNoise >= tsampleNoise):
                # commands.trackMessage('Observing noise source', indent=1)
                if runPacs: pacs.noise(combine)
                utils.observeNoise(noise['tint'], noise['record'], 
                                  verbose=True, indent=1)
                tintSinceLastNoise = 0.0
            elif tintSinceLastNoise == None:
                tintSinceLastNoise = 0

            # Tsys measurement
            if tintSinceLastTsys == None or \
               (mosaic['tsys'] <> None and 
                 tintSinceLastTsys >= abs(mosaic['tsys'])) :
                commands.trackMessage('Measuring tsys', indent=1)
                if runPacs: pacs.tsys(combine)
                commands.tsys()
                tintSinceLastTsys = 0.0

                # Off position, if needed
                if mosaic.has_key('off') and mosaic['off'] <> None:
                    tint = min(integTimeMosaic, 
                       utils.timeRemaining(source=souMosaic, lsttime=lststop) * 60)
                    nreps = utils.getNreps(tint, mosaic['record'], tsampleMinutes=tsampleNoise)
                    commands.pointstatus(commands.OFFSRC)
                    commands.trackMessage('Taking off-source integration', indent=1)
                    commands.equatOffset(mosaic['off'][0], mosaic['off'][1])
                    commands.integrate(mosaic['record'], nreps, antwait=mosaic['antwait'])
                    commands.pointstatus(commands.ONSRC)

            # Set integration time without exceeding time or elevation limits
            # However, never go less than one minute
            tint = min(integTimeMosaic, 
                       utils.timeRemaining(source=souMosaic, lsttime=lststop) * 60)
            if maximumTime <> None: 
                tint = min(tint, (maximumTime - commands.lst() + tstart) * 60.0)
            if tint < utils.minimumIntegrationTime() / 60.0: tint = 0.0
            if tint < 1.0 and tint <= 0.5*integTimeMosaic: continue
            if integTimeMosaic <= 1.0: tint = integTimeMosaic

            # Set mosaic position
            pos = max(1, utils.getLastMosaicPosition(mosaic['startpos']) + 1)
            if pos > npointingCenters: pos = 1

            # Print message
            t = str("%-10s" % souMosaic) + '   tint=' + \
                str("%9s" % utils.dtString(tint/60.0)) + '  el=' + \
                str("%4.1f" % utils.getSourceElevation(souMosaic)) + \
                '  Pos=' + str("%3d" % pos) + '  ' + \
                str("%6.3f" % offsets[pos-1][0]) + "' " + \
                str("%6.3f" % offsets[pos-1][1]) + "'"
            commands.trackMessage(t, indent=1)

            # Offset telescopes. pos starts at 1, while offsets are index=0.
            commands.equatOffset(offsets[pos-1][0], offsets[pos-1][1])

            # Integrate
            trecordMosaic = mosaic['record']
            if trecordMosaic > tint * 60.0: trecordMosaic = tint * 60.0
            nreps = utils.getNreps(tint, trecordMosaic)
            if nreps > 0:
                commands.integrate(trecordMosaic, nreps, tmo=mosaic['tmoMosaic'], 
                    antwait=mosaic['antwait'])
                observedSource = True
                tintSinceLastTsys  += nreps * trecordMosaic / 60.0
                tintSinceLastNoise += nreps * trecordMosaic / 60.0
                if nreps * trecordMosaic / 60.0 >= 0.5 * integTimeMosaic:
                    observedSourceInCycle = True
                    utils.setLastMosaicPosition(pos)
                    nposObserved += 1

    # Observe noise one last time if needed
    tsampleNoise = None
    if noise <> None and noise.has_key('tsample'): 
        tsampleNoise = noise['tsample']
    if tsampleNoise <> None:
        if runPacs: pacs.noise(mosaic['combine'])
        utils.observeNoise(noise['tint'], noise['record'], 
                           verbose=True, indent=1)
        tintSinceLastNoise = 0.0

    # Mosaic observations are finished, so now observe any extra sources
    for i in range(1,len(sou)):
        observeSource(sou[i], integTime[i], min(mosaic['record'], integTime[i]),
            tsysTime=mosaic['tsys'], lststop=lststop, 
            maximumTime=maximumTime, tmo=mosaic['tmoTrack'],
            antwait=mosaic['antwait'], intentObject='O', selfcal=False, 
            ambient=mosaic['ambient'], runPacs=runPacs, pacsSources=souPacs[i],
            noise=noise, combine=mosaic['combine'])

    return observedSource


def checkSnapshotSources(sources, exit=True):
    """ Makes sure the snapshot code exists and sources exist
        Inputs
            sources:  The usual sources dictionary
            exit   :  If true and no sources are found, then throw an exception
    """

    # Set database
    db = sn.DB_DEFAULT
    if sources.has_key('snapDb'): db = sources['snapDb']

    # First, make sure project code exists
    com = "select count(*) from projects where lower(project) = '" + sources["snapProject"].lower() + "'"
    result = sn.execdb(com, db=db)
    n = int(result[0][0])
    if n == 0:
         raise Exception,'The snapshot code ' + sources["snapProject"] + ' was not found in the snapshot database.\n Check the keyword sources["snapProject"] in the sources dictionary.'
    elif n > 1:
         raise Exception,'The snapshot code ' + sources["snapProject"] + ' was found multiple times in the snapshot database.\n Something is seriously wrong!' 

    # Project code exists, so now check that sources are available.
    com = "select count(*) " + \
          "from sources s,projects p " + \
          "where s.treq - s.tobs > 0.0 and s.pid=p.pid and " + \
          "lower(project) = '" + sources["snapProject"].lower() + "'" 
    result = sn.execdb(com, db=db)
    n = int(result[0][0])
    if n == 0:
         raise Exception,'No sources were found in the snapshot database for the snapshot code ' + sources["snapProject"]


def getNextSource(sources, tneed, pstop, phaseCalDefault):
    """ Returns the next source in the project 

        Inputs
            sources:  The usual sources dictionary
            tneed  :  time needed to perform remaining observations in hours
            pstop  :  LST stop time for the observations
            phaseCalDefault : default phase calibrator if none is specified

         Outputs
              [sou, primary, secondary, tint, sid]

              sou       : source name
              primary   : primary phase calibrator
              secondary : secondary phase calibrator
              tint      : on-source integration time remaining in minutes
              sid       : ID number if database
    """
    # Set database
    db = sn.DB_DEFAULT
    if sources.has_key('snapDb'): db = sources['snapDb']

    # Execute database command to find next sources
    com = "select s.sid,s.name,s.phasecal,s.secondcal,s.treq-s.tobs " + \
          "from sources s,projects p " + \
          "where s.treq-s.tobs > 0.0 and s.pid=p.pid and " + \
          "lower(project) = '" + sources["snapProject"].lower() + "'" + \
          " order by s.priority,s.sid"
    result = sn.execdb(com, db=db)

    # Loop over sources to find highest priority source that is still up
    sou = ''
    primaryCalibrator = ''
    secondaryCalibrator = ''
    tint = 0
    sid = 0
    for key, value in result.items():
        # Get source
        source = value[1]
        primaryCalibrator = value[2]
        secondaryCalibrator = value[3]
        phaseCal = phaseCalDefault
        if primaryCalibrator != "": phaseCal = primaryCalibrator

        # Check time remaining
        tremaining = utils.timeRemaining(source=source, phase=phaseCal, 
                       useTime=True, tneed=tneed,
                       lsttime=utils.getLstStopCycle(default=pstop))
        if tremaining <= 0.0: continue

        # Get source elevation
        el = utils.getSourceElevation(source)
        elcal = None
        if phaseCal <> None: elcal = utils.getSourceElevation(phaseCal)

        # Skip source if it is below system elevation limit
        if el    < utils.getElevationLimit(): continue
        if elcal <> None and elcal < utils.getElevationLimit(): continue

        # Skip source if it is above users elevation limit
        if sources['snapElmax'] <> None:
            if el    > sources['snapElmax']: continue
            if elcal <> None and elcal > sources['snapElmax']: continue

        # Check user-specified elevation ranges
        ha = utils.getHa(source)
        if sources['snapElHa'] <> None:
            if (el < sources['snapElHa'][0]) and \
               (ha < sources['snapElHa'][1] or ha > sources['snapElHa'][2]):
                continue

        # OK - we have a source
        sid = int(value[0])
        sou = source
        tint = float(value[4])

        # Found a source - break loop
        break

    # Return source
    return [sou, primaryCalibrator, secondaryCalibrator, tint, sid]


def doSnapshot(sources, phaseCalDefault, pointing, passband, fluxcal, 
             correlator, noise, primaryCal=None, pstop=None,
             resetDoppler=False):
    """ Perform one cycle between phaseCal and sources.

        Inputs: sources    - contains science targets and phase calibrators;
                             see template2.obs
                phaseCalDefault - default phase calibrator to use
                fluxcal    - flux calibrator options; see template2.obs
                pointing   - pointing options; see template2.obs
                passband   - passband options; see template2.obs
                correlator - correlator options; see template2.obs
                noise      - noise source options; see template2.obs
                primaryCal - list of primary flux calibrators; see template2.obs
                resetDoppler   : If true, then set the doppler command
                                 for each source in "sources"
                pstop      - ending LST time for this phase calibrator

         Output: None
    """
    # Set database name
    db = sn.DB_DEFAULT
    if sources.has_key('snapDb'): db = sources['snapDb']

    # Set overhead factor. 60 converts minutes to hours
    foverhead = utils.overhead() / 60.0

    # Nominal passband
    timePb = foverhead * passband['tint']

    # Add passband time if observing two correlator configurations
    if correlator['configCal'] <> None: timePb *= 2.0   

    # Add passband time if observing hybrid configurations
    if correlator['hybrid'] <> None: 
        timePb += foverhead * correlator['tintHybrid'] * len(correlator['hybrid'])

    # Flux calibrators
    timeFlux = foverhead * fluxcal['tint']

    # Last calibrator
    timeLastcal = foverhead * sources['tintPhaseCal']

    # Source observations
    sou,t = utils.setSourceTint(sources['target'], sources['tintTarget'])
    tprimaryCal = t[0]

    # Set multi-frequency snapshot parameters
    timeCycle = 0.0
    isMultiFreq = sources['snapFreq'] <> None
    snapFreq = None
    if isMultiFreq:
        # Get desired frequencies
        snapFreq = utils.makeListObsdef(sources['snapFreq'])

        # Get desired integration time
        snapTint = utils.makeListObsdef(sources['snapTint'])
        if snapTint == None:
            snapTint = [utils.makeListObsdef(t[0])] * len(snapFreq)
        else:
            snapTint = utils.expandList(snapTint, snapFreq)

        # Set IF
        snapIF = utils.makeListObsdef(sources['snapIF'])
        if snapIF == None:
            [frest, fif, flo] = commands.freqSetup()
            snapIF = [utils.makeListObsdef(fif)] * len(snapFreq)
        else:
            snapIF = utils.expandList(snapIF, snapFreq)

        # Set SB
        snapSB = utils.makeListObsdef(sources['snapSB'])
        if snapSB == None:
            [frest, fif, flo] = commands.freqSetup()
            sidebandLO = commands.USB
            if flo > frest: sidebandLO = commands.LSB
            snapSB = [utils.makeListObsdef(sidebandLO)] * len(snapFreq)
        else:
            snapSB = utils.expandList(snapSB, snapFreq)

        # See if current tuning is the right frequency
        [foundTune, tuneIndx, tuneCurrent] = sn.findTuneSnap(snapFreq, snapSB, snapIF)
        if not foundTune:
           tuneIndx = 0
           sn.tuneSnap(tuneIndx, snapFreq, snapSB, snapIF, utils.getDopplerSource(), indent=0)

        # Compute time to do a complete snap cycle. This does not factor in
        # any secondary calibrators
        timeCycle = 2.0*timeLastcal + foverhead * sum(snapTint)/60.0 + \
                    3.0/60.0 * len(snapFreq)          # tuning overhead

    # Set default integration time for secondary calibrators
    tsecondaryCal = None
    if len(t) > 1: tsecondaryCal = t[1]

    # Initialize cycles
    okSource  = False
    continueCycles = not utils.isEndOfTrack()
    commands.s.setScriptBool(INDX_BOOL_LASTCAL,False)
    commands.s.setScriptBool(INDX_BOOL_STOPNEXTCAL,False)
    ncycle = commands.s.getScriptInt(INDX_INT_LASTCYCLE)

    # Time between phase calibrators
    #   tintSinceLastPhaseCal : On-source integration time in minutes since 
    #                           last phase cal observatons
    #   phaseCalPrevious      : Name of the last-observed phase calibrator
    tintSinceLastPhaseCal = None
    phaseCalPrevious = None

    # Start cycles
    okSource = False
    sidPrevious = 0
    tuneIndxPrevious = None
    while continueCycles :
        # Compute time needed to complete remaining critical observations
        tneed = 0.0
        if not finishedPassband(passband) : tneed += timePb
        if not finishedFluxcal(fluxcal) : 
            tneed += timeFlux * getNfluxCal(fluxcal)
        tneed += timeCycle

        # Find which source to observe next
        result = getNextSource(sources, tneed, pstop, phaseCalDefault)

        # Get source/phasecal information
        phaseCal = None
        secondaryCal = None
        sou_snap = result[0].strip()
        if phaseCalDefault <> None: phaseCal = phaseCalDefault.strip()
        if result[1] <> None and result[1] != "": phaseCal = result[1].strip()
        if result[2] <> None and result[2] != "": secondaryCal = result[2].strip()
        tint = min(result[3], t[0])  # Not used for snap multi-frequency observations
        sid = result[4]

        # If no snap-sources remain, then stop
        if sou_snap == '' or sou_snap == None:
            commands.trackMessage('No snapshot sources found')
            continueCycles = False
            continue

        # Message
        ncycle += 1
        tremaining = utils.timeRemaining(source=sou_snap, phase=phaseCal, 
                        tneed=tneed, useTime=True, 
                        lsttime=utils.getLstStopCycle(default=pstop))
        if tremaining < 0.0: tremaining = 0.0
        msg = 'Cycle ' + str('%3d' % ncycle) + '     ' + \
              utils.dtString(tremaining) + ' remaining'
        commands.trackMessage(msg)
        commands.s.setScriptInt(INDX_INT_LASTCYCLE, ncycle)
        if tremaining <= 0:
            commands.trackMessage('time limit reached on source cycles', indent=1)
            continueCycles = False
            continue
        utils.printWeather()

        # Check receivers are still locked
        utils.relockReceivers(indent=1)
                
        # Pick reference target to find pointing sources. One reference source 
        # is set for finding the phase calibrator (srefCal), and another for 
        # the target source (srefTarget).
        #
        # For radio pointing, srefCal is the target source, and srefTarget=None.
        #
        # For optical pointing, there can be different results depending on 
        # whether optical pointing is done only on he phase calibrator, or 
        # whether it is done the the phase calibrator and target source.
        srefCal = utils.getSourceName(sou_snap, n=1, parse=True)
        srefTarget = srefCal
        if commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) and \
           commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT) and \
           phaseCal <> None and \
           pointing['optPointCal'] and pointing['optPointTarget']:
            srefCal = phaseCal

        # Check optical pointing. Radio pointing is always bracketed by phase
        # calibrator observations. We do not do this for optical pointing.
        performingOpticalPointing = False
        if commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) and \
           commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT) and \
           pointing['optPointCal'] and pointing['doOptPoint'] and \
           utils.continueObservations(isScience=True) and \
           utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0:
            result = doPoint(pointing, ref=srefCal, 
                             correlator=correlator, printHeader=False)
            performingOpticalPointing = True

        # If new calibrator is different from previous one, or 
        # tuning will change, then observe previous calibrator one last time.
        # This step is skipped for multi-frequency observations since
        # calibrator observations for that mode are self-contained
        if okSource and utils.continueObservations() and not isMultiFreq and \
           ( (phaseCalPrevious <> None and phaseCal != phaseCalPrevious) or
             (sources['snapRetune'] and sou_snap != utils.getDopplerSource())):
            observeSource(phaseCalPrevious, sources['tintPhaseCal'], 
                         sources['record'], tsysTime=sources['tsys'], 
                         reconfig=correlator['configCal'], noise=noise,
                         lststop=utils.getLstStopCycle(default=pstop),
                         tmo=sources['tmo'], antwait=sources['antwait'],
                         intentObject='G', selfcal=True, runMiriad=True,
                         combine=sources['combine'])
            tintSinceLastPhaseCal = 0.0

        # Check if we need to continue
        if utils.isStopAfterNextCal():
            continueCycles = False
            continue

        # Retune if needed
        if sources['snapRetune'] and sou_snap <> utils.getDopplerSource():
            [frest, fif, flo] = commands.freqSetup()
            sidebandLO1 = commands.USB
            if flo > frest: sidebandLO1 = commands.LSB
            commands.trackMessage("Tuning for source "+sou_snap,indent=1)
            commands.freq(frest, sidebandLO1, fif, sou_snap)
            commands.flattenPhases()

        # Set number of loops to make. This is nominally 1, but can be
        # more than one for a multifrequency track. 
        nfreq = 1
        if snapFreq <> None: nfreq = len(snapFreq)
        usedTune = [False] * nfreq
        tint_actual = 0.0
        phaseCalOrig = phaseCal

        # Loop over frequencies
        for indx in range(nfreq):
             # Tune only if snapshot multifrequency track
             tuneIndx = 0
             if isMultiFreq:
                 [foundTune, tuneIndx, tuneCurrent] = sn.findTuneSnap(snapFreq, snapSB, snapIF, used=usedTune)
                 if not tuneCurrent:
                     sn.tuneSnap(tuneIndx, snapFreq, snapSB, snapIF, utils.getDopplerSource())
                 if not foundTune:
                     raise Exception, 'Error selecting snapshot tuning frequency.'

             # Select phase calibrator automatically if needed
             phaseCal = phaseCalOrig
             tintPhaseCal = sources['tintPhaseCal']
             if phaseCalOrig == None:
                 # Expand autophase
                 autoPhase = utils.expandList(sources['snapAutoPhase'], snapFreq)
                 if autoPhase[tuneIndx]:
                     # Get flux, separation, and integration time
                     autoFlux = utils.expandList(sources['snapAutoFlux'], snapFreq)[tuneIndx]
                     autoSep  = utils.expandList(sources['snapAutoSep'], snapFreq)[tuneIndx]
                     if sources['snapAutoTint'] <> None:
                         tintPhaseCal = utils.expandList(sources['snapAutoTint'], snapFreq)[tuneIndx]
                     else:
                         tintPhaseCal = sources['tintPhaseCal']

                     # Get sources meeting criteria
                     f = commands.freqSetup()[2]
                     if snapFreq <> None: f = snapFreq[tuneIndx]
                     sourceList = utils.getBrightSources(autoFlux, f)
                     excludeExpref = ['W3OH']
                     phaseCal = utils.getSource(sourceList, ref=sou_snap,
                                     excludeExpref=excludeExpref, 
                                     maxsep=autoSep)
                     if phaseCal == None:
                         commands.trackMessage('Could not find a suitable phase calibrator', indent=1)
               
             # See if we need radio pointing
#            needRadioPointing = (not commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) or \
#                not commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT)) and \
#               pointing['doPoint'] and indx == 0 and \
#               utils.continueObservations(isScience=True) and \
#               utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0
             needRadioPointing = False
             if indx == 0 and not performingOpticalPointing:
                 needRadioPointing, junk, junk, junk, junk, = \
                     needPointing(pointing, sou_snap, None, pointing['maxsep'], False)

             # Observe phase calibrator
             okSource = False
             if phaseCal == None:
                 okCal = True
             elif utils.continueObservations() and tremaining > 0.0:
                 # Initialize
                 observePhaseCal = False

                 # Check if we need to observe the phase calibrator
                 if needRadioPointing:
                     observePhaseCal = True
                 elif phaseCalPrevious != phaseCal:
                     observePhaseCal = True
                 elif not isMultiFreq and tintSinceLastPhaseCal >= tprimaryCal: 
                     observePhaseCal = True
                 elif isMultiFreq and (tuneIndxPrevious==None or tuneIndxPrevious != tuneIndx):
                     observePhaseCal = True

                 # Observe phase calibrator if needed
                 if observePhaseCal:
                     okCal = observeSource(phaseCal, tintPhaseCal,
                              sources['record'], tsysTime=sources['tsys'], 
                              reconfig=correlator['configCal'], noise=noise,
                              lststop=utils.getLstStopCycle(default=pstop),
                              tmo=sources['tmo'], antwait=sources['antwait'],
                              intentObject='G', selfcal=True, runMiriad=True,
                              combine=sources['combine'])
                     if okCal: tintSinceLastPhaseCal = 0.0
             phaseCalPrevious = phaseCal
             sidPrevious = sid
             tuneIndxPrevious = tuneIndx
             if utils.isStopAfterNextCal():
                 continueCycles = False
                 continue

             # Check radio pointing. "pointed" only indicates if radio pointing
             # was performed, not optical pointing.
             # This is done on only the first frequency.
             pointed = False
             if needRadioPointing:
                 pointed = doPoint(pointing, ref=sou_snap,
                                   correlator=correlator, printHeader=False)
                 if pointed: 
                     phaseCalPrevious = None # Reinitialize phasecal observation
                     tuneIndxPrevious = None
                     break  # Break from frequency loop

             # Flux calibrator
             observedFluxcal = False
             if not finishedFluxcal(fluxcal) and fluxcal['middle'] and \
                utils.continueObservations(isScience=True) and \
                utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0:
                 observedFluxcal = doFluxcal(fluxcal, primaryCal, noise, correlator,
                       pointing, ref=sou_snap, lststop=pstop, printHeader=False)

             # Passband calibrator
             observedPassband = False
             if not finishedPassband(passband) and passband['middle'] and \
                utils.continueObservations(isScience=True) and \
                utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0:
                 observedPassband = doPassband(passband, correlator, noise,
                   pointing=pointing, ref=sou_snap, lststop=pstop, printHeader=False)

             # Compute time remaining
             tneed = 0.0
             if phaseCal <> None : tneed += timeLastcal
             if not finishedPassband(passband) : tneed += timePb
             if not finishedFluxcal(fluxcal) : 
                 tneed += timeFlux * getNfluxCal(fluxcal)
             tneed += timeCycle
             tremaining = utils.timeRemaining(source=sou_snap, phase=phaseCal, 
                                tneed=tneed, useTime=True, 
                                lsttime=utils.getLstStopCycle(default=pstop))

             # Observe source if
             #    1) we did not radio point
             # and
             #    2) did not observe flux/pb cal
             okSource = False
             if not pointed and not observedFluxcal and not observedPassband and \
                utils.continueObservations(isScience=True) and tremaining > 0.0:
                 # Mark tuning as used
                 usedTune[tuneIndx] = True

                 # Check optical pointing
                 if commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) and \
                    commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT) and \
                    pointing['doOptPoint'] and \
                    pointing['optPointTarget']:
                     doPoint(pointing, ref=srefTarget, 
                        correlator=correlator, printHeader=False)

                 # Get time at beginning of scan
                 utstart = utils.getUT(timestamp=True)

                 # Set integration time for multiFrequency observations. 
                 if sources['snapFreq'] <> None: tint = snapTint[tuneIndx]

                 # Observe source
                 t1 = time.time()
                 tz = min(tint, tprimaryCal - tintSinceLastPhaseCal)
                 tint_actual += observeSource(sou_snap, tz, sources['record'],
                     isScience=True, lststop=utils.getLstStopCycle(default=pstop),
                     tsysTime=sources['tsys'], maximumTime=tremaining, 
                     tmo=sources['tmo'], resetDoppler=resetDoppler,
                     antwait=sources['antwait'], intentObject='S', 
                     selfcal=False, returnTint=True, combine=sources['combine'])

                 # Adjust time remaining for integration time on source
                 dt = time.time() - t1
                 tremaining -= dt / 3600.0

                 # Observe secondary calibrator 
                 if secondaryCal <> None and tint_actual > 0:
                      observeSource(secondaryCal, tsecondaryCal, sources['record'],
                           isScience=True, lststop=utils.getLstStopCycle(default=pstop),
                           tsysTime=sources['tsys'], maximumTime=tremaining, 
                           tmo=sources['tmo'], resetDoppler=resetDoppler,
                           antwait=sources['antwait'], intentObject='S', 
                           selfcal=False, returnTint=True, 
                           combine=sources['combine'])

                 # For multifrequency observations, end on the calibrator
                 if isMultiFreq and tint_actual > 0 and phaseCal <> None:
                     observeSource(phaseCal, tintPhaseCal,
                               sources['record'], tsysTime=sources['tsys'], 
                               reconfig=correlator['configCal'], noise=noise,
                               lststop=utils.getLstStopCycle(default=pstop),
                               tmo=sources['tmo'], antwait=sources['antwait'],
                               intentObject='G', selfcal=True, runMiriad=True,
                               combine=sources['combine'])
                     phaseCalPrevious = phaseCal
                     sidPrevious = sid
                     tuneIndxPrevious = tuneIndx

        # Get delta time
        if tint_actual > 0:
            tintSinceLastPhaseCal += tint_actual/60.0
            okSource = True
            utils.addSourceTime(tint_actual/3600.0)
            weather = utils.getWeather()
            sn.addTimeDatabase(sid, tint_actual/60.0, utstart,
                      len(commands.currentAntennaNumbers()), 
                      utils.getObsblockName(), 
                      db, tau=weather[1], phase=weather[3])

        # Stop if nothing was observed or we have reached the time limit.
        if not utils.continueObservations(isScience=True) or tint_actual == 0.0 and \
           not (observedPassband or observedFluxcal or pointed):
            continueCycles = False

    # End cycle on phase calibrator
    if okSource and utils.continueObservations() and phaseCalPrevious <> None:
        commands.trackMessage('Observing ' + phaseCalPrevious + ' one last time')
        observeSource(phaseCalPrevious, sources['tintPhaseCal'], 
                 sources['record'], noise=noise, tsysTime=sources['tsys'], 
                 reconfig=correlator['configCal'], tmo=sources['tmo'],
                 antwait=sources['antwait'], intentObject='G', selfcal=True, 
                 runMiriad=True, combine=sources['combine'])

    # See why we are here
    if utils.isEndOfTrack():
        commands.trackMessage('Ending observations based on endtrack()')
    elif utils.isLastcal(): 
        commands.trackMessage('Ending cycle based on lastcal()')
    elif utils.isStopAfterNextCal():
        commands.trackMessage('Ending cycle based on stopAfterNextCal()')
