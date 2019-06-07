# obsdef3_sun.py: version of obsdef with flux cal turned off
#                 set minsepsun to 0.0
#
# @author John Carpenter
# $Id: obsdef3_sun.py,v 1.2 2012/12/18 21:20:23 mpound Exp $


import string
import time
import os
import copy
import printFunctions
import fileIOPython
import snapshot as sn
import pacs
import runCommand as rc

import subarrayCommands as commands
import obsdefUtils as utils
import carmaHelpers as helpers

from obsdefIndex import *

import refPoint
import radioPoint
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


def initializeTsysDictionary():
    d = dict()
    d['tintSinceLastTsys'] = None
    d['tintSinceLastNoise'] = None
    return d

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
    var['maxsens']   = commands.s.getScriptBool(INDX_BOOL_MAXSENS)
    var['pacs']      = commands.s.getScriptBool(INDX_BOOL_PACS)
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
        if mosaic.has_key('otf') and mosaic['otf']:
           commands.trackMessage('last position observed in OTF mosaic was fieldIndex=' +
                        str(lastpos), indent=1)
        else:
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
            if badants != '': 
                pointing['badOptAnts'] = utils.makeListObsdef(badants)

        # Optical pointing results
        if not pointing.has_key('optRes'):
            pointing['optRes'] = dict()
            z = sopt.split()
            for i in range(0,len(z),3):
               ant = int(z[i])
               d_az = float(z[i+1])
               d_el = float(z[i+2])
               pointing['optRes'][ant] = [d_az, d_el]


def findCalibrator(options=None, returnFluxes=False, verbose=True, 
                   force=False,  sources=None):
    """ Observes calibrators and finds one bright enough to observe.

        Inputs: options      - "sources" options in template3.obs
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
            raise rc.ScriptError, 'callist and tintPhaseCal have different sizes.'
        if len(calibrators) != len(tintPhaseCal):
            tintPhaseCal = [tintPhaseCal[0]] * len(calibrators)

    # Set integration time for observing calibrators in this function
    tint = 60.0
    if options <> None and options.has_key('tintlist'): 
        tint = options['tintlist']

    # Determine which bands have 500 MHz band width
    config = utils.getConfigband()
    bands62 = list()
    bands = list()
    for c in config:
        if c[2] == commands.BW62:  bands62.append(c[0])
        if c[2] == commands.BW500: bands.append(c[0])
    if len(bands) == 0:
        if len(bands62) > 0: 
            bands = bands62[:]
        else:
            commands.trackMessage('Must be at least one 500 MHz or 62 MHz band in configuration', 
                         indent=1)
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
    pointing['centroidLoopMax'] = utils.getVariable(pointing,'centroidLoopMax',8,nonone=True)
    pointing['doOptPoint']  = utils.getVariable(pointing,'doOptPoint',False,nonone=True)
    pointing['doPointSunRiseSet'] = \
              pointing['doPointNight'] or pointing['doPointDay']
    pointing['intervalOpt'] = utils.getVariable(pointing,'intervalOpt',10.0/60.0,nonone=True)
    pointing['intervalTran'] = utils.getVariable(pointing,'intervalTran',None)
    pointing['ncoadd']      = utils.getVariable(pointing,'ncoadd',None)
    pointing['maxmag']      = utils.getVariable(pointing,'maxmag',None)
    pointing['maxOptSep']   = utils.getVariable(pointing,'maxOptSep',20.0,nonone=True)
    pointing['mapPoints'] = utils.getVariable(pointing,'mapPoints',5,nonone=True)
    pointing['nbadAnts']    = utils.getVariable(pointing,'nbadAnts',3,nonone=True)
    pointing['nfailOpt'] = utils.getVariable(pointing,'nfailOpt',3,nonone=True)
    pointing['nrepCross'] = utils.getVariable(pointing,'nrepCross',1,nonone=True)
    pointing['nrepInt'] = utils.getVariable(pointing,'nrepInt',1,nonone=True)
    pointing['optPointCal'] = utils.getVariable(pointing,'optPointCal',True,nonone=True)
    pointing['optPointTarget'] = utils.getVariable(pointing,'optPointTarget',False,nonone=True)
    pointing['optradPreferred'] = utils.getVariable(pointing,'optradPreferred',None)
    pointing['optradTune95'] = utils.getVariable(pointing,'optradTune95',None)
    pointing['optradElmin'] = utils.getVariable(pointing,'optradElmin',30.0,nonone=True)
    pointing['optradElmax'] = utils.getVariable(pointing,'optradElmax',75.0,nonone=True)
    pointing['optElmin'] = utils.getVariable(pointing,'optElmin',None)
    pointing['optElmax'] = utils.getVariable(pointing,'optElmax',pointing['elmax'],nonone=True)
    pointing['subtract']    = utils.getVariable(pointing,'subtract',None)
    pointing['tune95']    = utils.getVariable(pointing,'tune95',False,nonone=True)
    pointing['tune95Pref'] = utils.getVariable(pointing,'tune95Pref',pointing['tune95'],nonone=True)


def setHiddenParameters(limits, fluxcal, mosaic, passband, pointing, 
                        projectInfo, sources, noise):
    """ Set hidden parameters in the various parameter options """
    # Callist
    if not sources.has_key('callist'): sources['callist'] = None

    # Ambient load
    pn = 'ambient';
    limits[pn]   = utils.getVariable(limits,pn,False,nonone=True)
    fluxcal[pn]  = utils.getVariable(fluxcal,pn,limits[pn],nonone=True)
    sources[pn]  = utils.getVariable(sources,pn,limits[pn],nonone=True)
    passband[pn] = utils.getVariable(passband,pn,limits[pn],nonone=True)
    pointing[pn] = utils.getVariable(pointing,pn,limits[pn],nonone=True)

    # Minimum sun separation
    minsepsun = None
    if commands.subarrayNo == 1: minsepsun = None
    pn = 'minsepsun'
    limits[pn]   = utils.getVariable(limits,pn,minsepsun,nonone=True)
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

    # Record
    paramsLimits = ['record']
    paramsNew    = ['record']
    for i in range(len(paramsLimits)):
        pl = paramsLimits[i]
        pn = paramsNew[i]
        fluxcal[pn]  = utils.getVariable(fluxcal,pn,limits[pl],nonone=True)
        passband[pn] = utils.getVariable(passband,pn,limits[pl],nonone=True)
        sources[pn]  = utils.getVariable(sources,pn,limits[pl],nonone=True)

    # Track timeout
    paramsLimits = ['tmoTrack']
    paramsNew    = ['tmo']
    for i in range(len(paramsLimits)):
        pl = paramsLimits[i]
        pn = paramsNew[i]
        fluxcal[pn]  = utils.getVariable(fluxcal,pn,limits[pl],nonone=True)
        passband[pn] = utils.getVariable(passband,pn,limits[pl],nonone=True)
        sources[pn] = utils.getVariable(sources,pn,limits[pl],nonone=True)
        mosaic[pn] = utils.getVariable(mosaic,pn,limits[pl],nonone=True)
    mosaic['tmoMosaic'] = utils.getVariable(mosaic,'tmoMosaic',15.0,nonone=True)

    # Tsys
    paramsLimits = ['tsys']
    paramsNew    = ['tsys']
    for i in range(len(paramsLimits)):
        pl = paramsLimits[i]
        pn = paramsNew[i]
        fluxcal[pn]  = utils.getVariable(fluxcal,pn,limits[pl],nonone=True)
        passband[pn] = utils.getVariable(passband,pn,limits[pl],nonone=True)
        sources[pn]  = utils.getVariable(sources,pn,limits[pl],nonone=True)

    # Optical pointing
    setHiddenOpticalPointing(pointing)

    # Pointing
    pointing['bracketPhasecal'] = utils.getVariable(pointing,'bracketPhasecal',True)
    pointing['antennas'] = utils.getVariable(pointing,'antennas', None, nonone=False)
    pointing['timeLimit'] = utils.getVariable(pointing,'timeLimit', 7.0, nonone=False)
    pointing['pntwait'] = utils.getVariable(pointing,'pntwait', 2)
    pointing['waitCycles'] = utils.getVariable(pointing,'waitCycles', 3)
    param = ['doPoint', 'maxsep']
    value = [False, pointing['maxsep'], 'True']
    for ip in range(len(param)):
        p = param[ip]
        v = value[ip]
        if not passband.has_key(p): passband[p] = v
        if not fluxcal.has_key(p):  fluxcal[p]  = v

    # Passband
    passband['doPoint'] = utils.getVariable(passband,'doPoint',False,nonone=True)
    passband['middle'] = utils.getVariable(passband,'middle',False,nonone=True)
    passband['forcePoint'] = utils.getVariable(passband,'forcePoint',False,nonone=True)
    passband['ncal'] = utils.getVariable(passband,'ncal',1,nonone=True)
    passband['interval'] = utils.getVariable(passband,'interval',None)

    # Project info
    projectInfo['subobsblock'] = utils.getVariable(projectInfo,'subobsblock',None)
    projectInfo['carma23'] = utils.getVariable(projectInfo,'carma23',False)

    # Fluxcal
    fluxcal['forcePoint'] = utils.getVariable(fluxcal,'forcePoint',False,nonone=True)
    fluxcal['ncal'] = utils.getVariable(fluxcal,'ncal',1,nonone=True)
    fluxcal['ncycles'] = utils.getVariable(fluxcal,'ncycles',1,nonone=True)
    fluxcal['interval'] = utils.getVariable(fluxcal,'interval',None)
    fluxcal['middle'] = utils.getVariable(fluxcal,'middle',False,nonone=True)

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

    # Sources
    if sources['doSnapshot']: checkSnapshotSources(sources)
    sources['doppler'] = utils.getVariable(sources,'doppler', None)
    sources['intentTarget'] = utils.getVariable(sources,'intentTarget', 'S', nonone=True)
    sources['pointstatusTarget'] = utils.getVariable(sources,'pointstatusTarget',commands.ONSRC, nonone=True)

    # Polarization
    sources['intervalPol'] = utils.getVariable(sources,'intervalPol', 0.66)
    sources['tintPol']     = utils.getVariable(sources,'tintPol', 0.5, nonone=True)

    # PACS parameters
    sources['pacs'] = utils.getVariable(sources,'pacs',None)
    sources['phaseCalPacs'] = utils.getVariable(sources,'phaseCalPacs',None)

    # Sample time for noise source
    if noise <> None:
        defsample = None
        noise['tsample'] = utils.getVariable(noise,'tsample',defsample,nonone=False)

    # Mosaicking
    mosaic['off'] = utils.getVariable(mosaic, 'off', None, nonone=False)
    mosaic['tintOff'] = utils.getVariable(mosaic, 'tintOff', 0.5, nonone=True)
    mosaic['intervalOff'] = utils.getVariable(mosaic, 'intervalOff', 2.0, nonone=True)

    # OTF mosaicking
    mosaic['otf']            = utils.getVariable(mosaic, 'otf', False)
    mosaic['otfFieldtime']   = utils.getVariable(mosaic, 'otfFieldtime', 10)
    mosaic['otfRows']        = utils.getVariable(mosaic, 'otfRows', 5)
    mosaic['otfFieldsInRow'] = utils.getVariable(mosaic, 'otfFieldsInRow', 5)
    mosaic['otfDelta']       = utils.getVariable(mosaic, 'otfDelta', 0.5)
    mosaic['otfPA']          = utils.getVariable(mosaic, 'otfPA', 0)
    mosaic['otfMaxtime']     = utils.getVariable(mosaic, 'otfMaxtime', 0)
    mosaic['otfMaxreps']     = utils.getVariable(mosaic, 'otfMaxreps', 100000)
    mosaic['otfStartFieldIndex'] = utils.getVariable(mosaic, 'otfStartFieldIndex', 0)
    mosaic['otfTrkThreshold']= utils.getVariable(mosaic, 'otfTrkThreshold', 0.1)
    mosaic['otfSlewtime']    = utils.getVariable(mosaic, 'otfSlewtime', 1.5)
    mosaic['otfRowOffset']   = utils.getVariable(mosaic, 'otfRowOffset', 0)
    mosaic['otfColOffset']   = utils.getVariable(mosaic, 'otfColOffset', 0)


def setMosaicParameters(mosaic, sources):
    """ Modified mosaic parameters so that they have same size as sources['target'].
        "mosaic" and "sources" are the usual obsdef3 dictionaries.

        The otf parameters need to be entered once, once for each mosaic target, or one for
        each target. checkOTFParameters will be set the OTF parameters to have a value 
        for each target.

        The mosaic dictionary will be modifed.
    """

    # Set the OTF keywords that need to be checked
    keywords = ['otfFieldtime',    'otfRows',     'otfFieldsInRow', 'otfDelta',
                'otfPA',           'otfMaxtime',  'otfMaxreps',
                'otfTrkThreshold', 'otfSlewtime', 'otfRowOffset',   'otfColOffset',
                'off',             'tintOff',     'intervalOff']

    # Count the number of targets
    sourceList = utils.makeListObsdef(sources['target'])
    ntargets = len(sourceList)

    # Count the number of targets that will be mosaicked.
    mosaicTarget = utils.makeListObsdef(sources['mosaicTarget'])
    if len(mosaicTarget) == 1: mosaicTarget = mosaicTarget * ntargets
    nmosaics = sum(mosaicTarget)

    # Modify the "off" position, if needed . This is needed since mosaic['off'] is a list itself.
    if mosaic['off'] <> None and len(mosaic['off']) == 2 and \
       not list in [type(mosaic['off'][0])] and mosaic['off'][0] <> None:
       mosaic['off'] = [mosaic['off']]

    # If the stored OTF field index is non-zero, then reset starting field index.
    # This only makes sense if there is one mosaic.
    # Starting index is 0 for OTF mosaics.
    if nmosaics == 1:
       lastpos = max(0, utils.getLastMosaicPosition(mosaic['otfStartFieldIndex']) + 1)
       # if lastpos > utils.makeListObsdef(mosaic['otfRows'])[0]: lastpos = 0
       if lastpos > 0: mosaic['otfStartFieldIndex'] = lastpos

    # Make sure that the number of OTF parameters is consistent
    for k in keywords:
       # Get values
       values = utils.makeListObsdef(mosaic[k])
       if values == None: values = [None]
       n = len(values)

       # Check length
       if n != 1 and n != nmosaics and n != ntargets:
          msg = "The number of arguments for mosaic['%s'] is inconsistent with the sources dictionary.\n" % k
          msg += 'Number of targets = %d\n' % ntargets
          msg += 'Number of mosaicked targets = %d\n' % nmosaics
          msg += "Number of values in mosaic['%s'] = %d\n" % (k, n)
          raise Exception, msg

       # Set the OTF parameters
       params = []
       nmos = 0
       for i in range(ntargets):
           if not mosaicTarget[i]:
              params.append(None)
           elif len(values) == 1:
              params.append(values[0])
           elif len(values) == nmosaics:
              params.append(values[nmos])
              nmos += 1
           else:
              params.append(values[i])
       mosaic[k] = params[:]

    # Now create the mosaics
    mosaic['otfMosaic'] = []
    for i in range(ntargets):
       if mosaicTarget[i] and mosaic['otf']:
          commands.trackMessage('Initializing mosaic for %8s ' % sourceList[i], indent=1)
          commands.trackMessage('... field time   = %.1f s' % mosaic['otfFieldtime'][i], indent=2)
          commands.trackMessage('... nrows        = %d   ' % mosaic['otfRows'][i], indent=2)
          commands.trackMessage('... field in row = %d   ' % mosaic['otfFieldsInRow'][i], indent=2)
          commands.trackMessage('... start index  = %d   ' % mosaic['otfStartFieldIndex'], indent=2)
          commands.trackMessage('... PA           = %.1f ' % mosaic['otfPA'][i], indent=2)
          commands.trackMessage('... row offset   = %.3f arcmin' % mosaic['otfRowOffset'][i], indent=2)
          commands.trackMessage('... col offset   = %.3f arcmin' % mosaic['otfColOffset'][i], indent=2)
          commands.trackMessage('... delta        = %.3f arcmin' % mosaic['otfDelta'][i], indent=2)
          mosaic['otfMosaic'].append(
             commands.Mosaic(mosaic['otfFieldtime'][i], mosaic['otfRows'][i], 
                   mosaic['otfFieldsInRow'][i], delta=mosaic['otfDelta'][i], 
                   pa=mosaic['otfPA'][i], maxtime=mosaic['otfMaxtime'][i],
                   maxreps=mosaic['otfMaxreps'][i], 
                   startFieldIndex=mosaic['otfStartFieldIndex'], 
                   trkThreshold=mosaic['otfTrkThreshold'][i], 
                   slewtime=mosaic['otfSlewtime'][i], 
                   rowOffset=mosaic['otfRowOffset'][i], 
                   colOffset=mosaic['otfColOffset'][i]))
       else:
          mosaic['otfMosaic'].append(None)

def initializeOpticalPointing():
    """ Launch optical pointing windows and set script variables """
    commands.s.setScriptString(INDX_STR_OPOINT_KEY, time.asctime())
    ants = commands.currentAntennaNumbers()
    for a in range(16,24):
       if ants.count(a) > 0: ants.remove(a)
    ogm.opticalInit(antVec=ants)
    doOptPoint = True
    # if pointing['doOptPoint'] and commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT_SET):
    if commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT_SET):
       doOptPoint = commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT)
    commands.s.setScriptBool(INDX_BOOL_DO_OPT_POINT, doOptPoint)
    commands.s.setScriptBool(INDX_BOOL_DO_OPT_POINT_SET, True)


def readOptions(var):
    # Default values
    var['alarm']     = True
    var['carma23']   = False
    var['email']     = True
    var['endtrack']  = None
    var['flux']      = None
    var['fluxstart'] = True
    var['full']      = False
    var['lststart']  = None
    var['lststop']   = None
    var['maxsens']   = False
    var['minsepsun'] = None
    var['pacs']      = pacs.RUN_PACS
    var['pb']        = None
    var['pbstart']   = True
    var['pnt']       = None
    var['pntstart']  = True
    var['opnt']      = None
    var['refpoint']  = False
    var['resetObsblock'] = False
    var['tmax']      = None
    var['tune']      = True
    var['tunePacs']  = True

    # Initialize run command options
    p = rc.Params()
    p.add("alarm",    default=None, type=bool,  description="Sound alarm at end of track", noneAllowed=True)
    p.add("carma23",  default=None, type=bool,  description="Run script in CARMA23 mode", noneAllowed=True)
    p.add("email",    default=None, type=bool,  description="Email scriptlog", noneAllowed=True)
    p.add("endtrack", default=None, type=str,   description="Ending LST time", noneAllowed=True)
    p.add("flux",     default=None, type=bool,  description="Observe flux cal", noneAllowed=True)
    p.add("fluxstart",default=None, type=bool,  description="Observe flux cal at start of track", noneAllowed=True)
    p.add("full",     default=None, type=bool,  description="Run full list in PACS test track", noneAllowed=True)
    p.add("lststart", default=None, type=str,   description="Starting LST time for phase-cal", noneAllowed=True)
    p.add("lststop",  default=None, type=str,   description="Ending LST time for phase-cal", noneAllowed=True)
    p.add("maxsens",  default=None, type=bool,  description="Maximum sensitivity track", noneAllowed=True)
    p.add("minsepsun", default=None, type=float, description="Minimum allowed source separation from the sun in degrees", noneAllowed=True)
    p.add("pacs",     default=None, type=bool,  description="Run PACS", noneAllowed=True)
    p.add("pb",       default=None, type=bool,  description="Observe passband cal", noneAllowed=True)
    p.add("pbstart",  default=None, type=bool,  description="Observe passband cal at start", noneAllowed=True)
    p.add("pnt",      default=None, type=bool,  description="Perform radio pointing", noneAllowed=True)
    p.add("pntstart", default=None, type=bool,  description="Perform radio pointing at start", noneAllowed=True)
    p.add("opnt",     default=None, type=bool,  description="Perform optical pointing", noneAllowed=True)
    p.add("refpoint", default=None, type=bool,  description="Use refpoint instead of the new pointing routine", noneAllowed=True)
    p.add("resetObsblock", default=None, type=bool,  description="Reset the obsblock to previously stored value when restart a track", noneAllowed=True)
    p.add("tmax",     default=None, type=float, description="Maximum number of hours to run track", noneAllowed=True)
    p.add("tune",     default=None, type=bool,  description="Tune the receivers", noneAllowed=True)
    p.add("tunePacs", default=None, type=bool,  description="Tune the receivers for PACS", noneAllowed=True)

    # Read command line options
    p.processInputParameters(inputParams=commands.scriptKeyVals)

    # Return parameters
    return p

def initializeTrack(sources,  mosaic,   projectInfo, limits,  
                    pointing, passband, fluxcal,     noise,         
                    restart=False, scriptName=None, scriptOptions=None):
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
           8) If pacs and tune=T, tune Sci2

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

    # Read inputs. This sets default values.
    var = dict()
    p = readOptions(var)

    # Read restart variables
    if restart: 
        # Read variables
        readRestartVariables(var, mosaic, pointing)

    else:
        # Clear variables
        commands.controlVariablesClear()
        commands.radioInit()
        # commands.zeroMountOffsets()
        utils.setPacsUT()

    # Add options to give observers flexibility on executing the script.
    # A few options can override script dictionaries, and therefore their
    # default value is None (=> do not override script dictionary)
    for k in p.keys():
        if k != "help" and p.keywordValueDict()[k] <> None: 
            var[k] = p.keywordValueDict()[k]

    # Always set tune to false on restart
    if restart: 
        if var['tune']: 
            commands.trackMessage('Setting tune to False for a track restart', 
                indent=1)
        var['tune'] = False

    # Reset tracking thresholds and drive blanking
    commands.trackThreshold(limits['trackingThreshold'])
    commands.driveErrorPreference(commands.PREF_BLANK)

    # Messages
    commands.setTrackStartTime()
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
            commands.trackMessage(k +  s +  ' ' + 
                str(commands.scriptKeyVals[k]), indent=2)
    commands.trackMessage('Project code    : ' + projectInfo['code'], indent=1)
    commands.addScriptString(INDX_STR_SCRIPTNAME, scriptName, append=False)
    commands.trackMessage('Elevation limit : ' + 
            str('%.1f' % utils.getElevationLimit()), indent=1)
    utils.checkAntennas(indent=1)

    # Check project code is a string and a scalar
    if projectInfo['code'] == None or str not in [type(projectInfo['code'])]:
        raise Exception, 'Error specifying project code'

    # Set most hidden parameters.
    setHiddenParameters(limits, fluxcal, mosaic, passband, pointing, \
                        projectInfo, sources, noise)

    # Make sure the OTF mosaic parameters have the same size as the target list
    # The mosaic dictionary will be modified
    setMosaicParameters(mosaic, sources)

    # This indicates if the full PACS test track should be run
    sources['full'] = var['full'] 

    # Summarize main options
    if utils.getLstStartCycle() <> None: 
        commands.trackMessage('LST start cycle = ' + 
            helpers.convertHmsString(utils.getLstStartCycle()), indent=1)
    if utils.getLstStopCycle() <> None: 
        commands.trackMessage('LST stop  cycle = ' + 
            helpers.convertHmsString(utils.getLstStopCycle()), indent=1)
    if utils.getTmax() <> None: 
        commands.trackMessage('Maximum track length = ' + 
            utils.dtString(utils.getTmax()), indent=1)

    # Store command line options back in variables
    if var['flux'] <> None: 
       fluxcal['doPrimary']   = var['flux']
       fluxcal['doSecondary'] = var['flux']
    if var['opnt'] <> None: 
       pointing['doOptPoint'] = var['opnt']
    if var['pnt'] <> None: 
       pointing['doPointNight'] = var['pnt']
       pointing['doPointDay'] = var['pnt']
    if var['pb'] <> None:
       passband['doPassband'] = var['pb']
    if var['carma23'] <> None:
       projectInfo['carma23'] = var['carma23']
    fluxcal['start']  = var['fluxstart']
    passband['start'] = var['pbstart']
    pointing['start'] = var['pntstart']
    if var['minsepsun'] <> None:
       fluxcal['minsepsun'] = var['minsepsun']
       limits['minsepsun'] = var['minsepsun']
       passband['minsepsun'] = var['minsepsun']
       pointing['minsepsun'] = var['minsepsun']
       sources['minsepsun'] = var['minsepsun']

    # Set PACS variables in both subarrays (if available) to indicate PACS mode
    if commands.s1 != None:
        commands.s1.setScriptBool(INDX_BOOL_PACS, var['pacs'])
    if commands.s2 != None:
        commands.s2.setScriptBool(INDX_BOOL_PACS, var['pacs'])
    runPacs = commands.s.getScriptBool(INDX_BOOL_PACS)
    
    # Make sure pacs script is run in sciall
    #if var['maxsens'] and not commands.Subarray.isSciall():
    #    raise rc.ScriptError, "Maximum sensitivity scripts must be run in sciall!"            
    commands.s.setScriptBool(INDX_BOOL_MAXSENS, var['maxsens'])

    # Make sure pacs script is run in sciall
    if runPacs and not commands.Subarray.isSciall():
        raise rc.ScriptError, "PACS scripts must be run in sciall!"            

    # If this is a snapshot multi-frequency track, then we must retune.
    # Otherwise, the correlator configuration may get messed up.
    if sources['snapFreq'] <> None: var['tune'] = True

    # Optical pointing. Launch the optical pointing windows if the
    # keyword is set in the script. But the script variable controls
    # it is actually done.
    if pointing['doOptPoint']: initializeOpticalPointing()

    # Save command line variables in memory in case script is restarted
    commands.s.setScriptBool(INDX_BOOL_TUNE, var['tune'])
    commands.endTrackAlarm(var['alarm'])
    utils.emailScriptLog(var['email'])
    commands.setLstEndTrack(var['endtrack'])
    utils.setLstStartCycle(var['lststart'], verbose=False)
    utils.setLstStopCycle(var['lststop'], verbose=False)
    commands.setTmax(var['tmax'], verbose=False)
    commands.s.setScriptBool(INDX_BOOL_COMMAND_FLUXSTART, var['fluxstart'])
    commands.s.setScriptBool(INDX_BOOL_COMMAND_PBSTART, var['pbstart'])
    commands.s.setScriptBool(INDX_BOOL_COMMAND_PNTSTART, var['pntstart'])

    # Some options have None as a default since they can override
    # dictionary options. We therefore need to store two variables:
    # one that indicates if the command option has been set, and
    # a second that stores the value.
    commands.s.setScriptBool(INDX_BOOL_COMMAND_FLUX_SET, (var['flux'] <> None))
    commands.s.setScriptBool(INDX_BOOL_COMMAND_PB_SET, (var['pb'] <> None))
    commands.s.setScriptBool(INDX_BOOL_COMMAND_PNT_SET, (var['pnt'] <> None))
    commands.s.setScriptBool(INDX_BOOL_COMMAND_OPNT_SET, (var['opnt'] <> None))
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_FLUX_SET):
        commands.s.setScriptBool(INDX_BOOL_COMMAND_FLUX, var['flux'])
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_PB_SET):
        commands.s.setScriptBool(INDX_BOOL_COMMAND_PB, var['pb'])
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_PNT_SET):
        commands.s.setScriptBool(INDX_BOOL_COMMAND_PNT, var['pnt'])
    if commands.s.getScriptBool(INDX_BOOL_COMMAND_OPNT_SET):
        commands.s.setScriptBool(INDX_BOOL_COMMAND_OPNT, var['opnt'])

    # Set obsblock
    tmpObsblock = projectInfo['obsblock']

    # Set subObsblock. Remove any spaces and '.'
    tmpSubobsblock = projectInfo['subobsblock']
    if tmpSubobsblock == None: tmpSubobsblock = ''
    tmpSubobsblock = tmpSubobsblock.split('.')[0]
    tmpSubobsblock = ''.join(tmpSubobsblock.split()) # Removes spaces

    # Get official obsblock
    tmpObsblock = utils.getOfficialObsblock(projectInfo['code'], tmpObsblock)

    # Get the obsblock name currently in the control system
    obsblockName = utils.getObsblockName()

    # Check that the script obsblock name is the same as the stored name.
    # The stored obsblock name contains the trial, which has to be 
    # stripped off before comparing.
    if restart:
       # Split stored obsblock name
       storedObsblock = utils.parseObsblock(commands.s.getScriptString(INDX_STR_OBSBLOCK_ID))

       # Check project code (versus the script)
       if storedObsblock['project'] != projectInfo['code']:
          raise Exception,'Error restarting script: The project code in the script (%s) does not match the project code stored in memory (%s).' % (projectInfo['code'], storedObsblock['project'])
       if storedObsblock['obsblock'] != projectInfo['obsblock']:

       # Check obsblock (versus the script)
          raise Exception,'Error restarting script: The obsblock in the script (%s) does not match the obsblock stored in memory (%s).' % (projectInfo['obsblock'], storedObsblock['obsblock'])

       # Check subobsblock (versus the script)
       # Do not check subobsblock in maxsens mode since the subobsblock is set by the control system.
       # Nonetheless, the full obsblock name is checked later.
       if not var['maxsens'] and storedObsblock['subobsblock'] != projectInfo['subobsblock']:
          raise Exception,'Error restarting script: The subobsblock in the script (%s) does not match the subobsblock stored in memory (%s).' % (str(projectInfo['subobsblock']), str(storedObsblock['subobsblock']))
       
       # Now make sure obsblock name in rtd window matches the obsblock stored in memory
       if obsblockName != commands.s.getScriptString(INDX_STR_OBSBLOCK_ID):
          if var['resetObsblock']:
             # Reset obsblock name
             utils.resetObsblockName(commands.s, commands.s.getScriptString(INDX_STR_OBSBLOCK_ID))

             # Check if sci2 obsblocks name matches sci1 obsblock name for PACS and MAXSENS mode
             if runPacs or var['maxsens']:
                if runPacs:
                   # Reset sci2 obsblock
                   utils.resetObsblockName(commands.s2, commands.s2.getScriptString(INDX_STR_OBSBLOCK_ID))

                   # Get obsblocks
                   sci1Obsblock = commands.s1.getScriptString(INDX_STR_OBSBLOCK_ID).split('.')
                   sci2Obsblock = commands.s2.getScriptString(INDX_STR_OBSBLOCK_ID).split('.')

                elif var['maxsens']:
                   # Get both sci1 and sci2 obsblock
                   o1 = utils.parseObsblock(commands.s.getScriptString(INDX_STR_OBSBLOCK_ID))
                   o2 = utils.parseObsblock(commands.s.getScriptString(INDX_STR_OBSBLOCK_ID2))
                   commands.multiSubarray('setAllObsblocks', commands.DEFAULT, 
                                o1['project'], o1['obsblock'], str(o1['subobsblock']), int(o1['trial']),
                                o2['project'], o2['obsblock'], str(o2['subobsblock']), int(o2['trial']))

                   # Get obsblocks
                   sci1Obsblock = commands.s.getScriptString(INDX_STR_OBSBLOCK_ID).split('.')
                   sci2Obsblock = commands.s.getScriptString(INDX_STR_OBSBLOCK_ID2).split('.')
                else:
                   raise Exception,'Error setting sci1 and sci2 obsblock names'

                # We cannot have "none" for a project
                if sci1Obsblock[0].upper() == "NONE":
                   raise Exception,'The project code for the sci1 obsblock is "NONE"'
                if sci2Obsblock[0].upper() == "NONE":
                   raise Exception,'The project code for the sci2 obsblock is "NONE"'
                 
                # For PACS tracks, make sure subobsblock is 'PACS'
                if runPacs and (len(sci2Obsblock) != 4 or sci2Obsblock[2] != 'PACS'):
                   raise Exception,'Subobsblock name for sci2 is not "PACS"'

                # For MAXSECS tracks, make sure subobsblock is 'SL' and 'WB'
                if var['maxsens']:
                   if len(sci1Obsblock) != 4 or sci1Obsblock[2] != 'SL':
                      raise Exception,'Subobsblock name for sci2 should be "SL"'
                   if len(sci2Obsblock) != 4 or sci2Obsblock[2] != 'WB':
                      raise Exception,'Subobsblock name for sci2 should be "WB"'

                # Check project, code, and trial
                i1 = [0, 1, len(sci1Obsblock)-1]
                i2 = [0, 1, len(sci2Obsblock)-1]
                name = ['project', 'code', 'trial']
                for i in range(len(i1)):
                   if sci1Obsblock[i1[i]] != sci2Obsblock[i2[i]]:
                      raise Exception, name[i] + ' do not match for sci1 and sci2. You can not use restart for this track.'
          else:
             msg  = 'The script cannot be restarted since the obsblock name is currently %s.\n' % obsblockName
             msg += 'You can resert the script with the obsblock %s by adding the option resetObsblock=True.\n' % commands.s.getScriptString(INDX_STR_OBSBLOCK_ID)
             msg += 'You should only do this if you are CERTAIN it is ok to append data to this obsblock.\n'
             raise Exception, msg

          # Get the obsblock name currently in the control system
          commands.sleep(1)
          obsblockName = utils.getObsblockName()

    if not restart or \
       obsblockName != commands.s.getScriptString(INDX_STR_OBSBLOCK_ID):
        obsblockName = commands.newProject(projectInfo['code'], tmpObsblock, 
            tmpSubobsblock, pacs=runPacs, maxsens=var['maxsens'])

    # Get the obsblock name. Sometimes the obsblock name is not updated
    # immediately, so I use the obsblock name returned from the newProject 
    # command
    commands.trackMessage('Obsblock name   : ' + obsblockName, indent=1)
    commands.s.setScriptString(INDX_STR_OBSBLOCK_ID, obsblockName)
    if runPacs or var['maxsens']:
       obsblockNameSci2 = commands.queryString('Control.WidebandCorrelator.obsblockid')
       if runPacs:
          commands.s2.setScriptString(INDX_STR_OBSBLOCK_ID, obsblockNameSci2)
       else:
          commands.s.setScriptString(INDX_STR_OBSBLOCK_ID2, obsblockNameSci2)

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
            fout = open(outputFile, 'w')
            fout.write(utils.getTrackHistory())
            fout.close()
        except Exception, ex:
            outputFile = None
    if outputFile == None:
        commands.trackMessage('WARNING: Could not create output file for script log')
        outputFile = ''
    commands.s.setScriptString(INDX_STR_SCRIPTLOG, outputFile)

    # Load user catalog
    if projectInfo['sourceCatalog'] <> None: 
        if runPacs: sa = commands.BOTH
        else:       sa = commands.DEFAULT
        commands.ucat(projectInfo['sourceCatalog'], subarray=sa)

    # Re-issue doppler command just in case the linelength was re-initialized
    if restart:
        d = utils.getSourceName(sources['target'], n=1, parse=True)
        if sources['doppler'] <> None: d = sources['doppler']
        if d != 'None': commands.doppler(d)

    # Constraints
    commands.constraints()

    # Check mosaic file
    if utils.isMosaic(sources['mosaicTarget']) and mosaic['offsetFile'] <> None:
        inputFile = utils.setMosaicFile(mosaic['offsetFile'])
        if not os.path.exists(inputFile):
            raise Exception, 'Cannot find mosaic file '+inputFile

    # Reset integration time to the same size as sources
    dummy, sources['tintTarget'] = \
        utils.setSourceVariable(sources['target'], sources['tintTarget'], 0)
    dummy, sources['intentTarget']  = \
        utils.setSourceVariable(sources['target'], sources['intentTarget'], 'O')

    # Set time stamps if needed
    if not commands.s.getScriptBool(INDX_BOOL_DATE):
        commands.addScriptString(INDX_STR_DATE, utils.getLocalTime(), 
            bindx=INDX_BOOL_DATE)
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
            if a <= 6 or a > 15: continue

            # Set monitor point
            m = utils.getAntennaMp(a) + \
                    '.AntennaCommon.Drive.Track.actualElevation'

            # Get elevation
            if commands.queryDouble(m) < MINIMUM_ELEVATION_TUNING:
                antlist.append(a)

        # Move 6m antennas in position for tuning (don't wait here)?
        if len(antlist) > 0:
            commands.trackMessage('Moving antennas ' + 
                  utils.list2string(antlist) + ' to ' + 
                  str(MINIMUM_ELEVATION_TUNING) + 
                  ' deg elevation for tuning')
            commands.move(el=MINIMUM_ELEVATION_TUNING, ants=antlist)
        
        # Tune Sci2?
        if runPacs and commands.s.getScriptBool(INDX_BOOL_TUNE) and var['tunePacs']:
            commands.scriptClear(subarray=commands.SCI2)
            commands.trackMessage('Tuning Sci2 to 35.938 GHz')
            commands.freq(35.938, commands.LSB, 0, None, subarray=commands.SCI2)
            commands.trackMessage('Configuring correlator for Sci2 with configwideastroband')
            commands.configwideastroband(subarray=commands.SCI2)

        # Wait on any 6m antennas that were moved to pre-position for tuing
        if len(antlist) > 0:
            commands.wait(commands.TRACK, ants=antlist, waiton=limits['antwait'])


def needPol(interval):
    """ Returns True or False if it is time to perform a polarization calibration
        observations.

        Inputs: interval    - time in hours between polarization observations
                              passband observations.
        Outputs: True or False, indicating if it is time for a polarization observation.
    """

    # Initialize
    observe = False

    # Check if observations needs to be performed base on time
    tlastpol = utils.getLstLastPolarization()
    if interval <> None:
       if tlastpol == None:
           observe = True
       elif utils.timeDifference(commands.lst(), tlastpol, pos=True) >= interval:
           observe = True

    return observe

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
        # resetMountOffsets = True # Removing for now - not clear it is doing any good
        resetMountOffsets = False

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


def measureOptradVector(pointing, force=False, preferredonly=True):
    """ Measures the optical-radio pointing vector, if needed. 

        pointing  : pointing options
        force     : If True, then force pointing
    """
    # Measure the vector
    if force or \
       (pointing['doOptPoint'] \
       and commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT) \
       and not commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) and
       (not preferredonly or pointing['optradPreferred'] <> None)):
         print 'Measuring optical-radio pointing vector'
         doPoint(pointing, preferred=pointing['optradPreferred'], 
               tune95Pref=pointing['optradTune95'], preferredonly=preferredonly)

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
    pointing['ambient'] = False
    pointing['elmin']   = 20.0
    pointing['elmax']   = 80.0
    pointing['antwait'] = -2
    pointing['mapPoints'] = utils.getVariable(pointing,'mapPoints',5,nonone=True)
    pointing['nrepCross'] = utils.getVariable(pointing,'nrepCross',1,nonone=True)
    pointing['nrepInt'] = utils.getVariable(pointing,'nrepInt',1,nonone=True)
    pointing['tune95']    = utils.getVariable(pointing,'tune95',True,nonone=True)
    pointing['tune95Pref'] = utils.getVariable(pointing,'tune95Pref',pointing['tune95'],nonone=True)
    pointing['intervalTran'] = utils.getVariable(pointing,'intervalTran',None)
    pointing['doPointSunRiseSet'] = utils.getVariable(pointing,'doPointSunRiseSet',pointing['doPointNight'] or pointing['doPointDay'])

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


def doPoint(options,             ref=None,
            crossTerminus=None,  printHeader=True, start=False, 
            force=False,         maxsep=None,      preferred=None,   
            interval=None,       indent=1,         tune95Pref=None,
            elmin=None,          elmax=None,       preferredonly=False,
            checkSource=False):
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

        Inputs: options     - pointing options; see template3.obs
                ref         - Name of reference source
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
                checkSource   - If True, then determine only if source is available for pointing.
                                But the source itself is not observed.
        Output: True/False  - If RADIO pointing was attempted. 
                              if checkSource = True, then return True/False that a pointing source is available.
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
                           minsepsun=options['minsepsun'],
                           maxsep=maxsep, excludeExpref=excludeExpref)

        # If we are only checking the source, then return
        if checkSource: return (source <> None)

        # If source is preferred source, check tune95Pref.
        # If no source was found, then print message
        if source == None:
            # Print message
            commands.trackMessage("no radio pointing source available", indent=indent)

            # If pointing was not successful but the previous pointing was on
            # a "bad" part of the sky, we must zero out the pointing offsets
            # if forceDistance: utils.zeroPointingOffsets(bima=True)
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
        # if runPacs: 
           # commands.trackMessage('Starting Sci2 radio pointing', indent=indent)
           #TODO - do point for SCI2
           #pacs.point()

        # Message
        ae = commands.azel(source)
        if tune95Point: 
            commands.trackMessage('tuning to 3mm for pointing',indent=indent)
        msg = 'pointing on ' + source + ' az=' + str('%.1f' % ae[0]) + \
                  '  el=' + str('%.1f' % ae[1])
        commands.trackMessage(msg, indent=indent)

        # Issue track command. Wait a few xtra secs for all antennas to arrive
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
                        antwait=options['antwait'], useRefPoint=False,
                        antennas=options['antennas'], 
                        timeLimit=options['timeLimit'],
                        pntwait=options['pntwait'],
                        waitCycles=options['waitCycles'])

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
                print "Begin radioPoint"
                results = radioPoint.radioPoint(source, type='triangle',
                        tune95=tune95Point, antwait=options['antwait'],
                        antennas=options['antennas'], 
                        timeLimit=options['timeLimit'],
                        pntwait=options['pntwait'],
                        waitCycles=options['waitCycles'])
                print "Radio pointing is complete"

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
                        msg += '     *** Radio pointing did not converge ***'
                        if i >= 7 and i <= 15: badlist.append(i)
                    commands.trackMessage(msg, indent=jindent)
                # if forceDistance and len(badlist) > 0: 
                #   utils.zeroPointingOffsets(badlist)

                # Optical pointing results
                if doOptPoint:
                    goodVals = list()
                    msg = 'Optical pointing on %s (V=%.2f mag)' % (opticalSource, optMag)
                    commands.trackMessage(msg, indent=jindent)
                    for i in commands.currentAntennaNumbers() :
                        if i<=15 and i not in options['badOptAnts'] : goodVals.append(i)
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
                            elif i >= 16:
                                msg += '      *** No optical cameras on the 3.5 m antennas ***'
                            else:
                                msg += '      *** Optical-radio difference not measured    ***'
                            commands.trackMessage(msg,indent=jindent)            

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
                         integers in obsdef3.py. Possible values are:
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


def doPassband(options,          noise, 
               pointing=None,    lststop=None,    ref=None, 
               maximumTime=None, force=False,    printHeader=True, 
               start=False,      email=None):
    """ Observe a passband calibrator with an elevation greater than the
        system limit and below elmax. The passband calibrator is first
        searched in "preferred", and if no suitable sources are found, then in
        sourceList. The observed passband source is chosen based upon:
           1) If ref is set, the source closest to "ref"
           2) If ref is not set, the sources highest in the sky.

        Inputs: options     - passband options; see template3.obs
                noise       - noise source options; see template3.obs
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
                email       - If True, then send email to observers after 
                              passband calibrator has been observed. If False, do
                              not send an email. If None, only send an email if
                              this is the first call to doPassband.
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

    # Get LO frequency
    lofreq = commands.freqSetup()[2]

    # Passband calibrators to avoid
    # BLLAC/0355+508/0212+735: Contain spectral lines in some velocities. 
    #                 Lucas & Liszt 1994, A&A, 282, L5
    # 0355+508 = 0359+509 = NRA150 
    # 0212+735 = 0212+735
    # W3OH     - not sure why it is even in the catalog
    excludeExpref = ['BLLAC', '2202+422','2200+420',
               '0355+508', 'NRA150', '0359+509', 
               '0212+735', '0217+738', 'W3OH', '3c111'
               ]
    if lofreq < 40.0: excludeExpref.append('3c273')

    # Get passband calibrator
    # sourceList  = utils.getBrightSources(options['minflux'], freq=lofreq)
    # passbandCal = utils.getSource(sourceList, preferred=options['preferred'], ref=ref,
    #                         elmin=options['elmin'], elmax=options['elmax'],
    #                         excludeExpref=excludeExpref, minsepsun=options['minsepsun'],
    #                         timeAvailable=options['tint'])
    # if passbandCal == None: 
    #     commands.trackMessage("No passband calibrator available", indent=1)
    #     return ok
    # else:
    #     if printHeader: utils.printWeather()
    #     commands.trackMessage("slewing to " + passbandCal + " for passband", indent=1)

    # Point
    # if pointing <> None and (options['doPoint'] or options['forcePoint']):
    #     doPoint(pointing, ref=passbandCal,
    #             maxsep=options['maxsep'], force=options['forcePoint'], 
    #             preferred=options['preferredPointing'], printHeader=False)
    # elif commands.subarrayNo == 2:
    #     # Force a single cross
    #     commands.trackMessage("running a pointing cross first --- offsets will not be applied", indent=1)
    #     ants = None
    #     if pointing <> None and pointing.has_key('antennas'): ants = pointing['antennas']
    #     results = radioPoint.radioPoint(passbandCal, tol=1000.0, type='cross', timeLimit=0.1, apply=False, antennas=ants)

    # Observe passband calibrator in current correlator configuration
    # Set elevation limit to None since elevation limit was checked above
    # in selecting the source. This way, the observation will not be cut off
    # if it falls below the actual elevation limit during the integration.
    intentObject = 'B'
    selfcal = True
    if email == True or (email == None and commands.s.getScriptInt(INDX_INT_NOBS_PB)==0):
      print ''
      print '*******************************'
      print '*      GOT COHERENCE?         *'
      print '*******************************'
      print ''
      utils.sendEmailMessage('obs@mmarray.org', 'obs@mmarray.org', 'The passband calibrator is about to be observed.\nPlease check the gains page at\n   http://cedarflat.mmarray.org/gains/index_sci%d.html\nto verify that all of the antennas have coherence.' % commands.subarrayNo, subject='Got coherence?')
    ok = observeSource(passbandCal, options['tint'], options['record'],
                       tsysTime=options['tsys'], lststop=lststop, 
                       maximumTime=maximumTime, noise=noise,
                       tmo=options['tmo'], elmin=None,
                       antwait=options['antwait'], extraWait=EXTRAWAIT,
                       intentObject=intentObject, selfcal=selfcal,
                       ambient=options['ambient'], runMiriad=True)

    # Done. "ok" indicates if passband calibrator was observed.
    if ok:
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


def doFluxcal(options,          primaryCal,  noise, 
              pointing=None,    ref=None,    lststop=None, 
              printHeader=True, force=False, start=False):
    """ Observe a flux calibrator with the elevation limits in fluxcal.
        The flux calibrator is first searched in "preferred", and if no 
        suitable sources are found, then in fluxSources. The observed flux 
        calibrator is chosen based upon:
           1) If ref is set, the source closest to "ref"
           2) If ref is not set, the sources highest in the sky.

        Inputs: options     - flux calibration options; see template3.obs
                primaryCal  - dictionary of primary flux calibrators;
                              see template3.obs
                ref         - Name of reference source
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
              primaryCal, nprimary, allowDup, 
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
              ref=ref, noise=noise, lststop=lststop, 
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
                           allowDup,        ref=None,
                           noise=None,      secondary=False, lststop=None, 
                           pointing=None):
    """ Observe up to nsources in sourceList. 

        Inputs: options    - flux calibration options; see template3.obs
                sourceList - list of sources that are ok to observe
                nsources   - maximum number of sources to observe
                allowDup   - observe duplicate source from previous observation
                ref        - name of reference source
                noise      - noise options; see template3.obs
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
                               timeAvailable=options['tint'], 
                               minsepsun=options['minsepsun'])
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
                doPoint(pointing, ref=sourceName, 
                       maxsep=options['maxsep'], force=options['forcePoint'],
                       preferred=preferredPointing, indent=2,
                       printHeader=False, interval=options['intervalPointing'])
                
            # Observe source
            okSource = observeSource(sourceName, options['tint'], 
                           options['record'], noise=noise, 
                           tsysTime=options['tsys'], antwait=options['antwait'],
                           lststop=lststop, 
                           tmo=options['tmo'], indent=2, extraWait=EXTRAWAIT,
                           intentObject='F', selfcal=True,
                           ambient=options['ambient'], runMiriad=True)
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
                      noise,     primaryCal=None, mosaic=None,
                      resetDoppler=None,       ncyclesMax=None):
    """ Cycle between phaseCal and sources.

        This function will cycle between phase calibrator and sources.
        phaseCal can be a list() that contains multiple phase calibrators
        and various start/stop times. This routine loops over the specified
        phase calibrators, and calls doSources for each calibrator in turn.

        Inputs: sources    - contains science targets and phase calibrators;
                             see template3.obs
                fluxcal    - flux calibrator options; see template3.obs
                pointing   - pointing options; see template3.obs
                passband   - passband options; see template3.obs
                noise      - noise source options; see template3.obs
                primaryCal - list of primary flux calibrators; see template3.obs
                mosaic     - mosaic options; see template3.obs
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
    tintPhase = utils.makeListObsdef(sources['tintPhaseCal'], parse=False)
    if phase == None: 
        phase = [None]
        tintPhase = [None]

    # The obsdef script cannot accept time ranges longer than 12 hours 
    # because it cannot elegantly handle the 24 hour wrap in time.
    # Therefore, if the phase cal stop-start time is more than 12 hours,
    # I break the calibrator into pieces.
    phase_orig = phase
    phase = utils.splitPhaseCalList(phase_orig)

    # Get reference source if we need to observe calibrators
    srefCal = utils.getSourceName(sources['target'], n=1, parse=True)

    # Loop over phase calibrators
    for i in range(len(phase)):
        # Read phase calibrator name, and optionally start/stop time
        pname, pstart, pstop, pcaltime = utils.getPhaseCal(phase[i], returnCaltime=True)

        # Get integration time for this phase calibrator
        ptint = tintPhase[len(tintPhase)-1]
        if i < len(tintPhase): ptint = tintPhase[i]

        # Ready to go
        action = pname
        if action <> None: action = action.upper()
        if action == "BANDPASS" or action == "PASSBAND":
            if not finishedPassband(passband):
                doPassband(passband, noise, pointing=pointing, ref=srefCal)
        elif action == "FLUX" or action == "FLUXCAL":
            if not finishedFluxcal(fluxcal):
                doFluxcal(fluxcal, primaryCal, noise, pointing, ref=srefCal)
        elif action == "POINT":
            doPoint(pointing, ref=srefCal, force=True)
        elif pcaltime <> None and pcaltime > 0:
            if pstart == None or utils.timeDifference(lst(),pstart) > 0:
                commands.trackMessage('Observing extra source')
                observeSource(pname, pcaltime,
                   sources['record'], tsysTime=sources['tsys'], 
                   noise=noise,
                   lststop=utils.getLstStopCycle(default=pstop),
                   tmo=sources['tmo'], antwait=sources['antwait'],
                   intentObject='G', selfcal=True,
                   ambient=sources['ambient'], runMiriad=False)
        elif sources['doSnapshot']:
            doSnapshot(sources, pname, pointing, passband, 
                  fluxcal, noise, primaryCal=primaryCal, 
                  pstop=pstop, resetDoppler=resetDoppler)
        elif utils.timeRemaining(source=sources['target'], phase=pname, 
                           lsttime=pstart, toRise=True) <= 0.0:
            if i == 0 and commands.s.getScriptInt(INDX_INT_NOBS_PB) == 0:
               print ''
               print '*******************************'
               print '*      GOT COHERENCE?         *'
               print '*******************************'
               utils.sendEmailMessage('obs@mmarray.org', 'obs@mmarray.org', 'A calibrator is about to be observed for the first time.\nPlease check the gains page at\n   http://cedarflat.mmarray.org/gains/index_sci%d.html\nto verify that all of the antennas have coherence.' % commands.subarrayNo, subject='Got coherence?')
            doCycles(sources, pname, pointing, passband, fluxcal, mosaic,
                     noise, primaryCal=primaryCal, tintPhaseCal=ptint,
                     pstop=pstop, resetDoppler=resetDoppler, 
                     ncyclesMax=ncyclesMax)


def doCycles(sources,         phaseCal,     pointing,     passband, 
             fluxcal,         mosaic,       noise, 
             primaryCal=None, pstop=None,   resetDoppler=False,
             ncyclesMax=None, tintPhaseCal=None):
    """ Cycle between phaseCal and sources and no sources are observed or time limit is reached.

        Inputs: sources    - contains science targets and phase calibrators;
                             see template3.obs
                phaseCal   - phase calibrator to use for this cycle
                fluxcal    - flux calibrator options; see template3.obs
                mosaic     - mosaic options; see template3.obs
                noise      - noise source options; see template3.obs
                pointing   - pointing options; see template3.obs
                passband   - passband options; see template3.obs
                primaryCal - list of primary flux calibrators; see template3.obs
                pstop      - ending LST time for this phase calibrator
                resetDoppler   : If true, then set the doppler command
                                 for each source in "sources"
                tintPhaseCal - integration time in minutes on phase calibrator.
                               Overrides sources['tintPhaseCal']

         Output: None
    """
    # Set overhead factor. 60 converts minutes to hours
    foverhead = utils.overhead() / 60.0

    # Nominal passband
    timePb = foverhead * passband['tint']

    # Flux calibrators
    timeFlux = foverhead * fluxcal['tint']

    # Last calibrator
    tintPhase = sources['tintPhaseCal']
    if tintPhaseCal <> None: tintPhase = tintPhaseCal
    timeLastcal = foverhead * tintPhase

    # Source observations
    sou, t = utils.setSourceVariable(sources['target'], sources['tintTarget'], 0.0)
    intents = utils.setSourceVariable(sources['target'], sources['intentTarget'], 'S')[1]
    pointstatus = utils.setSourceVariable(sources['target'], sources['pointstatusTarget'], commands.ONSRC)[1]
    if utils.isMosaic(sources['mosaicTarget']):
        timeCycle = timeLastcal + \
                       foverhead * (t[0] * mosaic['nphase'] + sum(t[1:]))
    else:
        timeCycle = timeLastcal + foverhead * sum(t)

    # PACS sources
    runPacs = commands.s.getScriptBool(INDX_BOOL_PACS)
    souPacs = [None] * len(sou)
    phaseCalPacs = None
    if runPacs:
        pacsSources  = sources['pacs']
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

    # Determine if these are full stokes observations
    isFullStokes = False
    if commands.subarrayNo == 1:
        iscarma23, astrobands = utils.getCorrelatorMode()
        polarizations = [ commands.queryString("SignalPath.Mapping.Astroband%d.confTag" % (ab)) for ab in astrobands ]
        isFullStokes = 'FULLSTOKES' in polarizations
        
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
           doPoint(pointing, ref=srefCal, printHeader=False)

        # Observe phase calibrator
        if phaseCal == None:
            okCal = True
        elif utils.continueObservations() and tremaining > 0:
            okCal = observeSource(phaseCal, tintPhase, 
                         sources['record'], tsysTime=sources['tsys'], 
                         lststop=utils.getLstStopCycle(default=pstop),
                         tmo=sources['tmo'], antwait=sources['antwait'],
                         intentObject='G', selfcal=True,
                         noise=noise, pacsSources=phaseCalPacs,
                         ambient=sources['ambient'], runMiriad=True)
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
                              printHeader=False)
            # If we do not require that radio pointing brackets the 
            # phase calibrator, then set "pointed" to False.
            if not pointing['bracketPhasecal']: pointed = False

        # Calibrate polarization
        if isFullStokes and \
           sources['intervalPol'] <> None and \
           needPol(sources['intervalPol']) and \
           utils.continueObservations(isScience=True):
            utils.observePol(sources['tintPol'], sources['record'], verbose=True, indent=1)

        # Flux calibrator
        observedFluxcal = False
        if not finishedFluxcal(fluxcal) and fluxcal['middle'] and \
           utils.continueObservations(isScience=True) and \
           utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0:
            observedFluxcal = doFluxcal(fluxcal, primaryCal, noise, 
                  pointing, ref=srefCal, lststop=pstop, printHeader=False)

        # Passband calibrator
        observedPassband = False
        if not finishedPassband(passband) and passband['middle'] and \
           utils.continueObservations(isScience=True) and \
           utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0:
            observedPassband = doPassband(passband, noise,
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
        nsources   = 0
        tremaining = utils.timeRemaining(source=sou, phase=phaseCal, useTime=True,
                                   lsttime=utils.getLstStopCycle(default=pstop),
                                   tneed=tneed)
        if not pointed and not observedFluxcal and not observedPassband and \
           utils.continueObservations(isScience=True) and tremaining > 0.0:
            # Check receivers are still locked for sci2.
            # We do this in sci2 only since the tertiary is unstable.
            if commands.subarrayNo == 2: utils.relockReceivers(indent=1)

            # Check optical pointing. Keep track of time spent pointing
            # and subtract from tremaining
            t1 = commands.lst()
            if commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) and \
               commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT) and \
               pointing['optPointTarget']:
                xt1 = commands.lst()
                doPoint(pointing, ref=srefTarget, printHeader=False)
            t1 = utils.timeDifference(commands.lst(), t1, pos=True)

            # Observe source
            tremaining -= t1
            mstart = commands.lst()
            mosaic_list = utils.makeListObsdef(sources['mosaicTarget'])
            nsources = 0
            tsysdict = initializeTsysDictionary()
            for i in range(len(sou)):
                # Default mosaic_source to the last entry in mosaic_list
                mosaic_source = mosaic_list[len(mosaic_list)-1]
                if i < len(mosaic_list): mosaic_source = mosaic_list[i]

                # Update time remaining for this source
                tremaining = utils.timeRemaining(source=sou[i], phase=phaseCal,
                     useTime=True, lsttime=utils.getLstStopCycle(default=pstop),
                     tneed=tneed)

                # Observe source
                inttime = 0.0
                if mosaic_source:
                    ok, xt = observeMosaic(sou[i], t[i], mosaic, sources,
                               maximumTime=tremaining, 
                               lststop=utils.getLstStopCycle(default=pstop),
                               pacsSources=souPacs[i], noise=noise,
                               intent=intents[i], pointstatus=pointstatus[i],
                               tsysdict=tsysdict, index=i)
                    inttime += xt
                else:
                    ok = False
                    # Run noise source for pacs observations
                    obsnoise = None
                    if noise.has_key('tsample') and (noise['tsample'] <> None): 
                          obsnoise = noise
                    ok = observeSource(sou[i], t[i], sources['record'], 
                          tsysTime=sources['tsys'], isScience=True, 
                          lststop=utils.getLstStopCycle(default=pstop),
                          maximumTime=tremaining, tmo=sources['tmo'],
                          antwait=sources['antwait'], noise=obsnoise,
                          intentObject='O', selfcal=False,
                          ambient=sources['ambient'], pacsSources=souPacs[i],
                          pointstatus=pointstatus[i], tsysdict=tsysdict, runMiriad=(intents[i]=='G'))
                    if ok: inttime = t[i]
                okSource = okSource or ok
                if ok: nsources += 1
            # utils.addSourceTime(utils.timeDifference(commands.lst(), mstart, pos=True))
            utils.addSourceTime(inttime/60.0)

        # Stop if nothing was observed or we have reached the time limit.
        # Also stop if we do not have time to finish remaining observations.
        tremaining = utils.timeRemaining(source=sou, phase=phaseCal, 
                    useTime=True, lsttime=utils.getLstStopCycle(default=pstop),
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
        observeSource(phaseCal, tintPhase, sources['record'],
                     noise=noise, tsysTime=sources['tsys'], 
                     pacsSources=phaseCalPacs, isScience=False,
                     tmo=sources['tmo'],
                     antwait=sources['antwait'], intentObject='G', 
                     selfcal=True, ambient=sources['ambient'], runMiriad=True)

    # See why we are here
    if utils.isEndOfTrack():
        commands.trackMessage('Ending observations based on endtrack()')
    elif utils.isLastcal(): 
        commands.trackMessage('Ending cycle based on lastcal()')
    elif utils.isStopAfterNextCal():
        commands.trackMessage('Ending cycle based on stopAfterNextCal()')


def observeSource(sources,          tint,                trecordDefault,
                  lststop=None,     tsysTime=None,       maximumTime=None,
                  isScience=False,  tmo=None,            resetDoppler=False,  
                  antwait=-2,       elmin=None,          indent=1,
                  extraWait=0,      intentObject="O",    selfcal=False,
                  returnTint=False, ambient=False,       runMiriad=False,
                  runPacs=None,     pacsSources=None,    pointstatus=commands.ONSRC,  
                  noise=None,       tsysdict=None):
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
               trecordDefault - record length in seconds for the integrations
                            if tint < trecordDefault, then tint is used as the record length
               lstStop    - stopping LST time in HH:MM:SS for the observations
               tsysTime   - Measure tsys every tsysTime minutes. Tsys is always
                            measured when moving to a source for the first time.
               maximumTime- Maximum time in hours to spend observing the sources
               isScience  - If true, then observing science target. This is 
                            needed to makr lastcal() and stopAfterNextCal()
                            have their intended affect.
               noise      - Noise source dictionary
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
               pointstatus - point status for the source
               tsysdict    - tsys dictionary that keeps track of integration time since last
                             tsys. Before, this well self-contained when all sources to be
                             observed were passed to this function. But now, the function
                             is calld for each source separately. tsysdict is a dictionary
                             since numerical variables are passed by value only.
 
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
    if not utils.continueObservations(isScience=isScience): 
        return observedSource

    # Put inputs in a list
    sou, integTime = utils.setSourceVariable(sources, tint, 0.0)
    intents = utils.setSourceVariable(sources, intentObject, 'S')[1]
    status = utils.setSourceVariable(sources, pointstatus, commands.ONSRC)[1]
 
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
    
    # Subarray selection
    sa = commands.DEFAULT
    if runPacs: sa = commands.BOTH
    SCI2 = commands.SCI2
    BOTH = commands.BOTH

    # Turn flux cal off if this the science target
    fluxcalOn = True
    if intentObject == "O": 
       commands.applyTsys(False)
       commands.applyFlux(False)
       fluxcalOn = False

    # Loop over sources
    tint_actual  = 0.0
    tint_maximum = 0.0
    if tsysdict == None: tsysdict = initializeTsysDictionary()
    # tsysdict['tintSinceLastTsys'] = None
    # tsysdict['tintSinceLastNoise'] = None
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
            tremaining = min(tremaining, (maximumTime - utils.timeDifference(commands.lst(),tstart,pos=True)) * 60.0)

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
            trecord = min([integrationTime*60.0, trecordDefault])
            if isNoiseSource:
                nreps = utils.getNreps(integrationTime, trecord)
            else:
                tsampleNoise = None
                if noise <> None and noise.has_key('tsample'): 
                    tsampleNoise = noise['tsample']
                nreps = utils.getNreps(integrationTime, trecord, 
                              tsysMinutes=tsysTime, tsampleMinutes=tsampleNoise,
                              tintSinceLastTsys=tsysdict['tintSinceLastTsys'],
                              tintSinceLastSample=tsysdict['tintSinceLastNoise'])

            # Track astronomical source. 
            # Observe noise source first if noise tint != None.
            if not isNoiseSource: 
                # Reset doppler
                if resetDoppler: commands.doppler(sou[isource])

                # Noise source
                if noise <> None:
                    commands.track(sou[isource])
                    utils.observeNoise(noise['tint'], noise['record'], 
                                verbose=True, indent=indent, pacs=runPacs)
                    tsysdict['tintSinceLastNoise'] = 0

                # Track source for PACS
                if runPacs: 
                    commands.trackMessage(souPacs[isource] + ' (sc12)', 
                            indent=indent)
                    intentPacs = string.upper(intents[isource])
                    if intentPacs == 'S' or intentPacs == 'O': intentPacs = 'A'
                    #pacs.observe(souPacs[isource], intentPacs)
                    commands.intent(souPacs[isource], intentPacs, subarray=SCI2)
                    commands.track(souPacs[isource], subarray=SCI2)

                # Track source
                commands.pointstatus(status[isource])
                commands.intent(sou[isource], intents[isource], selfcal)
                commands.track(sou[isource])
                #commands.track(sou[isource], waiton=antwait)
                #if extraWait > 0: 
                #    commands.track(sou[isource], waiton=commands.ALL, tmo=extraWait)

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
                       tsysdict['tintSinceLastNoise'] >= tsampleNoise:
                        utils.observeNoise(noise['tint'], noise['record'], 
                                    verbose=True, indent=indent, pacs=runPacs)
                        tsysdict['tintSinceLastNoise'] = 0

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
                    elif tsysTime < 0.0 and tsysdict['tintSinceLastTsys'] <> None and \
                         tsysdict['tintSinceLastTsys'] < abs(tsysTime):
                        measureTsys = False
                    elif tsampleNoise <> None and tsysdict['tintSinceLastTsys'] <> None \
                         and tsys > 0 and tsysdict['tintSinceLastTsys'] < tsysTime and \
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
                        # Wait for antennas to be at final elevation
                        # commands.wait(commands.TRACK,tmo=500,
                        #         waiton=commands.ALL, subarray=sa)  # Original
                        if commands.subarrayNo == 2:
                           commands.wait(commands.TRACK,tmo=tmo, # XXX Added by JMC for PACS
                                waiton=antwait, subarray=commands.SCI2)
                        else:
                           commands.wait(commands.TRACK,tmo=tmo, # XXX Added by JMC for PACS
                                waiton=antwait, subarray=commands.SCI1)
                        commands.tsys()
                        if runPacs: 
                           commands.tsys(subarray=SCI2)
                        tsysdict['tintSinceLastTsys'] = 0.0

                    # Integrate
                    sourcePrevious = sou[isource]
                    if ambient: commands.amb(subarray=sa);
                    if isNoiseSource:
                        trecnoise = trecord
                        if noise <> None: trecnoise = noise['record']
                        utils.observeNoise(trecord*n, trecnoise, verbose=True,
                                subarray=sa)
                        tsysdict['tintSinceLastNoise'] = 0
                    else:
                        # Check receivers are still locked for sci2.
                        # We do this in sci2 only since the tertiary is unstable.
                        if commands.subarrayNo == 2: 
                            utils.relockReceivers(indent=1)
                            commands.wait(commands.TRACK,tmo=tmo, # XXX Added by JMC for PACS
                                waiton=antwait, subarray=commands.SCI2)
                        else:
                            commands.wait(commands.TRACK,tmo=tmo, # XXX Added by JMC for PACS
                                waiton=antwait, subarray=commands.SCI1)
                        commands.integrate(trecord, n, antwait=commands.NONE, subarray=sa) # XXX Added by JMC for PACS
                        # commands.integrate(trecord, n, antwait=antwait, tmo=tmo, # Original
                        #     subarray=sa)
                        tint_actual += trecord * n
                        tint_maximum += integTime[isource]
                        if tsysdict['tintSinceLastTsys'] <> None: 
                            tsysdict['tintSinceLastTsys'] += (trecord * n) / 60.0
                        if tsysdict['tintSinceLastNoise'] <> None: 
                            tsysdict['tintSinceLastNoise'] += (trecord * n) / 60.0
                    if ambient: commands.sky(subarray=sa);

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
                com = '/usr/local/bin/lockrun --lockfile=/array/obs/bin/lockrun.sci%d -- /array/obs/bin/gainCheckSci%d.csh ' % (commands.subarrayNo, commands.subarrayNo) + \
                      'vis=' + miriadFile + ' ' + \
                      'refant=' + str(refAnt) + ' ' + \
                      'subarray=' + str(commands.subarrayNo)
                if runPacs:
                    pacsObsblock = commands.queryString('Control.subarray2.ObsblockId')
                    com += ' pacs=' + pacsObsblock + " " + \
                        'config=' + commands.queryString(utils._controlMPprefix() + 'configName')
                tt = os.popen3(com)
                tt[0].close()
                tt[1].close()
                tt[2].close()

    # Turn flux cal back on
    if not fluxcalOn:
       commands.applyTsys(True)
       commands.applyFlux(True)
       fluxcalOn = True

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


def observeMosaic(souMosaic, integTimeMosaic, mosaic, sources, lststop=None, 
                  maximumTime=None, pacsSources=None, noise=None, 
                  intent='S', pointstatus=commands.ONSRC, tsysdict=None,
                  index=0):
    """ Make mosaic

        Inputs: souMosaic     - source to observe
                integTimeMosaic  - integration times
                mosaic      - mosaic parameters
                sources     - sources dictionary
                noise       - noise parameters
                lststop     - stopping LST time for mosaic
                maximumTime - [hours] Maximum time to spend observing the mosaic
                tsysdict    - tsys dictionary that keeps track of integration time since last
                              tsys. Before, this well self-contained when all sources to be
                              observed were passed to this function. But now, the function
                              is calld for each source separately. tsysdict is a dictionary
                              since numerical variables are passed by value only.
                index       - Index for the otf parameters (e.g., fieldtime, rows, delta, etc...)

        Output: A two element list.
                The first element indicates True/False if at least one positions was observed.
                The second eleement gives the integration time in minutes.
    """
    # Initialize
    tstart = commands.lst()
    observedSource = False
    commands.pointstatus(pointstatus)

    # PACS
    runPacs = commands.s.getScriptBool(INDX_BOOL_PACS)
    souPacs = None
    if runPacs:
        if pacsSources == None:
           souPacs = sou
        else:
           souPacs = pacsSources
    
    # Subarray selection
    sa = commands.DEFAULT
    if runPacs: sa = commands.BOTH
    SCI2 = commands.SCI2
    BOTH = commands.BOTH

    # Set mosaic offsets or initialize OTF mosaic. 
    # Offsets are stored in arcminutes within mosaic['offsetsArcmin'] keyword
    if not mosaic['otf'] and not mosaic.has_key('offsetsArcmin'):
        # Read offsets
        if mosaic.has_key('offsets') and mosaic['offsets'] <> None:
            commands.trackMessage('setting mosaic offsets from input list in script',indent=1)
            mosaic['offsetsArcmin'] = mosaic['offsets']
        elif mosaic.has_key('offsetFile') and mosaic['offsetFile'] <> None:
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
            stepSize = min(commands.getNyquistStep(commands.currentAntennaNumbers(), userFreq=utils.getHighestFrequency()))
        for i in range(len(zoff)):
             x = float(zoff[i][0]) * stepSize
             y = float(zoff[i][1]) * stepSize
             zoff[i] = [x,y]
        mosaic['offsetsArcmin'] = zoff

    # Copy mosaic offsets
    if not mosaic['otf']:
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
    integrationTimeTotal = 0
    if utils.timeRemaining(source=souMosaic, lsttime=lststop) > 0:
        # Track source
        if runPacs: 
            commands.trackMessage('slewing sci2 to ' + souPacs, indent=1)
            commands.track(souPacs, subarray=SCI2)
            commands.intent(souPacs, 'A', subarray=SCI2)
        commands.trackMessage('slewing to ' + souMosaic, indent=1)
        commands.intent(souMosaic, intent, False)
        commands.track(souMosaic)

        # Initialize cycle
        if tsysdict == None: tsysdict = initializeTsysDictionary()
        tintSinceLastOffPosition = None
        observedSourceInCycle = True
        tintOTF = 0      # Used by OTF mosaicking only
        nposObserved = 0 # Number of positions that have been observed. Not used for OTF

        # Now start a loop for the observations. One of the following is run during each loop:
        #   1) Observe a sky position
        #   2) Measure the system temperature
        #   3) Observe one position in a pointed mosaic
        #   4) Observe a OTF mosaic
        # If ONE of these is observed, then the loop is repeated.
        # If none are observed, then the loop is terminated.
        # The boolean observedSourceinCycle indicates if something has been observed.
        while observedSourceInCycle and utils.continueObservations(isScience=True):
            # Initialize
            observedSourceInCycle = False

            # Set integration time without exceeding time or elevation limits.
            tintMosaic = min(integTimeMosaic, utils.timeRemaining(source=souMosaic, lsttime=lststop) * 60)
            if maximumTime <> None: 
                tintMosaic = min(tintMosaic, (maximumTime - utils.timeDifference(commands.lst(),tstart,pos=True)) * 60.0)
            if tintMosaic < utils.minimumIntegrationTime() / 60.0: continue

            # Check if the mosaic is completed
            if mosaic['otf']: 
               if tintOTF >= integTimeMosaic:
                  continue
               elif mosaic['otfMosaic'][index].maxFieldIndex > 0 and \
                    mosaic['otfMosaic'][index].fieldIndex >= mosaic['otfMosaic'][index].maxFieldIndex:
                  continue
            elif not mosaic['otf'] and nposObserved >= nposCycle:
               continue

            # Noise measurement
            if tsampleNoise <> None and noise <> None and \
               (tsysdict['tintSinceLastNoise'] == None or 
                tsysdict['tintSinceLastNoise'] >= tsampleNoise):
                utils.observeNoise(noise['tint'], noise['record'], verbose=True, indent=1, pacs=runPacs)
                tsysdict['tintSinceLastNoise'] = 0
            elif tsysdict['tintSinceLastNoise'] == None:
                tsysdict['tintSinceLastNoise'] = 0

            # Tsys measurement  (break loop if measured)
            if tsysdict['tintSinceLastTsys'] == None or \
                 (sources['tsys'] <> None and tsysdict['tintSinceLastTsys'] >= abs(sources['tsys'])) :
                commands.trackMessage('Measuring tsys', indent=1)
                commands.wait(commands.TRACK,tmo=500, waiton=commands.ALL, subarray=sa)
                commands.tsys()
                if runPacs: commands.tsys(subarray=SCI2)

               # Break loop
                tsysdict['tintSinceLastTsys'] = 0.0
                observedSourceInCycle = True

            # Observe off position, if needed  (break look if observed)
            if mosaic['off'][index] <> None and \
               (tintSinceLastOffPosition == None or tintSinceLastOffPosition >= mosaic['intervalOff'][index]):
               # Set integration time
               nreps = utils.getNreps(mosaic['tintOff'][index], sources['record'], tsampleMinutes=tsampleNoise)
               tint = sources['record'] * nreps / 60.0

               # Offset
               t = str("%-10s" % 'Sky') + '   tint=' + \
                   str("%9s" % utils.dtString(tint/60.)) + '  el=' + \
                   str("%4.1f" % utils.getSourceElevation(souMosaic)) + \
                   "  pos   = %8.3f' %8.3f'" % (mosaic['off'][index][0], mosaic['off'][index][1])
               commands.trackMessage(t, indent=1)
               commands.equatOffset(mosaic['off'][index][0], mosaic['off'][index][1], subarray=sa)

               # Integrate, potentially on both subarrays
               commands.pointstatus(commands.OFFSRC)
               commands.integrate(sources['record'], nreps, antwait=sources['antwait'], subarray=sa)
               commands.pointstatus(pointstatus)

               # Keep track of time since calibrations
               tsysdict['tintSinceLastTsys']  += tint
               tsysdict['tintSinceLastNoise'] += tint
               tintSinceLastOffPosition = 0
               tintOTF += tint

               # Break loop
               observedSourceInCycle = True
               continue
            elif tintSinceLastOffPosition == None:
               tintSinceLastOffPosition = 0

            # We are guaranteed at this point to observe a position in the mosaic.
            #
            # Integration time may be reduced based on:
            #    a) Total time for mosaic (OTF only)
            #    b) Time until next off position (if necessary)
            #    c) Time until next tsys measurement Time until next off position (if necessary)
            tint = tintMosaic
            if mosaic['otf']:
               tint = min(tint, integTimeMosaic-tintOTF)
            # OFF position
            if mosaic['off'][index] <> None:
               tint = min(tint, mosaic['intervalOff'][index] - tintSinceLastOffPosition)
            # TSYS 
            if sources['tsys'] <> None:
               tint = min(tint, abs(sources['tsys']) - tsysdict['tintSinceLastTsys'])

            # Impose minimum integration times
            if integTimeMosaic <= 1.0 and tint < integTimeMosaic: 
               tint = integTimeMosaic
            elif integTimeMosaic > 1.0 and tint < 1.0:
               tint = 1.0

            # Make the mosaic
            if mosaic['otf']:
               # Print message
               t = str("%-10s" % souMosaic) + '   tint=' + \
                   str("%9s" % utils.dtString(tint/60.0)) + '  el=' + \
                   str("%4.1f" % utils.getSourceElevation(souMosaic)) + \
                   '  index = %3d' % mosaic['otfMosaic'][index].fieldIndex
               commands.trackMessage(t, indent=1)

               # Integrate
               mosaic['otfMosaic'][index].takeData(maxtime=tint)

               # Update counters
               observedSource = True
               observedSourceInCycle = True
               utils.setLastMosaicPosition(mosaic['otfMosaic'][index].fieldIndex)
            else:
               # Set mosaic position
               pos = max(1, utils.getLastMosaicPosition(mosaic['startpos']) + 1)
               if pos > npointingCenters: pos = 1

               # Print message
               t = str("%-10s" % souMosaic) + '   tint=' + \
                   str("%9s" % utils.dtString(tint/60.0)) + '  el=' + \
                   str("%4.1f" % utils.getSourceElevation(souMosaic)) + \
                   '  pos=' + str("%3d" % pos) + '  ' + \
                   str("%6.3f" % offsets[pos-1][0]) + "amin " + \
                   str("%6.3f" % offsets[pos-1][1]) + "amin"
               commands.trackMessage(t, indent=1)

               # Offset telescopes. pos starts at 1, while offsets are index=0.
               commands.equatOffset(offsets[pos-1][0], offsets[pos-1][1])

               # Integrate
               trecordMosaic = min([tint * 60.0, sources['record']])
               nreps = utils.getNreps(tint, trecordMosaic)
               tint = nreps * trecordMosaic / 60.0
               if nreps > 0:
                   commands.integrate(trecordMosaic, nreps, tmo=mosaic['tmoMosaic'], 
                       antwait=sources['antwait'], subarray=sa)
                   if tint >= 0.5 * integTimeMosaic:
                       observedSource = True
                       observedSourceInCycle = True
                       utils.setLastMosaicPosition(pos)
                       nposObserved += 1

            # Keep track of time since calibrations
            if mosaic['otf']: tintOTF += tint
            tsysdict['tintSinceLastTsys']  += tint
            tsysdict['tintSinceLastNoise'] += tint
            tintSinceLastOffPosition += tint
            integrationTimeTotal += tint

    # Observe noise one last time if needed
    tsampleNoise = None
    if noise <> None and noise.has_key('tsample'): 
        tsampleNoise = noise['tsample']
    if tsampleNoise <> None:
        utils.observeNoise(noise['tint'], noise['record'], 
                           verbose=True, indent=1, pacs=runPacs)
        tsysdict['tintSinceLastNoise'] = 0.0

    return observedSource, integrationTimeTotal

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
              sid       : ID number in database
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
             noise, primaryCal=None, pstop=None, resetDoppler=False):
    """ Perform one cycle between phaseCal and sources.

        Inputs: sources    - contains science targets and phase calibrators;
                             see template3.obs
                fluxcal    - flux calibrator options; see template3.obs
                noise      - noise source options; see template3.obs
                passband   - passband options; see template3.obs
                phaseCalDefault - default phase calibrator to use
                pointing   - pointing options; see template3.obs
                primaryCal - list of primary flux calibrators; see template3.obs
                pstop      - ending LST time for this phase calibrator
                resetDoppler   : If true, then set the doppler command
                                 for each source in "sources"

         Output: None
    """
    # Set database name
    db = sn.DB_DEFAULT
    if sources.has_key('snapDb'): db = sources['snapDb']

    # Set overhead factor. 60 converts minutes to hours
    foverhead = utils.overhead() / 60.0

    # Nominal passband
    timePb = foverhead * passband['tint']

    # Flux calibrators
    timeFlux = foverhead * fluxcal['tint']

    # Last calibrator
    timeLastcal = foverhead * sources['tintPhaseCal']

    # Source observations
    sou,t = utils.setSourceVariable(sources['target'], sources['tintTarget'], 0)
    tprimaryCal = t[0]

    # Set multi-frequency snapshot parameters
    timeCycle = 0.0
    isMultiFreq = sources['snapFreq'] <> None
    snapFreq = None
    if isMultiFreq:
        # Set frequencies
        snapFreq = utils.makeListObsdef(sources['snapFreq'])

        # Set integration time
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
    continueCycles = not utils.isEndOfTrack()
    commands.s.setScriptBool(INDX_BOOL_LASTCAL,False)
    commands.s.setScriptBool(INDX_BOOL_STOPNEXTCAL,False)
    ncycle = commands.s.getScriptInt(INDX_INT_LASTCYCLE)

    # Time between phase calibrators
    #   tintSinceLastPhaseCal : On-source integration time in minutes since 
    #                           last phase cal observatons
    #   phaseCalPrevious      : Name of the last-observed phase calibrator
    tintSinceLastPhaseCal = 0
    phaseCalPrevious = None

    # Start cycles
    okSource = False        # Indicates if source was observed
    sidPrevious = 0         # SID number (from database) of previous source
    tuneIndxPrevious = None # Index ID number of previous tuning
    startingCycles = True   # Indicates we are starting the first cycle
    while continueCycles :
        # Compute time needed to complete remaining critical observations
        tneed = 0.0
        if not finishedPassband(passband) : tneed += timePb
        if not finishedFluxcal(fluxcal)   : tneed += timeFlux * getNfluxCal(fluxcal)
        tneed += timeCycle

        # Find which source to observe next
        okCal = False
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

        # If no snapshot sources remain, then stop
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
            result = doPoint(pointing, ref=srefCal, printHeader=False)
            performingOpticalPointing = True

        # If new calibrator is different from previous one, or 
        # tuning will change, then observe previous calibrator one last time.
        # This step is skipped for multi-frequency observations since
        # calibrator observations are self-contained for that mode.
        if okSource and utils.continueObservations() and not isMultiFreq and \
           ( (phaseCalPrevious <> None and phaseCal <> None and phaseCal != phaseCalPrevious) or
             (sources['snapRetune'] and sou_snap != utils.getDopplerSource())):
            commands.trackMessage("Observing the phase calibrator "+str(phaseCalPrevious)+" before using "+str(phaseCal), indent=1)
            okCal = observeSource(phaseCalPrevious, sources['tintPhaseCal'],
                         sources['record'], tsysTime=sources['tsys'], 
                         noise=noise, lststop=utils.getLstStopCycle(default=pstop),
                         tmo=sources['tmo'], antwait=sources['antwait'],
                         intentObject='G', selfcal=True, runMiriad=True)
            if okCal: okSource = False
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
        observedPassband = False
        observedFluxcal = False
        pointed = False

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
                     excludeExpref = ['W3OH', 'OMC', '3C274', '1230+123', 'VIRGO', 'ORIMSR', 'MWC349', 'M82', 'NGC7027', 'DR21', 'W51IRS2']
                     phaseCal = utils.getSource(sourceList, ref=sou_snap,
                                     excludeExpref=excludeExpref, preferred=phaseCalPrevious,
                                     maxsep=autoSep, minsepsun=sources['minsepsun'], sky=False)
                     if phaseCal == None:
                         commands.trackMessage('Could not find a suitable phase calibrator', indent=1)
                         raise Exception,'Exiting script since no phase caibrator met the criteria'
               
             # See if we need radio pointing and if a source is available
             needRadioPointing = False
             if indx == 0 and not performingOpticalPointing:
                 needRadioPointing = needPointing(pointing, sou_snap, None, pointing['maxsep'], False)[0]
                 if needRadioPointing and not doPoint(pointing, ref=sou_snap, printHeader=False, checkSource=True):
                    needRadioPointing = False
                    commands.trackMessage('no radio pointing source is available', indent=1)

             # Observe phase calibrator, if needed
             okCal = False
             if phaseCal == None:
                 okCal = True
             elif utils.continueObservations() and tremaining > 0.0:
                 # Check if we need to observe the phase calibrator
                 observePhaseCal = (needRadioPointing and okSource) or \
                                   (not startingCycles and phaseCalPrevious != phaseCal) or \
                                   (not isMultiFreq and tintSinceLastPhaseCal >= tprimaryCal) or \
                                   (isMultiFreq and (tuneIndxPrevious==None or tuneIndxPrevious != tuneIndx))

                 # Observe phase calibrator if needed
                 if observePhaseCal:
                     okCal = observeSource(phaseCal, tintPhaseCal,
                              sources['record'], tsysTime=sources['tsys'],
                              noise=noise, lststop=utils.getLstStopCycle(default=pstop),
                              tmo=sources['tmo'], antwait=sources['antwait'],
                              intentObject='G', selfcal=True, runMiriad=True)
             phaseCalPrevious = phaseCal
             sidPrevious = sid
             tuneIndxPrevious = tuneIndx
             if okCal: okSource = False
             if utils.isStopAfterNextCal():
                 continueCycles = False
                 continue

             # Check radio pointing. "pointed" only indicates if radio pointing
             # was performed, not optical pointing.
             # This is done for the first frequency only.
             pointed = False
             if needRadioPointing:
                 pointed = doPoint(pointing, ref=sou_snap, printHeader=False)
                 if pointed:
                     okCal = False           # Require the phase calibrator to be reobserved
                     phaseCalPrevious = None # Reinitialize phasecal observation
                     tuneIndxPrevious = None
                     break  # Break from frequency loop

             # Flux calibrator
             observedFluxcal = False
             if not finishedFluxcal(fluxcal) and fluxcal['middle'] and \
                utils.continueObservations(isScience=True) and \
                utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0:
                 observedFluxcal = doFluxcal(fluxcal, primaryCal, noise, 
                       pointing, ref=sou_snap, lststop=pstop, printHeader=False)

             # Passband calibrator
             observedPassband = False
             if not finishedPassband(passband) and passband['middle'] and \
                utils.continueObservations(isScience=True) and \
                utils.timeRemaining(useTime=True, lsttime=utils.getLstStopCycle(default=pstop)) > 0.0:
                 observedPassband = doPassband(passband, noise, 
                       pointing=pointing, ref=sou_snap, lststop=pstop, printHeader=False)

             # It is possible at this point that we have not 
             #   a) pointed,
             #   b) observed a flux calinrator
             #   c) observed a passband calinrator
             #   d) observed a phase calibrator
             # but we are ready to observe the source.
             # Therefore, check for that condition here, and observe
             # the phase calibrator if needed.
             if startingCycles and \
                not pointed and \
                not observedFluxcal and \
                not observedPassband and \
                not okCal and \
                phaseCal <> None:
                 okCal = observeSource(phaseCal, tintPhaseCal,
                              sources['record'], tsysTime=sources['tsys'],
                              noise=noise, lststop=utils.getLstStopCycle(default=pstop),
                              tmo=sources['tmo'], antwait=sources['antwait'],
                              intentObject='G', selfcal=True, runMiriad=True)
             startingCycles = False

             # Reset variables if phase calibrator was observed
             if okCal: 
                okSource = False
                tintSinceLastPhaseCal = 0.0

             # Compute time remaining
             tneed = 0.0
             if phaseCal <> None : tneed += timeLastcal
             if not finishedPassband(passband) : tneed += timePb
             if not finishedFluxcal(fluxcal)   : tneed += timeFlux * getNfluxCal(fluxcal)
             tneed += timeCycle
             tremaining = utils.timeRemaining(source=sou_snap, phase=phaseCal, 
                                tneed=tneed, useTime=True, 
                                lsttime=utils.getLstStopCycle(default=pstop))

             # Observe source if
             #    1) we did not radio point
             # and
             #    2) did not observe flux/pb cal
             if not pointed and not observedFluxcal and not observedPassband and \
                utils.continueObservations(isScience=True) and tremaining > 0.0:
                 # Mark tuning as used
                 usedTune[tuneIndx] = True

                 # Check optical pointing
                 if commands.s.getScriptBool(INDX_BOOL_OPTRAD_VECTOR) and \
                    commands.s.getScriptBool(INDX_BOOL_DO_OPT_POINT) and \
                    pointing['doOptPoint'] and \
                    pointing['optPointTarget']:
                     doPoint(pointing, ref=srefTarget, printHeader=False)

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
                     selfcal=False, returnTint=True)

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
                           selfcal=False, returnTint=True)

                 # For multifrequency observations, end on the calibrator
                 if isMultiFreq and tint_actual > 0 and phaseCal <> None:
                     observeSource(phaseCal, tintPhaseCal,
                               sources['record'], tsysTime=sources['tsys'], 
                               noise=noise,
                               lststop=utils.getLstStopCycle(default=pstop),
                               tmo=sources['tmo'], antwait=sources['antwait'],
                               intentObject='G', selfcal=True, runMiriad=True)
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
                 tmo=sources['tmo'], antwait=sources['antwait'], 
                 intentObject='G', selfcal=True, runMiriad=True)

    # See why we are here
    if utils.isEndOfTrack():
        commands.trackMessage('Ending observations based on endtrack()')
    elif utils.isLastcal(): 
        commands.trackMessage('Ending cycle based on lastcal()')
    elif utils.isStopAfterNextCal():
        commands.trackMessage('Ending cycle based on stopAfterNextCal()')
