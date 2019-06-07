# $Id: holoBeamMap.py,v 1.23 2011/09/26 17:39:11 iws Exp $
#
# This script contains functions for acquiring maps for holography
# data. It is derived from white.py, with unused and deprecated
# functionality removed or replaced.
#
# @Author James W. Lamb, Caltech
#
# History
# 28-Oct-2008: JWL      Fixed major error in calculating Nyquist step size
# 19-Oct-2008: JWL      Imported 'carmaHelpers' for 'makeList'
# 18-Oct-2008: JWL      Imported 'device' for 'CarmaAnt' class
# 10-Sep-2008: JWL      Removed etrack() -- deprecated
# 08-Sep-2008: JWL      Changed 'track()' to 'etrack()'. Consts from carmaConstants.py
# 11-Aug-2008: JWL      Added command to set levels before starting map
# 01-Aug-2008: JWL      Added reference point at end of map
# 25-Jul-2008: JWL      Added logging to file. Added Array Health email.
# 10-Jul-2008: JWL      Removed dependence on short.py
# 02-Jul-2008: JWL      Added function to calculate Nyquist step for holography
# 19-Jun-2008: JWL      Fixed bug where antennas would offset sequentially
# 17-Jun-2008: JWL      Added debug and restart parameters
# 13-Jun-2008: Original version
#


import subarrayCommands as sc
import refPoint
import arrayHealthUtils as ah
import carmaConstants as CONSTS
import device
import carmaHelpers as helpers

def mapBeam(testAnts, intTime, intReps, gridSize, stepSize, circleLimit, lm, debug) :
    """ Do a square or circular grid map of the beam

        Parameters:

            testAnts
                Single antenna or list of antennas to map

            intTime
                Single integration time

            intReps
                Number of integrations per grid point

            gridSize
                Linear size of grid (number of rows or columns)

            stepSize
                List of Az/El step sizes, one per test antenna (arcmin)

            circleLimit
                If True will measure only points within a circle of diameter
                (gridSize + 0.5) steps

            lm
                LogMessage handle for progress logging

            debug
                If True, do not send commands to the control system
    """
    maxRadSq = (0.5 * (gridSize - 1) + 0.5)**2
    noiseIndex = 0
    # Ensure that all levels are set up correctly before starting
    sc.tsys(ifsetup = True)
    for row in range(gridSize) :
        lm.message("Beginning row %d of %d" % (row + 1, gridSize))
        lm.message("  Reference point")
        # Before each row, measure center pointing for reference
        if not debug :
            sc.offset(0.0, 0.0, 0, waiton = sc.ALL)
            sc.integrate(intTime, intReps)
        lm.message("Row %d" % (row + 1))
        for col in range(gridSize) :
            if (noiseIndex % 6 == 0) :
                lm.message("  Noise integration")
                if not debug :
                    sc.noiseon()
                    sc.intent('noise', 'B')
                    sc.integrate(10, 1, antwait = sc.NONE)
                    sc.noiseoff()
            noiseIndex += 1
            dAz = 0.5 * (1 - gridSize) + col
            dEl = 0.5 * (1 - gridSize) + row
            if (not circleLimit) or ((dAz**2 + dEl**2) <= maxRadSq) :
                lm.message("    Grid point (%d, %d)" % (row + 1, col + 1), noLog = True)
                for ant in testAnts :
                    i = testAnts.index(ant)
                    azOffset = stepSize[i] * dAz
                    elOffset = stepSize[i] * dEl
                    if not debug :
                        sc.offset(azOffset, elOffset, ant, waiton = sc.NONE)
                if not debug : sc.integrate(intTime, intReps, antwait = sc.ALL, tmo = 20)

    # Final center pointing for reference
    if not debug :
        sc.offset(0.0, 0.0, 0, waiton = sc.ALL)
        sc.integrate(intTime, intReps)
    return

# end mapBeam


def acquireBeamMap(source, testAnts, stepSize, gridSize, mapReps, intTime, intReps, \
                   circleLimit, elevLimit, restart, lm, debug) :
    """ This function sets up, checks and executes a beam map acquisition

        Parameters:

            source
                Name of source to observe for beam map

            testAnts
                Antennas to map. May be single antenna or list. All other antennas in the
                array will be used as references.

            stepSize
                List of Az/El step sizes, one per test antenna (arcmin)

            gridSize
                Number of rows (elevation offsets) to scan and number of columns (azimuth
                offsets) to scan. There are gridSize**2 points in the map

            mapReps
                Number of times to repeat the complete map

            intTime
                Integration time, s

            intReps
                Number of integrations per grid point

            circleLimit
                If true only grid points within a circle with a diameter equal to the grid
                width will be observed

            elevLimit
                Lowest elevation to take data at.

            restart
                If this is true then do not do the pointing. Currently, the previous state
                of the map is not stored so the map will be restarted at the first point.

            lm
                LogMessage handle for progress logging

            debug
                Do not send any commands to the real-time system.
    """

    if not debug :
        sc.applyFlux(True)
        sc.applyTsys(True)

    if elevLimit != None :
        if not debug : sc.elevlimit(elevLimit)

    # Make sure noise source has not been left on
    if not sc.queryInt('Sldc.NoiseSourceContainer.NoiseSource.noiseStatus',24) :
        sc.noiseoff()

    # Create valid lists of test and reference antennas from the antennas in this
    # subarray. All the test antennas should be present. Any antennas not in the
    # test set are assigned to the reference set.
    allAnts = sc.currentAntennaNumbers()
    testAnts = helpers.makeList(testAnts)
    testAntList = []
    refAntList = []
    for ant in allAnts :
        if ant in testAnts :
            testAntList.append(ant)
            testAnts.remove(ant)
        else :
            refAntList.append(ant)

    # If we are not in debug mode, do not continue without all required antennas
    if not debug :
        if testAntList == [] :
            raise Exception, "No test antennas in current array"
        if refAntList == [] :
            raise Exception, "No reference antennas in current array"
        if testAnts != [] :
            raise Exception, "Test antenna(s) not in current array: " + \
                            str(testAnts)

    lm.message("Test antennas: " + str(testAntList))
    lm.message("Reference antennas: " + str(refAntList))

    if (not sc.isup(source)) and (not debug) :
        timeToSleep = sc.whenup(source) * 60.0
        if (timeToSleep > 0.0) :
            lm.message("Waiting %s for %s to rise" % (ah.dhms(timeToSleep), source))
            sc.sleep(timeToSleep)

    if not debug :
        sc.intent(source, 'BFGS', True, False)
        sc.offset(0.0, 0.0, 0)
        sc.track(source, waiton = sc.ALL, tmo = 500)

    #----Begin with a pointing----
    #
    lm.message("Pointing on %s" % source)
    if (not debug) and (not restart) :
        refPoint.refPoint(source)

    if not debug :
        sc.intent(source, 'BFGS', True, False)
        sc.offset(0.0, 0.0, 0)
        sc.track(source, waiton = sc.ALL, tmo = 500)

    # Begin loop over repetitions
    for rep in range(mapReps) :
        lm.message("Beginning %i of %i cycles" % (rep + 1, mapReps))
        mapBeam(testAntList, intTime, intReps, gridSize, stepSize, \
                circleLimit, lm, debug)

# end acquireBeamMap


def getAntennaDiameters(ants) :
    """ Returns the diameter of an antenna or list of antennas.

        Parameters:

            ants    A single antenna or a list of antennas

        Returns:

            Antenna diameter or list of diameters in meters

        If the function is a single integer, it returns the corresponding
        antenna diameter as a float. If it is a list of integers, it
        returns the diameters as a list of floats. Any other argument
        will result in a return value of -1
    """
    antennaDia = {
        'ovro' : CONSTS.OVRO_DIA,
        'bima' : CONSTS.BIMA_DIA,
        'sza' : CONSTS.SZA_DIA
        }

    if type(ants) == int :
        name = device.CarmaAnt().getName(ants).strip("0123456789")
        dia = antennaDia[name]
        return dia

    elif type(ants) == list :
        dia = []
        for ant in ants :
            name = device.CarmaAnt().getName(ant).strip("0123456789")
            dia.append(antennaDia[name])
        return dia

    else :
        return 0

# end getAntennaDiameters


def getHoloMapSteps(ants, freq, oversample) :
    """ Returns Nyquist step in arcminutes for holography maps

        Parameters:

            ants    An antenna or list of antennas

            freq    Frequency to calculate at, GHz

            oversample  Amount to over sample by

        Returns:

            A list of Nyquist steps for the antenna(s) requested, arcmin

       This function calculates the step size corresponding to Nyquist
       sampling for holography, i.e., lambda/D, rather than the value
       of 0.5 * lambda/D used for mosaics. The oversample value is the
       number to *divide* the step size by.
    """

    nyquistStepList = []
    wavelength = CONSTS.C / (freq * CONSTS.GHZ)
    if type(ants) == int :
        ants = [ants]

    diameterList = getAntennaDiameters(ants)
    for dia in diameterList :
       nyquistStepList.append((wavelength / dia) / (CONSTS.ARCMIN * oversample))
    return nyquistStepList

# end getHoloMapSteps