# 22-Oct-2008: JWL      Modified imports to more robust form# $Id: tsysVsIfLevel.py,v 1.14 2011/09/26 17:39:11 iws Exp $
# Script to measure the system temperature as a function of the IF power level
# setting on the ambient load.
#
# To use:
#
#   run('tsysVsIfLevel', <parameters>)
#
#   All parameters are optional.
#
# @Author James W. Lamb, Caltech
#
# History
# 07-Apr-2009: JWL      Updated for Steve Scott's run command
# 22-Oct-2008: JWL      Modified imports to more robust form
# 01-Aug-2008: JWL      Added log file
# 19-Jun-2008: JWL      Added a web/ directory for output
# 13-Jun-2008: Original version
#

#-------------------------- Help --------------------------
""" Measures Tsys as a function of IF level set on ambient load

    Usage:

    run('tsysVsIfLevel', <parameters>)

    Parameters:

        narrowBand = True
            Use the 62 MHz filter instead of the 500 MHz one

        minIf = 0.02
            Start power level on ambient load, mW

        maxIf = 5.0
            End power level on ambient load, mW

        numVals = 11
            Number of IF power levels to set

        loFreq = None
            If required, LO frequency to tune to

        goToStow = True
            Use to stow antennas before starting measurements

        bands = [1, 2, 3]
            IF bands to make measurement in

        ifFreqs = [1.25, 1.75, 2.25]

        debug = False
            If True, no commands will be sent to the subarray controller.
            However, the monitor points will be read and the plots will
            be produced. Data and plot file names will be prefaced with
            "DEBUG-"

   This function will measure the system noise temperature as a function of
   the IF level set on the ambient load. After it sets up the requested
   bands and frequencies it inserts the ambient load, sets the first requested
   power level and then requests a Tsys measurement. It repeats this with
   increasing power levels in equal logarithmic increments.
"""
#------------------------ End of Help ------------------------

#----Required imports----

import runCommand as rc
import device
import time
import subarrayCommands as sc
import printFunctions as pf
import pylab as plt
import os as os
import standardArrayPlots as ap
import arrayHealthUtils as ah
from numpy import log10

#----Parameters----
#
p = rc.Params()
p.add("narrowBand", type = bool, default = True,
       description = "Use narrow downconverter analog filters")
p.add("minIf", type = float, default = 1.0,
       description = "Start IF power level, mW")
p.add("maxIf", type = float, default = 5.0,
       description = "Stop IF power level, mW")
p.add("numVals", type = int, default = 11,
       description = "Number of power level values to set")
p.add("loFreq", type = float, default = None,
       description = "LO freq. None to leave at current value",
       noneAllowed = True)
p.add("bands", type = list, default = [1, 2, 3],
       description = "List of bands to test")
p.add("ifFreqs", type = list, default = [1.25, 1.75, 2.25],
       description = "List of IF center frequencies")
p.add("goToStow", type = bool, default = True,
       description = "Move antennas to zenith for measurement")
p.add("debug", type = bool, default = False,
       description = "Run in debug mode (minimum system impact).")

p.processInputParameters()
#----End Parameters----

narrowBand = p.narrowBand
minIf      = p.minIf
maxIf      = p.maxIf
numVals    = p.numVals
loFreq     = p.loFreq
bands      = p.bands
ifFreqs    = p.ifFreqs
goToStow   = p.goToStow
debug      = p.debug

#----Setup----
#
numAnts = 15
maxBandNo = 8

sc.alarmdisable()
pf.printInColor("\n\nStarting measurement of system temperature vs IF level", \
             color = "blue", linefeed = True)
idString = "tsysVsIfLevel.py, " + "$Revision: 1.14 $".strip('$')

#----Set up logging----
pathName = "web/tsysVsIfLevel/"
logFileName = time.strftime("tsysVsIfLevel%Y.log")

lm = ah.LogMessage("Tsys vs IF level", pathName, logFileName, idString)
if debug :
    pf.printInfo("In debug mode")

lm.message("\nOptions\n-------", noRtd = True)
lm.message(" narrowBand = %s " % narrowBand, noRtd = True)
lm.message(" minIf = %s " % minIf, noRtd = True)
lm.message(" maxIf = %s " % maxIf, noRtd = True)
lm.message(" numVals = %s " % numVals, noRtd = True)
lm.message(" loFreq = %s " % loFreq, noRtd = True)
lm.message(" bands = %s " % bands, noRtd = True)
lm.message(" ifFreqs = %s " % ifFreqs, noRtd = True)
lm.message(" goToStow = %s " % goToStow, noRtd = True)
lm.message(" debug = %s " % debug, noRtd = True)

# Get a list of who is in the array
currentAntennas = sc.currentAntennaNumbers()
nAnts = len(currentAntennas)
if nAnts == 0 :
    lm.message("No antennas in array")
    if not debug :
        lm.close()
        raise sc.ScriptReturn, 'No antennas in array!'

lm.message("\nAntennas included: " + str(currentAntennas))

# Generate time estimate for completion
timeLeft = 0
elapsedTime = 0
if goToStow :
    minEl = 90.0
    for ant in currentAntennas :
        mpString = device.CarmaAnt().getName(ant) + \
                   ".AntennaCommon.Drive.Track.actualElevation"
        minEl = min(sc.queryDouble(mpString, retries = 20), minEl)
    timeLeft += 2.5 * (90.0 - minEl)
if loFreq != None :
    timeLeft += 120
timeLeft += len(bands) * 3  # Time to set up band configurations
timeLeft += numVals * 35    # Time for setting levels and measuring Tsys
lm.message("Estimated time to finish: " + ah.dhms(timeLeft))
startTime = time.time()

# Will use the following bands
numBands = len(bands)
lm.message(("Number of bands: %d\n  " % numBands) + str(bands))

#----Hardware initialization----
#
# Stow antennas if requested
if goToStow == True :
    lm.message("Stowing antennas")
    if not debug : stow(0)
    elapsedTime = time.time() - startTime
    timeLeft -= elapsedTime
    lm.message("Time left: " + ah.dhms(timeLeft) + ", Elapsed time: " + ah.dhms(elapsedTime))

# Tune receivers if required
if loFreq != None :
    lm.message("Tuning to LO frequency of %.3f GHz" % loFreq)
    if not debug : freq(loFreq, USB, 0.0, None)
    elapsedTime = time.time() - startTime
    timeLeft -= elapsedTime
    lm.message("Time left: " + ah.dhms(timeLeft) + ", Elapsed time: " + ah.dhms(elapsedTime))
else :
    loFreq = sc.queryDouble("Control.Subarray1.loFreq", retries = 20)
    lm.message("LO frequency is %.3f GHz" % loFreq)

# Calculate level steps to cover specified range range
logMinIf = log10(minIf)
logMaxIf = log10(maxIf)
ifRange = logMaxIf - logMinIf
if numVals > 0 :
    logStep = ifRange / (numVals - 1)
else :
    logStep = 0.0

# Set band configurations for selected IF frequency
if narrowBand == True :
    bw = 0.062
    bandwidth = BW62
    bwStr = "62 MHz"
else :
    bw = 0.500
    bandwidth = BW500
    bwStr = "500 MHz"
lm.message("Filter bandwidth: " + bwStr)

for band in bands :
    ifFreq = ifFreqs[bands.index(band)]
    skyFreq = loFreq + ifFreq
    lm.message("Setting band %d IF frequency =  %.3f GHz" % (band, ifFreq))
    lm.message("Configuring band %d" % band)
    if band > maxBandNo :
        lm.message("Invalid band no. (%d)" % band)
    else :
        if not debug : configband(band = band, bw = bandwidth, fcenter = skyFreq)
elapsedTime = time.time() - startTime
elapsedTime = time.time() - startTime
timeLeft -= elapsedTime
lm.message("Time left: " + ah.dhms(timeLeft) + ", Elapsed time: " + ah.dhms(elapsedTime))

#----Get the data----
#
# Storage for plotting results
levelList = []
for i in range(numAnts) :
    levelList.append([0.0 for j in range(numVals)])
tSysList = []
for i in range(numAnts * numBands) :
    tSysList.append([0.0 for j in range(numVals)])
pSysList = []
for i in range(numAnts * numBands) :
    pSysList.append([0.0 for j in range(numVals)])

# Step through the IF level range with the specified logarithmic increment
logIfLevel = logMinIf
measureStartTime = time.time()

for i in range(numVals) :
    lm.message("Inserting ambient load")
    if not debug : amb(0)
    ifLevel = 10**logIfLevel
    lm.message("Setting IF level to %.3f mW" % ifLevel)
    if not debug : antennaIFpower(power = ifLevel, 0, ants = currentAntennas)
    sleep(2)
    if not debug : psysPreset(currentAntennas, band = 0)
    sleep(2)

    lm.message("Reading actual IF levels")
    for ant in currentAntennas :
        antName = device.CarmaAnt().getName(ant)
        if antName[0] == 'o' :
            mpString = antName + ".AntennaIfContainer.AntennaIF.ifOutTotalPower"
        elif antName[0] == 'b' :
            mpString = antName + ".AntennaIfContainer1.AntennaIF.ifOutTotalPower"
        else :
            mpString = ""
        (levelList[ant - 1])[i] = 10*log10(sc.queryDouble(mpString, retries = 20))

    lm.message("Measuring Tsys")
    if not debug : sc.tsys(ants = currentAntennas, ifsetup = False)
    sleep(2)

    for ant in currentAntennas :
        index = 0
        for band in bands :
            mpString = "slPipeline.Input%d.Band%d.Tsys.Dsb" % (ant, band)
            tSys = sc.queryDouble(mpString, retries = 20)
            (tSysList[numBands * (ant - 1) + index])[i] = tSys
            mpString = "sldc.Band%d.Input%d.psys" % (band, ant)
            pSys = sc.queryDouble(mpString, retries = 20)
            (pSysList[numBands * (ant - 1) + index])[i] = pSys
            index += 1
    logIfLevel += logStep
    timePerIteration = (time.time() - measureStartTime) / (i + 1)
    elapsedTime = time.time() - startTime
    timeLeft = (numVals - i - 1) * timePerIteration
    lm.message("Time left: " + ah.dhms(timeLeft) + ", Elapsed time: " + ah.dhms(elapsedTime))


#----File writing----
#
# Open a new file to store the results and add header info.
# If accessible the results are stored in the /array/rt/ area. If not
# then they are stored in /tmp/.
# Within that tree, the results are in /tsysVsIfLevel/<freq>/<date>/
# where <freq> is the frequency, rounded to the nearest GHz, and <date>
# is the date in the form, e.g.,  2008-Jun-15.
pathName = ("web/tsysVsIfLevel/%.0fGHz/" % loFreq) + time.strftime("%Y-%b-%d/")
dataPath = ah.getDataPath(pathName, verbose = True)
fileName = ("tsysVsIfLevel,%.0fGHz," % loFreq) + time.strftime("%Y-%b-%d,%Hh%Mm%Ss")
if debug :
    fileName = "DEBUG-" + fileName
resultsFileName = fileName + ".txt"
lm.message("Saving data in: " + resultsFileName)
resultsFile = open(dataPath + resultsFileName, "w")

# Write header and other information into data file
resultsFile.write("System noise temperature as a function of IF level\n")
resultsFile.write(idString + "\n\n")
if debug :
    resultsFile.write("debug = True\n\n")

resultsFile.write("            Date: " + time.strftime("%Y-%b-%d %Z%z\n"))
resultsFile.write("      Start time: " + time.strftime("%H:%M:%S") + "\n")
resultsFile.write("    LO frequency: %.3f GHz\n" % loFreq)
resultsFile.write("Filter bandwidth: " + bwStr + "\n")
tau225 = queryDouble("Weather.tau225", retries = 20)
resultsFile.write("   tau (225 GHz): %.2f" % tau225)

# Get elevation range
minEl = 90.0
maxEl = 0.0
for ant in currentAntennas :
    mpString = device.CarmaAnt().getName(ant) + \
               ".AntennaCommon.Drive.Track.actualElevation"
    thisEl = sc.queryDouble(mpString, retries = 20)
    minEl = min(thisEl, minEl)
    maxEl = max(thisEl, maxEl)
if (abs(maxEl - minEl) < 1) :
    elRangeStr = "\nEl = %.1f deg" % (0.5 * (minEl + maxEl))
else :
    elRangeStr = "\nEl range: %.1f-%.1f deg" % (minEl, maxEl)
resultsFile.write(elRangeStr)

# Write the Tsys data into the results file
resultsFile.write("\n\nNoise temperatures, K:\n\n")
for ant in currentAntennas :
    resultsFile.write(" ----------------- C%02d -----------------" % ant)
resultsFile.write("\n")
for ant in currentAntennas :
        resultsFile.write(" Lev., dBm")
        for band in bands :
            resultsFile.write("     Band%d" % band)
resultsFile.write("\n")

for i in range(numVals) :
    for ant in currentAntennas :
        index = 0
        resultsFile.write("%10.3f" % (levelList[ant - 1])[i])
        for band in bands :
            tSys = (tSysList[numBands * (ant - 1) + index])[i]
            resultsFile.write("%10.1f" % tSys)
            index += 1
    resultsFile.write("\n")

# Write the Psys data into the results file
resultsFile.write("\n\nSystem power, dBm:\n\n")
for ant in currentAntennas :
    resultsFile.write(" ----------------- C%02d -----------------" % ant)
resultsFile.write("\n")

for i in range(numVals) :
    for ant in currentAntennas :
        index = 0
        resultsFile.write("%10.3f" % (levelList[ant - 1])[i])
        for band in bands :
            pSys = (pSysList[numBands * (ant - 1) + index])[i]
            resultsFile.write("%10.1f" % pSys)
            index += 1
    resultsFile.write("\n")

lm.message("Closing data file")
resultsFile.close()

# ----Plotting data----
#
# First, Tsys
plt.ioff()
lm.message("Generating plots")
if loFreq > 150.0 :
    tMin = 0
    tMax = 300
else :
    tMin = 0
    tMax = 200
minIfDbm = 10 * logMinIf
maxIfDbm = 10 * logMaxIf
plotTitle = "System Noise Temperature vs IF Level"
infoTextLeft = ("LO freq = %.3f GHz\ntau (225 GHz) = %.2f\nBW = " % (loFreq, tau225)) + \
               bwStr + elRangeStr
infoTextRight = time.strftime("%d-%b-%Y")
xLabel = "IF level, dBm"
yLabel = "Tsys, K"
legends = []
for band in bands :
    legends.append("Band %d, %.3f GHz" % (band, ifFreqs[band - 1]))
lineStyles = ('b-', 'r-', 'g-', 'm-', 'c-', 'y-')

plotFileName = dataPath + fileName + "(1)"

err = ap.allAntennaMultiPlot(levelList, tSysList, currentAntennas, minIfDbm, maxIfDbm, tMin, tMax, \
    plotTitle = plotTitle, xLabel = xLabel, yLabel = yLabel, \
    infoTextLeft = infoTextLeft, infoTextRight = infoTextRight, \
    legends = legends, lineStyles = lineStyles, \
    plotFileName = plotFileName, makePdf = True, makePng = True, \
    orientation = 'portrait')
if err :
    lm.message("Error plotting Tsys")
else :
    lm.message("Plots made:")
    lm.message("  " + fileName + ".pdf")
    lm.message("  " + fileName + ".png")

# Second, Psys
pMin = -55
pMax = -15
plotTitle = "Downconverter Psys vs IF Frequency"
yLabel = "Psys, dBm"

plotFileName = dataPath + fileName + "(2)"

err = ap.allAntennaMultiPlot(levelList, pSysList, currentAntennas, minIfDbm, maxIfDbm, pMin, pMax, \
    plotTitle = plotTitle, xLabel = xLabel, yLabel = yLabel, \
    infoTextLeft = infoTextLeft, infoTextRight = infoTextRight, \
    legends = legends, lineStyles = lineStyles, \
    plotFileName = plotFileName, makePdf = True, makePng = True, \
    orientation = 'portrait')
if err :
    lm.message("Error plotting Psys")
else :
    lm.message("Plots made:")
    lm.message("  " + fileName + ".pdf")
    lm.message("  " + fileName + ".png")

lm.close()

# End tsysVsIfLevel
