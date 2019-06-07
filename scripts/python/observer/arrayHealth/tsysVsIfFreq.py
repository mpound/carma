# $Id: tsysVsIfFreq.py,v 1.18 2010/11/30 22:28:43 lamb Exp $
# Script to measure the system temperature as a function of IF frequency.
# Scans the downconverters through the IF band in steps equal to the
# bandwidth of the selected filter.
#
# To use:
#
#   run('tsysVsIfFreq', <parameters>)
#
#   All parameters are optional.
#
#
# @Author James W. Lamb, Caltech
#
# History
# 30-Dec-2010: JWL      Fixed reference to series rather than 'series'
# 20-Nov-2020: JWL      Changed bands 1-3 to 125 MHz
#                       Made 8 bands default
# 29-May-2020: JWL      Enabled alarm at end of script
# 26-May-2020: JWL      Added correlator threshold and delay setup commands
#                       Modified integration and sample times
# 20-Apr-2010: JWL      Refinements and debugging
# 26-Mar-2010: TP       Extended IF to 8 bands, coded options 'mode' (series/parallel) and 'source', changed plotting parameters accordingly.
# 07-Apr-2009: JWL      Updated for Steve Scott's run command
# 22-Oct-2008: JWL      Modified imports to more robust form
# 01-Aug-2008: JWL      Added log file
# 20-Jun-2008: JWL      Added a sideband select option
# 19-Jun-2008: JWL      Added a web/ directory for output
# 13-Jun-2008: JWL      Major revisions. Now script instead of function (to use 'run' command)
# 06-Jun-2008: Original version
#

#-------------------------- Help --------------------------
""" Scans the spectral line downconverters across the IF and measures Tsys
    Usage:
        run('tsysVsIfFreq', <parameters>)

    Parameters:

        narrowBand = True
            Use the 125 MHz filter instead of the 500 MHz one

        minIf = 1.0
            Start frequency in the IF (lower edge of filter band)

        maxIf = 9.0
            End frequency in the IF (upper edge of filter band)

        loFreq = None
            If required, LO frequency to tune to

        goToStow = True
            Use to stow antennas before starting measurements. Ignored if source
            is not None.

        bands = [1, 2, 3, 4, 5, 6, 7, 8]
            IF bands to make measurement in (1--8)

        sideband = AUTO
            Sideband to use. This will use the specified sideband where possible
            and change over where it is not. Options are LSB | USB | AUTO. If
            AUTO is selected, the control system will choose the sideband.

        mode = 'parallel'
            Frequency for each band. Options are 'parallel' | 'series'. This will use the
            'parallel' mode by default, thereby placing all bands at the same frequency.
            If 'series' is selected, then the bands are placed at adjacent frequencies.

        source = None
            If a source name is specified (e.g., source = '3C273'), this will do the
            measurements while tracking a source and calculate the single sideband noise
            from the sideband ratios for each antenna.

        tInt = 3.0
            Integration time (min) on source. This is the total time, with individual
            records of 10 s. Ignored if source = None.

        debug = False
            If True, no commands will be sent to the subarray controller.
            However, the monitor points will be read and the plots will
            be produced. Data and plot file names will be prefaced with
            "DEBUG-"

   This script will scan the spectral line downconverter across the IF band and
   measure the system noise temperature as a function of IF frequency. The step size is
   equal to the filter bandwidth (larger of 62 or 125 MHz if both types used, or 500 MHz).
   At each setting the IF and downconverter levels are set up. All bands are set to the
   same IF frequency.
"""
#------------------------ End of Help ------------------------

#----Required imports----

import runCommand as rc
import device
import time
import subarrayCommands as sc
import printFunctions as pf
import arrayHealthUtils as ah
import standardArrayPlots as sp
import math
import obsdefUtils as utils

# ------------ Parameters -------------
#
p = rc.Params()
p.add("narrowBand", type = bool, default = True,
       description = "Use narrow downconverter analog filters")
p.add("minIf", type = float, default = 1.0,
       description = "Start IF, GHz")
p.add("maxIf", type = float, default = 9.0,
       description = "Stop IF, GHz")
p.add("loFreq", type = float, default = None,
       description = "LO freq. None to leave at current value",
       noneAllowed = True)
p.add("bands", type = list, default = [1, 2, 3],
       description = "List of bands to test")
p.add("goToStow", type = bool, default = True,
       description = "Move antennas to zenith for measurement")
p.add("sideband", type = str, default = 'AUTO',
       description = "Downconverter sideband to use")
p.add("mode", type = str, default = 'parallel',
       description = "Frequency for each band")
p.add("source", type = str, default = None,
       description = "Source to track to measure SSB noise",
       noneAllowed = True)
p.add("tInt", type = float, default = 3.0,
       description = "Total integration to measure sideband ratio (min)",
       noneAllowed = True)
p.add("debug", type = bool, default = False,
       description = "Run in debug mode (minimum system impact).")

p.processInputParameters()
# ------------ End parameters -------------

#----Initialize command line options----

narrowBand = p.narrowBand
minIf      = p.minIf
maxIf      = p.maxIf
loFreq     = p.loFreq
bands      = p.bands
goToStow   = p.goToStow
sideband   = p.sideband.strip(' ').upper()
mode       = p.mode
source     = p.source
tInt       = p.tInt
debug      = p.debug

#----Setup----
#
numAnts = 15
maxBandNo = 8
narrowBandwidth = [
                    sc.BW62,
                    sc.BW62,
                    sc.BW62,
                    sc.BW125,
                    sc.BW125,
                    sc.BW125,
                    sc.BW125,
                    sc.BW125
                  ]
narrowBwStr =     [
                    "125 MHz",
                    "125 MHz",
                    "125 MHz",
                    "125 MHz",
                    "125 MHz",
                    "125 MHz",
                    "125 MHz",
                    "125 MHz"
                  ]
narrowBw =        [
                    0.125,
                    0.125,
                    0.125,
                    0.125,
                    0.125,
                    0.125,
                    0.125,
                    0.125
                  ]

integrationTime = 30
integrationCounts = int(round((60.0 * tInt) / integrationTime))
totalTime = integrationTime * integrationCounts

# Initial values for recording elevation range
minEl = 90.0
maxEl = 0.0

sc.alarmdisable()
pf.printInColor("\n\nStarting measurement of system temperature vs IF frequency", \
             color = "blue", linefeed = True)
idString = "tsysVsIfFreq.py, " + "$Revision: 1.18 $".strip('$')

#----Set up logging----
pathName = "web/tsysVsIfFreq/"
logFileName = time.strftime("tsysVsIfFreq%Y.log")

lm = ah.LogMessage("Tsys vs IF freq", pathName, logFileName, idString)
if debug :
    pf.printInfo("In debug mode")

lm.message("User supplied Options", noRtd = True)
lm.message(" narrowBand = %s" % narrowBand, noRtd = True)
lm.message(" minIf = %s" % minIf, noRtd = True)
lm.message(" maxIf = %s" % maxIf, noRtd = True)
lm.message(" loFreq = %s" % loFreq, noRtd = True)
lm.message(" bands = %s" % bands, noRtd = True)
lm.message(" goToStow = %s" % goToStow, noRtd = True)
lm.message(" sideband = %s" % sideband, noRtd = True)
lm.message(" mode = %s" % mode, noRtd = True)
lm.message(" source = %s" % source, noRtd = True)
lm.message(" debug = %s" % debug, noRtd = True)

# Log this in the arrayHealth email archive
if not debug :
    utils.sendEmailMessage(sendTo   = "arrayHealth@mmarray.org", \
                           sendFrom = "obs@mmarray.org", \
                           subject  = "Starting Tsys measurements...", \
                           msg      = "Running %s" % idString)

# Sanity checks
try :
    ['AUTO', 'LSB', 'USB'].index(sideband)
except ValueError :
    raise sc.ScriptReturn, "Error: 'sideband' must be a 'AUTO', 'LSB', or 'USB'"

for bandNo in bands :
    if (bandNo < 1) or (bandNo > maxBandNo) :
        raise sc.ScriptReturn, "Error: Invalid band requested: Band %d" % bandNo

if source != None :
    lm.message("Target source provided...")
    if mode != 'series' :
        lm.message("  'parallel' mode not allowed; forcing 'series'")
        mode = 'series'
    if narrowBand == True :
        lm.message("  'narrowBand' mode not allowed; setting to False")
        narrowBand = False
    if goToStow == True :
        lm.message("  Ignoring 'goToStow' option")
        goToStow = False

# Get a list of who is in the array
currentAntennas = sc.currentAntennaNumbers()
nAnts = len(currentAntennas)
if nAnts == 0 :
    lm.message("No antennas in array")
    if not debug :
        lm.close()
        raise sc.ScriptReturn, 'No antennas in array!'

lm.message("Antennas included: " + str(currentAntennas), noRtd = True)

# Will use the following bands
numBands = len(bands)
lm.message(("Number of bands: %d -- " % numBands) + str(bands), noRtd = True)

# Make lists of band properties
bw = []
bandwidth = []
bwStr = []
if narrowBand :
    for bandNo in bands :
        bw.append(narrowBw[bandNo - 1])
        bandwidth.append(narrowBandwidth[bandNo - 1])
        bwStr.append(narrowBwStr[bandNo - 1])
else :
    for bandNo in bands :
        bw.append(0.500)
        bandwidth.append(sc.BW500)
        bwStr.append("500 MHz")

# Calculate number of tunings to cover IF range
ifRange = maxIf - minIf
bwListStr = ""
for string in bwStr :
    bwListStr += string + ", "
bwListStr = bwListStr.rstrip(", ")
lm.message("Filter bandwidth(s): " + bwListStr)

# Fit bands into range with minimum overlap. All band edges are inside
# the specified IF range.
if mode == 'series' :
    numChunks = math.ceil(ifRange / (numBands * max(bw)))
    numSteps = numChunks * numBands
    fStep = (ifRange - max(bw)) / (numSteps - 1)
elif (mode == 'parallel') :
    numSteps = math.ceil(ifRange / max(bw))
    numChunks = numSteps
    fStep = (ifRange - max(bw)) / (numSteps - 1)

# Generate time estimate for completion
timeLeft = 0
elapsedTime = 0
if goToStow :
    timeLeft += 2.5 * (90.0 - minEl)
if loFreq != None :
    timeLeft += 120
timeLeft += numSteps * len(bands) * 3  # Time to set up band configurations
timeLeft += numChunks * 25             # Time for setting levels and measuring Tsys
if source != None :
    timeLeft += 6000                   # Time to acquire source
    timeLeft += numChunks * totalTime  # Add time for integrating on source
lm.message("Estimated time to finish: " + ah.dhms(timeLeft))
startTime = time.time()

#----Hardware initialization----
#
# Stow antennas if requested
if goToStow and source == None :
    lm.message("Stowing antennas")
    if not debug : sc.stow(0)
    elapsedTime = time.time() - startTime
    timeLeft -= elapsedTime
    lm.message("Time left: " + ah.dhms(timeLeft) + ", Elapsed time: " + ah.dhms(elapsedTime))

# Tune receivers if required
if loFreq != None :
    lm.message("Tuning to LO frequency of %.3f GHz" % loFreq)
    if not debug : sc.freq(loFreq, sc.USB, 0.0, None)
    elapsedTime = time.time() - startTime
    timeLeft -= elapsedTime
    lm.message("Time left: " + ah.dhms(timeLeft) + ", Elapsed time: " + ah.dhms(elapsedTime))
else :
    loFreq = sc.queryDouble("Control.Subarray1.loFreq", retries = 20)
    lm.message("LO frequency is %.3f GHz" % loFreq)

#----Set up variables for storage----
#
# Storage for plotting results
if source != None :
    size1 = 2
    size2 = 1
elif mode == 'series' :
    size1 = 1
    size2 = 1
else :
    size1 = numBands
    size2 = numBands

freqList = []
for i in range(numSteps) :
    freqList.append(0.0)

tSysList = []
for i in range(numAnts * size1) :
    tSysList.append([0.0 for j in range(numSteps)])

pSysList = []
gainRatioList = []
for i in range(numAnts * size2) :
    pSysList.append([0.0 for j in range(numSteps)])
    gainRatioList.append([1.0 for j in range(numSteps)])

#----Get the data----
#
# Step through the IF range in increments of the filter bandwidth, starting
# with the edge of the filter at the lowest frequency. The specified
# sideband of the second LO is used where possible.
ifFreq = minIf + 0.5 * max(bw)
measureStartTime = time.time()

min2ndLo = 1.0
max2ndLo = 4.5
basebandFreq = 0.75
blockDcFreq = 10.00

# Track source, if provided.
if source != None :
    sc.intent('3C111', 'g')
    lm.message("Moving to source %s..." % source)
    # Make sure we are really tracking ALL the antennas for sensible results
    if not debug :
        sc.track(source, waiton = ALL)
    lm.message("Target acquired. Destruction sequence initiated. Please stand back.")

# For now the thresholds and delays have to be set explicitly.
# The following flag is used to make sure that this is done the first time the
# correlator is configured.

corrConfigured = False

fIndex = 0

for i in range(numChunks) :
    # Get elevation range
    for ant in currentAntennas :
        mpString = device.CarmaAnt().getName(ant) + \
                   ".AntennaCommon.Drive.Track.actualElevation"
        thisEl = sc.queryDouble(mpString, retries = 20)
        minEl = min(thisEl, minEl)
        maxEl = max(thisEl, maxEl)

    # Set up bands
    index = 0
    for bandNo in bands :
        lm.message("Configuring band %d" % bandNo, noLog = True)

        # Account for extra downconversion when block downconverter is required
        if ifFreq > 5.00 :
            fr = blockDcFreq - ifFreq
        else :
            fr = ifFreq

        # Check if the LO will be in range with the specified sideband.
        # If not, then force other sideband.
        sb = sideband
        if (sb == 'LSB') and (fr + basebandFreq > max2ndLo) :
            sb = 'USB'
        elif (sb == 'USB') and (fr - basebandFreq < min2ndLo) :
            sb = 'LSB'

        lm.message("IF frequency = %.3f GHz" % ifFreq)
        lm.message("    sideband = %s" % sb)
        skyFreq = ifFreq + loFreq
        if not debug :
            if sb == 'LSB' :
                sc.configband(band = bandNo, bw = bandwidth[index], \
                              fcenter = skyFreq, sb2 = sc.LSB)
            elif sb == 'USB' :
                sc.configband(band = bandNo, bw = bandwidth[index], \
                              fcenter = skyFreq, sb2 = sc.USB)
            elif sb == 'AUTO' :
                sc.configband(band = bandNo, bw = bandwidth[index], \
                              fcenter = skyFreq, sb2 = sc.AUTO)
            if not corrConfigured :
                lm.message("Setting up correlator thresholds and delays")
                sc.flattenPhases()
                sc.optimizeThresholds()
                corrConfigured = True
        freqList[fIndex] = ifFreq

        # If we want the bands end-to-end, increment frequency now
        if mode == 'series' :
            ifFreq += fStep
            fIndex += 1
        index += 1

    # Set up the IF power levels and measure Tsys at the same time
    lm.message("Measuring Tsys")
    if not debug : sc.tsys(ifsetup = True)
    sc.sleep(4)

    # Measure gains, and calculate rejection ratio.
    if source != None :

        lm.message("Measuring sideband ratios")

        # Set up storage for averaging antenna gain amplitudes
        ampLsb = [[0.0 for idx in range(numBands)] for jdx in range(numAnts)]
        ampUsb = [[0.0 for idx in range(numBands)] for jdx in range(numAnts)]
        visL = [[(0.0 + 0.0j) for idx in range(numBands)] for jdx in range(numAnts)]
        visU = [[(0.0 + 0.0j) for idx in range(numBands)] for jdx in range(numAnts)]

        # Do a 'quick' integration so script is not blocked during integration
        # Read selfcal amplitudes during integration, with allowance for errors
        # in timing.
        guardTime = 3       # Time to pad on either side of integration to avoid bad values
        if not debug :
            sc.qinteg(integrationTime, integrationCounts)
        tStart = time.time()
        sc.sleep(integrationTime + guardTime)
        while (time.time() - tStart) < (totalTime - integrationTime - 2 * guardTime) :
            for ant in currentAntennas :
                index = 0
                for bandNo in bands :
                    mpString = "SlPipeline.Input%d.Band%d.VisAverages.Integrated.Lsb" % (ant, bandNo)
                    queryVal = sc.queryComplex(mpString, retries = 20)
                    visLsb = complex(queryVal[0], queryVal[1])
                    (visL[ant - 1])[index] += visLsb
                    (ampLsb[ant - 1])[index] += abs(visLsb)
                    mpString = "SlPipeline.Input%d.Band%d.VisAverages.Integrated.Usb" % (ant, bandNo)
                    queryVal = sc.queryComplex(mpString, retries = 20)
                    visUsb = complex(queryVal[0], queryVal[1])
                    (visU[ant - 1])[index] += visUsb
                    (ampUsb[ant - 1])[index] += abs(visUsb)
                    index += 1
            sc.sleep(integrationTime / 2)

        for ant in currentAntennas :
            index = 0
            for bandNo in bands :
                aLsb = (ampLsb[ant - 1])[index]
                aUsb = (ampUsb[ant - 1])[index]

                # Avoid possible divide-by-zero
                aLsb = max(aLsb, 0.0001)
                aUsb = max(aUsb, 0.0001)
                gainRatio =  aUsb / aLsb

                vl = (visL[ant - 1])[index]
                vu = (visU[ant - 1])[index]
                aLsb = max(abs(vl), 0.0001)
                aUsb = max(abs(vu), 0.0001)
                gainRatio =  aUsb / aLsb
                (gainRatioList[ant - 1])[i * numBands + index] = gainRatio

                mpString = "slPipeline.Input%d.Band%d.Tsys.Dsb" % (ant, bandNo)
                gainRatio = (gainRatioList[ant - 1])[i * numBands + index]
                tSysDsb = sc.queryDouble(mpString, retries = 20)
                tSysLsb = (1.0 + 1.0 / gainRatio) * tSysDsb
                tSysUsb = (1.0 + gainRatio) * tSysDsb
                (tSysList[2 * (ant - 1)])[numBands * i + index] = tSysLsb
                (tSysList[2 * (ant - 1) + 1])[numBands * i + index] = tSysUsb
                mpString = "sldc.Band%d.Input%d.psys" % (bandNo, ant)
                pSys = sc.queryDouble(mpString, retries = 20)
                (pSysList[ant - 1])[numBands * i + index] = pSys
                index += 1

    elif mode == 'series' :
        for ant in currentAntennas :
            index = 0
            for bandNo in bands :
                mpString = "slPipeline.Input%d.Band%d.Tsys.Dsb" % (ant, bandNo)
                tSysDsb = sc.queryDouble(mpString, retries = 20)
                (tSysList[ant - 1])[numBands * i + index] = tSysDsb
                mpString = "sldc.Band%d.Input%d.psys" % (bandNo, ant)
                pSys = sc.queryDouble(mpString, retries = 20)
                (pSysList[ant - 1])[numBands * i + index] = pSys
                index += 1

    elif mode == 'parallel' :
        for ant in currentAntennas :
            index = 0
            for bandNo in bands :
                mpString = "slPipeline.Input%d.Band%d.Tsys.Dsb" % (ant, bandNo)
                tSysDsb = sc.queryDouble(mpString, retries = 20)
                (tSysList[numBands * (ant - 1) + index])[i] = tSysDsb
                mpString = "sldc.Band%d.Input%d.psys" % (bandNo, ant)
                pSys = sc.queryDouble(mpString, retries = 20)
                (pSysList[numBands * (ant - 1) + index])[i] = pSys
                index += 1

        # In parallel mode, increment frequency only after setting all bands
        fIndex += 1
        ifFreq = ifFreq + fStep

    timePerIteration = (time.time() - measureStartTime) / (i + 1)
    elapsedTime = time.time() - startTime
    timeLeft = (numChunks - i - 1) * timePerIteration
    lm.message("Time left: " + ah.dhms(timeLeft) + ", Elapsed time: " + ah.dhms(elapsedTime))
# End of measurement loop


# Make sure we captured the elevation range, even if antennas not moved
for ant in currentAntennas :
    mpString = device.CarmaAnt().getName(ant) + \
               ".AntennaCommon.Drive.Track.actualElevation"
    thisEl = sc.queryDouble(mpString, retries = 20)
    minEl = min(thisEl, minEl)
    maxEl = max(thisEl, maxEl)

#----File writing----
#
# Open a new file to store the results and add header info.
# If accessible the results are stored in the /array/rt/ area. If not
# then they are stored in /tmp/.
# Within that tree, the results are in /tsysVsIfFreq/<freq>/<date>/
# where <freq> is the frequency, rounded to the nearest GHz, and <date>
# is the date in the form, e.g.,  2008-Jun-15.

pathName = ("web/tsysVsIfFreq/%.0fGHz/" % loFreq) + time.strftime("%Y-%b-%d/")
dataPath = ah.getDataPath(pathName, verbose = True)
fileName = ("tsysVsIfFreq,%.0fGHz," % loFreq) + time.strftime("%Y-%b-%d,%Hh%Mm%Ss")
if debug :
    fileName = "DEBUG-" + fileName
resultsFileName = fileName + ".txt"
lm.message("Saving data in: " + resultsFileName)
resultsFile = open(dataPath + resultsFileName, "w")

# Write header and other information into data file
resultsFile.write("System noise temperature as a function of IF frequency\n")
resultsFile.write(idString + "\n\n")
if debug :
    resultsFile.write("debug = True\n\n")

tau225 = sc.queryDouble("Weather.tau225", retries = 20)
resultsFile.write("             Date: " + time.strftime("%Y-%b-%d %Z%z\n"))
resultsFile.write("       Start time: " + time.strftime("%H:%M:%S") + "\n")
resultsFile.write("     LO frequency: %.3f GHz\n" % loFreq)
resultsFile.write("    Req. sideband: %s\n" % sideband)
resultsFile.write("Filter bandwidths: " + bwListStr + "\n")
resultsFile.write("    tau (225 GHz): %.2f" % tau225)

if (abs(maxEl - minEl) < 1) :
    elRangeStr = "\n               El: %.1f deg" % (0.5 * (minEl + maxEl))
else :
    elRangeStr = "\n         El range: %.1f-%.1f deg" % (minEl, maxEl)
resultsFile.write(elRangeStr)

numBandCols = 3         # Max. number of bands to list under each antenna

# Write the Tsys data into the results file

if source != None :
    resultsFile.write("\n\nNoise temperatures, K:\n\n")
    resultsFile.write("    f, GHz |-- Tsys SSB -->\n")
    resultsFile.write(" ---------")
    for ant in currentAntennas :
        resultsFile.write(" ------------ C%02d ------------" % ant)
    resultsFile.write("\n")
    resultsFile.write("          ")
    for ant in currentAntennas :
        resultsFile.write("       LSB       USB   USB/LSB")
    resultsFile.write("\n")

    for i in range(numSteps) :
        resultsFile.write("%10.3f" % freqList[i])
        for ant in currentAntennas :
            tSysLsb = (tSysList[2 * (ant - 1)])[i]
            tSysUsb = (tSysList[2 * (ant - 1) + 1])[i]
            sbr = (gainRatioList[(ant - 1)])[i]
            resultsFile.write("%10.1f%10.1f%10.3f" % (tSysLsb, tSysUsb, sbr))
        resultsFile.write("\n")

elif mode == 'series' :
    resultsFile.write("\n\nNoise temperatures, K:\n\n")
    resultsFile.write("    f, GHz |-- Tsys DSB -->\n")
    resultsFile.write(" ---------")
    for ant in currentAntennas :
        resultsFile.write(" -- C%02d --" % ant)
    resultsFile.write("\n")
    for i in range(numSteps) :
        resultsFile.write("%10.3f" % freqList[i])
        for ant in currentAntennas :
            tSys = (tSysList[ant - 1])[i]
            resultsFile.write("%10.1f" % tSys)
        resultsFile.write("\n")


elif mode == 'parallel' :
    resultsFile.write("\n\nNoise temperatures, K:\n\n")
    resultsFile.write("    f, GHz   |-- Tsys DSB -->\n")
    resultsFile.write(" ---------  ")
    for ant in currentAntennas :
        resultsFile.write(" ------------ C%02d ------------" % ant)
    resultsFile.write("\n")

    col1 = 0
    col2 = numBandCols
    while len(bands[col1 : col2]) > 0 :
        resultsFile.write("          ")
        for ant in currentAntennas :
            for bandNo in bands[col1 : col2] :
                resultsFile.write("     Band%1d" % bandNo)
            resultsFile.write((numBandCols - len(bands[col1 : col2])) * "          ")
        resultsFile.write("\n")

        for i in range(numSteps) :
            resultsFile.write("%10.3f" % freqList[i])
            for ant in currentAntennas :
                index = col1
                for bandNo in bands[col1 : col2] :
                    tSys = (tSysList[numBands * (ant - 1) + index])[i]
                    resultsFile.write("%10.1f" % tSys)
                    index += 1
                resultsFile.write((numBandCols - len(bands[col1 : col2])) * "          ")
            resultsFile.write("\n")
        resultsFile.write("\n")
        col1 = col2
        col2 += numBandCols

# Write the psys data into the results file

if (source != None) or (mode == 'series') :
    resultsFile.write("\n\nSystem power, dBm:\n\n")
    resultsFile.write("    f, GHz |-- Psys ------>\n")
    resultsFile.write(" ---------")
    for ant in currentAntennas :
        resultsFile.write(" -- C%02d --" % ant)
    resultsFile.write("\n")
    for i in range(numSteps) :
        resultsFile.write("%10.3f" % freqList[i])
        for ant in currentAntennas :
            pSys = (pSysList[ant - 1])[i]
            resultsFile.write("%10.1f" % pSys)
        resultsFile.write("\n")

elif mode == 'parallel' :
    resultsFile.write("\n\nSystem power, dBm:\n\n")
    resultsFile.write("    f, GHz   |-- Psys DSB -->\n")
    resultsFile.write(" ---------  ")
    for ant in currentAntennas :
        resultsFile.write(" ------------ C%02d ------------" % ant)
    resultsFile.write("\n")

    col1 = 0
    col2  = numBandCols
    while len(bands[col1 : col2]) > 0 :
        resultsFile.write("          ")
        for ant in currentAntennas :
            for bandNo in bands[col1 : col2] :
                resultsFile.write("     Band%1d" % bandNo)
            resultsFile.write((numBandCols - len(bands[col1 : col2])) * "          ")
        resultsFile.write("\n")

        for i in range(numSteps) :
            resultsFile.write("%10.3f" % freqList[i])
            for ant in currentAntennas :
                index = col1
                for bandNo in bands[col1 : col2] :
                    pSys = (pSysList[numBands * (ant - 1) + index])[i]
                    resultsFile.write("%10.1f" % pSys)
                    index += 1
                resultsFile.write((numBandCols - len(bands[col1 : col2])) * "          ")
            resultsFile.write("\n")
        resultsFile.write("\n")
        col1 = col2
        col2 += numBandCols

lm.message("Closing data file")
resultsFile.close()

# ----Plot data----
#
# First, Tsys
lm.message("Generating plots")
if loFreq > 150.0 :
    tMin = 0
    tMax = 300
else :
    tMin = 0
    tMax = 200
if source != None :
    tMin *= 2
    tMax *= 2

if source == None :
    plotTitle = "DSB System Noise Temperature vs IF Frequency"
else :
    plotTitle = "SSB System Noise Temperature vs IF Frequency"

if (abs(maxEl - minEl) < 1) :
    elRangeStr = "\nEl = %.1f deg" % (0.5 * (minEl + maxEl))
else :
    elRangeStr = "\nEl range = %.1f-%.1f deg" % (minEl, maxEl)

infoTextLeft = ("LO freq = %.3f GHz\ntau (225 GHz) = %.2f\nBW = " % (loFreq, tau225)) + \
               bwListStr + elRangeStr + ("\nReq. sideband: %s" % sideband)
infoTextRight = time.strftime("%Y-%b-%d")
xLabel = "IF frequency, GHz"
yLabel = "Tsys, K"

legends = []
if source != None :
    legends.append("LSB")
    legends.append("USB")
elif mode == 'series' :
    legends.append("All bands")
elif mode == 'parallel' :
    for i in bands :
        legends.append("Band %d" % i)

lineStyles = ('b-', 'r-', 'g-', 'm-', 'c-', 'y-', 'b--', 'r--')

plotFileName = dataPath + fileName + "(Tsys)"

err = sp.allAntennaMultiPlot(freqList, tSysList, currentAntennas, minIf, maxIf, tMin, tMax, \
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

if source != None :
    legends = []
    legends.append("All bands")

plotFileName = dataPath + fileName + "(Psys)"

err = sp.allAntennaMultiPlot(freqList, pSysList, currentAntennas, minIf, maxIf, pMin, pMax, \
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

# Third, sideband ratio
if source != None :
    gMin = 0.1
    gMax = 10
    plotTitle = "Sideband ratio vs IF Frequency"
    yLabel = "Gain USB/LSB"

    plotFileName = dataPath + fileName + "(SBR)"

    err = sp.allAntennaMultiPlot(freqList, gainRatioList, currentAntennas, minIf, maxIf, gMin, gMax, \
        plotTitle = plotTitle, xLabel = xLabel, yLabel = yLabel, \
        infoTextLeft = infoTextLeft, infoTextRight = infoTextRight, \
        legends = legends, lineStyles = lineStyles, \
        plotFileName = plotFileName, makePdf = True, makePng = True, \
        orientation = 'portrait', axisStyle = 'logy')
    if err :
        lm.message("Error plotting Psys")
    else :
        lm.message("Plots made:")
        lm.message("  " + fileName + ".pdf")
        lm.message("  " + fileName + ".png")

lm.close()
sc.alarmenable()

# End tsysVsIfFreq
