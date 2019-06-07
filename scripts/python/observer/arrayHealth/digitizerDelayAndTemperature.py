# $Id: digitizerDelayAndTemperature.py,v 1.10 2012/09/26 23:26:15 iws Exp $
# Script to collect data for determining the dependence of the digitizer
# delays on their temperatures.
# by James W. Lamb, OVRO, Caltech, 25 March 2008
#
# @Author James W. Lamb, Caltech
#
# History
# 04-Jan-2010: JWL  Allow list of bands to monitor. Temporary fix for CARMA boards
# 07-Apr-2009: JWL  Modified to use Steve Scott's run command parsing
# 20-Nov-2008: JWL  Converted phases from radians to degrees
# 22-Oct-2008: JWL  Fixed errors revealed by pycheck.py
# 20-Oct-2008: JWL  Changed "from * import <xx>" to "import <xx> [as yy]"
#                   Standardized help info format
# 25-Jun-2008: Original version
#

#-------------------------- Help --------------------------
""" Record noise source visibility phase and digitizer temperatures.

    Usage:

        run('digitizerDelayAndTemperature', runTime = 1.0, interval = 5.0, bands = [1, 3, 5])

    This script records the phases for all of the correlator baselines
    and bands and the digitizer temperatures as an aid to calibrating
    the temperature dependence of the digitizer delay. Correlator bands
    should be set up before the script is called. The actual band
    configurations are not critical since the noise source is strong
    enough to give correlations in any bandwidth, and it is injected
    after the 2nd LO so the frequency is irrelevant.

    Data are written to files in /array/rt/web/digitizer/phase in
    .csv format (comma-separated values).

    Parameters

        runTime = 1.0
            Total elapsed time, hr

        interval = 5
            Interval between readings, s

        debug = False
            If 'True', the noise source will not be turned on and file names
            will be prepended with 'DEBUG-'

        bands = [1, 2, 3]
            List of bands that are to be measured
"""
#------------------------ End of Help ------------------------

#----Required imports----

import runCommand as rc
import subarrayCommands as sc
import time
import arrayHealthUtils as ah
import numpy as np
import printFunctions as pf
import carmaConstants as CONSTS

idString = "digitizerDelayAndTemperature.py\n" + "$Revision: 1.10 $".strip('$')
sc.rtdComment("Array health - digitizerDelayAndTemperature")
print "\n\n------------------------------------------------"
print idString
print "------------------------------------------------\n"

#----Parameters----
#
p = rc.Params()
p.add("runTime", type = float, default = 1.0,
       description = "Total time to take data over, h")
p.add("interval", type = float, default = 5,
       description = "Interval between samples, s")
p.add("debug", type = bool, default = False,
       description = "Run in debug mode (minimum system impact)")
p.add("bands", type = list, default = [1, 2, 3],
       description = "List of bands to monitor")

p.processInputParameters()
#
#----End Parameters----

runTime  = p.runTime
interval = p.interval
debug    = p.debug
bands    = p.bands

if debug :
    print "\nIn DEBUG mode\n"

#----Array parameters ---
#
numInputs = 15
inputs = range(1, numInputs + 1)
numBands = len(bands)
numBaselines = (numInputs * (numInputs - 1)) / 2
baselines = range(1, numBaselines + 1)
numDig = numInputs * numBands
print "\nNumber of bands = %d" % numBands
print "Number of digitizers = %d" % numDig
print "Number of baselines = %d" % numBaselines

#----File creation----
#
# Open a new file to store the results and add header info.
# If accessible the results are stored in the /array/rt/ area. If not
# then they are stored in /tmp/.
# Within that tree, the results are in /digitizers/phase/<date>/
# where <date> is the date in the form, e.g.,  2008-Jun-15.
pathName = "web/digitizer/phase/" + time.strftime("%Y-%b-%d/")
dataPath = ah.getDataPath(pathName, verbose = True)
fileName = "temp-and-phase," + time.strftime("%Y-%b-%d,%Hh%Mm%Ss")
if debug :
    fileName = "DEBUG-" + fileName
resultsFileName = fileName + ".csv"
pf.printMessage("\nSaving data in: " + resultsFileName + "\n")
pf.printMessage("\n***20-Nov-2008: Phases now in degrees, not radians\n")
resultsFile = open(dataPath + resultsFileName, "w")

# Write header information to file
#
resultsFile.write(idString + "\n")
resultsFile.write(time.strftime("%Y-%b-%d\n%Hh%Mm%Ss\n"))
resultsFile.write("\nTemp.,Units,K\n ,Num. cols,%d\n" % numDig)
resultsFile.write("Phases,Units,deg\n ,Num. cols,%d\n\n" % numBaselines)
resultsFile.write("time (s),")
for band in bands :
    for inp in inputs :
        if (inp % 2) == 1 :
            resultsFile.write("Temp Band%dDig%dA," % (band, (inp + 1) // 2))
        else :
            resultsFile.write("Temp Band%dDig%dB," % (band, (inp + 1) // 2))

for band in bands :
    for bl in baselines :
            resultsFile.write("Phase BL%d.Band%d," % (bl, band))
resultsFile.write("\n")

#----Set up noise source---
#
print "Turning on noise source"
if not debug : sc.noiseon()

#---Data acquisition loop---
#
i = 1
startTime = time.time()
stopTime = startTime + 3600 * runTime
while time.time() <= stopTime :

    tLeft = ah.dhms(stopTime - time.time()) + " remaining"
    sc.rtdComment("Array health - digitizerDelayAndTemperature : " + tLeft)
    print "%6d:  %s" % (i, tLeft)

    resultsFile.write("%.1f," % (time.time() - startTime))
    # Read the temperatures for digitizers A and B on each board
    for band in bands :
        for inp in inputs :
            mpQuery = "CarmaSlcBand%d.Digitizer%d.Temperature.Fpga1" % (band, (inp + 1) // 2)
            digTemp = sc.queryDouble(mpQuery, 20)
            resultsFile.write("%.3f," % digTemp)

    for band in bands :
        for bl in baselines :
            mpQuery = "SlPipeline.Baseline%d.BandContainer%d.VisAverages.Frame.Lsb" % (bl, band)
            vis = sc.queryComplex(mpQuery, 20)
            resultsFile.write("%.4f," % ((np.arctan2(vis[1], vis[0])) / CONSTS.DEG))

    resultsFile.write("\n")
    sc.sleep(interval - 0.15) # Approximate correction for how long it takes to execute the loop
    i += 1

resultsFile.close()
sc.rtdComment("")
print "Turning off noise source"
if not debug : sc.noiseoff()

# end digitizerDelayandTemperature
