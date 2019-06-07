""" Holography data extraction script

    Data from a holography session run using the 'ct001_holography.obs'
    observing script are extracted from the relevant miriad file in
    a form for processing by Mathcad or other data reduction software.

    Miriad data is obtained by parsing output from standard Miriad commands
    such as 'uvindex'. At some point this should be replaced by a Python
    wrapper for the Miriad structure.
"""
import os

numWindows = 6
maxAnts = 15

#==============================================================================
# REQUIRED FUNCTIONS
#==============================================================================

def scanForSources(uvholData) :
    """ Scans the output of the Miriad 'uvhol' command for sources.

        Parameters:

            uvholData     Output listing from uvhol

        Return:

            Returns a list of sources in the file

        This function parses the output of the uvhol command for source names.
        It adds the source names to a list that it returns. If the
        noise source is in the Miriad data it is not added to the list,
        and an informational message is printed.
    """
    print "\n Checking for sources in Miriad data set"
    for line in range(len(uvholData)) :
        if uvholData[line].startswith("[Sources]") :
            break
    line += 1
    sourceList = []
    while not uvholData[line].startswith("#") :
        nextSource = uvholData[line].strip(' \n')
        if nextSource.lower() != "noise" :
            sourceList.append(nextSource)
        else :
            print "   Info: File contains noise source data"
        line += 1

    print "   Sources in file:"
    for source in sourceList :
        print "     " + source
    return sourceList
# end scanForSources


def scanForTestAntennas(uvholData) :
    """ Scans the output of the Miriad 'uvhol' command for moving antennas.

        Parameters:

            uvholData     Output listing from uvhol

        Return:

            Returns a list of antennas with non-zero offsets

        This function parses the output from the Miriad 'uvhol'
        command for antennas that have non-zero offsets.
    """

    print "\n Checking for test antennas in Miriad data set"
    for line in range(len(uvholData)) :
        if uvholData[line].startswith("[Moving Antennas]") :
            break
    line += 1
    antennaList = []
    while not uvholData[line].startswith("#") :
        antennaList.append(int(uvholData[line]))
        line += 1

    print "\n   Moving antennas in file:"
    for ant in antennaList :
        print "     C%d" % ant
    return antennaList

# end scanForTestAntennas


def scanForFrequencies(uvholData) :
    """ Scans the output of the Miriad 'uvlist' command for moving antennas.

        Parameters:

            uvholData     Output listing from uvhol

        Return:

            Returns a list of window frequencies

        This function directs the output from the Miriad 'uvlist'
        command to a temporary file, which it scans for window
        center frequencies.
    """

    print "\n Finding window center frequencies in Miriad data set"
    for line in range(len(uvholData)) :
        if uvholData[line].startswith("[Frequency]") :
            break
    line += 4
    freqList = []
    while not uvholData[line].startswith("#") :
        freqList.append(float(uvholData[line].split()[0]))
        line += 1

    print "\n   Band center frequencies:"
    for freq in freqList :
        print "     %.4f GHz" % freq
    return freqList

# end scanForFrequencies


#==============================================================================
# SETUP PARAMETERS
#==============================================================================

print "\n\n------------------------------------------------------------------"
print "    Holography data extraction script"
print "    " + "$Revision: 1.2 $".strip('$')
print "    James W. Lamb, Caltech"
print "------------------------------------------------------------------\n"

print "\n Removing any temporary files\n"
os.system("rm -rf *.tmp")
os.system("rm -rf temp*")

# Get user input describing the data set

print "----Data input----\n"
mirData = raw_input(" Miriad data set name: ")
while not os.path.exists(mirData) :
    mirData = mirData + '.mir'
    if not os.path.exists(mirData) :
        mirData = raw_input(" *** File does not exist. Re-enter or <return> to exit: ")
        if mirData == "" :
            raise Exception, "Miriad directory not found"

baseName = mirData.rstrip('/')
parts = os.path.splitext(baseName)
if parts[1].lower() == '.mir' :
    baseName = parts[0]
outputDir = baseName + ".holo"
if not os.path.exists(outputDir) :
    print "\n Creating directory %s for output data" % outputDir
    os.system("mkdir %s" % outputDir)

# Extract general information from the Miriad data set
os.system("uvhol vis=%s log=uvholdata.tmp" % mirData)
tf = open("uvholdata.tmp")
uvholData = tf.readlines()
tf.close()
os.system("rm -rf uvholdata.tmp")
sourceList = scanForSources(uvholData)
testAnts = scanForTestAntennas(uvholData)
freqList = scanForFrequencies(uvholData)

source = sourceList[0]
response = raw_input("\n Enter source name (%s): " % source)
if response != "" :
    source = response

response = raw_input(" Test antenna, or list of test antennas (%s): " % \
                     str(testAnts))
if response != "" :
    testAnts = eval(response)
if type(testAnts) == int :
    testAnts = [testAnts]
elif (type(testAnts) != list) or (max(testAnts) > maxAnts) :
    raise Exception, " *** Invalid antenna list"

refAnt = input(" Reference antenna for phase calibration: ")
if (refAnt > maxAnts) :
    raise Exception, " *** Invalid antenna number"

# Create a string for selecting antennas in Miriad commands

antSel = ""
for ant in testAnts :
    antSel = antSel + str(ant) + ','
antSel = antSel.rstrip(',')

#==============================================================================
# CALIBRATION
#==============================================================================

# First extract the data for the relevant test antenna and source

# Naming convention:
#   All temporary Miriad data sets start with 'temp.'
#   Visibilities for holography data, append data.n for stage n
#   of processing. Visibilities for central poining calibration data,
#   append cal.n for stage n of processing.
#
tempData1 = "temp.data.1.mir"
print "\n----Extracting data from %s----\n" % mirData
print " Test antenna(s): %s\n Source: %s" % (str(testAnts), source)
os.system("uvcat vis=%s out=%s 'select=ant(%s),source(%s)'" % \
          (mirData, tempData1, antSel, source))
os.system("uvlist_hol vis=%s options=spectra" % tempData1)

# Extract visibilities to use for calibration. Only baselines with no offset
# antennas are used
tempCal1 = "temp.cal.1.mir"
tempCal2 = "temp.cal.2.mir"
print "\n----Selecting out central pointings from %s for calibration----\n" \
       % mirData
select1 = "'select=-dazim(-10000.0,-0.1),-delev(-10000.0,-0.1),source(%s)'" % \
          source
select2 = "'select=-dazim(0.1,10000.0),-delev(0.1,10000.0)'"
os.system("uvcat vis=%s %s out=%s" % (mirData, select1, tempCal1))
os.system("uvcat vis=%s %s out=%s" % (tempCal1, select2, tempCal2))

# Bandpass and gain calibration
print "\n----Producing bandpass and gain tables----\n"
os.system("mfcal vis=%s 'select=source(%s)' refant=%d interval=0.1" % \
          (tempCal2, source, refAnt))
print "\n----Plotting passband magnitudes----\n"
os.system("gpplt vis=%s 'yrange(0,2)' device=/xs nxy=5,3 options=bandpass" \
          % tempCal2)
raw_input("Press Return to continue")
print "\n----Plotting passband phases----\n"
os.system("gpplt vis=%s yaxis=pha 'yrange(-180,180)' device=/xs nxy=5,3 options=bandpass" \
          % tempCal2)
raw_input("Press Return to continue")
print "\n----Plotting gain magnitudes----\n"
os.system("gpplt vis=%s 'yrange(0,2)' device=/xs nxy=5,3" % tempCal2)
raw_input("Press Return to continue")
print "\n----Plotting gain phases----\n"
os.system("gpplt vis=%s yaxis=pha 'yrange(-180,180)' device=/xs nxy=5,3 options=wrap" % \
          tempCal2)
print "\n----Applying gains to data set----\n"
tempData3 = "temp.data.3.mir"
os.system("gpcopy vis=%s out=%s" % (tempCal2, tempData1))
os.system("uvcat vis=%s out=%s" % (tempData1, tempData3))

#----Apply calibration and select final data set----
#
wideDataFile = "wide.%s" % mirData
print "\n----Selecting out wide data to %s----\n" % wideDataFile
# Reconstruct the wideband data from the channel data (reset=true)
tempData4 = "temp.data.4.mir"
os.system("uvwide vis=%s out=%s reset=true" % (tempData3, tempData4))
os.system("rm -rf %s" % wideDataFile)
os.system("uvcat vis=%s out=%s 'select=ant(%s),source(%s)' options=nochannel" % \
         (tempData4, wideDataFile, antSel, source))

#==============================================================================
# EXTRACT VISIBILITIES AND OFFSETS FOR TEST ANTENNA(S)
#==============================================================================

print "\n----Extracting visibilites for baselines witn antenna(s) %s---" % \
      str(testAnts)
os.system("uvhol vis=%s 'select=source(%s)' options=data line=wide \
           log=uvholdata.tmp refant=%d" % (wideDataFile, source, testAnts[0]))
try :
    tf = open("uvholdata.tmp")
except :
    raise Exception, "Unable to find scratch file"
uvholData = tf.readlines()
tf.close()
os.system("rm uvholdata.tmp -rf")
print "\n%d lines read from visibilities scratch file\n" % len(uvholData)

i = 0
while uvholData[i].find("[Vis]") == -1 :
    i += 1

# Parse out the space delimited values we want

visData = []
antList = []

start = i + 2
azOffset = []
for i in range(start, len(uvholData)) :
    if uvholData[i] != "" :
        vals = uvholData[i].split()
        ant1 = int(vals[1])
        ant2 = int(vals[2])
        if not ant1 in antList :
            antList.append(ant1)
        if not ant2 in antList :
            antList.append(ant2)
        dazim = float(vals[3])
        delev = float(vals[4])   # Note that this should be reversed in the data reduction.
        if dazim not in azOffset :
            azOffset.append(dazim)
        vis = []
        j = 6
        for w in range(numWindows) :
            vis.append(float(vals[j]))
            vis.append(float(vals[j + 1]))
            j += 3
        visData.append((ant1, ant2, dazim, delev, vis))

numRec = len(visData)
nGrid = len(azOffset)
stepSize = (max(azOffset) - min(azOffset)) / (nGrid - 1)
print "\n\n------------------------------------------------------------------"
print "\n %d visibilities read" % numRec
print " Grid size = %d" % nGrid
print " Step size = %.3f arcmin" % stepSize


#==============================================================================
# WRITE DATA TO FILES
#==============================================================================

print "\n----Writing data----\n"

refAnts = []
for a in antList :
    if a not in testAnts :
        refAnts.append(a)

for ant in testAnts :
    if (ant not in antList) :
        print "No visibilities for test antenna %d" % ant

    else :
        ignoreAnts = testAnts[ : ]
        ignoreAnts.remove(ant)
        print "\n Metadata: "
        fn = "%s/ant%d.txt" % (outputDir, ant)
        print "    %s\n" % fn
        os.system("rm -rf %s" % fn)
        metaData = open(fn, "w")
        metaData.write("Antenna:\t%d\n" % ant)
        metaData.write("Grid size:\t%d\n" % nGrid)
        metaData.write("Step size:\t%.3f\n" % stepSize)
        for win in range(numWindows) :
            metaData.write("Win %d freq:\t%.3f\n" % (win + 1, freqList[win]))
        metaData.write("Reference antennas: ")
        for a in refAnts :
            metaData.write("\t%d" % a)
        metaData.close()

        print " Output files:"
        for win in range(numWindows) :
            fn = "%s/ant%d.window%d.csv" % (outputDir, ant, win + 1)
            print "    %s" % fn
            exec("w%d = open(fn, 'w')" % (win + 1))
            exec("w%d.write('ant,daz,del,mag,pha\\n')" % (win + 1))

        for i in range(numRec) :
            vis = visData[i]
            ant1 = vis[0]
            ant2 = vis[1]
            dazim = vis[2]
            delev = vis[3]
            if (ant1 not in ignoreAnts) and (ant2 not in ignoreAnts) :
                for win in range(numWindows) :
                    mag = vis[4][2 * win]
                    pha = vis[4][2 * win + 1]
                    refAnt = ant1
                    if (ant1 != ant2) :
                        if (ant == ant1) :
                            pha = -pha
                            refAnt = ant2
                        exec("w%d.write('%d,%.3f,%.3f,%.3f,%.2f\\n')" % \
                            (win + 1, refAnt, dazim, delev, mag, pha))

        for win in range(numWindows) :
            exec("w%d.close()" % (win + 1))

#----Cleanup----
#
print "\n Removing all temporary files"
os.system("rm -rf temp*")

print "\nFinished\n"

# End extractHoloData
