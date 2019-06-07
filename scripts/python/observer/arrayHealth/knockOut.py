import numpy as np
from itertools import ifilterfalse
import subprocess
import shlex
import sys
import time
import subarrayCommands

#    Purpose:
#    Checks to make sure RR/LL/RL/LR are labeled correctly in the data. It retrieves
#    the current PAM IF atten. setting on each of the 30 Rx's, increases the setting 
#    by 10 dB to make Tsys shoot up on any baselines associated with that Rx, integrates, 
#    and resets the PAM setting to its original value.

#    Responsible party: Chat Hull (chat@astro.berkeley.edu) and
#    Dick Plambeck (plambeck@astro.berkeley.edu)

newProject('ct002', 'knockOut', '', False)

sourceName = '3c454.3'

# Track the requested source
track(sourceName)

lo1 = 241.5

# Purpose: bandpass calibration (no grids)
freq(lo1,USB,0.,None)
clearastroband(0)
configastroband(1, "FULLSTOKES", BW500, lo1 + 2.25)
configastroband(3, "FULLSTOKES", BW500, lo1 + 2.75)
configastroband(5, "FULLSTOKES", BW500, lo1 + 7.25)
configastroband(7, "FULLSTOKES", BW500, lo1 + 7.75)
optimizeThresholds()
flattenPhases()
tsys(ifsetup=True)

# List of antennas in current array
antList = currentAntennaNumbers()    
# Names (e.g. ovro3) of antennas in current array
nameList = typedAntennaNames()       
polList = ["L","R"]

for n,ant in enumerate(antList) :
    for pol in range (1,3) :              
        # Retrieve current PAM setting
        attenSave = queryDouble( nameList[n] + ".AntennaIfContainer" + str(pol) + ".AntennaIF.attenSet", 20) 
        print str(ant) + polList[pol-1], attenSave
        # Increase setting by 10 dB
        attenTarget = attenSave + 10
        antennaIFatten( attenTarget, pol, ant )
        time.sleep(5)
        integrate (3,10)
        antennaIFatten( attenSave, pol, ant )