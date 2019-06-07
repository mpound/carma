
# Script to detect when azimuth overlap switch engages for each OVRO antenna.
#
# @author Andrew Beard
# $Id: azOverlapSwitch.py,v 1.6 2009/04/17 16:59:10 scott Exp $
#
# $CarmaCopyright$
#

#-------------------------- Help --------------------------
"""The azimuth overlap switch must engage at a positive azimuth for the drives
to function properly.  Temporal changes in these values likely indicate
loose or shifting hardware and should be investigated immediately.  Failure
to do so may lead to catastrophic wrapups.

Execute this with "run 'azOverlapSwitch'" or
                   "run 'azOverlapSwitch', ants=2" or
                   "run 'azOverlapSwitch', ants=[1,2,5]"
"""
#------------------------ End of Help ------------------------

import os
import time
import carmaHelpers
import subarrayCommands
from numpy import *
import runCommand as rc


# ------------ Parameters -------------
p = rc.Params()
p.add("ants", type=rc.antlist, default=[0], 
       description="Antenna numbers")
p.processInputParameters()
# ------------ End parameters -------------

def datadir() :
    firstChoice = '/array/rt/azOverlap'
    secondChoice = '/tmp'
    try : 
        os.stat(firstChoice)
    except Exception :
        return secondChoice
    return firstChoice
            
def getAntsWithMatchingAzOverlap( on, antlist, overlapMpNames ):
    matchingAnts = []
    for ant in antlist:
        azOverlap = queryBool( overlapMpNames[ant], 1 )
        if azOverlap == on:
            matchingAnts.append( ant )
    return matchingAnts

def toggleAzOverlapSwitch(antlist, on, overlapMpNames, absAzDeltaArcMinutes):
    global getAntsWithMatchingAzOverlap

    msg = " Toggling az overlap switch "

    if on: 
        azDeltaArcMinutes = -absAzDeltaArcMinutes
        msg += "on"
    else: 
        azDeltaArcMinutes = absAzDeltaArcMinutes 
        msg += "off"

    print "%s for antennas %s using %d' increments." % \
        (msg, antlist, azDeltaArcMinutes)
    
    untoggled = getAntsWithMatchingAzOverlap( not on, antlist, overlapMpNames )
    while len( untoggled ) != 0:
        incoffset( azDeltaArcMinutes, 0.0 , untoggled, waiton=ALL ) 
        untoggled = getAntsWithMatchingAzOverlap( not on, antlist, overlapMpNames )

def collectAzimuths( antAzDict, azMpNames, retries ):
    for ant in antAzDict.keys():
        antAzDict[ant].append( queryDouble( azMpNames[ant], retries ) )
        
# User all current ovro antennae by default
if p.ants[0] == 0:
    antsByType = antennasByType(currentAntennaNumbers())
    p.ants = antsByType[0]

azOverlapMpNames = {}
actualAzMpNames = {}
azOverlapTriggered = {}
actualAzimuth = {}
for ant in p.ants:
    if ant > 6:
        raise Exception, "Antennas must be between 1-6" 

    azOverlapMpNames[ant] = "Ovro%d.Drive.DriveModule.azOverlap" % ant
    actualAzMpNames[ant] = "Ovro%d.Drive.Track.actualAzimuth" % ant
    azOverlapTriggered[ant] = False
    actualAzimuth[ant] = None

# Make sure we can open the output file
outputDir = datadir()
now = time.localtime()
filename = "%s/azOverlap.%s.txt" % \
            ( outputDir, time.strftime( "%y%B%d-%H:%M", now ) )
print ""
print "Results will be saved in %s" % filename
print ""
file = open( filename, 'a' )

# The azimuth overlap switch displays a sort of mechanical hysteresis in 
# that it triggers at an azimuth 1 degree or so below the azimuth it 
# disengages at.

# Start at a known azimuth and elevation.  Things will go much quicker 
# if all azimuth overlap switches are disengaged here.
initialAz = 20.0
initialEl = 45.0
print "Slewing to starting position (az=%.1f, el=%.1f)." % (initialAz,initialEl)
offset( 0.0, 0.0, p.ants, waiton=NONE ) # Reset any offsets
move( initialAz, initialEl, p.ants, waiton=ALL )

# Check for gross convergence by slewing to 0 az and observing when each
# az overlap switch triggers.
targetAz = -2.0
print "Finding gross overlap switch trigger azimuths."
move( targetAz, initialEl, p.ants, waiton=NONE )
toggled = []
while len( toggled ) != len( p.ants ):
    toggled = getAntsWithMatchingAzOverlap( True, p.ants, azOverlapMpNames )
    if len( toggled ) > 0:
        stop( toggled )
    time.sleep( 0.5 )

retries = 1
grossAzimuths = {}
for ant in p.ants:
    grossAzimuths[ant] = []

collectAzimuths( grossAzimuths, actualAzMpNames, retries )

for ant in grossAzimuths.keys():
    az = grossAzimuths[ant]
    move( az[0], initialEl, ant, waiton=NONE )
    print " Antenna %d overlap switched at roughly %.1f degrees." % (ant,az[0])

print ""

# Now converge on fine-scale overlap switch engage and disengage values.
switchOnValues = {} # Dictionary of a list
switchOffValues = {} # Dictionary of a list
for ant in p.ants:
    switchOnValues[ant] = []
    switchOffValues[ant] = []

fineAzIncArcMin = 15 
nSamps = 3 
print "Measuring fine scale on/off values (will toggle %d times)." % (nSamps*2)
for i in range( nSamps ):
    toggleAzOverlapSwitch( p.ants, False, azOverlapMpNames, fineAzIncArcMin )
    collectAzimuths( switchOffValues, actualAzMpNames, retries )
    toggleAzOverlapSwitch( p.ants, True, azOverlapMpNames, fineAzIncArcMin )
    collectAzimuths( switchOnValues, actualAzMpNames, retries )

offset( 0.0, 0.0, p.ants) # Reset offsets

print ""

# Tally and output the results
sumStr = "Using %d iterations and %.1f' azimuth increments on ants %s." % \
    (nSamps, fineAzIncArcMin, p.ants)
file.write( sumStr + "\n" ) 

for a in p.ants:
    onArray = array(switchOnValues[a])
    offArray = array(switchOffValues[a])
    outstr = "Antenna %d toggles on at %.1f+/-%.1f and off at %.1f+/-%.1f." %\
        (a,average(onArray),std(onArray),average(offArray),std(offArray))
    print outstr
    file.write( outstr + "\n" )
    

