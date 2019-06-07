# Script to measure drive system gain and offsets.
#
# Results of these measurements should go into conf/antenna/ovro/drive.conf.
#
# @author Andy Beard
# $Id: driveGains.py,v 1.4 2008/02/28 00:09:42 abeard Exp $
#
# $CarmaCopyright$
#

import math
import numpy
import time
from device import *
from subarrayCommands import *

def setDriveVoltages( antHandles, voltage, axis ):
    for a in antHandles:
        if axis == 'azimuth':
            a.drive().setRawDriveVoltages( voltage, 0.0 )
        elif axis == 'elevation':
            a.drive().setRawDriveVoltages( 0.0, voltage )

def queryDriveRates( ants, numSamples, axis ):
    d=dict()
    for ant in ants:
        antName = ant.getName()
        d[antName] = list() 

    for n in xrange( numSamples ):
        for ant in ants:
            antName =ant.getName()
            if axis=='azimuth':
                monName="%s.Drive.Track.azimuthRate" %(antName.capitalize())
            elif axis=='elevation':
                monName="%s.Drive.Track.elevationRate" %(antName.capitalize())
            else:
                print "ERROR"
                return 0.0
            queryResult = queryDouble( monName )
            (d[antName]).append( queryResult )

        time.sleep( 1.0 ) # Guarantee that we get distinct samples

    avgList = list()
    for ant in ants:
        name = ant.getName()
        sampArray = numpy.array( d[name] )
        avg = sampArray.mean( ) 
        sd = numpy.std( sampArray ) 
        print "    %s Average over %d samples: %.5f +/-%.5f deg/min." \
              %(name, numSamples, avg, sd)
        avgList.append( avg )

    return numpy.array( avgList )

def findGains( antlist, axis ):
    """Find drive gains for specified antennas on specified axis.

       antlist: List of ovro antenna numbers.
       axis: one of either 'azimuth' or 'elevation'"""

    ovroAntHandles = [ Ovro(ant) for ant in antlist ]
        
    # WARNING leverarm calcs depend on this 45.0 degree elevation!
    move( 135.0, 45.0, antlist ) 

    voltages=numpy.array( [-0.50, -0.10, -0.050, -0.020, -0.010, 0.00, \
                            0.50,  0.10,  0.050, 0.020, 0.010 ] )

    indexToNameList = list()
    numvoltages = ( numpy.shape( voltages ) )[0]
    numants = 0
    for ant in ovroAntHandles:
        indexToNameList.append( ant.getName() )
        numants += 1
        
    avgRates = numpy.zeros( (numants, numvoltages) )
    vcolumn = 0
    for voltage in voltages: 
        print "Setting %s drive voltage to %.3fV..." %(axis, voltage)
        setDriveVoltages( ovroAntHandles, voltage, axis )
        time.sleep( 4 )
        rates = queryDriveRates( ovroAntHandles, 10, axis )
        avgRates[:,vcolumn] = rates
        vcolumn += 1

    print "********** Final Results *************"
    antidx = 0
    for antRow in avgRates:
        linearfit = numpy.polyfit( voltages, antRow, 1 ) 
        gain = linearfit[0];
        if axis == 'elevation':
            # WARNING leverarm correction is 0.85 @ 45 degrees elevation!
            gain *= 0.85 
        dc_offset = -1.0 * linearfit[1] / gain 
        print "%s: %s dc_offset=%.3f Volts, gain=%.3f deg/min" \
               %(indexToNameList[antidx], axis, dc_offset, gain)
        antidx += 1 

    return avgRates 
