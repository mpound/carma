#
# $Id: holo.py,v 1.8 2011/12/16 20:50:48 iws Exp $
#
# holography script for 6m antennas on transmitter
#
#    setup - initial setup for holography:
#       - tune receivers
#       - set up correlator (bw2, bw8, bw31)
#       - point to nominal transmitter position
#
#    peakup - peak up pointing on the transmitter:
#       - scan telescope over a grid centered on starting position
#       - find offset with highest downconverter psys
#       - move telescope to that offset


import sys
import carmaIni
import carma
import Subarray
from subarrayCommands import *
import device
import math as m
import time
import datetime
import fpformat
import numpy

# these center positions were measured 11oct08, in C-array
az0 = {  7 : 159.691176,
         8 : 160.080784,
         9 : 159.930850,
        10 : 160.131992,
        11 : 160.291735,
        12 : 159.300520,
        13 : 159.712035,
        14 : 159.682919,
        15 : 159.412232 }
el0 = {  7 :  3.386966,
         8 :  3.343742,
         9 :  3.346734,
        10 :  3.352461,
        11 :  3.354384,
        12 :  3.299858,
        13 :  3.355135,
        14 :  3.271092,
        15 :  3.334919 }

# tune, set up correlator, track transmitter
def setup() :
    qmove(160,10)
    freq( 90.4, LSB, 2.0, None)
    configband(1,BW2,90.4)
    configband(2,BW8,90.4)
    configband(3,BW31,90.4)
    amb(tmo=20)
    psysPreset()
    sleep(3.1)
    sky(tmo=20)
    track('TRANS')

# go to nominal center position - typically is offset by about (6,1) arcmin from "trans" position
def center() :
    deltaAz = {}
    deltaEl = {}
    moveList = [7,8,9,10,11,12,13,14,15]
    for ant in range(7,16,1) :
       radioAperture(True,ant)
       deltaAz[ant] = 0.
       deltaEl[ant] = 0.
       qmove(az0[ant],el0[ant],ant)
    print "wait for antennas 7-15 reach center position"
    offsetWait( deltaAz, deltaEl, moveList )
    print "all antennas have reached center"

# peak up pointing on transmitter for a single telescope
def peakup(ant,nbyn,step) :

    moveList = []
    moveList.append(ant)
    radioAperture(True,ant)
    print "peak up ant %d, %d x %d grid, step %.2f arcmin" % (ant,nbyn,nbyn,step)

    deltaAz0 = {}
    deltaEl0 = {}
    deltaAz = {}
    deltaEl = {}
    
    deltaAz0[ant] = queryDouble("Bima"+str(ant-6)+".AntennaCommon.Drive.Point.offsetAz", 24)
    deltaEl0[ant] = queryDouble("Bima"+str(ant-6)+".AntennaCommon.Drive.Point.offsetEl", 24)
    print "start with offset (%.2f,%.2f)" % (deltaAz0[ant],deltaEl0[ant])
    deltaAz[ant] = deltaAz0[ant]
    deltaEl[ant] = deltaEl0[ant]

    pwr = numpy.ones([nbyn,nbyn])
    nh = int(nbyn/2)
    pmax = -100.
    for j in range( -nh, nh+1, 1 ) :
        deltaEl[ant] = float(j)*step + deltaEl0[ant]
        for i in range( -nh, nh+1, 1 ) :
#            print "daz0,del0 = %.2f,%.2f  i,j = %d,%d  daz,del = %.2f,%.2f" % \
#                (deltaAz0[ant],deltaEl0[ant],i,j,deltaAz[ant],deltaEl[ant])
            deltaAz[ant] = float(i)*step + deltaAz0[ant]
            offsetWait(deltaAz,deltaEl,moveList)
            pwr[i][j]=queryDouble("Sldc.Band1.Input"+str(ant)+".psys")
            print "offset (%6.2f, %6.2f)  pwr %.2f" % (deltaAz[ant],deltaEl[ant],pwr[i,j])

            if pwr[i][j] > pmax :
               print "...pwr higher than %.3f so update ipk,jpk" % pmax
               ipk = i
               jpk = j
               pmax = pwr[i][j]

    deltaAz[ant] = ipk * step + deltaAz0[ant]
    deltaEl[ant] = jpk * step + deltaEl0[ant]
    offsetWait(deltaAz,deltaEl,moveList)
    pwrfinal=queryDouble("Sldc.Band1.Input"+str(ant)+".psys")
    print "final offset (%.2f, %.2f)  pwr %.2f" % (deltaAz[ant],deltaEl[ant],pwrfinal)

# offset set of antennas, wait until all reach their targets
def offsetWait( deltaAz, deltaEl, moveList ) :

    seq0 = {}    # dictionary of { ant : sequence number } pairs
    for ant in moveList:
        seq0[ant] = queryInt("Bima"+str(ant-6)+".AntennaCommon.Drive.driveSeqNum",24) 
        offset( deltaAz[ant] ,deltaEl[ant], ant )
    
    count = 300   # 150 x 0.2 = 30 secs
    notFinished = 1
    while (notFinished and count) :
        sleep(0.2)
        count = count - 1
        notFinished = 0
        for ant in moveList :
            seq = queryInt("Bima"+str(ant-6)+".AntennaCommon.Drive.driveSeqNum",24)
            if ( seq < (seq0[ant] + 1) ) :
                notFinished = 1
 
# send offsets to a list of antennas, don't wait
def offsetNoWait( deltaAz, deltaEl, moveList ) : 

    for ant in moveList:
        offset( deltaAz[ant] ,deltaEl[ant], ant )

# slice scans antenna 'amove' back and forth across transmitter at one elevation
# center up moving and fixed antennas on source before beginning
def slice(amove) :

    deltaAz0 = queryDouble("Bima"+str(amove-6)+".AntennaCommon.Drive.Point.offsetAz", 24)
    deltaEl0 = queryDouble("Bima"+str(amove-6)+".AntennaCommon.Drive.Point.offsetEl", 24)

    d = device.Carma(amove).drive()
    d.setMaxRate(2.0,1.5)   # slow the azimuth speed

    driveErrorPreference(PREF_FLAG)  # flag data if you must, but don't blank!
    scanLength = 30.   # arcmin
#   slowAzRate = 0.004  used for test.trx.4,5
    slowAzRate = 0.01
    az0 = queryDouble("Bima"+str(amove-6)+".AntennaCommon.Drive.Track.actualAzimuth",20)
        # this is the azimuth of the transmitter
    azstop = az0 + scanLength/(2.*60.)
        # this is the azimuth at which we stop taking data

    # move to beginning of scan, wait for antenna to settle
    deltaAz = deltaAz0 - scanLength/2. - 2.
    print "move to beginning of scan"
    offsetWait( deltaAz, deltaEl0, amove )
       
    print "set maxAzRate to %.2f" % slowAzRate
    d.setMaxRate(slowAzRate,1.5)   # slow the azimuth speed

    deltaAz = deltaAz0 + scanLength/2. + 10.
    offset( deltaAz, deltaEl0, amove )
    az = queryDouble("Bima"+str(amove-6)+".AntennaCommon.Drive.Track.actualAzimuth",20)
    while (az < azstop) :
        integrate( integTime=2.0, reps=1, antwait=None )    # integrate 
        az = queryDouble("Bima"+str(amove-6)+".AntennaCommon.Drive.Track.actualAzimuth",20)
        print " ...az = %.3f" % az

    print "reverse direction"
    deltaAz = deltaAz0 + scanLength/2. + 2.
    azstop = az0 - scanLength/(2.*60.)
    print "move to beginning of scan"
    offsetWait( deltaAz, deltaEl0, amove )
    deltaAz = deltaAz0 - scanLength/2. - 10.
    offset( deltaAz, deltaEl0, amove )
    az = queryDouble("Bima"+str(amove-6)+".AntennaCommon.Drive.Track.actualAzimuth",20)
    while (az > azstop) :
        integrate( integTime=2.0, reps=1, antwait=None )    # integrate 
        az = queryDouble("Bima"+str(amove-6)+".AntennaCommon.Drive.Track.actualAzimuth",20)
        print " ...az = %.3f" % az
    
    # finish up: move back to center, restore normal drive speed, blanking
    offset( deltaAz0, deltaEl0, amove)
    print "restore az speed to 2.0"
    d.setMaxRate(2.0,1.5)
    driveErrorPreference(PREF_BLANK)
    
# grid assumes that we begin with all telescopes pointed up on transmitter
def grid( moveList=[8,10,11,12,13], stepArcmin=1.2, gridsize=11, ncalibrate=1, intTime=0.5 ) :
  
    deltaAz0 = {}
    deltaEl0 = {}
    deltaAz = {}
    deltaEl = {}
    stopAz = {} 

    for ant in moveList :
        deltaAz0[ant] = queryDouble("Bima"+str(ant-6)+".AntennaCommon.Drive.Point.offsetAz", 24) 
        deltaEl0[ant] = queryDouble("Bima"+str(ant-6)+".AntennaCommon.Drive.Point.offsetEl", 24) 
             # save center position for every moving antenna 
        stopAz[ant] =  queryDouble("Bima"+str(ant-6)+".AntennaCommon.Drive.Track.actualAzimuth",20) \
            + (gridsize/2.) * (stepArcmin/60.) 
             # this is the azimuth at which we stop taking data
        radioAperture(True,ant)   # make sure we use radio constants, in case TV camera is on

    slowAzRate = stepArcmin/(2.*intTime*60.)    # degrees/sec 
        # slow rate in degrees/sec; allow 2 integrations per resolution element
    numIntegrations = int( ((gridsize+2)*stepArcmin)/(intTime*60.*slowAzRate) )
    normalAzRate = 2.0    # 2 degrees/sec
    normalElRate = 1.5    # 1.5 degrees/sec
    driveErrorPreference(PREF_FLAG)  # flag data if you must, but don't blank!

    for i in range(gridsize) :

        if ( i % ncalibrate == 0 ) :   # calibrate on center every ncalibrate scans
            print "calibrate"
            offsetWait( deltaAz0, deltaEl0, moveList )
            integrate( integTime=intTime, reps=1, antwait=None )    # one integration

        for ant in moveList :
            deltaEl[ant] = deltaEl0[ant] + (float(i) - float(gridsize-1)/2.)*stepArcmin
            deltaAz[ant] = deltaAz0[ant] - (float(gridsize/2.)+0.5)*stepArcmin
               # start 0.5 cell before scan start so antenna moves smoothly

        offsetWait( deltaAz, deltaEl, moveList)
            # move to beginning of scan (at fast slew rate)

        print "begin line %d/%d  integrating for %d secs" % ( (i+1), gridsize, intTime*numIntegrations )
        for ant in moveList :
            deltaAz[ant] = deltaAz0[ant] + float(gridsize/2.)*stepArcmin + 10.
                # target Az is 10 arcmin beyond end of range
            device.Carma(ant).drive().setMaxRate( slowAzRate, normalElRate )
                # slow down the az drive speed

        offsetNoWait( deltaAz, deltaEl, moveList )
            # begin moving all antennas

        integrate( integTime=intTime, reps=numIntegrations, antwait=None )   

        for ant in moveList :
            az = queryDouble("Bima"+str(ant-6)+".AntennaCommon.Drive.Track.actualAzimuth",20)
            print "... ant %2d is %6.2f arcmin beyond stopAz" % (ant, 60.*(az-stopAz[ant]) )
            offset( deltaAz0[ant], deltaEl0[ant], ant )      
            device.Carma(ant).drive().setMaxRate( normalAzRate, normalElRate )
                 # restore normal drive rate

    # finish up: move back to center, restore normal drive speed, blanking
    driveErrorPreference(PREF_BLANK)
    offsetWait( deltaAz0, deltaEl0, moveList )
    print "finished!"

# --- print out az,el positions (these are not raw encoder values, but have pointing constants applied) --- #
def getCenters() :
    for ant in range(7,16,1) :
       az = queryDouble("Bima"+str(ant-6)+".AntennaCommon.Drive.Track.actualAzimuth",20)
       el = queryDouble("Bima"+str(ant-6)+".AntennaCommon.Drive.Track.actualElevation",20)
       print "%2d  %.6f  %.6f" % (ant,az,el)

