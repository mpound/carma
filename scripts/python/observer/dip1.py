# This is an Active script.  DO NOT MOVE.
# dip1.py
# Written and maintained by B. A. Zauderer, U. of Maryland
# azaudere@astro.umd.edu
# Great modifications suggested by M. LaVigne, 3/2007
# mlavigne@astro.umd.edu
# Last edit:  March 27, 2007
#---------------------------------------------------------------------------
# To run this script
# import dip1

# At an azimuth of 180 degrees:
# dip1.performDip([0])
# The rest of the inputs have defaults set.

# At the azimuth of the source you are at currently:
# dip1.performDip([0],KeepSourceAz=True)


#---------------------------------------------------------------------------

import math as m
import carma
import Subarray
from subarrayCommands import *
import short
import monitorDataManip as mDM
import fileIOPython as fIOP
import tTsys1
import numarray
import AZ_msg as AZM
import carmaHelpers as helpers

s=Subarray.getSubarray()
#freq = queryDouble('Control.Subarray1.loFreq',24)
defaultSBR = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

def performDip_notune(antref,SBR1=defaultSBR,SBR2=defaultSBR,SBR3=defaultSBR,\
    fileName=None,Azimuth=90.0,KeepSourceAz=False,reps=1,bands=[1,2,3]):
    #SBR = [1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000]
    #SBR2 = [1.000,1.02,1.05,1.000,1.02,1.17,1.01,1.12,1.30,1.27,1.10,1.21,1.04,1.12,0.78,0.97]
    #SBR3 = [1.000,0.90,1.01,1.000,1.03,1.04,0.98,1.24,1.21,1.38,1.13,1.15,1.04,1.27,0.85,0.95]
    """This performs a dip.  In essence, this script moves the antennas to a variety of different elevations, taking
       a Tsys measurement at each stop.  The Tsys function also records all the information needed to c
alculate
Tsys:
       the Psky, Pamb, yfators, ambient temp, cabin temps for the BIMA dishes, etc.

       The purpose of this script is to perform a dip in order to calculate the zenith opacity
        The tTsys function called actually prints the information needed:  antenna, AM, Psky.

        AM = 1/sin(elevation)

    P. Teuben explained how to document with triple quotes.  To print this:  print __doc__

    KeepSourceAz applied to move command - March 28th ML & SW

    """
   # NO tuning - to keep the same frequency tuning from another script
    dayLabel = yearMonthDay()
    frequency_mp = queryDouble('Control.Subarray1.loFreq',24)
    if fileName == None: 
        if (frequency_mp > 120.) :
            fileName = '/array/rt/Dips/'+dayLabel+'.dip1mm.dat'    
        else :
            fileName = '/array/rt/Dips/'+dayLabel+'.dip3mm.dat'    
    cancel()
    antVec = short.checkAntVectorType(antref)
    ourTelescopes = currentAntennaNumbers()
    [ovroTemp,bimaTemp,szaTemp] = short.separateAntType(ourTelescopes)
    tamb = []
    for i in antVec : tamb.append(short.getTamb(i)+273.15)
    #short.limitSubarray(antVec)         # This limits the subarray to the antennas that the user inputs.
    # If one antenna is entered, can be a single number, else, needs to
    # be a list contained in brackets.
    # steps=[1.05,1.1,1.20,1.25,1.3,1.40,1.50,1.60,1.75,2.0,2.25,2.50,2.75,3.0,3.25,3.5]
    steps=[1.05,1.25,1.75,2.50,3.0,3.5]
    numSteps=len(steps)
    stop(antVec)
    bands = helpers.makeList(bands)
    print 'Beginning the dip...'
    fd=open(fileName,'a')
    for ii in range(numSteps) :
        #print 'I made it into the loop.'
        el_radians = m.asin(1.0/steps[ii])    # m.asin returns the arc sin in radians
        elevation = (el_radians*180.0)/m.pi
        #print 'After the elevation values!',el_radians,elevation
        if KeepSourceAz :
            #azSlew = queryDouble("Bima%s.AntennaCommon.Drive.Track.actualAzimuth" % (str(bimaTemp[0])),24)
            move(el=elevation, ants=antVec)   # ML
        else:
            azSlew=Azimuth  # ML
            move(el=elevation,az=azSlew, ants=antVec)   # See documentation for "move" at bottom of this page.
        airmass = steps[ii]
        [tsysVals,tsysStats,junk] = tTsys1.multiTsys(airmass,antVec,SBR1,SBR2,SBR3,bands,1.5,reps)
        #print numSteps  # The random number printed out is the number of elevation steps
        utStamp = short.getMiriadUTStamp()
        tout=short.getTout()
        fd.write("\ntime:%s \t Tout: %f\t Freq: %s\t" % (utStamp,tout,str(frequency_mp)) )
        for j in range(len(antVec)) :
            fd.write("\n antenna%s \t Tamb:%s\t " % (str(antVec[j]),str(tamb[j])) )
            #print 'this should be antvec',len(tsysVals[0])
            for k in range(len(bands)) :
                tempMonitor = queryDouble("SlPipeline.Input%s.Band%s.Tsys.Dsb" % (str(antVec[j]),bands[k]),24)
                #print 'In dip1.py script'
                #print 'bands, steps, tsysVals, tempMonitor ',bands,steps,tsysVals,tempMonitor
                #print 'len(tsysVals[j]',len(tsysVals[0][j])
                fd.write("\nband %s: \t AM: %s \t SBR_Tsys(K): %s \t Tsys(monitor): %s \t" % (bands[k],str(steps[ii]),str(tsysVals[0][j][k]),str(tempMonitor) ) )
    #short.fillSubarray()                       # Returns all possible antennas to the subarray for next user
    fd.close()
    print 'The dip has finished.  Thank you for observing!'
    print 'Instructions to analyze the dip can be found in /array/rt/Dips/.'
    print 'Contact A. Zauderer at azaudere@astro.umd.edu with questions or if you have any problems.'
    AZM.sendMsg('Sky Dip','A. Zauderer','azaudere@astro.umd.edu','dip1.py')
    AZM.sendMsg('Sky Dip','B. Prager','bprager@astro.umd.edu','dip1.py')
    AZM.sendMsg('Sky Dip','S. White','white!astro.umd.edu','dip1.py')
    return tsysVals


def performDip_1mm(antref,SBR1=defaultSBR,SBR2=defaultSBR,SBR3=defaultSBR, \
    fileName=None,Azimuth=90.0,KeepSourceAz=False,reps=1,bands=[1,2,3]):
    #SBR = [1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000]
    #SBR2 = [1.000,1.02,1.05,1.000,1.02,1.17,1.01,1.12,1.30,1.27,1.10,1.21,1.04,1.12,0.78,0.97]
    #SBR3 = [1.000,0.90,1.01,1.000,1.03,1.04,0.98,1.24,1.21,1.38,1.13,1.15,1.04,1.27,0.85,0.95]
    """This performs a dip.  In essence, this script moves the antennas to a variety of different elevations, ta
king
       a Tsys measurement at each stop.  The Tsys function also records all the information needed to calculate
Tsys:
       the Psky, Pamb, yfators, ambient temp, cabin temps for the BIMA dishes, etc.

       The purpose of this script is to perform a dip in order to calculate the zenith opacity
        The tTsys function called actually prints the information needed:  antenna, AM, Psky.

        AM = 1/sin(elevation)

    P. Teuben explained how to document with triple quotes.  To print this:  print __doc__

    KeepSourceAz applied to move command - March 28th ML & SW

    """
   # Suggested Tuning - Close to 225 GHz
   #loFreq=224.0
   #ifFreq=2.15
   #centFreq=loFreq + ifFreq
   #freq(centFreq,LSB,ifFreq,'none')
   #run('tune225')
   #run('tune225')
   #run('tune225')
   #configband(1,BW500,centFreq-0.5,LSB,centFreq)
   #configband(2,BW500,centFreq    ,LSB,centFreq)
   #configband(3,BW500,centFreq+0.5,LSB,centFreq)
   #checkbands()

    freq(225.0,USB,3.0,None)
    configband(1,BW500,225.0-0.5,LSB)
    configband(2,BW500,225.0,LSB)
    configband(3,BW500,225.0+0.5,LSB)
    checkbands()

    dayLabel = yearMonthDay()
    frequency_mp = queryDouble('Control.Subarray1.loFreq',24)
    if fileName == None: 
        fileName = '/array/rt/Dips/'+dayLabel+'.dip1mm.dat'    
    cancel()
    antVec = short.checkAntVectorType(antref)
    ourTelescopes = currentAntennaNumbers()
    [ovroTemp,bimaTemp,szaTemp] = short.separateAntType(ourTelescopes)
    tamb = []
    for i in antVec : tamb.append(short.getTamb(i)+273.15)
    #short.limitSubarray(antVec)                        # This limits the subarray to the antennas that the user inputs.
    # If one antenna is entered, can be a single number, else, needs to
    # be a list contained in brackets.
    # steps=[1.05,1.1,1.20,1.25,1.3,1.40,1.50,1.60,1.75,2.0,2.25,2.50,2.75,3.0,3.25,3.5]
    steps=[1.05,1.25,1.75,2.50,3.0,3.5]
    numSteps=len(steps)
    stop(antVec)
    bands = helpers.makeList(bands)
    print 'Beginning the dip...'
    fd=open(fileName,'a')
    for ii in range(numSteps) :
        #print 'I made it into the loop.'
        el_radians = m.asin(1.0/steps[ii])    # m.asin returns the arc sin in radians
        elevation = (el_radians*180.0)/m.pi
        #print 'After the elevation values!',el_radians,elevation
        if KeepSourceAz :
            #azSlew = queryDouble("Bima%s.AntennaCommon.Drive.Track.actualAzimuth" % (str(bimaTemp[0])),24)
            move(el=elevation, ants=antVec)   # ML
        else:
            azSlew=Azimuth  # ML
            move(el=elevation,az=azSlew, ants=antVec)   # See documentation for "move" at bottom of this page.
        airmass = steps[ii]
        [tsysVals,tsysStats,junk] = tTsys1.multiTsys(airmass,antVec,SBR1,SBR2,SBR3,bands,1.5,reps)
        #print numSteps  # The random number printed out is the number of elevation steps
        utStamp = short.getMiriadUTStamp()
        tout=short.getTout()
        fd.write("\ntime:%s \t Tout: %f\t Freq: %s\t" % (utStamp,tout,str(frequency_mp)) )
        for j in range(len(antVec)) :
            fd.write("\n antenna%s \t Tamb:%s\t " % (str(antVec[j]),str(tamb[j])) )
            #print 'this should be antvec',len(tsysVals[0])
            for k in range(len(bands)) :
                tempMonitor = queryDouble("SlPipeline.Input%s.Band%s.Tsys.Dsb" % (str(antVec[j]),bands[k]),24)
                #print 'In dip1.py script'
                #print 'bands, steps, tsysVals, tempMonitor ',bands,steps,tsysVals,tempMonitor
                #print 'len(tsysVals[j]',len(tsysVals[0][j])
                fd.write("\nband %s: \t AM: %s \t SBR_Tsys(K): %s \t Tsys(monitor): %s \t" % (bands[k],str(steps
[ii]),str(tsysVals[0][j][k]),str(tempMonitor) ) )
    #short.fillSubarray()                       # Returns all possible antennas to the subarray for next user
    fd.close()
    print 'The dip has finished.  Thank you for observing!'
    print 'Instructions to analyze the dip can be found in /array/rt/Dips/.'
    print 'Contact A. Zauderer at azaudere@astro.umd.edu with questions or if you have any problems.'
    AZM.sendMsg('Sky Dip','A. Zauderer','azaudere@astro.umd.edu','dip1.py')
    return tsysVals

def performDip_3mm(antref,SBR1=defaultSBR,SBR2=defaultSBR,SBR3=defaultSBR, \
    fileName=None,Azimuth=180.0,KeepSourceAz=False,reps=1,bands=[1,2,3]):
    #SBR = [1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000]
    #SBR2 = [1.000,1.02,1.05,1.000,1.02,1.17,1.01,1.12,1.30,1.27,1.10,1.21,1.04,1.12,0.78,0.97]
    #SBR3 = [1.000,0.90,1.01,1.000,1.03,1.04,0.98,1.24,1.21,1.38,1.13,1.15,1.04,1.27,0.85,0.95]
    """This performs a dip.  In essence, this script moves the antennas to a variety of different elevations, taking
       a Tsys measurement at each stop.  The Tsys function also records all the information needed to calculate Tsys:
       the Psky, Pamb, yfators, ambient temp, cabin temps for the BIMA dishes, etc.  

       The purpose of this script is to perform a dip in order to calculate the zenith opacity
        The tTsys function called actually prints the information needed:  antenna, AM, Psky.
    
        AM = 1/sin(elevation)

    P. Teuben explained how to document with triple quotes.  To print this:  print __doc__
    
    KeepSourceAz applied to move command - March 28th ML & SW

    """
    dayLabel = yearMonthDay()
    if fileName == None: 
        fileName = '/home/obs/zauderer/Dips/'+dayLabel+'.dip3mm.dat'   
    freq = queryDouble('Control.Subarray1.loFreq',24)
    cancel()
    antVec = short.checkAntVectorType(antref)
    ourTelescopes = currentAntennaNumbers()
    [ovroTemp,bimaTemp,szaTemp] = short.separateAntType(ourTelescopes)
    tamb = []
    for i in antVec : tamb.append(short.getTamb(i)+273.15)
    #short.limitSubarray(antVec)                        # This limits the subarray to the antennas that the user inputs.  
    # If one antenna is entered, can be a single number, else, needs to 
    # be a list contained in brackets.
    #steps=[1.05,1.1,1.20,1.25,1.3,1.40,1.50,1.60,1.75,2.0,2.25,2.50,2.75,3.0,3.25,3.5]
    steps=[1.05,1.25,1.75,2.50,3.0,3.5]
    numSteps=len(steps)
    stop(antVec)
    bands = helpers.makeList(bands)
    print 'Beginning the dip...'
    fd=open(fileName,'a')
    for ii in range(numSteps) :
        #print 'I made it into the loop.'
        el_radians = m.asin(1.0/steps[ii])    # m.asin returns the arc sin in radians
        elevation = (el_radians*180.0)/m.pi
        #print 'After the elevation values!',el_radians,elevation
        if KeepSourceAz :
            #azSlew = queryDouble("Bima%s.AntennaCommon.Drive.Track.actualAzimuth" % (str(bimaTemp[0])),24)
            move(el=elevation, ants=antVec)   # ML
        else:  
            azSlew=Azimuth  # ML
            move(el=elevation,az=azSlew, ants=antVec)   # See documentation for "move" at bottom of this page.
        airmass = steps[ii]
        [tsysVals,tsysStats,junk] = tTsys1.multiTsys(airmass,antVec,SBR1,SBR2,SBR3,bands,1.5,reps)
        #print numSteps  # The random number printed out is the number of elevation steps
        utStamp = short.getMiriadUTStamp()
        tout=short.getTout()
        fd.write("\ntime:%s \t Tout: %f\t Freq: %s\t" % (utStamp,tout,str(freq)) )
        for j in range(len(antVec)) :
            fd.write("\n antenna%s \t Tamb:%s\t " % (str(antVec[j]),str(tamb[j])) )
            #print 'this should be antvec',len(tsysVals[0])
            for k in range(len(bands)) :
                tempMonitor = queryDouble("SlPipeline.Input%s.Band%s.Tsys.Dsb" % (str(antVec[j]),bands[k]),24)
                #print 'In dip1.py script'
                #print 'bands, steps, tsysVals, tempMonitor ',bands,steps,tsysVals,tempMonitor
                #print 'len(tsysVals[j]',len(tsysVals[0][j])
                fd.write("\nband %s: \t AM: %s \t SBR_Tsys(K): %s \t Tsys(monitor): %s \t" % (bands[k],str(steps[ii]),str(tsysVals[0][j][k]),str(tempMonitor) ) )
    #short.fillSubarray()                       # Returns all possible antennas to the subarray for next user
    fd.close()
    print 'The dip has finished.  Thank you for observing!'
    print 'Eventually, I will give you instructions here to analyze the dip data and get a zenith opacity.'
    print 'For now, contact A. Zauderer at azaudere@astro.umd.edu and ask her to analyze the data.'
    return tsysVals

#-----Documentation for "move" ---------------------------------------------
#def move(az=None, el=None, ants=0, tmo=0, waiton=ALL) :
#    """Move antennas to requested az/el and wait for all to acquire.
#    All of the parameters have defaults, and it the az or el are not specified
#    then they are unchanged. Examples:
#        move(10, 30)  # az=10, el=30
#        move(44)      # az=44, el unchanged
#        move(el=80)   # el=80, az unchanged
#    A cancel in another sac will break out of the wait.
#    Parameters:
#     az: azimuth in degrees
#     el: elevation in degrees
#     ants: A single or list of antenna numbers; zero is all antennas
#     tmo: Timeout value in seconds; zero inhibits timeout
#     waiton: All to be complete (ALL), or just the first (ANY), or NONE

