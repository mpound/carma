# Script maintained by A. Zauderer (azaudere@astro.umd.edu)
# Last modified February 2010
# SW Jul 08: read weather data to calculate 3 mm tau, make multiMap call optional,
#            read planet data and add to file 
# AZ Feb 2010:  edited to allow for five bands instead of 3.  Also edited dip1.py and tTsys1.py

# Script Name: ApertureEfficiencySD.py
#
# Purpose:     Gather data needed from the monitor system
#              to calculate the single dish aperture efficiencies
#              for each antenna and band at 1mm and 3mm observing
#              frequencies.
#
# To use:      sci1% import ApertureEfficiencySD as SD 
#              sci1% SD.CalculateApertureEff(tuning_frequency='3mm',planet='jupiter')
#
# Details:     The basic steps I follow are
#              0)  Bookkeeping - file output, date and time stamps, etc.
#              1)  Tune Rxs - 3mm or 1mm
#              2)  Track planet or choice
#              2a) If requested (checkPointing=True), run a multiMap cross on the planet
#              3)  Get nominal power measurements ON source
#              4)  Get power of ambient, blank sky and Tsys OFF source
#              5)  cycle  ON-OFF planet N times, where N ~10
#              6)  gather needed information (planet size, beam size, etc.)
#              7)  calculate aperture efficiency
#              8)  Repeat steps 1-7 for other frequency/planet combinations
#              

"""
ApertureEfficiencySD.py, version 3
February 2010
A. Zauderer
----------------------------------
This script enables the calculation of single dish aperture efficiency
by tracking a bright planet, and collecting measurements of power
on and off the source, and with and without the ambient load in place.
"""

# Import Statements  
# -----------------
# There are several functions copied into this script from other places.
# My philosophy is to make script robust to change in other people's scripts
# by relying on basic, system-level commands as much as possible.

import math as m  # need for functions like sqrt
import os         # necessary for checksource in the setPlanetSize option
import refPoint
import device
import carmaHelpers as helpers
from subarrayCommands import *
import short
import dip1
import white

#Don't know if I need next two statements yet...
#import subarrayControl,Subarray
#s=Subarray.getSubarray()


"""
This routine deals with sorting through antenna types.
checkAntVectorType was moved to shortcuts by Steve as makeList, but I
keep checkAntVectorType as it has the additional function that it
actually generates a vector 1 to 15 for all antennas instead of
allowing 0 for input.  Steve's version is more appropriate for
the implementation of subarrays as 0 can have a specific meaning
for a subarray.  Be cautious of the difference between
checkAntVectorType and makeList as some operations do very special
things for lists of antennas verses specifically 0
(e.g. subarrayCommands.track and s.equatOffset)
"""

def CalculateApertureEff(tuning_frequency,planet,performTuning=True,performPointing=True,pointing_source='3c84',skyDip=True,bands=[1,2,3,4,5],myoffset=5,checkPointing=False,numberOfCycles=3,setProject=True):
# 0) Bookkeeping
  #antVec=short.checkAntVectorType(antref)
  antVec=currentAntennaNumbers()
  bands=helpers.makeList(bands)
  antwait=-2

# Setting obsblock for multimap:
  projTemp = 'ct007'     
  obTemp = 'SDApEff'
  subObTemp = yearMonthDay()   #system level command.
  if setProject==True: newProject(projTemp,obTemp,subObTemp)


  # The following three lines are needed for multiMap - which will be run on the planet source:
  [ovroTemp,bimaTemp,szaTemp] = short.separateAntType(antVec)
  set1 = ovroTemp
  set2 = bimaTemp

  print "Getting the time..."
  daylabel=yearMonthDay()  # yearMonthDay is a system level command
  currentTime=getUT(True)  # getUT is a module written by J. Carpenter, copied below.
  filename='/misc/array/rt/apertureEff/singleDish/'+tuning_frequency+'freq'+daylabel+'.SD.'+planet+'.dat'
  fd=open(filename,'a')    # Open file for appending and print header information:
  fd.write("Single Dish Aperture Efficiency Calculation.\n")
  fd.write("Script:  ApertureEfficiencySD.py, file name: %s\n" % filename)
  fd.write("Source:  %s, Starting date and time: %s \t %s \n\n" % (planet,str(daylabel),str(currentTime)))

# 1)  Tune the array to either 3mm or 1mm - done in "select_my_tuning" function
  print "Tuning..."
  myfreq=select_my_tuning(tuning_frequency,performTuning)
  fd.write("Frequency: %s GHz" % str(myfreq))
  fd.close()

# 3)  Execute radio pointing - before tracking the planet
  if performPointing==True:
    print "Beginning radio pointing to point up before running SD script."
    print "Pointing up on source", pointing_source
    refPoint.refPoint(pointing_source)
    print "Radio pointing is done..."

# 2)  Track planet of choice - preferably less than 15 arcseconds
  #print "Tracking...", planet
  #track(planet,waiton=ALL, tmo=500)   # forces a wait on all antennas, with a time out of 500 seconds.


# 4)  Get nominal power on planet, ambient, and Tsys on planet
# Move off source and get power on blank sky, ambient and Tsys
# move off source

# There are 4 positions I am concerned with: (variable=powerLabel)
# (1) power ON source - (Psource)
# (2) power of sky OFF source  - (Psky)
# (3) power with ambient load in place ON source - (Pamb_source)
# (4) power with ambient load in place OFF source - (Pamb_sky)
# 
# (3) and (4) should be the same and essentially measure system noise
# 
# Will also keep track of Tsys on and off the source.  Make sure we
# have recent ambient.  Only want to calculate Tsys after a sky()
# command

  track(planet,waiton=-2, tmo=500)   # go to planet

  # only do this once, otherwise wastes too much time
  if checkPointing==True:             
    print 'Running an interferometer cross on the planet to check pointing: will take a few minutes.'
    white.multiMap(1,[planet],set1,set2,intTime=5.0,intRep=1,mapPoints=5,set1Step="Default 0.9*Nyquist",set2Step="Default 0.9*Nyquist",type='cross',waitSource=False,doCenter=True,doExtra=False,antwait=-2)

  conditions=print_weatherStuff(filename)

  for i in range(numberOfCycles):
    print "Tracking...", planet
    track(planet,waiton=ALL, tmo=500)   # forces a wait on all antennas, with a time out of 500 seconds.
    conditions=print_planetStuff(filename)
    offset(az=myoffset,el=0,waiton=ALL,tmo=25)
    amb(waiton=ALL,tmo=25)                                                      # put ambient load in
    fd=open(filename,'a')
    fd.write("Power off source, ambient load: \n")
    fd.close()
    t=print_power(filename,powerLabel='Pamb_sky',calcTsys=False)   # may not need to pass fileName?

    sky(waiton=ALL,tmo=25)                                                      # take ambient load out
    fd=open(filename,'a')
    fd.write("Power off source, sky: \n")
    fd.close()
    t=print_power(filename,powerLabel='Psky',calcTsys=True)  

    offset(az=0,el=0,waiton=ALL,tmo=25)             # move back on the source - wait to make sure all antennas reach.
    amb(waiton=ALL,tmo=25)
    fd=open(filename,'a')
    fd.write("Power ON source, ambient load: \n")   # this should be the same as power off source with ambient load!
    fd.close()
    t=print_power(filename,powerLabel='Pamb_source',calcTsys=False)

    sky(waiton=ALL,tmo=25)
    fd=open(filename,'a')
    fd.write("Power ON source, sky: \n")
    fd.close()
    t=print_power(filename,powerLabel='Psource',calcTsys=True)    
  
  fd=open(filename,'a')
  fd.write("Finished the on-off source/sky loop with power measurements and Tsys recorded.")
  daylabel=yearMonthDay()  # yearMonthDay is a system level command
  currentTime=getUT(True)  # getUT is a module written by J. Carpenter, copied below.
  fd.write("Source:  %s, Ending date and time: %s \t %s \n\n" % (planet,str(daylabel),str(currentTime)))

  conditions=print_weatherStuff(filename)

  if skyDip==True:
    dip1.performDip_notune([0])
    filename='/misc/array/rt/apertureEff/singleDish/'+tuning_frequency+'freq'+daylabel+'.SD.'+planet+'.dat'

# 5) Repeat steps 3 and 4 as often as observer would like - probably 3-5 times
# Lee says to do 10 times.

'''# 6) Calculate the aperture efficiency.  Collect needed data first.
  PlanetDiam = setPlanetSize(planet)
  # Calculating half-power beam diameter
  # beam = lambda/2 x D, where D is diameter of dish
  mylambda=setLambda(tuning_frequency)
  ovroBeam=mylambda/(2.0*10.4)  # OVRO dishes are 10.4 m in diameter
  bimaBeam=mylambda/(2.0*6.1)   # Bima dishes are 6.1 m in diameter

  fd=open(filename,'a')
  fd.write("Lambda: %s\n" % mylambda)
  fd.write("Planet diameter: %s\n" % PlanetDiam)
  fd.write("OVRO Beam:  %s\tBIMA Beam: %s\n" % (str(ovroBeam), str(bimaBeam)))
  #Tp=137.05  # uranus in K
  Tp=GetSourceInfo(planet, tuning_frequency)
  fd.write("Planet temp: %s\n" % Tp) 

  fd.close()
'''
# 7) Go back to step (1) and re-tune to other frequency

#
# Functions defined to make the main Calculate Aperture Eff simpler
#

def print_weatherStuff(filename) :
    antVec=currentAntennaNumbers()
    ourTelescopes = currentAntennaNumbers()
    tamb = []
    for i in antVec : tamb.append(short.getTamb(i)+273.15)
    tout=short.getTout()
    tau=queryDouble("Weather.Tau225",20)
    phase=queryDouble("PhaseMonitor.skyRMS",20)
    humidity=queryDouble("Weather.DewPointSensor.humidity",20)
    pressure=queryDouble("Weather.pressure",20)
    densh2o=queryDouble("Weather.waterDensity",20)
    precipmm=queryDouble("Weather.precipWater",20)
    fd=open(filename,'a')
    # fd.write("\nTout: %f\t Tau: %s\t PhaseRMS: %s\t" % (tout,str(tau),str(phase)))
    fd.write("\nTout: %f\t Tau: %s\t PhaseRMS: %s  Humidity: %s  Pressure: %s  DensH2O: %s  Precipmm: %s" % (tout,str(tau),str(phase),str(humidity),str(pressure),str(densh2o),str(precipmm)))
    for j in range(len(antVec)) :
      fd.write("\n antenna%s \t Tamb:%s\t " % (str(antVec[j]),str(tamb[j])) )
    fd.write("\n")
    fd.close()
    return tout

def getElevation(antNum) :
    """ The purpose of this function is to return the elevation of the given antenna, antNum.
    It returns ths elevation in radians.  Here a calculation is necessary, because the monitor
    system returns the elevation in degrees.
    """
    nameTele=device.CarmaAnt().getName(antNum).capitalize()
    # Must divide between OVRO and BIMA dishes, because the elevation monitor point is slightly
    # different for the two types of antennas.
    if antNum < 7 : elevation = queryDouble("%s.Drive.Track.actualElevation" % nameTele,24 )
    if 6 < antNum < 16 : elevation = queryDouble("%s.AntennaCommon.Drive.Track.actualElevation" % nameTele,24)
    #print "The elevation is %f degrees"% elevation
    #el = (elevation*m.pi)/180.0
    return elevation               # returns elevation for each antenna in degrees



# This function will actually do the looping over antennas and bands to print the power value to file
def print_power(filename,powerLabel,calcTsys) :
    #antVec=checkAntVectorType(antref)
    print "Measuring power for "+powerLabel
    antVec=currentAntennaNumbers()
    antNames=currentAntennaNames()
    bands=[1,2,3,4,5]
    bands=helpers.makeList(bands)
    fd=open(filename,'a')
    currentTime=getUT(True)  # getUT is a module written by J. Carpenter, copied below.
    fd.write("Time before loop over antennas and bands: %s\n" % (str(currentTime)))  
    reps=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
    for k in range(len(reps)):
      for i in range(len(antVec)):
         el=getElevation(antVec[i])
         fd.write("elevation: %s\n" % str(el))
         for j in range(len(bands)):
             power=getPsys(antVec[i],bands[j])
             totalpower=getTotal(antVec[i],bands[j])
             tempMonitor=queryDouble("SlPipeline.Input%s.Band%s.Tsys.Dsb" % ( str(antVec[i]),str(bands[j]) ),4)
             fd.write("Antenna%sBand%s%s\t Rep%s\t%s\t Ptot\t %s\tTsys\t %s\n"%(str(antVec[i]),str(bands[j]),powerLabel, str(reps[k]),str(power),str(totalpower),str(tempMonitor)))
      currentTime=getUT(True)
      fd.write("Time after loop: %s\n" % (str(currentTime)))  #This is to check how long it takes to loop thru.
      sleep(0.5)
    return currentTime

# extract planet parameters from the monitor stream, works once source=planet
def print_planetStuff(filename) :
    currentTime=getUT(True)
    source=queryString('Control.Subarray%s.Commands.Track.sourcename' % (s.getSubarrayNo()),10)
    pltemp=queryDouble('Control.Subarray%s.planetTemperature' % (s.getSubarrayNo()),10)
    plmaj=queryDouble('Control.Subarray%s.planetMajorAxis' % (s.getSubarrayNo()),10)    
    plmin=queryDouble('Control.Subarray%s.planetMinorAxis' % (s.getSubarrayNo()),10)    
    fd=open(filename,'a')
    fd.write("\n%s  Source: %s  PlanetTemp: %s  PlMaj: %s  PlMin: %s" % (str(currentTime),source,str(pltemp),str(plmaj),str(plmin)))
    fd.write("\n")
    fd.close()
    return pltemp

# getPsys is a simple function that I copied from short.py
def getPsys(antref,bands) :
    inputQuery = "Sldc.Band%s.Input%s.psys" %(str(bands),str(antref))
    psys = queryDouble(inputQuery,20)
    return psys

def getTotal(antref,bands) :
    carmaName = device.CarmaAnt().getName(antref).capitalize()
    if antref < 7:  inputQuery = "%s.AntennaIfContainer.AntennaIF.ifOutTotalPower" %(carmaName)
    else:  inputQuery = "%s.AntennaIfContainer1.AntennaIF.ifOutTotalPower" %(carmaName)
    totalpower = queryDouble(inputQuery,20)
    return totalpower

def checkAntVectorType(antref) :
    if antref==0 or antref==[0] : antref = currentAntennaNumbers()
    if list in [type(antref)] : antrefVector = antref
    else : antrefVector = [antref]
    return antrefVector

#currentAntennaNumbers
# -Return a list of carma antenna numbers that are members of
#    this subarray.
#-------->currentAntennaNumbers()
#     <1> [1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
#Sci#1[2]:x=currentAntennaNumbers()
#Sci#1[3]:x[0]
#     <3> 1
#Sci#1[4]:x[1]
#     <4> 2
#Sci#1[5]:x[3]
#     <5> 5
# My note - may be able to use currentAntennaNumbers() instead of
# the whole check vector rigamarole.

def select_my_tuning(tuning_frequency,performTuning):
  print "Setting tuning frequency..."
  if tuning_frequency=='3mm':
     myfreq=96.0
     corr_IF=3.00
     corr_band1=myfreq-0.46
     corr_band2=myfreq
     corr_band3=myfreq+0.46
     corr_band4=myfreq
     corr_band5=myfreq
  elif tuning_frequency=='1mm':
     myfreq=225.75
     corr_IF=2.75
     corr_band1=myfreq
     corr_band2=myfreq-0.5
     corr_band3=myfreq+0.5
     corr_band4=myfreq
     corr_band5=myfreq
  else:
     print "You must choose 3mm or 1mm for tuning frequency.  Defaulting to 3mm..."
     myfreq=96.0
     corr_band1=myfreq-0.46
     corr_band2=myfreq
     corr_band3=myfreq+0.46
     corr_band4=myfreq
     corr_band5=myfreq
  # The command that actually implements the tuning
  print ("Tuning to %s..." % str(myfreq))
  if performTuning==True:
    freq(myfreq,USB,corr_IF,'none')
  # Setting up the correlator configuration - for single dish, correlator configuration
  # does not matter. So, I took out configband commands and checkbands.
  configband(1,BW500,corr_band1,LSB,myfreq)
  configband(2,BW500,corr_band2,LSB,myfreq)
  configband(3,BW500,corr_band3,LSB,myfreq)
  configband(4,BW500,corr_band4,LSB,myfreq)
  configband(5,BW500,corr_band5,LSB,myfreq)
  checkbands()
  short.postConfigCorr()
  return myfreq
     

# 2)  Track planet of choice - preferably less than 15 arcseconds
def GetSourceInfo(planet,tuning_frequency):
  """ This simply returns the planet brightness temp. at 3mm
        and 1mm observing frequencies, as reported in Checksource.
  """
  if planet=='mars':
    if tuning_frequency=='3mm': Tp=206.69
    else:  Tp= 211.93         # 1mm  
  elif planet=='jupiter':
    if tuning_frequency=='3mm': Tp=179.0
    else:  Tp=179.0          # 1mm
  elif planet=='saturn':
    if tuning_frequency=='3mm': Tp=149.0
    else:  Tp=149.0          # 1mm
  elif planet=='uranus':
    if tuning_frequency=='3mm': Tp=137.05
    else:  Tp= 102.49         # 1mm
  elif planet=='neptune':
    if tuning_frequency=='3mm': Tp=132.15
    else:  Tp= 97.73         # 1mm
  elif planet=='moon':
    if tuning_frequency=='3mm': Tp=200.0
    else:  Tp=200.0          # 1mm
  elif planet=='sun':
    if tuning_frequency=='3mm': Tp=7000.0
    else:  Tp=6500.0          # 1mm
  else :  print "You have chosen a bad source.  Please choose moon, mars, jupiter, saturn, uranus or neptune."
  return Tp  # Returns the planet temp. 

# This section requires command line user input.
def setPlanetSize(planet):
  if planet=='mars':
    os.system('checksource source=mars | grep Major > tmpMaj')
    os.system('checksource source=mars | grep Minor > tmpMin')
  elif planet=='jupiter':
    os.system('checksource source=jupiter | grep Major > tmpMaj')
    os.system('checksource source=jupiter | grep Minor > tmpMin')
  elif planet=='saturn':
    os.system('checksource source=saturn | grep Major > tmpMaj')
    os.system('checksource source=saturn | grep Minor > tmpMin')
  elif planet=='uranus':
    os.system('checksource source=uranus | grep Major > tmpMaj')
    os.system('checksource source=uranus | grep Minor > tmpMin')
  elif planet=='neptune':
    os.system('checksource source=neptune | grep Major > tmpMaj')
    os.system('checksource source=neptune | grep Minor > tmpMin')
  elif planet=='moon':
    os.system('checksource source=moon | grep Major > tmpMaj')
    os.system('checksource source=moon | grep Minor > tmpMin')
  elif planet=='sun':
    os.system('checksource source=sun | grep Major > tmpMaj')
    os.system('checksource source=sun | grep Minor > tmpMin')

  print "Please re-type the following value for the major axis."
  os.system("awk '{print $4}' tmpMaj")
  majAxis=input("value: ")
  print "Please re-type the following value for the minor axis."
  os.system("awk '{print $4}' tmpMin")
  minAxis=input("value: ")
  # input for numerical, raw_input for string
  # See http://www.yukoncollege.yk.ca/~ttopper/COMP118/rCheatSheet.html
  majorAxis= majAxis   # in arcseconds
  minorAxis= minAxis   # in arcseconds
  print planet ,": major axis('')= ", majorAxis ,"minor axis('')= ", minorAxis
  PlanetDiam= m.sqrt(majorAxis*minorAxis)
  print PlanetDiam, 'is the calculated planet diameter'
  return PlanetDiam


def setLambda(tuning_frequency):
  """Returns wavelength in meters as a float point number
       based on user choise of 3mm or 1mm.
  """
  if tuning_frequency=='3mm': wavelength=float(0.003158)    # wavelength in meters for 95 GHz
  else: wavelength=float(0.001333)    # wavelength in meters for 225 GHz
  #wavelength=float(0.001333)
  return wavelength

def getUT(timestamp=False):
    """ Returns current universal time as a float point number
        If time stamp=True, then returns Mon DD, YYYY hh:mm:ss
        This module written by J. Carpenter and copied out of the
        obs utilities.
    """

    ut = time.gmtime()

    if timestamp:
        ut = str(ut[0]) + '-' + str(ut[1]) + '-' + str(ut[2]) + \
             ' ' + str(ut[3]) + ":" + str(ut[4]) + ":" + str(ut[5])
    else:
        ut = float(ut[3]) + float(ut[4])/60.0 + float(ut[5])/3600.0

    return ut

