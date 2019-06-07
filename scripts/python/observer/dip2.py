# dip2.py
#
# The purpose of this script is to perform a dip in order to calculate the zenith opacity
# 
#
# AM = 1/sin(elevation)
# AM    elevation (degrees)
# 1     90.00
# 1.05  72.25
# 1.1   65.38
# 1.15  60.41
# 1.20  56.44
# 1.25  53.13
# 1.50  41.81
# 1.75  34.85
# 2.0   30.00
# 2.25  26.39
# 2.50  23.58
# 2.75  21.32
# 3.0   19.47
# 3.25  17.92
# 3.5   16.60
# 3.75  15.47

import math as m
import carma
import subarrayControl,Subarray
from subarrayCommands import *
import short
import monitorDataManip as mDM
import fileIOPython as fIOP
import tTsys2
import numarray
import carmaHelpers as helpers

#
# 
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

s=Subarray.getSubarray()
dayLabel = short.yearMonthDay()

def performDip(antref,fileName='/home/obs/zauderer/Dips/'+dayLabel+'.dip2.dat',Azimuth=180.0,reps=3,bands=[1,2,3]) :
    """This performs a dip.  asdjkldsfajkdsfjrfjfj
    fjfjfjfjf
    to print this:  print __doc__
    """
    cancel()
    antVec = short.checkAntVectorType(antref)
    tamb = []
    for i in antVec : tamb.append(short.getTamb(i)+273.15)
    #short.limitSubarray(antVec)       # This limits the subarray to the antennas that the user inputs.  
    # If one antenna is entered, can be a single number, else, needs to 
    # be a list contained in brackets.
    #steps=[1.05,1.1,1.15,1.20,1.25,1.50,1.75,2.0,2.25,2.50,2.75,3.0,3.25,3.5]
    steps=[1.05,1.25,1.75,2.50,3.0,3.5]
    numSteps=len(steps)
    stop(antVec)
    bands = helpers.makeList(bands)
    print 'I made it before the loop!'
    for ii in range(numSteps) :
        print 'I made it into the loop.'
        el_radians = m.asin(1.0/steps[ii])    # m.asin returns the arc sin in radians
        elevation = (el_radians*180.0)/m.pi
        print 'After the elevation values!',el_radians,elevation
        move(el=elevation,az=180.0)
        [tsysVals,tsysStats,junk] = tTsys2.multiTsys(antVec,bands,1.5,reps)
        print numSteps
        utStamp = short.getMiriadUTStamp()
        tout=short.getTout()
        fd = open(fileName,'a')
        fd.write("\ntime:%s \t Tout: %f\t" % (utStamp,tout) )
        for j in range(len(antVec)) :
            fd.write("\n antenna%s \t Tamb:%s\t " % (str(antVec[j]),str(tamb[j])) )
            for k in range(len(bands)) :
                tempMonitor = queryDouble("SlPipeline.Input%s.Band%s.Tsys.Dsb" % (str(antVec[j]),bands[k]),4)
                fd.write("band%s: \t AM: %f \t SBR_Tsys(K): %f \t Tsys(monitor): %s \t" % (bands[k],steps[ii],tsysVals[j][k],str(tempMonitor) ) )
    fd.close()
    #short.fillSubarray()           # Returns all possible antennas to the subarray for next user
    return tsysVals
