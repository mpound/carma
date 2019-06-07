
import carma
import carmaIni
import Subarray
from device import *
import time
import carmaHelpers as helpers
import subarrayCommands as sac

#
s = Subarray.getSubarray()
#
"""
The things below seriously average and mess with monitor data.  Before using make sure
you fully understand the indexing as it is difficult in python and sometimes does not make
sense at all!
"""

def avgLinear(data,type) :
    rmsVal = 0.0
    reps = len(data)
    if type == 'median' :
        midPoint = int(reps/2)
        checkMid = reps%2
        data.sort()
        if ((checkMid) and (reps > 1)) : return1 = (data[midPoint]+data[midPoint+1])/2.0
        else : return1 = data[midPoint]
    elif type == 'mean' :
        return1 = 0.0
        for k in data : return1 = return1+(i/reps)
    else : return 'Either median or mean are supported, not %s' % type
    for i in data : rmsVal = rmsVal+(return1-i)**2.0
    if reps > 1: return2 = (rmsVal/(float(reps-1)))**0.5
    else: return2 = 0.0
    return [return1,return2]

def avgDbM(data,type) :
    dataLinear = []
    for i in data : dataLinear.append(10.0**(i/10.0))
    [return1,return2] = avgLinear(dataLinear,type)
    return1New = 10.0*m.log10(return1)
    if return2 == 0.0 : return2New='err'
    else : return2New = 10.0*m.log10(return2)
    return [return1New,return2New]
def getTamb(antNum) :
    nameTele = getAntName(antNum)
    if antNum < 7 : tamb = SAC.queryDouble("%s.Drive.Weather.ambientTemp" % nameTele,20 )
    if 6 < antNum < 16 : tamb = SAC.queryDouble("%s.BimaSpecific.CalPlate.tempAmb" % nameTele,20)
    return tamb

def getPsys(antref,band) :
    inputQuery = "Sldc.Band%s.Input%s.psys" %(str(band),str(antref))
    psys = SAC.queryDouble(inputQuery,20)
    return psys

def combineData(data,antVec,band,type='median',monitorPoint='psys',ofTheMean=True) :
    
#    print """This assumes that data is 3D with antVec, band and
#    repition terms and it assumes that the data is formated
#    data(k,i,j) where k is reps (intTime*2) i is antenna and j is
#    band. Type takes median as default, but can take mean.  Also you
#    can do standard deviation of the mean or sample if type is mean.
#    You should define what unit your get function is in by adding it
#    to the unitsTable as show below. In theory I could eliminat band as
#    the necessary data is included in data...but I've been using it too much
#    and it helps identifiy which axis is which! If you want to consistently
#    take it out of every script, be my guest.
#    """
    unitsTable = { 'psys' : 'dB','tamb' : 'linear'}
    band = helpers.makeList(band)
    antVec = sac.makeAntList(antVec)
    dataVal = []
    dataErr = []
    reps = len(data)
    if ((reps < 2) and (type == 'mean')) :
        print "Error: you are taking stddev with one point!"
        return 0
    for i in range(len(antVec)) :
        bandVal = []
        bandErr = []
        for j in range(len(band)) :
            statVal = []
            for k in range(reps) : statVal.append(data[k][i][j])
            if unitsTable[monitorPoint] == 'dB' :
                [tempVal,tempErr] = avgDbM(statVal,type)
            elif unitsTable[monitorPoint] == 'linear' :
                [tempVal,tempErr] = savgLinear(statVal,type)
            else: return 'Invalid monitor point, point or point units not defined.'
            bandVal.append(tempVal)
            bandErr.append(tempErr)
        dataVal.append(bandVal)
        dataErr.append(bandErr)
#    print dataVal,dataErr
    return [dataVal,dataErr]

def averageMonitorSingle(antVec,monitorPoint='psys',band=[2,3],intTime=0.5) :
#    print """This program makes alot of assumptions and will probably be moved out of short!
#    It assumes you have antenna based values (i.e. selfcal'd gains, psys) and band based values.
#    It also assumes you put in some code (in short) to call a wrapper to the monitor point you want.
#    The proper call may already be there but you may also have to make your own.  See getPsys in short
#    for an example...you will have to alter code to make new monitor points available..I'm not smart
#    enough to do it otherwise.  You should add these in short as wrapers for getting series of
#    monitor data, much like getPsys.  
    reps = int(round(intTime/0.5))
    antVec = sac.makeAntList(antVec)
    band = helpers.makeList(band)
    valAll = []
    for k in range(reps) :
        valAnt = []
        for i in range(len(antVec)) :
            valBand = []
            for j in range(len(band)) :
                if monitorPoint == 'psys' : valBand.append(getPsys(antVec[i],band[j]))
                elif monitorPoint == 'tamb' : valBand.append(getTamb(antVec[i])+273.15)
                else : return 'Error : Unrecognized monitor point.'
            valAnt.append(valBand)
        valAll.append(valAnt)
        time.sleep(0.6)
    return valAll

def averageMonitorDouble(antVec,monitorPoint1='psys',monitorPoint2='tamb',band=[2,3],intTime=0.5) :
#    print """This program is alot like averageMonitorSingle except is allows simultaneous averaging
#    of two points and thus takes two monitorPoint inputs."""
    reps = int(round(intTime/0.5))
    antVec = sac.makeAntList(antVec)
    band = helpers.makeList(band)
    valAll1 = []
    valAll2 = []
    for k in range(reps) :
        valAnt1 = []
        valAnt2 = []
        for i in range(len(antVec)) :
            valBand1 = []
            valBand2 = []
            for j in range(len(band)) :
                if monitorPoint1 == 'psys' : temp1 = getPsys(antVec[i],band[j])
                elif monitorPoint1 == 'tamb' : temp1 = getTamb(antVec[i])+273.15
                else : return 'Error : Unrecognized monitor point1.'
                valBand1.append(temp1)
                if monitorPoint2 == 'psys' : temp2 = getPsys(antVec[i],band[j])
                elif monitorPoint2 == 'tamb' : temp2 = getTamb(antVec[i])+273.15
                else : return 'Error : Unrecognized monitor point2.'
                valBand2.append(temp2)
            valAnt1.append(valBand1)
            valAnt2.append(valBand2)
        valAll1.append(valAnt1)
        valAll2.append(valAnt2)
        time.sleep(0.6)
    return [valAll1,valAll2]
