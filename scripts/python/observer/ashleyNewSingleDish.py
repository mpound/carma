# This script, ashleyNewSingleDish.py
# contains everything that is needed for single dish aperture efficiencies.  It 
# contains its own Tsys function
#


#
import math as m
import carma
import subarrayControl,Subarray
from subarrayCommands import *
import short
import monitorDataManip as mDM
import carmaHelpers as helpers

s=Subarray.getSubarray()
singleDishObjects = ['jupiter','venus','mars','sun','moon','mercury']
yak =  short.getMiriadUTStamp()
fileName='/home/obs/radioPointData/tsysData/apr24MarsSD.dat'
#fileName='/home/obs/radioPointData/tsysData/'+yak+'efficiencySD.dat'
#fileName='/home/obs/radioPointData/tsysData/April5efficiencySD.dat'


def getElevation(antNum) :
    nameTele = short.getAntName(antNum)
    if antNum < 7 : elevation = queryDouble("%s.Drive.Track.actualElevation" % nameTele,4 )
    if 6 < antNum < 16 : elevation = queryDouble("%s.AntennaCommon.Drive.Track.actualElevation" % nameTele,4)
    print "The elevation is %f degrees"% elevation
    el = (elevation*m.pi)/180.0
    return el                           # returns elevation for each antenna in radians

def multiTsys(antref,band=[2,3],intTime=0.5,reps=1,type='median',ofTheMean=True) :
    tsysReps = []
    psysReps = []
    antVec = short.checkAntVectorType(antref)
    band = helpers.makeList(band)
    for i in range(reps) :
        [tsysOut,psysAmb] = tTsys(antVec,band,intTime)
        tsysReps.append(tsysOut)
        psysReps.append(psysAmb)
    if reps > 1 :
        [statsVal,statsErr] = mDM.combineData(tsysReps,antVec,band,type,'psys',ofTheMean)
        return [statsVal,statsErr,psysReps]
    else : return [tsysReps,[0.0],psysReps]

def writeTsys(antref,fileName,bands=[2,3],intTime=0.5,reps=1) :
    [tsysVals,tsysStats,psysVals] = multiTsys(antref,bands,intTime,reps,type='median',ofTheMean=True)
    antVec = short.checkAntVectorType(antref)
    mjdVal = s.mjd(0)
    fd = open(fileName,'a')
    for k in range(reps) :
        for i in range(len(antVec)) :
            pamPowerVal = short.getPamPower(antVec[i])
            fd.write("rep: %s %s %s carma%s " % (k+1,time.asctime(),mjdVal,antVec[i]) )
        for j in range(len(bands)) :
            fd.write(" band%s:Tsys:%s " % (bands[j],tsysVals[k][i][j]) )
        fd.write("\n")
    fd.close()


def tTsys(antref,band,intTime=0.5) :
    fd = open(fileName,'a')
    eta=0.975                           # canonical value
    tcmb = 2.7
    tau = 0.1
    a = 4.799*10**(-2.0)            # 10^9 * h/k - to get in units of GHz
    #freq=88.5
    freq = queryDouble('Control.Subarray1.SubarrayCommands.loFreq',2)
    tsky = calc_RJ_equiv(freq,tcmb)
    fd.write("At freq(Hz) = %f Tsky = %f \n" % (freq,tsky) )
    tout = short.getTout()              # outdoor ambient temp. in Kelvin - from weather station
    intSteps = int(round(intTime/0.5))
    medianPosition = intSteps/2
    antVec = short.checkAntVectorType(antref)   # returns vector list of available antennas in subarray
    elevation = []
    AM = []
    for i in range(len(antVec)) :
        elevation.append(getElevation(antVec[i]) )
        AM.append( 1.0/m.sin(getElevation(antVec[i])) )
    for i in range(len(antVec)) :
        fd.write("Ant%s El (rad): %f  AM:  %s \n" % (str(antVec[i]),elevation[i],AM[i]))   
    bands = helpers.makeList(band)                      # returns bands as a list
    amb(antVec)                         # moves the cal wheel into place - tmo = 15 sec by default
    tamb = []
    psysFullAmb = mDM.averageMonitorSingle(antVec,'psys',bands,intTime)  # get avg. power with wheel in
    for i in antVec : tamb.append(short.getTamb(i)+273.15)               # get ambient temp.
    sky(antVec)                         # moves cal wheel out of the way - tmo = 15s by default
    psysFullSky = mDM.averageMonitorSingle(antVec,'psys',bands,intTime)
        #[psysMedAmb,psysErrAmb] = mDM.combineData(psysFullAmb,antVec,bands,monitorPoint='psys')
    [psysMedAmb,psysErrAmb] = mDM.combineData(psysFullAmb,antVec,bands,monitorPoint='psys')
        #[psysMedSky,psysErrSky] = mDM.combineData(psysFullSky,antVec,bands,monitorPoint='psys')
    [psysMedSky,psysErrSky] = mDM.combineData(psysFullSky,antVec,bands,monitorPoint='psys')

    yfactorB = []
    tsysB = []
    tsysSimple = []
    tsysComplex = []
    for i in range(len(antVec)) :
        yfactorB.append([])
        tsysB.append([])
        tsysSimple.append([])
        tsysComplex.append([])
        for j in range(len(bands)) :
            yfactorB[i].append(10.0**((psysMedAmb[i][j]-psysMedSky[i][j])/10.0))
            if (yfactorB[i][j] == 1.0) : yfactorB[i][j] = 1.00000001
            else : tsysB[i].append((tamb[i]-tsky)/(yfactorB[i][j]-1.0))
            tsysSimple[i].append(calc_simple(tamb[i],yfactorB[i][j],tsky))
            tsysComplex[i].append(calc_complicated(tau,AM[i],tout,tsky,eta,tamb[i],yfactorB[i][j]))
    #  Printing the results to file
    fd.write(" antenna \t band \t Tamb(K) \t Pamb \t Psky \t Yfactor \t Tsys \t Tsimple \t Tcomplex \n\n")
    for i in range(len(antVec)) :
        for j in range(len(bands)) :
            print "made it into loop"
            print len(antVec),len(bands),len(tamb),len(psysMedAmb),len(psysMedSky),len(yfactorB)
            print len(tsysB),len(tsysSimple),len(tsysComplex)
            print "Telescope %s has yfactors %s and tsys %s for bands %s" % (str(antVec[i]),yfactorB[i][j],tsysB[i][j],bands[j])
            #fd.write(" %s \t %s \t %f \t %f \t %f \t %f \t %f \t %f \t %f \n" % (str(antVec[i]), bands[j],tamb[i], psysMedAmb[i][j], psysMedSky[i][j],yfactorB[i][j],tsysB[i][j],tsysSimple[i][j],tsysComplex[i][j] ) )
            fd.write(" %s \t %s \t %s \t %s \t %s \t %s \t %s \t %s \t %s \n" % (str(antVec[i]), bands[j],str(tamb[i]), str(psysMedAmb[i][j]), str(psysMedSky[i][j]),str(yfactorB[i][j]),str(tsysB[i][j]),str(tsysSimple[i][j]),str(tsysComplex[i][j]) ) )
    currentTime = short.getMiriadUTStamp()
    fd.write("***** End of Tsys function.  UT time stamp: %s***\n\n"%currentTime)
    fd.close()
    #return [tsysB,tsysSimple,tsysComplex,psysMedAmb,psysMedSky,yfactorB,tamb,elevation,AM]
    return tsysB

def calc_simple(tamb,yfactor,tsky) :
    Tsys = (tamb - tsky)/(yfactor-1.0)                    # in Dave's code
#    Tsys = (tamb - yfactor*tsky)/(yfactor-1.0)             # in Stuartt's tTsys.py code
# Which is Correct???????  Dave says his is - subtracting out Tsky
    return Tsys
                                                                                                                                                         
def calc_complicated(tau,AM,Tout,tsky,eta,tamb,yfactor) :
    Tcal = (0.94 + 0.06*m.exp(tau*AM))*Tout - tsky + m.exp((tau*AM)/eta)*(tamb-Tout)
    Tsys = Tcal / (yfactor-1.0)
    return Tsys
                                                                                                                                                         
def calc_RJ_equiv(frequency, T) :  # freq in GHz, T in K
# Returns RJ brightness T for a bbody at physical temp. T
    a = 4.799*10**(-2.0)                # * 10^9 * h/k
    b =  (a*frequency/(m.exp(a*frequency/T)-1.0))
    print "%f K scaled to %f K" % (T,b)
    return b

def getStepOff(sourceName):     
    # how much to move off source in arcminutes
    if sourceName=='moon': 
        stepOff = 50
    elif sourceName == 'sun':
        stepOff = 50
    else: stepOff = 5
    return stepOff

def sourceSwitch(antref,sourceName,measuredOffAz=0.0,measuredOffEl=0.0,reps=5,stepOff=5.0,bands=[2,3]) :
    stepOff = getStepOff(sourceName)
    print "Reps is full cycle of B-A-B-C (a and c opposite sides of b)"    # better algorithm of on source, off, on, off in certain order
    antVec = short.checkAntVectorType(antref)
    bands = helpers.makeList(bands)
    if not checkSourceName(sourceName) :
       return 'You have not selected a sufficiently bright object, please select from: %s' % singleDishObjects
    powerValues = []
    onValues   = []
    offValues  = []
    offsetVal  = [stepOff]*len(antVec)
    offsetAzPattern = [0.0,-1.0*stepOff,0.0,1.0*stepOff]
    if (measuredOffAz == 0.0) : measuredOffAz = [0.0]*len(antVec)
    if (measuredOffEl == 0.0) : measuredOffEl = [0.0]*len(antVec)
    tsysValsFull = []
    for i in range(4*reps) :
        if not i%4 :
            print [offsetAzPattern[1]]*len(antVec),measuredOffEl,antVec
        offset([offsetAzPattern[1]]*len(antVec),measuredOffEl,antVec)
        tsysVals = tTsys(antVec,bands,1.5)
        tsysValsFull.append(tsysVals)
        tempAzOffset = offsetAzPattern[i%4]
        tempOffset = []
        monkey = i%2
        for j in range(len(antVec)) : tempOffset.append(measuredOffAz[j]+tempAzOffset)
        offset(tempOffset,measuredOffEl,antVec)
        if monkey :
            tempVal = mDM.averageMonitorSingle(antVec,'psys',bands)
            offValues.append(tempVal)
        if not monkey :
            tempVal = mDM.averageMonitorSingle(antVec,'psys',bands)
            onValues.append(tempVal)
    """ on/off 2*reps by one by ant by band"""
    diffValuesAntBA = []
    diffValuesAntCB = []
    tempAntT = []
    for j in range(len(antVec)) :
        diffValuesBandBA = []
        diffValuesBandCB = []
        tempBandT  = []
        for k in range(len(bands)) :
            tempBA = []
            tempCB = []
            tempT = 0.0
            for i in range(reps) :
                indexer = i*2
                tempBA.append(onValues[i][0][j][k]-offValues[i][0][j][k])
                tempCB.append(offValues[i+1][0][j][k]-onValues[i+1][0][j][k])
                tempT = tempT+tsysValsFull[i][j][k]/reps
            diffValuesBandBA.append(tempBA)
            diffValuesBandCB.append(tempCB)
            tempBandT.append(tempT)
        diffValuesAntBA.append(diffValuesBandBA)
        diffValuesAntCB.append(diffValuesBandCB)
        tempAntT.append(tempBandT)
    diffValsBACB = []
    for i in range(reps) :
        diffValsAnt = []
        for j in range(len(antVec)) :
            diffValsBand =[]
            for k in range(len(bands)) :
                diffValsBand.append((diffValuesAntBA[j][k][i]-diffValuesAntCB[j][k][i])/2)
            diffValsAnt.append(diffValsBand)
        diffValsBACB.append(diffValsAnt)
    averageDiff = []
    errorDiff   = []
    for i in range(len(antVec)) :
        bandTemp = []
        bandErr  = []
        for j in range(len(bands)) :
            tempData = 0.0
            tempErr  = 0.0
            for k in range(reps) :
                tempData = tempData+diffValsBACB[k][i][j]/reps
            if reps > 1 : 
                for k in range(reps) : tempErr = tempErr + (diffValsBACB[k][i][j]-tempData)**2.0/float(reps-1)
                tempErr = tempErr**0.5
            bandErr.append(tempErr)
            bandTemp.append(tempData)
        averageDiff.append(bandTemp)
        errorDiff.append(bandErr)
    utStamp = short.getMiriadUTStamp()
    fd = open(fileName,'a')
    fd.write("\n**** Single Dish function printing********** \n \n")    
    for i in range(reps) :
        fd.write("source: %s time: %s rep: %s \n" % (sourceName,utStamp,str(i+1)) )
        for j in range(len(antVec)) :
            fd.write("\n antenna: %s \t" % str(antVec[j]) )
            for k in range(len(bands)) :
                fd.write(" band%s:(diff(dBm): %f tsys(K): %f" % (bands[k],diffValsBACB[i][j][k],tsysValsFull[i][j][k]))
    for j in range(len(antVec)) :
        fd.write("antenna: %s  source: %s time: %s reps: %s \n" % (str(antVec[j]),sourceName,utStamp,reps) )
        for k in range(len(bands)) :
            fd.write("\n band%s:(AvgDiff(dBm): %f rms(diff(dBm)): %f tsys(K): %f \n" % (bands[k],averageDiff[j][k],errorDiff[j][k],tempAntT[j][k]) )
            linearRatio = 10.0**((averageDiff[j][k])/10.0)-1.0
            fd.write("Linear ratio to use is %f"  % (linearRatio) )
            if (antVec[j] > 6):  systemTemp = 1.05*tempAntT[j][k]+4.05
            else:  systemTemp = tempAntT[j][k]
            #measuredTemp = tempAntT[j][k]*linearRatio
            measuredTemp = systemTemp*linearRatio
            fd.write("  Measured temp of %s is %f (Tsys = %f). \n" % (str(antVec[j]),measuredTemp,systemTemp) )
                # The lines below will calculate the aper. eff.
            sourceDiameter = setSourceSize(sourceName)
            telescopeBeam = short.getNyquistStep(antVec[j])[0]*60.0          # True for OVRO, BIMA is more like 51.6''
            if (sourceDiameter < telescopeBeam) :
                xRatio = m.log(2.0)*(sourceDiameter/telescopeBeam)**2.0 
            else : xRatio = m.log(2.0)
            fRatio = xRatio / (1 - m.exp(-xRatio))
            fd.write("f factor: %f \t telescope beam:('') %f\t Source diameter:('') %f\n" % (fRatio,telescopeBeam,sourceDiameter) )
            brightnessTempOfPlanet=setSourceTemp(sourceName)    # the brightness temp of Jupiter at 90 GHz
            #freq=88.5
            freq = queryDouble('Control.Subarray1.SubarrayCommands.loFreq',2)
            #calculatedEff = 0.0309*(measuredTemp/fRatio)*(170.0/brightnessTempOfPlanet)*((telescopeBeam/sourceDiameter)**2.0)*(110.2/freq)**2
            antenna = antVec[j]
            calculatedEff1 = efficiencyCalculator1(sourceName,antenna,measuredTemp,fRatio,brightnessTempOfPlanet,telescopeBeam,sourceDiameter,freq)
            calculatedEff2 = efficiencyCalculator2(sourceName,antenna,measuredTemp,fRatio,brightnessTempOfPlanet,telescopeBeam,sourceDiameter,freq)
            fd.write("Efficiency:  %f, %f \t Planet Brightness Temp: %f \n\n" % (calculatedEff1*100.0, calculatedEff2*100.0,brightnessTempOfPlanet) )
    fd.close()
    offset(0.0,0.0,antVec)
    return [averageDiff,tsysVals]

def efficiencyCalculator1(sourceName,antenna,measuredTemp,fRatio,brightnessTempOfPlanet,telescopeBeam,sourceDiameter,freq) :
    if sourceName=='jupiter':
        if sourceDiameter < telescopeBeam:
            calculatedEff = 0.0309*(measuredTemp*fRatio)*(170.0/brightnessTempOfPlanet)*((telescopeBeam/sourceDiameter)**2.0)*(110.2/freq)**2
        else :
            calculatedEff = 0.0309*(measuredTemp)*(170.0/brightnessTempOfPlanet)*(110.2/freq)**2
    elif sourceName=='mars':
        calculatedEff = 0.0309*(measuredTemp*fRatio)*(207.58/brightnessTempOfPlanet)*((telescopeBeam/sourceDiameter)**2.0)*(110.2/freq)**2
    elif sourceName=='moon':
        f = 1.0     # or perhaps f = 1.38?
        wavelength = (3.0*10**8)/(freq*10**9)
        solidAngle = telescopeBeam**2*m.pi*(.000305/12960000.0)   # .000305 str per square degree and 12,960,000 square arcsec per sq deg
        #antennaArea = ((short.getDishDiameter(antenna)/1000.0)**2)*(m.pi/4.0)
        antennaArea = (10.4**2)*(m.pi/4.0)
        calculatedEff = ((measuredTemp*f)/brightnessTempOfPlanet)*(wavelength)**2*(1.0/(solidAngle*antennaArea))
        #calculatedEff = 0.0309*(measuredTemp)*(170.0/brightnessTempOfPlanet)*(110.2/freq)**2
    return calculatedEff

def efficiencyCalculator2(sourceName,antenna,measuredTemp,fRatio,brightnessTempOfPlanet,telescopeBeam,sourceDiameter,freq) :
    if sourceName=='jupiter':
            calculatedEff = 0.0309*(measuredTemp*fRatio)*(170.0/brightnessTempOfPlanet)*((telescopeBeam/sourceDiameter)**2.0)*(110.2/freq)**2
    elif sourceName=='mars':
        calculatedEff = 0.0309*(measuredTemp*fRatio)*(207.58/brightnessTempOfPlanet)*((telescopeBeam/sourceDiameter)**2.0)*(110.2/freq)**2
    elif sourceName=='moon':
        f = 1.0     # or perhaps f = 1.38?
        wavelength = (3.0*10**8)/(freq*10**9)
        solidAngle = telescopeBeam**2*m.pi*(.000305/12960000.0)   # .000305 str per square degree and 12,960,000 square arcsec per sq deg
        #antennaArea = ((short.getDishDiameter(antenna)/1000.0)**2)*(m.pi/4.0)
        antennaArea = (10.4**2)*(m.pi/4.0)
        calculatedEff = ((measuredTemp*f)/brightnessTempOfPlanet)*(wavelength)**2*(1.0/(solidAngle*antennaArea))
        #calculatedEff = 0.0309*(measuredTemp)*(170.0/brightnessTempOfPlanet)*(110.2/freq)**2
    return calculatedEff


def setSourceSize(sourceName) :
# make an if source= ['jupiter','venus','mars','sun','moon','mercury'] then set for each.
# need to do a pipe open to get to unix prompt and a checksource source='sourceName'
#
    print "The source currently is %s" % sourceName
    if sourceName == 'jupiter':
        majorAxis = 41.31               # major axis of Jupiter
        minorAxis = 38.63               # minor axis of Jupiter
    elif sourceName == 'mars':
        majorAxis = 6.44
        minorAxis = 6.40
    elif sourceName == 'moon':
        majorAxis = 1906
        minorAxis = 1902
    else:
        majorAxis = 10
        minorAxis = 10
    sourceDiameter = m.sqrt(majorAxis*minorAxis)
    return sourceDiameter

def setSourceTemp(sourceName) :
# http://home.cwru.edu/~sjr16/advanced/planets_main.html
    if sourceName == 'jupiter':
        Tp = 175
    elif sourceName == 'venus':
        Tp = 734
    elif sourceName == 'mars':
        Tp = 208
    elif sourceName == 'sun':
        Tp = 6000
    elif sourceName == 'mercury':
        Tp = 440
    elif sourceName == 'moon':
        Tp = 200                  # temp for moon at 100 GHz
    else:
        Tp = 360              
    return Tp


def checkSourceName(sourceName) :
    if (sourceName.lower() not in singleDishObjects) : return 0
    else : return 1
