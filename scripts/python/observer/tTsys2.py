# This version takes into account the different manner in which the ambient load is applied
# for the BIMA antennas - called by dip2
#
import carma
import subarrayControl
import Subarray
from device import *
import math as m
import time
from subarrayCommands import *
import short
import monitorDataManip as mDM
import carmaHelpers as helpers
#
s=Subarray.getSubarray()
#
def multiTsys(antref,band=[1,2,3],intTime=0.5,reps=1,type='median',ofTheMean=True) :
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

    
def tTsys(antref,band,intTime=0.5) :
    """ This function calculates the system temperature given an antenna [1-15], a band [1-3], and optionally
    an integration time.  If the latter is not specified, a default of 0.5 seconds is used.  Tsys is also
    calculated online, but this function allows minor changes to be made for easy comparison with the system
    calculation.
    *****
    Currently, we have the following sideband gain ratios at 95 GHz:
    Band 2: [1.02,1.05,1.000,1.02,1.17,1.01,1.12,1.30,1.27,1.10,1.21,1.04,1.12,0.78,0.97]
    Band 3: [0.90,1.01,1.000,1.03,1.04,0.98,1.24,1.21,1.38,1.13,1.15,1.04,1.27,0.85,0.95]
    *****
    The other main assumption is that the zenith opacity is 0.2.  In actuality, it may be closer to 0.1 or lower.
    (edited by A. Zauderer, 11/30/2006, azaudere@astro.umd.edu)
    """
    #SBR = [1.000,0.524,1.177,1.000,1.170,0.948,1.008,1.123,1.061,1.517,0.991,0.938,1.002,1.068,1.051,0.904]
    SBR = [1.000,1.02,1.05,1.000,1.02,1.17,1.01,1.12,1.30,1.27,1.10,1.21,1.04,1.12,0.78,0.97]
    #SBR = [1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000]
    eta=0.975           #AZ - this is canonical value
    tau = 0.1           #AZ -  eventually need to grep weather station to get more accurae zenith tau
    tcmb = 2.7          #AZ - Cosmic BG temp (K)
    freq = queryDouble('Control.Subarray1.loFreq',2)    #AZ - get freq.
    tsky = calc_RJ_equiv(freq,tcmb)     #AZ - calculate RJ temp - a small correction
    intSteps = int(round(intTime/0.5))
    medianPosition = intSteps/2
    antVec = short.checkAntVectorType(antref)
    bands = helpers.makeList(band)
    amb(antVec)
    elevation = []              #AZ - following 4 lines to get currernt elevation of telescope
    AM = []                             
    for i in range(len(antVec)) :
        elevation.append(getElevation(antVec[i]) )
        AM.append( 1.0/m.sin(getElevation(antVec[i])) )
    yfactorS    = []
    tsysS       = []
    tamb        = []
    tout = short.getTout()      #AZ - get outside temp.
    psysFullAmb = mDM.averageMonitorSingle(antVec,'psys',bands,intTime)
    sky(antVec)
    for i in antVec : tamb.append(short.getTamb(i)+273.15)
    psysFullSky = mDM.averageMonitorSingle(antVec,'psys',bands,intTime)
    [psysMedAmb,psysErrAmb] = mDM.combineData(psysFullAmb,antVec,bands,monitorPoint='psys')
    [psysMedSky,psysErrSky] = mDM.combineData(psysFullSky,antVec,bands,monitorPoint='psys')
    for i in range(len(antVec)) :
        R = SBR[antVec[i]]
        print "The antVec is %s and the SBR is %s"%(str(antVec[i]),str(R))
        yfactorB = []
        yfactorBS = []
        tsysB = []
        tsysBS = []
        for j in range(len(bands)) :
            yfactorB.append(10.0**((psysMedAmb[i][j]-psysMedSky[i][j])/10.0))
            yfactorBS.append(str(round(10.0**((psysMedAmb[i][j]-psysMedSky[i][j])/10.0)*100.0)/100.0))
        for j in range(len(bands)) :
#            tsysB.append(tamb[i]/(yfactorB[j]-1.0)-yfactorB[j]*tsky)
#            tsysBS.append(str(round((tamb[i]/(yfactorB[j]-1.0)-yfactorB[j]*tsky)*100.0)/100.0))
#            tsysB.append((tamb[i]-yfactorB[j]*tsky)/(yfactorB[j]-1.0))
            #AZ -  Calls the function below to get Tsys - for "simple" calculation, could switch to calc_simple
            tsysB.append(calc_complicated(tau,R,AM[i],tout,tsky,eta,tamb[i],yfactorB[j]))
            tsysBS.append(str(round( (calc_complicated(tau,R,AM[i],tout,tsky,eta,tamb[i],yfactorB[j]))*100.0)/100.0) )
            #tsysB.append((tamb[i]-tsky)/(yfactorB[j]-1.0))
            #tsysBS.append(str(round(((tamb[i]-yfactorB[j]*tsky)/(yfactorB[j]-1.0))*100.0)/100.0))
            print "Telescope %s has yfactors %s and tsys %s for bands %s" % (str(antVec[i]),yfactorBS,tsysBS,bands)
# Following section added by A. Zauderer to check Tsys calculations-----------------------------------------
            pskyS = []
            pambS = []
            tambS = []
            pskyS.append(str(round(psysMedSky[i][j]*100.0)/100.0 ))
            pambS.append(str(round(psysMedAmb[i][j]*100.0)/100.0 ))
            tambS.append(str(round(tamb[i]*100.0)/100.0))
#               print "\n Psky = %f \t Pamb = %f \t Tamb = %f \t Tsky = %f\n" % (psysMedSky[i][j], psysMedAmb[i][j], tamb[i], tsky)
            #print ("\n Psky=%s\t Pamb=%s\t Tamb=%s\t Tsky=%f\n" % (pskyS, pambS, tambS, tsky) )
#            print "\n Psky=%f\t Pamb=%f\t Tamb=%f\t Tsky=%f\n" % (psysMedSky[j],psysMedAmb[j],tamb[i],tsky)
# End A.Z. section added ---------------------------------------------------------------------
        yfactorS.append(yfactorB)
        tsysS.append(tsysB)
    return [tsysS,psysMedAmb]

def getElevation(antNum) :
    nameTele = short.getAntName(antNum)
    if antNum < 7 : elevation = queryDouble("%s.Drive.Track.actualElevation" % nameTele,4 )
    if 6 < antNum < 16 : elevation = queryDouble("%s.AntennaCommon.Drive.Track.actualElevation" % nameTele,4)
    print "The elevation is %f degrees"% elevation
    el = (elevation*m.pi)/180.0
    return el               # returns elevation for each antenna in radians

def calc_simple(tamb,yfactor,tsky) :
    Tsys = (tamb - tsky)/(yfactor-1.0)                    # in Dave's code
#    Tsys = (tamb - yfactor*tsky)/(yfactor-1.0)             # in Stuartt's tTsys.py code
# Which is Correct???????  Dave says his is - subtracting out Tsky
    return Tsys

#def calc_complicated(tau,AM,Tout,tsky,eta,tamb,yfactor) :
#    Tcal = (0.94 + 0.06*m.exp(tau*AM))*Tout - tsky + m.exp((tau*AM)/eta)*(tamb-Tout)
#    Tsys = Tcal / (yfactor-1.0)
#    return Tsys                                              

def calc_complicated(tau,R,AM,Tout,tsky,eta,tamb,yfactor) :
    tauS = tau    # lower sideband tau
    tauI = tau    # upper sideband tau
    term1 =  (1.00 + R)*(0.94*Tout - tsky)
    term2 = (1.00+R)*m.exp(tauS*AM)*(0.06*Tout)
    term3 = R*(m.exp((tauS-tauI)*AM)-1.00)*(0.94*Tout-tsky)
    term4 = (1.00+R)*m.exp((tauS*AM)/eta)*(tamb-Tout)
    TcalSbrL = term1 + term2 + term3 + term4

    Rup = (1.00/R)
    term1up =  (1.00 + Rup)*(0.94*Tout - tsky)
    term2up = (1.00+R)*m.exp(tauI*AM)*(0.06*Tout)
    term3up = Rup*(m.exp((tauI-tauS)*AM)-1.00)*(0.94*Tout-tsky)
    term4up = (1.00+Rup)*m.exp((tauI*AM)/eta)*(tamb-Tout)
    TcalSbrU = term1up + term2up + term3up + term4up
    Tcal = (TcalSbrL + TcalSbrU) / 4.00    # to get equivalent double sideband temp
    #Tcal = (0.94 + 0.06*m.exp(tau*AM))*Tout - tsky + m.exp((tau*AM)/eta)*(tamb-Tout)
    Tsys = Tcal / (yfactor-1.0)
    return Tsys


                                                                                                   
def calc_RJ_equiv(frequency, T) :  # freq in GHz, T in K
# Returns RJ brightness T for a bbody at physical temp. T
    a = 4.799*10**(-2.0)                # * 10^9 * h/k
    b =  (a*frequency/(m.exp(a*frequency/T)-1.0))
    print "%f K scaled to %f K" % (T,b)
    return b


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
