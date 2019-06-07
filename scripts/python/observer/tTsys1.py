#
# ACTIVE script.  Do NOT move.
#
# This version takes into account the different manner in which the ambient load is applied
# for the BIMA antennas - called by dip1.py
# Last edit:  December 2006
# Originally written by S. Corder, modifications/upkeep by B. Ashley Zauderer
# azaudere@astro.umd.edu
#
import carma
import Subarray
#from device import *
import math as m
import time
from subarrayCommands import *
import short
import monitorDataManip as mDM
import carmaHelpers as helpers

s=Subarray.getSubarray()

def multiTsys(airmass,antref,SBR1,SBR2,SBR3,band=[1,2,3],intTime=0.5,reps=1,type='median',ofTheMean=True) :
    """This function is a basic wrapper for the function tTsys below, which allows tTsys to be called
    multiple times, and an average taken.  It is supposed to also do statistics, however, this 
    functionality is currently not tested.  All variables are described below in the tTsys header, with
    the exception of the following:
        reps=1:  the number of repetitions of tTsys that multiTsys will average together.  Default is 1 rep.
        type='median':  This is the type of statistical average taken
        ofTheMean:      Also used in the statistical package, which is not fuly functioning  
    """
    tsysReps = []
    psysReps = []
    antVec = short.checkAntVectorType(antref)
    band = helpers.makeList(band)
    for i in range(reps) :
        [tsysOut,psysAmb] = tTsys(airmass,antVec,SBR1,SBR2,SBR3,band,intTime)
        tsysReps.append(tsysOut)
        psysReps.append(psysAmb)
    if reps > 1 :
        [statsVal,statsErr] = mDM.combineData(tsysReps,antVec,band,type,'psys',ofTheMean)
        return [statsVal,statsErr,psysReps]
    else : return [tsysReps,[0.0],psysReps]

    
def tTsys(airmass,antref,SBR1,SBR2,SBR3,band,intTime=0.5) :
    """ This function calculates the system temperature given an antenna [1-15], a band [1-3], and optionally
    an integration time.  If the latter is not specified, a default of 0.5 seconds is used.  Tsys is also
    calculated online, but this function allows minor changes to be made for easy comparison with the system
    calculation.
    *****
    -VARIABLES-
    airmass:  a variable passed from dip1.py in order to print the AM for SM analysis.  This variable
              can be removed if the fd.write statement is commented out.
    antref:   the vector list of antennas in the subarray
    SBR1:     the gain sideband ratios for band 1 - MUST be a 16 element array, where SBR1[0]=1.0
    SBR2:     "    "     "        "     "  band 2 - MUST be a 16 element array
    SBR3:     "    "     "        "     "  band 3 - MUST be a 16 element array
    band:     Vector/list of the bands used.  Most likely = [1,2,3]
    intTime:  integration time.  By default is set at a half second.
    *****
    Currently, we have the following sideband gain ratios at 95 GHz.  However, instead of
       hard coding these values in, they are put in as arguments for ease in changing. 
    Band 2: [1.02,1.05,1.000,1.02,1.17,1.01,1.12,1.30,1.27,1.10,1.21,1.04,1.12,0.78,0.97]
    Band 3: [0.90,1.01,1.000,1.03,1.04,0.98,1.24,1.21,1.38,1.13,1.15,1.04,1.27,0.85,0.95]
    *****
    The other main assumption is that the zenith opacity is 0.2.  In actuality, it may be closer to 0.1 or lower.
    (edited and maintained by A. Zauderer, 12/2006, azaudere@astro.umd.edu)
    """
    dayLabel=yearMonthDay()                                  # getting the date
    freq = queryDouble('Control.Subarray1.loFreq',24)   #AZ - get freq.
    # File handling:
    #fileName='/home/obs/zauderer/Dips/'+dayLabel+'.dip1_tsys.dat'  # setting the output file name
    if (freq > 120.) :
       fileName='/misc/array/rt/Dips/'+dayLabel+'.dip1mm_tsys.dat'     #setting output file name
    else :
       fileName='/misc/array/rt/Dips/'+dayLabel+'.dip3mm_tsys.dat'     #setting output file name
    try :
        os.stat('/array/rt/Dips/')
    except Exception :
        if (freq > 120.) :
            fileName = '/tmp/' + dayLabel + '.dip1mm_tsys.dat'
        else :
            fileName = '/tmp/' + dayLabel + '.dip3mm_tsys.dat'
    fd=open(fileName,'a')
    utStamp = short.getMiriadUTStamp()
    fd.write("\nDate:%s \t Time: %s \t" % (dayLabel, utStamp) )
    # Setting up the array of SBR values, by antenna and band.  Note this is a 16 element array
    SBR_null = [1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1.000]
    SBR=[SBR_null,SBR1,SBR2,SBR3]
    # Setting constants
    eta=0.975           #AZ - this is canonical value
    tau = 0.1           #AZ -  eventually need to grep weather station to get more accurae zenith tau
    tcmb = 2.7          #AZ - Cosmic BG temp (K)
    tsky = calc_RJ_equiv(freq,tcmb)                     #AZ - calculate RJ temp - a small correction
    intSteps = int(round(intTime/0.5))
    medianPosition = intSteps/2
    antVec = short.checkAntVectorType(antref)
    bands = helpers.makeList(band)
    # The command "amb" moves the ambient load in front of the Receiver
    amb(antVec)
    # Following 4 lines to get the current elevation of the antennas
    elevation = []              
    AM = []                             
    for i in range(len(antVec)) :
        elevation.append(getElevation(antVec[i]) )
        AM.append( 1.0/m.sin(getElevation(antVec[i])) )
    # Setting up arrays for the y factors, Tsys, and Ambient temps.
    yfactorS    = []     # "S" means that this is an array of string values
    tsysS       = []
    tamb        = []
    tout = short.getTout()      #AZ - get outside temp.
    psysFullAmb = mDM.averageMonitorSingle(antVec,'psys',bands,intTime)
    # The command "sky" moves the ambient load out, so Rx sees the sky
    sky(antVec)
    for i in antVec : tamb.append(short.getTamb(i)+273.15)
    psysFullSky = mDM.averageMonitorSingle(antVec,'psys',bands,intTime)
    [psysMedAmb,psysErrAmb] = mDM.combineData(psysFullAmb,antVec,bands,monitorPoint='psys')
    [psysMedSky,psysErrSky] = mDM.combineData(psysFullSky,antVec,bands,monitorPoint='psys')
    for i in range(len(antVec)) :
        #The following 4 lines are testing to make sure indices match up
        for j in range(len(bands)):
            R = SBR[bands[j]][antVec[i]]
            fd.write("The antVec is %s, the band is %s, and the SBR is %s\n" %(str(antVec[i]),str(bands[j]),str(R)) )
            print "The antVec is %s, the band is %s, and the SBR is %s"%(str(antVec[i]),str(bands[j]),str(R))
        yfactorB = []
        yfactorBS = []
        tsysB = []
        tsysBS = []
        for j in range(len(bands)) :
            # Calculating the y factor:  y = 10^[Pamb-Psky)/10]
            yfactorB.append(10.0**((psysMedAmb[i][j]-psysMedSky[i][j])/10.0))
            # Converting the y factor value to a string, for nice printing
            yfactorBS.append(str(round(10.0**((psysMedAmb[i][j]-psysMedSky[i][j])/10.0)*100.0)/100.0))
        for j in range(len(bands)) :
            #AZ -  Calls the function below to get Tsys - for "simple" calculation, could switch to calc_simple
            R=SBR[bands[j]][antVec[i]]
            tsysB.append(calc_complicated(tau,R,AM[i],tout,tsky,eta,tamb[i],yfactorB[j]))
            tsysBS.append(str(round( (calc_complicated(tau,R,AM[i],tout,tsky,eta,tamb[i],yfactorB[j]))*100.0)/100.0) )
            #print "Telescope %s has yfactors %s and tsys %s for bands %s" % (str(antVec[i]),yfactorBS,tsysBS,bands[j])
            fd.write("\nInside Tsys function:  Telescope %s has yfactors %s and tsys %s for bands %s\n" % (str(antVec[i]),yfactorBS[j],tsysBS[j],str(bands[j])) )
            fd.write("The SBR is %s\n" % (str(R)) )
# Following section added by A. Zauderer to check Tsys calculations-----------------------------------------
            pskyS = []
            pambS = []
            tambS = []
            pskyS.append(str(round(psysMedSky[i][j]*100.0)/100.0 ))
            pambS.append(str(round(psysMedAmb[i][j]*100.0)/100.0 ))
            tambS.append(str(round(tamb[i]*100.0)/100.0))
#               print "\n Psky = %f \t Pamb = %f \t Tamb = %f \t Tsky = %f\n" % (psysMedSky[i][j], psysMedAmb[i][j], tamb[i], tsky)
            fd.write("\n Psky = %s \t Pamb = %s \t Tamb = %f \t Tsky = %f\n" % (str(round(psysMedSky[i][j]*100.0)/100.0), str(round(psysMedAmb[i][j]*100.0)/100.0), tamb[i], tsky))
            # The following write line was added 12/14/2006 - imperative info. to caluclate zenith opacity from Psky measurements
            fd.write("\n SUPERMONGO AM: %s  Ant_%s_band_%s Psky %s\n" % ( str(airmass), str(antVec[i]), str(bands[j]), str(round(psysMedSky[i][j]*100.0)/100.0)) )
            fd.write("\n SMFREQ AM: %s  Ant_%s_band_%s Psky %s freq: %s\n" % ( str(airmass), str(antVec[i]), str(bands[j]), str(round(psysMedSky[i][j]*100.0)/100.0),str(freq) ) ) 
            #print ("\n Psky=%s\t Pamb=%s\t Tamb=%s\t Tsky=%f\n" % (pskyS, pambS, tambS, tsky) )
#            print "\n Psky=%f\t Pamb=%f\t Tamb=%f\t Tsky=%f\n" % (psysMedSky[j],psysMedAmb[j],tamb[i],tsky)
# End A.Z. section added ---------------------------------------------------------------------
        yfactorS.append(yfactorB)
        tsysS.append(tsysB)
    fd.close()
    return [tsysS,psysMedAmb]

def getElevation(antNum) :
    """ The purpose of this function is to return the elevation of the given antenna, antNum.
    It returns ths elevation in radians.  Here a calculation is necessary, because the monitor
    system returns the elevation in degrees.
    """
    nameTele = short.getAntName(antNum)
    # Must divide between OVRO and BIMA dishes, because the elevation monitor point is slightly
    # different for the two types of antennas.
    if antNum < 7 : elevation = queryDouble("%s.Drive.Track.actualElevation" % nameTele,24 )
    if 6 < antNum < 16 : elevation = queryDouble("%s.AntennaCommon.Drive.Track.actualElevation" % nameTele,24)
    print "The elevation is %f degrees"% elevation
    el = (elevation*m.pi)/180.0
    return el               # returns elevation for each antenna in radians

def calc_simple(tamb,yfactor,tsky) :
    bady=yfactor-1.0
    if bady == 0.0: Tsys=10000.0
    else: Tsys = (tamb - tsky)/(yfactor-1.0)                    # in Dave's code
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
    bady=yfactor-1.0
    if bady == 0.0: Tsys=10000.0
    else: Tsys = Tcal / (yfactor-1.0)
    return Tsys


                                                                                                   
def calc_RJ_equiv(frequency, T) :  # freq in GHz, T in K
# Returns RJ brightness T for a bbody at physical temp. T
    a = 4.799*10**(-2.0)                # * 10^9 * h/k
    b =  (a*frequency/(m.exp(a*frequency/T)-1.0))
    print "%f K scaled to %f K" % (T,b)
    return b


def writeTsys(antref,fileName,bands=[2,3],intTime=0.5,reps=1) :
    """ Currently this module is not funtioning. """
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
