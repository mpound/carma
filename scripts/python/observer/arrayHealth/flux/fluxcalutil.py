"""
Flux calibration script utility

"""
import subarrayCommands as sac
import obsdef2 as od
import fluxcalinput as fxinput

def tune1mm():
    restfreq = 232.65
    iffreq   = 2.65
    sideband = sac.USB
    sac.configband(1,sac.BW500,restfreq-1.30)
    sac.configband(2,sac.BW500,restfreq)
    sac.configband(3,sac.BW500,restfreq+1.35)
    # Tune receivers
    sac.trackMessage("Tuning freq=" + str("%.3f" % restfreq) + " GHz, " + 
       str(sideband)  + ", IFfreq=" + str("%.3f" % iffreq) + " GHz")
    sac.freq(restfreq, sideband, iffreq, 'None')
    sac.checkbands()
    return

def tune3mm():
    restfreq = 97.65
    iffreq   = 2.65
    sideband = sac.USB
    sac.configband(1,sac.BW500,restfreq-1.30)
    sac.configband(2,sac.BW500,restfreq)
    sac.configband(3,sac.BW500,restfreq+1.35)
    # Tune receivers
    sac.trackMessage("Tuning freq=" + str("%.3f" % restfreq) + " GHz, " + 
       str(sideband)  + ", IFfreq=" + str("%.3f" % iffreq) + " GHz")
    sac.freq(restfreq, sideband, iffreq, 'None')
    sac.checkbands()
    return

def observeSource(source,intentObj,tint,record,nreps):
    msg = 'Observing ' + source + ' for ' + str(tint) + ' minutes'
    sac.trackMessage(msg, indent=1)
    sac.intent(source, intentObj)
    sac.track(source)
    sac.tsys()
    sac.integrate(record, nreps)
    return

def wk_isup(source,elevLimit):
    elevLimit_org = sac.queryDouble('Control.Subarray1.elevLimit')
    sac.elevlimit(elevLimit)
    value = sac.isup(source) # 1 is up and 0 is down
    sac.elevlimit(elevLimit_org)
    return value
   
def addtofiledone(target,whichBand):
    filenameDone='/home/obs/scripts/flux/fluxcaldone'+whichBand+'.list'
    outfile=open(filenameDone,'a')
    outfile.write(target+'\n')
    outfile.close()
    return
 
def selectPrimary(whichList):
    if whichList=='weekly':
        primaryName = ['URANUS','MWC349','MARS','NEPTUNE']
    else:
        primaryName = ['URANUS','MWC349','MARS','NEPTUNE','3C84','3c273','3C345']
    elevLimit_org = sac.queryDouble('Control.Subarray1.elevLimit')
    sac.elevlimit(fxinput.minElevation)
    for source in primaryName:
        if sac.isup(source) == 1 and sac.azel(source)[1] < fxinput.maxElevation :
            sac.elevlimit(elevLimit_org)
            return source
    sac.elevlimit(elevLimit_org)
    return None

def possibleCals(whichList,whichBand):
    if whichList=='weekly':
        fluxcalName = ['3C345', '3C84', '3C273', 'MWC349']
    elif whichList=='monthly':
        fluxcalName = ['3C84','0530+135','0854+201','0927+390','3C273','3C279',\
                       '3C345','1751+096','2148+069','3C446','3C454.3','MWC349',\
                       'Bllac','3C111','0423-013','3C120','1058+015','2232+117',\
                       '0359+509','0102+584','W3OH','1517-243','1924-292','1911-201']
        currentBrightSources = od.getBrightSources(2.0)
        for name in currentBrightSources:
            if name in fluxcalName: continue
            if name == '2013+370': continue
            fluxcalName.append(name)
    elif whichList=='full':
    # Reading flux calibrators from files
    # fluxcaldone[31]mm.list : file with names of calibrators that have been observed
    # fluxcalname[31]mm.list : file with names of all calibrators
        fluxcalDone=[]
        fluxcalName=[]
        if whichBand != 'both':
            filenameDone='/home/obs/scripts/flux/fluxcaldone'+whichBand+'.list'
            filenameName='/home/obs/scripts/flux/fluxcalname'+whichBand+'.list'
        else:
            raise Exception, "Both bands (1 & 3 mm) are not available for the full list."
        for line in open(filenameDone,'r').readlines():
            name=line.split()[0]
            fluxcalDone.append(name)
        for line in open(filenameName,'r').readlines():
            name=line.split()[0]
            if name in fluxcalDone: continue
            fluxcalName.append(name)
        # add current "bright" calibrators for the case of 1mm full list
        if whichBand == '1mm':
           currentBrightSources = od.getBrightSources(1.0)
           for name in currentBrightSources:
               if name in fluxcalName: continue
               if name == '2013+370': continue
               if name in fluxcalDone: continue
               fluxcalName.append(name)
    else:
        raise Exception, "Please check the list option; it should be list='weekly', 'monthly', or 'full'"

    elevLimit_org = sac.queryDouble('Control.Subarray1.elevLimit')
    sac.elevlimit(fxinput.minElevation)
    listCals = []
    for source in fluxcalName:
        if sac.isup(source) == 1 and sac.azel(source)[1] < fxinput.maxElevation :
            listCals.append(source)
    sac.elevlimit(elevLimit_org)
    return listCals

def sortSources(sourceList):
    cutAz = 330.0
    startAz = 290.0
    # These sorting helpers below were taken from "stars.py" and modified.
    def cmpa(x,y):
        # sorting helper for azimuth (note the breakpoint at cutAz!!!)
        # sorting helper for azimuth (note the breakpoint at startAz!!!)
        def taz(a):
            #if a<cutAz: return a
            if a<startAz: return a
            return a-360
        a=taz(x[1])
        b=taz(y[1])
        if a<b: return -1
        if a>b: return 1
        return 0
    def cmpz(x,y):
        # sorting helper for reverse azimuth (note the breakpoint at cutAz!!!)
        # sorting helper for reverse azimuth (note the breakpoint at startAz!!!)
        def taz(a):
            #if a<cutAz: return a
            if a<startAz: return a
            return a-360
        a=taz(x[1])
        b=taz(y[1])
        if a<b: return 1
        if a>b: return -1
        return 0
    def cmpe(x,y):
        # sorting helper for elevation
        if x[2]<y[2]: return -1
        if x[2]>y[2]: return 1
        return 0
    def cmpza(x,y) :
        # sorting helper for zenith angle
        if x[2]<y[2]: return 1
        if x[2]>y[2]: return -1
        return 0
    def cmpt(x,y):
        # sorting helper for time remained before setting.
        if x[3]<y[3]: return -1
        if x[3]>y[3]: return 1
        return 0

    sourceData = []
    for source in sourceList:
        azNow       = sac.azel(source)[0]
        elNow       = sac.azel(source)[1]
        timeUp      = sac.whendown(source)
        timeTransit = sac.whentransit(source)
        sourceData.append([source,azNow,elNow,timeUp,timeTransit])
    sourceData.sort(cmpz)
    sourceDataAz = []
    for i in range(len(sourceList)):
        sourceDataAz.append(sourceData[i][1])
        if sourceDataAz[i] > startAz: sourceDataAz[i] = sourceDataAz[i] - 360.
    bins = []
    breakIndex = [0]
    sourceDataPart = []
    sourceDataSorted = []
    binSize = 20        # degrees
    for i in range(360/binSize+1): 
        bins.append(startAz-binSize*i) # bins = [290,270,250,...,10,-10,-30,-50,-70], 19 elements
    j = 0
    for i in range(len(bins)-1):       # i = 0 ~ 17 : 18 elements
        while((sourceDataAz[j] <= bins[i]) and (sourceDataAz[j] > bins[i+1]) and (j < len(sourceList)-1)):
            j=j+1
        breakIndex = breakIndex + [j] # breakIndex indicates the first cal in each bin.
    breakIndex[len(breakIndex)-1] = breakIndex[len(breakIndex)-1] + 1  # increase the last breakIndex by 1.

    for i in range(len(bins)-1):
        if i%2: my_cmp = cmpe
        else: my_cmp = cmpza
        sourceDataPart = sourceData[breakIndex[i]:breakIndex[i+1]]
        sourceDataPart.sort(my_cmp)
        sourceDataSorted = sourceDataSorted + sourceDataPart

    return sourceDataSorted
