# @file
# Measure tilt and tilt zeros

# @author Steve Scott
# $Id: tiltMeasurement.py,v 1.51 2014/07/23 18:31:41 scott Exp $
#
# $CarmaCopyright$

import carmaHelpers as helper
from device import *
from subarrayCommands import *
import math 
import time
import os
import pylab as plt
from matplotlib.dates import date2num
from matplotlib.dates import MonthLocator, WeekdayLocator, DayLocator
from matplotlib.dates import DateFormatter, MONDAY, DAILY
import datetime

TILT_ELEV   = 80  # Elevation for the tilt

def sin(a) :     return math.sin(a)    
def cos(a) :     return math.cos(a)    
def atan2(a,b) : return math.atan2(a,b)    
pi = math.pi

def datadir() :
    firstChoice = '/array/rt/tiltData'
    secondChoice = '/tmp'
    try : 
        os.stat(firstChoice)
    except Exception :
        return secondChoice
    return firstChoice
        
class Tilter(Carma) :
    """An object that has antenna specific info for a tilt.
    Parameters:
     ant: an antenna number"""
    def __init__(self, antnum, direction, samples, warmup, virtual=False):
        """Create a tilting antenna\n param: ant#
        Parameters:
         antnum: carma antenna number
         direction: 0=auto, positive=increasing az, negative=decreasing az
         samples: number of samples to take
         warmup: extra motion to eliminate hysteresis, in degrees
         virtual: used when a 6m has a second set of tiltmeters.
             inhibits setting zeros, etc. Default = False"""
        Carma.__init__(self, antnum)
        self.online       = True
        self.az           = 0
        self.el           = 45
        self.virtual      = virtual
        self.setSpeed()
        self.setLimits()
        self.getPosition()
        self.samples      = samples
        self.interval     = 360.0/samples
        self.warmup       = warmup
        self.pointCount   = -1
        self.numPoints    = samples
        totalRange = (1.0 - 1.0/samples)*360 + warmup
        centerAz = 0.5*(self.azlimPlus + self.azlimMinus)
        # Set direction and starting azimuth
        if direction == 0 :
            if self.az > centerAz : self.dirPlus = False
            else :                  self.dirPlus = True
        else :                      self.dirPlus = (direction >= 0)
        if self.dirPlus :           self.az0 = centerAz - 0.5*totalRange
        else:                       self.az0 = centerAz + 0.5*totalRange
        self.aftiltSample = list()
        self.lrtiltSample = list()
        self.azSample     = list()
        self.results      = list()
        self.threadsPerInch = 4  # For leg adjustments
        self.northLeg = 0        # Amount to turn to make level, degrees
        self.southLeg = 0
    def moveToNext(self) :
        "Return timeout estimate"
        az = self.az0
        if (self.pointCount >= 0) :
            inc = self.warmup + self.pointCount*self.interval
            if   self.dirPlus : az += inc
            else :              az -= inc
        self.pointCount += 1    
        if not self.online: return 0    
        tmo = self.computeTimeout(az) 
        if not self.virtual :
            move(az, TILT_ELEV, self.carmaAntnum_, waiton=NONE)
        st = "move(%.1f, %.1f, %d)" %(az, TILT_ELEV, self.carmaAntnum_)
        st += "   tmo=%.0f" %tmo
        #print st
        return tmo   
    def carmaAntname(self) :
        if self.virtual : return "V%d" %self.carmaAntnum_        
        return "C%d" %self.carmaAntnum_        
    def is10m(self) :
        return self.anttype_ == 'ovro'
    def is6m(self) :
        return self.anttype_ == 'bima'
    def isSza(self) :
        return self.anttype_ == 'sza'
    def getPosition(self) :
        if not self.online : return
        prefix = self.antname_ + ".antennacommon.drive.track."
        try :
            self.az = queryDouble(prefix + "actualazimuth",   4)
            self.el = queryDouble(prefix + "actualelevation", 4)
        except: 
            self.setOfflineQuery()
            self.az = 0
            self.el = 45
        return self.az
    def computeTimeout(self, az, el=TILT_ELEV) :
        FUDGE  = 1.30 # Ants often move a little slower than nominal
        ACCEL  = 6.5  # Acceleration/deceleration time, total, in seconds
        SETTLE = 5.5   # Drive settling time after the slew in seconds
        self.getPosition()
        daz   = abs(az-self.az)
        delev = abs(el-self.el)
        azTO = abs(FUDGE*60*(daz  )/self.azspeed) + ACCEL + SETTLE
        elTO = abs(FUDGE*60*(delev)/self.elspeed) + ACCEL + SETTLE
        #print self.getName() + ":", int(azTO), int(elTO)
        return max(azTO, elTO)
    def setSpeed(self) :
        "Speeds should be gotten from monsys once they are in place"
        if self.is10m() : 
            self.azspeed = 60
            self.elspeed = 30
        elif self.isSza() :
            self.azspeed = 58
            self.elspeed = 58
        else : 
            # 6.5m speeds      
            self.azspeed = 120
            self.elspeed = 90
    def setLimits(self) :
        mp = self.getName()+".AntennaCommon.Drive.Limit."
        self.azlimPlus  = queryDouble(mp+"azHighSwLimitVal")
        self.azlimMinus = queryDouble(mp+"azLowSwLimitVal")
         
    def takeSample(self) :
        ant = self.antname_
        aftilt = None
        lrtilt = None
        try :
            if self.is10m() :
                aftilt = queryDouble(ant + ".Tiltmeter.afTilt")
                lrtilt = queryDouble(ant + ".Tiltmeter.lrTilt")
            elif self.isSza() :
                # The SZA antenna report the tilts with zeros subtracted!!
                controlPrefix  = "Control.Antenna%d" %(self.carmaAntnum_)
                af0    = queryDouble(controlPrefix+".aftForwardTiltZero")
                aftilt = queryDouble(ant + ".Tiltmeter.afTilt") * 0.001+af0;
                lr0    = queryDouble(controlPrefix+".LeftRightTiltZero") 
                lrtilt = queryDouble(ant + ".Tiltmeter.lrTilt") * 0.001+lr0;
            else :
                container = ant + ".BimaSpecific.Drive.Point."
                if self.virtual :
                    aftilt = queryDouble(ant + ".Tiltmeter.afTilt")
                    lrtilt = queryDouble(ant + ".Tiltmeter.lrTilt")
                else :
                    aftilt = queryDouble(container + "tilt1")
                    if self.carmaAntnum_ == 15 :
                        lrtilt = queryDouble(container + "tilt2")
                    else :
                        lrtilt = None    
        except :
            self.setOfflineQuery()  
        ang =  self.getPosition()   
        fake = False  # Control adding in fake data
        if fake :
            print "***WARNING: Fake data being generated!!**"
            AFDC  = 1.23
            AFSIN = 3.4
            AFCOS = 6.2
            aftilt += AFDC + AFSIN*sin(ang*pi/180) + AFCOS*cos(ang*pi/180)             
            LRDC  = -4.0
            LRSIN = 20.0
            LRCOS = 11.0
            lrtilt += LRDC + LRSIN*sin(ang*pi/180) + LRCOS*cos(ang*pi/180)             
        self.aftiltSample.append(aftilt)       
        self.lrtiltSample.append(lrtilt)
        self.azSample.append(ang)
    def fstr(self, f, precision=3) :
        if f == None: return "   None"
        else : 
            fmt = " %" + "6.%df" %precision
            return fmt %f    
    def samplesToString(self) :
        # Header line
        takeDataDirection = "+"
        if self.dirPlus == False: takeDataDirection = "-"
        st = "Antenna:%s time:%d" %(self.carmaAntname(), time.time())
        st += " samples:%d atime:%s" %(self.samples, time.asctime())
        st += " dir:%s\n" %takeDataDirection
        # And a second header line for column headers
        st += "   samp    az     af     lr\n"
        af = self.results[0]
        lr = self.results[1]
        for i in range(len(self.azSample)) :
            st += "     %2d " %(i+1)
            st += " %5.1f" %self.azSample[i]
            st += self.fstr(self.aftiltSample[i])
            st += self.fstr(self.lrtiltSample[i])
            st += "\n"
        st += " %6s    0.0%s%s\n" \
                    %("offset", self.fstr(af[0]), self.fstr(lr[0]))  
        st += " %6s    0.0%s%s\n" \
                    %("mag",    self.fstr(af[1]), self.fstr(lr[1]))  
        st += " %6s    0.0%s%s\n" \
                    %("dir",    self.fstr(af[2],1), self.fstr(lr[2],1))  
        st += " %6s    0.0%s%s\n" \
                    %("resids", self.fstr(af[3]), self.fstr(lr[3]))  
        return st 
    def writeSamplesToFile(self, dir='/tmp', fname='tiltData.dat') :
        fname = dir + "/" + fname
        try :
            fd = open(fname,'a')
            fd.write(self.samplesToString())
            fd.close()
        except Exception, ex :
            st = "Unable to write sample file:%s" %fname
            print st, str(ex)    
    def norm360(self, phase) :
        if phase > 360:
            phase -= 360
            return self.norm360(phase)
        elif phase < 0 :
            phase += 360
            return self.norm360(phase)
        return phase                       
    def solve(self, tilt, isAF) :
        dc      = 0
        cosComp = 0
        sinComp = 0
        nsamp   = len(self.azSample)
        if tilt == None or tilt[0] == None: return [0,0,0,0,0,0]
        for i in range(nsamp) :
            ang = pi/180*self.azSample[i]
            cosComp += cos(ang)*tilt[i]
            sinComp += sin(ang)*tilt[i]
        cosComp /= 0.5*nsamp
        sinComp /= 0.5*nsamp    
        for i in range(nsamp) :
            ang = self.azSample[i]*pi/180
            dc += tilt[i] - cosComp*cos(ang) - sinComp*sin(ang)
        dc /= nsamp 
        errsquared = 0
        for i in range(nsamp) :
            ang = self.azSample[i]*pi/180
            err = tilt[i] - dc - cosComp*cos(ang) - sinComp*sin(ang)
            errsquared += err*err
        residuals = math.sqrt(errsquared/nsamp)
        phaserad = atan2(sinComp, cosComp)
        phase    = phaserad*180/pi
        if abs(90 - abs(phase)) < 0.001 :
            mag = cosComp/cos(phaserad)
        else :   
            mag = sinComp/sin(phaserad)
        # Add 90d to LR of 10m style tiltmeters    
        if not isAF and (self.is10m() or self.isSza() or self.virtual) : 
            phase += 90
        # Sub 90d from all regular 6m tiltmeters
        if self.is6m() and not self.virtual : phase -= 90
        phase = self.norm360(phase)
        # Leveling adjustment only computed from AF tiltmeter
        if isAF :
            #Compute how much legs are raised
            north = 0.024*cosComp - 0.014*sinComp # Inches  
            south = 0.024*cosComp + 0.014*sinComp # Inches  
            # Negative sign because you need to remove the raise 
            self.northLeg = -360*north*self.threadsPerInch  
            self.southLeg = -360*south*self.threadsPerInch  
        return [dc, mag, phase, residuals, sinComp, cosComp]   
    def resultsString(self, results) : 
        DETAILS = False   
        nsamp   = len(self.azSample)
        st  = "zero=%5.2f"        %results[0]
        if nsamp <= 1: return st
        st += " mag=%4.2f"        %results[1] 
        st += " direc=%5.1f"      %results[2]
        st += " resids=%.3f"      %results[3]
        if DETAILS :
            st += " sin=%5.2f"    %results[4] 
            st += " cos=%5.2f"    %results[5]
        return st
    def writeResultsToFile(self, dir = '/tmp', name='tiltResults.dat') : 
        fname = dir + '/' + name
        DETAILS = True
        num = self.carmaAntnum_ 
        virt = self.virtual
        try :
            fd = open(fname,'a')
            takeDataDirection = "+"
            if self.dirPlus == False: takeDataDirection = "-"
            # Write a header row
            st = "Antenna:%s time:%d %s" \
                %(self.carmaAntname(), time.time(), time.asctime())
            st += " " + takeDataDirection + "\n"
            fd.write(st)
            # And another header (column names)
            st = "  tilt  zero  mag direction residuals"
            if DETAILS : st += "   sin   cos"
            st += "\n"
            fd.write(st)
            for i in range(2) :
                r = self.results[i]
                tiltname = "null"
                if self.is10m() or self.isSza() or virt :
                    if i == 0: tiltname = "AF"
                    else     : tiltname = "LR"
                else : # real 6m
                    if i == 0: tiltname = "Tilt1"
                    else     : tiltname = "Tilt2"
                st = " %5s " %tiltname        
                st += "%5.2f %4.2f    %5.1f    %.3f" %(r[0],r[1],r[2],r[3])
                if DETAILS : 
                    st += "   %5.2f %5.2f" %(r[4],r[5])
                st += '\n'    
                writeIt = self.is10m() or self.isSza() or (i == 0) or (num == 15) or virt                     
                if writeIt : fd.write(st)
            fd.close()
        except Exception, ex :
            st = "Unable to write results file:%s\n%s" %(fname, ex)
            print st 
             
    def createResult(self) :
        retval = list()
        LOUD = True
        LOG  = True
        PREFIX = "Tilt result: "
        ant = self.carmaAntname()
        #print "Tilt finished for ", ant
        if False: print self.samplesToString() 
        result = self.solve(self.aftiltSample, True)
        self.results.append(result)
        if self.is10m() or self.isSza() or self.virtual : label = " AF    "
        else :                            label = " Tilt1 "
        st = "%s%-3s%s%s" %(PREFIX,ant,label,self.resultsString(result))
        if LOUD: print st
        if LOG: log(st)
        af0 = result[0]
        result = self.solve(self.lrtiltSample, False)
        self.results.append(result)
        if self.is10m() or self.isSza() or self.virtual : label = " LR    "
        else :                            label = " Tilt2 "
        st = "%s%-3s%s%s" %(PREFIX,ant,label,self.resultsString(result))
        if LOUD: print st
        if LOG:  log(st)
        lr0 = result[0]
        # Set new tiltzeros in antenna
        tiltzeros(af0, lr0, self.carmaAntnum_)
        st = "tiltzeros(%5.2f, %5.2f, %d)" %(af0, lr0, self.carmaAntnum_)
        #print st
    def printLegAdjustment(self) :
        if self.is6m() and not self.virtual :
            ant = self.carmaAntname()
            dn = "CW "
            ds = "CW "
            if self.northLeg < 0: dn = "CCW"
            if self.southLeg < 0: ds = "CCW"
            nl = abs(self.northLeg)
            sl = abs(self.southLeg)
            st = " %-3s  North leg: %3.0fdeg %s   South leg: %3.0fdeg %s" \
                      %(ant, nl, dn, sl, ds)
            print st   
    def printTiltOffsetCommand(self) : 
        if self.is6m() : return        
        st = " tiltzeros(%.3f, %.3f, %d)" \
                %(self.results[0][0], self.results[1][0], self.carmaAntnum_) 
        print st            
    def setOfflineQuery(self) : 
        self.online = False        
        st  = "***Query failed on " + self.carmaAntname() 
        st += ", removing from tilt***"
        print st    
    def setOfflineTimeout(self) : 
        self.online = False        
        st  = "TILT: ***Timeout on move command on " + self.carmaAntname() 
        st += ", removing from tilt***"
        commandlog(st)
        print st    
    def setOfflineMoveFail(self) : 
        self.online = False        
        st  = "TILT: ***Move command on " + self.carmaAntname() 
        st += " showed failure (maybe DISABLED, LOCAL, etc)" 
        st += ", removing from tilt***"
        commandlog(st)
        print st    
    def dump(self) :
        st = self.carmaAntname() + " "
        if (self.online) : st += "ONLINE"
        else :             st += "OFFLINE"
        st += " azRate=%.1f" %self.azspeed 
        st += " az=%.1f" %self.az 
        st += " dirPlus=%s" %self.dirPlus 
        return st       
    
def removeOffline(tilters) :
    """Take a dictionary of tilters and return a new 
    dictionary with the offline antennas removed."""
    online = dict() # For output
    for k in tilters:
        t = tilters[k]
        if t.online : online[k] = t
    return online
    
def getAntennas(tilters) :
    """Take a dictionary of tilters and return the list of antennas
    that they represent."""
    antlist = list() # For output
    for k in tilters:
        #print k, type(k)
        n = int(k[1:])
        alreadyInList = False
        for  a in antlist:
            if a == n: alreadyInList = True
        if not alreadyInList: antlist.append(n)
    return antlist
    
def removeNotready(tilters, antlist) :
    """Take a dictionaly of Tilter's or a list of 
    ready antenna numbers returned from a wait command and
    set the notready antennas to offline and return a new
    dictionary with just the online antennas.
    Also remove those that are ready but not successful."""
    online = dict() # For output
    for k in tilters:
        t = tilters[k]
        ready = False
        a = int(k[1:])
        if a in antlist : 
            ready = True
            mp = antCommonDrivePrefix(a) + "driveSeqNumSuccess"
            try:
                success = queryBool(mp, retries=2)
            except:
                success = False
                isBima = a > 6 and a < 16
                # The 6m ants don't implement this MP so allow them to fail
                if isBima:
                    m = "Ignoring missing driveSeqNumSuccess MP "
                    m += "for C%a; not (yet) implemented in BIMA ants"
                    success = True
        #print antlist, a, n, k, ready
        if not ready : 
            t.setOfflineTimeout()
        elif not success: 
            t.setOfflineMoveFail()
        if t.online : online[k] = t
    return online

def tilt(ants=0, direction=0, samples=12, delay=6.0, doplot=True,
         ignoreWind=False, showLegAdjustments=True) :
    """Measures the tilt and tilt zero for a set of antennas.
    The tilt zeros are set in the system at the conclusion of the command.
    The magnitude, direction and residuals are printed to the screen and
    the sample values and fit are plotted.
    After the first plot in a python session the python script will
    pause until the plot is interactively closed by the user.
    Subsequent plots do not halt the script. The behavior of the initial
    plot will cause scripts to hang if the tilt command is embedded within
    another script. Use the doplot parameter to inhibit plotting or do 
    an interactive plot before starting the script.
    A tilt containing only 3.5m antennas will automatically skip the check
    of the wind speed as the tilt measurement for these antennas is not
    significantly affected by the wind.
    Parameters:
     ants: a list of antenna numbers or a single antenna number.
           zero, the default, gives all antennas in the subarray.
     direction: any positive number will give increasing az, 
                a negative number decreasing az, and zero (the default) will 
                automatically choose the quickest. The tilt is always covers
                an azimuth range of approximately 180 degrees centered on the
                middle of the full azimuth range for the specific antenna.
     samples: number of samples, default=12.
       When only two samples are taken only the tilt offsets are measured,
       not the tilts, but it is much faster
     delay: time delay between acquisition of last antenna and sampling of
            tiltmeters; default is 6 seconds
     doplot: controls plotting, default=True. Set this parameter to false
       to avoid hanging unattended scripremovits. 
     ignoreWind: if the average wind speed is > 5.0 mph or the peak wind speed
                 is > 10.0 mps the tilt is not done unless ignoreWind=True.
                 Default = False. The wind limits are checked at each
                 measurement position during the tilt, although the limits
                 are increased by 1.5 mph. Therefore an increase in the 
                 wind speed can cause the tilt to fail before it completes.
                 A tilt containing only 3.5m antennas automatically ignores
                 the wind speed.
     showLegAdjustments: print out the 6m leg adjustments; default=True 
     See also tiltDataSummary, tiltSamplePlot,
             tiltZeroPlot, tiltMagPlot, tiltDirPlot, tiltResPlot"""
    # to eliminate hysteresis the rotation needs to start early
    WARMUP = 20    # Degrees
    delay = int(delay)
    
    def allLittle(antlist) :
        for a in antlist:
            if a < 16: return False
        return True
    def checkWind(firstCheck=False):
        """Returns true if the wind is OK for a tilt, False if not"""
        AVEWINDSPEED_LIMIT  =  5.0  # mph
        PEAKWINDSPEED_LIMIT = 10.0  # mph
        WINDSPEED_INCREMENT =  1.5
        
        if firstCheck: 
            avewindspeed_limit  =  AVEWINDSPEED_LIMIT
            peakwindspeed_limit =  PEAKWINDSPEED_LIMIT
        else:
            avewindspeed_limit  =  AVEWINDSPEED_LIMIT  + WINDSPEED_INCREMENT
            peakwindspeed_limit =  PEAKWINDSPEED_LIMIT + WINDSPEED_INCREMENT
        
        try:
            aveWind = queryDouble("Weather.averageWindSpeed", retries=3)
        except:
            m = "Cannot get averageWindSpeed monitor point; aborting"
            raise Exception, m
        try:
            peakWind = queryDouble("Weather.peakWindSpeed", retries=3)
        except:
            raise Exception, "Cannot get peakWindSpeed monitor point; aborting"
        r = True
        if aveWind > avewindspeed_limit:
            m = "Average wind speed (%.1f mph) " %aveWind
            m += "exceeds limit of %.1f mph"     %AVEWINDSPEED_LIMIT
            print m
            r = False 
        if peakWind > peakwindspeed_limit:
            m = "Peak wind speed (%.1f mph) " %peakWind
            m += "exceeds limit of %.1f mph"  %PEAKWINDSPEED_LIMIT
            print m
            r = False 
        return r
        
    if ants == 0: ants = currentAntennaNumbers()
    antlist = helper.makeList(ants)
    if allLittle(antlist):
        m = "TILT: Ignoring wind limits because tilt only has 3.5m antennas"        
        commandlog(m)
        print(m)
    elif ignoreWind:
        m = "TILT: Ignoring wind limits"        
        commandlog(m)
        print(m)
    else:
        if not checkWind(firstCheck=True):
            m = "TILT: Skipping tilt because wind is too high" 
            commandlog(m)
            print(m)
            return 1       
    
    if samples < 2:
        raise Exception, "Number of samples must be at least two!"
    try:                 
        alarmIntegdisable()
    except Exception, ex: 
        print "Unable to disable integration alarm, proceeding anyway..." 
          
    tilter = dict()
    oldTrackTolerance = dict()
    trackToleranceMP = ".AntennaCommon.Drive.Track.trackTolerance"
    for a in antlist :
        name = 'C' + str(a)
        tilter[name] = Tilter(antnum=a, direction=direction, 
                              samples=samples, warmup=WARMUP)
        t = tilter[name]
        #print t.dump()
        # Antenna 15 has a 2nd ovro style tiltmeter so we make it a virtual ant
        # Except the hw has failed and is not longer maintained, so we disable
        # the feature (but don't remove it in case we want it in the future).
        enableVirt = False
        if enableVirt and (a == 15) :
            name = 'V' + str(a)
            tilter[name] = Tilter(a, direction, samples, WARMUP, True)
            t = tilter[name]
            #print t.dump()
        # Set tracking threshold to 3.0 arcsec for all ants in the tilt
        trackToleranceMPname = device.Carma(a).getName()+trackToleranceMP
        try :
            oldTrackTolerance[a] = queryDouble(trackToleranceMPname)
        except:
            print "Couldnt get MP value for %s" %trackToleranceMPname
        trackThresholdOptical(3.0, antlist)
    tilter = removeOffline(tilter)
    # Include extra warmup point 
    r = range(0, samples + 1) 
    for i in r :
        tilter = removeOffline(tilter)
        antlist = getAntennas(tilter) 
        if len(antlist) == 0:
            m = "TILT: No antennas left for tilt - EXITING"
            commandlog(m)
            print(m)
            return 1
        tmomax = 0
        for k in tilter:
            t      = tilter[k]
            tmo    = t.moveToNext()
            tmomax = int(max(tmomax, tmo))
            #print t.dump(), tmo, tmomax, k
        antlist = getAntennas(tilter) 
        p = False # Control local debug printing
        if p: print "Antenna list for wait:", antlist, "tmo=", tmomax   
        
        ready = wait(TRACK, antlist, tmomax, waiton=ALL).ready  

        if p: print "Antenna list returned from wait, the ready list:", ready  
        tilter = removeNotready(tilter, ready)
        if len(getAntennas(tilter)) == 0:
            m = "TILT: No antennas left for tilt - EXITING"
            commandlog(m)
            print(m)
            return 1
        if i != 0 : # No sample for warmup position...
            if delay >= 1 :
                sys.stdout.write(" Waiting for tiltmeters to stabilize")
                sys.stdout.flush()
                for d in range(delay) :
                    sleep(1)
                    sys.stdout.write(".")
                    sys.stdout.flush()
            for k in tilter:
                t      = tilter[k]
                t.takeSample()
            st = "  Sample %d/%d taken" %(i, len(r)-1)
            print st
            
        # Inhibit alarm
        resetTimeSinceLastIntegration()    
    if allLittle(antlist) or ignoreWind:
        pass # Don't check the wind
    elif (not checkWind()):
            m = "TILT: Not writing tilt results because wind became too high"
            m += "during the procedure (it was OK at the start)"
            commandlog(m)
            print m 
            return 1       
    tilter = removeOffline(tilter)
    print "Writing data and results to: " + datadir()
    antlist = makeAntnameList(tilter)
    # print "Ordered antenna names: ", antlist
    numSza = 0
    num6m  = 0
    num10m = 0
    for a in antlist:
        t      = tilter[a]
        if t.isSza() : numSza += 1
        if t.is6m()  : num6m  += 1
        if t.is10m() : num10m += 1
        resultStrings = t.createResult()
        t.writeSamplesToFile(dir=datadir())
        t.writeResultsToFile(dir=datadir())
    m = "TILT:(debug) numAnts=%d/%d/%d" %(num10m,num6m,numSza)
    #commandlog(m)
    if num6m and showLegAdjustments:    
        print "6m leg adjustments: directions are as viewed from above"    
        for a in antlist:
            t      = tilter[a]
            t.printLegAdjustment()
        m = "TILT:(debug) printed out 6m leg adjustments"
        commandlog(m)
    
    # Mark completion times
    # The number of antennas required to define completion is somewhat arbitrary
    minbig = 10
    minSza = 6
    # print num10m, num6m, numSza
    numbig = num6m + num10m
    m = "TILT:(debug) numAnts=%d/%d/%d, numbig=%d" %(num10m,num6m,numSza,numbig)
    commandlog(m)
    if numbig >= minbig:
        m = "TILT: Marking Tilt Sci1 completion time"
        commandlog(m)
        print m
        rtTILT1completed() 
    elif numbig > 0:
        m  = "TILT: Not marking Tilt Sci1 complete "
        m += "because at least %d " %minbig
        m += "6m/10m antennas are required "
        m += "and only %d completed the tilt" %numbig 
        commandlog(m)
        print m     
    if numSza >= minSza:
        m = "TILT: Marking Tilt Sci2 completion time"
        commandlog(m)
        print m     
        rtTILT2completed()        
    elif numSza > 0:
        m  = "TILT: Not marking Tilt Sci2 complete "
        m += "because at least %d " %minSza
        m += "3.5m antennas are required "
        m += "and only %d completed the tilt"  %numSza
        commandlog(m)
        print m 
             
    m = "TILT: Tilt has completed"
    commandlog(m)
    print m     
            
    # Restore tracking tolerances
    for a in oldTrackTolerance.keys():
        trackThresholdOptical(oldTrackTolerance[a], ants=a)        
            
    # Plotting        
    numplots = len(tilter.keys())
    plotCount = 1    
    if doplot : plt.close()    
    for a in antlist:
        t      = tilter[a]
        if doplot : 
            rawSamplePlot(a, time.time(), t.azSample, t.aftiltSample, 
                t.lrtiltSample, t.results[0], t.results[1], numplots, plotCount)
        plotCount += 1
    if doplot : plt.show()    
    alarmIntegrestore()
    return 1
    
# ======================================================================
# ======================== Tilt Data Manipulation ======================    
# ======================================================================
       
def readSampleFile(start=None, stop=None, ants=0, fname = "tiltData.dat") :
    """Internal routine to read the data file and return a list of structs.
    Parameters:
      start: start time (see tiltDataPlot for format)
      stop: stop time (see tiltDataPlot for format)
      ants: a list of antenna numbers or a single antenna number.
            Zero, the default, gives all antennas in the data file.
      fname: filename, default=tiltData.dat"""
    fname = datadir() + "/" + fname
    now = helpers.string2ctime("0d")
    if start == None: t1 = 0
    else:             t1 = helpers.string2ctime(start)
    if stop == None:  t2 = now
    else:             t2 = helpers.string2ctime(stop)
    # print now, t1, t2
    if ants != 0 and type(ants) != list: ants = [ants]
    try:
        fd = open(fname,'r')
    except Exception, ex :
        st = "Unable to open data file:%s" %fname
        print st, str(ex) 
        return   
    try:
        data = fd.readlines()
        fd.close()
    except Exception, ex :
        st = "Unable to read data file:%s" %fname
        print st, str(ex) 
    if False: 
        for l in data : sys.stdout.write(l) 
    output = list()    
    c = 0    
    for line in data :
        # Strip off carriage return at the end of the line
        l = line[:len(line)-1]
        w = l.split(':') 
        if w[0] == 'Antenna' :
            h = w[1].split()
            ant   = h[0]
            antNo = int(ant[1:])
            secs  = int((w[2].split())[0])
            samps = int((w[3].split())[0])
            atime = l.split("atime:")[1]
            atime = atime.split("dir:")[0]
            x   = l.split("dir:")
            if len(x) > 1: dir = x[1]
            else:          dir = '?'
            #print "ant line: ", ant, secs, samps, "atime:", atime, "dir:", dir
            az = list()
            af = list()
            lr = list()
            startline = c + 2
            for i in range(startline,startline+samps) :
                d = data[i].split()
                samp = d[1]
                if samp == 'None' : az = None
                else :              az.append(float(samp))
                samp = d[2]
                if samp == 'None' : af = None
                else :              af.append(samp)
                samp = d[3]
                if samp == 'None' : lr = None
                else :              lr.append(samp)
            r = startline + samps 
            d = data[r].split()
            offsets = [float(d[2]), float(d[3])]   
            d = data[r+1].split()
            mags = [float(d[2]), float(d[3])]
            d = data[r+2].split()
            afdir = float(d[2])
            lrdir = float(d[3])
            dirs = [float(d[2]), float(d[3])]  
            antNo =  int(ant[1:])
            if antNo >= 16 and secs < 1361993300 :
                # Fix bad SZA L/R direction
                lrdir += 90
                if lrdir >= 360: lrdir -= 360
            dirs = [afdir, lrdir]
            #print secs, ant, antNo, dirs  
            d = data[r+3].split()
            resids = [float(d[2]), float(d[3])]  
            results = [offsets, mags, dirs, resids]
            timeCheck = (secs >= t1) and (secs <= t2)
            antCheck = (ants==0) or (ants.count(antNo) > 0)
            if timeCheck and antCheck:
                newrec = [[ant,secs,samps,atime],results,az,af,lr]
                # print newrec
                output.append(newrec)
        c += 1                          
    return output
    
def tiltSamplePlot(recs) :
    """Plot raw tilt sample values and fit. Get record list from
    tiltDataSummary command.
    Parameters:
     recs: a record number or list of record numbers to plot.
       A record is a single tilt on an antenna for one tilt meter axis
    See also tilt, tiltDataSummary, 
             tiltZeroPlot, tiltMagPlot, tiltDirPlot, tiltResPlot"""
    d = readSampleFile(None, None)
    #print "file has %d records" %len(d) 
    recs = helper.makeList(recs)
    totrecs = len(recs)
    recCount = 1
    plt.close()
    for rec in recs :
        r = d[rec-1]
        res = r[1]
        fit1 = [res[0][0], res[1][0], res[2][0]]
        fit2 = [res[0][1], res[1][1], res[2][1]]
        rawSamplePlot(r[0][0], r[0][1], r[2], r[3], r[4], fit1, fit2, 
                totrecs, recCount)
        recCount += 1
    plt.show()    
def antcmp(a,b) :
    anum = int(a[1:])        
    bnum = int(b[1:]) 
    return cmp(anum,bnum)       
def makeAntnameList(data) :
    ants = dict()
    for r in data :
        if type(data) == dict :
            name = r
        else :
            name = r[0][0]
        #print name
        ants[name] = True
    a = ants.keys() 
    a.sort(antcmp) 
    return a  
                        
def tiltDataPlot(parameterSelection = "Zeros", start=None, stop=None,
        ants=0, fname="tiltData.dat") :
    """Internal routine to plot various tilt parameters;
    zeros, magnitude direction, or residuals.
    Parameters:
      parameterSelection: case insensitive match to the first three 
        characters of 'zeros', 'magnitude', 'direction', 'residuals'
      start: starttime; default=None gives oldest
      stop:  stoptime; default=None gives most recent
      ants: a list of antenna numbers or a single antenna number.
            Zero, the default, gives all antennas in the data file.
      fname: data filename, default='tiltData.dat' """
    p = parameterSelection.lower()[:3]
    zerSelect = False
    magSelect = False
    dirSelect = False
    resSelect = False
    dataIdx   = 0
    if   p == 'zer' : zerSelect = True; dataIdx = 0
    elif p == 'mag' : magSelect = True; dataIdx = 1
    elif p == 'dir' : dirSelect = True; dataIdx = 2
    elif p == 'res' : resSelect = True; dataIdx = 3
    
    d = readSampleFile(start, stop, ants, fname)
    #print "file has %d records" %len(d) 
    ants = makeAntnameList(d)
    months     = MonthLocator(range(1,13), bymonthday=1) # every month
    monthsFmt  = DateFormatter("%b '%y")
    mondays    = WeekdayLocator(MONDAY) # every monday
    daily      = DayLocator() # every day
    every2days = DayLocator(interval=2) # every other day
    every4days = DayLocator(interval=4)
    every5days = DayLocator(interval=5)
    daymonFmt  = DateFormatter("%d%b")
    plt.close()
    fig = plt.figure()
    plt.ioff()
    numrows, numcols = calcSubplots(len(ants))
    #print("Ants:%d rows:%d cols:%d" %(len(ants), numrows, numcols))
    plotCount = 1
    for a in ants :
        t  = list()
        d1 = list()
        d2 = list()
        ts = list()
        for r in d:
            name = r[0][0]
            if name != a : continue
            timestamp = r[0][1]
            ts.append(timestamp)
            dt = datetime.datetime.fromtimestamp(timestamp)
            t.append(date2num(dt))
            if r[3] != None : d1.append(r[1][dataIdx][0])
            if r[4] != None : d2.append(r[1][dataIdx][1])
        numdays = (max(ts)-min(ts))/86400.0   
        sp = fig.add_subplot(numrows, numcols, plotCount)
        #print a, numrows, numcols, plotCount, "days:", numdays
        plotCount += 1
        sp.plot(t, d1) 
        if len(d2) > 0: sp.plot(t, d2)   
        tmin = int(min(t)) - 0.1
        tmax = int(max(t)) + 1.0 + 0.1
        numdays = tmax-tmin
        handle = plt.gca()
        handle.set_xlim([tmin, tmax]) 
        plt.title(a)
        if zerSelect : plt.ylabel('Tiltmeter zero(amin)')    
        if magSelect : plt.ylabel('Tilt magnitude(amin)')    
        if dirSelect : plt.ylabel('Tilt direction(deg)')    
        if resSelect : plt.ylabel('Tilt residuals(amin)')    
        if numdays < 5 :   
            sp.xaxis.set_major_locator(daily)
            sp.xaxis.set_major_formatter(daymonFmt)
        elif numdays < 9 :   
            sp.xaxis.set_major_locator(every2days)
            sp.xaxis.set_minor_locator(daily)
            sp.xaxis.set_major_formatter(daymonFmt)
        elif numdays < 17 :   
            sp.xaxis.set_major_locator(every4days)
            sp.xaxis.set_minor_locator(daily)
            sp.xaxis.set_major_formatter(daymonFmt)
        elif numdays < 30 :  
            sp.xaxis.set_major_locator(every5days)
            sp.xaxis.set_minor_locator(daily)
            sp.xaxis.set_major_formatter(daymonFmt)
        else: 
            sp.xaxis.set_major_locator(months)
            sp.xaxis.set_major_formatter(monthsFmt)
            sp.autoscale_view()
        sp.grid(True)
    plt.show()

_tweakHelp = "\nThe format of the start and stop times "
_tweakHelp += "is given below:\n%s" %(helper._stringDateFormat)
tiltDataPlot.__doc__ += _tweakHelp
                        
def tiltZeroPlot(start=None, stop=None, ants=0, fname="tiltData.dat") :
    """Plot tilt zeros (offsets) vs time.
    Parameters:
      start: Start time, default=None => earliest recorded data
      stop:  Stop time, default=None => latest recorded data
      ants: a list of antenna numbers or a single antenna number.
            Zero, the default, gives all antennas in the data file.
      fname: data filename, default='tiltData.dat'
    See also tilt, tiltDataSummary, tiltSamplePlot, tiltMagPlot, tiltDirPlot, 
    tiltResPlot"""
    tiltDataPlot("zeros", start, stop, ants, fname)
                        
def tiltMagPlot(start=None, stop=None, ants=0, fname="tiltData.dat") :
    """Plot tilt magnitude vs time
    Parameters:
      start: Start time, default=None => earliest recorded data
      stop:  Stop time, default=None => latest recorded data
      ants: a list of antenna numbers or a single antenna number.
            Zero, the default, gives all antennas in the data file.
      fname: data filename, default='tiltData.dat'
    See also tilt, tiltDataSummary, tiltSamplePlot, tiltZeroPlot, tiltDirPlot, 
    tiltResPlot"""
    tiltDataPlot("magnitude", start, stop, ants, fname)
                        
def tiltDirPlot(start=None, stop=None, ants=0, fname="tiltData.dat") :
    """Plot tilt direction vs time
    Parameters:
      start: Start time, default=None => earliest recorded data
      stop:  Stop time, default=None => latest recorded data
      ants: a list of antenna numbers or a single antenna number.
            Zero, the default, gives all antennas in the data file.
      fname: data filename, default='tiltData.dat'
    See also tilt, tiltDataSummary, tiltSamplePlot, tiltZeroPlot, tiltMagPlot, 
    tiltResPlot"""
    tiltDataPlot("direction", start, stop, ants, fname)
                        
def tiltResPlot(start=None, stop=None, ants=0, fname="tiltData.dat") :
    """Plot tilt residuals vs time
    Parameters:
      start: Start time, default=None => earliest recorded data
      stop:  Stop time, default=None => latest recorded data
      ants: a list of antenna numbers or a single antenna number.
            Zero, the default, gives all antennas in the data file.
      fname: data filename, default='tiltData.dat'
    See also tilt, tiltDataSummary, tiltSamplePlot, tiltZeroPlot, tiltMagPlot, 
    tiltDirPlot"""
    tiltDataPlot("residuals", start, stop, ants, fname)
    
tiltZeroPlot.__doc__ += _tweakHelp
tiltMagPlot.__doc__  += _tweakHelp
tiltDirPlot.__doc__  += _tweakHelp
tiltResPlot.__doc__  += _tweakHelp
     
def tiltDataSummary() :
    """Print a summary of available tilt data.
    See also tilt, tiltSamplePlot, tiltZeroPlot, tiltMagPlot, tiltDirPlot, tiltResPlot"""
    d = readSampleFile()
    print "Data file has %d records" %len(d) 
    for i in range(len(d)):
        r = d[i]
        st = "Rec:%3d %-3s %s" %(i+1, r[0][0], r[0][3]) 
        print st
    
def rawSamplePlot(antname, timestamp, x, y1, y2, fit1, fit2, maxplots, index) :
    """Internal routine to plot data samples and fit for a single antenna.
    Parameters:
     antname: antenna name for plot labelling
     timestamp: in unix time seconds
     x: list of azimuths of data points
     y1: list of a/f data points
     y2: list of l/r data points
     fit1: a list containing a/f mag/dir/offset
     fit2: a list containing l/r mag/dir/offset
     maxplots: number of subplots on the page
     index:  subplot index"""
    sp = calcSubplots(maxplots)
    plt.subplot(sp[0], sp[1], index)
    virt  = antname[0] == 'V'
    is10m = int(antname[1:]) <= 6
    isSZA = int(antname[1:]) >= 16
    
    off1  = fit1[0]
    mag1  = fit1[1]
    dir1  = fit1[2]
    off2  = fit2[0]
    mag2  = fit2[1]
    dir2  = fit2[2]
    
    #print antname, virt, is10m, y2==None
    yfit1 = list()
    yfit2 = list()
    if is10m or isSZA or virt :
        # Correct for orientation of tiltmeter
        dir1 -=  90
        dir2 -= 180
    azfit = range(int(round(min(x)))-2, int(round(max(x)))+2, 4)    
    for az in azfit :
        yfit1.append(off1 + mag1*sin(pi*(az - dir1)/180.0)) 
        yfit2.append(off2 + mag2*sin(pi*(az - dir2)/180.0)) 
    plt.plot(x, y1, azfit, yfit1)
    if y2 != None and y2[0] != None:
        plt.plot(x, y2, azfit, yfit2)
    plt.title(antname)
    plt.xlabel('Azimuth(deg)')
    plt.ylabel('Tiltmeter reading(amin)')    
    #plt.show()
    
def calcSubplots(maxplots) :
    """Internal routine to calc plot layout. 
    Returns [rows, cols]"""
    if maxplots ==  1 : return [1,1]
    if maxplots <=  4 : return [2,2]
    if maxplots <=  6 : return [2,3]
    if maxplots <=  9 : return [3,3]
    if maxplots <= 12 : return [4,3]
    if maxplots <= 16 : return [4,4]
    if maxplots <= 20 : return [5,4]
    return [5,5]    
