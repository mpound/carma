#
# $Id: rxlocktest.py,v 1.3 2006/03/09 17:54:06 obs Exp $
#

import sys
import carmaIni
import carma
import Subarray
import subarrayCommands as sC
import device
import math as m
import time
import datetime
import fpformat



class RxLockTestBase(object):
    """Class used to compute tilt measurements on antennas.
         Note: this only works for BIMA antennas right now."""
    def __init__(self,rng,start,end,inc,wait) :
        self.sac = Subarray.getSubarray()
        if isinstance(self.sac, carma.control._objref_SubarrayControl) == False:
            raise Exception, "Invalid subarrayControl ref, is the subarray controller up?"
        self.set(rng,start, end, inc, wait)
        return self

    def set(self,rng,start,end,inc,wait):
        self.antRange = rng
        self.start = start
        self.end = end
        self.inc = inc
        self.wait = wait
        self.printSetup()

    def printSetup(self):
        print ""
        print "-------------Parameters---------------"
        print "    Ant Range: ",self.antRange
        print "    Freq Range: [",self.start,",",self.end,"]"
        print "   increments: ",self.inc," GHz"
        print "        delay: ",self.wait
        print "-------------Parameters---------------"
        print ""
   
    def isAntDone(self,antno):
        return self.q(self.antName(antno)+".bimaspecific.rxstatusinfo")[0:16] == "Finished setFreq"

    def rxStatus(self,antno):
        return self.q(self.antName(antno)+".bimaspecific.rxstatusinfo")

    def run(self,file):
        file = open( file, "w" )
        file.write( "# Run: "+time.asctime(time.gmtime())+"\n" )
        file.write( "# Ant range "+str(self.antRange)+"\n" )
        file.write( "# Freq range ["+str(self.start)+","+str(self.end)+"]\n" )
        file.write( "# Increments: "+str(self.inc)+"\n" )
        file.write( "# Wait: "+str(self.wait)+"\n" )
        file.write( "# time(secs) ant freq yigstatus oscstatus rxinfo\n" )
        file.flush()
        sf = int(self.start*1000)
        ef = int(self.end*1000)
        incf = int(self.inc*1000)
        for freq in range(sf,ef+1,incf):
            freq = freq/1000.
            print "Setting freq: "+str(freq)
            sys.stdout.flush()
            self.sac.freq(freq)
            time.sleep(5) # give time for freq command to go out
            print "Waiting for finished notifies from bima ants"
            print "Will timeout after "+str(self.wait)+" seconds"
            allDone = False
            chkcnt = 0
            while allDone == False :
                chkcnt = chkcnt+1
                if ( chkcnt*5 > self.wait ):
                  break
                for ant in self.antRange:
                  if ( self.isAntDone(ant) == False ):
                    allDone = False
                    break
                  else:
                    allDone = True
                print "All done: "+str(allDone)
                time.sleep(5)
            sys.stdout.flush()
            for ant in self.antRange:
                file.write( str(time.time())+" " )
                file.write( self.antName(ant)+" " )
                file.write( str(fpformat.fix(freq,3))+" " )
                file.write( self.yigState(ant)+" " )
                file.write( self.loState(ant)+" " )
                file.write( "\""+self.rxStatus(ant)+"\"\n" )
            file.flush()

    def antName(self,ant):
        " ""Returns stringified ant"""
        return device.CarmaAnt().getName(ant).capitalize()

    def yigState(self,ant):
        """Returns current yigState"""
        return self.q(self.antName(ant)+".AntennaCommon.LO.yigState")

    def loState(self,ant):
        """Returns current loState"""
        return self.q(self.antName(ant)+".antennaCommon.LO.loState") # not being filled by bima

    def q(self,query):
        return sC.queryString(query,10)

    def __doc__(self) :
        print ""
 
class RxLockTest(object):
    """Create a RxTest object for testing rx's at a range of freqs.
       defaults to incrementing freq by 100MHz and waiting 60s
       before issuing a new freq command.
       The increments and wait time can be adjusted with the set
       method.
       Also defaults to watching all antennas
         param s - Ref to sub array controller
         param freq - freq to start at."""
    def __init__(self,start,end) :
#        device.CarmaAnt().getNumAnts() always returns 23 now
#        rng = range(1,device.CarmaAnt().getNumAnts()+1,1)
        rng = range(1,16,1)
        self.rxtester = RxLockTestBase(rng,start,end,.100,60)
    def run(self,file):
        self.rxtester.run(file)
    def set(self,rng,start,end,inc,wait):
        """set method:
           param antRange - Ants to watch, expects a 'range'
           param start - starting freq in GHz
           param end - ending freq in GHz
           param inc - increments in MHz
           parama wait - wait time in seconds between freq changes"""
        self.rxtester.set(rng,start,end,inc,wait)
    def printSetup(self):
        self.rxtester.printSetup()
       
    
    

