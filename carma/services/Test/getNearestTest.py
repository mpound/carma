# Timing tests for getNearest. 
import carma
import carmaIni
import subarrayCommands as sc
import time

def opttest() :
    for i in range(0,120,20):
        print "Doing OPTICAL search numReturn = %d" % i
        x=time.time()
        sc.getNearest("aumi",sourceList=["aumi"],numReturn=i,action=sc.EXCLUDE,getOptical=True)
        y=1000000*(time.time() - x)
        print "Elapsed time (microsec): %f " % y

def radtest() :
    for i in range(0,120,20):
        print "Doing RADIO search numReturn = %d" % i
        x=time.time()
        sc.getNearest("aumi",sourceList=["aumi"],numReturn=i,action=sc.EXCLUDE,fluxLimit=0.1,getOptical=False)
        y=1000000*(time.time() - x)
        print "Elapsed time (microsec): %f " % y
