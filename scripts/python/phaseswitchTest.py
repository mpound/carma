import sys
import carmaIni
import carmaHelp
from subarrayCommands import *

def test(chan=24, reps=1000) :
    """Test phase switch state.
    It usually takes 1.0 to 1.5 secs for the new state to show up in the 
    monitor system."""
    def chk(prefix, c, state):
        mp90  = "Loberotator.Channel%i.phaseSwitch90"  %(c)
        mp180 = "Loberotator.Channel%i.phaseSwitch180" %(c)
        m = prefix+"PS90/180 for chan %i was not %s" % (c, state)
        sleepStep = 0.25
        firstStep = 3
        lastStep  = 16
        sleep((firstStep-1)*sleepStep)
        qstate90  = "XX"
        qstate180 = "XX"
        for i in range(firstStep, lastStep+1) :
            sleep(sleepStep)
            qstate90  = queryString(mp90, 3) 
            qstate180 = queryString(mp180,3) 
            if qstate90 == state and qstate180 == state:
                if i == firstStep: return
                #print m+" for %.1f secs" %((i-1)*sleepStep)
                return
        if qstate90 != state:
            print prefix+"PS90 for chan %i was not %s" % (c, state)
        if qstate180 != state:
            print prefix+"PS180 for chan %i was not %s" % (c, state)
        
    for i in range(reps):
        if (i!=0) and (i%100==0) : print "Reps:", i
        prefix = "%d/%d: " %(i, reps)
        pswitchoff(chan)
        #chk(prefix, chan, "OFF")
        chk(prefix, chan, "OFF")
        pswitchon(chan)
        #chk(prefix, chan, "ON")
        chk(prefix, chan,  "ON")
