# Scripts to test delays by sending sawtooth pattern on
# single antenna and zeros for other antennas
# usage:  
#  import testDelays as td
#  td.sawtooth()
# $Id: testDelays.py,v 1.2 2009/05/23 05:50:06 mpound Exp $
import subarrayCommands as cmd

def zeroDelays() :
   cmd.delaysOff()
   ants = cmd.currentAntennaNumbers()

   # set the delay offset value of each antenna such that 
   # it makes total delay zero
   # first do a bulk query on the current total delay.
   mpList = []
   for myAnt in ants :
      mpString = "DelayEngine.DelayData%d.totalDelay" % myAnt
      mpList.append( mpString )
   mpValues = cmd.queryMpValues( mpList )
   for i in range(len(ants)) :
      cmd.delay( -mpValues[i], ants[i] )
   
def ramp( low=-1.0, high=1.0, refant=1, interval=20.0, setup=True ) :
    if ( setup ) :
    print " *** Doing setup *** "
    cmd.delayDefaults()
    cmd.sleep(3) # allow MPs to settle down.
    zeroDelays()
    cmd.sleep(4) # allow MPs to settle down.
    delta = ( high - low )/interval
        mpString = "DelayEngine.DelayData%d.totalDelay" % refant
        offset = cmd.queryDouble( mpString, retries=10 )
        refList = [ refant ]
        cmd.s.useAdjustableDelay(True, refList)
    print "****** BEGINNING RAMP ******"
    numSteps = int(interval)+1
    for i in range(0, numSteps) :
        adjust = offset + low + delta*i
        #print "i = %d Delay = %f " % ( i, adjust )
        cmd.adjustableDelay( adjust, refant )
        cmd.sleep(0.5)

def sawtooth( low=-1.0, high=1.0, refant=1, interval=20.0 ) :
    print " ^C to quit "
    print " *** Doing setup *** "
    cmd.delayDefaults()
    cmd.sleep(3) # allow MPs to settle down.
    zeroDelays()
    cmd.sleep(4) # allow MPs to settle down.
    delta = ( high - low )/interval
    mpString = "DelayEngine.DelayData%d.totalDelay" % refant
    offset = cmd.queryDouble( mpString, retries=20 )
    refList = [ refant ]
    cmd.s.useAdjustableDelay(True, refList)
    print "****** BEGINNING RAMP ******"
    while( True ) :
    numSteps = int(interval)+1
    for i in range(0, numSteps) :
        adjust = offset + low + delta*i
        #print "i = %d Delay = %f " % ( i, adjust )
        cmd.adjustableDelay( adjust, refant )
        cmd.sleep(0.5)
    for i in range(numSteps,0,-1) :
        adjust = offset + low + delta*i
        #print "i = %d Delay = %f " % ( i, adjust )
        cmd.adjustableDelay( adjust, refant )
        cmd.sleep(0.5)
