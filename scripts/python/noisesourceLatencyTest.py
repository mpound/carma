from subarrayCommands import *
from time import *

NOISE_DIFF_THRESHOLD = 10.0 
MAX_WAIT_TIME = 5.0
TIME_RES = 0.01

def getNoisePower( ) :
    return queryDouble( "Sldc.QuadModContainer1.QuadMod.outputPower" )

def printNoisePower( ) :
    noisePower = getNoisePower( )
    print "Noise power: %.1f @ %.2f" % ( noisePower , time() )
    
def testNoiseSourceLatency( ) :
    "Test noise source latency"
    noiseoff()
    latency = 0.0
    T0 = time( )
    N0 = getNoisePower( )
    print "noiseon command issued @ %.2f" % T0
    noiseon()
    for frame in range( MAX_WAIT_TIME/TIME_RES ) :
        N1 = getNoisePower( )
        T1 = time( )
        ndiff = N1 - N0
        if ndiff > NOISE_DIFF_THRESHOLD:
            latency = T1 - T0
            print "Noise on latency: %.2f s." % latency 
            break
        sleep( TIME_RES )
    print "noiseoff command issued @ %.2f" % time()
    noiseoff()
    return latency
