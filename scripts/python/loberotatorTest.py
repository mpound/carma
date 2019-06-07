
# Testing code for the loberotator
#
# @author Steve Scott
# $Id: loberotatorTest.py,v 1.2 2005/07/22 16:10:40 scott Exp $
#
# $CarmaCopyright$
#
# Load with "from loberotatorTest import *"
#

import carma
import carmaIni

def delayElem(antNum, delay, mjd, discon) :
    """Construct a DelayElement structure to send to setDelay([DE])
    param: antNum
    param: delay in nanoseconds
    param: mjd  for delay
    param: disc boolean discontinuity flag"""
    d = carma.loberotator.LoberotatorControl.\
    DelayElement(antNum, delay, mjd, discon)
    return d
    
def freqElem(antNum, freq, mul, div, sign) :
    """Construct a FrequencyElement structure to send to setLOFreq([FE])
    param: antNum
    param: freq in GHz
    param: multiply factor
    param: divisor  factor
    param: sign  factor (+1 or -1)"""
    f = carma.loberotator.LoberotatorControl.\
    FrequencyElement(antNum, freq, mul, div, sign)
    return f
    
fe3 = freqElem(3, 100.0, 1, 1, 1)
de3 = delayElem(3, 10.0, 56600.0, 0)    
