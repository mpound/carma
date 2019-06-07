
# Some high level initialization for a subarray.
# This behaves like a class with static methods
#
# @author Steve Scott
# $Id: Subarray.py,v 1.8 2010/10/27 00:41:32 scott Exp $
#
# $CarmaCopyright$
#

import sys, getopt
import carma
import carmaIni

cachedSubarrayNo = 0
isSciall_ = False

def getSubarrayNo() :
    "Get subarray number from cmd line, or from cache"
    global cachedSubarrayNo, isSciall_
    if cachedSubarrayNo == 0 : 
        longOptions = ["subarrayNumber=", "init="]
        inputValues = getopt.getopt(sys.argv[1:], "", longOptions) 
        n = int(inputValues[0][0][1])
        if n == 6:
            isSciall_ = True
            cachedSubarrayNo = 1
        else : 
            cachedSubarrayNo = n
    return cachedSubarrayNo

def getSubarray():
    "Get a ref to the subarray"
    return getSubarrayRef(getSubarrayNo())

def getSubarrayRef(num):
    "Get a reference to control DO for subarray"
    try:
        return carmaIni.getObj("carma/subarrayControl%d" %num, 
            carma.control.SubarrayControl)
    except Exception, ex:
        print "Error: couldn't get subarray%d" %num + " control object"
        print ex
        return 0
        
def isSciall():
    getSubarrayNo()
    return isSciall_
