
# Make a help system to display available methods
# @file
#
# @author Steve Scott
# $Id: carmaHelp.py,v 1.11 2011/09/26 23:01:03 scott Exp $
#
# $CarmaCopyright$
#

# pprint can be handy for trouble shooting
from pprint import pprint
import types


#------------------------------------------------------------
def _pruneMethods(inDict) :
    rtnDict = inDict.copy()
    for key in rtnDict.keys():
        keep=True
        # Remove internal functions
        if key[0] == '_':                 keep=False 
        if key[0:7] == 'restore':         keep=False 
        if key == 'Ant':                  keep=False 
        if key == 'antnamecheck':         keep=False 
        if key == 'cal':                  keep=False 
        if key == 'deprecate':            keep=False 
        if key == 'done':                 keep=False 
        if key == 'getDOref':             keep=False 
        if key == 'getRootDOname':        keep=False 
        if key == 'getoutput':            keep=False 
        if key == 'getoutputerror':       keep=False 
        if key == 'helpCarma':            keep=False 
        if key == 'lout':                 keep=False 
        if key == 'makeAntString':        keep=False 
        if key == 'makeRangeFormattedAntString': keep=False
        if key == 'modifyPath':           keep=False 
        if key == 'monsysDropouts':       keep=False 
        if key == 'mythrow':              keep=False 
        if key == 'myIncrementTrial':   keep=False 
        if key == 'OBDescSequenceFromProjectSequence': keep=False
        if key == 'pprint':               keep=False 
        if key == 'printKeyboardHelp':    keep=False 
        if key == 'printError':           keep=False 
        if key == 'printHelp':            keep=False 
        if key == 'printInColor':         keep=False 
        if key == 'printInfo':            keep=False 
        if key == 'printMessage':         keep=False 
        if key == 'printWarning':         keep=False 
        if key == 'printRefHelp':         keep=False 
        if key == 'pysh':                 keep=False 
        if key == 'raiseMPexception':     keep=False 
        if key == 'raiseNFexception':     keep=False 
        if key == 'raiseRetryException':  keep=False 
        if key == 'shell':                keep=False 
        if key == 'sout':                 keep=False 
        if key == 'system':               keep=False 
        if key == 'trackMessage':         keep=False 
        #print "  " + key + "     " + str(type(v)) + "   " + str(keep)
        if not keep : del rtnDict[key]
        #print key[0], key, keep
    return rtnDict
    
def helpCarma(theDict, listOnly=False): 
    "Print the doc string for the input class or method, and all its methods"
    # The obj can be a string or an object reference; a null string 
    # or "keyboard" gives the current global methods
    d = _pruneMethods(theDict)
    # Now names and doc strings for all methods
    keys = d.keys()
    keys.sort()
    for k in keys:
        print " " + k
        doc = d[k].__doc__
        if (not listOnly) and (doc != None) : print "  -" + str(doc)
        
