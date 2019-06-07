
# Top level python module for interactive control of a subarray
# The following variables should be available for use in subarrayCommands
# and other scripts that import * from this file:
#   s             Reference to subarray DO
#   subarrayNo    Subarray number
#   saName        Subarray name
#
# @author Steve Scott
# $Id: subarrayControl.py,v 1.130 2012/11/28 04:12:15 friedel Exp $
#
# $CarmaCopyright$
#

import sys
import getopt

# Access for all carma remote DOs
import carmaIni
import carmaHelp
from carmaHelp import helpCarma
from device import *
# pprint can be handy for trouble shooting
from pprint import pprint
import Subarray


# Control variables that determine how things run at the end!
# Automatically get the refs
class _Options(object):
    "Simple class that encapsulates all startup options"
    getdevices        = True
    printKeyboardHelp = False
    printDeviceHelp   = False
    printHelp         = False
    def displayHelp() :
        if __name__ != '__main__' : return
        "Prints out help if this is the main module"
        if _Options.printDeviceHelp :
            printRefHelp()
        if _Options.printKeyboardHelp == 1:
            printKeyboardHelp()
        if _Options.printHelp == 1:
            printHelp()
        else : 
            print "-------------------------------------------"
            print "Type help() to get help, and Exit to exit"
            print "Subarray control method help at:"
            print "  http://www.mmarray.org/subarrayControl"
            print "-------------------------------------------"
    displayHelp = staticmethod(displayHelp)

def getSubarrayName(saNo) :
    """Get subarray name as a string
    Parameter:
      saNo: subarray number"""
    if   saNo == 1: return "sci1"
    elif saNo == 2: return "sci2"
    elif saNo == 3: return "eng1"
    elif saNo == 4: return "eng2"
    elif saNo == 6: return "sciall"
    return "unknown"

#------------------------------------------------------------------------

#def getSubarray(num):
#    "Get a reference to control DO for requested subarray" 
#    try:
#        return carmaIni.getObj("carma/subarrayControl%d" %num)
#    except Exception, ex:
#        print "Error: couldn't get subarray%d" %num + " control object"
#        print ex
 #       return 0

# For convenience
def getObj(name, kind=None ) : 
    """Get an object reference
    param: objectName (e.g. \"carma.bima3.drive\" """
    return carmaIni.getObj(name, kind)
      
#------------------------------------------------------------------------
        
# Get the subarray number
subarrayNo = Subarray.getSubarrayNo()

# Flag for both sci1/sci2
isSciall = Subarray.isSciall()
    
# Get the subarray name
saName = getSubarrayName(subarrayNo)
if isSciall: saName = "sciall"

# Get the subarray DO reference
s  = Subarray.getSubarray()

# And the refs for sci1/sci2 for the sciall case
s1 = None
s2 = None
if Subarray.isSciall() or (Subarray.getSubarrayNo() == 1) :
    try :
        s1 = Subarray.getSubarrayRef(1)
    except:
        print "Couldn't get reference to subarrayController#1"
if Subarray.isSciall() or (Subarray.getSubarrayNo() == 2) :      
    try :
        s2 = Subarray.getSubarrayRef(2)
    except:
        print "Couldn't get reference to subarrayController#2"

# ----------------------------------------------------------------------    
#if __name__ == '__main__' : 
#    print "subarrayControl loading in main"
#else :
#    print "subarrayControl loading in %s" %__name__

# ----------------------------------------------------------------------
# Final imports and path

# Change path to pick up files from the scripts directory, 
# if it exists; otherwise it will be gotten from the install area.
# The import subarrayInit will need this and future mods to subarrayCommands
# could also use observer scripts.
def modifyPath() :
    observerScriptPath = '/array/rt/scripts'
    if sys.path[0] == observerScriptPath : return
    newsyspath  = [observerScriptPath]
    # Add the system scripts directories to the path
    newsyspath  += [observerScriptPath + '/arrayHealth'] 
    newsyspath  += [observerScriptPath + '/arrayHealth/flux'] 
    newsyspath += sys.path
    sys.path    = newsyspath
    # Append the current observing scripts directories to the search path
    sys.path  += ['/array/rt/scripts/currSci1']
    sys.path  += ['/array/rt/scripts/currSci2']
    sys.path  += ['/array/rt/scripts/fastTrack']
    sys.path  += ['/array/rt/scripts/currentConfig']

modifyPath()
#print sys.path

# Put main command set into the global (keyboard) namespace
from subarrayCommands import *

# The import of subarrayInit needs the modified search path
import subarrayInit

#---------------------------------------------------------------
def initialize() :
    """Initialize the subarray by running the script subarrayInit.py"""
    reload(subarrayInit)
    subarrayInit.subarrayInit()
    
def _getdevices() :
    """Get references for a random set of devices (DO's) and assign 
    to global variables. This is useful as a demo, and also tests the 
    availability of some of the DOs."""
    global alarm,de,clock,fs,lr,loref,ll,\
        oc4,od6,oe4,of4,oo2,o1mm2,o3mm2,ot2,\
        sldcsys,sldc,dcmod_b3i6,noisesource,quadmod,lomon,\
        cor3,slpl,wbpl,pdb,sp

    #print "getClock"
    alarm       = getAlarm()
    clock       = getClock()
    fs          = getFault()
    #print "getLR"
    lr          = getLoberotator()
    #print "getLoref"
    loref       = getLoRef()
    #print "getLL"
    ll          = getLinelength()
    
    #print "getOvro4"
    oc4         = Ovro(4).cryo()
    #print "getOvro6"
    od6         = Ovro(6).drive()
    #print "  getOvro6 done"
    oe4         = Ovro(4).enviro()
    of4         = Ovro(4).focus()
    oo2         = Ovro(2).opticalTel()
    #o1mm2       = getOvroRx1mm(2)
    #o3mm2       = getOvroRx3mm(2)
    ot2         = Ovro(2).tiltmeter()

    #print "getSldcsys"
    sldcsys     = getSldcSystem()
    #print "  getSldcsys done"
    sldc        = getSldc()
    dcmod_b3i6  = getSldcmod(3, 6)
    noisesource = getSlNoiseSource()
    quadmod     = getSlQuadmod()
    lomon       = getSlLomon()
    #print "getCorrel"
    cor3        = getCorrelator(3)
    slpl        = getSlPipeline()
    wbpl        = getWbPipeline()
    pdb         = getProjectDbMgr()
    sp          = getSignalPath()

if (__name__ == '__main__') and  _Options.getdevices :
    #print "getdevices"
    _getdevices()
    #print "  getdevices done"
    

# Define the help methods
def printKeyboardHelp(listOnly=False) :
 print "================ KEYBOARD ===================="
 help()
 
def printRefHelp() :
    pass  

#def helpsub() :
#    "Print help for all subarray commands"
#    helpCarma(s)

#def helpdev() :
#    "Print list of hardware device targets available"
#    printRefHelp()

def _keyboardDict():
    theDict = globals().copy()
    for k,v in theDict.items():
        #print k, type(v)
        if type(v) != types.FunctionType: del theDict[k]
    return theDict
    
def helpcmd() :
    """Print help for all commands available at the keyboard.
    These include the commands to get target devices."""
    #printKeyboardHelp()
    helpCarma(_keyboardDict())

def listcmd() :
    """Print a list of all commands available at the keyboard, without help.
    These include the commands to get target devices."""
    #printKeyboardHelp(listOnly=True)
    helpCarma(_keyboardDict(), listOnly=True)

def printHelp() :
    "Print out the standard help summary"
    print "INTRO"
    print "From the keyboard command line you can control the subarray"
    print "or issue engineering commands directly to the hardware" 
    print "bypassing the control system. Direct hardware device access can put" 
    print "the system into an inconsistent state. All input is of form"
    print "  command(parameter1, parameter2...)" 
    print "or"
    print "  target.command( parameter1, parameter2...)" 
    print "where the target can be the subarray or the hardware device access"
    print "point (Distributed Object reference in computer speak)."
    print "With Ipython, which we use, the parentheses are optional in simple" 
    print "cases, and can be replaced with spaces. In this case the method"
    print "with the inserted parenthese is echoed back."
    print "So we may also type"
    print "  command parameter1, parameter2..." 
    print "or"
    print "  target.command parameter1, parameter2..." 
    print "String parameters must be enclosed in quotation marks, single or"
    print "double as long as they match."
    print "Some commands have no parameters, and no parenthese are required."
    print "There are also commands availabe to access the monitor system."
    print "In python, indentation is equivalent to a curly brace in other"
    print "languages, and can therefore cause spurious error messages."
    print "Tabs are not the same as an equivalent number of spaces,"
    print "and are a cause of errors and frustration, so **avoid tabs**."
    print "For a nice tutorial on python, try"
    print "  http://docs.python.org/tut/tut.html"
    print ""
    print "DIRECT DEVICE CONTROL"
    print "To send a command to a device, type" 
    print "  dev.command parameter1, parameter2..." 
    print "In most cases the device that you need is not predefined,"
    print "so you will need to get it and define a target symbol for it."
    print "The commands that are available to get a device usually need"
    print "some input parameters, such as an antenna number, or a band number."
    print "For antennas, you get an antenna and then from that you get the device;"
    print "this can be done all in one step as in the example below."
    print "To see the list of available commands type 'helpcmd'"
    print "Here is an example of getting the target for the ovro#5 focus control."
    print "  of5 = ovro5.focus"
    print "Now setting the z position to 3.1, then 6.2"
    print "  of5.z 3.1"
    print "  of5.z 6.2"
    print "If you only want to control the focus once, you can do it one step"
    print "  ovro5.focus().z(3.1)       yes, parens are required"
    print "And if you will be doing a lot of fiddling with a specific antenna,"
    print "try"
    print "  a=ovro5"
    print "  a.focus().z(3.1)"
    print "  a.drive().stow()"
    print ""
    print "MONITOR SYSTEM"
    print "You can access the monitor system and get its structure and current"
    print "values as text or get monitor point values as integers or doubles."
    print "In both cases, the commands will need the monitor component name"
    print "(a container for structure or a monitor point for value)."
    print "The monitor point and container names are case insensitive."
    print "Here are some examples:"
    print "  query \"Sldc.Band9.Input3\")        -gets the structure/values"
    print "  queryDouble \"Sldc.Band9.Input3.totalpower\"  -gets the dbl value"
    print "  queryInt \"Sldc.Band2.Input6.temp\" -gets the int value"
    print ""
    print "COMMAND LINE COMPLETION, HELP, and EXIT"
    print "Command line completion is done with tab. "
    print "Help:"
    print "  help              - this summary"
    print "  listcmd           - all keyboard commands, without help"
    print "  helpcmd           - all keyboard commands, with help"
    print "  help cmdname      - for a single command"
    print "To exit type 'quit' or 'exit'."
    print ""

pythonhelp = help
def help(obj=None) :
    """If no argument is given, prints the standard help summary;
    if an argument is given, it gives help on the argument. 
    The argument can be a reference or a string representing a function.
    Example:
      help track"""
    if obj == None: printHelp()
    elif type(obj) == types.StringType:
        try :
            func = globals()[obj]
        except Exception:
            print "No command named '%s' found" %obj
            return
        pythonhelp(func)
    else: pythonhelp(obj)

    
# Conditional initialization
# Only initialize if this is the main python module (keyboard input)
if __name__ == '__main__' : 
    # Display help
    _Options.displayHelp()
    # Initialize subarrayControl if necessary
    "Get initialization flag from cmd line"
    longOptions = ["subarrayNumber=", "init="]
    inputValues = getopt.getopt(sys.argv[1:], "", longOptions)
    doInit = int(inputValues[0][1][1])                                                                             
    if doInit == 1:
        if s.getInitializationFlag() :
            print "Subarray controller already initialized"
        else :
            print "Initializing subarray controller..."
            initialize()
            # Don't do a self-test because we lose the restored obsblock
            #subarrayInit.selfTest()
    else :
        print "Initialization inhibited on command line"
        
