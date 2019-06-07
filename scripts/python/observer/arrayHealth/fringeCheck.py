"""
    fringeCheck.py

    Purpose: 
        This script observes the noise source for 60 seconds and a 
        bright quasar for 1 minute. Observers should run this script
        after every rebuild, and check the output to make sure the 
        system is behaving normally. Results can be viewed at:

                   http:cedarflat.mmarray.org/fringe/

    Instructions:
        The script should be run from the sci1 or sci2 subarray as follows:
           Sci#1[1]: run('fringeCheck')

        The script accepts several command line options:
           bits     =2     : Number of correlator sampling (2, 3, or 4)
           comment  =None  : Comment string for html page
           conf     =None  : Correlator configuration (LL,RR,DUALPOL, etc). None -> 'LL'
           decimate =False : Decimate?
           fill=False      : Run the manual filler? Useful if obsblock is set.
           freq=<float>    : Set sky frequency in GHz (default: 100 GHz in sci1, 35.938 GHz in sci2)
           fill     =True  : Fill the obsblocks?
           observe  =True  : Observe the source?
           obsblock=<name> : Instead of observing a source, process the obsblock <name>.
           pnt      =False,: Point up before observing
           record   =10.0  : Record length (seconds)
           refant   =None  : Number for reference antenna
           tint     =60.0  : Integration time (seconds) on the astronomical source
           pacs     =False : PACS mode
           sb              : Sideband (LSB or USB, in quotes)
           source=<name>   : Name of the source to observe.
           tune     =False : Do not tune the receivers and do not reconfigure the correlator
           tintPol  =0.5  : Grid-observation length (in minutes; full-Stokes fringeChecks only)

         Examples:
             Sci#1[2]: run('fringeCheck', tune=False, source='3c273')
             Sci#1[3]: run('fringeCheck', obsblock='fringe.3c273.2007jan20.1')

    What the script does:
        In default mode on sci1, the script will:
           a) Tune the receivers to LO=100 GHz, IF=2.5 GHz
           b) Configure the correlator to 8 x 500 MHz
           c) Go to a bright source (> 4 Jy) above 30 deg elevation
           d) Observe the noise source for 10 seconds
           e) Observe a bright source for 1 minute
           f) Run the filler manually to create the miriad file.
              A working directory in /tmp/scratch is created.
           g) Create gif images of amplitude vs. baseline length
           h) Post results at the web site http://cedarflat.mmarray.org

        If the <obsblock> keyword is set, then steps (a)-(e) are skipped.

    
    Notes:
        Sometimes the miriad file appears before all of the data are available.
        In this case, you can try re-processing the obsblock by setting
        the obsblock keyword.

    Adapted from original script by M. La Vigne & A. Zauderer
"""
#  
# 2007 Jan 17 DCJB/JMC  update for routine use after every build
# 2007 Jan 20 JMC       write results to web page
# 2007 Mar  2 JMC       Added 3 minute wait time for xml file 
# 2007 Mar 18 MH        Added Radio Pointing
# 2007 Mar 28 JWL       Removed 'tuneOvroRx()'
# 2011 Feb 23 JWL       Added check for SZA antennas. Throw exception if present.

import fringeCheckFunctions as fc
import runCommand as rc
import subarrayCommands as commands
import printFunctions as pc

# Clear all script variables
commands.controlVariablesClear()

# Add options to give observers flexibility on executing the script
p = rc.Params()
p.add("bits",     default=2,     type=float, description="Number of correlator sampling (2, 3, or 4)")
p.add("comment",  default=None,  type=str,   description="Comment string for html page")
p.add("conf",     default=None,  type=str,   description="Correlator configuration (default=LL)")
p.add("decimate", default=False,  type=bool, description="Decimate?")
p.add("freq",     default=None,  type=float, description="Observing sky frequency (GHz)", noneAllowed=True)
p.add("iffreq",   default=None,  type=float, description="IF frequency (GHz)", noneAllowed=True)
p.add("fill",     default=True,  type=bool,  description="Fill the obsblocks?")
p.add("observe",  default=True,  type=bool,  description="Observe the source?")
p.add("obsblock", default=None,  type=str,   description="Name of the obsblocks to fill")
p.add("pnt",      default=False, type=bool,  description="Point up before observing")
p.add("record",   default=10.0,  type=float, description="Record length (seconds)")
p.add("refant",   default=None,  type=int,   description="Number for reference antenna")
p.add("sb",       default=None,  type=str,   description="Sideband (USB or LSB, in quotes)", noneAllowed=True)
p.add("source",   default=None,  type=str,   description="Name of soruce to observe")
p.add("tint",     default=60.0,  type=float, description="Integration time (seconds)")
p.add("tune",     default=True,  type=bool,  description="Tune the receivers?")
p.add("pacs",     default=False,  type=bool,  description="PACS mode")
# EML defaulting this to None, so we can check if it has been specified
p.add("maxsens",  default=None,  type=bool,  description="MAXSENS mode")
p.add("tintPol",  default=0.5,  type=float, description="Grid-observation length (in minutes; full-Stokes fringeChecks only)")

# Various pathnames
var = dict()
var['fringeDir']     = '/array/utilities/apache2.0.54/htdocs/fringe'
var['fringeDirHtml'] = '/fringe'
var['fringeSubdir']  = 'results'
var['fringeLog']     = 'fringeCheckLog.txt'
var['fringeCheck']   = '/array/rt/scripts/arrayHealth/fringeCheck.csh'
var['scratchArea']   = '/tmp/scratch'
var['Filler']        = '/array/obs/bin/Fill'
var['output']        = "f" # Temporary directory for miriad reduction

# Initialize options
var['obsblockName'] = None
var['subObsblockName'] = None
var['date']  = None

# Source parameters
var['source'] = None
var['flux']   = None

# **********************************
# END OF OPTIONS - START MAIN SCRIPT
# **********************************

# Parse command line arguments
fc.readCommandLine(p, var)

subArrayNo = commands.s.getSubarrayNo()
# Check that only one correlator is assigned
if var['obsblockName'] == None:
   mycorr = assignedCorrelator(subArrayNo)
   if ( mycorr == 'NONE' ) :
      raise Exception, "This subarray has no correlators assigned. Use addCorrelator(CORR_SPECTRAL) or addCorrelator(CORR_WIDEBAND) to assign one"
#   if ( mycorr == 'ANY' ) :
#      raise Exception, "You can only run fringeCheck with one correlator assigned to the subarray has no correlators assigned. Use removeCorrelator(CORR_SPECTAL) or removeCorrelator(CORR_WIDEBAND) to remove one"


# Make html link to indicate observations are in progress
fc.createHtmlInformative(var)


# Observe source, if necessary
if p.observe: fc.observeSources(var, p)


# Run manual fill. Output file written to /home/obs/vis_data
# runFiller() returns antenna online in miriad dataset

okFill = True
if p.fill: 
   okFill = fc.runFiller(var)
   if not okFill:
      print ''
      print '*** astroheader file not found for obsblock=',var['obsblockName']
      print '*** No miriad file was created.'
      print ''


# Run fringeCheck.csh to reduce data in miriad
# Function returns antennas that were online with data,
# online without data, antennas offline, and the utdate
antennas = None
if okFill: antennas = fc.runFringeCheck(p, var, referenceAntenna=p.refant)


# Start html file
fc.createHtmlResults(var, antennas, p)


# Create archive listing
fc.createArchive(var)


# Reset project
if p.obsblock == None: 
   fc.commands.resetProject()
   commands.maint()
