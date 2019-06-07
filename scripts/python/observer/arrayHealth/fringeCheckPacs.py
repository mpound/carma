"""
    fringeCheck.py

    Purpose: 
        This script observes the noise source for 60 seconds and a 
        bright quasar for 3 minutes. Observers should run this script
        after every rebuild, and check the output to make sure the 
        system is behaving normally. Results can be viewed at:

                   http:cedarflat.mmarray.org/fringe/

    Instructions:
        The script should be run from the sci1 subarray as follows:
           Sci#1[1]: run('fringeCheck')

        The script accepts several command line options:
           fill=False     : Do not run the manual filler. Useful if obsblock
                            is set. (Default: True)
           freq           : Set sky frequency in GHz (default: 100 GHz)
           sb             : Sideband (LSB or USB, in quotes)
           iffreq         : Set IF Freq in GHz (default: 2.5 GHz)
           mm1=True       : If True, run 1mm fringe test (default: 3mm)
           obsblock=<name>: Instead of observing a source, process the
                            obsblock <name>.
           pnt=True       : If True, run radio pointing  (default: False)
           source=<name>  : Instead of having the script pick a source, 
                            observe source <name>.
           tune=False     : Do not tune the receivers and do not reconfigure 
                            the correlator
           record=10      : Record length in seconds for the astronomical source
           refant=None    : If specified, reduce antenna using refant as
                            the reference antenna.
           tint=60        : Integration time in seconds for the astronomical source
           pacs=true      : Observer in pacs mode

         Examples:
             Sci#1[2]: run('fringeCheck', tune=False, source='3c273')
             Sci#1[3]: run('fringeCheck', obsblock='fringe.3c273.2007jan20.1')
             Sci#1[3]: run('fringeCheck', mm1=True, pnt=True)
                           
    What the script does:
        In default mode, the script will:
           a) Tune the receivers to LO=100 GHz, IF=2.15 GHz
           b) Configure the correlator to 3 x 500 MHz, with the bands
              offset by 0.1 GHz
           c) Go to a bright source (> 4 Jy) above 30 deg elevation
           d) Observe the noise source for 10 seconds
           e) Observe a bright source for 3 minutes
           f) Run a manual "Fill" on the track
           g) Create gif images of amplitude vs. baseline length
           h) Post results at the web site http://cedarflat.mmarray.org

        If the <obsblock> keyword is set, then steps (a)-(e) are skipped.

    
    Notes:
        Sometimes the manual fill runs before all of the data are available.
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

import fringeCheckFunctionsPacs as fc
import runCommand as rc
import subarrayCommands as commands

# Clear all script variables
commands.controlVariablesClear()

# Add options to give observers flexibility on executing the script
p = rc.Params()
p.add("cm",       default=False, type=bool,  description="Set frequency for 1cm observing?")
p.add("iffreq",   default=2.0,   type=float, description="IF frequency (GHz)")
p.add("fill",     default=True,  type=bool,  description="Fill the obsblocks?")
p.add("freq",     default=100.0, type=float, description="Observing frequency (GHz)")
p.add("sb",       default='USB', type=str, description="Sideband (USB or LSB, in quotes)")
p.add("mm1",      default=False, type=bool,  description="Tune to 1mm?")
p.add("observe",  default=True,  type=bool,  description="Observe the source?")
p.add("obsblock", default=None,  type=str,   description="Name of the obsblocks to fill")
p.add("pacs",     default=False, type=bool,  description="Observe in PACS mode?")
p.add("pnt",      default=False, type=bool,  description="Point up before observing")
p.add("record",   default=10.0,  type=float, description="Record length (seconds)")
p.add("refant",   default=None,  type=int,   description="Number for reference antenna")
p.add("source",   default=None,  type=str,   description="Name of soruce to observe")
p.add("tint",     default=60.0,  type=float, description="Integration time (seconds)")
p.add("tune",     default=True,  type=bool,  description="Tune the receivers?")

# Various pathnames
var = dict()
var['fringeDir']     = '/array/utilities/apache2.0.54/htdocs/fringe'
var['fringeDirHtml'] = '/fringe'
var['fringeSubdir']  = 'results'
var['fringeLog']     = 'fringeCheckLog.txt'
var['fringeCheck']   = '/array/obs/bin/fringeCheckPacs.csh'
var['visdataDir']    = '/home/obs/vis_data'
var['Filler']        = '/array/obs/bin/Fill'
var['output']        = "f" # Temporary file for miriad reduction

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


# 1cm observing?
mp =  "control.subarray%i.loFreq" % commands.subarrayNo
lofreq = commands.queryDouble(mp)
var['cm'] = False
if p.freq < 50.0 or p.cm: var['cm'] = True


# Make html link to indicate observations are in progress
fc.createHtmlInformative(var)


# Observe source, if necessary
if p.observe: fc.observeSources(var, p)


# Run manual fill. Output file written to /home/obs/vis_data
# runFiller() returns antenna online in miriad dataset
if p.fill: fc.runFiller(var)


# Run fringeCheck.csh to reduce data in miriad
# Function returns antennas that were online with data,
# online without data, antennas offline, and the utdate
antennas = fc.runFringeCheck(var, referenceAntenna=p.refant, pacs=p.pacs)


# Start html file
fc.createHtmlResults(var, antennas)


# Create archive listing
fc.createArchive(var)


# Reset project
fc.commands.resetProject()
