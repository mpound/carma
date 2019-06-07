# $Id: phasetest.py,v 1.8 2012/12/18 21:20:25 mpound Exp $
#
# To use:
#
#   run('phasetest')
#
#   All parameters are optional.
#   See below for optional parameters.
#
#
# @Author John Carpenter
#
# History
# 02-Sep-2008: JMC      original version copied to arrayHealth
#
"""
    phasetest.py

    Purpose: 
        This script will stare at a quasar for a period of time
        interrupted only by tsys and noise-source measurements.

    Instructions:
        The script should be run from the sci1 subarray as follows:
           Sci#1[1]: run('phasetest')

        The script accepts several command line options:
           elmin=<f>      : Minimum elevation in degrees. Default = 30
           elmax=<f>      : Maximum elevation in degrees. Default = 70
           flux=<limit>   : If set, specify flux limit in Jansky's.
           fsky=<freq>    : Sky frequency for tuning (default: 95 GHz)
           noise=True     : If True, then observe noise source before
                            each integration (default: True)
           pnt=True       : If True, run radio pointing  (default: False)
           record=<f>     : Record length in seconds (default: 10.0 seconds)
           source=<name>  : Instead of having the script pick a source, 
                            observe source <name>.
           time=<f>       : Length of track in hours.
                            The time can be reset during the middle of the track
                            by entering the following command in the sac:
                                 s.setScriptDouble(1, <newtime>),
                            where <newtime> is given in hours.
           tune=False     : Do not tune the receivers and do not reconfigure 
                            the correlator
         Examples:
             Sci#1[1]: run('phasetest', tune=False, source='3c273')
             Sci#1[2]: run('phasetest', fsky=230.0, pnt=True)
                           
    What the script does:
        In default mode, the script will:
           a) Tune the receivers to LO=95 GHz, IF=1.30 GHz
           b) Configure the correlator to 8 x 500 MHz using configwideastroband()
           c) Go to a bright source (> 4 Jy) above 30 deg elevation
           e) Observe the bright source for 1 hour
"""

import subarrayCommands as commands
import obsdef2 as od
import refPoint
import time
import runCommand as rc

# Initialize command line options
p = rc.Params()
p.add('elmin',  default=30.0,  type=float, description='Minimum elevation (degrees)')
p.add('elmax',  default=70.0,  type=float, description='Maximum elevation (degrees)')
p.add('flux',   default=4.0,   type=float, description='Minimum flux (Jy)')
p.add('fsky',   default=95.0,  type=float, description='Sky frequency (GHz)')
p.add('noise',  default=True,  type=bool,  description='Perform noise integrations?')
p.add('pnt',    default=False, type=bool,  description='Perform radio pointing?')
p.add('record', default=10.0,  type=float, description='Record length in seconds')
p.add('source', default=None,  type=str,   description='Source name')
p.add('time',   default=1.0,   type=float, description='Length of track (hours)')
p.add('tune',   default=True,  type=bool,  description='Tune the receivers?')


# **********************************
# END OF OPTIONS - START MAIN SCRIPT
# **********************************

# Parse command line arguments
p.processInputParameters(inputParams=commands.scriptKeyVals)

# Project parameters
trackFilename = 'phasetest.py' 
projectCode   = 'ct002'
obsblock      = 'phase'
pi            = 'Array Health'

# Email parameters
sendTo        = 'arrayhealth@mmarray.org'
sendFrom      = 'obs@mmarray.org'
cc            = None
bcc           = None
subject       = 'phase stability test'

# Initialize
commands.newProject(projectCode, obsblock)
commands.radioInit()
commands.trackThreshold(0.1,0)
commands.controlVariablesClear()

# Set time
indx = 1
commands.s.setScriptDouble(indx, p.time)

# Line parameters
sb = commands.USB
ifFreq = 1.30
flo = p.fsky - ifFreq

# Get bright source
source = p.source
if source == None:
    brightSources = od.getBrightSources(p.flux, freq=p.fsky)
    source = od.getSource(brightSources, elmin=p.elmin, elmax=p.elmax)
if source == None:
   raise Exception,'No source found. Consider change flux, elmin, or elmax criteria.'

# Send email
msg  = 'Script name : ' + trackFilename + '\n'
msg += 'Obsblock    : ' + od.getObsblockName() + '\n'
msg += 'Source      : ' + source + '\n'
msg += 'Time        : ' + str(p.time) + ' hours\n'
od.sendEmailMessage(sendTo, sendFrom, msg, subject, cc, bcc)

# Slew to source
print 'Slewing to ', source
commands.track(source)

# Tune and set correlator
if p.tune:
   print 'Tuning to flo = ' + str(flo) + ' GHz'
   commands.freq(p.fsky, sb, ifFreq, source)
   commands.configwideastroband()
   commands.checkbands()
else:
   commands.tsys()

# Point up if needed
if p.pnt: refPoint.refPoint(source)

# Bright source
commands.intent(source, 'O')
commands.track(source)

# Integrate
tstart = time.time()
print 'To change length of the track to 10 hours (for example), type:'
print 's.setScriptDouble(%d, 10.0)' % indx
while ( (time.time() - tstart) / 3600.0 < commands.s.getScriptDouble(indx)):
    # Print message
    dt = commands.s.getScriptDouble(indx) - ((time.time() - tstart) / 3600.0)
    commands.trackMessage('Starting integration - %.1f hours remaining in test' % dt)

    # Measure tsys
    if p.noise: od.observeNoise(2,2);
    commands.tsys()

    # Integrate
    n = int(600.0 / p.record)
    if n < 1: n = 1
    commands.integrate(p.record, n)

# End of script
commands.resetProject()
