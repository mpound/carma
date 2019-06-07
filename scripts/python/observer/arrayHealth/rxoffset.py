# $Id: rxoffset.py,v 1.12 2012/12/18 21:20:25 mpound Exp $
# Script to measure pointing offset between 3mm and 1mm receivers
#
# To use:
#
#   run('rxoffset <parameters>')
#
#   All parameters are optional, they are space or comma separated pairs.
#   See below for optional parameters.
#
#
# @Author John Carpenter
#
# History
# 02-Sep-2008: JMC      original version copied to arrayHealth
#

"""
    rxoffset.py

    Purpose: 
        This script measures the pointing offsets between two frequencies (freq2 - freq1).

    Instructions:
        The script should be run as follows:
           Sci#1[1]: run('rxoffset')

        The script accepts the following command line options:
           source=<name>  : Name of source to point on. Default is to pick bright source from calibrator list.
           ncycles=<n>    : Number of pointing cycles. Default is 3
           freq1=<f>        LO1 frequency for first tuning. Default 95 GHz
           freq2=<f>        LO2 frequency for second tuning. Default 225 GHz
           flux=<f>       : Minimum acceptable flux at highest frequency. Default = 4 Jy
           elmin=<f>      : Minimum source elevation in degrees. Default = 30 deg
           elmax=<f>      : Maximum source elevation in degrees. Default = 80 deg
           conf=<f>       : Configuration for configwideastroband. DEFAULT='DEFAULT'

     Time:
         It takes about 20 minutes per cycle.

     Examples:
         Sci#1[2]: run('rxoffset', source='3c454.3' ncycles=3)
         Sci#1[2]: run('rxoffset', source='3c454.3' ncycles=3, freq1=95, freq2=225, flux=4)

     What the script does:
        In default mode, the script will:
           a) Tune to freq1 and point
           b) Tune to freq2 and point
           c) Print out the pointing differences
"""

import subarrayCommands as commands
import obsdef3 as od
import runCommand as rc
import time
import math

def pointUp(source, antennas, freq, conf):
    import random
    import radioPoint
    import copy

    # Reset frequency for 1cm observations
    if freq < 40.0: freq = 35.938

    # Set sideband
    sideband = commands.LSB
    if freq > 40.0: sideband = commands.USB

    # Tune receivers
    print 'Tuning to %.1f GHz' % freq
    commands.freq(freq, sideband, 0.0, None)

    # Set correlator
    commands.configwideastroband(conf=conf)
    if commands.subarrayNo == 1:
       commands.tsys()
    commands.checkbands()

    # Point
    results = radioPoint.radioPoint(source, waitCycles=5)   
 
    # Wait for monitor points to catch up
    commands.sleep(3)

    # Get antennas that successfully pointed
    goodAntennas = []
    if results <> None: goodAntennas = results.keys()

    # Read new mount offsets
    mount = dict()
    for a in goodAntennas:
        # Set monitor point name
        if a <= 6:
            root = 'Ovro' + str(a) 
        elif a <= 15:
            root = 'Bima' + str(a-6)
        else:
            root = 'Sza' + str(a-15)
        root += '.AntennaCommon.Drive.Point.mountOffset'
        mp1 = root + 'Az'
        mp2 = root + 'El'

        # Read mp values
        x1 = commands.queryDouble(mp1)
        x2 = commands.queryDouble(mp2)

        # Save
        mount[a] = [x1, x2]

    # Done
    return copy.copy(mount)


# **********************************
# START OF OPTIONS 
# **********************************

# Initialize command line options
p = rc.Params()
p.add('conf',    default='DEFAULT',  type=str, description='Configuration for configwideastroband')
p.add('elmin',   default=30.0,  type=float, description='Minimum elevation (degrees)')
p.add('elmax',   default=80.0,  type=float, description='Maximum elevation (degrees)')
p.add('ncycles', default=3,     type=int,   description='Number of cycles')
p.add('flux',    default=4.0,   type=float, description='Minimum flux at highest frequency(Jy)')
p.add('source',  default=None,  type=str,   description='Source name')
p.add('freq1',   default=95,    type=float, description='LO1 frequency for the first  tuning (GHz)')
p.add('freq2',   default=225,   type=float, description='LO1 frequency for the second tuning (GHz)')

# Parse command line arguments
p.processInputParameters(inputParams=commands.scriptKeyVals)

# **********************************
# END OF OPTIONS - START MAIN SCRIPT
# **********************************

# Project parameters
trackFilename = 'rxoffset.py' 
projectCode   = 'ct012'
obsblock      = 'rpnt'
pi            = 'Array Health'

# Email parameters
email    = 'arrayhealth@mmarray.org'
sendTo   = 'arrayhealth@mmarray.org'
sendFrom = 'obs@mmarray.org'
cc       = None
bcc      = None
subject  = 'rxoffset test'

# We must set a project even though we will not be saving any data
commands.newProject(projectCode, obsblock)
commands.radioInit()
commands.trackThreshold(0.1,0)
commands.controlVariablesClear()

# Send email
# commands.sendMsg(projectCode, pi, email, trackFilename, start = 1)

# Initialize logfile
log_rxoffset = ''

# Select source
source = p.source
if source == None:
    sourceList = od.utils.getBrightSources(p.flux, freq=max(p.freq1,p.freq2))
    source = od.utils.getSource(sourceList, elmin=p.elmin, elmax=p.elmax, allowShadowing=False)
    if source == None: raise Exception,'No source is available to point on'
commands.track(source)
log = 'Estimate flux density for %s\n' % source
log += '       @ %3.0f GHz :  %3.1f Jy\n' % (p.freq1, od.utils.getSourceFlux(source, freq=p.freq1))
log += '       @ %3.0f GHz :  %3.1f Jy\n' % (p.freq2, od.utils.getSourceFlux(source, freq=p.freq2))
log += '\n'
print ''
print log
log_rxoffset += log
   
# Initialize
antennas = commands.currentAntennaNumbers()

# Memory to store calculations
mo_sum  = dict()
mo_sum2 = dict()
mo_n    = dict()
for a in antennas:
    mo_sum[a]  = [0.0, 0.0]
    mo_sum2[a] = [0.0, 0.0]
    mo_n[a]    = 0

# Use script variables to keep track of cycles
indx_ncycles = 1
commands.s.setScriptInt(indx_ncycles, p.ncycles)
print ''
print 'If you want to change the number of cycles, enter the following command in a sac:'
print 's.setScriptInt(%d, NN)' % indx_ncycles
print 'where NN is the total number of cycles you want. Currently, NN = %d' % commands.s.getScriptInt(indx_ncycles)
print ''
print ''

# Loop over cycles
ncycle = 0
while ncycle < commands.s.getScriptInt(indx_ncycles):
    # Point up at freq1
    mount1 = pointUp(source, antennas, p.freq1, p.conf)

    # Point up at freq2
    mount2 = pointUp(source, antennas, p.freq2, p.conf)

    # Compute difference between mount offsets.
    diffMount = dict()
    for a in antennas:
        if mount1.has_key(a) and mount2.has_key(a):
           x = list()
           for i in [0,1]:
               x.append(mount2[a][i] - mount1[a][i])
           diffMount[a] = x[:]

    # Print results: header first
    sfreq1 = '%.0f GHz' % p.freq1
    sfreq2 = '%.0f GHz' % p.freq2
    log = "\n"
    log += "\n"
    log += "\n"
    log += "*******************************************************************************\n"
    log += "Results for cycle " + str('%2d' % (ncycle+1)) + " : " + time.asctime() + "\n"
    log += "\n"
    log += "      Mount offsets (this cycle)   Difference (%s - %s) in Mount Offsets\n" % (sfreq2, sfreq1)
    log += "      ---------------------------  ------------------------------------------------\n"
    log += "Ant      %-8s       %-8s     This cycle      Average           RMS     Npts\n" % (sfreq2, sfreq1)
    log += "----  ------------   ------------  -------------  -------------   ------------ ----\n"
    log_rxoffset += log
    print log

    # Loop over antennas
    ave = dict()
    for a in antennas:
        # Add to totals if this is a good star
        if diffMount.has_key(a):
            # Get current sum to date
            sum  = mo_sum[a]
            sum2 = mo_sum2[a]
            n    = mo_n[a]

            # Add to sum
            x = list()
            for i in [0,1]:
                sum[i]  += diffMount[a][i]
                sum2[i] += diffMount[a][i]**2
            n += 1

            # Save sum
            mo_sum[a]  = sum
            mo_sum2[a] = sum2
            mo_n[a]    = n

        # Print antenna
        log = str('%3d' % a)

        # freq2 offset from last cycle
        if mount2.has_key(a):
            log += '  ' + str('%6.3f %6.3f' % (mount2[a][0], mount2[a][1]))
        else:
            log += '  ' + str('%6s %6s' % (' ---- ', ' ---- '))

        # freq1 offset from last cycle
        if mount1.has_key(a):
            log += '  ' + str('%6.3f %6.3f' % (mount1[a][0], mount1[a][1]))
        else:
            log += '  ' + str('%6s %6s' % (' ---- ', ' ---- '))

        # Difference
        if diffMount.has_key(a):
            log += '  ' + str('%6.3f %6.3f' % (diffMount[a][0], diffMount[a][1]))
        else:
            log += '  ' + str('%6s %6s' % (' ---- ', ' ---- '))

        # Average
        if mo_n[a] > 0:
            # Compute average
            xave = list()
            for z in mo_sum[a]: xave.append(z / mo_n[a])
            ave[a] = xave

            # Print
            log += '  ' + str('%6.3f %6.3f' % (xave[0], xave[1]))
        else:
            log += '  ' + str('%6s %6s' % (' ---- ', ' ---- '))

        # RMS
        if mo_n[a] > 1:
            # Compute rms
            rms = list()
            rn = float(mo_n[a])
            for i in range(len(mo_sum[a])): 
                var = (mo_sum2[a][i]*rn - mo_sum[a][i]**2) / rn / (rn-1.0)
                if var < 0.0: var = 0.0  # Avoids precision errors
                rms.append(math.sqrt(var))

            # Print
            log += '  ' + str('%6.3f %6.3f' % (rms[0], rms[1]))
        else:
            log += '  ' + str('%6s %6s' % (' ---- ', ' ---- '))

        # Number of good pointins
        log += '  ' + str('%2d' % mo_n[a])

        # Print line
        log_rxoffset += log + '\n'
        print log

    # End 
    log = "\n*******************************************************************************\n"
    log_rxoffset += log
    print log

    # Increase ncycles
    ncycle += 1
    if ncycle < commands.s.getScriptInt(indx_ncycles):
       print '\n'
       print 'If you want to change the number of cycles, enter the following command in a sac:'
       print 's.setScriptInt(%d, NN)' % indx_ncycles
       print 'where NN is the total number of cycles you want. Currently, NN = %d' % commands.s.getScriptInt(indx_ncycles)

# Send email
od.utils.sendEmailMessage(sendTo, sendFrom, log_rxoffset, subject, cc, bcc)

# Reset project
commands.resetProject()
