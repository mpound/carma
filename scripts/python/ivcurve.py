# Script to measure and plot antenna iv curve.
#
# Results from these measurements will go into either /array/rt/ivcurve, /tmp
# or a user specified locale.
# 
# @author Andy Beard
# $Id: ivcurve.py,v 1.8 2008/10/14 20:44:31 scott Exp $
#
# $CarmaCopyright$
#

import carma
import carmaHelpers as helper
import os
import Subarray
import time
import printFunctions as pf

from pylab import *
from matplotlib.font_manager import FontProperties
from subarrayCommands import amb, sky, RX1MM, RX3MM, currentAntennaNumbers

s = Subarray.getSubarray()

# From James arrayHealth scripts - copied here to prevent 'unallowed' dependency
def getDataPath(pathName, verbose = True) :
    """ 
    Returns a valid directory for writing data

    This function will test to see if the /array/rt/ directory is
    available. If so it will add the user specified path to it,
    and if not it will be appended to /tmp/. Failing that it will
    append to the current directory. The user specified path may
    have an indefinite number of subdirectories.

    Parameters {default}:

    pathName
    The directory under /array/rt to write data to

    verbose    {True}
    Writes progress messages to the terminal if it needs
    to create new files

    Return:
    A string with the full path name
    """
   # Explicitly disallow absolute path
    pathName = pathName.lstrip('/')

# Check for existence of predefined start directory
    dataPath = "/array/rt/"
    if not os.path.exists(dataPath) :
        dataPath = "/tmp/"
    if not os.path.exists(dataPath) :
        dataPath = ""

# Check to see that each level exists, and create if it does not

    subdirs = string.split(pathName, '/')
    for subdir in subdirs :
        if subdir != "" :
            dataPath += subdir + '/'
            if not os.path.exists(dataPath) :
                if verbose :
                    pf.printMessage("Creating directory: " + dataPath)
                    os.mkdir(dataPath)

    return dataPath

def dimspec( nAnts ):
    """ Return a matplotlib subplot dimension specification based on the
    number of input antennas.
    """
    if nAnts == 1:
        return (1,1)
    elif nAnts <= 4:
        return (2,2) 
    elif nAnts <= 6:
        return (3,2)
    elif nAnts <= 9:
        return (3,3)
    elif nAnts <= 12:
        return (4,3)
    elif nAnts <= 15:
        return (5,3)
    elif nAnts <= 24:
        return (6,4)
    else: 
        return 

def make_subplot( rows, columns, subplotId ):
    row,col = divmod( subplotId - 1, columns )
    rfb = rows - row - 1 # rfb=rows from bottom
    xSubplotWidth = 1.0 / columns 
    ySubplotWidth = 1.0 / rows
    # Specify buffers for labels and ticks
    leftSubplotBuffer = 0.17 # As a percentage of the subplot (not the figure)
    rightSubplotBuffer = 0.16 # As a percentage of the subplot (not the figure)
    xSubplotBuffer = leftSubplotBuffer + rightSubplotBuffer
    topSubplotBuffer = 0.13
    bottomSubplotBuffer = 0.23
    ySubplotBuffer = topSubplotBuffer + bottomSubplotBuffer
    xGraphWidth = (1.0 - xSubplotBuffer)*xSubplotWidth
    yGraphWidth = (1.0 - ySubplotBuffer)*ySubplotWidth
    
    xpos = col*xSubplotWidth + leftSubplotBuffer*xSubplotWidth
    ypos = rfb*ySubplotWidth + bottomSubplotBuffer*ySubplotWidth

    majorLocator   = MultipleLocator(1.0)
    majorFormatter = FormatStrFormatter('%.1f')
    minorLocator   = MultipleLocator(0.500)
    ax = axes([xpos, ypos, xGraphWidth, yGraphWidth])
    ax.xaxis.set_major_locator(majorLocator)
    ax.xaxis.set_major_formatter(majorFormatter)
    ax.xaxis.set_minor_locator(minorLocator)
    xlabel( 'Vj (mV)' ) 
    ylabel( 'Ij (uA)' )
    grid(True)
    ax.xaxis.grid(True, which='minor')
    
def turn_off_tick_labels( ax ):
    ticks = ax.get_major_ticks()
    for t in ticks:
        t.label1On = False

def plotsummary( dirname, Ij, Vj, TpAmb=None, TpSky=None, Yfac=None, \
                 fileprefix='', fmt='pdf' ):

    nAnts = len( Ij.keys() )

    if nAnts <= 1:
        return

    rows,columns = dimspec( nAnts )
    clf()
    subplotId = 1
    for ant in Ij.iterkeys():
        make_subplot( rows, columns, subplotId )
        title( "Carma %d" % ant ) 
        subivline = plot( Vj[ant], Ij[ant], label='IV Curve' )

        if TpAmb != None and TpSky != None and Yfac != None:
        
            ax2 = twinx()
            turn_off_tick_labels(ax2.xaxis)
            ax2.yaxis.set_major_locator( NullLocator() )
            subamb = plot( Vj[ant], TpAmb[ant], 'r--', label='Tp Amb' )
            subsky = plot( Vj[ant], TpSky[ant], 'b-.', label='Tp Sky' )

            ax3 = twinx()
            ax3.yaxis.set_major_formatter( FormatStrFormatter('%.1f') )
            turn_off_tick_labels(ax3.xaxis)
            subyline = plot( Vj[ant], Yfac[ant], 'm-+', label="Y Factor" )
            ylabel( "Y Factor" )

            legend( (subivline, subyline, subamb, subsky),\
                    ('IV Curve', 'Y Factor','Tp Amb', 'Tp Sky'),\
                    loc='upper left', pad=0.1, markerscale=0.3, 
                    prop=FontProperties(size='x-small') )
        subplotId += 1

    # Save image to file
    filename = "%s/%s-All.%s" % (dirname,fileprefix,fmt)
    print "  Saving %s." % filename
    savefig( filename ) 

def plotsingle( ant, dirname, Ij, Vj, TpAmb=None, TpSky=None, Yfac=None, \
                fileprefix='', fmt='pdf' ):
    clf()
    title( "Carma %d" % ant ) 
    xlabel( 'Junction Voltage (mV)' )
    ylabel( 'Junction Current (uA)' )
    grid(True)

    ivLine=plot( Vj, Ij )

    # Plot Y Factors if defined 
    if TpAmb != None and TpSky != None and Yfac != None:
        # Use right hand Y axis for amb & sky total powers sans tick labels.
        ax2 = twinx()
        ax2.yaxis.set_major_locator( NullLocator() )
        turn_off_tick_labels(ax2.xaxis)
        
        ambLine=plot( Vj, TpAmb, 'r--', label='Tp Amb' )
        skyLine=plot( Vj, TpSky, 'b-.', label='Tp Sky' )

        # Use same axis for Y factor but this time set labels.
        ax3 = twinx()
        turn_off_tick_labels( ax3.xaxis )
        ax3.yaxis.set_major_formatter( FormatStrFormatter('%.1f') )
        yLine=plot( Vj, Yfac, 'm-+', label='Y-Factor' )
        ylabel( "Y-Factor" )
        
        legend( (ivLine, yLine, ambLine, skyLine),\
                ('IV Curve', 'Y Factor','Tp Amb', 'Tp Sky'),\
                loc='upper left' )

    # Save image to file
    filename = "%s/%s-C%d.%s" % (dirname,fileprefix,ant,fmt)
    print "  Saving %s." % filename
    savefig( filename ) 

def plotivcurve( dirname, Ij, Vj, TpAmb=None, TpSky=None,\
                 Yfac=None, fileprefix='', fmt='pdf', showGUI=False ):
    """ Plot iv curve for input Ij and Vj values as well as optionally plot
    total power and y factor values.  Outputs a summary plot containing all
    antennas as well as a single plot for each antenna.
    Parameters:
        dirname Absolute path to directory to store resulting plots in.
        Ij Dict of Ij arrays keyed by antenna 
        Vj Dict of Vj arrays keyed by antenna 
        TpAmb Dict of TpAmb arrays keyed by antenna 
        TpSky Dict of TpSky arrays keyed by antenna 
        Yfac Dict of y factor arrays keyed by antenna 
        fmt File output format.
        showGUI Show plots on GUI if True.
    """
    ioff()

    for ant in Ij.iterkeys():

        if TpAmb != None and TpSky != None and Yfac != None:
            plotsingle( ant, dirname, Ij[ant], Vj[ant], TpAmb[ant],\
                        TpSky[ant], Yfac[ant], fileprefix, fmt )
        else:
            plotsingle( ant, dirname, Ij[ant], Vj[ant],\
                        fileprefix=fileprefix, fmt=fmt )

    nAnts = len( Ij.keys() )
    if nAnts > 1:
        plotsummary( dirname, Ij, Vj, TpAmb, TpSky, Yfac, \
                     fileprefix=fileprefix, fmt=fmt )

    if showGUI:
        show()

def ivcurve( rx, start, stop, step, delta, power, \
             ants=0, fmt='pdf', dir=None, showGUI=True ):
    """Run an IV curve on the selected antennas and receivers. 
    Results are stored first in /array/rt/ivcurve or if not available 
    /tmp/ivcurve.
    Parameters:
        rx Run on specified receiver, one of RX1MM or RX3MM.
        start Vj start in milliVolts.
        stop Vj stop in milliVolts.
        step Vj step in milliVolts.
        delta Time delta in ms - rounded to nearest 100ms increment.
        power In addition to running an IV cruve also collect and plot antenna 
              IF total power information.  This puts the load in & out and plots
              the associated total powers and y factor.
        ants List of antennas to simultaneously run on. 
        fmt Output format, one of 'eps', 'pdf', 'png', 'ps' or 'svg'.  Defaults
            to 'pdf'.
        dir Directory to store output files in.  If not specified, defaults to
              /array/rt/ivcurve/ or /tmp if that doesn't exist.  Specified 
              directory must be writable.
        showGUI If True show the summary plot in an X windows gui.  
    """
    if ants == 0:
        antlist = currentAntennaNumbers()
    else:
        antlist = helper.makeList(ants)
    
    # Calculate timeout as the total IV curve run time plus 2s for housekeeping.
    timeout = ( delta / 1000.0 ) * ( abs( start - stop ) / abs( step ) ) + 2.0 

    # These and subsequent data structures are dictionaries of 
    # numpy arrays keyed by antenna number thank you.
    TpAmb = None 
    TpSky = None 
    Yfac = None 
    if power: 
        print " Placing ambient load into beam."
        amb( antlist )
        print " Initiating iv curve with ambient load in beam." 
        s.doIVcurve( rx, start, stop, step, delta, power, antlist )
        readyLists = s.wait( carma.control.WAIT_TUNED, antlist, timeout, \
                             carma.control.WAIT_ALL, -2)
        TpAmb = dict( [ (ant,array( s.getIVcurve( ant ).totPower )) \
                        for ant in antlist ] )
        print " Removing ambient load from beam."
        
    sky( antlist )

    print " Initiating iv curve on the sky." 

    s.doIVcurve( rx, start, stop, step, delta, power, antlist )
    readyLists = s.wait( carma.control.WAIT_TUNED, antlist, timeout, \
                         carma.control.WAIT_ALL, -2)
    if power:
        TpSky = dict( [ (ant,array(s.getIVcurve( ant ).totPower)) \
                        for ant in antlist ] )

        # Check that TpSky and TpAmb sizes match in all dimensions
        Yfac = dict( [ (ant,zeros( TpSky[ant].shape, float )) \
                       for ant in antlist] )
        for ant in antlist:
            Yfac[ant] = TpAmb[ant]/TpSky[ant]

    # Package up the data into a more digestible form, in this case our
    # dictionaries of arrays keyed by antenna.
    FJD = {}
    Ij = {} 
    Vj = {} 
    for ant in readyLists.ready:
        ivpoints = s.getIVcurve( ant )
        FJD[ant] = array( [ x.fjd for x in ivpoints.ivSequence ] )
        Ij[ant] = array( [ x.Ij for x in ivpoints.ivSequence ] )
        Vj[ant] = array( [ x.Vj for x in ivpoints.ivSequence ] )

    # Create path to store data in
    now = time.localtime()
    if rx == RX1MM:
        rxStr = "1mm"
    elif rx == RX3MM:
        rxStr = "3mm"

    if dir is None:
        absPathDataDir = getDataPath( 'ivcurve' )
    else:
        absPathDataDir = os.path.abspath( dir )

    absPathDataDir = os.path.normpath( absPathDataDir )

    if os.access( absPathDataDir, os.W_OK ):
        print " Plots will be saved in %s." % absPathDataDir
    else:
        print "You do not have permission to write to %s." % absPathDataDir
        return

    filePrefix= "%s-%s" % (rxStr,time.strftime("%y-%B-%d-%H:%M", now))

    if len( readyLists.ready ) > 0: 
        plotivcurve( absPathDataDir, Ij, Vj, TpAmb, TpSky, Yfac, \
                     filePrefix, fmt, showGUI )
    else:
        print "ERROR: All antennas timed out while waiting." 

