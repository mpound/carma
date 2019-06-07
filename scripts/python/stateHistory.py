# @file
# Routines to query and display system state changes.
# 
# @author Andy Beard
# $Id: stateHistory.py,v 1.27 2013/04/19 15:47:19 abeard Exp $
# 
# $CarmaCopyright$
#

import carma 
from carmaHelpers import frame2string, string2frame, makeList

class StateCategory( object ) :
    """Class to define a category of system state data."""

    def __init__( self, mpList, mpRange, columnlabels, 
                  rowlabel, rowprefix, rowRange, fmttuple ):
        self.mpNames = mpList
        self.mpRange = mpRange
        self.columnLabels = columnlabels
        self.formatTuple = fmttuple # date format tuple (width, precision, type)
        self.rowLabel = rowlabel
        self.rowPrefix = rowprefix
        self.rowRange = rowRange 
        self.maxRows = len(rowRange)

        # Create our header
        if self.maxRows != len( self.mpRange ):
            errMsg = "StateCategory.__init__: mpRange and rowRange lists "
            errMsg += "must have the same size."
            raise Exception, errMsg

        # Pick the larger of either the widths or column headers
        maxLabelWidth = max( [ len( s ) for s in self.columnLabels ] )
        if maxLabelWidth > self.formatTuple[0]:
            self.formatTuple = ( maxLabelWidth, 
                                 self.formatTuple[1],
                                 self.formatTuple[2] )
        
        header = self.rowLabel + " "
        for label in self.columnLabels:
            header += "%s "%label.center( self.formatTuple[0] )

        self.header = header
        

    def getMpListForQuery( self, items=0 ):
        if self.maxRows > 0:
            # Form up a list of all MPs with the 'item' number filled in
            # However, we must map rowRange index to mpRange index to retrieve
            # proper mp.  Yes I know the naming of this has gotten out of 
            # control - forgive me, I have sinned. 
            if items == 0: 
                items = self.mpRange
            else:
                for item in items:
                    if item not in self.rowRange:
                        errMsg = "Ant or item index %d not valid for "%item
                        errMsg += "category, must be one of %s"%self.rowRange
                        raise Exception, errMsg 
                        
                items = [ self.rowRange.index( item ) + 1 for item in items ]
                

            mpsPerRow = len( self.mpNames ) 
            canonMpList = []
            for item in items: 
                canonMpList += [ canonName%item for canonName in self.mpNames ]
            return canonMpList
        else:
            return self.mpNames

    def formatQueryValues( self, config, values, frame, fmt, items=0 ):

        from printFunctions import FG_COLOR_RED, ATTR_OFF

        gmtString = frame2string( frame, fmt ) + " (%s Array)"%config
    
        if items == 0: items = self.rowRange
         
        centered = gmtString.center( len( self.header ) )

        print "\n%s"%centered
        print self.header

        numRows = len( items )
        mpsPerRow = len( self.mpNames )

        # Check that the # of values matches the input number of ants
        expectedValues = numRows * mpsPerRow
        if len( values ) != expectedValues:
           errMsg = "Number of values (%d) does not match expected (%d)."
           errMsg = errMsg%( len( values ), expectedValues )
           raise Exception, errMsg

        # Good, it does, now use indexing tricks to format and output 

        for itemIdx in range(0, numRows):
            itemNo = items[itemIdx] 
            valBaseIdx = itemIdx * mpsPerRow
        
            # Form up line and colorize changed entries
            rowFmt = "%s%d:"
            rowLabel = rowFmt%(self.rowPrefix,itemNo)
            rowLabelWidth = max( [ len(self.rowLabel),
                                   len(rowFmt%(self.rowPrefix, self.maxRows ))])
            lineFormat = "%*s"%(rowLabelWidth,rowLabel)
            vals = []
            for v in values[ valBaseIdx: valBaseIdx + mpsPerRow ]:
                # Monitor strings in carma are padded with up to 8 NULLs
                if v.tmv._d == carma.monitor.MONITOR_VALUE_TYPE_STRING:
                    # Strip off crap by forcing that we only handle chars.
                    v.tmv._v = v.tmv._v[0]

                vals.append( v.tmv._v )

                if v.changed: # If this value changed, colorize it.
                    lineFormat += "%s%%%d.%d%c%s "%( FG_COLOR_RED, 
                                                     self.formatTuple[0],
                                                     self.formatTuple[1],
                                                     self.formatTuple[2],
                                                     ATTR_OFF )
                else: # If it didn't change, don't colorize, just print
                    lineFormat += "%%%d.%d%c "%( self.formatTuple[0],
                                                 self.formatTuple[1],
                                                 self.formatTuple[2] )
                    
            print lineFormat%tuple( vals )
    
STATE_ARRAY_CONFIG = StateCategory(
    mpList = [ "Control.Subarray%d.configName" ],
    mpRange = range(1,3),
    columnlabels = ["Config"],
    rowlabel = "Subarray", rowprefix ="Sub", rowRange = range(1,3), 
    fmttuple = (7,2,'s') )

STATE_OPTICAL = StateCategory( 
    mpList = [ "Control.Antenna%d.ApertureOptical.PointingConstants." + mp 
      for mp in  [ "azOffset", "elOffset", "sag" ] ], 
    mpRange = range(1,24),
    columnlabels = ["AzOffset", "ElOffset", "Sag"],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1,24), 
    fmttuple = (5,2,'f') )

STATE_MOUNTOFFSET = StateCategory( 
    mpList = [ "Control.Antenna%d." + mp 
      for mp in  [ "azimuthMountOffset", "elevationMountOffset"] ], 
    mpRange = range(1,24),
    columnlabels = ["AzOffset", "ElOffset"],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1,24), 
    fmttuple = (5,2,'f') )

STATE_RADIO3MM = StateCategory( 
    mpList = [ "Control.Antenna%d.Aperture3mm.PointingConstants." + mp 
      for mp in  [ "azOffset", "elOffset", "sag" ] ], 
    mpRange = range(1,24),
    columnlabels = ["AzOffset", "ElOffset", "Sag"],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1,24), 
    fmttuple = (5,2,'f') )

STATE_RADIO1MM = StateCategory( 
    mpList = [ "Control.Antenna%d.Aperture1mm.PointingConstants." + mp 
      for mp in  [ "azOffset", "elOffset", "sag" ] ], 
    mpRange = range(1,24),
    columnlabels = ["AzOffset", "ElOffset", "Sag"],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1,24), 
    fmttuple = (5,2,'f') )

STATE_RADIO1CM = StateCategory(
    mpList = [ "Control.Antenna%d.Aperture1cm.PointingConstants." + mp
        for mp in [ "azOffset", "elOffset", "sag" ] ],
    mpRange = range(1,24),
    columnlabels = ["AzOffset", "ElOffset", "Sag"],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1,24), 
    fmttuple = (5,2,'f') )

STATE_OVRO_MOUNT = StateCategory(
   mpList = [ "Control.Ovro%d." + mp for mp in [ "azEncoderOffset", 
     "elEncoderOffset", "axisNonOrthogonality", "northSouthAzAxisVerticality", 
     "eastWestAzAxisVerticality"] ], 
   mpRange = range(1,7),
   columnlabels =
    [ "AzEncOff", "ElEncOff", "AxisNonOrth", "NSAxisVert", "EWAxisVert"], 
   rowlabel = "Ant", rowprefix = "C", 
   rowRange = range(1,7), 
   fmttuple = (6,3,'f') )

STATE_BIMA_APC = StateCategory(
    mpList = [ "Control.Bima%%d.apc%d"%(c) for c in xrange(1, 10) ],
    mpRange = range(1,10),
    columnlabels = [ "apc%d"%c for c in xrange(1, 10) ], 
    rowlabel = "Ant", rowprefix = "C",
    rowRange = range(7, 16), 
    fmttuple = (7,3,'f') )

STATE_BIMA_EPC = StateCategory(
    mpList = [ "Control.Bima%%d.epc%d"%(c) for c in xrange(1, 10) ],
    mpRange = range(1,10),
    columnlabels = [ "epc%d"%c for c in xrange(1, 10) ], 
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(7, 16), 
    fmttuple = (7,3,'f') )

STATE_SZA_MOUNT_ENCODER_AZ = StateCategory(
    mpList = [ "Control.Sza%d." + mp for mp in [ 
        "azEncoderCountsPerTurn", "azMinEncoderCount", 
        "azMaxEncoderCount", "azEncoderZero" ] ],
    mpRange = range(1,9),
    columnlabels =
         [ "Az Counts/Turn", "Az Min Count", "Az Max Count", "Az Zero" ],
    rowlabel = "Ant", rowprefix = "C", 
   rowRange = range(16,24), 
   fmttuple = (7,5,'f') )

STATE_SZA_MOUNT_ENCODER_EL = StateCategory(
    mpList = [ "Control.Sza%d." + mp for mp in [ 
        "elEncoderCountsPerTurn", "elMinEncoderCount", 
        "elMaxEncoderCount", "elEncoderZero" ] ],
   mpRange = range(1,9),
   columnlabels =
    [ "El Counts/Turn", "El Min Count", "El Max Count", "El Zero" ],
   rowlabel = "Ant", rowprefix = "C", 
   rowRange = range(16,24), 
   fmttuple = (7,5,'f') )

STATE_SZA_MOUNT_TILT = StateCategory(
    mpList = [ "Control.Sza%d." + mp for mp in [ 
        "haTilt", "latTilt", "elTilt" ] ], 
   mpRange = range(1,9),
   columnlabels =
    [ "Hour Angle Tilt", "Latitude Tilt", "El Tilt" ],
   rowlabel = "Ant", rowprefix = "C", 
   rowRange = range(16,24), 
   fmttuple = (7,4,'f') )

STATE_SZA_MOUNT_OPTICAL = StateCategory(
    mpList = [ "Control.Sza%d." + mp for mp in [ 
        "opticalXCollimation", "opticalYCollimation", 
        "opticalFlexureSin", "opticalFlexureCos" ] ], 
   mpRange = range(1,9),
   columnlabels =
    [ "X Collimation", "Y Collimation", "Flexure Sin", "Flexure Cos" ],
   rowlabel = "Ant", rowprefix = "C", 
   rowRange = range(16,24), 
   fmttuple = (7,5,'f') )

STATE_SZA_MOUNT_RADIO = StateCategory(
    mpList = [ "Control.Sza%d." + mp for mp in [ 
        "radioXCollimation", "radioYCollimation", 
        "radioFlexureSin", "radioFlexureCos"] ],
   mpRange = range(1,9),
   columnlabels =
    [ "X Collimation", "Y Collimation", "Flexure Sin", "Flexure Cos" ],
   rowlabel = "Ant", rowprefix = "C", 
   rowRange = range(16,24), 
   fmttuple = (7,5,'f') )

STATE_TILT = StateCategory(
    mpList = [ "Control.Antenna%d." + mp 
               for mp in [ "aftForwardTiltZero", "leftRightTiltZero" ] ],
    mpRange = range(1,24),
    columnlabels = [ "AftForZero", "LeftRightZero" ],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1, 24), 
    fmttuple = (7,3,'f') )

STATE_ANT_OFFSET = StateCategory(
    mpList = [ "Control.Antenna%d.AntennaOffset." + mp
               for mp in [ "east", "north", "up" ] ],
    mpRange = range(1,24),
    columnlabels = [ "East", "North", "Up" ],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1, 24), 
    fmttuple = (7,1,'f') ) 

STATE_ANT_AXIS_NON_INTERSECTION = StateCategory( 
    mpList = [ "Control.Antenna%d.axisNonintersection" ],
    mpRange = range(1,24),
    columnlabels = [ "Axis-Non-Intersection" ],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1, 24), 
    fmttuple = (7,3,'f') ) 

STATE_FOCUS = StateCategory( 
    mpList = [ "Control.Antenna%d.focus" + mp for mp in [ "X", "Y", "Z" ] ],
    mpRange = range(1,24),
    columnlabels = [ "Focus X", "Focus Y", "Focus Z" ],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1, 24), 
    fmttuple = (7,1,'f') ) 

STATE_ROTATION_AND_FOV = StateCategory(
    mpList = [ "Control.Antenna%d." + mp 
               for mp in [ "cameraRotation", "cameraAzFOV", "cameraElFOV" ] ],
    mpRange = range(1,24),
    columnlabels = [ "Rotation", "Az FOV", "El FOV" ],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1, 24), 
    fmttuple = (7,2,'f') ) 

STATE_OVRO_REF_ATTEN = StateCategory(
    mpList = [ "Control.Antenna%d.refAtten" ],
    mpRange = range(1,7),
    columnlabels = [ "Ref Atten" ],
    rowlabel = "Ant", rowprefix = "O", 
    rowRange = range(1,7),
    fmttuple = (5,1,'f') ) 

STATE_PAD = StateCategory(
    mpList = [ "Control.Antenna%d.padNumber" ], 
    mpRange = range(1,24),
    columnlabels = [ "Pad #" ],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1,24),
    fmttuple = (4,0,'d') )

STATE_PAD_OFFSETS = StateCategory(
    mpList = [ "Control.Antenna%d.PadOffset." + mp 
               for mp in [ "east", "north", "up" ] ],
    mpRange = range(1,24),
    columnlabels = [ "East", "North", "Up" ],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1,24),
    fmttuple = (11,3,'f') )

STATE_PAD_INFO = StateCategory(
    mpList = [ "Control.Antenna%d.padNumber", 
               "Control.Antenna%d.PadOffset.east",
               "Control.Antenna%d.PadOffset.north",
               "Control.Antenna%d.PadOffset.up" ],
    mpRange = range(1,24),
    columnlabels = [ "Pad #", "East", "North", "Up" ],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1,24),
    fmttuple = (11,2,'f') )

STATE_DELAYS = StateCategory(
    mpList = [ "Control.Antenna%d.delayOffset3mmRx",
               "Control.Antenna%d.rxDelay3mmPol2",
               "Control.Antenna%d.rxDelay1mmPol1",
               "Control.Antenna%d.rxDelay1mmPol2",
               "Control.Antenna%d.rxDelay1cmPol1",
               "Control.Antenna%d.rxDelay1cmPol2" ],
    mpRange = range(1,24),
    columnlabels = [ "Delay", "Diff3mmPol2", "Diff1mmPol1", "Diff1mmPol2", "Diff1cmPol1", "Diff1cmPol2" ],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1,24),
    fmttuple = (11,3,'f') )

STATE_CORR_INPUTS = StateCategory(
    mpList = [ "Control.Antenna%d.correlatorInputNumber" ],
    mpRange = range(1,24),
    columnlabels = [ "CorrInput" ],
    rowlabel = "Ant", rowprefix = "C", 
    rowRange = range(1,24),
    fmttuple = (4,0,'d') )

def filterFramesByConfig( framelist, config, state ):
    """Filter frames from input framelist that don't match input config."""
    print "Collecting info on config changes, this may take a minute..."

    configMp = ["Control.Subarray1.configName"]
    oldestFrame = min(framelist)
    newestFrame = max(framelist)
    configChangeFrames = state.getStateChangeFrames( configMp, 
                                                     oldestFrame,
                                                     newestFrame )
    if configChangeFrames.count( oldestFrame ) == 0:
        configChangeFrames.append( oldestFrame )
    if configChangeFrames.count( newestFrame ) == 0:
        configChangeFrames.append( newestFrame )

    configChangeFrames.sort()
    configChangeFrames.reverse()
    framelist.sort()
    answer = []

    rindex = len( framelist ) - 1
    for configChangeFrame in configChangeFrames:
        # Retrieve the config value at this time
        configValueList = state.getStateValues( configMp, configChangeFrame )
        configVal = configValueList[0].tmv._v.strip('\x00')

        while rindex >= 0:
            candidateFrame = framelist[rindex]
            if candidateFrame >= configChangeFrame:
                if configVal.upper() == config.upper():
                    answer.append( candidateFrame )
                rindex -= 1
            else:
                break

    answer.reverse()
    return answer
    
_defaultBeginTime = '1 year ago'
def stateHistory( category, ants=0, last=5, config=None, 
                  begin=_defaultBeginTime, end='now'):
    """Output date, time and values, of historical changes to a given category.
    Changes are output in most-recent-first order. Current categories are:
        STATE_ANT_AXIS_NON_INTERSECTION  STATE_PAD_INFO
        STATE_ANT_OFFSET                 STATE_PAD_OFFSETS
        STATE_ARRAY_CONFIG               STATE_RADIO1CM
        STATE_BIMA_APC                   STATE_RADIO1MM
        STATE_BIMA_EPC                   STATE_RADIO3MM
        STATE_CORR_INPUTS                STATE_ROTATION_AND_FOV
        STATE_DELAYS                     STATE_SZA_MOUNT_ENCODER_AZ
        STATE_FOCUS                      STATE_SZA_MOUNT_ENCODER_EL
        STATE_MOUNTOFFSET                STATE_SZA_MOUNT_OPTICAL
        STATE_OPTICAL                    STATE_SZA_MOUNT_RADIO
        STATE_OVRO_MOUNT                 STATE_SZA_MOUNT_TILT
        STATE_OVRO_REF_ATTEN             STATE_TILT
        STATE_PAD   

    Dates are represented in UTC using a flexible format specifying either an
    exact or relative date. Exact date format is 'ddMmmyyyy:HH:MM:SS'.  If a 
    portion is omitted, sensible defaults are used in its place.  Examples are:
     '24Sep2004:15:30:00' - 3:30 PM September 24th 2004.
     '01Jan2001' - Midnight January 1st 2001.
     '01Jan' - Midnight January 1st of this year.
     '2Dec09:1' - 1AM December 2nd of this year.
    Note the month is specified by exactly three letters.  

    Relative dates are specified with a number followed by 'days', 'months' or 
    'years' all of which may be abbreviated to d,m or y.  For example:
     '1d 2m' - 2 months and a day ago.
     '14m 1y' - 2 years, 2 months ago.
     '1 day 2 months 3 years ago' - Just that.

    Parameters:
     category: State category to query as described above.
     ants: Specify a single ant or all if 0.  Not pertinent to all categories.
     last: Number of last states to display when dates are NOT specified (5 
      by default).
     config: Limit search to a particular array configuration (A-E).
     begin: Begin date string in above described format (default 1 year ago).
     end: End date string in above described format (default now).
    
    Examples:
    The below examples print pad locations for C1 & C2 with varying criteria.
     stateHistory(STATE_PAD, ants=[1,2], begin='2 years ago', end='1 year ago')
     stateHistory(STATE_PAD, ants=0, config='A', begin='1Jan09', end='1Jan10' )
     """
    from device import getState
    try:

        if ants != 0: ants = makeList(ants)

        fmt="%d%b%Y:%H:%M:%S UT"
        
        if last < 1: 
            last = 1
        
        state = getState()

        defaultDateSpecified = ( begin == _defaultBeginTime and end == 'now' )
        sameDateSpecified = ( begin == end )

        if end == 'now' or end == None:
            newestFrame = state.getNewestStateFrame()
        else:
            newestFrame = string2frame( end )

        absOldestFrame = state.getOldestStateFrame()
        if begin == 'now':
            oldestFrame = newestFrame
        elif begin == None:
            oldestFrame = absOldestFrame 
        else:
            oldestFrame = string2frame( begin )

        # If begin and end options are mixed just switch them around quietly
        if oldestFrame > newestFrame:
            tmp = newestFrame 
            newestFrame = oldestFrame 
            oldestFrame = tmp

        # Make sure we can fulfill the request
        if newestFrame < absOldestFrame:
            absOldStr = frame2string( absOldestFrame, fmt )
            oldStr = frame2string( oldestFrame, fmt )
            newStr = frame2string( newestFrame, fmt )
            print "No saved state in range %s to %s."%(
                oldStr, newStr)
            print "Saved state is available starting %s."%absOldStr
            return

        # Since the input time format has only 1 second of resolution, make sure 
        # that our frame boundaries each encompasses a full second.
        if newestFrame % 2 == 0: 
            newestFrame += 1

        if oldestFrame %2 == 1:
            oldestFrame -= 1
             
        canonNames = category.getMpListForQuery( ants )
        changeFrames = state.getStateChangeFrames( canonNames, 
                                                   oldestFrame,
                                                   newestFrame )

        # Try now to think of things in terms of states instead of changes since
        # we now have an explicit list of states to display
        stateFrames = changeFrames

        # We generally want to also display the state at the beginning of the 
        # input time frame so tack on the oldestFrame if it isn't already 
        # one of the 'changed state' frames.
        if changeFrames.count( oldestFrame ) == 0:
            # However, don't tack on oldest frame if we don't have state for it.
            # Just tack on oldest known state frame.
            if oldestFrame <= absOldestFrame:
                stateFrames.insert( 0, absOldestFrame )
            else:
                stateFrames.insert( 0, oldestFrame )
        
        if config != None:
            stateFrames = filterFramesByConfig( stateFrames, config, state )

        # CORBA makes no guarantee about the ordering of sequence items so order 'em
        stateFrames.sort()
        stateFrames.reverse() # Now newest to oldest
        
        totalStates = len( stateFrames )
        
        if not defaultDateSpecified:  
            if totalStates > 10:
                last = 10
            else:
                last = totalStates

        if totalStates > last:
            undisplayedStates = stateFrames[last:]
        else:
            undisplayedStates = []

        # Display the states
        for frame in stateFrames[0:last]:
            changeValues = state.getStateValues( canonNames, frame )
            [array] = state.getStateValues(["Control.Subarray1.configName"], frame)
            array =  array.tmv._v.strip('\x00')
            category.formatQueryValues( array, changeValues, frame, fmt, ants )

        # Now summarize what else we have which wasn't displayed.
        undisplayedChanges = len( undisplayedStates )

        if undisplayedChanges == 0:
            oldStr = frame2string( oldestFrame, fmt )
            if sameDateSpecified:
                print "\nDisplayed known state on %s.\n"%(oldStr)
            else:
                newStr = frame2string( newestFrame, fmt )
                print "\nDisplayed all known states from %s to %s.\n"%( oldStr, 
                                                                        newStr)
        elif undisplayedChanges == 1:
            timeStr = frame2string( undisplayedStates[0], fmt )
            print "\n1 undisplayed state on %s.\n"%timeStr
        else:
            oldStr = frame2string( undisplayedStates[-1], fmt )
            newStr = frame2string( undisplayedStates[0], fmt )
            print "\n%d undisplayed states from %s to %s.\n"%(
                undisplayedChanges, oldStr, newStr)
        
        if undisplayedChanges > 0:
            print "To display undisplayed states use 'last' option or modify search criteria.\n"

        print "To modify search criteria, use 'begin', 'end', 'ants' and/or ",
        print "'config' options."

    except Exception, err:
        print err
