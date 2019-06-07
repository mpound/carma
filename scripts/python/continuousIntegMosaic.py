# @file
#
# @author Steve Scott
# $Id: continuousIntegMosaic.py,v 1.36 2014/10/15 21:27:56 scott Exp $
#
# $CarmaCopyright$

from subarrayCommands import *


DIRECTORY_MOSAICS    = '/array/rt/mosaics/'

def _mymosaicsetup(tune=False):
    newProject('ct011', 'mostest', '')
    intent(queryString("Control.subarray"+str(subarrayNo)+".source"), 
            'S',True,False)
    radioInit(clearastrobands=False)
    if tune:
        freq(100, USB, 3,NONE)
        configwideastroband()
        checkbands()
    elif False :
        configwideastroband()

class Mosaic(object):
    def __init__(self, fieldtime, rows, fieldsInRow, delta=0.5, pa=0.0, 
            maxtime=20.0, maxreps=0, startFieldIndex=0, trkThreshold=0.10,
            slewtime=1.5, rowOffset=0.0, colOffset=0.0, userlist=None ):
        """ The mosaic is a quasi-rectangle with rows of equally spaced fields
        centered on the current source position.
        The fields are ordered from the upper left, then moving along a row
        from left to right, one field to another. The next row is done right
         to left, with the pattern repeating to give an efficient serpentine 
        traversal of the fields. The rows are spaced by root(3)/2 of the field
        spacing and even rows have one additional field and are offset 
        by half a field to make all fields equally spaced from any adjacent
        field. By default the mosaic is repeated ad infinitum, with even 
        repetitions in the reverse direction from the odd. This gives fields
        with no large discontinuities.
        The code internally uses the equatOffset command and moves the 
        phase center as well as the pointing center.
        Parameters:
         fieldtime: integration time on each field (secs). The actual data
             integration time will be the sum of the slewtime and fieldtime.
         rows: number of rows of fields
         fieldsInRow: number of fields in odd numbered rows (even have one more)
         delta: field spacing in arcminutes. Default is 0.5.
         pa: position angle of the rectangle in degrees. The default, 0. gives
             a rectangle with rows in RA. Increasing pa rotates the rectangle
             around the nominal source position from north toward east.
        maxtime: the maximum time in minutes to take data. This allows 
             calibrations to be scheduled at fixed intervals.
        maxreps: the maximum number of repeats of the mosaic. If both maxreps
             and maxtime are specified the first one to complete wins.
        startFieldIndex: index of first field, defaults to 0
        trkThreshold: tracking threshold in fractional beamwidths,
             default is 0.10. 
        slewtime: estimated time between fields for the antennas to move (secs).
             The data at the start of the integration during the slew should
             be blanked because of tracking.     
        rowOffset: An offset in arcminutes to add to the center y position,
                   allowing a non-centered rectangle.  Default: 0
        colOffset: An offset in arcminutes to add to the center x position,
                   allowing a non-centered rectangle.  Default: 0
        userlist:  An optional grid file with positions to be used instead of 
                   having this class compute the grid.  This allows 
                   for non-rectangular grids.  The file must be in the
                   standard CARMA grid file format: two columns of dRA and dDEC
                   in arcminutes. The default directory path for the file 
                   is /array/rt/mosaics. 

                   NOTE: If userlist is given (i.e. not None), then rows, 
                   fieldsInRow, delta, pa, rowOffset, and colOffset are ignored.
        """
        self.fieldtime     = fieldtime
        self.nrows         = rows
        self.ncols         = fieldsInRow
        self.delta         = delta
        self.pa            = pa
        self.maxtime       = maxtime
        self.slewtime      = slewtime
        self.maxreps       = maxreps
        self.trkThreshold  = trkThreshold
        self.fieldIndex    = startFieldIndex
        self.rowOffset     = rowOffset
        self.colOffset     = colOffset
        self.userlist      = userlist
        self._makePositions(rows, fieldsInRow, delta, pa)
        self.nFields       = len(self.mosaicFields)
        self.maxFieldIndex = maxreps*self.nFields
        if self.userlist == None :
            m = "Creating a %dx%d mosaic with %d positions at pa=%.1fdeg" \
                    %(rows, fieldsInRow, self.nFields, pa)
        else:
            m = "Creating mosaic with %d positions from user file %s" \
                    %(self.nFields,self.userlist)
        print m
        scriptlog(m)

    # copied from obsdefUtils
    def setMosaicFile(filename):
        """ Set input mosaic file including path name """
        inputFile = string.strip(filename)
        if inputFile.find('/') != 0:
            inputFile = DIRECTORY_MOSAICS + inputFile
        return inputFile

    def _makePositions(self, nrows, ncols, delta, pa) :    
        self.mosaicFields = list()
        if self.userlist == None:
            for r in range(nrows) :
                firstRow = (r==0)
                odd  = (r & 1) == 1
                even = (r & 1) == 0
                nRAfields = ncols
                if odd : nRAfields = ncols+ 1
                deco = self.rowOffset + delta*math.sqrt(3.0)/2*(0.5*(nrows-1)-r)
                for d in range(nRAfields):
                    if even: rao = self.colOffset+delta*(d-0.5*(nRAfields-1))
                    else   : rao = self.colOffset+delta*(0.5*(nRAfields-1)-d)
                    # Now rotate by pa
                    r = math.hypot(rao, deco)
                    a = math.atan2(deco, rao) - pa*math.pi/180
                    decopa = r*math.sin(a)
                    raopa  = r*math.cos(a)
                    self.mosaicFields.append([raopa, decopa])
        else:
            inputFile = utils.setMosaicFile(self.userlist)
            if not os.path.exists(inputFile):
                raise Exception, 'Cannot find mosaic file '+inputFile

            # put the offsets in a array of float pairs
            with open(inputFile) as f:
                self.mosaicFields = [[float(x) for x in line.split()] for line in f]
            #print self.mosaicFields
            # Make sure offsets are a list
            if list not in [type(self.mosaicFields)]:
                raise Exception, 'Mosaic offsets must be a list'
        #print self.dumpFields()

    def dumpFields(self):
        """Return a string with each field position on a separate line as
        index, ra, dec offset triplets."""
        o = ""
        i = 0
        for f in self.mosaicFields:
            o += "%3d: %6.2f, %6.2f\n" %(i, f[0],f[1])
            i += 1
        return o
    def reset(self):
        """Reset the field index to zero to restart the mosaic from scratch"""
        self.fieldIndex = 0
    def _getField(self, completionMsg=False, loud=True):
        """Return ra/dec offset pair for current field. Also useful to print
        out info about mosaic start/stop."""
        maprep = (self.fieldIndex)/self.nFields
        fieldInThisRep = (self.fieldIndex)%self.nFields
        even = (maprep & 1) == 0
        if even: index = fieldInThisRep
        else :   index = self.nFields-fieldInThisRep-1
        if even: dir = "forward"
        else:    dir = "reverse"
        bar  = "=========================================="
        m = ""
        if completionMsg:
            if fieldInThisRep == (self.nFields-1):
                m = "Completing mosaic rep %d\n" %(maprep+1)
                m += bar
        else:
            if self.fieldIndex == 0:
                m  = bar + "\n"
                m += "Starting first rep (forward direction)"
            elif fieldInThisRep == 0 :
                m = bar + "\n"
                m += "Starting mosaic rep %d (%s direction)" %(maprep+1,dir)
        if loud and len(m) > 0:
            print m
            scriptlog(m)        
        return self.mosaicFields[index]
        
    def _takeData(self, maxtime, waitTracking, subarray, fastSample, 
                mpInvalidate, blanktime) :
        """ Take data until maxtime or maxreps is exceeded. Will not 
        necessarily stop on a mosaic boundary. Starts from current field
        position, which is either the start of the mosaic as the first 
        takeData, or where the previous takeData finished. This allows 
        data collection to be started/stopped/restarted for calibrations.
        The first field is special because it involves acquisition after
        a potential slew. After that, we loop on the fly, changing the
        equatoffsets. At the offset change special shenanigans are done to
        make sure that the header information is correct for each field.
        This is done by setting the tracking tolerance to zero just before
        the transition so that all of the data are blanked. At the same time
        all of the critical header variables (ra/dec/offsets) are set invalid.
        After allowing for the new offsets to be tracked, the tracking 
        thresholds are set to normal values and the header variables are
        declared valid.
         Return: index of next field to be done
         Parameters:
         maxtime: maximum time (mins) to spend on this takeData invocation
            Zero means unlimited. 
         waitTracking: controls waiting for antennas to track; 
            when False waits for fixed slew time
         subarray:
         fastSample: When true, does 0.5sec integrations for debugging
         mpinvalidate:Turn of/off prewriter invalidation of monitor data 
            when visdata are blanked. Default is True, meaning invalidation 
            is on.
         blanktime:  """
        # Setup stuff
        hasPipeline=True # Set to False for sandbox testing
        if not hasPipeline:
            print "**********************************************"
            print "****DANGER: running with hasPipeline=False****"
            print "**********************************************"
        m  = "ContinuousMosaic.takeData(maxtime=%.2f) " %maxtime
        print m
        commandlog(m)   
        dbgtt       = False  # Control tracking threshold debug prints
        if maxtime == None: maxtime = self.maxtime
        if (maxtime <= 0) and (self.maxreps <= 0) :
            m = "Both maxtime and maxMosaics cannot be unlimited "
            m += "which is done by setting them to zero."
            raise Exception, m
        looptime      = 0.05 # Polling loop time, in seconds
        # Length of blanking (secs) when doing field slew
        blankDuration = blanktime  
        blankOffset   = 0.5
        fieldtime     = self.fieldtime
        slewtime      = self.slewtime
        iterationTime = fieldtime+slewtime # Time between consecutive fields
        nFields       = self.nFields
        if self.maxFieldIndex > 0:
            # Fields left in all reps of the mosaic
            fieldsLeft = self.maxFieldIndex - self.fieldIndex
        else:
            # Fields left in this mosaic
            fieldsLeft = nFields - self.fieldIndex
        if (maxtime > 0.0) and (maxtime*60 < fieldtime/2.0):
            m  = "maxtime (%.1f secs) is less than time to do " %(maxtime*60.0)
            m += "half of a single field (%.01f secs); Exiting" %(fieldtime/2.0)
            print m
            return self.fieldIndex
        # Integration time
        if fastSample : itime         = 0.5     
        else          : itime         = iterationTime     
        tbegin    = time.time()
        trackThreshold(self.trkThreshold)
        
        src = queryString("Control.Subarray"+str(subarrayNo)+".source")
        #intent(src, 'S',True,False)
        
        # Go to first field position
        f0   = self._getField()
        rao  = f0[0]
        deco = f0[1]
        equatOffset(rao, deco)
        if waitTracking:
            c1 = "Waiting for most antennas to acquire source"
            c2 = "Most antennas acquired"
            tmo = 500 # In seconds, to allow for a long slew
            wait(TRACK, tmo=tmo, waiton=-2, precomment=c1, postcomment=c2)
            c1 = "Waiting for last two antennas to acquire source"
            c2 = "Last two antennas acquired"
            tmo = 4 # Give stragglers a second chance for a few more seconds
            wait(TRACK, tmo=tmo, waiton=0, precomment=c1, postcomment=c2)
        else:
            wait(TIME, tmo=slewtime, subarray=subarray)

        # More setup for this set of fields        
        telapsed   = time.time()-tbegin
        if (maxtime > 0.0) and (maxtime*60 <= iterationTime+telapsed):            
            m ="There is not enough time to do one field; "
            m += "maxtime=%.0f secs, " %(maxtime*60)
            m += "elapsed time for setup=%.0f secs, " %telapsed
            m += "and time for a field=%.1f secs.\n" %iterationTime
            m += "But we will ignore maxtime as a single field is "
            m += "done if possible."
            #print m
        mf        = (maxtime*60.0-telapsed)/iterationTime
        maxtimefields = max(int(round(mf)), 0)
        if self.maxFieldIndex > 0:
            # Fields left in all reps of the mosaic
            totalFieldsLeft = self.maxFieldIndex - self.fieldIndex
            fieldsLeft = min(totalFieldsLeft, maxtimefields)
            fieldsLeftIsTimeFields = False
        else: 
            # Number of mosaics is unlimited, so just use time   
            fieldsLeft = maxtimefields
            fieldsLeftIsTimeFields = True
        if fieldsLeft <= 0:
            if fieldsLeftIsTimeFields :
                m  = "ContinuousMosaic: "
                m += "The maxtime specified (%.1f secs) " %(maxtime*60)
                m += "is not enough to do half a field. " 
                m += "After the slew there is time for %.2f fields." %mf
                commandlog(m)
                print m
            else : 
                print "No fields left to do; Exiting"
            return self.fieldIndex
        reps = int(round((fieldsLeft*iterationTime)/itime))
        
        # Kick off the continuous integrations through all fields
        rtdComment("Integrating...") 
        multiSubarray('integrate', subarray, itime, reps, 0.0, True)
        tintegstart = time.time()
        olddeco = deco
        field     = 0
        prefield  = 0
        postfield = 0
        if mpInvalidate : notword = ""
        else :            notword = " NOT"
        m  = "Monitor points will%s be invalidated " %notword
        m += "when visibilities are blanked."
        print m
        print "%5.1f" %0.0, "%2d" %field, "%5.2f" %rao, "%5.2f" %deco
        t0 = time.time() + 0.80   # The last time offset is from pipeline
        hasSlew = False
        while(postfield < (fieldsLeft-1)) :
            hasSlew = True
            now  = time.time()
            t    = now - t0
            nextField     = int(t/iterationTime)
            nextPrefield  = int((t+blankOffset)/iterationTime)
            nextPostfield = int((t+blankOffset-blankDuration)/iterationTime)
            #print "%.1f" %t, field, nextPrefield, nextField, nextPostfield
            if (nextPrefield > prefield) :
                prefield = nextPrefield
                trackThreshold(0.0)
                if dbgtt: print "%5.1f" %t, "Start blanking everything" 
            if (nextField > field) :
                field = nextField
                self._getField(completionMsg=True)
                self.fieldIndex += 1
                f = self._getField()
                rao  = f[0]
                deco = f[1]
                if (mpInvalidate) :
                    setInvalidationForMosaics(True)
                # EQUATOFFSET applied here!!
                equatOffset(rao, deco, whileIntegrating=True)
                #
                tslewstart = time.time()
                firstFieldInRep = (self.fieldIndex%self.nFields) == 0
                if abs(olddeco-deco) < 0.0001 and not firstFieldInRep: 
                    decostr = ""
                else: 
                    decostr = "%5.2f" %deco
                olddeco = deco
                print "%5.1f" %t, "%2d" %field, "%5.2f" %rao, decostr
            if (nextPostfield > postfield) :
                postfield = nextPostfield
                trackThreshold(self.trkThreshold)
                if (mpInvalidate) :
                    setInvalidationForMosaics(False)
                if dbgtt: print "%5.1f" %t, "Normal blanking" 
            sleep(looptime)
        if hasPipeline:
            pc ="Integration complete"
            rtn = wait(INTEG, postcomment=pc, subarray=subarray)
        else : 
            rtn = wait(TIME, tmo=fieldtime, subarray=subarray)
        self._getField(completionMsg=True)
        # Set index to next one to be done
        self.fieldIndex += 1
        if hasSlew: lastField = time.time() - 1.0 - tslewstart
        else:       lastField = time.time() - 1.0 - tintegstart
        m  = "ContinuousMosaic: Length last field integration: "
        m += "%.2f secs" %lastField
        commandlog(m)
        print(m)
        m = "ContinuousMosaic: Total time: %.1fsecs" %(time.time()-tbegin)
        commandlog(m)
        print(m)
        return self.fieldIndex

    def takeData(self, maxtime=None, waitTracking=True,
            subarray=DEFAULT, fastSample=False, mpInvalidate=True, blanktime=1.0 ) :
        """ Take data until maxtime or maxreps is exceeded. Will not 
        necessarily stop on a mosaic boundary. Starts from current field
        position, which is either the start of the mosaic is the first 
        takeData, or where the previous takeData finished. This allows 
        data collection to be started/stopped/restarted for calibrations.
        Returns index of next mosaic field to be done (indices start with zero).
        Parameters:
         fastSample: when True takes data every frame for debugging. 
            Default is False.
         mpInvalidate: Turn of/off prewriter invalidation of monitor data 
            when visdata are blanked. Default is True, meaning invalidation 
            is on.
         blanktime: Length of blanking (secs) when doing field slew. 
            Default: 1.0
        """
        # The purpose of this function to make sure the tracking threshold
        # is reset after an exception of any kind.
        try:
            f = self._takeData(maxtime, waitTracking, subarray, fastSample, 
                    mpInvalidate, blanktime)
        except Exception, ex:
            trackThreshold(self.trkThreshold)
            raise
        return f    
            
        
