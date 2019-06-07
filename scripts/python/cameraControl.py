#
# @author Steve Scott
#
# $Id: cameraControl.py,v 1.98 2014/05/15 23:50:53 scott Exp $
#
# This requires the Python Image Library.
# Make sure that you have built the image library using the 
# carma tools python.
#
# CARMA Frame Grabber and optical telescope CCD control interface module
#  including use of centroiding
#
#

import Tkinter
import tkFont
import Image, ImageTk, ImageDraw
import carma
import time
import datetime
import array
import device
import stats
import Subarray
import os
import numarray
import math
import fcntl
from subarrayCommands import *
try:
  import pyfits
except:
  errstr = "No pyfits available: not a fatal error"
  print errstr


def logdir() :
    firstChoice = '/array/rt/opointLog'
    secondChoice = '/tmp'
    try :
        os.stat(firstChoice)
    except Exception :
        return secondChoice
    return firstChoice

class OpenLock(object):
  """helper class that acts like 'open' but locks the file"""
  def __init__(self,name,mode):
    self.name_ = name
    self.mode_ = mode
    self.fd_   = open(name,mode)
    fcntl.flock(self.fd_, fcntl.LOCK_EX)
  def write(self,s):
    self.fd_.write(s)
  def close(self):
    fcntl.flock(self.fd_, fcntl.LOCK_UN)
    self.fd_.close()

class CameraBase(object):
    """Create an optical camera; turn it on with on(), and turn it off
    by exiting from the GUI.
    This is an internal building block - don't use it.
     param ant - Carma antenna number
     param size - display size in percent of max image (default=60); 
        affects cpu usage, so don't make it too big - the default is good."""
    def __init__(self, ant, displaySize) :
        # For backward compatibility we allow the ant to  be an antref
        if isinstance(ant, device.Ant) == True:
            #raise Exception, "Parameter must be an Antenna number"
            print "Deprecated usage; use opticalSystem(carmaAntNumber)"
            self.ant_    = ant
            self.antnum_ = self.ant_.getCarmaAntnum()
        else :  # Normal input (new way)    
            self.antnum_ = ant
            self.ant_    = device.Carma(ant)
        self.antName_ = self.ant_.getName().capitalize() # e.g. Ovro3
        self.monsys_  = "%s.AntennaCommon.Drive.Point." %(self.antName_)
        # ----- Operational constants -----    
        # When measured offset is < this limit we apply offset and stop
        self.offsetQuitLimit_    = 10 # arcsec 
        # Define valid measured offset by magnitude of its errors (stddev)
        self.errLimit_           = 1.5  # In arcsec
        # If all the reps are bad and we hit this limit then quit early (fail)
        self.quitNoHopeRepLimit_ = 2
        self.dutyCycle_      = 100  # Percent time that grab+display can take
        self.maxFrameRate_   = 8.0  # In Hz
        self.brightness_     = 50
        self.contrast_       = 50
        self.zoom_           = 1   # GUI only
        # Centroiding only, determines image clipping
        # Will handle rotations up to 15deg, removing edges
        self.centroidZoom_   = 1.8 
        self.reticle_        = 128      # Reticle pixel value
        self.reticleOpacity_ = 40
        # Pixels per arcmin (default for high res image)
        self.screenScale_    = 480/10.0  
        self.zoom1NativeResolution_ = 200/10 # pixel/arcmin
        # ----- End operational constants -----    
        self.sac_   = Subarray.getSubarray()
        sacrefType = carma.control._objref_SubarrayControl
        if isinstance(self.sac_, sacrefType) == False: 
            raise Exception, "Invalid subarrayControl ref"
        self.object_ = ''
        self.displaySize_ = displaySize
        self.alive_ = False;
        self.canvas_ = 0
        self.root_ = 0
        self.imageHandle_ = 0
        self.updateInterval_ = 0  # In milliseconds
        self.deadTime_ = 0  # Dead time between frame grabs, in milliseconds
        self.timerID_ = 0
        self.tkimg_ = 0
        self.lastUpdateTime_ = 0
        self.frameRate_  = 0 # in Hz
        self.frameCount_ = 0
        self.contrastSlider_ = 0
        self.brightnessSlider_ = 0
        self.zoomSlider_ = 0
        self.reticleSlider_ = 0
        self.centroidButton_  = 0
        self.xmax_  = 0   # Max pixels, any resolution
        self.ymax_  = 0
        self.xres_  = 0   # Max pixels, current resolution
        self.yres_  = 0
        self.xdisp_ = 0  # Size of the display image, in pixels
        self.ydisp_ = 0 
        self.xcrop_ = 0  # Cropped image size 
        self.ycrop_ = 0        
        self.fieldOfView_             = 10.0   # size of smallest dimension, arcmin 
        self.zoom1ScreenResolution_   = 0      # pixel/arcmin
        self.nativeResolution_        = 200/10 # pixel/arcmin (recomputed later)
        self.zoom1Scale_              = 1.0    # pixel/arcmin
        self.scale_                   = 1.0    # to apply to croppped image to get to screen 
        self.zoomResolutionThreshold_ = 2.4    # High res above threshold, low below
        self.startMeasureCentroid_    = False  # Control flag
        self.measureCentroid_         = False  # Control flag
        self.applyCentroid_           = False  # Control flag
        self.centroidLoopCount_       = []     # Loop counters
        self.minSamples_              = None   # minimum number of samples for valid measurement. see function minsamples()
        self.centroidLoopMax_         = 16     # but never try more than these (some may fail)
        self.statsX                   = stats.Stats()
        self.statsY                   = stats.Stats()
        self.sum                      = []     # sum of all images  (this will be a numarray)
        self.nsum                     = 0      # number of images in sum
        self.auto_                    = False  # automatic(batch) or interactive
        self.maxAutoreps_             = 1      # number of repeats in Auto mode
        self.autoreps_                = 0      # counter for auto reps
        self.linger_                  = False  # leave cam on after auto completes
        self.lingerTime_              = 5      # Time to linger (seconds)
        self.showGUI_                 = True   # in batch mode we can turn this off?
        self.fitOK_                   = False  # status of centroid fit
        self.time_                    = 0.0    # wall clock time
        self.dontClose_               = False
        self.qmax_                    = 20     # max number of queries
        self.startTime_               = 0      # in seconds
        self.allBadReps_              = 0      # num reps with all frames bad
        self.azoff_                   = 0      # Applied offsets
        self.eloff_                   = 0      # Applied offsets
        self.azoffErr_                = 0      # Error in applied offsets
        self.eloffErr_                = 0      # Error in applied offsets
        self.initialAzoff_            = 0
        self.initialEloff_            = 0
        self.takeBackground_          = False # Take a background image and save
        self.subtractBackground_      = False  # Subtract background image before centroiding
        self.backgroundOffset_        = 2.0    # Arcmin
        self.takeBackground_          = False
        self.numBackgroundFrames_     = 1
        # results are [azoff,errazoff,eloff, erreloff, valid, zoom] all in arcmin
        self.results_                 = [0,0,0,0,False, 1.1] 
        self.normalize_               = True #Expand image to fill [0-255]
        self.selectApertVar_          = None
        self.logfname_  = logdir() + "/opointC" + str(self.antnum_) + ".log"
        self.lowResImage_             = None
        self.highResImage_            = None
        
    def on(self, auto, object, repeat, zoom, linger, showGUI, dontClose,
        brightness, apply, subtractBackground, ncoadd, dazbkg,
        centroidLoopMax, minsamples, verbose=False, opticalSystem=False):
        """Turn on the camera and start displaying frames; 
        see Camera class for parameter documentation"""
        if opticalSystem == False:
            print "Deprecated usage: use opticalSystem(carmaAntNumber)"
        tstart = time.time() # get start time
        self.alive_       = True
        self.auto_        = auto
        self.maxAutoreps_ = repeat
        self.centroidLoopCount_ = []
        self.centroidLoopCount_ = [ 0 for i in range(self.maxAutoreps_) ]
        self.object_      = object
        self.linger_      = linger
        self.brightness_  = brightness
        self.showGUI_     = showGUI
        self.dontClose_   = dontClose
        self.autoreps_    = 0
        self.allBadReps_  = 0
        self.subtractBackground_ = subtractBackground
        if auto and subtractBackground :
            self.takeBackground_     = True
        if self.takeBackground_ : 
            self.numBackgroundFrames_ = ncoadd
        self.numCoaddFrames_     = ncoadd
        self.centroidLoopMax_    = centroidLoopMax
        self.minSamples_         = minsamples
        self.backgroundOffset_   = dazbkg
        self.statsX.clear()
        self.statsY.clear()
        camera(ON, self.antnum_)
        # Get FOV, max x/y
        self.getImageParameters()
        # Save off images at both resolutions just to have headers
        self.setLowRes()
        # no coadd, background subtract, median or image normalization
        self.lowResImage_  = s.getImage(1, False, False, False, self.antnum_)
        self.setHighRes()
        self.highResImage_ = s.getImage(1, False, False, False, self.antnum_)
        # Initial calcs are done at low res scale
        self.setLowRes()
        self.imageCalcs(False)
        # Now wait for antenna to be acquired or for 20s
        wait(item=TRACK, ants=self.antnum_, tmo=20, waiton=ALL)
        # Get time from which to do time delay measurements
        self.deltat() 
        # Get initial offsets
        self.initialAzoff_ = queryDouble(self.monsys_ + "offsetAz", self.qmax_)
        self.initialEloff_ = queryDouble(self.monsys_ + "offsetEl", self.qmax_)
        self.frameCount_   = 0
        self.zoom_         = zoom
        self.verbose_      = verbose
        if showGUI :
            self.window()
            self.zoomSlider_.set(self.zoom_)
            self.updateZoom()  # This also updates the resolution
            self.brightness_ = self.brightnessSlider_.get() 
        else :
            self.updateResolution() 
        s.setFrameBrightness(0.01 * self.brightness_, [self.antnum_])
        if showGUI : self.autoUpdateInterval()
        thisZoom = self.zoom_
        if self.auto_:
            thisZoom = self.centroidZoom_
            if self.showGUI_: self.updateApplyCentroid()
            else:             self.startMeasureCentroid_ = True
            self.applyCentroid_ = apply
        t = time.time()
        self.startTime_ = t
        tstr = time.strftime("%Y/%m/%d %H:%M:%S", time.gmtime(t))
        azo = self.initialAzoff_
        elo = self.initialEloff_
        azo = queryDouble(self.monsys_ + "offsetAz", self.qmax_)
        str  = "Begin Camera(), %s, ant=%s, " %(tstr, self.antName_)
        str += ", offsets(%.2f,%.2f), " %(azo, elo)
        x = "source=%s, zoom=%.1f, reps=%d, coadds=%-2d,"\
            %(object, thisZoom, repeat, self.numCoaddFrames_)
        x += " background=%d, brightness=%d, time=%d" \
            %(self.subtractBackground_, self.brightness_, t)
        print str+x
        offStr = "offsets(%.2f,%.2f)" %(azo, elo) 
        str = tstr + " Starting opoint with %s\n%s" %(offStr,x)
        self.writeToLog(str)

        self.update()
        if self.showimage() and self.alive_:
            self.root_.mainloop()
        else :
            while self.alive_ : self.update()
        dt = time.time() -tstart # delta time for opoint (secs)
        str = "Opoint took %.1f seconds." % dt 
        self.writeToLog(str)
        print str
        return self.results_
                                
    def off(self):
        "Exit the GUI, close the flap/cap (if enabled) "
        if not self.dontClose_ :  # OK, shut off the cam, return to radio 
            if self.showGUI_ and self.linger_:
                # leave it on the screen for a bit,
                time.sleep(self.lingerTime_)  
            camera(OFF, self.antnum_)
        else :
            # Switch to radio even though we leave the camera on (flap open)...    
            radioAperture(True, self.antnum_)
        if self.showGUI_: self.root_.after_cancel(self.timerID_)
        self.alive_       = False
        if self.showGUI_: self.root_.destroy()
        self.canvas_      = 0
        self.root_        = 0
        self.imageHandle_ = 0
        self.timerID_     = 0
        self.tkimg_       = 0
        self.imgbkg_      = None

    def writeToLog(self, str) :
        try :
            fd = open(self.logfname_,'a')
            fd.write(str + '\n')
            fd.flush()
            fd.close()
        except Exception, ex :
            st = "Unable to write string to log file:%s" %(self.logfname_)
            print st, str(ex)
        
    def deltat(self):
        "Returns delta time from last call of deltat(). Overhead < 0.1msec"
        t  = time.time()
        dt = t - self.time_
        self.time_ = t
        return dt

    def minSamples(self):
        """Returns minimum number of samples needed for valid measurement"""
        minsamples = self.minSamples_
        if minsamples == None: 
            minsamples = max(4, self.centroidLoopMax_ / 2)
        return minsamples

    def measureCentroid(self) :
        """Have the antenna measure the centroid position of the brightest
        spot in the image.
        Applies the measurement to the offsets if requested,
        and repeats this procedure if requested. 
        Returns True if still measuring, False if not"""
        if False: print "Centroid %s %s %d" \
                        %(self.startMeasureCentroid_, 
                          self.measureCentroid_, 
                          self.centroidLoopCount_[self.autoreps_])
        if self.startMeasureCentroid_: 
            self.startMeasureCentroid_ = False
            self.measureCentroid_ = True
            self.centroidLoopCount_[self.autoreps_] = 0
            self.statsX.clear()
            self.statsY.clear()
            # Setup
            self.setHighRes()
            self.imageCalcs(True)
        if self.measureCentroid_ != True : return False
            
        numFramesPerImage    = self.numCoaddFrames_
        maxAttempts          = self.centroidLoopMax_ 
        minSamples           = self.minSamples()
        numEdgePixels        = 0
        apertureRadiusPixels = 15 # What is this?
        pixelThresholdSigma = 3.0
        # Hope this is now obsolete, 03/03/2014, SLS
        #if self.antnum_ == 8:  pixelThresholdSigma = 2.5
        autoBackground       = self.subtractBackground_
        normalizeMedian      = True 
        # Non-blocking centroid routine
        s.findCentroid( \
                    numFramesPerImage, minSamples, maxAttempts, \
                    numEdgePixels,apertureRadiusPixels, pixelThresholdSigma, \
                    autoBackground, normalizeMedian, [self.antnum_])
        # Timeout based on measured framerate on ovro ant
        aLittleBitExtra = 5.0  # seconds
        centroidTime    = 0.15 # Time for antenna to do a centroid (secs)
        tmo = (centroidTime*numFramesPerImage+0.1)*maxAttempts + aLittleBitExtra
        res = wait(CENTROID, self.antnum_, tmo=tmo, waiton=ALL)
        success = self.antnum_ in res.ready
        if not success:
            # Must be a timeout
            m  = "!!! Timeout on optical findCentroid() for C" 
            m += "%d !!!"  %(self.antnum_)
            print m
            commandlog(m)
            self.writeToLog(m)
            
        # TODO: Bailout on timeout
        res = s.getCentroidResults(self.antnum_)            

        for r in res :
            self.centroidLoopCount_[self.autoreps_] += 1
            reps = self.centroidLoopCount_[self.autoreps_]
            if False : print "Image count:%d autoreps:%d/%d" \
                %(reps,self.autoreps_, self.maxAutoreps_)
            xoff =  r.xOffsetInArcminutes   
            yoff =  r.yOffsetInArcminutes 
            if r.valid :  
                self.statsX.add(xoff)
                self.statsY.add(yoff)

            # Params:
            #  SNR:  SNR of maximum pixel
            #  Size: width (rms*2) of image in asec, using pixel val as weight  
            #  N:    number of pixels above threshhold inside aperture 
            if r.valid : v = 'Valid'
            else :       v = 'Invalid'
            msg  =  "Rep:%2d " %(reps)
            msg += "Inc offset: (%5.2f, %5.2f)amin  " %(xoff, yoff)
            msg += "(%5.1f, %5.1f)asec " %(60*xoff, 60*yoff)
            msg += "peakSNR=%4.1f Size=%4.1fasec N=%2d %s" \
                    %(r.peakPixelSNR, 60*r.sizeInArcminutes, \
                      r.aperturePixelCount, v)
            self.writeToLog(msg)
            if self.verbose_: print msg
        self.centroidWrapup()
        return self.measureCentroid_ or self.startMeasureCentroid_
        
    def centroidWrapup(self) :
        """Wrapup all work from a set of centroid measurements.
        Includes doing stats of offsets from each frame, determining if
        they are good enough to apply, and applying them.
        Determines if offsets are good enough to quit early or if another
        centroid set is needed. If completely done, records offsets
        and turns off auto loop. Resets the timeSinceLastIntegration to
        help inhibit this alarm."""
        resetTimeSinceLastIntegration()
        antNum = self.antnum_
        self.nsum = 0
        xsamps = self.statsX.samps()
        ysamps = self.statsY.samps()
        # If this stack of frames is all garbage, increment counter
        if xsamps == 0 and ysamps == 0 : self.allBadReps_ += 1
        minsamps = self.minSamples()
        enoughSamps = (xsamps >= minsamps) and (ysamps >= minsamps)
        # Stats.mean() will throw if we have zero samples
        try :
            aveX = self.statsX.mean()
            aveY = self.statsY.mean()
        except :
            aveX = 20
            aveY = 20
        centroidValid = False
        self.fitOK_   = False
        if enoughSamps :
            errX = self.statsX.error()
            errY = self.statsY.error()
            str = "Incremental offset(asec): %.1f +/- %.1f,  %.1f +/- %.1f" \
                    %(aveX*60, errX*60, aveY*60, errY*60)
            print str
            self.writeToLog(str)        
            errLimit = self.errLimit_
            centroidValid = (errX*60 < errLimit) and (errY*60 < errLimit)
            self.results_ = [aveX, errX, aveY, errY, centroidValid, self.zoom_]
            if self.applyCentroid_:
                # Only move antennas when offsets are good enough
                if centroidValid :
                    azoff   = queryDouble(self.monsys_ + "offsetAz", self.qmax_)
                    eloff   = queryDouble(self.monsys_ + "offsetEl", self.qmax_)
                    self.azoff_ = azoff + aveX
                    self.eloff_ = eloff + aveY
                    str = "Applying absolute offsets (%.2f, %.2f) amin" \
                                %(self.azoff_, self.eloff_)
                    print str
                    self.writeToLog("  " + str)
                    # We wait for acquisition at the start of the next loop            
                    offset(self.azoff_, self.eloff_, antNum, waiton=NONE)
                    self.azoffErr_ = errX
                    self.eloffErr_ = errY
                    self.fitOK_ = True
                else: 
                    str = "Offset not applied, errors too large"
                    str = "%s (%.1f, %.1f)asec" %(str, 60*errX, 60*errY)
                    print str
                    self.writeToLog(str)
                    #print "errX,errY(asec)=%.1f %.1f" % (60*errX, 60*errY)
                    self.fitOK_ = False
                self.results_ = \
                    [self.azoff_, self.azoffErr_, self.eloff_, self.eloffErr_, \
                     self.fitOK_, self.zoom_]
        else:  # Not enough samples
            str = "Not enough valid samples, "
            str += "got %d, " %self.statsX.samps()
            str += "need at least %d; nothing done" % self.minSamples()
            print str
            self.writeToLog(str)
        self.autoreps_ += 1
        quitNoHope = (self.autoreps_ >= self.quitNoHopeRepLimit_) \
                     and (self.autoreps_ == self.allBadReps_)
        offsetMag = 60*math.sqrt(aveX*aveX + aveY*aveY) # arcsec
        moreRepsTodo = self.autoreps_ < self.maxAutoreps_ 
        convergence  = self.fitOK_ and (offsetMag < self.offsetQuitLimit_)
        offsetQuit   = convergence and moreRepsTodo
        autoQuit = self.applyCentroid_ and centroidValid and offsetQuit 
        if moreRepsTodo and (not autoQuit) and (not quitNoHope):
            # Do another centroid rep
            self.startMeasureCentroid_ = True
            # Make sure we are tracking (offsets may have just been applied)
            if self.fitOK_ : wait(TRACK, ants=antNum, waiton=ALL)
            return
        # ------- Done with all centroiding on this star, wrap it up now -------
        #print "Wrapping up this star"
        if quitNoHope :
            str = "Stopping early after "
            str += "%d/%d reps, " %(self.autoreps_, self.maxAutoreps_)
            str += "because all data so far are no good"
            print str
            self.writeToLog(str)
        if offsetQuit :  # Print out informational message
            str ="Stopping early after "
            str += "%d/%d reps: " %(self.autoreps_, self.maxAutoreps_)
            str += "offset(%.1f) less than " %offsetMag
            str += "limit(%.1f) asec"  %self.offsetQuitLimit_
            print str
            self.writeToLog(str)
        if self.auto_:
            # If running in auto and applyCent:
            #   if converged, record data
            #   if not converged, go back to initial offsets
            if self.applyCentroid_ :
                if convergence : 
                    self.updateRecordData()
                else : 
                    str = "Returning to initial offsets"
                    print str
                    self.writeToLog(str)
                    self.azoff_ = self.initialAzoff_
                    self.eloff_ = self.initialEloff_
                    offset(self.azoff_, self.eloff_, antNum, waiton=NONE)
            self.off()             #  close GUI and maybe the flap
            str = "Opoint done, offset(%.2f,%.2f), " %(self.azoff_,self.eloff_)
            str += "reps=%d/%d " %(self.autoreps_, self.maxAutoreps_)
            str += " (rep:centroids "
            for i in range( self.maxAutoreps_ ):
                str+= "%d:%d" %(i+1, self.centroidLoopCount_[i])
                if i != self.maxAutoreps_ - 1:
                    str += ","
            str += ") deltaTime=%.1f " %(time.time()-self.startTime_)
            str += " time=%d" %(time.time())
            print str
            self.writeToLog(str)
            star  = self.object_
            az,el = azel(star)
            str   = "   az/el(%s): %.1f, %.1f" %(star,  az, el)
            az,el = azel("sun")
            str  += "   az/el(%s): %.1f, %.1f" %("sun", az, el)
            self.writeToLog(str)
        if self.root_ != 0 and self.showGUI_:
             self.zoomSlider_.config(state="normal", background='#d9d9d9') 
             self.recordData_.config(bg='green', 
                    activebackground='green', text="RecordData")       
        self.measureCentroid_ = False
        self.applyCentroid_   = False
        self.autoreps_ = 0
        #print "Done with centroiding"
        # Go back to GUI resolution
        self.updateResolution()
        
    def drawbox(self, data, xmax, ymax, size, reticleIntensity) :
        """Draw a reticle box on the data passed in.
        Parameters:
         xmax: image size in x in pixels
         ymax: image size in y in pixels
         size: box size in arcminutes"""
        x0 = xmax/2
        y0 = ymax/2
        maxindex = xmax*ymax
        beta = 1.0 - 0.01*self.reticleOpacity_
        weightedReticle = 0.01* self.reticleOpacity_ * reticleIntensity
        halfbox = int(0.5*size*self.screenScale_+0.5) # in pixels
        # Horizontal
        if halfbox < y0:
            topOffset = (y0 + halfbox)*xmax
            botOffset = (y0 - halfbox)*xmax
            for x in range(x0 - halfbox, x0 + halfbox) :
                i = x + topOffset
                if i >= 0 and i < maxindex :
                    data[i] = int(data[i]*beta + weightedReticle)
                i = x + botOffset
                if i >= 0 and i < maxindex :
                   data[i] = int(data[i]*beta + weightedReticle)
        # Vertical
        if halfbox < x0:
            leftOffset =  x0 - halfbox
            rightOffset = x0 + halfbox
            for y in range(y0-halfbox, y0+halfbox) :
                i = leftOffset + y*xmax
                if i >= 0 and i < maxindex :
                    data[i] = int(data[i]*beta + weightedReticle)
                i = rightOffset + y*xmax
                if i >= 0 and i < maxindex :
                    data[i] = int(data[i]*beta + weightedReticle)
            
    def reticle(self, image, reticleIntensity=128, box=True) :
        """Takes an Image and draws a reticle on it,
        using recticle intensity and opacity"""
        # The imagedata is an immutable string, so we must copy it into an array,
        # and then manipulate it.
        data = array.array('B',image.tostring())
        xmax = image.size[0]
        ymax = image.size[1]
        self.drawbox(data, xmax, ymax, 1.0, reticleIntensity)
        self.drawbox(data, xmax, ymax, 2.0, reticleIntensity)
        self.drawbox(data, xmax, ymax, 3.0, reticleIntensity)
        x0 = xmax/2
        y0 = ymax/2
        beta = 1.0 - 0.01*self.reticleOpacity_
        weightedReticle = 0.01* self.reticleOpacity_ * reticleIntensity
        offset = ymax/2*xmax
        # Horizontal
        for x in range(xmax) :
            i = x + offset
            data[i] = int(data[i]*beta + weightedReticle)
        # Vertical
        for y in range(ymax) :
            i = int(xmax*(y+0.5) + 0.5)
            data[i] = int(data[i]*beta + weightedReticle)
        image.putdata(data)        
        return   
            
    def isLowRes(self) :
        if self.zoom_ < self.zoomResolutionThreshold_ : return True
        return False

    def getDisplayHeightCompress(self):
        """ Returns display heigt and compress factor for raw images """

        # *** A fact of life: the pixels returned by the 
        #   cameras are asymettric, being longer in elevation.
        #   We correct by compressing the image in the y dimension here.
        #   The cause is 4/3 NTSC image aspect ratio divided by the 
        #   frame grabber pixelization of 768/480 = 1.200
        #   The disagreement with theory may be because of the black band?
        factor = 1.17
        compress = 1.0/factor 
        displayHeight = int(compress*self.ydisp_+0.5)

        return [displayHeight, compress]

    def showimage(self):
        """ Returns True/False if the image will be displayed in the GUI
        Returns false if either no GUI display is requested or during
        centroiding."""
        #return (self.root_ != 0 and self.showGUI_ == True)
        return (self.root_ != 0 and self.showGUI_ == True \
                and self.measureCentroid_ == False \
                and self.startMeasureCentroid_ == False)

    def convertToNumarray(self, rawimg):
        """ Converts rawimg in Image() format to numarray format """

        pdata = array.array('B',rawimg.tostring())
        dtemp = numarray.reshape(pdata,rawimg.size[1],rawimg.size[0])
        return dtemp

    def convertRawImage(self, rawimg):
        """ Converts raw image (grabFrame) to Image() format """
        img = Image.frombuffer( "L", (rawimg.x, rawimg.y), \
                  rawimg.opticalData, "raw", "L", 0, 1 )

        # Sometimes the image size returned is not what is requested!!
        if self.xcrop_ != img.size[0] or self.ycrop_ != img.size[1] :
            print "Received mage size (%d,%d) must equal crop size(%d,%d)" \
                  %(img.size[0], img.size[1], self.xcrop_, self.ycrop_)

        # Resize to correct for image being long in elevation.
        # See getDisplayHeightCompress() for details.
        compress = self.getDisplayHeightCompress()[1]
        img=img.resize((img.size[0], int(compress*img.size[1]+0.5)))

        # Done
        return(img)

    def getImage(self):
        """ Returns an image in  Image() format """
        apertMP = self.monsys_ + "Constants.selectedApert"
        self.coeffString_ = queryString(apertMP, self.qmax_)
        self.updateCoeffs()

        nframes = self.numCoaddFrames_
        bg      = self.subtractBackground_
        t0 = time.time()
        # The s.getImage is a synchronous method
        rawimg  = s.getImage(nframes, bg, bg, self.normalize_, self.antnum_)
        #print "Elapsed time to get raw image, %.1f" %(time.time()-t0)
        return rawimg

    def displayImage(self, rawimg=None) :
        if rawimg == None: 
            rawimg = self.getImage()
            #print "displayimage(None)"
        time0 = time.time()       
        img = self.convertRawImage(rawimg)
        nx = rawimg.x
        ny = rawimg.y
        displayHeight = self.getDisplayHeightCompress()[0]

        fromBufferTime = int(1000*(time.time() - time0))

        centroidTime = int(1000*(time.time() - time0))
        self.screenScale_ = self.scale_*self.nativeResolution_
        time0 = time.time()
        img=img.resize((self.xdisp_, displayHeight))
        resizeTime = int(1000*(time.time() - time0))
        if False :
            msg = "nativeRes=%.1fpix/amin scale=%.2f screenScale=%.1fpix/amin" \
                %(self.nativeResolution_,self.scale_, self.screenScale_)
            print msg
            msg = "zoom=%.2f" %(self.zoom_)
            print "Resized size:", img.size
        time0 = time.time()
        self.reticle(img, self.reticle_)
        reticleTime = int(1000*(time.time() - time0))
        #x, y = img.size
        self.tkimg_ = ImageTk.PhotoImage(img)
        if self.canvas_ == 0: 
            self.canvas_ = Tkinter.Canvas(width=self.xdisp_, 
                height=displayHeight, bg="blue")
        if self.imageHandle_ != 0: self.canvas_.delete(self.imageHandle_)
        time0 = time.time()
        imageHandle_ = self.canvas_.create_image(self.xdisp_/2+1, 
                int(0.5*displayHeight+0.5)+1,  image=self.tkimg_)
        canvasTime = int(1000*(time.time() - time0))
        self.canvas_.grid(column=1, columnspan=2, row=1, rowspan=20)
        printTimes = False
        if printTimes :
            print "from buffer:", fromBufferTime
            print "centroid:",    centroidTime
            print "resize:",      resizeTime
            print "reticle:",     reticleTime
            print "canvas:",      canvasTime
        
    def update(self):
        if self.alive_ == False: 
            return
        currentTime = time.time()
        if self.showGUI_ and self.lastUpdateTime_ != 0:
             groupSize = 400  # How often to print out update rate
             modulo = self.frameCount_%groupSize
             if modulo == 0 and self.frameCount_ != 0 : 
                self.frameRate_ = 1.0/(currentTime - self.lastUpdateTime_)
                print "%.1f Hz" %self.frameRate_ 
        self.lastUpdateTime_ = currentTime
        if self.takeBackground_ : 
            # May use the return value (not yet coded) in case of tmo
            offset = self.backgroundOffset_
            str = "Taking background image, offset=%.1famin" %offset
            print str
            self.writeToLog(str)
            r = incoffset(offset, 0, self.antnum_, tmo=10)
            s.takeBackgroundImage(self.numBackgroundFrames_, [self.antnum_])
            wait(CENTROID, self.antnum_, 10.0, ALL)
            r = incoffset(-offset, 0, self.antnum_, tmo=10)             
            self.takeBackground_ = False
        
        # Measure centroid
        self.measureCentroid()
        if self.showimage() : self.displayImage()
        self.frameCount_ += 1
        if self.showGUI_ and not self.auto_ :
            if self.root_ != 0 :
                self.timerID_ = self.root_.after(self.deadTime_, self.update)
        #else : time.sleep(self.deadTime_/1000.0)
        
    def measureGrabTime(self):
        "Measures maximum time to get a frame" 
        # Which it turns out is for the low res frame, because we will crop
        # the high res to make it no bigger than the low res frame
        self.setLowRes()
        reps =5
        frameCount = 0
        time0 = time.time()
        for i in range(reps):
            rawimg = s.getImage(1, False, False, False, self.antnum_)
            frameCount += 1
        deltaTime = time.time() - time0
        # Reset the resolution
        self.updateResolution()
        return  1000*deltaTime/frameCount # in milliseconds
    
    def setHighRes(self) :
        #print "Setting high resolution"
        s.setFramegrabberResolution(carma.antenna.common.HIGH_RES, [self.antnum_])
        
    def setLowRes(self) :
        #print "Setting low resolution"
        s.setFramegrabberResolution(carma.antenna.common.LOW_RES, [self.antnum_])        
        
    def measureDisplayTime(self) :
        "Measures maximum time to display a frame" 
        reps =5
        frameCount = 0
        oldZoom = self.zoom_
        self.zoom_ = 1
        self.updateResolution
        time0 = time.time()
        rawimg = s.getImage(1, False, False, False, self.antnum_)
        for i in range(reps):
            self.displayImage(rawimg)
            frameCount += 1
        # Reset the resolution
        self.zoom_ = oldZoom
        self.updateResolution()
        return 1000*(time.time() - time0)/frameCount
        
    def getImageParameters(self) :
        # Store resolution independent image parametes 
        rawimg = s.getImage(1, False, False, False, self.antnum_)
        self.fieldOfView_ = rawimg.fovWidth  # get from image
        self.xmax_ = rawimg.xMax  # max for any resolution
        self.ymax_ = rawimg.yMax
        # The size of the display depends on the size of the image
        self.xdisp_ = int(0.01*self.displaySize_*self.xmax_ + 0.5)
        self.ydisp_ = int(0.01*self.displaySize_*self.ymax_ + 0.5)
        #print "Size of displayed image", self.xdisp_, self.ydisp_
           
    def autoUpdateInterval(self) :
        "Measures maximum rate that frames can be retrieved (low res)" 
        grabTime    = self.measureGrabTime()
        displayTime = self.measureDisplayTime()
        #print "grabTime:%d   displayTime:%d msec" %(int(grabTime), int(displayTime))
        sleepTime = max(grabTime*(100 - self.dutyCycle_)/100 - displayTime, 0)
        upd = grabTime + displayTime + sleepTime
        minUpdateInterval = 1000.0/self.maxFrameRate_ # in msec
        self.updateInterval_ = int(max(minUpdateInterval, upd))
        self.deadTime_ = int(self.updateInterval_ - (grabTime+displayTime))
        self.deadTime_ = max(self.deadTime_, 1)
        if False: print "Update interval= " + str(self.updateInterval_) + \
            " msecs; deadtime=" + str(self.deadTime_)            

    def updateContrast(self, *ignore):
        self.contrast_ = self.contrastSlider_.get()  
        s.setFrameContrast(0.01 * self.contrast_, [self.antnum_])
        #print "constrast:%d" %self.contrast_
                                  
    def updateBrightness(self, *ignore):
        self.brightness_ = self.brightnessSlider_.get()  
        s.setFrameBrightness(0.01 * self.brightness_, [self.antnum_])
        #print "brightness:%d" %self.brightness_

    def imageCalcs(self, centroid=False):
        """Gets an image and does all geometry calcs for it 
         (scaling, cropping, etc)"""
        fov = self.fieldOfView_ 
        lowResImage = self.lowResImage_
        if self.isLowRes() and not centroid:
             rawimg     = self.lowResImage_
        else : 
             rawimg     = self.highResImage_           
        # Update the max pixels at this resolution
        self.xres_ = rawimg.xRes  # max x-dim image pixels for this resolution
        self.yres_ = rawimg.yRes
        self.xres_ = max(self.xres_, 100)
        self.yres_ = max(self.yres_, 100)
        # Resolution for this zoom in pixels/arcmin
        self.nativeResolution_      = self.xres_/fov
        self.zoom1NativeResolution_ = lowResImage.xRes/fov 
        zoom1ScaleX                 = float(self.xdisp_)/lowResImage.xRes       
        zoom1ScaleY                 = float(self.ydisp_)/lowResImage.yRes        
        self.zoom1Scale_            = 0.5*(zoom1ScaleX + zoom1ScaleY)       
        resRatio = self.zoom1NativeResolution_/self.nativeResolution_
        if centroid : z = self.centroidZoom_   
        else :        z = self.zoom_ 
        scaleX = zoom1ScaleX * z * resRatio
        scaleY = zoom1ScaleY * z * resRatio
        self.scale_ = 0.5*(scaleX + scaleY)
        self.xcrop_ = int(round(self.xdisp_/scaleX))
        self.ycrop_ = int(round(self.ydisp_/scaleY))
        xborder = int(round((self.xres_ - self.xcrop_)/2.0))
        yborder = int(round((self.yres_ - self.ycrop_)/2.0))
        if False:
            print "zoom1Scale=", self.zoom1Scale_, " zoom=", z, \
                  " scale=", self.scale_
            print "res z1native=", self.zoom1NativeResolution_, \
                  "native=", self.nativeResolution_, "centroid=", centroid
            print "Cropped size, offset:", self.xcrop_, self.ycrop_, \
                  xborder, yborder
            print "Max image size for this res:", self.xres_, self.yres_
        # Set the cropping box in the antenna
        s.setFrameDimensions(self.xcrop_, self.ycrop_, xborder, yborder,\
                [self.antnum_])
        #box = (xborder, yborder, self.xcrop_-xborder, self.ycrop_-yborder)
        #print "Box:", box
        
    def updateResolution(self, centroid=False) :
        if self.isLowRes(): self.setLowRes()
        else:               self.setHighRes()
        self.imageCalcs(centroid)
         
    def updateZoom(self, *ignore):
        self.subtractBackground_ = False
        self.displaySubtractBackground()
        self.zoom_ = self.zoomSlider_.get() 
        #print "zoom:%.1f" %self.zoom_
        self.updateResolution(False)
               
    def updateReticle(self, *ignore):
        self.reticle_ = self.reticleSlider_.get() 
        #print "reticle:%d" %self.reticle_
                                 
    def updateReticleOpacity(self, *ignore):
        self.reticleOpacity_ = self.reticleOpacitySlider_.get() 
        #print "reticleOpacity:%d" %self.reticleOpacity_
                                 
    def updateNumBackgroundFrames(self, *ignore):
        self.numBackgroundFrames_ = self.backgroundSlider_.get() 
        #print "numBackgroundFrames:%d" %self.numBackgroundFrames_

    def updateBackgroundOffset(self, *ignore):
        self.backgroundOffset_ = self.backgroundOffsetSlider_.get() 
        #print "backgroundOffset:%d" %self.backgroundOffset_

    # Update the display of relevant sliders w/current params                                                                  
    def displaySliders(self):
        self.coaddSlider_.set(self.numCoaddFrames_)  
        self.backgroundSlider_.set(self.numBackgroundFrames_)  
        self.backgroundOffsetSlider_.set(self.backgroundOffset_)
        self.contrastSlider_.set(self.contrast_)
        self.brightnessSlider_.set(self.brightness_)
        self.zoomSlider_.set(self.zoom_)
        self.reticleSlider_.set(self.reticle_) 
        self.reticleOpacitySlider_.set(self.reticleOpacity_)
                                                                 
    def updateNumCoaddFrames(self, *ignore):
        self.numCoaddFrames_ = self.coaddSlider_.get() 
        #print "numCoaddFrames:%d" %self.numCoaddFrames_
                                 
    def updateCentroid(self, *ignore):
        self.zoomSlider_.config(state="disabled", background='red')       
        self.startMeasureCentroid_ = True
                                 
    def updateApplyCentroid(self, *ignore):
        self.zoomSlider_.config(state="disabled", background='red')       
        self.startMeasureCentroid_ = True
        self.applyCentroid_        = True
                                 
    def displaySubtractBackground(self):
        if self.subtractBackground_ : t = 'SubtractBackground'
        else :                        t = 'NoBackgroundSubtract'
        self.subtractBackgroundToggle_.config(text=t)      
         
    def toggleSubtractBackground(self, *ignore):
        self.subtractBackground_ = not self.subtractBackground_
        self.displaySubtractBackground()      

    def takeBackground(self, *ignore):
        self.takeBackground_ = True
                                 
    def nextCoeffs(self, *args):
        R1MM = carma.antenna.common.DriveControl.RADIO1MM
        R3MM = carma.antenna.common.DriveControl.RADIO3MM
        R1CM = carma.antenna.common.DriveControl.RADIO1CM
        OPT  = carma.antenna.common.DriveControl.OPTICAL
        #print "nextCoeffs pressed:", self.selectApertVar_.get()
        if self.selectApertVar_.get() == "Current Coeffs: optical":
          self.ant_.drive().selectAperture(R1MM)
          print "  Selecting 1mm Aperture Coeffs"
        elif self.selectApertVar_.get() == "Current Coeffs: 1mm":
          self.ant_.drive().selectAperture(R3MM)
          print "  Selecting 3mm Aperture Coeffs"
        elif self.selectApertVar_.get() == "Current Coeffs: 3mm":
          self.ant_.drive().selectAperture(R1CM)
          print "  Selecting 1cm Aperture Coeffs"
        elif self.selectApertVar_.get() == "Current Coeffs: 1cm":
          self.ant_.drive().selectAperture(OPT)
          print "  Selecting Optical Aperture Coeffs"

    def updateRecordData(self, *ignore):
        home = os.environ['HOME']
        date = os.popen('date -u +%d%b%y').readlines()[0].strip()
        mjd=self.sac_.mjd(0)
        source = self.object_
        name = self.ant_.getName()
        print "Recording point for %s  time=%d" \
                %(self.ant_.getName(), time.time())
        if self.showGUI_:
            self.recordData_.config(bg='red', activebackground='red', 
                    text="Recorded")
        if 0:
            self.sac_.recordPoint(self.ant_.getName())
        else:
            pass #print "Skipping recordPoint: ",self.ant_.getName()
        # alternate way doing it in a format that ovro's POINT understand, and also miriad's PNT
        # ant-no date az,el,daz,dazerr, del, delerr
        # source m1...m5 o1..o3
        # Monitor system names
        ant   = self.ant_.getAntnum()
        name  = self.ant_.getName()
        msa   = name[0].upper() + name[1:]
        mscmn = msa + ".AntennaCommon.Drive"
        mspc  = mscmn + ".Point.Constants.ApertureCoefficients1"
        dfile = "%s/optical.%s.data" %(home,name[:-1])
        if name[0] == 'o':
            pass
        elif name[0] == 'b':
            pass
        elif name[0] == 's':
            pass
        else:
            print "Telescope name %s not recognized, no pointing data written" %name
            return
            
        az=queryDouble('%s.Track.requestedAzimuth'   %mscmn, self.qmax_)
        el=queryDouble('%s.Track.requestedElevation' %mscmn, self.qmax_)
        if len(source) == 0:  source='ANON'
        o = []
        o.append(queryDouble("%s.crossElCollErr" %mspc, self.qmax_ ))
        o.append(queryDouble("%s.elCollErr" %mspc, self.qmax_ ))
        o.append(queryDouble("%s.sag" %mspc, self.qmax_ ))
        if name[0] == 'o':
            # only do this for OVRO ants
            m=[]
            for i in range(5):
              m.append(queryDouble('%s.Drive.Point.Constants.m%d' %(msa,i+1),self.qmax_))
            fd = OpenLock(dfile,'a')
            fd.write('MM%d OPT %-12s %s %11.5f  %6.2f  %6.2f %7.3f %6.3f %7.3f %6.3f\n' \
              % (ant,source,date,mjd,az,el,\
                 self.azoff_, self.azoffErr_, self.eloff_, self.eloffErr_))
            fd.write('          00%d  %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f\n' % (ant,m[0],m[1],m[2],m[3],m[4],o[0],o[1],o[2]))
            fd.close()
        elif name[0] == 's':
            # only do this for SZA ants
            m=[]
            for i in range(5):
              m.append(0.0)  # Kludge for now
              #m.append(queryDouble('Ovro%d.Drive.Point.Constants.m%d' % (ant,i+1),self.qmax_))
            fd = OpenLock(dfile,'a')
            fd.write('MM%d OPT %-12s %s %11.5f  %6.2f  %6.2f %7.3f %6.3f %7.3f %6.3f\n' \
                %(ant,source,date,mjd,az,el,\
                  self.azoff_, self.azoffErr_, self.eloff_, self.eloffErr_))
            fd.write('          00%d  %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f %7.3f\n' \
                %(ant,m[0],m[1],m[2],m[3],m[4],o[0],o[1],o[2]))
            fd.close()
        elif name[0] == 'b':
            ut=(mjd-int(mjd))*24.0
            # this is the format for the PNT program with options telescop=carma !!! version 12-sep-2005 or later
            fd = OpenLock(dfile,'a')
            fd.write('%s%9.3f %9.3f %9.3f %9.3f %9.3f %9.3f %2d %-8s\n' \
                     % (date,ut,az,el,self.azoff_,self.eloff_,0.0,ant,source))
            fd.close()
            [azCoeff,elCoeff,aperCoeff] = self.getBimaPointModel(ant)
            file = "%s/%s" % (home,'optical.bima.coeff')
            fd = OpenLock(file,'a')
            fd.write('%s%9.3f %9.3f %9.3f %2d ' % (date,ut,az,el,ant) )
            for i in range(0,9) : fd.write('%9.3f ' %azCoeff[i])
            for i in range(0,8) : fd.write('%9.3f ' %elCoeff[i])
            for i in range(0,3) : fd.write('%9.3f ' %aperCoeff[i])
            fd.write('%-8s\n' % source)
            fd.close()

    def getBimaPointModel(self,antref) :
        azCo = []
        elCo = []
        aperCo = []
        bimaAper = ['AprtOpAz','AprtOpEl','AprtOpSag']
        pcon = "Bima%i.BimaSpecific.Drive.Point.Constants" %(antref) 
        for i in range(1, 10) :
            azCo.append(queryDouble("%s.apc%i" %(pcon,i), 20))
            elCo.append(queryDouble("%s.epc%i" %(pcon,i), 20))
        for j in range(3) :
            aperCo.append(queryDouble("%s.%s" % (pcon,bimaAper[j]), 20))
        return [azCo,elCo,aperCo]
        
    def updateCoeffs(self) : 
        if self.selectApertVar_ == None: return   
        if self.coeffString_ == "OPTICAL":
            self.selectApertVar_.set("Current Coeffs: optical")
        elif ( self.coeffString_ == "RADIO1MM"):
            self.selectApertVar_.set("Current Coeffs: 1mm")
        elif ( self.coeffString_ == "RADIO3MM" ):
            self.selectApertVar_.set("Current Coeffs: 3mm")
        elif ( self.coeffString_ == "RADIO1CM" ):
            self.selectApertVar_.set("Current Coeffs: 1cm")   
                                                              
    def window(self):
        slideLen = 170
        fsize=8
        padlen = (slideLen - 1.10*fsize*13)/2
        titleString = "%s Optical Camera" %self.ant_.getCarmaAntname()
        self.root_ = Tkinter.Tk()
        self.root_.title(titleString)
        # size=5 is unreadable on macosx X11, but 10 is way too big on linux
        font = tkFont.Font(family="Arial", size=fsize) 
        bigfont = tkFont.Font(family="Arial", size=fsize+1) 
        menubar = Tkinter.Menu(self.root_)
        menubar.add_command(label="Quit",  command=self.off)
        self.contrastSlider_ = Tkinter.Scale(orient=Tkinter.HORIZONTAL, 
            borderwidth=1, relief=Tkinter.GROOVE, font=font, length=slideLen,
            label="Contrast", command=self.updateContrast)
        self.brightnessSlider_ = Tkinter.Scale(orient=Tkinter.HORIZONTAL, 
            borderwidth=1, relief=Tkinter.GROOVE, font=font, length=slideLen, 
            label="Brightness", command=self.updateBrightness)
        self.zoomSlider_ = Tkinter.Scale(orient=Tkinter.HORIZONTAL, 
            borderwidth=1, relief=Tkinter.GROOVE, font=font, length=slideLen,
            label="Zoom",digits=2, command=self.updateZoom, from_=1, to=8, 
            resolution=0.1)    
        self.reticleSlider_ = Tkinter.Scale(orient=Tkinter.HORIZONTAL, 
            borderwidth=1, relief=Tkinter.GROOVE, font=font, length=slideLen, 
            label="Reticle black/white", digits=3, command=self.updateReticle, from_=0, to=255)
        self.reticleOpacitySlider_ = Tkinter.Scale(orient=Tkinter.HORIZONTAL, \
            borderwidth=1, relief=Tkinter.GROOVE, font=font, length=slideLen,
            label="Reticle opacity", command=self.updateReticleOpacity, 
            from_=0, to=100)
        self.backgroundSlider_ = Tkinter.Scale(orient=Tkinter.HORIZONTAL, \
            borderwidth=1, relief=Tkinter.GROOVE, font=font, length=slideLen,
            label="NumBackgroundFrames", command=self.updateNumBackgroundFrames, 
            from_=1, to=100)
        self.coaddSlider_ = Tkinter.Scale(orient=Tkinter.HORIZONTAL, \
            borderwidth=1, relief=Tkinter.GROOVE, font=font, length=slideLen,
            label="NumCoaddFrames", command=self.updateNumCoaddFrames, 
            from_=1, to=100)
        self.backgroundOffsetSlider_ = Tkinter.Scale(orient=Tkinter.HORIZONTAL, \
            borderwidth=1, relief=Tkinter.GROOVE, font=font, length=slideLen,
            label="BackgroundOffsetPos", command=self.updateBackgroundOffset, 
            from_=0, to=10, resolution=0.1)
        self.centroidButton_ = Tkinter.Button( 
            borderwidth=3, relief=Tkinter.RAISED, font=font, padx=1.2*padlen, 
            text="MeasureCentroid", command=self.updateCentroid)
        self.centroidApplyButton_ = Tkinter.Button( 
            borderwidth=3, relief=Tkinter.RAISED, font=font, padx=0.77*padlen, 
            text="ApplyCentroid", command=self.updateApplyCentroid)
        self.selectApertVar_ = Tkinter.StringVar()

        self.changeApertCoeffs_ = Tkinter.Button( 
            borderwidth=3, relief=Tkinter.RAISED, font=font, padx=padlen,
            text="ChangeCoeffs", command=self.nextCoeffs)
        self.displayApertCoeffs_ = Tkinter.Label( 
            borderwidth=0, relief=Tkinter.FLAT, font=bigfont, padx=padlen,
            text="Select:", textvariable=self.selectApertVar_)
        self.recordData_ = Tkinter.Button( 
            borderwidth=3, relief=Tkinter.RAISED, font=font, padx=padlen, 
            text="RecordData ", command=self.updateRecordData, 
            bg="green", activebackground='green')
        self.takeBackgroundButton_ = Tkinter.Button( 
            borderwidth=3, relief=Tkinter.RAISED, font=font, padx=padlen, 
            text="TakeBackground ", command=self.takeBackground)
        self.subtractBackgroundToggle_ = Tkinter.Button( 
            borderwidth=3, relief=Tkinter.RAISED, font=font, padx=padlen, 
            text="NoBackgroundSubtract", command=self.toggleSubtractBackground)
        if self.showGUI_ :
            self.changeApertCoeffs_.grid(       column=1, row=0)
            self.displayApertCoeffs_.grid(      column=2, row=0)
            r = 10
            self.contrastSlider_.grid(          column=0, row=r+1)
            self.brightnessSlider_.grid(        column=0, row=r+2)
            self.reticleSlider_.grid(           column=0, row=r+3)
            self.reticleOpacitySlider_.grid(    column=0, row=r+4)
            r = 10
            self.zoomSlider_.grid(              column=3, row=r+1)
            self.recordData_.grid(              column=3, row=r+2)
            self.centroidButton_.grid(          column=3, row=r+3)
            self.centroidApplyButton_.grid(     column=3, row=r+4)
            r = 21
            self.backgroundSlider_.grid(        column=1, row=r)
            self.coaddSlider_.grid(             column=2, row=r)
            self.backgroundOffsetSlider_.grid(  column=1, row=r+1)
            self.subtractBackgroundToggle_.grid(column=2, row=r+1)
            self.takeBackgroundButton_.grid(    column=1, row=r+2, columnspan=2)
            self.displaySliders()
            self.displaySubtractBackground()
            # To capture the "windowClose" clicks
            self.root_.protocol("WM_DELETE_WINDOW", self.off)


# Now a class with just the interactive methods, so the other can be hidden
class Camera(object):
    """Deprecated. See help for opticalSystem."""
    def __init__(self, ant, displaySize=75) :
        self.camera_ = CameraBase(ant, displaySize)
    def on(self, auto=False, object='', repeat=1, zoom=1.2, linger=False,
            showGUI=True, dontClose=False, brightness=50,
            apply=True, subtractBackground=False, ncoadd=1, dazbkg=2.0,
            centroidLoopMax=16, minsamples=None, verbose=False,
            opticalSystem=False):
        """Deprecated. See help for opticalSystem."""
        self.camera_.on(auto, object, repeat, zoom, linger, 
            showGUI, dontClose, brightness, apply,
            subtractBackground, ncoadd, dazbkg, centroidLoopMax, minsamples,
            verbose=verbose, opticalSystem=opticalSystem)
            
    def results(self):
        """Get the last results. If apply=True, then the results are
        absolute offsets, if False, then they are incremental. 
        The offsets and errors in the results are in arcmin. Results:
         azoff, azoffErr, eloff, eloffErr, validMeasurement, zoom"""
        return self.camera_.results_

def opticalSystem(ant, auto=False, object=None, repeat=1, zoom=1.2, 
            showGUI=True, dontClose=False, brightness=50,
            apply=True, subtractBackground=False, ncoadd=1, dazbkg=2.0,
            centroidLoopMax=16, minsamples=None, verbose=False,
            displaySize=75):
    """Very flexible control of the optical camera, optional interactive GUI,
    and centroiding system. Optional parameters give extensive control of  
    the automatic centroid mode and its behavior. In auto mode the
    data are automatically recorded on successful centroiding and a result
    set is returned. The result set contains:
       azoff, azoffErr, eloff, eloffErr, validMeasurement, zoom 
    The centroiding uses the pointing offsets (not mountOffsets) and does
    so incrementally. If application of the offsets is selected as an
    input parameter then the offsets at the invocation of this command will
    be changed only if they have adequate SNR. Otherwise the original offsets
    are preserved. 
    If the GUI is selected then this command blocks until the GUI is exited.
    The GUI intensity (whiteness/blackness) and transparency of the reticle
    can be controlled.
    The GUI record button writes data into the default directory of the 
    subarray controller, which is usually /home/control.
    The ability to write FITS files is now supported in the snapshot command.
    The boxes drawn on the GUI are 1, 2, and 3 arcmin on a side.
    Detailed centroid results, including snr, etc. are written 
    to /array/rt/opointLog.
    Parameters:
     ant - Carma antenna number
     auto - do automatic centroiding (False)
     object - name for object (star name, informational only).
     repeat - number of centroids to do in the auto loop (1)
     zoom - zoom value on startup (1.2); centroid fails when < 1.13
     showGUI - set false w/auto=t if GUI is too slow (remote X)
     dontClose - don't close lenscap on exit, used by auto session (False)
     brightness - framegrabber brightness control, use 30 in daytime
     apply - apply the centroid measurement to the offsets 
       (with the offset() command) as incremental results
     subtractBackground - subtract sky background (False)
     ncoadd - number of images to average (1)
     dazbkg - azimuth offset in arcminutes for background offset position (2.0)
     centroidLoopMax - maximum number of tries per repeat (16)
     minsamples - minimum number of samples for a valid measurement (None).
       A value of None gives the max of 4 or centroidLoopMax/2
     displaySize - display size of GUI in percent of max image (75); 
        affects cpu usage, so dont make it too big - the default is good.
     verbose - prints out info on snr, etc. Default is False.   """
    c = Camera(ant, displaySize)
    if object == None:
        # This won't be the correct sourcename if trackSingle was done...
        object = queryString("Control.Subarray%d.source" %subarrayNo)
    c.on(auto=auto, object=object, repeat=repeat, zoom=zoom, linger=False, 
            showGUI=showGUI, dontClose=dontClose, brightness=brightness, 
            apply=apply, subtractBackground=subtractBackground,
            ncoadd=ncoadd, dazbkg=dazbkg, centroidLoopMax=centroidLoopMax,
            minsamples=minsamples, verbose=verbose, opticalSystem=True)
    return c.results()        
                  
def snapshot(ant, numFrames=1, subtractBackground=False, show=True,
             writeFITS=False) :
    """Gets an optical image from an antenna and displays the raw image
    on the screen. Does not turn the camera on or off.
    Parameters:
     ant - a single carma antenna number, zero not allowed
     numFrames - number of frames to co-add
     show - boolean control of showing the image, default=True.
            Why not always show the image? It is nice to turn it off for
            timing tests, or just writing a fits file.
     writeFITS - writes a FITS file of the image"""
    def closeSnapshot() :
        root_.destroy()
    def getCompressedImageHeight(y) :
        """ Returns display heigt and compress factor for raw images """
        # *** An ugly fact of life: the pixels returned by the
        #   cameras are asymettric, being longer in elevation.
        #   We correct by compressing the image in the y dimension here.
        #   The cause is 4/3 NTSC image aspect ratio divided by the
        #   frame grabber pixelization of 768/480 = 1.200
        #   The disagreement with theory may be because of the black band?
        factor = 1.17
        compress = 1.0/factor
        return  int(compress*y+0.5)
        
    def writefits(x, y, fov, opticalData) :
        qmax = 10 # Max times to try query
        nativeRes =  min(x, y)/fov
        img = Image.frombuffer("L", (x, y), opticalData, "raw", "L", 0, 1)
        x = img.size[0]
        y = getCompressedImageHeight(img.size[1])
        img = img.resize((x, y))
        pdata   = array.array('B', img.tostring())
        fitsimg = numarray.reshape(pdata, y, x)
        home = os.environ['HOME']
        date = os.popen('date -u +%d%b%y').readlines()[0].strip()
        antname = device.Carma(ant).getCarmaAntname()
        subarrayNum = Subarray.getSubarrayNo()
        mp = "Control.Subarray%d.Commands.Track.sourcename" %subarrayNum
        source = queryString(mp, qmax)
        datestring = os.popen('date -u +%Y-%m-%d').readlines()[0].strip()
        dirname = 'stars-%s' %datestring
        fitsfile = '%s/%s/%s-%s.fits' % (home,dirname,source,antname)
        date = os.popen('date -u +%Y-%m-%dT%H:%M:%S').readlines()[0].strip()
        os.system('mkdir -p %s/%s; rm -f %s' % (home,dirname,fitsfile))
        if len(source) == 0:  source='ANON'
        if ant <= 6: prefix = "Ovro%d.AntennaCommon.Drive.Track." %ant
        else :       prefix = "Bima%d.AntennaCommon.Drive.Track." %ant
        az=queryDouble(prefix + "requestedAzimuth",   qmax)
        el=queryDouble(prefix + "requestedElevation", qmax)
        print "Writing fits file %s" %fitsfile
        fitsobj = pyfits.HDUList()
        hdu = pyfits.PrimaryHDU()
        hdu.data = fitsimg
        h = hdu.header
        # some of these header variables should come from variables!
        cdelt = 1.0/nativeRes/60.0
        h.update('CRPIX1',100.0)
        h.update('CRVAL1',0.0,       comment='center arbitrary')
        h.update('CDELT1',-cdelt,comment='3"/pixel in lowres OVRO mode')
        h.update('CTYPE1','AZIM-TAN')
        h.update('CRPIX2',160.0)
        h.update('CRVAL2',0.0,        comment='center arbitrary')
        h.update('CDELT2',cdelt,comment='3"/pixel in lowres OVRO mode')
        h.update('CTYPE2','ELEV-TAN')
        h.update('DATE-OBS',date, comment='date -u => UT')
        h.update('OBJECT', source, comment='Ref: CARMA source catalog')
        h.update('AZ---SRC',az,comment='Azimuth of source')
        h.update('EL---SRC',el,comment='Elevation of source')
        h.update('TELESCOP','CARMA',comment='See INSTRUME for which antenna')
        h.update('INSTRUME','Optical Refractor %s' %antname)
        h.update('PLATSCAL',nativeRes,comment='CCD Resolution pixels/arcmin')
        h.update('SOFTWARE','CARMA/scripts/python/cameraControl.py')
        fitsobj.append(hdu)
        fitsobj.writeto(fitsfile)  
        # End of writeFits
                  
    antref = device.Carma(ant)
    root_ = None
    t0 = time.time()
    bg = subtractBackground
    maxContrast = True
    rawimg = s.getImage(numFrames, bg, bg, maxContrast, ant)
    elapsed = int(1000*(time.time() - t0))
    x = rawimg.x
    y = rawimg.y
    print "Image size =", (x,y), "  Time to get frame =", elapsed, "msec" 
    if writeFITS: writefits(x, y, rawimg.fovWidth, rawimg.opticalData)
    if not show: return
    img = Image.frombuffer( "L", (x, y), rawimg.opticalData, "raw", "L", 0, 1 )
    titleString = "%s Optical Camera" %antref.getLabel()
    root_ = Tkinter.Tk()
    root_.title(titleString)
    # size=5 is unreadable on macosx X11, but 10 is way too big on linux
    font = tkFont.Font(family="Arial", size=8) 
    menubar = Tkinter.Menu()
    menubar.add_command(label="Quit",  command=closeSnapshot)
    # To capture the "windowClose" clicks
    root_.protocol("WM_DELETE_WINDOW", closeSnapshot)
    canvas_ = Tkinter.Canvas(width=x, height=y, bg="blue")
    tkimg_  = ImageTk.PhotoImage(img)    
    imageHandle_ = canvas_.create_image(x/2+1, y/2+1, image=tkimg_)
    canvas_.pack()
    root_.config(menu=menubar)

    root_.mainloop()
