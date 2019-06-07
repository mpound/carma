# 
# Utility routines for analyzing and debugging antenna centroiding
# Also contains routines to compare python centroiding to antenna based
# centroiding
#

import numpy
import carma
from cameraControl import Camera
import time
import subarrayCommands as SAC

def centroidViaAntenna( antref, numImages=16, framesPerImage=1, \
                        subBackground=False, zoom=1.0 ):
    """Centroid on specified antenna using OpticalTelControl::findCentroid
       method directly. 
       Input:
        antref Handle to desired antenna reference
       Output:
        results as a list [ azoff, azerr, eloff, elerr, validimgs ] """

    print "******************** Centroiding with Antenna *********************"
    apertureRadiusPixels = 15
    numEdgePixels = 0 # Note that image edge pixels are typically cropped out 
    pixelThresholdSigma = 3.0 # Masks all pixel values below mean + 3.0 * sigma 
    normalizeMedian = False
    bgOffsetArcmin = 2.0 / zoom
    
    opticalTelRef = antref.opticalTel()
    driveRef = antref.drive()
    
    opticalTelRef.turn( carma.antenna.common.ON )
    driveRef.selectAperture( carma.antenna.common.DriveControl.OPTICAL )

    time.sleep( 5 ) # Let the CCD warm up, lens cap be removed, birds sing, etc

    if subBackground:
        print "Offsetting %1.f arcmins to sample background." %(bgOffsetArcmin)
        antnum = antref.getAntnum()
        SAC.offset( bgOffsetArcmin, bgOffsetArcmin,\
                    ants=[antnum], waiton=SAC.ALL )
        opticalTelRef.takeBackgroundImage( framesPerImage )  
        SAC.offset( 0.0, 0.0, ants=[antnum], waiton=SAC.ALL )

    print "Finding centroid." 
    results = opticalTelRef.findCentroid(\
                                  framesPerImage, numImages / 2, numImages,\
                                  numEdgePixels, apertureRadiusPixels,\
                                  pixelThresholdSigma, subBackground, \
                                  normalizeMedian )
    
    opticalTelRef.turn( carma.antenna.common.ON )
    antref.drive().selectAperture( carma.antenna.common.DriveControl.OPTICAL )

    avgresults = averageAntennaResults( results )
    printCentroidResults( avgresults )
    print ""
    return avgresults

def centroidViaPython( antref, numImages=16, framesPerImage=1,\
                       subBackground=False, zoom=1.0 ): 
    """Use cameraControl.py to obtain centroid results."""
    print "******************** Centroiding with Python *********************"
    c = Camera( antref )
    c.on( auto=True, object='', repeat=1, zoom=zoom, linger=False, \
          showGUI=False, dontClose=False, writeFITS=False, brightness=50, \
          apply=False, subtractBackground=subBackground, ncoadd=framesPerImage,\
          centroidLoopMax=numImages * 2, minsamples=numImages)
    pyresults = c.results() 
    printCentroidResults( pyresults )
    print ""
    return pyresults 

def averageAntennaResults( results ):
    """Average centroid results from a single antenna.
       Inputs:
           results: list of CentroidResult objects
       Outputs:
           list [azoff, azerror, eloffset, elerror, validimgs] in arcmins """

    xoffsetsArcmins = numpy.array([])
    yoffsetsArcmins = numpy.array([])
    valid = 0
    invalid = 0
    for result in results:
        if result.valid: 
            xoffsetsArcmins = numpy.append( xoffsetsArcmins,\
                                            result.xOffsetInArcminutes )
            yoffsetsArcmins = numpy.append( yoffsetsArcmins,\
                                            result.yOffsetInArcminutes )
            valid += 1
        else:
            invalid += 1

    if valid > 0:
        xmean = xoffsetsArcmins.mean()
        xerr = xoffsetsArcmins.std()
        ymean = yoffsetsArcmins.mean()
        yerr = yoffsetsArcmins.std()
        return [ xmean, xerr, ymean, yerr, valid]
    else: 
        return [ 0.0, 0.0, 0.0, 0.0, 0 ]

def printCentroidResults( results ):
    print "Antenna offsets (asec): %.1f +/- %.1f, %.1f +/- %.1f."\
            %( results[0] * 60.0, results[1] * 60.0,\
               results[2] * 60.0, results[3] * 60.0 )

def printAntennaResults( results ):
    for result in results:
        print "Centroid",
        if result.valid:
            print "valid,",
        else:
            print "invalid,",
        print "offsets (asec) %.1f, %.1f, SNR %.1f, size (asec) %.1f."\
            %( result.xOffsetInArcminutes*60.0, result.yOffsetInArcminutes*60.,\
               result.peakPixelSNR, result.sizeInArcminutes*60 )

def printCameraResults( results ):
    print "cameraControl offsets (asec): %.1f +/- %.1f, %.1f +/- %.1f."\
          %(results[0], results[1], results[2], results[3])

def compareAntennaAndPythonCentroiding( antref, zoom=1.0, repeat=1, subBackground=True ):

    # Centroid using python first to recycle the 'zoom' factor from
    # cameraControl.  This is kludgy as ideally I'd calculate this 
    # independently, but the calculation is not straight forward in 
    # cameraControl and would be more work than it at first might seem.
    results = list()
    for i in xrange( repeat ):
        pyresults = centroidViaPython( antref, subBackground=subBackground,\
                                       zoom=zoom )
        antresults = centroidViaAntenna( antref, subBackground=subBackground,\
                                         zoom=zoom )
        results.append( (pyresults, antresults) )

    print "**************************** Results *****************************"
    for i in xrange( repeat ):
        result = results[i]
        antresults = result[0]
        pyresults = result[1]
        azdiff = antresults[0] - pyresults[0]
        azerr = antresults[1] + pyresults[1]
        eldiff = antresults[2] - pyresults[2]
        elerr = antresults[3] + pyresults[3]
        print "Offset difference (asec): %.1f +/- %.1f, %.1f +/- %.1f."\
            %( azdiff * 60.0, azerr * 60.0, eldiff * 60.0, elerr * 60.0 )

    print ""
