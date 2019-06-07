/**
 * OpticalTelCommon control interface for CARMA
 *
 * @author Colby Gutierrez-Kraybill
 * $Revision: 1.67 $
 * $Date: 2014/02/19 22:24:29 $
 * $Id: OpticalTelCommon.cc,v 1.67 2014/02/19 22:24:29 scott Exp $
 *
 */

#if 1
#define COUT(statement) \
  {\
  }
#else
#define COUT(statement) \
  {\
    std::ostringstream _macroOs; \
    _macroOs << statement << std::endl; \
    std::cout << _macroOs.str();	\
  }
#endif

#include "carma/antenna/common/OpticalTelCommon.h"

#include "carma/antenna/common/FrameContext.h"
#include "carma/antenna/common/FrameGrabber.h"
#include "carma/antenna/common/Image.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/UserException.h"

// CARMA Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// Other includes
#include <cstdlib>
#include <string>

using namespace carma;
using namespace carma::util;
using namespace carma::antenna::common;
using namespace carma::monitor;
using namespace cimg_library;
using namespace log4cpp;
using namespace std;

namespace {

    FrameGrabber::ResolutionType
    getFrameGrabberResolution( const carma::antenna::common::Resolution res )
    {
        switch ( res ) {
            case HIGH_RES: return FrameGrabber::HI_RES;
            case MEDIUM_RES: return FrameGrabber::MID_RES;
            case LOW_RES: return FrameGrabber::LO_RES;
            default:
                throw CARMA_EXCEPTION( IllegalArgumentException,
                                       "Invalid enum." );
        }
    }

    template<typename T>
    void
    rotate( pair<T, T> & resolution, const float rotationInDegrees )
    {
        if ( rotationInDegrees == 90.0f || rotationInDegrees == 270.0f ) {
            ::std::swap( resolution.first, resolution.second );
        }
    }

    typedef ScopedExclusiveLock< PthreadRWLock > ScopedWriteLock;
    typedef ScopedSharedLock< PthreadRWLock > ScopedReadLock;

} // namespace < unnamed >

string
OpticalTelCommon::getResolutionAsString(
    const carma::antenna::common::Resolution res )
{
    switch ( res ) {
        case HIGH_RES: return "HIGH_RES";
        case MEDIUM_RES: return "MEDIUM_RES";
        case LOW_RES: return "LOW_RES";
        default: return "<error>";
    }
}

void
OpticalTelCommon::copyFrameToFOI( Image & image,
                                  flattenedOpticalImage * foi,
                                  const FrameContext & frameContext,
                                  const float rotationInDegrees,
                                  const float azFieldOfViewInArcminutes,
                                  const float elFieldOfViewInArcminutes,
                                  bool normalizeImage )
{
    compileTimeCheck<
        sizeof(RawImage::value_type) == sizeof(CORBA::Octet)>();

    const Image::value_type rawMin =
        numeric_limits<RawImage::value_type>::min();
    const Image::value_type rawMax =
        numeric_limits<RawImage::value_type>::max();


    // If we have negative pixels, rescale so smallest pixel value is 0.
    if ( image.min() < rawMin ) {
        const double absmin = ::fabs( image.min() );
        image += absmin;
        // Normalize if image exceeds max
        if ( absmin + image.max() >= rawMax + 1.0 ) {
            normalizeImage = true;
        }
    }

    if ( normalizeImage ) {
        image.normalize( rawMin, rawMax );
    }

    RawImage rawimage( image );

    foi->x = rawimage.width();
    foi->y = rawimage.height();

    const pair< short, short > res = frameContext.getResolution( );
    foi->xRes = res.first;
    foi->yRes = res.second;

    const pair< short, short > maxRes = frameContext.getMaxResolution( );
    foi->xMax = maxRes.first;
    foi->yMax = maxRes.second;

    foi->fovWidth = azFieldOfViewInArcminutes;
    foi->rotationInDegrees = rotationInDegrees;

    foi->numImages = 1;
    foi->pixelDepth = rawimage.spectrum() * 8;

    foi->opticalData.length( rawimage.size() );

    ::memcpy( static_cast< void * >( foi->opticalData.get_buffer() ),
            static_cast< void * >( rawimage.data() ),
            rawimage.size() * sizeof( ::CORBA::Octet ) );
}

namespace {

    void
    logCentroidResult( const CentroidResult & result )
    {
        ostringstream msg;
        msg << "CentroidResults - "
            << "valid=" << ( result.valid == 1 ? "true" : "false" ) << ", "
            << "size=" << result.sizeInArcminutes << " arcmin, "
            << "offset(arcmin)=( "
            << result.xOffsetInArcminutes << ", " << result.yOffsetInArcminutes
            << " ), aperturePixelCount=" << result.aperturePixelCount << ".";

        programLogInfoIfPossible( msg.str( ) );
    }

    CentroidResult
    findSingleCentroid( const Image & frameToCentroid,
                        const FrameContext & frameContext,
                        const CORBA::UShort numEdgePixels,
                        const CORBA::UShort apertureRadiusPixels,
                        const CORBA::Float pixelThresholdSigma,
                        const float azFieldOfViewInArcminutes,
                        const float elFieldOfViewInArcminutes )
    {
        CentroidResult result;
        result.valid = 0;
        result.peakPixelSNR = 0.0;
        result.aperturePixelCount = 0;
        result.xOffsetInArcminutes = 0.0f;
        result.yOffsetInArcminutes = 0.0f;
        result.sizeInArcminutes = 0.0f;

        Image frame = frameToCentroid;

        // Crop image to remove edge pixels on all sides checking to make
        // sure we don't crop the thing out of existence.
        if ( numEdgePixels > 0 ) {
            if ( numEdgePixels > frame.width( ) / 2 ||
                 numEdgePixels > frame.height( ) / 2 ) {
                ostringstream msg;
                msg << "findSingleCentroid( ) - Number of edge pixels "
                    << numEdgePixels << " crops image to nothing!";
                throw CARMA_EXCEPTION( UserException, msg.str().c_str( ) );
            }

            frame.crop( numEdgePixels, numEdgePixels,
                        ( frame.width( ) - 1 ) - numEdgePixels,
                        ( frame.height( ) - 1 ) - numEdgePixels );
        }

        {
            ostringstream msg;
            msg << "findSingleCentroid( ) - numEdgePixels=" << numEdgePixels
                << ", input frame (" << frameToCentroid.width()
                << ", " << frameToCentroid.height() << ") clipped frame ("
                << frame.width() << ", " << frame.height() << ").";
            programLogInfoIfPossible( msg.str( ) );
        }

        const CImgStats frameStats( frame.get_stats() );

        // Quit if the image is flat
        if ( frameStats.min == frameStats.max ) {
            result.errorString = CORBA::string_dup( "Image is flat." );
            return result;
        }

        // Find 'peak' value - this assumes that if there are multiple peak
        // values, they're adjacent and the peak is in the center.
        int x0 = frame.width(), y0 = frame.height();
        int x1 = -1, y1 = -1;
        int x = 0, y = 0;
        cimg_forXY( frame, x, y ) {
            if ( frame( x, y ) == frameStats.max ) {
                if ( x < x0 )
                    x0 = x;
                if ( x > x1 )
                    x1 = x;
                if ( y < y0 )
                    y0 = y;
                if ( y > y1 )
                    y1 = y;
            }
        }

        const int peakX = ( x0 + x1 ) / 2;
        const int peakY = ( y0 + y1 ) / 2;

        const float sigma = ::sqrtf( frameStats.variance );
        result.peakPixelSNR = (frameStats.max - frameStats.mean) / sigma;

        // Create mask containing pixels greater than 'clipThreshold' intensity.
        const float clipThreshold =
            frameStats.mean + pixelThresholdSigma * sigma;
        const Image brightPixelMask = frame.get_threshold( clipThreshold );

        // Create mask containing pixels within 'apertureRadiusPixels' of peak.
        const Image::value_type color[1] = { 1 };

        Image radiusMask( frame.width( ), frame.height( ), 1, 1 );
        radiusMask.draw_circle( peakX, peakY, apertureRadiusPixels, color );

        // Final mask is the bitwise AND of the above two masks which are
        // pixels within apertureRadiusPixels of peak AND above clipThreshold.
        radiusMask &= brightPixelMask;

        result.aperturePixelCount =
            static_cast<CORBA::UShort>( radiusMask.sum( ) );

        // Apply mask to frame - frame mean.
        Image maskedImage = ( frame - frameStats.mean ).mul( radiusMask );

        // Find centroid as the x and y weighted average from our peak.
        x = 0;
        y = 0;
        double xFromPeak = 0.0, yFromPeak = 0.0;
        double weightedSumX = 0.0, weightedSumY = 0.0, sum = 0.0;
        double weightedSumXX = 0.0, weightedSumYY = 0.0;
        double radiusFromPeak = 0.0;
        cimg_forXY( maskedImage, x, y ) {
            xFromPeak = x - peakX;
            yFromPeak = y - peakY;
            radiusFromPeak = ::hypot( xFromPeak, yFromPeak );
            if ( radiusFromPeak < static_cast<double>(apertureRadiusPixels) ) {
                sum += maskedImage(x,y);
                weightedSumX += xFromPeak * maskedImage(x,y);
                weightedSumY += yFromPeak * maskedImage(x,y);
                weightedSumXX += xFromPeak * xFromPeak * maskedImage(x,y);
                weightedSumYY += yFromPeak * yFromPeak * maskedImage(x,y);
            }
        }

        // If the sum isn't at least 1 everything was masked out - fail.
        if ( sum < 0.5 ) {
            logCentroidResult( result );
            result.errorString =
                CORBA::string_dup( "No bright pixels within aperture." );
            return result;
        }

        const double meanX = weightedSumX / sum;
        const double meanY = weightedSumY / sum;

        const double xOffset = (meanX + peakX) - ( frame.width()/2.0 );
        const double yOffset = ( frame.height()/2.0 ) - ( meanY + peakY );

        const pair<short, short> res = frameContext.getResolution( );

        if ( res.first == 0 || res.second == 0 ) {
            logCentroidResult( result );
            result.errorString = CORBA::string_dup( "Resolution 0x0." );
            return result;
        }

        const float azScale = azFieldOfViewInArcminutes / res.first;
        const float elScale = elFieldOfViewInArcminutes / res.second;
        result.xOffsetInArcminutes = xOffset * azScale;
        result.yOffsetInArcminutes = yOffset * elScale;

        // We define our star size as twice the weighted standard deviation.
        const double xStdDevInPixels = ::sqrt(
            ( weightedSumXX/sum - (weightedSumX * weightedSumX)/(sum * sum)) );
        const double yStdDevInPixels = ::sqrt(
            ( weightedSumYY/sum - (weightedSumY * weightedSumY)/( sum * sum)) );

        {
            ostringstream msg;
            msg << "Star size weightedSumXX=" << weightedSumXX
                << ", weightedSumX=" << weightedSumX << ".";
            programLogInfoIfPossible( msg.str( ) );
        }

        result.sizeInArcminutes = 2.0 * ::hypot( xStdDevInPixels * azScale,
                                                 yStdDevInPixels * elScale );

        result.valid = 1;

        logCentroidResult( result );

        return result;
    }

    void
    subtractBackground( Image & workImage,
                        const Image & backgroundImage,
                        const bool normalizeMedian )
    {
        if ( backgroundImage.width() != workImage.width() ||
             backgroundImage.height() != workImage.height() ||
             backgroundImage.size() != workImage.size() ) {
            ostringstream msg;
            msg << "Background image dimensions do not match input frame "
                << "dimensions.  Either a background image has not been taken, "
                << "or frame dimensions have been changed since the last "
                << "background image was taken.";
            throw CARMA_EXCEPTION( UserException, msg.str().c_str( ) );
        }

        if ( normalizeMedian ) {
            const Image::value_type offset =
                workImage.median() - backgroundImage.median();
            workImage -= ( backgroundImage + offset );
        } else {
            workImage -= backgroundImage;
        }

    }

} // namespace < unnamed >

OpticalTelCommon::OpticalTelCommon (
    carma::monitor::AntennaCommon::OpticalTel &mon,
    FrameGrabber &fg,
    const float azFieldOfViewInArcminutes,
    const float elFieldOfViewInArcminutes,
    const float rotationInDegrees,
    const bool simulate ) :
  seqNo_( 0 ),
  fg_( fg ),
  mon_( mon ),
  log_( Program::getLogger() ),
  azFieldOfViewInArcminutes_( azFieldOfViewInArcminutes ),
  elFieldOfViewInArcminutes_( elFieldOfViewInArcminutes ),
  grossRotationInDegrees_( rotationInDegrees ),
  fineRotationInDegrees_( 0.0f ),
  frameContext_( new FrameContext( rotationInDegrees,
                                   0.0f, // fine rotation
                                   FrameGrabber::getResolution(
                                        FrameGrabber::HI_RES ) ) ),
  fakeStarPosition_( make_pair( 0.0f, 0.0f ) ),
  fakeStarOffset_( make_pair( 0.0f, 0.0f ) ),
  simulate_( simulate )
{
  COUT("OPTC 0");
    CARMA_CPTRACE(Trace::TRACE5, "OpticalTelCommon - Creating.");

  COUT("OPTC 1");
    centroidResults_.length( 0 );

  COUT("OPTC 2");
    setFramegrabberResolution( HIGH_RES );

  COUT("OPTC 3");
    if ( simulate_ )
        setFakeStarHoldingWriteLock( ); // Ok, I'm not holding the write lock
                                        // but it's the ctor.
  COUT("OPTC 4");
}

OpticalTelCommon::~OpticalTelCommon()
{
  CARMA_CPTRACE(Trace::TRACE5, "~OpticalTelCommon() - D'tor.");
}

void
OpticalTelCommon::setFrameDimensions( const CORBA::Short x,
                                      const CORBA::Short y,
                                      const CORBA::Short x0,
                                      const CORBA::Short y0 )
try {

    log_ << Priority::INFO << "OpticalTelCommon::setFrameDimensions(x="
      << x << ", y=" << y << ", x0=" << x0 << ", y0=" << y0;

    ScopedWriteLock scopelock( rwLock_ );

    frameContext_->setDimensions( x, y, x0, y0 );

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
OpticalTelCommon::setBrightness( const CORBA::Float brightness )
try {
  COUT("Inside setBrightness 0");
    unsigned short actual;
    if ( brightness < 0.0 )
      actual = 0;
    else if (brightness > 1.0 )
      actual = 65535;
    else
      actual = static_cast<unsigned short>(brightness * 65535.0);

  COUT("Inside setBrightness 1");
    log_ << Priority::INFO << "OpticalTelCommon::setBrightness( "
        << "brightness=" << brightness << " (pegged to " << actual << ") ).";

    ScopedWriteLock scopelock( rwLock_ );

  COUT("Inside setBrightness 2");
    fg_.setBrightness( actual );
  COUT("Inside setBrightness 3");
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
OpticalTelCommon::setContrast( const CORBA::Float contrast )
try {
    unsigned short actual;
    if ( contrast < 0.0 )
      actual = 0;
    else if (contrast > 1.0 )
      actual = 65535;
    else
      actual = static_cast<unsigned short>(contrast * 65535.0);

    log_ << Priority::INFO << "OpticalTelCommon::setContrast( "
        << "contrast=" << contrast << " (pegged to " << actual << ") ).";

    ScopedWriteLock scopelock( rwLock_ );

    fg_.setContrast( actual );
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
OpticalTelCommon::setFramegrabberResolution( const Resolution res )
try {

    log_ << Priority::INFO << "OpticalTelCommon::setFramegrabberResolution( "
        << " res=" << getResolutionAsString( res ) << " ).";

    const double mjdStart = util::Time::MJD();

    const FrameGrabber::ResolutionType fgRes = getFrameGrabberResolution( res );

    pair<short, short> resolution = FrameGrabber::getResolution( fgRes );

    ScopedWriteLock scopelock( rwLock_ );

    frameContext_->setRawFrameResolution( resolution.first,
                                          resolution.second );
    fg_.setResolution( fgRes );

    const double mjdEnd = util::Time::MJD();

    ostringstream msg;
    msg << "OpticalTelCommon::setFramegrabberResolution( res="
        << getResolutionAsString( res ) << " ) - took "
        << ( ( mjdEnd - mjdStart ) * util::Time::SECONDS_PER_DAY )
        << " seconds to complete.";

    programLogInfoIfPossible( msg.str() );

    if ( simulate_ )
        setFakeStarHoldingWriteLock( );

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
OpticalTelCommon::setRotationAndFieldsOfView(
    const CORBA::Float rotationInDegrees,
    const CORBA::Float azFOVInArcminutes,
    const CORBA::Float elFOVInArcminutes )
try {

    log_ << Priority::INFO << "OpticalTelCommon::setRotationAndFieldsOfView( "
        << "rotationInDegrees=" << rotationInDegrees << ", "
        << "azFOVInArcminutes=" << azFOVInArcminutes << ", "
        << "elFOVInArcminutes=" << elFOVInArcminutes << " ).";

    ScopedWriteLock scopelock( rwLock_ );

    fineRotationInDegrees_ = rotationInDegrees;

    frameContext_->setImageRotation( rotationInDegrees );

    azFieldOfViewInArcminutes_ = azFOVInArcminutes;
    elFieldOfViewInArcminutes_ = elFOVInArcminutes;

    if ( simulate_ )
        setFakeStarHoldingWriteLock( );

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

flattenedOpticalImage *
OpticalTelCommon::grabFrame( )
{
    auto_ptr< flattenedOpticalImage > foi;

    try {
        log_ << Priority::INFO << "OpticalTelCommon::grabFrame().";

        Image rawFrame;

        ScopedReadLock scopelock( rwLock_ );

        rawFrame = fg_.grabFrame( 1 );

        frameContext_->rotateAndCrop( rawFrame );

        foi = auto_ptr< flattenedOpticalImage >( new flattenedOpticalImage() );

        copyFrameToFOI( rawFrame, foi.get( ),
                        *frameContext_, fineRotationInDegrees_,
                        azFieldOfViewInArcminutes_,
                        elFieldOfViewInArcminutes_,
                        false );

    } catch (...) {
        logCaughtAsErrorAndRethrowAsUser( log_ );
    }

    return foi.release( );
}

flattenedOpticalImage *
OpticalTelCommon::getImage( const CORBA::UShort numFrames,
                            const CORBA::Boolean subBackground,
                            const CORBA::Boolean normalizeMedian,
                            const CORBA::Boolean normalizeImage )
{
    auto_ptr< flattenedOpticalImage > foi;

    try {
        log_ << Priority::INFO << "OpticalTelCommon::getImage( numFrames="
             << numFrames << " ).";

  COUT("Inside OT getImage: 1");
        Image rawFrame;

        ScopedReadLock scopelock( rwLock_ );

  COUT("Inside OT getImage: 2");
        rawFrame = fg_.grabFrame( numFrames );

  COUT("Inside OT getImage: 3");
        frameContext_->rotateAndCrop( rawFrame );

        if ( subBackground ) {
            subtractBackground( rawFrame, backgroundImage_, normalizeMedian );
        }

        foi = auto_ptr< flattenedOpticalImage >( new flattenedOpticalImage() );

  COUT("Inside OT getImage: 4");
        copyFrameToFOI( rawFrame, foi.get( ),
                        *frameContext_, fineRotationInDegrees_,
                        azFieldOfViewInArcminutes_,
                        elFieldOfViewInArcminutes_,
                        normalizeImage );

  COUT("Inside OT getImage: 5");
    } catch (...) {
  COUT("Inside OT getImage: ERROR");

        logCaughtAsErrorAndRethrowAsUser( log_ );
    }

  COUT("Inside OT getImage: 6");
    return foi.release( );
}

void
OpticalTelCommon::takeBackgroundImage( const CORBA::UShort numFrames,
                                       const CORBA::ULong seqNo )
{
    {
        log_ << Priority::INFO
            << "OpticalTelCommon::takeBackgroundImage( "
            << "numFrames=" << numFrames << ", "
            << "seqNo=" << seqNo << " ).";
    }

    TakeBackgroundThreadArgs args( *this );

    args.numFrames = numFrames;
    args.seqNo = seqNo;

    StartPthreadWithCopy(
        &OpticalTelCommon::takeBackgroundThread,
        args,
        "OpticalTelCommon::takeBackgroundThread",
        &PthreadAttr( PTHREAD_CREATE_DETACHED ).InternalPthreadAttr( ) );

}

void
OpticalTelCommon::takeBackgroundThread( TakeBackgroundThreadArgs & args )
{
    Image rawBackgroundImage;

    {
        ScopedReadLock scopelock( args.This.rwLock_ );

        rawBackgroundImage = args.This.fg_.grabFrame( args.numFrames );

        args.This.frameContext_->rotateAndCrop( rawBackgroundImage );
    }

    { // Now that we're done swap out the background image and sequence no
        ScopedWriteLock scopelock( args.This.rwLock_ );

        args.This.seqNo_ = args.seqNo;
        args.This.backgroundImage_ = rawBackgroundImage;
    }
}

flattenedOpticalImage *
OpticalTelCommon::getStoredBackgroundImage( )
{
    auto_ptr< flattenedOpticalImage > foi;

    try {
        log_ << Priority::INFO
             << "OpticalTelCommon::getStoredBackgroundImage().";

        foi = auto_ptr< flattenedOpticalImage >( new flattenedOpticalImage( ) );

        ScopedReadLock scopelock( rwLock_ );

        Image bgImg( backgroundImage_ );

        copyFrameToFOI( bgImg, foi.get( ),
                        *frameContext_,
                        fineRotationInDegrees_,
                        azFieldOfViewInArcminutes_,
                        elFieldOfViewInArcminutes_,
                        false );

    } catch (...) {
        logCaughtAsErrorAndRethrowAsUser( log_ );
    }

    return foi.release( );
}

void 
OpticalTelCommon::turn( carma::antenna::common::SwitchState state )
try {
    log_ << Priority::INFO << "turn";
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
OpticalTelCommon::findCentroid(
    const CORBA::UShort numFramesPerImage,
    const CORBA::UShort minValidCentroids,
    const CORBA::UShort maxCentroidAttempts,
    const CORBA::UShort numEdgePixels,
    const CORBA::UShort apertureRadiusPixels,
    const CORBA::Float pixelThresholdSigma,
    const CORBA::Boolean subBackground,
    const CORBA::Boolean normalizeMedian,
    const CORBA::ULong seqNo )
try {

    {
        log_ << Priority::INFO << "findCentroid( "
            << "numFramesPerImage=" << numFramesPerImage << ", "
            << "minValidCentroids=" << minValidCentroids << ", "
            << "maxCentroidAttempts=" << maxCentroidAttempts << ", "
            << "numEdgePixels=" << numEdgePixels << ", "
            << "apertureRadiusPixels=" << apertureRadiusPixels << ", "
            << "pixelThresholdSigma=" << pixelThresholdSigma << ", "
            << "subBackground=" << subBackground << ", "
            << "normalizeMedian=" << normalizeMedian << ", "
            << "seqNo=" << seqNo << " ).";
    }

    // Start find centroid thread and get out of dodge as quick as possible.
    OpticalTelCommon::FindCentroidThreadArgs args( *this );
    // I deliberately don't add these to the constructor since a moved argument
    // place could cause problems.  Named args in C++ would be nice.
    args.numFramesPerImage = numFramesPerImage;
    args.minValidCentroids = minValidCentroids;
    args.maxCentroidAttempts = maxCentroidAttempts;
    args.numEdgePixels = numEdgePixels;
    args.apertureRadiusPixels = apertureRadiusPixels;
    args.pixelThresholdSigma = pixelThresholdSigma;
    args.subBackground = subBackground;
    args.normalizeMedian = normalizeMedian;
    args.seqNo = seqNo;

    StartPthreadWithCopy(
        &OpticalTelCommon::findCentroidThread,
        args,
        "OpticalTelCommon::findCentroidThread",
        &PthreadAttr( PTHREAD_CREATE_DETACHED ).InternalPthreadAttr( ) );

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
OpticalTelCommon::findCentroidThread( FindCentroidThreadArgs & args )
try {
    args.This.log_ << Priority::INFO << "findCentroidThread started";

    CentroidResults results( args.maxCentroidAttempts );

    CORBA::ULong validCentroids = 0;

    {
        // Note I lock for *ALL* centroid attempts - I don't want any
        // parameters such as size or rotation changing out from under my feet.
        ScopedReadLock scopelock( args.This.rwLock_ );
        for ( CORBA::UShort i = 0; i < args.maxCentroidAttempts; ++i ) {

            CentroidResult result;
            result.aperturePixelCount  = 100;
            result.sizeInArcminutes    = 10.0;
            result.peakPixelSNR        = 0.0;
            result.xOffsetInArcminutes = 0.0;
            result.yOffsetInArcminutes = 0.0;

            try {
                Image frame = args.This.fg_.grabFrame( args.numFramesPerImage );
                args.This.frameContext_->rotateAndCrop( frame );

                if ( args.subBackground ) {
                    subtractBackground( frame,
                                        args.This.backgroundImage_,
                                        args.normalizeMedian );
                }

                result = findSingleCentroid(
                        frame,
                        *( args.This.frameContext_ ),
                        args.numEdgePixels,
                        args.apertureRadiusPixels,
                        args.pixelThresholdSigma,
                        args.This.azFieldOfViewInArcminutes_,
                        args.This.elFieldOfViewInArcminutes_);

            } catch ( ... ) {
                const string errMsg( getStringForCaught( ) );
                result.valid = 0;
                result.errorString = CORBA::string_dup( errMsg.c_str( ) );
            }

            results.length( results.length( ) + 1 );

            results[i] = result;

            if ( result.valid )
                ++validCentroids;

            if ( validCentroids >= args.minValidCentroids )
                break;

        }
    }

    { // Now that we're done, swap out the old seq no and results with the new
        ScopedWriteLock scopelock( args.This.rwLock_ );
        args.This.seqNo_ = args.seqNo;
        args.This.centroidResults_ = results;
    }

} catch ( ... ) {
    logCaughtAsError( );
}

CentroidResults *
OpticalTelCommon::getCentroidResults( )
{
    auto_ptr< CentroidResults > results;

    try {
        log_ << Priority::INFO << "getCentroidResults()";

        ScopedReadLock scopelock( rwLock_ );

        results = auto_ptr< CentroidResults >(
            new CentroidResults( centroidResults_ ) );

    } catch (...) {
        logCaughtAsErrorAndRethrowAsUser( log_ );
    }

    return results.release( );
}

void
OpticalTelCommon::setFakeStarHoldingWriteLock( )
{
    // Draw a fake star on the frame grabber test image.
    // 1) Start with an image resolution identical to frame grabber resolution.
    const pair< short, short > res = fg_.getResolution( );

    const Image::value_type background = 175.0; // Typical daytime pixel value
    Image fakeStar( res.first, res.second, 1, 1, background );

    // 2) Add noise and rotate it by the gross rotation amount (e.g. 90 degrees)
    //    to put it in a coordinate system grossly aligned with az/el.
    const float backgroundNoiseSigma = 1.0;
    fakeStar.noise( backgroundNoiseSigma );
    fakeStar.rotate( grossRotationInDegrees_ );

    // 3) Calculate 'star position' as position + offsets from center.
    pair<float, float> starPos; // arcminutes from center
    starPos.first = fakeStarPosition_.first + fakeStarOffset_.first;
    starPos.second = fakeStarPosition_.second + fakeStarOffset_.second;

    // 4) Find star position in finely rotated coordinate system by
    //    first calculating the field of views in the non-finely rotated c.s.
    //    (e.g. c.s of grossly rotated image only).  This is slightly tricky
    //    because our stored field of view values are for the
    //
    float theta = fineRotationInDegrees_ * ( M_PI / 180.f );
    const float xFov = azFieldOfViewInArcminutes_ /
                       ( ::cos( theta )  + ::fabs( 4.0 * ::sin( theta ) ) / 3.0 );
    const float yFov = 4.0 * xFov / 3.0; // Assume NTSC, not 100% accurate

    // Convert to pixels ( still from center ) note that +y is -el
    const float xPixels = ( starPos.first ) * ( fakeStar.width() / xFov );
    const float yPixels = -1.0 * ( starPos.second ) * ( fakeStar.height() / yFov );

    // Rotate opposite direction, assuming image is centered about origin
    theta = -1.0 * theta;
    float xPrimePixels = xPixels * ::cos( theta ) -
                         yPixels * ::sin( theta );

    float yPrimePixels = xPixels * ::sin( theta ) +
                         yPixels * ::cos( theta );

    // Translate back so that image exists entirely in 1st quadrant
    xPrimePixels += fakeStar.width() / 2;
    yPrimePixels += fakeStar.height() / 2;

    // Add some dither to better simulate real conditions
    xPrimePixels += ( rand() % 2 ? -1.0 : 1.0 ) *
        static_cast< float >( ::rand() ) / static_cast< float >( RAND_MAX );
    yPrimePixels += ( rand() % 2 ? -1.0 : 1.0 ) *
        static_cast< float >( ::rand() ) / static_cast< float >( RAND_MAX );

    // For a fake star, I draw an isotropic gaussian centered about the
    // specified offset. The 'size' of the star is determined by sigma, which
    // in turn is directly proportional to the resolution. Note that this is
    // not physically accurate. In reality, a star will not be isotropic due to
    // the fact that we have a 6:5 pixel aspect ratio which is arrived at by
    // mapping our standard 8:5 frame resolutions over a 4:3 NTSC video image.
    const int starX = static_cast<int>( xPrimePixels );
    const int starY = static_cast<int>( yPrimePixels );

    if ( starX < fakeStar.width() && starY < fakeStar.height() ) {

        const pair<short, short> hiRes =
            FrameGrabber::getResolution( FrameGrabber::HI_RES );
        const float sigma = 2.0 * (
            static_cast< float >( res.first) /
            static_cast< float >( hiRes.first ) ) ;
        const ::Image::value_type color[1] = { 50 };
        const float opacity = 1.0;

        fakeStar += Image( fakeStar.width(),
                           fakeStar.height(),
                           1, 1 ).draw_gaussian( starX, starY,
                                                 sigma, color, opacity );
    }

    fakeStar.rotate( -1.0 * grossRotationInDegrees_ );

    fg_.setTestImage( fakeStar );
}

void
OpticalTelCommon::applyTestOffset( const CORBA::Double azInArcmin,
                                   CORBA::Double elInArcmin )
try {
    ostringstream logmsg;
    logmsg << "OpticalTelCommon::applyTestOffset( azInArcmin="
        << azInArcmin << ", elInArcmin=" << elInArcmin << " ).";
    programLogInfoIfPossible( logmsg.str( ) );

    ScopedWriteLock scopelock( rwLock_ );

    fakeStarOffset_.first = -1.0f * azInArcmin;
    fakeStarOffset_.second = -1.0f * elInArcmin;

    setFakeStarHoldingWriteLock( );

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
OpticalTelCommon::zeroTestOffset( )
try {
    ScopedWriteLock scopelock( rwLock_ );

    fakeStarPosition_.first =
        fakeStarPosition_.first + fakeStarOffset_.first;
    fakeStarPosition_.second =
        fakeStarPosition_.second + fakeStarOffset_.second;
    fakeStarOffset_.first = 0.0;
    fakeStarOffset_.second = 0.0;

} catch (...) {
    logCaughtAsErrorAndRethrowAsUser( log_ );
}

void
OpticalTelCommon::writeMonitorData( )
{
    ScopedReadLock scopelock( rwLock_ );

    const pair<short, short> imgSize = frameContext_->getDimensions( );
    const pair<short, short> imgOffset = frameContext_->getOffset( );
    const float rotation = frameContext_->getImageRotationInDegrees( );

    mon_.sizeX().setValue( imgSize.first );
    mon_.sizeY().setValue( imgSize.second );
    mon_.offsetX().setValue( imgOffset.first );
    mon_.offsetY().setValue( imgOffset.second );
    mon_.azFov().setValue( azFieldOfViewInArcminutes_ );
    mon_.elFov().setValue( elFieldOfViewInArcminutes_ );
    mon_.imageRotation().setValue( rotation );
    mon_.centroidSeqNum().setValue( seqNo_ );
}

OpticalTelCommon::FindCentroidThreadArgs::FindCentroidThreadArgs(
    OpticalTelCommon & otcRef ) : This( otcRef ) { }

OpticalTelCommon::TakeBackgroundThreadArgs::TakeBackgroundThreadArgs(
    OpticalTelCommon & otcRef ) : This( otcRef ) { }
