/**
 * @file
 *
 * carma::antenna::common::OpticalTelCommon control interface for CARMA
 *
 * @author Colby Gutierrez-Kraybill
 * $Revision: 1.43 $
 * $Date: 2013/04/10 22:55:26 $
 * $Id: OpticalTelCommon.h,v 1.43 2013/04/10 22:55:26 abeard Exp $
 *
 */
#ifndef CARMA_OPTICALTELCOMMON_H
#define CARMA_OPTICALTELCOMMON_H

#include "carma/corba/corba.h"

#include "carma/antenna/common/Image.h"
#include "carma/antenna/common/OpticalTelControl.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/UserException.h"

namespace log4cpp {
    // Forward dec
    class Category;
} // End namespace log4cpp

namespace carma
{
  namespace antenna
  {
    namespace common
    {

      class FrameContext;
      class FrameGrabber;

      class OpticalTelCommon 
      {
      public:
          /**
           * Constructor
           * This class implements all common Optical Telescope functionality
           * among the different antenna types.  This is possible due to the
           * fact that the frame grabber hardware is identical for all antennas.
           * Ovro and bima implementations subclass this to implement the
           * turn method.
           */
          OpticalTelCommon( carma::monitor::AntennaCommon::OpticalTel &monObj,
                            FrameGrabber & fg,
                            float azFieldOfViewInArcminutes,
                            float elFieldOfViewInArcminutes,
                            float rotationInDegrees,
                            bool simulate );

          virtual ~OpticalTelCommon( ); 

          void setFrameDimensions( CORBA::Short x, CORBA::Short y,
                  CORBA::Short x0 = 0, CORBA::Short y0 = 0 );

          void setBrightness( CORBA::Float brightness );

          void setContrast( CORBA::Float contrast );

          void setFramegrabberResolution( Resolution res );

          void setRotationAndFieldsOfView( CORBA::Float rotationInDegrees,
                                           CORBA::Float azFOVInArcminutes,
                                           CORBA::Float elFOVInArcminutes );

          flattenedOpticalImage * grabFrame();

          flattenedOpticalImage * getImage( CORBA::UShort numFrames,
                                            CORBA::Boolean subBackground,
                                            CORBA::Boolean normalizeMedian,
                                            CORBA::Boolean normalizeImage );

          void takeBackgroundImage( CORBA::UShort numFrames,
                                    CORBA::ULong seqNo );

          flattenedOpticalImage * getStoredBackgroundImage( );

          virtual void turn( carma::antenna::common::SwitchState state );

          void findCentroid( CORBA::UShort numFramesPerImage,
                             CORBA::UShort minValidCentroids,
                             CORBA::UShort maxCentroidAttempts,
                             CORBA::UShort numEdgePixels,
                             CORBA::UShort apertureRadiusPixels,
                             CORBA::Float pixelThresholdSigma,
                             CORBA::Boolean subBackground,
                             CORBA::Boolean normalizeMedian,
                             CORBA::ULong seqNo );

          CentroidResults * getCentroidResults( );

          void applyTestOffset( CORBA::Double azInArcmin,
                                CORBA::Double elInArcmin );

          void zeroTestOffset( );

          static void copyFrameToFOI( Image & image,
                                      flattenedOpticalImage * foi,
                                      const FrameContext & frameContext,
                                      const float rotationInDegrees,
                                      const float azFieldOfViewInArcminutes,
                                      const float elFieldOfViewInArcminutes,
                                      bool normalizeImage );

          static ::std::string
          getResolutionAsString( carma::antenna::common::Resolution res );

          void writeMonitorData( );

      protected:

          void setFakeStarHoldingWriteLock( );

          struct FindCentroidThreadArgs {

              FindCentroidThreadArgs( OpticalTelCommon & otcRef );

              CORBA::UShort numFramesPerImage;
              CORBA::UShort minValidCentroids;
              CORBA::UShort maxCentroidAttempts;
              CORBA::UShort numEdgePixels;
              CORBA::UShort apertureRadiusPixels;
              CORBA::Float pixelThresholdSigma;
              CORBA::Boolean subBackground;
              CORBA::Boolean normalizeMedian;
              CORBA::ULong seqNo;
              OpticalTelCommon & This;
          };

          static void
          findCentroidThread( FindCentroidThreadArgs & args );

          struct TakeBackgroundThreadArgs {

              TakeBackgroundThreadArgs( OpticalTelCommon & otcRef );

              CORBA::UShort numFrames;
              CORBA::ULong seqNo;
              OpticalTelCommon & This;
          };

          static void
          takeBackgroundThread( TakeBackgroundThreadArgs & args );

          // A note on synchronization here:  Its pretty complicated
          // based on the fact that we don't want framegrabber parameters
          // or image context to change while centroiding or taking a
          // background, but at the same time we need to make sure and write
          // monitor data in a timely manner.  The latter requirement means
          // we want to minimize the amount of time we block the monitor
          // system out since we don't want to miss the write deadline.
          // While not perfect, using a RWLock nicely solves this since it
          // allows the monitor system to go on writing while centroiding or
          // performing other time consuming tasks (i.e. touching hardware).
          // So for example, I'll coadd 100 frames while only read locking
          // so that the monitor system can still write without blocking for the
          // 1-2 seconds it takes to grab and coadd 100 frames.
          carma::util::PthreadRWLock rwLock_;

          CORBA::ULong seqNo_;

          FrameGrabber & fg_;
          carma::monitor::AntennaCommon::OpticalTel &mon_;
          log4cpp::Category &log_;

          float azFieldOfViewInArcminutes_;
          float elFieldOfViewInArcminutes_;
          const float grossRotationInDegrees_;
          float fineRotationInDegrees_; // !deprecated - use context

          CentroidResults centroidResults_;

          Image backgroundImage_;

          std::auto_ptr< FrameContext > frameContext_;

          ::std::pair<float, float> fakeStarPosition_; // arcmin from center
          ::std::pair<float, float> fakeStarOffset_; // arcmin from fake star
          const bool simulate_;

      }; // class OpticalTelCommon
    } // namespace common
  } // namespace antenna
} // namespace carma

#endif // CARMA_OPTICALTELCOMMON_H
