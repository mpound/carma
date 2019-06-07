/**
 * @file
 * FrameGrabber Device class definition.
 * 
 * @Author Colby Gutierrez-Kraybill
 * @Author Andy Beard
 * $Revision: 1.40 $
 * $Date: 2014/02/18 23:06:49 $
 * $Id: FrameGrabber.h,v 1.40 2014/02/18 23:06:49 eml Exp $
 */
#ifndef CARMA_ANTENNA_COMMON_FRAMEGRABBER_H
#define CARMA_ANTENNA_COMMON_FRAMEGRABBER_H

#include "carma/antenna/common/Image.h"
#include "carma/util/PthreadMutex.h"
#include "carma/services/Table.h"

#include <memory>
#include <string>
#include <vector>

namespace carma {
namespace antenna {
namespace common {
            
    /**
     * Encapsulate hardware access to framegrabber device.
     * This class is purely for controlling and accessing the frame grabber
     * device.  No extra image processing (e.g. rotation, cropping, etc) is 
     * done within this class.
     */
    class FrameGrabber
    {
        public:

            /**
             * Constructor
             * @param fgdev Framegrabber device (e.g. /dev/video0)
             * @param fginput Framegrabber input from which to grab frames.
             * @param emulate Emulate using test image.
             * @see FrameGrabber::setTestImage
             */
            FrameGrabber( const std::string & fgdev,
                          int fginput,
                          bool emulate,
			  carma::services::Table* table=0);

            FrameGrabber( carma::services::Table* table);

            /**
             * Destructor
             */
            ~FrameGrabber();

            /**
             * Set pixel depth.
             * @param depth Pixel depth in bits.
             */
            void setPixelDepth( short depth );

            /**
             * Set brightness
             * @param b Unnormalized brightness value from [0,65535].
             */
            void setBrightness( short b );

            /** 
             * Set contrast
             * @param c Unnormalized contrast value [0,65535].
             */
            void setContrast( short c );
    
            /**
             * Supported resolution types.
             */
            typedef enum {
                HI_RES,   /**< 768x480 */
                MID_RES,  /**< 640x400 */
                LO_RES,   /**< 320x200 */
                RES_COUNT /**< Number of supported resolutions */
            } ResolutionType;

            /**
             * Set resolution
             * @param resolution Resolution.
             * @see ResolutionType
             */
            void setResolution( ResolutionType resolution );
    
            ::std::pair<short, short> getResolution( ) const;

            static ::std::pair<short, short> getResolution(ResolutionType res);

            /**
             * Grab a frame.
             * @numFrame Number of frames to integrate.
             */
            Image grabFrame( int numFrames = 1, std::vector<float>* data=0 ); 
             
            /**
             * Set test image when emulating.
             * If image size does not match the current resolution, it will 
             * be internally resized.
             */
            void setTestImage( const Image & image );

	    void setDebug(carma::services::Table* table) {
	      table_ = table;
	    }

    public:

	    carma::services::Table* table_;

            FrameGrabber( const FrameGrabber & ); // No copy
            FrameGrabber & operator=( const FrameGrabber & ); // No assignment
            
	    void initializeCaptureBuffer();
            void createCaptureBuffer();
            void unmapCaptureBuffer();
               
            class FrameGrabberPimpl;            // Hide video interface
            ::std::auto_ptr<FrameGrabberPimpl> pimpl_;

            ::std::pair<short, short> resolution_; // Current resolution
	    bool emulate_;

	    //------------------------------------------------------------
	    // Capture buffer info
	    //------------------------------------------------------------

            void* imageBuffer_;                   // Where the image is held.

#ifdef V4L2
	    std::vector<void*> captureBuffers_;
#endif

	    unsigned bufLen_;
	    unsigned bufOffset_;

            carma::util::PthreadMutex mutex_;      // Synchronize it all

            Image testImage_;

    }; // class FrameGrabber

} // namespace common 
} // namespace antenna
} // namespace carma
#endif // CARMA_FRAMEGRABBER_H
