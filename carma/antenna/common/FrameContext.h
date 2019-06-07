#ifndef CARMA_ANTENNA_COMMON_FRAMECONTEXT_H
#define CARMA_ANTENNA_COMMON_FRAMECONTEXT_H

#include "carma/antenna/common/Image_fwd.h"

#include <memory>
#include <utility>

namespace carma {
namespace antenna {
namespace common {

/**
 * Optical telescopes and cameras on Carma dishes are not mounted uniformly and
 * thus plate scales vary substantially from dish to dish.  This class
 * maintains state information and provides methods for alligning a raw 
 * frame grabber image with a common az/el coordinate system.
 */
class FrameContext {
public:

    /**
     * Create a FrameContext.
     * Rotation assumes standard image processing coordinate system where the
     * top left of an image is defined as the (x,y) origin.  Max frame 
     * resolution refers to the maximum unrotated resolution.
     * @param grossRotationInDegrees The gross rotation is a 90 degree multiple
     *      representing the general rotation of the az/el cs, relative to 
     *      the optical telescope mount.
     * @param fineRotationInDegrees Small scale rotation representing fine 
     *      rotation necessary to align az/el coordinate system with raw frame.
     *      This is added directly to the grossRotation.
     * @param maxRawFrameResolution Maximum raw frame resolution as width 
     *                              height pair.
     */
    explicit FrameContext( float grossRotationInDegrees, 
                           float fineRotationInDegrees,
                           ::std::pair< short, short > maxRawFrameResolution );

    ~FrameContext( );

    /** 
     * Set raw frame resolution.
     * Set current resolution of the raw frames being returned by the frame 
     * grabber.
     */
    void setRawFrameResolution( short w, short h );

    /** 
     * Get resolution in context 
     * @return resolution as width height pair.
     */
    ::std::pair<short, short> getResolution( ) const;

    /**
     * Get maximum resolution of frame within context.
     */
    ::std::pair<short, short> getMaxResolution( ) const;

    /**
     * Set frame dimensions.
     * @param w Width of cropped frame in pixels
     * @param h Height of cropped frame in pixels
     * @param x0 X offset in pixels into frame (for cropping).
     * @param y0 Y offset in pixels into frame (for cropping).
     * @throw IllegalArgumentException if dimensions exceed current resolution.
     * @see setResolution
     */
    void setDimensions( short w, short h, short x0, short y0 );

    /**
     * Get dimensions as x,y pair (in pixels).
     */
    std::pair< short, short > getDimensions( ) const;

    /**
     * Get image offset (crop offset) in pixels.
     */
    std::pair< short, short > getOffset( ) const;

    /**
     * Set fine scale image rotation angle.
     * Rotation is about the center of the image in a standard image
     * coordinate system (orgin at top left of image).  In this CS
     * clockwise is positive.  The frame is rotated on top of a rectangular
     * canvas which is sized according to the rotation angle.  This routine 
     * resets dimensions to this canvas size and offsets to 0.  
     * @see setDimensions
     */
    void setImageRotation( float degrees );

    /**
     * Get rotation in degrees
     */
    float getImageRotationInDegrees( ) const;

    /**
     * Rotate and crop image into context frame of reference.
     */
    void rotateAndCrop( Image & image ) const;

private:

    struct Impl;
    ::std::auto_ptr< Impl > impl_;
    
}; // class FrameContext

}}} // namespace carma::antenna::common
#endif
