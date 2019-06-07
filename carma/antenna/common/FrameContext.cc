#include "carma/antenna/common/FrameContext.h"

#include "carma/antenna/common/Image.h"
#include "carma/antenna/common/OpticalTelCommon.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/programLogging.h"

#include <iostream>

using namespace carma;
using namespace carma::antenna::common;
using namespace carma::util;
using namespace cimg_library;
using namespace std;

struct FrameContext::Impl {

    Impl( const float grossRotation, 
          const float fineRotation,
          const pair< short, short > & maxRawFrameResolution );

    void 
    setDimensions( const short w, const short h, 
                   const short x0, const short y0 );

    void 
    updateContextImage( );

    /**
     * Structure to hold resolutions and sizes (i.e. window).
     */
    typedef struct {
        short w; // Width
        short h; // Height
    } FrameSizeType;
    
    /**
     * Structure to hold specific coordinates.
     */
    typedef struct {
        short x;
        short y;
    } CoordinateType;


    FrameSizeType frameSize;           // Current frame dimensions.
    CoordinateType frameOffset;        // Current frame offset.
    Image contextImage;                // Context image
    float grossRotationInDegrees;      // Image rotation amount
    float fineRotationInDegrees;
    pair< short, short > rawFrameResolution;
    pair< short, short > maxResolution;
    const pair< short, short > maxRawFrameResolution;

}; // struct FrameContext::Impl

FrameContext::Impl::Impl( const float grossRotation,
                          const float fineRotation,
                          const pair< short, short > & maxRawFrameResolution ) :
    grossRotationInDegrees( grossRotation ),
    fineRotationInDegrees( fineRotation ),
    rawFrameResolution( maxRawFrameResolution ),
    maxRawFrameResolution( maxRawFrameResolution )
{
    updateContextImage( );
}

void 
FrameContext::Impl::setDimensions( const short w, const short h, 
                                   const short x0, const short y0 )
{
    // Check that new offset dimensions don't exceed current resolution
    const FrameSizeType newSize = { x0 + w, y0 + h };

    if ( newSize.w > contextImage.width() || 
         newSize.h > contextImage.height() ) {
        ostringstream msg;
        msg << "Requested dimensions (w=" << w << ", h=" << h << ") at "
            << "offset (x=" << x0 << ", y=" << y0 << ") exceed the current "
            << "resolution of (w=" << contextImage.width() << ", h="  
            << contextImage.height() << ")."; 
        programLogErrorIfPossible( msg.str( ) );
        throw CARMA_EXCEPTION( IllegalArgumentException, msg.str() );
    }

    frameSize.w = w;
    frameSize.h = h;
    frameOffset.x = x0;
    frameOffset.y = y0;
}

void
FrameContext::Impl::updateContextImage( )
{
    const float rotationInDegrees = 
        grossRotationInDegrees + fineRotationInDegrees;
    
    Image newContextImage( rawFrameResolution.first,
                           rawFrameResolution.second, 1, 1 );
    newContextImage.rotate( rotationInDegrees, 2 );
        
    Image maxFrame( maxRawFrameResolution.first, 
                    maxRawFrameResolution.second, 1, 1 );

    maxFrame.rotate( rotationInDegrees, 2 );

    maxResolution = make_pair( static_cast< short >( maxFrame.width() ), 
                               static_cast< short >( maxFrame.height() ) );

    contextImage = newContextImage;

    setDimensions( contextImage.width(), contextImage.height(), 0, 0 );
}

FrameContext::FrameContext( const float grossRotationInDegrees,
                            const float fineRotationInDegrees,
                            const pair< short, short > maxRawFrameResolution ) :
    impl_( new FrameContext::Impl( grossRotationInDegrees,
                                   fineRotationInDegrees, 
                                   maxRawFrameResolution ) )
{ }

FrameContext::~FrameContext() { }

void 
FrameContext::setRawFrameResolution( const short w, const short h )
{
    impl_->rawFrameResolution = make_pair( w, h );
    impl_->updateContextImage();
}

pair< short, short >
FrameContext::getResolution( ) const
{
    return make_pair( impl_->contextImage.width(), 
                      impl_->contextImage.height() );
}

pair< short, short >
FrameContext::getMaxResolution( ) const
{
    return impl_->maxResolution;
}

void 
FrameContext::setImageRotation( const float degrees )
{
    impl_->fineRotationInDegrees = degrees;
    impl_->updateContextImage();
}

float
FrameContext::getImageRotationInDegrees( ) const
{
    return impl_->fineRotationInDegrees;
}

void 
FrameContext::setDimensions( const short w, const short h, 
                             const short x0, const short y0 )
{
    impl_->setDimensions( w, h, x0, y0 );
}
    
pair< short, short > 
FrameContext::getDimensions( ) const
{
    return make_pair( impl_->frameSize.w, impl_->frameSize.h );
}

pair< short, short >
FrameContext::getOffset( ) const
{
    return make_pair( impl_->frameOffset.x, impl_->frameOffset.y );
}

void 
FrameContext::rotateAndCrop( Image & image ) const
{
    const float rotation = 
        impl_->grossRotationInDegrees + impl_->fineRotationInDegrees;
    image.rotate( rotation, 2 );

    image.crop(
        impl_->frameOffset.x, 
        impl_->frameOffset.y + impl_->frameSize.h - 1,
        impl_->frameOffset.x + impl_->frameSize.w - 1,
        impl_->frameOffset.y,
        false );
}
