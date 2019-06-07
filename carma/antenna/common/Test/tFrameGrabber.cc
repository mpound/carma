/**
 * @file
 * CppUnit Test harness to test FrameGrabber operations.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.11 $
 * $Date: 2010/07/12 20:23:21 $
 * $Id: tFrameGrabber.cc,v 1.11 2010/07/12 20:23:21 abeard Exp $
 */

#include "carma/corba/corba.h"

#include <iostream>

#include <memory>

#include <cppunit/TextTestRunner.h>
#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

#include "carma/antenna/common/FrameContext.h"
#include "carma/antenna/common/FrameGrabber.h"
#include "carma/antenna/common/OpticalTelCommon.h"
#include "carma/antenna/common/OpticalTelControl.h"
#include "carma/util/Program.h"

using namespace carma::antenna::common;
using namespace carma::util;
using namespace std;

namespace {

    const FrameGrabber::ResolutionType kFgResolution = FrameGrabber::LO_RES;
    const carma::antenna::common::Resolution kResolution = 
        carma::antenna::common::LOW_RES; 

    ::std::auto_ptr< flattenedOpticalImage >
    getTestFOI( FrameGrabber & fg,
                const float rotationInDegrees )
    {
        FrameContext frameContext( rotationInDegrees,
                0.0, // Fine rotation
                FrameGrabber::getResolution(
                    FrameGrabber::HI_RES ) );

        const pair<short, short> res = fg.getResolution( );

        frameContext.setRawFrameResolution( res.first, res.second );

        Image rawFrame = fg.grabFrame( );

        frameContext.rotateAndCrop( rawFrame );

        flattenedOpticalImage * foi = new flattenedOpticalImage();

        OpticalTelCommon::copyFrameToFOI( rawFrame, foi, frameContext, 
                                          rotationInDegrees, 0., 0., false );

        return auto_ptr< flattenedOpticalImage >( foi );
    }

    ::std::auto_ptr< flattenedOpticalImage >
    getTestFOI( FrameGrabber & fg,
                const float rotationInDegrees,
                const CORBA::Short x,
                const CORBA::Short y,
                const CORBA::Short x0,
                const CORBA::Short y0 )
    {
        FrameContext frameContext( rotationInDegrees,
                0.0, // Fine rotation
                FrameGrabber::getResolution(
                    FrameGrabber::HI_RES ) );

        const pair<short, short> res = fg.getResolution( );

        frameContext.setRawFrameResolution( res.first, res.second );

        frameContext.setDimensions( x, y, x0, y0 );

        Image rawFrame = fg.grabFrame( );

        frameContext.rotateAndCrop( rawFrame );

        flattenedOpticalImage * foi = new flattenedOpticalImage();

        OpticalTelCommon::copyFrameToFOI( rawFrame, foi, frameContext, 
                                          rotationInDegrees, 0.0, 0.0, false );

        return auto_ptr< flattenedOpticalImage >( foi );
    }

    /**
     * Set the filename for the test image to be read.
     * Test images are in raw file format with 1 byte per pixel.
     * Test images for this test are in raw format with the name
     * of the image suffixed by the width of the image followed by '.raw'.
     */
    void setTestImageFilenamePrefix( const ::std::string & prefix,
                                     FrameGrabber & fg )
    {
        // Form up our filename
        ostringstream filename;
        string dir = Program::getConfDir();
        const pair<short, short> fgRes = fg.getResolution( );

        filename << dir << prefix << fgRes.first << ".raw";

        RawImage testImage;
        testImage.load_raw( filename.str().c_str(), 
                            fgRes.first, fgRes.second, 1, 1 );

        fg.setTestImage( testImage );
    }

}

class FrameGrabberTest : public CppUnit::TestFixture {
public:

    void setUp();

    void tearDown();

    CPPUNIT_TEST_SUITE( FrameGrabberTest );

    CPPUNIT_TEST( basicRotationTest );
    CPPUNIT_TEST( basicCropTest );
    CPPUNIT_TEST( offsetPixelCropTest );
    CPPUNIT_TEST( shadedBoxCropTest );

    CPPUNIT_TEST_SUITE_END();

    void basicRotationTest();
    void basicCropTest();
    void offsetPixelCropTest();
    void shadedBoxCropTest();

private:
    
    // Helper routines.
    void setFilenamePrefix(const ::std::string prefix);

    carma::antenna::common::FrameGrabber *fgNoRotation;
    carma::antenna::common::FrameGrabber *fg90Rotation;
    carma::antenna::common::FrameGrabber *fgNeg90Rotation;
    carma::antenna::common::FrameGrabber *fg180Rotation;

};

void FrameGrabberTest::setUp() 
{
    fgNoRotation = new FrameGrabber ( "NoDevice", 0, true );
    fg90Rotation = new FrameGrabber ( "NoDevice", 0, true );
    fgNeg90Rotation = new FrameGrabber ( "NoDevice", 0, true );
    fg180Rotation = new FrameGrabber ( "NoDevice", 0, true );

    fgNoRotation->setResolution( kFgResolution );
    fg90Rotation->setResolution( kFgResolution );
    fgNeg90Rotation->setResolution( kFgResolution );
    fg180Rotation->setResolution( kFgResolution );
}

void FrameGrabberTest::tearDown() 
{
    delete fgNoRotation;
    delete fg90Rotation;
    delete fgNeg90Rotation;
    delete fg180Rotation;
}

void FrameGrabberTest::setFilenamePrefix(const ::std::string prefix) 
{
    ::std::string preprefix = "data/antenna/framegrabber/" + prefix;
    setTestImageFilenamePrefix( preprefix, *( fgNoRotation ) );
    setTestImageFilenamePrefix( preprefix, *( fg90Rotation ) );
    setTestImageFilenamePrefix( preprefix, *( fgNeg90Rotation ) );
    setTestImageFilenamePrefix( preprefix, *( fg180Rotation ) );
}
    
void FrameGrabberTest::basicRotationTest() 
{
    setFilenamePrefix("pixel_top_left_corner_");

    // Test image contains single bright pixel (255) in the top left corner.
    // Make sure that pixel is in the proper location in rotated frames.
    auto_ptr< flattenedOpticalImage > foiNoRot(  
                           getTestFOI( *fgNoRotation, 
                                       0.0f ) );
    auto_ptr< flattenedOpticalImage > foi90Rot( 
                           getTestFOI( *fg90Rotation, 
                                       270.0 ) );
    auto_ptr< flattenedOpticalImage > foiNeg90Rot( 
                           getTestFOI( *fgNeg90Rotation, 
                                       90.0 ) );
    auto_ptr< flattenedOpticalImage > foi180Rot( 
                           getTestFOI( *fg180Rotation, 
                                       180.0 ) );

    // Test that the first pixel is on
    CPPUNIT_ASSERT( foiNoRot->opticalData[0] == 255 );
    CPPUNIT_ASSERT( foi90Rot->opticalData[319*200] == 255 ); 
    CPPUNIT_ASSERT( foiNeg90Rot->opticalData[199] == 255 );
    CPPUNIT_ASSERT( foi180Rot->opticalData[320*200 - 1] == 255 );
}    

void FrameGrabberTest::basicCropTest() 
{
    setFilenamePrefix("pixel_top_left_corner_");

    // Test image contains single bright pixel (255) in the top left corner.
    // Make sure that pixel is the only pixel in cropped 1x1 frames offset
    // to the position of the rotated pixel.
    auto_ptr< flattenedOpticalImage > foiCropped( 
                           getTestFOI( *fgNoRotation, 0.0f, 1, 1, 0, 0 ) );

    auto_ptr< flattenedOpticalImage > foi180Cropped(  
                           getTestFOI( *fg180Rotation, 180.0, 1, 1, 319, 199 ) );
        
    auto_ptr< flattenedOpticalImage > foi90Cropped(  
                           getTestFOI( *fg90Rotation, 270.0f, 1, 1, 0, 319 ) );

    auto_ptr< flattenedOpticalImage > foiNeg90Cropped( 
                           getTestFOI( *fgNeg90Rotation, 90.0f, 1, 1, 199, 0 ) );

    CPPUNIT_ASSERT( foiCropped->opticalData[0] == 255 );
    CPPUNIT_ASSERT( foi180Cropped->opticalData[0] == 255 );
    CPPUNIT_ASSERT( foi90Cropped->opticalData[0] == 255 );
    CPPUNIT_ASSERT( foiNeg90Cropped->opticalData[0] == 255 );
}

void FrameGrabberTest::offsetPixelCropTest() 
{
    // Set filename to 1 x 1 offset pixel test image.
    setFilenamePrefix("pixel_top_left_corner_1pix_offset_");
     
    // Test image contains a single bright pixel (255) at position (1,1).
    // Crop using that offset and a 1x1 dimension and verify that
    // cropped frame contains that pixel.
    auto_ptr< flattenedOpticalImage > foiNoRot( 
                           getTestFOI( *fgNoRotation, 0.0f, 1, 1, 1, 1 ) );

    auto_ptr< flattenedOpticalImage > foi180( 
                           getTestFOI( *fg180Rotation, 180.0, 1, 1, 318, 198 ) );

    auto_ptr< flattenedOpticalImage > foi90( 
                           getTestFOI( *fg90Rotation, 270.0f, 1, 1, 1, 318 ) );

    auto_ptr< flattenedOpticalImage > foiNeg90( 
                           getTestFOI( *fgNeg90Rotation, 90.0f, 1, 1, 198, 1 ) );

    CPPUNIT_ASSERT( foiNoRot->opticalData[0] == 255 );
    CPPUNIT_ASSERT( foi180->opticalData[0] == 255 );
    CPPUNIT_ASSERT( foi90->opticalData[0] == 255 );
    CPPUNIT_ASSERT( foiNeg90->opticalData[0] == 255 );
}

void FrameGrabberTest::shadedBoxCropTest()
{
    setFilenamePrefix("shaded_no_offset_");

    // Test image contains a 2x2 box with four different values.
    // Rotate and crop images to 2x2 and then verify that pixels are 
    // in the appropriate position with the appropriate values.
    auto_ptr< flattenedOpticalImage > foi( 
                          getTestFOI( *fgNoRotation, 0.0f, 2, 2, 0, 0 ) );
    auto_ptr< flattenedOpticalImage > foi90( 
                          getTestFOI( *fg90Rotation, 270.0f, 2, 2, 0, 318 ) );
    auto_ptr< flattenedOpticalImage > foiNeg90( 
                          getTestFOI( *fgNeg90Rotation, 90.0f, 2, 2, 198, 0 ) );
    auto_ptr< flattenedOpticalImage > foi180( 
                          getTestFOI( *fg180Rotation, 180.0, 2, 2, 318, 198 ) );

    CPPUNIT_ASSERT ( foi->opticalData[0] == 252 );
    CPPUNIT_ASSERT ( foi->opticalData[1] == 253 );
    CPPUNIT_ASSERT ( foi->opticalData[2] == 254 );
    CPPUNIT_ASSERT ( foi->opticalData[3] == 255 );

    CPPUNIT_ASSERT ( foi90->opticalData[0] == 253 );
    CPPUNIT_ASSERT ( foi90->opticalData[1] == 255 );
    CPPUNIT_ASSERT ( foi90->opticalData[2] == 252 );
    CPPUNIT_ASSERT ( foi90->opticalData[3] == 254 );

    CPPUNIT_ASSERT ( foiNeg90->opticalData[0] == 254 ); 
    CPPUNIT_ASSERT ( foiNeg90->opticalData[1] == 252 );
    CPPUNIT_ASSERT ( foiNeg90->opticalData[2] == 255 ); 
    CPPUNIT_ASSERT ( foiNeg90->opticalData[3] == 253 );

    CPPUNIT_ASSERT ( foi180->opticalData[0] == 255 );
    CPPUNIT_ASSERT ( foi180->opticalData[1] == 254 );
    CPPUNIT_ASSERT ( foi180->opticalData[2] == 253 );
    CPPUNIT_ASSERT ( foi180->opticalData[3] == 252 );
}


//
// @noKeys
//
// @logger TEST_FACILITY carma.test.antenna.common.tFrameGrabber
//
int Program::main() 
{
    try {

        bool result;
        CppUnit::TextTestRunner runner;
        runner.addTest( FrameGrabberTest::suite() );
        result = runner.run();
        return (result ? EXIT_SUCCESS : EXIT_FAILURE);
    } catch (...) {
        cerr << "main() - Unknown exception caught." << endl;
        return EXIT_FAILURE;
    }

    return 0;
}
