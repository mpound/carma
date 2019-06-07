/** @file
 * Simple test application to retrieve a remote frame and output it to 
 * standard out.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.18 $
 * $Date: 2012/03/13 05:17:48 $
 * $Id: getremoteframe.cc,v 1.18 2012/03/13 05:17:48 abeard Exp $
 */


#include "carma/corba/Client.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/antenna/common/OpticalTelControl.h"
#include "carma/antenna/common/DriveControl.h"

#include <cmath>
#include <unistd.h>

using namespace std;
using namespace carma;
using namespace carma::antenna::common;
using namespace carma::util;

/**
 * @version $Revision: 1.18 $
 *
 * @usage \nUsage: grabRemoteFrame [imr=hostname] [antenna=ovro1] > img.pnm\n
 *
 * @description
 * Retrieve an optical telescope frame from a remote host and output it to
 * standard output in pnm format (see man pnm).  You can easily convert this
 * to your favorite format by piping the output to a pnm conversion routine 
 * such as pnmtojpeg.
 *
 * @key antenna    @mandatory s Antenna type and number (e.g. bima8)
 * @key        res          0 i Resolution of frame (0 High, 1 Medium, 2 Low)
 * @key     frames          1 i Frames per image.
 * @key       crop      false b Crop image?
 * @key      width          0 i Crop width.
 * @key     height          0 i Crop height.
 * @key    xoffset          0 i Crop x offset.
 * @key    yoffset          0 i Crop y offset.
 * @key   removebg      false b Remove background (must already exist).
 * @key   bgoffset        2.0 d Background offset in arcminutes.
 * @key  normalize      false b Normalize image to a fixed scale of 0-255.
 * @key brightness @noDefault i Set brightness [0-65535]
 * @key   contrast @noDefault i Set contrast [0-65535] 
 * @logger DEFAULT_FACILITY carma.antenna.common
 */
int Program::main()
{
    OpticalTelControl_var fg; // Frame grabber
    DriveControl_var drive;
    const string antenna = getStringParameter( "antenna" );
    const unsigned short frames = getIntParameter( "frames" );
    const Resolution res = static_cast<Resolution>( getIntParameter( "res" ) );
    const bool crop = getBoolParameter("crop");
    const short w = getIntParameter("width"), h = getIntParameter("height");
    const short x = getIntParameter("xoffset"), y = getIntParameter("yoffset");
    const bool normalizeImage = getBoolParameter( "normalize" );
    const bool removebg = getBoolParameter( "removebg" );
    const bool normalizeMedian = true;
    const double bgoffset = getDoubleParameter( "bgoffset" );
    
    const string fgPublishedName = "carma." + antenna + "." + OPTICAL_TEL_NAME;
    const string driveName = "carma." + antenna + "." + DRIVE_NAME; 

    // Retrieve DO reference to OpticalTel...
    corba::Client & client = getCorbaClient();
    try {
        fg = client.resolveName<OpticalTelControl>( fgPublishedName );
        drive = client.resolveName<DriveControl>( driveName ); 
    } catch (...) {
        cerr << "Unable to resolve either " << fgPublishedName << " or "
            << driveName << "." << endl;
        return EXIT_FAILURE;
    }

    // Well, we made it this far, what next... Let's try to get a frame.
    cerr << "Successfully resolved OpticalTelControl for " << antenna << endl;

    cerr << "Setting framegrabber resolution" << endl;
    fg->setFramegrabberResolution(res);

    if (crop) { 
        cerr << "Setting frame dimensions" << endl;
        fg->setFrameDimensions(w, h, x, y);
    }

    if ( parameterWasSpecified("brightness") ) {
        cerr << "Setting brightness" << endl;
        int b = getIntParameter("brightness");
        fg->setBrightness(b);
    }

    if ( parameterWasSpecified("contrast") ) {
        cerr << "Setting contrast" << endl;
        int c = getIntParameter("contrast");
        fg->setContrast(c);
    }

    flattenedOpticalImage_var foi;

    try {
        if ( removebg ) {
            cerr << "Setting parameters to remove background." << endl;
            drive->setOffset( bgoffset, bgoffset, 0 ); 
            sleep( 2 );
            fg->takeBackgroundImage( frames, 0 );
            drive->setOffset( 0.0, 0.0, 0 );
            sleep( 2 );
        }

        cerr << "Retrieving image" << endl;
        foi = fg->getImage( frames, removebg, normalizeMedian, normalizeImage );

    } catch (carma::util::UserException &ex) {
        cerr << "UserException caught: " << ex.errorMsg << endl;
        return EXIT_FAILURE;
    } catch (...) {
        const string err = getStringForCaught();
        cerr << "Unable to grab frame: " << err << endl;
        
        return EXIT_FAILURE;
    }

    cerr << "Sequence length: " << foi->opticalData.length() << endl;

    cerr << "Image data: " << endl;
    cerr << " depth: " << foi->pixelDepth << endl;
    cerr << " x: " << foi->x << endl;
    cerr << " y: " << foi->y << endl;
    cerr << " numImages: " << foi->numImages << endl;

    const int depth = static_cast<int>( 
        ::pow( 2.0, static_cast<double>( foi->pixelDepth ) ) - 1 );
    cout << "P5" << endl;
    cout << foi->x << " " << foi->y << endl;
    cout << depth << endl;
    for (unsigned i = 0; i < foi->opticalData.length(); i++) {
        cout << foi->opticalData[i];
    }
    return EXIT_SUCCESS;
}
