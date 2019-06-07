
/**@file
 * FrameGrabber Device class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.1 $
 * $Date: 2005/03/08 19:56:35 $
 * $Id: fg.cc,v 1.1 2005/03/08 19:56:35 colby Exp $
 */

#include <stdlib.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <fcntl.h>

#include <string>
#include <sstream>
#include <vector>

#include <errno.h>

#include "./fg.h"

using namespace std;

namespace carma
{
 namespace antenna
 {
  namespace common
  {

FrameGrabber::FrameGrabber ()
{
  init( FRAMEGRABBER_DEFAULT_DEV );
}

FrameGrabber::FrameGrabber ( string devFileName )
{
  init( devFileName );
}

void FrameGrabber::init( string devFileName )
{


  openFrameGrabber( devFileName );

  setChannel( 2 ); 
  setWindow( FRAMEGRABBER_DEFAULT_WIDTH, FRAMEGRABBER_DEFAULT_HEIGHT );
  createCaptureBuffer();

}


void FrameGrabber::openFrameGrabber ( string devFileName )
{
  _devFileName = devFileName;
  _grabBoard = open( _devFileName.c_str(), O_RDWR );
 
  if ( _grabBoard == -1 )
  {
     ostringstream errorMsg;
     errorMsg << "Unable to open frame grabber device file: " << _devFileName;
     cerr << errorMsg.str() << endl;
  }
}

void FrameGrabber::setWindow( int width, int height )
{

  _vw.x = _vw.y = 0;
  _vw.width = _width = width;
  _vw.height = _height = height;
  _vw.chromakey = 0;
  _vw.flags = 0;
  _vw.clips = NULL;
  _vw.clipcount = 0;

  if ( ioctl( _grabBoard, VIDIOCSWIN, &_vw ) == -1 )
    cerr << "Unable to set up grab window" << endl;
}


void FrameGrabber::setChannel( int channel )
{

  _cp.channel = channel;

  if ( ioctl( _grabBoard, VIDIOCGCHAN, &_cp ) == -1 )
    cerr << "Unable to query channel information" << endl;

  _cp.norm = VIDEO_MODE_NTSC;

  if ( ioctl( _grabBoard,  VIDIOCSCHAN, &_cp ) == -1 )
  {
    ostringstream oss;
    oss << "Unable to set channel to: " << channel;

    cerr << oss.str() << endl;
  }
}

void FrameGrabber::createCaptureBuffer ()
{
  _imageBuffer = NULL;

  if ( ioctl( _grabBoard, VIDIOCGMBUF, &_vm ) < -1)
    cerr << "Unable to query image grab buffer" << endl;
  
  _imageBuffer = mmap( 0, _vm.size, PROT_READ|PROT_WRITE, MAP_SHARED, _grabBoard, 0 );

  if ( (int)_imageBuffer == -1 )
  {
	const int err = errno;
    ostringstream errorMsg;	
    errorMsg << "Unable to mmap image buffer: " << strerror( err );
	
    cerr << errorMsg.str() << endl;
  }
}

void FrameGrabber::getImage( vector<char> &returnImage )
{
  void *rawBuffer;
  _vb.frame = 0;
  _vb.format = VIDEO_PALETTE_GREY;
  _vb.width = _width;
  _vb.height = _height;

  if ( ioctl( _grabBoard, VIDIOCMCAPTURE, &_vb ) == -1 )
  {
    const int err = errno;
    ostringstream errorMsg;	
    errorMsg << "Capture start" << strerror( err );
	
    cerr << errorMsg.str() << endl;
  }

  if ( ioctl( _grabBoard, VIDIOCSYNC, &_vb ) == -1 )
  {
    const int err = errno;
    ostringstream errorMsg;	
    errorMsg << "Capture sync" << strerror( err );
	
    cerr << errorMsg.str() << endl;
  }

  returnImage.resize( _vw.width * _vw.height ); // works for char vector

  rawBuffer = &returnImage[0];

  memcpy( rawBuffer, _imageBuffer, _vw.width * _vw.height );
}

void FrameGrabber::setImageParameters ( int width, int height )
{
  setWindow( width, height );
}



    } // namespace common
  } // namespace antenna
} // namespace carma
