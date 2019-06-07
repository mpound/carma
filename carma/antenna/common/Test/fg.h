
/**@file
 * FrameGrabber Device class definition.
 * 
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.1 $
 * $Date: 2005/03/08 19:56:35 $
 * $Id: fg.h,v 1.1 2005/03/08 19:56:35 colby Exp $
 */


#ifndef CARMA_FRAMEGRABBER_H
#define CARMA_FRAMEGRABBER_H

#include <iostream>
#include <string>
#include <vector>

#include <linux/types.h>
#include <linux/videodev.h>


#define FRAMEGRABBER_DEFAULT_DEV	"/dev/video0"
#define FRAMEGRABBER_DEFAULT_WIDTH	768
#define FRAMEGRABBER_DEFAULT_HEIGHT	480

namespace carma
{
  namespace antenna
  {
    namespace common
    {
        class FrameGrabber
  	{

	public:
	  FrameGrabber();

	  FrameGrabber( std::string devFileName );

	  ~FrameGrabber();

	  void init( std::string devFileName );
          void setChannel( int channel );
          void setImageParameters( int width, int height );
          void getImage( std::vector<char> &theImage );

        private:
	  int _grabBoard;
	  int _width;
	  int _height;
          void *_imageBuffer;
	  std::string _devFileName;
	  struct video_capability _vc;
          struct video_channel _cp;
          struct video_picture _vp;
          struct video_window _vw;
          struct video_mbuf _vm;
          struct video_mmap _vb;

	  void openFrameGrabber( std::string devFileName );
	  void setWindow( int width, int height );
	  void createCaptureBuffer();

      }; // class FrameGrabber
    
    } // namespace common 
  } // namespace antenna
} // namespace carma

#endif // CARMA_FRAMEGRABBER_H


