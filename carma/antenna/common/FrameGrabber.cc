/**@file
 * FrameGrabber Device class definition.
 *
 * @author Colby Gutierrez-Kraybill
 * @author Andy Beard
 * @author Erik Leitch -- added support for V4L2 and cleaned up some of the mess
 *
 * $Revision: 1.59 $
 * $Date: 2014/04/02 23:10:51 $
 * $Id: FrameGrabber.cc,v 1.59 2014/04/02 23:10:51 iws Exp $
 */

#define V4L2

#include "carma/antenna/common/FrameGrabber.h"

// System includes

#include <errno.h>
#include <fcntl.h>

#ifdef V4L2
#include <linux/videodev2.h>
#endif

#include <linux/videodev.h>

#include <sys/select.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <unistd.h>

// C++ STL includes

#include <vector>
#include <iosfwd>
#include <iostream>
#include <fstream>

// CARMA include

#include "carma/antenna/common/Image.h"
#include "carma/util/BaseException.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Trace.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

#if 0
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

using namespace std;
using namespace carma::util;
using namespace carma::antenna::common;

namespace { // Anonymous namespace for local constants and such
  
  const int            DEFAULT_PIXEL_DEPTH         =    8; // Bits
  const unsigned short DEFAULT_BRIGHTNESS          = 32768; 
  const unsigned short DEFAULT_CONTRAST            = 27648; 
  
} // End namespace <unnamed>

// -----------------------------------------------------------------------------
// FrameGrabber::FrameGrabberPimpl - Private Implementation for FrameGrabber
//                                 - Encapsulates Video Interface Details
// -----------------------------------------------------------------------------

class FrameGrabber::FrameGrabberPimpl {
public:
  
  FrameGrabberPimpl( ::std::string dev, int input, bool emulate, carma::services::Table* table=0 );
  
  ~FrameGrabberPimpl();
  
  void captureFrame( pair<short, short> resolution, unsigned int frame );
  unsigned syncFrame( unsigned int frame=0);
  void startCapture();
  void stopCapture();
  void waitForDevice();
  
  void* createMmapBuffer(unsigned& bufLen);
  void createMmapBuffers(std::vector<void*>& buffers, unsigned& bufLen);
  
  void setBrightness(unsigned short brightness = DEFAULT_BRIGHTNESS);
  
  void setContrast(unsigned short contrast = DEFAULT_CONTRAST);
  
  void setDepth(int depth = DEFAULT_PIXEL_DEPTH);
  
  void setInput(int input);
  
  void setResolution( pair<short, short> resolution );
  
  int getMaxMemoryBufferFrames( ) const;
  
  void queryCapabilities();
  void queryGrabWindow();
  void queryImageGrabBuffer();
  
public:
  
#ifdef V4L2
  struct ::v4l2_capability v4l2c_;
  struct ::v4l2_window v4l2w_;
  unsigned channels_;
  carma::services::Table* table_;
  unsigned nBuf_;
#endif
  // V4L1 Structures
  struct ::video_capability vc_;
  struct ::video_channel cp_;
  struct ::video_picture vp_;
  struct ::video_window vw_;
  struct ::video_mbuf vm_;
  struct ::video_mmap vb_;
  int fd_;  // FrameGrabber file descriptor
  bool emulate_;
  mutable int maxMemoryBufferFrames_; // Conceptually const
  
  void fgioctl( int request, void * mem, string errmsg );
  ::std::string getCapabilities();
  
}; // End FrameGrabber::FrameGrabberPimpl

/**.......................................................................
 * Pimpl constructor
 */
FrameGrabber::FrameGrabberPimpl::FrameGrabberPimpl(::std::string dev, 
                                                   int input,
                                                   bool emulate, 
						   carma::services::Table* table)
{
  table_      = table;
  fd_         = 0;
  emulate_    = emulate;
  nBuf_       = 0;

  if(!emulate_) {
    fd_ = open( dev.c_str(), O_RDWR );
    
    CARMA_TEST( (fd_ > -1), ErrorException,
                "Unable to open frame grabber device file "
                << dev << ": " << strerror( errno ) );
  }
  
  // Explicitly initialize the other V4L data structures we'll be using.  
  
  queryCapabilities();
  queryGrabWindow();
  queryImageGrabBuffer();

  // Initialize to requested settings

  setInput(input);
  setDepth();
  setBrightness();
  setContrast();

  CARMA_CPTRACE( Trace::TRACE4, getCapabilities() );
}

/**.......................................................................
 * Destructor merely closes the device node
 */
FrameGrabber::FrameGrabberPimpl::~FrameGrabberPimpl() 
{
  if(fd_ > 0) {
    close(fd_);
    fd_ = 0;
  }
};

/**.......................................................................
 * Select input channel
 */
void FrameGrabber::FrameGrabberPimpl::setInput( const int input ) 
{
#ifdef V4L2

  //------------------------------------------------------------
  // Under V4L2, we just set the current video input to the requested
  // input
  //------------------------------------------------------------

  fgioctl( VIDIOC_S_INPUT, (void*)&input, "Unable to query chan/input information: " );
  
  // Set the current standard to NTSC

  v4l2_std_id std = V4L2_STD_NTSC;
  fgioctl(VIDIOC_S_STD, &std, "Unable to set standard: ");

#else
  cp_.channel = input;
  
  fgioctl( VIDIOCGCHAN, &cp_, "Unable to query chan/input information: " );
  
  cp_.norm = VIDEO_MODE_NTSC;
  
  ostringstream errmsg;
  errmsg << "Unable to set channel/input to " << input << ": ";
  fgioctl( VIDIOCSCHAN, &cp_, errmsg.str( ) );
#endif
  
} // End FrameGrabber::FrameGrabberPimpl::setInput

/**.......................................................................
 * Change the brightness settings on the device
 */
void FrameGrabber::FrameGrabberPimpl::setBrightness(const unsigned short brightness)
{
  if(emulate_)
    return;

#ifdef V4L2
  struct v4l2_queryctrl qCtl;
  qCtl.id = V4L2_CID_BRIGHTNESS;
  fgioctl( VIDIOC_QUERYCTRL, &qCtl, "Unable to query brightness control: " );
  
  if(brightness < qCtl.minimum || brightness > qCtl.maximum)
    throw CARMA_EXCEPTION( ErrorException, "Requested brightness is out of range for this device");

  struct v4l2_control ctl;
  ctl.id = V4L2_CID_BRIGHTNESS;
  ctl.value = brightness;
  fgioctl( VIDIOC_S_CTRL, &ctl, "Unable to set brightness: " );

#else
  fgioctl( VIDIOCGPICT, &vp_, "Unable to get image propery information: " );
  
  vp_.brightness = brightness; // Change only the brightness
  
  fgioctl( VIDIOCSPICT, &vp_, "Unable to set image property information: " );
#endif
  
} // End FrameGrabber::FrameGrabberPimpl::setBrightness

/**.......................................................................
 * Change the contrast settings on the device
 */
void FrameGrabber::FrameGrabberPimpl::setContrast(const unsigned short contrast)
{
  if(emulate_)
    return;

#ifdef V4L2
  struct v4l2_queryctrl qCtl;
  qCtl.id = V4L2_CID_CONTRAST;
  fgioctl( VIDIOC_QUERYCTRL, &qCtl, "Unable to query constrast control: " );
  
  if(contrast < qCtl.minimum || contrast > qCtl.maximum)
    throw CARMA_EXCEPTION( ErrorException, "Requested contrast is out of range for this device");

  struct v4l2_control ctl;
  ctl.id = V4L2_CID_CONTRAST;
  ctl.value = contrast;
  fgioctl( VIDIOC_S_CTRL, &ctl, "Unable to set contrast: " );
#else
  fgioctl( VIDIOCGPICT, &vp_, "Unable to get image propery information: " );
  
  vp_.contrast = contrast; // Change only the contrast
  
  fgioctl( VIDIOCSPICT, &vp_, "Unable to set image property information: " );
#endif  
} // End FrameGrabber::FrameGrabberPimpl::setContrast

/**.......................................................................
 * Change the image depth
 */
void FrameGrabber::FrameGrabberPimpl::setDepth(const int depth)
{
#ifdef V4L2

  //------------------------------------------------------------
  // Under V4L2, the depth is implied by the format and is no longer
  // explicitly settable.  Palette has moved to the v4l2_pix_format
  // struct
  //------------------------------------------------------------

  struct v4l2_format fmt;
  fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  fgioctl(VIDIOC_G_FMT, &fmt, "Unable to get image format: ");

  fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_GREY;
  fmt.fmt.pix.field       = V4L2_FIELD_INTERLACED;

  fgioctl(VIDIOC_S_FMT, &fmt, "Unable to set image format: ");
#else
  fgioctl( VIDIOCGPICT, &vp_, "Unable to get image propery information: " );
  
  vp_.palette = VIDEO_PALETTE_GREY;
  vp_.depth = depth; 
  
  fgioctl( VIDIOCSPICT, &vp_, "Unable to set image property information: " );
#endif
  
} // End FrameGrabber::FrameGrabberPimpl::setDepth

/**.......................................................................
 * Set the resolution of the captured image
 */
void FrameGrabber::FrameGrabberPimpl::setResolution(const pair<short, short> resolution)
{
#ifdef V4L2
  enum v4l2_priority prio;
  fgioctl(VIDIOC_G_PRIORITY, &prio, "Unable to query priority: ");

  struct v4l2_format fmt;
  fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  fgioctl(VIDIOC_G_FMT, &fmt, "Unable to get image format: ");

  fmt.fmt.pix.width  = resolution.first;
  fmt.fmt.pix.height = resolution.second;

  try {
    fgioctl(VIDIOC_S_FMT, &fmt, "Unable to set image resolution: ");
  } catch(...) {
  }
#else
  vw_.x = vw_.y = 0;
  vw_.width = resolution.first;
  vw_.height = resolution.second;
  vw_.chromakey = 0;
  vw_.flags = 0;
  vw_.clips = NULL;
  vw_.clipcount = 0;
  
  fgioctl( VIDIOCSWIN, &vw_, "Unable to set up grab window: " );
#endif
  
} // End FrameGrabber::FrameGrabberPimpl::setResolution

int FrameGrabber::FrameGrabberPimpl::getMaxMemoryBufferFrames( ) const
{
  return maxMemoryBufferFrames_; // Conceptually const
}

/**.......................................................................
 * Create the mmap'd buffer we will use to read back images from the
 * grabber
 */
void* FrameGrabber::FrameGrabberPimpl::createMmapBuffer(unsigned& bufLen)
{
  void* retval = NULL;
#ifndef V4L2
  fgioctl( VIDIOCGMBUF, &vm_, "Unable to query image grab buffer: " );
  retval = mmap(0, vm_.size, PROT_READ|PROT_WRITE, MAP_SHARED, fd_, 0);

  bufLen     = b.vm_.size;
#endif
  
  CARMA_TEST( reinterpret_cast<long>(retval) != -1,
	      ErrorException,
	      "Unable to mmap image buffer: " << strerror( errno ) );
  
  return retval;
  
} // End FrameGrabber::FrameGrabberPimpl::createMmapBuffer

/**.......................................................................
 * Create the mmap'd buffer we will use to read back images from the
 * grabber
 */
void FrameGrabber::FrameGrabberPimpl::createMmapBuffers(std::vector<void*>& buffers, unsigned& bufLen)
{
#ifdef V4L2
  v4l2_requestbuffers rb;
  rb.count  = buffers.size();
  rb.type   = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  rb.memory = V4L2_MEMORY_MMAP;

  fgioctl(VIDIOC_REQBUFS, &rb, "Unable to request capture buffers: ");

  nBuf_ = buffers.size();

  //------------------------------------------------------------
  // Now memory map the buffers
  //------------------------------------------------------------

  for(unsigned i=0; i < buffers.size(); i++) {

    struct v4l2_buffer b;

    b.index  = i;
    b.type   = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    b.memory = V4L2_MEMORY_MMAP;
    
    fgioctl(VIDIOC_QUERYBUF, &b, "Unable to query capture buffer: ");
    
    buffers[i] = mmap(0, b.length, PROT_READ|PROT_WRITE, MAP_SHARED, fd_, b.m.offset);

    bufLen = b.length;
  }
#endif
} // End FrameGrabber::FrameGrabberPimpl::createMmapBuffer

/**.......................................................................
 * Initiate capture of a single frame
 */
void FrameGrabber::FrameGrabberPimpl::startCapture()
{
#ifdef V4L2
  for(unsigned i=0; i < nBuf_; i++) {
    struct v4l2_buffer b;
    b.index  = i;
    b.type   = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    b.memory = V4L2_MEMORY_MMAP;

    fgioctl(VIDIOC_QBUF, &b, "Unable to queue a buffer: ");
  }

  enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  fgioctl(VIDIOC_STREAMON, &type, "Unable to turn the capture stream on: ");
#endif
}

/**.......................................................................
 * Initiate capture of a single frame
 */
void FrameGrabber::FrameGrabberPimpl::captureFrame(const pair<short, short> resolution,
						   const unsigned int frame)
{
#ifndef V4L2
  vb_.frame = frame;
  vb_.format = VIDEO_PALETTE_GREY;
  vb_.width = resolution.first;
  vb_.height = resolution.second;
  
  fgioctl( VIDIOCMCAPTURE, &vb_, "Capture start, " );
#endif
  
} // End FrameGrabber::FrameGrabberPimpl

void FrameGrabber::FrameGrabberPimpl::stopCapture()
{
#ifdef V4L2
  enum v4l2_buf_type type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  fgioctl(VIDIOC_STREAMOFF, &type, "Unable to turn the capture stream off: ");
#else
  return;
#endif
}

/**.......................................................................
 * Terminate capture of a single frame
 */
unsigned FrameGrabber::FrameGrabberPimpl::syncFrame( const unsigned int frame )
{
  if(emulate_)
    return 0;

#ifdef V4L2
  waitForDevice();

  struct v4l2_buffer b;
  b.index = 0;
  b.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  b.memory = V4L2_MEMORY_MMAP;

  fgioctl(VIDIOC_DQBUF, &b, "Unable to dequeue a buffer: ");

  return b.index;
#else
  vb_.frame = frame;
  fgioctl( VIDIOCSYNC, &vb_, "Capture sync, " );
  return 0;
#endif
}

/**.......................................................................
 * Wait until the device is readable
 */
void FrameGrabber::FrameGrabberPimpl::waitForDevice() 
{
  fd_set fdSet;
  FD_ZERO(&fdSet);
  FD_SET(fd_, &fdSet);
  
  //------------------------------------------------------------
  // Wait until the file descriptor becomes readable
  //------------------------------------------------------------
  
  struct timeval tv;
  tv.tv_sec  = 2;
  tv.tv_usec = 0;

  int nready = select(fd_+1, &fdSet, NULL, NULL, &tv);

  if(nready < 0) {
    throw CARMA_EXCEPTION( ErrorException, "Error waiting for device to return a capture buffer");
  }

  if(nready == 0) {
    throw CARMA_EXCEPTION( ErrorException, "Timed out waiting for device to return a capture buffer");
  }
}

/**.......................................................................
 * Get device capabilities string
 */
::std::string FrameGrabber::FrameGrabberPimpl::getCapabilities() 
{
  ::std::ostringstream os;
  
#ifdef V4L2
  COUT("getCapabilities() was called");
#else
  // Format capabilities
  os << endl;
  os << "****************************" << endl;
  os << "Video Board Capabilities" << endl;
  os << "****************************" << endl;
  os << "Name: " << vc_.name << endl;
  os << "Channels (video/audio): " << vc_.channels << "/" << vc_.audios << endl;
  os << "Max Dimensions: " << vc_.maxwidth << "x" << vc_.maxheight << endl;
  os << "Min Dimensions: " << vc_.minwidth << "x" << vc_.minheight << endl;
  os << "Video Capture: " << (vc_.type & VID_TYPE_CAPTURE ? "" : "Not ") 
     << "Supported." << endl;
  os << "Video Tuner: " << (vc_.type & VID_TYPE_TUNER ? "" : "Not ") 
     << "Supported." << endl;
  os << "Teletext: " << (vc_.type & VID_TYPE_TELETEXT ? "" : "Not ") 
     << "Supported." << endl;
  os << "Video Overlay: " << (vc_.type & VID_TYPE_OVERLAY ? "" : "Not ") 
     << "Supported." << endl;
  os << "Chromakey Overlay: " << (vc_.type & VID_TYPE_CHROMAKEY ? "" : "Not ") 
     << "Supported." << endl;
  os << "Video Clipping: " << (vc_.type & VID_TYPE_CLIPPING ? "" : "Not ") 
     << "Supported." << endl;
  os << "Use Framebuffer: " << (vc_.type & VID_TYPE_FRAMERAM ? "" : "Not ") 
     << "Supported." << endl;
  os << "Scaleable: " << (vc_.type & VID_TYPE_SCALES ? "" : "Not ") 
     << "Supported." << endl;
  os << "Monochrome Only: " << (vc_.type & VID_TYPE_MONOCHROME ? "Yes" : "No") 
     << endl;
  os << "Subarea Capture: " << (vc_.type & VID_TYPE_SUBCAPTURE ? "" : "Not ")
     << "Supported." << endl;
  os << "Raw Type Bitfield: 0x" << hex << vc_.type << dec << endl;
  os << endl;
  os << "Capture Window Capabilities:" << endl;
  os << "Coordinates (X,Y): (" << vw_.x << "," << vw_.y << ")." << endl; 
  os << "Dimensions: " << vw_.width << "x" << vw_.height << endl;
  os << "Chromakey: " << vw_.chromakey << endl << endl;
  os << "Current Picture Properties:" << endl;
  os << "Brightness/Contrast: " << vp_.brightness << "/" 
     << vp_.contrast << endl;
  os << "Depth: " << vp_.depth << " bits." << endl;
  os << "Pallette: " << vp_.palette << endl;
  os << "Memory buffer frames: " << vm_.frames << endl;
  os << "****************************" << endl;
#endif
  return os.str(); 
}

/**.......................................................................
 * Wrapper around ioctl() with error handling
 */
void FrameGrabber::FrameGrabberPimpl::fgioctl(int request, void* mem, string errmsg)
{
  if(emulate_) 
    return;
  
  const int status = ioctl( fd_, request, mem );
  
  if(status == -1) {
    errmsg += strerror( errno );
    COUT("fgioctl: " << errmsg);
    throw CARMA_EXCEPTION( ErrorException, errmsg );
  }
}

void FrameGrabber::FrameGrabberPimpl::queryImageGrabBuffer()
{
#ifdef V4L2
  CARMALOGINFO("Querying buffers not supported under V4L2 API");
  maxMemoryBufferFrames_ = 1;
#else
  fgioctl( VIDIOCGMBUF, &vm_, "Unable to query image grab buffer: " );
  maxMemoryBufferFrames_ = vm_.frames;
#endif
}

void FrameGrabber::FrameGrabberPimpl::queryGrabWindow()
{
#ifdef V4L2
  struct v4l2_cropcap cc;
  cc.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  fgioctl(VIDIOC_CROPCAP, &cc, "Unable to query cropping limits: ");
  COUT("Available cropping bounds are: " << cc.bounds.width << " x " << cc.bounds.height);
#else
  fgioctl( VIDIOCGWIN, &vw_, "Unable to query grab window: " );
#endif
  
}

void FrameGrabber::FrameGrabberPimpl::queryCapabilities()
{
#ifdef V4L2
  fgioctl( VIDIOC_QUERYCAP, &v4l2c_, "Unable to query for video capabilities: " );
  
  // Get the number of video channels
  
  struct v4l2_input input;
  input.index = 0;
  
  if(!emulate_) {
    do {
      errno = 0;
      ioctl(fd_, VIDIOC_ENUMINPUT, &input);
      ++input.index;
    } while(errno != EINVAL);
  }
  errno = 0;
  
  // Set the number of video input channels
  
  COUT("Number of channels = " << input.index);
  
  channels_ = input.index;
  
#else
  fgioctl( VIDIOCGCAP, &vc_, "Unable to query for video capabilities: " );
#endif
}

// -----------------------------------------------------------------------------
// FrameGrabber definitions.
// -----------------------------------------------------------------------------

FrameGrabber::FrameGrabber( const string & fgdev, 
                            const int fginput, 
                            const bool emulate,
			    carma::services::Table* table) :
  pimpl_(new FrameGrabber::FrameGrabberPimpl(fgdev, fginput, emulate, table))
{
  table_ = table;
  resolution_  = getResolution(HI_RES);
  emulate_     = emulate;
  imageBuffer_ = 0;
  bufLen_      = 0;
  bufOffset_   = 0;

  initializeCaptureBuffer();

  CARMA_CPTRACE( Trace::TRACE6, 
                 "FrameGrabber( fgdev=" << fgdev << ", fginput=" << fginput << 
                 ", emulate=" << emulate_ << " )" );

  setResolution( HI_RES );
}

/**.......................................................................
 * EML: Test constructor to debug Table memory stomp -- NB this is
 * still unresolved (noticed while writing V4L2 interface, but I
 * haven't had time to get to the bottom of it), but doesn't seem to
 * cause any problems
 */
FrameGrabber::FrameGrabber(carma::services::Table* table)
{
  if(table) {
    COUT("FG -1: ncol = " << table->getNcols());
  }
}

/**.......................................................................
 * Destructor just unmaps any memory-mapped buffers
 */
FrameGrabber::~FrameGrabber()
{
  // Todo: Close framegrabber and unmmap 

  //------------------------------------------------------------
  // EML: I hate comments like the above.  Just DO IT.
  //------------------------------------------------------------

  unmapCaptureBuffer();

  //------------------------------------------------------------
  // Additionally, we assume that the auto_ptr will delete pimpl_ when
  // the last reference to it is gone.  Hence no explicit delete here
  //------------------------------------------------------------
}

void FrameGrabber::setPixelDepth( const short depth )
{
  ScopedPthreadMutexLock lock(mutex_);
  pimpl_->setDepth( depth );
}

void FrameGrabber::setBrightness( const short b )
{
  ScopedPthreadMutexLock lock(mutex_);
  pimpl_->setBrightness(b);
}

void FrameGrabber::setContrast( const short c )
{
  ScopedPthreadMutexLock lock(mutex_);
  pimpl_->setContrast(c);
}

/**.......................................................................
 * Under the V4L2 interface, changing the resolution has implications
 * for the memory-mapped buffers in the driver.  Therefore allocating
 * the buffers and changing the resolution are now coupled.  We first
 * unmap any buffers that may be mapped, then change the resolution,
 * then remap the buffers
 */
void FrameGrabber::setResolution ( const ResolutionType resolution )
{
  ScopedPthreadMutexLock lock(mutex_);
  
  CARMA_TEST ( resolution < RES_COUNT && resolution >= 0,
	       ErrorException, "Invalid resolution.");
  
  resolution_ = getResolution( resolution );

  unmapCaptureBuffer();

  pimpl_->setResolution( resolution_ );

  createCaptureBuffer();
}

void FrameGrabber::initializeCaptureBuffer()
{
#ifdef V4L2
  unsigned nBuf = 4;

  captureBuffers_.resize(nBuf);
  for(unsigned i=0; i < nBuf; i++)
    captureBuffers_[i] = 0;
#endif
}

/**.......................................................................
 * Create the buffer into which we will capture images
 */
void FrameGrabber::createCaptureBuffer()
{
  if(emulate_) 
    return;
  
#ifdef V4L2
  pimpl_->createMmapBuffers(captureBuffers_, bufLen_);
#else
  imageBuffer_ = pimpl_->createMmapBuffer(bufLen_);
#endif
  
  CARMA_TEST( imageBuffer_ >= 0 , ErrorException,
	      "Unable to create image buffer: " << strerror( errno ) );
}

/**.......................................................................
 * Unmap any memory-mapped capture buffers
 */
void FrameGrabber::unmapCaptureBuffer()
{
#ifdef V4L2
  for(unsigned i=0; i < captureBuffers_.size(); i++) {
    if(captureBuffers_[i]) {
      munmap(captureBuffers_[i], bufLen_);
      captureBuffers_[i] = 0;
    }
  }
#else
  if(imageBuffer_) {
    munmap(imageBuffer_, bufLen_);
    imageBuffer_ = 0;
  }
#endif
}

/**.......................................................................
 * Grab a frame
 */
Image FrameGrabber::grabFrame(const int numFrames, std::vector<float>* data)
{
  // Lock everybody out while we're grabbing ALL frames

  ScopedPthreadMutexLock lock(mutex_);
  
  if(numFrames <= 0) {
    ostringstream errmsg;
    errmsg << "FrameGrabber::grabFrame( ) - called with numFrames<=0!?!";
    throw CARMA_EXCEPTION( ErrorException, errmsg );
  }
  
  Image frame( resolution_.first, resolution_.second, 1, 1, 0 );

#if 1
  void* vptr=0;
#endif
  
  if(emulate_) {
    
    if(testImage_.width() != resolution_.first || 
       testImage_.height() != resolution_.second) {
      
      testImage_ = testImage_.get_resize( resolution_.first, 
					  resolution_.second );
    }
    
    for ( int i = 0; i < numFrames; ++i ) {
      frame += testImage_;
    }
    
  } else {
    
#ifdef V4L2
    // Collect multiple frames using 'double' buffering.
    // Note nomenclature - capturing a frame refers to telling the hardware
    // to begin a capture.  Syncing refers to pulling that data and 
    // processing it.  Thus a frame is not grabbed until it is first 
    // captured and then synced.

    unsigned index;
    unsigned framesSynced = 0;

    while(framesSynced < static_cast<const unsigned>(numFrames)) {
      
      pimpl_->startCapture();
      index = pimpl_->syncFrame();
      pimpl_->stopCapture();
      
      vptr = captureBuffers_[index];

      frame += RawImage(static_cast<RawImage::value_type *>( captureBuffers_[index] ), 
                        resolution_.first, resolution_.second, 1, 1 );
      ++framesSynced;
    }
#else
    // Collect multiple frames using 'double' buffering.
    // Note nomenclature - capturing a frame refers to telling the hardware
    // to begin a capture.  Syncing refers to pulling that data and 
    // processing it.  Thus a frame is not grabbed until it is first 
    // captured and then synced.
    int framesSynced = 0, framesCaptured = 0;
    const int bufsize = pimpl_->getMaxMemoryBufferFrames();

    for ( int i = 0; i < bufsize - 1 && framesCaptured < numFrames; ++i ) {
      pimpl_->captureFrame( resolution_, i );
      ++framesCaptured;
    }
    
    while ( framesSynced < numFrames ) {
      
      // Instruct hardware to capture next frame....
      if ( framesCaptured < numFrames ) { 
	pimpl_->captureFrame( resolution_, framesCaptured % bufsize );
	++framesCaptured;
      }
      
      // In the meantime we'll process *this* frame

      pimpl_->syncFrame( framesSynced % bufsize );

      ++framesSynced;

      frame += RawImage(static_cast<RawImage::value_type *>( imageBuffer_ ), 
                        resolution_.first, resolution_.second, 1, 1 );
    }
#endif
  }

#if 1
  if(data) {
    
    data->resize(resolution_.first * resolution_.second);
    
    unsigned char* cptr = (unsigned char*) vptr;
    
    for(unsigned i=0; i < data->size(); i++) {
      (*data)[i] = (float) ((int)(cptr[i]));
    }
  }
#endif

  return frame / numFrames; 
}

void FrameGrabber::setTestImage( const Image & image ) 
{
  testImage_ = image;
}

/**.......................................................................
 * Return the current resolution
 */
pair<short, short> FrameGrabber::getResolution( ) const
{
  return resolution_;
} 

/**.......................................................................
 * Return the resolution corresponding to the requested enum
 */
pair<short, short> FrameGrabber::getResolution( const FrameGrabber::ResolutionType res ) 
{
  switch ( res ) {
  case FrameGrabber::HI_RES:  return make_pair( 768, 480 );
  case FrameGrabber::MID_RES: return make_pair( 640, 400 );
  case FrameGrabber::LO_RES:  return make_pair( 320, 200 );
  default: return make_pair( 0, 0 );
  }
}
