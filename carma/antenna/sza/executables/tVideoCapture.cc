/*
 *  V4L2 video capture example
 *
 *  This program can be used and distributed without restrictions.
 *
 *      This program is provided with the V4L2 API
 * see http://linuxtv.org/docs.php for more information
 */
#include <iostream>
#include <iomanip>

#include <cmath>
#include <vector>

#include "carma/szautil/Program.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/Declination.h"
#include "carma/szautil/Debug.h"

#include "carma/szapgutil/PgUtil.h"

using namespace std;
using namespace sza::util;

PROGRAM_KEYWORDS = {
  { "rad",     "0",   "d", USAGE "Radians"},
  { "c",       "0.5", "d", USAGE "Contrast"},
  { "b",       "0.5", "d", USAGE "Brightness"},
  { "usevals", "f",   "b", USAGE "True to use values"},
  { "w",       "320", "i", USAGE "Width"},
  { "h",       "240", "i", USAGE "Height"},
  { "chan",    "1",   "i", USAGE "Channel"},
  { "usewh",   "f",   "b", USAGE "True to use values"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {};

#define DEF_BRIGHTNESS 32768
#define DEF_CONTRAST 27648

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <getopt.h>             /* getopt_long() */

#include <fcntl.h>              /* low-level i/o */
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <sys/ioctl.h>

#include <linux/videodev2.h>

#define CLEAR(x) memset(&(x), 0, sizeof(x))

enum io_method {
  IO_METHOD_READ,
  IO_METHOD_MMAP,
  IO_METHOD_USERPTR,
};

struct buffer {
  void   *start;
  size_t  length;
};

static char            *dev_name;
static enum io_method   io = IO_METHOD_MMAP;
static int              fd = -1;
struct buffer          *buffers;
static unsigned int     n_buffers;
static int              out_buf;
static int              force_format;
static int              frame_count = 1;
static int              chan_ = 1;
static unsigned         width_;
static unsigned         height_;
static double           brightness_;
static double           contrast_;
static bool             useVals_;
static bool             useWh_;

static void errno_exit(const char *s)
{
  fprintf(stderr, "%s error %d, %s\n", s, errno, strerror(errno));
  exit(EXIT_FAILURE);
}

static int xioctl(int fh, int request, void *arg)
{
  int r;
  
  do {
    r = ioctl(fh, request, arg);
  } while (-1 == r && EINTR == errno);
  
  return r;
}

static void process_image(const void *p, int size)
{
#if 1
  std::vector<float> data(width_* height_);
  std::vector<float> dataf(width_* height_);
  std::vector<float> dataAll(width_* height_);
  
  unsigned nBytePerPix = size / (width_*height_);
  
  COUT("Size = " << size << " wxh = " << width_*height_ << " nBytePerPix = " << nBytePerPix);
  
  if(size != width_ * height_) {
    COUT("Size is " << size << " expected " << (width_*height_));
  }
  
  if(nBytePerPix == 2) {
    COUT("Assigning shorts");
    unsigned short* cptr = (unsigned short*)p;
    
    for(unsigned i=0; i < data.size(); i++) {
      data[i] = (float) ((int)(cptr[i]));
      dataAll[i] = data[i];
    }
  } else {
    char* cptr = (char*)p;
    
    for(unsigned i=0; i < data.size(); i++) {
      data[i] = (float) ((int)(cptr[i]));
      dataAll[i] = data[i];
    }
  }
  
  for(unsigned iH=0; iH < height_; iH++) {
    for(unsigned iW=0; iW < width_; iW++) {
      unsigned ind     = width_*iH + iW;
      unsigned indflip = width_*(height_-iH) + iW;
      dataf[indflip] = data[ind];
    }
  }
  PgUtil::greyScale(dataf.size(), &dataf[0], width_, height_);
  
#endif
  if (out_buf)
    fwrite(p, size, 1, stdout);
  
  fflush(stderr);
  fprintf(stderr, ".");
  fflush(stdout);
}

static int read_frame(void)
{
  struct v4l2_buffer buf;
  unsigned int i;
  
  switch (io) {
  case IO_METHOD_READ:
    if (-1 == read(fd, buffers[0].start, buffers[0].length)) {
      switch (errno) {
      case EAGAIN:
	return 0;
	
      case EIO:
	/* Could ignore EIO, see spec. */
	
	/* fall through */
	
      default:
	errno_exit("read");
      }
    }
    
    process_image(buffers[0].start, buffers[0].length);
    break;
    
  case IO_METHOD_MMAP:
    CLEAR(buf);
    
    buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory = V4L2_MEMORY_MMAP;
    
    if (-1 == xioctl(fd, VIDIOC_DQBUF, &buf)) {
      switch (errno) {
      case EAGAIN:
	return 0;
	
      case EIO:
	/* Could ignore EIO, see spec. */
	
	/* fall through */
	
      default:
	errno_exit("VIDIOC_DQBUF");
      }
    }
    
    COUT("DQBUF returned with index = " << buf.index);

    assert(buf.index < n_buffers);
    
    process_image(buffers[buf.index].start, buf.bytesused);
    
    if (-1 == xioctl(fd, VIDIOC_QBUF, &buf))
      errno_exit("VIDIOC_QBUF");
    break;
    
  case IO_METHOD_USERPTR:
    CLEAR(buf);
    
    buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory = V4L2_MEMORY_USERPTR;
    
    if (-1 == xioctl(fd, VIDIOC_DQBUF, &buf)) {
      switch (errno) {
      case EAGAIN:
	return 0;
	
      case EIO:
	/* Could ignore EIO, see spec. */
	
	/* fall through */
	
      default:
	errno_exit("VIDIOC_DQBUF");
      }
    }
    
    for (i = 0; i < n_buffers; ++i)
      if (buf.m.userptr == (unsigned long)buffers[i].start
	  && buf.length == buffers[i].length)
	break;
    
    assert(i < n_buffers);
    
    process_image((void *)buf.m.userptr, buf.bytesused);
    
    if (-1 == xioctl(fd, VIDIOC_QBUF, &buf))
      errno_exit("VIDIOC_QBUF");
    break;
  }
  
  return 1;
}

static void mainloop(void)
{
  unsigned int count;
  
  count = frame_count;
  
  while (count-- > 0) {
    for (;;) {
      fd_set fds;
      struct timeval tv;
      int r;
      
      FD_ZERO(&fds);
      FD_SET(fd, &fds);
      
      /* Timeout. */
      tv.tv_sec = 2;
      tv.tv_usec = 0;
      
      r = select(fd + 1, &fds, NULL, NULL, &tv);
      
      if (-1 == r) {
	if (EINTR == errno)
	  continue;
	errno_exit("select");
      }
      
      if (0 == r) {
	fprintf(stderr, "select timeout\n");
	exit(EXIT_FAILURE);
      }
      
      if (read_frame())
	break;
      /* EAGAIN - continue select loop. */
    }
  }
}

static void stop_capturing(void)
{
  enum v4l2_buf_type type;
  
  switch (io) {
  case IO_METHOD_READ:
    /* Nothing to do. */
    break;
    
  case IO_METHOD_MMAP:
  case IO_METHOD_USERPTR:
    type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if (-1 == xioctl(fd, VIDIOC_STREAMOFF, &type))
      errno_exit("VIDIOC_STREAMOFF");

    COUT("STREAMOFF returned");
    break;
  }
}

static void start_capturing(void)
{
  unsigned int i;
  enum v4l2_buf_type type;
  
  switch (io) {
  case IO_METHOD_READ:
    /* Nothing to do. */
    break;
    
  case IO_METHOD_MMAP:
    for (i = 0; i < n_buffers; ++i) {
      struct v4l2_buffer buf;
      
      CLEAR(buf);
      buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      buf.memory = V4L2_MEMORY_MMAP;
      buf.index = i;
      
      COUT("Calling QBUF with index = " << i);

      if (-1 == xioctl(fd, VIDIOC_QBUF, &buf))
	errno_exit("VIDIOC_QBUF");
    }
    type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if (-1 == xioctl(fd, VIDIOC_STREAMON, &type))
      errno_exit("VIDIOC_STREAMON");
    break;
    
  case IO_METHOD_USERPTR:
    for (i = 0; i < n_buffers; ++i) {
      struct v4l2_buffer buf;
      
      CLEAR(buf);
      buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
      buf.memory = V4L2_MEMORY_USERPTR;
      buf.index = i;
      buf.m.userptr = (unsigned long)buffers[i].start;
      buf.length = buffers[i].length;
      
      if (-1 == xioctl(fd, VIDIOC_QBUF, &buf))
	errno_exit("VIDIOC_QBUF");
    }
    type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    if (-1 == xioctl(fd, VIDIOC_STREAMON, &type))
      errno_exit("VIDIOC_STREAMON");
    break;
  }
}

static void uninit_device(void)
{
  unsigned int i;
  
  switch (io) {
  case IO_METHOD_READ:
    free(buffers[0].start);
    break;
    
  case IO_METHOD_MMAP:
    for (i = 0; i < n_buffers; ++i)
      if (-1 == munmap(buffers[i].start, buffers[i].length))
	errno_exit("munmap");
    break;
    
  case IO_METHOD_USERPTR:
    for (i = 0; i < n_buffers; ++i)
      free(buffers[i].start);
    break;
  }
  
  free(buffers);
  
  //------------------------------------------------------------
  // Reset brightness and contrast to defaults
  //------------------------------------------------------------
  
  struct v4l2_control ctl;
  ctl.id = V4L2_CID_CONTRAST;
  ctl.value = DEF_CONTRAST;
  xioctl(fd, VIDIOC_S_CTRL, &ctl);
  
  ctl.id = V4L2_CID_BRIGHTNESS;
  ctl.value = DEF_BRIGHTNESS;
  xioctl(fd, VIDIOC_S_CTRL, &ctl);
  
}

static void init_read(unsigned int buffer_size)
{
  buffers = (struct buffer*)calloc(1, sizeof(*buffers));
  
  if (!buffers) {
    fprintf(stderr, "Out of memory\n");
    exit(EXIT_FAILURE);
  }
  
  buffers[0].length = buffer_size;
  buffers[0].start = malloc(buffer_size);
  
  if (!buffers[0].start) {
    fprintf(stderr, "Out of memory\n");
    exit(EXIT_FAILURE);
  }
}

static void init_mmap(void)
{
  struct v4l2_requestbuffers req;
  
  CLEAR(req);
  
  req.count = 4;
  req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  req.memory = V4L2_MEMORY_MMAP;
  
  if (-1 == xioctl(fd, VIDIOC_REQBUFS, &req)) {
    if (EINVAL == errno) {
      fprintf(stderr, "%s does not support "
	      "memory mapping\n", dev_name);
      exit(EXIT_FAILURE);
    } else {
      errno_exit("VIDIOC_REQBUFS");
    }
  }
  
  if (req.count < 2) {
    fprintf(stderr, "Insufficient buffer memory on %s\n",
	    dev_name);
    exit(EXIT_FAILURE);
  }
  
  buffers = (struct buffer*)calloc(req.count, sizeof(*buffers));
  
  if (!buffers) {
    fprintf(stderr, "Out of memory\n");
    exit(EXIT_FAILURE);
  }
  
  for (n_buffers = 0; n_buffers < req.count; ++n_buffers) {
    struct v4l2_buffer buf;
    
    CLEAR(buf);
    
    buf.type        = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory      = V4L2_MEMORY_MMAP;
    buf.index       = n_buffers;

    COUT("Calling QUERYBUF with index = " << n_buffers);

    if (-1 == xioctl(fd, VIDIOC_QUERYBUF, &buf))
      errno_exit("VIDIOC_QUERYBUF");
    
    buffers[n_buffers].length = buf.length;
    buffers[n_buffers].start =
      mmap(NULL /* start anywhere */,
	   buf.length,
	   PROT_READ | PROT_WRITE /* required */,
	   MAP_SHARED /* recommended */,
	   fd, buf.m.offset);
    
    if (MAP_FAILED == buffers[n_buffers].start)
      errno_exit("mmap");
  }
}

static void init_userp(unsigned int buffer_size)
{
  struct v4l2_requestbuffers req;
  
  CLEAR(req);
  
  req.count  = 4;
  req.type   = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  req.memory = V4L2_MEMORY_USERPTR;
  
  if (-1 == xioctl(fd, VIDIOC_REQBUFS, &req)) {
    if (EINVAL == errno) {
      fprintf(stderr, "%s does not support "
	      "user pointer i/o\n", dev_name);
      exit(EXIT_FAILURE);
    } else {
      errno_exit("VIDIOC_REQBUFS");
    }
  }
  
  buffers = (struct buffer*)calloc(4, sizeof(*buffers));
  
  if (!buffers) {
    fprintf(stderr, "Out of memory\n");
    exit(EXIT_FAILURE);
  }
  
  for (n_buffers = 0; n_buffers < 4; ++n_buffers) {
    buffers[n_buffers].length = buffer_size;
    buffers[n_buffers].start = malloc(buffer_size);
    
    if (!buffers[n_buffers].start) {
      fprintf(stderr, "Out of memory\n");
      exit(EXIT_FAILURE);
    }
  }
}

static void init_device(void)
{
  struct v4l2_capability cap;
  struct v4l2_cropcap cropcap;
  struct v4l2_crop crop;
  struct v4l2_format fmt;
  unsigned int min;
  
  if (-1 == xioctl(fd, VIDIOC_QUERYCAP, &cap)) {
    if (EINVAL == errno) {
      fprintf(stderr, "%s is no V4L2 device\n",
	      dev_name);
      exit(EXIT_FAILURE);
    } else {
      errno_exit("VIDIOC_QUERYCAP");
    }
  }
  
  if (!(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE)) {
    fprintf(stderr, "%s is no video capture device\n",
	    dev_name);
    exit(EXIT_FAILURE);
  }
  
  switch (io) {
  case IO_METHOD_READ:
    if (!(cap.capabilities & V4L2_CAP_READWRITE)) {
      fprintf(stderr, "%s does not support read i/o\n",
	      dev_name);
      exit(EXIT_FAILURE);
    }
    break;
    
  case IO_METHOD_MMAP:
  case IO_METHOD_USERPTR:
    if (!(cap.capabilities & V4L2_CAP_STREAMING)) {
      fprintf(stderr, "%s does not support streaming i/o\n",
	      dev_name);
      exit(EXIT_FAILURE);
    }
    break;
  }
  
  
  /* Select video input, video standard and tune here. */
  
  
  CLEAR(cropcap);
  
  cropcap.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  
  if (0 == xioctl(fd, VIDIOC_CROPCAP, &cropcap)) {
    crop.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    crop.c = cropcap.defrect; /* reset to default */
    
    if (-1 == xioctl(fd, VIDIOC_S_CROP, &crop)) {
      switch (errno) {
      case EINVAL:
	/* Cropping not supported. */
	break;
      default:
	/* Errors ignored. */
	break;
      }
    }
  } else {
    /* Errors ignored. */
  }
  
  
  CLEAR(fmt);
  
  fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  
  
  if (force_format) {
    fmt.fmt.pix.width       = 640;
    fmt.fmt.pix.height      = 480;
    fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
    fmt.fmt.pix.field       = V4L2_FIELD_INTERLACED;
    
    if (-1 == xioctl(fd, VIDIOC_S_FMT, &fmt))
      errno_exit("VIDIOC_S_FMT");
    
    /* Note VIDIOC_S_FMT may change width and height. */
  } else {
    /* Preserve original settings as set by v4l2-ctl for example */
    if (-1 == xioctl(fd, VIDIOC_G_FMT, &fmt))
      errno_exit("VIDIOC_G_FMT");
  }
  
  //------------------------------------------------------------
  // Force the cropping if requested
  //------------------------------------------------------------
  
  if(useWh_) {
    CLEAR(fmt);
    fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    
    if (-1 == xioctl(fd, VIDIOC_G_FMT, &fmt))
      errno_exit("VIDIOC_G_FMT");
    
    fmt.fmt.pix.width       = width_;
    fmt.fmt.pix.height      = height_;
    fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_GREY;
    fmt.fmt.pix.field       = V4L2_FIELD_INTERLACED;
    
    COUT("About to set field to " << width_ << " x " << height_);
    
    if (-1 == xioctl(fd, VIDIOC_S_FMT, &fmt))
      errno_exit("VIDIOC_S_FMT");
    
    if (-1 == xioctl(fd, VIDIOC_G_FMT, &fmt))
      errno_exit("VIDIOC_G_FMT");
  }
  
  //------------------------------------------------------------
  // Continue
  //------------------------------------------------------------
  
  /* Buggy driver paranoia. */
  min = fmt.fmt.pix.width * 2;
  if (fmt.fmt.pix.bytesperline < min)
    fmt.fmt.pix.bytesperline = min;
  min = fmt.fmt.pix.bytesperline * fmt.fmt.pix.height;
  
  COUT("Bytes perline = " << fmt.fmt.pix.bytesperline << " width      = " << fmt.fmt.pix.width << " height = " << fmt.fmt.pix.height);
  COUT("Pixel format  = " << fmt.fmt.pix.pixelformat  << " YUYV       = " << V4L2_PIX_FMT_GREY);
  COUT("Field         = " << fmt.fmt.pix.field        << " interlaced = " << V4L2_FIELD_INTERLACED);
  
  width_  = fmt.fmt.pix.width;
  height_ = fmt.fmt.pix.height;
  
  if (fmt.fmt.pix.sizeimage < min)
    fmt.fmt.pix.sizeimage = min;
  
  //------------------------------------------------------------
  // Detect what standard is currently being used
  //------------------------------------------------------------
  
  v4l2_std_id std;
  if (-1 == xioctl(fd, VIDIOC_G_STD, &std))
    errno_exit("VIDIOC_G_STD");
  
  COUT("Driver returned std = " << std);
  COUT("NTSC  = " << V4L2_STD_NTSC);
  COUT("PAL   = " << V4L2_STD_PAL);
  COUT("SECAM = " << V4L2_STD_SECAM);
  
  // Attempt to set to NTSC
  
  COUT("Attemping to set to NTSC");
  
  std = V4L2_STD_NTSC;
  if (-1 == xioctl(fd, VIDIOC_S_STD, &std))
    errno_exit("VIDIOC_S_STD");
  
  if (-1 == xioctl(fd, VIDIOC_G_STD, &std))
    errno_exit("VIDIOC_G_STD");
  
  COUT("Driver returned std = " << std);
  COUT("NTSC  = " << V4L2_STD_NTSC);
  COUT("PAL   = " << V4L2_STD_PAL);
  COUT("SECAM = " << V4L2_STD_SECAM);
  
  //------------------------------------------------------------
  // Set to greyscale
  //------------------------------------------------------------
  
  struct v4l2_format fmt2;
  fmt2.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  xioctl(fd, VIDIOC_G_FMT, &fmt2);
  
  fmt2.fmt.pix.pixelformat = V4L2_PIX_FMT_GREY;
  
  xioctl(fd, VIDIOC_S_FMT, &fmt2);
  
  //------------------------------------------------------------
  // Query brightness control
  //------------------------------------------------------------
  
  struct v4l2_queryctrl v4l2Qc;
  v4l2Qc.id = V4L2_CID_BRIGHTNESS;
  xioctl( fd, VIDIOC_QUERYCTRL, &v4l2Qc);
  COUT("Got a response: type = " << v4l2Qc.type << " name = " << v4l2Qc.name <<
       " min = " << v4l2Qc.minimum << " max = " << v4l2Qc.maximum);
  
  unsigned bMax = v4l2Qc.maximum;
  
  struct v4l2_control ctl;
  ctl.id = V4L2_CID_BRIGHTNESS;
  xioctl(fd, VIDIOC_G_CTRL, &ctl);
  COUT("Got a response: current brightness = " << ctl.value);
  
  v4l2Qc.id = V4L2_CID_CONTRAST;
  xioctl(fd, VIDIOC_QUERYCTRL, &v4l2Qc);
  COUT("Got a response: type = " << v4l2Qc.type << " name = " << v4l2Qc.name <<
       " min = " << v4l2Qc.minimum << " max = " << v4l2Qc.maximum);
  
  unsigned cMax = v4l2Qc.maximum;
  
  ctl.id = V4L2_CID_CONTRAST;
  xioctl(fd, VIDIOC_G_CTRL, &ctl);
  COUT("Got a response: current contrast = " << ctl.value);
  
  if(useVals_) {
    
    unsigned val = (unsigned)(contrast_ * (double)cMax);
    COUT("Setting contrast to: c = " << contrast_ << " cMax = " << cMax << " v = " << val);
    ctl.id = V4L2_CID_CONTRAST;
    ctl.value = val;
    xioctl(fd, VIDIOC_S_CTRL, &ctl);
    
    val = (unsigned)(brightness_ * (double)bMax);
    COUT("Setting brightness to: b = " << brightness_ << " bMax = " << bMax << " v = " << val);
    ctl.value = val;
    ctl.id = V4L2_CID_BRIGHTNESS;
    xioctl(fd, VIDIOC_S_CTRL, &ctl);
    
  }

  // Under V4L2, we just set the current video input to the requested input

  struct v4l2_input input;
  input.index = 0;
  
  do {
    errno = 0;
    xioctl(fd, VIDIOC_ENUMINPUT, &input);
    ++input.index;
  } while(errno != EINVAL);

  errno = 0;
  
  // Set the number of video input channels
  
  COUT("Number of channels = " << input.index);

  COUT("Setting channel to " << chan_);
  xioctl(fd, VIDIOC_S_INPUT, (void*)&chan_);
  
  switch (io) {
  case IO_METHOD_READ:
    init_read(fmt.fmt.pix.sizeimage);
    break;
    
  case IO_METHOD_MMAP:
    init_mmap();
    break;
    
  case IO_METHOD_USERPTR:
    init_userp(fmt.fmt.pix.sizeimage);
    break;
  }
}

static void close_device(void)
{
  if (-1 == close(fd))
    errno_exit("close");
  
  fd = -1;
}

static void open_device(void)
{
  struct stat st;
  
  if (-1 == stat(dev_name, &st)) {
    fprintf(stderr, "Cannot identify '%s': %d, %s\n",
	    dev_name, errno, strerror(errno));
    exit(EXIT_FAILURE);
  }
  
  if (!S_ISCHR(st.st_mode)) {
    fprintf(stderr, "%s is no device\n", dev_name);
    exit(EXIT_FAILURE);
  }
  
  fd = open(dev_name, O_RDWR /* required */ | O_NONBLOCK, 0);
  
  if (-1 == fd) {
    fprintf(stderr, "Cannot open '%s': %d, %s\n",
	    dev_name, errno, strerror(errno));
    exit(EXIT_FAILURE);
  }
}

static void usage(FILE *fp, int argc, char **argv)
{
  fprintf(fp,
	  "Usage: %s [options]\n\n"
	  "Version 1.3\n"
	  "Options:\n"
	  "-d | --device name   Video device name [%s]\n"
	  "-h | --help          Print this message\n"
	  "-m | --mmap          Use memory mapped buffers [default]\n"
	  "-r | --read          Use read() calls\n"
	  "-u | --userp         Use application allocated buffers\n"
	  "-o | --output        Outputs stream to stdout\n"
	  "-f | --format        Force format to 640x480 YUYV\n"
	  "-c | --count         Number of frames to grab [%i]\n"
	  "",
	  argv[0], dev_name, frame_count);
}

static const char short_options[] = "d:hmruofc:";

static const struct option
long_options[] = {
  { "device", required_argument, NULL, 'd' },
  { "help",   no_argument,       NULL, 'h' },
  { "mmap",   no_argument,       NULL, 'm' },
  { "read",   no_argument,       NULL, 'r' },
  { "userp",  no_argument,       NULL, 'u' },
  { "output", no_argument,       NULL, 'o' },
  { "format", no_argument,       NULL, 'f' },
  { "count",  required_argument, NULL, 'c' },
  { 0, 0, 0, 0 }
};

int Program::main()
{
  dev_name = "/dev/video0";
  
  brightness_ = Program::getDoubleParameter("b");
  contrast_   = Program::getDoubleParameter("c");
  useVals_    = Program::getBoolParameter("usevals");
  
  width_   = Program::getIntParameter("w");
  height_  = Program::getIntParameter("h");
  chan_    = Program::getIntParameter("chan");
  useWh_   = Program::getBoolParameter("usewh");
  
  COUT("C = " << contrast_ << " b = " << brightness_);
#if 0
  for (;;) {
    int idx;
    int c;
    
    c = getopt_long(argc, argv,
		    short_options, long_options, &idx);
    
    if (-1 == c)
      break;
    
    switch (c) {
    case 0: /* getopt_long() flag */
      break;
      
    case 'd':
      dev_name = optarg;
      break;
      
    case 'h':
      usage(stdout, argc, argv);
      exit(EXIT_SUCCESS);
      
    case 'm':
      io = IO_METHOD_MMAP;
      break;
      
    case 'r':
      io = IO_METHOD_READ;
      break;
      
    case 'u':
      io = IO_METHOD_USERPTR;
      break;
      
    case 'o':
      out_buf++;
      break;
      
    case 'f':
      force_format++;
      break;
      
    case 'c':
      errno = 0;
      frame_count = strtol(optarg, NULL, 0);
      if (errno)
	errno_exit(optarg);
      break;
      
    default:
      usage(stderr, argc, argv);
      exit(EXIT_FAILURE);
    }
  }
#endif
  
  open_device();
  init_device();
  
#if 0
  //	stop_capturing();
  uninit_device();
#else
  COUT("Here 0");
  start_capturing();
  COUT("Here 1");
  mainloop();
  COUT("Here 2");
  stop_capturing();
  COUT("Here 3");
  uninit_device();
  COUT("Here 4");
#endif
  close_device();
  fprintf(stderr, "\n");
  return 0;
}
