#ifndef optical_camera_h
#define optical_camera_h

#include "carma/szaarrayutils/rtcnetcoms.h"
#include "carma/szautil/SzaPorts.h"

/*
 * Optical camera information that is shared between sun control program and
 * VxWorks tasks.
 */

/*
 * Enumerate control-program -> optcam-task message types.
 */
typedef enum {
  OPTCAM_GREETING   /* Sent by a client on connecting to the optcam task */
} CpToOptCam;

/*
 * Enumerate optcam-task -> control-program message types.
 */
typedef enum {
  OPTCAM_DATA_MSG   /* A message containing an image */
} OptCamToCp;

/*
 * Set the size of the optcam network read-buffer.
 */
enum {OPTCAM_MAX_CMD_SIZE=100};

/* The number of pixels in both axes of the frame grabber image */

//#define GRABBER_XNPIX 512
//#define GRABBER_YNPIX 480  /* NTSC standard -- last 32 rows of the frame grabber image are junk */

#define GRABBER_XNPIX 512
#define GRABBER_YNPIX 480

/*
 * The border, in pixels, to blank.
 */
#define OPTCAM_IMAGE_BORDER 2

/* The size of the image (in pixels) returned by the frame grabber */

#define GRABBER_IM_SIZE (GRABBER_XNPIX*GRABBER_YNPIX)

/* The length (in bytes) of the date string to be sent with an image */

#define OPTCAM_DATE_LEN 8

/* 
 * The length (in bytes) of the tracker position to be sent with an image 
 * This is two 3-element registers of unsigned longs = 2 * 3 * 4 = 24 bytes
 */

#define OPTCAM_TRK_POS_LEN 24

/*
 * The network buffer that is used to communicate images
 * to the control program has size (in bytes) (image is returned as shorts)
 *   NET_PREFIX_LEN + OPTCAM_DATE_LEN + GRABBER_IM_SIZE*2
 */
#define OPTCAM_BUFF_SIZE  (NET_PREFIX_LEN + OPTCAM_DATE_LEN + OPTCAM_TRK_POS_LEN + GRABBER_IM_SIZE*2)

#endif
