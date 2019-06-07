#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <math.h>

#include "lprintf.h"
#include "szacontrol.h"
#include "szaconst.h"
#include "szaregs.h"
#include "netbuf.h"
#include "arcfile.h"
#include "optcam.h"
#include "fitsio.h"
#include "grabber.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/FdSet.h"

using namespace sza::array;

// The optical camera field of view

static const sza::util::Angle defaultOptCamFov_ = 
sza::util::Angle(sza::util::Angle::ArcMinutes(), 12.0);

static sza::util::Angle optCamFov_ = defaultOptCamFov_;

// The optical camera aspect ratio (y/x)

static const double defaultOptCamAspect_  = 0.8;;

static double optCamAspect_ = defaultOptCamAspect_;

// The optical camera collimation angle

static const sza::util::Angle defaultOptCamCollimation_ = 
sza::util::Angle(sza::util::Angle::Degrees(), 0.0);

static sza::util::Angle optCamCollimation_ = defaultOptCamCollimation_;

/*
 * Image buffers will be maintained in objects of the following type.
 * Callers must acquire the guard mutex before attempting to access
 * the image buffer.
 */
typedef struct {
  pthread_mutex_t guard;       /* The mutual exlusion guard of the image */
  int guard_ok;                /* True after initializing 'guard' */
  pthread_cond_t start;        /* When the grabberr has finished saving */
                               /*  the latest image, it will set */
                               /*  the 'save' flag to 1 and signal */
                               /*  the 'start' condition variable */
  int start_ok;                /* True after initializing 'start' */
  int save;
  double dx;                    /* The x-increment of a pixel */
  double dy;                    /* The y-increment of a pixel */
  double xa;                    /* The x-coordinate of the blc of the image */
  double ya;                    /* The y-coordinate of the blc of the image */
  double xpeak;                 /* The coordinate of the peak of the image */
  double ypeak;                 /* The y-coordinate of the peak of the image */
  double max;                   /* The max pixel value */
  double snr;                   /* signal to noise of the peak pixel */
  unsigned short image[GRABBER_IM_SIZE];
  unsigned long utc[2];
  unsigned seq;
} ImageBuffer;

static int grabber_image_error(ImageBuffer *im);

/*
 * An object of the following type is used to hold the state of
 * the grabber thread.
 */
struct Grabber {
  ControlProg *cp;             /* The state-object of the control program */

#ifndef NEW_PIPE
  Pipe *pipe;                  /* An externally allocated control-input pipe */
#else
  sza::util::Pipe *pipe;                  /* An externally allocated control-input pipe */
#endif

  int archive;                 /* True if we are archiving images. */
  char *dir;                   /* The directory in which to place new images */
  char *path;                  /* The pathname of the current archive file */
  FILE *fp;                    /* The file-pointer attached to 'path' */
  ImageBuffer im;              /* The image buffer */
  NetBuf *net;                 /* The network buffer of the output image */
};
static int chdir_grabber(Grabber *grabber, char *dir);
static int open_grabfile(Grabber *grabber, char *dir, ArcTimeStamp *time);
static void flush_grabfile(Grabber *grabber);
static void close_grabfile(Grabber *grabber);
static int grabber_write_image(Grabber *grabber);
static Grabber *cp_Grabber(ControlProg *cp);
/*.......................................................................
 * Create the state object of a grabber thread.
 *
 * Input:
 *  cp     ControlProg *   The state object of the control program.
 *  pipe          Pipe *   The pipe on which to listen for control messages.
 * Output:
 *  return        void *   The new Grabber object, or NULL on error.
 */
CP_NEW_FN(new_Grabber)
{
  Grabber *grabber;     /* The object to be returned */
  int status;        /* The status return value of a pthread function */
  double xa,xb,ya,yb;
  
  // Allocate the container.
  
  grabber = (Grabber* )malloc(sizeof(Grabber));
  
  if(!grabber) {
    lprintf(stderr, "new_Grabber: Insufficient memory.\n");
    return NULL;
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can be safely
  // passed to del_Grabber().
  
  grabber->cp          = cp;
  grabber->pipe        = pipe;
  grabber->dir         = NULL;
  grabber->path        = NULL;
  grabber->fp          = NULL;
  grabber->archive     = 0;
  grabber->im.guard_ok = 0;
  grabber->im.start_ok = 0;
  grabber->im.save     = 1;
  
  // Initialize parameters.  All positions will be computed in pixels.
  // In these unitsm the first pixel of the image is centered at -N/2 + 0.5.
  
  xa = -(double)(GRABBER_XNPIX)/2 + 0.5;
  ya = -(double)(GRABBER_YNPIX)/2 + 0.5;
  xb =  (double)(GRABBER_XNPIX)/2 - 0.5;
  yb =  (double)(GRABBER_YNPIX)/2 - 0.5;
  
  // Store the start pixel values, and the image deltas, in degrees

  grabber->im.xa = xa;
  grabber->im.ya = ya;
  grabber->im.dx = 1.0;
  grabber->im.dy = 1.0;

  grabber->net = NULL;
  
  // Create the mutex that protects access to the image buffer buffer.
  
  status = pthread_mutex_init(&grabber->im.guard, NULL);
  if(status) {
    lprintf(stderr, "pthread_mutex_init: %s\n", strerror(status));
    return del_Grabber(grabber);
  };
  grabber->im.guard_ok = 1;
  
  // Create the condition variable that the grabber uses to signal
  // that it has finished copying the latest image from its image
  // buffer into its network buffer.
  
  status = pthread_cond_init(&grabber->im.start, NULL);
  if(status) {
    lprintf(stderr, "pthread_cond_init: %s\n", strerror(status));
    return del_Grabber(grabber);
  };
  grabber->im.start_ok = 1;
  
  // Allocate a buffer in which to compose image records.

  grabber->net = new_NetBuf(NET_PREFIX_LEN + FITS_HEADER_SIZE + 
			    GRABBER_IM_SIZE*2);
  if(!grabber->net)
    return del_Grabber(grabber);
  
  return grabber;
}

/*.......................................................................
 * Delete the state-object of an grabber thread.
 *
 * Input:
 *  obj         void *  The Grabber object to be deleted.
 * Output:
 *  return      void *  The deleted Grabber object (always NULL).
 */
CP_DEL_FN(del_Grabber)
{
  Grabber *grabber = (Grabber* )obj;
  if(grabber) {
    if(grabber->dir) {
      free(grabber->dir);
      grabber->dir = NULL;
    };
    close_grabfile(grabber);
    if(grabber->im.guard_ok)
      pthread_mutex_destroy(&grabber->im.guard);
    if(grabber->im.start_ok)
      pthread_cond_destroy(&grabber->im.start);
    grabber->net = del_NetBuf(grabber->net);
    free(grabber);
  };
  return NULL;
}

/*.......................................................................
 * Return the grabber resource object.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 * Output:
 *  return      Grabber *   The grabber resource object.
 */
static Grabber *cp_Grabber(ControlProg *cp)
{
  return (Grabber* )cp_ThreadData(cp, CP_GRABBER);
}

/*.......................................................................
 * Attempt to send a message to a grabber thread.
 *
 * Input:
 *  cp      ControlProg *  The state-object of the control program.
 *  msg GrabberMessage *  The message to be sent. This must have been
 *                         filled by one of the pack_grabber_<type>()
 *                         functions.
 *  timeout        long    The max number of milliseconds to wait for the
 *                         message to be sent, or -1 to wait indefinitely.
 * Output:
 *  return    PipeState    The status of the transaction:
 *                           PIPE_OK    - The message was read successfully.
 *                           PIPE_BUSY  - The send couldn't be accomplished
 *                                        without blocking (only returned
 *                                        when timeout=PIPE_NOWAIT).
 *                           PIPE_ERROR - An error occurred.
 */
PipeState send_GrabberMessage(ControlProg *cp, GrabberMessage *msg,
			      long timeout)
{
#ifndef NEW_PIPE
  return write_pipe(cp_Grabber(cp)->pipe, msg, sizeof(*msg), timeout);
#else
  return cp_Grabber(cp)->pipe->write(msg, sizeof(*msg), timeout);
#endif
}

/*.......................................................................
 * Send a shutdown message to the grabber thread using non-blocking I/O.
 * Return 0 if the message was sent, 0 otherwise.
 *
 * Input:
 *  cp      ControlProg *  The control-program resource object.
 * Output:
 *  return          int    0 - Message sent ok.
 *                         1 - Unable to send message.
 */
CP_STOP_FN(stop_Grabber)
{
  GrabberMessage msg;   /* The message to be sent */
  return pack_grabber_shutdown(&msg) ||
    send_GrabberMessage(cp, &msg, PIPE_NOWAIT) != PIPE_OK;
}

/*.......................................................................
 * Prepare a shutdown message for subsequent transmission to the
 * grabber thread.
 *
 * Input:
 *  msg  GrabberMessage *  The message object to be prepared.
 * Output:
 * return       int     0 - OK.
 *                      1 - Error.
 */
int pack_grabber_shutdown(GrabberMessage *msg)
{
  /*
   * Check arguments.
   */
  if(!msg) {
    lprintf(stderr, "pack_grabber_shutdown: NULL argument.\n");
    return 1;
  };
  msg->type = GRAB_SHUTDOWN;
  return 0;
}

/*.......................................................................
 * Prepare a change-default-directory message for subsequent transmission
 * to the grabber thread.
 *
 * Input:
 *  msg   GrabberMessage *  The message object to be packed for subsequent
 *                         transmission.
 *  dir            char *  The directory in which to open subsequent log
 *                         files.
 * Output:
 * return           int    0 - OK.
 *                         1 - Error.
 */
int pack_grabber_chdir(GrabberMessage *msg, char *dir)
{
  /*
   * Check arguments.
   */
  if(!msg || !dir) {
    lprintf(stderr, "pack_grabber_chdir: NULL argument(s).\n");
    return 1;
  };
  msg->type = GRAB_CHDIR;
  strncpy(msg->body.chdir.dir, dir, CP_FILENAME_MAX);
  msg->body.chdir.dir[CP_FILENAME_MAX] = '\0';
  return 0;
}

/*.......................................................................
 * Prepare an open-data-file message for subsequent transmission to the
 * grabber thread.
 *
 * Input:
 *  msg  GrabberMessage *  The message object to be prepared.
 *  dir             char *   The directory in which to open the file.
 * Output:
 * return            int     0 - OK.
 *                           1 - Error.
 */
int pack_grabber_open(GrabberMessage *msg, char *dir)
{
  /*
   * Check arguments.
   */
  if(!msg || !dir) {
    lprintf(stderr, "pack_grabber_open: NULL argument(s).\n");
    return 1;
  };
  msg->type = GRAB_OPEN;
  strncpy(msg->body.open.dir, dir, CP_FILENAME_MAX);
  msg->body.open.dir[CP_FILENAME_MAX] = '\0';
  return 0;
}

/*.......................................................................
 * Prepare a flush-data-file message for subsequent transmission to the
 * grabber thread.
 *
 * Input:
 *  msg  GrabberMessage *  The message object to be prepared.
 * Output:
 * return       int     0 - OK.
 *                      1 - Error.
 */
int pack_grabber_flush(GrabberMessage *msg)
{
  /*
   * Check arguments.
   */
  if(!msg) {
    lprintf(stderr, "pack_grabber_flush: NULL argument.\n");
    return 1;
  };
  msg->type = GRAB_FLUSH;
  return 0;
}

/*.......................................................................
 * Prepare a close-data-file message for subsequent transmission to the
 * grabber thread.
 *
 * Input:
 *  msg  GrabberMessage *  The message object to be prepared.
 * Output:
 * return       int     0 - OK.
 *                      1 - Error.
 */
int pack_grabber_close(GrabberMessage *msg)
{
  /*
   * Check arguments.
   */
  if(!msg) {
    lprintf(stderr, "pack_grabber_close: NULL argument.\n");
    return 1;
  };
  msg->type = GRAB_CLOSE;
  return 0;
}

/*.......................................................................
 * Prepare a save-grabber-image message for subsequent transmission to
 * the grabber thread.
 *
 * Input:
 *  msg  GrabberMessage *  The message object to be prepared.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
int pack_grabber_image(GrabberMessage *msg)
{
  /*
   * Check arguments.
   */
  if(!msg) {
    lprintf(stderr, "pack_grabber_image: NULL argument.\n");
    return 1;
  };
  /*
   * Compose the message.
   */
  msg->type = GRAB_IMAGE;
  return 0;
}
/*.......................................................................
 * This is the error return function of grabber_integrate_image(). It
 * releases exclusive access to the image buffer and returns the error
 * code of grabber_integrate_image().
 *
 * Input:
 *  im     ImageBuffer *  The locked image buffer.
 * Output:
 *  return         int    1.
 */
static int grabber_image_error(ImageBuffer *im)
{
  pthread_mutex_unlock(&im->guard);
  return 1;
}

/*.......................................................................
 * This is the entry-point of the grabber thread.
 *
 * Input:
 *  arg          void *  A pointer to the Grabber state object pointer,
 *                       cast to (void *).
 * Output:
 *  return       void *  NULL.
 */
CP_THREAD_FN(grabber_thread)
{
  Grabber *grabber = (Grabber* )arg; /* The state-object of the
					current thread */
  GrabberMessage msg; /* An message reception container */
  /*
   * Enable logging of the scheduler's stdout and stderr streams.
   */
  if(log_thread_stream(grabber->cp, stdout) ||
     log_thread_stream(grabber->cp, stderr)) {
    cp_report_exit(grabber->cp);
    return NULL;
  };
  /*
   * Wait for commands from other threads.
   */
#ifndef NEW_PIPE
  while(read_pipe(grabber->pipe, &msg, sizeof(msg), PIPE_WAIT) == PIPE_OK) {
#else

    sza::util::FdSet fdSet;
    fdSet.registerReadFd(grabber->pipe->readFd());

    while(select(fdSet.size(), fdSet.readFdSet(), NULL, NULL, NULL) > 0) {

      if(grabber->pipe->read(&msg, sizeof(msg), PIPE_WAIT) != PIPE_OK)
	break;

#endif
    /*
     * Interpret the message.
     */
    switch(msg.type) {
    case GRAB_IMAGE:
      (void) grabber_write_image(grabber);
      break;
    case GRAB_SHUTDOWN:
      close_grabfile(grabber);
      cp_report_exit(grabber->cp);
      return NULL;
      break;
    case GRAB_CHDIR:
      (void) chdir_grabber(grabber, msg.body.chdir.dir);
      break;
    case GRAB_OPEN: /* Tell the grabber thread to save subsequent images to
		       disk.  If a second*/
      grabber->archive = 1;
      (void) chdir_grabber(grabber, msg.body.open.dir); /* Pass on to chdir in
							   case a new directory
							   was specified */
      break;
    case GRAB_FLUSH:
      flush_grabfile(grabber);
      break;
    case GRAB_CLOSE: /* Tell the grabber thread not to save subsequent images to
			disk. */
      grabber->archive = 0;
      break;
    case GRAB_SEQ:   /* Update the grabber transaction sequence number */
      grabber->im.seq = msg.body.seq;
      break;
    default:
      lprintf(stderr, "grabber_thread: Unknown command-type received.\n");
      break;
    };
  };
  fprintf(stderr, "Grabber thread exiting after pipe read error.\n");
  cp_report_exit(grabber->cp);
  return NULL;
}

/*.......................................................................
 * Flush unwritten data to the current grabber data-file.
 *
 * Input:
 *  grabber     Grabber *   The state-object of the grabber thread.
 */
static void flush_grabfile(Grabber *grabber)
{
  if(grabber->fp && fflush(grabber->fp)) {
    lprintf(stderr, "Error flushing grabber file: %s\n", grabber->path);
    close_grabfile(grabber);
  };
}

/*.......................................................................
 * Close the current grabber data-file. Until a new data file is opened,
 * subsequently integrated images will be discarded.
 *
 * Input:
 *  grabber     Grabber *   The state-object of the grabber thread.
 */
static void close_grabfile(Grabber *grabber)
{
  if(grabber->fp) {
    if(fclose(grabber->fp))
      lprintf(stderr, "Error closing grabber file: %s\n", grabber->path);
    else
      lprintf(stdout, "Closing grabber file: %s\n", grabber->path);
  };
  grabber->fp = NULL;
  if(grabber->path)
    free(grabber->path);
  grabber->path = NULL;
}
/*.......................................................................
 * Open a new grabber file in a given directory. If the file is opened
 * successfully, close the previous grabber file.
 *
 * Input:
 *  grabber   Grabber *   The state-object of the current thread.
 *  dir       char *   The name of the directory in which to create the
 *                     file or NULL or "" to use the last directory that
 *                     was specified.
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */
static int open_grabfile(Grabber *grabber, char *dir, ArcTimeStamp *time)
{
  char *path;                  /* The path name of the file */
  FILE *fp;                    /* The file-pointer of the open file */
  /*
   * Record a new log file directory?
   */
  if(dir && *dir!='\0')
    (void) chdir_grabber(grabber, dir);
  else
    dir = grabber->dir;
  /*
   * Compose the full pathname of the grabber file.  Once we include passing an
   * accurate time stamp with the image, we should pass this as the second
   * argument, instead of NULL, so that the time when the image was taken will
   * be incorporated into the file name.
   */
  path = arc_path_name(dir, time, ARC_GRAB_FILE);
  if(!path)
    return 1;
  /*
   * Attempt to open the new data file.
   */
  fp = fopen(path, "wb");
  if(!fp) {
    lprintf(stderr, "Unable to open grabber file: %s\n", path);
    free(path);
    return 1;
  };
  /*
   * Close the current grabber file if open.
   */
  close_grabfile(grabber);
  /*
   * Install the new grabber file.
   */
  grabber->path = path;
  grabber->fp = fp;
  /*
   * Report the successful opening of the file.
   */
  lprintf(stdout, "Opening grabber file: %s\n", path);
  
  return 0;
}

/*.......................................................................
 * Change the directory in which subsequent grabber files will be written.
 *
 * Input:
 *  dir      char *   The directory to use when creating subsequent
 *                    grabber files.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
static int chdir_grabber(Grabber *grabber, char *dir)
{
  /*
   * Make a copy of the name of the current grabber directory.
   */
  if(dir != grabber->dir && *dir != '\0') {
    size_t bytes = strlen(dir)+1;
    char *tmp = (char* )(grabber->dir ? realloc(grabber->dir, bytes) : 
			 malloc(bytes));
    if(!tmp) {
      lprintf(stderr, "Unable to record new grabber directory.\n");
      return 1;
    } else {
      strcpy(tmp, dir);
      grabber->dir = tmp;
    };
  };
  return 0;
}
/*.......................................................................
 * Write the latest image to disk.
 *
 * Input:
 *  grabber    Grabber *  The state object of the current thread.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
static int grabber_write_image(Grabber *grabber)
{
  int status;        /* The status return value of a pthread function */
  /*
   * Get convenient aliases of the image buffer and output buffer.
   */
  ImageBuffer *im = &grabber->im;
  NetBuf *net = grabber->net;
  ArcTimeStamp time;
  /*
   * Acquire exclusive access to the image buffer.
   */
  if((status=pthread_mutex_lock(&im->guard))) {
    lprintf(stderr, "grabber_write_image (mutex_lock): %s.\n",
	    strerror(status));
    return 1;
  };
  /*
   * Construct an output record of the image.
   * Note that the return status is ignored until exclusive access to
   * the buffer has been released further below.
   */
  net->nget = net->nput = 0;
  status |= net_put_fitshead(net, im->utc);
  status |= net_put_short(net, GRABBER_IM_SIZE, im->image);
  /*
   * Set the date when this image was taken so that we can open a file
   * with the appropriate name.
   */
  time.mjd = im->utc[0];
  time.sec = im->utc[1]/1000; /* Convert from milliseconds to seconds. */
  /*
   * Prepare the image buffer for the next image.
   */
  im->save = 1;
  /*
   * Signal other threads that the image-buffer is now ready for a
   * new image.
   */
  pthread_cond_signal(&im->start);
  /*
   * Relinquish exclusive access to the buffer.
   */
  pthread_mutex_unlock(&im->guard);
  /*
   * Having safely released exclusive access to the image buffer, abort if
   * there was an error.
   */
  if(status)
    return 1;
  /*
   * If archiving, attempt to open a new grabber file here.
   */
  if(grabber->archive)
    if(open_grabfile(grabber, grabber->dir, &time))
      return 1;
  /*
   * Do we have an grabber file to save to, and should we save this
   * image?
   */
  if(grabber->fp && grabber->archive) {
    if((long)fwrite(net->buf, sizeof(net->buf[0]), net->nput, grabber->fp) != net->nput) {
      lprintf(stderr, "Error writing grabber data file (%s).\n",
	      strerror(errno));
      close_grabfile(grabber);
      return 1;
    };
#ifdef DEBUG
    lprintf(stdout,"Just wrote to the grabfile.\n");
#endif
    /*
     * And close the grabfile, since we will open a new one on the next receipt
     * of an image.
     */
    if(grabber->fp)
      close_grabfile(grabber);
  };
  return 0;
}
/*.......................................................................
 * Copy the latest image from the control thread image buffer and tell
 * the grabber thread to save it to disk.
 *
 * Input:
 *  grabber    Grabber *  The state object of the current thread.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
int grabber_save_image(ControlProg *cp, unsigned short *image, unsigned long
		       utc[2], signed actual[3], signed expected[3])
{
  Grabber *grabber = cp_Grabber(cp);
  ImageBuffer *im = &grabber->im;
  int status;        /* The status return value of a pthread function */
  int ix,iy,indFrom, indTo,ind,ixmax=0,iymax=0;
  int first=1;
  int ixmid,iymid;
  unsigned short max=0;
  double fpix;
  double mean=0.0,sd=0.0,rms;

  /*
   * Gain exclusive access to the image buffer.
   */
  if((status=pthread_mutex_lock(&im->guard))) {
    lprintf(stderr, "arc_integrate_frame (mutex_lock): %s.\n",
	    strerror(status));
    return 1;
  };
  /*
   * If a save isn't currently in progress wait for the
   * grabber to release the buffer.
   */
  while(!im->save) {
    status = pthread_cond_wait(&im->start, &im->guard);
    if(status) {
      lprintf(stderr, "grabber_integrate_frame (cond_wait): %s.\n",
	      strerror(status));
      return grabber_image_error(im);
    };
  };
  /*
   * Now copy the control program image to the grabber image buffer.
   * Store the peak on the fly.
   */
  for(iy=0; iy < GRABBER_YNPIX; iy++) {
    for(ix=0; ix < GRABBER_XNPIX; ix++) {
       
      // Flip the image in y
      
      indFrom = iy * GRABBER_XNPIX + ix;
      indTo   = (GRABBER_YNPIX - iy - 1) * GRABBER_XNPIX + ix;
      
      grabber->im.image[indTo] = image[indFrom];
      
      fpix = (double)(image[indFrom]);
      
      if(first) {
	max = grabber->im.image[indTo];
	ixmax = ix;
	iymax = iy;
	
	mean = sd = 0.0;
	
	first = 0;
      }
      if(grabber->im.image[indTo] > max) {
	ixmax = ix;
	iymax = iy;
	max = grabber->im.image[indTo];
      }
      /*
       * Accumulate first and second moments. With the first loop in y,
       * indFrom runs from 0 to NPIX, so we can safely use it as the point
       * counter.
       */
      mean += (fpix        - mean) / (indFrom+1);
      sd   += (fpix * fpix - sd)  / (indFrom+1);
    }
  }
  
  // Now write a crosshair into the image.  We will do this by setting
  // a 1-pixel wide row and column to the maximum pixel value found.

  ixmid = GRABBER_XNPIX/2;
  iymid = GRABBER_YNPIX/2;
  
  // Write out the 1-pixel crosshair in x.

  for(ix=0;ix < GRABBER_XNPIX;ix++) { 
    ind = iymid*GRABBER_XNPIX + ix;
    grabber->im.image[ind] = max;
  }
  
  // Write out the 1-pixel crosshair in y.

  for(iy=0;iy < GRABBER_YNPIX;iy++) { 
    ind = iy*GRABBER_XNPIX + ixmid;
    grabber->im.image[ind] = max;
  }
  
  // Store the peak offsets, in fractional pixels

  grabber->im.xpeak = grabber->im.xa + ixmax * grabber->im.dx;
  grabber->im.ypeak = grabber->im.ya + iymax * grabber->im.dy;

  //  std::cout << "Peak is at x pixel: " << grabber->im.xpeak << std::endl;
  //  std::cout << "Peak is at y pixel: " << grabber->im.ypeak << std::endl;
  
  // To correct to the star position, we have to move the telescope in
  // the opposite direction of the offsets.

  grabber->im.xpeak *=  -1;  // The amount we have to move the
			    // telescope to get on peak in x
			    // (arcseconds)
  grabber->im.ypeak *=  -1;  // The amount we have to move the
			    // telescope to get on peak in y
			    // (arcseconds)
  
  // Store the max pixel value and the snr.

  grabber->im.max = (double)max;
  rms = sqrt(sd - mean*mean);
  grabber->im.snr = rms==0.0 ? 0.0 : grabber->im.max/rms;
  
  // And copy the utc.

  grabber->im.utc[0] = utc[0];
  grabber->im.utc[1] = utc[1];
  
  // And queue the latest image for archiving.

  {
    GrabberMessage msg;
    im->save = 1;
    if(pack_grabber_image(&msg) ||
       send_GrabberMessage(cp, &msg, PIPE_WAIT) != PIPE_OK)
      return grabber_image_error(im);
  };
  
  // And send the scheduler thread a command telling it that the new
  // image has been acquired.

  {
    SchedulerMessage msg;
    if(pack_scheduler_grab_done(&msg, grabber->im.seq) ||
       send_SchedulerMessage(cp, &msg, PIPE_WAIT) != PIPE_OK)
      return grabber_image_error(im);
  };
  
  // Relinquish exclusive access to the image buffer.

  pthread_mutex_unlock(&im->guard);
  
  return 0;
}
/*.......................................................................
 * Return the offset of the peak in x and y
 *
 * Input/Output:
 *
 *  xoff  double  *  The x-offset of the peak. (in mas)
 *  yoff  double  *  The y-offset of the peak. (in mas)
 *
 * Ouput:
 * 
 * 0 -- ok
 * 1 -- error.
 */
void grabber_offset_info(ControlProg *cp, double& xoff, double& yoff)
{
  Grabber *grabber = cp_Grabber(cp);

  // Get the peak, in pixel units

  double x = grabber->im.xpeak;
  double y = grabber->im.ypeak;

  // Rescale to the current FOV and aspect ratio

  x *= optCamFov_.degrees()/GRABBER_XNPIX;
  y *= optCamFov_.degrees()/GRABBER_YNPIX * optCamAspect_;

  // And correct for collimation

  double dColl = optCamCollimation_.radians();

  xoff =  cos(dColl)*x + sin(dColl)*y;
  yoff = -sin(dColl)*x + cos(dColl)*y;

  //  std::cout << "Peak is at: x = (arcsec): " 
  //	    << xoff * sza::util::Angle::arcSecPerDegree_ << std::endl;
  //  std::cout << "Peak is at: y = (arcsec): " 
  //	    << yoff * sza::util::Angle::arcSecPerDegree_ << std::endl;
}

/*.......................................................................
 * Return the requested statistic about the peak value of the frame grabber
 * image.
 *
 * Input/Output:
 *
 *  peak double  *  The value of the peak
 *  snr  double  *  The snr of the peak
 *
 * Output:
 * 
 * 0 -- ok
 * 1 -- error.
 */
int grabber_peak_info(ControlProg *cp, double *peak, double *snr)
{
  Grabber *grabber = cp_Grabber(cp);
  
  *peak = grabber->im.max;
  *snr = grabber->im.snr;
  
  return 0;
}

/**.......................................................................
 * Public method to set the optical camera FOV
 */
void setOpticalCameraFov(const sza::util::Angle& fov)
{
  optCamFov_ = fov;
}

/**.......................................................................
 * Public method to reset the optical camera FOV
 */
void setOpticalCameraFov()
{
  setOpticalCameraFov(defaultOptCamFov_);
}

/**.......................................................................
 * Public method to set the optical camera FOV
 */
void setOpticalCameraAspect(double aspect)
{
  optCamAspect_ = (aspect > 0.0 ? aspect : defaultOptCamAspect_);
}

/**.......................................................................
 * Public method to set the optical camera collimation
 */
void setOpticalCameraCollimation(const sza::util::Angle& collimation)
{
  optCamCollimation_ = collimation;
}

/**.......................................................................
 * Public method to set the optical camera collimation
 */
void setOpticalCameraCollimation()
{
  setOpticalCameraCollimation(defaultOptCamCollimation_);
}
