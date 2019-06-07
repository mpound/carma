#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <sys/ioctl.h>

#include "lprintf.h"
#include "szacontrol.h"
#include "szaregs.h"
#include "netbuf.h"
#include "arcfile.h"

#include "archiver.h"
#include "list.h"
#include "slprintf.h"

#include "carma/szautil/Debug.h"
#include "carma/szautil/FdSet.h"

#include "carma/szaarrayutils/archive.h"

using namespace sza::array;
using namespace sza::util;

/*
 * Register frames will be maintained in objects of the following type.
 * Callers must acquire the guard mutex before attempting to access
 * the frame buffer.
 */
typedef struct {
  pthread_mutex_t guard;       /* The mutual exlusion guard of the frame */
  int guard_ok;                /* True after initializing 'guard' */
  pthread_cond_t start;        /* When the archiver has finished saving */
                               /*  the latest integration, it will set */
                               /*  the 'integrate' flag to 1 and signal */
                               /*  the 'start' condition variable */
  int start_ok;                /* True after initializing 'start' */
  int integrate;               /* See the documentation of 'start' */
  unsigned nframe;             /* The number of frames to accumulate per */
                               /*  integration. */
  unsigned count;              /* The number of frames accumulated during */
                               /*  an ongoing integration */
  bool newFrame;               /* True if we should begin a new frame
				  on the next half-second boundary */
  unsigned currFrameSeq;       /* The sequence number associated with
				  the current frame */
  unsigned lastFrameSeq;       /* The sequence number associated with
				  the current frame */
  ArrayMap *arraymap;          /* The array map of the SZA */
  RegRawData *frame;           /* The frame being integrated or copied */
  unsigned int* features;      /* A pointer to the frame.features register */
                               /*  in frame[] */
  unsigned int* markSeq;       /* A pointer to the frame.mark_seq register */
                               /*  in frame[] */
  unsigned int lastSeq;        /* The value of *mark_seq when the last */
                               /*  frame was saved to disk. */
} FrameBuffer;

static int arc_frame_error(FrameBuffer *fb);

/*
 * An object of the following type is used to hold the state of
 * the archiver thread.
 */
struct Archiver {
  ControlProg *cp;             /* The state-object of the control program */
  Pipe *pipe;                  /* An externally allocated control-input pipe */
  ListMemory *list_mem;        /* Memory for allocating lists and their nodes */
  List *clients;               /* The list of connected clients */
  CcPipeMsg ccmsg;             /* A control client message container to use */
                               /*  for sending status messages to clients in */
                               /*  the clients list. */
  char *dir;                   /* The directory in which to place new archive */
                               /*  files. */
  char *path;                  /* The pathname of the current archive file */
  FILE *fp;                    /* The file-pointer attached to 'path' */
  FrameBuffer fb;              /* The integration frame buffer */
  NetBuf *net;                 /* The network buffer of the output frame */
  int filter;                  /* If true only record frames that contain */
                               /*  feature markers. */
  int file_size;               /* The number of frames to record before */
                               /*  starting a new archive file, or 0 if */
                               /*  no limit is desired. */
  int nrecorded;               /* The number of frames recorded in the */
                               /*  current file. */
  struct {
    int pending;
    unsigned seq;
  } tv_offset;
};

static int chdir_archiver(Archiver *arc, char *dir);
static int open_arcfile(Archiver *arc, char *dir);
static void flush_arcfile(Archiver *arc);
static void close_arcfile(Archiver *arc);
static int arc_save_integration(Archiver *arc);
static int arc_update_sampling(Archiver *arc, unsigned nframe, unsigned seq);
static int arc_send_ccmsg(Archiver *arc, Pipe *client, const char *fmt, ...);
static int arc_add_client(Archiver *arc, Pipe *client);
static int arc_rem_client(Archiver *arc, Pipe *client);
static void* arc_find_reg(Archiver *arc, char* regmap, char *board, 
			  char *name, DataType::Type type);

/*.......................................................................
 * Create the state object of a data-archiver thread.
 *
 * Input:
 *  cp     ControlProg *   The state object of the control program.
 *  pipe          Pipe *   The pipe on which to listen for control messages.
 * Output:
 *  return        void *   The new Archiver object, or NULL on error.
 */
CP_NEW_FN(new_Archiver)
{
  Archiver *arc;     /* The object to be returned */
  int status;        /* The status return value of a pthread function */
/*
 * Allocate the container.
 */
  arc = (Archiver* )malloc(sizeof(Archiver));
  if(!arc) {
    lprintf(stderr, "new_Archiver: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the container
 * at least up to the point at which it can be safely passed to del_Archiver().
 */
  arc->cp = cp;
  arc->pipe = pipe;
  arc->list_mem = NULL;
  arc->clients  = NULL;
  arc->ccmsg.id = CC_ARC_MSG;
  arc->ccmsg.msg.arc.text[0] = '\0';
  arc->dir  = NULL;
  arc->path = NULL;
  arc->fp   = NULL;
  arc->fb.guard_ok = 0;
  arc->fb.start_ok = 0;
  arc->fb.integrate = 1;
  arc->fb.nframe = ARC_DEF_NFRAME;
  arc->fb.count = 0;
  arc->fb.arraymap = cp_ArrayMap(cp);
  arc->fb.frame = NULL;
  arc->fb.features = NULL;
  arc->fb.markSeq = NULL;
  arc->fb.lastSeq = 0;
  arc->fb.currFrameSeq = 0;
  arc->fb.lastFrameSeq = 0;
  arc->fb.newFrame = false;
  arc->net = NULL;
  arc->filter = 0;
  arc->file_size = 0;
  arc->nrecorded = 0;
  arc->tv_offset.pending = 0;
  arc->tv_offset.seq = 0;

/*
 * Allocate a memory supplier for lists and their nodes.
 */
  arc->list_mem = new_ListMemory(5, 20);
  if(!arc->list_mem)
    return del_Archiver(arc);
/*
 * Allocate the list of control-client reply pipes.
 */
  arc->clients = new_List(arc->list_mem);
  if(!arc->clients)
    return del_Archiver(arc);
/*
 * Create the mutex that protects access to the integration frame
 * buffer.
 */
  status = pthread_mutex_init(&arc->fb.guard, NULL);
  if(status) {
    lprintf(stderr, "pthread_mutex_init: %s\n", strerror(status));
    return del_Archiver(arc);
  };
  arc->fb.guard_ok = 1;
/*
 * Create the condition variable that the archiver uses to signal that
 * it has finished copying the latest integrated frame from its
 * frame buffer into its network buffer.
 */
  status = pthread_cond_init(&arc->fb.start, NULL);
  if(status) {
    lprintf(stderr, "pthread_cond_init: %s\n", strerror(status));
    return del_Archiver(arc);
  };
  arc->fb.start_ok = 1;
  
  // Allocate the frame-store in which integrated frames are
  // accumulated.

  arc->fb.frame = new_RegRawData(arc->fb.arraymap, true);
  if(!arc->fb.frame)
    return del_Archiver(arc);
  
  // Determine the sizes of each of the records that appear in archive
  // files.

  {
    long size1 = NET_LONG_SIZE;          /* Initial buffer-dimension record */
    long size2 = net_SzaArrayMap_size(); /* Array map record */
    long size3 = net_RegRawData_size(arc->fb.frame); /* Frame record */
    
    // Determine the largest of the record sizes.

    long recsize = size1 > size2 ? size1 : size2;
    if(size3 > recsize)
      recsize = size3;
    
    // Allocate a buffer in which to compose archive records.

    arc->net = new_NetBuf(NET_PREFIX_LEN + recsize);
    if(!arc->net)
      return del_Archiver(arc);
  };


  // Find useful registers

  if(!(arc->fb.features        = (unsigned int*) arc_find_reg(arc, "array",  "frame", "features", DataType::UINT)) ||
     !(arc->fb.markSeq         = (unsigned int*) arc_find_reg(arc, "array",  "frame", "markSeq",  DataType::UINT)))
      return del_Archiver(arc);

  return arc;
}

/*.......................................................................
 * This is a private function of new_Archiver(), used to lookup the frame
 * buffer address of a specified register and report an error if not found.
 *
 * Input:
 *  arc         Archiver *   The resource object of the archiver thread.
 *  board           char *   The name of the parent board of the register.
 *  name            char *   The name of the register.
 * Output:
 *  return unsigned long *   The address of the register in fb->frame->slots[].
 */
static void* arc_find_reg(Archiver *arc, char* regmap, char *board, 
			  char *name, DataType::Type type)
{
  sza::util::ArrayDataFrameManager* fm = arc->fb.frame->fm;
  int offset = fm->byteOffsetInFrameOf(regmap, board, name);

  if(offset < 0) {
    lprintf(stderr, "new_Archiver: Lookup of %s.%s.%s failed.\n", regmap, 
	    board, name);
    return NULL;
  }

  return fm->frame()->getPtr(offset, type);
}

/*.......................................................................
 * Delete the state-object of an archiver thread.
 *
 * Input:
 *  obj         void *  The Archiver object to be deteled.
 * Output:
 *  return      void *  The deleted Archiver object (always NULL).
 */
CP_DEL_FN(del_Archiver)
{
  Archiver *arc = (Archiver* )obj;
  if(arc) {
    if(arc->dir) {
      free(arc->dir);
      arc->dir = NULL;
    };
/*
 * close_arcfile() tries to send a CLOSE message to all connected
 * control clients. We aren't in a state to do this, so prevent it
 * by clearing the list before calling close_arcfile().
 */
    clr_List(arc->clients);
    close_arcfile(arc);
    arc->clients = del_List(arc->clients);
    if(arc->fb.guard_ok)
      pthread_mutex_destroy(&arc->fb.guard);
    if(arc->fb.start_ok)
      pthread_cond_destroy(&arc->fb.start);
    arc->fb.frame = del_RegRawData(arc->fb.frame);
    arc->net = del_NetBuf(arc->net);
/*
 * Delete the list memory supplier after having deleted all lists
 * that used it.
 */
    arc->list_mem = del_ListMemory(arc->list_mem, 1);
    free(arc);
  };
  return NULL;
}

/*.......................................................................
 * Return the archiver resource object.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 * Output:
 *  return      Archiver *   The archiver resource object.
 */
Archiver *cp_Archiver(ControlProg *cp)
{
  return (Archiver* )cp_ThreadData(cp, CP_ARCHIVER);
}

/*.......................................................................
 * Attempt to send a message to a archiver thread.
 *
 * Input:
 *  cp      ControlProg *  The state-object of the control program.
 *  msg ArchiverMessage *  The message to be sent. This must have been
 *                         filled by one of the pack_archiver_<type>()
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
PipeState send_ArchiverMessage(ControlProg *cp, ArchiverMessage *msg,
			       long timeout)
{
#ifndef NEW_PIPE
  return write_pipe(cp_Archiver(cp)->pipe, msg, sizeof(*msg), timeout);
#else
  return cp_Archiver(cp)->pipe->write(msg, sizeof(*msg), timeout);
#endif
}

/*.......................................................................
 * Send a shutdown message to the archiver thread using non-blocking I/O.
 * Return 0 if the message was sent, 0 otherwise.
 *
 * Input:
 *  cp      ControlProg *  The control-program resource object.
 * Output:
 *  return          int    0 - Message sent ok.
 *                         1 - Unable to send message.
 */
CP_STOP_FN(stop_Archiver)
{
  ArchiverMessage msg;   /* The message to be sent */

  return pack_archiver_shutdown(&msg) ||
    send_ArchiverMessage(cp, &msg, PIPE_NOWAIT) != PIPE_OK;
}

/*.......................................................................
 * Prepare a shutdown message for subsequent transmission to the
 * archiver thread.
 *
 * Input:
 *  msg  ArchiverMessage *  The message object to be prepared.
 * Output:
 * return       int     0 - OK.
 *                      1 - Error.
 */
int pack_archiver_shutdown(ArchiverMessage *msg)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_archiver_shutdown: NULL argument.\n");
    return 1;
  };
  msg->type = ARC_SHUTDOWN;
  return 0;
}

/*.......................................................................
 * Prepare a change-default-directory message for subsequent transmission
 * to the archiver thread.
 *
 * Input:
 *  msg   ArchiverMessage *  The message object to be packed for subsequent
 *                         transmission.
 *  dir            char *  The directory in which to open subsequent log
 *                         files.
 * Output:
 * return           int    0 - OK.
 *                         1 - Error.
 */
int pack_archiver_chdir(ArchiverMessage *msg, char *dir)
{
/*
 * Check arguments.
 */
  if(!msg || !dir) {
    lprintf(stderr, "pack_archiver_chdir: NULL argument(s).\n");
    return 1;
  };
  msg->type = ARC_CHDIR;
  strncpy(msg->body.chdir.dir, dir, CP_FILENAME_MAX);
  msg->body.chdir.dir[CP_FILENAME_MAX] = '\0';
  return 0;
}

/*.......................................................................
 * Prepare a tv_offset done message for subsequent transmission
 * to the archiver thread.
 *
 * Input:
 *  msg   ArchiverMessage *  The message object to be packed for subsequent
 *                         transmission.
 *  dir            char *  The directory in which to open subsequent log
 *                         files.
 * Output:
 * return           int    0 - OK.
 *                         1 - Error.
 */
int pack_archiver_tv_offset_done(ArchiverMessage *msg, unsigned seq)
{
  msg->type = ARC_TV_OFFSET_DONE;
  msg->body.tv_offset_done.seq = seq;
  return 0;
}

/*.......................................................................
 * Prepare a tv_offset done message for subsequent transmission
 * to the archiver thread.
 *
 * Input:
 *  msg   ArchiverMessage *  The message object to be packed for subsequent
 *                         transmission.
 *  dir            char *  The directory in which to open subsequent log
 *                         files.
 * Output:
 * return           int    0 - OK.
 *                         1 - Error.
 */
int pack_archiver_newFrame(ArchiverMessage *msg, unsigned seq)
{
  msg->type = ARC_NEW_FRAME;
  msg->body.newFrame.seq = seq;
  return 0;
}

/*.......................................................................
 * Prepare an open-data-file message for subsequent transmission to the
 * archiver thread.
 *
 * Input:
 *  msg  ArchiverMessage *  The message object to be prepared.
 *  dir             char *   The directory in which to open the file.
 * Output:
 * return            int     0 - OK.
 *                           1 - Error.
 */
int pack_archiver_open(ArchiverMessage *msg, char *dir)
{
/*
 * Check arguments.
 */
  if(!msg || !dir) {
    lprintf(stderr, "pack_archiver_open: NULL argument(s).\n");
    return 1;
  };
  msg->type = ARC_OPEN;
  strncpy(msg->body.open.dir, dir, CP_FILENAME_MAX);
  msg->body.open.dir[CP_FILENAME_MAX] = '\0';
  return 0;
}

/*.......................................................................
 * Prepare a flush-data-file message for subsequent transmission to the
 * archiver thread.
 *
 * Input:
 *  msg  ArchiverMessage *  The message object to be prepared.
 * Output:
 * return       int     0 - OK.
 *                      1 - Error.
 */
int pack_archiver_flush(ArchiverMessage *msg)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_archiver_flush: NULL argument.\n");
    return 1;
  };
  msg->type = ARC_FLUSH;
  return 0;
}

/*.......................................................................
 * Prepare a close-data-file message for subsequent transmission to the
 * archiver thread.
 *
 * Input:
 *  msg  ArchiverMessage *  The message object to be prepared.
 * Output:
 * return       int     0 - OK.
 *                      1 - Error.
 */
int pack_archiver_close(ArchiverMessage *msg)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_archiver_close: NULL argument.\n");
    return 1;
  };
  msg->type = ARC_CLOSE;
  return 0;
}

/*.......................................................................
 * Prepare a change-integration-time message for subsequent transmission
 * to the archiver thread.
 *
 * Input:
 *  cp   ControlProg *  The state-object of the control program.
 *  nframe  unsigned    The number of samples per integration.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
int pack_archiver_sampling(ArchiverMessage *msg, unsigned nframe, unsigned seq)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_archiver_sampling: NULL argument.\n");
    return 1;
  };

  msg->type = ARC_SAMPLING;
  msg->body.sampling.nframe = nframe;
  msg->body.sampling.seq    = seq;

  return 0;
}

/*.......................................................................
 * Prepare a save-archive-frame message for subsequent transmission to
 * the archiver thread.
 *
 * Input:
 *  msg  ArchiverMessage *  The message object to be prepared.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
int pack_archiver_frame(ArchiverMessage *msg)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_archiver_frame: NULL argument.\n");
    return 1;
  };
/*
 * Compose the message.
 */
  msg->type = ARC_FRAME;
  return 0;
}

/*.......................................................................
 * Prepare a filter enable/disable message for subsequent transmission
 * to the archiver thread.
 *
 * Input:
 *  cp   ControlProg *  The state-object of the control program.
 *  enable       int    True to enable feature based filtering.
 *                      False to archive everything.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
int pack_archiver_filter(ArchiverMessage *msg, int enable)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_archiver_filter: NULL argument.\n");
    return 1;
  };
  msg->type = ARC_FILTER;
  msg->body.filter.enable = enable;
  return 0;
}

/*.......................................................................
 * Pack a message that will request a change of the number of frames per
 * archive file.
 *
 * Input:
 *  cp   ControlProg *  The state-object of the control program.
 *  nframe  unsigned    The number of frames per file (or zero to request
 *                      no limit).
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
int pack_archiver_file_size(ArchiverMessage *msg, unsigned nframe)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_archiver_file_size: NULL argument.\n");
    return 1;
  };
  msg->type = ARC_FILE_SIZE;
  msg->body.file_size.nframe = nframe;
  return 0;
}

/*.......................................................................
 * Prepare a register-control-client message for subsequent transmission
 * to the archiver thread.
 *
 * Input:
 *  msg    ArchiverMessage *  The message object to be packed for subsequent
 *                            transmission.
 *  client            Pipe *  The reply pipe of the new control client.
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int pack_archiver_add_client(ArchiverMessage *msg, Pipe *client)
{
/*
 * Check arguments.
 */
  if(!msg || !client) {
    lprintf(stderr, "pack_archiver_add_client: NULL argument(s).\n");
    return 1;
  };
  msg->type = ARC_ADD_CLIENT;
  msg->body.client.pipe = client;
  return 0;
}

/*.......................................................................
 * Prepare an unregister-control-client message for subsequent
 * transmission to the archiver thread.
 *
 * Input:
 *  msg   ArchiverMessage *  The message object to be packed for subsequent
 *                           transmission.
 *  client           Pipe *  The reply pipe of the new control client.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
int pack_archiver_rem_client(ArchiverMessage *msg, Pipe *client)
{
/*
 * Check arguments.
 */
  if(!msg || !client) {
    lprintf(stderr, "pack_archiver_rem_client: NULL argument(s).\n");
    return 1;
  };
  msg->type = ARC_REM_CLIENT;
  msg->body.client.pipe = client;
  return 0;
}

/**.......................................................................
 * Add a new frame to the current integration, or to a new integration
 * if one has just finished. If the archiver is busy copying the last
 * integration out of the integration buffer, then this function waits
 * for it to finish before proceding.
 *
 * Input:
 *  cp     ControlProg *   The state-object of the control program.
 *  frame   RegRawData *   The frame to add to the latest integration.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
int arc_integrate_frame_old(ControlProg *cp, RegRawData *frame)
{
  Archiver *arc = cp_Archiver(cp);
  FrameBuffer *fb = &arc->fb;
  int status;        /* The status return value of a pthread function */
  int i;
  
  // Gain exclusive access to the frame buffer.

  if((status=pthread_mutex_lock(&fb->guard))) {
    lprintf(stderr, "arc_integrate_frame (mutex_lock): %s.\n",
	    strerror(status));
    return 1;
  };
  
  // If an integration isn't currently in progress wait for the
  // archiver to release the buffer.

  while(!fb->integrate) {
    status = pthread_cond_wait(&fb->start, &fb->guard);
    if(status) {
      lprintf(stderr, "arc_integrate_frame (cond_wait): %s.\n",
	      strerror(status));
      return arc_frame_error(fb);
    };
  };
  
  // Initialize the buffer on the first record of an integration.

  if(fb->count == 0) {
    memcpy(fb->frame->slots, frame->slots, frame->nslot *
	   sizeof(frame->slots[0]));
  } else {

    ArrayMap *arraymap = cp_ArrayMap(cp);

    // Get pointers to the start of each of the frame arrays.

    long *from = frame->slots;
    long *to   = fb->frame->slots;
    
    // Integrate the new frame.

    int iarregmap;
    for(iarregmap=0; iarregmap<arraymap->nregmap; iarregmap++) {
      ArrRegMap* arregmap = arraymap->regmaps[iarregmap];
      RegMap* regmap      = arregmap->regmap;
      int board;

      // Skip register map that contain no archived registers.

      if(regmap->narchive_ == 0)
	continue;

      for(board=0; board<regmap->nboard_; board++) {
	RegMapBoard* brd = regmap->boards_[board];
	int block;
	
	// Skip boards that contain no archived registers.
	
	if(brd->narchive == 0)
	  continue;
	
	// Process the archived blocks of the current board.
	
	for(block=0; block<brd->nblock; block++) {
	  RegMapBlock* blk = brd->blocks[block];
	  
	  // Skip unarchived blocks.
	  
	  if(blk->flags_ & REG_EXC)
	    continue;
	  
	  // Integrate the register?
	  
	  if(blk->flags_ & REG_SUM) {
	    
	    // Integrate signed registers.
	    
	    if(blk->flags_ & REG_INT) {
	      for(i=0; i < (int)blk->nreg_; i++) {
		*to++ += *from++;
	      }
	      
	      // Integrate unsigned registers.
	      
	    } else {
	      for(i=0; i < (int)blk->nreg_; i++)
		*(unsigned long *)to++ += *(unsigned long *)from++;
	    };
	    
	    // Merge bit-mask unions.
	    
	  } else if(blk->flags_ & REG_UNION) {
	    for(i=0; i < (int)blk->nreg_; i++)
	      *(unsigned long *)to++ |= *(unsigned long *)from++;
	    
	    // Take the final value of a snapshot register?
	    
	  } else {
	    for(i=0; i < (int)blk->nreg_; i++)
	      *to++ = *from++;
	  };
	};
      };
    };
  };
  
  // If the new integration brought us to the end of the current
  // integration, disable further integration and queue the completed
  // integration for archiving.

  if(++fb->count >= fb->nframe) {
    ArchiverMessage msg;
    fb->integrate = 0;
    if(pack_archiver_frame(&msg) ||
       send_ArchiverMessage(cp, &msg, PIPE_WAIT) != PIPE_OK)
      return arc_frame_error(fb);
  };
  
  // Release the frame buffer.

  pthread_mutex_unlock(&fb->guard);

  return 0;
}

/**.......................................................................
 * Add a new frame to the current integration, or to a new integration
 * if one has just finished. If the archiver is busy copying the last
 * integration out of the integration buffer, then this function waits
 * for it to finish before proceding.
 *
 * Input:
 *  cp     ControlProg *   The state-object of the control program.
 *  frame   RegRawData *   The frame to add to the latest integration.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
int arc_integrate_frame(ControlProg *cp, RegRawData *frame)
{
  Archiver *arc = cp_Archiver(cp);
  FrameBuffer *fb = &arc->fb;
  int status;        /* The status return value of a pthread function */
  
  // Gain exclusive access to the frame buffer.

  if((status=pthread_mutex_lock(&fb->guard))) {
    lprintf(stderr, "arc_integrate_frame (mutex_lock): %s.\n",
	    strerror(status));
    return 1;
  };
  
  // If an integration isn't currently in progress wait for the
  // archiver to release the buffer.

  while(!fb->integrate) {
    status = pthread_cond_wait(&fb->start, &fb->guard);
    if(status) {
      lprintf(stderr, "arcIntegrateFrame (cond_wait): %s.\n",
	      strerror(status));
      return arc_frame_error(fb);
    };
  };

  // Initialize the buffer on the first record of an integration.

  if(fb->count == 0) 
    *fb->frame->fm  = *frame->fm;
  else
    *fb->frame->fm += *frame->fm;

  //  COUT(std::endl << "Just added a frame in arc_integrate_frame: "
  //       << " fb->count = " << fb->count 
  //       << " fb->lastSeq = " << fb->lastSeq
  //       << " *fb->markSeq = " << *fb->markSeq
  //       << " ff = " << *ptr
  //       << " *fbf = " << *fb->features);

  // If the new integration brought us to the end of the current
  // integration, or we were requested to start a new frame, disable
  // further integration and queue the completed integration for
  // archiving.

  bool sendMsg = false;

  if(++fb->count >= fb->nframe || fb->newFrame) {

    //    COUT(std::endl << "New frame will start on the next integration: " 
    //	 << " fb->count = " << fb->count
    //	 << " fb->nframe = " << fb->nframe
    //	 << " fb->newFrame = " << fb->newFrame);

    fb->integrate = 0;
    fb->newFrame = false;
    sendMsg = true;
  };

  // Release the frame buffer.

  pthread_mutex_unlock(&fb->guard);

  // The next clause used to be inside the above loop, before the call
  // to pthread_mutex_unlock(), but this can cause a deadlock, in the
  // following way:
  //
  // Let's say arc_integrate_frame() is called by the control thread,
  // and before reaching the send_ArchiverMessage() line, the archiver
  // thread gets an ARC_SAMPLING message.  Then the archiver calls
  // arc_update_sampling(), which blocks trying to lock fb->guard,
  // because the control thread has the lock in this function.  Then
  // the control thread calls send_ArchiverMessage() with PIPE_WAIT,
  // which blocks if the queue is full, waiting for the archiver
  // thread to read a message out of it to make some room.  But the
  // archiver thread is blocked on the call to pthread_mutex_lock(),
  // so we are deadlocked.

  if(sendMsg) {
    ArchiverMessage msg;
    if(pack_archiver_frame(&msg) ||
       send_ArchiverMessage(cp, &msg, PIPE_WAIT) != PIPE_OK)
      return arc_frame_error(fb);
  }  

  return 0;
}

/*.......................................................................
 * This is the error return function of arc_integrate_frame(). It
 * releases exclusive access to the frame buffer and returns the error
 * code of arc_integrate_frame().
 *
 * Input:
 *  fb     FrameBuffer *  The locked frame buffer.
 * Output:
 *  return         int    1.
 */
static int arc_frame_error(FrameBuffer *fb)
{
  pthread_mutex_unlock(&fb->guard);
  return 1;
}

/*.......................................................................
 * This is the entry-point of the archiver thread.
 *
 * Input:
 *  arg          void *  A pointer to the Archiver state object pointer,
 *                       cast to (void *).
 * Output:
 *  return       void *  NULL.
 */
CP_THREAD_FN(archiver_thread)
{
  Archiver *arc = (Archiver* )arg; /* The state-object of the current thread */
  ArchiverMessage msg; /* An message reception container */
/*
 * Enable logging of the archiver's stdout and stderr streams.
 */
  if(log_thread_stream(arc->cp, stdout) ||
     log_thread_stream(arc->cp, stderr)) {
    cp_report_exit(arc->cp);
    return NULL;
  };
/*
 * Wait for commands from other threads.
 */
#ifndef NEW_PIPE
  while(read_pipe(arc->pipe, &msg, sizeof(msg), PIPE_WAIT) == PIPE_OK) {
#else

    sza::util::FdSet fdSet;
    fdSet.registerReadFd(arc->pipe->readFd());

    while(select(fdSet.size(), fdSet.readFdSet(), NULL, NULL, NULL) > 0) {

      if(arc->pipe->read(&msg, sizeof(msg), PIPE_WAIT) != PIPE_OK)
	break;

#endif
/*
 * Interpret the message.
 */
    switch(msg.type) {
    case ARC_FRAME:
      (void) arc_save_integration(arc);
      break;
    case ARC_NEW_FRAME:
      arc->fb.newFrame = true;

      // Only increment the sequence number if it wasn't from an
      // interactive command

      if(msg.body.newFrame.seq > 0)
	arc->fb.currFrameSeq = msg.body.newFrame.seq;

      break;
    case ARC_SHUTDOWN:
      close_arcfile(arc);
      cp_report_exit(arc->cp);
      return NULL;
      break;
    case ARC_CHDIR:
      (void) chdir_archiver(arc, msg.body.chdir.dir);
      break;
    case ARC_OPEN:
      (void) open_arcfile(arc, msg.body.open.dir);
      break;
    case ARC_FLUSH:
      flush_arcfile(arc);
      break;
    case ARC_CLOSE:
      close_arcfile(arc);
      break;
    case ARC_SAMPLING:
      (void) arc_update_sampling(arc, msg.body.sampling.nframe, msg.body.sampling.seq);
      break;
    case ARC_FILTER:
      arc->filter = msg.body.filter.enable;
      arc_send_ccmsg(arc, NULL, "FILTER %s", arc->filter ? "yes":"no");
      break;
    case ARC_FILE_SIZE:
      arc->file_size = msg.body.file_size.nframe;
      arc_send_ccmsg(arc, NULL, "SIZE %d", arc->file_size);
      break;
    case ARC_ADD_CLIENT:
      arc_add_client(arc, msg.body.client.pipe);
      break;
    case ARC_REM_CLIENT:
      arc_rem_client(arc, msg.body.client.pipe);
      break;
    case ARC_TV_OFFSET_DONE:
      arc->tv_offset.pending = 1;
      arc->tv_offset.seq = msg.body.tv_offset_done.seq;
      break;
    default:
      lprintf(stderr, "archiver_thread: Unknown command-type received.\n");
      break;
    };
  };
  fprintf(stderr, "Archiver thread exiting after pipe read error.\n");
  cp_report_exit(arc->cp);
  return NULL;
}

/*.......................................................................
 * Flush unwritten data to the current archive data-file.
 *
 * Input:
 *  arc     Archiver *   The state-object of the archiver thread.
 */
static void flush_arcfile(Archiver *arc)
{
  if(arc->fp && fflush(arc->fp)) {
    lprintf(stderr, "Error flushing archive file: %s\n", arc->path);
    close_arcfile(arc);
  };
}

/*.......................................................................
 * Close the current archive data-file. Until a new data file is opened,
 * subsequently integrated frames will be discarded.
 *
 * Input:
 *  arc     Archiver *   The state-object of the archiver thread.
 */
static void close_arcfile(Archiver *arc)
{
  if(arc->fp) {
    if(fclose(arc->fp))
      lprintf(stderr, "Error closing archive file: %s\n", arc->path);
    else
      lprintf(stdout, "Closing archive file: %s\n", arc->path);
  };
  arc->fp = NULL;
  if(arc->path)
    free(arc->path);
  arc->path = NULL;
  arc->nrecorded = 0;
  arc_send_ccmsg(arc, NULL, "CLOSE");
}

/*.......................................................................
 * Open a new archive file in a given directory. If the file is opened
 * successfully, close the previous archive file.
 *
 * Input:
 *  arc   Archiver *   The state-object of the current thread.
 *  dir       char *   The name of the directory in which to create the
 *                     file or NULL or "" to use the last directory that
 *                     was specified.
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */
static int open_arcfile(Archiver *arc, char *dir)
{
  char *path;                  /* The path name of the file */
  FILE *fp;                    /* The file-pointer of the open file */
  unsigned long size;          /* The network buffer size */
/*
 * Get the network buffer used to pack records.
 */
  NetBuf *net = arc->net;
/*
 * Record a new log file directory?
 */
  if(dir && *dir!='\0')
    (void) chdir_archiver(arc, dir);
  else
    dir = arc->dir;
/*
 * Compose the full pathname of the archive file.
 */
  path = arc_path_name(dir, NULL, ARC_DAT_FILE);
  if(!path)
    return 1;
/*
 * Attempt to open the new data file.
 */
  fp = fopen(path, "wb");
  if(!fp) {
    lprintf(stderr, "Unable to open archive file: %s\n", path);
    free(path);
    return 1;
  };
/*
 * Close the current archive file if open.
 */
  close_arcfile(arc);
/*
 * Install the new archive file.
 */
  arc->path = path;
  arc->fp = fp;
  arc_send_ccmsg(arc, NULL, "OPEN %s", arc->path);
/*
 * Report the successful opening of the file.
 */
  lprintf(stdout, "Starting new archive file: %s\n", path);
/*
 * Output the initial record that contains the minimum network buffer
 * size needed to read records from the file.
 */
  size = net->size;
  if(net_start_put(net, ARC_SIZE_RECORD) ||
     net_put_long(net, 1, &size) ||
     net_end_put(net) ||
     (long)fwrite(net->buf, sizeof(net->buf[0]),
	    net->nput, arc->fp) != net->nput) {
    lprintf(stderr, "Error writing archive-file size (%s).\n",
	    strerror(errno));
    close_arcfile(arc);
    return 1;
  };
/*
 * Output the record that contains the details of the array map.
 */
  if(net_start_put(net, ARC_ARRAYMAP_RECORD) ||
     net_put_SzaArrayMap(net) ||
     net_end_put(net) ||
     (long)fwrite(net->buf, sizeof(net->buf[0]),
	    net->nput, arc->fp) != net->nput) {
    lprintf(stderr, "Error writing archive-file regmap (%s).\n",
	    strerror(errno));
    close_arcfile(arc);
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Change the directory in which subsequent archive files will be written.
 *
 * Input:
 *  dir      char *   The directory to use when creating subsequent
 *                    archive files.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
static int chdir_archiver(Archiver *arc, char *dir)
{
/*
 * Make a copy of the name of the current archive directory.
 */
  if(dir != arc->dir && *dir != '\0') {
    size_t bytes = strlen(dir)+1;
    char *tmp = (char* )(arc->dir ? realloc(arc->dir, bytes) : malloc(bytes));
    if(!tmp) {
      lprintf(stderr, "Unable to record new archive directory.\n");
      return 1;
    } else {
      strcpy(tmp, dir);
      arc->dir = tmp;
      arc_send_ccmsg(arc, NULL, "DIR %s", arc->dir);
    };
  };
  return 0;
}

/*.......................................................................
 * Record the latest integrated frame.
 *
 * Input:
 *  arc    Archiver *  The state object of the current thread.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
static int arc_save_integration(Archiver *arc)
{
  int status;        /* The status return value of a pthread function */
  
  // Get convenient aliases of the frame buffer and output buffer.

  FrameBuffer *fb = &arc->fb;
  NetBuf *net = arc->net;
  
  // Acquire exclusive access to the frame buffer.

  if((status=pthread_mutex_lock(&fb->guard))) {
    lprintf(stderr, "arc_save_integration (mutex_lock): %s.\n",
	    strerror(status));
    return 1;
  };

  // Construct an output record of the integrated data.  Note that the
  // return status is ignored until exclusive access to the buffer has
  // been released further below.

  status = net_start_put(net, ARC_FRAME_RECORD) ||
           net_put_RegRawData(net, fb->frame) ||
	   net_end_put(net);
  
  // Prepare the frame buffer for the next integration.

  fb->integrate = 1;
  fb->count = 0;
  
  // Signal other threads that the frame-buffer is now ready for a new
  // integration.

  pthread_cond_signal(&fb->start);
  
  // Relinquish exclusive access to the buffer.

  pthread_mutex_unlock(&fb->guard);
  
  // Having safely released exclusive access to the frame buffer,
  // abort if there was an error.

  if(status)
    return 1;
  
  // If the effects of a new mark command are being saved, send a
  // message to the scheduler to tell it that the mark transaction has
  // been completed.

  if(fb->lastSeq < *fb->markSeq) {
    SchedulerMessage msg;
    if(pack_scheduler_mark_done(&msg, *fb->markSeq)==0 &&
       send_SchedulerMessage(arc->cp, &msg, PIPE_WAIT)==PIPE_OK) {
      fb->lastSeq = *fb->markSeq;
    };
  };

  // If the effects of a new mark command are being saved, send a
  // message to the scheduler to tell it that the mark transaction has
  // been completed.

  if(fb->lastFrameSeq < fb->currFrameSeq) {
    SchedulerMessage msg;
    if(pack_scheduler_frame_done(&msg, fb->currFrameSeq)==0 &&
       send_SchedulerMessage(arc->cp, &msg, PIPE_WAIT)==PIPE_OK) {
      fb->lastFrameSeq = fb->currFrameSeq;
    };
  };
  
  // Do we have an archive file to save to, and should we save this
  // frame?

  if(arc->fp && (!arc->filter || *fb->features)) {
    if((long)fwrite(net->buf, sizeof(net->buf[0]), net->nput, arc->fp) != 
       net->nput) {
      lprintf(stderr, "Error writing archive data file (%s).\n",
	      strerror(errno));
      close_arcfile(arc);
      return 1;
    };
    
    // If the current archive file has reached the maximum size
    // specified by the user, open a new one for the next record.

    if(arc->file_size > 0 && ++arc->nrecorded >= arc->file_size)
      return open_arcfile(arc, arc->dir);
    
    // If we are waiting for the effects of a tv_offset command to be
    // written to the archive, send a transaction completion message
    // to the scheduler now.

    if(arc->tv_offset.pending) {
      SchedulerMessage msg;
      if(pack_scheduler_tv_offset_done(&msg, arc->tv_offset.seq)==1 ||
	 send_SchedulerMessage(arc->cp, &msg, PIPE_WAIT)==PIPE_ERROR) 
	return 1;
      arc->tv_offset.pending = 0;
    }
  };
  return 0;
}

/*.......................................................................
 * Change the number of snapshot records per integration.
 *
 * Input:
 *  arc       Archiver *   The state-object of this thread.
 *  nframe    unsigned     The new number of records to accumulate per
 *                         integration.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
static int arc_update_sampling(Archiver *arc, unsigned nframe, unsigned seq)
{
  int status;   /* The return status of pthread functions */
  FrameBuffer *fb = &arc->fb;
/*
 * Acquire exclusive access to the frame buffer.
 */
  if((status=pthread_mutex_lock(&fb->guard))) {
    lprintf(stderr, "arc_update_sampling (mutex_lock): %s.\n",strerror(status));
    return 1;
  };

  fb->nframe = nframe > 0 ? nframe : 1;

  // Update the sequence number for the frame as well

  fb->currFrameSeq = seq;

/*
 * Relinquish exclusive acces to the frame buffer.
 */
  pthread_mutex_unlock(&fb->guard);
/*
 * Tell control clients about the new setting.
 */
  arc_send_ccmsg(arc, NULL, "SAMPLE %u", fb->nframe);
  return 0;
}

/*.......................................................................
 * Add a control-client reply pipe to the list of control-clients to
 * which error messages should be echoed.
 *
 * Input:
 *  arc      Archiver *   The state-object of the archiver thread.
 *  client       Pipe *   The reply pipe of a control client.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int arc_add_client(Archiver *arc, Pipe *client)
{
/*
 * Append the client pipe to the list of reply pipes.
 */
  if(append_ListNode(arc->clients, client) == NULL)
    return 1;
/*
 * Report the current status of the archiver.
 */
  if(arc_send_ccmsg(arc, client, "FILTER %s", arc->filter ? "on":"off") ||
     arc_send_ccmsg(arc, client, "SIZE %d", arc->file_size) ||
     (arc->path ? arc_send_ccmsg(arc, client, "OPEN %s", arc->path) :
                  arc_send_ccmsg(arc, client, "CLOSE")) ||
     arc_send_ccmsg(arc, client, "DIR %s", arc->dir ? arc->dir : ".") ||
     arc_send_ccmsg(arc, client, "SAMPLE %u", arc->fb.nframe))
    return 1;
  return 0;
}

/*.......................................................................
 * Remove a given control-client reply pipe from the list of
 * control-clients to which error messages should be echoed.
 *
 * Input:
 *  arc      Archiver *   The state-object of the archiver thread.
 *  client       Pipe *   The reply pipe of a control client.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int arc_rem_client(Archiver *arc, Pipe *client)
{
  ListNode *node;    /* A node of the list of connected clients */
  
  // Locate the specified client.

  for(node = arc->clients->head; node; node=node->next) {
    if(node->data == (void *)client) {
      del_ListNode(arc->clients, node, NULL);
      return 0;
    };
  };
  
  // Client not found.

  lprintf(stderr, "arc_rem_client: Client not found.\n");
  return 1;
}

/*.......................................................................
 * Compose and send a status message to one or more clients.
 *
 * Input:
 *  arc      Archiver *   The state-object of the archiver thread.
 *  client       Pipe *   The client to write to, or NULL to write to
 *                        all of the clients in arc->clients.
 *  fmt          char *   A printf-style format.
 *  ...                   The arguments to be formatted by fmt[].
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int arc_send_ccmsg(Archiver *arc, Pipe *client, const char *fmt, ...)
{
  va_list ap;    /* The variable argument list */
  int nsent;     /* The number of characters written */
  
  // Initialize the variable argument list and have it processed.

  va_start(ap, fmt);
  nsent = vslprintf(arc->ccmsg.msg.arc.text, CC_MSG_MAX, fmt, ap);
  va_end(ap);
  
  // Error?

  if(nsent < 0)
    return 1;
  
  // Send the message to the specified client, or all connected
  // clients.

  if(client) {
    queue_cc_message(client, &arc->ccmsg);
  } else {
    ListNode *node;  /* A node in the list of connected clients */
    for(node = arc->clients->head; node; node=node->next)
      queue_cc_message((Pipe* )node->data, &arc->ccmsg);
  };

  return 0;
}

/*.......................................................................
 * A public function to return the value (unsigned) of a single register
 * out of the frame buffer.
 *
 * Input:
 *
 *  arc    Archiver  *  The resource container of the archiver task.
 *  board  char  *      The name of the board.
 *  name   char  *      The name of the register.
 *
 * Output:
 *  
 *  value  long         The value of the register.
 */
int get_reg_info(Archiver *arc, short iregmap, short board, short block, 
		 short index, unsigned long *val)
{
  ArrayMap *arraymap = arc->fb.arraymap;
  RegMapBlock *blk=NULL;
  RegMap* regmap=NULL;
  char *regName=0, *brdName=0, *blkName=0;

  // Check the register specification.

  if(iregmap >= arraymap->nregmap) {
    lprintf(stderr, "get_reg_info: Regmap index out of range.\n");
    return 1;
  };

  regmap  = arraymap->regmaps[iregmap]->regmap;
  regName = arraymap->regmaps[iregmap]->name;

  if(board >= regmap->nboard_) {
    lprintf(stderr, "get_reg_info: Board index out of range.\n");
    return 1;
  };

  brdName = regmap->boards_[board]->name;

  if(block >= regmap->boards_[board]->nblock) {
    lprintf(stderr, "get_reg_info: Block index out of range.\n");
    return 1;
  };
  
  // Get the register block to be written to.

  blk = regmap->boards_[board]->blocks[block];
  blkName = blk->name_;

  if(index < 0 || index > (short)blk->nreg_-1) {
    lprintf(stderr, "get_reg_info: Invalid index range.\n");
    return 1;
  };

  *val = *(((unsigned int*)arc_find_reg(arc, regName, brdName, blkName, 
					DataType::UINT))+index);
  
  return 0;
}
