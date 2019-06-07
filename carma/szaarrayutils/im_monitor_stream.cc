#include <stdlib.h>

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/im_monitor_stream.h"
#include "carma/szaarrayutils/optcam.h"

/*
 * Define the contents of a generic image monitor stream.  This is used for
 * reading frame grabber image data from a supplier.
 */
struct ImMonitorStream {
  void *context;                  /* The context of the current data-source */
  IMS_DESTRUCTOR(*del_fn);        /* The 'context' destructor function */
  IMS_READ_IMAGE(*read_fn);       /* The method used to read the next frame */
  IMS_SEND_MSG(*send_fn);         /* The method that is called to send */
  IMS_SELECT_FD(*fd_fn);          /* The method that returns the current fd */
  unsigned short image[GRABBER_IM_SIZE];   /* A buffer in which to store the 
					   frame grabber image. */
  int sending;                    /* True while an ongoing send operation is 
				     incomplete. */
};
/*.......................................................................
 * Read all or part of the next image from an image monitor stream.
 *
 * Input:
 *  ims     ImMonitorStream *  The stream to read from.
 *  dowait           int    Whether to block until the transaction is
 *                          complete.
 *                            0 - If the input stream temporarily
 *                                blocks while reading, defer completion
 *                                of the transaction to a later call to
 *                                this function.
 *                            1 - Wait for the transaction to complete
 *                                before returning.
 * Output:
 *  return   ImsReadState    The status of the operation, from:
 *                            IMS_READ_ENDED  - The end of the stream
 *                                             was reached before reading
 *                                             another image.
 *                            IMS_READ_AGAIN  - Call this function again
 *                                             to complete the transaction.
 *                                             This implies dowait=0.
 *                            IMS_READ_DONE   - The transaction completed.
 *                                             The calibrated registers
 *                                             can be found in ims->cal.
 */
ImsReadState ims_read_image(ImMonitorStream *ims, int dowait)
{
  ImsReadState status;  /* The status returned by read_fn() */
/*
 * Check the arguments.
 */
  if(!ims || !ims->context) {
    lprintf(stderr, "ims_read_frame: No data-source has been specified.\n");
    return IMS_READ_ENDED;
  };
/*
 * Attempt to read the next image.
 */
  status = ims->read_fn(ims, dowait);
  switch(status) {
  case IMS_READ_ENDED:
  case IMS_READ_AGAIN:
    return status;
    break;
  case IMS_READ_DONE:
/*
 * Discard incoming frames while configuration messages are being
 * sent.
 */
    if(ims->sending)
      return IMS_READ_AGAIN;
    return IMS_READ_DONE;
  };
  return IMS_READ_ENDED;
}

/*.......................................................................
 * Return the current file descriptor of the data-source. This must be
 * suitable for use in select().
 *
 * Input:
 *  ms     ImMonitorStream *  The stream to query.
 * Output:
 *  return           int    The file descriptor, or -1 if the connection
 *                          is closed.
 */
int ims_select_fd(ImMonitorStream *ims)
{
/*
 * Query the fd via the stream-specific method.
 */
  return (ims && ims->context) ? ims->fd_fn(ims) : -1;
}

/*.......................................................................
 * Create a generic iterator for use in reading monitor data from
 * varied sources. The iterator must be connected to a specific data
 * source via a call to open_ImMonitorStream(), and can thereafter be
 * reattached to a new stream by further calls to open_ImMonitorStream(),
 * or explicitly detached via a call to close_ImMonitorStream().
 *
 * Output:
 *  return   ImMonitorStream *  The new stream iterator.
 */
ImMonitorStream *new_ImMonitorStream(void)
{
  ImMonitorStream *ims;  /* The new iterator */
/*
 * Allocate the container.
 */
  ims = (ImMonitorStream *) malloc(sizeof(ImMonitorStream));
  if(!ims) {
    lprintf(stderr, "new_ImMonitorStream: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can safely be passed
 * to del_ImMonitorStream().
 */
  ims->context = NULL;
  ims->read_fn = 0;
  ims->send_fn = 0;
  ims->del_fn = 0;
  ims->sending = 0;
  return ims;
}

/*.......................................................................
 * Delete an image monitor-stream iterator. If a data-source is currently
 * open, close_ImMonitorStream() will be called first.
 *
 * Input:
 *  ims        ImMonitorStream *  The iterator to be deleted.
 * Output:
 *  return    ImMonitorStream *  The deleted iterator (always NULL).
 */
ImMonitorStream *del_ImMonitorStream(ImMonitorStream *ims)
{
  if(ims) {
    if(ims->context)
      close_ImMonitorStream(ims);
    free(ims);
  };
  return NULL;
}
/*.......................................................................
 * Return the current image.
 *
 * Input:
 *  ims     ImMonitorStream *  The stream to query.
 * Output:
 *  return      short The image from the network buffer.
 */
unsigned short *ims_get_image(ImMonitorStream *ims)
{
  return ims ? ims->image : NULL;
}
/*.......................................................................
 * Return the type-specific stream context object.
 *
 * Input:
 *  ims     ImMonitorStream *  The stream to query.
 * Output:
 *  return          void *  The context object of the client stream
 *                          inplementation. This will be NULL if the
 *                          stream is closed or if ms==NULL.
 */
void *ims_SourceContext(ImMonitorStream *ims)
{
  return ims ? ims->context : NULL;
}

/*.......................................................................
 * Connect an image monitor stream to a specific source of monitor data. Any
 * pre-existing connection will first be terminated via a call to
 * close_ImMonitorStream(ims).
 *
 * Input:
 *  ims            ImMonitorStream *  The stream to connect the data source to.
 *  context                void *  Any source-specific context.
 *  del_fn        IMS_DESTRUCTOR(*) The function to call to delete 'context'.
 *  read_fn       IMS_READ_IMAGE(*) The function to call to incrementally
 *                                 read some or all of the next image.
 *  send_fn         IMS_SEND_MSG(*) If the supplier is a remote entity that
 *                                 needs to have the values that are passed
 *                                 via regset_fn and interval_fn
 *                                 incrementally sent to it, provide a
 *                                 function here that can perform the
 *                                 incremental sends. It should return
 *                                 IMS_SEND_DONE when complete. Otherwise
 *                                 pass 0 here.
 * Output:
 *  return                  int    0 - OK.
 *                                 1 - Error (Note that del_fn(context) will
 *                                     have been invoked, so *context will
 *                                     no longer exist).
 */
int open_ImMonitorStream(ImMonitorStream *ims, void *context, 
			 IMS_DESTRUCTOR(*del_fn),
			 IMS_READ_IMAGE(*read_fn), IMS_SEND_MSG(*send_fn),
			 IMS_SELECT_FD(*fd_fn))
{
/*
 * Close any existing data-source.
 */
  if(ims)
    close_ImMonitorStream(ims);
/*
 * Check the input arguments.
 */
  if(!ims || !context || !del_fn || !read_fn || !fd_fn) {
    lprintf(stderr, "open_ImMonitorStream: Bad argument(s).\n");
    if(context && del_fn)
      context = del_fn(context);
    return 1;
  };
/*
 * Record the input parameters.
 */
  ims->context = context;
  ims->del_fn = del_fn;
  ims->read_fn = read_fn;
  ims->send_fn = send_fn;
  ims->fd_fn = fd_fn;

  return 0;
}

/*.......................................................................
 * Close a previously opened image monitor-stream data-source.
 *
 * Input:
 *  ims     ImMonitorStream *   The stream to be detached from its current
 *                           data-source.
 */
void close_ImMonitorStream(ImMonitorStream *ims)
{
  if(ims) {
    if(ims->context)
      ims->context = ims->del_fn(ims->context);
    ims->del_fn = 0;
    ims->read_fn = 0;
    ims->send_fn = 0;
    ims->sending = 0;
  };
}
