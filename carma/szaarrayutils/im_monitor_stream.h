#ifndef im_monitor_stream_h
#define im_monitor_stream_h

/*-----------------------------------------------------------------------
 * This module provides a generic stream interface for reading frame grabber
 * images from a source. Supported sources include a network
 * connection to the control program.
 *
 * The internals of a stream are logically split into two parts, a
 * local consumer and a potentially remote supplier. Given that the
 * supplier can be in a separate process, messaging via either
 * non-blocking or blocking I/O is used to dispatch control messages
 * to, and receive data from a given supplier. The only time that
 * non-blocking I/O is not an option is when the stream makes a
 * connection to a new supplier. At this time, information, such as
 * the register map of the supplier and buffer size information is
 * read from the supplier using blocking I/O.
 *
 * The following datatype provides an opaque handle for all stream
 * types.  Its internals are private to im_monitor_stream.c.
 */
typedef struct ImMonitorStream ImMonitorStream;

/*
 * Create a new ImMonitorStream object and attach it to a control
 * program on a given host. The host argument should contain
 * the name or internet address of the computer on which the
 * control program is running.
 *
 * When successful this function returns the new ImMonitorStream
 * object. On failure it returns NULL.
 */
ImMonitorStream *new_NetImMonitorStream(char *host);

/*
 * The following function closes and deletes a monitor stream.
 * Note that del_ImMonitorStream() is idempotent (ie. it returns NULL
 * to represent a deleted stream, and can safely take NULL as an
 * argument).
 */
ImMonitorStream *del_ImMonitorStream(ImMonitorStream *ims);

/*-----------------------------------------------------------------------
 * Stream method functions.
 *
 * The following functions should only be used while a stream is
 * connected to a supplier.
 *---------------------------------------------------------------------*/

/*
 * The function that reads data from the supplier can be told to wait
 * for a complete record to be received, or to read as much as possible
 * without blocking, then return with an indication that a future call
 * will be necessary to retrieve the remaining data. The following
 * values are returned by said function to indicate the state of the
 * read operation.
 */
typedef enum {     /* Frame-reader read status */
  IMS_READ_ENDED,   /* The end of the stream has been reached */
  IMS_READ_AGAIN,   /* The read operation is incomplete (dowait=0) */
  IMS_READ_DONE     /* A new image has been read */
} ImsReadState;

/*
 * When sending a control message to the supplier, one first submits
 * the message to be packed for transmission by calling the
 * appropriate submittal function, then calls ims_send_msg() to
 * transmit the message. When calling ims_send_msg() one can ask to have
 * the message sent in its entirety before returning, or to have as much
 * as possible sent without blocking. In the latter case, completion
 * of the send operation must be completed by subsequent calls to
 * ims_send_msg().
 *
 * Note that while a send operation is incomplete the values submitted
 * via intervening calls to ims_queue_regset() and/or ims_queue_interval()
 * will be queued for re-submision when the current send
 * completes. ims_send_msg() will not return IMS_SEND_DONE until all
 * such transactions have been completed.
 *
 * The following values are returned by ims_send_msg() to indicate the
 * state of the send operation.
 */
typedef enum {     /* Frame-reader write status */
  IMS_SEND_ERROR,   /* An unrecoverable error occurred */
  IMS_SEND_AGAIN,   /* The send operation is incomplete (dowait=0) */
  IMS_SEND_DONE     /* The message has been sent */
} ImsSendState;

/*
 * Incrementally read the next frame grabber image from the established 
 * data supplier. If dowait==0, retrieval of each
 * image may require multiple calls to this function. In such cases
 * select() can be used to wait for the arrival of more data. See
 * ims_select_fd() below for details.
 */
ImsReadState ims_read_image(ImMonitorStream *ims, int dowait);

/*
 * Incrementally send a message, previously submitted via a
 * call to ims_queue_interval(), ims_queue_regset() or ims_queue_rewind,
 * to the supplier. Multiple calls to this function may be required if
 * dowait==0. In such cases select() can be used to wait for the
 * output channel to free up. See ims_select_fd() for details.
 */
ImsSendState ims_send_msg(ImMonitorStream *ims, int dowait);

/*
 * Return a file descriptor that can be used with select() to
 * determine when more data is available, or when the output
 * channel to the supplier is ready to accept more data. If the
 * stream isn't connected, -1 will be returned.
 *
 * WARNING: This file descriptor may change after any call to
 *          ims_read_image(), so be sure to call ims_select_fd()
 *          before every call to select().
 */
int ims_select_fd(ImMonitorStream *ims);

/*-----------------------------------------------------------------------
 * The rest of this file regards implementation of new stream sources.
 *
 * The specific implementation of a particular stream source is
 * implemented through the following method functions, plus an
 * anonymous implementation object allocated by the stream source.
 *---------------------------------------------------------------------*/

/*
 * To create an unconnected monitor stream call new_ImMonitorStream().
 */
ImMonitorStream *new_ImMonitorStream(void);


/*.......................................................................
 * The following method is called upon to read all or part of the
 * next frame of registers. It should behave like its generic
 * counterpart described above.
 */
#define IMS_READ_IMAGE(fn) ImsReadState (fn)(ImMonitorStream *ims, int dowait)

/*.......................................................................
 * Where provided, the following optional method is called upon to
 * send part or all of the most recently packed output message.
 */
#define IMS_SEND_MSG(fn) ImsSendState (fn)(ImMonitorStream *ims, int dowait)

/*.......................................................................
 * The following method returns a file descriptor for use in select() to
 * see when the stream is ready for reading or writing. Note that the
 * select() user is expected to call this before each use of select().
 * This allows the fd to change with time (for example, the file
 * monitor will read across file boundaries, so the fd may well change).
 */
#define IMS_SELECT_FD(fn) int (fn)(ImMonitorStream *ims)
 
/*.......................................................................
 * The following method is called to delete the stream-specific
 * implementation context.
 */
#define IMS_DESTRUCTOR(fn) void *(fn)(void *context)

/*.......................................................................
 * The following function is used to connect a given stream source to
 * a monitor stream object. It requires the register map read from the
 * supplier, a stream-specific context object + its destructor, and
 * the above method functions. It returns non-zero if the call fails.
 */
int open_ImMonitorStream(ImMonitorStream *ims, void *context, 
			 IMS_DESTRUCTOR(*del_fn),
			 IMS_READ_IMAGE(*read_fn), IMS_SEND_MSG(*send_fn),
			 IMS_SELECT_FD(*fd_fn));
/*
 * The above method functions can retrieve the context object that
 * was registered with open_ImMonitorStream(), by calling the following
 * function.
 */
void *ims_SourceContext(ImMonitorStream *ims);

/*
 * The above method functions can retrieve the image buffer, by calling the 
 * following function.
 */
unsigned short *ims_get_image(ImMonitorStream *ims);

/*
 * The following function is used to close a ImMonitorStream. This
 * is done for you by del_ImMonitorStream(). Also open_ImMonitorStream()
 * calls this function before connecting a stream to a new supplier.
 * Note that close_ImMonitorStream() is idempotent.
 */
void close_ImMonitorStream(ImMonitorStream *ims);


#endif
