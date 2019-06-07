#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>

/* Include files for network transmission of objects */

#include <sys/types.h>
#include <netinet/in.h>
#include <unistd.h>
#include <errno.h>

#include "netbuf.h"
#include "lprintf.h"

#include "carma/szautil/Debug.h"

using namespace sza::array;
using namespace sza::util;

static int nrs_state(NetReadStr *nrs, int state);
static int nss_state(NetSendStr *nss, int state);

/*.......................................................................
 * Allocate and initialize a new network buffer.
 *
 * Input:
 *  size     long   The required buffer length in bytes.
 *                  Alternatively, if an external buffer is to be
 *                  supplied via net_set_buffer() then 'size' should be
 *                  zero.
 * Output:
 *  return NetBuf * The new network I/O buffer, or NULL on error.
 */
NetBuf *new_NetBuf(long size)
{
  NetBuf *net=0;
/*
 * Allocate the network buffer container.
 */
  net = (NetBuf *) malloc(sizeof(NetBuf));
  if(!net) {
    lprintf(stderr, "new_NetBuf: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before performing any operation that might fail, initialize the
 * container at least up to the point at which it is safe to pass it
 * to del_NetBuf().
 */
  net->buf = NULL;
  net->size = 0;
  net->nget = 0;
  net->nput = 0;
  net->external = 0;
/*
 * If a fixed I/O buffer is required, allocate it with a size sufficient
 * to contain a message byte-count, a message-type opcode and the message
 * itself.
 */
  if(size > 0) {
    net->size = NET_PREFIX_LEN + size;
    net->buf = (unsigned char *) malloc(net->size);
    if(!net->buf) {
      lprintf(stderr, "new_NetBuf: Insufficient memory.\n");
      return del_NetBuf(net);
    };
  };
/*
 * Return the initialized container for use.
 */
  return net;
}

/*.......................................................................
 * Delete a network buffer.
 *
 * Input:
 *  net     NetBuf *  The buffer to be deleted.
 * Output:
 *  return  NetBuf *  The deleted buffer (always NULL).
 */
NetBuf *del_NetBuf(NetBuf *net)
{
  if(net) {
    if(!net->external && net->buf)
      free(net->buf);
    free(net);
  };
  return NULL;
}

/*.......................................................................
 * Assign or resize the buffer used by a given NetBuf object.
 *
 * This function can be called multiple times. Note that existing data
 * will not be copied from the previous buffer to the new one, even if
 * the old one is just being enlarged.
 *
 * The previous buffer will be discarded before attempting to assign
 * a new one. If the previous buffer was dynamically allocated this
 * will include presenting it to free().
 *
 * To dettach or free the previous buffer without assigning a new one,
 * pass buf=NULL and length=0. Note that after this has been done the
 * NetBuf object will not be usable again until a subsequent call to
 * this function establishes a valid buffer.
 *
 * If an external buffer is supplied, its allocated storage duration
 * must exceed the lifetime of its use by the NetBuf object. If you
 * need to free the external buffer, detach it from the NetBuf object
 * first.
 *
 * Usage summary:
 *   To assign an external buffer for subsequent use by a netbuf object:
 *     buf    = char external_buffer[dim]
 *     length = dim
 *  
 *   To change the size of the buffer by allocating a new one:
 *     buf    = NULL
 *     length = new_size
 *  
 *   To detach a previous external buffer, or simply to free a previous
 *   dynamically allocated buffer:
 *     buf    = NULL
 *     length = 0
 *
 * Input:
 *  net     NetBuf *  The target network buffer.
 *  buf       void *  An external buffer, or NULL to request that a
 *                    buffer of size 'length' be dynamically allocated.
 *  length    long    The buffer size in bytes. Note that this must
 *                    include room for NET_PREFIX_LEN bytes at the
 *                    start of the buffer for the message header.
 * Output:
 *  return    void *  A pointer to the new buffer, or NULL on error.
 */
void *net_set_buffer(NetBuf *net, void *buf, size_t length)
{
/*
 * We can't discard the old buffer without having a handle to its
 * netbuf.
 */
  if(!net) {
    lprintf(stderr, "new_set_buffer: NULL NetBuf.\n");
    return NULL;
  };
/*
 * Before further checking the function arguments, discard the
 * existing buffer. This allows us to guarantee that the old
 * buffer is discarded regardless of errors.
 */
  if(!net->external && net->buf)
    free(net->buf);
  net->buf = NULL;
  net->size = 0;
  net->nget = 0;
  net->nput = 0;
  net->external = 0;
/*
 * Do we need to assign a new buffer?
 */
  if(length > 0) {
    if(length < NET_PREFIX_LEN) {
      lprintf(stderr, "net_set_buffer: Buffer too small.\n");
      return NULL;
    };
/*
 * Allocate or assign the new buffer.
 */
    net->buf = (unsigned char* )(buf ? buf : malloc(length));
    if(!net->buf) {
      lprintf(stderr, "net_set_buffer: Insufficient memory.\n");
      return NULL;
    };
    net->external = buf != NULL;
    net->size = length;
  };
  return net->buf;
}

/*.......................................................................
 * If an external buffer has been supplied allow the caller to increment
 * the output buffer pointer to account for externally written data in
 * the buffer.
 *
 * Input:
 *  net     NetBuf *   A network buffer containing an external buffer.
 *  nbytes    long     The number of bytes written to the buffer.
 * Output:
 *  return    long     The index of the next byte after the current
 *                     end of message, or -1 on error.
 */
long net_inc_nput(NetBuf *net, long nbytes)
{
/*
 * Disallow this call for non-external buffers.
 */
  if(!net->external) {
    lprintf(stderr, "net_inc_nput: Not applicable for internal buffers.\n");
    return -1;
  };
/*
 * Is there sufficient room in the network buffer?
 */
  if(net->nput + nbytes > net->size) {
    lprintf(stderr, "net_inc_nput: Network buffer too small.\n");
    return -1;
  };
  net->nput += nbytes;
  return net->nput;
}

/*.......................................................................
 * Allocate and initialize a new network input stream buffer.
 *
 * Input:
 *  fd            int   The reabable stream file descriptor to associate
 *                      with the buffer, or -1 if this is to be supplied
 *                      later via attach_NetReadStr().
 *  size         long   The buffer length in bytes.
 * Output:
 *  return NetReadStr * The new stream input buffer, or NULL on error.
 */
NetReadStr *new_NetReadStr(int fd, long size)
{
  NetReadStr *nrs;
/*
 * Allocate the network buffer container.
 */
  nrs = (NetReadStr *) malloc(sizeof(NetReadStr));
  if(!nrs) {
    lprintf(stderr, "new_NetReadStr: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before performing any operation that might fail, initialize the
 * container at least up to the point at which it is safe to pass it
 * to del_NetReadStr().
 */
  nrs->net = NULL;
  nrs->msglen = 0;
  nrs->fd = fd;
#ifdef _GPP
  nrs->state = fd >= 0 ? NetReadStr::NET_READ_DONE : NetReadStr::NET_READ_CLOSED;
#else
  nrs->state = fd >= 0 ? NET_READ_DONE : NET_READ_CLOSED;
#endif
/*
 * Allocate the network buffer.
 */
  nrs->net = new_NetBuf(size);
  if(!nrs->net)
    return del_NetReadStr(nrs);
/*
 * Return the initialized container for use.
 */
  return nrs;
}

/*.......................................................................
 * Delete a network read buffer.
 *
 * Input:
 *  nrs     NetReadStr *  The buffer to be deleted.
 * Output:
 *  return  NetReadStr *  The deleted buffer (always NULL).
 */
NetReadStr *del_NetReadStr(NetReadStr *nrs)
{
  if(nrs) {
    nrs->net = del_NetBuf(nrs->net);
    free(nrs);
  };
  return NULL;
}

/*.......................................................................
 * Attach a readable stream file-descriptor to a network-read-stream
 * iterator. Any previously attached fd will be displaced.
 *
 * Input:
 *  nrs     NetReadStr *  The iterator to attach the stream to.
 *  fd             int    The file-descriptor to be read from, or
 *                        -1 to detach an existing fd.
 */
void attach_NetReadStr(NetReadStr *nrs, int fd)
{
  if(nrs) {
    nrs->fd = fd;
#ifdef _GPP
    nrs->state = fd >= 0 ? NetReadStr::NET_READ_DONE : NetReadStr::NET_READ_CLOSED;
#else
    nrs->state = fd >= 0 ? NET_READ_DONE : NET_READ_CLOSED;
#endif
  };
}

/*.......................................................................
 * Allocate and initialize a new network send buffer for writing messages
 * from a given file-descriptor.
 *
 * Input:
 *  fd            int   The writeable stream file descriptor to associate
 *                      with the stream, or -1 if a file-descriptor is
 *                      be attached later attach_NetSendStr().
 *  size         long   The buffer length in bytes.
 * Output:
 *  return NetSendStr * The new network I/O buffer, or NULL on error.
 */
NetSendStr *new_NetSendStr(int fd, long size)
{
  NetSendStr *nss;
/*
 * Allocate the network buffer container.
 */
  nss = (NetSendStr *) malloc(sizeof(NetSendStr));
  if(!nss) {
    lprintf(stderr, "new_NetSendStr: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before performing any operation that might fail, initialize the
 * container at least up to the point at which it is safe to pass it
 * to del_NetSendStr().
 */
  nss->net = NULL;
  nss->fd = fd;
#ifdef _GPP
  nss->state = fd >= 0 ? NetSendStr::NET_SEND_DONE : NetSendStr::NET_SEND_CLOSED;
#else
  nss->state = fd >= 0 ? NET_SEND_DONE : NET_SEND_CLOSED;
#endif
/*
 * Allocate the network buffer.
 */
  nss->net = new_NetBuf(size);
  if(!nss->net)
    return del_NetSendStr(nss);
/*
 * Return the initialized container for use.
 */
  return nss;
}

/*.......................................................................
 * Delete a network send buffer.
 *
 * Input:
 *  nss     NetSendStr *  The buffer to be deleted.
 * Output:
 *  return  NetSendStr *  The deleted buffer (always NULL).
 */
NetSendStr *del_NetSendStr(NetSendStr *nss)
{
  if(nss) {
    nss->net = del_NetBuf(nss->net);
    free(nss);
  };
  return NULL;
}

/*.......................................................................
 * Attach a writeable stream file-descriptor to a network-send-stream
 * iterator. Any previously attached fd will be displaced.
 *
 * Input:
 *  nss     NetSendStr *  The iterator to attach the stream to.
 *  fd             int    The file-descriptor to be written to, or
 *                        -1 to detach an existing fd.
 */
void attach_NetSendStr(NetSendStr *nss, int fd)
{
  if(nss) {
    nss->fd = fd;
#ifdef _GPP
    nss->state = fd >= 0 ? NetSendStr::NET_SEND_DONE : NetSendStr::NET_SEND_CLOSED;
#else
    nss->state = fd >= 0 ? NET_SEND_DONE : NET_SEND_CLOSED;
#endif
  };
}

/*.......................................................................
 * Read a network message.
 *
 * Input:
 *  nrs  NetReadStr * The network buffer container to read into.
 *                    On output nrs->net->nput will contain a count of
 *                    the number of characters copied into nrs->net->buf[].
 * Output:
 *  return      int   The current state of the read operation, from:
 *
 *                     NET_READ_SIZE   - The byte-count prefix of the
 *                                       message has not been completely
 *                                       read yet.
 *                     NET_READ_DATA   - The body of the message has not
 *                                       been completely read yet.
 *                     NET_READ_DONE   - Message completely read and ready
 *                                       for processing via net_get_*().
 *                     NET_READ_CLOSED - The connection was found to be
 *                                       closed before the first byte of
 *                                       a message had been read.
 *                     NET_READ_ERROR  - Read error - no further processing
 *                                       is possible.
 *                    Note that the above have numeric values that increment
 *                    in the order shown. Note also that unless the
 *                    fd is open for non-blocking I/O, NET_READ_SIZE and
 *                    NET_READ_DATA will never be returned. For non-blocking
 *                    I/O nrs_read_msg() may need to be called many times
 *                    before NET_READ_DONE is returned.
 */
int nrs_read_msg(NetReadStr *nrs)
{
  NetBuf *net;
/*
 * Get a pointer to the network I/O buffer.
 */
  net = nrs->net;
/*
 * Determine how to respond from the I/O status last recorded.
 */
  switch(nrs->state) {
  case NetReadStr::NET_READ_SIZE:             /* Continue reading an incompletely */
  case NetReadStr::NET_READ_DATA:             /* read message */
    break;
  case NetReadStr::NET_READ_DONE: /* Prepare to read the byte-count prefix */
    nrs->state = NetReadStr::NET_READ_SIZE;   /* of a new message */
    nrs->msglen = 4;
    net->nput = 0;
    net->nget = 0;
    if(!net->buf) {
      lprintf(stderr, "nrs_read_msg: Buffer not provided.\n");
      return nrs_state(nrs, NetReadStr::NET_READ_ERROR);
    };
    break;
  case NetReadStr::NET_READ_CLOSED:           /* Don't report errors more than once */
  case NetReadStr::NET_READ_ERROR:
    return nrs->state;
    break;
  };
/*
 * Read as many bytes of the message as possible.
 */
  while(net->nput < nrs->msglen) {
    int nread;  /* The latest number of bytes returned by read() */
/*
 * Read as many bytes as possible up to the required size.
 */
    errno = 0;
    nread = read(nrs->fd, net->buf + net->nput, nrs->msglen - net->nput);

    DBPRINT(false, Debug::DEBUG3, "read " << nread << " bytes");

    // Did we manage to read anything?

    if(nread > 0) {
      net->nput += nread;
      
      // If we just completed reading the message-size prefix then
      // substitute the real value of the target message size for the
      // previous dummy size.

      if(net->nput >= nrs->msglen && nrs->state==NetReadStr::NET_READ_SIZE) {
	nrs->state = NetReadStr::NET_READ_DATA;

	if(net_get_long(net, 1, (unsigned long int* )&nrs->msglen) ||
	   nrs->msglen < NET_PREFIX_LEN) {

	  lprintf(stderr, "nrs_read_msg: Bad message length: nput = %d msglen = %d\n", net->nput, nrs->msglen);
	  return nrs_state(nrs, NetReadStr::NET_READ_ERROR);
	}

	// Check if the message length is larger than the network
	// buffer size.  
	//
	// I'm changing this 3 Aug 2010 so that this is not
	// necessarily an error; ie, it allows for variable-sized
	// network objects.
	//
	// Now we attempt to resize the network buffer, if it was NOT
	// externally allocated. (If it was externally allocated, we
	// don't want to change the size under the hood of objects
	// allocated further upstream.)

	if(nrs->msglen > net->size) {

	  if(!net->external) {

	    net->size = NET_PREFIX_LEN + nrs->msglen;
	    net->buf = (unsigned char *) realloc(net->buf, net->size);

	    if(net->buf == 0) {
	      lprintf(stderr, "new_NetBuf: Insufficient memory.\n");
	      return nrs_state(nrs, NetReadStr::NET_READ_ERROR);
	    }

	  } else {
	    lprintf(stderr, "nrs_read_msg: Bad message length\n");
	    return nrs_state(nrs, NetReadStr::NET_READ_ERROR);
	  }
	};

      };
    } else {
/*
 * Check for errors.
 */
      switch(errno) {
/*
 * Connection closed?
 */
      case 0:
	if(net->nput == 0) {
#ifdef _GPP
	  return nrs_state(nrs, NetReadStr::NET_READ_CLOSED);
#else
	  return nrs_state(nrs, NET_READ_CLOSED);
#endif
	} else {
	  lprintf(stderr, "nrs_read_msg: End of stream reached prematurely.\n");
#ifdef _GPP
	  return nrs_state(nrs, NetReadStr::NET_READ_ERROR);
#else
	  return nrs_state(nrs, NET_READ_ERROR);
#endif
	};
	break;
/*
 * No data currently available for non-blocking I/O?
 */
#ifdef EWOULDBLOCK
      case EWOULDBLOCK:     /* BSD non-blocking I/O */
	return nrs->state;
#endif
#if defined(EAGAIN) && !(defined(EWOULDBLOCK) && EWOULDBLOCK==EAGAIN)
      case EAGAIN:          /* System V non-blocking I/O */
	return nrs->state;
#endif
/*
 * Continue after interrupts.
 */
      case EINTR:
	break;
/*
 * Report unexpected I/O errors.
 */
      default:
	fprintf(stderr, "nrs_read_msg I/O error (%s)\n", strerror(errno));
#ifdef _GPP
	return nrs_state(nrs, NetReadStr::NET_READ_ERROR);
#else
	return nrs_state(nrs, NET_READ_ERROR);
#endif
      };
    };
  };
#ifdef _GPP
  return nrs_state(nrs, NetReadStr::NET_READ_DONE);
#else
  return nrs_state(nrs, NET_READ_DONE);
#endif
}

/*.......................................................................
 * Private return function of nrs_read_msg().
 *
 * Input:
 *  nrs  NetReadStr *  The read message buffer.
 *  state       int    The state to both assign to nrs->state and
 *                     return.
 * Output:
 *  return      int    The value of 'state'.
 */
static int nrs_state(NetReadStr *nrs, int state)
{
#ifdef _GPP
  nrs->state = (NetReadStr::NetReadId)state;
#else
  nrs->state = state;
#endif
  return state;
}

/*.......................................................................
 * Write a previously composed network message.
 *
 * Input:
 *  nss  NetSendStr * The network send buffer to write from.
 * Output:
 *  return      int   The current state of the send operation, from:
 *                     NET_SEND_DATA   -  A message is in the process of
 *                                        being written.
 *                     NET_SEND_DONE   -  The last message has been
 *                                        completely transfered.
 *                     NET_SEND_ERROR  -  Write I/O error detected.
 *                     NET_SEND_CLOSED -  No open file-descriptor has
 *                                        been specified yet.
 *
 *                    Note that NET_SEND_DATA will only ever be
 *                    returned if nss->fd is registered for non-blocking
 *                    I/O.
 */
int nss_send_msg(NetSendStr *nss)
{
  NetBuf *net;
/*
 * Get a pointer to the encapsulated network I/O buffer.
 */
  net = nss->net;
/*
 * Determine how to respond from the I/O status last recorded.
 */
  switch(nss->state) {
#ifdef _GPP
  case NetSendStr::NET_SEND_DATA:             /* Continue sending an incompletely */
#else
  case NET_SEND_DATA:             /* Continue sending an incompletely */
#endif
    break;                        /* sent message */
#ifdef _GPP
  case NetSendStr::NET_SEND_DONE:             /* Send a new message */
#else
  case NET_SEND_DONE:             /* Send a new message */
#endif
#ifdef _GPP
    nss->state = NetSendStr::NET_SEND_DATA;
#else
    nss->state = NET_SEND_DATA;
#endif
    net->nget = 0;
/*
 * Check the message length encompasses at least the byte-count and
 * opcode.
 */
    if(net->nput < NET_PREFIX_LEN) {
      lprintf(stderr, "nss_send_msg: Message too short.\n");
#ifdef _GPP
      return nss_state(nss, NetSendStr::NET_SEND_ERROR);
#else
      return nss_state(nss, NET_SEND_ERROR);
#endif
    } else if(net->nput > net->size) {
      lprintf(stderr, "nss_send_msg: Message longer than buffer!\n");
#ifdef _GPP
      return nss_state(nss, NetSendStr::NET_SEND_ERROR);
#else
      return nss_state(nss, NET_SEND_ERROR);
#endif
    };
    break;
#ifdef _GPP
  case NetSendStr::NET_SEND_CLOSED:           /* Don't report errors more than once */
#else
  case NET_SEND_CLOSED:           /* Don't report errors more than once */
#endif
#ifdef _GPP
  case NetSendStr::NET_SEND_ERROR:
#else
  case NET_SEND_ERROR:
#endif
    return nss->state;
    break;
  };
/*
 * Write as many bytes of the message as possible.
 */
  while(net->nget < net->nput) {
    int nnew;    /* The lastest number of bytes sent by write() */
    errno = 0;
    nnew = write(nss->fd, net->buf + net->nget, net->nput - net->nget);

    DBPRINT(false, Debug::DEBUG3, "sent " << nnew << " bytes");
/*
 * Did we manage to send anything?
 */
    if(nnew > 0) {
      net->nget += nnew;
    } else {
/*
 * Handle write errors.
 */
      switch(errno) {
/*
 * No error reported in errno even though no bytes were transfered.
 */
      case 0:
	lprintf(stderr,
	  "nss_send_msg: write() returned %d without an explanation.\n", nnew);
#ifdef _GPP
	return nss_state(nss, NetSendStr::NET_SEND_ERROR);
#else
	return nss_state(nss, NET_SEND_ERROR);
#endif
	break;
/*
 * Insufficient room currently available for non-blocking I/O?
 */
#ifdef EWOULDBLOCK
      case EWOULDBLOCK:     /* BSD non-blocking I/O */
	return nss->state;
#endif
#if defined(EAGAIN) && !(defined(EWOULDBLOCK) && EWOULDBLOCK==EAGAIN)
      case EAGAIN:          /* System V non-blocking I/O */
	return nss->state;
#endif
/*
 * Continue after interrupts.
 */
      case EINTR:
	break;
/*
 * Report unexpected I/O errors.
 */
      default:
	lprintf(stderr, "nss_send_msg I/O error (%s)\n", strerror(errno));
#ifdef _GPP
	return nss_state(nss, NetSendStr::NET_SEND_ERROR);
#else
	return nss_state(nss, NET_SEND_ERROR);
#endif
      };
    };
  };
/*
 * The message has been successfully sent.
 */
#ifdef _GPP
  return nss_state(nss, NetSendStr::NET_SEND_DONE);
#else
  return nss_state(nss, NET_SEND_DONE);
#endif
}

/*.......................................................................
 * Private return function of nss_send_msg().
 *
 * Input:
 *  nss  NetReadStr *  The send message buffer.
 *  state       int    The state to both assign to nss->state and
 *                     return.
 * Output:
 *  return      int    The value of 'state'.
 */
static int nss_state(NetSendStr *nss, int state)
{
#ifdef _GPP
  nss->state = (NetSendStr::NetSendId)state;
#else
  nss->state = state;
#endif
  return state;
}

/*.......................................................................
 * Start the extraction of a message from a network buffer.
 * This involves skipping the byte-count and opcode message prefix
 * and returning the message-type identification code.
 *
 * Input:
 *  net     NetBuf *  The network buffer to extract from.
 * Input/Output:
 *  opcode     int *  On output *opcode will be initialized with the
 *                    message-type identification code erad from the
 *                    start of the message.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
int net_start_get(NetBuf *net, int *opcode)
{
/*
 * Check arguments.
 */
  if(!net || !opcode) {
    lprintf(stderr, "net_start_get: Invalid %s argument.\n",
	    net ? "opcode":"net");
    return 1;
  };
/*
 * Message too short?
 */
  if(net->nput < NET_PREFIX_LEN) {
    lprintf(stderr, "net_start_get: Message header missing.\n");
    return 1;
  };
/*
 * Position the extraction pointer at the start of the opcode.
 */
  net->nget = 4;
/*
 * Read the opcode.
 */
  {
    unsigned long ul_opcode;
    if(net_get_long(net, 1, &ul_opcode))
      return 1;
    *opcode = ul_opcode;
  };
  return 0;
}

/*.......................................................................
 * Complete the extraction of a message from a network buffer by checking
 * whether any unexpected bytes remain to be read.
 *
 * Input:
 *  net    NetBuf *  The network buffer to complete extraction from.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
int net_end_get(NetBuf *net)
{
  if(net->nget < net->nput) {
    lprintf(stderr, "net_end_get: Unexpected bytes at end of message.\n");
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Start the composition of a new network message.
 *
 * On return a dummy byte-count [to be later overwritten by net_end_put()]
 * and the message-type identification code will have been inserted at
 * the start of the buffer. The next net_put_*() function used will
 * append bytes after this prefix.
 *
 * Input:
 *  net     NetBuf *  The network buffer to initialize.
 *  opcode     int    The enumerated message-type.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
int net_start_put(NetBuf *net, int opcode)
{
  unsigned long ul_opcode;  /* The opcode in network form */
/*
 * Check arguments.
 */
  if(!net || opcode < 0) {
    lprintf(stderr, "net_start_put: Invalid %s argument.\n",
	    net ? "opcode":"net");
    return 1;
  };
/*
 * Make sure that an I/O buffer has been provided.
 */
  if(!net->buf) {
    lprintf(stderr, "new_start_put: No buffer.\n");
    return 1;
  };
/*
 * Initialize the composed message length byte-counter and the
 * buffer extraction and composition pointers.
 */
  net->nget = 0;
  net->nput = 0;
/*
 * Insert a dummy zero valued byte-count at the start of the container.
 * This will be re-written when the message is finished by net_end_msg().
 */
  if(net_put_long(net, 1, (long unsigned int* )&net->nput))
    return 1;
/*
 * Write the message-type code.
 */
  ul_opcode = opcode;
  return net_put_long(net, 1, &ul_opcode);
}

/*.......................................................................
 * Put the finishing touches to the composition of network message by
 * overwriting the dummy byte-count prepended by net_start_put(),
 * with the final byte count achieved.
 *
 * Input:
 *  net    NetBuf *  The network buffer to initialize.
 * Output:
 *  return    int    0 - OK.
 *                   1 - Error.
 */
int net_end_put(NetBuf *net)
{
  unsigned long msglen = net->nput; /* Record the current message length */
/*
 * Rewind the buffer pointer, write the byte-count and then
 * re-instate the original buffer pointer.
 */
  net->nput = 0;
  if(net_put_long(net, 1, &msglen))
    return 1;
  net->nput = msglen;
  return 0;
}

/*.......................................................................
 * Extract one or more 8-bit char elements from a network buffer.
 *
 * Input:
 *  net         NetBuf * The network buffer to extract char's from.
 *  ndata         long   The number of elements to be read.
 *  data unsigned char * A pointer to an output array of at least ndata
 *                       elements.
 * Output:
 *  return         int   0 - OK.
 *                       1 - Error.
 */
int net_get_char(NetBuf *net, long ndata, unsigned char *data)
{
/*
 * Do we have sufficient data in the buffer to satisfy the request?
 */
  if(net->nget + ndata > net->nput) {
    lprintf(stderr, "net_get_char: Insufficient data in buffer.\n");
    return 1;
  };
/*
 * Copy the buffered data directly into the output array.
 */
  memcpy(data, net->buf + net->nget, ndata);
  net->nget += ndata;
  return 0;
}

/*.......................................................................
 * Extract one or more short integer elements from a network buffer.
 *
 * Input:
 *  net          NetBuf * The network buffer to read from.
 *  ndata          long   The number of elements to be read.
 *  data unsigned short * A pointer to an output array of at least ndata
 *                        elements.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
int net_get_short(NetBuf *net, long ndata, unsigned short *data)
{
  u_short net_short;
  long i;
/*
 * Do we have sufficient data in the buffer to satisfy the request?
 */
  if(net->nget + NET_SHORT_SIZE * ndata > net->nput) {
    lprintf(stderr, "net_get_short: Insufficient data in buffer.\n");
    return 1;
  };
/*
 * Copy one element at a time from the network buffer, convert it to host
 * byte order and copy the result to the output array.
 */
  for(i=0; i<ndata; i++) {
    memcpy(&net_short, net->buf + net->nget, NET_SHORT_SIZE);
    net->nget += NET_SHORT_SIZE;
    data[i] = ntohs(net_short);
  };
  return 0;
}

/*.......................................................................
 * Extract one or more int integer elements from a network buffer.
 */
int net_get_int(NetBuf *net, int ndata, unsigned int *data)
{
  unsigned int net_int;
  int i;
  /*
   * Do we have sufficient data in the buffer to satisfy the request?
   */
  if(net->nget + NET_INT_SIZE * ndata > net->nput) {
    lprintf(stderr, "net_get_int: Insufficient data in buffer.\n");
    return 1;
  };
  /*
   * Copy one element at a time from the network buffer, convert
   * it to host byte order and copy the result to the output array.
   */
  for(i=0; i<ndata; i++) {
    memcpy(&net_int, net->buf + net->nget, NET_INT_SIZE);
    net->nget += NET_INT_SIZE;

#ifdef DEBUG_NETBUF
    if(output_ints)
      fprintf(stdout,"%d\n",net_int);
#endif

    data[i] = ntohl(net_int);
  };
  return 0;
}


/*.......................................................................
 * Extract one or more long integer elements from a network buffer.
 *
 * Input:
 *  net         NetBuf * The network buffer to read from.
 *  ndata         long   The number of elements to be read.
 *  data unsigned long * A pointer to an output array of at least ndata
 *                       elements.
 * Output:
 *  return         int   0 - OK.
 *                       1 - Error.
 */
int net_get_long(NetBuf *net, long ndata, unsigned long *data)
{
  u_long net_long;
  long i;
/*
 * Do we have sufficient data in the buffer to satisfy the request?
 */
  if(net->nget + NET_LONG_SIZE * ndata > net->nput) {
    lprintf(stderr, "net_get_long: Insufficient data in buffer.\n");
    return 1;
  };
/*
 * Copy one element at a time from the network buffer, convert it to host
 * byte order and copy the result to the output array.
 */
  for(i=0; i<ndata; i++) {
    memcpy(&net_long, net->buf + net->nget, NET_LONG_SIZE);
    net->nget += NET_LONG_SIZE;

#if 0
    fprintf(stdout,"%d %d\n",net_long,ntohl(net_long));
#endif

    data[i] = ntohl(net_long);
  };

  return 0;
}

/*.......................................................................
 * Extract one or more float elements from a network buffer.
 * The network buffered float must be in IEEE 754 format, except that
 * IEEE special values such as NaN and Inf are not supported.
 *
 * Input:
 *  net    NetBuf * The network buffer to read from.
 *  ndata    long   The number of elements to be read.
 *  data    float * A pointer to an output array of at least ndata elements.
 * Output:
 *  return    int   0 - OK.
 *                  1 - Error.
 */
int net_get_float(NetBuf *net, long ndata, float *data)
{
/*
 * Do we have sufficient data in the buffer to satisfy the request?
 */
  if(net->nget + NET_FLOAT_SIZE * ndata > net->nput) {
    lprintf(stderr, "net_get_float: Insufficient data in buffer.\n");
    return 1;
  };
/*
 * For hosts that support IEEE-754 floats simply copy from the network
 * buffer to the output array.
 */
#if CPU==MC68040 || CPU==MC68060 || defined sparc
  memcpy(data, net->buf + net->nget, NET_FLOAT_SIZE * ndata);
  net->nget += NET_FLOAT_SIZE * ndata;
/*
 * For other architectures convert from IEEE-754 to the host type.
 * The following algorithm should work on any architecture, albeit
 * probably not very efficiently.
 *
 * An IEEE 754 number can be decoded as follows, where [a..b] means
 * an integer formed from bits a to b, with bit 1 being the lsb.
 *
 * value = pow(-1,[32]) * pow(2,[31..24]-127) * [23..1]/pow(2,24)
 */
#else
  {
    int i;
    for(i=0; i<ndata; i++) {
      u_long net_long;  /* The float packed in a 32-bit network long */
      float host_float; /* A float converted to host format */
      int sign_bit;     /* Non-zero if the number is negative */
      unsigned iexp;    /* The power-of-two exponent */
      double mantissa;  /* The mantissa of the number */
/*
 * Extract the IEEE-754 float into a local 32-bit network long.
 */
      memcpy(&net_long, net->buf + net->nget, NET_FLOAT_SIZE);
      net->nget += NET_FLOAT_SIZE;
/*
 * Convert to host byte-order.
 */
      net_long = ntohl(net_long);
/*
 * Get the sign bit (bit 32).
 */
      sign_bit = net_long>>31U & 1U;
/*
 * Get the exponent (bits 31-24).
 */
      iexp = (net_long >> 23U & 0xffU) - 126U;
/*
 * Get the mantissa (bits 23-1).
 */
      mantissa = net_long & 0x7fffffU;
/*
 * Compute the host-specific float.
 */
      data[i] = ldexp(mantissa / 0x1000000 + 0.5, iexp) * (sign_bit ? -1 : 1);
    };
  };
#endif
  return 0;
}

/*.......................................................................
 * Extract one or more double elements from a network buffer.
 * The network buffered double must be in IEEE 754 format, except that
 * IEEE special values such as NaN and Inf are not supported.
 *
 * Input:
 *  net    NetBuf * The network buffer to read from.
 *  ndata    long   The number of elements to be read.
 *  data   double * A pointer to an output array of at least ndata elements.
 * Output:
 *  return    int   0 - OK.
 *                  1 - Error.
 */
int net_get_double(NetBuf *net, long ndata, double *data)
{
/*
 * Do we have sufficient data in the buffer to satisfy the request?
 */
  if(net->nget + NET_DOUBLE_SIZE * ndata > net->nput) {
    lprintf(stderr, "net_get_double: Insufficient data in buffer.\n");
    return 1;
  };
/*
 * For hosts that support big-endian IEEE-754 64-bit doubles, simply
 * copy the input data into the output buffer.
 */
#if CPU==MC68040 || CPU==MC68060 || defined sparc
  memcpy(data, net->buf + net->nget, NET_DOUBLE_SIZE * ndata);
  net->nget += NET_DOUBLE_SIZE * ndata;
#else
/*
 * For other architectures convert from IEEE-754 to the host type.
 * The following algorithm should work on any architecture, albeit
 * slowly.
 *
 * An IEEE 754 64-bit double can be decoded as follows, where [a..b]
 * means an integer formed from bits a to b, with bit 1 being the lsb.
 *
 * value = pow(-1,[64]) * pow(2,[63..53]-1023) * [52..1]/pow(2,53)
 *
 */
  {
    int i;
    for(i=0; i<ndata; i++) {
      unsigned char *bytes = net->buf + net->nget;
/*
 * Get the sign bit.
 */
      int sign_bit = bytes[0]>>7U & 1U;
/*
 * Get the exponent (bits 63-53).
 */
      unsigned iexp = (bytes[0] & 0x7FU) << 4U | bytes[1] >> 4U;
/*
 * Get the mantissa (bits 52-1).
 */
      double mantissa = ((bytes[1] & 0xFU) << 24U | bytes[2] << 16U |
			 bytes[3] << 8U | bytes[4]) * (double) (1U<<24U) +
			   (bytes[5] << 16U | bytes[6] << 8U | bytes[7]);
/*
 * Compose the host-format double in the output array.
 */
      data[i] = ldexp(mantissa/ldexp(1,53)+0.5, iexp-1022U) * (sign_bit ? -1:1);
/*
 * Step to the next packed double in the input buffer.
 */
      net->nget += NET_DOUBLE_SIZE;
    };
  };
#endif
  return 0;
}

/*.......................................................................
 * Read and discard a given number of bytes from a netbuf.
 *
 * Input:
 *  net    NetBuf *   The network buffer to read from.
 *  nbytes   long     The number of bytes to read.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
int net_inc_nget(NetBuf *net, long nbytes)
{
/*
 * Do we have sufficient data in the buffer to satisfy the request?
 */
  if(net->nget + nbytes > net->nput) {
    lprintf(stderr, "net_inc_nget: Insufficient data in buffer.\n");
    return 1;
  };
  net->nget += nbytes;
  return 0;
}

/*.......................................................................
 * Write one or more char elements to a network buffer.
 *
 * Input:
 *  net         NetBuf * The network buffer to write to.
 *  ndata         long   The number of elements to be written.
 *  data unsigned char * The array of 'ndata' elements to be copied into
 *                       the buffer.
 * Output:
 *  return         int   0 - OK.
 *                       1 - Error.
 */
int net_put_char(NetBuf *net, long ndata, unsigned char *data)
{
/*
 * Is there sufficient room in the network buffer?
 */
  if(net->nput + ndata > net->size) {
    lprintf(stderr, "net_put_char: Network buffer too small.\n");
    return 1;
  };
/*
 * Copy the data array directly to the network buffer.
 */
  memcpy(net->buf + net->nput, data, ndata);
  net->nput += ndata;
  return 0;
}

/*.......................................................................
 * Write one or more short integer elements to a network buffer.
 *
 * Input:
 *  net          NetBuf * The network buffer to write to.
 *  ndata          long   The number of elements to be written.
 *  data unsigned short * The array of 'ndata' elements to be copied into the
 *                        buffer.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
int net_put_short(NetBuf *net, long ndata, unsigned short *data)
{
  u_short net_short;
  int i;
/*
 * Is there sufficient room in the network buffer?
 */
  if(net->nput + ndata * NET_SHORT_SIZE > net->size) {
    lprintf(stderr, "net_put_short: Network buffer too small.\n");
    return 1;
  };
/*
 * Copy one element at a time from the input array, convert it to network
 * byte order and copy the result to the network buffer.
 */
  for(i=0; i<ndata; i++) {
    net_short = htons(data[i]);
    memcpy(net->buf + net->nput, &net_short, NET_SHORT_SIZE);
    net->nput += NET_SHORT_SIZE;
  };
  return 0;
}

/*.......................................................................
 * Write one or more long integer elements to a network buffer.
 *
 * Input:
 *  net         NetBuf * The network buffer to write to.
 *  ndata         long   The number of elements to be written.
 *  data unsigned long * The array of 'ndata' elements to be copied into the
 *                       buffer.
 * Output:
 *  return         int   0 - OK.
 *                       1 - Error.
 */
int net_put_long(NetBuf *net, long ndata, unsigned long *data)
{
  u_long net_long;
  int i;
/*
 * Is there sufficient room in the network buffer?
 */
  if(net->nput + ndata * NET_LONG_SIZE > net->size) {
    lprintf(stderr, "net_put_long: Network buffer too small.\n");
    return 1;
  };
/*
 * Copy one element at a time from the input array, convert it to network
 * byte order and copy the result to the network buffer.
 */
  for(i=0; i<ndata; i++) {
    net_long = htonl(data[i]);

#ifdef DEBUG_NETBUF
    if(output_longs)
      fprintf(stdout,"%d\n",net_long);
#endif

    memcpy(net->buf + net->nput, &net_long, NET_LONG_SIZE);
    net->nput += NET_LONG_SIZE;
  };
  return 0;
}

/*.......................................................................
 * Write one or more int integer elements to a network buffer.  
 */
int net_put_int(NetBuf *net, long ndata, unsigned int *data)
{
  u_int net_int;
  int i;

  /*
  * Is there sufficient room in the network buffer?
  */
  if(net->nput + ndata * NET_INT_SIZE > net->size) {
    lprintf(stderr, "net_put_int: Network buffer too small.\n");
    return 1;
  };
  /*
   * Copy one element at a time from the input array, convert it to
   * network byte order and copy the result to the network buffer.
   */
  for(i=0; i<ndata; i++) {
    net_int = htonl(data[i]);

#ifdef DEBUG_NETBUF
    if(output_ints)
      fprintf(stdout,"%d\n",net_int);
#endif

    memcpy(net->buf + net->nput, &net_int, NET_INT_SIZE);
    net->nput += NET_INT_SIZE;
  };
  return 0;
}

/*.......................................................................
 * Write one or more float integer elements to a network buffer.
 * Each element is converted to an IEEE-754 float and written in
 * network byte order.
 *
 * Input:
 *  net    NetBuf * The network buffer to write to.
 *  ndata    long   The number of elements to be written.
 *  data    float * The array of 'ndata' elements to be copied into the
 *                    buffer.
 * Output:
 *  return    int   0 - OK.
 *                  1 - Error.
 */
int net_put_float(NetBuf *net, long ndata, float *data)
{
/*
 * Is there sufficient room in the network buffer?
 */
  if(net->nput + ndata * NET_FLOAT_SIZE > net->size) {
    lprintf(stderr, "net_put_float: Network buffer too small.\n");
    return 1;
  };
/*
 * For hosts that support big-endian IEEE-754 32-bit floats
 * simply copy the input array into the output buffer.
 */
#if CPU==MC68040 || CPU==MC68060 || defined sparc
  memcpy(net->buf + net->nput, data, NET_FLOAT_SIZE * ndata);
  net->nput += NET_FLOAT_SIZE * ndata;
#else
/*
 * For other architectures convert from host float format to
 * IEEE-754. The following algorithm should work on any architecture,
 * albeit slowly.
 */
  {
    int i;
    for(i=0; i<ndata; i++) {
      u_long net_long;       /* The output float packed into a network long */
      unsigned long mantissa;    /* Normalized mantissa scaled by 0x1000000 */
      float host_float = data[i];         /* The next float to be converted */
      float abs_float = fabs(host_float); /* Avoid sign extension problems */
      long sign_bit = host_float<0;       /* Sign bit (0 or 1) */
      int iexp;                           /* Power of two exponent */
/*
 * NaN and Inf have architecture-depended representations, so transmute
 * them to FLT_MAX.
 */
      if(abs_float > FLT_MAX || !(abs_float > 0.0))
	abs_float = FLT_MAX;
/*
 * Compute the mantissa and the exponent.
 */
      mantissa = ((frexp(abs_float, &iexp)-0.5) * 0x1000000);
      iexp += 126U;
/*
 * Compose the IEEE float in the 32-bit network long.
 */
      net_long = mantissa |  iexp << 23 | sign_bit << 31;
/*
 * Convert the host-byte-order long to network byte order
 * and copy it to the network buffer.
 */
      net_long = htonl(net_long);
      memcpy(net->buf + net->nput, &net_long, NET_FLOAT_SIZE);
      net->nput += NET_FLOAT_SIZE;
    };
  };
#endif
  return 0;
}

/*.......................................................................
 * Write one or more double precision elements to a network buffer.
 * Each element is converted to an IEEE-754 float and written in
 * network byte order.
 *
 * Input:
 *  net    NetBuf * The network buffer to write to.
 *  ndata    long   The number of elements to be written.
 *  data    double * The array of 'ndata' elements to be copied into the
 *                    buffer.
 * Output:
 *  return    int   0 - OK.
 *                  1 - Error.
 */
int net_put_double(NetBuf *net, long ndata, double *data)
{
/*
 * Is there sufficient room in the network buffer?
 */
  if(net->nput + ndata * NET_DOUBLE_SIZE > net->size) {
    lprintf(stderr, "net_put_double: Network buffer too small.\n");
    return 1;
  };
/*
 * For hosts that support big-endian 64-bit IEEE-754 doubles, simply copy
 * the host-byte-order double to the output long in host byte order.
 */
#if CPU==MC68040 || CPU==MC68060 || defined sparc
  memcpy(net->buf + net->nput, data, NET_DOUBLE_SIZE * ndata);
  net->nput += NET_DOUBLE_SIZE * ndata;
#else
/*
 * For other architectures convert each element from host double format to
 * IEEE-754 double format. The following algorithm should work on any
 * architecture, albeit slowly.
 */
  {
    int i;
    for(i=0; i<ndata; i++) {
      double mantissa;                  /* Normalized mantissa scaled by 2^53 */
      u_long msbs, lsbs;                /* The most/least significant bytes */
      double host_double = data[i];     /* The next double to be converted */
      double abs_double = fabs(host_double);/* Avoid sign extension problems */
      double two_pow_32 = ldexp(1.0,32.0);  /* 2^32 */
      unsigned sign_bit = host_double<0;    /* Sign bit (0 or 1) */
      int iexp;                             /* Power of two exponent */
/*
 * NaN and Inf have architecture-depended representations, so transmute
 * them to DBL_MAX.
 */
      if(abs_double > DBL_MAX || !(abs_double > 0.0))
	abs_double = DBL_MAX;
/*
 * Compute the mantissa and the exponent.
 */
      mantissa = (frexp(abs_double, &iexp)-0.5) * ldexp(1.0,53.0);
      iexp += 1022U;
/*
 * IEEE says the msb is a sign bit, the next 11 bits encode the exponent
 * and the final 52 bits encode the mantissa. These components are
 * derived such that the original number is
 *
 * value = (-1)^sign x 2^(exponent-1023) x mantissa
 *
 * Form the 4 most and least significant bytes in two long's.
 */
      lsbs = modf(mantissa / two_pow_32, &mantissa) * two_pow_32;
      msbs = ((u_long)mantissa) | (iexp << 20U) | (sign_bit << 31U);
/*
 * Convert the 2 4-byte words into network byte order.
 */
      lsbs = htonl(lsbs);
      msbs = htonl(msbs);
/*
 * Append the resulting bytes to the output buffer.
 */
      memcpy(net->buf + net->nput, &msbs, 4);
      net->nput += 4;
      memcpy(net->buf + net->nput, &lsbs, 4);
      net->nput += 4;
    };
  };
#endif
  return 0;
}

/*.......................................................................
 * Return the size that was passed to new_NetBuf(). Note that this is
 * NOT the same as net->size.
 *
 * Input:
 *  net    NetBuf *  The network input buffer to examine.
 * Output:
 *  return   long    The requested size, or 0 if net==NULL.
 */
long size_NetBuf(NetBuf *net)
{
  if(!net) {
    lprintf(stderr, "size_NetBuf: NULL argument.\n");
    return 0;
  };
  return net->size - NET_PREFIX_LEN;
}
