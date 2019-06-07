#include <stdlib.h>
#include <string.h>

#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <unistd.h>

#include "carma/szaarrayutils/monitor.h"
#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/im_monitor_stream.h"
#include "carma/szaarrayutils/netbuf.h"
#include "carma/szaarrayutils/tcpip.h"
#include "carma/szaarrayutils/optcam.h"

#include "carma/szaarrayutils/monitor.h"

using namespace sza::array;

#define DEBUG

typedef struct {
  char *host;         /* The address of the control-program host */
  NetReadStr *nrs;    /* The network input stream */
  NetSendStr *nss;    /* The network output stream */
  int fd;             /* The TCP/IP socket of the connection */
} NetImMonitor;

static NetImMonitor *new_NetImMonitor(char *host);
static NetImMonitor *del_NetImMonitor(NetImMonitor *nim);

static IMS_DESTRUCTOR(nim_destructor);
static IMS_READ_IMAGE(nim_read_image);
static IMS_SEND_MSG(nim_send_msg);
static IMS_SELECT_FD(nim_select_fd);

static int open_NetImMonitorStream(ImMonitorStream *ims, char *host);

static ImsReadState nim_read_msg(NetImMonitor *nim, int dowait);
static int nim_read_sizes(NetImMonitor *nim, NetReadStr *nrs, long *nrs_size,
			 long *nss_size);

/*.......................................................................
 * Connect a given ImMonitorStream iterator to the SZA control program via
 * TCP/IP.
 *
 * Input:
 *  ims      ImMonitorStream *  The stream iterator to be connected.
 *  host             char *  The IP name or address of the control-computer.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
static int open_NetImMonitorStream(ImMonitorStream *ims, char *host)
{
  NetImMonitor *nim;   /* The network context of the iterator */
/*
 * Check arguments.
 */
  if(!ims || !host) {
    lprintf(stderr, "open_NetImMonitorStream: NULL argument.\n");
    return 1;
  };
/*
 * Attempt to open the TCP/IP channel.
 */
  nim = new_NetImMonitor(host);
  if(!nim)
    return 1;
/*
 * Assign the channel to the generic monitor-stream iterator.
 */
  return open_ImMonitorStream(ims, (void *) nim, nim_destructor,
			    nim_read_image, nim_send_msg, nim_select_fd);
}
/*.......................................................................
 * Return a new image monitor stream that has been connected to the SZA
 * control program.
 *
 * Input:
 *  host            char *  The IP name or address of the control-computer.
 * Output:
 *  return ImMonitorStream *  The connected image monitor stream iterator, or
 *                          NULL on error.
 */
ImMonitorStream *new_NetImMonitorStream(char *host)
{
  ImMonitorStream *ims = new_ImMonitorStream();
  if(!ims || open_NetImMonitorStream(ims, host))
    return del_ImMonitorStream(ims);
  return ims;
}

/*.......................................................................
 * This is a wrapper around del_NetImMonitor() which deletes a NetImMonitor
 * object via the (void *) alias used by ImMonitorStream objects.
 *
 * Input:
 *  context          void *  The NetImMonitor object to be deleted, cast
 *                           to (void *).
 * Output:
 *  return           void *  The deleted context (always NULL).
 */
static IMS_DESTRUCTOR(nim_destructor)
{
  return (void *) del_NetImMonitor((NetImMonitor *)context);
}

/*.......................................................................
 * Create the context of a TCP/IP image monitor-stream iterator. This is
 * designed for use in receiving "real-time" monitor data from the
 * SZA control program.
 *
 * Input:
 *  host          char *  The name or address of the control-computer.
 * Output:
 *  return  NetImMonitor *  The new network monitor object, or NULL on
 *                        error.
 */
static NetImMonitor *new_NetImMonitor(char *host)
{
  NetImMonitor *nim;/* The object to be returned */
  long nrs_size;    /* The size of the network input buffer */
  long nss_size;    /* The size of the network output buffer */
/*
 * Allocate the container.
 */
  nim = (NetImMonitor *) malloc(sizeof(NetImMonitor));
  if(!nim) {
    lprintf(stderr, "new_NetImMonitor: Insufficient memory.\n");
    return NULL;
  };
/*
 * Initialize the container at least up to the point at which it is
 * safe to call del_NetImMonitor().
 */
  nim->host = NULL;
  nim->nrs = NULL;
  nim->nss = NULL;
  nim->fd = -1;
/*
 * Make a copy of the host address.
 */
  nim->host = (char *) malloc(strlen(host) + 1);
  if(!nim->host) {
    lprintf(stderr, "new_NetImMonitor: Insufficient to record host name.\n");
    return del_NetImMonitor(nim);
  };
  strcpy(nim->host, host);
/*
 * Attempt to contact the control program at the specified host.
 */
  nim->fd = tcp_connect(nim->host, CP_IM_MONITOR_PORT, 1);
  if(nim->fd < 0)
    return del_NetImMonitor(nim);
/*
 * Allocate a temporary network read stream to use solely to read
 * the required sizes of the final read and send streams.
 */
  nim->nrs = new_NetReadStr(nim->fd, 100);
  if(!nim->nrs)
    return del_NetImMonitor(nim);
/*
 * Read the required sizes of the input and output stream buffers
 * from the control program.
 */
  if(nim_read_sizes(nim, nim->nrs, &nrs_size, &nss_size))
    return del_NetImMonitor(nim);
/*
 * Discard the temporary network-input iterator.
 */
  nim->nrs = del_NetReadStr(nim->nrs);
/*
 * Allocate network iterators with the buffer sizes that were
 * read from the control program.
 */
  nim->nrs = new_NetReadStr(nim->fd, nrs_size);
  if(!nim->nrs)
    return del_NetImMonitor(nim);
  nim->nss = new_NetSendStr(nim->fd, nss_size);
  if(!nim->nss)
    return del_NetImMonitor(nim);
  return nim;
}

/*.......................................................................
 * Delete a network image monitor object.
 *
 * Input:
 *  nim      NetImMonitor *  The object to be deleted.
 * Output:
 *  return  NetImMonitor *  The deleted object (always NULL).
 */
static NetImMonitor *del_NetImMonitor(NetImMonitor *nim)
{
  if(nim) {
    if(nim->host)
      free(nim->host);
    nim->nrs = del_NetReadStr(nim->nrs);
    nim->nss = del_NetSendStr(nim->nss);
    if(nim->fd >= 0) {
      shutdown(nim->fd, 2);
      close(nim->fd);
    };
    free(nim);
  };
  return NULL;
}

/*.......................................................................
 * This is a private function of new_NetImMonitor(). It reads the 
 * greeting message sent by the control program, which contains the
 * required sizes of the network read and write buffers.
 *
 * Input:
 *  nim     NetImMonitor *  The network monitor object.
 *  nrs    NetReadStr *  The input stream to use to read the message.
 * Input/Output:
 *  nrs_size     long *  The size required for the network input stream
 *                       will be recorded in *nrs_size.
 *  nss_size     long *  The size required for the network output stream
 *                       will be recorded in *nss_size.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int nim_read_sizes(NetImMonitor *nim, NetReadStr *nrs, long *nrs_size,
			 long *nss_size)
{
  int opcode;   /* The type of message that has been read */
/*
 * Read the next message from the control program.
 */
  if(nim_read_msg(nim, 1) != IMS_READ_DONE)
    return 1;
/*
 * Unpack the message.
 */
  if(net_start_get(nrs->net, &opcode) ||
     opcode != IMC_SIZE_MSG ||
     net_get_long(nrs->net, 1, (long unsigned int* )nrs_size) ||
     net_get_long(nrs->net, 1, (long unsigned int* )nss_size) ||
     net_end_get(nrs->net)) {
    lprintf(stderr, "new_NetImMonitor: Error reading size message.\n"); 
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Read some or all of the next image from the
 * control program.  When this function returns IMS_READ_DONE, the image
 * will be deposited in ims->image.
 *
 * Input:
 *  ims   ImMonitorStream *  The monitor-stream iterator.
 *  dowait         int    If dowait==0 then return before a complete
 *                        message has been received if it can't be
 *                        read without blocking.
 * Output:
 *  return ImsReadState    The status of the read, from:
 *                         IMS_READ_ENDED - The connection was lost.
 *                         IMS_READ_AGAIN - (dowait==1) Message incomplete.
 *                                         Call again when more data is
 *                                         available.
 *                         IMS_READ_DONE  - A complete image has been
 *                                         read.
 */
static IMS_READ_IMAGE(nim_read_image)
{
  NetImMonitor *nim = (NetImMonitor *) ims_SourceContext(ims);
  NetBuf *net;           /* The network read buffer */
  unsigned short *image = ims_get_image(ims);
  int opcode;            /* The received message type */
  ImsReadState state;             /* The state of the read operation */
/*
 * Get an alias to the network read buffer.
 */
  net = nim->nrs->net;
  /*
   * Read as much of the current message as possible.
   */
  state = nim_read_msg(nim, dowait);
  if(state != IMS_READ_DONE)
    return state;
  /*
   * Unpack the header of the message.
   */
  if(net_start_get(net, &opcode) ||
     opcode != IMC_IMAGE_MSG) {
    lprintf(stderr, "Error decoding message from control program.\n");
    return IMS_READ_ENDED;
  };
  /*
   * Read the contents of the image message.
   */
  if(net_get_short(net, GRABBER_IM_SIZE, image)) 
    return IMS_READ_ENDED;
  return IMS_READ_DONE;
}

/*.......................................................................
 * Read all or part of the next message from the control program.
 * The message will be left for subsequent unpacking in nim->nrs.
 *
 * Input:
 *  nim      NetImMonitor *  The network monitor object.
 *  dowait         int    If true wait until a whole message has been
 *                        read before returning. Otherwise, if the
 *                        input stream is blocked, defer completion
 *                        of the transaction to a subsequent call to
 *                        this function.
 * Output:
 *  return ImsReadState    The state of the transaction. One of:
 *                          IMS_READ_ENDED - The connection was lost.
 *                          IMS_READ_AGAIN - The output stream is temporarily
 *                                          blocked and dowait=0. Call this
 *                                          function again to complete the
 *                                          transaction.
 *                          IMS_READ_DONE  - The message has been sent.
 */
static ImsReadState nim_read_msg(NetImMonitor *nim, int dowait)
{
  int state;             /* The state of the read operation */
/*
 * Select non-blocking or blocking I/O as requested.
 */
  if(tcp_set_blocking(nim->fd, dowait))
    return IMS_READ_ENDED;
/*
 * Read as much of the message as possible.
 */
  do {
    state = nrs_read_msg(nim->nrs);
    switch(state) {
#ifdef _GPP
    case NetReadStr::NET_READ_SIZE:
#else
    case NET_READ_SIZE:
#endif
#ifdef _GPP
    case NetReadStr::NET_READ_DATA:
#else
    case NET_READ_DATA:
#endif
      return IMS_READ_AGAIN;
      break;
#ifdef _GPP
    case NetReadStr::NET_READ_DONE:
#else
    case NET_READ_DONE:
#endif
      break;
#ifdef _GPP
    case NetReadStr::NET_READ_CLOSED:
#else
    case NET_READ_CLOSED:
#endif
      lprintf(stderr, "Monitor connection lost to control program.\n");
      return IMS_READ_ENDED;
      break;
#ifdef _GPP
    case NetReadStr::NET_READ_ERROR:
#else
    case NET_READ_ERROR:
#endif
      lprintf(stderr, "Error reading monitor message from control program.\n");
      return IMS_READ_ENDED;
      break;
    };
#ifdef _GPP
  } while(state != NetReadStr::NET_READ_DONE);
#else
  } while(state != NET_READ_DONE);
#endif
  return IMS_READ_DONE;
}

/*.......................................................................
 * Send a previously packed message to the control program.
 *
 * Input:
 *  ims   ImMonitorStream *  The monitor-stream iterator.
 *  dowait         int    If true wait until the whole message has been
 *                        sent before returning. Otherwise, if the
 *                        output stream is blocked, defer completion
 *                        of the transaction to a subsequent call to
 *                        this function.
 * Output:
 *  return ImsSendState    The state of the transaction. One of:
 *                          IMS_SEND_ERROR - An unrecoverable error occurred.
 *                          IMS_SEND_AGAIN - The output stream is temporarily
 *                                          blocked and dowait=0. Call this
 *                                          function again to complete the
 *                                          transaction.
 *                          IMS_SEND_DONE  - The message has been sent.
 */
static IMS_SEND_MSG(nim_send_msg)
{
  NetImMonitor *nim = (NetImMonitor *) ims_SourceContext(ims);
  int state;             /* The state of the send operation */
/*
 * Select non-blocking or blocking I/O as requested.
 */
  if(tcp_set_blocking(nim->fd, dowait))
    return IMS_SEND_ERROR;
/*
 * Send as much of the message as possible.
 */
  do {
    state = nss_send_msg(nim->nss);
    switch(state) {
#ifdef _GPP
    case NetSendStr::NET_SEND_DATA:
#else
    case NET_SEND_DATA:
#endif
      return IMS_SEND_AGAIN;
      break;
#ifdef _GPP
    case NetSendStr::NET_SEND_DONE:
#else
    case NET_SEND_DONE:
#endif
      break;
#ifdef _GPP
    case NetSendStr::NET_SEND_ERROR:
#else
    case NET_SEND_ERROR:
#endif
#ifdef _GPP
    case NetSendStr::NET_SEND_CLOSED:
#else
    case NET_SEND_CLOSED:
#endif
      lprintf(stderr,
	      "nim_send_msg: Error sending message to control program.\n");
      return IMS_SEND_ERROR;
      break;
    };
#ifdef _GPP
  } while(state != NetSendStr::NET_SEND_DONE);
#else
  } while(state != NET_SEND_DONE);
#endif
  return IMS_SEND_DONE;
}
/*.......................................................................
 * This is the image monitor-stream method that returns the socket fd for use
 * in select().
 */
static IMS_SELECT_FD(nim_select_fd)
{
  NetImMonitor *nim = (NetImMonitor *) ims_SourceContext(ims);
  return nim->fd;
}

