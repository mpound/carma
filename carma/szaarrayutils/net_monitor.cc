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
#include "carma/szaarrayutils/szaregs.h"
#include "carma/szaarrayutils/arraytemplate.h"
#include "carma/szaarrayutils/monitor_stream.h"
#include "carma/szaarrayutils/netbuf.h"
#include "carma/szaarrayutils/tcpip.h"

#include "carma/szautil/SzaPorts.h"

#include "carma/szaarrayutils/monitor.h"

using namespace sza::array;

typedef struct {
  char *host;         /* The address of the control-program host */
  NetReadStr *nrs;    /* The network input stream */
  NetSendStr *nss;    /* The network output stream */
  int fd;             /* The TCP/IP socket of the connection */
  RegMap *regmap;     /* The register map of the control program */
  ArrayMap *arraymap; /* The array map of the control program */
} NetMonitor;

static NetMonitor *new_NetMonitor(char *host);
static NetMonitor *del_NetMonitor(NetMonitor *nm);

static MS_DESTRUCTOR(nm_destructor);
static MS_READ_FRAME(nm_read_frame);
static MS_SEND_MSG(nm_send_msg);
static MS_QUEUE_REGSET(nm_queue_regset);
static MS_QUEUE_INTERVAL(nm_queue_interval);
static MS_SELECT_FD(nm_select_fd);
static MS_ARRAYMAP(nm_arraymap);

static int open_NetMonitorStream(MonitorStream *ms, char *host);

static MsReadState nm_read_msg(NetMonitor *nm, int dowait);
static int nm_read_sizes(NetMonitor *nm, NetReadStr *nrs, long *nrs_size,
			 long *nss_size);
static ArrayMap *nm_read_ArrayMap(NetMonitor *nm, NetReadStr *nrs);

/*.......................................................................
 * Connect a given MonitorStream iterator to the SZA control program via
 * TCP/IP.
 *
 * Input:
 *  ms      MonitorStream *  The stream iterator to be connected.
 *  host             char *  The IP name or address of the control-computer.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
static int open_NetMonitorStream(MonitorStream *ms, char *host)
{
  NetMonitor *nm;   /* The network context of the iterator */
/*
 * Check arguments.
 */
  if(!ms || !host) {
    lprintf(stderr, "open_NetMonitorStream: NULL argument.\n");
    return 1;
  };
/*
 * Attempt to open the TCP/IP channel.
 */
  nm = new_NetMonitor(host);
  if(!nm)
    return 1;
/*
 * Assign the channel to the generic monitor-stream iterator.
 */
  return open_MonitorStream(ms, (void *) nm, nm_destructor,
			    nm_read_frame, nm_send_msg, nm_queue_regset,
			    nm_queue_interval, 0, nm_select_fd, nm_arraymap,
			    false);
}
/*.......................................................................
 * Return a new monitor stream that has been connected to the SZA
 * control program.
 *
 * Input:
 *  host            char *  The IP name or address of the control-computer.
 * Output:
 *  return MonitorStream *  The connected monitor stream iterator, or
 *                          NULL on error.
 */
MonitorStream *new_NetMonitorStream(char *host)
{
  MonitorStream *ms = new_MonitorStream(false);
  if(!ms || open_NetMonitorStream(ms, host))
    return del_MonitorStream(ms);
  return ms;
}

/*.......................................................................
 * This is a wrapper around del_NetMonitor() which deletes a NetMonitor
 * object via the (void *) alias used by MonitorStream objects.
 *
 * Input:
 *  context          void *  The NetMonitor object to be deleted, cast
 *                           to (void *).
 * Output:
 *  return           void *  The deleted context (always NULL).
 */
static MS_DESTRUCTOR(nm_destructor)
{
  return (void *) del_NetMonitor((NetMonitor *)context);
}

/*.......................................................................
 * Create the context of a TCP/IP monitor-stream iterator. This is
 * designed for use in receiving "real-time" monitor data from the
 * SZA control program.
 *
 * Input:
 *  host          char *  The name or address of the control-computer.
 * Output:
 *  return  NetMonitor *  The new network monitor object, or NULL on
 *                        error.
 */
static NetMonitor *new_NetMonitor(char *host)
{
  NetMonitor *nm;   /* The object to be returned */
  long nrs_size;    /* The size of the network input buffer */
  long nss_size;    /* The size of the network output buffer */
/*
 * Allocate the container.
 */
  nm = (NetMonitor *) malloc(sizeof(NetMonitor));
  if(!nm) {
    lprintf(stderr, "new_NetMonitor: Insufficient memory.\n");
    return NULL;
  };
/*
 * Initialize the container at least up to the point at which it is
 * safe to call del_NetMonitor().
 */
  nm->host = NULL;
  nm->nrs = NULL;
  nm->nss = NULL;
  nm->regmap = NULL;
  nm->arraymap = NULL;
  nm->fd = -1;
/*
 * Make a copy of the host address.
 */
  nm->host = (char *) malloc(strlen(host) + 1);
  if(!nm->host) {
    lprintf(stderr, "new_NetMonitor: Insufficient to record host name.\n");
    return del_NetMonitor(nm);
  };
  strcpy(nm->host, host);
/*
 * Attempt to contact the control program at the specified host.
 */
  nm->fd = tcp_connect(nm->host, CP_MONITOR_PORT, 1);
  if(nm->fd < 0)
    return del_NetMonitor(nm);
/*
 * Allocate a temporary network read stream to use solely to read
 * the required sizes of the final read and send streams.
 */
  nm->nrs = new_NetReadStr(nm->fd, 100);
  if(!nm->nrs)
    return del_NetMonitor(nm);
/*
 * Read the required sizes of the input and output stream buffers
 * from the control program.
 */
  if(nm_read_sizes(nm, nm->nrs, &nrs_size, &nss_size))
    return del_NetMonitor(nm);
/*
 * Discard the temporary network-input iterator.
 */
  nm->nrs = del_NetReadStr(nm->nrs);
/*
 * Allocate network iterators with the buffer sizes that were
 * read from the control program.
 */
  nm->nrs = new_NetReadStr(nm->fd, nrs_size);
  if(!nm->nrs)
    return del_NetMonitor(nm);
  nm->nss = new_NetSendStr(nm->fd, nss_size);
  if(!nm->nss)
    return del_NetMonitor(nm);
  
  // Read the array map from the control program.

  nm->arraymap = nm_read_ArrayMap(nm, nm->nrs);
  if(!nm->arraymap)
    return del_NetMonitor(nm);

  // A kludge while I'm modifying this code.

  nm->regmap = nm->arraymap->regmaps[0]->regmap;

  return nm;
}

/*.......................................................................
 * Delete a network monitor object.
 *
 * Input:
 *  nm      NetMonitor *  The object to be deleted.
 * Output:
 *  return  NetMonitor *  The deleted object (always NULL).
 */
static NetMonitor *del_NetMonitor(NetMonitor *nm)
{
  if(nm) {
    if(nm->host)
      free(nm->host);
    nm->nrs = del_NetReadStr(nm->nrs);
    nm->nss = del_NetSendStr(nm->nss);
    if(nm->fd >= 0) {
      shutdown(nm->fd, 2);
      close(nm->fd);
    };
    nm->arraymap = del_ArrayMap(nm->arraymap);
    free(nm);
  };
  return NULL;
}

/*.......................................................................
 * This is a private function of new_NetMonitor(). It reads the 
 * greeting message sent by the control program, which contains the
 * required sizes of the network read and write buffers.
 *
 * Input:
 *  nm     NetMonitor *  The network monitor object.
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
static int nm_read_sizes(NetMonitor *nm, NetReadStr *nrs, long *nrs_size,
			 long *nss_size)
{
  int opcode;   /* The type of message that has been read */
/*
 * Read the next message from the control program.
 */
  if(nm_read_msg(nm, 1) != MS_READ_DONE)
    return 1;
/*
 * Unpack the message.
 */
  if(net_start_get(nrs->net, &opcode) ||
     opcode != MC_SIZE_MSG ||
     net_get_long(nrs->net, 1, (unsigned long* )nrs_size) ||
     net_get_long(nrs->net, 1, (unsigned long* )nss_size) ||
     net_end_get(nrs->net)) {
    lprintf(stderr, "new_NetMonitor: Error reading size message.\n"); 
    return 1;
  };
  return 0;
}

/*.......................................................................
 * This is a private function of new_NetMonitor(). It reads the 
 * second greeting message sent by the control program, which contains a
 * template from which a register map can be constructed, and uses it
 * to create a register map.
 *
 * Input:
 *  nm    NetMonitor *  The network monitor object.
 *  nrs   NetReadStr *  The input stream to use to read the message.
 * Output:
 *  return    RegMap *  The register map template, or NULL on error.
 */
static ArrayMap *nm_read_ArrayMap(NetMonitor *nm, NetReadStr *nrs)
{
  ArrayMap *arraymap = NULL; // The array map to be returned 
  int opcode;              // The type of message that has been read 
  int error=0;
  
  // Read the next message from the control program.

  if(nm_read_msg(nm, 1) != MS_READ_DONE)
    return NULL;
  
  // Unpack the message.

  error  = net_start_get(nrs->net, &opcode);
  error |= opcode != MC_REGMAP_MSG;
  error |= (arraymap = net_get_ArrayMap(nrs->net)) == NULL;
  error |= net_end_get(nrs->net);

  if(error) {
    lprintf(stderr, "new_NetMonitor: Error reading register-map message.\n");
    return del_ArrayMap(arraymap);
  };

  return arraymap;
}

/*.......................................................................
 * Read some or all of the next frame of selected registers from the
 * control program into ms_RegRawData(ms).
 *
 * Input:
 *  ms   MonitorStream *  The monitor-stream iterator.
 *  dowait         int    If dowait==0 then return before a complete
 *                        message has been received if it can't be
 *                        read without blocking.
 * Output:
 *  return MsReadState    The status of the read, from:
 *                         MS_READ_ENDED - The connection was lost.
 *                         MS_READ_AGAIN - (dowait==1) Message incomplete.
 *                                         Call again when more data is
 *                                         available.
 *                         MS_READ_DONE  - A complete frame has been
 *                                         read and calibrated.
 */
static MS_READ_FRAME(nm_read_frame)
{
  NetMonitor *nm = (NetMonitor *) ms_SourceContext(ms);
  NetRegState regstate;  /* The state of the new register frame */
  NetBuf *net;           /* The network read buffer */
  int opcode;            /* The received message type */
/*
 * Get an alias to the network read buffer.
 */
  net = nm->nrs->net;
/*
 * If a new register set has just been sent to the control program
 * some register frames that pertain to the old selection may still
 * be incoming from the control program. These should be discarded.
 * Loop for contemporary frames.
 */
  while(1) {
/*
 * Read as much of the current message as possible.
 */
    MsReadState state = nm_read_msg(nm, dowait);
    if(state != MS_READ_DONE)
      return state;
/*
 * Unpack the header of the message.
 */
    if(net_start_get(net, &opcode) ||
       opcode != MC_REGS_MSG) {
      lprintf(stderr, "Error decoding message from control program.\n");
      return MS_READ_ENDED;
    };
/*
 * Read the contents of the register set message into ms_RegRawData(ms)
 * if the new frame is compatible with the latest register selection.
 */

#if 0
    std::cout << "Raw net data size is: " << ms_RegRawData(ms)->fm->sizeInBytesOfData() << std::endl;
#endif

    regstate = netGetRegs(ms_RegSet(ms)->regSet(), ms_RegRawData(ms), net);
    switch(regstate) {
    case NETREG_OK:
      return MS_READ_DONE;
      break;
    case NETREG_SKIP:
      continue;
    case NETREG_ERROR:
      return MS_READ_ENDED;
      break;
    };
  };
}

/*.......................................................................
 * Read all or part of the next message from the control program.
 * The message will be left for subsequent unpacking in nm->nrs.
 *
 * Input:
 *  nm      NetMonitor *  The network monitor object.
 *  dowait         int    If true wait until a whole message has been
 *                        read before returning. Otherwise, if the
 *                        input stream is blocked, defer completion
 *                        of the transaction to a subsequent call to
 *                        this function.
 * Output:
 *  return MsReadState    The state of the transaction. One of:
 *                          MS_READ_ENDED - The connection was lost.
 *                          MS_READ_AGAIN - The output stream is temporarily
 *                                          blocked and dowait=0. Call this
 *                                          function again to complete the
 *                                          transaction.
 *                          MS_READ_DONE  - The message has been sent.
 */
static MsReadState nm_read_msg(NetMonitor *nm, int dowait)
{
  int state;             /* The state of the read operation */
/*
 * Select non-blocking or blocking I/O as requested.
 */
  if(tcp_set_blocking(nm->fd, dowait))
    return MS_READ_ENDED;
/*
 * Read as much of the message as possible.
 */
  do {
    state = nrs_read_msg(nm->nrs);
    switch(state) {
#ifdef _GPP /* If compiling with g++, we have to define the namespace */
    case NetReadStr::NET_READ_SIZE:
#else
    case NET_READ_SIZE:
#endif
#ifdef _GPP
    case NetReadStr::NET_READ_DATA:
#else
    case NET_READ_DATA:
#endif
      return MS_READ_AGAIN;
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
      return MS_READ_ENDED;
      break;
#ifdef _GPP
    case NetReadStr::NET_READ_ERROR:
#else
    case NET_READ_ERROR:
#endif
      lprintf(stderr, "Error reading monitor message from control program.\n");
      return MS_READ_ENDED;
      break;
    };
#ifdef _GPP
  } while(state != NetReadStr::NET_READ_DONE);
#else
  } while(state != NET_READ_DONE);
#endif
  return MS_READ_DONE;
}

/*.......................................................................
 * Send a previously packed message to the control program.
 *
 * Input:
 *  ms   MonitorStream *  The monitor-stream iterator.
 *  dowait         int    If true wait until the whole message has been
 *                        sent before returning. Otherwise, if the
 *                        output stream is blocked, defer completion
 *                        of the transaction to a subsequent call to
 *                        this function.
 * Output:
 *  return MsSendState    The state of the transaction. One of:
 *                          MS_SEND_ERROR - An unrecoverable error occurred.
 *                          MS_SEND_AGAIN - The output stream is temporarily
 *                                          blocked and dowait=0. Call this
 *                                          function again to complete the
 *                                          transaction.
 *                          MS_SEND_DONE  - The message has been sent.
 */
static MS_SEND_MSG(nm_send_msg)
{
  NetMonitor *nm = (NetMonitor *) ms_SourceContext(ms);
  int state;             /* The state of the send operation */
/*
 * Select non-blocking or blocking I/O as requested.
 */
  if(tcp_set_blocking(nm->fd, dowait))
    return MS_SEND_ERROR;
/*
 * Send as much of the message as possible.
 */
  do {
    state = nss_send_msg(nm->nss);
    switch(state) {
#ifdef _GPP
    case NetSendStr::NET_SEND_DATA:
#else
    case NET_SEND_DATA:
#endif
      return MS_SEND_AGAIN;
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
	      "nm_send_msg: Error sending message to control program.\n");
      return MS_SEND_ERROR;
      break;
    };
#ifdef _GPP
  } while(state != NetSendStr::NET_SEND_DONE);
#else
  } while(state != NET_SEND_DONE);
#endif
  return MS_SEND_DONE;
}

/*.......................................................................
 * Pack a modified register set for subsequent transmission to the
 * control program.
 *
 * Input:
 *  ms   MonitorStream *  The monitor-stream iterator.
 * Output:
 *  return        int     MS_SEND_ERROR - Abnormal completion.
 *                        MS_SEND_AGAIN - Normal completion, but
 *                                        nm_send_msg() must be
 *                                        called to complete the
 *                                        transaction.
 */
static MS_QUEUE_REGSET(nm_queue_regset)
{
  NetMonitor *nm = (NetMonitor *) ms_SourceContext(ms);
  NetBuf *net;           /* The network send buffer */
/*
 * Get a local alias to the output network buffer.
 */
  net = nm->nss->net;
/*
 * Pack the register set for subsequent transmission to the
 * control program.
 */
  if(net_start_put(net, MC_REGSET_MSG) ||
     net_put_RegSet(ms_RegSet(ms)->regSet(), net) ||
     net_end_put(net))
    return MS_SEND_ERROR;
  return MS_SEND_AGAIN;
}

/*.......................................................................
 * Pack a changed sampling interval for subsequent transmission to the
 * control program.
 *
 * Input:
 *  ms   MonitorStream *  The monitor-stream iterator.
 * Output:
 *  return        int     MS_SEND_ERROR - Abnormal completion.
 *                        MS_SEND_AGAIN - Normal completion, but
 *                                        nm_send_msg() must be
 *                                        called to complete the
 *                                        transaction.
 */
static MS_QUEUE_INTERVAL(nm_queue_interval)
{
  NetMonitor *nm = (NetMonitor *) ms_SourceContext(ms);
  NetBuf *net;             /* The network send buffer */
/*
 * Get a local alias to the output network buffer.
 */
  net = nm->nss->net;
/*
 * Pack the register set for subsequent transmission to the
 * control program.
 */
  {
    unsigned long interval = ms_get_interval(ms);
    if(net_start_put(net, MC_INTERVAL_MSG) ||
       net_put_long(net, 1, &interval) ||
       net_end_put(net))
      return MS_SEND_ERROR;
  };
  return MS_SEND_AGAIN;
}

/*.......................................................................
 * This is the monitor-stream method that returns the socket fd for use
 * in select().
 */
static MS_SELECT_FD(nm_select_fd)
{
  NetMonitor *nm = (NetMonitor *) ms_SourceContext(ms);
  return nm->fd;
}

/*.......................................................................
 * This is the monitor-stream method that returns the register map
 * of the stream.
 */
static MS_ARRAYMAP(nm_arraymap)
{
  NetMonitor *nm = (NetMonitor *) ms_SourceContext(ms);
  return nm->arraymap;
}

