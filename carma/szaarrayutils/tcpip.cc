#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>

#ifdef VXW
#include <sockLib.h>
#include <ioLib.h>    /* ioctl() */
#else
#include <netdb.h>
#include <fcntl.h>
#endif

#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif

#include "carma/szaarrayutils/tcpip.h"
#include "carma/szaarrayutils/lprintf.h"

#include "carma/szautil/LogStream.h"
#include "carma/szautil/Exception.h"

static int server_socket(int port, int protocol, int qlen);
static int error_close(int sock);
static int connect_to_server(char *host, int port, int protocol, int dowait);

/*.......................................................................
 * Allocate a UDP or TCP socket, bind the socket to a given or service
 * dictated port number and return the socket number.
 *
 * Input:
 *  port       int    The port number to bind to.
 *  protocol   int    The transport protocol to use, from:
 *                     SOCK_DGRAM  - UDP
 *                     SOCK_STREAM - TCP
 *  qlen       int    The max length of the server connection-request
 *                    queue.
 * Output:
 *  return     int    The socket number allocated, or -1 on error.
 */
static int server_socket(int port, int protocol, int qlen)
{
  struct sockaddr_in address;    /* Socket address */
  int sock;                      /* Socket number */
  char *proto_name;              /* The name of the protocol */
/*
 * Get a readable name for the protocol.
 */
  switch(protocol) {
  case SOCK_DGRAM:
    proto_name = "UDP";
    break;
  case SOCK_STREAM:
    proto_name = "TCP/IP";
    break;
  default:
    proto_name = NULL;
    break;
  };
  if(!proto_name) {
    lprintf(stderr, "server_socket: Unknown transport protocol.\n");
    return -1;
  };
/*
 * Zero all bytes in the 'sin' address descriptor.
 */
  memset(&address, 0, sizeof(address));
/*
 * Allow connections from any of the machine's IP addresses.
 */
  address.sin_addr.s_addr = htonl(INADDR_ANY);
/*
 * Specify that this is to be a TCP/IP-family address.
 */
  address.sin_family = AF_INET;
/*
 * Legal port number?
 */
  if(port < 1) {
    lprintf(stderr, "server_socket: Bad %s port number: %d\n",
	    proto_name, port);
    return -1;
  };
/*
 * Convert the port number to network-byte-order.
 */
  address.sin_port = htons((u_short) port);
/*
 * Allocate a socket of this type.
 */
  sock = socket(PF_INET, protocol, 0);
  if(sock < 0) {
    lprintf(stderr, "server_socket: Unable to allocate %s socket.\n",
	    proto_name);
    return -1;
  };
/*
 * Allow local address re-use.
 */
  {
    int optval = 1;           /* Boolean true for SO_REUSEADDR option */
    int optlen = sizeof(int);
    if(setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *) &optval, optlen)<0) {
      lprintf(stderr, "setsockopt(): %s\n", strerror(errno));
      return error_close(sock);
    };
  };
/*
 * Bind the socket to the selected port.
 */
  if(bind(sock, (struct sockaddr *)&address, sizeof(address)) < 0) {
    lprintf(stderr, "Unable to bind %s socket to port %d.\n", proto_name,
	    port);
    return error_close(sock);
  };
/*
 * If this is a TCP socket, make it receptive to connection requests.
 */
  if(protocol==SOCK_STREAM && listen(sock, qlen)<0) {
    lprintf(stderr, "server_socket: Unable to listen on port %d\n", port);
    return error_close(sock);
  };
  return sock;
}

/*.......................................................................
 * Private error-cleanup function of passivesock().
 */
static int error_close(int sock)
{
  perror("");
  close(sock);
  return -1;
}

/*.......................................................................
 * Public TCP interface to server_socket().
 *
 * Input:
 *  port     int    The port number to bind to.
 *  qlen     int    The max number of clients to allow pending connection
 *                  requests for.
 * Output:
 *  return   int    The server socket, or -1 on error.
 */
int tcp_server_sock(int port, int qlen)
{
  return server_socket(port, SOCK_STREAM, qlen);
}

/*.......................................................................
 * Public UDP interface to server_socket().
 *
 * Input:
 *  port     int    The port number to bind to.
 *  qlen     int    The max number of clients to allow pending connection
 *                  requests for.
 * Output:
 *  return   int    The server socket, or -1 on error.
 */
int udp_server_sock(int port, int qlen)
{
  return server_socket(port, SOCK_DGRAM, qlen);
}

/*.......................................................................
 * Allocate and connect to a UDP or TCP socket.
 *
 * Input:
 *  host      char *  The IP address or name of the remote host.
 *  port       int    The target port on the remote host.
 *  protocol   int    The transport protocol to use, from:
 *                     SOCK_DGRAM  - UDP
 *                     SOCK_STREAM - TCP
 *  dowait      int    If this argument is false, the socket will
 *                     be configured for non-blocking I/O before
 *                     attempting the connection. In this case
 *                     the returned socket may not be initially
 *                     useable. select() will report the socket as
 *                     writeable when the connection completes, or
 *                     readable with zero bytes available if the
 *                     connection fails.
 * Output:
 *  return     int    The allocated socket, or -1 on error.
 */
static int connect_to_server(char *host, int port, int protocol, int dowait)
{
#ifndef VXW
  struct hostent *host_info;   /* Pointer to host information */
#endif
  struct sockaddr_in address;  /* TCP/IP endpoint address */
  char *proto_name;            /* The name of the protocol */
  int sock;                    /* Allocated socket */
/*
 * Get a readable name for the protocol.
 */
  switch(protocol) {
  case SOCK_DGRAM:
    proto_name = "UDP";
    break;
  case SOCK_STREAM:
    proto_name = "TCP/IP";
    break;
  default:
    proto_name = NULL;
    break;
  };
  if(!proto_name) {
    lprintf(stderr, "connect_to_server: Unknown transport protocol.\n");
    return -1;
  };
/*
 * Clear the address.
 */
  memset((void *) &address, 0, sizeof(address));
/*
 * Specify a TCP/IP-family address.
 */
  address.sin_family = AF_INET;
/*
 * Legal port number?
 */
  if(port < 1) {
    lprintf(stderr, "connect_to_server: Illegal %s port number (%d).\n",
	    proto_name, port);
    return -1;
  };
/*
 * Convert the port number to network-byte-order.
 */
  address.sin_port = htons((u_short) port);
/*
 * Now get the IP address of the host.
 */
#ifndef VXW
  host_info = gethostbyname(host);
/*
 * If this succeeded, copy the host address from the host_info
 * decsriptor.  Otherwise assume that 'host' contained an IP
 * numeric-dotted address and try to read that.
 */
  if(host_info) {
    if(host_info->h_addrtype != AF_INET) {
      lprintf(stderr, "Unknown address type returned by gethostbyname.\n");
      return -1;
    };
    memcpy((void *) &address.sin_addr, host_info->h_addr, host_info->h_length);
  } else
#endif
    address.sin_addr.s_addr = inet_addr(host);
/*
 * Bad host address?
 */
  if(address.sin_addr.s_addr == INADDR_NONE) {
    lprintf(stderr, "Unknown %s host: \"%s\".\n", proto_name, host);
    return -1;
  };
/*
 * Allocate the socket.
 */
  sock = socket(PF_INET, protocol, 0);
  if(sock < 0) {
    lprintf(stderr, "connect_to_server: Socket allocation failed\n");
    return -1;
  };
/*
 * If requested configure the socket for non-blocking I/O.
 */
  if(!dowait && tcp_set_blocking(sock, 0))
    return error_close(sock);

/*
 * Allow local address re-use.
 */
  {
    int optval = 1;           /* Boolean true for SO_REUSEADDR option */
    int optlen = sizeof(int);
    if(setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (char *) &optval, optlen)<0) {
      lprintf(stderr, "setsockopt(): %s\n", strerror(errno));
      return error_close(sock);
    };
  };
/*
 * Finally - connect the socket.
 */
  if(connect(sock, (struct sockaddr *) &address, sizeof(address)) < 0) {
    if(dowait || errno != EINPROGRESS) {
      ReportSimpleError(proto_name << " port " << port 
			<< " at address \"" << host 
			<< " \" is currently unreachable");
      return error_close(sock);
    };
  };
  return sock;
}

/*.......................................................................
 * Allocate and connect a TCP/IP socket.
 *
 * Input:
 *  host       char *  Name of host or its numeric INET address.
 *  port        int    The target port number on the remote host.
 *  dowait      int    If this argument is false, the socket will
 *                     be configured for non-blocking I/O before
 *                     attempting the connection. In this case
 *                     the returned socket may not be initially
 *                     useable. select() will report the socket as
 *                     writeable when the connection completes, or
 *                     readable with zero bytes available if the
 *                     connection fails.
 * Output:
 *  return      int    The allocated socket, or -1 on error.
 */
int tcp_connect(char *host, int port, int dowait)
{
  return connect_to_server(host, port, SOCK_STREAM, dowait);
}

/*.......................................................................
 * Allocate and connect a UDP socket.
 *
 * Input:
 *  host      char *  Name of host or its numeric INET address.
 *  port      int     The target port number on the remote host.
 *  dowait    int    If this argument is false, the socket will
 *                   be configured for non-blocking I/O before
 *                   attempting the connection. In this case
 *                   the returned socket may not be initially
 *                   useable. select() will report the socket as
 *                   writeable when the connection completes, or
 *                   readable with zero bytes available if the
 *                   connection fails.
 * Output:
 *  return    int    The allocated socket, or -1 on error.
 */
int udp_connect(char *host, int port, int dowait)
{
  return connect_to_server(host, port, SOCK_DGRAM, dowait);
}

/*.......................................................................
 * Set whether a TCP/IP socket should use blocking or non-blocking I/O.
 *
 * Input:
 *  sock         int    The file-descriptor of the target socket.
 *  doblock      int    0 - Use non-blocking I/O.
 *                      1 - Use blocking I/O.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
int tcp_set_blocking(int sock, int doblock)
{
#ifdef VXW
  {
    int on;   /* Use non-blocking I/O if true */
    on = !doblock;
    if(ioctl(sock, FIONBIO, &on)) {
      if(doblock)
        perror("tcp_set_blocking: Unable to select blocking I/O");
      else
        perror("tcp_set_blocking: Unable to select non-blocking I/O");
      return 1;
    };
  };
#else
  {
/*
 * Get the non-blocking I/O file-mode bit appropriate to the current
 * system.
 */
#ifdef O_NONBLOCK
    int bit = O_NONBLOCK;  /* POSIX.1 */
#elif defined(O_NDELAY)
    int bit = O_NDELAY;    /* System V */
#elif defined(FNDELAY)
    int bit = FNDELAY;     /* BSD */
#else
#error "tc_set_blocking: Non-blocking I/O doesn't appear to be supported."
#endif
/*
 * Get a copy of the current file-descriptor flags.
 */
    int flags = fcntl(sock, F_GETFL, 0);
    if(flags < 0) {
      perror("tcp_set_blocking: fcntl(F_GETFL).");
      return 1;
    };
/*
 * Add or remove the non-blocking flag.
 */
    if(doblock)
      flags &= ~bit; /* Clear non-blocking flag */
    else
      flags |= bit;  /* Set non-blocking flag */
/*
 * Install the modified flags.
 */
    if(fcntl(sock, F_SETFL, flags) < 0) {
      if(doblock)
        perror("tcp_set_blocking: Unable to select blocking I/O");
      else
        perror("tcp_set_blocking: Unable to select non-blocking I/O");
      return 1;
    };
  };
#endif
  return 0;
}
