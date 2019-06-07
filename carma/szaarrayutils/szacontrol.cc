#include <iostream>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <signal.h>
#include <math.h>      /* modf */

#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>

#include "netbuf.h"
#include "netobj.h"
#include "tcpip.h"
#include "szaregs.h"
#include "arraymap.h"
#include "arraytemplate.h"
#include "regdata.h"
#include "regset.h"
#include "lprintf.h"
#include "scanner.h"
#include "control.h"
#include "szacontrol.h"
#include "astrom.h"
#include "optcam.h"
#include "szaconst.h"
#include "list.h"

#include "carma/szaarrayutils/monitor.h"
#include "carma/szaarrayutils/arraymaprev.h"

#include "carma/szautil/AntNum.h"
#include "carma/szautil/ArrayDataFrameManager.h"
#include "carma/szautil/CarmaConfig.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/LogMsgHandler.h"

#include <vector>

using namespace sza::array;
using namespace sza::util;

using sza::util::Debug;

/*
 * Enumerate the known types of communication channels.
 */
typedef enum {
  CHAN_SOCK,          /* A duplex network socket */
  CHAN_PIPE           /* An unnamed pipe */
} ChanType;

/*
 * Provide a forward declaration of the generic first member of
 * all communication channel types.
 */
typedef struct ComHeader ComHeader;

/*
 * All types of communication channel must provide functions of the
 * following type.
 */

/*
 * When data arrives on the file-descriptor of a readable communication
 * channel, or a writeable channel has room for more data, the appropriate
 * aspect of the channel is removed from the list of active channels, then
 * a function of the following type is called. If the I/O doesn't complete,
 * be sure to re-register for completion via add_readable/writeable_channel().
 *
 * Input:
 *  cp    ControlProg *   The control program resource object.
 *  head    ComHeader *   The communication channel.
 */
#define CHAN_IO_FN(fn) void (fn)(ControlProg *cp, ComHeader *head)

/*
 * Each channel has two members of the following type, representing
 * the two I/O directions.
 */
typedef struct ComAspect ComAspect;
struct ComAspect {
  ComHeader *parent;  /* The generic header of the containing channel */
  int fd;             /* The file-descriptor to be watched. */
  fd_set *active_set; /* A pointer to the select set to be checked, or NULL */
                      /*  if this channel aspect isn't being watched */
  CHAN_IO_FN(*io_fn); /* The function used to respond to I/O readiness */
  ComAspect *next;    /* The next channel in the list */
  ComAspect *prev;    /* The previous channel in the list */
};

/*
 * All communication channels start with a member of the following type.
 */
struct ComHeader {
  ChanType type;      /* The type of communications channel */
  void *client_data;  /* Client-specific data associated with the channel */
  ComAspect read;     /* The readable aspect of the channel */
  ComAspect write;    /* The writeable aspect of the channel */
};

/*
 * The following function must be called from the initialization part
 * channel constructor functions.
 */
static void ini_ComHeader(ComHeader *head, ChanType type, void *client_data,
			  CHAN_IO_FN(*read_fn), CHAN_IO_FN(*write_fn));
static void set_channel_fds(ControlProg *cp, ComHeader *head, int read_fd,
			    int write_fd);

/*
 * List the virtual method functions of a socket channel.
 */
typedef struct SockChan SockChan;

/*
 * When a complete message has been read into sock->nrs, then a
 * function of the following form is called to process the message.
 * Before it is called, rem_readable_channel(cp, sock) is called to
 * remove the socket from the list of channels to be watched
 * for further messages.
 *
 * Input:
 *  cp    ControlProg *   The control program resource object.
 *  sock     SockChan *   The socket that received the message.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
#define SOCK_RCVD_FN(fn) int (fn)(ControlProg *cp, SockChan *sock)

/*
 * When the message in sock->nss has been succesfully sent, then a
 * function of the following form is called.
 *
 * Before it is called, rem_writeable_channel(cp, sock) is called to
 * remove the socket from the list of channels to be watched
 * for writeability.
 *
 * Input:
 *  cp    ControlProg *   The control program resource object.
 *  sock     SockChan *   The socket that sent the message.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
#define SOCK_SENT_FN(fn) int (fn)(ControlProg *cp, SockChan *sock)

/*
 * If an I/O error occurs while reading or writing to a socket,
 * or if the socket connection is lost, then a function of the
 * following form is called.
 *
 * Before it is called, close_socket_channel(cp, sock) is called to
 * remove the socket from the list of channels to be watched
 * for I/O and to close the associated file descriptor.
 *
 * Input:
 *  cp    ControlProg *   The control program resource object.
 *  sock     SockChan *   The communication socket that encountered the
 *                        exceptional condition.
 *  cond     SockCond     The type of exceptional condition that was
 *                        encountered.
 */

typedef enum {     /* The type of exceptional condition */
  SOCK_CLOSE,      /* End of file */
  SOCK_ERROR       /* I/O error */
} SockCond;

#define SOCK_COND_FN(fn) void (fn)(ControlProg *cp, SockChan *sock,	\
				   SockCond cond)

/*
 * Each network communications socket is represented by an iterator object
 * of the following type.
 */
struct SockChan {
  ComHeader head;         /* The generic header of a communications channel */
  SOCK_RCVD_FN(*rcvd_fn); /* The method called to process a received message */
  SOCK_SENT_FN(*sent_fn); /* The method called when a message has been sent */
  SOCK_COND_FN(*cond_fn); /* The method called on exceptional conditions */
  sza::array::NetReadStr *nrs;        /* Communications iterator used to read from fd */
  sza::array::NetSendStr *nss;        /* Communications iterator used to write to fd */
};

static SockChan *new_SockChan(ControlProg *cp, void *client_data,
			      long nrs_size, long nss_size,
			      SOCK_RCVD_FN(*rcvd_fn),
			      SOCK_SENT_FN(*sent_fn),
			      SOCK_COND_FN(*cond_fn));
static SockChan *del_SockChan(ControlProg *cp, SockChan *sock);
static int open_socket_channel(ControlProg *cp, SockChan *sock, int fd);
static void close_socket_channel(ControlProg *cp, SockChan *sock,
				 SockCond reason);
static CHAN_IO_FN(sock_read_fn);
static CHAN_IO_FN(sock_write_fn);

/*
 * The following functions add or remove communications channels
 * from the lists of channels to watch for readability and
 * writeability. The add_ functions can be called at any time,
 * but the rem_ functions must only be called by cp_event_loop().
 * add_readable_channel() should be called when a module is ready
 * to receive and process another message from the specified channel.
 * add_writeable_channel() should be called when a message is ready
 * to be sent.
 */
static int add_readable_channel(ControlProg *cp, ComHeader *head);
static int rem_readable_channel(ControlProg *cp, ComHeader *head);
static int add_writable_channel(ControlProg *cp, ComHeader *head);
static int rem_writable_channel(ControlProg *cp, ComHeader *head);

/*
 * List the virtual method functions of an unnamed pipe channel.
 */
typedef struct PipeChan PipeChan;

/*
 * When a complete message has been read into pipe->rbuf, then a
 * function of the following form is called to process the message.
 * Before it is called, rem_readable_channel(cp, pipe) is called to
 * remove the pipe from the list of channels to be watched
 * for further messages.
 *
 * Input:
 *  cp    ControlProg *   The control program resource object.
 *  pipe     PipeChan *   The pipe that received the message.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
#define PIPE_RCVD_FN(fn) int (fn)(ControlProg *cp, PipeChan *pipe)

/*
 * When the message in pipe->wbuf has been succesfully sent, then a
 * function of the following form is called.
 *
 * Input:
 *  cp    ControlProg *   The control program resource object.
 *  pipe     PipeChan *   The pipe that sent the message.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
#define PIPE_SENT_FN(fn) int (fn)(ControlProg *cp, PipeChan *pipe)

/*
 * The following class of object is used to record the state of an
 * unnamed pipe.
 */
struct PipeChan {
  ComHeader head;          /* The generic header of a communications channel */

  sza::util::Pipe* pipe;

  size_t msgsize;          /* The number of bytes per piped message */
  PIPE_RCVD_FN(*rcvd_fn);  /* The method when a new message has been read */
                           /*  into rbuf */
  void *rbuf;              /* A received message */
  PIPE_SENT_FN(*sent_fn);  /* The method called when the contents of */
                           /*  wbuf[] have been sent */
  void *wbuf;              /* A message to be sent */
};

static PipeChan *new_PipeChan(ControlProg *cp, void *client_data,
			      size_t msgsize, PIPE_RCVD_FN(*rcvd_fn),
			      PIPE_SENT_FN(*sent_fn));
static PipeChan *del_PipeChan(ControlProg *cp, PipeChan *pipe);
static CHAN_IO_FN(pipe_read_fn);
static CHAN_IO_FN(pipe_write_fn);

/*
 * Declare a container for a doubly-linked list of communications channel
 * aspects.
 */
typedef struct {
  ComAspect *head;   /* The head of the list */
  ComAspect *tail;   /* The tail of the list */
} ComList;

/*
 * Define an object to record the resources used to communicate with the
 * real-time controller task.
 */
typedef struct {
  int port;            /* The server socket that the real-time controller */
                       /* connects to. */
  SockChan *sock;      /* The network communications object */
  PipeChan *pipe;      /* A queue of messages to be sent to the controller */
  struct {                  /* Use cp_rtc_online() to read from this struct */
                            /* Use record_rtc_status() to write to it */
    pthread_mutex_t guard;  /* A mutual exclusion guard for this struct */
    int guard_ready;        /* True after 'guard' has been initialized */
    int online;             /* True when the controller is connected */
  } status;

} RtController;

static RtController *new_RtController(ControlProg *cp);
static RtController *del_RtController(ControlProg *cp, RtController *rtc);
static int connect_controller(ControlProg *cp, RtController *rtc);
static SOCK_RCVD_FN(rtc_rcvd_fn);
static SOCK_SENT_FN(rtc_sent_fn);
static SOCK_COND_FN(rtc_cond_fn);
static int record_rtc_status(ControlProg *cp, int online);
static int watch_rtc_readability(ControlProg *cp, RtController *rtc);

/*
 * When messages are queued to be written to the real-time controller
 * socket, they are placed in RtController::pipe in the following form.
 */
typedef struct {
  bool deactivateTimeout; // True if the command timeout should be
  // deactivated when this command is sent
  NetCmdId id;      /* The type of network message in 'cmd' */
  RtcNetCmd rtc;    /* The network command to be sent */
} RtcPipeCmd;

static PIPE_RCVD_FN(rtc_pipe_rcvd_fn);

struct arrPtr {
  unsigned char* conf_;
  unsigned int*  nPad_;
  unsigned int*  pad_;
  unsigned int*  antId_;
  unsigned int*  antType_;
  double*        loc_;
};

/*
 * Define an object to record the resources used to communicate with the
 * real-time scanner task.
 */
typedef struct {
  int port;            /* The server socket that the real-time scanner */
                       /* connects to. */
  SockChan *sock;      /* The channel connected to the real-time scanner */
  RegRawData *frame;   /* The latest register frame */

  arrPtr sza_;
  arrPtr carma_;

} RtScanner;

static RtScanner *new_RtScanner(ControlProg *cp, ArrayMap *arraymap);
static RtScanner *del_RtScanner(ControlProg *cp, RtScanner *rts);
static int connect_scanner(ControlProg *cp, RtScanner *rts);
static SOCK_RCVD_FN(rts_rcvd_fn);
static SOCK_SENT_FN(rts_sent_fn);
static SOCK_COND_FN(rts_cond_fn);

/*
 * Define an object to record the resources used to communicate with the
 * optical camera task.
 */
typedef struct {
  int port;            /* The server socket that the optcam task */
                       /* connects to. */
  SockChan *sock;      /* The channel connected to the optcam task */
  unsigned short image[GRABBER_IM_SIZE];   /* The latest image */
} RtOptCam;

static RtOptCam *new_RtOptCam(ControlProg *cp);
static RtOptCam *del_RtOptCam(ControlProg *cp, RtOptCam *rto);
static int connect_optcam(ControlProg *cp, RtOptCam *rto);
static SOCK_RCVD_FN(rto_rcvd_fn);
static SOCK_SENT_FN(rto_sent_fn);
static SOCK_COND_FN(rto_cond_fn);

/*
 * Define an object to record the resources of an external monitor
 * client.
 */
typedef enum {        /* The sequential state of a client connection */
  MC_NOT_SENDING,     /* No client is connected */
  MC_SENDING_SIZE,    /* Currently sending size of send network buffer */
  MC_SENDING_REGMAP,  /* Currently sending the control-program regmap */
  MC_SENDING_REGS     /* Normal operation */
} MonitorState;

typedef struct {
  SockChan *sock;      /* The socket channel of the client */
  MonitorState state;  /* The state of the connection to the client */
  RegSet *regset;      /* A list of the registers being monitored */
  unsigned interval;   /* The sampling interval */
  unsigned dropped;    /* The number of dropped frames since the last send */
  sza::util::NetMonitorFrame* nmf_;
} MonitorClient;
/*
 * Define an object to record the resources of an external iamge monitor
 * client.
 */
typedef enum {         /* The sequential state of a client connection */
  IMC_NOT_SENDING,     /* No client is connected */
  IMC_SENDING_SIZE,    /* Currently sending size of send network buffer */
  IMC_SENDING_IMAGE    /* Normal operation */
} ImMonitorState;

typedef struct {
  SockChan *sock;      /* The socket channel of the client */
  ImMonitorState state;/* The state of the connection to the client */
  unsigned dropped;    /* The number of dropped images since the last send */
} ImMonitorClient;

/*
 * Preset the potential number of monitor clients (and image monitor clients).
 */
#define MONITOR_CLIENTS 20
#define IM_MONITOR_CLIENTS 2

/*
 * Define an object to record the resources used to communicate
 * with external monitor client programs.
 */
typedef struct {
  int port;          /* The TCP/IP port on which to listen for clients */
  long nss_size;     /* The size of the network send buffers */
  long nrs_size;     /* The size of the network read buffers */
  MonitorClient clients[MONITOR_CLIENTS]; /* An array of potential clients */
  sza::util::NetMonitorFrame* nmf_;
} MonitorServer;

static MonitorServer *new_MonitorServer(ControlProg *cp, sza::util::NetMonitorFrame* nmf);
static MonitorServer *new_MonitorServer(ControlProg *cp, ArrayMap *arraymap);
static MonitorServer *del_MonitorServer(ControlProg *cp, MonitorServer *ms);
static SOCK_RCVD_FN(mc_rcvd_fn);
static SOCK_SENT_FN(mc_sent_fn);
static SOCK_COND_FN(mc_cond_fn);
static int connect_monitor_client(ControlProg *cp, MonitorServer *ms);

/*
 * Define an object to record the resources used to communicate
 * with external image monitor client programs.
 */
typedef struct {
  int port;          /* The TCP/IP port on which to listen for clients */
  long nss_size;     /* The size of the network send buffers */
  long nrs_size;     /* The size of the network receive buffers */
  ImMonitorClient clients[IM_MONITOR_CLIENTS]; /* An array of potential clients */
} ImMonitorServer;

static ImMonitorServer *new_ImMonitorServer(ControlProg *cp);
static ImMonitorServer *del_ImMonitorServer(ControlProg *cp, ImMonitorServer *ms);
static SOCK_RCVD_FN(imc_rcvd_fn);
static SOCK_SENT_FN(imc_sent_fn); 
static SOCK_COND_FN(imc_cond_fn);
static int connect_im_monitor_client(ControlProg *cp, ImMonitorServer *ims);

/*
 * Define an object to record the state of an external control client.
 */
typedef struct {
  SockChan *sock;      /* The socket channel of the client */
  PipeChan *reply;     /* The pipe of messages to be sent to the client */
} ControlClient;

static PIPE_RCVD_FN(cc_pipe_rcvd_fn);

/*
 * Preset the potential number of control clients.
 */
#define CONTROL_CLIENTS 20

/*
 * Define an object to encapsulate the resources of the control-client
 * connection.
 */
typedef struct {
  int port;            /* The TCP/IP port on which to listen for clients */
  ControlClient clients[CONTROL_CLIENTS]; /* An array of potential clients */
} ControlServer;

static ControlServer *new_ControlServer(ControlProg *cp);
static ControlServer *del_ControlServer(ControlProg *cp, ControlServer *cs);
static SOCK_RCVD_FN(cc_sock_rcvd_fn);
static SOCK_SENT_FN(cc_sock_sent_fn);
static SOCK_COND_FN(cc_sock_cond_fn);
static int connect_control_client(ControlProg *cp, ControlServer *cs);

/*
 * Functions that control I/O to the scheduler-thread input pipe.
 */
static PIPE_SENT_FN(sent_Scheduler);
static int add_scheduler_channel(ControlProg *cp);
static int rem_scheduler_channel(ControlProg *cp);

/*
 * Functions that control I/O to the navigator-thread input pipe.
 */
static PIPE_SENT_FN(sent_Navigator);
static int add_navigator_channel(ControlProg *cp);
static int rem_navigator_channel(ControlProg *cp);

/*
 * Functions that control I/O to the logger-thread input pipe.
 */
static PIPE_SENT_FN(sent_Logger);
static int add_logger_channel(ControlProg *cp);
static int rem_logger_channel(ControlProg *cp);

static void addPagerRegister(ControlProg* cp, std::string regSpec, 
			     double min, double max, bool delta, 
			     unsigned nFrame, 
			     bool outOfRange, std::string comment);

static void remPagerRegister(ControlProg* cp, std::string regSpec);

static void clearPager(ControlProg* cp);
static void resetPager(ControlProg* cp);
static int enablePager(ControlProg* cp, bool enable);

// Internal methods to respond to messages about the array configuration

static void setArrayConfig(ControlProg* cp, unsigned array, unsigned config);
static void addArrayAntenna(ControlProg* cp, unsigned array, unsigned iPad, 
			    unsigned antType, int iAnt);
static void remArrayAntenna(ControlProg* cp, unsigned array, unsigned iPad);

static void printConfig(sza::util::CarmaConfig* config);

static void writeArrayConfigurations(ControlProg* cp);
static void* findReg(RtScanner* rts, char* regmap, char *board, 
		     char *name, sza::util::DataType::Type type);

/*
 * Produce a table of thread descriptions, indexed by the CpThreadId
 * enumerators defined in szacontrol.h.
 */
typedef struct {
  CpThreadId id;           /* The enumerated thread-table index */
  CP_NEW_FN(*new_fn);      /* The thread resource object constructor function */
  CP_DEL_FN(*del_fn);      /* The thread resource object destructor function */
  CP_THREAD_FN(*start_fn); /* The entry point of the thread */
  CP_STOP_FN(*stop_fn);    /* Send a shutdown message to the thread */
  PIPE_SENT_FN(*sent_fn);  /* The pipe message-sent method */
  size_t sizeof_message;   /* The size of the thread's pipe message object */
} CpThreadType;

static CpThreadType thread_table[] = {
  {CP_SCHEDULER, new_Scheduler, del_Scheduler, scheduler_thread,
   stop_Scheduler, sent_Scheduler, sizeof(SchedulerMessage)},
  {CP_ARCHIVER,  new_Archiver,  del_Archiver,  archiver_thread,
   stop_Archiver,  0,              sizeof(ArchiverMessage)},
  {CP_LOGGER,    new_Logger,    del_Logger,    logger_thread,
   stop_Logger,    sent_Logger,    sizeof(LoggerMessage)},
  {CP_NAVIGATOR, new_Navigator, del_Navigator, navigator_thread,
   stop_Navigator, sent_Navigator, sizeof(NavigatorMessage)},
  {CP_GRABBER,   new_Grabber,   del_Grabber, grabber_thread,
   stop_Grabber,   0,              sizeof(GrabberMessage)},
  {CP_TERM,      new_Term,      del_Term,    term_thread,
   stop_Term,      0,              sizeof(TermMessage)},
};
enum {NTHREAD = sizeof(thread_table)/sizeof(thread_table[0])};

/*
 * Each client thread of the control program is represented by an
 * object of the following type.
 */
typedef struct {
  CpThreadType *type;       /* The associated entry from thread_table[] */
  PipeChan *pipe;           /* The input pipe of the thread */
  void *state;              /* The resource object of the thread */
  pthread_t thread;         /* The pthreads id of the thread */
  int running;              /* True when the thread is running */
} CpThread;

static CpThread *find_CpThread(ControlProg *cp, pthread_t tid);

//-----------------------------------------------------------------------
// A class for managing timeouts
//-----------------------------------------------------------------------

namespace sza {
  namespace array {

    class TimeOut {
    public:

      static const unsigned int defaultInterval_ = 5*60;

      // Public methods

      TimeOut();
      void setIntervalInSeconds(unsigned int seconds);
      void activate(bool active);
      void reset();
      bool pagingAllowed();
      void allowPaging(bool allow);
      void registerDeactivateTimeout(bool deactivate);
      int registerTimeOut(ControlProg* cp);
      struct timeval* tVal();

      bool active_;
      bool resetPending_;
      sza::util::TimeVal rtcCmdTimeOut_; // A timeout that will
					 // indicate no commands have
					 // been sent to the rtc
      bool pagingAllowed_;               // True if the pager can be activated

      // Certain commands are generated automatically (tracking
      // commands, for example) by the control system.  We do not want
      // to reset the timeout just because one of these was
      // successfully sent, but only if a command was sent by the
      // scheduler or manually.  
      //
      // As each command is queued, we will register
      // command was sent.

      bool resetTimeOutOnCurrentCommand_; 
    };

    // Constructor which defaults to an interval, and remains inactive

    TimeOut::TimeOut() {
      resetTimeOutOnCurrentCommand_ = true; 
      setIntervalInSeconds(defaultInterval_);
      activate(false);
      allowPaging(true);
      resetPending_ = true;
    }

    // Set the timeout, in seconds

    void TimeOut::setIntervalInSeconds(unsigned int seconds) {
      rtcCmdTimeOut_.setTime(seconds,0);
      resetPending_ = true;
    }

    // Activate the timeout

    void TimeOut::activate(bool active) {
      active_ = active;
    }

    struct timeval* TimeOut::tVal() {

      if(active_) {

	// If the pending flag was set, reset the timeout now

	if(resetPending_) {
	  rtcCmdTimeOut_.reset();
	  resetPending_ = false;
	}

	return rtcCmdTimeOut_.timeVal();
      } else {
	return NULL;
      }

    }

    // Return true if paging is currently allowed

    bool TimeOut::pagingAllowed() {
      return pagingAllowed_;
    };

    // Set whether or not paging is allowed.

    void TimeOut::allowPaging(bool allow) {
      pagingAllowed_ = allow;
    }

    // Register that a timeout has occurred

    int TimeOut::registerTimeOut(ControlProg* cp) {

      // Send a message to the pager thread, if paging is allowed

      if(pagingAllowed()) {

	// Disallow further paging commands from monitor clients

	sendPagingState(cp, false);

	// And activate the pager

	TermMessage msg;

	if(pack_term_msg_page(&msg, "No commands were sent to the RTC", true) || 
	   send_TermMessage(cp, &msg, PIPE_WAIT)==PIPE_ERROR)
	  return 1;
      } else {
      }

      // And reset the timeout, regardless of which command we timed
      // out on (ie, don't call reset() here, because if the last
      // command was a navigator command, the reset will do nothing,
      // and we will re-enter the select() loop with a zero timeout,
      // which will cause select() to exit immediately; ie, we will
      // enter a polling state.
      
      rtcCmdTimeOut_.reset();

      return 0;
    }

    void TimeOut::reset() {
      if(resetTimeOutOnCurrentCommand_) {
	rtcCmdTimeOut_.reset();
      }
    }

    void TimeOut::registerDeactivateTimeout(bool deactivate) {
      resetTimeOutOnCurrentCommand_ = deactivate;
    }

  }
}

/*
 * Define the object that encapsulates the resources of the
 * control program.
 */
struct ControlProg {
  ArrayMap *arraymap;       /* The register map of the SZA */
  PipeChan *pipe;           /* The communications thread control pipe */
  CpThread thread[NTHREAD]; /* The client threads of the control program */
  RtController *rtc;        /* The connection to the real-time controller */
  RtScanner *rts;           /* The connection to the real-time scanner */
  RtOptCam *rto;            /* The connection to the real-time optical camera
			       task */
  MonitorServer *ms;        /* The monitor-client server */
  ImMonitorServer *ims;     /* The monitor-client image server */
  ControlServer *cs;        /* The control-client server */
  ComList active_list;      /* The list of channels to watch for I/O */
  fd_set read_fds;          /* The set of readable file-descriptors to watch */
  fd_set send_fds;          /* The set of writable file-descriptors to watch */
  int fd_set_size;          /* The size of the read_fds and send_fds sets */
  CpWhatNext whatnext;      /* The continuation status of the event loop */
  sza::util::AntNum defaultAntSet;     // A set of antennas to be used as the
  // default for command which take an
  // optional antenna argument

  sza::util::CarmaConfig* carmaConfig_; // An object for managing the
				        // CARMA configuration

  sza::util::CarmaConfig* szaConfig_;   // An object for managing the
				        // SZA configuration

  // An object for managing timeouts when communicating with the rtc

  sza::array::TimeOut* timeOut_;

  std::string* startupScript_;
};

/*
 * Enumerate control-thread (the main thread) message types.
 */
typedef enum {
  CP_SHUTDOWN_MSG,      /* A request to shutdown the control program */
  CP_RESTART_MSG,       /* A request to restart the control program */
  CP_EXITING_MSG,       /* A thread-exit report */
  CP_INITIALIZED_MSG,   /* The initialization thread has completed, so */
                        /*  start listening for connection requests from */
                        /*  the real-time controller and scanner tasks */
  CP_ADD_PAGER_REG_MSG, // Add a register to the list to be monitored
			// for paging
  CP_REM_PAGER_REG_MSG, // Remove a register from the list for
			// monitoring
  CP_PAGER_CLEAR_MSG,   // Clear the pager list
  CP_PAGER_LIST_MSG,    // List the pager registers
  CP_PAGER_RESET_MSG,   // Reset all pager monitor points
  CP_PAGER_ENABLE_MSG,  // Enable paging
  CP_ARRAY_CONFIG_MSG,  // Set an array configuration
  CP_ADD_ARRAY_ANTENNA_MSG, // Add an antenna to an array configuration
  CP_REM_ARRAY_ANTENNA_MSG, // Remove an antenna from an array configuration

} CpMessageType;

/*
 * Objects of the following form are used to pass messages to the
 * main thread.
 */
typedef struct {
  CpMessageType type;           /* The type of control message */
  union {                       /* The body of the message */

    struct {                    /* type=CP_EXITING_MSG */
      pthread_t tid;            /* The id of the aborting thread */
    } exiting;

    struct {
      char regName[CP_REGNAME_MAX+1];
      double min;
      double max;
      bool delta;
      bool outOfRange;
      unsigned nFrame;
      char comment[CP_REGNAME_MAX+1];
    } addPagerReg;

    struct {
      char regName[CP_REGNAME_MAX+1];
    } remPagerReg;

    struct {
      bool enable;
    } enablePager;

    struct {
      unsigned array;
      unsigned config;
    } arrayConfig;

    struct {
      unsigned array;
      int iPad;
      unsigned antType;
      int iAnt;
    } addArrayAntenna;

    struct {
      unsigned array;
      int iPad;
      int iAnt;
    } remArrayAntenna;

  } body;
} ControlMessage;

static int send_ControlMessage(ControlProg *cp, ControlMessage *msg);
static PIPE_RCVD_FN(cp_pipe_rcvd_fn);

static int bufferLogMsg(ControlProg* cp, SockChan* sock, RtcNetMsg& netmsg);

/*
 * The following functions add or remove a file descriptor from
 * the sets of those to be watched for readability or writeability
 * by select(). For normal channels they are called automatically
 * by add_readable_channel(),rem_readable_channel(),
 * add_writable_channel() and rem_writable_channel() and should
 * not be called directly. For server ports they are called directly.
 */
static void cp_add_read_fd(ControlProg *cp, int fd);
static void cp_rem_read_fd(ControlProg *cp, int fd);
static void cp_add_send_fd(ControlProg *cp, int fd);
static void cp_rem_send_fd(ControlProg *cp, int fd);

/*.......................................................................
 * Allocate and initialize the resources of the control program.
 *
 * Output:
 *  return   ControlProg *  The resource object, or NULL on error.
 */
ControlProg *new_ControlProg(std::string startupScript)
{
  ControlProg *cp;    /* The resource object to be returned */
  int status;         /* The return status of a pthreads function */
  int i;
  /*
   * Allocate the container.
   */
  cp = (ControlProg *) malloc(sizeof(ControlProg));
  if(!cp) {
    lprintf(stderr, "new_ControlProg: Insufficient memory.\n");
    return NULL;
  };
  /*
   * Before attempting any operation that might fail, initialize the
   * container at least up to the point at which it can safely be passed
   * to del_ControlProg().
   */
  cp->arraymap = NULL;
  cp->pipe = NULL;
  for(i=0; i<NTHREAD; i++) {
    CpThread *t = cp->thread + i;
    t->type = thread_table + i;
    t->pipe = NULL;
    t->state = NULL;
    t->running = 0;
  };
  cp->rtc = NULL;
  cp->rts = NULL;
  cp->rto = NULL;
  cp->ms  = NULL;
  cp->ims = NULL;
  cp->cs  = NULL;
  cp->active_list.head = NULL;
  cp->active_list.tail = NULL;
  FD_ZERO(&cp->read_fds);
  FD_ZERO(&cp->send_fds);
  cp->fd_set_size = 0;
  cp->whatnext = CP_CONTINUE;
  
  // Compile the register map of the SZA.

  cp->arraymap = new_SzaArrayMap();
  if(!cp->arraymap)
    return del_ControlProg(cp);
  
  // Create a pipe channel for use in sending shutdown messages.

  cp->pipe = new_PipeChan(cp, NULL, sizeof(ControlMessage), cp_pipe_rcvd_fn, 0);
  if(!cp->pipe || add_readable_channel(cp, &cp->pipe->head))
    return del_ControlProg(cp);
 
  // Create the resources of the control-program threads.

  for(i=0; i<NTHREAD; i++) {
    CpThread *t = cp->thread + i;
    CpThreadType *tt = t->type;
    t->pipe = new_PipeChan(cp, t, tt->sizeof_message, 0, tt->sent_fn);
    if(!t->pipe)
      return del_ControlProg(cp);
    t->state = tt->new_fn(cp, t->pipe->pipe);
    if(!t->state)
      return del_ControlProg(cp);
  };
  
  // Allocate resources for the connection to the real-time controller
  // task.

  cp->rtc = new_RtController(cp);
  if(!cp->rtc)
    return del_ControlProg(cp);
  
  // Allocate resources for the connection to the real-time scanner
  // task.

  cp->rts = new_RtScanner(cp, cp->arraymap);
  if(!cp->rts)
    return del_ControlProg(cp);
  
  // Allocate resources for the connection to the real-time optical
  // camera task.

  cp->rto = new_RtOptCam(cp);
  if(!cp->rto)
    return del_ControlProg(cp);
  
  // Allocate resources for serving monitor clients.  Don't register
  // the monitor server port with select() until we have a connection
  // to the real-time scanner.

  cp->ms = new_MonitorServer(cp, cp->arraymap);
  if(!cp->ms)
    return del_ControlProg(cp);
  /*
   * Allocate resources for serving image monitor clients.
   * Don't register the image monitor server port with select() until
   * we have a connection to the real-time optical camera task.
   */
  cp->ims = new_ImMonitorServer(cp);
  if(!cp->ims)
    return del_ControlProg(cp);
  /*
   * Allocate resources for serving control clients.
   * Don't register the control server port with select() until
   * we have a connection to the real-time controller.
   */
  cp->cs = new_ControlServer(cp);
  if(!cp->cs)
    return del_ControlProg(cp);
  /*
   * When an attempt is made to write to a socket who's connection
   * has been broken, a sigpipe signal is generated. By default this
   * kills the process. Arrange to ignore this signal. nss_send_msg
   * will still detect the error via the return value of write().
   */
  signal(SIGPIPE, SIG_IGN);

  // Set the default antenna set to all antennas

  cp->defaultAntSet.set(sza::util::AntNum::ANTALL);

  // Initialize the timeout

  cp->timeOut_ = new sza::array::TimeOut();

  // Initialize the CARMA configuration object

  cp->carmaConfig_ = new sza::util::CarmaConfig();
  cp->szaConfig_   = new sza::util::CarmaConfig();
 
  // Initialize the startup script

  cp->startupScript_ = new std::string(startupScript);

  // Now that all of the program resources have been created, start
  // the client threads.

  for(i=0; i<NTHREAD; i++) {
    CpThread *t = cp->thread + i;
    CpThreadType *tt = t->type;
    status = pthread_create(&t->thread, NULL, tt->start_fn, t->state);
    if(status) {
      lprintf(stderr, "pthread_create: %s\n", strerror(status));
      return del_ControlProg(cp);
    };

    t->running = 1;
  };


  // Provide lwps for all threads.

#ifdef sparc
  thr_setconcurrency(5);
#endif
  
  // Divert all further lprintf() messages to the logger task.

  if(log_thread_stream(cp, stdout) ||
     log_thread_stream(cp, stderr))
    return del_ControlProg(cp);

  return cp;
}

/*.......................................................................                                     
 * Allocate and initialize the resources of the control program.                                              
 *                                                                                                            
 * Output:                                                                                                    
 *  return   ControlProg *  The resource object, or NULL on error.                                            
 */
ControlProg* new_ControlProg()
{
  ControlProg *cp;    /* The resource object to be returned */
  int i;
  /*                                                                                                          
   * Allocate the container.                                                                                  
   */
  cp = (ControlProg *) malloc(sizeof(ControlProg));
  if(!cp) {
    lprintf(stderr, "new_ControlProg: Insufficient memory.\n");
    return NULL;
  };
  /*                                                                                                          
   * Before attempting any operation that might fail, initialize the                                          
   * container at least up to the point at which it can safely be passed                                      
   * to del_ControlProg().                                                                                    
   */
  cp->arraymap = NULL;
  cp->pipe = NULL;
  for(i=0; i<NTHREAD; i++) {
    CpThread *t = cp->thread + i;
    t->type = thread_table + i;
    t->pipe = NULL;
    t->state = NULL;
    t->running = 0;
  };

  cp->rtc = NULL;
  cp->rto = NULL;
  cp->rts = NULL;
  cp->ims = NULL;
  cp->ms  = NULL;
  cp->cs  = NULL;

  cp->active_list.head = NULL;
  cp->active_list.tail = NULL;
  FD_ZERO(&cp->read_fds);
  FD_ZERO(&cp->send_fds);
  cp->fd_set_size      = 0;
  cp->whatnext = CP_CONTINUE;
  cp->startupScript_   = 0;

  // Compile the register map                                                                                 

  cp->arraymap = new_SzaArrayMap();

  if(!cp->arraymap)
    return del_ControlProg(cp);

  return cp;
}

/*.......................................................................                                     
 * Allocate and initialize the resources of the control program.                                              
 *                                                                                                            
 * Output:                                                                                                    
 *  return   ControlProg *  The resource object, or NULL on error.                                            
 */
ControlProg* new_ControlProgViewerServer()
{
  ControlProg *cp;    /* The resource object to be returned */
  int i;
  
  // Allocate the container.

  cp = (ControlProg *) malloc(sizeof(ControlProg));
  if(!cp) {
    lprintf(stderr, "new_ControlProg: Insufficient memory.\n");
    return NULL;
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_ControlProg().

  cp->arraymap  = NULL;
  cp->pipe      = NULL;
  for(i=0; i<NTHREAD; i++) {
    CpThread *t = cp->thread + i;
    t->type     = thread_table + i;
    t->pipe     = NULL;
    t->state    = NULL;
    t->running  = 0;
  };

  cp->rtc = NULL;
  cp->rto = NULL;
  cp->rts = NULL;
  cp->ims = NULL;
  cp->ms  = NULL;
  cp->cs  = NULL;

  cp->active_list.head = NULL;
  cp->active_list.tail = NULL;

  FD_ZERO(&cp->read_fds);
  FD_ZERO(&cp->send_fds);

  cp->fd_set_size      = 0;
  cp->whatnext         = CP_CONTINUE;
  cp->startupScript_   = 0;

  // Initialize the timeout

  cp->timeOut_ = new sza::array::TimeOut();

  return cp;
}

/*.......................................................................
 * Delete the resources of the control program. Note that the client
 * threads should have been shutdown before calling this function. Any
 * that weren't will be unceremoniously cancelled.
 *
 * Input:
 *  cp     ControlProg *  The resource container to be deleted.
 * Output:
 *  return ControlProg *  The deleted container (ie. NULL).
 */
ControlProg *del_ControlProg(ControlProg *cp)
{
  int i;
  if(cp) {
    int nthread=0;       /* The number of threads being shutdown */
    ControlMessage msg;  /* A message received from another thread */
    /*
     * Tell all running threads to clean up and exit. If a shutdown
     * message can't be delivered to a particular thread, cancel that
     * thread.
     */
    for(i=0; i<NTHREAD; i++) {
      CpThread *t = cp->thread + i;
      if(t->running) {
	if(t->type->stop_fn(cp)) {
	  pthread_cancel(t->thread);
	  pthread_detach(t->thread);
	  t->running = 0;
	} else {
	  nthread++;
	};
      };
    };
    /*
     * Wait for the client threads to finish cleaning up and exit.
     */
    while(nthread) {

      switch(cp->pipe->pipe->read(&msg, sizeof(msg),
				  CP_SHUTDOWN_TIMEOUT*1000)) {

      case PIPE_OK:
	
	// Discard all but thread exit messages.

	if(msg.type == CP_EXITING_MSG) {
	  
	  // Mark the specified thread as shutdown.

	  CpThread *t = find_CpThread(cp, msg.body.exiting.tid);
	  if(t->running) {
	    nthread--;
	    pthread_detach(t->thread);
	    t->running = 0;
	  };
	};
	break;
      case PIPE_BUSY:
	fprintf(stderr, "Timed out waiting for a thread to exit.\n");

        // Note the deliberate lack of a break statement here 

      default:
	
	// Forcibly shutdown the remaining threads.

	for(i=0; i<NTHREAD; i++) {
	  CpThread *t = cp->thread + i;
	  if(t->running) {
	    pthread_cancel(t->thread);
	    pthread_detach(t->thread);
	    t->running = 0;
	  };
	};
	nthread = 0;
	break;
      };
    };
    

    // Destroy the timeout resources

    if(cp->timeOut_) {
      delete cp->timeOut_;
      cp->timeOut_ = 0;
    }

    // Destroy the CARMA configuration resources

    if(cp->carmaConfig_) {
      delete cp->carmaConfig_;
      cp->carmaConfig_ = 0;
    }

    if(cp->szaConfig_) {
      delete cp->szaConfig_;
      cp->szaConfig_ = 0;
    }

    // Destroy the startup script

    if(cp->startupScript_) {
      delete cp->startupScript_;
      cp->startupScript_ = 0;
    }


    // Now destroy the client thread resources.

    for(i=0; i<NTHREAD; i++) {
      CpThread *t = cp->thread + i;
      CpThreadType *tt = t->type;
      t->pipe = del_PipeChan(cp, t->pipe);
      t->state = tt->del_fn(t->state);
    };
    
    // Delete other resources.

    cp->rtc      = del_RtController(cp, cp->rtc);
    cp->rts      = del_RtScanner(cp, cp->rts);
    cp->rto      = del_RtOptCam(cp, cp->rto);
    cp->ms       = del_MonitorServer(cp, cp->ms);
    cp->ims      = del_ImMonitorServer(cp, cp->ims);
    cp->cs       = del_ControlServer(cp, cp->cs);
    cp->arraymap = del_SzaArrayMap(cp->arraymap);
    free(cp);
  };

  return NULL;
}

/*.......................................................................
 * Return the resource object of a given thread.
 *
 * Input:
 *  cp     ControlProg *  The resource container of the control program.
 *  id      CpThreadId    The enumerated index of the thread (see
 *                        szacontrol.h).
 * Output:
 *  return        void *  The resource object, or NULL if unknown.
 */
void *cp_ThreadData(ControlProg *cp, CpThreadId id)
{
  if((int)id < 0 || (int)id >= NTHREAD) {
    lprintf(stderr, "cp_ThreadData: Unknown thread id.\n");
    return NULL;
  };
  return cp->thread[id].state;
}

/*.......................................................................
 * Provide readonly access to the SZA register map to all threads.
 *
 * Input:
 *  cp      ControlProg *  The resource object of the control program.
 * Output:
 *  return       RegMap *  The SZA register map.
 */
ArrayMap *cp_ArrayMap(ControlProg *cp)
{
  return cp->arraymap;
}

/*.......................................................................
 * Provide access to the default antenna set
 *
 * Input:
 *  cp      ControlProg *  The resource object of the control program.
 * Output:
 *  return       AntNum *  The default antenna set
 */
sza::util::AntNum* cp_AntSet(ControlProg *cp)
{
  return &(cp->defaultAntSet);
}

/*.......................................................................
 * This is the event loop of the control program. It only returns if
 * a fatal error occurs or when a shutdown request is received from
 * a control client.
 *
 * Input:
 *  cp    ControlProg *  The resource container of the control program.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
int cp_event_loop(ControlProg *cp)
{
  int waserr = 0;

  // Loop until an error occurs or a shutdown is requested.

  while(!waserr && cp->whatnext==CP_CONTINUE) {
    ComAspect *chan; /* The read or write part of a communications channel */
    int nready;      /* The number of active file descriptors */
    fd_set read_fds = cp->read_fds;
    fd_set send_fds = cp->send_fds;
    /*
     * Wait for activity on any of the file descriptors that are being watched.
     * Note that only one active channel is serviced per return from select().
     * This allows the resulting action functions to add and remove channels
     * from the read and send lists or even to close active channels.
     *
     * The first channel with activity will be serviced each time. When this
     * results in completion of an I/O operation, the channel is removed from
     * the corresponding list before the channel's action function is called.
     * The action function can then add it back to the list if necessary, but
     * since this will result in the channel being placed at the end of the
     * list, channels near the start of the list are prevented from starving
     * channels near the end of the list. This implements a crude form of
     * round-robin scheduling.
     */
    nready = select(cp->fd_set_size, &read_fds, &send_fds, NULL, 
		    cp->timeOut_->tVal());

    switch(nready) {
    case -1:           /* Error */
      perror("cp_event_loop: select error");
      waserr = 1;
      break;
    case 0:
      waserr = cp->timeOut_->registerTimeOut(cp);
      break;
    default:           // One or more fds are ready for I/O 
      
      // Find the first channel (if any) that is ready to be read or
      // written.

      for(chan=cp->active_list.head; chan; chan = chan->next) {
	if(chan->active_set == &cp->read_fds ?
	   FD_ISSET(chan->fd, &read_fds) : FD_ISSET(chan->fd, &send_fds))
	  break;
      };
      
      // If an active channel was found, remove it from the set of
      // channels being watched and call its I/O method. Note that if
      // the I/O method adds the channel back to the set, it will be
      // put at the end of the list. This ensures that no channel can
      // starve out the rest by being lucky enough to be at the front
      // of the list.

      if(chan) {
	if(chan->active_set == &cp->read_fds)
	  rem_readable_channel(cp, chan->parent);
	else
	  rem_writable_channel(cp, chan->parent);
	chan->io_fn(cp, chan->parent);
	
	// Check for connection requests on the control-server port.

      } else if(cp->cs->port >= 0 && FD_ISSET(cp->cs->port, &read_fds)) {
	connect_control_client(cp, cp->cs);
	
	// Check for connection requests on the monitor-server port.

      } else if(cp->ms->port >= 0 && FD_ISSET(cp->ms->port, &read_fds)) {
	connect_monitor_client(cp, cp->ms);
	
	// Check for connection requests on the image monitor-server
	// port.

      } else if(cp->ims->port >= 0 && FD_ISSET(cp->ims->port, &read_fds)) {
	connect_im_monitor_client(cp, cp->ims);
	
	// Check for connection requests on the real-time controller
	// port.

      } else if(cp->rtc->port >= 0 && FD_ISSET(cp->rtc->port, &read_fds)) {
	connect_controller(cp, cp->rtc);
	
	// Check for connection requests on the real-time scanner
	// port.

      } else if(cp->rts->port >= 0 && FD_ISSET(cp->rts->port, &read_fds)) {
	connect_scanner(cp, cp->rts);
	
	// Check for connection requests on the real-time optical
	// camera port.

      } else if(cp->rto->port >= 0 && FD_ISSET(cp->rto->port, &read_fds)) {
	connect_optcam(cp, cp->rto);
      };
    };
  };

  return waserr;
}

/*.......................................................................
 * Initialize the header of a given communication channel. This should only
 * be called by channel constructor functions.
 *
 * Input:
 *  head      ComHeader *  The header to be initialized.
 *  type       ChanType    The type of communication channel, from:
 *                          CHAN_SOCK   - A duplex network socket.
 *                          CHAN_PIPE   - An unnamed pipe.
 *  client_data    void *  The instance data of the channel.
 *  read_fn CHAN_IO_FN(*)  The function to call when the channel is ready
 *                         to be read (This can be 0 if the channel
 *                         won't ever be added to the set of readable
 *                         channels).
 *  write_fn CHAN_IO_FN(*) The function to call when the channel is ready
 *                         to be written. (This can be 0 if the channel
 *                         won't ever be added to the set of readable
 *                         channels).
 */
static void ini_ComHeader(ComHeader *head, ChanType type, void *client_data,
			  CHAN_IO_FN(*read_fn), CHAN_IO_FN(*write_fn))
{
  /*
   * Check arguments.
   */
  if(!head) {
    lprintf(stderr, "ini_ComHeader: NULL header argument.\n");
    return;
  };
  /*
   * There must be at least one I/O method function.
   */
  if(!read_fn && !write_fn)
    lprintf(stderr, "new_ComHeader: Missing method function(s).\n");
  /*
   * Initialize the header.
   */
  head->type = type;
  head->client_data = client_data;
  head->read.parent = (ComHeader *) head;
  head->read.fd = -1;
  head->read.active_set = NULL;
  head->read.io_fn = read_fn;
  head->read.next = NULL;
  head->read.prev = NULL;
  head->write.parent = (ComHeader *) head;
  head->write.fd = -1;
  head->write.active_set = NULL;
  head->write.io_fn = write_fn;
  head->write.next = NULL;
  head->write.prev = NULL;
  return;
}

/*.......................................................................
 * Assign a given fd to a channel. This should be called from channel
 * type-specific functions that acquire the file descriptors. If the
 * channel is already open, the channel will first be removed from the
 * active list of channels.
 *
 * Input:
 *  cp   ControlProg *  The resource object of the control program.
 *  head   ComHeader *  The generic header of the channel.
 *  read_fd      int    The readable file descriptor to be assigned to
 *                      the channel. This should be -1 if the channel
 *                      is closed or unreadable.
 *  write_fd     int    The writable file descriptor to be assigned to
 *                      the channel. This should be -1 if the channel
 *                      is closed or unwritable.
 */
static void set_channel_fds(ControlProg *cp, ComHeader *head, int read_fd,
			    int write_fd)
{
  /*
   * If the channel is open, remove it from the active read and write set.
   */
  if(head->read.fd) {
    rem_readable_channel(cp, head);
    rem_writable_channel(cp, head);
  };
  /*
   * Assign the new fd.
   */
  head->read.fd = read_fd;
  head->write.fd = write_fd;
}

/*.......................................................................
 * Create a socket channel.
 *
 * Input:
 *  cp        ControlProg *  The resource object of the control program.
 *  client_data      void *  Any data that the client wishes to have
 *                           available to the socket's method functions.
 *  nrs_size         long    The size of the network read buffer including
 *                           the normal message prefix (bytes).
 *  nss_size         long    The size of the network send buffer including
 *                           the normal message prefix (bytes).
 *  rcvd_fn  SOCK_RCVD_FN(*) The function to be called when a complete
 *                           message has been received in sock->nrs.
 *  sent_fn  SOCK_SENT_FN(*) The function to be called when the message in
 *                           sock->nss has been sent.
 *  cond_fn  SOCK_COND_FN(*) The function to be called when an error
 *                           condition occurs during I/O.
 * Output:
 *  return       SockChan *  The new socket channel, or NULL on error.
 */
static SockChan *new_SockChan(ControlProg *cp, void *client_data,
			      long nrs_size, long nss_size,
			      SOCK_RCVD_FN(*rcvd_fn),
			      SOCK_SENT_FN(*sent_fn),
			      SOCK_COND_FN(*cond_fn))
{
  SockChan *sock;  /* The object to be returned */
  /*
   * Check arguments.
   */
  if(nrs_size < NET_PREFIX_LEN || nss_size < NET_PREFIX_LEN) {
    lprintf(stderr, "new_SockChan: nrs_size or nss_size < NET_PREFIX_LEN.\n");
    return NULL;
  };
  if(!rcvd_fn || !sent_fn || !cond_fn) {
    lprintf(stderr, "new_SockChan: Missing method function.\n");
    return NULL;
  };
  /*
   * Allocate the container.
   */
  sock = (SockChan* )malloc(sizeof(SockChan));
  if(!sock)
    return NULL;
  /*
   * Before attempting any operation that might fail, initialize the
   * container at least up to the point at which it can safely be
   * passed to del_SockChan().
   */
  ini_ComHeader(&sock->head, CHAN_SOCK, client_data, sock_read_fn,
		sock_write_fn);
  sock->rcvd_fn = rcvd_fn;
  sock->sent_fn = sent_fn;
  sock->cond_fn = cond_fn;
  sock->nrs = NULL;
  sock->nss = NULL;
  /*
   * Allocate the communications input iterator.
   */
  sock->nrs = new_NetReadStr(-1, nrs_size);
  if(!sock->nrs)
    return del_SockChan(cp, sock);
  /*
   * Allocate the communications output iterator.
   */
  sock->nss = new_NetSendStr(-1, nss_size);
  if(!sock->nss)
    return del_SockChan(cp, sock);
  return sock;
}

/*.......................................................................
 * Delete a socket communications channel.
 *
 * Input:
 *  cp    ControlProg *  The resource object of the control program.
 *  sock     SockChan *  The iterator to be deleted.
 * Output:
 *  return   SockChan *  The deleted channel (always NULL).
 */
static SockChan *del_SockChan(ControlProg *cp, SockChan *sock)
{
  if(sock) {
    sock->nrs = del_NetReadStr(sock->nrs);
    sock->nss = del_NetSendStr(sock->nss);
    if(sock->head.read.fd >= 0)
      close(sock->head.read.fd);
    set_channel_fds(cp, &sock->head, -1, -1);
    free(sock);
  };
  return NULL;
}

/*.......................................................................
 * This is the read method function for all socket channels.
 *
 * Input:
 *  cp    ControlProg *   The control program resource object.
 *  head    ComHeader *   The socket channel to be read.
 */
static CHAN_IO_FN(sock_read_fn)
{
  SockChan *sock = (SockChan *) head;
  /*
   * Read the latest bytes of the incoming message into the network
   * read stream.
   */
  switch(nrs_read_msg(sock->nrs)) {
  case sza::array::NetReadStr::NET_READ_SIZE:   /* Message partially read before blocking */
  case sza::array::NetReadStr::NET_READ_DATA:
    add_readable_channel(cp, head);
    break;
  case sza::array::NetReadStr::NET_READ_DONE:   /* Message completely read */
    /*
     * Delegate message processing to a socket-specific method.
     */
    if(sock->rcvd_fn(cp, sock))
      close_socket_channel(cp, sock, SOCK_ERROR);
    break;
  case sza::array::NetReadStr::NET_READ_ERROR:  /* I/O Error */
    close_socket_channel(cp, sock, SOCK_ERROR);
    break;
  case sza::array::NetReadStr::NET_READ_CLOSED: /* Channel closed by peer */
    close_socket_channel(cp, sock, SOCK_CLOSE);
    break;
  };
}

/*.......................................................................
 * This is the read method function for all socket channels.
 *
 * Input:
 *  cp    ControlProg *   The control program resource object.
 *  head    ComHeader *   The socket channel to be read.
 */
static CHAN_IO_FN(sock_write_fn)
{
  SockChan *sock = (SockChan *) head;
  /*
   * Send as much of the current message as possible.
   */
  switch(nss_send_msg(sock->nss)) {
  case sza::array::NetSendStr::NET_SEND_DATA:  /* Message partially sent before
				      blocking */
    add_writable_channel(cp, head);
    break;
  case sza::array::NetSendStr::NET_SEND_DONE:		/* Message completely sent */
    if(sock->sent_fn(cp, sock))
      close_socket_channel(cp, sock, SOCK_ERROR);
    break;
  case sza::array::NetSendStr::NET_SEND_ERROR:		/* I/O Error */
    close_socket_channel(cp, sock, SOCK_ERROR);
    break;
  case sza::array::NetSendStr::NET_SEND_CLOSED:
    close_socket_channel(cp, sock, SOCK_CLOSE);
    break;
  };
}

/*.......................................................................
 * Register a given file-descriptor to a given socket channel.
 *
 * Input:
 *  cp        ControlProg *  The control program resource object.
 *  sock         SockChan *  The socket to open.
 *  fd                int    The new socket file-descriptor.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static int open_socket_channel(ControlProg *cp, SockChan *sock, int fd)
{
  /*
   * Check that the channel isn't already open.
   */
  if(sock->head.read.fd >= 0) {
    lprintf(stderr, "open_socket_channel: Channel already open.\n");
    return 1;
  };
  /*
   * Select non-blocking I/O.
   */
  tcp_set_blocking(fd, 0);
  set_channel_fds(cp, &sock->head, fd, fd);
  attach_NetReadStr(sock->nrs, fd);
  attach_NetSendStr(sock->nss, fd);
  return 0;
}

/*.......................................................................
 * Close the fd of a given communications socket. Note that
 * the cond_fn() method of the socket will be invoked to report the
 * closure.
 *
 * Input:
 *  cp   ControlProg *  The control program resource object.
 *  sock    SockChan *  The communications socket to close.
 *  reason  SockCond    The reason for closing the socket, from:
 *                        SOCK_CLOSE  - End of file.
 *                        SOCK_ERROR  - I/O error.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static void close_socket_channel(ControlProg *cp, SockChan *sock,
				 SockCond reason)
{
  int fd = sock->head.read.fd;
  /*
   * Does the socket need closing?
   */
  if(fd >= 0) {
    /*
     * Close the file-descriptor.
     */
    shutdown(fd, 2);
    close(fd);
    attach_NetReadStr(sock->nrs, -1);
    attach_NetSendStr(sock->nss, -1);
    set_channel_fds(cp, &sock->head, -1, -1);
    /*
     * Report the closure and its reason via the socket condition method.
     */
    if(sock->cond_fn)
      sock->cond_fn(cp, sock, reason);
  };
}

/*.......................................................................
 * Create an unnamed pipe channel.
 *
 * Input:
 *  cp        ControlProg *  The resource object of the control program.
 *  client_data      void *  Any data that the client wishes to have
 *                           available to the queue's method functions.
 *  msgsize        size_t    The size of each pipe message.
 *  rcvd_fn  PIPE_RCVD_FN(*) The function to be called when a new
 *                           message has been received in pipe->rbuf[].
 *  sent_fn  PIPE_SENT_FN(*) The function to be called when the message
 *                           in pipe->wbuf[] has been sent.
 * Output:
 *  return       PipeChan *  The new channel, or NULL on error.
 */
static PipeChan *new_PipeChan(ControlProg *cp, void *client_data,
			      size_t msgsize, PIPE_RCVD_FN(*rcvd_fn),
			      PIPE_SENT_FN(*sent_fn))
{
  PipeChan *pipe;    /* The object to be returned */
  /*
   * Valid message size?
   */
  if(msgsize < 1) {
    lprintf(stderr, "new_PipeChan: Illegal message size.\n");
    return NULL;
  };
  /*
   * Allocate the container.
   */
  pipe = (PipeChan* )malloc(sizeof(PipeChan));
  if(!pipe)
    return NULL;
  /*
   * Before attempting any operation that might fail, initialize the
   * container at least up to the point at which it can safely be
   * passed to del_PipeChan().
   */
  ini_ComHeader(&pipe->head, CHAN_PIPE, client_data, pipe_read_fn,
		pipe_write_fn);
  pipe->pipe = NULL;
  pipe->msgsize = msgsize;
  pipe->rbuf = NULL;
  pipe->rcvd_fn = rcvd_fn;
  pipe->wbuf = NULL;
  pipe->sent_fn = sent_fn;
  
  // Allocate the pipe.
  
  pipe->pipe = new sza::util::PipeQueue();

  if(!pipe->pipe)
    return del_PipeChan(cp, pipe);

  // Allocate the send and receive buffers.

  pipe->rbuf = malloc(msgsize);
  pipe->wbuf = malloc(msgsize);
  if(!pipe->rbuf || !pipe->wbuf) {
    lprintf(stderr, "new_PipeChan: Insufficient memory for I/O buffers.\n");
    return del_PipeChan(cp, pipe);
  };
  
  // Record the select() file-descriptors of the pipe.

  set_channel_fds(cp, &pipe->head, pipe->pipe->readFd(),
		  pipe->pipe->writeFd());

  return pipe;
}

/*.......................................................................
 * Delete a pipe communications channel.
 *
 * Input:
 *  cp    ControlProg *  The resource object of the control program.
 *  pipe     PipeChan *  The iterator to be deleted.
 * Output:
 *  return   PipeChan *  The deleted channel (always NULL).
 */
static PipeChan *del_PipeChan(ControlProg *cp, PipeChan *pipe)
{
  if(pipe) {

    delete pipe->pipe;
    pipe->pipe = 0;

    if(pipe->wbuf)
      free(pipe->wbuf);

    if(pipe->rbuf)
      free(pipe->rbuf);

    set_channel_fds(cp, &pipe->head, -1, -1);

    free(pipe);
  };

  return NULL;
}

/*.......................................................................
 * This is the read method function for all pipe channels.
 *
 * Input:
 *  cp    ControlProg *   The control program resource object.
 *  head    ComHeader *   The pipe channel to be read.
 */
static CHAN_IO_FN(pipe_read_fn)
{
  PipeChan *pc = (PipeChan *) head;

  sza::util::Pipe *pipe = pc->pipe;

  // Attempt to read from the pipe, using non-blocking I/O.
  
  switch(pipe->read(pc->rbuf, pc->msgsize, PIPE_NOWAIT)) {

  case PIPE_OK:                     /* Message read successfully */
    
    // Delegate message processing to a pipe-specific method.

    if(pc->rcvd_fn)
      pc->rcvd_fn(cp, pc);
    break;
  case PIPE_BUSY:   /* Couldn't read without blocking */
    add_readable_channel(cp, head);
    break;
  case PIPE_ERROR:  /* I/O Error */
    break;
  };
}

/*.......................................................................
 * This is the read method function for all pipe channels.
 *
 * Input:
 *  cp    ControlProg *   The control program resource object.
 *  head    ComHeader *   The pipe channel to be written.
 */
static CHAN_IO_FN(pipe_write_fn)
{
  PipeChan *pc = (PipeChan *) head;

  sza::util::Pipe *pipe = pc->pipe;

  // Attempt to send the message, using non-blocking I/O.

  switch(pipe->write(pc->wbuf, pc->msgsize, PIPE_NOWAIT)) {

  case PIPE_OK:                     /* Message written successfully */
    
    // Delegate message-completion actions to a pipe-specific method.

    if(pc->sent_fn)
      pc->sent_fn(cp, pc);
    break;
  case PIPE_BUSY:   /* Couldn't write without blocking */
    add_writable_channel(cp, head);
    break;
  case PIPE_ERROR:  /* I/O Error */
    break;
  };
}

/*.......................................................................
 * Add a given communications channel to the set of channels that are
 * to be watched for readability.
 *
 * Input:
 *  cp    ControlProg *   The control-program resource object.
 *  head    ComHeader *   The communications channel to be watched.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int add_readable_channel(ControlProg *cp, ComHeader *head)
{
  ComAspect *ca = &head->read;
  /*
   * Ignore the call if the channel is already being watched.
   */
  if(!ca->active_set) {
    ComList *list = &cp->active_list;
    /*
     * Make sure that the channel is open and has a read method function.
     */
    if(ca->fd < 0 || !ca->io_fn) {
      lprintf(stderr, "add_readable_channel: Channel closed.\n");
      return 1;
    };
    /*
     * Add the channel to the tail of the channels that are being
     * watched for I/O readiness.
     */
    if(list->head) {
      ca->prev = list->tail;
      ca->prev->next = ca;
      ca->next = NULL;
      list->tail = ca;
    } else {
      list->head = list->tail = ca;
      ca->next = ca->prev = NULL;
    };
    /*
     * Register the read as active.
     */
    cp_add_read_fd(cp, ca->fd);
    ca->active_set = &cp->read_fds;
  };
  return 0;
}

/*.......................................................................
 * Add a given communications channel to the set that are being
 * watched for writeability.
 *
 * Input:
 *  cp    ControlProg *   The control-program resource object.
 *  head    ComHeader *   The communications channel to be watched.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int add_writable_channel(ControlProg *cp, ComHeader *head)
{
  ComAspect *ca = &head->write;
  /*
   * Ignore the call if the channel is already being watched.
   */
  if(!ca->active_set) {
    ComList *list = &cp->active_list;
    /*
     * Make sure that the channel is open and has a write method function.
     */
    if(ca->fd < 0 || !ca->io_fn) {
      lprintf(stderr, "add_writable_channel: Channel closed.\n");
      return 1;
    };
    /*
     * Add the channel to the tail of the channels that are being
     * watched for writeability.
     */
    if(list->head) {
      ca->prev = list->tail;
      ca->prev->next = ca;
      ca->next = NULL;
      list->tail = ca;
    } else {
      list->head = list->tail = ca;
      ca->next = ca->prev = NULL;
    };
    /*
     * Register the send as active.
     */
    cp_add_send_fd(cp, ca->fd);
    ca->active_set = &cp->send_fds;
  };
  return 0;
}


/*.......................................................................
 * Remove a given communications channel from the set that are being
 * watched for readability.
 *
 * Input:
 *  cp    ControlProg *   The control-program resource object.
 *  head    ComHeader *   The communications channel to be removed.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int rem_readable_channel(ControlProg *cp, ComHeader *head)
{
  ComAspect *ca = &head->read;
  /*
   * Ignore the call if the channel has already been removed.
   */
  if(ca->active_set) {
    ComList *list = &cp->active_list;
    /*
     * Relink the active channel-list around the channel.
     */
    if(ca->prev) {
      ca->prev->next = ca->next;
    } else {
      list->head = ca->next;
    };
    if(ca->next) {
      ca->next->prev = ca->prev;
    } else {
      list->tail = ca->prev;
    };
    /*
     * Mark the channel as inactive.
     */
    ca->next = ca->prev = NULL;
    cp_rem_read_fd(cp, ca->fd);
    ca->active_set = NULL;
  };
  return 0;
}

/*.......................................................................
 * Remove a given communications channel from the set that are being watched
 * for writeability.
 *
 * Input:
 *  cp    ControlProg *   The control-program resource object.
 *  head    ComHeader *   The communications channel to be removed.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int rem_writable_channel(ControlProg *cp, ComHeader *head)
{
  ComAspect *ca = &head->write;
  /*
   * Ignore the call if the channel has already been removed.
   */
  if(ca->active_set) {
    ComList *list = &cp->active_list;
    /*
     * Relink the send-list around the channel.
     */
    if(ca->prev) {
      ca->prev->next = ca->next;
    } else {
      list->head = ca->next;
    };
    if(ca->next) {
      ca->next->prev = ca->prev;
    } else {
      list->tail = ca->prev;
    };
    /*
     * Mark the channel as inactive.
     */
    ca->next = ca->prev = NULL;
    cp_rem_send_fd(cp, ca->fd);
    ca->active_set = NULL;
  };
  return 0;
}

/*.......................................................................
 * Add a file descriptor to the set of file descriptors to watch for
 * readability.
 *
 * Input:
 *  cp     ControlProg *   The control-program resource container.
 *  fd             int     The file descriptor to add.
 */
static void cp_add_read_fd(ControlProg *cp, int fd)
{
  FD_SET(fd, &cp->read_fds);
  if(cp->fd_set_size < fd + 1)
    cp->fd_set_size = fd + 1;
}

/*.......................................................................
 * Remove a file descriptor from the set of file descriptors to be
 * watched for readability.
 *
 * Input:
 *  cp     ControlProg *   The control-program resource container.
 *  fd             int     The file descriptor to remove.
 */
static void cp_rem_read_fd(ControlProg *cp, int fd)
{
  FD_CLR(fd, &cp->read_fds);
}

/*.......................................................................
 * Add a file descriptor to the set of file descriptors to watch for
 * readability.
 *
 * Input:
 *  cp     ControlProg *   The control-program resource container.
 *  fd             int     The file descriptor to add.
 */
static void cp_add_send_fd(ControlProg *cp, int fd)
{
  FD_SET(fd, &cp->send_fds);
  if(cp->fd_set_size < fd + 1)
    cp->fd_set_size = fd + 1;
}

/*.......................................................................
 * Remove a file descriptor from the set of file descriptors to be
 * watched for readability.
 *
 * Input:
 *  cp     ControlProg *   The control-program resource container.
 *  fd             int     The file descriptor to remove.
 */
static void cp_rem_send_fd(ControlProg *cp, int fd)
{
  FD_CLR(fd, &cp->send_fds);
}

/*.......................................................................
 * Allocate and initialize the resources of the connection to the
 * real-time scanner task.
 *
 * Input:
 *  cp     ControlProg *   The resource object of the control program.
 *  regmap      RegMap *   The SZA register map.
 * Output:
 *  return   RtScanner *   The resource object, or NULL on error.
 */
static RtScanner *new_RtScanner(ControlProg *cp, ArrayMap *arraymap)
{
  RtScanner *rts;    /* The resource object to be returned */
  /*
   * Allocate the container.
   */
  rts = (RtScanner *) malloc(sizeof(RtScanner));
  if(!rts) {
    lprintf(stderr, "new_RtScanner: Insufficient memory.\n");
    return NULL;
  };
  /*
   * Before attempting any operation that might fail, initialize the
   * container at least up to the point at which it can safely be passed
   * to del_RtScanner().
   */
  rts->port = -1;
  rts->sock = NULL;
  rts->frame = NULL;
  /*
   * Attempt to allocate a TCP/IP port on which to listen for
   * connection requests from the real-time controller.
   */
  rts->port = tcp_server_sock(CP_RTS_PORT, 1);
  if(rts->port == -1)
    return del_RtScanner(cp, rts);
  /*
   * Allocate the socket channel.
   */
  DBPRINT(true, Debug::DEBUG3, "Array map is: " 
	  << arraymap->narchive << " registers long");
  DBPRINT(true, Debug::DEBUG3, "Creating a buffer which is: "
	  << SCAN_BUFF_BYTE_SIZE(arraymap->nByte(true)) << " bytes long");

  rts->sock = new_SockChan(cp, NULL,
			   SCAN_BUFF_BYTE_SIZE(arraymap->nByte(false)),
			   SCAN_MAX_CMD_SIZE,
			   rts_rcvd_fn, rts_sent_fn, rts_cond_fn);
  if(!rts->sock)
    return del_RtScanner(cp, rts);

  // Allocate the raw-data array used to record the latest received
  // register frame.

  rts->frame = new_RegRawData(arraymap, false);

  if(!rts->frame)
    return del_RtScanner(cp, rts);

  // Find pointers to pertinent registers of the array data frame

  rts->sza_.pad_       = 0;
  rts->sza_.antId_     = 0;
  rts->sza_.antType_   = 0;
  rts->sza_.loc_       = 0;

  rts->carma_.pad_     = 0;
  rts->carma_.antId_   = 0;
  rts->carma_.antType_ = 0;
  rts->carma_.loc_     = 0;

  // Find useful registers.

  if(!(rts->carma_.conf_    = (unsigned char*)findReg(rts, "array",  "carma", "config",   DataType::UCHAR))  ||
     !(rts->carma_.nPad_    = (unsigned int*) findReg(rts, "array",  "carma", "nPad",     DataType::UINT))   ||
     !(rts->carma_.pad_     = (unsigned int*) findReg(rts, "array",  "carma", "pad",      DataType::UINT))   ||
     !(rts->carma_.antId_   = (unsigned int*) findReg(rts, "array",  "carma", "antId",    DataType::UINT))   ||
     !(rts->carma_.antType_ = (unsigned int*) findReg(rts, "array",  "carma", "antType",  DataType::UINT))   ||
     !(rts->carma_.loc_     = (double*)       findReg(rts, "array",  "carma", "location", DataType::DOUBLE)) ||
     
     !(rts->sza_.conf_      = (unsigned char*)findReg(rts, "array",  "sza",   "config",   DataType::UCHAR))  ||
     !(rts->sza_.pad_       = (unsigned int*) findReg(rts, "array",  "sza",   "pad",      DataType::UINT))   ||
     !(rts->sza_.antId_     = (unsigned int*) findReg(rts, "array",  "sza",   "antId",    DataType::UINT))   ||
     !(rts->sza_.loc_       = (double*)       findReg(rts, "array",  "sza",   "location", DataType::DOUBLE)))
    
    return del_RtScanner(cp, rts);

  // And set up the handler to be called when a register goes out of
  // range

  rts->frame->pm->setHandler(sendRegPage, (void*)cp);

  return rts;
}

/*.......................................................................
 * Delete the resources of the connection to the real-time scanner
 * task.
 *
 * Input:
 *  cp   ControlProg *   The resource object of the control program.
 *  rts    RtScanner *  The resource container to be deleted.
 * Output:
 *  return RtScanner *  The deleted container (ie. NULL).
 */
static RtScanner *del_RtScanner(ControlProg *cp, RtScanner *rts)
{
  if(rts) {
    if(rts->port >= 0)
      close(rts->port);
    rts->sock = del_SockChan(cp, rts->sock);
    rts->frame = del_RegRawData(rts->frame);
    free(rts);
  };
  return NULL;
}

/*.......................................................................
 * Handle receipt of a new register frame from the real-time scanner.
 */
static SOCK_RCVD_FN(rts_rcvd_fn)
{
  RtScanner *rts;       /* The real-time scanner resource object */
  int opcode;           /* The message-type opcode */
  NetBuf *net;          /* The network read buffer */
  int i;
  static unsigned counter=0;

  counter++;

  // Get the buffer that contains the message.

  net = sock->nrs->net;
  
  // Get the resource object of the real-time scanner.

  rts = cp->rts;

  // Extract the data into the frame buffer.

  if(net_start_get(net, &opcode) ||
     net_inc_nget(net, SCAN_HEADER_PAD) ||
     net_get_char(net, rts->frame->fm->sizeInBytesOfData(),
     		  rts->frame->fm->frame()->
		  getUcharPtr(rts->frame->fm->byteOffsetInFrameOfData())) ||
     net_end_get(net))
    return 1;

  // Fill in array configuration information

  writeArrayConfigurations(cp);

  // Distribute selected registers of the new frame to monitor
  // clients.
  
  for(i=0; i < MONITOR_CLIENTS; i++) {

    MonitorClient *client = cp->ms->clients + i;
    SockChan *client_sock = client->sock;

    if(client_sock->head.read.fd >= 0) {
      
      // If the preceding frame is still being sent, or the sampling
      // interval hasn't been reached, drop the frame.

      if(client_sock->head.write.active_set || client->interval==0 ||
	 client->dropped+1 < client->interval) {
	client->dropped++;
	
	// Pack the frame for transmission.

      } else {
	NetBuf *client_net = client_sock->nss->net;
	if(net_start_put(client_net, MC_REGS_MSG) ||
	   netPutRegs(client->regset, rts->frame, client_net) ||
	   net_end_put(client_net))
	  return 1;
	
	// Register the message to be sent via cp_event_loop().

	if(add_writable_channel(cp, &client_sock->head))
	  return 1;
	
	// Reset the interval frame-counter.

	client->dropped = 0;
      };
    };
  };
  
  // Add the new frame to the current archive-file integration.

  if(arc_integrate_frame(cp, rts->frame))
    return 1;
  
  // And check any registers being monitored for the pager

  rts->frame->pm->checkRegisters();

  // Prepare to receive the next frame.

  if(add_readable_channel(cp, &rts->sock->head))
    return 1;

  return 0;
}

/*.......................................................................
 * Handle completion of sending a message to the real-time scanner.
 */
static SOCK_SENT_FN(rts_sent_fn)
{
  /*
   * The only message that is sent to the real-time scanner is the
   * connection greeting message. Once this has been sent, the socket
   * can be registered for reading frames.
   */
  if(add_readable_channel(cp, &cp->rts->sock->head))
    return 1;
  return 0;
}

/*.......................................................................
 * Handle the receipt of an exceptional condition from the real-time
 * scanner.
 */
static SOCK_COND_FN(rts_cond_fn)
{
  switch(cond) {
  case SOCK_ERROR:
    lprintf(stderr, "Scanner connection to szaTranslator terminated after error.\n");
    break;
  case SOCK_CLOSE:
    lprintf(stderr, "Scanner connection to szaTranslator was closed.\n");
    break;
  };
}

/*.......................................................................
 * This function is called to accept or reject a connection request
 * from the real-time scanner.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 *  rts        RtScanner *   The control-server resource object.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Fatal error.
 */
static int connect_scanner(ControlProg *cp, RtScanner *rts)
{
  SockChan *sock;                /* The communications channel to the scanner */
  struct sockaddr_in client_addr;/* The address of the client */
  int addr_len;                  /* The size of client_addr */
  int fd;                        /* The socket assigned to the client */
  /*
   * Get the scanner communications channel and its socket-specific
   * form.
   */
  sock = rts->sock;
  /*
   * Terminate any existing connection. This allows a dead connection to be
   * terminated and restarted if the real-time CPU reboots unexpectedly.
   */
  if(sock->head.read.fd >= 0) {
    lprintf(stderr, "Closing stale scanner connection.\n");
    close_socket_channel(cp, sock, SOCK_CLOSE);
  };
  /*
   * Allow the caller to connect.
   */
  addr_len = (int)sizeof(client_addr);
  fd = accept(rts->port, (struct sockaddr *) &client_addr, 
	      (socklen_t* )&addr_len);
  if(fd < 0) {
    lprintf(stderr,
	    "connect_scanner: Error accepting TCP/IP connection.\n");
    return 1;
  };
  /*
   * Report the connection for security purposes.
   */
  lprintf(stdout, "Accepted scanner connection from %s.\n",
	  inet_ntoa(client_addr.sin_addr));
  /*
   * Attach the scanner file descriptor to the scanner communications
   * iterator.
   */
  if(open_socket_channel(cp, sock, fd)) {
    shutdown(fd, 2);
    close(fd);
    return 0;
  };
  /*
   * The scanner requires that it be sent a greeting message that
   * contains the RegMap structure revision count and the number of
   * archive registers. If either end has not been recompiled since
   * the register map last changed then one of these numbers will hopefully
   * differ and the scanner will terminate the connection.
   */
  {
    NetBuf *net = sock->nss->net;
    unsigned long arraymap_revision = ARRAYMAP_REVISION;
    unsigned long arraymap_narchive = cp->arraymap->narchive;
    if(net_start_put(net, SCAN_GREETING) ||
       net_put_long(net, 1, &arraymap_revision) ||
       net_put_long(net, 1, &arraymap_narchive) ||
       net_end_put(net))
      return 1;
    
    // Arrange for the message to be sent by cp_event_loop().

    if(add_writable_channel(cp, &sock->head))
      return 1;
  };
  return 0;
}
/*.......................................................................
 * Allocate and initialize the resources of the connection to the
 * real-time optical camera
 *
 * Input:
 *  cp     ControlProg *   The resource object of the control program.
 *  regmap      RegMap *   The SZA register map.
 * Output:
 *  return   RtOptCam *   The resource object, or NULL on error.
 */
static RtOptCam *new_RtOptCam(ControlProg *cp)
{
  RtOptCam *rto;    /* The resource object to be returned */
  /*
   * Allocate the container.
   */
  rto = (RtOptCam *) malloc(sizeof(RtOptCam));
  if(!rto) {
    lprintf(stderr, "new_RtOptCam: Insufficient memory.\n");
    return NULL;
  };
  /*
   * Before attempting any operation that might fail, initialize the
   * container at least up to the point at which it can safely be passed
   * to del_RtOptCam().
   */
  rto->port = -1;
  rto->sock = NULL;
  /*
   * Attempt to allocate a TCP/IP port on which to listen for
   * connection requests from the real-time optical camera task.
   */
  rto->port = tcp_server_sock(CP_RTO_PORT, 1);
  if(rto->port == -1)
    return del_RtOptCam(cp, rto);
  /*
   * Allocate the socket channel.
   */
  rto->sock = new_SockChan(cp, NULL, OPTCAM_BUFF_SIZE,  OPTCAM_MAX_CMD_SIZE, 
			   rto_rcvd_fn, rto_sent_fn, rto_cond_fn);
  if(!rto->sock)
    return del_RtOptCam(cp, rto);
  return rto;
}

/*.......................................................................
 * Delete the resources of the connection to the real-time optical camera
 * task.
 *
 * Input:
 *  cp   ControlProg *   The resource object of the control program.
 *  rto    RtOptCam *  The resource container to be deleted.
 * Output:
 *  return RtOptCam *  The deleted container (ie. NULL).
 */
static RtOptCam *del_RtOptCam(ControlProg *cp, RtOptCam *rto)
{
  if(rto) {
    if(rto->port >= 0)
      close(rto->port);
    rto->sock = del_SockChan(cp, rto->sock);
    free(rto);
  };
  return NULL;
}

/*.......................................................................
 * Handle receipt of a new image from the real-time optical camera task.
 */
static SOCK_RCVD_FN(rto_rcvd_fn)
{
  RtOptCam *rto;       /* The real-time scanner resource object */
  int opcode,i;        /* The message-type opcode */
  NetBuf *net;         /* The network read buffer */
  unsigned long utc[2];  /* The date of the current image */
  signed actual[3];/* The tracker position when the image was taken */
  signed expected[3];/* The tracker position when the image was taken */

  DBPRINT(true, Debug::DEBUG7, "Receiving an image");

  /*
   * Get the buffer that contains the message.
   */
  net = sock->nrs->net;
  /*
   * Get the resource object of the real-time optical camera connection.
   */
  rto = cp->rto;
  /*
   * Extract the data into the image buffer.
   */
  if(net_start_get(net, &opcode) ||
     net_get_long(net, 2, utc) ||
     net_get_long(net, 3, (unsigned long *)actual) ||
     net_get_long(net, 3, (unsigned long *)expected) ||
     net_get_short(net, GRABBER_IM_SIZE, rto->image) ||
     net_end_get(net))
    return 1;

  DBPRINT(true, Debug::DEBUG7, "Finished extracting");

  /*
   * Distribute selected registers of the new frame to monitor
   * clients.
   */
  for(i=0; i<IM_MONITOR_CLIENTS; i++) {
    ImMonitorClient *client = cp->ims->clients + i;
    SockChan *client_sock = client->sock;

    if(client_sock->head.read.fd >= 0) {
      /*
       * If the preceding image is still being sent, drop the image.
       */
      if(client_sock->head.write.active_set) {
	client->dropped++;

	DBPRINT(true, Debug::DEBUG7, "Image dropped");
	/*
	 * Pack the image for transmission.
	 */
      } else {
	NetBuf *client_net = client_sock->nss->net;

	DBPRINT(true, Debug::DEBUG7, "Packing an image");

	if(net_start_put(client_net, IMC_IMAGE_MSG) ||
	   net_put_short(client_net, GRABBER_IM_SIZE, rto->image) ||
	   net_end_put(client_net))
	  return 1;
	/*
	 * Register the message to be sent via cp_event_loop().
	 */
	if(add_writable_channel(cp, &client_sock->head))
	  return 1;
	/*
	 * Reset the image counter.
	 */
	client->dropped = 0;
      }
    }
  }

  DBPRINT(true, Debug::DEBUG7, "No clients");

  /*
   * Save the new image to disk (if requested).
   */
  if(grabber_save_image(cp, rto->image, utc, actual, expected))
    return 1;
  /*
   * Prepare to receive the next image
   */
  if(add_readable_channel(cp, &rto->sock->head))
    return 1;
  return 0;
}

/*.......................................................................
 * Handle completion of sending a message to the real-time optical camera
 * task.
 */
static SOCK_SENT_FN(rto_sent_fn)
{
  /*
   * The only message that is sent to the optical camera task is the
   * connection greeting message. Once this has been sent, the socket
   * can be registered for reading images
   */
  if(add_readable_channel(cp, &cp->rto->sock->head))
    return 1;
  return 0;
}

/*.......................................................................
 * Handle the receipt of an exceptional condition from the real-time
 * optical camera task.
 */
static SOCK_COND_FN(rto_cond_fn)
{
  switch(cond) {
  case SOCK_ERROR:
    lprintf(stderr, "Optical camera connection terminated after error.\n");
    break;
  case SOCK_CLOSE:
    lprintf(stderr, "The optical camera task disconnected.\n");
    break;
  };
}

/*.......................................................................
 * This function is called to accept or reject a connection request
 * from the real-time optical camera task.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 *  rto        RtOptCam  *   The control-server resource object.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Fatal error.
 */
static int connect_optcam(ControlProg *cp, RtOptCam *rto)
{
  SockChan *sock;                /* The communications channel to the optcam 
				    task */
  struct sockaddr_in client_addr;/* The address of the client */
  int addr_len;                  /* The size of client_addr */
  int fd;                        /* The socket assigned to the client */
  /*
   * Get the optical camera communications channel and its socket-specific
   * form.
   */
  sock = rto->sock;
  /*
   * Terminate any existing connection. This allows a dead connection to be
   * terminated and restarted if the real-time CPU reboots unexpectedly.
   */
  if(sock->head.read.fd >= 0) {
    lprintf(stderr, "Closing stale optical camera connection.\n");
    close_socket_channel(cp, sock, SOCK_CLOSE);
  };
  /*
   * Allow the caller to connect.
   */
  addr_len = sizeof(client_addr);
  fd = accept(rto->port, (struct sockaddr *) &client_addr, 
	      (socklen_t* )&addr_len);
  if(fd < 0) {
    lprintf(stderr,
	    "connect_optcam: Error accepting TCP/IP connection.\n");
    return 1;
  };
  /*
   * Report the connection for security purposes.
   */
  lprintf(stdout, "Accepted frame grabber connection from %s.\n",
	  inet_ntoa(client_addr.sin_addr));
  /*
   * Attach the optcam file descriptor to the optcam communications
   * iterator.
   */
  if(open_socket_channel(cp, sock, fd)) {
    shutdown(fd, 2);
    close(fd);
    return 0;
  };
  /*
   * The optical camera task requires that it be sent a greeting message.
   */
  {
    NetBuf *net = sock->nss->net;
    if(net_start_put(net, OPTCAM_GREETING) ||
       net_end_put(net))
      return 1;
    /*
     * Arrange for the message to be sent by cp_event_loop().
     */
    if(add_writable_channel(cp, &sock->head))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Allocate and initialize the resources of the monitor-server.
 *
 * Constructor for use with NetMonitorFrame object
 */
static MonitorServer *new_MonitorServer(ControlProg *cp, sza::util::NetMonitorFrame* nmf)
{
  MonitorServer *ms;    /* The resource object to be returned */
  int i;
  
  // Allocate the container.

  ms = (MonitorServer *) malloc(sizeof(MonitorServer));
  if(!ms) {
    lprintf(stderr, "new_MonitorServer: Insufficient memory.\n");
    return NULL;
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_MonitorServer().

  ms->port = -1;
  ms->nss_size = 0;
  ms->nrs_size = 0;
  for(i=0; i<MONITOR_CLIENTS; i++) {
    MonitorClient *client = ms->clients + i;
    client->sock     = NULL;
    client->state    = MC_NOT_SENDING;
    client->regset   = NULL;
    client->interval = 0;
    client->dropped  = 0;
    client->nmf_     = nmf;
  };
  
  // Attempt to allocate a TCP/IP port on which to listen for external
  // monitor clients.

  COUT("Listening on port (2)" << CP_MONITOR_PORT);
  ms->port = tcp_server_sock(CP_MONITOR_PORT, 1);
  if(ms->port == -1)
    return del_MonitorServer(cp, ms);
  
  // Initialize per-client resources.

  for(i=0; i<MONITOR_CLIENTS; i++) {
    MonitorClient *client = ms->clients + i;
    
    // Allocate the object that maintains a list of the registers that
    // the client has expressed interest in.

    ms->nmf_ = nmf;

    client->regset = new_RegSet(nmf->getArrayMap());
    if(!client->regset)
      return del_MonitorServer(cp, ms);
    
    // Determine the required network buffer sizes as soon as the
    // first register set is constructed.

    if(i==0) {
      long size1 = net_ArrayTemplate_size(nmf->getArrayTemplate());
      long size2 = net_regs_size(client->regset);

      ms->nss_size = NET_PREFIX_LEN + (size1 > size2 ? size1:size2);
      ms->nrs_size = NET_PREFIX_LEN + net_RegSet_size(client->regset);
    };
    
    // Allocate the client communications iterator.

    client->sock = new_SockChan(cp, client, ms->nrs_size, ms->nss_size,
				mc_rcvd_fn, mc_sent_fn, mc_cond_fn);
    if(!client->sock)
      return del_MonitorServer(cp, ms);
  };
  
  // Allow monitor clients to connect.

  cp_add_read_fd(cp, ms->port);
  return ms;
}

static MonitorServer *new_MonitorServer(ControlProg *cp, ArrayMap *arraymap)
{
  MonitorServer *ms;    /* The resource object to be returned */
  int i;
  /*
   * Allocate the container.
   */
  ms = (MonitorServer *) malloc(sizeof(MonitorServer));
  if(!ms) {
    lprintf(stderr, "new_MonitorServer: Insufficient memory.\n");
    return NULL;
  };
  /*
   * Before attempting any operation that might fail, initialize the
   * container at least up to the point at which it can safely be passed
   * to del_MonitorServer().
   */
  ms->port     = -1;
  ms->nss_size = 0;
  ms->nrs_size = 0;
  ms->nmf_     = 0;

  for(i=0; i<MONITOR_CLIENTS; i++) {
    MonitorClient *client = ms->clients + i;
    client->sock     = NULL;
    client->state    = MC_NOT_SENDING;
    client->regset   = NULL;
    client->interval = 0;
    client->dropped  = 0;
    client->nmf_     = 0;
  };
  /*
   * Attempt to allocate a TCP/IP port on which to listen for external
   * monitor clients.
   */
  COUT("Listening on port " << CP_MONITOR_PORT);
  ms->port = tcp_server_sock(CP_MONITOR_PORT, 1);
  if(ms->port == -1)
    return del_MonitorServer(cp, ms);
  /*
   * Initialize per-client resources.
   */
  for(i=0; i<MONITOR_CLIENTS; i++) {
    MonitorClient *client = ms->clients + i;
    /*
     * Allocate the object that maintains a list of the registers
     * that the client has expressed interest in.
     */
    client->regset = new_RegSet(arraymap);
    if(!client->regset)
      return del_MonitorServer(cp, ms);
    /*
     * Determine the required network buffer sizes as soon as the
     * first register set is constructed.
     */
    if(i==0) {
      long size1 = net_SzaArrayMap_size();
      long size2 = net_regs_size(client->regset);
      ms->nss_size = NET_PREFIX_LEN + (size1 > size2 ? size1:size2);
      ms->nrs_size = NET_PREFIX_LEN + net_RegSet_size(client->regset);
    };
    /*
     * Allocate the client communications iterator.
     */
    client->sock = new_SockChan(cp, client, ms->nrs_size, ms->nss_size,
				mc_rcvd_fn, mc_sent_fn, mc_cond_fn);
    if(!client->sock)
      return del_MonitorServer(cp, ms);
  };
  /*
   * Allow monitor clients to connect.
   */
  cp_add_read_fd(cp, ms->port);
  return ms;
}

/*.......................................................................
 * Delete the resources of the monitor-server gateway.
 *
 * Input:
 *  cp       ControlProg *  The resource object of the control program.
 *  ms     MonitorServer *  The resource container to be deleted.
 * Output:
 *  return MonitorServer *  The deleted container (ie. NULL).
 */
static MonitorServer *del_MonitorServer(ControlProg *cp, MonitorServer *ms)
{
  int i;
  if(ms) {
    if(ms->port >= 0)
      close(ms->port);
    for(i=0; i<MONITOR_CLIENTS; i++) {
      MonitorClient *client = ms->clients + i;
      client->regset = del_RegSet(client->regset);
      client->sock = del_SockChan(cp, client->sock);
    };
    free(ms);
  };
  return NULL;
}

/*.......................................................................
 * Process a message received from a monitor client.
 */
static SOCK_RCVD_FN(mc_rcvd_fn)
{
  COUT("Inside mc_rcvd_fn**************************************");

  NetBuf *net;            /* The network buffer that contains the message */
  int opcode;             /* The message-type enumerator */
  MonitorClient *client;  /* The associated monitor client resource object */
  /*
   * Get the associated client object.
   */
  client = (MonitorClient *) sock->head.client_data;
  net = sock->nrs->net;
  /*
   * Get the message type.
   */
  if(net_start_get(net, &opcode))
    return 1;
  /*
   * Unpack the specified message type.
   */
  switch(opcode) {
  case MC_REGSET_MSG:  /* A new register selection set */
    COUT("Inside mc_rcvd_fn: got a REGSET MSG******************************");
    if(net_get_RegSet(client->regset, net) ||
       net_end_get(net))
      return 1;
    break;
  case MC_INTERVAL_MSG: /* A change in sampling interval */
    {
      unsigned long interval;
      if(net_get_long(net, 1, &interval) ||
	 net_end_get(net))
	return 1;
      client->interval = interval;
    };
    break;
  default:
    break;
  };
  /*
   * Prepare for subsequent messages.
   */
  if(add_readable_channel(cp, &sock->head))
    return 1;
  return 0;
}

/*.......................................................................
 * Handle the completion of sending a message to a monitor client.
 */
static SOCK_SENT_FN(mc_sent_fn)
{
  MonitorClient *client = (MonitorClient* )sock->head.client_data;
  NetBuf *net;   /* The output network buffer */
  /*
   * Get the network buffer of the output stream.
   */
  net = sock->nss->net;
  /*
   * After opening a connection to a monitor client a number of
   * messages are sent to set up the connection. After each has
   * been sent the next one should be loaded into the network
   * buffer and queued to be sent to the client.
   */
  switch(client->state) {
  case MC_NOT_SENDING:     /* No client is connected */
    lprintf(stderr, "mc_sent_fn: Corrupt control-client state.\n");
    return 1;
    break;
  case MC_SENDING_SIZE:    /* The size of the send buffer has been sent */
    
    // Pack the local register map into the output network buffer.
    // 
    // If our frame originates from a NetMonitorFrame server, use the
    // array template of the frame that was received from the server, else
    // default to the system array template

    if(client->nmf_) {

      if(net_start_put(net, MC_REGMAP_MSG) ||
	 net_put_ArrayTemplate(client->nmf_->getArrayTemplate(), net) ||
	 net_end_put(net))
	return 1;

    } else {

      if(net_start_put(net, MC_REGMAP_MSG) ||
	 net_put_SzaArrayMap(net) ||
	 net_end_put(net))
	return 1;

    }
    
    // Arrange for the regmap message to be sent to the client.

    if(add_writable_channel(cp, &sock->head))
      return 1;
    client->state = MC_SENDING_REGMAP;
    break;
  case MC_SENDING_REGMAP:  /* The local register map has been sent */
    /*
     * The connection has now been set up, so switch to the normal state in
     * which selected registers are sent to the client whenever a new
     * register frame is received from the real-time scanner.
     */
    client->state = MC_SENDING_REGS;
    /*
     * Also prepare to read updates to the register set.
     */
    if(add_readable_channel(cp, &sock->head))
      return 1;
    break;
  case MC_SENDING_REGS:  /* Normal operation */
    break;
  };
  return 0;
}

/*.......................................................................
 * Handle an error condition on a monitor-client socket
 */
static SOCK_COND_FN(mc_cond_fn)
{
  MonitorClient *client = (MonitorClient *) sock->head.client_data;
  switch(cond) {
  case SOCK_ERROR:
    lprintf(stderr, "Monitor client connection terminated after error.\n");
    break;
  case SOCK_CLOSE:
    lprintf(stderr, "A monitor client disconnected.\n");
    break;
  };
  client->state = MC_NOT_SENDING;
  renew_RegSet(client->regset);
  client->interval = 0;
  client->dropped = 0;
}

/*.......................................................................
 * This function is called to accept or reject a connection request
 * on the monitor-server port.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 *  ms     MonitorServer *   The monitor-server resource object.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Fatal error.
 */
static int connect_monitor_client(ControlProg *cp, MonitorServer *ms)
{
  struct sockaddr_in client_addr;/* The address of the client */
  int addr_len;                  /* The size of client_addr */
  MonitorClient *client;         /* The monitor socket assigned to client */
  int fd;                        /* The socket assigned to the client */
  int i;
  /*
   * Allow the caller to connect.
   */
  addr_len = sizeof(client_addr);
  fd = accept(ms->port, (struct sockaddr *) &client_addr, 
	      (socklen_t* )&addr_len);
  if(fd < 0) {
    lprintf(stderr,
	    "connect_monitor_client: Error accepting TCP/IP client.\n");
    return 1;
  };
  /*
   * Attempt to find a free monitor-client socket.
   */
  client = NULL;
  for(i=0; i<MONITOR_CLIENTS; i++) {
    MonitorClient *mc = ms->clients + i;
    if(mc->sock->head.read.fd < 0) {
      client = mc;
      break;
    };
  };
  /*
   * If all monitor-client sockets are in use, reject the connection
   * by closing its socket.
   */
  if(!client) {
    shutdown(fd, 2);
    close(fd);
    lprintf(stderr, "Rejected monitor client from %s (too many clients).\n",
	    inet_ntoa(client_addr.sin_addr));
    return 0;
  };
  /*
   * Report the connection for security purposes.
   */
  lprintf(stdout, "Accepted monitor-client connection from %s.\n",
	  inet_ntoa(client_addr.sin_addr));
  /*
   * Attach the client file descriptor to the monitor-client communications
   * iterator.
   */
  if(open_socket_channel(cp, client->sock, fd)) {
    shutdown(fd, 2);
    close(fd);
    return 0;
  };
  /*
   * Arrange for a message to be sent to the client telling it the size
   * of the socket.
   */
  {
    SockChan *sock = client->sock;
    NetBuf *net = sock->nss->net;
    long unsigned int size[2];
    size[0] = sock->nss->net->size;
    size[1] = sock->nrs->net->size;
    if(net_start_put(net, MC_SIZE_MSG) ||
       net_put_long(net, 2, size) ||
       net_end_put(net) ||
       add_writable_channel(cp, &sock->head)) {
      cc_sock_cond_fn(cp, sock, SOCK_ERROR);
      return 0;
    };
    client->state = MC_SENDING_SIZE;
  };
  return 0;
}
/*.......................................................................
 * Allocate and initialize the resources of the monitor-image server.
 *
 * Input:
 *  cp     ControlProg *   The resource object of the control program.
 *  regmap         RegMap *  The SZA register map.
 * Output:
 *  return  MonitorServer *  The resource object, or NULL on error.
 */
static ImMonitorServer *new_ImMonitorServer(ControlProg *cp)
{
  ImMonitorServer *ims;    /* The resource object to be returned */
  int i;
  /*
   * Allocate the container.
   */
  ims = (ImMonitorServer *) malloc(sizeof(ImMonitorServer));
  if(!ims) {
    lprintf(stderr, "new_ImMonitorServer: Insufficient memory.\n");
    return NULL;
  };
  /*
   * Before attempting any operation that might fail, initialize the
   * container at least up to the point at which it can safely be passed
   * to del_ImMonitorServer().
   */
  ims->port = -1;
  ims->nss_size = 0;
  for(i=0; i<IM_MONITOR_CLIENTS; i++) {
    ImMonitorClient *client = ims->clients + i;
    client->sock = NULL;
    client->state = IMC_NOT_SENDING;
    client->dropped = 0;
  };
  /*
   * Attempt to allocate a TCP/IP port on which to listen for external
   * monitor clients.
   */
  ims->port = tcp_server_sock(CP_IM_MONITOR_PORT, 1);
  if(ims->port == -1)
    return del_ImMonitorServer(cp, ims);
  /*
   * Initialize per-client resources.
   */
  ims->nss_size = NET_PREFIX_LEN + GRABBER_IM_SIZE*2;
  ims->nrs_size = NET_PREFIX_LEN;
  for(i=0; i<IM_MONITOR_CLIENTS; i++) {
    ImMonitorClient *client = ims->clients + i;
    /*
     * Allocate the client communications iterator.
     */
    client->sock = new_SockChan(cp, client, ims->nrs_size, ims->nss_size,
				imc_rcvd_fn, imc_sent_fn, imc_cond_fn);
    if(!client->sock)
      return del_ImMonitorServer(cp, ims);
  };
  /*
   * Allow monitor clients to connect.
   */
  cp_add_read_fd(cp, ims->port);
  return ims;
}

/*.......................................................................
 * Delete the resources of the monitor-image server gateway.
 *
 * Input:
 *  cp       ControlProg *  The resource object of the control program.
 *  ims     ImMonitorServer *  The resource container to be deleted.
 * Output:
 *  return ImMonitorServer *  The deleted container (ie. NULL).
 */
static ImMonitorServer *del_ImMonitorServer(ControlProg *cp, ImMonitorServer *ims)
{
  int i;
  if(ims) {
    if(ims->port >= 0)
      close(ims->port);
    for(i=0; i<IM_MONITOR_CLIENTS; i++) {
      ImMonitorClient *client = ims->clients + i;
      client->sock = del_SockChan(cp, client->sock);
    };
    free(ims);
  };
  return NULL;
}

/*.......................................................................
 * Process a message received from an image monitor client.
 */
static SOCK_RCVD_FN(imc_rcvd_fn)
{
  NetBuf *net;            /* The network buffer that contains the message */
  int opcode;             /* The message-type enumerator */
  ImMonitorClient *client;/* The associated monitor client resource object */
  /*
   * Get the associated client object.
   */
  client = (ImMonitorClient *) sock->head.client_data;
  net = sock->nrs->net;
  /*
   * Get the message type.
   */
  if(net_start_get(net, &opcode))
    return 1;
  /*
   * Unpack the specified message type.
   */
  switch(opcode) {
  default:
    lprintf(stderr,"imc_rcvd_fn: Unrecognized message received.\n");
    break;
  };
  /*
   * Prepare for subsequent messages.
   */
  if(add_readable_channel(cp, &sock->head))
    return 1;
  return 0;
}

/*.......................................................................
 * Handle the completion of sending a message to a monitor image client.
 */
static SOCK_SENT_FN(imc_sent_fn)
{
  ImMonitorClient *client = (ImMonitorClient* )sock->head.client_data;
  NetBuf *net;   /* The output network buffer */
  /*
   * Get the network buffer of the output stream.
   */
  net = sock->nss->net;
  /*
   * After opening a connection to a monitor client a number of
   * messages are sent to set up the connection. After each has
   * been sent the next one should be loaded into the network
   * buffer and queued to be sent to the client.
   */
  switch(client->state) {
  case IMC_NOT_SENDING:     /* No client is connected */
    lprintf(stderr, "imc_sent_fn: Corrupt control-client state.\n");
    return 1;
    break;
  case IMC_SENDING_SIZE:    /* The size of the send buffer has been sent */
    /*
     * The connection has now been set up, so switch to the normal state in
     * which frame grabber images are sent to the client whenever a new
     * image is received from the real-time optical camera task.
     */
    client->state = IMC_SENDING_IMAGE;
    break;
  case IMC_SENDING_IMAGE:  /* Normal operation */
    break;
  };
  return 0;
}

/*.......................................................................
 * Handle an error condition on an image monitor-client socket
 */
static SOCK_COND_FN(imc_cond_fn)
{
  ImMonitorClient *client = (ImMonitorClient *) sock->head.client_data;
  switch(cond) {
  case SOCK_ERROR:
    lprintf(stderr, "Image monitor client connection terminated after error.\n");
    break;
  case SOCK_CLOSE:
    lprintf(stderr, "An image monitor client disconnected.\n");
    break;
  };
  client->state = IMC_NOT_SENDING;
  client->dropped = 0;
}

/*.......................................................................
 * This function is called to accept or reject a connection request
 * on the monitor-image server port.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 *  ims     ImMonitorServer *   The monitor-server resource object.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Fatal error.
 */
static int connect_im_monitor_client(ControlProg *cp, ImMonitorServer *ims)
{
  struct sockaddr_in client_addr;/* The address of the client */
  int addr_len;                  /* The size of client_addr */
  ImMonitorClient *client;       /* The monitor socket assigned to client */
  int fd;                        /* The socket assigned to the client */
  int i;
  /*
   * Allow the caller to connect.
   */
  addr_len = sizeof(client_addr);
  fd = accept(ims->port, (struct sockaddr *) &client_addr, 
	      (socklen_t* )&addr_len);
  if(fd < 0) {
    lprintf(stderr,
	    "connect_monitor_im_client: Error accepting TCP/IP client.\n");
    return 1;
  };
  /*
   * Attempt to find a free monitor-image client socket.
   */
  client = NULL;
  for(i=0; i<IM_MONITOR_CLIENTS; i++) {
    ImMonitorClient *imc = ims->clients + i;
    if(imc->sock->head.read.fd < 0) {
      client = imc;
      break;
    };
  };
  /*
   * If all monitor-image client sockets are in use, reject the connection
   * by closing its socket.
   */
  if(!client) {
    shutdown(fd, 2);
    close(fd);
    lprintf(stderr, "Rejected monitor image client from %s (too many clients).\n",
	    inet_ntoa(client_addr.sin_addr));
    return 0;
  };
  /*
   * Report the connection for security purposes.
   */
  lprintf(stdout, "Accepted monitor-image client connection from %s.\n",
	  inet_ntoa(client_addr.sin_addr));
  /*
   * Attach the client file descriptor to the monitor-client communications
   * iterator.
   */
  if(open_socket_channel(cp, client->sock, fd)) {
    shutdown(fd, 2);
    close(fd);
    return 0;
  };
  /*
   * Arrange for a message to be sent to the client telling it the size
   * of the socket.
   */
  {
    SockChan *sock = client->sock;
    NetBuf *net = sock->nss->net;
    unsigned long int size[2];
    size[0] = sock->nss->net->size;
    size[1] = sock->nrs->net->size;
    if(net_start_put(net, IMC_SIZE_MSG) ||
       net_put_long(net, 2, size) ||
       net_end_put(net) ||
       add_writable_channel(cp, &sock->head) ||
       add_readable_channel(cp, &sock->head)) {
      cc_sock_cond_fn(cp, sock, SOCK_ERROR);
      return 0;
    };
    client->state = IMC_SENDING_SIZE;
  };
  return 0;
}

/*.......................................................................
 * Allocate and initialize the resources of the control-server gateway.
 *
 * Input:
 *  cp         ControlProg *  The resource object of the control program.
 * Output:
 *  return   ControlServer *  The resource object, or NULL on error.
 */
static ControlServer *new_ControlServer(ControlProg *cp)
{
  ControlServer *cs;    /* The resource object to be returned */
  int i;
  /*
   * Allocate the container.
   */
  cs = (ControlServer *) malloc(sizeof(ControlServer));
  if(!cs) {
    lprintf(stderr, "new_ControlServer: Insufficient memory.\n");
    return NULL;
  };
  /*
   * Before attempting any operation that might fail, initialize the
   * container at least up to the point at which it can safely be passed
   * to del_ControlServer().
   */
  cs->port = -1;
  for(i=0; i<CONTROL_CLIENTS; i++) {
    ControlClient *client = cs->clients + i;
    client->sock = NULL;
    client->reply = NULL;
  };
  /*
   * Attempt to allocate a TCP/IP port on which to listen for external
   * control clients.
   */
  cs->port = tcp_server_sock(CP_CONTROL_PORT, 1);
  if(cs->port == -1)
    return del_ControlServer(cp, cs);
  /*
   * Initialize per-client resources.
   */
  for(i=0; i<CONTROL_CLIENTS; i++) {
    ControlClient *client = cs->clients + i;
    /*
     * Allocate the client socket channel.
     */
    client->sock = new_SockChan(cp, client,
				NET_PREFIX_LEN + net_max_obj_size(&cc_cmd_table),
				NET_PREFIX_LEN + net_max_obj_size(&cc_msg_table),
				cc_sock_rcvd_fn, cc_sock_sent_fn, cc_sock_cond_fn);
    if(!client->sock)
      return del_ControlServer(cp, cs);
    /*
     * Allocate the pipe that is used to queue replies to be sent on the
     * client socket.
     */
    client->reply = new_PipeChan(cp, client, sizeof(CcPipeMsg),
				 cc_pipe_rcvd_fn, 0);
    if(!client->reply)
      return del_ControlServer(cp, cs);
  };
  /*
   * Allow control clients to connect.
   */
  cp_add_read_fd(cp, cs->port);
  return cs;
}

/*.......................................................................
 * Delete the resources of the control-server gateway.
 *
 * Input:
 *  cp       ControlProg *  The resource object of the control program.
 *  cs     ControlServer *  The resource container to be deleted.
 * Output:
 *  return ControlServer *  The deleted container (ie. NULL).
 */
static ControlServer *del_ControlServer(ControlProg *cp, ControlServer *cs)
{
  int i;
  if(cs) {
    if(cs->port >= 0)
      close(cs->port);
    for(i=0; i<CONTROL_CLIENTS; i++) {
      ControlClient *client = cs->clients + i;
      client->sock = del_SockChan(cp, client->sock);
      client->reply = del_PipeChan(cp, client->reply);
    };
    free(cs);
  };
  return NULL;
}

/*.......................................................................
 * This function is called to accept or reject a connection request
 * on the control-server port.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 *  cs     ControlServer *   The control-server resource object.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Fatal error.
 */
static int connect_control_client(ControlProg *cp, ControlServer *cs)
{
  struct sockaddr_in client_addr;/* The address of the client */
  int addr_len;                  /* The size of client_addr */
  ControlClient *client;         /* The control slot assigned to the client */
  int fd;                        /* The socket fd assigned to the client */
  LoggerMessage logmsg;          /* A message to be sent to the logger */
  SchedulerMessage schmsg;       /* A message to be sent to the scheduler */
  ArchiverMessage arcmsg;        /* A message to be sent to the archiver */
  int i;
  /*
   * Allow the caller to connect.
   */
  addr_len = sizeof(client_addr);
  fd = accept(cs->port, (struct sockaddr *) &client_addr, 
	      (socklen_t* )&addr_len);
  if(fd < 0) {
    lprintf(stderr,
	    "connect_control_client: Error accepting TCP/IP client.\n");
    return 1;
  };
  /*
   * Attempt to find a free control-client socket.
   */
  client = NULL;
  for(i=0; i<CONTROL_CLIENTS; i++) {
    ControlClient *cc = cs->clients + i;
    if(cc->sock->head.read.fd < 0) {
      client = cc;
      break;
    };
  };
  /*
   * If all control-client sockets are in use, reject the connection
   * by closing its socket.
   */
  if(!client) {
    shutdown(fd, 2);
    close(fd);
    lprintf(stderr, "Rejected control client from %s (too many clients).\n",
	    inet_ntoa(client_addr.sin_addr));
    return 0;
  };
  /*
   * Report the connection for security purposes.
   */
  lprintf(stdout, "Accepted control-client connection from %s.\n",
	  inet_ntoa(client_addr.sin_addr));
  /*
   * Attach the client file descriptor to the control-client communications
   * iterator.
   */
  if(open_socket_channel(cp, client->sock, fd)) {
    shutdown(fd, 2);
    close(fd);
    return 0;
  };
  /*
   * Discard any legacy contents from the client-reply pipe.
   */
  {
    PipeChan *pc = client->reply;

    while(pc->pipe->read(pc->rbuf, pc->msgsize, PIPE_NOWAIT) == PIPE_OK);

  };
  /*
   * Register the socket for the receipt of commands, and its reply
   * message queue for receiving reply and error messages to be sent
   * to the client.
   */
  if(add_readable_channel(cp, &client->sock->head) ||
     add_readable_channel(cp, &client->reply->head))
    return 1;
  /*
   * Add the client to the list that the logger thread sends error messages
   * to.
   */
  if(pack_logger_add_client(&logmsg, client->reply->pipe) ||
     send_LoggerMessage(cp, &logmsg, PIPE_WAIT))
    return 1;
  /*
   * Add the client to the list that the scheduler thread sends status
   * messages to.
   */
  if(pack_scheduler_add_client(&schmsg, client->reply->pipe) ||
     send_SchedulerMessage(cp, &schmsg, PIPE_WAIT))
    return 1;
  /*
   * Add the client to the list that the archiver thread sends status
   * messages to.
   */
  if(pack_archiver_add_client(&arcmsg, client->reply->pipe) ||
     send_ArchiverMessage(cp, &arcmsg, PIPE_WAIT))
    return 1;
  return 0;
}

/*.......................................................................
 * Handle receipt of a command-message from a control client.
 */
static SOCK_RCVD_FN(cc_sock_rcvd_fn)
{
  ControlClient *client = (ControlClient* )sock->head.client_data; /* The
								      originating
								      client */
  NetBuf *net = sock->nrs->net;      /* The network buffer of the socket */
  CcNetCmd netcmd;     /* The container of the received command */
  int opcode;          /* The message-type opcode */
  /*
   * Get the scheduler control-pipe channel.
   */
  PipeChan *scheduler_channel = cp->thread[CP_SCHEDULER].pipe;
  /*
   * Read the message into a local buffer.
   */
  if(net_start_get(net, &opcode) ||
     net_to_obj(&cc_cmd_table, net, opcode, &netcmd) ||
     net_end_get(net))
    return 1;
  /*
   * Forward the contents of the message to the appropriate thread.
   */
  switch((CcNetCmdId) opcode) { /* The cast enables compiler checks */
  case CC_INPUT_CMD:            /* A command to be sent to the scheduler */
    pack_scheduler_command((SchedulerMessage* )scheduler_channel->wbuf, 
			   netcmd.input.cmd, client->reply->pipe);
    add_scheduler_channel(cp);  /* Stage the command to be sent. */
    break;
  default:
    lprintf(stderr, "Unknown command type received from a control client.\n");
    break;
  };
  return 0;
}

/*.......................................................................
 * Handle completion of sending a message to a control client.
 */
static SOCK_SENT_FN(cc_sock_sent_fn)
{
  /* 
   * The target control client
   */
  ControlClient *client = (ControlClient* )sock->head.client_data; 
  /*
   * Allow receipt of another message from the client reply pipe.
   */
  add_readable_channel(cp, &client->reply->head);
  return 0;
}

/*.......................................................................
 * Handle the receipt of an exceptional condition from a control client.
 */
static SOCK_COND_FN(cc_sock_cond_fn)
{
  /* 
   * The target control client
   */
  ControlClient *client = (ControlClient* )sock->head.client_data; 
  LoggerMessage logmsg;     /* A message to be sent to the logger thread */
  SchedulerMessage schmsg;  /* A message to be sent to the scheduler thread */
  switch(cond) {
  case SOCK_ERROR:
    fprintf(stderr,"A control client connection was terminated by an error.\n");
    break;
  case SOCK_CLOSE:
    lprintf(stderr, "A control client disconnected.\n");
  };
  /*
   * Remove the client from the list that the logger thread sends error messages
   * to.
   */
  if(pack_logger_rem_client(&logmsg, client->reply->pipe) ||
     send_LoggerMessage(cp, &logmsg, PIPE_WAIT))
    return;
  /*
   * Remove the client from the list that the scheduler thread sends
   * status messages to.
   */
  if(pack_scheduler_rem_client(&schmsg, client->reply->pipe) ||
     send_SchedulerMessage(cp, &schmsg, PIPE_WAIT))
    return;
  return;
}

/*.......................................................................
 * Allocate and initialize the resources of the connection to the
 * real-time controller task.
 *
 * Input:
 *  cp        ControlProg *   The resource object of the control program.
 * Output:
 *  return   RtController *   The resource object, or NULL on error.
 */
static RtController *new_RtController(ControlProg *cp)
{
  RtController *rtc;    /* The resource object to be returned */
  int status;           /* The return status of pthreads functions. */
  /*
   * Allocate the container.
   */
  rtc = (RtController *) malloc(sizeof(RtController));
  if(!rtc) {
    lprintf(stderr, "new_RtController: Insufficient memory.\n");
    return NULL;
  };
  /*
   * Before attempting any operation that might fail, initialize the
   * container at least up to the point at which it can safely be passed
   * to del_RtController().
   */
  rtc->port = -1;
  rtc->sock = NULL;
  rtc->pipe = NULL;
  rtc->status.guard_ready = 0;
  rtc->status.online = 0;
  /*
   * Attempt to allocate a TCP/IP port on which to listen for
   * connection requests from the real-time controller.
   */
  rtc->port = tcp_server_sock(CP_RTC_PORT, 1);
  if(rtc->port == -1)
    return del_RtController(cp, rtc);
  /*
   * Allocate the communications iterator.
   */
  rtc->sock = new_SockChan(cp, NULL, 
			   NET_PREFIX_LEN + net_max_obj_size(&rtc_msg_table),
			   NET_PREFIX_LEN + net_max_obj_size(&rtc_cmd_table),
			   rtc_rcvd_fn, rtc_sent_fn, rtc_cond_fn);
  if(!rtc->sock)
    return del_RtController(cp, rtc);
  /*
   * Create the pipe to which the scheduler task queues commands to be
   * written to the real-time controller socket.
   */
  rtc->pipe = new_PipeChan(cp, rtc, sizeof(RtcPipeCmd), rtc_pipe_rcvd_fn, 0);
  if(!rtc->pipe)
    return del_RtController(cp, rtc);
  /*
   * Create the mutual exclusion semaphore for the status container.
   */
  status = pthread_mutex_init(&rtc->status.guard, NULL);
  if(status) {
    lprintf(stderr, "new_RtController: pthread_mutex_init -> %s\n",
	    strerror(status));
    return del_RtController(cp, rtc);
  };
  rtc->status.guard_ready = 1;
  return rtc;
}

/*.......................................................................
 * Delete the resources of the connection to the real-time controller
 * task.
 *
 * Input:
 *  cp      ControlProg *   The resource object of the control program.
 *  rtc    RtController *  The resource container to be deleted.
 * Output:
 *  return RtController *  The deleted container (ie. NULL).
 */
static RtController *del_RtController(ControlProg *cp, RtController *rtc)
{
  if(rtc) {
    if(rtc->port >= 0)
      close(rtc->port);
    rtc->sock = del_SockChan(cp, rtc->sock);
    rtc->pipe = del_PipeChan(cp, rtc->pipe);
    if(rtc->status.guard_ready) {
      pthread_mutex_destroy(&rtc->status.guard);
      rtc->status.guard_ready = 0;
    };
    free(rtc);
  };
  return NULL;
}

/*.......................................................................
 * This function should be called whenever a new connection is made to
 * the real-time contoller, and whenever an existing connection is lost.
 *
 * Input:
 *  cp      ControlProg *  The resource object of the control program.
 *  online          int    0 - Mark the controller as offline.
 *                         1 - Mark the controller as online.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
static int record_rtc_status(ControlProg *cp, int online)
{
  RtController *rtc = cp->rtc; /* The controller resource object */
  int status;                  /* The return status of pthreads functions. */
  /*
   * Acquire exclusive access to the thread-safe status object.
   */
  status = pthread_mutex_lock(&rtc->status.guard);
  if(status) {
    lprintf(stderr, "record_rtc_status: Couldn't lock mutex.\n");
    return 1;
  };
  /*
   * Record the new status.
   */
  rtc->status.online = online;
  /*
   * Relinquish exclusive access to the status object.
   */
  pthread_mutex_unlock(&rtc->status.guard);
  /*
   * Compose messages to inform the scheduler and navigator of the new
   * status, and stage them to be sent. Note that these messages may
   * overwrite previously queued but unsent messages.
   */
  pack_scheduler_rtc_state((SchedulerMessage* )cp->thread[CP_SCHEDULER].pipe->wbuf, online);
  add_scheduler_channel(cp);
  pack_navigator_rtc_state((NavigatorMessage* )cp->thread[CP_NAVIGATOR].pipe->wbuf, online);
  add_navigator_channel(cp);
  return 0;
}

/*.......................................................................
 * This function can be called by any thread to query whether the
 * real-time contoller is currently connected.
 *
 * Input:
 *  cp      ControlProg *  The resource object of the control program.
 * Output:
 *  return          int    0 - The controller is offline.
 *                         1 - The controller is online.
 */
int cp_rtc_online(ControlProg *cp)
{
  RtController *rtc = cp->rtc; /* The controller resource object */
  int status;                  /* The return status of pthreads functions. */
  int online;                  /* The value of rtc->status.online */
  /*
   * Acquire exclusive access to the thread-safe status object.
   */
  status = pthread_mutex_lock(&rtc->status.guard);
  if(status) {
    lprintf(stderr, "cp_rtc_online: Couldn't lock mutex.\n");
    return 0;
  };
  /*
   * Get the value to be returned.
   */
  online = rtc->status.online;
  /*
   * Relinquish exclusive access to the status object.
   */
  pthread_mutex_unlock(&rtc->status.guard);
  return online;
}

/*.......................................................................
 * Handle receipt of a message from the real-time controller.
 */
static SOCK_RCVD_FN(rtc_rcvd_fn)
{
  ArchiverMessage arcmsg;   /* A logger-thread message container */
  RtcNetMsg netmsg;    /* A union of message containers */
  int opcode;          /* The message-type opcode */
  
  // Read the message type and unpack the message.

  if(net_start_get(sock->nrs->net, &opcode) ||
     net_get_long(sock->nrs->net, 1, &netmsg.antenna) ||
     net_to_obj(&rtc_msg_table, sock->nrs->net, opcode, &netmsg.msg) ||
     net_end_get(sock->nrs->net))
    return 1;
  
  // Forward the message to the appropriate thread.

  switch((NetMsgId) opcode) { /* The cast enables compiler checks */
    
    // Send log messages to the logger thread.

  case NET_LOG_MSG:

    if(bufferLogMsg(cp, sock, netmsg))
      return 1;
    if(add_readable_channel(cp, &sock->head))
      return 1;

    break;

    // Send an update of ephemeris data from the navigator thread to
    // the AC

  case NET_NAV_UPDATE_MSG:
    pack_navigator_rtc_state((NavigatorMessage* )cp->thread[CP_NAVIGATOR].pipe->wbuf, 1);
    add_navigator_channel(cp);
    break;
    
    // Send phase-shifter and channelizer transaction completion
    // messages to the scheduler thread.

  case NET_NOISE_DONE_MSG:
  case NET_PMAC_DONE_MSG:
  case NET_SOURCE_SET_MSG:
  case NET_SETREG_DONE_MSG:
  case NET_CALTERT_DONE_MSG:
  case NET_CAN_DONE_MSG:
    if(pack_scheduler_rtcnetmsg((SchedulerMessage* )
				cp->thread[CP_SCHEDULER].pipe->wbuf, 
				(NetMsgId)opcode, &netmsg))
      return 1;
    
    // Stage the message to be sent.

    add_scheduler_channel(cp);
    break;
    
    // If the effects of a tv_offset command have been placed in a
    // frame sent to the archiver, let the archiver know.

  case NET_TV_OFFSET_DONE_MSG:
    if(pack_archiver_tv_offset_done(&arcmsg, netmsg.msg.tv_offset_done.seq) ||
       send_ArchiverMessage(cp, &arcmsg, PIPE_NOWAIT)==PIPE_ERROR)
      return 1;
    if(add_readable_channel(cp, &sock->head))
      return 1;
    break;
  default:
    break;
  };
  return 0;
}

/**.......................................................................
 * Method to buffer messages received from the control system
 */
static int bufferLogMsg(ControlProg* cp, SockChan* sock, RtcNetMsg& netmsg)
{
  LogMsgHandler handler;

  unsigned seq = netmsg.msg.log.seq;
  bool isErr   = netmsg.msg.log.bad;
  bool isLast  = netmsg.msg.log.end;

  handler.append(seq, netmsg.msg.log.text);

  if(isLast) {
    return sendLoggerMessage(cp, handler.getMessage(seq), isErr, false);
  }

  return 0;
}


/*.......................................................................
 * Handle completion of sending a message to the real-time controller.
 */
static SOCK_SENT_FN(rtc_sent_fn)
{
  PipeChan *pipe = cp->rtc->pipe;

  // Reset the dead-man timeout every time a command is sent to the rtc:

  cp->timeOut_->reset();

  // Arrange to listen for new messages from the internal rtc pipe.

  if(add_readable_channel(cp, &pipe->head))
    return 1;

  return 0;
}

/*.......................................................................
 * Handle the receipt of an exceptional condition from the real-time
 * controller.
 */
static SOCK_COND_FN(rtc_cond_fn)
{
  /*
   * Report the condition.
   */
  switch(cond) {
  case SOCK_ERROR:
    lprintf(stderr,"Control connection to szaTranslator terminated after error.\n");
    break;
  case SOCK_CLOSE:
    lprintf(stderr, "Control connection to szaTranslator was closed.\n");
    break;
  };
  /*
   * Record the new status for public access.
   */
  record_rtc_status(cp, 0);
  /*
   * The scheduler and navigator threads may still write a message or two
   * to the rtc output pipe before receiving the above warning, so enable
   * receipt of messages from the pipe while the controller is down.
   * These messages will be discarded.
   */
  add_readable_channel(cp, &cp->rtc->pipe->head);
}

/*.......................................................................
 * Act on the receipt of a message from the internal pipe of messages
 * queued to be sent to the real-time-controller.
 */
static PIPE_RCVD_FN(rtc_pipe_rcvd_fn)
{
  SockChan *sock = cp->rtc->sock;   /* The output socket to the controller */
  NetBuf *net = sock->nss->net;     /* The network send buffer of 'sock' */
  RtcPipeCmd *rpc = (RtcPipeCmd* )pipe->rbuf;     /* The received pipe
						     message */
  // If the controller is offline, discard the message and prepare to
  // read the next.

  if(sock->head.read.fd < 0) {
    if(add_readable_channel(cp, &pipe->head))
      return 1;
  } else {
    
    // Pack the message for transmission to the real-time controller.
    
    if(net_start_put(net, rpc->id) ||
       net_put_long(net, 1, (unsigned long*)&rpc->rtc.antennas) ||
       obj_to_net(&rtc_cmd_table, net, rpc->id, &rpc->rtc.cmd) ||
       net_end_put(net))
      return 1;

    // Register if we should deactivate the timeout when this command
    // has been sent

    cp->timeOut_->registerDeactivateTimeout(rpc->deactivateTimeout);
    
    // Arrange for the message to be sent.

    if(add_writable_channel(cp, &sock->head))
      return 1;

  };
  return 0;
}

/*.......................................................................
 * This function adds the real-time-controller socket to be watched for
 * incoming messages, but only if all of the pipes to which it forwards
 * these messages are ready to accept more messages.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 *  rtc     RtController *   The control-server resource object.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Error.
 */
static int watch_rtc_readability(ControlProg *cp, RtController *rtc)
{
  /*
   * Get the navigator control-pipe channel.
   */
  PipeChan *nav_chan = cp->thread[CP_NAVIGATOR].pipe;
  /*
   * Get the scheduler control-pipe channel.
   */
  PipeChan *sch_chan = cp->thread[CP_SCHEDULER].pipe;
  /*
   * Get the logger control-pipe channel.
   */
  PipeChan *log_chan = cp->thread[CP_LOGGER].pipe;
  /*
   * If any of the pipes that incoming controller messages are
   * forwarded to are currently occupied sending a previous message,
   * we shouldn't arrange to receive messages from the controller for
   * the moment.
   */
  if(nav_chan->head.write.active_set ||
     sch_chan->head.write.active_set ||
     log_chan->head.write.active_set)
    return 0;
  /*
   * It appears that we can safely forward messages to the above
   * channels, so arrange to watch for incoming messages from the
   * controller.
   */
  if(cp->rtc->sock->head.read.fd >= 0 &&
     add_readable_channel(cp, &cp->rtc->sock->head))
    return 1;
  return 0;
}

/*.......................................................................
 * This function is called to accept or reject a connection request
 * from the real-time controller.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 *  rtc     RtController *   The control-server resource object.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Fatal error.
 */
static int connect_controller(ControlProg *cp, RtController *rtc)
{
  struct sockaddr_in client_addr;/* The address of the client */
  int addr_len;                  /* The size of client_addr */
  int fd;                        /* The socket assigned to the client */
  RtcNetCmd rtcCmd;              /* The greeting command */
  sza::array::Date utc;          /* The gregorian date (utc) */
  double mjd;                    /* The utc expressed as a MJD */
  
  // Terminate any existing connection. This allows dead connections
  // to be terminated and restarted if the real-time CPU reboots
  // unexpectedly.

  if(rtc->sock->head.read.fd >= 0) {
    lprintf(stderr, "Closing stale controller connection.\n");
    close_socket_channel(cp, rtc->sock, SOCK_CLOSE);
  };
  
  // Allow the caller to connect.

  addr_len = (int)sizeof(client_addr);
  fd = accept(rtc->port, (struct sockaddr *) &client_addr, 
	      (socklen_t* )&addr_len);
  if(fd < 0) {
    lprintf(stderr,
	    "connect_controller: Error accepting TCP/IP connection.\n");
    return 1;
  };
  
  // Report the connection for security purposes.

  lprintf(stdout, "Accepted controller connection from %s.\n",
	  inet_ntoa(client_addr.sin_addr));
  
  // Attach the controller file descriptor to the controller socket
  // channel.

  if(open_socket_channel(cp, rtc->sock, fd)) {
    shutdown(fd, 2);
    close(fd);
    return 0;
  };
  
  // Register interest in receiving messages from the controller,
  // unless any of the pipes to which these messages would be
  // forwarded is currently blocked.

  if(watch_rtc_readability(cp, rtc))
    return 1;
  
  // Get the current UTC as a modified Julian date.

  if(current_date(&utc) || (mjd = date_to_mjd_utc(&utc)) < 0)
    return 1;
  
  // Compose the mandatory initialization message.

  rtcCmd.cmd.init.start = 1;

  // Queue the message to be sent.

  {
    NetBuf *net = rtc->sock->nss->net;
    if(net_start_put(net, NET_INIT_CMD) || 
       net_put_long(net, 1, (unsigned long*)&rtcCmd.antennas) ||
       obj_to_net(&rtc_cmd_table, net, NET_INIT_CMD, &rtcCmd.cmd) ||
       net_end_put(net))
      return 1;
    if(add_writable_channel(cp, &rtc->sock->head))
      return 1;
  };
  
  // Disable the receipt of messages to be forwarded to the controller
  // until the initialization message has been sent.

  rem_readable_channel(cp, &rtc->pipe->head);
  
  // Discard any legacy contents from the pipe-queue of messages to be
  // forwarded to the controller.

  {
    PipeChan *pc = rtc->pipe;

    while(pc->pipe->read(pc->rbuf, pc->msgsize, PIPE_NOWAIT) == PIPE_OK);
  };
  
  // Record the new status for public access.

  if(record_rtc_status(cp, 1))
    return 1;
  return 0;
}

/*.......................................................................
 * When a message has been placed in the
 * cp->thread[CP_SCHEDULER].pipe->wbuf, call this function to arrange for
 * it to be sent. This involves registering the write fd of the pipe to
 * be watched for writability, and unregistering the read fd's of any
 * channels who's messages get forwarded to the scheduler input pipe.
 * This ensures that the write buffer won't be overwritten until the
 * latest message has been sent.
 *
 * Input:
 *  cp       ControlProg *  The resource object of the control program.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int add_scheduler_channel(ControlProg *cp)
{
  int i;
  /*
   * Get the scheduler control-pipe channel.
   */
  PipeChan *scheduler_channel = cp->thread[CP_SCHEDULER].pipe;
  /*
   * Stage the message to be sent.
   */
  if(add_writable_channel(cp, &scheduler_channel->head))
    return 1;
  /*
   * Until the message has been sent, don't listen to any channels
   * whose contents result in messages being sent to the scheduler.
   */
  for(i=0; i<CONTROL_CLIENTS; i++) {
    ControlClient *cc = cp->cs->clients + i;
    if(cc->sock->head.read.fd > 0 && rem_readable_channel(cp, &cc->sock->head))
      return 1;
  };
  if(cp->rtc->sock->head.read.fd >= 0 &&
     rem_readable_channel(cp, &cp->rtc->sock->head))
    return 1;
  return 0;
}

/*.......................................................................
 * When a message has been succesfully been written to the scheduler
 * control pipe, call this function to arrange for the channels that
 * forward messages to this pipe, to be watched for readability.
 *
 * Input:
 *  cp       ControlProg *  The resource object of the control program.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int rem_scheduler_channel(ControlProg *cp)
{
  int i;
  /*
   * Get the scheduler control-pipe channel.
   */
  PipeChan *scheduler_channel = cp->thread[CP_SCHEDULER].pipe;
  /*
   * Stop watching the scheduler channel for writability until a
   * new message is signalled via a call to add_scheduler_channel().
   */
  if(rem_writable_channel(cp, &scheduler_channel->head))
    return 1;
  /*
   * Allow receipt of messages from channels that forward to the
   * scheduler pipe.
   */
  for(i=0; i<CONTROL_CLIENTS; i++) {
    ControlClient *cc = cp->cs->clients + i;
    if(cc->sock->head.read.fd > 0 && add_readable_channel(cp, &cc->sock->head))
      return 1;
  };
  if(watch_rtc_readability(cp, cp->rtc))
    return 1;
  return 0;
}

/*.......................................................................
 * Act on the completion of a message having been written to the scheduler
 * control pipe.
 */
static PIPE_SENT_FN(sent_Scheduler)
{
  /*
   * Re-enable the receipt of messages from channels that forward to the
   * scheduler pipe.
   */
  return rem_scheduler_channel(cp);
}

/*.......................................................................
 * When a message has been placed in the cp->thread[CP_NAVIGATOR].pipe->wbuf,
 * call this function to arrange for it to be sent. This involves
 * registering the write fd of the pipe to be watched for writability,
 * and unregistering the read fd's of any channels who's messages get
 * forwarded to the navigator input pipe. This ensures that the write
 * buffer won't be overwritten until the latest message has been sent.
 *
 * Input:
 *  cp       ControlProg *  The resource object of the control program.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int add_navigator_channel(ControlProg *cp)
{
  /*
   * Get the navigator control-pipe channel.
   */
  PipeChan *navigator_channel = cp->thread[CP_NAVIGATOR].pipe;
  /*
   * Stage the message to be sent.
   */
  if(add_writable_channel(cp, &navigator_channel->head))
    return 1;
  /*
   * Until the message has been sent, don't listen to any channels
   * who's contents result in messages being sent to the navigator.
   */
  if(cp->rtc->sock->head.read.fd >= 0 &&
     rem_readable_channel(cp, &cp->rtc->sock->head))
    return 1;
  return 0;
}

/*.......................................................................
 * When a message has been succesfully been written to the navigator
 * control pipe, call this function to arrange for the channels that
 * forward messages to this pipe, to be watched for readability.
 *
 * Input:
 *  cp       ControlProg *  The resource object of the control program.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int rem_navigator_channel(ControlProg *cp)
{
  /*
   * Get the navigator control-pipe channel.
   */
  PipeChan *navigator_channel = cp->thread[CP_NAVIGATOR].pipe;
  /*
   * Stop watching the navigator channel for writability until a
   * new message is signalled via a call to add_navigator_channel().
   */
  if(rem_writable_channel(cp, &navigator_channel->head))
    return 1;
  /*
   * Allow receipt of messages from channels that forward to the
   * navigator pipe.
   */
  if(watch_rtc_readability(cp, cp->rtc))
    return 1;
  return 0;
}

/*.......................................................................
 * Act on the completion of a message having been written to the navigator
 * control pipe.
 */
static PIPE_SENT_FN(sent_Navigator)
{
  /*
   * Re-enable the receipt of messages from channels that forward to the
   * navigator pipe.
   */
  return rem_navigator_channel(cp);
}

/*.......................................................................
 * When a message has been placed in the
 * cp->thread[CP_LOGGER].pipe->wbuf, call this function to arrange for
 * it to be sent. This involves registering the write fd of the pipe to
 * be watched for writability, and unregistering the read fd's of any
 * channels who's messages get forwarded to the logger input pipe.
 * This ensures that the write buffer won't be overwritten until the
 * latest message has been sent.
 *
 * Input:
 *  cp       ControlProg *  The resource object of the control program.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int add_logger_channel(ControlProg *cp)
{
  int i;
  /*
   * Get the logger control-pipe channel.
   */
  PipeChan *logger_channel = cp->thread[CP_LOGGER].pipe;
  /*
   * Stage the message to be sent.
   */
  if(add_writable_channel(cp, &logger_channel->head))
    return 1;
  /*
   * Until the message has been sent, don't listen to any channels
   * whose contents result in messages being sent to the logger.
   */
  for(i=0; i<CONTROL_CLIENTS; i++) {
    ControlClient *cc = cp->cs->clients + i;
    if(cc->sock->head.read.fd > 0 && rem_readable_channel(cp, &cc->sock->head))
      return 1;
  };
  if(cp->rtc->sock->head.read.fd >= 0 &&
     rem_readable_channel(cp, &cp->rtc->sock->head))
    return 1;
  return 0;
}

/*.......................................................................
 * When a message has been succesfully been written to the logger
 * control pipe, call this function to arrange for the channels that
 * forward messages to this pipe, to be watched for readability.
 *
 * Input:
 *  cp       ControlProg *  The resource object of the control program.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int rem_logger_channel(ControlProg *cp)
{
  /*
   * Get the logger control-pipe channel.
   */
  PipeChan *logger_channel = cp->thread[CP_LOGGER].pipe;
  /*
   * Stop watching the logger channel for writability until a
   * new message is signalled via a call to add_logger_channel().
   */
  if(rem_writable_channel(cp, &logger_channel->head))
    return 1;
  /*
   * Allow receipt of messages from channels that forward to the
   * logger pipe.
   */
  if(watch_rtc_readability(cp, cp->rtc))
    return 1;
  return 0;
}

/*.......................................................................
 * Act on the completion of a message having been written to the logger
 * control pipe.
 */
static PIPE_SENT_FN(sent_Logger)
{
  /*
   * Re-enable the receipt of messages from channels that forward to
   * the logger pipe.
   */
  return rem_logger_channel(cp);
}

/*.......................................................................
 * Act on the receipt of a message to be forwarded to a control client.
 */
static PIPE_RCVD_FN(cc_pipe_rcvd_fn)
{
  /* 
   * The target control client
   */
  ControlClient *client = (ControlClient* )pipe->head.client_data; 
  SockChan *sock = client->sock;  /* The client network socket */
  NetBuf *net = sock->nss->net;   /* The output network buffer of the client */
  CcPipeMsg *pmsg = (CcPipeMsg* )pipe->rbuf;   /* The received pipe message */
  /*
   * Is the client still connected?
   */
  if(sock->head.read.fd >= 0) {
    /*
     * Pack the message to be written to the client socket.
     */
    if(net_start_put(net, pmsg->id) ||
       obj_to_net(&cc_msg_table, net, pmsg->id, &pmsg->msg) ||
       net_end_put(net))
      return 1;
    /*
     * Arrange for the message to be sent when the client socket becomes
     * writable.
     */
    if(add_writable_channel(cp, &sock->head))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Queue a message to be sent to a control client. Given that this
 * function can be called unpredictably from any thread [eg. via calls to
 * lprintf(stderr,...)], non-blocking I/O is used to prevent deadlocks.
 *
 * Input:
 *  client      Pipe *  The pipe-queue of the target client.
 *  pmsg   CcPipeMsg *  The message to be sent.
 * Ouput:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
int queue_cc_message(sza::util::Pipe *client, CcPipeMsg *pmsg)
{
  /*
   * Check arguments.
   */
  if(!client || !pmsg) {
    lprintf(stderr, "queue_cc_message: NULL argument(s).\n");
    return 1;
  };
  /*
   * Attempt to queue it to the pipe, using non-blocking I/O.
   */

  return client->write(pmsg, sizeof(*pmsg), PIPE_NOWAIT) == PIPE_ERROR;
}

/*.......................................................................
 * Write a message to the real-time controller staging pipe, for
 * subsequent sending to the real-time controller. This is a blocking
 * function.
 *
 * Input:
 *  cp     ControlProg *   The resource object of the control program.
 *  rtc      RtcNetCmd *   The command object to be sent (see rtcnetcoms.h),
 *                         or NULL for object types that don't have any
 *                         associated data.
 *  type      NetCmdId     The type of object in RtcNetCmd.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
int queue_rtc_command(ControlProg *cp, RtcNetCmd *rtc, 
		      NetCmdId type, bool deactivateTimeout)
{
  RtcPipeCmd msg;  /* The message container */
  /*
   * Check arguments.
   */
  if(!cp) {
    lprintf(stderr, "queue_rtc_command: NULL argument(s).\n");
    return 1;
  };

  msg.deactivateTimeout = deactivateTimeout;
  msg.id = type;

  if(rtc)
    msg.rtc = *rtc;

  return cp->rtc->pipe->pipe->write(&msg, sizeof(msg), PIPE_WAIT) ==
    PIPE_ERROR;
}

/*.......................................................................
 * Send a shutdown request to the control thread. This results in the
 * control program cleaning up resources and exiting. This is a blocking
 * call.
 *
 * Input:
 *  cp      ControlProg *   The control program resource object.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
int cp_request_shutdown(ControlProg *cp)
{
  ControlMessage msg;
  msg.type = CP_SHUTDOWN_MSG;
  return send_ControlMessage(cp, &msg);
}

/*.......................................................................
 * Send a restart request to the control thread. This results in the
 * control program reclaiming resources, shutting down its threads, then
 * restarting them from scratch. The control program itself doesn't exit.
 *
 * Input:
 *  cp     ControlProg *   The control program resource object.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
int cp_request_restart(ControlProg *cp)
{
  ControlMessage msg;
  msg.type = CP_RESTART_MSG;
  return send_ControlMessage(cp, &msg);
}

/*.......................................................................
 * Just before a thread exits it should call the following function to
 * tell the control thread that it is aborting.
 *
 * Input:
 *  cp     ControlProg *   The control program resource object.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
int cp_report_exit(ControlProg *cp)
{
  ControlMessage msg;
  msg.type = CP_EXITING_MSG;
  msg.body.exiting.tid = pthread_self();
  return send_ControlMessage(cp, &msg);
}

/*.......................................................................
 * When the scheduler thread finishes executing the control-program
 * initialization script, it calls this function to tell this thread
 * to start listening for connections from the real-time cpu.
 *
 * Input:
 *  cp     ControlProg *   The control program resource object.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
int cp_initialized(ControlProg *cp)
{
  ControlMessage msg;
  msg.type = CP_INITIALIZED_MSG;
  return send_ControlMessage(cp, &msg);
}

/*.......................................................................
 * Send a message to the control thread. Note that this is a blocking
 * call.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 *  msg   ControlMessage *   The message to be sent.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Error.
 */
static int send_ControlMessage(ControlProg *cp, ControlMessage *msg)
{
  return cp->pipe->pipe->write(msg, sizeof(*msg), PIPE_WAIT) == PIPE_ERROR;
}

/*.......................................................................
 * The event loop calls this function when it receives a message from the
 * control pipe.
 */
static PIPE_RCVD_FN(cp_pipe_rcvd_fn)
{
  ControlMessage msg;   /* The message that was received */
  int waserr=0;
  /*
   * Extract the message from the pipe buffer.
   */
  memcpy(&msg, pipe->rbuf, pipe->msgsize);
  /*
   * Determine the type of message.
   */
  switch(msg.type) {
  case CP_SHUTDOWN_MSG:
    cp->whatnext = CP_SHUTDOWN;
    break;
  case CP_RESTART_MSG:
    cp->whatnext = CP_RESTART;
    break;
  case CP_EXITING_MSG:
    /*
     * Locate the thread and mark it as no longer running.
     */
    {
      CpThread *t = find_CpThread(cp, msg.body.exiting.tid);
      if(t)
	t->running = 0;
      pthread_detach(msg.body.exiting.tid);
    };
    /*
     * An exiting thread here must be exiting on error. Shutdown the
     * control program if this happens.
     */
    lprintf(stderr, "The control program is being shut down because a thread exited.\n");
    cp->whatnext = CP_SHUTDOWN;
    break;
    /*
     * After the scheduler initialization script completes, we are ready to
     * receive connections from the real-time cpu.
     */
  case CP_INITIALIZED_MSG:
    /*
     * Add the ports of the controller and scanner servers to the set of
     * file descriptors to be watched for readability.
     */
    cp_add_read_fd(cp, cp->rtc->port);
    cp_add_read_fd(cp, cp->rts->port);
    cp_add_read_fd(cp, cp->rto->port);
    break;

  case CP_ADD_PAGER_REG_MSG:

    addPagerRegister(cp, 
		     msg.body.addPagerReg.regName,
		     msg.body.addPagerReg.min,
		     msg.body.addPagerReg.max,
		     msg.body.addPagerReg.delta,
		     msg.body.addPagerReg.nFrame,
		     msg.body.addPagerReg.outOfRange,
		     msg.body.addPagerReg.comment);

    break;

  case CP_REM_PAGER_REG_MSG:

    remPagerRegister(cp, msg.body.remPagerReg.regName);
    break;

  case CP_PAGER_CLEAR_MSG:

    clearPager(cp);
    break;

  case CP_PAGER_LIST_MSG:

    listPager(cp);
    break;

  case CP_PAGER_RESET_MSG:

    resetPager(cp);
    break;

  case CP_PAGER_ENABLE_MSG:

    waserr |= enablePager(cp, msg.body.enablePager.enable);
    break;

  case CP_ARRAY_CONFIG_MSG:
    setArrayConfig(cp, msg.body.arrayConfig.array, msg.body.arrayConfig.config);
    break;

  case CP_ADD_ARRAY_ANTENNA_MSG:
    addArrayAntenna(cp, 
		    msg.body.addArrayAntenna.array,
		    msg.body.addArrayAntenna.iPad,
		    msg.body.addArrayAntenna.antType,
		    msg.body.addArrayAntenna.iAnt);
    break;

  case CP_REM_ARRAY_ANTENNA_MSG:
    remArrayAntenna(cp, 
		    msg.body.remArrayAntenna.array,
		    msg.body.remArrayAntenna.iPad);
    break;

  };

  // Allow receipt of further control messages.

  add_readable_channel(cp, &pipe->head);
  return waserr;
}

/*.......................................................................
 * Lookup a given thread by its thread id.
 *
 * Input:
 *  cp     ControlProg *   The resource object of the control program.
 *  tid      pthread_t     The POSIX thread id to look up.
 * Output:
 *  return    CpThread *   The context object of the thread.
 */
static CpThread *find_CpThread(ControlProg *cp, pthread_t tid)
{
  int i;
  for(i=0; i<NTHREAD; i++) {
    CpThread *t = cp->thread + i;
    if(pthread_equal(t->thread, tid))
      return t;
  };
  return NULL;
}

// Public access to methods of the TimeOut class

/**.......................................................................
 * Configure the command timeout interval
 */
void configureCmdTimeout(ControlProg* cp, unsigned int seconds)
{
  cp->timeOut_->setIntervalInSeconds(seconds);
  sendCmdTimeoutConfiguration(cp, seconds);
}

/**.......................................................................
 * Activate the command timeout
 */
void configureCmdTimeout(ControlProg* cp, bool activate)
{
  cp->timeOut_->activate(activate);
  sendCmdTimeoutConfiguration(cp, activate);
}

void allowTimeOutPaging(ControlProg* cp, bool allow) 
{
  cp->timeOut_->allowPaging(allow);
}

std::string cp_startupScript(ControlProg *cp)
{
  return *cp->startupScript_;
}

//-----------------------------------------------------------------------
// Register paging
//-----------------------------------------------------------------------

/**.......................................................................
 * Send a message to activate the pager
 */
int sendAddPagerRegisterMsg(ControlProg* cp, std::string regName,
			    double min, double max, bool delta, unsigned nFrame, 
			    bool outOfRange, char* comment)
{
  ControlMessage msg;

  if(regName.size() > CP_REGNAME_MAX) {
    lprintf(stderr,"Register name too long\n");
    return 1;
  }

  strncpy(msg.body.addPagerReg.regName, (char*)regName.c_str(), regName.size());

  msg.body.addPagerReg.regName[regName.size()] = '\0';

  msg.body.addPagerReg.min   = min;
  msg.body.addPagerReg.max   = max;
  msg.body.addPagerReg.delta = delta;
  
  msg.body.addPagerReg.nFrame = nFrame;
  msg.body.addPagerReg.outOfRange = outOfRange;

  if(comment) {

    std::string commentString(comment);

    if(commentString.size() > CP_REGNAME_MAX) {
      lprintf(stderr,"Comment string too long\n");
      return 1;
    }
    
    strncpy(msg.body.addPagerReg.comment, 
	    (char*)commentString.c_str(), commentString.size());
    
    msg.body.addPagerReg.comment[commentString.size()] = '\0';
  } else {
    msg.body.addPagerReg.comment[0] = '\0';
  }

  msg.type = CP_ADD_PAGER_REG_MSG;

  return send_ControlMessage(cp, &msg);
}

/**.......................................................................
 * Add a register condition to activate the pager
 */
static void addPagerRegister(ControlProg* cp, std::string regSpec, 
			     double min, double max, bool delta, unsigned nFrame, 
			     bool outOfRange, std::string comment)
{
  cp->rts->frame->pm->lock();

  try {

    // Add the monitor point to our pager manager

    if(outOfRange) {
      cp->rts->frame->pm->addOutOfRangeMonitorPoint(regSpec, min, max, delta, nFrame, comment);
    } else {
      cp->rts->frame->pm->addInRangeMonitorPoint(regSpec, min, max, delta, nFrame, comment);
    }

    // Tell any connected clients about the new monitor point

    sza::util::PagerMonitor::RegSpec reg(regSpec, min, max, delta, nFrame, outOfRange);

    sendPagerCondition(cp, PAGECOND_ADD, &reg);
    sendPagerCondition(cp, PAGECOND_UPDATE);

  } catch(Exception& err) {
    lprintf(stderr, "%s\n", err.what());
  } catch(...) {
    lprintf(stderr, "Caught an unknown error while adding a pager register\n");
  }

  cp->rts->frame->pm->unlock();
}

/**.......................................................................
 * Send a message to remove a register from the pager
 */
int sendRemPagerRegisterMsg(ControlProg* cp, std::string regName)
{
  ControlMessage msg;

  if(regName.size() > CP_REGNAME_MAX) {
    lprintf(stderr,"Register name too long\n");
    return 1;
  }

  strncpy(msg.body.remPagerReg.regName, (char*)regName.c_str(), regName.size());

  msg.body.remPagerReg.regName[regName.size()] = '\0';
  msg.type = CP_REM_PAGER_REG_MSG;

  return send_ControlMessage(cp, &msg);
}

/**.......................................................................
 * Rem a register condition to activate the pager
 */
static void remPagerRegister(ControlProg* cp, std::string regSpec)
{
  cp->rts->frame->pm->lock();

  try {

    // Remove the monitor point from our pager manager

    cp->rts->frame->pm->remMonitorPoint(regSpec);

    // And tell any connected clients about the removal

    sza::util::PagerMonitor::RegSpec reg(regSpec);
    sendPagerCondition(cp, PAGECOND_REMOVE, &reg);

  } catch(Exception& err) {
    lprintf(stderr, "%s\n", err.what());
  } catch(...) {
    lprintf(stderr, "Caught an unknown error while removing a pager register\n");
  }

  cp->rts->frame->pm->unlock();
}

/**.......................................................................
 * Send a message to reset the pager monitor points
 */
int sendResetPagerMsg(ControlProg* cp)
{
  ControlMessage msg;

  msg.type = CP_PAGER_RESET_MSG;

  return send_ControlMessage(cp, &msg);
}

/**.......................................................................
 * Reset the pager
 */
static void resetPager(ControlProg* cp)
{
  cp->rts->frame->pm->lock();

  try {

    cp->rts->frame->pm->reset();

  } catch(...) {
  }

  cp->rts->frame->pm->unlock();
}

/**.......................................................................
 * Send this thread a message to clear the pager
 */
int sendClearPagerMsg(ControlProg* cp)
{
  ControlMessage msg;
  msg.type = CP_PAGER_CLEAR_MSG;
  return send_ControlMessage(cp, &msg);
}

/**.......................................................................
 * Clear the pager
 */
static void clearPager(ControlProg* cp)
{
  cp->rts->frame->pm->lock();

  try {

    cp->rts->frame->pm->clear();

    sendPagerCondition(cp, PAGECOND_CLEAR);

  } catch(...) {
  }

  cp->rts->frame->pm->unlock();
}

/**.......................................................................
 * Send this thread a message to clear the pager
 */
int sendListPagerMsg(ControlProg* cp)
{
  ControlMessage msg;
  msg.type = CP_PAGER_LIST_MSG;
  return send_ControlMessage(cp, &msg);
}

/**.......................................................................
 * Generate a list of pager registers
 */
void listPager(ControlProg* cp, ListNode* node)
{
  cp->rts->frame->pm->lock();

  try {

    // First clear the pager on the other end

    sendPagerCondition(cp, PAGECOND_CLEAR, 0, node);

    // Now send the list of pager regs this pager monitor knows aobut

    std::vector<PagerMonitor::RegSpec> regs = cp->rts->frame->pm->getRegs();

    for(unsigned iReg=0; iReg < regs.size(); iReg++) 
      sendPagerCondition(cp, PAGECOND_ADD, &regs[iReg], node);

    // Now tell the remote clients to update their pager display
    
    sendPagerCondition(cp, PAGECOND_UPDATE, 0, node);

  } catch(...) {
  }

  // Finally, send the command timeout state

  sendCmdTimeoutConfiguration(cp, cp->timeOut_->active_, node);
  sendCmdTimeoutConfiguration(cp, (unsigned int)cp->timeOut_->rtcCmdTimeOut_.getSeconds(), 
			      node);

  cp->rts->frame->pm->unlock();

}

/**.......................................................................
 * Send this thread a message to (re)enable the pager
 */
int sendEnablePagerMsg(ControlProg* cp, bool enable)
{
  ControlMessage msg;
  msg.type = CP_PAGER_ENABLE_MSG;
  msg.body.enablePager.enable = enable;
  return send_ControlMessage(cp, &msg);
}

/**.......................................................................
 * Enable the pager
 */
int enablePager(ControlProg* cp, bool enable)
{
  // If this is a request to enable the pager, reset the PagerMonitor
  // now, so that register counters will restart from this point

  if(enable) 
    resetPager(cp);

  // And forward the message to the terminal thread

  TermMessage msg;
  if(pack_pager_enable(&msg, enable) || send_TermMessage(cp, &msg, PIPE_WAIT)==PIPE_ERROR)
    return 1;

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
double getRegVal(ControlProg* cp, sza::util::RegDescription& regDesc)
{
  sza::util::ArrayDataFrameManager* fm = cp->rts->frame->fm;
  return fm->getRegVal(regDesc);
}

//-----------------------------------------------------------------------
// Configuration commands maintained by the control thread
//-----------------------------------------------------------------------

int sendArrayConfigMsg(ControlProg* cp, unsigned array, unsigned config)
{
  ControlMessage msg;

  msg.body.arrayConfig.array  = array;
  msg.body.arrayConfig.config = config;

  msg.type = CP_ARRAY_CONFIG_MSG;

  return send_ControlMessage(cp, &msg);
}

void setArrayConfig(ControlProg* cp, unsigned array, unsigned config)
{
  sza::util::CarmaConfig* conf=0;

  switch(array) {
  case sza::util::CarmaConfig::CARMA:
    conf = cp->carmaConfig_;
    break;
  case sza::util::CarmaConfig::SZA:
    conf = cp->szaConfig_;
    break;
  default:
    return;
    break;
  }

  conf->guard_.lock();

  try {

    conf->setCurrentConfiguration(config);

    // Tell any connected clients about the new configuration

    sendArrayConfiguration(cp, 0, CONFIG_CONFIG, array, config);
    sendArrayConfiguration(cp, 0, CONFIG_UPDATE);
  
    printConfig(conf);

  } catch(Exception& err) {
    lprintf(stderr, "%s\n", err.what());
  } catch(...) {
    lprintf(stderr, "Caught an unknown error while adding an antenna\n");
  }

  conf->guard_.unlock();
}

int sendAddArrayAntennaMsg(ControlProg* cp, unsigned array, unsigned iPad, 
			   unsigned antType, int iAnt)
{
  ControlMessage msg;

  msg.body.addArrayAntenna.array   = array;
  msg.body.addArrayAntenna.iPad    = iPad;
  msg.body.addArrayAntenna.antType  = antType;
  msg.body.addArrayAntenna.iAnt     = iAnt;

  msg.type = CP_ADD_ARRAY_ANTENNA_MSG;

  return send_ControlMessage(cp, &msg);
}

void addArrayAntenna(ControlProg* cp, unsigned array, unsigned iPad, 
		     unsigned antType, int iAnt)
{
  sza::util::CarmaConfig* config=0;

  switch(array) {
  case sza::util::CarmaConfig::CARMA:
    config = cp->carmaConfig_;
    break;
  case sza::util::CarmaConfig::SZA:
    config = cp->szaConfig_;
    break;
  default:
    return;
    break;
  }

  config->guard_.lock();

  try {

    config->addPad(iPad, antType);

    if(iAnt >= 0)
      config->associatePadAndAntenna(iPad, iAnt);
    

    // Tell any connected clients about the new configuration
    
    sendAddArrayAntenna(cp, 0, array, iPad, antType, iAnt);
    sendArrayConfiguration(cp, 0, CONFIG_UPDATE);
    
    printConfig(config);

  } catch(Exception& err) {
    lprintf(stderr, "%s\n", err.what());
  } catch(...) {
    lprintf(stderr, "Caught an unknown error while adding an antenna\n");
  }

  config->guard_.unlock();
}

int sendRemArrayAntennaMsg(ControlProg* cp, unsigned array, unsigned iPad)
{
  ControlMessage msg;

  msg.body.remArrayAntenna.array   = array;
  msg.body.remArrayAntenna.iPad    = iPad;

  msg.type = CP_REM_ARRAY_ANTENNA_MSG;

  return send_ControlMessage(cp, &msg);
}

void remArrayAntenna(ControlProg* cp, unsigned array, unsigned iPad)
{
  sza::util::CarmaConfig* config=0;

  switch(array) {
  case sza::util::CarmaConfig::CARMA:
    config = cp->carmaConfig_;
    break;
  case sza::util::CarmaConfig::SZA:
    config = cp->szaConfig_;
    break;
  default:
    return;
    break;
  }

  config->guard_.lock();

  try {

    config->removePad(iPad);

    // Tell any connected clients about the new configuration

    sendRemArrayAntenna(cp, 0, array, iPad);
    sendArrayConfiguration(cp, 0, CONFIG_UPDATE);

    printConfig(config);

  } catch(Exception& err) {
    lprintf(stderr, "%s\n", err.what());
  } catch(...) {
    lprintf(stderr, "Caught an unknown error while adding an antenna\n");
  }


  config->guard_.unlock();
}

void printConfig(sza::util::CarmaConfig* config)
{
  std::vector<sza::util::CarmaConfig::PadLocation> pads = 
    config->getCurrentConfiguration();

  COUT("");
  for(unsigned i=0; i < pads.size(); i++) {
    COUT(pads[i]);
  }
  COUT("");

}

/**.......................................................................
 * Generate a listing of the array configuration to be sent to a
 * client
 */
void listArrayConfig(ControlProg* cp, ListNode* node, unsigned array)
{
  sza::util::CarmaConfig* arr=0;

  switch(array) {
  case sza::util::CarmaConfig::CARMA:
    arr = cp->carmaConfig_;
    break;
  case sza::util::CarmaConfig::SZA:
    arr = cp->szaConfig_;
    break;
  default:
    return;
    break;
  }

  arr->guard_.lock();

  try {

    // First clear the configuration on the other end for this array

    sendArrayConfiguration(cp, node, CONFIG_CLEAR, array);

    // Now send the list of antennas in this array.  If this is a
    // known configuration, we just have to send the configuration
    // flag.  If not, we have to send every antenna in the
    // configuration

    if(arr->currentConfFlag_ != sza::util::CarmaConfig::UNKNOWN) {

      sendArrayConfiguration(cp, node, CONFIG_CONFIG, array, arr->currentConfFlag_);

    } else {

      std::vector<sza::util::CarmaConfig::PadLocation> pads = arr->getCurrentConfiguration();

      for(unsigned iPad=0; iPad < pads.size(); iPad++) {
	sendAddArrayAntenna(cp, node, array, pads[iPad].padNumber_,
			    pads[iPad].ant_.antFlag_, pads[iPad].ant_.antNumber_);
      }
    }

    // Now tell the remote client to update its configuration display
    
    sendArrayConfiguration(cp, node, CONFIG_UPDATE);

  } catch(...) {
  }


  arr->guard_.unlock();
}

sza::util::CarmaConfig* getArrayConfig(ControlProg* cp, unsigned array)
{
  switch(array) {
  case sza::util::CarmaConfig::CARMA:
    return cp->carmaConfig_;
    break;
  case sza::util::CarmaConfig::SZA:
    return cp->szaConfig_;
    break;
  default:
    return 0;
    break;
  }
}

static void writeArrayConfigurations(ControlProg* cp)
{
  std::vector<sza::util::CarmaConfig::PadLocation>szaPads;
  std::vector<sza::util::CarmaConfig::PadLocation>carmaPads;

  ArrayConfig::Type szaConf   = ArrayConfig::UNKNOWN;
  ArrayConfig::Type carmaConf = ArrayConfig::UNKNOWN;

  //------------------------------------------------------------
  // Get references to the array configurations maintained by the
  // ControlProg object
  //------------------------------------------------------------

  sza::util::CarmaConfig* sza   = 
    getArrayConfig(cp, sza::util::CarmaConfig::SZA);

  sza::util::CarmaConfig* carma = 
    getArrayConfig(cp, sza::util::CarmaConfig::CARMA);

  //------------------------------------------------------------
  // Get the array configurations, making sure we unlock the objects
  // in case of failure
  //------------------------------------------------------------

  sza->guard_.lock();
  try {
    szaPads =  sza->getCurrentConfiguration();
    szaConf =  sza->confType();
  } catch (...) {
  }
  sza->guard_.unlock();

  carma->guard_.lock();
  try {
    carmaPads =  carma->getCurrentConfiguration();
    carmaConf =  carma->confType();
  } catch (...) {
  }
  carma->guard_.unlock();

  //------------------------------------------------------------
  // Now write the CARMA array into the frame buffer
  //------------------------------------------------------------

  *cp->rts->carma_.conf_ = (unsigned int)carmaConf;
  *cp->rts->carma_.nPad_ = carmaPads.size();

  for(unsigned i=0; i < carmaPads.size(); i++) {

    *(cp->rts->carma_.pad_ + i)       = carmaPads[i].padNumber_;

    *(cp->rts->carma_.antId_ + i)     = carmaPads[i].ant_.antNumber_;

    *(cp->rts->carma_.antType_ + i)   = carmaPads[i].ant_.getAntType();

    *(cp->rts->carma_.loc_ + i*3 + 0) = carmaPads[i].east_.meters();
    *(cp->rts->carma_.loc_ + i*3 + 1) = carmaPads[i].north_.meters();
    *(cp->rts->carma_.loc_ + i*3 + 2) = carmaPads[i].up_.meters();
  }

  //------------------------------------------------------------
  // Now write the SZA array into the frame buffer
  //------------------------------------------------------------

  *cp->rts->sza_.conf_ = (unsigned int)szaConf;

  for(unsigned i=0; i < szaPads.size(); i++) {

    *(cp->rts->sza_.pad_ + i)       = szaPads[i].padNumber_;

    *(cp->rts->sza_.antId_ + i)     = szaPads[i].ant_.antNumber_;

    *(cp->rts->sza_.loc_ + i*3 + 0) = szaPads[i].east_.meters();
    *(cp->rts->sza_.loc_ + i*3 + 1) = szaPads[i].north_.meters();
    *(cp->rts->sza_.loc_ + i*3 + 2) = szaPads[i].up_.meters();
  }

}

static void* findReg(RtScanner* rts, char* regmap, char *board, 
		     char *name, DataType::Type type)
{
  sza::util::ArrayDataFrameManager* fm = rts->frame->fm;
  int offset = fm->byteOffsetInFrameOf(regmap, board, name);

  if(offset < 0) {
    lprintf(stderr, "findReg: Lookup of %s.%s.%s failed.\n", regmap, 
	    board, name);
    return NULL;
  }

  return fm->frame()->getPtr(offset, type);
}

CpWhatNext whatNext(ControlProg* cp)
{
  return cp->whatnext;
}

//=======================================================================
// Methods of ViewerServer
//=======================================================================

ViewerServer::ViewerServer(bool spawnThread, unsigned port, NetMonitorFrame* nmf, int fdRead) :
  sza::util::NetMonitorFrameServer(spawnThread, port, nmf, fdRead)
{
  cp_ = new_ControlProgViewerServer();

  // Add the passed-in read fd as well

  cp_add_read_fd(cp_, fdRead);
}

ViewerServer::~ViewerServer() 
{
}

void ViewerServer::run()
{
  int waserr = 0;

  // Loop until an error occurs or a shutdown is requested.

  while(!waserr && cp_->whatnext==CP_CONTINUE) {
    ComAspect *chan; /* The read or write part of a communications channel */
    int nready;      /* The number of active file descriptors */
    fd_set read_fds = cp_->read_fds;
    fd_set send_fds = cp_->send_fds;
    /*
     * Wait for activity on any of the file descriptors that are being watched.
     * Note that only one active channel is serviced per return from select().
     * This allows the resulting action functions to add and remove channels
     * from the read and send lists or even to close active channels.
     *
     * The first channel with activity will be serviced each time. When this
     * results in completion of an I/O operation, the channel is removed from
     * the corresponding list before the channel's action function is called.
     * The action function can then add it back to the list if necessary, but
     * since this will result in the channel being placed at the end of the
     * list, channels near the start of the list are prevented from starving
     * channels near the end of the list. This implements a crude form of
     * round-robin scheduling.
     */
    nready = select(cp_->fd_set_size, &read_fds, &send_fds, NULL, 
		    cp_->timeOut_->tVal());

    switch(nready) {
    case -1:           /* Error */
      perror("cp_event_loop: select error");
      waserr = 1;
      break;
    case 0:
      waserr = cp_->timeOut_->registerTimeOut(cp_);
      break;
    default:           // One or more fds are ready for I/O 
      
      // Find the first channel (if any) that is ready to be read or
      // written.

      for(chan=cp_->active_list.head; chan; chan = chan->next) {
	if(chan->active_set == &cp_->read_fds ?
	   FD_ISSET(chan->fd, &read_fds) : FD_ISSET(chan->fd, &send_fds))
	  break;
      };
      
      // If an active channel was found, remove it from the set of
      // channels being watched and call its I/O method. Note that if
      // the I/O method adds the channel back to the set, it will be
      // put at the end of the list. This ensures that no channel can
      // starve out the rest by being lucky enough to be at the front
      // of the list.

      if(chan) {
	if(chan->active_set == &cp_->read_fds)
	  rem_readable_channel(cp_, chan->parent);
	else
	  rem_writable_channel(cp_, chan->parent);
	chan->io_fn(cp_, chan->parent);
	
	// Check for connection requests on the control-server port.

      } else if(cp_->ms && cp_->ms->port >= 0 && FD_ISSET(cp_->ms->port, &read_fds)) {
	connect_monitor_client(cp_, cp_->ms);
	
	// If fdRead has become readable, this means that a data frame
	// has arrived

      } else if(fdRead_ >= 0 && FD_ISSET(fdRead_, &read_fds)) {
	registerReceiptOfFrame();
      }

    };
  };

  ThrowError("Error encountered in run loop");
}

/**.......................................................................
 * React to the receipt of a frame from the frame server
 */
void ViewerServer::registerReceiptOfFrame()
{
  unsigned byte;
  ::read(fdRead_, &byte, 1);
	  
  // If we just received an array map, set our internal array
  // map pointing to it, and allow monitor clients to connect

  if(nmf_->nadfm_.getType() == NetArrayDataFrameManager::MEM_TEMPLATE) {

    COUT("JUST GOT A TEMPLATE***********************");

    if(cp_->ms) {
      cp_->ms = del_MonitorServer(cp_, cp_->ms);
    }

    cp_->arraymap = nmf_->getArrayMap();
    cp_->ms = new_MonitorServer(cp_, nmf_);

    if(!cp_->ms)
      ThrowError("Unable to allocate new MonitorServer");

    COUT("Got a template");

  } else {
	  
    // Now that a frame has been received, set our internal flag to
    // true
	    
    haveFrame_ = true;
    COUT("Got a frame");

    for(unsigned i=0; i < MONITOR_CLIENTS; i++) {

      MonitorClient *client = cp_->ms->clients + i;
      SockChan *client_sock = client->sock;
      
      if(client_sock->head.read.fd >= 0) {
	
	// If the preceding frame is still being sent, or the sampling
	// interval hasn't been reached, drop the frame.
	
	if(client_sock->head.write.active_set || client->interval==0 ||
	   client->dropped+1 < client->interval) {
	  client->dropped++;
	  
	  // Pack the frame for transmission.
	  
	} else {
	  NetBuf *client_net = client_sock->nss->net;
	  if(net_start_put(client_net, MC_REGS_MSG) ||
	     netPutRegs(client->regset, nmf_, client_net) ||
	     net_end_put(client_net))
	    ThrowError("Error packing data");
	  
	  // Register the message to be sent via cp_event_loop().
	  
	  if(add_writable_channel(cp_, &client_sock->head))
	    ThrowError("Error preparing to send data");
	  
	  // Reset the interval frame-counter.
	  
	  client->dropped = 0;
	}
      }
    }
    
  }
}
