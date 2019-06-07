#ifndef szacontrol_h
#define szacontrol_h

/*
 * In order to get the correct include file definitions for posix
 * threads, _POSIX_C_SOURCE must be defined to be at least 199506.
 */
#if MAC_OSX == 1 
#define _POSIX_C_SOURCE 199506L
#endif

#if !defined(_POSIX_C_SOURCE) || _POSIX_C_SOURCE < 199506L
#error "Add -D_POSIX_C_SOURCE=199506L to the compile line"
#endif

#include <stdio.h>   /* FILENAME_MAX */
#include <time.h>    /* struct tm */
#include <limits.h>  /* PIPE_BUF */

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/control.h"
#include "carma/szaarrayutils/list.h"

#define NEW_PIPE
#define PIPE_QUEUE

#include "carma/szautil/PipeQueue.h"

#include "carma/szaarrayutils/regmap.h"
#include "carma/szaarrayutils/rtcnetcoms.h"

#include "carma/szaarrayutils/regdata.h"
#include "carma/szaarrayutils/control.h"


#include "carma/szautil/AntNum.h"
#include "carma/szautil/CarmaConfig.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/MonitorPoint.h"
#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/NetMonitorFrameServer.h"

#include "carma/szaarrayutils/TransactionManager.h"

/*
 * Name the SZA initialization scripts. The
 * first script is run when szacontrol starts up, whereas the
 * second is run whenever a new connection is received from the
 * real time controller.
 */
#if DIR_IS_STABLE
#define SZACONTROL_INIT_SCRIPT "$SZA_DIR/conf/szaArrayControl.init"
#else
#define SZACONTROL_INIT_SCRIPT "$SZA_UDIR/conf/szaArrayControl.init"
#endif

/*
 * Set the maximum time to wait for threads to shutdown after being
 * told to do so. The timeout is measured in seconds.
 */
#define CP_SHUTDOWN_TIMEOUT 5

/*
 * Work out the longest pathname to support. Under Linux
 * FILENAME_MAX bytes is right at the limit of what can be sent by a
 * pipe (FILENAME_MAX==PIPE_BUF), so the remaining members of the message
 * structures cause us not to be able send messages with arrays of
 * this length included.
 */
#if FILENAME_MAX + 16 > PIPE_BUF
#define CP_FILENAME_MAX (PIPE_BUF - 16)
#else
#define CP_FILENAME_MAX FILENAME_MAX
#endif

#define CP_REGNAME_MAX CP_FILENAME_MAX/2

/*
 * Declare the type of the resource container of the program.
 */
typedef struct ControlProg ControlProg; /* (see szacontrol.c) */

/*
 * Enumerate the client threads of the control program. The value of
 * each enumerator is the index of the thread in the table of threads in
 * szacontrol.c.
 */
typedef enum {
  CP_SCHEDULER,        /* The scheduler thread */
  CP_ARCHIVER,         /* The archiver thread */
  CP_LOGGER,           /* The logger thread */
  CP_NAVIGATOR,        /* The navigator thread */
  CP_GRABBER,          /* The frame-grabber archiver thread */
  CP_TERM,             /* The terminal I/O thread */
} CpThreadId;

// Look up the resource object of a given thread.

void *cp_ThreadData(ControlProg *cp, CpThreadId id);

// The following macros declare the method functions of
// control-program threads.

// Allocate the resource object of a thread 

#define CP_NEW_FN(fn) void *(fn)(ControlProg *cp, sza::util::Pipe *pipe)

// Delete the resource object of a thread and return NULL 

#define CP_DEL_FN(fn) void *(fn)(void *obj)

// The thread start function. The thread resource object is passed via 'arg' 

#define CP_THREAD_FN(fn) void *(fn)(void *arg)

// The following function sends a shutdown message to the thread using
// non-blocking I/O. On success it should return 0. If the message
// couldn't be sent without blocking, it should return 1.

#define CP_STOP_FN(fn) int (fn)(ControlProg *cp)

/*-----------------------------------------------------------------------
 * Declare the API of the logger thread.
 *-----------------------------------------------------------------------*/

typedef struct Logger Logger;        // The type of the thread resource object 
typedef struct Logger SzaArrayLogger;// The type of the thread resource object 

CP_THREAD_FN(logger_thread);        // The logger thread start function 
CP_NEW_FN(new_Logger);              // The logger resource constructor 
CP_DEL_FN(del_Logger);              // The logger resource destructor 
CP_STOP_FN(stop_Logger);            // Send a shutdown message to the logger 


// Enumerate the message types that the logger task understands.

typedef enum {
  LOG_SHUTDOWN,              // Cleanup and shutdown the logger thread 
  LOG_CHDIR,                 // Change the default log file directory 
  LOG_OPEN,                  // Open a new log file in a given place 
  LOG_FLUSH,                 // Flush unwritten text to the current
                             // log file.(Given that line buffering is
                             // requested when log files are opened
                             // this shouldn't ever be necessary.)
  LOG_CLOSE,                 // Close the current log file 
  LOG_MESSAGE,               // Write a specified message to the log 
  LOG_ADD_CLIENT,            // Add a control client to the output list 
  LOG_REM_CLIENT,            // Remove a control client from the output list 
  LOG_TRANS_CATALOG,         // Read a transaction catalog
  LOG_LOG_TRANS,             // Log a transaction
  LOG_TRANS_EMAIL            // Configure the transaction email list 
} LoggerMessageType;


// Objects of the following form are used to pass messages to the
// logger task. Please use pack_logger_<type>() to set up one of these
// messages.

#define DATE_LEN 20

typedef struct {
  LoggerMessageType type;      // The type of logger input message 
  union {                      // The body of the message 

    struct {                   // type = LOG_CHDIR 
      char dir[CP_FILENAME_MAX+1];// The directory on which to open the file 
    } chdir;

    struct {                   // type = LOG_OPEN 
      char dir[CP_FILENAME_MAX+1];// The directory on which to open the file 
    } open;

    struct {                   // type = LOG_MESSAGE 
      unsigned seq;            // The sequence number of this message
      bool end;                // True if this is the end of a message
      bool interactive;        // True if this message was generated
			       // by an interactive command
      sza::array::LogStream nature; // The disposition of the message 
      char text[LOG_MSGLEN+1]; // The text of the message 
    } message;

    struct {                   // type = LOG_ADD_CLIENT | L5BOG_REM_CLIENT 
      sza::util::Pipe* pipe;   // The control-client reply pipe 
    } client;

    struct {                   // type = LOG_TRANSACTION_CATALOG
      char catalog[CP_FILENAME_MAX+1];// The catalog to open
      bool clear;
    } transCatalog;

    struct {                   // type = LOG_TRANSACTION_CATALOG
      // The device name
      char device[sza::array::TransactionManager::DEV_NAME_MAX+1];
      // The serial number
      char serial[sza::array::TransactionManager::SERIAL_NAME_MAX+1];
      // The location name
      char location[sza::array::TransactionManager::LOCATION_NAME_MAX+1];
      // The date
      double date;
      // Who?
      char who[sza::array::TransactionManager::WHO_NAME_MAX+1];
      // Comment
      char comment[LOG_MSGLEN-sza::array::TransactionManager::PREFIX_LEN+1];
    } logTrans;

    struct {
      bool add;                        // If true, add the address, if false, delete it
      char address[CP_FILENAME_MAX+1]; // An email address
    } email;

  } body;

} LoggerMessage;

// The following functions pack messages into a LoggerMessage object.

int pack_logger_shutdown(LoggerMessage *msg);
int pack_logger_chdir(LoggerMessage *msg, char *dir);
int pack_logger_open(LoggerMessage *msg, char *dir);
int pack_logger_flush(LoggerMessage *msg);
int pack_logger_close(LoggerMessage *msg);

int pack_logger_message(LoggerMessage *msg, char *text, 
			sza::array::LogStream nature, 
			unsigned seq=0, bool end=true, bool interactive=false);

int pack_logger_add_client(LoggerMessage *msg, sza::util::Pipe *client);
int pack_logger_rem_client(LoggerMessage *msg, sza::util::Pipe *client);

int pack_logger_transaction_catalog(LoggerMessage *msg, char *path, bool clear);
int pack_logger_log_transaction(LoggerMessage *msg, char *device, char* serial,
				char* location, double date, char* who,
				char* comment);
int pack_logger_transaction_email(LoggerMessage *msg, bool add, char *email);

// Return true if the named device is recognized.

bool log_isValidDevice(Logger* log, char* device);

/*
 * The following function attempts to send a packed LoggerMessage to the
 * logger thread.
 */
PipeState send_LoggerMessage(ControlProg *cp, LoggerMessage *msg,
			     long timeout);

int sendLoggerMessage(ControlProg* cp, std::string message, bool isErr, bool interactive);

/*
 * The following function redirects the specified stream (stdout or stderr)
 * of the calling thread to the input pipe of the logger thread.
 */
int log_thread_stream(ControlProg *cp, FILE *stream);

/**.......................................................................
 * Return a list of transaction email addresses.
 */
std::vector<std::string>* getTransactionEmailList(ControlProg *cp);

/*-----------------------------------------------------------------------
 * Declare the API of the archiver thread.
 *-----------------------------------------------------------------------*/

typedef struct Archiver Archiver;  /* The type of the thread resource object */

CP_THREAD_FN(archiver_thread);     /* The archiver thread start function */
CP_NEW_FN(new_Archiver);           /* The archiver resource constructor */
CP_DEL_FN(del_Archiver);           /* The archiver resource destructor */
CP_STOP_FN(stop_Archiver);         /* Send a shutdown message to the archiver */

/*
 * Enumerate the archiver message types.
 */
typedef enum {
  ARC_FRAME,          /* Save the latest integration. When done */
                      /*  set arc->fb.integrate to 1 and signal */
                      /*  arc->fb.start */
  ARC_SHUTDOWN,       /* Cleanup and shutdown the archiver thread */
  ARC_CHDIR,          /* Change the default archive file directory */
  ARC_OPEN,           /* Open a new archive file in a given place */
  ARC_FLUSH,          /* Flush unwritten data to the current file */
  ARC_CLOSE,          /* Close the current archive file */
  ARC_SAMPLING,       /* Set the number of samples per integration */
  ARC_FILTER,         /* Enable/disable feature-based filtering */
  ARC_FILE_SIZE,      /* The number of frames to record in each */
                      /*  archive file before automatically opening */
                      /*  a new one. */
  ARC_ADD_CLIENT,     /* Add a control client to the list of connected */
                      /*  clients */
  ARC_REM_CLIENT,     /* Remove a control client from the list of connected */
                      /*  clients */
  ARC_TV_OFFSET_DONE, /* A message sent by the control program to let
			 the archiver know that a tv_offset command
			 has been successfully completed.  This is
			 sent by the scanner task when the new offsets
			 have been written to the latest frame sent to
			 the archiver */
  ARC_NEW_FRAME      /* A message sent by the control program to
			request the archiver to terminate the current
			integration and start a new one */
} ArchiverMessageType;

/*
 * Objects of the following form are used to pass messages to the
 * archiver task. Please use pack_archiver_<type>() to set up one of
 * these messages.
 */
typedef struct {
  ArchiverMessageType type;    /* The type of archiver input message */
  union {                      /* The body of the message */
    struct {                   /* type = ARC_CHDIR */
      char dir[CP_FILENAME_MAX+1];/* The current archiving directory */
    } chdir;
    struct {                   /* type = ARC_OPEN */
      char dir[CP_FILENAME_MAX+1];/* The current archiving directory */
    } open;
    struct {                   /* type = ARC_SAMPLING */
      unsigned nframe;         /* The number of samples per integration */
      unsigned seq;            /* A sequence number associated with this command */
    } sampling;
    struct {                   /* type = ARC_FILTER */
      int enable;              /* True to tell the archiver only record */
                               /*  frames who's frame.features register */
                               /*  has a non-zero value. False to tell it */
                               /*  to archive everything (the default) */
    } filter;
    struct {                   /* type = ARC_FILE_SIZE */
      unsigned nframe;         /* The number of frames per file, or zero to */
                               /*  allow any number of frames per file. */
    } file_size;
    struct {                   /* type = ARC_ADD_CLIENT | ARC_REM_CLIENT */
#ifndef NEW_PIPE
      Pipe *pipe;              /* The control-client reply pipe */
#else
      sza::util::Pipe *pipe;              /* The control-client reply pipe */
#endif
    } client;
    struct {
      unsigned seq;
    } tv_offset_done;
    struct {
      unsigned seq;
    } newFrame;
  } body;
} ArchiverMessage;

/*
 * The following functions pack messages into an ArchiverMessage object.
 */
int pack_archiver_frame(ArchiverMessage *msg);
int pack_archiver_shutdown(ArchiverMessage *msg);
int pack_archiver_chdir(ArchiverMessage *msg, char *dir);
int pack_archiver_open(ArchiverMessage *msg, char *dir);
int pack_archiver_flush(ArchiverMessage *msg);
int pack_archiver_close(ArchiverMessage *msg);
int pack_archiver_sampling(ArchiverMessage *msg, unsigned nframe, unsigned seq);
int pack_archiver_filter(ArchiverMessage *msg, int enable);
int pack_archiver_file_size(ArchiverMessage *msg, unsigned nframe);

#ifndef NEW_PIPE
int pack_archiver_add_client(ArchiverMessage *msg, Pipe *client);
int pack_archiver_rem_client(ArchiverMessage *msg, Pipe *client);
#else
int pack_archiver_add_client(ArchiverMessage *msg, sza::util::Pipe *client);
int pack_archiver_rem_client(ArchiverMessage *msg, sza::util::Pipe *client);
#endif

int pack_archiver_tv_offset_done(ArchiverMessage *msg, unsigned seq);
int pack_archiver_newFrame(ArchiverMessage *msg, unsigned seq);
/*
 * The following function attempts to send a packed ArchiverMessage to the
 * archiver thread.
 */
PipeState send_ArchiverMessage(ControlProg *cp, ArchiverMessage *msg,
			       long timeout);

/*
 * This function adds a register frame to the current integration.
 */
int arc_integrate_frame(ControlProg *cp, RegRawData *frame);
int arc_integrate_frame_old(ControlProg *cp, RegRawData *frame);

/*
 * Set the default archiver integration duration (frames).
 */
enum {ARC_DEF_NFRAME=10};

/*-----------------------------------------------------------------------
 * Declare the API of the scheduler thread.
 *-----------------------------------------------------------------------*/

typedef struct Scheduler Scheduler; /* The type of the thread resource object */

Scheduler *cp_Scheduler(ControlProg *cp);

CP_THREAD_FN(scheduler_thread);   /* The scheduler thread start function */
CP_NEW_FN(new_Scheduler);         /* The scheduler resource constructor */
CP_DEL_FN(del_Scheduler);         /* The scheduler resource destructor */
CP_STOP_FN(stop_Scheduler);       /* Send a shutdown message to the scheduler */

/*
 * Enumerate scheduler message types.
 */
typedef enum {
  SCH_SHUTDOWN,        /* An instruction to cleanup and shutdown */
  SCH_COMMAND,         /* A user command string */
  SCH_RTC_STATE,       /* Controller connection status */
  SCH_ADD_CLIENT,      /* Add a control client to the output list */
  SCH_REM_CLIENT,      /* Remove a control client from the output list */
  SCH_RTCNETMSG,       /* A message received from the controller */
  SCH_MARK_DONE,       /* A marker transaction completion message */
  SCH_GRAB_DONE,       /* A marker transaction completion message */
  SCH_SETREG_DONE,     /* A setreg transaction completion message */
  SCH_TV_OFFSET_DONE,  /* A tv_offset transaction completion message */
  SCH_AUTO_DIR,        /* An autoqueue chdir operation */
  SCH_AUTO_POLL,       /* An autoqueue polling inerval change */
  SCH_AUTO_STATE,      /* An autoqueue on/off toggle */
  SCH_FRAME_DONE       /* A new frame has begun */
} SchedulerMessageType;

/*
 * Objects of the following form are used to pass messages to the
 * scheduler task. Please use pack_scheduler_<type>() to set up one of
 * these messages.
 */
typedef struct {
  SchedulerMessageType type;      /* The type of scheduler message */
  union {                         /* The body of the message */

    struct {                      /* type=SCH_COMMAND */
      char string[sza::array::CC_CMD_MAX+1];  /* The command string */
#ifndef NEW_PIPE
      Pipe *client;               /* The originating client */
#else
      sza::util::Pipe *client;               /* The originating client */
#endif
    } command;

    int rtc_online;               /* type=SCH_RTC_STATE */

    struct {                      /* type = SCH_ADD_CLIENT | SCH_REM_CLIENT */
#ifndef NEW_PIPE
      Pipe *pipe;                 /* The control-client reply pipe */
#else
      sza::util::Pipe *pipe;               /* The originating client */
#endif
    } client;

    struct {                      /* type=SCH_RTCNETMSG */
      sza::array::NetMsgId id;    /* The type of rtc network message */
      sza::array::RtcNetMsg msg;  /* The network message */
    } rtcnetmsg;

    struct {                      /* type=SCH_MARK_DONE */
      unsigned seq;               /* The sequence number of the transaction */
    } mark_done;

    struct {                      /* type=SCH_GRAB_DONE */
      unsigned seq;               /* The sequence number of the transaction */
    } grab_done;

    struct {                      /* type=SCH_SETREG_DONE */
      unsigned seq;               /* The sequence number of the transaction */
    } setreg_done;

    struct {                      /* type=SCH_TV_OFFSET_DONE */
      unsigned seq;               /* The sequence number of the transaction */
    } tv_offset_done;

    struct {                      /* type=SCH_FRAME_DONE */
      unsigned seq;               /* The sequence number of the transaction */
    } frame_done;

    struct {                      /* type=SCH_AUTO_DIR */
      char dir[CP_FILENAME_MAX] ; /* The name of the directory */
    } auto_dir;

    struct {                      /* type=SCH_AUTO_POLL */
      long poll;                  /* The interval in ms to poll */
    } auto_poll;

    struct {                      /* type=SCH_AUTO_STATE */
      int on;                     /* The interval in ms to poll */
    } auto_state;

  } body;

} SchedulerMessage;

/*
 * The following functions pack messages into an SchedulerMessage object.
 */
int pack_scheduler_shutdown(SchedulerMessage *msg);
int pack_scheduler_rtc_state(SchedulerMessage *msg, int online);

#ifndef NEW_PIPE
int pack_scheduler_command(SchedulerMessage *msg, char *command, Pipe *client);
int pack_scheduler_add_client(SchedulerMessage *msg, Pipe *client);
int pack_scheduler_rem_client(SchedulerMessage *msg, Pipe *client);
#else
int pack_scheduler_command(SchedulerMessage *msg, char *command, sza::util::Pipe *client);
int pack_scheduler_add_client(SchedulerMessage *msg, sza::util::Pipe *client);
int pack_scheduler_rem_client(SchedulerMessage *msg, sza::util::Pipe *client);
#endif

int pack_scheduler_rtcnetmsg(SchedulerMessage *msg, sza::array::NetMsgId id,
			     sza::array::RtcNetMsg *rtcmsg);
int pack_scheduler_mark_done(SchedulerMessage *msg, unsigned seq);
int pack_scheduler_grab_done(SchedulerMessage *msg, unsigned seq);
int pack_scheduler_setreg_done(SchedulerMessage *msg, unsigned seq);
int pack_scheduler_tv_offset_done(SchedulerMessage *msg, unsigned seq);
int pack_scheduler_frame_done(SchedulerMessage *msg, unsigned seq);

/*
 * The following function attempts to send a packed SchedulerMessage to the
 * scheduler thread.
 */
PipeState send_SchedulerMessage(ControlProg *cp, SchedulerMessage *msg,
				long timeout);

// Methods to do with the pager

int send_paging_state(ControlProg* cp, bool allow);

void listPager(ControlProg* cp, ListNode* node=0);

int sendPagingState(ControlProg* cp, int allow, 
		    unsigned mask=sza::array::PAGE_ENABLE, char* msg=0, ListNode* node=0);

int sendPagerCondition(ControlProg* cp, unsigned mode, 
		       sza::util::PagerMonitor::RegSpec* reg=0, ListNode* node=0);

int sendCmdTimeoutConfiguration(ControlProg* cp, unsigned seconds, ListNode* node=0);
int sendCmdTimeoutConfiguration(ControlProg* cp, bool active, ListNode* node=0);

// Methods to do with the array configuration

int sendArrayConfiguration(ControlProg* cp, ListNode* node, 
			   unsigned mode, 
			   unsigned array  = sza::util::CarmaConfig::NONE, 
			   unsigned config = sza::util::CarmaConfig::NONE);

int sendAddArrayAntenna(ControlProg* cp, ListNode* node,
			unsigned array, 
			unsigned iPad,
			unsigned antType, 
			int iAnt = -1);

int sendRemArrayAntenna(ControlProg* cp, ListNode* node,
			unsigned array, unsigned iPad);

void listArrayConfig(ControlProg* cp, ListNode* node, unsigned config);

/*-----------------------------------------------------------------------
 * Declare the API of the navigator thread.
 *-----------------------------------------------------------------------*/

#ifndef navigator_h
typedef struct Navigator Navigator; /* The type of the thread resource object */
#endif

Navigator *cp_Navigator(ControlProg *cp);

CP_THREAD_FN(navigator_thread);   /* The navigator thread start function */
CP_NEW_FN(new_Navigator);         /* The navigator resource constructor */
CP_DEL_FN(del_Navigator);         /* The navigator resource destructor */
CP_STOP_FN(stop_Navigator);       /* Send a shutdown message to the navigator */

/*
 * Enumerate navigator message types.
 */
typedef enum {
  NAV_SHUTDOWN,        /* An instruction to cleanup and shutdown */
  NAV_RTC_STATE,       /* Controller connection status */
  NAV_CATALOG,         /* A "read source catalog" command */
  NAV_SCAN_CATALOG,    /* A "read scan catalog" command */
  NAV_UT1UTC,          /* A "read UT1-UTC ephemeris" command */
  NAV_SITE             /* A "record sza location" command */
} NavigatorMessageType;

/*
 * The following container is used to convey site-location messages.
 */
typedef struct {
  double longitude;               /*  East longitude (radians) */
  double latitude;                /*  Latitude (radians) */
  double altitude;                /*  Altitude ASL (m) */
} NavSiteMsg;

/*
 * Objects of the following form are used to pass messages to the
 * navigator task. Please use pack_navigator_<type>() to set up one of
 * these messages.
 */
typedef struct {
  NavigatorMessageType type;              // The type of navigator message 
  union {                                 // The body of the message 
    int rtc_online;                       // type=NAV_RTC_STATE 
    char catalog[CP_FILENAME_MAX+1];      // type=NAV_CATALOG 
    char scan_catalog[CP_FILENAME_MAX+1]; // type=NAV_SCAN_CATALOG 
    char ut1utc[CP_FILENAME_MAX+1];       // type=NAV_UT1UTC 
    NavSiteMsg site;                      // type=NAV_SITE 
  } body;
} NavigatorMessage;

/*
 * The following functions pack messages into an NavigatorMessage object.
 */
int pack_navigator_shutdown(NavigatorMessage *msg);
int pack_navigator_rtc_state(NavigatorMessage *msg, int online);
int pack_navigator_catalog(NavigatorMessage *msg, char *path);
int pack_navigator_scan_catalog(NavigatorMessage *msg, char *path);
int pack_navigator_ut1utc(NavigatorMessage *msg, char *path);
int pack_navigator_site(NavigatorMessage *msg, double longitude,
			double latitude, double altitude);
/*
 * The following function attempts to send a packed NavigatorMessage to the
 * navigator thread.
 */
PipeState send_NavigatorMessage(ControlProg *cp, NavigatorMessage *msg,
				long timeout);

/*-----------------------------------------------------------------------
 * Declare the API of the grabber thread.
 *-----------------------------------------------------------------------*/

typedef struct Grabber Grabber;  /* The type of the thread resource object */

CP_THREAD_FN(grabber_thread);     /* The grabber thread start function */
CP_NEW_FN(new_Grabber);           /* The grabber resource constructor */
CP_DEL_FN(del_Grabber);           /* The grabber resource destructor */
CP_STOP_FN(stop_Grabber);         /* Send a shutdown message to the grabber */

/*
 * Enumerate the grabber message types.
 */
typedef enum {
  GRAB_IMAGE,                  /* Save the latest image. When done */
                               /*  set grab->im.save to 1 and signal */
                               /*  grab->im.start */
  GRAB_SHUTDOWN,               /* Cleanup and shutdown the grabber thread */
  GRAB_CHDIR,                  /* Change the default grabber file directory */
  GRAB_OPEN,                   /* Tell the grabber thread to archive suqsequent
				  image */
  GRAB_FLUSH,                  /* Flush unwritten data to the current file */
  GRAB_CLOSE,                  /* Tell the grabber thread not to archive
				  subsequent images */
  GRAB_SEQ                     /* Tell the gabber thread to update its grabber
				  sequence number */
} GrabberMessageType;

/*
 * Objects of the following form are used to pass messages to the
 * grabber task. Please use pack_grabber_<type>() to set up one of
 * these messages.
 */
typedef struct {
  GrabberMessageType type;    /* The type of grabber input message */
  union {                      /* The body of the message */
    struct {                   /* type = GRAB_CHDIR */
      char dir[CP_FILENAME_MAX+1];/* The current archiving directory */
    } chdir;
    struct {                   /* type = GRAB_OPEN */
      char dir[CP_FILENAME_MAX+1];/* The current archiving directory */
    } open;
    unsigned seq;
  } body;
} GrabberMessage;

/*
 * The following functions pack messages into an GrabberMessage object.
 */
int pack_grabber_image(GrabberMessage *msg);
int pack_grabber_shutdown(GrabberMessage *msg);
int pack_grabber_chdir(GrabberMessage *msg, char *dir);
int pack_grabber_open(GrabberMessage *msg, char *dir);
int pack_grabber_flush(GrabberMessage *msg);
int pack_grabber_close(GrabberMessage *msg);

/*
 * The following function attempts to send a packed GrabberMessage to the
 * grabber thread.
 */
PipeState send_GrabberMessage(ControlProg *cp, GrabberMessage *msg,
			       long timeout);

/*
 * This function copies the current image to the grabber image buffer for
 * archiving.
 */
int grabber_save_image(ControlProg *cp, unsigned short *image, 
		       unsigned long utc[2], signed actual[3],
		       signed expected[3]);

/*-----------------------------------------------------------------------
 * Declare the API of the terminal thread.
 *-----------------------------------------------------------------------*/

typedef struct Term Term;  /* The type of the thread resource object */

CP_THREAD_FN(term_thread);     /* The term thread start function */
CP_NEW_FN(new_Term);           /* The term resource constructor */
CP_DEL_FN(del_Term);           /* The term resource destructor */
CP_STOP_FN(stop_Term);         /* Send a shutdown message to the term */

/*
 * Enumerate the term message types.
 */
typedef enum {
  TERM_SHUTDOWN,               /* Cleanup and shutdown the term thread */
  TERM_ENABLE,                 /* Tell the terminal thread to
				  dis/allow subsequent pager commands */
  TERM_REG_PAGE,               /* De/Activate the pager */
  TERM_MSG_PAGE,               /* De/Activate the pager */
  TERM_IP,                     /* Set the IP address to use for the pager */
  TERM_EMAIL,                  /* Add an email address */
  TERM_AUTO_INTERVAL,          // Set the interval for auto-reenable of the pager
  TERM_AUTO_ENABLE             // Enable auto-reenabling of the pager
} TermMessageType;
/*
 * Enumerate the supported pager devices
 */
typedef enum {
  PAGER_MAPO_REDLIGHT,
  PAGER_DOME_PAGER,
  PAGER_ELDORM_PAGER,
  PAGER_NONE
} PagerDev;
/*
 * Objects of the following form are used to pass messages to the
 * term task. Please use pack_term_<type>() to set up one of
 * these messages.
 */
typedef struct {

  TermMessageType type;  /* The type of term input message */

  union {
    char ip[CP_FILENAME_MAX+1]; // An IP address for the pager control
				// device
    struct {
      bool add;                 // If true, add the address, if false, delete it
      char address[CP_FILENAME_MAX+1]; // An IP address for the pager control device
    } email;

    struct {
      int on;              /* 1 -- turn pager on
			      0 -- turn pager off */
      char msg[CP_FILENAME_MAX+1]; // The message
      char comment[CP_FILENAME_MAX+1]; // A comment
    } page;

    struct {
      bool enable; // True to enable the pager, false to disable it
    } enable;

    struct {
      bool enable; // True to turn on auto-reenabling of the pager
    } autoEnable;

    struct {
      unsigned seconds; // The interval, in seconds, after which to
			// automatically reenable the pager
    } autoInterval;

  } body;

  PagerDev dev;        /* Which device are we addressing? */

} TermMessage;

/*
 * The following functions pack messages into an TermMessage object.
 */
int pack_term_shutdown(TermMessage *msg);
int pack_term_reg_page(TermMessage *msg, char* reg=NULL, char* comment=NULL, bool on=true);
int pack_term_msg_page(TermMessage *msg, std::string txt, bool on=true);
int pack_pager_ip(TermMessage *msg, PagerDev dev, char *ip);
int pack_pager_email(TermMessage *msg, bool add, char *email);
int pack_pager_enable(TermMessage *msg, bool enable);
int packPagerAutoEnable(TermMessage *msg, bool enable);
int packPagerAutoEnable(TermMessage *msg, unsigned seconds);

int send_reg_page_msg(ControlProg* cp, char* reg, char* comment=0);

MONITOR_CONDITION_HANDLER(sendRegPage);

/*
 * The following function attempts to send a packed TermMessage to the
 * term thread.
 */
PipeState send_TermMessage(ControlProg *cp, TermMessage *msg,
			       long timeout);

/**.......................................................................
 * Return a list of pager email addresses.
 */
std::vector<std::string>* getPagerEmailList(ControlProg *cp);

/*-----------------------------------------------------------------------
 * Other functions.
 *-----------------------------------------------------------------------*/

/*
 * The following function can be called to retrieve the state object of
 * a given thread.
 */

/*
 * Get the register map of the control program. Note that
 * once created by the control program the register map
 * must be treated as readonly. No locking is then required.
 */
ArrayMap *cp_ArrayMap(ControlProg *cp);

/*
 * Provide access to the default antenna set
 *
 * Input:
 *  cp      ControlProg *  The resource object of the control program.
 * Output:
 *  return       AntNum *  The default antenna set
 */
sza::util::AntNum* cp_AntSet(ControlProg *cp);

/*
 * The following functions send messages to control client output
 * pipes.
 */

  /* Send a text message to a control client (timeout=0) */

typedef struct {
  sza::array::CcNetMsgId id;    /* The type of network message in 'mgs' */
  sza::array::CcNetMsg msg;     /* The network message to be sent */
} CcPipeMsg;

#ifndef NEW_PIPE
int queue_cc_message(Pipe *client, CcPipeMsg *msg);
#else
int queue_cc_message(sza::util::Pipe *client, CcPipeMsg *msg);
#endif

/*
 * Queue a network command object to be sent to the real-time controller.
 *
 * By default, we will deactivate the timer when any command queued by
 * this method is sent to the translator.
 */
int queue_rtc_command(ControlProg *cp, sza::array::RtcNetCmd *cmd, 
		      sza::array::NetCmdId type, bool deactivateTimeout=true);

/*
 * Return true if the real-time controller is currently connected.
 */
int cp_rtc_online(ControlProg *cp);

/*
 * The following functions pack messages into an ControlMessage object.
 */
int cp_request_shutdown(ControlProg *cp);
int cp_request_restart(ControlProg *cp);
int cp_report_exit(ControlProg *cp);
int cp_initialized(ControlProg *cp);

void configureCmdTimeout(ControlProg* cp, unsigned int seconds);
void configureCmdTimeout(ControlProg* cp, bool activate);
void allowTimeOutPaging(ControlProg* cp, bool allow);

std::string cp_startupScript(ControlProg *cp);

int sendClearPagerMsg(ControlProg* cp);
int sendResetPagerMsg(ControlProg* cp);
int sendListPagerMsg(ControlProg* cp);
int sendEnablePagerMsg(ControlProg* cp, bool enable);

int sendAddPagerRegisterMsg(ControlProg* cp, std::string regName,
			    double min, double max, bool delta, unsigned nFrame, 
			    bool outOfRange, char* comment);

int sendRemPagerRegisterMsg(ControlProg* cp, std::string regName);


ControlProg* new_ControlProg();
ControlProg* new_ControlProgViewerServer();

double getRegVal(ControlProg* cp, sza::util::RegDescription& regDesc);

// External methods to send messages about the array configuration

int sendArrayConfigMsg(ControlProg* cp, unsigned array, unsigned config);

int sendAddArrayAntennaMsg(ControlProg* cp, unsigned array, unsigned iPad, 
			   unsigned antType, int iAnt);

int sendRemArrayAntennaMsg(ControlProg* cp, unsigned array, unsigned iPad);

sza::util::CarmaConfig* getArrayConfig(ControlProg* cp, unsigned array);

ControlProg* new_ControlProg(std::string startupScript);
ControlProg* del_ControlProg(ControlProg* cp);
int cp_event_loop(ControlProg* cp);
int cp_event_loop_MonitorServer(ControlProg* cp);

/*
 * Define the continuation statuses of the event loop.
 */
typedef enum {
  CP_CONTINUE,     /* Continue the event loop */
  CP_SHUTDOWN,     /* Shutdown the control program */
  CP_RESTART       /* Restart the control program */
} CpWhatNext;

CpWhatNext whatNext(ControlProg* cp);

//=======================================================================
// Define a class for serving up a monitor frame to viewer clients
//=======================================================================

namespace sza {
  namespace array {

    class ViewerServer : public sza::util::NetMonitorFrameServer {
    public:

      ViewerServer(bool spawnThread, unsigned port, sza::util::NetMonitorFrame* nmf, int fdRead);
      ~ViewerServer();

      void run();

    private:

      void registerReceiptOfFrame();

      ControlProg* cp_;
    };

  }
}
#endif
