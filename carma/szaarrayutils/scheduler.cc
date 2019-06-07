#include <iostream>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <utime.h>
#include <dirent.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/ioctl.h>

#include "lprintf.h"
#include "szacontrol.h"
#include "szascript.h"
#include "list.h"
#include "szaconst.h"
#include "astrom.h"
#include "scheduler.h"
#include "pathname.h"

#include "carma/szautil/CarmaConfig.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/FdSet.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/LogMsgHandler.h"
#include "carma/szautil/TimeVal.h"

#include <sstream>

using namespace std;
using namespace sza::array;

/*
 * The following structure is used to record the status of noise
 * source transactions.
 */
typedef struct {
  unsigned seq;    /* The sequence number of the last noise source command */
  int done;        /* The boolean completion-status of the last transaction */
} Noise;

/*
 * The following structure is used to record the status of phase-shifter
 * positioning transactions.
 */
typedef struct {
  unsigned seq;    // The sequence number of the last positioning
                   //  command sent to the caltert module
  unsigned done;   // A bitmask of completion status for all
		   // antennas.  Each bit will be set to 1 when the
		   // corresponding antenna has completed
} CalTert;

/*
 * The following structure is used to record the status of the last
 * newFrame command
 */
typedef struct {
  unsigned seq;    // The sequence number of the last positioning
                   //  command sent to the caltert module
  unsigned done;   // A bitmask of completion status for all
		   // antennas.  Each bit will be set to 1 when the
		   // corresponding antenna has completed
} FrameStatus;

/*
 * The following structure is used to record the status of a CAN command
 */
typedef struct {
  unsigned seq;    // The sequence number of the last positioning
                   //  command sent 
  unsigned done;   // A bitmask of completion status for all
		   // antennas.  Each bit will be set to 1 when the
		   // corresponding antenna has completed
} Can;

/*
 * The following structure is used to record the status of antenna IF switch
 * positioning transactions.
 */
typedef struct {
  unsigned seq;    // The sequence number of the last positioning
                   //  command sent to the antenna IF module
  unsigned done;   // A bitmask of completion status for all
		   // antennas.  Each bit will be set to 1 when the
		   // corresponding antenna has completed
} IFMod;

/*
 * The following structure is used to record the status of channelizer
 * long-duration configuration transactions.
 */
typedef struct {
  unsigned seq;    /* The sequence number of the last long-duration */
                   /*  command sent to the channelizer. */
  int done;        /* The boolean completion-status of the last transaction */
} Chzr;

/*
 * The state of the last pmac transaction is recorded in a container
 * of the following type.
 */
typedef struct {
  unsigned seq;     // The pmac sequence number of the last 
                    // requested transaction. 
  unsigned done;    // A bitmask of completion status for all
		    // antennas.  Each bit will be set to 1 when the
		    // corresponding antenna has completed
  unsigned antennas;
 } PmacState;

/*
 * The state of the last frame-marker transaction is recorded in a container
 * of the following type.
 */
typedef struct {
  unsigned seq;             /* The sequence number of the last requested */
                            /*  marker transaction. */
  int done;                 /* True after transaction number 'seq' has */
                            /*  completed. */
} MarkState;

/**
 * The state of the last setreg transaction is recorded in a container
 * of the following type.
 */
typedef struct {
  unsigned seq;             /* The sequence number of the last requested */
                            /*  setreg transaction. */
  int done;                 /* True after transaction number 'seq' has */
                            /*  completed. */
} SetRegState;

/*
 * The state of the last tv_offset transaction is recorded in a container
 * of the following type.
 */
typedef struct {
  unsigned seq;             /* The sequence number of the last requested */
                            /*  offset transaction. */
  int done;                 /* True after transaction number 'seq' has */
                            /*  completed. */
} TvOffsetState;

/**
 * The state of the last frame grabber transaction is recorded in a container
 * of the following type.
 */
typedef struct {
  unsigned seq;             /* The sequence number of the last requested */
                            /*  grabber transaction. */
  int done;                 /* True after transaction number 'seq' has */
                            /*  completed. */
} Grab;
  
/*
 * The following stream is passed to step_script() and
 * run_interactive_command() as the stream to use for log messages.
 */
typedef struct {
  OutputStream *output;     /* The log stream */
  char buf[CC_MSG_MAX+1];   /* The buffer into which 'output' writes to */
  int nbuf;                 /* The number of characters in buf[] */
  int trunc;                /* True if the string in buf[] has been truncated */
} ScriptLog;

static OUTPUT_WRITE_FN(sch_log_output);
static OUTPUT_WRITE_FN(sch_log_output_old);

/*
 * The following stream and message container are used to report
 * the status of the schedule queue to connected control clients.
 */
typedef struct {
  OutputStream *output;  /* The output stream to use to compose messages */
  CcPipeMsg pmsg;        /* The message container */
} SchedStatus;

/*
 * A Scheduler object is used to maintain the state of the scheduler thread.
 */
typedef List ScriptList;

struct Scheduler {
  ControlProg *cp;             /* The state object of the control program */
  sza::util::Pipe *pipe;       /* An externally allocated control-input pipe */

  InputStream *input;          /* An input stream for schedules etc.. */
  ScriptLog log;               /* The log output context for lprintf() */
  ListMemory *list_mem;        /* A free-list of list objects and nodes */
  Script *schedule;            /* The currently running schedule */
  Script *interactive;         /* The script environment used to
				  compile and run interactive
				  commands */
  Script *initialize;          /* The script that is run after reboots */
  ScriptList *sched_pool;      /* The list of unused schedule environments */
  ScriptList *sched_list;      /* The list of schedules waiting to be run */
  List *input_pool;            /* A list of InputStream's to use when */
                               /*  compiling scripts. */
  List *clients;               /* The list of connected clients */
  SchedStatus stat;            /* The schedule status message container */
  Site *site;                  /* The SZA site description */
  int rtc_online;              /* True if the controller is connected */
  int initializing;            /* True until the control-program */
                               /*  initialization script has completed. */
  int suspend;                 /* True if the currently running schedule */
                               /*  is temporarily suspended. */
  long poll_time;              /* The interval between executing commands */

  struct {
    int on;                    /* True if auto queuing is turned on */
    int pending;               /* True if the schedule we are currently running
				  is an auto-queued file */
    char *dir;                 /* The head of the auto_queue directory */
    char *penddir;             /* The directory in which to look for pending
				  auto-queued files */
    char *donedir;             /* The directory in which to look for pending
				  auto-queued files */
    char *pendname;            /* The full pathname of the auto queued file */
    char *donename;            /* The full pathname of the file to which 
				  pendname should be moved after the schedule
				  has been executed */
    long poll_time;            /* The interval between successive checks of the
				  pending directory when no schedule is 
				  present */
  } autoqueue;

  int pager_allow;             /* True if paging requests are allowed */

  FrameStatus frame;           // The frame transaction status
  CalTert caltert;             // The caltert transaction status
  Can can;                     // The can transaction status
  IFMod ifMod;                 // The antenna IF transaction status
  Noise noise;                 /* The noise source status */
  Chzr chzr;                   /* The channelizer transaction statuses */
  Grab grab;                   /* The frame grabber transaction statuses */
  double horizon;              /* The default elevation of the horizon */
                               /*  to be used by the show command (radians). */
  PmacState pmac;              /* The container of the status of the last */
                               /*  tracker/pmac transaction. */
  MarkState mark;              /* The container of the status of the last */
                               /*  marker transaction. */
  SetRegState setreg;          /* The container of the status of the last */
                               /*  setreg transaction. */
  TvOffsetState tv_offset;     /* The container of the status of the last */
                               /*  tv_offset transaction. */
  struct {
    HashTable *table;          /* The symbol table of signal names */
    Symbol *source_set;        /* The signal that is sent when a source sets */
  } signals;
  SchedCache cache;            /* The cache of pertinent user-specified */
                               /*   configuration parameters. */
};

static LOG_DISPATCHER(client_output_dispatcher);
static LOG_DISPATCHER(writeClientMsg);

static int sch_execute_command(Scheduler *sch, char *string, 
			       sza::util::Pipe *client);

static int sch_next_schedule(Scheduler *sch);
static int sch_reschedule(Scheduler *sch);

static int sch_stage_schedule(Scheduler *sch, Script *sc);
static ListNode *sch_nth_schedule(Scheduler *sch, unsigned n);

static int sch_add_client(Scheduler *sch, sza::util::Pipe *client);

static int sch_rem_client(Scheduler *sch, sza::util::Pipe *client);

static int prefix_status_report(Scheduler *sch, char *prefix);

static int send_status_report(Scheduler *sch, sza::util::Pipe *client);

static int sch_reset_signals(Scheduler *sch);
static HASH_SCAN_FN(sch_zero_signal);
/*
 * Functions to do with auto queueing.
 */
static int sch_next_auto_file(Scheduler *sch);
static int sch_remove_pending_file(Scheduler *sch);
static int chdir_auto(Scheduler *sch, char *dir);

/*
 * When a schedule is running, the time between executing leaf statements
 * is specified by the following macros (milliseconds). The first is
 * used for normal script statements. The second is used when the script
 * is polling an until statement.
 */
#define SCH_RUN_INTERVAL 10
#define SCH_POLL_INTERVAL 100

/*.......................................................................
 * Create the context object of a scheduler thread.
 *
 * Input:
 *  cp     ControlProg *   The state object of the control program.
 *  pipe          Pipe *   The pipe to receive control input from.
 * Output:
 *  return        void *   The new object, or NULL on error.
 */
CP_NEW_FN(new_Scheduler)
{
  Scheduler *sch;          /* The object to be returned */
/*
 * Allocate the container.
 */
  sch = (Scheduler* )malloc(sizeof(Scheduler));
  if(!sch) {
    lprintf(stderr, "new_Scheduler: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the container
 * at least up to the point at which it can be safely passed to del_Scheduler().
 */
  sch->cp = cp;
  sch->pipe = pipe;
  sch->input = NULL;
  sch->log.output = NULL;
  sch->log.buf[0] = '\0';
  sch->log.nbuf = 0;
  sch->log.trunc = 0;
  sch->list_mem = NULL;
  sch->schedule = NULL;
  sch->interactive = NULL;
  sch->initialize = NULL;
  sch->sched_pool = NULL;
  sch->sched_list = NULL;
  sch->input_pool = NULL;
  sch->clients = NULL;
  sch->stat.output = NULL;
  sch->stat.pmsg.id = CC_SCHED_MSG;
  sch->stat.pmsg.msg.sched.text[0] = '\0';
  sch->site = NULL;
  sch->rtc_online = 0;
  sch->initializing = 1;
  sch->suspend = 0;
  sch->poll_time = -1;   /* Don't poll while a schedule isn't running */
  sch->noise.seq = 0;
  sch->noise.done = 0;
  sch->chzr.seq = 0;
  sch->chzr.done = 0;
  sch->grab.seq = 0;
  sch->grab.done = 0;
  sch->horizon = 0.0;
  sch->frame.seq = 0;
  sch->frame.done = 0;
  sch->caltert.seq = 0;
  sch->caltert.done = 0;
  sch->can.seq = 0;
  sch->can.done = 0;
  sch->ifMod.seq = 0;
  sch->ifMod.done = 0;
  sch->pmac.seq = 0;
  sch->pmac.done = 0;
  sch->pmac.antennas = 0;
  sch->mark.seq = 0;
  sch->mark.done = 0;
  sch->tv_offset.seq = 0;
  sch->tv_offset.done = 0;
  sch->signals.table = NULL;
  sch->signals.source_set = NULL;
  sch->cache.archive.combine = ARC_DEF_NFRAME;
  sch->cache.archive.filter = 0;
  /*
   * By default, turn auto queueing off, and NULLify the directory and
   * pathname pointers.
   */
  sch->autoqueue.on = 0;
  /*
   * Allow paging initially.  Once the pager is activated, paging will
   * be disallowed until the pager is reset.
   */
  sch->pager_allow = 1;
  /* 
   *  Set to 10 minutes, but we won't poll at all while auto-queueing is 
   *  turned off 
   */
  sch->autoqueue.poll_time = 1000*60*10; 
  sch->autoqueue.pending = 0;
  sch->autoqueue.dir = NULL;
  sch->autoqueue.penddir = NULL;
  sch->autoqueue.donedir = NULL;
  sch->autoqueue.pendname = NULL;
  sch->autoqueue.donename = NULL;
/*
 * Check for the environment variable that contains the top-level
 * SZA configuration directory.
 */
  if(!getenv("SZA_DIR")) {
    lprintf(stderr, "The SZA_DIR environment variable needs to be defined.\n");
    return del_Scheduler(sch);
  };
/*
 * Allocate an log output stream.
 */
  sch->log.output = new_OutputStream();
  if(!sch->log.output || open_OutputStream(sch->log.output, sch,
					   sch_log_output, 0))
    return del_Scheduler(sch);
/*
 * Allocate a freelist from which to allocate lists and their nodes.
 */
  sch->list_mem = new_ListMemory(10, 20);
  if(!sch->list_mem)
    return del_Scheduler(sch);
/*
 * Allocate the list of allocated but no longer assigned client schedule
 * environments.
 */
  sch->sched_pool = new_List(sch->list_mem);
  if(!sch->sched_pool)
    return del_Scheduler(sch);
/*
 * Allocate the list pending schedules.
 */
  sch->sched_list = new_List(sch->list_mem);
  if(!sch->sched_list)
    return del_Scheduler(sch);
/*
 * Allocate the list of allocated but unassigned parser input-streams.
 */
  sch->input_pool = new_List(sch->list_mem);
  if(!sch->input_pool)
    return del_Scheduler(sch);
/*
 * Allocate a list for recording connected control clients.
 */
  sch->clients = new_List(sch->list_mem);
  if(!sch->clients)
    return del_Scheduler(sch);
/*
 * Allocate the output stream that is used to compose schedule queue
 * status messages.
 */
  sch->stat.output = new_OutputStream();
  if(!sch->stat.output ||
     open_StringOutputStream(sch->stat.output, 1,sch->stat.pmsg.msg.sched.text,
			     CC_MSG_MAX))
    return del_Scheduler(sch);
/*
 * Create a site description object.
 */
  sch->site = new_Site();
  if(!sch->site)
    return del_Scheduler(sch);
/*
 * Create a hash-table for recording signal names.
 * All names added and looked up in this table will already have been
 * converted to lower case so there is no need to have the
 * hash table add the overhead needed to make case-insensitive
 * comparisons.
 */
  sch->signals.table = new_HashTable(NULL, 31, HONOUR_CASE, NULL, 0);
  if(!sch->signals.table)
    return del_Scheduler(sch);
/*
 * Predefine signals that we send to scripts on seeing certain events.
 */
  sch->signals.source_set = sch_add_signal(sch, "source_set");
  if(!sch->signals.source_set)
    return del_Scheduler(sch);
/*
 * Make sure that the hash table doesn't get deleted when scripts
 * are deleted.
 */
  ref_HashTable(sch->signals.table);
/*
 * Allocate the single script environment that is used for running
 * client commands.
 */
  sch->interactive = new_SzaScript(cp, 0, sch->signals.table);
  sch->interactive->interactive_ = true;

  if(!sch->interactive)
    return del_Scheduler(sch);
  return sch;
}

/*.......................................................................
 * Delete the state-object of a scheduler thread.
 *
 * Input:
 *  obj     void *   The Scheduler object to be deleted.
 * Output:
 *  return  void *   The deleted Scheduler object (always NULL).
 */
CP_DEL_FN(del_Scheduler)
{
  Scheduler *sch = (Scheduler* )obj;
  if(sch) {
    sch->log.output = del_OutputStream(sch->log.output);
    sch->schedule = sch_discard_schedule(sch, sch->schedule);
    sch->interactive = del_Script(sch->interactive);
    sch->initialize = del_Script(sch->initialize);
/*
 * Delete the pool of pending script environments.
 */
    if(sch->sched_list) {
      List *list = sch->sched_list;
      while(list->head)
	sch_discard_schedule(sch, (Script* )del_ListNode(list, list->head, 
							 NULL));
      sch->sched_list = del_List(sch->sched_list);
    };
/*
 * Delete the pool of unused script environments.
 */
    if(sch->sched_pool) {
      List *list = sch->sched_pool;
      while(list->head)
	del_Script((Script* )del_ListNode(list, list->head, NULL));
      sch->sched_pool = del_List(sch->sched_pool);
    };
/*
 * Delete the pool of input-streams.
 */
    if(sch->input_pool) {
      List *list = sch->input_pool;
      while(list->head)
	del_InputStream((InputStream* )del_ListNode(list, list->head, NULL));
      sch->input_pool = del_List(sch->input_pool);
    };
/*
 * Delete the list of control clients.
 */
    sch->clients = del_List(sch->clients);
/*
 * Delete the site description object.
 */
    sch->site = del_Site(sch->site);
/*
 * Delete the symbol table of signals.
 */
    sch->signals.table = del_HashTable(sch->signals.table);
    sch->signals.source_set = NULL;
    /*
     * Delete the auto-queuing pathnames and directories.
     */
    if(sch->autoqueue.dir)
      free(sch->autoqueue.dir);

    if(sch->autoqueue.penddir)
      free(sch->autoqueue.penddir);
    if(sch->autoqueue.donedir)
      free(sch->autoqueue.donedir);

    if(sch->autoqueue.pendname)
      free(sch->autoqueue.pendname);
    if(sch->autoqueue.donename)
      free(sch->autoqueue.donename);
/*
 * Delete the freelist of lists.
 */
    sch->list_mem = del_ListMemory(sch->list_mem, 1);
    free(sch);
  };
  return NULL;
}

/*.......................................................................
 * Return the scheduler resource object.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 * Output:
 *  return     Scheduler *   The scheduler resource object.
 */
Scheduler *cp_Scheduler(ControlProg *cp)
{
  return (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
}

/*.......................................................................
 * Attempt to send a message to a scheduler thread.
 *
 * Input:
 *  cp       ControlProg *  The state-object of the control program.
 *  msg SchedulerMessage *  The message to be sent. This must have been
 *                          filled by one of the pack_scheduler_<type>()
 *                          functions.
 *  timeout         long    The max number of milliseconds to wait for the
 *                          message to be sent, or -1 to wait indefinitely.
 * Output:
 *  return     PipeState    The status of the transaction:
 *                            PIPE_OK    - The message was read successfully.
 *                            PIPE_BUSY  - The send couldn't be accomplished
 *                                         without blocking (only returned
 *                                         when timeout=PIPE_NOWAIT).
 *                            PIPE_ERROR - An error occurred.
 */
PipeState send_SchedulerMessage(ControlProg *cp, SchedulerMessage *msg,
				long timeout)
{
  return cp_Scheduler(cp)->pipe->write(msg, sizeof(*msg), timeout);
}

/*.......................................................................
 * Send a shutdown message to the scheduler thread using non-blocking I/O.
 * Return 0 if the message was sent, 0 otherwise.
 *
 * Input:
 *  cp      ControlProg *  The control-program resource object.
 * Output:
 *  return          int    0 - Message sent ok.
 *                         1 - Unable to send message.
 */
CP_STOP_FN(stop_Scheduler)
{
  SchedulerMessage msg;   /* The message to be sent */
  return pack_scheduler_shutdown(&msg) ||
         send_SchedulerMessage(cp, &msg, PIPE_NOWAIT) != PIPE_OK;
}

/*.......................................................................
 * Prepare a shutdown message for subsequent transmission to the
 * scheduler thread.
 *
 * Input:
 *  msg   SchedulerMessage *  The message object being prepared for
 *                            subsequent transmission.
 * Output:
 *  return             int     0 - OK.
 *                             1 - Error.
 */
int pack_scheduler_shutdown(SchedulerMessage *msg)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_scheduler_shutdown: NULL argument.\n");
    return 1;
  };
  msg->type = SCH_SHUTDOWN;
  return 0;
}

/*.......................................................................
 * Prepare a command-string message for subsequent transmission to the
 * scheduler thread.
 *
 * Input:
 *  msg   SchedulerMessage *  The message object being prepared for
 *                            subsequent transmission.
 *  command           char *  The command string to be compiled and executed.
 *  client            Pipe *  The reply pipe of the control client that
 *                            sent the command.
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int pack_scheduler_command(SchedulerMessage *msg, char *command, 
			   sza::util::Pipe *client)
{
  
  // Check arguments.

  if(!msg || !command || !client) {
    lprintf(stderr, "pack_scheduler_command: NULL argument(s).\n");
    return 1;
  };

  msg->type = SCH_COMMAND;
  strncpy(msg->body.command.string, command, CC_CMD_MAX);
  msg->body.command.string[CC_CMD_MAX] = '\0';
  msg->body.command.client = client;

  return 0;
}

/*.......................................................................
 * Prepare a controller status message for subsequent transmission to the
 * scheduler thread.
 *
 * Input:
 *  msg  SchedulerMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  online            int    True if the controller is connected.
 * Output:
 * return             int    0 - OK.
 *                           1 - Error.
 */
int pack_scheduler_rtc_state(SchedulerMessage *msg, int online)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_scheduler_rtc_state: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_RTC_STATE;
  msg->body.rtc_online = online;
  return 0;
}

/*.......................................................................
 * Prepare a newly recieved controller message for subsequent forwarding
 * to the scheduler thread.
 *
 * Input:
 *  msg  SchedulerMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  id           NetMsgId    The type of message in msg (see rtcnetcoms.h).
 *  rtcmsg      RtcNetMsg *  The controller message to be forwarded.
 * Output:
 * return             int    0 - OK.
 *                           1 - Error.
 */
int pack_scheduler_rtcnetmsg(SchedulerMessage *msg, NetMsgId id,
			     RtcNetMsg *rtcmsg)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_scheduler_rtcnetmsg: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_RTCNETMSG;
  msg->body.rtcnetmsg.id = id;
  msg->body.rtcnetmsg.msg = *rtcmsg;
  return 0;
}

/*.......................................................................
 * Prepare a register-control-client message for subsequent transmission
 * to the scheduler thread.
 *
 * Input:
 *  msg   SchedulerMessage *  The message object to be packed for subsequent
 *                            transmission.
 *  client            Pipe *  The reply pipe of the new control client.
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int pack_scheduler_add_client(SchedulerMessage *msg, sza::util::Pipe *client)
{
/*
 * Check arguments.
 */
  if(!msg || !client) {
    lprintf(stderr, "pack_scheduler_add_client: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_ADD_CLIENT;
  msg->body.client.pipe = client;
  return 0;
}

/*.......................................................................
 * Prepare an unregister-control-client message for subsequent
 * transmission to the scheduler thread.
 *
 * Input:
 *  msg   SchedulerMessage *  The message object to be packed for subsequent
 *                         transmission.
 *  client         Pipe *  The reply pipe of the new control client.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int pack_scheduler_rem_client(SchedulerMessage *msg, sza::util::Pipe *client)
{
/*
 * Check arguments.
 */
  if(!msg || !client) {
    lprintf(stderr, "pack_scheduler_rem_client: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_REM_CLIENT;
  msg->body.client.pipe = client;
  return 0;
}

/*.......................................................................
 * Prepare a mark-completion status message for subsequent transmission
 * to the scheduler thread.
 *
 * Input:
 *  msg  SchedulerMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  seq          unsigned    The sequence number of the completed
 *                           transaction.
 * Output:
 * return             int    0 - OK.
 *                           1 - Error.
 */
int pack_scheduler_frame_done(SchedulerMessage *msg, unsigned seq)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_scheduler_frame_done: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_FRAME_DONE;
  msg->body.frame_done.seq = seq;
  return 0;
}

/*.......................................................................
 * Prepare a mark-completion status message for subsequent transmission
 * to the scheduler thread.
 *
 * Input:
 *  msg  SchedulerMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  seq          unsigned    The sequence number of the completed
 *                           transaction.
 * Output:
 * return             int    0 - OK.
 *                           1 - Error.
 */
int pack_scheduler_mark_done(SchedulerMessage *msg, unsigned seq)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_scheduler_mark_done: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_MARK_DONE;
  msg->body.mark_done.seq = seq;
  return 0;
}
/*.......................................................................
 * Prepare a tv_offset-completion status message for subsequent transmission
 * to the scheduler thread.
 *
 * Input:
 *  msg  SchedulerMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  seq          unsigned    The sequence number of the completed
 *                           transaction.
 * Output:
 * return             int    0 - OK.
 *                           1 - Error.
 */
int pack_scheduler_tv_offset_done(SchedulerMessage *msg, unsigned seq)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_scheduler_tv_offset_done: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_TV_OFFSET_DONE;
  msg->body.tv_offset_done.seq = seq;
  return 0;
}
/*.......................................................................
 * Prepare a setreg-completion status message for subsequent transmission
 * to the scheduler thread.
 *
 * Input:
 *  msg  SchedulerMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  seq          unsigned    The sequence number of the completed
 *                           transaction.
 * Output:
 * return             int    0 - OK.
 *                           1 - Error.
 */
int pack_scheduler_setreg_done(SchedulerMessage *msg, unsigned seq)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_scheduler_setreg_done: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_SETREG_DONE;
  msg->body.setreg_done.seq = seq;
  return 0;
}
/*.......................................................................
 * Prepare a grabber-completion status message for subsequent transmission
 * to the scheduler thread.
 *
 * Input:
 *  msg  SchedulerMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  seq          unsigned    The sequence number of the completed
 *                           transaction.
 * Output:
 * return             int    0 - OK.
 *                           1 - Error.
 */
int pack_scheduler_grab_done(SchedulerMessage *msg, unsigned seq)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_scheduler_mark_done: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_GRAB_DONE;
  msg->body.mark_done.seq = seq;
  return 0;
}
/*.......................................................................
 * This is the entry-point of the scheduler thread.
 *
 * Input:
 *  arg          void *  A pointer to the Scheduler state object pointer,
 *                       cast to (void *).
 * Output:
 *  return       void *  NULL.
 */
CP_THREAD_FN(scheduler_thread)
{
  Scheduler *sch = (Scheduler* )arg; /* The state-object of the
					current thread */
  SchedulerMessage msg; /* A message received from another threads */
  PipeState state=PIPE_OK;      /* The completion status of a pipe read */
  NetMsg netmsg;     /* A message from the real-time controller */
  Script *startup;      /* The startup script */
  unsigned antenna;

  // Enable logging of the scheduler's stdout and stderr streams.

  if(log_thread_stream(sch->cp, stdout) ||
     log_thread_stream(sch->cp, stderr)) {
    cp_report_exit(sch->cp);
    return NULL;
  };
  
  // Start the initialization script.

  startup = sch_compile_schedule(sch, "", (char*)cp_startupScript(sch->cp).c_str(), NULL);

  if(!startup || sch_queue_schedule(sch, startup)) {
    cp_report_exit(sch->cp);
    return NULL;
  };
  
  // Wait for commands from other threads.

  sza::util::FdSet fdSet;
  fdSet.registerReadFd(sch->pipe->readFd());

  // Convert the wakeup interval (ms) to nano-seconds and set the
  // timeout

  unsigned int sec  = sch->poll_time/1000;
  unsigned int nsec = (sch->poll_time - sec*1000) * 1000000;
  sza::util::TimeVal timeOut(sec, nsec);
  int nready=-1;

  while((nready=select(fdSet.size(), fdSet.readFdSet(), NULL, NULL, 
		       timeOut.timeVal())) >= 0) {

    // Now read the message off the queue

    if(nready > 0) {

      state = sch->pipe->read(&msg, sizeof(msg), PIPE_WAIT);

      if(state == PIPE_ERROR) {
	break;
      }

    // If we timed out, reset the timeout counter
 
    } else {
      timeOut.reset();
      state = PIPE_BUSY;
    }

    // Did we get a new message or is it time to run a new command?

    if(state == PIPE_OK) {
      
      // Interpret the message.

      switch(msg.type) {
      case SCH_SHUTDOWN:
	cp_report_exit(sch->cp);
	return NULL;
	break;
      case SCH_COMMAND:
	sch_execute_command(sch, msg.body.command.string,
			    msg.body.command.client);
	break;
      case SCH_RTC_STATE:
	sch->rtc_online = msg.body.rtc_online;
	
	// If the controller goes down, return the current script to
	// the schedule queue.

	sch_reschedule(sch);
	
	// When the controller comes back on line, arrange for the
	// initialization script to be executed first.

	if(sch->rtc_online && sch->initialize) {
	  sch_reference_schedule(sch->initialize);
	  sch_stage_schedule(sch, sch->initialize);
	};
	break;
      case SCH_ADD_CLIENT:
	sch_add_client(sch, msg.body.client.pipe);
	break;
      case SCH_REM_CLIENT:
	sch_rem_client(sch, msg.body.client.pipe);
	break;
      case SCH_RTCNETMSG:
	netmsg = msg.body.rtcnetmsg.msg.msg;
	antenna = msg.body.rtcnetmsg.msg.antenna;
	switch(msg.body.rtcnetmsg.id) {
	case NET_NOISE_DONE_MSG:
	  if(netmsg.noise_done.seq == sch->noise.seq)
	    sch->noise.done = true;
	  break;
	case NET_PMAC_DONE_MSG:
	  {
	    COUT("Got a pmac done message from antenna: " << antenna << " seq = " << netmsg.pmac_done.seq <<
		 " expected: " << sch->pmac.seq);
	    if(netmsg.pmac_done.seq == sch->pmac.seq) 
	      sch->pmac.done |= antenna;
	  }
	  break;
	case NET_CALTERT_DONE_MSG:
	  if(netmsg.calTertDone.seq == sch->caltert.seq) 
	    sch->caltert.done |= antenna;
	  break;
	case NET_CAN_DONE_MSG:
	  if(netmsg.canDone.seq == sch->can.seq) 
	    sch->can.done |= antenna;
	  break;
	case NET_IFMOD_DONE_MSG:
	  if(netmsg.IFModDone.seq == sch->ifMod.seq) 
	    sch->ifMod.done |= antenna;
	  break;
	case NET_SETREG_DONE_MSG:
	  if(netmsg.setreg_done.seq == sch->setreg.seq)
	    sch->setreg.done |= antenna;
	  break;
	case NET_SOURCE_SET_MSG:
	  if(sch->schedule && netmsg.source_set.seq == sch->pmac.seq)
	    signal_script(sch->schedule, sch->signals.source_set);
	  break;
	default:
	  lprintf(stderr, "Scheduler: Unexpected network message.\n");
	  break;
	};
	break;
      case SCH_MARK_DONE:
	if(msg.body.mark_done.seq == sch->mark.seq)
	  sch->mark.done = 1;
	break;
      case SCH_GRAB_DONE:
	if(msg.body.grab_done.seq == sch->grab.seq)
	  sch->grab.done = 1;
	break;
      case SCH_TV_OFFSET_DONE:
	if(msg.body.tv_offset_done.seq == sch->tv_offset.seq)
	  sch->tv_offset.done = 1;
	break;
      case SCH_FRAME_DONE:
	if(msg.body.frame_done.seq == sch->frame.seq) 
	  sch->frame.done = 1;
	break;
	
	// Change the autopolling interval.  If we are not currently
	// running a schedule, and autoqueueing is turned on, set the
	// polling interval.

      case SCH_AUTO_POLL:
	sch->autoqueue.poll_time = msg.body.auto_poll.poll;
	if(!sch->schedule && sch->autoqueue.on)
	  sch->poll_time = sch->autoqueue.poll_time;
	break;
	
	// Else we are changing the state.

      case SCH_AUTO_STATE:
	sch->autoqueue.on = msg.body.auto_state.on;
	/* 
	 * If we are not running a schedule set the poll time
         */
	if(!sch->schedule) {
	  if(sch->autoqueue.on)
	    sch->poll_time = sch->autoqueue.poll_time;
	  else
	    sch->poll_time = -1;	    
	};
	break;
      case SCH_AUTO_DIR:
	(void) chdir_auto(sch, msg.body.auto_dir.dir);
	break;
      default:
	lprintf(stderr, "scheduler_thread: Unknown message-type received.\n");
	break;
      };
    } else {
      
      // If a schedule is running, and isn't suspended, execute its
      // next command.

      if(sch->schedule) {
	if(!sch->suspend) {
	  ScriptState state = step_script(sch->schedule, sch->log.output);

	  switch(state) {
	  case SCRIPT_ACTIVE:  /* Script still in progress */
	  case SCRIPT_EXITING: /* An exit handler is being run */
	    sch->poll_time = script_is_polling(sch->schedule) ?
	      SCH_POLL_INTERVAL : SCH_RUN_INTERVAL;
	    break;
	  default:             /* Script complete */
	    
	    // Has the initialization script just completed?

	    if(sch->initializing) {
	      sch->initializing = 0;
	      startup = sch_discard_schedule(sch, startup);
	      cp_initialized(sch->cp);
	    };
	    
	    // Start the next schedule, if any.

	    sch_next_schedule(sch);
	    break;
	  };
	};
	
	// Else check for auto-queued schedules provided the
	// controller is online.

      } else {
	if(sch->autoqueue.on/*&& (sch->rtc_online || sch->initializing)*/) {
	  Script *nextauto=NULL;
	  
	  // Try to find the next file to be auto-queued.

	  if(!sch_next_auto_file(sch) && sch->autoqueue.pending) {
	    nextauto=sch_compile_schedule(sch,"",sch->autoqueue.pendname,NULL);
	    if(nextauto!=NULL)  /* if it compiles */
	      sch_stage_schedule(sch, nextauto);
	    else {
	      output_printf(sch->log.output,
			    "Bad auto script, moving to done directory\n");
	      sch_remove_pending_file(sch);    
	    };
	  };
	};
      };
    };
  };
  if(state == PIPE_ERROR)
    lprintf(stderr, "Aborting scheduler thread due to pipe read-error.\n");
  else
    lprintf(stdout, "Scheduler thread exiting normally.\n");
  
  // Report our exit to the control thread.

  cp_report_exit(sch->cp);
  return NULL;
}

/*.......................................................................
 * Execute an interactive command received from a control client.
 *
 * Input:
 *  sch     Scheduler *   The scheduler state-object.
 *  string       char *   The single-line command to be executed.
 *  client       Pipe *   The reply pipe of the originating control client.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int sch_execute_command(Scheduler *sch, char *string, 
			       sza::util::Pipe *client)
{
  int waserr = 0;   /* True after an error */
  
  // Temporarily redirect the output of this thread to the client
  // reply pipe.

  waserr = divert_lprintf(stdout, client_output_dispatcher, client,NULL,NULL) ||
           divert_lprintf(stderr, client_output_dispatcher, client,NULL,NULL);
  
  // Execute the command.

  waserr = waserr || run_interactive_command(sch->interactive,
					     sch->log.output, string);
  
  // Discard the contents of the temporary script.

  discard_script(sch->interactive);
  
  // Return to sending all output to the logger thread.

  if(log_thread_stream(sch->cp, stdout) ||
     log_thread_stream(sch->cp, stderr))
    return 1;

  return waserr;
}

/*.......................................................................
 * This is an lprintf() dispatch function used to send standard output to
 * a given control-client reply pipe.
 */
static LOG_DISPATCHER(client_output_dispatcher)
{
  static sza::util::LogMsgHandler handler;

  unsigned newSeq = handler.nextSeq();

  handler.append(newSeq, message, id);

  bool isLast=false;

  do {

    // Get the next substring of this message and write it to
    // clients and the logfile
    
    std::string message = 
      handler.getNextMessageSubstr(newSeq, CC_MSG_MAX, isLast);
   
    if(writeClientMsg((char*)message.c_str(), id, context, newSeq, isLast))
      return 1;

  } while(!isLast);

  return 0;
}

static LOG_DISPATCHER(writeClientMsg)
{
  sza::util::Pipe *client = (sza::util::Pipe* )context; // The client
							// reply pipe

  CcPipeMsg pmsg;            // The message to queue to the pipe 
  
  // Assemble the message.

  pmsg.id = CC_REPLY_MSG;
  pmsg.msg.reply.error = (id != LOG_STDOUT);
  pmsg.msg.reply.end = isEnd;
  pmsg.msg.reply.seq = seq;
  strncpy(pmsg.msg.reply.text, message, CC_MSG_MAX);
  pmsg.msg.reply.text[CC_MSG_MAX] = '\0';

  return queue_cc_message(client, &pmsg);
}

/*.......................................................................
 * Abort the current schedule, if any, and if another schedule is waiting
 * to be run, prepare it for execution.
 *
 * Input:
 *  sch     Scheduler *   The scheduler state-object.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int sch_next_schedule(Scheduler *sch)
{
  
  // Report script termination.

  if(sch->schedule) {

    // Get the lst

    Date utc;
    if(current_date(&utc))
      return 1;

    double lstRad = date_to_lst(&utc, sch_Site(sch), 0.0, 0.0);
    sza::util::HourAngle lst;
    lst.setRadians(lstRad);
    std::ostringstream os;

    os << "(LST: " << lst << ") Exiting schedule: ";

    output_printf(sch->log.output, os.str().c_str());

    // Now print the argument list too

    output_script_spec(sch->log.output, sch->schedule);
    output_printf(sch->log.output, "\n");

    // If this script was the initialization script, send a message to
    // the rtc that the initialization script is done.

    if(sch->schedule == sch->initialize)
      if(sch_init_ended_send(sch))
	return 1;
  };
  
  // Remove the script that is currently executing.

  sch->schedule = sch_discard_schedule(sch, sch->schedule);
  
  // If the last file was an autoqueue file, and it was not aborted
  // move it from the pending to done directory now.

  if(sch->autoqueue.pending) {
    output_printf(sch->log.output,
		  "Moving completed autoqueue schedule to done directory\n");
    sch_remove_pending_file(sch);
  }
  
  // If there is another schedule in the queue, and the controller is
  // connected, remove the schedule from the queue and stage it to be
  // run.

  if(sch->sched_list->head /*&& (sch->rtc_online || sch->initializing)*/) {
    if(prefix_status_report(sch, "RM ") ||
       output_ulong(sch->stat.output, (OutputBase)10, "", 0, 0, 0) ||
       send_status_report(sch, NULL))
      return 1;
    sch_stage_schedule(sch,
		       (Script* )del_ListNode(sch->sched_list, 
					      sch->sched_list->head, NULL));
    
    // Clear legacy signals so that the new schedule isn't affected by
    // signals that were intended for the previous schedule.

    sch_reset_signals(sch);
  } else {
    sch_stage_schedule(sch, NULL);
  };
  return 0;
}

/*.......................................................................
 * Return the current schedule to the head of the schedule queue and
 * prepare it to be restarted. This is designed for the situation where
 * the controller goes down while executing a schedule.
 *
 * Input:
 *  sch     Scheduler *  The resource object of the scheduler.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
static int sch_reschedule(Scheduler *sch)
{
  if(sch->schedule) {
    
    // Rewind the script to its first statement.

    if(rewind_script(sch->schedule))
      return 1;
    
    // If the controller is offline, move the schedule back to the
    // head of the schedule queue (unless it is the initialization
    // script). It will then be restarted when the controller reboots.

    if(!sch->rtc_online) {
      if(sch->schedule && sch->schedule != sch->initialize &&
	 prepend_ListNode(sch->sched_list, sch->schedule) == NULL) {
	sch_stage_schedule(sch, sch_discard_schedule(sch, sch->schedule));
	return 1;
      };
      sch_stage_schedule(sch, NULL);
    };
  };
  return 0;
}

/*.......................................................................
 * Append a compiled schedule to the schedule queue.
 *
 * Input:
 *  sch          Scheduler *  The resource object of the scheduler thread.
 *  sc              Script *  The scheduling script to be queued.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Failed.
 */
int sch_queue_schedule(Scheduler *sch, Script *sc)
{
/*
 * Check arguments.
 */
  if(!sch || !sc) {
    lprintf(stderr, "sch_queue_schedule: NULL argument(s).\n");
    return 1;
  };
/*
 * Attempt to add the script to the schedule queue.
 */
  if(!append_ListNode(sch->sched_list, sc))
    return 1;
/*
 * Report the change.
 */
  if(prefix_status_report(sch, "ADD ") ||
     output_script_spec(sch->stat.output, sc) ||
     send_status_report(sch, NULL))
    return 1;
/*
 * Increment the reference count of the script so that it doesn't
 * get deleted before it is removed from the schedule queue.
 */
  sch_reference_schedule(sc);
/*
 * If no schedule is currently running, prepare the new one to be run.
 */
  if(!sch->schedule && sch_next_schedule(sch))
    return 1;
  return 0;
}

/*.......................................................................
 * Return a schedule to the script pool unless it is currently in use.
 *
 * Input:
 *  sch    Scheduler *  The scheduler resource object.
 *  sc        Script *  The script to be returned to the pool.
 * Output:
 *  return    Script *  Always NULL.
 */
Script *sch_discard_schedule(Scheduler *sch, Script *sc)
{
  if(sc) {
    ScheduleData *data = (ScheduleData* )sc->data;
/*
 * If the schedule isn't still in use, attempt to return it to the
 * schedule pool.
 */
    if(--data->ref_count == 0) {
      discard_script(sc);
      if(prepend_ListNode(sch->sched_pool, sc) == NULL)
	sc = del_SzaScript(sc);
/*
 * If it still is in use, rewind it for its next use.
 */
    } else {
      rewind_script(sc);
    };
  };
  return NULL;
}

/*.......................................................................
 * Increment the reference count of a schedule.
 *
 * Input:
 *  sc      Script *  The script to be referenced.
 * Output:
 *  return  Script *  The same as 'sc' but after incrementing the reference
 *                    count.
 */
Script *sch_reference_schedule(Script *sc)
{
  ScheduleData *sd;
  if(!sc) {
    lprintf(stderr, "sch_reference_schedule: NULL argument.\n");
    return NULL;
  };
  sd = (ScheduleData* )sc->data;
  sd->ref_count++;
  return sc;
}

/*.......................................................................
 * Return a newly compiled schedule.
 *
 * Input:
 *  sch          Scheduler *  The resource object of the scheduler thread.
 *  dir               char *  The directory that contains the filename,
 *                            or "" if filename is a full pathname.
 *  filename          char *  The file name of the schedule.
 *  arguments  InputStream *  If not NULL, the stream should contain
 *                            the argument list of the schedule,
 *                            enclosed in () parentheses. If the compilation
 *                            is successful, the stream pointer will
 *                            be left after the close ) parenthesis.
 * Output:
 *  return       SzaScript *  The compiled script, or NULL on error.
 */
Script *sch_compile_schedule(Scheduler *sch, char *dir, char *filename,
			     InputStream *arguments)
{
  Script *sc=NULL;          /* The container of the new script */
  InputStream *input=NULL;  /* The stream to use to parse the script */
  char *pathname;           /* The expanded path name */
  int waserr = 0;           /* True after an error */
/*
 * Expand the pathname so that we can pass the absolute pathname
 * as the name of the script.
 */
  pathname = new_pathname("", filename);
  if(!pathname)
    return NULL;
/*
 * Get a new script container.
 */
  if(sch->sched_pool->head)
    sc = (Script* )del_ListNode(sch->sched_pool, sch->sched_pool->head, NULL);
  else
    sc = new_SzaScript(sch->cp, 1, sch->signals.table);
/*
 * Get a new input-stream for parsing the script.
 */
  if(sch->input_pool->head)
    input = (InputStream* )del_ListNode(sch->input_pool, sch->input_pool->head, 
					NULL);
  else
    input = new_InputStream();
/*
 * Attempt to compile the script.
 */
  waserr = !sc || !input || open_FileInputStream(input, dir, pathname) ||
           compile_script(sc, pathname, arguments, input, 0, false);
/*
 * We no longer need the filename.
 */
  free(pathname);
/*
 * Close and return the input stream to the pool.
 */
  if(input) {
    close_InputStream(input);
    if(prepend_ListNode(sch->input_pool, input) == NULL)
      input = del_InputStream(input);
  };
/*
 * If the compilation failed, try to return the redundant script to
 * the schedule pool.
 */
  if(waserr) {
    if(prepend_ListNode(sch->sched_pool, sc) == NULL)
      sc = del_SzaScript(sc);
    return NULL;
  };
/*
 * Reset the reference count of the schedule.
 */
  ((ScheduleData *)sc->data)->ref_count = 1;
  return sc;
}

/*.......................................................................
 * Stage a given unqueued schedule to be run next.
 *
 * Input:
 *  sch     Scheduler *  The resource object of the scheduler thread.
 *  sc         Script *  The script to be run, or NULL if no schedule
 *                       is to be run.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int sch_stage_schedule(Scheduler *sch, Script *sc)
{
  if(sc) {

    rewind_script(sc);
    sch->schedule = sc;
    sch->poll_time = SCH_RUN_INTERVAL;

    if(prefix_status_report(sch, "RUN ") ||
       output_script_spec(sch->stat.output, sc) ||
       send_status_report(sch, NULL))
      return 1;

    // Get the lst

    Date utc;
    if(current_date(&utc))
      return 1;

    double lstRad = date_to_lst(&utc, sch_Site(sch), 0.0, 0.0);
    sza::util::HourAngle lst;
    lst.setRadians(lstRad);
    std::ostringstream os;

#if 1
    os << "(LST: " << lst << ") Starting schedule: ";
#else
    os << "This is a test of sending a really long message via the new log \n"
       << "mechanism to make sure it doesn't get truncated or otherwise mangled\n"
       << "in transit.  Hopefully it'll work as I expect and I can incorporate\n"
       << "it into the control system asap.";
#endif

    output_printf(sch->log.output, os.str().c_str());
    
    // Now print the argument list too
    
    output_script_spec(sch->log.output, sc);
    output_printf(sch->log.output, "\n");

  } else {
    if(prefix_status_report(sch, "IDLE") || send_status_report(sch, NULL))
      return 1;
    sch->schedule = NULL;
    
    // Select next wait time

    if(sch->autoqueue.on)
      sch->poll_time = sch->autoqueue.poll_time;
    else
      sch->poll_time = -1;
  };
  
  // Cancel any suspension of execution that was associated with the
  // previous schedule.

  sch_resume_schedule(sch);

  return 0;
}

/*.......................................................................
 * Abort execution of the current script.
 *
 * Input:
 *  sch     Scheduler *  The resource object of the scheduler thread.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
int sch_abort_schedule(Scheduler *sch)
{
/*
 * Is there a schedule to be aborted?
 */
  if(sch->schedule) {
/*
 * If this was an autoqueue schdeule and autoqueue is on turn it off.
 */
    if(sch->autoqueue.on && sch->autoqueue.pending)
      {
	sch->autoqueue.on = 0;
	output_printf(sch->log.output,"Forcing autoqueue off\n");
      }
/* 
 * Make sure the pending flag is reset so if this was an autoqueue
 * schedule it won't get moved to done.
 */
    sch->autoqueue.pending = 0;
/*
 * Ask the script to exit, and warn the user if this starts
 * an exit handler running.
 */
    if(exit_script(sch->schedule, "abort") == SCRIPT_EXITING) {
      output_printf(sch->log.output,
      "Warning: Schedule termination deferred by execution of exit handler.\n");
      return 0;
    };
/*
 * The script has finished, so discard it and start the next script
 * in the queue.
 */
    return sch_next_schedule(sch);
  };
  return 0;
}

/*.......................................................................
 * Temporarily suspend execution of the current schedule.
 *
 * Input:
 *  sch     Scheduler *  The resource object of the scheduler thread.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
int sch_suspend_schedule(Scheduler *sch)
{
  if(sch->schedule) {
    sch->suspend = 1;
    if(prefix_status_report(sch, "SUSPEND") || send_status_report(sch, NULL))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Resume the suspended execution of the currently running schedule.
 *
 * Input:
 *  sch     Scheduler *  The resource object of the scheduler thread.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
int sch_resume_schedule(Scheduler *sch)
{
  sch->suspend = 0;
  if(prefix_status_report(sch, "RESUME") || send_status_report(sch, NULL))
    return 1;
  return 0;
}

/*.......................................................................
 * Return a pointer to the site configuration object.
 *
 * Input:
 *  sch     Scheduler *  The resource object of the scheduler thread.
 * Output:
 *  return       Site *  The site-specification object - readonly,
 *                       or NULL on error.
 */
Site *sch_Site(Scheduler *sch)
{
  return sch ? sch->site : NULL;
}

/*.......................................................................
 * Send a message to the real-time control system that the
 * initialization script has ended
 *
 * Input:
 *  sch      Scheduler *  The resource object of the scheduler thread.
 *
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int sch_init_ended_send(Scheduler *sch)
{
  RtcNetCmd rtc;            /* The network object to be sent */
  
  // Check arguments.

  if(!sch) {
    lprintf(stderr, "sch_init_ended_send: NULL argument.\n");
    return 1;
  };
  
  // Compose the message that is to be sent to the real-time
  // controller.

  rtc.cmd.init.start = 0;
  
  // Send the command to the real-time controller.

  if(queue_rtc_command(sch->cp, &rtc, NET_INIT_CMD))
    return 1;
  return 0;
}


/*.......................................................................
 * Send a message to the real-time control system to request new
 * total-power detector outputs for a given set of IF channels.
 *
 * Input:
 *  sch      Scheduler *  The resource object of the scheduler thread.
 *  bands     unsigned    The bit-set of bands who's total-power outputs
 *                        are to be adjusted. The 10 least significant bits
 *                        represent the 10 bands.
 *  antennas  unsigned    The bit-set of antennas who's total-power outputs
 *                        are to be adjusted. The 13 least significant bits
 *                        represent the 13 antennas.
 *  power         long    The target total-power detector output voltage,
 *                        over the range -5..5 volts.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int sch_chzr_send_power(Scheduler *sch, unsigned bands, unsigned antennas,
			double power)
{
  RtcNetCmd rtc;            /* The network object to be sent */
/*
 * Check arguments.
 */
  if(!sch) {
    lprintf(stderr, "sch_chzr_send_power: NULL argument.\n");
    return 1;
  };
/*
 * Get the new transaction number and mark the transaction as incomplete.
 */
  sch->chzr.seq++;
  sch->chzr.done = 0;
/*
 * Compose the message that is to be sent to the real-time controller.
 */
  rtc.cmd.chzr_power.seq = sch->chzr.seq;
  rtc.cmd.chzr_power.bands = bands;
  rtc.cmd.chzr_power.receivers = antennas;
/*
 * Convert from -5.0..5.0 to 0..256 * 1.0e+6.
 */
  /*  rtc.cmd.chzr_power.power = floor((power + 5.0) * 25.6e6 + 0.5); */
  rtc.cmd.chzr_power.power = (float) power;
/*
 * Send the command to the real-time controller.
 */
  if(queue_rtc_command(sch->cp, &rtc, NET_CHZR_POWER_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * Send a message to the real-time control system to ask it to grab a new
 * image from the frame grabber.
 *
 * Input:
 *  sch      Scheduler *  The resource object of the scheduler thread.
 *
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int sch_grab_send(Scheduler *sch)
{
/*
 * Check arguments.
 */
  if(!sch) {
    lprintf(stderr, "sch_grab_send: NULL argument.\n");
    return 1;
  };
/*
 * Get the new transaction number and mark the transaction as incomplete.
 */
  sch->grab.seq++;
  sch->grab.done = 0;
  /*
   * Update the sequence number in the grabber thread.
   */
  {
    GrabberMessage msg;
    msg.body.seq = sch->grab.seq;
    msg.type = GRAB_SEQ;
    if(send_GrabberMessage(sch->cp, &msg, PIPE_NOWAIT) != PIPE_OK)
      return 1;
  }
  /*
   * And send the command to the real-time controller.
   */
  if(queue_rtc_command(sch->cp, NULL, NET_GRABBER_CMD))
    return 1;
  return 0;
}
/*.......................................................................
 * Send a message to the real-time control system to ask it to start
 * measuring the zero offsets of the total-power detectors of a given set
 * of IF channels.
 *
 * Input:
 *  sch      Scheduler *  The resource object of the scheduler thread.
 *  bands     unsigned    The bit-set of bands who's total-power detectors
 *                        are to be zeroed. The 10 least significant bits
 *                        represent the 10 bands.
 *  antennas  unsigned    The bit-set of antennas who's detectors
 *                        are to be zeroed. The 13 least significant bits
 *                        represent the 13 antennas.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int sch_chzr_send_zero(Scheduler *sch, unsigned bands, unsigned antennas)
{
  RtcNetCmd rtc;            /* The network object to be sent */
/*
 * Check arguments.
 */
  if(!sch) {
    lprintf(stderr, "sch_chzr_send_zero: NULL argument.\n");
    return 1;
  };
/*
 * Get the new transaction number and mark the transaction as incomplete.
 */
  sch->chzr.seq++;
  sch->chzr.done = 0;
/*
 * Compose the message that is to be sent to the real-time controller.
 */
  rtc.cmd.chzr_zero.seq = sch->chzr.seq;
  rtc.cmd.chzr_zero.bands = bands;
  rtc.cmd.chzr_zero.receivers = antennas;
/*
 * Send the command to the real-time controller.
 */
  if(queue_rtc_command(sch->cp, &rtc, NET_CHZR_ZERO_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * Return true if the last long-duration channelizer command completed.
 *
 * Input:
 *  sch      Scheduler *  The resource object of the scheduler thread.
 * Output:
 *  return         int    True if the transaction completed.
 */
unsigned int sch_chzr_done(Scheduler *sch)
{
  if(!sch) {
    lprintf(stderr, "sch_chzr_done: NULL argument.\n");
    return 0;
  };
  return sch->chzr.done;
}
/*.......................................................................
 * Return true if the last long-duration frame grabber command completed.
 *
 * Input:
 *  sch      Scheduler *  The resource object of the scheduler thread.
 * Output:
 *  return         int    True if the transaction completed.
 */
unsigned int sch_grab_done(Scheduler *sch)
{
  if(!sch) {
    lprintf(stderr, "sch_grab_done: NULL argument.\n");
    return 0;
  };
  return sch->grab.done;
}

/*.......................................................................
 * Attempt to install a replacement rtc initialization script.
 *
 * Input:
 *  sch    Scheduler *  The resource object of the scheduler thread.
 *  sc        Script *  The script to be installed.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Failed.
 */
int sch_change_init_script(Scheduler *sch, Script *sc)
{
/*
 * If we are replacing an existing script, return the old one to
 * the schedule pool.
 */
  sch->initialize = sch_discard_schedule(sch, sch->initialize);
/*
 * Install the new script.
 */
  sch->initialize = sc;
  sch_reference_schedule(sc);
  return 0;
}

/*.......................................................................
 * Set the default elevation of the local horizon.
 *
 * Input:
 *  sch   Scheduler *   The scheduler resource object.
 *  angle    double     The new horizon elevation (radians).
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int sch_set_horizon(Scheduler *sch, double angle)
{
  if(!sch) {
    lprintf(stderr, "sch_set_horizon: NULL argument.\n");
    return 1;
  };
  sch->horizon = angle;
  return 0;
}

/*.......................................................................
 * Get the default elevation of the local horizon.
 *
 * Input:
 *  sch   Scheduler *   The scheduler resource object.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
double sch_get_horizon(Scheduler *sch)
{
  return sch ? sch->horizon : 0.0;
}

/*.......................................................................
 * Increment the CALTERT transaction counter, record this transaction as
 * incomplete and return it to be sent with the caller's transaction
 * request.
 *
 * Input:
 *  sch     Scheduler *   The resource container of the scheduler thread.
 * Output:
 *  return   unsigned     The requested sequence number.
 */
unsigned sch_next_caltert_seq(Script* sc, Scheduler *sch)
{
  if(sc->interactive_)
    return 0;
  
  // Mark the new transaction as incomplete.

  sch->caltert.done = 0x0;
  
  // Return the next transaction number.

  return ++sch->caltert.seq;
}

/*.......................................................................
 * Return true if the last caltert transaction completed.
 *
 * Input:
 *  sch   Scheduler *  The resource container of the scheduler thread.
 * Output:
 *  return      int    0 - Not complete (or error).
 *                     1 - Complete.
 */
unsigned int sch_caltert_done(Scheduler *sch)
{
  return sch->caltert.done;
}
/*.......................................................................
 * Increment the FRAME transaction counter, record this transaction as
 * incomplete and return it to be sent with the caller's transaction
 * request.
 *
 * Input:
 *  sch     Scheduler *   The resource container of the scheduler thread.
 * Output:
 *  return   unsigned     The requested sequence number.
 */
unsigned sch_next_frame_seq(Script* sc, Scheduler *sch)
{
  if(sc->interactive_)
    return 0;
  
  // Mark the new transaction as incomplete.

  sch->frame.done = 0x0;
  
  // Return the next transaction number.

  return ++sch->frame.seq;
}

/*.......................................................................
 * Return true if the last frame transaction completed.
 *
 * Input:
 *  sch   Scheduler *  The resource container of the scheduler thread.
 * Output:
 *  return      int    0 - Not complete (or error).
 *                     1 - Complete.
 */
unsigned int sch_frame_done(Scheduler *sch)
{
  return sch->frame.done;
}

/*.......................................................................
 * Increment the SELECT_RX transaction counter, record this transaction as
 * incomplete and return it to be sent with the caller's transaction
 * request.
 *
 * Input:
 *  sch     Scheduler *   The resource container of the scheduler thread.
 * Output:
 *  return   unsigned     The requested sequence number.
 */
unsigned sch_next_can_seq(Script* sc, Scheduler *sch)
{
  if(sc->interactive_)
    return 0;
  
  // Mark the new transaction as incomplete.

  sch->can.done = 0x0;
  
  // Return the next transaction number.

  return ++sch->can.seq;
}

/*.......................................................................
 * Return true if the last can transaction completed.
 *
 * Input:
 *  sch   Scheduler *  The resource container of the scheduler thread.
 * Output:
 *  return      int    0 - Not complete (or error).
 *                     1 - Complete.
 */
unsigned int sch_can_done(Scheduler *sch)
{
  return sch->can.done;
}

/*.......................................................................
 * Increment the IFMOD transaction counter, record this transaction as
 * incomplete and return it to be sent with the caller's transaction
 * request.
 *
 * Input:
 *  sch     Scheduler *   The resource container of the scheduler thread.
 * Output:
 *  return   unsigned     The requested sequence number.
 */
unsigned sch_next_IFMod_seq(Script* sc, Scheduler *sch)
{
  if(sc->interactive_)
    return 0;
  
  // Mark the new transaction as incomplete.

  sch->ifMod.done = 0x0;
  
  // Return the next transaction number.

  return ++sch->ifMod.seq;
}

/*.......................................................................
 * Return true if the last IFMod transaction completed.
 *
 * Input:
 *  sch   Scheduler *  The resource container of the scheduler thread.
 * Output:
 *  return      int    0 - Not complete (or error).
 *                     1 - Complete.
 */
unsigned int sch_IFMod_done(Scheduler *sch)
{
  return sch->ifMod.done;
}

/*.......................................................................
 * Increment the PMAC transaction counter, record this transaction as
 * incomplete and return it to be sent with the caller's transaction
 * request.
 *
 * Input:
 *  sch     Scheduler *   The resource container of the scheduler thread.
 * Output:
 *  return   unsigned     The requested sequence number.
 */
unsigned sch_next_pmac_seq(Script* sc, Scheduler *sch, unsigned antennas)
{
  if(sc->interactive_)
    return 0;
  
  // Mark the new transaction as incomplete.

  sch->pmac.done     = 0x0;
  sch->pmac.antennas = antennas;
  
  // Cancel the source-set signal. The tracker will send us a new one
  // if the new transaction leaves the current source below the
  // horizon.

  sch->signals.source_set->code = 0x0;
  
  // Return the next transaction number.

  return ++sch->pmac.seq;
}

/*.......................................................................
 * Return true if the last pmac transaction completed.
 *
 * Input:
 *  sch   Scheduler *  The resource container of the scheduler thread.
 * Output:
 *  return      int    0 - Not complete (or error).
 *                     1 - Complete.
 */
unsigned int sch_pmac_done(Scheduler *sch)
{
  // Only check completion status for the intersection of the antennas
  // to which the command was issued, and the default antennas
  // currently in effect.
  //
  // This is because if an antenna has been removed from or added to
  // the default set since the command was issued, we may never get a
  // completion message from that antenna

  unsigned antennas = sch->pmac.antennas & cp_AntSet(sch->cp)->getId();

#if 1
  static unsigned counter = 0;
  if(++counter % 100 == 0)
    COUT("pmac antennas = " << sch->pmac.antennas << " antennas = " << antennas << " done = " << sch->pmac.done);
#endif


  return (antennas & ~sch->pmac.done)==0x0;
}

/*.......................................................................
 * Increment the PMAC transaction counter, record this transaction as
 * incomplete and return it to be sent with the caller's transaction
 * request.
 *
 * Input:
 *  sch     Scheduler *   The resource container of the scheduler thread.
 * Output:
 *  return   unsigned     The requested sequence number.
 */
unsigned sch_next_noise_seq(Script* sc, Scheduler *sch)
{
  if(sc->interactive_)
    return 0;

  // Mark the new transaction as incomplete.

  sch->noise.done = false;
  
  // Return the next transaction number.

  return ++sch->noise.seq;
}

/*.......................................................................
 * Return true if the last noise source command completed.
 *
 * Input:
 *  sch      Scheduler *  The resource object of the scheduler thread.
 * Output:
 *  return         int    True if the transaction completed.
 */
unsigned sch_noise_done(Scheduler *sch)
{
  return sch->noise.done;
}


/*.......................................................................
 * Increment the mark-command transaction counter, record this transaction as
 * incomplete and return it to be sent with the caller's transaction
 * request.
 *
 * Input:
 *  sch     Scheduler *   The resource container of the scheduler thread.
 * Output:
 *  return   unsigned     The requested sequence number.
 */
unsigned sch_next_mark_seq(Script* sc, Scheduler *sch)
{
  if(sc->interactive_)
    return 0;
  
  // Mark the new transaction as incomplete.

  sch->mark.done = 0;
  
  // Return the next transaction number.

  return ++sch->mark.seq;
}
/*.......................................................................
 * Increment the offset-command transaction counter, record this transaction as
 * incomplete and return it to be sent with the caller's transaction
 * request.
 *
 * Input:
 *  sch     Scheduler *   The resource container of the scheduler thread.
 * Output:
 *  return   unsigned     The requested sequence number.
 */
unsigned sch_next_tv_offset_seq(Script* sc, Scheduler *sch)
{
  if(sc->interactive_)
    return 0;
  
  // Mark the new transaction as incomplete.

  sch->tv_offset.done = 0;
  
  // Return the next transaction number.

  return ++sch->tv_offset.seq;
}
/*.......................................................................
 * Increment the setreg-command transaction counter, record this transaction as
 * incomplete and return it to be sent with the caller's transaction
 * request.
 *
 * Input:
 *  sch     Scheduler *   The resource container of the scheduler thread.
 * Output:
 *  return   unsigned     The requested sequence number.
 */
unsigned sch_next_setreg_seq(Script* sc, Scheduler *sch)
{
  if(sc->interactive_)
    return 0;

/*
 * Mark the new transaction as incomplete.
 */
  sch->setreg.done = 0;
/*
 * Return the next transaction number.
 */
  return ++sch->setreg.seq;
}
/*.......................................................................
 * Increment the grab-command transaction counter, record this transaction as
 * incomplete and return it to be sent with the caller's transaction
 * request.
 *
 * Input:
 *  sch     Scheduler *   The resource container of the scheduler thread.
 * Output:
 *  return   unsigned     The requested sequence number.
 */
unsigned sch_next_grab_seq(Script* sc, Scheduler *sch)
{
  if(sc->interactive_)
    return 0;

/*
 * Mark the new transaction as incomplete.
 */
  sch->grab.done = 0;
/*
 * Return the next transaction number.
 */
  return ++sch->grab.seq;
}

/*.......................................................................
 * Return true if the last mark transaction completed.
 *
 * Input:
 *  sch   Scheduler *  The resource container of the scheduler thread.
 * Output:
 *  return      int    0 - Not complete (or error).
 *                     1 - Complete.
 */
unsigned int sch_mark_done(Scheduler *sch)
{
  return sch->mark.done;
}
/*.......................................................................
 * Return true if the last setreg transaction completed.
 *
 * Input:
 *  sch   Scheduler *  The resource container of the scheduler thread.
 * Output:
 *  return      int    0 - Not complete (or error).
 *                     1 - Complete.
 */
unsigned int sch_setreg_done(Scheduler *sch)
{
  return sch->setreg.done;
}

/*.......................................................................
 * Return true if the last tv_offset transaction completed.
 *
 * Input:
 *  sch   Scheduler *  The resource container of the scheduler thread.
 * Output:
 *  return      int    0 - Not complete (or error).
 *                     1 - Complete.
 */
unsigned int sch_tv_offset_done(Scheduler *sch)
{
  return sch->tv_offset.done;
}

/*.......................................................................
 * This is the write_fn() callback of the sch->log.output stream.
 */
static OUTPUT_WRITE_FN(sch_log_output)
{
  int nnew;      /* The number of characters to write next */
  int ndone=0;   /* The number of characters written so far */
  static std::ostringstream os;
  
  // Get the resource object of the scheduler.

  Scheduler *sch = (Scheduler* )stream->data;
  
  // Get the log buffer container.

  ScriptLog *log = &sch->log;
  
  // Get the length of the string to be output.

  int nc = strlen(text);
  
  // If there are newline characters in text[] then we will need to
  // accumulate and dispatch the intervening string segments one at a
  // time. Loop until all characters in buffer have been consumed.

  while(ndone < nc) {
    int eos = 0;      /* The index of the end of the next string segment */
    
    // Find the index of the end of the next string segment to be
    // output.  If the remaining string contains a newline character,
    // end the string there.

    for(eos=ndone; eos<nc && text[eos] != '\n'; eos++)
      ;
    
    // How many characters should we attempt to copy?

    nnew = eos - ndone;
    
    // Add up to nnew characters to the buffer unless it is already
    // marked as full.

    if(!log->trunc) {
      int ncopy;      /* The number of characters to copy */
      ncopy = nnew;
      
      // Copy the new characters into the message buffer.

      for(int i=0; i < ncopy; i++) {
	os << *(text + ndone + i);
      }

      log->nbuf += ncopy;
    };
    
    // Keep a record of how many characters have been consumed from
    // text[].

    ndone += nnew;
    
    // If a new line was seen, send the contents of the buffer to the
    // logger thread.

    if(eos < nc) {
      
      // Place the message on the pipe queue of the logger thread.

      if(sendLoggerMessage(sch->cp, (char*)os.str().c_str(), false, log->output->interactive))
	return 1;
      
      // Reset the buffer.

      os.str("");

      log->nbuf = 0;
      log->trunc = 0;
      
      // Skip the newline.

      ndone++;
    };
  };
  return 0;
}

/*.......................................................................
 * This is the write_fn() callback of the sch->log.output stream.
 */
static OUTPUT_WRITE_FN(sch_log_output_old)
{
  int nnew;      /* The number of characters to write next */
  int ndone=0;   /* The number of characters written so far */
/*
 * Get the resource object of the scheduler.
 */
  Scheduler *sch = (Scheduler* )stream->data;
/*
 * Get the log buffer container.
 */
  ScriptLog *log = &sch->log;
/*
 * Get the length of the string to be output.
 */
  int nc = strlen(text);
/*
 * If there are newline characters in text[] then we will need
 * to accumulate and dispatch the intervening string segments
 * one at a time. Loop until all characters in buffer have been
 * consumed.
 */
  while(ndone < nc) {
    int eos = 0;      /* The index of the end of the next string segment */
/*
 * Find the index of the end of the next string segment to be output.
 * If the remaining string contains a newline character, end the string
 * there.
 */
    for(eos=ndone; eos<nc && text[eos] != '\n'; eos++)
      ;
/*
 * How many characters should we attempt to copy?
 */
    nnew = eos - ndone;
/*
 * Add up to nnew characters to the buffer unless it is already marked as
 * full.
 */
    if(!log->trunc) {
      int ncopy;      /* The number of characters to copy */
/*
 * Truncate the new string to fit the bounds of the buffer?
 */
      if(log->nbuf + nnew > CC_MSG_MAX) {
	ncopy = CC_MSG_MAX - log->nbuf;
	log->trunc = 1;
      } else {
	ncopy = nnew;
      };
/*
 * Copy the new characters into the message buffer.
 */
      memcpy(log->buf + log->nbuf, text + ndone, ncopy);
      log->nbuf += ncopy;
    };
/*
 * Keep a record of how many characters have been consumed from text[].
 */
    ndone += nnew;
/*
 * If a new line was seen, send the contents of the buffer to the
 * logger thread.
 */
    if(eos < nc) {
/*
 * Terminate the string.
 */
      log->buf[log->nbuf] = '\0';
/*
 * Place the message on the pipe queue of the logger thread.
 */
      if(sendLoggerMessage(sch->cp, log->buf, false, log->output->interactive))
	return 1;
/*
 * Reset the buffer.
 */
      log->nbuf = 0;
      log->trunc = 0;
/*
 * Skip the newline.
 */
      ndone++;
    };
  };
  return 0;
}

/*.......................................................................
 * Add a symbol to the list of recognized signal names. This function
 * must only be called from functions of the scheduler thread.
 *
 * Input:
 *  sch   Scheduler *  The resource container of the scheduler thread.
 *  name       char *  The signal name to be added. It isn't an error
 *                     to specify the same name twice.
 * Output:
 *  return   Symbol *  The symbol table entry of the new signal.
 */
Symbol *sch_add_signal(Scheduler *sch, char *name)
{
  Symbol *sig;   /* The symbol table entry of the signal */
/*
 * If the symbol has already been added, do nothing.
 */
  sig = find_HashSymbol(sch->signals.table, name);
  if(sig)
    return sig;
/*
 * Add the new symbol.
 */
  return new_HashSymbol(sch->signals.table, name, 0, 0, NULL, 0);
}

/*.......................................................................
 * Return the symbol table entry of a given signal.
 *
 * Input:
 *  sch      Scheduler *   The resource object of the scheduler thread.
 *  name          char *   The name to lookup.
 * Output:
 *  return      Symbol *   The symbol table entry corresponding to *name,
 *                         or NULL if not found.
 */
Symbol *sch_lookup_signal(Scheduler *sch, char *name)
{
  return find_HashSymbol(sch->signals.table, name);
}

/*.......................................................................
 * Return the list node of the n'th entry in the schedule queue, where
 * 0 is the schedule that will run next, and 1 is the schedule that
 * will run after schedule 0.
 *
 * Input:
 *  sch      Scheduler *  The resource object of the scheduler thread.
 *  n         unsigned    The schedule number to be looked up.
 * Output:
 *  return    ListNode *  The list node of the entry, or NULL if there
 *                        is no n'th schedule.
 */
static ListNode *sch_nth_schedule(Scheduler *sch, unsigned n)
{
  ListNode *node;   /* A node in the list of pending schedules */
  int i;            /* The index of the node being looked at */
/*
 * Find the n'th node.
 */
  for(i=0,node=sch->sched_list->head; node && i < (int)n; node=node->next,i++)
    ;
  return node;
}

/*.......................................................................
 * Remove the nth pending schedule from the schedule queue.
 *
 * Input:
 *  sch      Scheduler *  The resource object of the scheduler thread.
 *  n         unsigned    The queue entry number containing the schedule
 *                        to be removed.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int sch_remove_schedule(Scheduler *sch, unsigned n)
{
  ListNode *node;   /* The list entry of the schedule */
  Script *sc;       /* The removed script */
/*
 * Check arguments.
 */
  if(!sch) {
    lprintf(stderr, "sch_remove_schedule: NULL argument(s).\n");
    return 1;
  };
/*
 * Get the list-entry of the requested schedule.
 */
  node = sch_nth_schedule(sch, n);
/*
 * Already removed?
 */
  if(!node)
    return 0;
/*
 * Delete the node from the list.
 */
  sc = (Script* )del_ListNode(sch->sched_list, node, NULL);
/*
 * Delete the script.
 */
  sch_discard_schedule(sch, sc);
/*
 * Report the change, using an easily parsed syntax.
 */
  if(prefix_status_report(sch, "RM ") ||
     output_ulong(sch->stat.output, (OutputBase)10, "", 0, 0, n) ||
     send_status_report(sch, NULL))
    return 1;
  return 0;
}

/*.......................................................................
 * Change the position of a given schedule in the list of pending
 * schedules.
 *
 * Input:
 *  sch      Scheduler *  The resource object of the scheduler thread.
 *  n         unsigned    The queue entry number containing the schedule
 *                        to be moved.
 *  dn             int    The desired change in the entry number of the
 *                        schedule.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int sch_move_schedule(Scheduler *sch, unsigned n, int dn)
{
  ListNode *node;   /* The list entry of the schedule */
  ListNode *prev;   /* The list-node that precedes the reinsertion point */
  int dst;          /* The destination entry number of the script */
/*
 * Check arguments.
 */
  if(!sch) {
    lprintf(stderr, "sch_remove_schedule: NULL argument(s).\n");
    return 1;
  };
/*
 * Get the list-entry of the requested schedule.
 */
  node = sch_nth_schedule(sch, n);
/*
 * Does the entry actually exist?
 */
  if(!node) {
    lprintf(stderr, "Can't move non-existent queue entry %u.\n", n);
    return 1;
  };
/*
 * Compute the new entry number with the original entry still in the list.
 */
  dst = n + dn + (dn >= 0);
  if(dst < 0) {
    dst = 0;
    dn = -n;
  };
  if(dst > (int)sch->sched_list->nnode) {
    dst = sch->sched_list->nnode;
    dn = dst - 1 - n;
  };
/*
 * Locate the node after which to re-insert the schedule on the list.
 */
  if(dst==0) {
    prev = NULL;
  } else {
    prev = sch_nth_schedule(sch, dst-1);
    if(!prev) {
      lprintf(stderr, "Unable to find destination queue entry.\n");
      return 1;
    };
  };
/*
 * Attempt to insert the node on the list.
 */
  if(!insert_ListNode(sch->sched_list, prev, node->data))
    return 1;
/*
 * Remove the original entry from the list.
 */
  del_ListNode(sch->sched_list, node, NULL);
/*
 * Report the change, using an easily parsed syntax.
 */
  if(prefix_status_report(sch, "MV ") ||
     output_printf(sch->stat.output, "%u %d", n, (int)n+dn) < 0 ||
     send_status_report(sch, NULL))
    return 1;
  return 0;
}

/*.......................................................................
 * Clear and write the identification prefix of a status report message,
 * in sch->stat.pmsg.msg.output.
 *
 * Input:
 *  sch    Scheduler *   The resource object of the scheduler thread.
 *  prefix      char *   The prefix to write.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
static int prefix_status_report(Scheduler *sch, char *prefix)
{
  return clr_StringOutputStream(sch->stat.output) ||
         write_OutputStream(sch->stat.output, prefix);
}

/*.......................................................................
 * Send a scheduler status message to one or more control clients.
 * Before calling this function, the caller should compose a message in
 * sch->stat.pmsg.msg.output[].
 *
 * Input:
 *  sch    Scheduler *   The resource object of the scheduler thread.
 *  client      Pipe *   The output pipe of the client to send the
 *                       report to, or NULL to have the message to
 *                       all connected clients.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
static int send_status_report(Scheduler *sch, sza::util::Pipe *client)
{
  
  // Send the message to each control client.

  if(client) {

    queue_cc_message(client, &sch->stat.pmsg);

  } else {

    ListNode *node;  /* A node in the list of connected clients */

    for(node = sch->clients->head; node; node=node->next) {
      queue_cc_message((sza::util::Pipe* )node->data, &sch->stat.pmsg);
    }

  };

  return 0;
}

/*.......................................................................
 * Add a control-client reply pipe to the list of connected control-clients.
 *
 * Input:
 *  sch     Scheduler *   The state-object of the scheduler thread.
 *  client       Pipe *   The reply pipe of a control client.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int sch_add_client(Scheduler *sch, sza::util::Pipe *client)
{
  ListNode *node;   /* A node in the list of pending schedules */
  
  // Append the client pipe to the list of reply pipes.

  if((node=append_ListNode(sch->clients, client))==NULL)
    return 1;
  
  // Arrange for the current pager status to be communicated to the
  // new client

  if(sch_send_paging_state(sch, sch->pager_allow, node))
    return 1;
  
  // Arrange for the current default antenna selection to be
  // communicated to the new client

  if(sch_send_antenna_selection(sch, node))
    return 1;
  
  // Report the current status of the currently running schedule.

  if(sch->schedule) {
    if(prefix_status_report(sch, "RUN ") ||
       output_script_spec(sch->stat.output, sch->schedule) ||
       send_status_report(sch, client))
      return 1;
  } else {
    if(prefix_status_report(sch, "IDLE") || send_status_report(sch, client))
      return 1;
  };
  
  // Arrange for the current paging conditions to be sent to this
  // client only

  listPager(sch->cp, node);

  // Arrange for the current array configuration to be sent to this
  // client only

  listArrayConfig(sch->cp, node, sza::util::CarmaConfig::SZA);
  listArrayConfig(sch->cp, node, sza::util::CarmaConfig::CARMA);

  // Finally, report the list of schedules that are currently in the
  // schedule queue.

  for(node=sch->sched_list->head; node; node=node->next) {
    if(prefix_status_report(sch, "ADD ") ||
       output_script_spec(sch->stat.output, (Script *)node->data) ||
       send_status_report(sch, client))
      return 1;
  };

  return 0;
}

/*.......................................................................
 * Remove a given control-client reply pipe from the list of
 * connected control-clients.
 *
 * Input:
 *  sch     Scheduler *   The state-object of the scheduler thread.
 *  client       Pipe *   The reply pipe of a control client.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int sch_rem_client(Scheduler *sch, sza::util::Pipe *client)
{
  
  // Locate the specified client.

  if(del_ListNode(sch->clients, NULL, client) != client) {
    lprintf(stderr, "sch_rem_client: Client not found.\n");
    return 1;
  }
  return 0;
}

/*.......................................................................
 * Send a signal to the currently running script.
 *
 * Input:
 *  sch   Scheduler *   The resource container of the scheduler thread.
 *  sig      Symbol *   The symbol table entry of the signal.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int sch_signal_schedule(Scheduler *sch, Symbol *sig)
{
/*
 * Check the arguments.
 */
  if(!sch || !sig) {
    lprintf(stderr, "sch_signal_schedule: NULL argument(s).\n");
    return 1;
  };
/*
 * Do we have a schedule to signal?
 */
  if(!sch->schedule) {
    lprintf(stderr, "There is no running schedule to signal.\n");
    return 1;
  };
/*
 * Send the signal.
 */
  return signal_script(sch->schedule, sig);
}

/*.......................................................................
 * Discard all legacy signals.
 *
 * Input:
 *  sch      Scheduler *    The resource object of the scheduler task.
 * Output:
 *  return         int      0 - OK.
 *                          1 - Error.
 */
static int sch_reset_signals(Scheduler *sch)
{
  return scan_HashTable(sch->signals.table, sch_zero_signal, NULL);
}

/*.......................................................................
 * This is a hash-table-traversal work function, used to clear the signal
 * represented by a given symbol.
 *
 * Input:
 *  sym     Symbol *   The symbol table of a signal to be cleared.
 *  context   void *   Ignored.
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */
static HASH_SCAN_FN(sch_zero_signal)
{
  sym->code = 0;
  return 0;
}

/*.......................................................................
 * Return sch->sched_cache for use by szascript.
 *
 * Input:
 *  sch        Scheduler *  The resource object of this thread.
 * Output:
 *  return    SchedCache *  The parameter cache, or NULL on error.
 */
SchedCache *sch_sched_cache(Scheduler *sch)
{
  if(!sch) {
    lprintf(stderr, "sch_sched_cache: NULL argument(s).\n");
    return NULL;
  };
  return &sch->cache;
}

/*.......................................................................
 * Iterate through the auto-queue directory, looking for the next
 * schedule to be executed.
 *
 * Input:
 *  sch   Schedule    *  The scheduler thread container.
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int sch_next_auto_file(Scheduler *sch)
{
  char *errmsg;             /* An error message */
  DIR *dir=NULL;            /* The directory iterator object */
  struct dirent *dent;      /* The directory entry returned by readdir() */
  struct dirent *buff=NULL; /* A buffer for reading directory entries */
#if _POSIX_C_SOURCE >= 199506L
  int name_max;             /* The maximum length of a directory entry */
  int status=0;               /* The return status of a system call */
#endif
  char *dname=NULL, *pname=NULL; /* Pointers to the names of the next file */
  char *path=NULL;           /* The expanded current file name */
  char *pdir = NULL, *ddir = NULL;
  long prior, highest_prior=0;
  int first=1;
  struct stat stat_curr, stat_highest;
  int waserr=0;

  sch->autoqueue.pending = 0;
  /*
   * Check that the auto directories have been specified.
   */
  if(!sch->autoqueue.penddir || !sch->autoqueue.donedir ) {
    lprintf(stderr, "Autoqueue directories not specified");
    return 1;
  }
  pdir = sch->autoqueue.penddir;
  ddir = sch->autoqueue.donedir;
/*
 * Attempt to open the pending directory.
 */
  dir = opendir(pdir);
  if(!dir) {
    lprintf(stderr, "Can\'t open \"%s\": %s\n", pdir, strerror(errno));
    return 1;
  };
/*
 * Determine the maximum file-name length of a directory entry.
 */
#if _POSIX_C_SOURCE >= 199506L /* This code will not work if not true */
  errno = 0;
  name_max = pathconf(pdir, _PC_NAME_MAX);
  if(name_max < 0) {
    if(errno) {
      lprintf(stderr, "pathconf(%s, _PC_NAME_MAX): %s\n", pdir,
	      strerror(errno));
      return 1;
    } else {
      name_max = 1024;
    };
  };
/*
 * Allocate a directory-entry container.
 */
  buff = (struct dirent* )malloc(sizeof(struct dirent) + name_max + 1);
  if(!buff) {
    lprintf(stderr, "Error allocating directory traversal buffer.\n");
    waserr = 1;
  };
  /*
   * Allocate space for a copy of the expanded file name.
   */
  if((path=(char* )malloc(name_max + 1))==NULL) {
    lprintf(stderr, "Error allocating directory traversal pathname.\n");
    waserr = 1;
  };
  /*
   * And (Re)-allocate space for a copy of the expanded file name
   * in the scheduler container.
   */
  if((sch->autoqueue.pendname=(char* )realloc(sch->autoqueue.pendname, name_max + 1))
     ==NULL) {
    lprintf(stderr, "Error allocating pending file path.\n");
   waserr = 1;
  };
  /*
   * And (Re)-allocate space for a copy of the expanded name to which
   * this file should be moved when finished executing.
   */
  if((sch->autoqueue.donename=(char* )realloc(sch->autoqueue.donename, name_max + 1))
     ==NULL) {
    lprintf(stderr, "Error allocating done file path.\n");
    waserr = 1;
  };
#endif
  /*
   * More convenient pointers to the scheduler names.
   */  
  pname = sch->autoqueue.pendname;
  dname = sch->autoqueue.donename;
/*
 * Iterate through the directory looking for entries.
 */
#if _POSIX_C_SOURCE >= 199506L
  while(!waserr && (status=readdir_r(dir, buff, &dent))==0 && dent != NULL)
#else
  while(!waserr && (dent = readdir(dir)) != NULL)
#endif
  {
    /*
     * Ignore . and .. entries (and any .xxx files)
     */
    if(dent->d_name[0]=='.')
      continue;
    /*
     * Ignore emacs leavings
     */
    if(dent->d_name[0]=='#' || dent->d_name[strlen(dent->d_name)-1]=='~')
      continue;
    /*
     * Ignore if first char not number
     */
    if(!isdigit((int)dent->d_name[0]))
      continue;
    /*
     * Compose the full path name of the file. 
     */
    strcpy(path, pdir);
    strcat(path, "/");
    strcat(path, dent->d_name);
    /*
     * Check entry is a regular file and readable
     */
    errmsg = test_pathname(path, PATH_IS_REG, PATH_READ);
    if(errmsg) {
      lprintf(stderr, "Ignoring %s (%s).\n", path, errmsg);
      continue;
    }
    /*
     * Read priority tag
     */
    prior=strtol(dent->d_name, NULL, 10);
    if(errno) {
      lprintf(stderr, "Error reading priority of %s: %s\n", path,
	      strerror(errno));
      continue;
    };
    /*
     * If this is the first schedule of this directory scan
     */
    if(first) {
      first=0;
      sch->autoqueue.pending=1;
      highest_prior = prior;
      strcpy(pname, path);
      strcpy(dname, ddir); strcat(dname, "/"); strcat(dname, dent->d_name);
    }
    /*
     * Now check the priority against the last higest priority file found.
     * The highest priority file will have the lowest priority number
     * attached to it.
     */
    if(prior < highest_prior) {
      highest_prior = prior;
      strcpy(pname, path);
      strcpy(dname, ddir); strcat(dname, "/"); strcat(dname, dent->d_name);
    }
    /*
     * Else if the files have the same priority, use the one with the 
     * earliest creation date.
     */ 
    else if(prior==highest_prior) {
      if(stat(path, &stat_curr) < 0) {
	lprintf(stderr, "Error checking information for file %s: %s\n", path, 
		strerror(errno));
	waserr = 1;
      }
      if(stat(pname, &stat_highest) < 0) {
	lprintf(stderr, "Error checking information for file %s: %s\n", path, 
		strerror(errno));
	waserr |= 1;
      }
      if(!waserr) {
	/*
	 * Check the timestamps of the files if their priorities are the same
	 */
#if defined(VWX) /* Use posix realtime extensions where available */
	if((stat_curr.st_mtim.tv_sec < stat_highest.st_mtim.tv_sec) ||
	   ((stat_curr.st_mtim.tv_sec == stat_highest.st_mtim.tv_sec) &&
	    (stat_curr.st_mtim.tv_nsec < stat_highest.st_mtim.tv_nsec))) {
#else
	if((stat_curr.st_mtime < stat_highest.st_mtime)) {
#endif
	  strcpy(pname, path);
	  strcpy(dname, ddir); strcat(dname, "/"); strcat(dname, dent->d_name);
	}
      }
    }
  }
/*
 * Close the directory and discard the directory-entry buffer.
 */
  closedir(dir);
  free(buff);
  dir = NULL;
  buff = NULL;
/*
 * Error?
 *
 * Note that under Solaris releases before version 7, there appears to
 * be a bug in readdir_r() that causes status to be set to EINVAL
 * instead of 0 when there are no more directory entries to be processed.
 */
#if _POSIX_C_SOURCE >= 199506L
  if(status && status!=EINVAL) {
    lprintf(stderr, "Error reading %s: %s\n", pdir, strerror(status));
    waserr = 1;
  };
#endif
  /*
   * Don't do anything if we failed to find any files -- this is not 
   * (necessarily) an error.
   */
  /*
   * Free memory allocated in this function.
   */
  if(path)
    free(path);

  return waserr;
}
/*.......................................................................
 * Move the last file we executed from the "pending" directory to the
 * "done" directory.
 *
 * Input:
 *  
 *  sch  Scheduler  *  The container of the scheduler thread.
 *
 * Output:
 * 
 *  0 -- ok.
 */
static int sch_remove_pending_file(Scheduler *sch)
{
  output_printf(sch->log.output,"moving %s to %s\n",
		sch->autoqueue.pendname, sch->autoqueue.donename);
  if(rename(sch->autoqueue.pendname, sch->autoqueue.donename) != 0) {
    lprintf(stderr, "Error renaming file %s: %s\n", sch->autoqueue.pendname, 
	    strerror(errno));
    return 1;
  }
  if(utime(sch->autoqueue.donename,NULL) != 0) {
    lprintf(stderr, "Error updating timestamp of file %s: %s\n",
	    sch->autoqueue.donename, strerror(errno));
    return 1;
  }
  sch->autoqueue.pending=0;
  return 0;  
}
/*.......................................................................
 * Prepare a pager message for subsequent transmission
 * to connected control clients.
 *
 * Input:
 *  msg     SchedulerMessage *  The message object being prepared for
 *                              subsequent transmission.
 *  allow   int                 If true, the client is allowed to send pager 
 *                              requests
 *  client  ListNode         *  The client to send to, or NULL for all clients
 *
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int sendPagingState(ControlProg* cp, int allow, unsigned mask, char* msg, ListNode* node)
{
  return sch_send_paging_state(cp_Scheduler(cp), allow, node, mask, msg);
}

int sch_send_paging_state(Scheduler *sch, int allow, ListNode *client, 
			  unsigned mask, char* msg)
{
  ListNode *node;  /* A node in the list of connected clients */
  CcPipeMsg pmsg;
  
  // Check arguments.

  if(!sch) {
    lprintf(stderr, "pack_scheduler_pager: NULL argument(s).\n");
    return 1;
  };

  pmsg.id = CC_PAGE_MSG;

  pmsg.msg.page.text[0]  = '\0';
  pmsg.msg.page.mask     = mask;
  pmsg.msg.page.allow    = allow;

  if(msg) {
    unsigned msglen = strlen(msg);
    msglen = msglen > CC_MSG_MAX ? CC_MSG_MAX : msglen;
    strncpy(pmsg.msg.page.text, msg, msglen);
    pmsg.msg.page.text[msglen]  = '\0';
  }
    
  if(client) {
    queue_cc_message((sza::util::Pipe* )client->data, &pmsg);
  } else {
    for(node = sch->clients->head; node; node=node->next) {
      queue_cc_message((sza::util::Pipe* )node->data, &pmsg);
    }
  }
  /*
   * And update the stored paging status
   */
  sch->pager_allow = allow;

  // And set whether or not timeout paging is allowed

  allowTimeOutPaging(sch->cp, allow);

  return 0;
}

/*.......................................................................
 * Prepare an antenna message for subsequent transmission
 * to connected control clients.
 *
 * Input:
 *  msg     SchedulerMessage *  The message object being prepared for
 *                              subsequent transmission.
 *  allow   int                 If true, the client is allowed to send pager 
 *                              requests
 *  client  ListNode         *  The client to send to, or NULL for all clients
 *
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int sch_send_antenna_selection(Scheduler *sch, ListNode *client)
{
  ListNode *node;  /* A node in the list of connected clients */
  CcPipeMsg pmsg;
  
  // Check arguments.

  if(!sch) {
    lprintf(stderr, "sch_send_antenna_selection: NULL argument(s).\n");
    return 1;
  };

  pmsg.id = CC_ANT_MSG;

  strncpy(pmsg.msg.ant.text, 
	  cp_AntSet(sch->cp)->getString().c_str(), CC_MSG_MAX);

  if(client) {
    queue_cc_message((sza::util::Pipe* )client->data, &pmsg);
  } else {
    for(node = sch->clients->head; node; node=node->next) {
      queue_cc_message((sza::util::Pipe* )node->data, &pmsg);
    }
  }

  return 0;
}
/*.......................................................................
 * Prepare a change-default-directory message for subsequent transmission
 * to the scheduler thread.
 *
 * Input:
 *  msg   SchedulerMessage *  The message object being prepared for
 *                            subsequent transmission.
 *  dir               char *  The directory in which to look for autoqueue files
 *
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int pack_scheduler_auto_dir(SchedulerMessage *msg, char *dir)
{
/*
 * Check arguments.
 */
  if(!msg || !dir) {
    lprintf(stderr, "pack_scheduler_auto_dir: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_AUTO_DIR;
  strncpy(msg->body.auto_dir.dir, dir, CP_FILENAME_MAX);
  msg->body.auto_dir.dir[CP_FILENAME_MAX] = '\0';
  return 0;
}
/*.......................................................................
 * Prepare a polling interval message for subsequent transmission to the
 * scheduler thread.
 *
 * Input:
 *  msg   SchedulerMessage *  The message object being prepared for
 *                            subsequent transmission.
 *  ms           long      *  The number of milliseconds to wait between polling
 *                            the qutoqueue directory.
 *
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int pack_scheduler_auto_poll(SchedulerMessage *msg, long ms)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_scheduler_auto_poll: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_AUTO_POLL;
  msg->body.auto_poll.poll = ms;
  return 0;
}

/*.......................................................................
 * Prepare an autoqueue state-change message for subsequent transmission to the
 * scheduler thread.
 *
 * Input:
 *  msg   SchedulerMessage *  The message object being prepared for
 *                            subsequent transmission.
 *  on                 int       Autoqueueing on (1) or off (0) ?
 *
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int pack_scheduler_auto_state(SchedulerMessage *msg, int on)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_scheduler_auto_state: NULL argument(s).\n");
    return 1;
  };
  msg->type = SCH_AUTO_STATE;
  msg->body.auto_state.on = on;
  return 0;
}
/*.......................................................................
 * Change the directory in which we will look for subsequent auto-queue
 * files.
 *
 * Input:
 *  dir      char *   The directory to use when opening subsequent auto-queue
 *                    files.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
static int chdir_auto(Scheduler *sch, char *dir)
{
/*
 * Make a copy of the name of the current scheduler directory.
 */
  if(dir != sch->autoqueue.dir && *dir != '\0') {
    size_t bytes = strlen(dir)+strlen("pending")+1;
    char *ptmp = (char* )(sch->autoqueue.penddir ? realloc(sch->autoqueue.penddir, 
						  bytes) : malloc(bytes));
    char *dtmp=NULL;
    if(!ptmp) {
      lprintf(stderr, "Unable to record new auto-queue directory.\n");
      return 1;
    } else {
      strcpy(ptmp, dir);
      strcat(ptmp, "/pending");
      sch->autoqueue.penddir = ptmp;
    };

    dtmp = (char* )(sch->autoqueue.donedir ? realloc(sch->autoqueue.donedir, 
						     bytes) : malloc(bytes));
    if(!dtmp) {
      lprintf(stderr, "Unable to record new auto-queue directory.\n");
      return 1;
    } else {
      strcpy(dtmp, dir);
      strcat(dtmp, "/done");
      sch->autoqueue.donedir = dtmp;
    };
  };
  /*
   * Now attempt to open the specified directories.
   */
  {
    DIR *test=NULL;
    int waserr=0;

    test = opendir(sch->autoqueue.penddir);
    if(!test) {
      lprintf(stderr, "Can\'t open \"%s\": %s\n", strerror(errno), 
	      sch->autoqueue.penddir);
      waserr = 1;
    };
    if(!waserr)
      closedir(test);

    test = NULL;
    test = opendir(sch->autoqueue.penddir);
    if(!test) {
      lprintf(stderr, "Can\'t open \"%s\": %s\n", strerror(errno), 
	      sch->autoqueue.penddir);
      waserr = 1;
    };
    if(!waserr)
      closedir(test);
    if(waserr)
      return 1;
  }
  return 0;
}

/*.......................................................................
 * Prepare a pager message for subsequent transmission
 * to connected control clients.
 *
 * Input:
 *  msg     SchedulerMessage *  The message object being prepared for
 *                              subsequent transmission.
 *  allow   int                 If true, the client is allowed to send pager 
 *                              requests
 *  client  ListNode         *  The client to send to, or NULL for all clients
 *
 * Output:
 *  return             int    0 - OK.
 *                            1 - Error.
 */
int sch_sendPagerCondition(Scheduler* sch, ListNode* client, 
			   unsigned mode, sza::util::PagerMonitor::RegSpec* regSpec)
{
  ListNode *node;  // A node in the list of connected clients
  CcPipeMsg pmsg;

  // Check arguments.

  if(!sch) {
    lprintf(stderr, "sendPagerCondition: NULL argument(s).\n");
    return 1;
  };

  pmsg.id = CC_PAGECOND_MSG;

  // If this is a clear condition, then the RegSpec pointer will be
  // NULL.  Otherwise, it shouldn't be

  if(mode != PAGECOND_CLEAR && mode != PAGECOND_UPDATE) {

    if(regSpec == 0)
      ThrowError("RegSpec argument is NULL");

    // Copy the pager condition
    
    if(regSpec->name_.size() > CC_MSG_MAX) {
      lprintf(stderr, "sendPagerCondition: Pager condition is too long\n");
      return 1;
    }

    strcpy(pmsg.msg.pageCond.text, (char*)regSpec->name_.c_str());

    pmsg.msg.pageCond.min          = regSpec->min_;
    pmsg.msg.pageCond.max          = regSpec->max_;
    pmsg.msg.pageCond.isDelta      = regSpec->isDelta_;
    pmsg.msg.pageCond.nFrame       = regSpec->nFrame_;
    pmsg.msg.pageCond.isOutOfRange = regSpec->isOutOfRange_;
  }

  pmsg.msg.pageCond.mode         = mode;

  if(client) {
    queue_cc_message((sza::util::Pipe* )client->data, &pmsg);
  } else {
    for(node = sch->clients->head; node; node=node->next) {
      queue_cc_message((sza::util::Pipe* )node->data, &pmsg);
    }
  }

  return 0;
}

/*.......................................................................
 * Prepare a command timeout message for subsequent transmission
 * to connected control clients.
 */
int sch_sendCmdTimeoutConfiguration(Scheduler* sch, ListNode* client, 
				    unsigned seconds)
{
  ListNode *node;  // A node in the list of connected clients
  CcPipeMsg pmsg;

  // Check arguments.

  if(!sch) {
    lprintf(stderr, "sch_sendCmdTimeoutConfiguration: NULL argument(s).\n");
    return 1;
  };

  pmsg.id = CC_CMD_TIMEOUT_MSG;
  pmsg.msg.cmdTimeout.seconds = seconds;
  pmsg.msg.cmdTimeout.mode    = CT_TIMEOUT;

  if(client) {
    queue_cc_message((sza::util::Pipe* )client->data, &pmsg);
  } else {
    for(node = sch->clients->head; node; node=node->next) {
      queue_cc_message((sza::util::Pipe* )node->data, &pmsg);
    }
  }

  return 0;
}

/*.......................................................................
 * Prepare a command timeout message for subsequent transmission
 * to connected control clients.
 */
int sch_sendCmdTimeoutConfiguration(Scheduler* sch, ListNode* client, 
				    bool active)
{
  ListNode *node;  // A node in the list of connected clients
  CcPipeMsg pmsg;

  // Check arguments.

  if(!sch) {
    lprintf(stderr, "sch_sendCmdTimeoutConfiguration: NULL argument(s).\n");
    return 1;
  };

  pmsg.id = CC_CMD_TIMEOUT_MSG;
  pmsg.msg.cmdTimeout.active = active;
  pmsg.msg.cmdTimeout.mode   = CT_ACTIVE;

  if(client) {
    queue_cc_message((sza::util::Pipe* )client->data, &pmsg);
  } else {
    for(node = sch->clients->head; node; node=node->next) {
      queue_cc_message((sza::util::Pipe* )node->data, &pmsg);
    }
  }

  return 0;
}

/**.......................................................................
 * Prepare a pager message for subsequent transmission
 * to connected control clients.
 */
int sendPagerCondition(ControlProg* cp, unsigned mode, 
		       sza::util::PagerMonitor::RegSpec* reg, ListNode* node)
{
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);

  if(sch_sendPagerCondition(sch, node, mode, reg))
    return 1;

  return 0;
}

/**.......................................................................
 * Prepare a pager message for subsequent transmission
 * to connected control clients.
 */
int sendCmdTimeoutConfiguration(ControlProg* cp, unsigned seconds, ListNode* node)
{
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);

  if(sch_sendCmdTimeoutConfiguration(sch, node, seconds))
    return 1;

  return 0;
}

/**.......................................................................
 * Prepare a pager message for subsequent transmission
 * to connected control clients.
 */
int sendCmdTimeoutConfiguration(ControlProg* cp, bool enable, ListNode* node)
{
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);

  if(sch_sendCmdTimeoutConfiguration(sch, node, enable))
    return 1;

  return 0;
}

/*.......................................................................
 * Prepare an array configuration message for subsequent transmission
 * to connected control clients.
 */
int sendArrayConfiguration(ControlProg* cp, ListNode* node,
			   unsigned mode,
			   unsigned array, unsigned config)
{
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);

  if(sch_sendArrayConfiguration(sch, node, mode, array, config))
    return 1;
  
  return 0;
}

/*.......................................................................
 * Prepare an array configuration message for subsequent transmission
 * to connected control clients.
 */
int sch_sendArrayConfiguration(Scheduler* sch, ListNode* client, 
			       unsigned mode, unsigned array, unsigned config)
{
  ListNode *node;  // A node in the list of connected clients
  CcPipeMsg pmsg;

  // Check arguments.

  if(!sch) {
    lprintf(stderr, "sendConfiguration: NULL argument(s).\n");
    return 1;
  };

  pmsg.id = CC_CONFIG_MSG;

  // If this is a clear condition, then the RegSpec pointer will be
  // NULL.  Otherwise, it shouldn't be

  pmsg.msg.config.mode    = mode;

  if(mode == CONFIG_CONFIG && 
     (array == sza::util::CarmaConfig::NONE || config == sza::util::CarmaConfig::NONE)) {
    ThrowError("You must specify an array/config");
  }
  
  pmsg.msg.config.array   = array;
  pmsg.msg.config.config  = config;
  
  // If client was specified, send just to that client.  Otherwise
  // send to all connected clients

  if(client) {
    queue_cc_message((sza::util::Pipe* )client->data, &pmsg);
  } else {
    for(node = sch->clients->head; node; node=node->next) {
      queue_cc_message((sza::util::Pipe* )node->data, &pmsg);
    }
  }

  return 0;
}

/*.......................................................................
 * Prepare an array configuration message for subsequent transmission
 * to connected control clients.
 */
int sendAddArrayAntenna(ControlProg* cp, ListNode* node,
			unsigned array, unsigned iPad,
			unsigned antType,
			int iAnt)
{
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);
  
  if(sch_sendAddArrayAntenna(sch, node, array, iPad, antType, iAnt))
    return 1;
  
  return 0;
}

/*.......................................................................
 * Prepare an array configuration message for subsequent transmission
 * to connected control clients.
 */
int sch_sendAddArrayAntenna(Scheduler* sch, ListNode* client, 
			    unsigned array, unsigned iPad, 
			    unsigned antType,
			    int iAnt)
{
  ListNode *node;  // A node in the list of connected clients
  CcPipeMsg pmsg;
  
  // Check arguments.

  if(!sch) {
    lprintf(stderr, "sendConfiguration: NULL argument(s).\n");
    return 1;
  };

  pmsg.id = CC_CONFIG_MSG;

  // If this is a clear condition, then the RegSpec pointer will be
  // NULL.  Otherwise, it shouldn't be

  pmsg.msg.config.mode    = CONFIG_ADDANT;
  pmsg.msg.config.array   = array;
  pmsg.msg.config.iPad    = iPad;
  pmsg.msg.config.antType = antType;
  pmsg.msg.config.iAnt    = iAnt;

  // If client was specified, send just to that client.  Otherwise
  // send to all connected clients

  if(client) {
    queue_cc_message((sza::util::Pipe* )client->data, &pmsg);
  } else {
    for(node = sch->clients->head; node; node=node->next) {
      queue_cc_message((sza::util::Pipe* )node->data, &pmsg);
    }
  }
  
  return 0;
}

/*.......................................................................
 * Prepare an array configuration message for subsequent transmission
 * to connected control clients.
 */
int sendRemArrayAntenna(ControlProg* cp, ListNode* node,
			unsigned array, unsigned iPad)
{
  Scheduler *sch = (Scheduler* )cp_ThreadData(cp, CP_SCHEDULER);

  if(sch_sendRemArrayAntenna(sch, node, array, iPad))
    return 1;
  
  return 0;
}

/*.......................................................................
 * Prepare an array configuration message for subsequent transmission
 * to connected control clients.
 */
int sch_sendRemArrayAntenna(Scheduler* sch, ListNode* client, 
			    unsigned array, unsigned iPad)
{
  ListNode *node;  // A node in the list of connected clients
  CcPipeMsg pmsg;

  // Check arguments.

  if(!sch) {
    lprintf(stderr, "sendConfiguration: NULL argument(s).\n");
    return 1;
  };

  pmsg.id = CC_CONFIG_MSG;

  // If this is a clear condition, then the RegSpec pointer will be
  // NULL.  Otherwise, it shouldn't be

  pmsg.msg.config.mode    = CONFIG_REMANT;
  pmsg.msg.config.array   = array;
  pmsg.msg.config.iPad    = iPad;

  // If client was specified, send just to that client.  Otherwise
  // send to all connected clients

  if(client) {
    queue_cc_message((sza::util::Pipe* )client->data, &pmsg);
  } else {
    for(node = sch->clients->head; node; node=node->next) {
      queue_cc_message((sza::util::Pipe* )node->data, &pmsg);
    }
  }

  return 0;
}
