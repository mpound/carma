#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <time.h>
#include <sstream>

#include "lprintf.h"
#include "szacontrol.h"
#include "astrom.h"
#include "list.h"
#include "arcfile.h"

#include "carma/szaarrayutils/TransactionManager.h"

#include "carma/szautil/FdSet.h"
#include "carma/szautil/LogMsgHandler.h"

using namespace sza::array;
using namespace std;

// An object of the following type is used to maintain the state of
// the logger thread.

struct Logger {
  ControlProg* cp;             // The state-object of the control program 
  sza::util::Pipe* pipe;       // An externally allocated control-input pipe 
  ListMemory* list_mem;        // Memory for allocating lists and their nodes 
  List* clients;               // The list of connected control clients 
  int year, month, day;        // The current date (after being logged) 
  char msg[LOG_MSGLEN+1];      // The message that is currently being written 
  char* dir;                   // The directory in which to place new
                               // log files.
  char* path;                  // The pathname of the current log file 
  FILE* fp;                    // The file-pointer attached to 'path' 

  // An object to manage transaction definitions

  TransactionManager* transManager_;

  // A list of emails to send to when a transaction is logged

  std::vector<std::string>* emails_;
};

static void flush_logfile(Logger *log);
static void close_logfile(Logger *log);
static int chdir_logger(Logger *log, char *dir);
static int open_logfile(Logger *log, char *dir);
static int log_date_message(Logger *log, Date *date);

static int dispatch_log_message(Logger *log, char *text, LogStream nature, 
				unsigned seq, bool isEnd, 
				bool interactive=false);

static int write_logclient(Logger *log, char *text, LogStream nature, 
			   unsigned seq, bool isEnd, bool interactive=false);

static int write_logfile(Logger *log, char *text, LogStream nature, 
			 unsigned seq, bool isEnd, bool interactive=false);

static int write_terminal(Logger *log, char *text, LogStream nature);

static int drain_log_pipe(Logger *log);

static int log_add_client(Logger *log, sza::util::Pipe *client);
static int log_rem_client(Logger *log, sza::util::Pipe *client);

static LOG_DISPATCHER(lprintf_to_logger);
static Logger *cp_Logger(ControlProg *cp);

static LOG_HANDLER_FN(stdoutLogger);
static LOG_HANDLER_FN(stderrLogger);

static int log_log_transaction(Logger* log, char* device, char* serial, 
			       char* location, double date, char* who,
			       char* comment);
static int logAddEmailAddress(Logger *logger, bool add, char* email);

static int sendTransactionEmail(Logger *logger, std::ostringstream& message);

/*.......................................................................
 * Create the context object of a logger thread.
 *
 * Input:
 *  cp     ControlProg *   The state object of the control program.
 *  pipe          Pipe *   The pipe on which to listen for control messages.
 * Output:
 *  return        void *   The new Logger object, or NULL on error.
 */
CP_NEW_FN(new_Logger)
{
  Logger *log;     /* The object to be returned */
/*
 * Allocate the container.
 */
  log =(Logger* )malloc(sizeof(Logger));
  if(!log) {
    lprintf(stderr, "new_Logger: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the container
 * at least up to the point at which it can be safely passed to del_Logger().
 */
  log->transManager_ = 0;
  log->emails_ = 0;
  log->cp = cp;
  log->pipe = pipe;
  log->list_mem = NULL;
  log->clients = NULL;
  log->year = log->month = log->day;
  log->msg[0] = '\0';
  log->dir = NULL;
  log->path = NULL;
  log->fp = NULL;
/*
 * Allocate a memory supplier for lists and their nodes.
 */
  log->list_mem = new_ListMemory(5, 20);
  if(!log->list_mem)
    return del_Logger(log);
/*
 * Allocate the list of control-client reply pipes.
 */
  log->clients = new_List(log->list_mem);
  if(!log->clients)
    return del_Logger(log);

  // Allocate a new transaction manager

  log->transManager_ = new TransactionManager();

  log->emails_ = new std::vector<std::string>();

  return log;
}

/*.......................................................................
 * Delete the state-object of a logger thread.
 *
 * Input:
 *  obj     void *   The Logger object to be deleted.
 * Output:
 *  return  void *   The deleted Logger object (always NULL).
 */
CP_DEL_FN(del_Logger)
{
  Logger *log = (Logger* )obj;
  if(log) {
    log->clients = del_List(log->clients);
    if(log->dir) {
      free(log->dir);
      log->dir = NULL;
    };
    close_logfile(log);
    
    // Delete the list memory supplier after having deleted all lists
    // that used it.

    log->list_mem = del_ListMemory(log->list_mem, 1);

    // Finally, delete the transaction manager

    if(log->transManager_ != 0)
      delete log->transManager_;

    if(log->emails_ != 0)
      delete log->emails_;

    free(log);
  };
  return NULL;
}

/*.......................................................................
 * Return the logger resource object.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 * Output:
 *  return        Logger *   The logger resource object.
 */
static Logger *cp_Logger(ControlProg *cp)
{
  return (Logger* )cp_ThreadData(cp, CP_LOGGER);
}

/*.......................................................................
 * Attempt to send a message to a logger thread.
 *
 * Input:
 *  cp      ControlProg *  The state-object of the control program.
 *  msg   LoggerMessage *  The message to be sent. This must have been
 *                         filled by one of the pack_logger_<type>()
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
PipeState send_LoggerMessage(ControlProg *cp, LoggerMessage *msg,
			     long timeout)
{
  return cp_Logger(cp)->pipe->write(msg, sizeof(*msg), timeout);
}

/*.......................................................................
 * Send a shutdown message to the logger thread using non-blocking I/O.
 * Return 0 if the message was sent, 0 otherwise.
 *
 * Input:
 *  cp      ControlProg *  The control-program resource object.
 * Output:
 *  return          int    0 - Message sent ok.
 *                         1 - Unable to send message.
 */
CP_STOP_FN(stop_Logger)
{
  LoggerMessage msg;   /* The message to be sent */
  return pack_logger_shutdown(&msg) ||
         send_LoggerMessage(cp, &msg, PIPE_NOWAIT) != PIPE_OK;
}

/*.......................................................................
 * Prepare a shutdown message for subsequent transmission to the
 * logger thread.
 *
 * Input:
 *  msg   LoggerMessage *  The message object to be packed for subsequent
 *                         transmission.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int pack_logger_shutdown(LoggerMessage *msg)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_logger_shutdown: NULL argument.\n");
    return 1;
  };
  msg->type = LOG_SHUTDOWN;
  return 0;
}

/*.......................................................................
 * Prepare a change-default-directory message for subsequent transmission
 * to the logger thread.
 *
 * Input:
 *  msg   LoggerMessage *  The message object to be packed for subsequent
 *                         transmission.
 *  dir            char *  The directory in which to open subsequent log
 *                         files.
 * Output:
 * return           int    0 - OK.
 *                         1 - Error.
 */
int pack_logger_chdir(LoggerMessage *msg, char *dir)
{
/*
 * Check arguments.
 */
  if(!msg || !dir) {
    lprintf(stderr, "pack_logger_chdir: NULL argument(s).\n");
    return 1;
  };
  msg->type = LOG_CHDIR;
  strncpy(msg->body.chdir.dir, dir, CP_FILENAME_MAX);
  msg->body.chdir.dir[CP_FILENAME_MAX] = '\0';
  return 0;
}

/*.......................................................................
 * Prepare an open-log-file message for subsequent transmission to the
 * logger thread.
 *
 * Input:
 *  msg   LoggerMessage *  The message object to be packed for subsequent
 *                         transmission.
 *  dir            char *  The directory in which to open the file.
 * Output:
 * return           int    0 - OK.
 *                         1 - Error.
 */
int pack_logger_open(LoggerMessage *msg, char *dir)
{
/*
 * Check arguments.
 */
  if(!msg || !dir) {
    lprintf(stderr, "pack_logger_open: NULL argument(s).\n");
    return 1;
  };
  msg->type = LOG_OPEN;
  strncpy(msg->body.open.dir, dir, CP_FILENAME_MAX);
  msg->body.open.dir[CP_FILENAME_MAX] = '\0';
  return 0;
}

/*.......................................................................
 * Prepare a flush-log-file message for subsequent transmission to the
 * logger thread.
 *
 * Input:
 *  msg   LoggerMessage *  The message object to be packed for subsequent
 *                         transmission.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int pack_logger_flush(LoggerMessage *msg)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_logger_flush: NULL argument.\n");
    return 1;
  };
  msg->type = LOG_FLUSH;
  return 0;
}

/*.......................................................................
 * Prepare a close-log-file message for subsequent transmission to the
 * logger thread.
 *
 * Input:
 *  msg   LoggerMessage *  The message object to be packed for subsequent
 *                         transmission.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int pack_logger_close(LoggerMessage *msg)
{
/*
 * Check arguments.
 */
  if(!msg) {
    lprintf(stderr, "pack_logger_close: NULL argument.\n");
    return 1;
  };
  msg->type = LOG_CLOSE;
  return 0;
}

/*.......................................................................
 * Prepare a log-message for subsequent transmission to the
 * logger thread.
 *
 * Input:
 *  msg   LoggerMessage *  The message object to be packed for subsequent
 *                         transmission.
 *  text           char *  The message to be logged.
 *  nature    LogStream    The disposition of the message:
 *                          LOG_STDOUT - An informational message.
 *                          LOG_STDERR - An error message.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int pack_logger_message(LoggerMessage *msg, char *text, LogStream nature,
			unsigned seq, bool end, bool interactive)
{
  // Check arguments.
  
  if(!msg || !text) {
    lprintf(stderr, "pack_logger_message: NULL argument(s).\n");
    return 1;
  };
  
  msg->type = LOG_MESSAGE;
  
  msg->body.message.nature = nature;
  msg->body.message.seq    = seq;
  msg->body.message.end    = end;
  msg->body.message.interactive = interactive;
  
  strncpy(msg->body.message.text, text, LOG_MSGLEN);
  msg->body.message.text[LOG_MSGLEN] = '\0';
  
  return 0;
}

void bufferLogMsg(char* message)
{
}

/*.......................................................................
 * Prepare a register-control-client message for subsequent transmission
 * to the logger thread.
 *
 * Input:
 *  msg   LoggerMessage *  The message object to be packed for subsequent
 *                         transmission.
 *  client         Pipe *  The reply pipe of the new control client.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int pack_logger_add_client(LoggerMessage *msg, sza::util::Pipe *client)
{
  
  // Check arguments.

  if(!msg || !client) {
    lprintf(stderr, "pack_logger_add_client: NULL argument(s).\n");
    return 1;
  };

  msg->type = LOG_ADD_CLIENT;
  msg->body.client.pipe = client;

  return 0;
}

/*.......................................................................
 * Prepare an unregister-control-client message for subsequent
 * transmission to the logger thread.
 *
 * Input:
 *  msg   LoggerMessage *  The message object to be packed for subsequent
 *                         transmission.
 *  client         Pipe *  The reply pipe of the new control client.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.
 */
int pack_logger_rem_client(LoggerMessage *msg, sza::util::Pipe *client)
{
  
  // Check arguments.

  if(!msg || !client) {
    lprintf(stderr, "pack_logger_rem_client: NULL argument(s).\n");
    return 1;
  };

  msg->type = LOG_REM_CLIENT;
  msg->body.client.pipe = client;

  return 0;
}

/*.......................................................................
 * This is the entry-point of the logger thread.
 *
 * Input:
 *  arg          void *  A pointer to the Logger state object pointer,
 *                       cast to (void *).
 * Output:
 *  return       void *  NULL.
 */
CP_THREAD_FN(logger_thread)
{
  Logger *log = (Logger* )arg;   /* The state-object of the current thread */
  LoggerMessage msg;   /* An object for receiving messages from other threads */
/*
 * Arrange for the stdout and stderr output of this thread to be logged.
 */
  if(log_thread_stream(log->cp, stdout) ||
     log_thread_stream(log->cp, stderr)) {
    cp_report_exit(log->cp);
    return NULL;
  };
  
  // Wait for commands from other threads.

  sza::util::FdSet fdSet;
  fdSet.registerReadFd(log->pipe->readFd());
  
  while(select(fdSet.size(), fdSet.readFdSet(), NULL, NULL, NULL) > 0) {
    
    if(log->pipe->read(&msg, sizeof(msg), PIPE_WAIT) != PIPE_OK)
      break;
    
    // Interpret the message.

    switch(msg.type) {
    case LOG_SHUTDOWN:
      drain_log_pipe(log);
      close_logfile(log);
      cp_report_exit(log->cp);
      return NULL;
      break;
    case LOG_CHDIR:
      (void) chdir_logger(log, msg.body.chdir.dir);
      break;
    case LOG_OPEN:
      (void) open_logfile(log, msg.body.open.dir);
      break;
    case LOG_FLUSH:
      flush_logfile(log);
      break;
    case LOG_CLOSE:
      close_logfile(log);
      break;
    case LOG_MESSAGE:
      (void) dispatch_log_message(log, msg.body.message.text,
				  msg.body.message.nature,
				  msg.body.message.seq,
				  msg.body.message.end,
				  msg.body.message.interactive);
      break;
    case LOG_ADD_CLIENT:
      log_add_client(log, msg.body.client.pipe);
      break;
    case LOG_REM_CLIENT:
      log_rem_client(log, msg.body.client.pipe);
      break;
    case LOG_TRANS_CATALOG:
      if(msg.body.transCatalog.clear)
	log->transManager_->clearCatalog();
      try {
	log->transManager_->readCatalog("", msg.body.transCatalog.catalog);
      } catch(...) {
      }
      //  log->transManager_->printAll();
      break;
    case LOG_LOG_TRANS:
      log_log_transaction(log,
			  msg.body.logTrans.device, 
			  msg.body.logTrans.serial, 
			  msg.body.logTrans.location,
			  msg.body.logTrans.date,
			  msg.body.logTrans.who,
			  msg.body.logTrans.comment);
      break;
    case LOG_TRANS_EMAIL:
      logAddEmailAddress(log, msg.body.email.add, msg.body.email.address);
      break;
    default:
      lprintf(stderr, "logger_thread: Unknown command-type received.\n");
      break;
    };
  };
  fprintf(stderr, "Logger thread exiting after pipe read error.\n");
  cp_report_exit(log->cp);
  return NULL;
}

/*.......................................................................
 * Flush unwritten data from the stdio buffers to the current log file.
 * This should currently be redundant because line buffering is requested
 * when the file is opened.
 *
 * Input:
 *  log     Logger *   The state-object of the logger thread.
 */
static void flush_logfile(Logger *log)
{
  if(log->fp && fflush(log->fp)) {
    lprintf(stderr, "Error flushing log file: %s\n", log->path);
    close_logfile(log);
  };
}

/*.......................................................................
 * Close the current log file. Until a new log file is opened, subsequent
 * log messages will be discarded.
 *
 * Input:
 *  log     Logger *   The state-object of the logger thread.
 */
static void close_logfile(Logger *log)
{
  if(log->fp && fclose(log->fp))
    lprintf(stderr, "Error closing log file: %s\n", log->path);
  log->fp = NULL;
  if(log->path)
    free(log->path);
  log->path = NULL;
}

/*.......................................................................
 * Open a new log file in a given directory. If the file is opened
 * successfully, close the previous log file.
 *
 * Input:
 *  log     Logger *   The state-object of the logger thread.
 *  dir       char *   The name of the directory in which to create the
 *                     file or NULL or "" to use the last directory that
 *                     was specified.
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */
static int open_logfile(Logger *log, char *dir)
{
  char *path;                /* The path name of the file */
  FILE *fp;                  /* The file-pointer of the open file */
/*
 * Record a new log file directory?
 */
  if(dir && *dir!='\0')
    (void) chdir_logger(log, dir);
  else
    dir = log->dir;
/*
 * Compose the full pathname of the log file.
 */
  path = arc_path_name(dir, NULL, ARC_LOG_FILE);
  if(!path)
    return 1;
/*
 * Attempt to open the new log file.
 */
  fp = fopen(path, "w");
  if(!fp) {
    free(path);
    return 1;
  };
/*
 * It is important that the log file always shows the latest information,
 * so switch it to line buffering.
 */
  setvbuf(fp, NULL, _IOLBF, BUFSIZ);
/*
 * Close the current log file if one is open.
 */
  close_logfile(log);
/*
 * Install the new log file.
 */
  log->path = path;
  log->fp = fp;
  return 0;
}

/*.......................................................................
 * Change the directory in which subsequent log files will be written.
 *
 * Input:
 *  dir      char *   The directory to use when creating subsequent
 *                    log files.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
static int chdir_logger(Logger *log, char *dir)
{
/*
 * Make a copy of the name of the current log directory.
 */
  if(dir && dir != log->dir) {
    size_t bytes = strlen(dir)+1;
    char *tmp = (char* )(log->dir ? realloc(log->dir, bytes) : malloc(bytes));
    if(!tmp) {
      lprintf(stderr, "Unable to record new log-file directory.\n");
      return 1;
    } else {
      strcpy(tmp, dir);
      log->dir = tmp;
    };
  };
  return 0;
}

/*.......................................................................
 * Dispatch a log message to a connected control client.
 *
 * Input:
 *  log       Logger *  The state object of the logger thread.
 *  text        char *  The text of the message.
 *  nature LogStream    The disposition of the message:
 *                        LOG_STDOUT - An informational message.
 *                        LOG_STDERR - An error message.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static int write_logclient(Logger *log, char *text, LogStream nature, 
			   unsigned seq, bool isEnd, bool interactive)
{
  ListNode *node;        // One node of the list of control clients 
  CcPipeMsg pmsg;        // The message to queue to the client pipes 
  
  // Assemble the log message to be sent over the pipe.

  pmsg.id = CC_LOG_MSG;
  pmsg.msg.log.error = (nature != LOG_STDOUT);
  pmsg.msg.log.seq = seq;
  pmsg.msg.log.end = isEnd;
  pmsg.msg.log.interactive = interactive;
  strncpy(pmsg.msg.log.text, text, CC_MSG_MAX);

  pmsg.msg.log.text[CC_MSG_MAX] = '\0';
  
  // Send the message to each control client.

  for(node = log->clients->head; node; node=node->next) {
    queue_cc_message((sza::util::Pipe* )node->data, &pmsg);
  }

  return 0;
}

/*.......................................................................
 * Append a log message to the current log file, or to the controlling
 * terminal if no log file is currently open.
 *
 * Input:
 *  log       Logger *  The state object of the logger thread.
 *  text        char *  The text of the message.
 *  nature LogStream    The disposition of the message:
 *                        LOG_STDOUT - An informational message.
 *                        LOG_STDERR - An error message.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static int write_logfile(Logger *log, char *text, LogStream nature, 
			 unsigned seq, bool isEnd, bool interactive)
{
  
  // If no log file is open write the log message to the controlling
  // terminal of the program.

  if(!log->fp)
    return write_terminal(log, text, nature);
  
  // If the error indicator of the stream is set, clear it and attempt
  // to start a newline, to see if the causative condition (such as a
  // full disk) has been cleared. If this fails, attempt to log the
  // message to the controlling terminal of the program.

  if(ferror(log->fp)) {
    clearerr(log->fp);
    if(fputs("\nLog file restarted after error.\n", log->fp) == EOF)
      return write_terminal(log, text, nature);
  };
  
  // Write the time stamp, followed by the log message, followed by a
  // newline.

  if(fputs(text, log->fp) == EOF ||
     fputc('\n', log->fp) == EOF) {
    lprintf(stderr, "Error writing to log file.\n");
    return 1;
  };

  return 0;
}

/*.......................................................................
 * Write a log message to the appropriate stream of the controlling
 * terminal of the program.
 *
 * Input:
 *  log       Logger *  The state object of the logger thread.
 *  text        char *  The text of the message.
 *  nature LogStream    The disposition of the message:
 *                        LOG_STDOUT - An informational message.
 *                        LOG_STDERR - An error message.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static int write_terminal(Logger *log, char *text, LogStream nature)
{
/*
 * Get the stream to write to.
 */
  FILE *fp = (nature == LOG_STDOUT) ? stdout:stderr;
/*
 * Do nothing if the error indicator of the stream is set. This
 * usually means that the controlling terminal has exited.
 */
  if(ferror(fp))
    return 1;
/*
 * Write the time stamp, followed by the log message, followed by
 * a newline.
 */
  if(fputs(text, fp) == EOF ||
     fputc('\n', fp) == EOF) {
    lprintf(stderr, "Error writing to %s of szacontrol.\n",
	    fp==stdout ? "stdout":"stderr");
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Add a control-client reply pipe to the list of control-clients to
 * which error messages should be echoed.
 *
 * Input:
 *  log        Logger *   The state-object of the logger thread.
 *  client       Pipe *   The reply pipe of a control client.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int log_add_client(Logger *log, sza::util::Pipe *client)
{
  
  // Append the client pipe to the list of reply pipes.

  if(append_ListNode(log->clients, client) == NULL)
    return 1;

  return 0;
}

/*.......................................................................
 * Remove a given control-client reply pipe from the list of
 * control-clients to which error messages should be echoed.
 *
 * Input:
 *  log        Logger *   The state-object of the logger thread.
 *  client       Pipe *   The reply pipe of a control client.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int log_rem_client(Logger *log, sza::util::Pipe *client)
{
  ListNode *node;    /* A node of the list of connected clients */
  
  // Locate the specified client.

  for(node = log->clients->head; node; node=node->next) {
    if(node->data == (void *)client) {
      del_ListNode(log->clients, node, NULL);
      return 0;
    };
  };
  
  // Client not found.

  lprintf(stderr, "log_rem_client: Client not found.\n");

  return 1;
}

/*.......................................................................
 * This is a public function that diverts stdout or stderr output of the
 * calling thread to the logger thread, via a call to divert_lprintf().
 *
 * Input:
 *  cp   ControlProg *   The state-object of the control program.
 *  stream      FILE *   The stream to be logged. This must be
 *                       stdout or stderr.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
int log_thread_stream(ControlProg *cp, FILE *stream)
{
  if(!cp || (stream!=stdout && stream!=stderr)) {
    lprintf(stderr, "log_thread_stream: Invalid arguments.\n");
    return 1;
  };
  
  try {
    if(stream == stdout)
      sza::util::Logger::installLogHandler(stdoutLogger);
    else
      sza::util::Logger::installErrHandler(stderrLogger);
  } catch(...) {
    return 1;
  }

  return divert_lprintf(stream, lprintf_to_logger, cp, NULL, NULL);
}

/*.......................................................................
 * This is an lprintf() dispatch function that can be registered via
 * divert_lprintf() to turn stdout and/or stderr output into a call to
 * queue_logger_message().
 */
static LOG_DISPATCHER(lprintf_to_logger)
{
  ControlProg *cp = (ControlProg* )context;   // The resource object
					      // of the control
					      // program

  return sendLoggerMessage(cp, message, id==LOG_STDERR, false);
}

/*.......................................................................
 * When requested to shutdown the logger thread, the logger event loop
 * calls this function to read any unlogged messages from the logger
 * input pipe and write them to the log file. Other message types are
 * discarded.
 *
 * Input:
 *  log      Logger *  The logger resource object.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
static int drain_log_pipe(Logger *log)
{
  LoggerMessage msg;
  
  // Read messages from the logger input pipe until it is empty.

  while(log->pipe->read(&msg, sizeof(msg), PIPE_NOWAIT) == PIPE_OK) {

    // Write log messages to the log file.

    if(msg.type == LOG_MESSAGE) {
      (void) dispatch_log_message(log, msg.body.message.text,
				  msg.body.message.nature,
				  msg.body.message.seq,
				  msg.body.message.end,
				  msg.body.message.interactive);
    };
  };
  return 0;
}

/*.......................................................................
 * Dispatch a log message to log clients and the log file.
 *
 * Input:
 *  log       Logger *  The state object of the logger thread.
 *  text        char *  The text of the message.
 *  nature LogStream    The disposition of the message:
 *                        LOG_STDOUT - An informational message.
 *                        LOG_STDERR - An error message.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static int dispatch_log_message(Logger *log, char *text, LogStream nature, 
				unsigned seq, bool isEnd, bool interactive)
{
  Date utc;              /* The current value of utc */
  int nmsg;              /* The number of characters written to log->msg */
  static bool prependDate = true;
  static sza::util::LogMsgHandler handler;

  // Get a timestamp for the message.

  if(current_date(&utc))
    return 1;
  
  // If the date has changed since the last message, open a new log
  // file

  if(utc.year != log->year || utc.month != log->month || utc.day != log->day){

    // Record the date so that we know not to do this again. 

    log->year = utc.year;
    log->month = utc.month;
    log->day = utc.day;

    // If we are already logging open a new file in the current directory 

    if(log->fp)
      open_logfile(log, "");
  }
  
  if(prependDate) {
    // Start to construct the output log message by writing a timestamp
    // prefix.
    
    char date[100];
    sprintf(date, "%02d%02d%02d %02d:%02d:%02d %n",utc.year%100,
	    utc.month, utc.day, utc.hour, utc.min, utc.sec, &nmsg);
    
    handler.append(seq, date, nature);
  }

  // Append the log message.

  handler.appendWithSpace(seq, text, nature);
  
  // If this is the end of a message, iteratively send it until the
  // whole message has been processed

  if(isEnd) {

    bool isLast=false;

    do {

      // Get the next substring of this message and write it to
      // clients and the logfile

      std::string message = 
	handler.getNextMessageSubstr(seq, CC_MSG_MAX, isLast);
      
      // Dispatch the message to all connected log clients.
      
      (void) write_logclient(log, (char*)message.c_str(), nature, 
			     seq, isLast, interactive);
      
      // Write a copy of the message to the log file.
      
      (void) write_logfile(log, (char*)message.c_str(), nature, 
			   seq, isLast, interactive);
      
    } while(!isLast);

    prependDate = true;
  } else {
    prependDate = false;
  }

  return 0;
}

/*.......................................................................
 * Dispatch a date log-message to log clients and the log file.
 *
 * Input:
 *  log    Logger *  The state object of the logger thread.
 *  utc      Date *  The date to be logged (only the year,month,day
 *                   members will be used).
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static int log_date_message(Logger *log, Date *date)
{
  char date_message[80];  /* The message to be logged */
  char *mname;            /* The name of the current month */
  
  // Record the date so that we know not to log it again.

  log->year  = date->year;
  log->month = date->month;
  log->day   = date->day;
  
  // Get the abbreviated name of the current month.

  mname = (char*)name_of_month(date->month, 0, 1);
  if(!mname)
    return 1;
  
  // Construct the date message.

  sprintf(date_message, "Date: %02d-%s-%04d UTC", date->day, mname, date->year);
  
  // Log the message.

  return dispatch_log_message(log, date_message, LOG_STDOUT, 0, true, false);
}

/**.......................................................................
 * Define handlers for logging and error messages.
 */
static LOG_HANDLER_FN(stdoutLogger) {
  lprintf(stdout, logStr.c_str());
}

static LOG_HANDLER_FN(stderrLogger) {
  lprintf(stderr, logStr.c_str());
}

/*.......................................................................
 * Prepare a read-transaction-catalog message for subsequent transmission to
 * the logger thread.
 *
 * Input:
 *  msg  LoggerMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  path             char *  The full pathname of the file (Note
 *                           that pathname.h provides utilities for
 *                           composing/expanding pathnames). A copy
 *                           of this pathname will be made.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
int pack_logger_transaction_catalog(LoggerMessage *msg, char *path, bool clear)
{
  
  // Check arguments.

  if(!msg || !path) {
    lprintf(stderr, "pack_logger_transaction_catalog: NULL argument(s).\n");
    return 1;
  };
  msg->type = LOG_TRANS_CATALOG;
  strncpy(msg->body.transCatalog.catalog, path, CP_FILENAME_MAX);
  msg->body.transCatalog.catalog[CP_FILENAME_MAX] = '\0';
  msg->body.transCatalog.clear = clear;
  return 0;
}

/*.......................................................................
 * Prepare a log transaction message for subsequent transmission to
 * the logger thread.
 *
 * Input:
 *  msg  LoggerMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  path             char *  The full pathname of the file (Note
 *                           that pathname.h provides utilities for
 *                           composing/expanding pathnames). A copy
 *                           of this pathname will be made.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
int pack_logger_log_transaction(LoggerMessage *msg, char *device, char* serial,
				char* location, double date, char* who,
				char* comment)
{
  
  // Check arguments.

  if(!msg || !device || !serial || !location) {
    lprintf(stderr, "pack_logger_log_transaction: NULL argument(s).\n");
    return 1;
  };
  msg->type = LOG_LOG_TRANS;

  strncpy(msg->body.logTrans.device, device, TransactionManager::DEV_NAME_MAX);
  msg->body.logTrans.device[TransactionManager::DEV_NAME_MAX] = '\0';

  strncpy(msg->body.logTrans.serial, serial, TransactionManager::SERIAL_NAME_MAX);
  msg->body.logTrans.serial[TransactionManager::SERIAL_NAME_MAX] = '\0';

  strncpy(msg->body.logTrans.location, location, TransactionManager::LOCATION_NAME_MAX);
  msg->body.logTrans.location[TransactionManager::LOCATION_NAME_MAX] = '\0';

  msg->body.logTrans.date = date;

  strncpy(msg->body.logTrans.who, who, TransactionManager::WHO_NAME_MAX);
  msg->body.logTrans.who[TransactionManager::WHO_NAME_MAX] = '\0';

  strncpy(msg->body.logTrans.comment, comment, 
	  LOG_MSGLEN-TransactionManager::PREFIX_LEN);
  msg->body.logTrans.comment[LOG_MSGLEN-TransactionManager::PREFIX_LEN] = '\0';

  return 0;
}

/**.......................................................................
 * Return true if the named device is recognized.
 */
bool log_isValidDevice(Logger* log, char* device)
{
  return log->transManager_->isValidDevice(device);
}

/**.......................................................................
 * Return true if the named serial number is recognized for the given
 * device
 */
bool log_isValidSerialNumber(Logger* log, char* device, char* serial)
{
  return log->transManager_->isValidSerialNumber(device, serial);
}

/**.......................................................................
 * Return true if the named serial number is recognized for the given
 * device
 */
bool log_isValidLocation(Logger* log, char* device, char* location)
{
  return log->transManager_->isValidLocation(device, location);
}

/**.......................................................................
 * Log a transaction
 */
int log_log_transaction(Logger* log, char* device, char* serial, 
			char* location, double date, char* who, char* comment)
{
  // Check arguments

  if(!log->transManager_->isValidDevice(device)) {
    lprintf(stderr, "Invalid device: %s\n", device);
    return 1;
  }
  if(!log->transManager_->isValidSerialNumber(device, serial)) {
    lprintf(stderr, "Invalid serial number (%s) for device: %s\n", 
	    serial, device);
    return 1;
  }
  if(!log->transManager_->isValidLocation(device, location)) {
    lprintf(stderr, "Invalid location (%s) for device: %s\n", location, device);
    return 1;
  }

  // Compose a date

  OutputStream* stream=0;
  char dateString[DATE_LEN+1];    

  // Create an output stream.

  stream = new_OutputStream();

  if(stream==0) {
    lprintf(stderr, "Unable to allocate a new output stream\n");
    return 1;
  }
  
  // Connect the string to an output stream and compose the date in
  // it.

  if(open_StringOutputStream(stream, 1, dateString, DATE_LEN+1)) {
    lprintf(stderr, "Unable to connect input stream to string buffer\n");
    return 1;
  }

  // Compose the date

  if(output_utc(stream, "", 0, 0, date)) {
    lprintf(stderr, "Error in output_utc\n");
    return 1;
  }

  // Close the stream
    
  if(stream != 0)
    del_OutputStream(stream);

  // Compose a message

  std::ostringstream message;
  std::ostringstream email;

  message << "TRANSACTION Device: (" 
	  << device << ": " 
	  << serial << ") ("
	  << location << ") ("
	  << dateString << ") Logged by: ("
	  << who << ")" << ends;

  email   << "TRANSACTION Device: (" 
	  << device << ": " 
	  << serial << ") ("
	  << location << ") ("
	  << dateString << ") Logged by: ("
	  << who << ")" << "\n\n";

  unsigned waserr;
  waserr =  dispatch_log_message(log, (char*)message.str().c_str(), LOG_STDOUT, 0, true, false);

  // If a comment was included, log that too

  if(comment[0] != '\0') {

    message.str("");
    message << "TRANSACTION " << comment << ends;

    email << "TRANSACTION " << comment << "\"" << ends;

    waserr |=  
      dispatch_log_message(log, (char*)message.str().c_str(), LOG_STDOUT, 0, true, false);
  }

  // And send email to anyone registered to listen

  sendTransactionEmail(log, email);

  return waserr;
}

/*.......................................................................
 * Prepare an email message for subsequent transmission to the
 * logger thread.
 *
 * Input:
 *
 *    msg    LoggerMessage *  The message object to be prepared.
 *    dev    PagerDev       The device whose IP address we are changing
 *    ip     char *         The new IP address
 *
 * Output:
 *
 *   return  int            0 - OK.
 *                          1 - Error.
 */
int pack_logger_transaction_email(LoggerMessage *msg, bool add, char *email)
{
  size_t len;
  
  // Check arguments.

  if(!msg) {
    lprintf(stderr, "pack_transaction_email: NULL argument.\n");
    return 1;
  };

  msg->type = LOG_TRANS_EMAIL;
  msg->body.email.add = add;

  if(email) {
    len = strlen(email);

    if(len > CP_FILENAME_MAX) {
      lprintf(stderr,"Email address too long\n");
      return 1;
    }
    strncpy(msg->body.email.address, email, len);
    msg->body.email.address[len] = '\0';
  } else {

    // Don't allow addition of null addresses

    if(add) 
      ThrowError("No Email address specified");

    msg->body.email.address[0] = '\0';
  }
    
  return 0;
}

/*.......................................................................
 * Add/remove an email address from the transaction list
 *
 * Input:
 *  dev      char *   The device to use when creating subsequent
 *                    term files.
 * Output:
 *  return    int     0 - OK.
 *                    1 - Error.
 */
static int logAddEmailAddress(Logger *logger, bool add, char* email)
{
  if(add)
    logger->emails_->push_back(email);
  else {

    if(email==NULL || email[0]=='\0') {
      logger->emails_->clear();
      return 0;
    }

    std::vector<std::string>::iterator iString;

    for(iString=logger->emails_->begin(); iString != logger->emails_->end();
	iString++)
      if(*iString == email)
	break;
    if(iString != logger->emails_->end())
      logger->emails_->erase(iString);
  }

  return 0;
}

/**.......................................................................
 * Return a list of transaction email addresses.
 */
std::vector<std::string>* getTransactionEmailList(ControlProg *cp)
{
  return cp_Logger(cp)->emails_;
}

/*.......................................................................
 * Send a transaction message to a list of emails
 */
static int sendTransactionEmail(Logger *logger, std::ostringstream& message)
{
  int waserr=0;
  std::ostringstream os;

  for(unsigned i=0; i < logger->emails_->size(); i++) {
    os.str("");
    os << "echo \"" << message.str() << " | Mail -s \"SZA Transaction\" "
       << logger->emails_->at(i);
    system(os.str().c_str());
  }

  return waserr;
}

/*.......................................................................
 * Static function to send a message to the logger thread.  All
 * logging functions, including lprintf_to_logger now go through this
 * function so that a unique sequence number is generated for each
 * message sent to the logger thread, regardless of where it originates
 */
int sendLoggerMessage(ControlProg* cp, std::string logStr, bool isErr, bool interactive)
{
  static sza::util::LogMsgHandler handler;

  // Generate a unique sequence number for this message

  unsigned seq = handler.nextSeq();
  handler.append(seq, logStr);

  // Now iterate sending messages to the logger thread until the
  // message is complete

  std::string message;
  bool isLast = false;

  LoggerMessage logmsg;

  do {

    message = handler.getNextMessageSubstr(seq, NET_LOG_MAX, isLast);

    // To avoid deadlocks between threads, don't use blocking I/O to
    // write the message. If the logger pipe fills up, we'll simply lose
    // a log message or two. If this happens frequently, then there is
    // something seriously wrong with the scheduling of the control
    // program threads.

    if(pack_logger_message(&logmsg, (char*)message.c_str(),
			   isErr ? LOG_STDERR:LOG_STDOUT,
			   seq, isLast, interactive) ||
       send_LoggerMessage(cp, &logmsg, PIPE_NOWAIT) == PIPE_ERROR)
      return 1;

  } while(!isLast);

  return 0;
}

