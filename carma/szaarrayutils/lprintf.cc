/* System-specific includes */

#ifdef VXW
#include <vxWorks.h>
#include <fioLib.h>
#include <taskLib.h>
#include <taskVarLib.h>
#include <taskHookLib.h>
#elif _POSIX_C_SOURCE >= 199506L
#include <pthread.h>
#endif

/* Standard includes */

#include <stdlib.h>
#include <string.h>

/* Local includes */

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/print.h"

/*
 * The following type records the application-specified dispatcher
 * function and any external data that it needs. A persistent object
 * of this type is accessed through calls to get_StreamState().
 */
typedef struct {
  sza::array::LogStream id;             /* The stream id (LOG_STDOUT or LOG_STDERR) */
  LOG_DISPATCHER(*log_fn);  /* The function to call to dispatch messages */
  void *context;            /* Anonymous data to pass to log_fn() */
  char buff[LOG_MSGLEN+1];  /* The buffer in which messages are constructed */
  int nc;                   /* The length of the string in buff[] */
  int trunc;                /* True when the message has been truncated */
  std::ostringstream* os;
} StreamState;

static StreamState *get_StreamState(FILE *stream);
static void init_StreamState(sza::array::LogStream id, StreamState *ss);
static int dispatch_stream(StreamState *ss, int force);
static int dispatch_stream_old(StreamState *ss, int force);

typedef struct {
  StreamState err;          /* The target of stderr messages */
  StreamState out;          /* The target stream of stdout messages */
} LogState;

static LogState *new_LogState(void);
static LogState *del_LogState(LogState *ls);

/*
 * The function that writes into the buffer of a StreamState object.
 */
static PRT_OUT_FN(output_fn);
static PRT_OUT_FN(output_fn_old);

/*
 * In pthreads programs the following file-scope variables are used
 * to ensure that LogState objects are created for each thread. They
 * are used solely by get_StreamState().
 */
#if _POSIX_C_SOURCE >= 199506L
static pthread_mutex_t key_init_mutex = PTHREAD_MUTEX_INITIALIZER;
static int key_initialized = 0;        /* True after initialization of */
                                       /*  thread_log_state */
static pthread_key_t thread_log_state; /* pthread key of thread-specific */
                                       /*  LogState object */
static void free_LogState(void *ls);   /* thread_log_state destructor */

/* 
 * In VxWorks programs the following file-scope variables are used to
 * ensure that private LogState objects are created for each
 * task. They are used solely by get_StreamState() and
 * vw_install_lprintf().
 */
#elif VXW
static int vw_install_called = 0;   /* True after calling vw_install_lprintf */
static LogState *task_log_state = NULL; /* Task-specific LogState object */
static void vw_create_log_state(WIND_TCB *tcb); /* Create task_log_state */
static void vw_delete_log_state(WIND_TCB *tcb); /* Delete task_log_state */

/*
 * In non-threaded programs the following pointer contains the single
 * LogState object accessed by get_StreamState().
 */
#else
static LogState *global_log_state = NULL; /* Program-global log-state object */
#endif


/*.......................................................................
 * The public logging function. This is a plug in replacement for
 * fprintf(), useable for stdout and stderr.
 *
 * Input:
 *  stream    FILE *   Either stdout or stderr.
 *  fmt       char *   The printf format.
 *  ...                Arguments to associate with fmt.
 * Output:
 *  return     int     The number of characters written.
 */
int lprintf(FILE *stream, const char *fmt, ...)
{
  va_list ap;  /* The variadic argument list context */
  int nc;      /* The number of characters written */
/*
 * Initialize the variable-argument list for iteration.
 */
  va_start(ap, fmt);
  nc = vlprintf(stream, fmt, ap);
  va_end(ap);
  return nc;
}

/*.......................................................................
 * The public logging function. This is a plug in replacement for
 * vfprintf(), useable for stdout and stderr.
 *
 * Input:
 *  stream    FILE *   Either stdout or stderr.
 *  fmt       char *   The printf format.
 *  ap     va_list     Arguments to associate with fmt.
 * Output:
 *  return     int     The number of characters written.
 */
int vlprintf(FILE *stream, const char *fmt, va_list ap)
{
  StreamState *ss; /* The state object of the specified stream */
/*
 * Get the log context of the current thread.
 */
  ss = get_StreamState(stream);
/*
 * Logging disabled?
 */
  if(!ss || !ss->log_fn)
    return vfprintf(stream, fmt, ap);
/*
 * Parse the formatted message into ss->buff. As print_format() parses
 * each section of the format string it sends the resulting string
 * segment to output_fn(). output_fn() accumulates the result in the
 * ss->buff[] string, and keeps a tally of the number of characters
 * written, in ss->nc. Whenever a newline character is seen, ouput_fn()
 * passes the contents of ss->buff to the user-specified logging function.
 */
  return print_format(fmt, ap, output_fn, (PrintContext)ss);
}

/*.......................................................................
 * This function is called by print_format to accumulate lprintf()
 * output in the output buffer of the associated stream.  It
 * accumulates the formatted message parcelled out by one or more
 * calls to print_format(), taking care to truncate the string if it
 * exceeds the length of the buffer, and whenever a newline is seen,
 * it sends the accumulated characters to the user-specified dispatch
 * function.
 *
 * Input:
 *  buffer         char * The latest segment of the parsed format string.
 *  nc              int   The number of characters to take from buffer[].
 *  outarg PrintContext   The StreamState container that contains the output
 *                        buffer and logging function.
 * Output:
 *  return          int    0 - no error detected.
 *                        -1 - Error detected.
 */
static PRT_OUT_FN(output_fn)
{
  StreamState *ss = (StreamState *) context;

  int nnew;      /* The number of characters to write */
  int ndone=0;   /* The number of characters written so far */
  
  // If there are newline characters in buffer[] then we will need to
  // accumulate and dispatch the intervening string segments one at a
  // time. Loop until all characters in buffer have been consumed.

  while(ndone < nc) {
    int eos = 0;      /* The index of the end of the next string segment */
    
    // Find the index of the end of the next string segment to be
    // output.  If the remaining string contains a newline character,
    // end the string there.

    for(eos=ndone; eos<nc && buffer[eos] != '\n'; eos++)
      ;
    
    // How many characters should we attempt to copy?

    nnew = eos - ndone;
    
    // Add up to nnew characters to the buffer unless it is already
    // marked as full.

    if(!ss->trunc) {
      int ncopy;      /* The number of characters to copy */
      
      ncopy = nnew;
        
      // Copy the new characters into the message buffer.

      for(unsigned i=0; i < ncopy; i++) {
	*(ss->os) << *(buffer + ndone + i);
      }

      ss->nc += ncopy;
    };
    
    // Keep a record of how many characters have been consumed from
    // buffer[].

    ndone += nnew;
    
    // If a new line was seen, send the contents of the buffer to the
    // user-specified dispatch function and reset ss->nc to 0.

    if(eos < nc) {
      
      // Call the dispatch function.

      if(dispatch_stream(ss, 1))
	return -1;
      
      // Skip the newline.

      ndone++;
    };
  };
  return 0;
}

static PRT_OUT_FN(output_fn_old)
{
  StreamState *ss = (StreamState *) context;
  int nnew;      /* The number of characters to write */
  int ndone=0;   /* The number of characters written so far */
  
  // If there are newline characters in buffer[] then we will need to
  // accumulate and dispatch the intervening string segments one at a
  // time. Loop until all characters in buffer have been consumed.

  while(ndone < nc) {
    int eos = 0;      /* The index of the end of the next string segment */
    
    // Find the index of the end of the next string segment to be
    // output.  If the remaining string contains a newline character,
    // end the string there.

    for(eos=ndone; eos<nc && buffer[eos] != '\n'; eos++)
      ;
    
    // How many characters should we attempt to copy?

    nnew = eos - ndone;
    
    // Add up to nnew characters to the buffer unless it is already
    // marked as full.

    if(!ss->trunc) {
      int ncopy;      /* The number of characters to copy */
      
      // Truncate the new string to fit the bounds of the buffer?

      if(ss->nc + nnew > LOG_MSGLEN) {
	ncopy = LOG_MSGLEN - ss->nc;
	ss->trunc = 1;
      } else {
	ncopy = nnew;
      };
      
      // Copy the new characters into the message buffer.

      memcpy(ss->buff + ss->nc, buffer + ndone, ncopy);
      ss->nc += ncopy;
    };
    
    // Keep a record of how many characters have been consumed from
    // buffer[].

    ndone += nnew;
    
    // If a new line was seen, send the contents of the buffer to the
    // user-specified dispatch function and reset ss->nc to 0.

    if(eos < nc) {
      
      // Call the dispatch function.

      if(dispatch_stream(ss, 1))
	return -1;
      
      // Skip the newline.

      ndone++;
    };
  };
  return 0;
}

/*.......................................................................
 * This is a logging replacement for lputc().
 *
 * Input:
 *  c         int   The character to be passed to the specified stream.
 *  stream   FILE * The output stream.
 * Output:
 *  return    int   This is >= 0 if successful, EOF on error.
 */
int lputc(int c, FILE *stream)
{
  StreamState *ss; /* The state object of the specified stream */
/*
 * Get the log context of the current thread.
 */
  ss = get_StreamState(stream);
/*
 * Logging disabled?
 */
  if(!ss || !ss->log_fn) {
    return fputc(c, stream);
/*
 * If a newline is being appended, dispatch the completed line.
 */
  } else if(c=='\n') {
    return dispatch_stream(ss, 1) ? 0 : EOF;
/*
 * Skip the character if the line buffer is already full.
 */
  } else if(ss->trunc) {
    return 0;
/*
 * Truncate the string?
 */
  } else if(ss->nc + 1 > LOG_MSGLEN) {
    ss->trunc = 1;
/*
 * Append the character to the buffer.
 */
  } else {
    ss->buff[ss->nc++] = c;
  };
  return 0;
}

/*.......................................................................
 * This is a logging replacement for lputs().
 *
 * Input:
 *  s        char * The string to be output.
 *  stream   FILE * The output stream.
 * Output:
 *  return    int   This is >= 0 if successful, EOF on error.
 */
int lputs(const char *s, FILE *stream)
{
  StreamState *ss; /* The state object of the specified stream */
/*
 * Get the log context of the current thread.
 */
  ss = get_StreamState(stream);
/*
 * Logging disabled?
 */
  if(!ss || !ss->log_fn)
    return fputs(s, stream);
/*
 * Add the string to the stream buffer. Note that the (char *) cast
 * is there solely to suppress warnings about a (const char *) being
 * used as a (char *) argument. Output_fn treats the argument as
 * constant.
 */
  return output_fn((char *)s, strlen(s), (PrintContext)ss) ? EOF : 0;
}

/*.......................................................................
 * Implement the logging equivalent of fflush().
 *
 * Input:
 *  stream    FILE *  The stream to be flushed.
 * Output:
 *  return     int    0   - OK.
 *                    EOF - Error.
 */
int lflush(FILE *stream)
{
  StreamState *ss; /* The state object of the specified stream */
/*
 * Get the log context of the current thread.
 */
  ss = get_StreamState(stream);
/*
 * Logging disabled?
 */
  if(!ss || !ss->log_fn)
    return fflush(stream);
/*
 * Flush output from the stream buffer to the dispatch function.
 */
  return dispatch_stream(ss, 0) ? EOF : 0;
}

/*.......................................................................
 * Send a new line of lprintf() output to the user-specified dispatch
 * function of the given stream, then reset the stream buffer.
 *
 * Input:
 *  ss    StreamState *  The state object of the stream to be flushed.
 *  force         int    If ss->nc==0, dispatch_stream() won't do anything
 *                       unless force is true. If force is true, it will
 *                       dispatch an empty line.
 * Output:
 *  return        int    The return value of the user dispatch function.
 */
static int dispatch_stream(StreamState *ss, int force)
{
  int status = 0;  /* The return value the dispatch function */
  
  // Call the dispatch function?

  if((ss->nc > 0 || force) && ss->log_fn) {
    *(ss->os) << '\0';       /* Terminate the output string */
    status = ss->log_fn((char*)(ss->os->str().c_str()), ss->id, ss->context, 0, true);
  };
  
  // Reset the stream buffer.

  ss->os->str("");
  ss->nc = 0;
  ss->trunc = 0;
  return status;
}

static int dispatch_stream_old(StreamState *ss, int force)
{
  int status = 0;  /* The return value the dispatch function */
/*
 * Call the dispatch function?
 */
  if((ss->nc > 0 || force) && ss->log_fn) {
    ss->buff[ss->nc] = '\0';       /* Terminate the output string */
    status = ss->log_fn(ss->buff, ss->id, ss->context, 0, true);
  };
/*
 * Reset the stream buffer.
 */
  ss->nc = 0;
  ss->trunc = 0;
  return status;
}

#ifdef VXW
/*.......................................................................
 * Under VxWorks you must call vw_install_lprintf() once before creating
 * any other task that will use any of the functions in this file.
 *
 * Output:
 *  return   int   0 - OK.
 *                 1 - Initialization failed.
 */
int vw_install_lprintf(void)
{
/*
 * Already initialized?
 */
  if(vw_install_called) {
    lprintf(stderr, "Warning: Redundant call to vw_install_lprintf().\n");
    return 0;
  };
/*
 * Make sure that the task-variable facility is installed.
 */
  taskVarInit();
/*
 * Whenever a new task starts, arrange for the creation of a
 * task-specific version of the task_log_state variable.
 */
  if(taskCreateHookAdd((FUNCPTR) vw_create_log_state) == ERROR) {
    fprintf(stderr, "vw_install_lprintf: taskCreateHookAdd failed.\n");
    return 1;
  };
/*
 * Whenever a new task quits, arrange for the deletion of the
 * task-specific version of the task_log_state variable.
 */
  if(taskDeleteHookAdd((FUNCPTR) vw_delete_log_state) == ERROR) {
    fprintf(stderr, "vw_install_lprintf: taskDeleteHookAdd failed.\n");
    taskCreateHookDelete((FUNCPTR) vw_create_log_state);
    return 1;
  };
/*
 * Create the task_log_state variable of the calling task.
 */
  vw_create_log_state(taskTcb(0));
  vw_install_called = 1;
  return 0;
}

/*.......................................................................
 * Before unloading a module that contains lprintf.o, you should call this
 * function to remove the task-creation and deletion hooks that were
 * installed by vw_install_lprintf().
 *
 * Output:
 *  return   int   0 - OK.
 *                 1 - Removal failure.
 */
int vw_remove_lprintf(void)
{
/*
 * Not installed?
 */
  if(!vw_install_called)
    return 0;
/*
 * Unregister the task-variable creation function.
 */
  if(taskCreateHookDelete((FUNCPTR) vw_create_log_state) == ERROR) {
    fprintf(stderr, "vw_remove_lprintf: taskCreateHookDelete failed.\n");
    return 1;
  };
/*
 * Unregister the task-variable removal function.
 */
  if(taskDeleteHookDelete((FUNCPTR) vw_delete_log_state) == ERROR) {
    fprintf(stderr, "vw_remove_lprintf: taskDeleteHookDelete failed.\n");
    return 1;
  };
/*
 * Remove this task's log-state task variable.
 */
  vw_delete_log_state(taskTcb(0));
  vw_install_called = 0;
  return 0;
}

/*.......................................................................
 * This function is called whenever a new VxWorks task is created. It
 * creates a task-specific version of the file-scope log_state pointer
 * and initializes it to NULL.
 */
static void vw_create_log_state(WIND_TCB *tcb)
{
  int tid = (int) tcb;                  /* The task id */
  int *pvar = (int *) &task_log_state;  /* The task-variable identifier */
  taskVarAdd(tid, pvar);                /* Create the task-specific variable */
  taskVarSet(tid, pvar, 0);             /* Initialize the variable to NULL */
}

/*.......................................................................
 * This function is called whenever a new VxWorks task is deleted. It
 * deletes the task-specific log-state object (if allocated), and
 * removes the task-specific version of task_log_state from the
 * task control block.
 */
static void vw_delete_log_state(WIND_TCB *tcb)
{
  int tid = (int) tcb;                 /* The task id */
  int *pvar = (int *) &task_log_state; /* The task variable identifier */
  int value = taskVarGet(tid, pvar);   /* Get the task-variable value */
  if(value != ERROR) {
    del_LogState((LogState *)value);   /* Delete the log-state object */
    taskVarDelete(tid, pvar);          /* Discard the task variable */
  };
}
#endif

/*.......................................................................
 * Register a logging function to be used by subsequent calls to lprintf()
 * and vlprintf().
 *
 * Input:
 *  stream            FILE *   The stream to divert. This must be either
 *                             stdout or stderr.
 *  log_fn  LOG_DISPATCHER(*)  The function to pass messages to. To remove
 *                             a previously registered function without
 *                             providing a new one, send 0.
 *  context           void *   If log_fn() needs to be passed any external
 *                             data (to avoid globals), specify a pointer
 *                             to that data here.
 * Input/Output:
 *  old_fn  LOG_DISPATCHER(**) If both old_fn and old_context are non-NULL,
 *                             then any previous dspatch function and its
 *                             context object will be assigned to them.
 *  old_context       void **  (See old_fn).
 * Output:
 *  return             int     0 - OK.
 *                             1 - Error.
 */
int divert_lprintf(FILE *stream, LOG_DISPATCHER(*log_fn), void *context,
		   LOG_DISPATCHER(**old_fn), void **old_context)
{
  StreamState *ss;   /* The state object of the specified stream */
/*
 * Check the validity of the stream argument.
 */
  if(stream != stdout && stream != stderr) {
    fprintf(stderr, "divert_lprintf: Invalid stream.\n");
    return 1;
  };
/*
 * Get the state object of the specified stream.
 */
  ss = get_StreamState(stream);
  if(!ss)
    return 1;
/*
 * Flush the stream before diverting it.
 */
  (void) dispatch_stream(ss, 0);
/*
 * Return the previous dispatch function and its callback data?
 */
  if(old_fn && old_context) {
    *old_fn = ss->log_fn;
    *old_context = ss->context;
  };
/*
 * Record the new dispatch function.
 */
  ss->log_fn = log_fn;
  ss->context = context;
  return 0;
}

/*.......................................................................
 * Get the log state object of the current thread.
 *
 * Input:
 *  stream       FILE *  The target stream.
 * Output:
 *  return   LogState *  The lprintf() state object of the current thread,
 *                       or NULL if not available. In the latter case
 *                       vfprintf() should be used directly.
 */
static StreamState *get_StreamState(FILE *stream)
{
  LogState *state;   /* The object that contains the stream states */
/*
 * If the state object of the current thread hasn't been created yet,
 * do so and record its pointer in 'state'.
 */

/*
 * The following function gets the thread-specific state object
 * in a pthreads program.
 */
#if _POSIX_C_SOURCE >= 199506L
/*
 * Perform one-time initialization of the thread-specific log-state key.
 */
  if(!key_initialized) {
    int status = 0;
    pthread_mutex_lock(&key_init_mutex);
    if(!key_initialized) {
      status = pthread_key_create(&thread_log_state, free_LogState);
      if(status)
	fprintf(stderr, "get_StreamState: pthread_key_create failed.\n");
      else
	key_initialized = 1;
    };
    pthread_mutex_unlock(&key_init_mutex);
    if(status)
      return NULL;
  };
/*
 * Get the thread-specific state object.
 */
  state = (LogState* )pthread_getspecific(thread_log_state);
  if(!state) {
    state = new_LogState();
    if(state && pthread_setspecific(thread_log_state, state)) {
      fprintf(stderr, "get_LogState: Error setting lprintf state object.\n");
      return NULL;
    };
  };
/*
 * Get the task-variable state object in a VxWorks program.
 */
#elif VXW
  if(!vw_install_called) {
    fprintf(stderr,
	    "ERROR: lprintf functions called before vw_install_lprintf().\n");
    return NULL;
  };
  if(!task_log_state)
    task_log_state = new_LogState();
  state = task_log_state;

/*
 * Get the state object of a unthreaded program.
 */
#else
  if(!global_log_state)
    global_log_state = new_LogState();
  state = global_log_state;
#endif

/*
 * Return the stream-state object.
 */
  if(state) {
    if(stream == stdout)
      return &state->out;
    else if(stream == stderr)
      return &state->err;
  };
  return NULL;
}

/*.......................................................................
 * Create a new LogState object.
 *
 * Output:
 *  return  LogState *  The new object, or NULL on error.
 */
static LogState *new_LogState()
{
  LogState *ls;  /* The object to be returned */
/*
 * Allocate the container.
 */
  ls = (LogState* )malloc(sizeof(LogState));
  if(!ls) {
    lprintf(stderr, "new_LogState: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can safely be passed
 * to del_LogState().
 */
  init_StreamState(sza::array::LOG_STDERR, &ls->err);
  init_StreamState(sza::array::LOG_STDOUT, &ls->out);
  return ls;
}

/*.......................................................................
 * This is a private function of new_LogState() called to initialize
 * a StreamState object.
 *
 * Input:
 *  id      LogStream    The stream associated with the StreamState object.
 *  ss    StreamState *  The object to be initialized.
 */
static void init_StreamState(sza::array::LogStream id, StreamState *ss)
{
  ss->id = id;
  ss->log_fn = 0;
  ss->context = NULL;
  ss->buff[0] = '\0';
  ss->nc = 0;
  ss->trunc = 0;
  ss->os = new std::ostringstream();
}

/*.......................................................................
 * Delete a LogState object.
 *
 * Input:
 *  ls     LogState *  The object to be deleted.
 * Output:
 *  return LogState *  The deleted object (always NULL).
 */
static LogState *del_LogState(LogState *ls)
{
  if(ls) {

    if(ls->out.os)
      delete ls->out.os;
    if(ls->err.os)
      delete ls->err.os;

    free(ls);
  };
  return NULL;
}

/*.......................................................................
 * Create a wrapper around del_LogState() for use as a pthread key
 * value destructor function.
 */
#if _POSIX_C_SOURCE >= 199506L
static void free_LogState(void *ls)
{
  del_LogState((LogState* )ls);
}
#endif

