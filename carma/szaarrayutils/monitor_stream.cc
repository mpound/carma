#include <stdlib.h>

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/monitor_stream.h"

#include "carma/szautil/CoordRange.h"
#include "carma/szautil/DataType.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/RegCal.h"
#include "carma/szautil/RegAxisRange.h"

using namespace sza::array;

/*
 * List registers that must be selected at all times.
 */
typedef struct {
  char *regmap_name;     // The name of the register map that contains
			 // the register
  char *brd_name;        /* The name of the board that contains the register */
  char *blk_name;        /* The name of the block that contains the register */
  int base;              /* The index of the first element required */
  int dim;               /* The number of elements required, or 0 to select */
                         /*  all elements from base onwards */
} MandReg;

static MandReg mandatory_regs[] = {
  {"array", "frame", "nsnap", 0, 0}, /* Needed by calibrate_regdata */
};

static int ms_change_regmap(MonitorStream *ms);
static int bad_regmap_change(MonitorStream *ms);

/*.......................................................................
 * Read all or part of the next register frame from a monitor stream.
 * Whenever a complete frame has been read, apply the current calibrations
 * from ms->regcal.
 *
 * Input:
 *  ms     MonitorStream *  The stream to read from.
 *  dowait           int    Whether to block until the transaction is
 *                          complete.
 *                            0 - If the input stream temporarily
 *                                blocks while reading, defer completion
 *                                of the transaction to a later call to
 *                                this function.
 *                            1 - Wait for the transaction to complete
 *                                before returning.
 * Output:
 *  return   MsReadState    The status of the operation, from:
 *                            MS_READ_ENDED  - The end of the stream
 *                                             was reached before reading
 *                                             another register frame.
 *                            MS_READ_AGAIN  - Call this function again
 *                                             to complete the transaction.
 *                                             This implies dowait=0.
 *                            MS_READ_BREAK  - The end of one part of a
 *                                             multi-segment stream has been
 *                                             reached. On the next call the
 *                                             file descriptor and register
 *                                             map may change.
 *                            MS_READ_REGMAP - A new register map was read
 *                                             and all register-map dependent
 *                                             objects have been reallocated.
 *                            MS_READ_DONE   - The transaction completed.
 *                                             The calibrated registers
 *                                             can be found in ms->cal.
 */
MsReadState ms_read_frame(MonitorStream *ms, int dowait)
{
  MsReadState status;  /* The status returned by read_fn() */
  
  // Check the arguments.
  
  if(!ms || !ms->context) {
    lprintf(stderr, "ms_read_frame: No data-source has been specified.\n");
    return MS_READ_ENDED;
  };
  
  // Attempt to read the next frame.
  
  status = ms->read_fn(ms, dowait);
  switch(status) {
  case MS_READ_ENDED:
  case MS_READ_BREAK:
  case MS_READ_AGAIN:
    return status;
    break;
  case MS_READ_REGMAP:
    return ms_change_regmap(ms) ? MS_READ_ENDED : MS_READ_REGMAP;
    break;
  case MS_READ_DONE:
    
    // Discard incoming frames while configuration messages are being
    // sent.
    
    if(ms->sending)
      return MS_READ_AGAIN;
    
    // Once acquired, calibrate the new register values.

    try {
      ms->regCal->calibrateRegData(ms->regSet, ms->raw);
    } catch(sza::util::Exception& err) {
      std::cout << err.what() << std::endl;
      return MS_READ_ENDED;
    }

    return MS_READ_DONE;
  };
  return MS_READ_ENDED;
}

/*.......................................................................
 * Count register frames from a monitor stream.
 *
 * Input:
 *  ms     MonitorStream *  The stream to read from.
 *  dowait           int    Whether to block until the transaction is
 *                          complete.
 *                            0 - If the input stream temporarily
 *                                blocks while reading, defer completion
 *                                of the transaction to a later call to
 *                                this function.
 *                            1 - Wait for the transaction to complete
 *                                before returning.
 * Output:
 *  return   MsReadState    The status of the operation, from:
 *                            MS_READ_ENDED  - The end of the stream
 *                                             was reached before reading
 *                                             another register frame.
 *                            MS_READ_AGAIN  - Call this function again
 *                                             to complete the transaction.
 *                                             This implies dowait=0.
 *                            MS_READ_BREAK  - The end of one part of a
 *                                             multi-segment stream has been
 *                                             reached. On the next call the
 *                                             file descriptor and register
 *                                             map may change.
 *                            MS_READ_REGMAP - A new register map was read
 *                                             and all register-map dependent
 *                                             objects have been reallocated.
 *                            MS_READ_DONE   - The transaction completed.
 *                                             The calibrated registers
 *                                             can be found in ms->cal.
 */
MsReadState ms_count_frame(MonitorStream *ms, int dowait)
{
  MsReadState status;  /* The status returned by read_fn() */
  
  // Check the arguments.
  
  if(!ms || !ms->context) {
    lprintf(stderr, "ms_read_frame: No data-source has been specified.\n");
    return MS_READ_ENDED;
  };
  
  // Attempt to read the next frame.
  
  status = ms->read_fn(ms, dowait);
  switch(status) {
  case MS_READ_ENDED:
  case MS_READ_BREAK:
  case MS_READ_AGAIN:
    return status;
    break;
  case MS_READ_REGMAP:
    return ms_change_regmap(ms) ? MS_READ_ENDED : MS_READ_REGMAP;
    break;
  case MS_READ_DONE:
    
    // Discard incoming frames while configuration messages are being
    // sent.
    
    if(ms->sending)
      return MS_READ_AGAIN;

    return MS_READ_DONE;
  };
  return MS_READ_ENDED;
}


/*.......................................................................
 * Whenever the stream source indicates that the register map has changed
 * we must replace all objects that were tied to the previous
 * register map.
 *
 * Input:
 *  ms     MonitorStream *  The stream to be reconfigured.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error - the stream is now closed.
 */
static int ms_change_regmap(MonitorStream *ms)
{
  // First discard any redundant structures.
  
  ms->arraymap    = NULL;
  
  if(ms->prepRegSet != 0) {
    delete ms->prepRegSet;
    ms->prepRegSet = 0;
  }
  
  if(ms->regSet != 0) {
    delete ms->regSet;
    ms->regSet = 0;
  }
  
  if(ms->regCal != 0) {
    delete ms->regCal;
    ms->regCal = 0;
  }
  
  ms->raw = del_RegRawData(ms->raw);
  
  // Get the new register map.

  ms->arraymap = ms->arraymap_fn(ms);

  // Allocate two register sets. One will be used by the user of the
  // iterator to construct new register sets. The other will be used
  // to record the register set that has been established with the
  // supplier of register frames.
  
  ms->prepRegSet = new sza::util::RegisterSet(ms->arraymap, ms->archivedOnly_);
  if(ms->prepRegSet==0)
    return bad_regmap_change(ms);
  
  ms->regSet = new sza::util::RegisterSet(ms->arraymap, ms->archivedOnly_);
  if(ms->regSet==0)
    return bad_regmap_change(ms);
  
  // Allocate the register calibration object.
  
  ms->regCal = new sza::util::RegCal(ms->arraymap, ms->archivedOnly_);
  if(ms->regCal==0)
    return bad_regmap_change(ms);
  
  // Allocate an object to contain an uncalibrated register frame.

  ms->raw = new_RegRawData(ms->arraymap, ms->archivedOnly_);
  if(!ms->raw)
    return bad_regmap_change(ms);
  
  return 0;
}

/*.......................................................................
 * This is a private error cleanup function of ms_change_regmap(). It
 * closes the stream and returns the error code of ms_change_regmap().
 */
static int bad_regmap_change(MonitorStream *ms)
{
  close_MonitorStream(ms);
  return 1;
}


/*.......................................................................
 * Attempt to complete a previously arranged send operation (see
 * ms_queue_regset().
 *
 * Input:
 *  ms     MonitorStream *  The stream to send on.
 *  dowait           int    Whether to block until the transaction is
 *                          complete.
 *                            0 - If the output stream temporarily
 *                                blocks while writing, defer completion
 *                                of the transaction to a later call to
 *                                this function.
 *                            1 - Wait for the transaction to complete
 *                                before returning.
 * Output:
 *  return   MsSendState    The status of the operation, from:
 *                            MS_SEND_ERROR - The transaction failed.
 *                            MS_SEND_AGAIN - Call this function again
 *                                            to complete the transaction.
 *                                            This implies dowait=0.
 *                            MS_SEND_DONE  - The transaction completed.
 */
MsSendState ms_send_msg(MonitorStream *ms, int dowait)
{
  MsSendState status;   // The status of the transaction 
  
  // Check the arguments.
  
  if(!ms || !ms->context) {
    lprintf(stderr, "ms_send_msg: No data-source has been specified.\n");
    return MS_SEND_ERROR;
  };
  
  // Delegate the send operation to the stream-specific method.
  
  status = ms->send_fn ? ms->send_fn(ms, dowait) : MS_SEND_DONE;
  
  // Did the send complete?
  
  if(status == MS_SEND_DONE) {
    ms->sending = 0;
    
    // Has an udpate of the interval been queued?
    
    if(ms->send_interval) {
      ms->send_interval = 0;
      status = ms_queue_interval(ms, ms->interval);
      if(status == MS_SEND_AGAIN)
	return ms_send_msg(ms, dowait);
    };
    
    // Has an udpate of the register set been queued?
    
    if(ms->send_regset) {
      ms->send_regset = 0;
      status = ms_queue_regset(ms);
      if(status == MS_SEND_AGAIN)
	return ms_send_msg(ms, dowait);
    };
    
    // Has a rewind message been queued?
    
    if(ms->send_rewind) {
      ms->send_rewind = 0;
      status = ms_queue_rewind(ms);
      if(status == MS_SEND_AGAIN)
	return ms_send_msg(ms, dowait);
    };
  };
  return status;
}

/*.......................................................................
 * Install the contents of the preparation register set ms->prep_regset
 * as the register set to be used henceforth to select which
 * registers to read and calibrate. If the supplier of the registers
 * is at the other end of the stream, this will also involve packing
 * the register set for subsequent transmission to the supplier.
 * If so MS_SEND_AGAIN will be returned to indicate that ms_send_msg()
 * should be called to send the message to the supplier.
 *
 * Input:
 *  ms     MonitorStream *  The stream to modify.
 * Output:
 *  return   MsSendState    The status of the transaction, from:
 *                           MS_SEND_ERROR - The transaction failed.
 *                           MS_SEND_AGAIN - Call ms_send_msg() to
 *                                           complete the transaction.
 *                           MS_SEND_DONE  - The transaction is complete.
 */
MsSendState ms_queue_regset(MonitorStream *ms)
{
  MsSendState status;  // The return value of the regset_fn() method 
  int i;
  
  // Check that the stream is open.
  
  if(!ms || !ms->context) {
    lprintf(stderr, "ms_queue_regset: No data-source has been specified.\n");
    return MS_SEND_ERROR;
  };
  
  // Postpone the update if another output transaction is in progress.
  
  if(ms->sending) {
    ms->send_regset = 1;
    return MS_SEND_AGAIN;
  };
  
  // Add the mandatory registers to the preparation register set.
  
  for(i=0; i < (int)(sizeof(mandatory_regs)/sizeof(mandatory_regs[0])); i++) {

    MandReg *range = mandatory_regs + i;

    // The description of the register 

    sza::util::RegDescription reg(ms->prepRegSet->archivedOnly(), ms->arraymap);
    
    // Find the register.
    
    try {
      reg.setTo(range->regmap_name, range->brd_name, 
		range->blk_name, REG_PLAIN, REG_INT_PLAIN);
    } catch(...) {
      return MS_SEND_ERROR;
    }
    
    // Insert it in the register selection set.
    
    try {
      ms->prepRegSet->addRegister(reg);
    } catch(...) {
      return MS_SEND_ERROR;
    }
  };
  
  // If the new register set is the same as the currently established
  // set then no further work is required.
  
  if(*ms->prepRegSet == *ms->regSet)
    return MS_SEND_DONE;
  
  // Install the register set by copying its contents.
  
  if(dup_RegSet(ms->regSet->regSet(), ms->prepRegSet->regSet()))
    return MS_SEND_ERROR;
  
  // Invoke the optional stream-specific method to establish the
  // register set with the supplier.
  
  status = ms->regset_fn ? ms->regset_fn(ms) : MS_SEND_DONE;
  
  // If the transaction is incomplete, flag the outbound channel as
  // busy.
  
  if(status != MS_SEND_DONE)
    ms->sending = 1;
  
  // Return the transaction status.
  
  return status;
}

/*.......................................................................
 * Change the sampling interval. If the supplier of the registers
 * is at the other end of the stream, this will also involve packing
 * the interval record for subsequent transmission to the supplier.
 * If so MS_SEND_AGAIN will be returned to indicate that ms_send_msg()
 * should be called to send the message to the supplier.
 *
 * Input:
 *  ms     MonitorStream *  The stream to modify.
 *  interval    unsigned    The new sampling interval.
 * Output:
 *  return   MsSendState    The status of the transaction, from:
 *                           MS_SEND_ERROR - The transaction failed.
 *                           MS_SEND_AGAIN - Call ms_send_msg() to
 *                                           complete the transaction.
 *                           MS_SEND_DONE  - The transaction is complete.
 */
MsSendState ms_queue_interval(MonitorStream *ms, unsigned interval)
{
  MsSendState status;  /* The return status of the interval_fn() method */
  /*
   * Check that the stream is open.
   */
  if(!ms || !ms->context) {
    lprintf(stderr, "ms_queue_interval: No data-source has been specified.\n");
    return MS_SEND_ERROR;
  };
  /*
   * Install the new interval locally.
   */
  ms->interval = interval > 0 ? interval : 1;
  /*
   * Postpone sending the update if another output transaction is in progress.
   */
  if(ms->sending) {
    ms->send_interval = 1;
    return MS_SEND_AGAIN;
  };
  /*
   * Invoke the optional stream-specific method.
   */
  status = ms->interval_fn ? ms->interval_fn(ms) : MS_SEND_DONE;
  /*
   * If the transaction is incomplete, flag the outbound channel
   * as busy.
   */
  if(status != MS_SEND_DONE)
    ms->sending = 1;
  /*
   * Return the transaction status.
   */
  return status;
}

/*.......................................................................
 * Request that the stream be rewound. If the supplier of the registers
 * is at the other end of the stream, this will also involve packing
 * the interval record for subsequent transmission to the supplier.
 * If so MS_SEND_AGAIN will be returned to indicate that ms_send_msg()
 * should be called to send the message to the supplier.
 *
 * Input:
 *  ms     MonitorStream *  The stream to modify.
 * Output:
 *  return   MsSendState    The status of the transaction, from:
 *                           MS_SEND_ERROR - The transaction failed.
 *                           MS_SEND_AGAIN - Call ms_send_msg() to
 *                                           complete the transaction.
 *                           MS_SEND_DONE  - The transaction is complete.
 */
MsSendState ms_queue_rewind(MonitorStream *ms)
{
  MsSendState status;  /* The return status of the interval_fn() method */
  /*
   * Check that the stream is open.
   */
  if(!ms || !ms->context) {
    lprintf(stderr, "ms_queue_rewind: No data-source has been specified.\n");
    return MS_SEND_ERROR;
  };
  /*
   * Postpone sending the update if another output transaction is in progress.
   */
  if(ms->sending) {
    ms->send_rewind = 1;
    return MS_SEND_AGAIN;
  };
  /*
   * Invoke the optional stream-specific method.
   */
  status = ms->rewind_fn ? ms->rewind_fn(ms) : MS_SEND_DONE;
  /*
   * If the transaction is incomplete, flag the outbound channel
   * as busy.
   */
  if(status != MS_SEND_DONE)
    ms->sending = 1;
  /*
   * Return the transaction status.
   */
  return status;
}

/*.......................................................................
 * Return true if the data source of a given stream can be rewound.
 * If false, calls to ms_queue_rewind() will do nothing more than return
 * MS_SEND_DONE.
 *
 * Input:
 *  ms     MonitorStream *  The stream to query.
 * Output:
 *  return           int    0 - Not rewindable.
 *                          1 - Rewindable.
 */
int ms_can_be_rewound(MonitorStream *ms)
{
  return ms && ms->context && ms->rewind_fn != 0;
}

/*.......................................................................
 * Return the current file descriptor of the data-source. This must be
 * suitable for use in select().
 *
 * Input:
 *  ms     MonitorStream *  The stream to query.
 * Output:
 *  return           int    The file descriptor, or -1 if the connection
 *                          is closed.
 */
int ms_select_fd(MonitorStream *ms)
{
  /*
   * Query the fd via the stream-specific method.
   */
  return (ms && ms->context) ? ms->fd_fn(ms) : -1;
}

/*.......................................................................
 * Update the calibration object of the monitor stream from a given
 * text calibration file.
 *
 * Input:
 *  ms    MonitorStream *   The monitor stream object to modify.
 *  dir            char *   The directory containing the cal file (or
 *                          "" if the full name is given in name[]).
 *  name           char *   The name of the cal file.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
int ms_load_cal_file(MonitorStream *ms, char *dir, char *name)
{
  if(!ms || !dir || !name) {
    lprintf(stderr, "ms_load_cal_file: NULL argument.\n");
    return 1;
  };
  try {
    ms->regCal->loadCalFile(dir, name);
  } catch(sza::util::Exception& err) {
    lprintf(stderr, "%s\n", err.what());
    return 1;
  }
  return 0;
}

/*.......................................................................
 * Update the calibration object of the monitor stream from a given
 * text input stream.
 *
 * Input:
 *  ms    MonitorStream *   The monitor stream object to modify.
 *  stream  InputStream *   The input text stream to read the 
 *                          calibration from.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
int ms_load_cal_stream(MonitorStream *ms, InputStream *stream)
{
  if(!ms || !stream) {
    lprintf(stderr, "ms_load_cal_stream: NULL argument.\n");
    return 1;
  };
  
  try {
    ms->regCal->loadCalStream(stream);
  } catch(...) {
    return 1;
  }
  return 0;
}

/*.......................................................................
 * Reset all register calibrations to unit scale factor and zero offset.
 *
 * Input:
 *  ms    MonitorStream *   The monitor stream object to modify.
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.
 */
int ms_reset_cal(MonitorStream *ms)
{
  if(!ms) {
    lprintf(stderr, "ms_reset_cal: NULL argument.\n");
    return 1;
  };
  
  try {
    ms->regCal->reset();
  } catch(...) {
    return 1;
  }
  return 0;
}

/*.......................................................................
 * Create a generic iterator for use in reading monitor data from
 * varied sources. The iterator must be connected to a specific data
 * source via a call to open_MonitorStream(), and can thereafter be
 * reattached to a new stream by further calls to open_MonitorStream(),
 * or explicitly detached via a call to close_MonitorStream().
 *
 * Output:
 *  return   MonitorStream *  The new stream iterator.
 */
MonitorStream *new_MonitorStream(bool archivedOnly)
{
  MonitorStream *ms;  /* The new iterator */
  /*
   * Allocate the container.
   */
  ms = (MonitorStream *) malloc(sizeof(MonitorStream));
  if(!ms) {
    lprintf(stderr, "new_MonitorStream: Insufficient memory.\n");
    return NULL;
  };
  /*
   * Before attempting any operation that might fail, initialize the
   * container at least up to the point at which it can safely be passed
   * to del_MonitorStream().
   */
  ms->context = NULL;
  ms->read_fn = 0;
  ms->send_fn = 0;
  ms->regset_fn = 0;
  ms->interval_fn = 0;
  ms->rewind_fn = 0;
  ms->del_fn = 0;
  ms->arraymap = NULL;
  ms->prepRegSet = NULL;
  ms->regSet = NULL;
  ms->regCal = NULL;
  ms->raw = NULL;
  ms->interval = 1;
  ms->sending = 0;
  ms->sending = 0;
  ms->send_interval = 0;
  ms->send_regset = 0;
  ms->send_rewind = 0;
  ms->archivedOnly_ = archivedOnly;
  return ms;
}

/*.......................................................................
 * Delete a monitor-stream iterator. If a data-source is currently
 * open, close_MonitorStream() will be called first.
 *
 * Input:
 *  ms        MonitorStream *  The iterator to be deleted.
 * Output:
 *  return    MonitorStream *  The deleted iterator (always NULL).
 */
MonitorStream *del_MonitorStream(MonitorStream *ms)
{
  if(ms) {
    if(ms->context)
      close_MonitorStream(ms);
    free(ms);
  };
  return NULL;
}

/*.......................................................................
 * Connect a monitor stream to a specific source of monitor data. Any
 * pre-existing connection will first be terminated via a call to
 * close_MonitorStream(ms).
 *
 * Input:
 *  ms            MonitorStream *  The stream to connect the data source to.
 *  context                void *  Any source-specific context.
 *  del_fn        MS_DESTRUCTOR(*) The function to call to delete 'context'.
 *  read_fn       MS_READ_FRAME(*) The function to call to incrementally
 *                                 read some or all of the next frame of
 *                                 selected registers.
 *  send_fn         MS_SEND_MSG(*) If the supplier is a remote entity that
 *                                 needs to have the values that are passed
 *                                 via regset_fn and interval_fn
 *                                 incrementally sent to it, provide a
 *                                 function here that can perform the
 *                                 incremental sends. It should return
 *                                 MS_SEND_DONE when complete. Otherwise
 *                                 pass 0 here.
 *  regset_fn   MS_QUEUE_REGSET(*) The function to call to establish a new
 *                                 selection of registers to be read. This
 *                                 is optional and should be specified as 0
 *                                 if not required.
 *  rewind_fn   MS_QUEUE_REWIND(*) The optional function to call to request
 *                                 that the stream be rewound. Send 0 if
 *                                 not pertinent.
 *  interval_fn MS_QUEUE_INTERVAL(*) The function to call to establish a new
 *                                 subsampling frame interval. This is
 *                                 optional.
 * Output:
 *  return                  int    0 - OK.
 *                                 1 - Error (Note that del_fn(context) will
 *                                     have been invoked, so *context will
 *                                     no longer exist).
 */
int open_MonitorStream(MonitorStream *ms, void *context, MS_DESTRUCTOR(*del_fn),
		       MS_READ_FRAME(*read_fn), MS_SEND_MSG(*send_fn),
		       MS_QUEUE_REGSET(*regset_fn),
		       MS_QUEUE_INTERVAL(*interval_fn),
		       MS_QUEUE_REWIND(*rewind_fn),
		       MS_SELECT_FD(*fd_fn),
		       MS_ARRAYMAP(*arraymap_fn),
		       bool archivedOnly)
{
  /*
   * Close any existing data-source.
   */
  if(ms)
    close_MonitorStream(ms);
  /*
   * Check the input arguments.
   */
  if(!ms || !context || !del_fn || !read_fn || !fd_fn|| !arraymap_fn) {
    lprintf(stderr, "open_MonitorStream: Bad argument(s).\n");
    if(context && del_fn)
      context = del_fn(context);
    return 1;
  };
  /*
   * Record the input parameters.
   */
  ms->context = context;
  ms->del_fn = del_fn;
  ms->read_fn = read_fn;
  ms->send_fn = send_fn;
  ms->regset_fn = regset_fn;
  ms->interval_fn = interval_fn;
  ms->rewind_fn = rewind_fn;
  ms->arraymap_fn = arraymap_fn;
  ms->fd_fn = fd_fn;
  ms->arraymap = NULL;
  ms->interval = 1;
  ms->archivedOnly_ = archivedOnly;

  /*
   * Get the new register map and allocate register-map dependent objects.
   * On error this function closes the monitor stream and
   * returns 1.
   */
  if(ms_change_regmap(ms))
    return 1;
  return 0;
}

/*.......................................................................
 * Close a previously opened monitor-stream data-source.
 *
 * Input:
 *  ms     MonitorStream *   The stream to be detached from its current
 *                           data-source.
 */
void close_MonitorStream(MonitorStream *ms)
{
  if(ms) {
    if(ms->context)
      ms->context = ms->del_fn(ms->context);
    ms->del_fn = 0;
    ms->read_fn = 0;
    ms->send_fn = 0;
    ms->regset_fn = 0;
    ms->interval_fn = 0;
    ms->rewind_fn = 0;
    ms->arraymap = NULL;
    
    if(ms->prepRegSet != 0) {
      delete ms->prepRegSet;
      ms->prepRegSet = 0;
    }
    
    if(ms->regSet != 0) {
      delete ms->regSet;
      ms->regSet = 0;
    }
    
    if(ms->regCal != 0) {
      delete ms->regCal;
      ms->regCal = 0;
    }
    
    ms->raw = del_RegRawData(ms->raw);
    ms->interval = 1;
    ms->sending = 0;
    ms->send_interval = 0;
    ms->send_regset = 0;
    ms->send_rewind = 0;
  };
}

/*.......................................................................
 * Return the array map of a monitor stream.
 *
 * Input:
 *  ms     MonitorStream *  The stream to query.
 * Output:
 *  return        RegMap *  The register map of the stream. This will
 *                          be NULL if the stream is closed or if ms==NULL.
 */
ArrayMap *ms_ArrayMap(MonitorStream *ms)
{
  return ms ? ms->arraymap : NULL;
}

/*.......................................................................
 * Return the container of the array of calibrated registers.
 *
 * Input:
 *  ms     MonitorStream *  The stream to query.
 * Output:
 *  return    RegCalData *  The container of calibrated registers. This will
 *                          be NULL if the stream is closed or if ms==NULL.
 */
sza::util::RegCal::RegCalData* ms_RegCalData(MonitorStream *ms)
{
  return ms ? (ms->regCal ? ms->regCal->calData() : NULL) : NULL;
}

/*.......................................................................
 * Return the container of the array of uncalibrated registers.
 *
 * Input:
 *  ms     MonitorStream *  The stream to query.
 * Output:
 *  return    RegRawData *  The container of uncalibrated registers. This
 *                          will be NULL if the stream is closed or if
 *                          ms==NULL.
 */
RegRawData *ms_RegRawData(MonitorStream *ms)
{
  return ms ? ms->raw : NULL;
}

/*.......................................................................
 * Return the preparation register-selection set.
 *
 * Input:
 *  ms     MonitorStream *  The stream to query.
 * Output:
 *  return        RegSet *  The preparation register set, this will be
 *                          NULL if the stream is closed or if ms==NULL.
 */
sza::util::RegisterSet *ms_prep_RegSet(MonitorStream *ms)
{
  return ms ? ms->prepRegSet : NULL;
}

/*.......................................................................
 * Return the established register-selection set.
 *
 * Input:
 *  ms     MonitorStream *  The stream to query.
 * Output:
 *  return        RegSet *  The established register set, this will be
 *                          NULL if the stream is closed or if ms==NULL.
 */
sza::util::RegisterSet *ms_RegSet(MonitorStream *ms)
{
  return ms ? ms->regSet : NULL;
}

/*.......................................................................
 * Return the current sub-sampling interval.
 *
 * Input:
 *  ms     MonitorStream *  The stream to query.
 * Output:
 *  return      unsigned    The sub-sampling interval, or 0 if the stream
 *                          is closed or ms==NULL.
 *                          NULL if the stream is closed or if ms==NULL.
 */
unsigned ms_get_interval(MonitorStream *ms)
{
  return ms ? ms->interval : 0;
}

/*.......................................................................
 * Return the type-specific stream context object.
 *
 * Input:
 *  ms     MonitorStream *  The stream to query.
 * Output:
 *  return          void *  The context object of the client stream
 *                          inplementation. This will be NULL if the
 *                          stream is closed or if ms==NULL.
 */
void *ms_SourceContext(MonitorStream *ms)
{
  return ms ? ms->context : NULL;
}

/*.......................................................................
 * Tell a monitor stream which registers you are interested in
 * receiving in subsequent calls to ms_read_frame().
 *
 * Input:
 *  ms     MonitorStream *  The stream to select registers from.
 *  dowait           int    If non-zero, don't return until the
 *                          selection has been sent to the supplier
 *                          or an error occurs. If zero, return immediately
 *                          with a return code of MS_READ_AGAIN if the
 *                          message can't be sent without blocking. In the
 *                          latter case subsequent calls to ms_send_msg()
 *                          will be required to complete the transaction.
 *  pedantic         int    If true, treat the non-existence of any of
 *                          the registers in *sel as a fatal error. If
 *                          false, simply emit a warning and arrange
 *                          to substitute zero for the value of each
 *                          missing register. The corresponding regs[]
 *                          element will have its 'slot' member set to
 *                          -1.
 *  sel MonitorSelection *  An array of 'nsel' specifications of registers
 *                          that are to be read on subsequent calls to
 *                          ms_read_frame(). Note that the id field of
 *                          each element must match the index of the element
 *                          in the sel[] array.
 *  nsel        unsigned    The number of register selections in sel[]
 *                          and the corresponding number of selection
 *                          results to be recorded in regs[].
 * Input/Output:
 *  regs       RegMapReg *  On input pass an array of nsel elements.
 *                          On output this will contain the details of
 *                          each selection. Note that the contents are
 *                          specific to the current register map, so
 *                          they should be renewed via a further call
 *                          to ms_select_regs() whenever ms_read_frame()
 *                          reports the receipt of a new register map.
 *                          ArrRegMapReg is defined in regmap.h.
 * Output:
 *  return   MsSendState    The completion status of the transaction, from:
 *                           MS_SEND_ERROR - An error occurred 
 *                           MS_SEND_AGAIN - The send operation remains
 *                                           incomplete (only if dowait=0).
 *                           MS_SEND_DONE  - The selection has been
 *                                           dispatched.
 */
MsSendState ms_select_regs(MonitorStream *ms, 
			   bool dowait, bool pedantic,
			   std::vector<MonitorSelection>& selections,
			   std::vector<sza::util::RegDescription>& regs)
{
  try {
    // Check the arguments.
    
    if(!ms) {
      lprintf(stderr, "ms_select_regs: NULL argument(s).\n");
      return MS_SEND_ERROR;
    };
    
    if(!ms->context) {
      lprintf(stderr,
	      "ms_select_regs: The monitor stream is currently closed.\n");
      return MS_SEND_ERROR;
    };
    
    // Clear the register selection set.
    
    ms->prepRegSet->reset();
    
    // Loop through our array of registers, look them up in the register
    // map and record the results in the return array.
    
    for(unsigned iSel=0; iSel < selections.size(); iSel++) {
      MonitorSelection* sel = &selections[iSel];
      
      // Archive files contain only archived registers
      
      sza::util::RegDescription desc(ms->prepRegSet->archivedOnly(), 
				     ms->arraymap);
      
      // Verify that the enumerator of the selection matches its index.
      
      if(sel->id_ != (signed)iSel) {
	lprintf(stderr, "Out of order MonitorSelection enumerators.\n");
	return MS_SEND_ERROR;
      };
      
      // Check that the names are set.
      
      if(!sel->boardName_ || !sel->blockName_) {
	lprintf(stderr, "ms_select_regs: NULL register name(s).\n");
	return MS_SEND_ERROR;
      };
      
      // Look up the register in the register map.
      
      desc.setTo(sel->regMapName_, sel->boardName_, sel->blockName_, REG_PLAIN, REG_INT_PLAIN,
		 *sel->range_);
      
      // Unarchived registers aren't assigned slots in archive frames,
      // so they are denoted by slot==-1.
      
      if(desc.iSlot() < 0) {
	lprintf(stderr, "Register %s.%s.%s isn't archived.\n", sel->regMapName_, 
		sel->boardName_, sel->blockName_);
	if(pedantic)
	  return MS_SEND_ERROR;
      };
      
      // Add the latest register to the selection of those that are to
      // be monitored.
      
      if(desc.iSlot() >= 0) {
	
	// Add the register description to the set of registers to be
	// monitored
	
	ms->prepRegSet->addRegister(desc);
	
	// And copy it to the input array
	
	regs[iSel] = desc;
      }
    }
    
    // Queue the revised register set to be sent to the supplier.
    
    if(ms_queue_regset(ms) == MS_SEND_ERROR)
      return MS_SEND_ERROR;
    
    // Dispatch the register selection to the supplier.
    
    return ms_send_msg(ms, dowait);

  } catch(sza::util::Exception& err) {

    std::cout << err.what() << std::endl;
    
    return MS_SEND_ERROR;
  }
}

/*.......................................................................
 * Read calibrated data from the latest register frame into a supplied
 * array of n float's.
 *
 * Input:
 *  ms   MonitorStream *  The stream containing the specified registers.
 *  desc RegDescription*  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data         float *  An array with room for n float elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int ms_get_float(MonitorStream *ms, sza::util::RegDescription* desc, float *data,
		 sza::util::CoordRange* range)
{
  // Retrieve the requested registers.
  
  try {
    
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalFloat(desc, data, range);
    
  } catch(...) {
    return 1;
  }
  return 0;
}

int ms_get_float(MonitorStream *ms, sza::util::RegDescription* desc, 
		 sza::util::MonitorDataType* data,
		 sza::util::RegAxisRange& range)
{
  // Retrieve the requested registers.
  
  try {
    
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalFloat(desc, data, range);
    
  } catch(...) {
    return 1;
  }
  return 0;
}

/*.......................................................................
 * Read calibrated data from the latest register frame into a supplied
 * array of n double's.
 *
 * Input:
 *  ms   MonitorStream *  The stream containing the specified registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data        double *  An array with room for n double elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int ms_get_double(MonitorStream *ms, sza::util::RegDescription* desc, double *data, 
		  sza::util::CoordRange* range)
{
  try {
    
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalDouble(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

int ms_get_double(MonitorStream *ms, sza::util::RegDescription* desc, 
		  sza::util::MonitorDataType* data,
		  sza::util::RegAxisRange& range)
{
  try {
    
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalDouble(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

/*.......................................................................
 * Read calibrated data from the latest register frame into a supplied
 * array of n unsigned int's.
 *
 * Input:
 *  ms   MonitorStream *  The stream containing the specified registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data      unsigned *  An array with room for n unsigned elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int ms_get_uint(MonitorStream *ms, sza::util::RegDescription* desc, unsigned *data,
		sza::util::CoordRange* range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalUint(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

int ms_get_uint(MonitorStream *ms, sza::util::RegDescription* desc,
		sza::util::MonitorDataType* data,
		sza::util::RegAxisRange& range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalUint(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}


/*.......................................................................
 * Read calibrated data from the latest register frame into a supplied
 * array of n int's.
 *
 * Input:
 *  ms   MonitorStream *  The stream containing the specified registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data           int *  An array with room for n int elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int ms_get_char(MonitorStream *ms, sza::util::RegDescription* desc, char* data, 
	       sza::util::CoordRange* range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalChar(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

int ms_get_char(MonitorStream *ms, sza::util::RegDescription* desc,
		sza::util::MonitorDataType* data,
		sza::util::RegAxisRange& range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalChar(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

/*.......................................................................
 * Read calibrated data from the latest register frame into a supplied
 * array of n int's.
 *
 * Input:
 *  ms   MonitorStream *  The stream containing the specified registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data           int *  An array with room for n int elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int ms_get_uchar(MonitorStream *ms, sza::util::RegDescription* desc, unsigned char* data, 
	       sza::util::CoordRange* range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalUchar(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

int ms_get_uchar(MonitorStream *ms, sza::util::RegDescription* desc,
		sza::util::MonitorDataType* data,
		sza::util::RegAxisRange& range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalUchar(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

/*.......................................................................
 * Read calibrated data from the latest register frame into a supplied
 * array of n int's.
 *
 * Input:
 *  ms   MonitorStream *  The stream containing the specified registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data           int *  An array with room for n int elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int ms_get_int(MonitorStream *ms, sza::util::RegDescription* desc, int* data, 
	       sza::util::CoordRange* range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalInt(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

int ms_get_int(MonitorStream *ms, sza::util::RegDescription* desc,
	       sza::util::MonitorDataType* data,
	       sza::util::RegAxisRange& range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalInt(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}


/*.......................................................................
 * Read calibrated data from the latest register frame into a supplied
 * array of n unsigned long's.
 *
 * Input:
 *  ms   MonitorStream *  The stream containing the specified registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data unsigned long *  An array with room for n unsigned long elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int ms_get_ulong(MonitorStream *ms, sza::util::RegDescription* desc, unsigned long *data,
		 sza::util::CoordRange* range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalUlong(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

int ms_get_ulong(MonitorStream *ms, sza::util::RegDescription* desc,
		 sza::util::MonitorDataType* data,
		 sza::util::RegAxisRange& range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalUlong(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}


/*.......................................................................
 * Read calibrated data from the latest register frame into a supplied
 * array of n long's.
 *
 * Input:
 *  ms   MonitorStream *  The stream containing the specified registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data          long *  An array with room for n long elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int ms_get_long(MonitorStream *ms, sza::util::RegDescription* desc, long *data, 
		sza::util::CoordRange* range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalLong(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

int ms_get_long(MonitorStream *ms, sza::util::RegDescription* desc,
		sza::util::MonitorDataType* data,
		sza::util::RegAxisRange& range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalLong(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}


/*.......................................................................
 * Unpack a string from a given register of the latest register frame
 * into a supplied array of nc characters.
 *
 * Input:
 *  ms   MonitorStream *  The stream containing the specified registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  nc        unsigned    The max number of characters to be placed in
 *                        data (including the '\0' terminator).
 * Input/Output:
 *  string        char *  The array of nc characters to return the string
 *                        in.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int ms_get_string(MonitorStream *ms, sza::util::RegDescription* desc, unsigned nc, 
		  char *string)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalString(desc, string, nc);
  } catch(...) {
    return 1;
  }
  return 0;
}

/*.......................................................................
 * Read calibrated data from the latest register frame into a supplied
 * array of n dates
 *
 * Input:
 *  ms   MonitorStream *  The stream containing the specified registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data          long *  An array with room for n long elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int ms_get_date(MonitorStream *ms, sza::util::RegDescription* desc, 
		sza::util::RegDate::Data* data, 
		sza::util::CoordRange* range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalDate(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

int ms_get_date(MonitorStream *ms, sza::util::RegDescription* desc,
		sza::util::MonitorDataType* data,
		sza::util::RegAxisRange& range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalDate(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}


/*.......................................................................
 * Read calibrated data from the latest register frame into a supplied
 * array of n dates
 *
 * Input:
 *  ms   MonitorStream *  The stream containing the specified registers.
 *  reg      ArrRegMapReg *  The register-range specification that was used
 *                        to select the required register for monitoring.
 *  index     unsigned    The first index of the register that is to
 *                        be returned (0 returns the first element of the
 *                        register).
 *  n         unsigned    The number of register elements to return.
 *                        Note that *reg must cover all elements in
 *                        the index range index...index+n-1.
 * Input/Output:
 *  data          long *  An array with room for n long elements.
 *                        On return this will contain the values of
 *                        the requested register elements.
 * Output:                        
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int ms_get_complex_float(MonitorStream *ms, sza::util::RegDescription* desc, 
			 sza::util::Complex<float>::Data* data, 
			 sza::util::CoordRange* range)
{
  // Retrieve the requested registers.

  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalComplexFloat(desc, data, range);
  } catch(...) {
    return 1;
  }
  return 0;
}

int ms_get_complex_float(MonitorStream *ms, sza::util::RegDescription* desc,
			 sza::util::MonitorDataType* data,
			 sza::util::RegAxisRange& range)
{
  // Retrieve the requested registers.
  
  try {
    if(ms==0 || desc==0)
      ThrowError("Received NULL argument");
    
    ms->regCal->calData()->getCalComplexFloat(desc, data, range);
  } catch(...) {
    return 1;
  }
  //  CTOUT("Leaving ms_get_complex_float");
  return 0;
}


/**.......................................................................
 * Constructor initialization for MonitorSelection
 */
void MonitorSelection::initialize(int id,
				  std::string regMapName, 
				  std::string boardName, 
				  std::string blockName,
				  sza::util::CoordRange* range)
{
  // Set these to something sensible to default
  
  id_          = id;
  regMapName_  =  0;
  boardName_   =  0;
  blockName_   =  0;
  range_       =  0;
  
  // Copy the strings
  
  regMapStr_ = regMapName;
  boardStr_  = boardName;
  blockStr_  = blockName;
  
  // And set the char pointers pointing to them.
  
  regMapName_ = (char*)regMapStr_.c_str();
  boardName_  = (char*)boardStr_.c_str();
  blockName_  = (char*)blockStr_.c_str();
  
  // Allocate a new range, copying the passed one
  
  range_ = new sza::util::CoordRange(range);
}

/**.......................................................................
 * Constructor for MonitorSelection
 */
MonitorSelection::MonitorSelection(int id,
				   std::string regMapName, 
				   std::string boardName, 
				   std::string blockName,
				   sza::util::CoordRange* range)
{
  initialize(id, regMapName, boardName, blockName, range);
}

/**.......................................................................
 * Copy constructor for MonitorSelection
 */
MonitorSelection::MonitorSelection(const MonitorSelection& selection)
{
  initialize(selection.id_, selection.regMapStr_, selection.boardStr_, 
	     selection.blockStr_, selection.range_);
};

/**.......................................................................
 * Destructor MonitorSelection
 */
MonitorSelection::~MonitorSelection()
{
  if(range_ != 0) {
    delete range_;
    range_ = 0;
  }
}
