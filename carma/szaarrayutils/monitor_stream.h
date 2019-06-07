#ifndef monitor_stream_h
#define monitor_stream_h

#include <string>

#include "carma/szaarrayutils/arraymap.h"
#include "carma/szaarrayutils/regset.h"
#include "carma/szaarrayutils/regcal.h"
#include "carma/szaarrayutils/regdata.h"

#include "carma/szautil/Complex.h"
#include "carma/szautil/RegDate.h"
#include "carma/szautil/RegisterSet.h"
#include "carma/szautil/RegCal.h"

namespace sza {
  namespace util {
    class DataType;
    class RegAxisRange;
  }
}

/*.......................................................................
 * The following method is called upon to read all or part of the
 * next frame of registers. It should behave like its generic
 * counterpart described above.
 */
#define MS_READ_FRAME(fn) \
sza::array::MsReadState (fn)(sza::array::MonitorStream *ms, int dowait)

/*.......................................................................
 * Where provided, the following optional method is called upon to
 * send part or all of the most recently packed output message.
 */
#define MS_SEND_MSG(fn) \
sza::array::MsSendState (fn)(sza::array::MonitorStream *ms, int dowait)

/*.......................................................................
 * Where provided the following optional method is called whenever the
 * current selection of registers in ms->regset is changed. This allows
 * for the register set to be packed for transmission to a remote
 * register supplier. If calls to the MS_SEND_MSG() method will be
 * needed to complete the transaction, MS_SEND_AGAIN should be returned.
 */
#define MS_QUEUE_REGSET(fn) \
sza::array::MsSendState (fn)(sza::array::MonitorStream *ms)

/*.......................................................................
 * Where provided the following optional method is called whenever the
 * current sampling interval [ms_get_interval()] is changed. This allows
 * for the interval to be packed for transmission to a remote
 * register supplier. If calls to the MS_SEND_MSG() method will be
 * needed to complete the transaction, MS_SEND_AGAIN should be returned.
 */
#define MS_QUEUE_INTERVAL(fn) \
sza::array::MsSendState (fn)(sza::array::MonitorStream *ms)

/*.......................................................................
 * Where pertinent, the following optional method requests that a stream
 * be "rewound" to its beginning. If this requires a message to be sent
 * to the supplier, then this function should pack the message in
 * preparation for subsequent calls to ms_send_msg(), and return
 * MS_SEND_AGAIN. Otherwise it should rewind the stream and return
 * MS_SEND_DONE.
 */
#define MS_QUEUE_REWIND(fn) \
sza::array::MsSendState (fn)(sza::array::MonitorStream *ms)

/*.......................................................................
 * The following method returns a file descriptor for use in select() to
 * see when the stream is ready for reading or writing. Note that the
 * select() user is expected to call this before each use of select().
 * This allows the fd to change with time (for example, the file
 * monitor will read across file boundaries, so the fd may well change).
 */
#define MS_SELECT_FD(fn) int (fn)(sza::array::MonitorStream *ms)

/*.......................................................................
 * The following method is called to delete the stream-specific
 * implementation context.
 */
#define MS_DESTRUCTOR(fn) void *(fn)(void *context)

/*.......................................................................
 * Return the current array map of the stream.
 */
#define MS_ARRAYMAP(fn) ArrayMap *(fn)(sza::array::MonitorStream *ms)

/*.......................................................................
 * Return the current register map of the stream.
 */
#define MS_REGMAP(fn) RegMap *(fn)(sza::array::MonitorStream *ms)

namespace sza {
  namespace array {
    
    /*
     * The function that reads data from the supplier can be told to wait
     * for a complete record to be received, or to read as much as possible
     * without blocking, then return with an indication that a future call
     * will be necessary to retrieve the remaining data. The following
     * values are returned by said function to indicate the state of the
     * read operation.
     *
     * Special note should be taken of the MS_READ_REGMAP return value.
     * This is a warning that the next call will return registers that may
     * occupy different slots and have different dimensions than before.
     * Furthermore some registers that existed in the previous register
     * map may not exist in the new register map, and some new ones may
     * now be available.
     *
     * Note that when MS_READ_REGMAP is returned, the previous register
     * map and all of the objects returned by previous calls to
     * ms_RegMap(), ms_RegCalData(), ms_RegRawData(), ms_RegSet(), and
     * ms_prep_RegSet() will have been destroyed, and thus should not be
     * used. At this point you should destroy any of your own objects
     * who's constructors took the return value of a previous call to
     * ms_RegMap(), then create them anew using the return value of a new
     * call to ms_RegMap().
     *
     * In order to give one a chance to finish up any defered work that
     * involved the previous register map, ms_read_frame() returns
     * MS_READ_BREAK whenever it encounters a point in the stream beyond
     * which a new register map *might* change. For example, in multi-file
     * file-input streams this occurs at the end of each file.
     */
    typedef enum {     /* Frame-reader read status */
      MS_READ_ENDED,   /* The end of the stream has been reached */
      MS_READ_AGAIN,   /* The read operation is incomplete (dowait=0) */
      MS_READ_BREAK,   /* The end of one part of a multi-segment
			  stream has been */
      /*  reached. On the next call the register map may change, */
      /*  in which case MS_READ_REGMAP will be returned. */
      /*  This forewarning allows the caller to finish up any */
      /*  defered operations that involve the old register map */
      /*  before it gets destroyed. */
      MS_READ_REGMAP,  /* A new incompatible register map has been
			  encountered */
      /*  (see above for details). */
      MS_READ_DONE     /* A new frame of selected registers has been read */
    } MsReadState;
    
    /*
     * When sending a control message to the supplier, one first submits
     * the message to be packed for transmission by calling the
     * appropriate submittal function, then calls ms_send_msg() to
     * transmit the message. When calling ms_send_msg() one can ask to have
     * the message sent in its entirety before returning, or to have as much
     * as possible sent without blocking. In the latter case, completion
     * of the send operation must be completed by subsequent calls to
     * ms_send_msg().
     *
     * Note that while a send operation is incomplete the values submitted
     * via intervening calls to ms_queue_regset() and/or ms_queue_interval()
     * will be queued for re-submision when the current send
     * completes. ms_send_msg() will not return MS_SEND_DONE until all
     * such transactions have been completed.
     *
     * The following values are returned by ms_send_msg() to indicate the
     * state of the send operation.
     */
    typedef enum {     /* Frame-reader write status */
      MS_SEND_ERROR,   /* An unrecoverable error occurred */
      MS_SEND_AGAIN,   /* The send operation is incomplete (dowait=0) */
      MS_SEND_DONE     /* The message has been sent */
    } MsSendState;
    
    /*-----------------------------------------------------------------------
     * This module provides a generic stream interface for reading monitor
     * data from a variety of sources. Supported sources include a network
     * connection to the control program, and sequential archive files on
     * disk.
     *
     * The internals of a stream are logically split into two parts, a
     * local consumer and a potentially remote supplier. Given that the
     * supplier can be in a separate process, messaging via either
     * non-blocking or blocking I/O is used to dispatch control messages
     * to, and receive data from a given supplier. The only time that
     * non-blocking I/O is not an option is when the stream makes a
     * connection to a new supplier. At this time, information, such as
     * the register map of the supplier and buffer size information is
     * read from the supplier using blocking I/O.
     *
     * The following datatype provides an opaque handle for all stream
     * types.  Its internals are private to monitor_stream.c.
     */
    /*
     * Define the contents of a generic monitor stream.
     */
    typedef struct MonitorStream {
      void *context;                  // The context of the current
				      // data-source
      MS_DESTRUCTOR(*del_fn);         // The 'context' destructor
				      // function
      MS_READ_FRAME(*read_fn);        // The method used to read the
				      // next frame
      MS_SEND_MSG(*send_fn);          // The method that is called to
				      // send queued requests to the
				      // supplier.
      MS_QUEUE_REGSET(*regset_fn);    // The method that is called to
				      // queue a new register
				      // selection.
      MS_QUEUE_INTERVAL(*interval_fn);// The method that is called to
				      // queue a new sub-sampling
				      // interval.
      MS_QUEUE_REWIND(*rewind_fn);    // The method that is called to
				      // queue a rewind request.
      MS_SELECT_FD(*fd_fn);           // The method that returns the current fd 
      MS_ARRAYMAP(*arraymap_fn);      // The method that returns the
				      // current register map.

      bool archivedOnly_;             // This flag affects the
				      // interpretation of all of the
				      // following:

      ArrayMap *arraymap;             // The register map of the monitor supplier 

      // The register set in which applications submit new register
      // selections

      sza::util::RegisterSet* prepRegSet;

      // The established selection of registers 
      
      sza::util::RegisterSet* regSet;

      // The register calibration object 

      sza::util::RegCal* regCal;

      RegRawData *raw;                // The latest array of
				      // un-calibrated registers
      RegCalData *cal;                // The latest array of calibrated registers 
      unsigned interval;              // The sampling interval. The
				      // supplier should drop all but
				      // one of every 'interval'
				      // frames.
      int sending;                    // True while an ongoing send
				      // operation is incomplete.

      // If a request to send a regset or interval request to the
      // supplier is received before a previous request has been
      // completely dispatched, set the corresponding flag below to
      // have the request dispatched as soon as the previous
      // transaction is completed.
       
      int send_interval;              // True to queue an update from 'interval' 
      int send_regset;                // True to queue an update from
				      // 'prep_regset'
      int send_rewind;                // True to queue a rewind-stream message 

    } MonitorStream;
  }
}    
/*
 * The following is an example of the simplest usage of a monitor
 * stream.
 *
 * |* Start by defining indexes for the registers that you are interested *|
 * |* in retrieving. *|
 *
 * typedef enum {
 *   FRAME_MJD,    |* The frame.mjd register *|
 *   FRAME_UTC,    |* The frame.utc register *|
 *   CORR0_VIS,    |* The corr0.vis[] register *|
 *   NUM_MY_REGS   |* The number of register selections - this must be last *|
 * } MyRegIndex;
 *
 *
 * |* Now in the same order as the above enumeration, associate each *|
 * |* enumerator with the register-map name of the corresponding register. *|
 *
 * static MonitorSelection my_sel[NUM_MY_REGS] = {
 *   {FRAME_MJD, "frame", "mjd"},
 *   {FRAME_UTC, "frame", "utc"},
 *   {CORR0_VIS, "corr0", "vis"},
 * };
 *
 * ...
 * MonitorStream *ms;                |* The monitor stream to be created *|
 *
 * |* The following array will be filled in below by ms_select_regs() *|
 *
 * ArrRegMapReg my_regs[NUM_MY_REGS];   |* The array of selected registers *|
 * 
 * |* Create a monitor stream for all the files of a disk based archive *|
 *
 * ms = new_FileMonitorStream("/scr/archive", 0.0, 0.0);
 * if(!ms)
 *   return error;
 *
 * |* Tell the monitor stream about the registers that we are interested in. *|
 * |* Details of each of the selected registers are recorded in my_regs[] *|
 * |* These details change from register map to register map, so later *|
 * |* we are careful to reselect then if a new register map is encountered *|
 *
 * if(ms_select_regs(ms, 1, 0, my_sel, NUM_MY_REGS, &my_regs) != MS_SEND_DONE)
 *   return error...
 *
 * |* Load the calibration parameters of the initial register map. *|
 *
 * if(ms_load_cal_file(ms, "", calfile))
 *   return error...
 *
 * |* Read successive register samples, stopping at the end of the stream *|
 *
 * while(1) {
 *   switch(ms_read_frame(ms, 1)) {
 *   case MS_READ_AGAIN:   |* We should never get this when the dowait *|
 *                         |*  argument of ms_read_frame() is 1 *|
 *   case MS_READ_BREAK:   |* We have reached a point beyond which a *|
 *                         |*  new register map may be encountered. *|
 *     break;
 *   case MS_READ_ENDED:   |* We have reached the end of the stream *|
 *     printf("End of stream reached.\n");
 *     return ok;
 *     break;
 *   case MS_READ_DONE:    |* We received a new frame of registers *|
 *     if(ms_get_double(ms, my_regs + FRAME_MJD, 0, 1, &mjd) ||
 *        ms_get_double(ms, my_regs + FRAME_UTC, 0, 2, &utc) ||
 *        ms_get_float(ms, my_regs + CORR0_VIS, 0, 156 , &vis))
 *       return error;
 *     ....process the visibility data in vis[]....;
 *     break;
 *   case MS_READ_REGMAP:  |* An incompatible register map was encountered *|
 *                         |* Reselect our registers in the new register map *|
 *
 *     if(ms_select_regs(ms, 1, 0, my_sel, NUM_MY_REGS, &my_regs)!=MS_SEND_DONE)
 *       return error...
 *                         |* Load the calibration parameters of the new *|
 *                         |*  register map. *|
 *
 *     if(ms_load_cal_file(ms, "", calfile))
 *       return error...
 *    break;
 *   };
 * };
 *
 */


/*
 * Create a new MonitorStream object and attach it to a control
 * program on a given host. The host argument should contain
 * the name or internet address of the computer on which the
 * control program is running.
 *
 * When successful this function returns the new MonitorStream
 * object. On failure it returns NULL.
 */
sza::array::MonitorStream *new_NetMonitorStream(char *host);

/*
 * Create a new MonitorStream object and attach it to a time-ordered
 * sequence of disk files in the directory named by 'dir'. Note that
 * expansion of ~/ and ~user/ is supported within directory names.
 * The ta and tb arguments can be used to limit the time range over which
 * data should be retrieved. They are UTC's, expressed as Modified
 * Julian Dates. If ta <= 0, the earliest time available in the directory
 * will be substituted. If tb <= 0, the latest time available will be
 * substituted. So to retrieve all of the data in a given directory,
 * irrespective of its time range, pass both ta and tb as 0.0.
 *
 * When successful this function returns the new MonitorStream
 * object. On failure it returns NULL.
 */
sza::array::MonitorStream *new_FileMonitorStream(char *dir, double ta, double tb);

/*
 * The following function closes and deletes a monitor stream.
 * Note that del_MonitorStream() is idempotent (ie. it returns NULL
 * to represent a deleted stream, and can safely take NULL as an
 * argument).
 */
sza::array::MonitorStream *del_MonitorStream(sza::array::MonitorStream *ms);

/*-----------------------------------------------------------------------
 * Stream method functions.
 *
 * The following functions should only be used while a stream is
 * connected to a supplier.
 *---------------------------------------------------------------------*/

/*
 * Incrementally read the next set of the selected registers from
 * the established data supplier. If dowait==0, retrieval of each
 * frame may require multiple calls to this function. In such cases
 * select() can be used to wait for the arrival of more data. See
 * ms_select_fd() below for details.
 */
sza::array::MsReadState ms_read_frame(sza::array::MonitorStream *ms, int dowait);
sza::array::MsReadState ms_count_frame(sza::array::MonitorStream *ms, int dowait);

/*
 * After reading new register values with ms_read_frame() the
 * following functions can be used to copy the returned values
 * into local arrays. The reg argument of each function should
 * be one of the register-range specifications that was used to
 * select registers to be monitored. The index and n arguments
 * should select subsets of these ranges, and the output data
 * arrays must have room for at least n elements. Each of these
 * functions returns non-zero on error. Note that zeroes will be
 * substituted for any of the selected registers that aren't in the
 * archive (see the documentation of the pedantic argument of
 * ms_select_regs() above).
 */
int ms_get_float(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		 float *data, sza::util::CoordRange* range=0);
int ms_get_double(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		  double* data, sza::util::CoordRange* range=0);
int ms_get_uint(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		unsigned* data, sza::util::CoordRange* range=0);
int ms_get_int(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
	       int* data, sza::util::CoordRange* range=0);
int ms_get_uchar(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		 unsigned char* data, sza::util::CoordRange* range=0);
int ms_get_char(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		char* data, sza::util::CoordRange* range=0);
int ms_get_ulong(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		 unsigned long* data, sza::util::CoordRange* range=0);
int ms_get_long(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		long* data, sza::util::CoordRange* range=0);

int ms_get_date(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		sza::util::RegDate::Data* data, sza::util::CoordRange* range=0);

int ms_get_complex_float(sza::array::MonitorStream *ms, 
			 sza::util::RegDescription* desc, 
			 sza::util::Complex<float>::Data* data, 
			 sza::util::CoordRange* range=0);


int ms_get_float(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		 sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);

int ms_get_double(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		  sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);

int ms_get_uint(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);

int ms_get_int(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
	       sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);

int ms_get_uchar(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);

int ms_get_char(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);

int ms_get_ulong(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		 sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);

int ms_get_long(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);

int ms_get_date(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
		sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);

int ms_get_complex_float(sza::array::MonitorStream *ms, sza::util::RegDescription* desc, 
			 sza::util::MonitorDataType* data, sza::util::RegAxisRange& range);

/*
 * Unpack a string from a string register. The *reg argument must refer
 * to the whole of the register. Up to nc-1 characters of the string
 * will be returned in string[], which must be an array of at least nc
 * characters. If the string won't fit within nc-1 characters, it will
 * be terminated by placing a '\0' terminator in string[nc-1]. Otherwise
 * the string will be terminated with a '\0' at the natural end of the
 * string. On error non-zero is returned. Note that an empty string will be
 * substituted if the register isn't in the archive (see the documentation
 * of the pedantic argument of ms_select_regs() above).
 */
int ms_get_string(sza::array::MonitorStream *ms, sza::util::RegDescription *reg, unsigned nc, char *string);

/*
 * Incrementally send a message, previously submitted via a
 * call to ms_queue_interval(), ms_queue_regset() or ms_queue_rewind,
 * to the supplier. Multiple calls to this function may be required if
 * dowait==0. In such cases select() can be used to wait for the
 * output channel to free up. See ms_select_fd() for details.
 */
sza::array::MsSendState ms_send_msg(sza::array::MonitorStream *ms, int dowait);

/*
 * You probably won't need to use this function directly, because
 * ms_select_regs() calls it for you (see later).
 *
 * Submit a modified register selection to be subsequently sent to the
 * supplier via one or more calls to ms_send_msg(). The modified
 * register set should be composed in the preparation register set
 * that is returned by a call to ms_prep_RegSet(). If subsequent
 * calls to ms_send_msg() will not be required, ms_queue_regset()
 * returns MS_SEND_DONE.
 */
sza::array::MsSendState ms_queue_regset(sza::array::MonitorStream *ms);

/*
 * Submit a new subsampling interval to be sent to the
 * data supplier. If MsSendState is MS_SEND_AGAIN then subsequent
 * calls to ms_send_msg() will be needed to complete the transaction.
 *
 * Note that an interval of 2 means "supply every second record".
 * At startup the interval is initialized to 1.
 */
sza::array::MsSendState ms_queue_interval(sza::array::MonitorStream *ms, unsigned interval);

/*
 * If the stream can be rewound, submit a rewind request to
 * be sent to the supplier, otherwise do nothing more than return
 * MS_SEND_DONE. If the return code is MS_SEND_AGAIN, subsequent
 * calls to ms_send_msg() will be needed to complete the transaction.
 */
sza::array::MsSendState ms_queue_rewind(sza::array::MonitorStream *ms);

/*
 * Return true if the data source of the specified stream can be rewound.
 */
int ms_can_be_rewound(sza::array::MonitorStream *ms);

/*
 * Return a file descriptor that can be used with select() to
 * determine when more data is available, or when the output
 * channel to the supplier is ready to accept more data. If the
 * stream isn't connected, -1 will be returned.
 *
 * WARNING: This file descriptor may change after any call to
 *          ms_read_frame(), so be sure to call ms_select_fd()
 *          before every call to select().
 */
int ms_select_fd(sza::array::MonitorStream *ms);

/*
 * Load new calibration parameters from a calibration file. Note that
 * the contents of the dir[] and name[] arguments of ms_load_cal_file()
 * will be concatenated to form the path name of the calibration file,
 * so if desired the directory part of the path name can be passed via
 * the dir[] argument, and the file name can be passed via the name[]
 * argument. Alternatively, if you want to specify the path name of
 * the file as a single string, pass this string via the name[]
 * argument and set the dir[] argument to "".  The return value of
 * ms_load_cal_file() is non-zero on failure.
 */
int ms_load_cal_file(sza::array::MonitorStream *ms, char *dir, char *name);

/*
 * Load calibration parameters from a text input stream.
 * See input.h to see how to create input streams.
 */
int ms_load_cal_stream(sza::array::MonitorStream *ms, InputStream *stream);

/*
 * Reset all calibration parameters to unit scale factors, and
 * zero offsets.
 */
int ms_reset_cal(sza::array::MonitorStream *ms);

/*
 * An array of 'nsel' elements of the following type of container
 * is what you pass to ms_select_regs() to tell it which register
 * ranges to select.
 */
struct MonitorSelection {
  int id_;           // The index of this MonitorSelection in its parent array 
  char *regMapName_; // The name of the target board in the register map 
  char *boardName_;  // The name of the target board in the register map 
  char *blockName_;  // The name of the target block on the above board 
  std::string regMapStr_;
  std::string boardStr_;
  std::string blockStr_;

  // The range of indices to select

  sza::util::CoordRange* range_;

  //  Leave this as NULL if you always want to select all of the
  //  elements of the register.

  // Initialize internals of this object

  void initialize(int id, std::string regMapName, std::string boardName, 
		  std::string blockName, sza::util::CoordRange* range);

  // Constructor

  MonitorSelection(int id, std::string regMapName, std::string boardName, 
		   std::string blockName, sza::util::CoordRange* range=0);

  // Copy constructor

  MonitorSelection(const MonitorSelection& selection);

  // Destructor

  ~MonitorSelection();
};

/*
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
 *  regs       ArrRegMapReg *  On input pass an array of nsel elements.
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
sza::array::MsSendState ms_select_regs(sza::array::MonitorStream *ms, 
				       bool dowait, bool pedantic,
				       std::vector<MonitorSelection>& selections, 
				       std::vector<sza::util::RegDescription>& regs);

/*-----------------------------------------------------------------------
 * Functions that provide readonly access to selected stream internals.
 *---------------------------------------------------------------------*/

/*
 * Get the array map of a monitor stream (or NULL if the stream
 * is unconnected). See arraymap.h for a description of the ArrayMap
 * datatype.
 */
ArrayMap *ms_ArrayMap(sza::array::MonitorStream *ms);

/*
 * Get the register map of a monitor stream (or NULL if the stream
 * is unconnected). See regmap.h for a description of the RegMap
 * datatype.
 */
ArrayMap *ms_ArrayMap(sza::array::MonitorStream *ms);

/*
 * Return the container of the array of calibrated registers (or NULL
 * if the stream is unconnected). This will only contain valid data
 * after each call to ms_read_frame() that returns MS_READ_DONE. Its
 * contents will be overwritten on each call to ms_read_frame().
 * See regdata.h for a description of the RegCalData datatype.
 */
sza::util::RegCal::RegCalData* ms_RegCalData(sza::array::MonitorStream *ms);

/*
 * Return the container of the array of uncalibrated registers (or NULL
 * if the stream is unconnected). This will only contain valid data
 * after each call to ms_read_frame() that returns MS_READ_DONE. Its
 * contents will be overwritten on each call to ms_read_frame().
 * See regdata.h for a description of the RegCalData datatype.
 */
RegRawData *ms_RegRawData(sza::array::MonitorStream *ms);

/*
 * Return the established register selection set (or NULL if the
 * stream is unconnected). See regset.h for a description of
 * the RegSet datatype.
 */
sza::util::RegisterSet* ms_RegSet(sza::array::MonitorStream *ms);

/*
 * Return the current sub-sampling interval (or 0 if the stream is
 * unconnected).
 */
unsigned ms_get_interval(sza::array::MonitorStream *ms);

/*-----------------------------------------------------------------------
 * Functions that provide read/write access to selected stream internals.
 *---------------------------------------------------------------------*/

/*
 * You won't need this if you use ms_select_regs().
 *
 * Return the preparation register-selection set (or NULL if the
 * stream is unconnected). This is to be used with ms_queue_regset()
 * to select the set of registers to be supplied. Note that if a
 * register set update initiated by ms_queue_regset() has to be
 * postponed because of a preceding incomplete transaction,
 * the re-submittal of the register set will be base on whatever is
 * in the preparation register set at the time of the resubmission.
 * Thus beware that while it is safe to modify this register set
 * between calls to ms_send_msg(), the register set should be in a
 * chosen state whenever ms_send_msg() is called.
 */
sza::util::RegisterSet *ms_prep_RegSet(sza::array::MonitorStream *ms);


/*-----------------------------------------------------------------------
 * The rest of this file regards implementation of new stream sources.
 *
 * The specific implementation of a particular stream source is
 * implemented through the following method functions, plus an
 * anonymous implementation object allocated by the stream source.
 *---------------------------------------------------------------------*/

/*
 * To create an unconnected monitor stream call new_MonitorStream().
 */
sza::array::MonitorStream *new_MonitorStream(bool archivedOnly);


/*.......................................................................
 * The following function is used to connect a given stream source to
 * a monitor stream object. It requires the register map read from the
 * supplier, a stream-specific context object + its destructor, and
 * the above method functions. It returns non-zero if the call fails.
 */
int open_MonitorStream(sza::array::MonitorStream *ms, void *context, MS_DESTRUCTOR(*del_fn),
		       MS_READ_FRAME(*read_fn), MS_SEND_MSG(*send_fn),
		       MS_QUEUE_REGSET(*regset_fn),
		       MS_QUEUE_INTERVAL(*interval_fn),
		       MS_QUEUE_REWIND(*rewind_fn),
		       MS_SELECT_FD(*fd_fn),
		       MS_ARRAYMAP(*arraymap_fn),
		       bool archivedOnly);

/*
 * The above method functions can retrieve the context object that
 * was registered with open_MonitorStream(), by calling the following
 * function.
 */
void *ms_SourceContext(sza::array::MonitorStream *ms);

/*
 * The following function is used to close a MonitorStream. This
 * is done for you by del_MonitorStream(). Also open_MonitorStream()
 * calls this function before connecting a stream to a new supplier.
 * Note that close_MonitorStream() is idempotent.
 */
void close_MonitorStream(sza::array::MonitorStream *ms);

#endif
