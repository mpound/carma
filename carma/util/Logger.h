#ifndef CARMA_UTIL_LOGGER_H
#define CARMA_UTIL_LOGGER_H
/**
 * This is the include file for Carma logging util.
 * @file Logger.h
 * @author Marc Pound
 */

// Include commonly-used log4cpp appenders, so that client
// programs don't need to include them.
#include <log4cpp/Category.hh>
#include <log4cpp/FileAppender.hh>
#include <log4cpp/Layout.hh>
#include <log4cpp/SimpleLayout.hh>
#include <log4cpp/NDC.hh>
#include <log4cpp/Portability.hh>
#include <log4cpp/Priority.hh>
#include <log4cpp/OstreamAppender.hh>
#include <log4cpp/RemoteSyslogAppender.hh>
#include <log4cpp/SyslogAppender.hh>
#include <log4cpp/TimeStamp.hh>

#include <syslog.h>

#include "carma/util/FacilityType.h"

namespace carma {
  namespace util {

    /**
     * A convenience class for returning predefined log4cpp Categories.
     * This class has only static methods.
     */
    class Logger
    {

    public:

        /** The destructor for Logger.  */
        virtual ~Logger();

        /** 
         * @return A Category instance with either a local or remote 
         * syslog appender attached.  The layout (format) of the syslog 
         * messages is that of SyslogLayout. 
         *
         * @param identity The identity for the attached Appender,
         * i.e. the program name. This corresponds the <code>ident</code> 
         * parameter in the <tt>openlog(3)</tt> call.
         * 
         * @param host The host name for 
         *        the syslog.  If <code>host</code> string
         *        matches that returned by 
         *        <tt>gethostname(3C)</tt> or is "localhost", then 
	 *        a SysLogAppender
         *        is attached as the default Appender, otherwise 
         *        a RemoteSysLogAppender is used.  The default value
	 *        is "localhost"
         *
         * @param logname The hierarchical namespace for 
         * this Category, e.g. "carma.monitor".  The root logger will 
         * be "carma", all other loggers should be 
         * "carma.something[.somethingelse]" to make use of <br>
         * a) log4cpp's automatic heirarchical logging and <br>
         * b) for sorting in e.g. database queries.<br>
	 * The default is "carma.default" but you really should change it.
         *
	 * @param priority The minimum level of priority that messages
	 * must have in order to be logged.  Default to Priority::DEBUG
	 *
	 * @param facility The syslog facility argument used to specify 
	 * what type of program  is  logging  the  message.  This lets 
	 * the syslog configuration file specify that messages from different 
	 * facilities will be handled differently. For CARMA we have defined
	 * the following facilities:<br>
	 * <ul>
	 * <li>RX_FACILITY - The facility for receivers/antenna IF/dewar (local1)</li>
	 * <li>DEFAULT_FACILITY - The default facility (local2), use if 
	 *     no facility is specified. <i>logname</i> will default
	 *     to "carma.default"</li>
	 * <li>MONITOR_FACILITY - The facility for the Monitor and Fault
	 * Systems (local3). 
	 * <i>logname</i> will default * to "carma.monitor"
	 * </li>
	 * <li>INTERFEROMETRY_FACILITY - For the correlator, lobe rotator, and
	 * delay engine (local4)
	 * <i>logname</i> will default * to "carma.interferometry"
	 * </li>
	 * <li>CONTROL_FACILITY - For the control system and SAT. (local5)
	 * <i>logname</i> will default * to "carma.control"
	 * </li>
	 * <li>ENVIRONMENT_FACILITY - For Phase Monitor, WVR, 
	 * Weather Station, Linelength System, and LO Reference. (local6)
	 * <i>logname</i> will default * to "carma.environment"
	 * </li>
	 * </ul>
	 * Facilities local0 and local1 are reserved for future use; local7
	 * is already used by RedHat.  
	 *
	 * @see <tt>syslog(3)</tt> man page.
         * @see SyslogLayout
         * @see log4cpp::Category
         * @see log4cpp::SyslogAppender
         * @see log4cpp::RemoteSyslogAppender
         */
        static log4cpp::Category&
            getSyslogger(const std::string& identity,
                         const std::string& host = "localhost",
                         const std::string& logname = "carma.default",
			 log4cpp::Priority::PriorityLevel priority = 
			 log4cpp::Priority::DEBUG,
			 facilityType facility = DEFAULT_FACILITY);



        /**
         * @return a Category instance with a file appender attached. The
         * layout (format) of the log messages is that of FileLayout
         *
         * @param identity  The identity of the logger,
         * i.e. the program name. This corresponds the <code>ident</code> 
         * parameter in the <tt>openlog(3)</tt> call.
         *
         * @param pathname  The file to open. This corresponds
         * to pathname in <tt>open(2)</tt>.  
         *
         * @param logname  The hierarchical namespace for 
         * this logger, e.g. "carma.monitor".  The root logger will 
         * be "carma", all other loggers should be 
         * "carma.something[.somethingelse]" to make use of <br>
         * a) log4cpp's automatic heirarchical logging and <br>
         * b) for sorting in e.g. database queries.<br>
	 * The default is "carma" but you really should change it.
         *
         * @param append If the file already exists, 
         * <code>true</code> means to append to it, 
         * <code>false</code> means truncate it.
         * The file will be opened with default flag 
	 * <tt>O_CREAT | O_APPEND | O_WRONLY</tt>. 
	 *
	 * @param mode The file mode as is <tt>open(2)</tt>.
	 * The default mode is <tt>00644</tt> (file permission will be
	 * <tt>mode & ~umask</tt>).  
	 *
	 * @param priority The minimum level of priority that messages
	 * must have in order to be logged.  Default to Priority::DEBUG
	 *
	 * @see FileLayout
	 * @see log4cpp::Category
	 * @see log4cpp::FileAppender
	 */
	static log4cpp::Category&
	    getFilelogger(const std::string& identity,
			  const std::string& pathname,
			  const std::string& logname = "carma.default",
			  bool append = true,
			  mode_t mode = 00644,
			  log4cpp::Priority::PriorityLevel priority = 
			  log4cpp::Priority::DEBUG,
			  bool traceVerbose = true );


        /**
         * @return a Category instance with a ostream appender attached. The
         * layout (format) of the log messages is that of FileLayout.
         *
         * @param identity  The identity of the logger,
         * i.e. the program name. This corresponds the <code>ident</code> 
         * parameter in the <tt>openlog(3)</tt> call.
         *
         * @param stream Pointer to the output stream to append to, defaults to
	 * &std::cerr
	 *
         * @param logname  The hierarchical namespace for 
         * this logger, e.g. "carma.monitor".  The root logger will 
         * be "carma", all other loggers should be 
         * "carma.something[.somethingelse]" to make use of <br>
         * a) log4cpp's automatic heirarchical logging and <br>
         * b) for sorting in e.g. database queries.<br>
	 * The default is "carma.default" but you really should change it.
         *
	 * @param priority The minimum level of priority that messages
	 * must have in order to be logged.  Default to Priority::DEBUG
	 *
	 * @see FileLayout
	 * @see log4cpp::Category
	 * @see log4cpp::OstreamAppender
	 */
	static log4cpp::Category&
	    getOstreamlogger(const std::string& identity,
			     std::ostream *stream = &std::cout,
			     const std::string& logname = "carma.default",
			     log4cpp::Priority::PriorityLevel priority = 
			     log4cpp::Priority::DEBUG,
			     bool traceVerbose = true );

    };


    /**
     * Syslog layout class for CARMA logging. 
     */
    class SyslogLayout : public log4cpp::Layout 
    {

    public:

	/** Default constructor for SyslogLayout */
	SyslogLayout();

	/** Destructor for SyslogLayout */
	virtual ~SyslogLayout();

	/**
	 * Formats the LoggingEvent in SyslogLayout style:<br>
	 * "Time Identity: {Year} {Priority} {Category} 
	 * {Nested Diagnostic Context} 
	 * {Message}", where
	 * <br>Time is the timestamp added by the Linux syslog daemon,
	 * <br>Year is the current year, needed because syslog does
	 * not include year in its timestamp.
	 * <br>Identity is the parameter passed to getSysLogger(),
	 * <br>Nested Diagnostic Context is a context specific message,
	 * <br>Priority corresponds to the priority levels used by 
	 * <tt>syslog(3)</tt>
	 * 
	 * @param event The Logging Event generated by the Category method 
	 * which passed the original log message.
	 *
	 * @return The log message formatted according to SyslogLayout rules.
	 *
	 * @see carma::util::Time::computeCurrentFrame()
	 * @see log4cpp::LoggingEvent
         */
         virtual std::string format(const log4cpp::LoggingEvent& event);

    };


    /**
     * File layout class for CARMA logging. 
     */
    class FileLayout : public log4cpp::Layout 
    {

    public:

	/** Default constructor for FileLayout */
	FileLayout();

	/** Destructor for FileLayout */
	virtual ~FileLayout();

	/**
	 * Formats the LoggingEvent in FileLayout style:<br>
	 * "Time {Priority} {Category} {Nested Diagnostic Context} {Message}",
	 * where 
	 * <br> Time is a FITS timestamp:  yyyy-dd-mmThh:mm:ss.ss,
	 * <br> Identity is the parameter passed to getFilelogger(),
	 * <br>Nested Diagnostic Context is a context specific message,
	 * <br> Priority corresponds to the priority levels used by 
	 * <tt>syslog(3)</tt>, 
	 * 
	 * @param event The Logging Event generated by the Category method 
	 * which passed the original log message.
	 *
	 * @return The log message formatted according to FileLayout rules.
	 *
	 * @see log4cpp::LoggingEvent
	 */
         virtual std::string format(const log4cpp::LoggingEvent& event);

    };

    /**
     * Very Simple layout class for CARMA tracing. 
     */
    class VerySimpleLayout : public log4cpp::Layout 
    {

      public:

	/** Default constructor for VerySimpleLayout */
	VerySimpleLayout();

	/** Destructor for VerySimpleLayout */
	virtual ~VerySimpleLayout();

	/**
	 * Formats the LoggingEvent in VerySimpleLayout style:<br>
	 * "Name:NN: Message"
	 * where
	 * <br> Name is the file name, e.g. Program.cc or Program.h
	 * <br> NN is the line number
	 * <br> Message is the traced message
	 * 
	 * @param event The Logging Event generated by the Category method 
	 * which passed the original log message.
	 *
	 * @return The log message formatted according to VerySimpleLayout rules.
	 *
	 * @see log4cpp::LoggingEvent
	 */
	virtual std::string format(const log4cpp::LoggingEvent& event);

    };
  }
}
#endif // CARMA_UTIL_LOGGER_H
