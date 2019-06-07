#ifndef CARMA_UTIL_TRACE_H
#define CARMA_UTIL_TRACE_H

//! @file
//! @brief Interface file for the carma::util::Trace class and related macros.
//!
//! @author Chul Gwon
//!


#include <sstream>

#include "carma/util/FacilityType.h"


namespace log4cpp {
    class Category;
}


// **** MACRO DEFINITIONS ****


//! @brief Macro for giving file and location information.
//!
//! @hideinitializer
#define CARMA_TRACELINE " From " << __FILE__ << ":" <<  __LINE__ << ": "


//! @brief Macro for Trace output.
//!
//! Need to pass through the Trace object as well as the desired output message
//!
//! Note: renamed os to carmaUtilTraceOss to avoid clashes with code
//!       that may use os as a local variable.
//! @hideinitializer
#define CARMA_TRACE(traceObject, priorityLevel, message)                   \
  {                                                                        \
    carma::util::Trace * const carmaUtilTraceTO = traceObject;             \
    const carma::util::Trace::TraceLevel carmaUtilTracePL = priorityLevel; \
                                                                           \
    if ( (carmaUtilTraceTO != 0) &&                                        \
         (carmaUtilTraceTO->willWrite( carmaUtilTracePL )) ) {             \
      ::std::ostringstream carmaUtilTraceOss;                              \
                                                                           \
      carmaUtilTraceOss << CARMA_TRACELINE << message;                     \
                                                                           \
      carmaUtilTraceTO->write( carmaUtilTracePL, carmaUtilTraceOss );      \
    }                                                                      \
  }


//! @brief Macro for people who just want to use the Trace object created for
//!        them in carma::util::Program.
//!
//! Note: renamed os to carmaUtilTraceOss to avoid clashes with code
//!       that may use os as a local variable.
//! @hideinitializer
#define CARMA_CPTRACE(priorityLevel, message)                              \
  {                                                                        \
    const carma::util::Trace::TraceLevel carmaUtilTracePL = priorityLevel; \
    carma::util::Trace * const carmaUtilTraceTO =                          \
      carma::util::Trace::getProgramTraceIfWillWrite( carmaUtilTracePL );  \
                                                                           \
    if ( carmaUtilTraceTO != 0 ) {                                         \
      ::std::ostringstream carmaUtilTraceOss;                              \
                                                                           \
      carmaUtilTraceOss << CARMA_TRACELINE << message;                     \
                                                                           \
      carmaUtilTraceTO->write( carmaUtilTracePL, carmaUtilTraceOss );      \
    }                                                                      \
  }


//! @brief Old deprecated macro name for CARMA_CPTRACE.
//!
//! @deprecated Please use CARMA_CPTRACE instead.
#define CPTRACE(priorityLevel, message) CARMA_CPTRACE(priorityLevel, message)


namespace carma {
  namespace util {

    /**
     * The Trace class provides an efficient means for
     * managing debug statements throughout CARMA code.  We
     * want to be able to trace execution of a program through output
     * statements, but be able to modify at compile and/or runtime
     * which statements get printed.  The Trace object would be
     * assigned a ``debug level'' via a command-line parameter.  Each
     * debug message would also be assigned a ``debug level'', so that
     * message generation will depend on the level of the Trace
     * object.  Compile-time switches would also be available to
     * reduce the runtime overhead.
     */

    class Trace {
    public:

      /**
       * These values are the indices for the private priorities
       * array, which corresponds directly to log4cpp/Priority.hh
       * TRACE0 corresponds to the most critical level trace messages
       * (the least verbose), TRACE7 the least critical (most verbose)
       * messages, usually used for intense debugging.
       */
      typedef enum {
          TRACE0   = 1,
          TRACE1   = 1,
          TRACE2   = 2,
          TRACE3   = 3,
          TRACE4   = 4,
          TRACE5   = 5,
          TRACE6   = 6,
          TRACE7   = 7,
          TRACEALL = 7  // Deprecated
      } TraceLevel;

      /**
       * Enumeration of possible output destinations.
       * STDOUT -> Standard output<br>
       * FILE   -> A file<br>
       * SYSLOG -> syslog<br>
       * SIMPLE -> Simplified output<br>
       */
      typedef enum {
          STDOUT,
          FILE,
          SYSLOG,
          SIMPLE
      } TraceDestination;

      /**
       * Constructor.
       * @param traceLevel - the trace level
       * @param traceDestination - where the output should go.
       * @param traceVerbose - print out trace info with full logging info
       * @param objectName - descriptive string for [the parent of] this Trace
       * @param facility - for syslog traces, the syslog facility
       * to use. This param is ignored for trace messages that
       * do not go to syslog.
       *
       * @see carma::util::Logger::getSyslogger()
       * @see carma::util::Logger::getFilelogger()
       * @see carma::util::Logger::getOstreamlogger()
       * @see syslog(3) manpage
       */
      Trace(TraceLevel traceLevel,
            const std::string& traceDestination, bool traceVerbose=true,
        const std::string& objectName="Trace",
        carma::util::facilityType facility = DEFAULT_FACILITY
        );

      virtual ~Trace();

      /**
       * @return A pointer to the default carma::util::Program Trace object
       */
      static carma::util::Trace * getProgramTrace();

      static carma::util::Trace * getProgramTraceIfAvailable();
      
      static carma::util::Trace *
      getProgramTraceIfWillWrite( TraceLevel traceLevel );

      /**
       * method allowing traceLevel to be modified
       * @param traceLevel - new level for Trace object
       */
      void setObjectTraceLevel(const TraceLevel traceLevel);

      // output methods
      /**
       * Send trace messages to standard output
       * @param debugMessage - a string containing the trace message
       */
      void writeStdout(const std::string& debugMessage);

      /**
       * Send trace messages to standard output
       * @param debugMessage - a std::ostringstream containing the trace message
       */
      void writeStdout(const std::ostringstream &debugMessage);

      /**
       * Send trace messages to a file.
       * @param debugMessage - a string containing the trace message
       */
      void writeFile(const std::string& debugMessage);

      /**
       * Send trace messages to a file.
       * @param debugMessage - a std::ostringstream containing the trace message
       */
      void writeFile(const std::ostringstream &debugMessage);

      /**
       * Send trace messages to syslog
       * @param debugMessage - a string containing the trace message
       */
      void writeSysLog(const std::string& debugMessage);

      /**
       * Send trace messages to syslog
       * @param debugMessage - a std::ostringstream containing the trace message
       */
      void writeSysLog(const std::ostringstream &debugMessage);

      /**
       * Generic output method, output will go to destination
       * specified in constructor.
       * @param traceLevel - the trace level
       * @param debugMessage - a string containing the trace message
       */
      void write(TraceLevel traceLevel, const std::string& debugMessage);

      /**
       * Generic output method, output will go to destination
       * specified in constructor.
       * @param traceLevel - the trace level
       * @param debugMessage - a std::ostringstream containing the trace message
       */
      void write(TraceLevel traceLevel, const std::ostringstream &debugMessage);

      /**
       * @return the destination of trace messages for this object,
       * as a TraceDestination enumeration value.
       */
      TraceDestination getDestination() const
      {
        return traceDestination_;
      }

      bool willWrite( TraceLevel traceLevel ) const;

    private:
      // used to obtain machine name for syslog category
      // must be declared early so that it is initialised early
      const ::std::string systemNodeName_;

      // destination where Trace output will be sent
      const TraceDestination traceDestination_;

      // trace level for the Trace object
      TraceLevel traceLevel_;

      // Set verbose in tracing, defaults to true
      // If false, uses log4cpp::SimpleLayout
      const bool traceVerbose_;

      // categories for sending trace information
      log4cpp::Category & sysLogCategory;
      log4cpp::Category & fileCategory;
      log4cpp::Category & ostreamCategory;

      // methods for actually writing to stdout/file/syslog
      void stdoutMethod(const std::string & debugMessage);
      void fileMethod(const std::string & debugMessage);
      void sysLogMethod(const std::string & debugMessage);
      void writeMethod(TraceLevel traceLevel, const std::string & debugMessage);

    }; // end class Trace


    // define output methods as inline functions
#ifdef NOTRACE
    inline void Trace::writeStdout(const std::string& debugMessage) {}
    inline void Trace::writeStdout(const std::ostringstream &debugMessage) {}
    inline void Trace::writeFile(const std::string& debugMessage) {}
    inline void Trace::writeFile(const std::ostringstream &debugMessage) {}
    inline void Trace::writeSysLog(const std::string& debugMessage) {}
    inline void Trace::writeSysLog(const std::ostringstream &debugMessage) {}
    inline void Trace::write(const std::string& debugMessage) {}
    inline void Trace::write(const std::ostringstream &debugMessage) {}
#else

    inline void Trace::writeStdout(const std::string& debugMessage)
    {
      stdoutMethod(debugMessage);
    }

    inline void Trace::writeStdout(const std::ostringstream &debugMessage)
    {
      stdoutMethod(debugMessage.str());
    }

    inline void Trace::writeFile(const std::string& debugMessage)
    {
      fileMethod(debugMessage);
    }

    inline void Trace::writeFile(const std::ostringstream &debugMessage)
    {
      fileMethod(debugMessage.str());
    }

    inline void Trace::writeSysLog(const std::string& debugMessage)
    {
      sysLogMethod(debugMessage);
    }

    inline void Trace::writeSysLog(const std::ostringstream &debugMessage)
    {
      sysLogMethod(debugMessage.str());
    }

    inline void Trace::write(TraceLevel traceLevel,
                         const std::string& debugMessage)
    {
      writeMethod(traceLevel, debugMessage);
    }

    inline void Trace::write(TraceLevel traceLevel,
                         const std::ostringstream &debugMessage)
    {
      writeMethod(traceLevel, debugMessage.str());
    }

#endif // #ifndef TRACE_ON

  } // end namespace util
} // end namespace carma

#endif // #ifndef CARMA_UTIL_TRACE_H
