#ifndef SZA_UTIL_LOGGER_H
#define SZA_UTIL_LOGGER_H

/**
 * @file Logger.h
 * 
 * Tagged: Sun Mar 14 17:53:26 PST 2004
 * 
 * @author Erik Leitch
 */
#include <string>
#include <pthread.h> // Needed for POSIX thread function calls

#define LOG_HANDLER_FN(fn) void (fn)(std::string logStr)

#include "carma/szautil/LogFile.h"
#include "carma/szautil/Mutex.h"

namespace sza {
  namespace util {
    
    class LogStream;

    class Logger {
    public:
      
      /**
       * Destructor.
       */
      virtual ~Logger();
      
      /**
       * Log a message to the logfile
       */
      static void log(std::string message, bool isError);
      static void log(LogStream* ls);
      /**
       * Dispatch a message to handlers
       */
      static void dispatch(std::string message, bool isError);
      static void dispatch(LogStream* ls);

      /**
       * Report a message to both handlers and logfile
       */
      static void report(std::string message, bool isError);
      static void report(LogStream* ls);

      /**
       * Install a log-message handler.
       */
      static void installLogHandler(LOG_HANDLER_FN(*logHandler));

      /**
       * Install an error-message handler.
       */
      static void installErrHandler(LOG_HANDLER_FN(*errHandler));

      /**
       * Set a string to be prepended to every message we log.
       */
      static void setPrefix(std::string prefix);

      /**
       * Set the prefix for a logfile
       */
      static void setLogFilePrefix(const std::string& prefix);
      
      /**
       * Set the directory for a logfile
       */
      static void setLogFileDirectory(const std::string& dir);

      /**
       * Open the logfile
       */
      static void openLogFile();

      /**
       * Close the logfile
       */
      static void closeLogFile();

      /*
       * Install a default print function for printing to stdout
       */
      static void installStdoutPrintFn(LOG_HANDLER_FN(*stdoutPrintFn));
      static void installStderrPrintFn(LOG_HANDLER_FN(*stderrPrintFn));

      static void defaultStdoutPrintFn(std::string str);
      static void defaultStderrPrintFn(std::string str);

      static void printToStdout(std::string message);
      static void printToStderr(std::string message);

    private:

      /**
       * Private constructor prevents instantiation (singleton class)
       */
      Logger();

      static LOG_HANDLER_FN(*logHandler_);
      static LOG_HANDLER_FN(*errHandler_);

      static LOG_HANDLER_FN(*stdoutPrintFn_);
      static LOG_HANDLER_FN(*stderrPrintFn_);

      static std::string prefix_;

      static bool haveLogFile_;
      static LogFile logFile_;

      /**
       * We will use this mutex to ensure that calls to dispatch
       * complete atomically.
       */
      static Mutex guard_;
      static void write(const std::string& message, bool isError, bool dispatch, bool log);

    }; // End class Logger
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_LOGGER_H
