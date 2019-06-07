#ifndef SZA_UTIL_LOGFILE_H
#define SZA_UTIL_LOGFILE_H

/**
 * @file LogFile.h
 * 
 * Tagged: Mon May 10 17:23:41 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <string>
#include <sstream>

#include "carma/szautil/TimeVal.h"
#include "carma/szautil/LogFile.h"

namespace sza {
  namespace util {
    
    class LogFile {

    public:
      
      /**
       * Constructor.
       */
      LogFile();
      
      /**
       * Destructor.
       */
      virtual ~LogFile();
      
      /**
       * Set the destination directory for logfiles created by this
       * object.
       */
      void setDirectory(const std::string& dir);

      /**
       * Set the prefix for logfiles created by this object.
       */
      void setPrefix(const std::string& prefix);

      /**
       * Create a date_based name for this file
       */
      void setDatePrefix();

      /**
       * Open a logfile
       */
      void open();

      /**
       * Close a logfile
       */
      void close();

      void flush();

      /**
       * Return true if a file exists
       */
      bool fileExists(std::string fileName);

      /*
       * Create a unique file name based on the prefix and files
       * already in the log directory
       */
      std::string newFileName();

      /**
       * Public method to write a string to our log file
       */
      void prepend(std::string message);

      /**
       * Public method to write a string to our log file
       */
      void append(std::string message);

      /**
       * Return a reference to our logFile object
       */
      LogFile& logFile();

    private:

      // Define a maximum version number for log files

      static const int MAX_VERSION;

      // A prefix to name log files

      std::string prefix_;
      bool datePrefix_;

      // The directory in which to write log files

      std::string directory_;

      // The file descriptor associated with our log file

      FILE* file_;

      // A message to prepend to any line we write to the log file

      std::string prependStr_;

      // The buffer into which we will accumulate data to be written
      // to a file

      std::ostringstream lineBuffer_;

      /**
       * Initialize the line buffer
       */
      void initBuffer();

      /**
       * Write a line to the logfile
       */
      void putLine();

      /**
       * Write a string to the file
       */
      void write(std::string line);


    }; // End class LogFile
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_LOGFILE_H
