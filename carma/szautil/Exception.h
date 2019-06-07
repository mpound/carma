#ifndef EXCEPTION_H
#define EXCEPTION_H

/**
 * @file Exception.h
 * 
 * Tagged: Fri Nov 14 12:39:33 UTC 2003
 * 
 * @author Erik Leitch
 */
// System includes

#include <iostream>
#include <sstream>
#include <string>

#include "carma/szautil/ErrHandler.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/IoLock.h"
#include "carma/szautil/Logger.h"
#include "carma/szautil/XtermManip.h"

// Create an Exception class in namespace carma::antenna::sza

namespace sza {
  namespace util {

    class LogStream;

    class Exception {
      
    public:
      
      /**
       * Construct an Exception with a detailed message.
       *
       * @param str String describing error.
       * @param filename where exception originated.
       * @param lineNumber exception occurred on.
       */
      Exception(std::string str, const char * filename,  
		const int lineNumber, bool report);
      
      /**
       * Construct an Error with a detailed message.
       * 
       * @param os ostringstream containing message
       * @param filename where exception originated.
       * @param lineNumber exception occurred on.
       */
      Exception(std::ostringstream& os, const char * filename, 
		const int lineNumber, bool report);
      
      /** 
       * Constructor with an LogStream&.
       */
      Exception(sza::util::LogStream& ls, 
		const char* filename, const int lineNumber, bool report);
      
      /** 
       * Constructor with an LogStream*.
       */
      Exception(sza::util::LogStream* ls, 
		const char* filename, const int lineNumber, bool report);
      

      /**
       * Destructor
       */
      virtual ~Exception();

      /**
       * Report error to standard err.
       * Reports error to standard error by printing the
       * error message, filename and line number.
       */
      inline void report() {}      

      /**
       * Report error to standard err.
       * Reports error to standard error by printing the
       * error message, filename and line number.
       */
      inline void report(std::string& what) 
	{
	  sza::util::IoLock::lockCerr();
	  std::cerr << what;
	  sza::util::IoLock::unlockCerr();
	}      

      /**
       * Report error to standard err.
       * Reports error to standard error by printing the
       * error message, filename and line number.
       */
      inline void report(std::string what) 
	{
	  sza::util::IoLock::lockCerr();
	  std::cerr << what;
	  sza::util::IoLock::unlockCerr();
	}      

      inline const char* what() { 
	return message_.c_str();
      }
      
    private: 
      
      std::string message_;

    }; // End class Exception
    
  } // namespace util
} // namespace sza

#define Error(x) sza::util::Exception((x), __FILE__, __LINE__, true)
#define ErrorNoReport(x) sza::util::Exception((x), __FILE__, __LINE__, false)
#define ErrorDef(x,y) sza::util::Exception (x)((y), __FILE__, __LINE__, true)

#ifdef ThrowError
#undef ThrowError
#endif

#define ThrowError(text) \
{\
  std::ostringstream _macroOs; \
  _macroOs << text;\
  sza::util::ErrHandler::throwError(_macroOs, __FILE__, __LINE__, __PRETTY_FUNCTION__, true, false, false);\
}

#ifdef ThrowColorError
#undef ThrowColorError
#endif

#define ThrowColorError(text, color)			\
{\
  XtermManip _macroXtm;\
  std::ostringstream _macroOs; \
  _macroOs << _macroXtm.bg("black") << _macroXtm.fg(color) << _macroXtm.textMode("bold");\
  _macroOs << text;\
  _macroOs << _macroXtm.bg("default") << _macroXtm.fg("default") << _macroXtm.textMode("normal");\
  sza::util::ErrHandler::throwError(_macroOs, __FILE__, __LINE__, __PRETTY_FUNCTION__, true, false, false);\
}

#ifdef ThrowSimpleError
#undef ThrowSimpleError
#endif

#define ThrowSimpleError(text) \
{\
  std::ostringstream _macroOs; \
  _macroOs << text;\
  sza::util::ErrHandler::throwError(_macroOs, __FILE__, __LINE__, __PRETTY_FUNCTION__, true, true, false);\
}

#ifdef ThrowSysError
#undef ThrowSysError
#endif

#define ThrowSysError(text) \
{\
  std::ostringstream _macroOs; \
  _macroOs << text;\
  sza::util::ErrHandler::throwError(_macroOs, __FILE__, __LINE__, __PRETTY_FUNCTION__, true, false, true);\
}

#ifdef ReportError
#undef ReportError
#endif

#define ReportError(text) \
{\
  std::ostringstream _macroOs; \
  _macroOs << text;\
  sza::util::ErrHandler::report(_macroOs, __FILE__, __LINE__, __PRETTY_FUNCTION__, true, false, false);\
}

#ifdef ReportSimpleError
#undef ReportSimpleError
#endif

#define ReportSimpleError(text) \
{\
  std::ostringstream _macroOs; \
  _macroOs << text;\
  sza::util::ErrHandler::report(_macroOs, __FILE__, __LINE__, __PRETTY_FUNCTION__, true, true, false);\
}

#ifdef ReportSysError
#undef ReportSysError
#endif

#define ReportSysError(text) \
{\
  std::ostringstream _macroOs; \
  _macroOs << text;\
  sza::util::ErrHandler::report(_macroOs, __FILE__, __LINE__, __PRETTY_FUNCTION__, true, false, true);\
}

#ifdef ReportMessage
#undef ReportMessage
#endif

#define ReportMessage(text) \
{\
  std::ostringstream _macroOs; \
  _macroOs << text;\
  sza::util::ErrHandler::report(_macroOs, __FILE__, __LINE__, __PRETTY_FUNCTION__, false, true, false);\
}

#ifdef LogMessage
#undef LogMessage
#endif

#define LogMessage(error, text) \
{\
  std::ostringstream _macroOs; \
  _macroOs << text;\
  sza::util::ErrHandler::log(_macroOs, __FILE__, __LINE__, __PRETTY_FUNCTION__, false, true, false);\
}

#define COUT(statement) \
{\
    std::ostringstream _macroOs; \
    _macroOs << statement << std::endl; \
    sza::util::Logger::printToStdout(_macroOs.str()); \
}

#define COUTCOLOR(statement, color)			\
{\
  XtermManip _macroXtm;\
  std::ostringstream _macroOs;		\
  _macroOs << _macroXtm.bg("black") << _macroXtm.fg(color) << _macroXtm.textMode("bold");\
  _macroOs << statement << std::endl; \
  _macroOs << _macroXtm.bg("default") << _macroXtm.fg("default") << _macroXtm.textMode("normal");\
  sza::util::Logger::printToStdout(_macroOs.str()); \
}

#define COUTCOLORNNL(statement, color) \
  {\
    sza::util::XtermManip _macroXtm; \
    std::ostringstream _macroOs; \
    _macroOs << _macroXtm.bg("black") << _macroXtm.fg(color) << _macroXtm.textMode("bold");\
    _macroOs << statement;      \
    _macroOs << _macroXtm.bg("default") << _macroXtm.fg("default") << _macroXtm.textMode("normal");\
    sza::util::Logger::printToStdout(_macroOs.str());			\
  }

#define CTOUT(statement) \
{\
    sza::util::TimeVal _macroTimeVal;\
    _macroTimeVal.setToCurrentTime();\
    std::ostringstream _macroOs; \
    _macroOs << _macroTimeVal << ": " << statement << std::endl; \
    sza::util::Logger::printToStdout(_macroOs.str()); \
}

#define CERR(statement) \
{\
    std::ostringstream _macroOs; \
    _macroOs << statement << std::endl; \
    sza::util::Logger::printToStderr(_macroOs.str()); \
}

#define CTERR(statement) \
{\
    sza::util::TimeVal _macroTimeVal;\
    _macroTimeVal.setToCurrentTime();\
    std::ostringstream _macroOs; \
    _macroOs << _macroTimeVal << ": " << statement << std::endl; \
    sza::util::Logger::printToStderr(_macroOs.str()); \
}

#endif
