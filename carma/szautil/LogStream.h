#ifndef SZA_UTIL_LOGSTREAM_H
#define SZA_UTIL_LOGSTREAM_H

#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <iostream>
#include <sstream>
#include <string>
#include <cstring>

/**
 * @file LogStream.h
 * 
 * Started: Sun Dec 14 07:19:50 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class LogStream : public std::ostringstream {
    public:
      
      /**
       * Constructor.
       */
      LogStream();
      
      /**
       * Constructor with message.
       */
      LogStream(std::string message, std::string fileName, 
		int lineNo, std::string functionName,
		bool isError, bool usePrefix);
      
      /**
       * Constructor with stream.
       */
      LogStream(std::ostringstream os, std::string fileName, 
		int lineNo, std::string functionName,
		bool isError, bool usePrefix);
      
      /**
       * Destructor.
       */
      virtual ~LogStream();
      
      /**
       * Set a message to the stream.
       */
      void setLogStreamMessage(std::string message,
			       std::string fileName, int lineNo, 
			       std::string functionName,
			       bool isError,
			       bool usePrefix);
      
      /**
       * Set the message.
       */
      void setLogStreamMessage(std::ostringstream& os,
			       std::string fileName, int lineNo, 
			       std::string functionName,
			       bool isError,
			       bool usePrefix);
      
      /**
       * Append a message to the stream.
       */
      void appendLogStreamMessage(std::string message, 
				  std::string fileName,
				  int lineNo, 
				  std::string functionName,
				  bool isError,
				  bool usePrefix);
      
      /**
       * Append a message to the stream.
       */
      void appendLogStreamMessage(std::ostringstream& os,
				  std::string fileName, int lineNo, 
				  std::string functionName,
				  bool isError,
				  bool usePrefix);
      /**
       * Initialize a message.
       */
      void initLogStreamMessage(std::string fileName, int lineNo, 
				std::string functionName,
				bool isError,
				bool usePrefix);
      
      /**
       * Set the message to the last system error message.
       */
      void setSysLogStreamError(std::string sysFunction, 
				std::string fileName, 
				int lineNo, 
				std::string functionName,
				bool isError,
				bool usePrefix);
      /**
       * Append a system error message to the stream.
       */
      void appendSysLogStreamError(std::string sysFunction, 
				   std::string fileName, 
				   int lineNo, 
				   std::string functionName,
				   bool isError,
				   bool usePrefix);
      /**
       * Return the message as a string.
       */
      std::string getMessage();
      
      /**
       * Return true if this is an error message.
       */
      bool isError();
      
      /**
       * Clear the error status and message string.
       */
      void clear();
      
      /**
       * Append an end-of-string token to the string.
       */
      void finish();
      
      /**
       * Report a message.
       */
      void report();

      /**
       * Log a message
       */
      void log();

    private:
      
      bool isError_;

      void addPrefix(int lineNo, std::string functionName);

    }; // End class LogStream
    
  }; // End namespace util
}; // End namespace sza

// These have to be macros, or else the FILE, LINE, etc. macros won't
// work.  Ie, if __FILE__, etc. were simply set as default arguments
// to setMessage() for example, the expanded macros would refer to the
// context in this file, not the context where the call is made,
// defeating the purpose of those macros!

#ifdef LogStr
#undef LogStr
#endif

#define LogStr(isErr, message) LogStream((message), __FILE__, __LINE__, \
__PRETTY_FUNCTION__, isErr, true)

#ifdef setMessage
#undef setMessage
#endif

#define setMessage(isErr, message) setLogStreamMessage((message), \
__FILE__, __LINE__, __PRETTY_FUNCTION__, isErr, true)

#ifdef appendMessage
#undef appendMessage
#endif

#define appendMessage(isErr, message) appendLogStreamMessage((message), \
__FILE__, __LINE__, __PRETTY_FUNCTION__, isErr, true)

#ifdef initMessage
#undef initMessage
#endif

#define initMessage(isErr) initLogStreamMessage(__FILE__, __LINE__, \
__PRETTY_FUNCTION__, isErr, true)

#ifdef setSysError
#undef setSysError
#endif

#define setSysError(isErr, sysFunction) setSysLogStreamError((sysFunction), \
__FILE__, __LINE__, __PRETTY_FUNCTION__, isErr, true)

#ifdef appendSysError
#undef appendSysError
#endif

#define appendSysError(isErr, sysFunction) \
appendSysLogStreamError((sysFunction), __FILE__, __LINE__, \
__PRETTY_FUNCTION__, isErr, true)

#ifdef LogStrSimple
#undef LogStrSimple
#endif

#define LogStrSimple(isErr, message) LogStream((message), __FILE__, __LINE__, \
__PRETTY_FUNCTION__, isErr, false)

#ifdef setMessageSimple
#undef setMessageSimple
#endif

#define setMessageSimple(isErr, message) setLogStreamMessage((message), \
__FILE__, __LINE__, __PRETTY_FUNCTION__, isErr, false)

#ifdef appendMessageSimple
#undef appendMessageSimple
#endif

#define appendMessageSimple(isErr, message) appendLogStreamMessage((message), \
__FILE__, __LINE__, __PRETTY_FUNCTION__, isErr, false)

#ifdef initMessageSimple
#undef initMessageSimple
#endif

#define initMessageSimple(isErr) initLogStreamMessage(__FILE__, __LINE__, \
__PRETTY_FUNCTION__, isErr, false)

#ifdef setSysErrorSimple
#undef setSysErrorSimple
#endif

#define setSysErrorSimple(isErr, sysFunction) setSysLogStreamError((sysFunction), \
__FILE__, __LINE__, __PRETTY_FUNCTION__, isErr, false)

#ifdef appendSysErrorSimple
#undef appendSysErrorSimple
#endif

#define appendSysErrorSimple(isErr, sysFunction) \
appendSysLogStreamError((sysFunction), __FILE__, __LINE__, \
__PRETTY_FUNCTION__, isErr, false)

#endif // End #ifndef 
