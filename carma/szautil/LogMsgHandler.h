// $Id: LogMsgHandler.h,v 1.1 2010/12/13 21:06:30 eml Exp $

#ifndef SZA_UTIL_LOGMSGHANDLER_H
#define SZA_UTIL_LOGMSGHANDLER_H

/**
 * @file LogMsgHandler.h
 * 
 * Tagged: Mon Feb 12 07:14:18 NZDT 2007
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:30 $
 * 
 * @author Erik Leitch
 */

#include <iostream>
#include <sstream>
#include <map>

#include "carma/szaarrayutils/lprintf.h"

#include "carma/szautil/Mutex.h"

namespace sza {
  namespace util {

    class LogMsgHandler {

    public:

      struct LogMsg {

	enum Type {
	  TYPE_UNSPEC,
	  TYPE_ERR,
	  TYPE_MESS
	};

	LogMsg(unsigned seq) {
	  seq_ = seq;
	  lastReadIndex_ = 0;
	  type_ = TYPE_UNSPEC;
	}

	~LogMsg() {}

	std::ostringstream os_;
	unsigned seq_;
	unsigned lastReadIndex_;
	Type type_;
      };

      /**
       * Constructor.
       */
      LogMsgHandler();

      /**
       * Destructor.
       */
      virtual ~LogMsgHandler();

      // Get the next unique sequence number

      unsigned nextSeq();

      // Append a string to an existing message

      void append(unsigned seq, std::string text, 
		  LogMsg::Type type=LogMsg::TYPE_UNSPEC);

      void append(unsigned seq, std::string text, 
		  sza::array::LogStream nature);

      void appendWithSpace(unsigned seq, std::string text, 
			   LogMsg::Type type=LogMsg::TYPE_UNSPEC);

      void appendWithSpace(unsigned seq, std::string text, 
			   sza::array::LogStream nature);

      // Get a tagged message

      std::string getMessage(unsigned seq);
      std::string readMessage(unsigned seq);

      std::string getNextMessageSubstr(unsigned seq, unsigned maxChars, bool& isLast);


    private:

      Mutex seqLock_;
      unsigned seq_;
      
      std::map<unsigned, LogMsg*> messages_;

      // Return the next message

      void eraseMessage(unsigned seq);

      // Return the next message

      LogMsg* findMessage(unsigned seq);

    }; // End class LogMsgHandler

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_LOGMSGHANDLER_H
