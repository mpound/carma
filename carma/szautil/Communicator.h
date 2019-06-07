#ifndef SZA_UTIL_COMMUNICATOR_H
#define SZA_UTIL_COMMUNICATOR_H

/**
 * @file Communicator.h
 * 
 * Tagged: Mon Jul 19 14:47:35 PDT 2004
 * 
 * @author Erik Leitch
 */
#include <list>
#include <string>
#include <sstream>

#include "carma/szautil/AntNum.h"
#include "carma/szautil/String.h"
#include "carma/szautil/TcpClient.h"
#include "carma/szautil/TimeVal.h"

#define COMM_PARSER_FN(fn) void (fn)(sza::util::String& str, void* arg)
#define COMM_END_PARSER_FN(fn) void (fn)(void* arg)

#define LOG_OUTPUT

#ifdef LOG_OUTPUT
#include <fstream>
#endif

namespace sza {
namespace util {
    
    class Communicator {
    public:
      
#ifdef LOG_OUTPUT
      std::ofstream* fout_;
#endif

      // A generalized object for managing strings received from the
      // power strips.  If a parser is registered, it will be called
      // to process the string

      struct RcvdStr {
	std::string start_;
	std::string stop_;
	COMM_PARSER_FN(*parser_);
	void* arg_;
	COMM_END_PARSER_FN(*endParser_);
	void* endArg_;
	bool matchAny_;
	std::ostringstream os;

	RcvdStr();
	RcvdStr(const RcvdStr& str);
	RcvdStr(std::string str, COMM_PARSER_FN(*parser)=0, void* arg=0);
	RcvdStr(std::string start, std::string stop, 
		COMM_PARSER_FN(*parser), void* arg, 
		COMM_END_PARSER_FN(*endParser), void* endArg);

	void initialize();
      };

      /**
       * Constructor.
       */
      Communicator();
      
      /**
       * Destructor.
       */
      virtual ~Communicator();
      
      // The number of seconds we will wait for a response

      static const unsigned COMMAND_TIMEOUT_SEC = 10;

      /**
       * Return true if this communicator timed out waiting for an
       * appropriate response to the last sent string
      */
      bool timedOut();

      /**
       * Return the fd associated with this communicator
       */
      virtual int getFd();

      // Write a string to the server

      virtual void writeString(std::string);

      // Concatenate a string received from the server

      virtual void concatenateString(std::ostringstream& os);

      void run();

      /**
       * Read a line from the power strip and determine what to do
       */
      virtual void processClientMessage();
      
      /**
       * React to a failure on the part of the power strip to reply
       */
      void registerTimeOut();
      
    protected:
      
      // An object for managing the TCP/IP connection to the power strip

      sza::util::TcpClient* client_;

      // A timer we will use for keeping track of timeouts

      sza::util::TimeVal timer_;
      
      /**
       * A stack of command/responses for communicating with the strip
       * via telnet.
       */
      std::list<std::string> sentStrings_;
      std::list<RcvdStr> rcvdStrings_;
      
      /**
       * A pointer to the current element of the above lists
       */
      std::list<std::string>::iterator sentStringIter_;
      std::list<RcvdStr>::iterator rcvdStringIter_;
      
      std::ostringstream os_;

      /**
       * Terminate a command sequence
       */
      virtual void terminateCommSequence(bool error);
      
      /**
       * Set a timeout for waiting for a response from the strip.
       */
      void enableTimeOut(bool enable);
      
      void checkLine();

      static COMM_PARSER_FN(sendNextString);
      void sendNextString();

      virtual void execSendNextString();

      void doSomething();

      /**
       * Reset in preparation for searching for the next string
       */
      void advanceIterator(bool bufferReset);
	
      void checkIterators();

    }; // End class Communicator
    
} // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_COMMUNICATOR_H
