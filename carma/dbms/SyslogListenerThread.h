//
// @file        syslog2carmalog.cc
// @version     $Id: SyslogListenerThread.h,v 1.3 2007/11/28 07:57:53 colby Exp $
// @author      Colby Gutierrez-Kraybill
//
// @description
//


#ifndef CARMA_DBMS_SYSLOGLISTENERTHREAD_H
#define CARMA_DBMS_SYSLOGLISTENERTHREAD_H

// System includes
#include <string>
#include <queue>
#include <unistd.h>

// CARMA tools includes
#include "log4cpp/Category.hh"

// CARMA software includes
#include "carma/util/Time.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadCond.h"

namespace carma
{
  namespace dbms
  {
    class SyslogListenerThread 
    {
      public:
        SyslogListenerThread (
	    std::string pipeFileName,
	    std::queue<std::string * > *rawBufferQueue,
	    carma::util::PthreadMutex *queueGuard,
	    carma::util::PthreadMutex *readyGuard,
	    carma::util::PthreadCond *readyCond
	    );

        static void thread( SyslogListenerThread &This );
        void run();
        void stop();

	double getStartTime() { return _timeOfStart; };
	long long getMessageCount() { return _messageCountFromStartOfRun; };
	long long getTotalBytes() { return _totalBytesReceived; };

	void ready();
	std::string *waitForNextMessage();

      private:
        log4cpp::Category &_log;
	std::string _pipeFileName;

	// book keeping
	double _timeOfStart; // in MJD
	long long _messageCountFromStartOfRun;
	long long _totalBytesReceived;
	SyslogListenerThread *_myThread;
	int _pipeFD;

	std::string _logName;
	std::string::size_type _newMsgReset;

	// passed in from SyslogListenerManager
	std::queue<std::string * > *_rawBufferQueue;
	carma::util::PthreadMutex *_queueGuard;
	carma::util::PthreadMutex *_readyGuard;
	carma::util::PthreadCond *_readyCond;

    }; // class SyslogListenerThread
  } // namespace dbms
} // namespace carma

#endif //  CARMA_DBMS_SYSLOGLISTENERTHREAD_H
