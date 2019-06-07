//
// @file        syslog2carmalog.cc
// @version     $Id: SyslogListenerManager.h,v 1.1 2007/11/21 12:01:59 colby Exp $
// @author      Colby Gutierrez-Kraybill
//
// @description
//


#ifndef CARMA_DBMS_SYSLOGLISTENERMANAGER_H
#define CARMA_DBMS_SYSLOGLISTENERMANAGER_H

// System includes
#include <string>
#include <queue>
#include <unistd.h>

// CARMA tools includes
#include "log4cpp/Category.hh"

// CARMA software includes
#include "carma/util/Time.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadCond.h"

#include "carma/dbms/SyslogListenerThread.h"

namespace carma
{
  namespace dbms
  {
    class SyslogListenerManager 
    {
      public:
        SyslogListenerManager ( std::string pipeFileName );

        static void thread( SyslogListenerManager &This );
        void run();
        void stop();

	double getStartTime() { return _timeOfStart; };
	long long getMessageCount() { return _messageCountFromStartOfRun; };
	long long getTotalBytes() { return _totalBytesReceived; };

	void ready();
	std::string *waitForNextMessage();

      private:
        log4cpp::Category &_log;
	std::vector<std::string> _pipeFileNames;

	// book keeping
	double _timeOfStart; // in MJD
	long long _messageCountFromStartOfRun;
	long long _totalBytesReceived;
	SyslogListenerManager *_myThread;
	int _pipeFD;

	std::map< std::string, SyslogListenerThread * > _listeners;

	static std::queue<std::string * > *_rawBufferQueue;

	static carma::util::PthreadMutex *_queueGuard;
	static carma::util::PthreadMutex *_readyGuard;
	static carma::util::PthreadCond *_readyCond;

    }; // class SyslogListenerManager
  } // namespace dbms
} // namespace carma

#endif //  CARMA_DBMS_SYSLOGLISTENERMANAGER_H
