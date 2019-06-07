// $Id: SystemStatusSubsystemMutex.h,v 1.1 2011/07/19 20:53:21 eml Exp $

#ifndef SZA_UTIL_SYSTEMSTATUSSUBSYSTEMMUTEX_H
#define SZA_UTIL_SYSTEMSTATUSSUBSYSTEMMUTEX_H

/**
 * @file SystemStatusSubsystemMutex.h
 * 
 * Tagged: Wed Jul 13 13:56:11 PDT 2011
 * 
 * @version: $Revision: 1.1 $, $Date: 2011/07/19 20:53:21 $
 * 
 * @author username: Command not found.
 */
#include "carma/monitor/SystemStatus.h"

#include "carma/szautil/Mutex.h"

namespace sza {
  namespace util {

    class SystemStatusSubsystemMutex {
    public:

      /**
       * Constructor.
       */
      SystemStatusSubsystemMutex();

      /**
       * Destructor.
       */
      virtual ~SystemStatusSubsystemMutex();

      void lock();
      void unlock();

    public:

      carma::monitor::SystemStatusSubsystem* ss_;
      Mutex guard_;

    }; // End class SystemStatusSubsystemMutex

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SYSTEMSTATUSSUBSYSTEMMUTEX_H
