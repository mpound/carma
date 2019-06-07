// $Id: TimeOut.h,v 1.2 2011/07/19 20:53:21 eml Exp $

#ifndef SZA_UTIL_TIMEOUT_H
#define SZA_UTIL_TIMEOUT_H

/**
 * @file TimeOut.h
 * 
 * Tagged: Tue May  2 16:31:46 PDT 2006
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/07/19 20:53:21 $
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/TimeVal.h"

namespace sza {
  namespace util {

    class TimeOut {
    public:

      // Default interval 

      static const unsigned int defaultInterval_ = 5*60;

      // Public methods

      TimeOut();

      void setIntervalInSeconds(unsigned int seconds);
      void setInterval(unsigned int seconds, unsigned nanoSeconds);

      void activate(bool active);

      void reset();

      struct timeval* tVal();

      TimeVal timeOut_; // The timeval managed by this class

    private:

      bool active_;
      bool resetPending_;
      
    }; // End class TimeOut

  } // End namespace util
} // End namespace sza


#endif // End #ifndef SZA_UTIL_TIMEOUT_H
