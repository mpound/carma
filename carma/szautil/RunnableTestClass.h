// $Id: RunnableTestClass.h,v 1.1 2010/12/13 21:06:32 eml Exp $

#ifndef SZA_UTIL_RUNNABLETESTCLASS_H
#define SZA_UTIL_RUNNABLETESTCLASS_H

/**
 * @file RunnableTestClass.h
 * 
 * Tagged: Sat Jan  1 23:52:43 CST 2005
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:32 $
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Runnable.h"

namespace sza {
  namespace util {

    class RunnableTestClass : public Runnable {
    public:

      /**
       * Constructor.
       */
      RunnableTestClass(bool spawn);

      /**
       * Destructor.
       */
      virtual ~RunnableTestClass();

      static RUN_FN(runFn);

      void run();

    }; // End class RunnableTestClass

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RUNNABLETESTCLASS_H
