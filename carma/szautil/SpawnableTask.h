// $Id: SpawnableTask.h,v 1.3 2013/07/09 17:12:00 eml Exp $

#ifndef SZA_UTIL_SPAWNABLETASK_H
#define SZA_UTIL_SPAWNABLETASK_H

/**
 * @file SpawnableTask.h
 * 
 * Tagged: Fri Jan 26 16:49:57 NZDT 2007
 * 
 * @version: $Revision: 1.3 $, $Date: 2013/07/09 17:12:00 $
 * 
 * @author Erik Leitch
 */

#include "carma/szautil/GenericTask.h"
#include "carma/szautil/RunnableTask.h"
#include "carma/szautil/Exception.h"

#include "carma/util//ErrorException.h"

#include <iostream>

namespace sza {
  namespace util {

    /**.......................................................................
     * Define a template class for an object which can run in its own
     * thread.  This inherits the message queue mechanism from
     * GenericTask for communciation with this thread, and spawnable
     * capabilities from Runnable
     */
    template <class Msg>
      class SpawnableTask : public sza::util::GenericTask<Msg>,
      public RunnableTask {

      public:

      /**
       * Constructor.  If spawn==true, then a call to spawn() will
       * start this thread
       */
      SpawnableTask(bool spawn) : RunnableTask(spawn) {}

      /**
       * Destructor.
       */
      virtual ~SpawnableTask() {};

      protected:
      
      // All the work in this class is done by processMsg().  
      //
      // Inheritors need only define what this function does for
      // different message types, and the rest will run itself.

      virtual void processMsg(Msg* msg) {};

    public:

      void run() {
	try {
	  GenericTask<Msg>::run();
	} catch(Exception& err) {
	  COUT("Caught an exception: " << err.what());
	} catch(carma::util::ErrorException& err) {
	  COUT("Caught a CARMA exception: " << err.what());
	} catch(...) {
	  COUT("Caught an unknown exception (ST 0)");
	}
      }

    }; // End class SpawnableTask

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_SPAWNABLETASK_H
