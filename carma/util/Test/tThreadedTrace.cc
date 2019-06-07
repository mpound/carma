// $Id: tThreadedTrace.cc,v 1.3 2004/12/01 19:45:32 tcosta Exp $
// tThreadedTrace.cc - test for Trace object in threaded environment
// @usage tThreadedTrace traceLevel=[0-8] traceFile=[filename, syslog, or stdout]
//

#include <iostream>
#include "carma/util/Trace.h"
#include "carma/util/Program.h"

#include <pthread.h>

#define NTHREADS 5 // number of threads we want for our test

void *testTrace(void *localTraceLevel) {

  // force pointer value into an int ... maybe not the safest thing to
  // do, but works for the time being.
  int iLocalLevel = (int)localTraceLevel;

  carma::util::Trace::TraceLevel localLevel = 
    (carma::util::Trace::TraceLevel) iLocalLevel;

  for (int i=0; i < 3; i++) {
    CARMA_CPTRACE(localLevel,
	    "something" << 
	    " is" <<
	    " up with " <<
	    localLevel);
  }

  pthread_exit(NULL);
}

int carma::util::Program::main() {

  pthread_t testThreads[NTHREADS];
  int returnCode;

  for (int i = 0; i < NTHREADS; i++) {
    // set index i as a pointer value ... do not use &i
    returnCode = pthread_create(&testThreads[i], NULL, testTrace, (void*)i);
    if (returnCode) {
      CARMA_CPTRACE((carma::util::Trace::TraceLevel)i,
	      "Error from pthread_create for thread " << i);
      return 1;
    }
  }

  for (int i = 0; i < NTHREADS; i++) {
    returnCode = pthread_join(testThreads[i], NULL);
    if (returnCode) {
      CARMA_CPTRACE((carma::util::Trace::TraceLevel)i,
	      "Error from pthread_join for thread " << i);
      return 1;
    }
  }
  
  return 0;
}
