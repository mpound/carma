// $Id: tTrace.cc,v 1.16 2006/04/14 02:34:05 tcosta Exp $
// testTrace.cc - example of how to use Trace object
// @usage tTrace traceLevel=[0-8] traceFile=[filename, syslog, or stdout] localLevel=[0-8]
// @description
//   a quick test to see if Trace works the way it is expected to in
//   conjunction with carma::util::Program.  
//   - traceLevel sets the threshold for messages that will be output by the Trace object
//   - localLevel is the debug level for the messages in the test
//   (if this is greater than traceLevel, or greater than 7, you will see no output)
//   - traceFile gives the destination where you want the debug output to go
// @key   localLevel   8       i    level of output for this file
//
// @logger TEST_FACILITY carma.test.util.tTrace
//

#include <iostream>
#include "carma/util/Trace.h"
#include "carma/util/Program.h"

int outsideFunctionTest(carma::util::Trace::TraceLevel level) {
  // CARMA_CPTRACE uses Program::getTraceObject()
  CARMA_CPTRACE(level,"CARMA_CPTRACE in external function works");

  return 0;
}

int anotherOutsideFunctionTest(carma::util::Trace::TraceLevel level) {
  // unlike above, try to use Trace::getProgramTrace() instead
  CARMA_TRACE(carma::util::Trace::getProgramTrace(),
	      level,
	      "CARMA_TRACE using getProgramTrace() works");
  return 0;
}


int
carma::util::Program::main( ) {
  const carma::util::Trace::TraceLevel localLevel = 
    static_cast< carma::util::Trace::TraceLevel >( getIntParameter( "localLevel" ) );

  CARMA_CPTRACE(localLevel,
              "something" << 
              "is" <<
              "up");

  outsideFunctionTest(localLevel);

  carma::util::Trace *myTrace = 
    new carma::util::Trace(carma::util::Trace::TRACEALL,
			   "stdout",
			   "myNewCategories");

  anotherOutsideFunctionTest(localLevel);

  CARMA_TRACE(myTrace,
	      localLevel,
	      "CARMA_TRACE using myNewCategories works");

  return 0;
}
