
//
// @description 
// Trying to figure out weird Trace/Logger bug where log identity
// gets set to Trace:
// @usage Test program to ferret out bug
//
// @noKeys
//
// @logger DEFAULT_FACILITY carma.util.Test
//

#include <iostream>

#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace log4cpp;
using namespace carma::util;

int Program::main()
{
  getLogger().info("Program logger message before.");
  CARMA_CPTRACE(Trace::TRACE6,"This is the trace message before");

  /*
  log4cpp::Category& directLogger = 
      log4cpp::Category::getInstance("direct logger");
  directLogger.setPriority( log4cpp::Priority::INFO );
  log4cpp::SyslogAppender* sappender = new SyslogAppender("direct logger",
	  "tTraceLog direct identity", DEFAULT_FACILITY); 
  directLogger.addAppender(sappender);
  directLogger.info("Direct logger ONE instantiated");

  log4cpp::Category& directLoggerTwo = 
      log4cpp::Category::getInstance("direct logger two");
  directLoggerTwo.setPriority( log4cpp::Priority::INFO );
  log4cpp::RemoteSyslogAppender* sappenderTwo = 
      new RemoteSyslogAppender("direct logger two",
	  "tTraceLog direct identity two", "localhost", DEFAULT_FACILITY); 
  directLoggerTwo.addAppender(sappenderTwo);
  directLoggerTwo.info("Direct logger TWO instantiated");

  directLogger.info("Direct logger ONE after TWO instantiation");
  */


  /*
  Category& myLogger = Logger::getSyslogger(
	  "myIdentity #1",
	  "localhost",
	  "myLogName #1",
	  log4cpp::Priority::INFO,
	  DEFAULT_FACILITY
	  );
  myLogger.info("myLogger message before myOtherLogger instantiation.");
  Category& myOtherLogger = Logger::getSyslogger(
	  "myIdentity #2",
	  "localhost",
	  "myLogName #2",
	  log4cpp::Priority::INFO,
	  DEFAULT_FACILITY
	  );
  myLogger.info("myLogger message after myOtherLogger instantiation.");
  myOtherLogger.info("myOtherLoger message");
  */
  getLogger().info("Program logger message after.");
  CARMA_CPTRACE(Trace::TRACE6,"This is the trace message after");
  return 0;
}


