#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include <cstdio>
#include <iostream>
#include <syslog.h>

using namespace std;
using namespace carma::util;

//
// @version	$Revision: 1.2 $
//
// @usage   tLoggerPerf - test the performance of Logger vs. straight syslog
// @key     direct   false  bool Use direct syslog(3) call rather than log4cpp
// @key     nloop     1     int  Number of times to loop 
// @description test to see the performance problem Peter complained about.
// This is done by sending a message to syslog via Logger/log4cpp or by
// a direct call to syslog(3).   The loop will run nloop times, so
// you can run time(1) and divide by nloop.
//
// @logger DEFAULT_FACILITY carma.util.Test


int Program::main()
{

    log4cpp::Category& d = Program::getLogger();
    bool direct = getBoolParameter("direct");
    int  nLoop  = getIntParameter("nloop");
    if (direct) {
	openlog("tLoggerPerf (direct)",0, LOG_LOCAL2);
	for (int i = 0 ; i < nLoop ; i++ ) {
	    std::ostringstream os;
	    os << "This is message number :" << i;
	    syslog( LOG_INFO | LOG_LOCAL2, "%s",os.str().c_str() );
	}
	closelog();
    } else {
	for (int i = 0 ; i < nLoop ; i++ ) {
	    std::ostringstream os;
	    os << "This is message number :" << i;
	    d.info(os.str());
	}
    }


    return EXIT_SUCCESS;
}


