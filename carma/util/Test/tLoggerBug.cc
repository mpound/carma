#include "carma/util/Program.h"
#include "carma/util/Logger.h"
#include <cstdio>
#include <iostream>

using namespace std;
using namespace carma::util;


//
// @version $Revision: 1.5 $
//
// @usage tLoggerBug
//
// @description test the Logger bug that sends multiple output messages
//
// @noKeys
//
// @logger DEFAULT_FACILITY carma.util.Test
//

int
Program::main() {
    /*
    // get a Category named "i'm buggy"
    log4cpp::Category& a = Logger::getFilelogger("test","/tmp/foo", "i'm buggy",true);
    a.info("This space for rent");

    // get the same Category named "i'm buggy"
    log4cpp::Category& b = Logger::getFilelogger("test","/tmp/foo", "i'm buggy",true);
    b.info("This message will be repeated twice");
    
    // get the same Category named "i'm buggy"
    log4cpp::Category& c= Logger::getFilelogger("test","/tmp/foo", "i'm buggy",true);
    c.info("This message will be repeated thrice");
    */

    // ditto for syslogger
    log4cpp::Category& d = Logger::getSyslogger(
	    "test","localhost","i'm also buggy",
	     log4cpp::Priority::DEBUG,MONITOR_FACILITY);
    d.info("This space for rent");
    log4cpp::Category& e = Logger::getSyslogger(
	    "test2","localhost","i'm also buggy",
	     log4cpp::Priority::DEBUG,MONITOR_FACILITY);
    e.info("This message will be repeated twice");
    log4cpp::Category& f = Logger::getSyslogger(
	    "test2","eagle","i'm also buggy",
	     log4cpp::Priority::DEBUG,MONITOR_FACILITY);
    f.info("This message will be repeated thrice");

    ostringstream os;
    int abc = 123;
    os << "Is it dead? Foobar =" << abc;
    d << log4cpp::Priority::CRIT << os;

    /*
    log4cpp::Category& g = Logger::getOstreamlogger("ostreamtest",&std::cout,"I too am buggy");
    g.setPriority(log4cpp::Priority::DEBUG);
    g.debug("OSTREAM TEST");
    g.debug("g OSTREAM TEST AGAIN");
    ostringstream os;
    log4cpp::Category& h = Logger::getOstreamlogger("ostreamtest2",&os,"I am too buggy");
    h.debug("OSTREAM TEST AGAIN");
    h.debug("OSTREAM TEST AGAIN");
    g.debug("OSTREAM TEST AGAIN");
    cout << os.str() << endl;
    
    Program::getLogger().info("Program logger hoohah");
    */

    return 0;
}


