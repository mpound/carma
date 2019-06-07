/**
 * @file carma/util/Test/LoggerTest.cc
 * Unit test for logging to file, syslog and outputstream.
 *
 * @author Original: Marc Pound
 * $Id: LoggerTest.cc,v 1.4 2004/10/28 14:25:30 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */

#include "LoggerTest.h"

#include <iostream>
#include <sstream>
#include <string>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>

using namespace std;
using namespace CppUnit;
using namespace carma::util;

void LoggerTest::setUp() { }

void LoggerTest::tearDown() { }

void LoggerTest::testFileLayout()
{
    cout << "FileLayout test" << endl;
    const char *filename = "/tmp/filetest.log";
    string progName = "File Logger Unit Test";
    string outfile(filename);
    log4cpp::Category& myLogger = carma::util::Logger::getFilelogger(
		         progName, outfile, 
			 "carma.unittest.filelogger",
			 false);

    log4cpp::NDC::push("Starting unit test.");
    myLogger.notice("Hello, world.");
    log4cpp::NDC::pop();
    log4cpp::NDC::push("Continuing unit test");
    myLogger.debug("This is a DEBUG level statement.");
    myLogger.warn("This is a WARN level statement.");
    myLogger << log4cpp::Priority::NOTICE 
	     << "This is a NOTICE level statement.";
    myLogger.info("This is a INFO level statement.");
//  Don't test EMERG because it broadcasts to all on system
//  myLogger << log4cpp::Priority::EMERG << "This is a EMERG level statement.";
    myLogger.alert("This is a ALERT level statement.");
    myLogger.critStream() << "This is a CRITICAL level statement.";
    log4cpp::NDC::pop();
    log4cpp::NDC::push("Ending unit test");
    myLogger.notice("Goodbye.");
    log4cpp::NDC::pop();
    myLogger.shutdown();
    unlink(filename);
}

void LoggerTest::testSyslogLayout()
{
    cout << "SyslogLayout test" << endl;
    string progName = "Syslog Logger Unit Test";
    char *hostname = new char[64];
    gethostname(hostname,64);
    string localhost = string(hostname);
    log4cpp::Category& myLogger = carma::util::Logger::getSyslogger(
		         progName, localhost, "carma.unittest.syslogger",
                         log4cpp::Priority::DEBUG,
			 MONITOR_FACILITY);
    log4cpp::NDC::push("Starting unit test.");
    myLogger.notice("Hello, world.");
    log4cpp::NDC::pop();
    log4cpp::NDC::push("Continuing unit test");
    myLogger.info("Unit test will use MONITOR facility");
    myLogger << log4cpp::Priority::DEBUG
	     << "This DEBUG level statement should go "
	     << "into /var/carma/log/monfault.debug";
    myLogger.warn("This is a WARN level statement.");
    myLogger << log4cpp::Priority::NOTICE 
	     << "This is a NOTICE level statement.";
    myLogger.alert("This is a ALERT level statement.");
    myLogger.critStream() << "This is a CRITICAL level statement.";
    log4cpp::NDC::pop();
    log4cpp::NDC::push("Ending unit test");
    myLogger.notice("Goodbye.");
    log4cpp::NDC::pop();
    myLogger.shutdown();
    delete[] hostname;
}

void LoggerTest::testOstreamLayout()
{
    cout << "Ostream Layout test" << endl;
    string progName = "Ostream Logger Unit Test";
    log4cpp::Category& myLogger = Logger::getOstreamlogger(progName);
    log4cpp::NDC::push("Starting unit test.");
    myLogger.notice("Hello, world.");
    log4cpp::NDC::pop();
    log4cpp::NDC::push("From Lincoln's Second Inaugural Address");
    myLogger << log4cpp::Priority::WARN
	    << "Yet, if God wills that it continue until all the wealth "
	    << "piled by the bondsman's two hundred and fifty years of "
	    << "unrequited toil shall be sunk, and until every drop of "
	    << "blood drawn with the lash shall be paid by another drawn "
	    << "with the sword, as was said three thousand years ago, so "
	    << "still it must be said 'the judgments of the Lord are true "
	    << "and righteous altogether'.";
    log4cpp::NDC::pop();
    log4cpp::NDC::push("Ending unit test");
    myLogger.notice("Goodbye.");
    log4cpp::NDC::pop();
    myLogger.shutdown();
}
