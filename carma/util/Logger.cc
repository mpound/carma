/**
 * A convenience class for returning predefined log4cpp Categories.
 * This class has only static methods.
 * $Id: Logger.cc,v 1.18 2006/06/20 08:53:35 colby Exp $
 * @file Logger.cc
 * @author Marc Pound
 */

#include "carma/util/Logger.h"

// required log4cpp header files.
#include <log4cpp/Category.hh>
#include <log4cpp/FileAppender.hh>
#include <log4cpp/RollingFileAppender.hh>
#include <log4cpp/Layout.hh>
#include <log4cpp/Portability.hh>
#include <log4cpp/Priority.hh>
#include <log4cpp/OstreamAppender.hh>
#include <log4cpp/RemoteSyslogAppender.hh>
#include <log4cpp/SyslogAppender.hh>
#include <log4cpp/TimeStamp.hh>

#include <iostream>
#include <sstream>
#include <stdarg.h>
#include <unistd.h>

// Some builtin sizes. See note below. (Some of) These could be carma global.
#define MAX_FILESIZE 10*1024*1024 // maximum size before rolling over file
#define MAX_BACKUPINDEX 3         // maximum number of rolled over files to 
                                  // keep.
#define HOSTNAME_MAXCHAR 64       // maximum characters for a hostname


using namespace std;
using namespace log4cpp;
using namespace carma::util;


// The destructor does nothing. Do we need this if we 
// don't have a constructor?
Logger::~Logger() { }

/** 
 * Return a Category instance with a either a local or remote 
 * syslog appender attached.  The layout (format) of the syslog 
 * messages is that of SyslogLayout. 
 */
log4cpp::Category& Logger::getSyslogger(
                          const string& identity,
			  const string& host,
			  const string& logname,
			  log4cpp::Priority::PriorityLevel priority,
			  facilityType facility) {
    
    // Instantiate category (logger) object, or get it if
    // it already exists. 
    log4cpp::Category& myLogger = log4cpp::Category::getInstance(logname);
    // The requested priority may have changed, so reset it.
    myLogger.setPriority(priority);

    //yes logname not identity
    Appender* currentAppender = myLogger.getAppender(logname); 

    // If the requested appender is already there, then we
    // are done. 
    if ( currentAppender != 0 )  {
        // Please, no output to stdout - it breaks all rtd programs!!
        // If this is important, write it to the log...
        //cout << "Found existing syslogger " << logname << "/" << identity << endl;
    	return myLogger;
    }
	//cout << "Creating sylogger " << logname << endl;

    // Appender does not exist, but others might. Remove them, since
    // we are only allowing one appender.
    myLogger.removeAllAppenders();

    log4cpp::LayoutAppender* app;
    SyslogLayout* layout;

    // get the local host name, to compare with "host".
    // If the localhost, create an appender for local syslog,
    // else create one for the remote syslog.
    char *hostname = new char[HOSTNAME_MAXCHAR];  
    gethostname(hostname,HOSTNAME_MAXCHAR);
    string s = string(hostname);
    // we allow them to input "localhost"
    string localhost("localhost");
    if (localhost == host || s == host) {
	// create local syslogger
	app = new SyslogAppender(logname,identity,facility);
    } else {
       // create remote syslogger
	app = new RemoteSyslogAppender(logname,identity,host,facility);
    }

    // create the appropriate layout object and add to appender
    layout = new SyslogLayout();
    app->setLayout(layout);
    
    // Add the appender, set additivity
    // to false which means that the current appender becomes the 
    // only one (no chained appenders).
    myLogger.addAppender(app);
    myLogger.setAdditivity(false);

    delete[] hostname;
    return myLogger;
}

log4cpp::Category& Logger::getFilelogger(
                          const string& identity,
			  const string& pathname,
			  const string& logname,
			  bool append,
			  mode_t mode,
			  log4cpp::Priority::PriorityLevel priority,
			  bool traceVerbose ) {
    
    // Instantiate category (logger) object, or get it if
    // it already exists. 
    log4cpp::Category& myLogger = log4cpp::Category::getInstance(logname);
    Appender* currentAppender = myLogger.getAppender(identity);

    // The requested priority may have changed, so reset it.
    myLogger.setPriority(priority);

    // If the requested appender is already there, then we
    // are done. 
    if ( currentAppender != 0 ) 
    	return myLogger;

    // Appender does not exist, but others might. Remove them, since
    // we are only allowing one appender.
    myLogger.removeAllAppenders();

    // These params are used by the constructor for 
    // RollingFileAppender.  The maximum filesize before rolling
    // over is the default from log4cpp. The maximum number of 
    // backup files is 3, different from the log4cpp default of 1
    // (which really doesnt make sense--why have a rolling file if
    // the default is not to roll it?).

    size_t maxFileSize = MAX_FILESIZE;
    unsigned int maxBackupIndex = MAX_BACKUPINDEX;

    log4cpp::RollingFileAppender* app;

    // Create the appender object.
    app = new log4cpp::RollingFileAppender(
		       identity,
		       pathname,
		       maxFileSize,
		       maxBackupIndex,
		       append,
		       mode);

    // create the appropriate layout object and add to appender
    if ( traceVerbose )
      app->setLayout(new FileLayout());
    else
      app->setLayout(new VerySimpleLayout);

    // Add the appender, set additivity
    // to false which means that the current appender becomes the 
    // only one (no chained appenders).
    myLogger.addAppender(app);
    myLogger.setAdditivity(false);

    return myLogger;
}


log4cpp::Category& Logger::getOstreamlogger(
                          const string& identity,
			  std::ostream *stream,
			  const string& logname,
			  log4cpp::Priority::PriorityLevel priority,
			  bool traceVerbose) {

    // Instantiate category (logger) object, or get it if
    // it already exists. 
    log4cpp::Category& myLogger = log4cpp::Category::getInstance(logname);
    Appender* currentAppender = myLogger.getAppender(identity);

    // The requested priority may have changed, so reset it.
    myLogger.setPriority(priority);

    // If the requested appender is already there, then we
    // are done. 
    if ( currentAppender != 0 )  {
	//cout << logname << " " << identity << " getOSTREAMLOGGER RETURNING" << endl;
    	return myLogger;
    }

	//cout << logname << " " << identity << " getOSTREAMLOGGER PROCEEDING" << endl;

    // Appender does not exist, but others might. Remove them, since
    // we are only allowing one appender.
    myLogger.removeAllAppenders();
    

    log4cpp::OstreamAppender* app;

    // Create the appender object.
    app = new log4cpp::OstreamAppender(identity, stream);

    // create the appropriate layout object and add to appender.
    // use a file layout--its simple.
    if ( traceVerbose )
      app->setLayout(new FileLayout());
    else
      app->setLayout(new VerySimpleLayout());

    // Add the appender, set additivity
    // to false which means that the current appender becomes the 
    // only one (no chained appenders).
    myLogger.setAppender(app);
    myLogger.setAdditivity(false);

    return myLogger;
}

