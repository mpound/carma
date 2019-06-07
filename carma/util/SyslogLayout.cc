/* $Id: SyslogLayout.cc,v 1.3 2005/01/27 15:13:56 mpound Exp $
 * 
 * Syslog Layout (format) for CARMA Logging messages.
 */
#include <iostream>
#include <iomanip>
#include <time.h>
#include <ctime>
#include "carma/util/Logger.h"
#include "carma/util/Time.h"

using namespace std;
using namespace log4cpp;

namespace carma {
  namespace util {

    SyslogLayout::SyslogLayout() { }
    
    SyslogLayout::~SyslogLayout() { }

    /**
     * The format of messages from a SyslogLayout
     */
    std::string SyslogLayout::format(const LoggingEvent& event) {
        std::ostringstream message;

	// This is the string with the priority level, e.g. "DEBUG"
        const string& priorityName = Priority::getPriorityName(event.priority);
	// UTC Year added so that frame count may be computed for index
	// into database. syslog date format does not include year and
	// cannot be configured to do so. 
	// See RFC3164: "The BSD syslog Protocol".
	// Year must be the local timezone year, even though frame count 
	// is UTC based.  Differences between local timezone and UTC
	// are handled in LogProcessor.
	//
	// Note we do NOT want frame count as a column here because
	// that will defeat syslog's repeated message decimation.
	// localtime_r() is threadsafe, localtime() is not.
	struct tm myTime;
	time_t seconds;
	time ( &seconds );
	localtime_r( &seconds, &myTime );
        message << " {" 
	        << myTime.tm_year+1900
		<< "} {"
		<< priorityName
		<< "} {"
                << event.categoryName
		<< "} {"
                << event.ndc 
		<< "} {"
                << event.message 
                << "}"
		<< endl;

        return message.str();
    }

  }
}
