/* $Id: FileLayout.cc,v 1.3 2003/08/07 14:08:07 mpound Exp $
 * 
 * File Layout (format) for CARMA Logging messages.
 * This format includes a FITS timestamp: yyyy-dd-mmThh:mm:ss.s
 */
#include <iostream>
#include "carma/util/Logger.h"
#include "carma/util/Time.h"

using namespace std;
using namespace log4cpp;

namespace carma {
  namespace util {

    FileLayout::FileLayout() { }
    
    FileLayout::~FileLayout() { }

    /**
     * The format of messages from a FileLayout
     */
    std::string FileLayout::format(const LoggingEvent& event) {
	carma::util::Time t;
        std::ostringstream message;

	// This is the string with the priority level, e.g. "DEBUG"
        const string& priorityName = Priority::getPriorityName(event.priority);

	// convert unix seconds to a date string via MJD. Could probably
	// do this with ctime(), but may as well use carma Time methods.
	time_t tsec = static_cast<time_t>( event.timeStamp.getSeconds() );
	double mjd = t.computeMJD(tsec);

        message << t.getFITSdateTimeString(mjd,1)
		<< " {"
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
