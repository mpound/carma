#include "carma/dbms/Syslog2DBMSConversions.h"
#include <string>

namespace carma {
    namespace dbms {

      dbPriorityType priority2DB(const std::string& priority) {
	if (priority == UNKNOWN_STR)
	    return UNKNOWN;
	if (priority == NOTSET_STR)
	    return NOTSET;
	if (priority == DEBUG_STR)
	    return DEBUG;
	if (priority == INFO_STR)
	    return INFO;
	if (priority == NOTICE_STR)
	    return NOTICE;
	if (priority == WARN_STR)
	    return WARN;
	if (priority == ERROR_STR)
	    return ERROR;
	if (priority == ALERT_STR)
	    return ALERT;
	if (priority == CRIT_STR)
	    return CRIT;
	if (priority == FATAL_STR)
	    return FATAL;
	
	// if no matches, return UNKNOWN.
	return UNKNOWN;
      }

	const std::string& db2Priority(const dbPriorityType dbPriority ) {
	    switch (dbPriority) {
		default:
		case UNKNOWN:
		    return UNKNOWN_STR;
		case NOTSET:
		    return NOTSET_STR;
		case DEBUG:
		    return DEBUG_STR;
		case INFO:
		    return INFO_STR;
		case NOTICE:
		    return NOTICE_STR;
		case WARN:
		    return WARN_STR;
		case ERROR:
		    return ERROR_STR;
		case CRIT:
		    return CRIT_STR;
		case ALERT:
		    return ALERT_STR;
		case FATAL:
		    return FATAL_STR;
	    }
	}


      const PriorityMap getDB2PriorityMap() {
	PriorityMap pm;
	pm.insert(make_pair(UNKNOWN, UNKNOWN_STR));
	pm.insert(make_pair(NOTSET,NOTSET_STR));
	pm.insert(make_pair(DEBUG,DEBUG_STR));
	pm.insert(make_pair(INFO,INFO_STR));
	pm.insert(make_pair(NOTICE,NOTICE_STR));
	pm.insert(make_pair(WARN,WARN_STR));
	pm.insert(make_pair(ERROR,ERROR_STR));
	pm.insert(make_pair(CRIT,CRIT_STR));
	pm.insert(make_pair(ALERT,ALERT_STR));
	pm.insert(make_pair(FATAL,FATAL_STR));
	return pm;
      }

    }// dbms
}// carma
