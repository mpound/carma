/**
 * LogDatabase implementation
 *
 * @author: Dave Mehringer
 *
 * $Id: LogDatabase.cc,v 1.3 2005/01/17 18:27:02 dmehring Exp $
 * $CarmaCopyright$
 *
 */

#include "carma/dbms/DBConnection.h"
#include "carma/dbms/LogDatabase.h"
#include "carma/dbms/Syslog2DBMSConversions.h"
#include "carma/dbms/TableNames.h"
#include "carma/dbms/Typedefs.h"
#include "carma/util/Trace.h"

using namespace std;
using namespace carma::dbms;


LogDatabase::LogDatabase(const DBConnection * const dbc) : dbc_(dbc) {}

void LogDatabase::populateLogPrioritiesTable() const {
    ostringstream statement;
    PriorityMap pm = getDB2PriorityMap();
    PriorityMap::const_iterator iter = pm.begin();
    for(; iter != pm.end() ; iter++) {
        statement.str("");
        statement << "INSERT INTO " << getTableName(LOG_PRIORITIES_TABLE) 
                  << " (priorityID, "
                  << "priority) VALUES (" 
                  << iter->first
                  << ", '" 
                  << iter->second
                  << "')";
        CPTRACE(carma::util::Trace::TRACEALL, statement.str());
        dbc_->directSQLInsert_(statement.str());
    }
}



