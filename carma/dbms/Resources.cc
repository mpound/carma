/**
 * @file
 * implementation of Resources methods
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */
#include <sstream>
#include "carma/dbms/Resources.h"
#include "carma/util/ErrorException.h"

namespace carma {
namespace dbms {

    std::string getResource(const Resource& resource) {
        switch(resource) {
        case DEFAULT_RDBMS:
            return "mysql";
        case DEFAULT_DATASRC:
            return "myodbc3";
        case DEFAULT_DBUSER:
            // FIXME making this an unlikely name so no one will accidentally 
            // connect via TagIDAuthority during development, switch back when 
            // integration begins
            // return "carmauser";
            return "carmauserblahblah";
        case DEFAULT_DBNAME:
            return "carma";
        case DEFAULT_ODBCINI:
            return "carma/dbms/odbc.ini";
        case DEFAULT_PORT:
            return "3306";
        case DEFAULT_SOCKET:
            return "/tmp/mysql.sock";
        default:
            std::ostringstream os;
            os << "Unhandled resource " << resource 
               << ". carma::dbms::getResource needs to be updated";
            throw CARMA_ERROR(os.str());
        }
    }
}}
