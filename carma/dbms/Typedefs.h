#ifndef CARMA_DBMS_TYPEDEFS_H
#define CARMA_DBMS_TYPEDEFS_H

/**
 * @file
 * typdefs for carma::dbms.
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#include <map>
#include <string>

namespace carma {

namespace dbms {
    class MonitorDescription;
    /**
     * map of id (normally a tagID) -> monitor configuration
     */
    typedef std::map<const int, const carma::dbms::MonitorDescription> 
      ID2MDMap;

    /**
     * Database priority level 
     */
    typedef unsigned short dbPriorityType;

    /**
     * A map of DB priority levels -> syslog priority (string) levels
     */
    typedef std::map<const dbPriorityType, const std::string> PriorityMap;

}}


#endif //CARMA_DBMS_TYPEDEFS.H
