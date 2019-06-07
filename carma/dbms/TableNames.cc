/**
 * @file
 * implementation of Resources methods
 *
 * @author: Dave Mehringer
 *
 * $Id: TableNames.cc,v 1.6 2006/03/30 16:42:18 hravlin Exp $
 * $CarmaCopyright$
 *
 */
#include <sstream>
#include "carma/dbms/TableNames.h"
#include "carma/util/ErrorException.h"

namespace carma {
namespace dbms {

    std::string getTableName(const DBTable& table) {
        switch(table) {
        case AGGREGATE_SUBSYSTEMS_TABLE:
            return "AggregateSubsystems";
        case BLANKING_FLAGS_TABLE:
            return "BlankingFlags";
        case CHANGEABLE_MONITOR_CONFIG_TABLE:
            return "MonitorConfigChangeableParms";
        case DEVICES_TABLE:
            return "Devices";
        case LOCATIONS_TABLE:
            return "Locations";
	case LOG_PRIORITIES_TABLE:
            return "LogPriorities";
        case LOG_MESSAGES_TABLE:
            return "LogMessages";
        case MONITOR_ENUM_TABLE:
            return "MonitorEnumerators";
        case MONITOR_ENUM_INDEX_TABLE:
            return "MonitorEnumeratorIndex";
        case MONITOR_INDEX_TABLE:
            return "MonitorDataTableIndex";
        case MONITOR_POINT_DATATYPES_TABLE:
            return "MonitorPointDataTypes";
        case MONITOR_POINT_TYPES_TABLE:
            return "MonitorPointTypes";
        case PARTITIONS_TABLE:
            return "Partitions";
        case STATIC_MONITOR_CONFIG_TABLE:
            return "MonitorConfigStaticParms";
        case SUBSYSTEM_TABLE:
            return "Subsystems";
        case TAGIDNAMESIGNATURES_TABLE:
            return "TagIDNameSignatures";
        case VALIDITIES_TABLE:
            return "Validities";
        case STATIC_MONITOR_CONFIG_CHANGELOG_TABLE:
            return "StaticParmsChangeLog";
        default:
            std::ostringstream os;
            os << "Unhandled db table " << table 
               << ". carma::dbms::getTableName() needs to be updated";
            throw CARMA_ERROR(os.str());
        }
    }
}}
