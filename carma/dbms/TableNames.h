#ifndef CARMA_DBMS_TABLENAMES_H
#define CARMA_DBMS_TABLENAMES_H

/**
 * @file
 * access of table names in the database
 *
 * @author: Dave Mehringer
 * @version $Id: TableNames.h,v 1.6 2006/03/30 16:42:18 hravlin Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <string>

namespace carma {
    /**
     * API for access to the CARMA database
     */
namespace dbms {

    typedef enum {
        AGGREGATE_SUBSYSTEMS_TABLE,
        BLANKING_FLAGS_TABLE,
        CHANGEABLE_MONITOR_CONFIG_TABLE,
        DEVICES_TABLE,
        LOCATIONS_TABLE,
        LOG_MESSAGES_TABLE,
        LOG_PRIORITIES_TABLE,
        MONITOR_ENUM_TABLE,
        MONITOR_ENUM_INDEX_TABLE,
        MONITOR_INDEX_TABLE,
        MONITOR_POINT_DATATYPES_TABLE,
        MONITOR_POINT_TYPES_TABLE,
        PARTITIONS_TABLE,
        STATIC_MONITOR_CONFIG_TABLE,
        SUBSYSTEM_TABLE,
        TAGIDNAMESIGNATURES_TABLE,
        VALIDITIES_TABLE,
	STATIC_MONITOR_CONFIG_CHANGELOG_TABLE
    } DBTable;

    std::string getTableName(const DBTable& table);
}}

#endif
