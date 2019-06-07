#ifndef CARMA_DBMS_COLUMNNAMES_H
#define CARMA_DBMS_COLUMNNAMES_H

/**
 * @file
 * access of column names in the database
 *
 * @author: Dave Mehringer
 * @version $Id: ColumnNames.h,v 1.9 2005/01/25 16:30:36 dmehring Exp $
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

    /**
     * database columns
     */
    typedef enum {
        COLUMN_BLANKINGFLAG,
        COLUMN_BLANKINGFLAGID,
        COLUMN_CALLER,
        COLUMN_DATATYPEID,
        COLUMN_DEVICEID,
        COLUMN_ENUMID,
        COLUMN_ENUMINDEX,
        COLUMN_ENUMVALUE,
        COLUMN_FRAMECOUNT,
        COLUMN_IMAGPART,
        COLUMN_INTEGRATEDIMAGPART,
        COLUMN_INTEGRATEDREALPART,
        COLUMN_INTEGRATEDVALUE,
        COLUMN_INTEGRATIONID,
        COLUMN_ISAMPLE,
        COLUMN_LOCATIONID,
        COLUMN_LOGNAME,
        COLUMN_MAX,
        COLUMN_MAXIMAGPART,
        COLUMN_MAXREALPART,
        COLUMN_MESSAGE,
        COLUMN_MIN,
        COLUMN_MINIMAGPART,
        COLUMN_MINREALPART,
        COLUMN_MPTYPEID,
        COLUMN_NAME,
        COLUMN_NDC,
        COLUMN_PRIORITYID,
        COLUMN_PRIORITY,
        COLUMN_REALPART,
        COLUMN_SIGNATURE,
        COLUMN_SUBSYSID,
        COLUMN_SUBSYSNAME,
        COLUMN_TAGID,
        COLUMN_VALIDITY,
        COLUMN_VALIDITYID,
        COLUMN_VALUE
    } DBColumn;

    /**
     * enum representing table sort order in an order by clause
     */
    typedef enum {
        ASCENDING,
        DESCENDING
    } SortOrder;

    /**
     * get the actual column name for the specified column
     * @param column the db column for which to get the name
     * @return the column name
     */
    std::string getColumnName(const DBColumn& column);

    /**
     * get a string representing the specified sort order
     */
    std::string toString(const SortOrder& sortOrder);

}}

#endif
