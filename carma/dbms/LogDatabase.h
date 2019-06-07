#ifndef CARMA_DBMS_LOGDATABASE_H
#define CARMA_DBMS_LOGDATABASE_H

/**
 * @file
 * LogDatabase
 *
 * @author: Dave Mehringer
 *
 * $Id: LogDatabase.h,v 1.1 2005/01/17 14:26:39 dmehring Exp $
 * $CarmaCopyright$
 *
 */


namespace carma {
namespace dbms {

    class DBConnection;

/**
 * This class contains methods for accessing the log table in the database
 */
class LogDatabase {

public:
    /**
     * constructor
     */
    LogDatabase(const carma::dbms::DBConnection * const dbc);

    /**
     * populate the LogPriorities table 
     * should only be run for database initialization
     */
    void populateLogPrioritiesTable() const;

 private:
    const DBConnection *dbc_;

};
}}

#endif // CARMA_DBMS_LOGDATABASE_H
