#ifndef CARMA_DBMS_MONITORDATATABLEFINISHERTHREAD_H
#define CARMA_DBMS_MONITORDATATABLEFINISHERTHREAD_H

/**
 * @file
 * MonitorDataTableFinisherThread class.
 *
 * @author: Dave Mehringer
 * @version $Id: MonitorDataTableFinisherThread.h,v 1.3 2011/08/01 20:24:37 abeard Exp $
 *
 * $CarmaCopyright$
 *
 */

#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"

#include <boost/thread.hpp>
#include <string>

namespace carma {
namespace dbms {

    class DBConnection;

/**
 * Thread class for executing commands on a monitor data table after it 
 * has been populated.
 */
class MonitorDataTableFinisherThread {

public:

    /**
     * constructor
     * @param dbc a valid database connection
     * @param tableName the table to finish up with
     * @param averageType the tables average type
     */
    explicit MonitorDataTableFinisherThread
        (const std::string& threadName, 
         const carma::dbms::DBConnection * const dbc, 
         const std::string& tableName,
         const carma::dbms::MonitorAverageType& averageType);

    ~MonitorDataTableFinisherThread( );

    bool isFinished() const;
    
    /**
     * the actions to take to finish up with the table
     */
    void operator()();

protected:
    
    /**
     * disallow default constructor
     */
     const carma::dbms::DBConnection *dbc_;
     carma::dbms::MonitorAverageType avgType_;
     std::string tableName_;

     boost::thread thread_;
     bool finished_;
     mutable boost::mutex finishedMutex_;
};


}}

#endif // CARMA_DBMS_MONITORDATAFINISHERTHREAD_H

