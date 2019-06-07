#ifndef CARMA_DBMS_DBCONFIGURATOR_H
#define CARMA_DBMS_DBCONFIGURATOR_H

/**
 * @file
 * DBConfigurator class
 *
 * @author: Dave Mehringer
 * @version: $Id: DBConfigurator.h,v 1.16 2011/12/21 22:56:43 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */
#include <map>
#include <string>
#include <vector>
#include "carma/dbms/MonitorDataIndex.h"
#include "carma/dbms/Resources.h"


namespace carma {
namespace dbms {

/**
 * Class used for configuring a DBConnection
 */

class DBConfigurator {

public:
    /**
     *  constructor
     * @param filename config file name
     * @throws NotFoundException if the file cannot be read
     */
    DBConfigurator(const std::string& filename);
 
    /**
     * Destructor
     */
    ~DBConfigurator();
    
    /**
     * get configuration data
     */
    inline std::map<std::string,std::string> getConfiguration() const {
        return pairs_;
    }
    
    /**
     * get the monitor point data directories corresponding to the specified
     * average type and the specified 
     * 
     * @param avgType the average type of the data
     * @param the area of interest
     * @return areas where the specified monitor point data are located
     * @throws NotFoundException if a data area hasn't been defined
     */
    std::map<carma::dbms::MonitorAggregateType,std::string> 
       getAverageAreas(const carma::dbms::MonitorAverageType& avgType,
                       const carma::dbms::MonitorDataAreaType& area);

    /**
     * get monitor point data areas as a map of monitorAreaIndices to directory
     * names
     * @return monitor data areas
     */
    std::map<carma::dbms::MonitorDataIndex, std::string> getMonitorDataAreas()
        const;
    
    /**
     * get the name of the odbc.ini file
     */
    std::string getODBCini() const;

    /**
     * get the ODBC data source
     */
    std::string getDataSource() const;

    /**
     * get the database user
     */
    std::string getDBUser() const;

    /**
     * get the database name
     */
    std::string getDBName() const;


    /**
     * get the password file for the dbuser
     * this method makes no attempt to do file or existence checking
     */
    std::string getPasswordFile() const;

    /**
     * get the rdbms
     */
    std::string getRDBMS() const;

    /**
     * get the socket for local, native connections
     */
    std::string getSocket() const;

    /**
     * get the port for remote, native connections
     */
    unsigned getPort() const;

    /**
     * get the specified data directory
     *
     */

    std::string getDataDirectory(const MonitorDataAreaType& mpArea, 
                                 const MonitorAverageType& avgType,
                                 const MonitorAggregateType& aggType) 
        const;

    /**
     * @return base file name for processed data that syslog2db;
     * frame count is appended to this name.
     */
    std::string getSyslogMMAPFileName() const;

    /**
     * @return base file name for processed data that syslog2db;
     * frame count is appended to this name.
     */
    std::string getLogWriteFileBaseName() const;

    /**
     * @return index file that syslog2db will write 
     * @deprecated No longer used in favor of in memory buffer
     */
    // std::string getLogIndex() const;

    /**
     * @return index file that syslog2db will write 
     */
    std::string getLogBufferSharedMemoryName() const;

    /**
     * @return a list of comma separated fully qualified paths to pipe files
     */
    std::string getPipeFileNames() const;

    /**
     * @return the list of all log files, including debug logs
     * @deprecated No longer used in favor of IPQ
     */
    // std::string getFullLogFileList() const;

    /**
     * @return The directory to which the flat log files were written,
     * so that logDataDeleter can mark them for delete them.
     */
    std::string getLogWriteDirectory() const;

    /**
     * @return The directory to which the flat log files were symlinked,
     * so that logDataLoader can load them into the dbms
     */
    std::string getLogLoadDirectory() const;

    /**
     * @return The directory to be used to write ASCII files that have been
     * converted from binary.
     */
    std::string getWorkDir() const;

    /**
     * @return The top directory to be used to write ASCII files.
     */
    std::string getTopDir() const;
protected:
    std::map<std::string,std::string> pairs_; 
    std::map <MonitorAverageType, std::string> averageType2String_;
    std::map <MonitorDataAreaType, std::string> areaType2String_;
    std::map <MonitorAggregateType, std::string> aggType2String_;

    std::string getValue(const std::string& key) const;
};

}}
#endif
