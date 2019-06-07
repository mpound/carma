/** @file
 * Declaration of dbms FileManager classes; modularized from the MAW.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.4 $
 * $Date: 2012/05/01 21:53:57 $
 * $Id: dbFileManagers.h,v 1.4 2012/05/01 21:53:57 abeard Exp $
 */
#ifndef CARMA_DBMS_DBFILEMANAGERS_H
#define CARMA_DBMS_DBFILEMANAGERS_H

#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"
#include "carma/util/types.h"

#include <iosfwd>
#include <memory>

namespace carma {

namespace monitor {
    class AverageAccumulator;
}

namespace dbms {

class DBConfigurator;

/**
 * Class to manage database ingest files and symlinks using stdio.
 */
class DbFileManager {
public:

    DbFileManager( dbms::MonitorAverageType avgType,
                   const bool dotransfer,
                   const bool dosdp,
                   const carma::dbms::DBConfigurator & dbConf );

    ~DbFileManager( );

    void OpenNewFiles( const std::string & endName,
                       carma::util::frameType frameCount );

    void writeLongAveragesToFile( 
        carma::monitor::AverageAccumulator & accumulator,
        const long frameCount );

    void DoneWithFiles( const bool dodbload );

private:

    void addMonitorDataArea( carma::dbms::MonitorDataAreaType monitorDataArea,
                             const carma::dbms::DBConfigurator & dbConf ); 

    struct Impl;
    ::std::auto_ptr< Impl > impl_;

}; // class DbFileManager 

/**
 * Class to manage database ingest files and symlinks using FFIO files.
 */
class DbFFIOFileManager {
public:
    
    DbFFIOFileManager( carma::dbms::MonitorAverageType avgType,
                       const carma::dbms::DBConfigurator & dbConf ); 
    
    ~DbFFIOFileManager( );

    void OpenNewFiles( const ::std::string & endName,
                       carma::util::frameType frameCount );
    
    void writeInstAveragesToFile( 
        carma::monitor::AverageAccumulator & accumulator,
        const long frameCount );
    
    void DoneWithFiles( const bool dodbload ); 

private:
    
    struct Impl;

    ::std::auto_ptr< Impl > impl_;

}; // class DbFFIOFileManager

} } // namespace carma::dbms

#endif
