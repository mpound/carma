/**
 *
 * Implementation for PhysicalDeviceIDAuthority
 *
 * @author: Dave Mehringer
 *
 * $Id: PhysicalDeviceIDAuthority.cc,v 1.1 2010/02/18 17:00:07 abeard Exp $
 * $CarmaCopyright$
 *
 */
#include "carma/dbms/PhysicalDeviceIDAuthority.h"

#include <sstream>

#include "carma/dbms/DBConnection.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "carma/dbms/TableNames.h"
#include "carma/util/CommonExceptions.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::dbms;
using namespace carma::util;


PhysicalDeviceIDAuthority::PhysicalDeviceIDAuthority(
    const bool             production,
    const DBConfigurator * dbconf ) : 
usingDB_( production )
{
    for(int i=0; i < locationCount_; i++) {
        registerLocationNameID(string(locationNames_[i]), i);
    }

    for(int i=0; i < deviceCount_; i++) {
        registerDeviceNameID(string(deviceNames_[i]), i);
    }

    ostringstream os;
    if ( usingDB_ ) {
        DBConnection *dbc = 0;
        if (DBConnection::isUp(dbconf)) {
            try {
                dbc = DBConnectionFactory::createConnection(dbconf);

                os << "Database connection successfully opened";
                
                programLogInfoIfPossible( os.str() );
            } catch ( const DBConnectionException & exc ) {
                os << "Unable to open a connection to the database. " 
                   << exc.what();

                programLogWarnIfPossible( os.str() );

                throw CARMA_EXCEPTION(DBConnectionException, os.str());
            }                

            int idFromTable;
            string nameFromTable;
            MonitorConfigurationDatabase mcdb(dbc);
            try {
                Table locationTable = mcdb.getLocationsTable();
                // verify locations are the same in the database as in the
                // C++ array
                int locRowCount = locationTable.rowCount();
                if ( locationCount_ != locRowCount ) {
                    os << "locationCount_ in PhysicalDeviceIDAuthority is "
                       << locationCount_ << " Number of locations in db "
                       << "table " 
                       << getTableName(LOCATIONS_TABLE)
                       << " is " << locationTable.rowCount() << ". These must "
                       << "be equal.";
                    throw CARMA_ERROR(os.str());
                }
                for(int i=0; i < locationCount_; i++) {
                    idFromTable = locationTable
                        .getShortColumn("locationID")[i];
                    nameFromTable = locationTable.getStringColumn("name")[i];
                    if(locationID2Name_[idFromTable] != nameFromTable) {
                        os << "Mismatch between db and C++.  Location "
                           << idFromTable << " in the db belongs to location "
                           << nameFromTable << " while this id belongs to "
                           << locationID2Name_[idFromTable] << " in "
                           << "carma::dbms::PhysicalDeviceIDAuthority::locationNames_";
                        throw CARMA_ERROR(os.str());
                    }
                }
            } catch ( const DBConnectionException & exc ) {
                delete dbc;
                dbc = 0;
                os << "Unable to read " << getTableName(LOCATIONS_TABLE) 
                   << " db table. " << exc.what();
                throw CARMA_EXCEPTION(DBConnectionException, os.str());
            }
            try {
                //Table deviceTable = dbc->getDeviceTable();
                Table deviceTable = mcdb.getDevicesTable();
                // verify locations are the same in the database as in the
                // C++ array
		int deviceRowCount = deviceTable.rowCount();
                if ( deviceCount_ != deviceRowCount ) {
                    os << "deviceCount_ in PhysicalDeviceIDAuthority is "
                       << deviceCount_ << " Number of devices in db "
                       << "table " << getTableName(DEVICES_TABLE) 
                       << " is " << deviceTable.rowCount() << ". These must "
                       << "be equal.";
                    throw CARMA_ERROR(os.str());
                }
                for(int i=0; i < deviceCount_; i++) {
                    idFromTable = deviceTable.getShortColumn("deviceID")[i];
                    nameFromTable = deviceTable.getStringColumn("name")[i];
                    if(deviceID2Name_[idFromTable] != nameFromTable) {
                        os << "Mismatch between db and C++.  Device "
                           << idFromTable << " in the db belongs to device "
                           << nameFromTable << " while this id belongs to "
                           << deviceID2Name_[idFromTable] << " in "
                           << "carma::dbms::PhysicalDeviceIDAuthority::deviceNames_";
                    throw CARMA_ERROR(os.str());
                    }
                }
            } catch ( const DBConnectionException & exc ) {
                delete dbc;
                dbc = 0;
                os << "Unable to read " << getTableName(DEVICES_TABLE) 
                   << " db table. " << exc.what();
                throw CARMA_EXCEPTION(DBConnectionException, os.str());
            }
        }
    } 
}


namespace {

PhysicalDeviceIDAuthority * gAuthority = 0;

}  // namespace < anonymous >


void
PhysicalDeviceIDAuthority::closeAuthority( )
{
    if ( gAuthority ) {
        PhysicalDeviceIDAuthority * temp = 0;
        
        ::std::swap( gAuthority, temp );
        
        delete temp;
    }
}


const PhysicalDeviceIDAuthority &
PhysicalDeviceIDAuthority::getAuthority(
    const bool                   production,
    const DBConfigurator * const dbconf )
{
    if ( gAuthority == 0 )
        gAuthority = new PhysicalDeviceIDAuthority( production, dbconf );
        
    return *gAuthority;
}


void
PhysicalDeviceIDAuthority::registerLocationNameID( const string & locationName, 
                                                   const int &    locationID )
{
    if ( locationID2Name_.find( locationID ) != locationID2Name_.end() ) {
        ostringstream msg;

        msg << "Attempt to reassign locationID " << locationID << " to " 
            << locationName ;

        throw CARMA_EXCEPTION(IllegalArgumentException, msg);
    }

    if ( locationName2ID_.find(locationName) != locationName2ID_.end() ) {
        ostringstream msg;

        msg << "locationName " << locationName << " has already been assigned" ;

        throw CARMA_EXCEPTION(IllegalArgumentException, msg);
    }        

    locationID2Name_[locationID] = locationName;
    locationName2ID_[locationName] = locationID;
}


void 
PhysicalDeviceIDAuthority::registerDeviceNameID( const string & deviceName, 
                                                 const int &    deviceID )
{
    if ( deviceID2Name_.find( deviceID ) != deviceID2Name_.end() ) {
        ostringstream msg;
        
        msg << "Attempt to reassign deviceID " << deviceID << " to " 
            << deviceName ;
        
        throw CARMA_EXCEPTION(IllegalArgumentException, msg);
    }

    if ( deviceName2ID_.find( deviceName ) != deviceName2ID_.end() ) {
        ostringstream msg;

        msg << "deviceName " << deviceName << " has already been assigned" ;

        throw CARMA_EXCEPTION(IllegalArgumentException, msg);
    }
    
    deviceID2Name_[deviceID] = deviceName;
    deviceName2ID_[deviceName] = deviceID;
}


int
PhysicalDeviceIDAuthority::getLocationID( const string & location ) const
{
    const Name2IdMap::const_iterator i = locationName2ID_.find( location );

    if ( i == locationName2ID_.end() ) {
        string emsg = "Unknown location " + location;
        
        throw CARMA_EXCEPTION(NotFoundException, emsg);
    }

    return i->second;
}


int
PhysicalDeviceIDAuthority::getLocationID(
    const string &               location,
    const bool                   production,
    const DBConfigurator * const dbconf )
{
    return getAuthority( production, dbconf ).getLocationID( location );
}


int
PhysicalDeviceIDAuthority::getDeviceID( const string & device ) const
{
    const Name2IdMap::const_iterator i = deviceName2ID_.find( device );
    
    if ( i == deviceName2ID_.end() ) {
        string emsg = "Unknown device " + device;

        throw CARMA_EXCEPTION(NotFoundException, emsg);
    }

    return i->second;
}


int
PhysicalDeviceIDAuthority::getDeviceID(
    const string &               device,
    const bool                   production,
    const DBConfigurator * const dbconf )
{
    return getAuthority( production, dbconf ).getDeviceID( device );
}


string
PhysicalDeviceIDAuthority::getLocation( const int & locationID ) const
{
    const Id2NameMap::const_iterator i = locationID2Name_.find( locationID );

    if ( i == locationID2Name_.end() ) {
        ostringstream emsg;
        emsg << "Unknown location ID " << locationID;
        throw CARMA_EXCEPTION(NotFoundException, emsg.str());
    }

    return i->second;
}


string
PhysicalDeviceIDAuthority::getLocation(
    const int &                  locationID,
    const bool                   production,
    const DBConfigurator * const dbconf )
{
    return getAuthority( production, dbconf ).getLocation( locationID );
}


string
PhysicalDeviceIDAuthority::getDevice( const int & deviceID ) const
{
    const Id2NameMap::const_iterator i = deviceID2Name_.find( deviceID );

    if ( i == deviceID2Name_.end() ) {
        ostringstream emsg;
        emsg << "Unknown device ID " << deviceID;
        throw CARMA_EXCEPTION(NotFoundException, emsg.str());
    }

    return i->second;
}


string
PhysicalDeviceIDAuthority::getDevice(
    const int &                  deviceID,
    const bool                   production,
    const DBConfigurator * const dbconf )
{
    return getAuthority( production, dbconf ).getDevice( deviceID );
}
