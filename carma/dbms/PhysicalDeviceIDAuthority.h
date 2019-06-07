#ifndef CARMA_DBMS_PHYSICALDEVICEIDAUTHORITY_H
#define CARMA_DBMS_PHYSICALDEVICEIDAUTHORITY_H


/**
 * @file
 *
 * PhysicalDeviceIDAuthority class.
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */


#include <string>
#include <map>

namespace carma {

namespace dbms {

class DBConfigurator;


/**
 *  A lookup class for mapping names and IDs of locations and devices.  
 *  This class is instantiated
 *  as a singleton via a factory method.  The CARMA Database is the 
 *  authoritative source for location and device names and IDs. However,
 *  because the Locations and Devices database tables are initialized using
 *  the names and array indices of locationnames and deviceNames of this class
 *  this class only needs to use these arrays.  Because of this 
 *  interdependence, IT IS NECESSARY THAT NEW LOCATIONS AND DEVICES ARE ADDED
 *  ONLY AT THE END OF locationNames_ and deviceNames_. If this isn't done,
 *  loss of database integrity will occur.
 */
class PhysicalDeviceIDAuthority {
public:

    /**
     * get the singleton instance of the PhysicalDeviceIDAuthority.  
     * if the instance doesn't already exist, this method will create it
     * if it does exist, this method simply returns the instance
     * @param production use the database to get IDs
     * @param dbconf DBConfigurator pointer for configuring the db connection,
     *        not used if production=false
     */
    static const PhysicalDeviceIDAuthority &
    getAuthority( bool                         production = false,
                  const dbms::DBConfigurator * dbconf = 0 );

    /**
     * return the number of locations
     * @return the total number of locations
     */
    static int getLocationCount( );

    /**
     * return the number of devices
     * @return the total number of devices
     */
    static int getDeviceCount( );

    /**
     * indicate that one's use of the authority is complete.  If we are 
     * running in production mode, the singleton instance will be deleted.
     * It is safe to call getAuthority() at anytime before or after this
     * function is called.
     */
    static void closeAuthority( );

    /**
     * return the ID for a given hardware location name
     * @param location   the location name
     * @throws NotFoundException  if name is not matched
     */
    int getLocationID( const ::std::string & location ) const;
    
    static int getLocationID( const ::std::string &        location,
                              bool                         production,
                              const dbms::DBConfigurator * dbconf );


    /**
     * return the ID for a given hardware device name
     * @param device   the device name
     * @throws NotFoundException  if name is not matched
     */
    int getDeviceID( const ::std::string & device ) const;
    
    static int getDeviceID( const ::std::string &        device,
                            bool                         production,
                            const dbms::DBConfigurator * dbconf );


    /**
     * return the location name for for an ID
     * @param locationID  the locationID
     * @throws NotFoundException  if name is not matched
     */
    ::std::string getLocation( const int & locationID ) const;

    static ::std::string getLocation( const int &                  locationID,
                                      bool                         production,
                                      const dbms::DBConfigurator * dbconf );


    /**
     * return the device name for for an ID
     * @param deviceID  the deviceID
     * @throws NotFoundException  if name is not matched
     */
    ::std::string getDevice( const int & deviceID ) const;

    static ::std::string getDevice( const int &                  deviceID,
                                    bool                         production,
                                    const dbms::DBConfigurator * dbconf );


private:
    typedef ::std::map< ::std::string, int > Name2IdMap;
    typedef ::std::map< int, ::std::string > Id2NameMap;

    /**
     * construct a PhysicalDeviceIDAuthority object. 
     * @param production if true, connect to the database to verify the id to
     *        name maps of locations and devices match those in the private
     *        arrays of this class. if they do not match, this mismatch 
     *        indicates an integrity problem and a logic_error is thrown
     * @param dbconf pointer to a DBConfiguration object, this is only used
     *        if production is true. If NULL, the default configuration is
     *        used when connecting to the db
     * @throws logic_error if there is a mismatch between the ID->name maps
     *         in the db vs. those in the private arrays of this class
     * @throws DBConnectionException of production=true and the db cannot
     *         be contacted
     */
    PhysicalDeviceIDAuthority( bool                         production,
                               const dbms::DBConfigurator * dbconf );

    /**
     * register a location name-ID pair The purpose of this function is 
     * to ensure that all locations are tagged consistently across multiple 
     * processes.
     *
     * @throws IllegalStateException  if the ID is already registered
     */
    void registerLocationNameID( const ::std::string & location,
                                 const int &           locationID );

    /**
     * register a device name-ID pair The purpose of this function is 
     * to ensure that all devices are tagged consistently across multiple 
     * processes.
     *
     * @throws IllegalStateException  if the ID is already registered
     */
    void registerDeviceNameID( const ::std::string & device,
                               const int &           deviceID );

    static const char * const locationNames_[];
    static const char * const deviceNames_[];

    static const int locationCount_;
    static const int deviceCount_;

    const bool          usingDB_;
    Name2IdMap          deviceName2ID_;
    Name2IdMap          locationName2ID_;
    Id2NameMap          deviceID2Name_;
    Id2NameMap          locationID2Name_;
};


}
}


inline int
carma::dbms::PhysicalDeviceIDAuthority::getLocationCount( )
{
    return locationCount_;
}


inline int
carma::dbms::PhysicalDeviceIDAuthority::getDeviceCount( )
{
    return deviceCount_;
}


#endif // CARMA_DBMS_PHYSICALDEVICEIDAUTHORITY_H
