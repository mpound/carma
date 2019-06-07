#ifndef CARMA_DBMS_MONITORCONFIGURATIONDATABASE_H
#define CARMA_DBMS_MONITORCONFIGURATIONDATABASE_H

/**
 * @file
 * MonitorConfigurationDatabase
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#include <map>
#include "carma/dbms/Table.h"
#include "carma/dbms/ColumnNames.h"
#include "carma/dbms/Typedefs.h"
#include "carma/util/Logger.h"
#include "carma/util/Time.h"

namespace carma {
namespace dbms {

    class DBConnection;
    class MonitorDescription;

/**
 * This class contains methods for accessing monitor configuration information
 */
class MonitorConfigurationDatabase {

public:
    /**
     * constructor
     */
    explicit MonitorConfigurationDatabase( const DBConnection * dbc );

    //--------------- Subsystems table access methods --------------------

    /**
     * populate the Subsystem table using values from 
     * monitor::TagIDAuthority::subsysnames
     */
    void populateSubsystemsTable( ) const;

    /**
     * get the ID for the specified subsystem
     * @param subsys the subsystem name
     * @param ignore the case of the input subsystem name when looking for
     *        a matching value in the DB
     * @return subsystem ID, or -1 if no such subsytem is in the DB
     */
    short getSubsystemID( const ::std::string & subsysName, 
                          bool                  ignoreCase = false ) const;

    /**
     * get the subsystem name associated with the specified subsystem ID
     * callers are responsible for deleting the returned pointer to prevent
     * memory leaks
     * @param subsysID the subsystem ID
     * @return the subsystem name or a null pointer if subsystem ID does not
     *         exist in the DB
     * @throws DBConnectionException
     */
    ::std::string * getSubsystemName( unsigned short subsysID ) const;

    /**
     * get the subsystem table
     * @return the subsystem Table
     */
    dbms::Table getSubsystemsTable( ) const;

    //-------------- AggregateSubsystems table access methods -------------
    
    /**
     * insert a record into the aggregate subsystems table
     * @param name the aggregate subsystem name
     * @param count the number of subsystems in the aggregate
     * @param maxsamples the maxsamples value in the aggregate
     * @param maxpoints the maxpoints value in the aggregate
     * @throws DBConnectionException
     */

    void insertAggregateSubsystemsRecord(
        const ::std::string & name,
        unsigned                  count,
        unsigned                  maxsamples,
        unsigned                  maxpoints ) const;

    /**
     * get the aggregate subsystems table
     * @return the aggregate subsystems table
     */
    dbms::Table getAggregateSubsystemsTable( ) const;

    /**
     * get the value of maxpoints summed over all subsystems
     * @return the summed value of maxpoints
     */
    unsigned getTotalMaxPoints( ) const;

    /**
     * get the value of maxsamples summed over all subsystems
     * @return the summed value of maxsamples
     */
    unsigned getTotalMaxSamples( ) const;

    //--------------- Locations table access methods --------------------
    /**
     * @brief populate the Locations table
     *
     * populate the Locations table using values from 
     * monitor::PhysicalDeviceIDAuthority::locationNames_
     * if an ID (array index) already exists in the table, no attempt is
     * made to enter it again.  That is, only new entries in the array
     * are inserted into the table
     */
    void populateLocationsTable( ) const;

    /**
     * get the location table
     * @return the location Table
     */
    dbms::Table getLocationsTable( ) const;

    //--------------- Devices table access methods --------------------
    /**
     * populate the Devices table using values from 
     * monitor::PhysicalDeviceIDAuthority::deviceNames
     * if an ID (array index) already exists in the table, no attempt is
     * made to enter it again.  That is, only new entries in the array
     * are inserted into the table
     */
    void populateDevicesTable( ) const;

    /**
     * get the devices table
     * @return the devices Table
     */
    dbms::Table getDevicesTable( ) const;

    /**
     * add a monitor point configuration to the database
     * @param md the MonitorDescription to insert
     * @param updateOnlyIfNecessary update a previous description with the
     *        same name only if the description's field, other than the 
     *        frameCount field has changed (that is, don't add this 
     *        description to the db if a previous similar description exists 
     *        and the only difference is the frameCount fields)
     * @return the tagID of the new monitor configuration
     * @throws out_of_range if number of monitor points in the specified 
     *         subsystem exceeds 65536
     * @throws DBConnectionException if there is a problem with the db 
     *         connection
     * @throws InsertDeniedExcpetion if the named configuration exists and
     *         there is a change in one of its static parameters
     * @throws SQLException if there is a problem when executing the query
     */
    unsigned insertMonitorConfiguration(
        const MonitorDescription & md,
        bool  updateOnlyIfNecessary = true ) const;

    /** As above, but may return an error string instead of throwing
     *  an InsertDeniedException.
     * @param msg Formatted message string to be returned.
     * @param noInsertDenied If true, don't throw InsertDeniedException.
     *        Return a formatted error string instead.
     */
    unsigned insertMonitorConfiguration(
	const MonitorDescription & Md,
	::std::string &msg,
	bool updateOnlyIfNecessary = true,
	bool noInsertDenied=false ) const;

    /**
     * Change a value in the static parameter table for a monitor point.
     * NOTE: This is not a general purpose editing routine.
     * The table is meant to be static. Calling this routine implies
     * there was a problem that needs to be fixed. If care is not taken,
     * the database could be corrupted.
     * An entry describing the change is made in the MP change log table.

     * @param disposition How to handle - edit or new/rename/make alias.
     * @param tagID tagID of monitor point to be changed.
     * @param mpName Name of monitor point to be changed.
     * @param fieldName Name of field in MonitorConfigStaticParms to be
     *        changed.
     * @param newValue New value to be placed in field.
     * @param requester User name of user requesting the change.
     * @return true if change succeeded. false otherwise.
     * @throws DBConnectionException if there is a problem with the db 
     *         connection.
     * @throws SQLException if there is a problem when executing the query.
     */
    bool mutateStaticParms(const ::std::string &disposition,
			   unsigned tagID, const ::std::string &mpName,
			   const ::std::string &fieldName,
			   const ::std::string &newValue,
			   const ::std::string &requester);

    /** Same as above, but the args are in one string.
     * @param changeString One line from the output of
     *         monitorConfigurationLoader edited to include the login name
     *         of the person requesting the change and the disposition.
     */
    bool mutateStaticParms(const ::std::string &changeString);

    /**
     * get monitor description associated with the specified canonical name
     * which is valid at the specified frameCount.  Callers are responsible 
     * for deleting the returned pointer to prevent a memory leak
     * @param canonicalName canonical name of the monitor description
     * @param frameCount get the configuration which is valid at this 
     *        frameCount
     * @return the monitor description 
     * @throws NotFoundException if no description with the the specified
     *         tagID exists at the specified frameCount
     * @throws DBConnectionException
     */
    MonitorDescription getMonitorConfiguration(
        const ::std::string & canonicalName,
        util::frameType       frameCount =
            util::Time::computeCurrentFrame( ) ) const;


     /**
      * get the full monitor configuration table (static parms table joined
      * with changable parms table + joins with auxiliary tables to convert
      * IDs to strings
      * the SQL query which is used to produce this table is
      * <code>SELECT S.tagID,frameCount, S.name, shortName, longName, units, 
      * updateInterval, description, isPersistent, isSpectrum, dataTypeID, 
      * warnLo, warnHi, errLo, errHi, L.name AS location, D.name AS device, 
      * mpTypeID FROM MonitorConfigStaticParms AS S JOIN 
      * MonitorConfigChangeableParms AS C ON S.tagID=C.tagID JOIN  
      * Locations AS L ON S.locationID=L.locationID JOIN Devices AS D ON 
      * S.deviceID=D.deviceID WHERE [C.tagID IN (<tagIDs>) AND] 
      * frameCount<=<frameCount> ORDER BY tagID,frameCount DESC </code>
      * Thus, for a given tagID, the most recent (ie, valid for <frameCount)
      * description appears first in the table, other (invalid) 
      * configurations for that tagID appear in the rows immediately following
      * the valid configuration for that tagID.  The tagID component in the
      * WHERE clause is not used if the request is for all tagIDs (ie the
      * tagID parameter is NULL). Rows of this table are suitable to pass
      * to tableRow2MonitorDescription
      * @see tableRow2MonitorDescription
      * @param tagIDs the tagIDs for which to get monitor configuration info
      *        a NULL pointer means get info for all tagIDs in the DB,
      *        which is not expensive to do
      * @param frameCount return configuration info that is valid at this
      *        frameCount
      * @param retrieveOnlyValidConfigs get only the monitor configurations 
      *        which are valid at the specified @p frameCount. If true, a
      *        "GROUP BY tagID" clause is added to the select statement,
      *        so that each tagID is guaranteed to have only a single entry
      * @return the full monitor configuration table
      */
    dbms::Table getFullMonitorConfigurationTable(
        const ::std::vector< int > * const tagIDs = 0,
        bool                               retreiveOnlyValidConfigs = false,
        util::frameType                    frameCount =
            util::Time::computeCurrentFrame( ) ) const;


     /**
      * get monitor configuration info for the specified tagIDs
      * @param tagIDs the tagIDs for which to get monitor configuration info
      *        a NULL pointer means get info for all tagIDs in the DB, but
      *        don't do that, because its extremely expensive
      * @param frameCount return configuration info that is valid at this
      *        frameCount
      * @return the tagID->MonitorDescription map
      */
    dbms::ID2MDMap getMonitorConfigurations(
        const ::std::vector< int > * tagIDs,
        util::frameType              frameCount =
            util::Time::computeCurrentFrame( ) ) const;

    /**
     * get monitor description associated with the specified tagID which is
     * valid at the specified frameCount.  Callers are responsible for deleting
     * the returned pointer to prevent a memory leak
     * @param tagID ID of the monitor point
     * @param frameCount get the configuration which is valid at this 
     *        frameCount
     * @return the monitor description 
     * @throws NotFoundException if no description with the the specified
     *         tagID exists at the specified frameCount
     * @throws DBConnectionException
     */
    MonitorDescription getMonitorConfiguration(
        int             tagID,
        util::frameType frameCount = util::Time::computeCurrentFrame( ) ) const;

    /**
     * get the maximum tagID for the specified subsystem
     * @param subsysID the subsystem ID for which to find the maximum tagID
     * @return the maximum tagID for the specified subsystem, or 0 if the
     *         subsystem has no associated monitor points
     * @throws DBConnectionException
     */
    unsigned getMaxTagID( unsigned short subsysID ) const;

    /**
     * Does the specified tagID exist in the DB?
     * @param tagID the tagID for which to check the existence
     * @return true if the tagID exists
     * @throws DBConnectionException
     */
    bool tagIDExists( int tagID ) const;

    /**
     * get the conanical container name and monitor point name of a monitor 
     * point with the specified canonical name
     * example getContainerName("A.B.C.D") returns "A.B.C" for the canonical
     *          container name and "D" for the monitor point name
     * @param hname [in] canonical name of the monitor point
     * @param cname [out] canonical container name
     * @param mpname [out] (non-canonical) monitor point name
     */
    void getContainerAndMPNames( const char *    hname,
                                 ::std::string & cname, 
                                 ::std::string & mpname) const;

    /**
     * get a map of tagid's to canonical names of monitor points
     * @return tagID->canonical name map
     * @throws DBConnection exception
     */
    ::std::map< int, ::std::string > getTagID2NameMap( ) const;

    /**
     * get a table containing the tagID to dataTypeID mappings for all monitor
     * points
     */
    dbms::Table getTagIDToDataTypeIDTable( ) const;


    /**
     * get the canonical name for a monitor point associated with the specified tagId
     * @param tagID tagID of the monitor point for which to get the canonical name
     * @return the corresponding canonical name or empty string if the
     * monitor point doesn't exist
     * @throws DBConnectionException
     */
    ::std::string getCanonicalName( const int tagID ) const;

    /**
     * get the canonical name for a monitor point associated with the specified tagId
     * @param tagID tagID of the monitor point for which to get the canonical name
     * @return the corresponding canonical name or empty string if the
     * monitor point doesn't exist
     * @throws DBConnectionException
     */
    ::std::string getCanonicalName( const ::std::string & tagID ) const;

    /**
     * get the tagID associated with the specified monitor point name
     * @param mpName canonical name of the monitor point for which to get the
     *        tagID
     * @return the corresponding tagID or -1 if the monitor point doesn't
     *         exist
     * @throws DBConnectionException
     */
    int getTagID( const ::std::string & mpName ) const;

    /**
     * get the tagIDs corresponding to the specified canonical names
     * @param canonicalNames monitor point names for which to get the tagIDs
     *        a NULL pointer means get all the tagIDs in the db
     * @return the tagIDs corresponding to the specified canonical names
     * @throws NotFoundException if any of the specified canonical names do
     *         not exist in the db
     */
    //dbms::Column<int> getTagIDs
    //(const ::std::vector<::std::string>* const canonicalNames=0) const;

    /**
     * get the tagID->canonoical name mapping of a possibly wildcarded 
     * canonical name using an SQL LIKE filter.
     * @param wildcardedName the wild carded canonical name
     * @param multiCharacterWildcard this string represents the multicharacter
     *        wildcard in the input string and is translated into the SQL
     *        multicharacter wildcard % when the search is performed
     * @param multiCharacterWildcard this string represents the multicharacter
     *        wildcard (matches 0,1, or any other number of characters) 
     *        in the input string and is translated into the SQL
     *        multicharacter wildcard (%) when the search is performed
     * @param singleCharacterWildcard this string represents the single 
     *        character matches exactly 1 character)
     *        in the input string and is translated into the SQL
     *        single character wildcard (_) when the search is performed.
     *        Note this is not the same as the regexp "?" since "?" in regexp
     *        also matches 0 characters
     * @return a map of tagIDs to canonical names which fulfill the search
     *         criterion
     */
    ::std::map< int, ::std::string > getTagIDs(
        const ::std::string & wildcardedName, 
        const ::std::string & multiCharacterWildcard = "%",
        const ::std::string & singleCharacterWildcard = "_" ) const;
         
    /**
     * @brief does the monitor configuration exist in the db?
     * @param canonicalName canonical (full hierarchical) name of the monitor
     *        configuration
     * @throws DBConnection if there is a problem with the connection to the
     *         database
     * @throws SQLException if there is a problem executing the SQL query
     */
    bool monitorConfigExists( const ::std::string & canonicalName ) const;

    /**
     * populate the Validities table
     */
    void populateValiditiesTable( ) const;

    /**
     * get the Validities table
     * @return the Validities table
     */
    dbms::Table getValiditiesTable( ) const;

    /**
     * populate the Validities table
     */
    void populateBlankingFlagsTable( ) const;

    /**
     * get the BlankingFlags table
     * @return the BlankingFlags table
     */
    dbms::Table getBlankingFlagsTable( ) const;

    /**
     * populate the MonitorPointDataTypes table
     */
    void populateMonitorPointDataTypesTable( ) const;

    /**
     * get the MonitorPointDataTypes table
     * @return the MonitorPointDataTypes table
     */
    dbms::Table getMonitorPointDataTypesTable( ) const;

    /**
     * populate the MonitorPointTypes table
     */
    void populateMonitorPointTypesTable( ) const;

    /**
     * get the MonitorPointTypes table
     * @return the MonitorPointTypes table
     */
    dbms::Table getMonitorPointTypesTable( ) const;


    /**
     * transform a monitor configuration represented by a table row into
     * a MonitorDescription object.  The table row should have all the columns
     * in the table produced by getFullMonitorConfigurationTable()
     * @see getFullMonitorConfigurationTable
     * @param t the table
     * @param row the row number of t to transform
     * @param tagID the tagID of the associated monitor configuration
     * @return the MonitorDescription
     */
    dbms::MonitorDescription tableRow2MonitorDescription(
        const dbms::Table & t,
        int                 row ) const;

    /**
     * get the maximum length permitted for the shortName of a monitor point
     * @return the maximum length permitted for the shortName of a monitor 
     *         point
     */
    //static unsigned shortNameMaxLength() { return 64; }

    /**
     * insert a record into the TagIDNameSignatures table
     * @param frameCount frameCount at which the signature is valid
     * @param signature the tagID-Name map signature
     * @param insertOnlyIfNecessary insert this record only if the signature
     *        differs from the signature of the previous record in the table
     * @return true if the table was actually updated (will return false only
     *         in some cases when updateOnlyIfNecessary=true)
     */
    bool insertTagIDNameSignaturesRecord(
        util::frameType       frameCount, 
        const ::std::string & signature,
        bool                  updateOnlyIfNecessary = true ) const;

    /**
     * get the TagIDNameSignatures table
     * @param orderBy the column to order the table by, (NULL -> don't bother 
     *        sorting)
     * @param sortOrder the order in which to sort
     * @return the TagIDNamesSignatures table
     */
    dbms::Table getTagIDNameSignatures(
        const dbms::DBColumn * orderBy,
        dbms::SortOrder        sortOrder = dbms::DESCENDING ) const;

    /**
     * get the currently valid tagID->canonical names map signature
     * the returned pointer is created by by this method using <code>new</code>
     * so clients are responsible for deleting it to avoid memory leaks
     * (or just use an <code>auto_ptr</code> if possible)
     * @return a pointer to the string containing the current signature, or
     *         NULL if there is no valid pointer (ie if the database table 
     *         contains no records
     */
    ::std::string * getCurrentTagIDNameSignature( ) const;

    /**
     * get the tagID->canonical names map signature valid at the specified
     * frame count. the returned pointer is created by by this method 
     * using <code>new</code> so clients are responsible for deleting it to 
     * avoid memory leaks (or just use an <code>auto_ptr</code> if possible)
     * @param frameCount the frame count for which the returned signature
     *        is valid
     * @return a pointer to the string containing the current signature, or
     *         NULL if there is no valid pointer (ie if the database table 
     *         contains no records
     */
    ::std::string * getTagIDNameSignature(
        util::frameType frameCount= util::Time::computeCurrentFrame( ) ) const;


private:

    /**
     * set the enumerators in the specified monitor configuration by
     * querying the database
     * @param tagID the tagID corresponding to the monitor configuration
     * @param md the monitor configuration for which to set enumerators
     */
    void setEnumerators_( int                        tagID,
                          const ::std::string &      name,
                          dbms::MonitorDescription & md ) const;


    /**
     * get the enumerators for the specified tagID by querying the database
     * @param tagID [in] the tagID of the monitor point in question
     * @param name [in] the name of the monitor point in question (used only
     *        if an error is thrown), if NULL, it will be retrieved from the
     *        database if necessary using its tagID
     * @param enumValues [out] the enumerator values
     * @param enumDescriptions [out] the enumerator descriptions
     * @param enumIndices [out] the enumerator indices
     */
    void getEnumerators_( unsigned                            tagID,
                          const ::std::string *           name,
                          dbms::Column< ::std::string > & enumValues, 
                          dbms::Column< ::std::string > & enumDescriptions, 
                          dbms::Column< short > &         enumIndices ) const;


    /**
     * insert enumerators into the db
     */
    void insertEnumerators_( unsigned                             tagID, 
                             const dbms::MonitorDescription & md ) const;

    /********************************************************************/
    /*		Routines to support editing of static fields.		*/
    /** Edit an existing enumerator in a way it wasn't meant to be edited.
	This could cause problems if called carelessly.
	Returns true iff the change was done, else false.
     */
    bool mutateEnumerator(
     const MonitorDescription &existingMD,
     carma::util::frameType frameCount,
     unsigned tagID, const std::string &mpName,
     const std::string &fieldName, const std::string &newValue,
     const std::string &username);

    /* Get max version # for this monitor point. */
    int getMaxVersionNumber(const std::string &mpname) const;

    /** Change (increment) the version # of a monitor point.
     */
    bool makeNewMPVersion(const MonitorDescription &md,
			  carma::util::frameType frameCount,
			  unsigned tagID, const ::std::string &mpName,
			  const ::std::string &userName);


    /* Make an entry in the static parameter change log table indicating
     * what was done.
     * This is usually called from within the same try/catch block that
     * contains the SQL command changing the database.
     * This is only called by the mutate routines and is used to keep
     * track of potentially dangerous changes.
     */
    void makeStaticParamChangeLogEntry(carma::util::frameType frameCount,
	unsigned tagID,
	const ::std::string &mpname, const ::std::string &fieldname,
	const ::std::string &newValue, const ::std::string &oldValue,
	const ::std::string &userName, int index=0);
    /********************************************************************/

    /** Returns true & the enumID for a name/description pair. If not found,
     * returns false.
     */
    bool getEnumID(const ::std::string &enumName,
		   const ::std::string &description,
		   int &enumID);
    /**
     * get the ID for the specified location
     * @param locationName the location name
     * @param ignore the case of the input location name when looking for
     *        a matching value in the DB
     * @return location ID, or -1 if no such subsytem is in the DB
     */
    short getLocationID_( const ::std::string & locationName, 
                          bool                  ignoreCase = false ) const;

    /**
     * get the location name associated with the specified location ID
     * callers are responsible for deleting the returned pointer to prevent
     * memory leaks
     * @param locationID the location ID
     * @return the location name or a null pointer if location ID does not
     *         exist in the DB
     * @throws DBConnectionException
     */
    ::std::string * getLocationName_( unsigned short locationID ) const;

    /**
     * get the ID for the specified device
     * @param deviceName the device name
     * @param ignore the case of the input device name when looking for
     *        a matching value in the DB
     * @return device ID, or -1 if no such device is in the DB
     */
    short getDeviceID_( const ::std::string & deviceName, 
                        bool                  ignoreCase = false ) const;

    /**
     * get the device name associated with the specified device ID
     * callers are responsible for deleting the returned pointer to prevent
     * memory leaks
     * @param deviceID the device ID
     * @return the device name or a null pointer if device ID does not
     *         exist in the DB
     * @throws DBConnectionException
     */
    ::std::string * getDeviceName_( unsigned short deviceID ) const;


    const DBConnection * dbc_;
    log4cpp::Category &  logger_;
};


}
}


#endif // CARMA_DBMS_MONITORCONFIGURATIONDATABASE_H
