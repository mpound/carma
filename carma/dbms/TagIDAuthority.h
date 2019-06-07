#ifndef CARMA_DBMS_TAGIDAUTHORITY_H
#define CARMA_DBMS_TAGIDAUTHORITY_H


/**
 * @file
 *
 * TagIDAuthority class.
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */


#include <string>
#include <map>
#include <vector>

#include "carma/util/PthreadRWLock.h"
#include "carma/monitor/types.h"


namespace log4cpp {

class Category;

}  // namespace log4cpp


namespace carma {
namespace dbms {

class DBConfigurator;

/**
 *  A lookup class for mapping names and IDs.  This class is instantiated
 *  as a singleton via a factory method, <code>getAuthority()</code>.
 * <B>Before</B> instantiating the singleton, a call to the static method
 * <code>configureAuthority()</code> can be made to configure the still to be
 * created object to retrieve tagID-canonical name pairs from the database
 * (which is what should be done in production) or to create tagIDs on the
 * fly as needed.
 * Currently the default is to create tagIDs on the fly (i.e., not to use
 * the database).  This default configuration will likely change once
 * we move into production so that the database (with a sensible configuration)
 * is used.
 */

class TagIDAuthority {
public:

    /**
     * get the singleton instance of the TagIDAuthority.
     */
    static TagIDAuthority & getAuthority( );

    virtual ~TagIDAuthority( );

    static ::std::string getDefaultCanonicalNamesToTagIDsConfFile( );

    /**
     * configure the singleton, this should be called <b>before</b> the
     * initial call to getAuthority; the singleton must be configured before
     * it is created
     * @param usedb use the database to get tagIDs?
     * @param dbconf db config file to use to configure the db connection,
     *        only used if @p usedb=true
     * @param useConfFileIfDBIsDown if usedb is true, and the database cannot
     *        be contacted, use a config file with name to tagID mappings for
     *        the tagID source
     * @param canonicalNamesToTagIDsConfFile the conf file which maps
     *        canonical names to tagIDs which is used when the database
     *        cannot be contacted. This file should have been written by
     *        querying the database when it was up, or the database may lose
     *        integrity. Only used if @p usedb=true and
     *        @p useConfFileIfDBIsDown=true
     * @throws NotFoundException if usedb=true and dbConfFile does not exist
     *         or is not readable
     */
    static void configureAuthority(
        const bool            useDb,
        const ::std::string & dbConfFile,
        const bool            useConfFileIfDBIsDown = true,
        const ::std::string & canonicalNamesToTagIDsConfFile =
            getDefaultCanonicalNamesToTagIDsConfFile( ) );

    /**
     * indicate that one's use of the authority is complete.  If we are
     * running in production mode, the singleton instance will be deleted.
     * It is safe to call getAuthority() at anytime before or after this
     * function is called.
     */
    static void closeAuthority() ;

    /**
     * @return the Tag ID for a given hierarchical name
     * @param name   the full hierarchical name
     * @throws NotFoundException  if name is not matched
     */
    carma::monitor::tagIDType lookupID( const ::std::string & name ) const;

    /**
     * return the subsystem ID for a given subsystem name.
     * This implementation is
     * identical to lookupID(const ::std::string&) where the result is
     * demoted to a unsigned short.
     * @param name   the subsystem name
     * @throws NotFoundException  if name is not matched
     */
    unsigned short lookupSubsystemID( const ::std::string & name ) const;

    bool tagIdAssignedOnTheFly( const ::std::string & name ) const;

    bool tagIdAssignedOnTheFly( carma::monitor::tagIDType tagId ) const;

    /**
     * return the full hierarchical name for for an ID
     * @param tagID  the Tag ID
     * @throws NotFoundException  if name is not matched
     */
    ::std::string lookupName( carma::monitor::tagIDType tagId ) const;

    /**
     * return the subsystem name from a monitor component's canonical name
     * the singleton does not need to exist (the subsystem name is just
     * extracted from the canonicalName string)
     * @param canonicalName the canonicalName of the monitor point
     * @return the subsystem name corresponding to the canonicalName
     */
    static ::std::string
    getSubsystemName( const ::std::string & canonicalName );

    /**
     * return the subsystem name for the corresponding subsystem id
     * the singleton object does not need to be instantiated
     * the name is gotten from the subsysnames_ array
     * (located in carma/util/subsystemnames.cc).  The database has been
     * initilized to use this array when creating the Subsystems table.
     * Thus, the subsystem name corresponding to a given id will be the
     * same whether or not the database is being used to obtain monitor
     * configuration information
     * @param subsysID  the subsystem id (1-based, a value of 0 or greater
     *        than the total number of subsystems results in a
     *        IllegalArgumentException being thrown)
     * @return the corresponding subsystem name
     * @throws IllegalArgumentException if subsysID == 0 or
     *         greater than the number of subsystems
     */
    static ::std::string getSubsystemName( const unsigned subsysID );

    /**
     * return the number of subsystems, the singleton object does not need to
     * be instantiated
     * @return the total number of subsystems
     */
    static int getSubsystemCount();

    /**
     * Crafts a tagID given the subsystemID and the ID of the monitor point
     * (pointID) and returns the tagID. Class static method.
     *
     * @param subsystemID unsigned short subsystem ID 
     * @param pointID unsigned short pointID 
     * @return tagID composed using the specified subsystemID 
     *         and the pointID.
     */
    static carma::monitor::tagIDType composeTagID( 
        unsigned short subsystemId, unsigned short monitorPointId );

    /**
     * Converse of composeTagID. Returns subsystemID 
     * corresponding to a specified tagID. Class static method.
     *
     * @param tagID 
     * @return unsigned short subsystemID corresponding to specified tagID.
     */
    static unsigned short getSubsystemID( carma::monitor::tagIDType tagId );

    /**
     * Converse of composeTagID. Returns pointID 
     * corresponding to a specified tagID. Class static method.
     *
     * @param tagID 
     * @return unsigned short pointID corresponding to specified tagID.
     */
    static unsigned short getPointID( carma::monitor::tagIDType tagId );

    static unsigned short maxPossiblePointID( unsigned short subsystemId );

    /**
     * get the sha1 sum of the tagID to canonical name map.
     * @return the sha1sum of the tagID-canonical name map
     */
    ::std::string tagIDNameMapSha1Sum() const;

    struct TagIdOriginStats {
        size_t total;
        size_t totalDb;
        size_t totalConfFile;
        size_t totalOtf;

        // These maps are sparse. They only have an entry for a subsystem ID
        // if the value for that subsystem ID is non-zero.
        ::std::map< unsigned short, size_t > subsystemTotal;
        ::std::map< unsigned short, size_t > subsystemDb;
        ::std::map< unsigned short, size_t > subsystemConfFile;
        ::std::map< unsigned short, size_t > subsystemOtf;
    };

    TagIdOriginStats collectTagIdOriginStats( ) const;

    void logTagIdOriginStats() const;

    void logNotableOnTheFlyCreationsThusFarAndInTheFuture();

    void
    logNotableOnTheFlyCreationsThusFarAndInTheFuture( size_t targetLineCount );

private:
    struct StrictlyWeakStringOrdering {
        bool operator()( const ::std::string & lhs,
                         const ::std::string & rhs ) const;
    };

public:

    typedef enum {
        SUBSYS_TABLE_ORIGIN,
        DB_ORIGIN,
        CONF_FILE_ORIGIN,
        ON_THE_FLY_ORIGIN
    } OriginType;

private:

    typedef enum {
        SUBSYS_DOES_NOT_EXIST,
        SUBSYS_ID_MISMATCH,
        SUBSYS_MATCHES
    } SubsysMatchType;

public:

    struct NameMapInfo {
        carma::monitor::tagIDType tagId_;
        OriginType origin_;

        NameMapInfo( carma::monitor::tagIDType id, OriginType origin );
    };

    /**
     * Retrieve tag id and origin information for a given canonical name.
     * @param canonicalName String representation of canonical name.
     * @param nameMapInfo Object to fill - unmodified if tag not found.
     * @return True if tag found, false otherwise.
     */ 
    bool retrieveTagInfo( const std::string & canonicalName, 
                          NameMapInfo & nameMapInfo ) const;

private:

    typedef ::std::map< ::std::string, NameMapInfo, StrictlyWeakStringOrdering >
            Name2InfoMap;

    typedef ::std::map< ::std::string, carma::monitor::tagIDType, StrictlyWeakStringOrdering >
            Name2IdMap;

    typedef ::std::map< carma::monitor::tagIDType, ::std::string > Id2NameMap;

    TagIDAuthority( bool                  canUseDb,
                    bool                  canUseConfFile,
                    const ::std::string & dbConfFile,
                    const ::std::string & tagsConfFile );

    /**
     * register a name-ID pair
     * @throws IllegalStateException  if the ID is already registered
     */
    void registerNameHoldingWriteLock( const ::std::string & name,
                                       carma::monitor::tagIDType tagId,
                                       OriginType            origin );

    /**
     * register the known subsystem names.  This is called during construction
     * when the database is not available.  The purpose of this function is
     * to ensure that all subsystems are tagged consistently across multiple
     * processes.
     */
    void registerSubsystemNamesHoldingWriteLock( );

    /**
     * get the the names to tagIDs map from the conffile which was previously
     * written by the db
     */
    void getNamesToTagIDsFromConfFile( const ::std::string & fileName );
    void getNamesToTagIDsFromBinaryConfFile( const ::std::string & fileName );
    void getNamesToTagIDsFromTextConfFile( const ::std::string & fileName );

    void registerNameIDsUsingDB( const dbms::DBConfigurator * dbconf );

    carma::monitor::tagIDType 
    convertNameToTagIdHoldingLock( const ::std::string & name ) const;

public:

    carma::monitor::tagIDType
    findIdOrAssignOtf( const ::std::string & name,
                       bool &                assignedOTF,
                       const ::std::string & subsysName );

private:

    TagIdOriginStats collectTagIdOriginStatsHoldingLock( ) const;

    SubsysMatchType checkSubsysMatchHoldingLock(
        const ::std::string &          canonicalName,
        carma::monitor::tagIDType      nonSubsysTagId,
        Name2InfoMap::const_iterator & hint ) const;

    size_t estimateMemHoldingLock() const;


    // subsysnames_ is initialized in subsystemnames.cc
    static const char * const subsysnames_[ ];

    // subsysnamesCount_ is initialized in subsystemnames.cc
    static const int subsysnamesCount_;


    mutable util::PthreadRWLock guard_;

    OriginType          masterOrigin_;
    ::std::string       sha1sum_;

    Name2InfoMap        name2Info_;
    Id2NameMap          id2Name_;
    Name2IdMap          subsys2NextOtfId_;

    bool                logNotableOtfCreations_;
    bool                haveUnloggedNotableOtfCreations_;
};


}  // namespace carma::dbms
}  // namespace carma


inline carma::monitor::tagIDType
carma::dbms::TagIDAuthority::composeTagID( const unsigned short subsystemId,
                                              const unsigned short monitorPointId )
{
    return ((static_cast< carma::monitor::tagIDType >( subsystemId ) << 16) |
            static_cast< carma::monitor::tagIDType >( monitorPointId ));
}

inline unsigned short 
carma::dbms::TagIDAuthority::getSubsystemID( const carma::monitor::tagIDType tagId )
{
    return (tagId >> 16);
}


inline unsigned short 
carma::dbms::TagIDAuthority::getPointID( const carma::monitor::tagIDType tagId )
{
    return (tagId & 0x0000FFFF);
}

inline unsigned short
carma::dbms::TagIDAuthority::maxPossiblePointID( const unsigned short subsystemId )
{
    return 0x0000FFFF;
}


#endif // CARMA_DBMS_TAGIDAUTHORITY_H
