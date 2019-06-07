/**
 * Implementation for TagIDAuthority
 *
 * @author: Dave Mehringer, Ray Plante
 *
 * $Id: TagIDAuthority.cc,v 1.6 2014/01/14 23:32:25 iws Exp $
 * $CarmaCopyright$
 *
 */
#include <carma/dbms/TagIDAuthority.h>
#include <carma/dbms/DBMS.pb.h>

#include <fstream>
#include <sstream>

#include <openssl/evp.h>

#include <carma/dbms/ColumnNames.h>
#include <carma/dbms/DBConnection.h>
#include <carma/dbms/DBConfigurator.h>
#include <carma/dbms/MonitorConfigurationDatabase.h>
#include <carma/dbms/Table.h>
#include <carma/dbms/TableNames.h>
#include <carma/util/CommonExceptions.h>
#include <carma/util/FileUtils.h>
#include <carma/util/KeyValueConfigFile.h>
#include <carma/util/Program.h>
#include <carma/util/programLogging.h>
#include <carma/util/PthreadMutex.h>
#include <carma/util/ScopedLock.h>
#include <carma/util/ScopedExclusiveLock.h>
#include <carma/util/ScopedSharedLock.h>
#include <carma/util/StringUtils.h>
#include <carma/util/Trace.h>
#include <carma/util/Backtrace.h>
#include <carma/util/StopWatch.h>
#include <carma/util/Time.h>

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;
using namespace carma::dbms;
using carma::util::StopWatch;



namespace {

typedef ScopedSharedLock< PthreadRWLock > GuardReadLockType;
typedef ScopedExclusiveLock< PthreadRWLock > GuardWriteLockType;


bool
isSubsysTagID( const tagIDType tagId )
{
    return ((tagId >= 0) && (tagId <= 0xffff));
}


bool
innerStrictlyWeakStringOrdering( const string & lhs,
                                 const string & rhs )
{
    return (lhs < rhs);
}


}  // namespace < anonymous >


inline bool
TagIDAuthority::StrictlyWeakStringOrdering::operator()(
    const string & lhs,
    const string & rhs ) const
{
    const string::size_type lhsSize = lhs.size();
    const string::size_type rhsSize = rhs.size();

    if ( lhsSize < rhsSize )
        return true;

    if ( rhsSize < lhsSize )
        return false;

    return innerStrictlyWeakStringOrdering( lhs, rhs );
}


string
TagIDAuthority::getDefaultCanonicalNamesToTagIDsConfFile( )
{
    return Program::getConfFile( "monitor/canonicalNamesToTagIDs.conf" );
}


TagIDAuthority::NameMapInfo::NameMapInfo( const tagIDType  tagId,
                                          const OriginType origin ) :
tagId_( tagId ),
origin_( origin )
{
}


unsigned short
TagIDAuthority::lookupSubsystemID( const string & name ) const
{
    return static_cast< unsigned short >( lookupID( name ) );
}


TagIDAuthority::TagIDAuthority( const bool     canUseDb,
                                const bool     canUseConfFile,
                                const string & dbConfFile,
                                const string & tagsConfFile ) :
guard_(),

masterOrigin_( ON_THE_FLY_ORIGIN ),
sha1sum_(),

name2Info_(),
id2Name_(),
subsys2NextOtfId_(),

logNotableOtfCreations_( false ),
haveUnloggedNotableOtfCreations_( false )
{

    StopWatch getID_DB(StopWatch::WALL_CLOCK,"DB connection time");
    StopWatch getID_conf(StopWatch::WALL_CLOCK,"conf read time");
    CARMA_CPTRACE( Trace::TRACE2, "TagIDAuthority constructor called" );

    try {
        string btText;

        {
            Backtrace bt;

            bt.captureNoThrow();

            btText = bt.formattedAsString( "    ", "\n" );
        }

        CARMA_CPTRACE( Trace::TRACE2, "Backtrace is:\n" << btText );
    } catch ( ... ) {
        // Just stifle any exception
    }

    {
        const GuardWriteLockType writeLock( guard_ );

        registerSubsystemNamesHoldingWriteLock();
    }

    string tagsConfFileName = tagsConfFile;
    if ( canUseConfFile && tagsConfFileName.empty() ) {
        tagsConfFileName =
            Program::getConfFile( "monitor/canonicalNamesToTagIDs.conf" );
    }

    if ( canUseDb ) {
        string dbConfFileName = dbConfFile;

        if ( dbConfFileName.empty() )
            dbConfFileName = Program::getConfFile( "dbms/dbms.remote.conf" );

        auto_ptr<DBConfigurator> dbconf( new DBConfigurator( dbConfFileName ) );

        if ( DBConnection::isUp( dbconf.get() ) ) {
	    getID_DB.start();
            masterOrigin_ = DB_ORIGIN;

            try {
                registerNameIDsUsingDB( dbconf.get() );
            } catch ( const DBConnectionException & exc ) {
                if ( canUseConfFile ) {
		    getID_conf.start();
                    masterOrigin_ = CONF_FILE_ORIGIN;

                    getNamesToTagIDsFromConfFile( tagsConfFileName );
		    getID_conf.stop();
                } else {
                    string emsg = " Error reading values from the database ";
                    emsg += "and the singleton has been configured not to ";
                    emsg += "retrieve tagIds from a config file";

                    throw CARMA_EXCEPTION(DBConnectionException,
                                          exc.what() + emsg);
                }
            }
	    getID_DB.stop();
	    const double getID_DB_Secs = getID_DB.getCumulativeElapsedTime( true );
	    const double getID_conf_Secs = getID_conf.getCumulativeElapsedTime( true );
	    ostringstream msg;
	    msg << "DB Connection stats: DB " << getID_DB_Secs;
	    msg << "  conf " << getID_conf_Secs;
	    carma::util::programLogInfoIfPossible(msg.str());
        } else if ( canUseConfFile ) {
	    getID_conf.start();
            {
                string msg;

                msg += "Database cannot be contacted, so using the ";
                msg += "canonical name to tagId map in conf file ";
                msg += tagsConfFileName + ". ";
                msg += "It had better match that in the database or db ";
                msg += "integrity will be compromised";

                programLogWarnIfPossible( msg );
                CARMA_CPTRACE( Trace::TRACE1, msg );
            }

            masterOrigin_ = CONF_FILE_ORIGIN;

            getNamesToTagIDsFromConfFile( tagsConfFileName );
	    getID_conf.stop();
	    const double getID_DB_Secs = getID_DB.getCumulativeElapsedTime( true );
	    const double getID_conf_Secs = getID_conf.getCumulativeElapsedTime( true );
	    ostringstream msg;
	    msg << "DB Connection stats: DB " << getID_DB_Secs;
	    msg << "  conf " << getID_conf_Secs;
	    carma::util::programLogInfoIfPossible(msg.str());
        } else {
            string emsg = "The database cannot be contacted and the ";
            emsg += "singleton has been configured not to retrieve tagIds ";
            emsg += "from a config file";

            throw CARMA_EXCEPTION(ErrorException,emsg);
        }
    }
    // We are not using the DBMS...
    else {
        CARMA_CPTRACE( Trace::TRACE2,
                       "constructor(): Not using a database, but " <<
                       "registering subsystem names and assigning tagIds " <<
                       "myself..." );

        if ( canUseConfFile ) {
            string msg;
            msg += "Using canonical name to tagId map in conf file ";
            msg += tagsConfFileName + ". ";
            msg += "It had better match that in the database or db ";
            msg += "integrity will be compromised.";
            programLogWarnIfPossible( msg );
            CARMA_CPTRACE( Trace::TRACE1, msg );

            masterOrigin_ = CONF_FILE_ORIGIN;
            getNamesToTagIDsFromConfFile( tagsConfFileName );

        }
        // Not using conf file
        else {
            masterOrigin_ = ON_THE_FLY_ORIGIN;

            programLogNoticeIfPossible(
                "Not using db to get tagIds, creating them on-the-fly" );
        }
    }

    {
        string s;

        {
            ostringstream oss;

            {
                const GuardReadLockType readLock( guard_ );

                Id2NameMap::const_iterator i = id2Name_.begin();
                const Id2NameMap::const_iterator iEnd = id2Name_.end();

                for ( ; i != iEnd; ++i )
                    oss << i->first << i->second;
            }

            s = oss.str();
        }

        sha1sum_ = StringUtils::computeMessageDigest( s , util::SHA1 );
    }

    CARMA_CPTRACE( Trace::TRACE2, "TagIDAuthority constructor finished" );
}


TagIDAuthority::~TagIDAuthority()
try {
    if ( haveUnloggedNotableOtfCreations_ ) {
        programLogErrorIfPossible(
            "TagIDAuthority notable on-the-fly creations were never logged" );
    }
} catch ( ... ) {
    // Just stifle any exception

    return;
}


string
TagIDAuthority::getSubsystemName( const string & canonicalName )
{
    const string::size_type p = canonicalName.find( '.' );

    return canonicalName.substr( 0, p );
}


string
TagIDAuthority::getSubsystemName( const unsigned subsysID )
{
    if ( subsysID == 0 ) {
        ostringstream msg;
        msg << "Subsystem IDs start at 1, so a value of 0 is not allowed";
        throw CARMA_EXCEPTION(IllegalArgumentException,msg.str());
    }

    if ( (subsysID < 1) ||
         (static_cast< int >( subsysID ) > subsysnamesCount_) ) {
        ostringstream msg;

        msg << "Function parameter value " << subsysID <<
            " is larger than or equal to the the number of subsystems, " <<
            "which is " << subsysnamesCount_;

        throw CARMA_EXCEPTION(IllegalArgumentException,msg.str());
    }

    return subsysnames_[ subsysID - 1 ];
}


int
TagIDAuthority::getSubsystemCount( )
{
    return subsysnamesCount_;
}


tagIDType
TagIDAuthority::convertNameToTagIdHoldingLock( const string & name ) const
{
    const Name2InfoMap::const_iterator i = name2Info_.find( name );

    if ( i == name2Info_.end() ) {
        ostringstream oss;
        oss << "Monitor subsystem name not found: " << name << ". Check that dbms/subsystemnames.cc contains the expected name, including any index number.";
        const string msg = oss.str();

        programLogWarnIfPossible( msg );
        CARMA_CPTRACE( Trace::TRACE1, msg );

        throw CARMA_EXCEPTION( NotFoundException, msg );
    }

    return i->second.tagId_;
}


tagIDType
TagIDAuthority::lookupID( const string & name ) const
{
    const GuardReadLockType readLock( guard_ );

    return convertNameToTagIdHoldingLock( name );
}


string
TagIDAuthority::lookupName( const tagIDType tagId ) const
{
    const GuardReadLockType readLock( guard_ );

    Id2NameMap::const_iterator i = id2Name_.find( tagId );

    if ( i == id2Name_.end() ) {
        const unsigned short subsysID = (tagId >> 16);
        const unsigned short pointID = (tagId & 0x0000FFFF);

        string subsysName;

        if ( (subsysID < 1) ||
             (static_cast< int >( subsysID ) > subsysnamesCount_) )
            subsysName = "< BAD >";
        else
            subsysName = subsysnames_[ subsysID - 1 ];

        ostringstream oss;

        oss << "Tag ID not found: " << tagId
            << " {" << subsysID << "(" << subsysName << "), "
            << pointID << "}";

        throw CARMA_EXCEPTION( NotFoundException, oss.str() );
    }

    return i->second;
}


tagIDType
TagIDAuthority::findIdOrAssignOtf(
    const string & name,
    bool &         assignedOTF,
    const string & subsysName )
{
    // Notice that I first check that it's not there already WHILE holding the
    // write lock so that I don't insert something that has just sneaked in
    // before the write lock was grabbed.
    const GuardWriteLockType writeLock( guard_ );

    {
        const Name2InfoMap::const_iterator i = name2Info_.find( name );

        if ( i != name2Info_.end() ) {
            assignedOTF = (i->second.origin_ == ON_THE_FLY_ORIGIN);

            return i->second.tagId_;
        }
    }

    // lookup up the subsystem ID
    const tagIDType subsysIdAsTagId =
        convertNameToTagIdHoldingLock( subsysName );

    if ( isSubsysTagID( subsysIdAsTagId ) == false  )
        throw runtime_error( "Bad subsys id lookup result" );

    const unsigned short subsysId = static_cast< unsigned short >( subsysIdAsTagId );

    Name2IdMap::iterator j = subsys2NextOtfId_.find( subsysName );

    if ( j == subsys2NextOtfId_.end() ) {
        // Initialise the next available monitor point id to be
        // used in on-the-fly tag id assignment

        subsys2NextOtfId_[ subsysName ] = maxPossiblePointID( subsysId );

        j = subsys2NextOtfId_.find( subsysName );

        if ( j == subsys2NextOtfId_.end() )
            throw runtime_error( "Unable to init the first on-the-fly id" );
        else {
            CARMA_CPTRACE( Trace::TRACE6,
                           "Initialised first on-the-fly point id" <<
                           " for subsystem \"" << subsysName << "\" (" <<
                           subsysId << ") to " << j->second << "." );
        }
    }

    const tagIDType tagId = composeTagID( subsysId, j->second );

    if ( masterOrigin_ != ON_THE_FLY_ORIGIN ) {
        string msg;

        {
            ostringstream oss;

            oss << "Creating tag id #" << tagId
                << " ( " << (tagId >> 16) << ", " << (tagId & 0xffff) << ") "
                << " ( " << subsysId << ", " << j->second << ") "
                << "for \"" << name << "\" on-the-fly";

            msg = oss.str();
        }

        if ( logNotableOtfCreations_ )
            programLogWarnIfPossible( msg );
        else
            haveUnloggedNotableOtfCreations_ = true;

        CARMA_CPTRACE( Trace::TRACE6, msg );
    }

    registerNameHoldingWriteLock( name,
                                  tagId,
                                  ON_THE_FLY_ORIGIN );

    --(j->second);

    assignedOTF = true;

    return tagId;
}


namespace {

typedef map< unsigned short, size_t > SubsysCountMap;

}  // namespace < anonymous >


TagIDAuthority::TagIdOriginStats
TagIDAuthority::collectTagIdOriginStatsHoldingLock( ) const
{
    TagIdOriginStats result;

    result.total = 0;
    result.totalDb = 0;
    result.totalConfFile = 0;
    result.totalOtf = 0;

    Name2InfoMap::const_iterator i = name2Info_.begin();
    const Name2InfoMap::const_iterator iEnd = name2Info_.end();

    for ( ; i != iEnd; ++i ) {
        const tagIDType tagId = i->second.tagId_;

        if ( isSubsysTagID( tagId ) )
            continue;  // subsystem ID entry

        const unsigned short subsysId = (tagId >> 16);

        result.total += 1;

        {
            const SubsysCountMap::iterator j =
                result.subsystemTotal.find( subsysId );

            if ( j == result.subsystemTotal.end() )
                result.subsystemTotal.insert( make_pair( subsysId, 1 ) );
            else
                j->second += 1;
        }

        SubsysCountMap * subsysOriginCountMap = 0;

        switch ( i->second.origin_ ) {
            case SUBSYS_TABLE_ORIGIN:
                // BULLSHIT_TWC - We should never get here
                break;

            case DB_ORIGIN:
                result.totalDb += 1;
                subsysOriginCountMap = &(result.subsystemDb);
                break;

            case CONF_FILE_ORIGIN:
                result.totalConfFile += 1;
                subsysOriginCountMap = &(result.subsystemConfFile);
                break;

            case ON_THE_FLY_ORIGIN:
                result.totalOtf += 1;
                subsysOriginCountMap = &(result.subsystemOtf);
                break;
        }

        if ( subsysOriginCountMap != 0 ) {
            const SubsysCountMap::iterator j =
                subsysOriginCountMap->find( subsysId );

            if ( j == subsysOriginCountMap->end() )
                subsysOriginCountMap->insert( make_pair( subsysId, 1 ) );
            else
                j->second += 1;
        }
    }

    return result;
}


TagIDAuthority::TagIdOriginStats
TagIDAuthority::collectTagIdOriginStats( ) const
{
    const GuardReadLockType readLock( guard_ );

    return collectTagIdOriginStatsHoldingLock();
}


void
TagIDAuthority::logTagIdOriginStats() const
{
    const GuardReadLockType readLock( guard_ );

    const TagIdOriginStats originStats = collectTagIdOriginStatsHoldingLock();

    if ( originStats.total > 0 ) {
        ostringstream oss;

        oss << "There have been " << originStats.total
            << " tag ids created thus far";

        programLogInfoIfPossible( oss.str() );
    }

    if ( originStats.totalDb > 0 ) {
        ostringstream oss;

        oss << "  There have been " << originStats.totalDb
            << " tag ids created from the database thus far";

        programLogInfoIfPossible( oss.str() );
    }

    if ( originStats.totalConfFile > 0 ) {
        ostringstream oss;

        oss << "  There have been " << originStats.totalConfFile
            << " tag ids created from the conf file thus far";

        programLogInfoIfPossible( oss.str() );
    }

    if ( originStats.totalOtf > 0 ) {
        ostringstream oss;

        oss << "  There have been " << originStats.totalOtf
            << " tag ids created on-the-fly thus far";

        programLogInfoIfPossible( oss.str() );
    }

    SubsysCountMap::const_iterator j =
        originStats.subsystemTotal.begin();

    const SubsysCountMap::const_iterator jEnd =
        originStats.subsystemTotal.end();

    for ( ; j != jEnd; ++j ) {
        const unsigned short subsysId = j->first;

        string subsysName;
        {
            const Id2NameMap::const_iterator k = id2Name_.find( subsysId );

            if ( k != id2Name_.end() )
                subsysName = k->second;
            else
                subsysName = "< unknown >";
        }

        ostringstream oss;

        oss << "    Subsystem #" << subsysId << " (" << subsysName << ") "
            << "has had " << j->second << " tag creations thus far( ";

        bool firstOne = true;

        {
            const SubsysCountMap::const_iterator k =
                originStats.subsystemDb.find( subsysId );

            if ( k != originStats.subsystemDb.end() ) {
                if ( firstOne )
                    firstOne = false;
                else
                    oss << ",";

                oss << k->second << " database";
            }
        }

        {
            const SubsysCountMap::const_iterator k =
                originStats.subsystemConfFile.find( subsysId );

            if ( k != originStats.subsystemConfFile.end() ) {
                if ( firstOne )
                    firstOne = false;
                else
                    oss << ",";

                oss << k->second << " conf file";
            }
        }

        {
            const SubsysCountMap::const_iterator k =
                originStats.subsystemOtf.find( subsysId );

            if ( k != originStats.subsystemOtf.end() ) {
                if ( firstOne )
                    firstOne = false;
                else
                    oss << ",";

                oss << k->second << " on-the-fly";
            }
        }

        oss << " )";

        programLogInfoIfPossible( oss.str() );
    }
}


void
TagIDAuthority::logNotableOnTheFlyCreationsThusFarAndInTheFuture( )
{
    logNotableOnTheFlyCreationsThusFarAndInTheFuture( 10 );
}


void
TagIDAuthority::logNotableOnTheFlyCreationsThusFarAndInTheFuture(
    const size_t targetLineCount )
{
    // Let's quickly use a read lock to see if we actually need to do anything
    {
        const GuardReadLockType readLock( guard_ );
    
        if ( (logNotableOtfCreations_ == true) &&
             (haveUnloggedNotableOtfCreations_ == false) )
            return;
    }
    
    // NOTE: At this point I need a write lock as opposed to a read lock
    //       because I muck with the values of logNotableOtfCreations_ and
    //       haveUnloggedNotableOtfCreations_
    const GuardWriteLockType writeLock( guard_ );

    logNotableOtfCreations_ = true;

    if ( haveUnloggedNotableOtfCreations_ == false )
        return;

    const TagIdOriginStats originStats = collectTagIdOriginStatsHoldingLock();

    if ( originStats.totalOtf > 0 ) {
        ostringstream oss;

        oss << "There have been " << originStats.totalOtf
            << " tag ids created on-the-fly thus far";

        programLogWarnIfPossible( oss.str() );
    }

    typedef set< unsigned short > SubsysIdSet;
    typedef multimap< size_t, unsigned short > OtfsToSubsysMultimap;

    SubsysIdSet detailPile;
    OtfsToSubsysMultimap summaryPile;
    size_t detailLines = 0;
    size_t summaryLines = 0;

    {
        SubsysCountMap::const_iterator i =
            originStats.subsystemOtf.begin();

        const SubsysCountMap::const_iterator iEnd =
            originStats.subsystemOtf.end();

        for ( ; i != iEnd; ++i ) {
            const size_t subsysId = i->first;
            const size_t subsysOtfs = i->second;

            if ( subsysOtfs <= 1 ) {
                detailPile.insert( subsysId );
                detailLines += subsysOtfs;
            } else {
                summaryPile.insert( make_pair( subsysOtfs, subsysId ) );
                summaryLines += 1;
            }
        }
    }


    if ( (detailLines + summaryLines) < targetLineCount ) {
        while ( summaryPile.empty() == false ) {
            const OtfsToSubsysMultimap::iterator i = summaryPile.begin();

            const size_t subsysOtfs = i->first;

            const size_t newDetailLines = detailLines + subsysOtfs;
            const size_t newSummaryLines = summaryLines - 1;

            if ( (newDetailLines + newSummaryLines) > targetLineCount )
                break;

            {
                ostringstream oss;

                oss << "Moving subsystem #" << i->second << " with "
                    << subsysOtfs << " otfs to the detail pile which gives "
                    << "a new total of " << (newDetailLines + newSummaryLines)
                    << " lines";

                CARMA_CPTRACE( Trace::TRACE7, oss.str() );
            }

            detailPile.insert( i->second );
            detailLines = newDetailLines;

            summaryPile.erase( i );
            summaryLines = newSummaryLines;
        }
    }

    {
        OtfsToSubsysMultimap::const_reverse_iterator i =
            summaryPile.rbegin();

        const OtfsToSubsysMultimap::const_reverse_iterator iEnd =
            summaryPile.rend();

        for ( ; i != iEnd; ++i ) {
            const size_t subsysOtfs = i->first;
            const unsigned short subsysId = i->second;

            string subsysName;
            {
                const Id2NameMap::const_iterator j = id2Name_.find( subsysId );

                if ( j != id2Name_.end() )
                    subsysName = j->second;
                else
                    subsysName = "< unknown >";
            }

            ostringstream oss;

            oss << "Subsystem #" << subsysId << " (" << subsysName << ") "
                << "has had " << subsysOtfs
                << " tag ids created on-the-fly thus far";

            programLogWarnIfPossible( oss.str() );
        }
    }

    {
        SubsysIdSet::const_iterator j = detailPile.begin();
        const SubsysIdSet::const_iterator jEnd = detailPile.end();

        for ( ; j != jEnd; ++j ) {
            const unsigned short jSubsysId = *j;

            Name2InfoMap::const_iterator i = name2Info_.begin();
            const Name2InfoMap::const_iterator iEnd = name2Info_.end();

            for ( ; i != iEnd; ++i ) {
                if ( i->second.origin_ != ON_THE_FLY_ORIGIN )
                    continue;

                const tagIDType tagId = i->second.tagId_;

                if ( isSubsysTagID( tagId ) )
                    continue;  // subsystem ID entry

                const unsigned short subsysId = (tagId >> 16);

                if ( subsysId != jSubsysId )
                    continue;

                string msg;

                msg += "Created tag id for \"";
                msg += i->first;
                msg += "\" on-the-fly";

                programLogWarnIfPossible( msg );
            }
        }
    }

    haveUnloggedNotableOtfCreations_ = false;
}


bool
TagIDAuthority::tagIdAssignedOnTheFly( const string & name ) const
{
    bool foundName = false;
    bool assignedOTF = true;

    // Search for monitor point name in current tag id. map
    {
        const GuardReadLockType readLock( guard_ );

        const Name2InfoMap::const_iterator i = name2Info_.find( name );

        if ( i != name2Info_.end() ) {
            foundName = true;
            assignedOTF = (i->second.origin_ == ON_THE_FLY_ORIGIN);
        }
    }

    if ( foundName == false )
        throw CARMA_ERROR( "name \"" + name + "\" was not found" );
        
    return assignedOTF;
}


bool
TagIDAuthority::tagIdAssignedOnTheFly( const tagIDType tagId ) const
{
    bool foundTagId = false;
    bool foundName = false;
    bool assignedOTF = true;

    // Search for monitor point name in current tag id. map
    {
        const GuardReadLockType readLock( guard_ );

        const Id2NameMap::const_iterator i = id2Name_.find( tagId );

        if ( i != id2Name_.end() ) {
            foundTagId = true;
            
            const Name2InfoMap::const_iterator j =
                name2Info_.find( i->second );

            if ( j != name2Info_.end() ) {
                foundName = true;
                assignedOTF = (j->second.origin_ == ON_THE_FLY_ORIGIN);
            }
        }
    }

    if ( foundTagId == false )
        throw CARMA_ERROR( "tag id was not found" );

    if ( foundName == false )
        throw CARMA_ERROR( "name assigned to tag id was not found" );
    
    return assignedOTF;
}

bool
TagIDAuthority::retrieveTagInfo( const string & canonicalName,
                                 NameMapInfo & nameMapInfo ) const
{
    const GuardReadLockType readLock( guard_ );

    const Name2InfoMap::const_iterator j = name2Info_.find( canonicalName );
    
    if ( j == name2Info_.end() ) {
        return false;
    } else {
        nameMapInfo = j->second;
        return true;
    }
}


void
TagIDAuthority::registerNameHoldingWriteLock( const string &   name,
                                              const tagIDType  tagId,
                                              const OriginType origin )
{
    const Id2NameMap::iterator iId2NameLowerBound =
        id2Name_.lower_bound( tagId );

    if ( (iId2NameLowerBound != id2Name_.end()) &&
         (iId2NameLowerBound->first == tagId) ) {
        const string existingName = iId2NameLowerBound->second;

        const Name2InfoMap::const_iterator iExistingInfo =
            name2Info_.find( existingName );

        string msg;

        {
            ostringstream oss;

            if ( (iExistingInfo == name2Info_.end()) ||
                 (iExistingInfo->second.tagId_ != tagId) ) {
                oss << "There is some disagreement between id2Name_ and "
                    << "name2Info_ over the tag id for \""
                    << existingName << "\".";
            } else {
                oss << "Attempting to assign tag id " << tagId
                    << " (";

                switch ( origin ) {
                    case SUBSYS_TABLE_ORIGIN: oss << "SUBSYS_TABLE_ORIGIN"; break;
                    case DB_ORIGIN:           oss << "DB_ORIGIN";           break;
                    case CONF_FILE_ORIGIN:    oss << "CONF_FILE_ORIGIN";    break;
                    case ON_THE_FLY_ORIGIN:   oss << "ON_THE_FLY_ORIGIN";   break;
                }

                oss << ") to \"" << name
                    << "\" but it is already is assigned to \""
                    << existingName << "\" (";

                switch ( iExistingInfo->second.origin_ ) {
                    case SUBSYS_TABLE_ORIGIN: oss << "SUBSYS_TABLE_ORIGIN"; break;
                    case DB_ORIGIN:           oss << "DB_ORIGIN";           break;
                    case CONF_FILE_ORIGIN:    oss << "CONF_FILE_ORIGIN";    break;
                    case ON_THE_FLY_ORIGIN:   oss << "ON_THE_FLY_ORIGIN";   break;
                }

                oss << ").";
            }

            msg = oss.str();
        }

        programLogWarnIfPossible( msg );
        CARMA_CPTRACE( Trace::TRACE1, msg );

        throw CARMA_EXCEPTION( IllegalArgumentException, msg );
    }

    const bool insertedOkay =
        name2Info_.insert(
            make_pair( name, NameMapInfo( tagId, origin ) ) ).second;

    if ( insertedOkay == false ) {
        const string msg = string( "Name already assigned tag id: " ) + name;

        programLogWarnIfPossible( msg );
        CARMA_CPTRACE( Trace::TRACE1, msg );

        throw CARMA_EXCEPTION( IllegalArgumentException, msg );
    }

    id2Name_.insert( iId2NameLowerBound, make_pair( tagId, name ) );
}


void
TagIDAuthority::registerSubsystemNamesHoldingWriteLock( )
{
    for ( int i = 0; i < subsysnamesCount_; ++i ) {
        const string subsysName = subsysnames_[ i ];
        const int subsysId = i + 1;

        CARMA_CPTRACE( Trace::TRACE6,
                       "TagIDAuthority registering subsystem \"" <<
                       subsysName <<
                       "\" with an on-the-fly subsystem id of " << subsysId );

        registerNameHoldingWriteLock( subsysName,
                                      subsysId,
                                      SUBSYS_TABLE_ORIGIN );
    }
}


TagIDAuthority::SubsysMatchType
TagIDAuthority::checkSubsysMatchHoldingLock(
    const string &                 canonicalName,
    const tagIDType                nonSubsysTagId,
    Name2InfoMap::const_iterator & hint ) const
{
    const string subsysName = getSubsystemName( canonicalName );
    
    Name2InfoMap::const_iterator i;
    
    if ( (hint != name2Info_.end()) && (hint->first == subsysName) )
        i = hint;
    else {
        // programLogInfoIfPossible( "hint miss" );
        
        i = name2Info_.find( subsysName );

        if ( i == name2Info_.end() )
            return SUBSYS_DOES_NOT_EXIST;

        hint = i;
    }

    const tagIDType subsysIdFromNameAsTagId = i->second.tagId_;

    if ( isSubsysTagID( subsysIdFromNameAsTagId ) == false )
        throw runtime_error( "Bad subsys id lookup result" );

    const unsigned short subsysIdFromName =
        static_cast< unsigned short >( subsysIdFromNameAsTagId );

    const unsigned short subsysIdFromTagId = (nonSubsysTagId >> 16);

    if ( subsysIdFromTagId != subsysIdFromName )
        return SUBSYS_ID_MISMATCH;

    return SUBSYS_MATCHES;
}


/**
 * Setup the TagIDAuthority using a configuration file (as opposed to
 * using the DBMS).
 *
 * This version prefers to use the binary file format, due to the massive
 * increase in performance this provides. However, a fallback to the original
 * text file format is supported.
 */
void TagIDAuthority::getNamesToTagIDsFromConfFile( const string & fileName )
{
    if (FileUtils::exists(fileName + ".bin")) {
        getNamesToTagIDsFromBinaryConfFile(fileName + ".bin");
    } else if (FileUtils::exists(fileName)) {
        getNamesToTagIDsFromTextConfFile(fileName);
    } else {
        std::ostringstream oss;
        oss << "Unable to locate binary/text configuration file: " << fileName;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }
}

void
TagIDAuthority::getNamesToTagIDsFromBinaryConfFile( const string & fileName )
{
    const GuardWriteLockType writeLock( guard_ );

    Name2InfoMap::const_iterator hint = name2Info_.begin();
    dbmsproto::NameTagidMap ntm;

    {
        std::fstream input(fileName.c_str(), std::ios::in | std::ios::binary);
        if (!ntm.ParseFromIstream(&input)) {
            std::ostringstream oss;
            oss << "Failed to parse binary name-to-tagid file: " << fileName;
            programLogErrorIfPossible(oss.str());
            throw CARMA_ERROR(oss.str());
        }
    }

    // double check that the size of both lists are exactly equal
    if (ntm.id_size() != ntm.name_size()) {
        std::ostringstream oss;
        oss << "Binary name-to-tagid file has different lengths of name and id fields: " << fileName;
        programLogErrorIfPossible(oss.str());
        throw CARMA_ERROR(oss.str());
    }

    for (int i = 0; i < ntm.id_size(); i++) {
        const tagIDType tagId = static_cast<tagIDType>(ntm.id(i));
        const std::string &name = ntm.name(i);

        if (isSubsysTagID(tagId))
            continue;

        if (checkSubsysMatchHoldingLock(name, tagId, hint) != SUBSYS_MATCHES)
            continue;

        registerNameHoldingWriteLock(name, tagId, CONF_FILE_ORIGIN);
    }
}

void
TagIDAuthority::getNamesToTagIDsFromTextConfFile( const string & fileName )
{
    // print warning message when using legacy format
    programLogWarnIfPossible("Using fallback to legacy text file format");
    programLogWarnIfPossible("This will make monitor system creation slower");

    const GuardWriteLockType writeLock( guard_ );

    const map< string, string > tmpMap = KeyValueConfigFile::load( fileName );

    map< string, string >::const_iterator i = tmpMap.begin();
    const map< string, string >::const_iterator iEnd = tmpMap.end();

    Name2InfoMap::const_iterator hint = name2Info_.begin();

    for ( ; i != iEnd; ++i ) {
        const tagIDType tagId =
            static_cast< tagIDType >( StringUtils::stringToInt( i->second ) );

        if ( isSubsysTagID( tagId ) )
            continue;

        if ( checkSubsysMatchHoldingLock( i->first,
                                          tagId,
                                          hint ) != SUBSYS_MATCHES )
            continue;

        registerNameHoldingWriteLock( i->first,
                                      tagId,
                                      CONF_FILE_ORIGIN );
    }
}


void
TagIDAuthority::registerNameIDsUsingDB( const DBConfigurator * const dbconf )
{
    const GuardWriteLockType writeLock( guard_ );

    auto_ptr< DBConnection > dbc;

    try {
        auto_ptr< DBConnection >
            tmp( DBConnectionFactory::createConnection( dbconf ) );

        dbc = tmp;

        programLogInfoIfPossible( "Database connection successfully opened" );
    } catch ( const DBConnectionException & exc ) {
        ostringstream oss;

        oss << "Unable to open a connection to the database. " << exc.what();

        programLogCriticalIfPossible( oss.str() );

        throw CARMA_EXCEPTION( DBConnectionException, oss.str() );
    }

    try {
        dbms::MonitorConfigurationDatabase mcdb( dbc.get() );

        try {
            // Register up all the tagId mappings from the DBMS
            const map< int, string > dbmsMap = mcdb.getTagID2NameMap();

            programLogInfoIfPossible(
                "Registering canon name -> tagID mappings from DBMS table" );

            map< int, string >::const_iterator i = dbmsMap.begin();
            const map< int, string >::const_iterator iEnd = dbmsMap.end();

            Name2InfoMap::const_iterator hint = name2Info_.begin();
            
            for ( ; i != iEnd; ++i ) {
                const tagIDType tagId = i->first;

                if ( isSubsysTagID( tagId ) )
                    continue;

                if ( checkSubsysMatchHoldingLock( i->second,
                                                  tagId,
                                                  hint ) != SUBSYS_MATCHES )
                    continue;

                registerNameHoldingWriteLock( i->second,
                                              tagId,
                                              DB_ORIGIN );
            }
        } catch ( const DBConnectionException & exc ) {
            ostringstream oss;

            oss << "Unable to read "
                << getTableName( STATIC_MONITOR_CONFIG_TABLE )
                << " db table. " << exc.what();

            programLogCriticalIfPossible( oss.str() );

            throw CARMA_EXCEPTION( DBConnectionException, oss.str() );
        }
    } catch ( const DBConnectionException & exc ) {
        ostringstream oss;

        oss << "Unable to read Subsystems db table. " << exc.what();

        programLogWarnIfPossible( oss.str() );

        throw CARMA_EXCEPTION( DBConnectionException, oss.str() );
    }

    programLogNoticeIfPossible( "Using db to get tagIds" );
}


string TagIDAuthority::tagIDNameMapSha1Sum() const
{
    return sha1sum_;
}


namespace {


typedef ScopedLock< ::pthread_mutex_t > GlobalsGuardLockType;

::pthread_mutex_t gGlobalsGuard = PTHREAD_MUTEX_INITIALIZER;

TagIDAuthority * gAuthority = 0;

bool gConfigurationGlobalsInitialised = false;
bool gCanUseDb = false;
bool gCanUseConfFile = false;
string gDbConfFile;
string gCanonicalNamesToTagIDsConfFile;


}  // namespace < anonymous >


void
TagIDAuthority::configureAuthority(
    const bool     useDb,
    const string & dbConfFile,
    const bool     useConfFileIfDbIsDown,
    const string & canonicalNamesToTagIDsConfFile )
{
    const GlobalsGuardLockType lock( gGlobalsGuard );

    if ( gAuthority != 0 ) {
        const string msg =
            "Singleton instance already exists. The specified "
            "configuration will have no effect until this "
            "instance is destroyed and a new one is created";

        programLogWarnIfPossible( msg );
        CARMA_CPTRACE( Trace::TRACE1, msg );
    }

    gCanUseDb = useDb;
    gDbConfFile = dbConfFile;
    gCanUseConfFile = useConfFileIfDbIsDown;

    if ( useDb && useConfFileIfDbIsDown )
        gCanonicalNamesToTagIDsConfFile = canonicalNamesToTagIDsConfFile;

    gConfigurationGlobalsInitialised = true;
}


void
TagIDAuthority::closeAuthority( )
{
    const GlobalsGuardLockType lock( gGlobalsGuard );

    TagIDAuthority * deadAuthorityWalking = 0;

    ::std::swap( deadAuthorityWalking, gAuthority );

    delete deadAuthorityWalking;

    EVP_cleanup();
}


TagIDAuthority &
TagIDAuthority::getAuthority( )
{
    if ( gAuthority == 0 ) {
        const GlobalsGuardLockType lock( gGlobalsGuard );

        if ( gAuthority == 0 ) {
            if ( gConfigurationGlobalsInitialised == false )  {
                const bool useDb = Program::getUseDBMS();

                gCanUseDb = useDb;
                gCanUseConfFile = true; // Always try to use conf file

                gConfigurationGlobalsInitialised = true;
            }

            gAuthority = new TagIDAuthority( gCanUseDb,
                                             gCanUseConfFile,
                                             gDbConfFile,
                                             gCanonicalNamesToTagIDsConfFile);
        }
    }

    return *gAuthority;
}
