/**
 *
 * @author: Tom Costa
 *
 * $Id: grepMonitorSystem.cc,v 1.41 2013/08/27 21:44:27 eml Exp $
 *
 * $CarmaCopyright$
 *
 */

//
// @version $Revision: 1.41 $
//
// @description
//    This program searches the monitor system for keywords, descriptive
//    text
//    For example, to find monitor points with the word 'refrac' use
//      grepMonitorSystem words=refrac brief=t
//    To find actual values of the monitor points, use dumpMonitor
//
//
// @usage search the monitor system for text
//
// @key words "" string
//               Whitespace and/or comma separated list of words to require
//               for a match.
//               Empty means no requirement.
//
// @key phrase "" string
//                Exact phrase to require (including any whitespace)
//                for a match.
//                Empty means no requirement.
//
// @key phrase2 "" string
//                 Second exact phrase to require (including any whitespace)
//                 for a match.
//                 Empty means no second requirement.
//
// @key phrase3 "" string
//                 Third exact phrase to require (including any whitespace)
//                 for a match.
//                 Empty means no third requirement.
//
// @key phrase4 "" string
//                 Fourth exact phrase to require (including any whitespace)
//                 for a match.
//                 Empty means no fourth requirement.
//
// @key caseSensitive false bool
//                    Whether or not matches are determined case sensitive.
//
// @key brief true bool
//            Whether or not to only output the canonical names for matches.
//
// @key showSearchTexts false bool
//                      Whether or not to print out the list of search texts
//                      that were requested. Mostly a diagnostic for debugging
//                      this program itself.
//
// @key strict false bool
//            Require strict matching when comparing monitor point names (ie, match ONLY names, not descriptions or units, etc)
//
// @key tally true bool
//            Whether or not to output a final tally of the number of matching
//            monitor points.
//
// @key persistent @noDefault bool
//                 If given then only monitor points with the given value for
//                 their persistent attribute are matched.
//
// @key otfTagId @noDefault bool
//               If given then only monitor points whose tag ids were,
//               or were not depending on the value, assigned on-the-fly are
//               matched.
//
// @key samps @noDefault int
//            If given and positive then only monitor points with the given
//            samples per frame are matched.
//            If given and negative then only monitor points with samples per
//            frame greater than the negation are matched (i.e. -1 matches
//            monitor points with greater than 1 sample per frame).
//
// @key type @noDefault string
//            If given then only monitor points with the given value type are
//            matched unless the value starts with a dash (i.e. '-') and then
//            only monitor points not of the given value type are matched.
//            Possible values are byte, short, int, bool, float, double,
//            complex, string, or serial.
//
// @key archive @noDefault string
//              If given then only monitor points with the given archive
//              priority are matched unless the value starts with a
//              dash (i.e. '-') and then only monitor points not of the given
//              archive priority are matched.
//              Possible values are vital, useful, normal, debug, verbose,
//              or dont.
//
// @key tagId @noDefault int
//            If given then only the monitor point with the given tag id is
//            matched.
//
// @key name @noDefault string
//  If given then only the monitor point with the given name is matched
//  (case insensitive).
//
// @logger MONITOR_FACILITY carma.monitor.grepMonitorSystem
//

#include <iostream>
#include <string>
#include <vector>

#include <carma/dbms/TagIDAuthority.h>
#include <carma/monitor/MonitorPoint.h>
#include <carma/monitor/monitorPointSpecializations.h>
#include <carma/monitor/MonitorPointIterator.h>
#include <carma/monitor/MonitorSystem.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
#include <carma/util/Program.h>
#include <carma/util/programLogging.h>
#include <carma/util/ScopedLogNdc.h>
#include <carma/util/StringUtils.h>

#include <boost/algorithm/string/join.hpp>
#include <boost/foreach.hpp>

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {


typedef enum {
    PERSISTENT_CRITERIA_NONE,
    PERSISTENT_CRITERIA_TRUE,
    PERSISTENT_CRITERIA_FALSE
} PersistentCriteria;


typedef enum {
    OTF_TAG_ID_CRITERIA_NONE,
    OTF_TAG_ID_CRITERIA_TRUE,
    OTF_TAG_ID_CRITERIA_FALSE
} OtfTagIdCriteria;


class SampsCriteria {
    public:
        typedef enum {
            NONE,
            MATCH,
            GREATER,
            LESS
        } Comparison;

        explicit SampsCriteria( );

        SampsCriteria( Comparison comp,
                       int        value );

        bool matches( const MonitorPoint & mp ) const;

    private:
        Comparison comp_;
        int        value_;
};


SampsCriteria::SampsCriteria( ) :
comp_( NONE ),
value_( 0 )
{
}


SampsCriteria::SampsCriteria( const Comparison comp,
                              const int        value ) :
comp_( comp ),
value_( value )
{
}


bool
SampsCriteria::matches( const MonitorPoint & mp ) const
{
    switch ( comp_ ) {
        case NONE:
            return true;

        case MATCH:
            if ( mp.getNumSamples() == value_ )
                return true;
            break;

        case GREATER:
            if ( mp.getNumSamples() > value_ )
                return true;
            break;

        case LESS:
            if ( mp.getNumSamples() < value_ )
                return true;
            break;
    }

    return false;
}


class TagIdCriteria {
    public:
        typedef enum {
            NONE,
            MATCH,
            NOT_MATCH
        } Comparison;

        explicit TagIdCriteria( );

        TagIdCriteria( Comparison comp,
                       tagIDType  value );

        bool matches( const MonitorPoint & mp ) const;

    private:
        Comparison comp_;
        tagIDType  value_;
};


TagIdCriteria::TagIdCriteria( ) :
comp_( NONE ),
value_( 0 )
{
}


TagIdCriteria::TagIdCriteria( const Comparison comp,
                              const tagIDType  value ) :
comp_( comp ),
value_( value )
{
}


bool
TagIdCriteria::matches( const MonitorPoint & mp ) const
{
    switch ( comp_ ) {
        case NONE:
            return true;

        case MATCH:
            if ( mp.getTagID() == value_ )
                return true;
            break;

        case NOT_MATCH:
            if ( mp.getTagID() != value_ )
                return true;
            break;
    }

    return false;
}


class MvtCriteria {
    public:
        typedef enum {
            NONE,
            MATCH,
            NOT_MATCH
        } Comparison;

        explicit MvtCriteria( );

        MvtCriteria( Comparison       comp,
                     MonitorValueType value );

        bool matches( const MonitorPoint & mp ) const;

    private:
        Comparison       comp_;
        MonitorValueType value_;
};


MvtCriteria::MvtCriteria( ) :
comp_( NONE ),
value_( MONITOR_VALUE_TYPE_INTEGER )
{
}


MvtCriteria::MvtCriteria( const Comparison       comp,
                          const MonitorValueType value ) :
comp_( comp ),
value_( value )
{
}


bool
MvtCriteria::matches( const MonitorPoint & mp ) const
{
    switch ( comp_ ) {
        case NONE:
            return true;

        case MATCH:
            if ( mp.getValuetype() == value_ )
                return true;
            break;

        case NOT_MATCH:
            if ( mp.getValuetype() != value_ )
                return true;
            break;
    }

    return false;
}


class McapCriteria {
    public:
        typedef enum {
            NONE,
            MATCH,
            NOT_MATCH
        } Comparison;

        explicit McapCriteria( );

        McapCriteria( Comparison                         comp,
                      MonitorComponent::ARCHIVE_PRIORITY value );

        bool matches( const MonitorPoint & mp ) const;

    private:
        Comparison                         comp_;
        MonitorComponent::ARCHIVE_PRIORITY value_;
};


McapCriteria::McapCriteria( ) :
comp_( NONE ),
value_( MonitorComponent::DEFAULT )
{
}


McapCriteria::McapCriteria(
    const Comparison                         comp,
    const MonitorComponent::ARCHIVE_PRIORITY value ) :
comp_( comp ),
value_( value )
{
}


bool
McapCriteria::matches( const MonitorPoint & mp ) const
{
    switch ( comp_ ) {
        case NONE:
            return true;

        case MATCH:
            if ( mp.getArchivePriority() == value_ )
                return true;
            break;

        case NOT_MATCH:
            if ( mp.getArchivePriority() != value_ )
                return true;
            break;
    }

    return false;
}


struct MatchCriteria {
    explicit MatchCriteria( );

  bool matches( const MonitorPoint & mp, bool strict = false ) const;

    TagIdCriteria      tagIdCriteria;
    MvtCriteria        mvtCriteria;
    McapCriteria       mcapCriteria;
    SampsCriteria      sampsCriteria;
    PersistentCriteria persistent;
    OtfTagIdCriteria   otfTagId;
    vector< string >   searchTexts;
    bool               caseSensitive;
};


MatchCriteria::MatchCriteria( ) :
tagIdCriteria(),
mvtCriteria(),
mcapCriteria(),
sampsCriteria(),
persistent( PERSISTENT_CRITERIA_NONE ),
otfTagId( OTF_TAG_ID_CRITERIA_NONE ),
searchTexts(),
caseSensitive( false )
{
}


bool
stringMatches( const string & text,
               const string & searchText,
               const bool     caseSensitive ) {
    if ( searchText.empty() )
        return true;

    if ( caseSensitive ) {
        if ( text.find( searchText ) != string::npos )
            return true;
        else
            return false;
    } else {
        const string ucText =
            StringUtils::lowASCIIAlphaNumericToUpper( text );

        const string ucSearchText =
            StringUtils::lowASCIIAlphaNumericToUpper( searchText );

        if ( ucText.find( ucSearchText ) != string::npos )
            return true;
        else
            return false;
    }
}


vector< string >
getStringsForEnumValues( const MonitorPointEnum & mpEnum, const bool withValues = false )
{
    const int numValues = mpEnum.getNumEnumerations();
    std::vector<std::string> result;

    result.reserve(numValues);
    for (int i = 0; i < numValues; i++) {
        const int value = mpEnum.isBitmask() ? (1 << i) : i;
        std::ostringstream oss;
        oss << mpEnum.getRawStringForEnumValue(value);
        if (withValues)
            oss << "=" << value;

        result.push_back(oss.str());
    }

    return result;
}


bool
mpMatches( const MonitorPoint & mp,
           const string &       searchText,
           const bool           caseSensitive,
	   const bool           strict)
{
    if ( searchText.empty() )
        return true;

    if ( stringMatches( mp.getCanonicalName(), searchText, caseSensitive ) )
        return true;

    // EML: if strict matching was required, don't check anything but the canonical name

    if(strict)
      return false;

    if ( stringMatches( mp.getName(), searchText, caseSensitive ) )
        return true;

    if ( stringMatches( mp.getShortName(), searchText, caseSensitive ) )
        return true;

    if ( stringMatches( mp.getLongName(), searchText, caseSensitive ) )
        return true;

    if ( stringMatches( mp.getDescription(), searchText, caseSensitive ) )
        return true;

    if ( stringMatches( mp.getUnits(), searchText, caseSensitive ) )
        return true;

    const MonitorPointEnum * const mpEnum =
        dynamic_cast< const MonitorPointEnum * >( &mp );

    if ( mpEnum != 0 ) {
        const vector< string > valueStrings =
            getStringsForEnumValues( *mpEnum );

        vector< string >::const_iterator i = valueStrings.begin();
        const vector< string >::const_iterator iEnd = valueStrings.end();

        for ( ; i != iEnd; ++i ) {
            if ( stringMatches( *i, searchText, caseSensitive ) )
                return true;
        }
    }

    return false;
}


bool
MatchCriteria::matches( const MonitorPoint & mp, bool strict ) const
{
    if ( tagIdCriteria.matches( mp ) != true )
        return false;

    if ( mvtCriteria.matches( mp ) != true )
        return false;

    if ( mcapCriteria.matches( mp ) != true )
        return false;

    if ( sampsCriteria.matches( mp ) != true )
        return false;

    switch ( persistent ) {
        case PERSISTENT_CRITERIA_NONE:
            break;

        case PERSISTENT_CRITERIA_TRUE:
            if ( mp.isPersistent() != true )
                return false;
            break;

        case PERSISTENT_CRITERIA_FALSE:
            if ( mp.isPersistent() != false )
                return false;
            break;
    }

    {
        vector< string >::const_iterator i =
            searchTexts.begin();

        const vector< string >::const_iterator iEnd =
            searchTexts.end();

        for ( ; i != iEnd; ++i ) {
            if ( i->empty() )
                continue;

            if ( mpMatches( mp,
                            *i,
                            caseSensitive,
			    strict ) != true )
                return false;
        }
    }

    switch ( otfTagId ) {
        case OTF_TAG_ID_CRITERIA_NONE:
            break;

        case OTF_TAG_ID_CRITERIA_TRUE:
            if ( dbms::TagIDAuthority::getAuthority().
                    tagIdAssignedOnTheFly( mp.getCanonicalName() ) != true )
                return false;
            break;

        case OTF_TAG_ID_CRITERIA_FALSE:
            if ( dbms::TagIDAuthority::getAuthority().
                    tagIdAssignedOnTheFly( mp.getCanonicalName() ) != false )
            break;
    }

    return true;
}


string
getStringForMonitorValueType( const MonitorValueType mvt )
{
    switch ( mvt ) {
        case MONITOR_VALUE_TYPE_BYTE:           return "BYTE";
        case MONITOR_VALUE_TYPE_SHORT:          return "SHORT";
        case MONITOR_VALUE_TYPE_INTEGER:        return "INTEGER";
        case MONITOR_VALUE_TYPE_BOOLEAN:        return "BOOLEAN";
        case MONITOR_VALUE_TYPE_FLOAT:          return "FLOAT";
        case MONITOR_VALUE_TYPE_DOUBLE:         return "DOUBLE";
        case MONITOR_VALUE_TYPE_COMPLEX:        return "COMPLEX";
        case MONITOR_VALUE_TYPE_STRING:         return "STRING";
        case MONITOR_VALUE_TYPE_SERIAL_NUMBER:  return "SERIAL_NUMBER";
    }

    {
        ostringstream oss;

        oss << "< unknown value " << mvt << " >";

        return oss.str();
    }
}


MonitorValueType
getMonitorValueTypeForString( const string & s )
{
    const string ucS = StringUtils::lowASCIIAlphaNumericToUpper( s );

    if ( ucS == "BYTE" )
        return MONITOR_VALUE_TYPE_BYTE;

    if ( ucS == "SHORT" )
        return MONITOR_VALUE_TYPE_SHORT;

    if ( ucS == "INTEGER" )
        return MONITOR_VALUE_TYPE_INTEGER;

    if ( ucS == "INT" )
        return MONITOR_VALUE_TYPE_INTEGER;

    if ( ucS == "BOOLEAN" )
        return MONITOR_VALUE_TYPE_BOOLEAN;

    if ( ucS == "BOOL" )
        return MONITOR_VALUE_TYPE_BOOLEAN;

    if ( ucS == "FLOAT" )
        return MONITOR_VALUE_TYPE_FLOAT;

    if ( ucS == "DOUBLE" )
        return MONITOR_VALUE_TYPE_DOUBLE;

    if ( ucS == "COMPLEX" )
        return MONITOR_VALUE_TYPE_COMPLEX;

    if ( ucS == "STRING" )
        return MONITOR_VALUE_TYPE_STRING;

    if ( ucS == "SERIAL_NUMBER" )
        return MONITOR_VALUE_TYPE_SERIAL_NUMBER;

    if ( ucS == "SERIAL" )
        return MONITOR_VALUE_TYPE_SERIAL_NUMBER;

    throw CARMA_ERROR(
        "Unrecognized monitor value type \"" + s + "\" is not one of "
        "byte, short, int, bool, float, double, complex, string, or serial" );
}


string
getStringForEnumValueType( const MonitorPointEnum & mpEnum )
{
    ostringstream oss;

    oss << getStringForMonitorValueType( mpEnum.getValuetype() ) << " ENUM";
    if (mpEnum.isBitmask())
        oss << " BITMASK";

    oss << " {";

    {
        const vector< string > valueStrings = getStringsForEnumValues( mpEnum, true );
        oss << boost::algorithm::join(valueStrings, ", ");
    }

    oss << "}";

    return oss.str();
}


string
getStringForMpValueType( const MonitorPoint & mp )
{
    const MonitorPointEnum * const mpEnum =
        dynamic_cast< const MonitorPointEnum * >( &mp );

    if ( mpEnum != 0 )
        return getStringForEnumValueType( *mpEnum );
    else
        return getStringForMonitorValueType( mp.getValuetype() );
}


string
getStringForMpArchivePriority( const MonitorPoint & mp )
{
    const MonitorComponent::ARCHIVE_PRIORITY mcap =
        mp.getArchivePriority();

    switch ( mcap ) {
        case MonitorComponent::VITAL:        return "VITAL";
        case MonitorComponent::USEFUL:       return "USEFUL";
        case MonitorComponent::NORMAL:       return "NORMAL";
        case MonitorComponent::DEBUG:        return "DEBUG";
        case MonitorComponent::VERBOSE:      return "VERBOSE";
        case MonitorComponent::DEFAULT:      return "DEFAULT";

        case MonitorComponent::DONTARCHIVE:
            {
                string result = "DONTARCHIVE";

                if ( dbms::TagIDAuthority::getAuthority().
                        tagIdAssignedOnTheFly( mp.getCanonicalName() ) ) {
                    result += " (Probably because this MP had it's tagID ";
                    result += "assigned on-the-fly)";
                }

                return result;
            }
    }

    {
        ostringstream oss;

        oss << "< unknown value " << mcap << " >";

        return oss.str();
    }
}


MonitorComponent::ARCHIVE_PRIORITY
getArchivePriorityForString( const string & s )
{
    const string ucS = StringUtils::lowASCIIAlphaNumericToUpper( s );

    if ( ucS == "VITAL" )
        return MonitorComponent::MonitorComponent::VITAL;

    if ( ucS == "USEFUL" )
        return MonitorComponent::USEFUL;

    if ( ucS == "NORMAL" )
        return MonitorComponent::NORMAL;

    if ( ucS == "DEBUG" )
        return MonitorComponent::DEBUG;

    if ( ucS == "VERBOSE" )
        return MonitorComponent::VERBOSE;

    if ( ucS == "DEFAULT" )
        return MonitorComponent::DEFAULT;

    if ( ucS == "DONTARCHIVE" )
        return MonitorComponent::DONTARCHIVE;

    if ( ucS == "DONT" )
        return MonitorComponent::DONTARCHIVE;

    if ( ucS == "DON'T" )
        return MonitorComponent::DONTARCHIVE;

    throw CARMA_ERROR(
        "Unrecognized archive priority \"" + s + "\" is not one of "
        "vital, useful, normal, debug, verbose or dont" );
}


string
getStringForUnits( const string & unitsVal )
{
    if ( unitsVal.empty() )
        return string();

    if ( unitsVal.find_first_not_of( " \t\n\r" ) != 0 )
        return "\"" + unitsVal + "\"";

    if ( unitsVal.find_last_not_of( " \t\n\r" ) != (unitsVal.size() - 1) )
        return "\"" + unitsVal + "\"";

    return unitsVal;
}


string
getStringForTagId( const tagIDType tagId )
{
    ostringstream oss;

    oss << tagId
        << " (" << hex << setfill( '0' )
        << setw( 4 ) << (tagId >> 16) << ":"
        << setw( 4 ) << (tagId & 0x0000ffff) << ")";

    return oss.str();
}


string
getStringForMonitorValue( const MonitorValueType mvt,
                          const MonitorValue &   mv )
{
    ostringstream oss;

    switch ( mvt ) {
        case MONITOR_VALUE_TYPE_BYTE:
            oss << static_cast< int >( mv.byte );
            break;

        case MONITOR_VALUE_TYPE_SHORT:
            oss << mv.sh;
            break;

        case MONITOR_VALUE_TYPE_INTEGER:
            oss << mv.lo;
            break;

        case MONITOR_VALUE_TYPE_BOOLEAN:
            oss << (mv.bo ? "true" : "false");
            break;

        case MONITOR_VALUE_TYPE_FLOAT:
            oss << mv.fl;
            break;

        case MONITOR_VALUE_TYPE_DOUBLE:
            oss << mv.db;
            break;

        case MONITOR_VALUE_TYPE_COMPLEX:
            if ( mv.complex[0] != 0.0 ) {
                oss << mv.complex[0];

                if ( mv.complex[1] < 0.0 )
                    oss << " - " << (-(mv.complex[1])) << "i";
                else if ( mv.complex[1] > 0.0 )
                    oss << " + " << mv.complex[1] << "i";
            } else if ( mv.complex[1] != 0.0 )
                oss << mv.complex[1] << "i";
            else
                oss << "0.0";
            break;

        case MONITOR_VALUE_TYPE_STRING:
            break;

        case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
            oss << mv.sn;
            break;
    }

    return oss.str();
}


string
getStringForEnumThreshold( const MonitorPointEnum & mpEnum,
                           const MonitorValue &     enumThreshold )
{
    const int numValues = mpEnum.getNumEnumerations();
    std::vector<std::string> vec;

    for (int i = 0; i < numValues; i++) {
        const int value = mpEnum.isBitmask() ? (1 << i) : i;

        // skip enumerations which do not have a threshold set
        if ((enumThreshold.lo & (1 << i)) == 0)
            continue;

        std::ostringstream oss;
        oss << mpEnum.getRawStringForEnumValue(value) << "=" << value;
        vec.push_back(oss.str());
    }

    return "{" + boost::algorithm::join(vec, ", ") + "}";
}


string
getStringForEnumThresholds( const MonitorPointEnum & mpEnum )
{
    string result;

    if ( mpEnum.errorHighDefaultIsSet() ) {
        result += "    error high:       ";
        result += getStringForEnumThreshold( mpEnum,
                                             mpEnum.getErrorHighDefault() );
        result += "\n";
    }

    if ( mpEnum.warnHighDefaultIsSet() ) {
        result += "    warn high:        ";
        result += getStringForEnumThreshold( mpEnum,
                                             mpEnum.getWarnHighDefault() );
        result += "\n";
    }

    if ( mpEnum.warnLowDefaultIsSet() ) {
        result += "    warn low:         ";
        result += getStringForEnumThreshold( mpEnum,
                                             mpEnum.getWarnLowDefault() );
        result += "\n";
    }

    if ( mpEnum.errorLowDefaultIsSet() ) {
        result += "    error low:        ";
        result += getStringForEnumThreshold( mpEnum,
                                             mpEnum.getErrorLowDefault() );
        result += "\n";
    }

    if ( result.empty() )
        return string();

    return ("  default thresholds\n" + result);
}


string
getStringForThresholds( const MonitorPoint & mp )
{
    {
        const MonitorPointEnum * const mpEnum =
            dynamic_cast< const MonitorPointEnum * >( &mp );

        if ( mpEnum != 0 )
            return getStringForEnumThresholds( *mpEnum );
    }

    const MonitorValueType mvt = mp.getValuetype();

    if ( mvt == MONITOR_VALUE_TYPE_STRING )
        return string();

    string result;

    if ( mp.errorHighDefaultIsSet() ) {
        result += "    error high:       >= ";
        result += getStringForMonitorValue( mvt, mp.getErrorHighDefault() );
        result += "\n";
    }

    if ( mp.warnHighDefaultIsSet() ) {
        result += "    warn high:        >= ";
        result += getStringForMonitorValue( mvt, mp.getWarnHighDefault() );
        result += "\n";
    }

    if ( mp.warnLowDefaultIsSet() ) {
        result += "    warn low:         <= ";
        result += getStringForMonitorValue( mvt, mp.getWarnLowDefault() );
        result += "\n";
    }

    if ( mp.errorLowDefaultIsSet() ) {
        result += "    error low:        <= ";
        result += getStringForMonitorValue( mvt, mp.getErrorLowDefault() );
        result += "\n";
    }

    if ( result.empty() )
        return string();

    return ("  default thresholds\n" + result);
}


void
outputMpMatch( ostream &            os,
               const MonitorPoint & mp,
               const bool           brief )
{
    os << mp.getCanonicalName() << "\n";

    if ( brief != true ) {
        os << "  tag id:             " << getStringForTagId( mp.getTagID() ) << "\n";
        os << "  name:               " << mp.getName() << "\n";
        os << "  short name:         " << mp.getShortName() << "\n";
        os << "  long name:          " << mp.getLongName() << "\n";
        os << "  value type:         " << getStringForMpValueType( mp ) << "\n";
        os << "  units:              "
           << getStringForUnits( mp.getUnits() ) << "\n";
        os << "  samples per frame:  " << mp.getNumSamples() << "\n";
        os << "  persistent:         "
           << (mp.isPersistent() ? "yes" : "no") << "\n";
        os << "  archive priority:   "
           << getStringForMpArchivePriority( mp ) << "\n";

        os << getStringForThresholds( mp );

        os << "  description:        " << mp.getDescription() << "\n";
    }
}


}  // namespace < anonymous >


int
Program::main( )
try {
    const bool strict = getBoolParameter( "strict" );
    const bool brief = getBoolParameter( "brief" );
    const bool showSearchTexts = getBoolParameter( "showSearchTexts" );
    const bool showTally = getBoolParameter( "tally" );

    MatchCriteria matchCriteria;

    if ( parameterWasSpecified( "persistent" ) ) {
        const bool paramVal = getBoolParameter( "persistent" );

        if ( paramVal )
            matchCriteria.persistent = PERSISTENT_CRITERIA_TRUE;
        else
            matchCriteria.persistent = PERSISTENT_CRITERIA_FALSE;
    }

    if ( parameterWasSpecified( "otfTagId" ) ) {
        const bool paramVal = getBoolParameter( "otfTagId" );

        if ( paramVal )
            matchCriteria.otfTagId = OTF_TAG_ID_CRITERIA_TRUE;
        else
            matchCriteria.otfTagId = OTF_TAG_ID_CRITERIA_FALSE;
    }

    if ( parameterWasSpecified( "samps" ) ) {
        const int paramVal = getIntParameter( "samps" );

        if ( paramVal == 0 ) {
            matchCriteria.sampsCriteria =
                SampsCriteria( SampsCriteria::LESS, 1 );
        } else if ( paramVal > 0 ) {
            matchCriteria.sampsCriteria =
                SampsCriteria( SampsCriteria::MATCH, paramVal );
        } else {
            matchCriteria.sampsCriteria =
                SampsCriteria( SampsCriteria::GREATER, -paramVal );
        }
    }

    if ( parameterWasSpecified( "type" ) ) {
        const string paramVal = getStringParameter( "type" );

        if ( paramVal.empty() != true ) {
            if ( paramVal.at( 0 ) == '-' ) {
                const MonitorValueType mvt =
                    getMonitorValueTypeForString( paramVal.substr( 1 ) );

                matchCriteria.mvtCriteria =
                    MvtCriteria( MvtCriteria::NOT_MATCH, mvt );
            } else {
                const MonitorValueType mvt =
                    getMonitorValueTypeForString( paramVal );

                matchCriteria.mvtCriteria =
                    MvtCriteria( MvtCriteria::MATCH, mvt );
            }
        }
    }

    if ( parameterWasSpecified( "tagId" ) ) {
        const int paramVal = getIntParameter( "tagId" );

        matchCriteria.tagIdCriteria =
            TagIdCriteria( TagIdCriteria::MATCH, paramVal );
    }

    if ( parameterWasSpecified( "archive" ) ) {
        const string paramVal = getStringParameter( "archive" );

        if ( paramVal.empty() != true ) {
            if ( paramVal.at( 0 ) == '-' ) {
                const MonitorComponent::ARCHIVE_PRIORITY mcap =
                    getArchivePriorityForString( paramVal.substr( 1 ) );

                matchCriteria.mcapCriteria =
                    McapCriteria( McapCriteria::NOT_MATCH, mcap );
            } else {
                const MonitorComponent::ARCHIVE_PRIORITY mcap =
                    getArchivePriorityForString( paramVal );

                matchCriteria.mcapCriteria =
                    McapCriteria( McapCriteria::MATCH, mcap );
            }
        }
    }

    {
        const string words = getStringParameter( "words" );

        const char separatorChars[] = ", \t\n\r";

        string::size_type beginPos =
            words.find_first_not_of( separatorChars );

        while ( beginPos != string::npos ) {
            const string::size_type endPos =
                words.find_first_of( separatorChars, beginPos );

            if ( endPos == string::npos ) {
                const string finalWord( words, beginPos );

                matchCriteria.searchTexts.push_back( finalWord );

                break;
            }

            const string word( words, beginPos, (endPos - beginPos) );

            matchCriteria.searchTexts.push_back( word );

            beginPos = words.find_first_not_of( separatorChars, endPos );
        }
    }

    {
        const string phrase = getStringParameter( "phrase" );

        if ( phrase.empty() != true )
            matchCriteria.searchTexts.push_back( phrase );
    }

    {
        const string phrase2 = getStringParameter( "phrase2" );

        if ( phrase2.empty() != true )
            matchCriteria.searchTexts.push_back( phrase2 );
    }

    {
        const string phrase3 = getStringParameter( "phrase3" );

        if ( phrase3.empty() != true )
            matchCriteria.searchTexts.push_back( phrase3 );
    }

    {
        const string phrase4 = getStringParameter( "phrase4" );

        if ( phrase4.empty() != true )
            matchCriteria.searchTexts.push_back( phrase4 );
    }

    matchCriteria.caseSensitive = getBoolParameter( "caseSensitive" );


    if ( showSearchTexts ) {
        std::vector<std::string> vec;
        BOOST_FOREACH(const std::string &s, matchCriteria.searchTexts) {
            vec.push_back("\"" + s + "\"");
        }

        cout << "Search texts: {" << boost::algorithm::join(vec, ",") << "}\n";
    }

    const auto_ptr< CarmaMonitorSystem > cms( new CarmaMonitorSystem );

    if ( parameterWasSpecified( "name" ) ) {
        const string paramVal = getStringParameter( "name" );

        if ( paramVal.empty() != true ) {
            try {
                const MonitorPoint &mp = cms->getMonitorPoint(paramVal, false);
                outputMpMatch(cout, mp, brief);
                return EXIT_SUCCESS;
            } catch (...) {
                cerr << "ERROR: unable to find monitor point: " << paramVal << endl;
                return EXIT_FAILURE;
            }
        }
    }

    size_t matchTally = 0;

    {
        MonitorPointIterator mpIter( *(cms.get()) );

        while ( ++mpIter ) {
            const MonitorPoint & mp = mpIter.getMonitorPoint();

            if ( matchCriteria.matches( mp, strict ) ) {
                ++matchTally;
                outputMpMatch( cout, mp, brief );
            }
        }
    }

    if ( showTally ) {
        if ( matchTally == 1 )
            cout << matchTally << " matching monitor point was found.\n";
        else
            cout << matchTally << " matching monitor points were found.\n";
    }

    return EXIT_SUCCESS;
} catch ( ... ) {
    const string msg = "Exiting on an exception - " + getStringForCaught();

    cerr << msg << endl;
    programLogErrorIfPossible( msg );

    throw;
}
