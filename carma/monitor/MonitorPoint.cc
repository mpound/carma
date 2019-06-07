/**
 *
 * Implementation of the monitor point class.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorPoint.cc,v 1.117 2011/05/10 14:21:03 scott Exp $
 * $CarmaCopyright$
 *
 */

#include "carma/monitor/MonitorPoint.h"

#include <boost/lexical_cast.hpp>
#include <complex>
#include <ctime>
#include <iostream>
#include <iomanip>
#include <memory>
#include <sstream>

#include "carma/dbms/TagIDAuthority.h"
#include "carma/monitor/MonitorPointThreshold.h"
#include "carma/monitor/sanityChecks.h"
#include "carma/monitor/SubsystemFrame.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/Time.h"

//#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {

// When a full monitor system is created, we use sizeof(MP)xnumMP memory,
// so it is a good idea to keep an eye on the size. 
// Also, when iterating over a monitor system you might run into cache flush
// issues if the size of an MP becomes too large.
void compileTimeChecks( )
{
    // Document and keep an eye the size in bytes of a MonitorPoint instance
#if __WORDSIZE == 64
    compileTimeCheck< (sizeof( MonitorPoint ) == 136) >();
#else
    compileTimeCheck< (sizeof( MonitorPoint ) == 76) >();
#endif
}


const string kDefaultCommentText;
const string kDefaultCommentUsername( "none" );
const double kDefaultCommentMjd = 0.0;

const string kUnknownCommentUsername( "unknown" );


MonitorValue
defaultThresholdMonitorValue( )
{
    MonitorValue mv;

    mv.lo = 0;

    return mv;
}


}  // namespace < anonymous >


struct MonitorPoint::CommentAttrs {
    CommentAttrs( const string & inText,
                  const string & inUsername,
                  double         inMjd );

    string text;
    string username;
    double mjd;
};


MonitorPoint::CommentAttrs::CommentAttrs( const string & inText,
                                          const string & inUsername,
                                          double         inMjd ) :
text( inText ),
username( inUsername ),
mjd( inMjd )
{
}


struct MonitorPoint::DefThreshAttrs {
    explicit DefThreshAttrs( );

    void clearAll( );

    long         flags;
    MonitorValue thresholdMv[ THRESHOLD_NUM_VALUES ];
};


MonitorPoint::DefThreshAttrs::DefThreshAttrs( ) :
flags( THRESHOLD_NONE_SET )
{
    clearAll();
}


void
MonitorPoint::DefThreshAttrs::clearAll( )
{
    flags = THRESHOLD_NONE_SET;

    const MonitorValue mv = defaultThresholdMonitorValue();

    for ( int i = 0; i < THRESHOLD_NUM_VALUES; ++i )
        thresholdMv[ i ] = mv;
}


MonitorPoint::MonitorPoint( const string &           name,
                            const MonitorValueType   valuetype,
                            const MONITOR_POINT_TYPE monitorPointType ) :
MonitorComponent( name ),
timeSeries_( true ),
width_( 7 ),  // Default width for value as a string is 7 characters
units_(),
precision_( 0 ),
persistent_( false ),
tagID_( 0 ),
valuetype_( valuetype ),
monitorPointType_( monitorPointType ),
monitorPointHeader_( 0 ),
// The DEFAULT value is essential to allow MP's to inherit from containers above
archivePriority_(MonitorComponent::DEFAULT),
updateInterval_( 1 ),
commentAttrs_( 0 ),
snapshotAverage_(false),
defThreshAttrs_( 0 )
{
}


MonitorPoint::~MonitorPoint( )
{
  if (debug_) cout << "MonitorPoint destructor" << endl;

  delete monitorPointHeader_;
  delete commentAttrs_;
  delete defThreshAttrs_;

  // There is no need to deallocate the storage in the frameBuffer
}


string
MonitorPoint::getUnits( ) const
{
    return units_;
}


void
MonitorPoint::setUnits( const string & units )
{
    units_ = units;
}


void MonitorPoint::setWidth(short w) const
{
  width_ = w;
}

short MonitorPoint::getWidth() const
{
  return width_;
}

void MonitorPoint::setPrecision(short digits)
{
  precision_ = digits;
}

short MonitorPoint::getPrecision() const
{
  return precision_;
}



void MonitorPoint::setPersistent(bool persistent)
{
  persistent_ = persistent;
}

void
MonitorPoint::clearAllDefaults( )
{
    if ( defThreshAttrs_ != 0 )
        defThreshAttrs_->clearAll();
}


void
MonitorPoint::setErrorHighDefault( const MonitorValue threshold )
{
    if ( defThreshAttrs_ == 0 )
        defThreshAttrs_ = new DefThreshAttrs;

    defThreshAttrs_->thresholdMv[ THRESHOLD_HIGH_ERROR_VALUE ] = threshold;
    defThreshAttrs_->flags |= THRESHOLD_ERROR_HIGH_SET;
}


void
MonitorPoint::setWarnHighDefault( const MonitorValue threshold )
{
    if ( defThreshAttrs_ == 0 )
        defThreshAttrs_ = new DefThreshAttrs;

    defThreshAttrs_->thresholdMv[ THRESHOLD_HIGH_WARN_VALUE ] = threshold;
    defThreshAttrs_->flags |= THRESHOLD_WARN_HIGH_SET;
}


void
MonitorPoint::setWarnLowDefault( const MonitorValue threshold )
{
    if ( defThreshAttrs_ == 0 )
        defThreshAttrs_ = new DefThreshAttrs;

    defThreshAttrs_->thresholdMv[ THRESHOLD_LOW_WARN_VALUE ] = threshold;
    defThreshAttrs_->flags |= THRESHOLD_WARN_LOW_SET;
}


void
MonitorPoint::setErrorLowDefault( const MonitorValue threshold )
{
    if ( defThreshAttrs_ == 0 )
        defThreshAttrs_ = new DefThreshAttrs;

    defThreshAttrs_->thresholdMv[ THRESHOLD_LOW_ERROR_VALUE ] = threshold;
    defThreshAttrs_->flags |= THRESHOLD_ERROR_LOW_SET;
}


MonitorValue
MonitorPoint::getErrorHighDefault( ) const
{
    if ( defThreshAttrs_ == 0 )
        return defaultThresholdMonitorValue();

    return defThreshAttrs_->thresholdMv[ THRESHOLD_HIGH_ERROR_VALUE ];
}


MonitorValue
MonitorPoint::getWarnHighDefault( ) const
{
    if ( defThreshAttrs_ == 0 )
        return defaultThresholdMonitorValue();

    return defThreshAttrs_->thresholdMv[ THRESHOLD_HIGH_WARN_VALUE ];
}


MonitorValue
MonitorPoint::getWarnLowDefault( ) const
{
    if ( defThreshAttrs_ == 0 )
        return defaultThresholdMonitorValue();

    return defThreshAttrs_->thresholdMv[ THRESHOLD_LOW_WARN_VALUE ];
}


MonitorValue
MonitorPoint::getErrorLowDefault( ) const
{
    if ( defThreshAttrs_ == 0 )
        return defaultThresholdMonitorValue();

    return defThreshAttrs_->thresholdMv[ THRESHOLD_LOW_ERROR_VALUE ];
}


bool
MonitorPoint::errorHighDefaultIsSet( ) const
{
    if ( defThreshAttrs_ == 0 )
        return false;

    return ((defThreshAttrs_->flags & THRESHOLD_ERROR_HIGH_SET)
            == THRESHOLD_ERROR_HIGH_SET);
}


bool
MonitorPoint::errorLowDefaultIsSet( ) const
{
    if ( defThreshAttrs_ == 0 )
        return false;

    return ((defThreshAttrs_->flags & THRESHOLD_ERROR_LOW_SET)
            == THRESHOLD_ERROR_LOW_SET);
}


bool
MonitorPoint::warnHighDefaultIsSet( ) const
{
    if ( defThreshAttrs_ == 0 )
        return false;

  return ((defThreshAttrs_->flags & THRESHOLD_WARN_HIGH_SET)
          == THRESHOLD_WARN_HIGH_SET);
}


bool
MonitorPoint::warnLowDefaultIsSet( ) const
{
    if ( defThreshAttrs_ == 0 )
        return false;

  return ((defThreshAttrs_->flags & THRESHOLD_WARN_LOW_SET)
          == THRESHOLD_WARN_LOW_SET);
}


void
MonitorPoint::evaluateTolerance( const MonitorPointThreshold & threshold )
{
}


void
MonitorPoint::checkThreshold( const MonitorPointThreshold & threshold ) const
{
  tagIDType thisTagID   = getTagID();
  tagIDType threshTagID = threshold.getTagID();

  if ((threshTagID != thisTagID)
      ||  (threshold.getValueType() != getValuetype()))  {
    ostringstream os;
    os  << "MonitorPoint::checkThreshold: Checking threshold for "
        << "monitor point " << getCanonicalName()
        << " with TagID " << getTagID()
        << " {" << dbms::TagIDAuthority::getSubsystemID (thisTagID)
        << "/"  << dbms::TagIDAuthority::getPointID (thisTagID)
        << "} "
        <<  "\nInput MonitorPointThreshold does not match. "
        << "Input threshold tagID is {"
        << dbms::TagIDAuthority::getSubsystemID (threshTagID)
        << "/" << dbms::TagIDAuthority::getPointID (threshTagID)
        << "} and value type is "
        << threshold.getValueType()
        << "\n";
    throw CARMA_EXCEPTION (InvalidThresholdException, os.str());
  }
}


void
MonitorPoint::setComment( const string & text )
{
    setComment( text,
                kUnknownCommentUsername,
                (40587 + time(0) / 86400.0) );
}


void
MonitorPoint::setComment( const string & text,
                          const string & username,
                          const double   mjd )
{
    // May throw
    auto_ptr< CommentAttrs >
        newAttrs( new CommentAttrs( text, username, mjd ) );

    // Won't throw
    const auto_ptr< CommentAttrs > oldAttrs( commentAttrs_ );

    // Won't throw
    commentAttrs_ = newAttrs.release();

    // destructing oldAttrs may throw but shouldn't
}


void
MonitorPoint::clearComment( )
{
    // Won't throw
    const auto_ptr< CommentAttrs > oldAttrs( commentAttrs_ );

    // Won't throw
    commentAttrs_ = 0;

    // destructing oldAttrs may throw but shouldn't
}


string
MonitorPoint::getComment( ) const
{
    if ( commentAttrs_ == 0 )
        return kDefaultCommentText;

    return commentAttrs_->text;
}


string
MonitorPoint::getCommentUser( ) const
{
    if ( commentAttrs_ == 0 )
        return kDefaultCommentUsername;

    return commentAttrs_->username;
}


double
MonitorPoint::getCommentTime( ) const
{
    if ( commentAttrs_ == 0 )
        return kDefaultCommentMjd;

    return commentAttrs_->mjd;
}


void MonitorPoint::setMonitorPointHeader(MonitorPointHeader header)
{
  monitorPointHeader_ = new MonitorPointHeader(header);
}


namespace {


const MonitorSampleValue &
getMonitorSampleValue0Ref( const MonitorPointHeader & mph )
{
    const int iSample =
        static_cast< int >( mph.getNumSamplesPerCycle() != 1 );

    return mph.getMonitorSampleValueRef( iSample );
}


void
throwNoSamplesError( const MonitorPoint & mp,
                     const ushort         mphSampleOffset,
                     const tagIDType      mphTagId )
{
    const tagIDType mpTagId = mp.getTagID();

    ostringstream oss;

    oss << "MonitorPoint(" << mp.getCanonicalName() << ") "
        << "has no samples. "
        << " TagID = " << mpTagId
        << " (" << dbms::TagIDAuthority::getSubsystemID( mpTagId )
        << "/"  << dbms::TagIDAuthority::getPointID( mpTagId )
        << ")"
        << ", sample offset = " << mphSampleOffset
        << ", header TagID = " << mphTagId;

    throw CARMA_ERROR( oss.str() );
}


}  // namespace < anonymous >

// ---------------------------------------------------------
// Basic accessor
// This interface has indices that start at 0, but in the actual storage
// the first sample (index=0) is the average.
// If there is only one sample then it maps to the average to save space
// and computation of the average.

MonitorPointSample
MonitorPoint::getMonitorPointSample0( ) const
{
    const int iSample =
        static_cast< int >( monitorPointHeader_->getNumSamplesPerCycle() != 1 );

    return monitorPointHeader_->getMonitorPointSample( iSample );
}


MonitorPointSample
MonitorPoint::getMonitorPointSample( const int sampleIndex ) const
{
    const int numSamples = getNumSamples();

    // Handle the special but common case first: single sample per frame
    if ( (sampleIndex == 0) && (numSamples == 1) )
        return monitorPointHeader_->getMonitorPointSample( 0 );

    if ( (sampleIndex >= 0) && (sampleIndex < numSamples) )
        return monitorPointHeader_->getMonitorPointSample( sampleIndex + 1 );

    if ( numSamples == 0 ) {
        throwNoSamplesError( *this,
                             monitorPointHeader_->getSampleOffset(),
                             monitorPointHeader_->getTagID() );
    }

    ostringstream oss;

    oss << "MonitorPoint(" << getCanonicalName() << ") "
        << "sampleIndex " << sampleIndex
        << " is out of range [0, " << numSamples << ")";

    throw CARMA_ERROR( oss.str() );
}


void MonitorPoint::setNumSamples(const int numSamples) const
{
  monitorPointHeader_->setNumSamplesPerCycle(numSamples);
}


void
MonitorPoint::setValidity( const VALIDITY validity,
                           const int      sampleIndex ) const
{
    const uchar saneVf =
        sanityCheckValidityFlags(
            static_cast< unsigned char >( validity ),
            this,
            true,
            "MonitorPoint::setValidity" );

    getMonitorPointSample( sampleIndex ).setKnownSaneValidityFlags( saneVf );
}


void
MonitorPoint::setAveValidity( const VALIDITY validity ) const
{
    const uchar saneVf =
        sanityCheckValidityFlags(
            static_cast< unsigned char >( validity ),
            this,
            true,
            "MonitorPoint::setAveValidity" );

    getSampleAverage().setKnownSaneValidityFlags( saneVf );
}


void
MonitorPoint::setNoData( ) const
{
    const int numSamples = getNumSamples();

    const uchar saneVf = static_cast< unsigned char >( INVALID_NO_DATA );

    if ( numSamples == 1 ) {
        monitorPointHeader_->getMonitorPointSample( 0 ).
            setKnownSaneValidityFlags( saneVf );

        return;
    }

    if ( numSamples <= 0 )
        return;

    // Loop over [1, numSamples]
    for ( int i = 1; i <= numSamples; ++i ) {
        monitorPointHeader_->getMonitorPointSample( i ).
            setKnownSaneValidityFlags( saneVf );
    }
}


void
MonitorPoint::setAllValidity( const VALIDITY validity,
                              const bool markMpAsModified ) const
{
    const uchar saneVf =
        sanityCheckValidityFlags(
            static_cast< unsigned char >( validity ),
            this,
            true,
            "MonitorPoint::setAllValidity" );

    const int numSamples = getNumSamples();

    if ( numSamples == 1 ) {
        monitorPointHeader_->getMonitorPointSample( 0 ).
            setKnownSaneValidityFlags( saneVf, markMpAsModified );

        return;
    }

    if ( numSamples <= 0 ) return;

    // Loop over [1, numSamples]
    for ( int i = 1; i <= numSamples; ++i ) {
        monitorPointHeader_->getMonitorPointSample( i ).
            setKnownSaneValidityFlags( saneVf, markMpAsModified );
    }

    // Set the average validity too
    monitorPointHeader_->getMonitorPointSample( 0 ).
        setKnownSaneValidityFlags( saneVf, markMpAsModified );
}


MonitorPoint::VALIDITY
MonitorPoint::getValidity( ) const
{
    return static_cast< VALIDITY >(
        getMonitorSampleValue0Ref( *monitorPointHeader_ ).getValidityFlags() );
}


MonitorPoint::VALIDITY
MonitorPoint::getValidity( const int sampleIndex ) const
{
  return static_cast< VALIDITY >(getMonitorPointSample(sampleIndex).getValidityFlags());
}


MonitorPoint::VALIDITY
MonitorPoint::getAveValidity( ) const
{
    return static_cast< VALIDITY >( getSampleAverage().getValidityFlags() );
}


bool
MonitorPoint::isValid( ) const
{
    return isValid( getValidity() );
}


bool
MonitorPoint::isValid( const int sampleIndex ) const
{
    return isValid( getValidity( sampleIndex ) );
}


int
MonitorPoint::getNumValidSamples( ) const
{
    const int numSamples = getNumSamples();

    if ( numSamples == 1 ) {
        const VALIDITY v =
            static_cast< VALIDITY >(
                monitorPointHeader_->
                    getMonitorSampleValueRef( 0 ).getValidityFlags() );

        if ( isValid( v ) )
            return 1;
        else
            return 0;
    }

    int numValidSamples = 0;

    for ( int i = 0; i < numSamples; ++i ) {
        if ( isValid( i ) )
            ++numValidSamples;
    }

    return numValidSamples;
}


void MonitorPoint::setBlankingFlagging(BLANKING_FLAGGING flag) const
{
  getSampleAverage().setBlankingFlags(static_cast< unsigned char >(flag));
}

string MonitorPoint::getSnapshotAverageToString() const 
{
    if (isSnapshotAverage()) return "SnapshotAverage";
    else                     return "NormalAverage";
}    

string MonitorPoint::getPaddedValueString(int sampleIndex) const
{
  return getPaddedValueString(getWidth(), sampleIndex);
}

string MonitorPoint::getPaddedValueString(int w, int sampleIndex) const
{
  string s = getValueToString(sampleIndex);
  int len = s.length();
  if (len > w) {
    return string(w, '*');
  }
  if (len == w) return s;

  return string(w-len, ' ') + s;
}

string MonitorPoint::getPaddedAverageString() const
{
  return getPaddedAverageString(getWidth());
}

string MonitorPoint::getPaddedAverageString(int w) const
{
  string s = getAverageToString();
  int len = s.length();
  if (len > w) {
    return string(w, '*');
  }
  if (len == w) return s;
  return string(w-len, ' ') + s;
}

string MonitorPoint::getCoreValueAsString( const int sampleIndex ) const
{
    using namespace boost;
    const MonitorValueType mpType = getValuetype( );
    switch ( mpType ) {
        case MONITOR_VALUE_TYPE_BYTE:
            return lexical_cast< string >( getValueChar( sampleIndex ) );
        case MONITOR_VALUE_TYPE_SHORT:
            return lexical_cast< string >( getValueShort( sampleIndex ) );
        case MONITOR_VALUE_TYPE_INTEGER:
            return lexical_cast< string >( getValueLong( sampleIndex ) );
        case MONITOR_VALUE_TYPE_BOOLEAN:
            return lexical_cast< string >( getValueBoolean( sampleIndex ) );
        case MONITOR_VALUE_TYPE_FLOAT:
            return lexical_cast< string >( getValueFloat( sampleIndex ) );
        case MONITOR_VALUE_TYPE_DOUBLE:
            return lexical_cast< string >( getValueDouble( sampleIndex ) );
        case MONITOR_VALUE_TYPE_COMPLEX:
            return lexical_cast< string >( getValueComplex( sampleIndex ) );
        case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
            return lexical_cast< string >( getValueSerialNo() );
        case MONITOR_VALUE_TYPE_STRING:
            return getValueToString( sampleIndex );
        default:
            throw CARMA_ERROR( "Invalid type enumeration." );
    }
}

string MonitorPoint::toStringShort(int sampleIndex) const
{
  ostringstream s;
  s << getName() << "=" << getPaddedValueString(sampleIndex);
  return s.str();
}

string MonitorPoint::toString(bool canonicalName, bool verbose,
                              bool value, int sampleIndex, int indent) const
{
  return
    toString(canonicalName, verbose, value, sampleIndex, indent, false);
}

string MonitorPoint::toStringAverage(bool canonicalName, bool verbose,
                                     bool value, int indent) const
{
  return
    toString(canonicalName, verbose, value, 0, indent, true);
}


string MonitorPoint::toString(bool canonicalName, bool verbose,
           bool value, int sampleIndex, int indent, bool average) const
{
  const string separator(" ");
  ostringstream s;
  s.setf(ios::fixed);
  int id = getTagID();
  unsigned short ssID = dbms::TagIDAuthority::getSubsystemID(id);
  unsigned short mpID = dbms::TagIDAuthority::getPointID(id);

  string valueString;
  string validityString;
  bool   valid;
  if (average) {
    valueString    = getPaddedAverageString();
    validityString = validityToString(getAveValidity());
    valid          = isAveValid();
  }
  else {
    valueString    = getPaddedValueString(sampleIndex);
    validityString = validityToString(getValidity(sampleIndex));
    valid          = isValid(sampleIndex);
  }

  s << setw(indent) << "";
  if (canonicalName) {
    s << getCanonicalName();
  }
  else {
    if (!verbose)s << setw(18);
    s << getName();
  }
  if(verbose) s << getPhysicalDeviceString();
  if (value && !verbose) {
    s << separator
      << valueString;
    if (valid) {
      s << "g"; // good
    }
    else {
      s << "b";  // bad
    }
  }

  indent++; // Subsequent lines (if any) will have more indent
  if (verbose) {
    s << '\n'  << setw(indent) << ""
      << "Valuetype:" << valuetypeToString()
      << separator
      << "timeSeries:"
      << (isTimeSeries()?"Y":"N")
      << '\n'  << setw(indent) << ""
      << "Desc:" << "\'" << getDescription() << "\'"
      << "\n" << setw(indent) << ""
      << (persistent_ ? "Persistent" : "Non-persistent")
      << separator
      << "tag=" << id   << "(" << ssID << "/"  << mpID << ")"
      << separator
      << "archivePriority:" << archivePriorityToString()
      << separator
      << "type:" << monitorPointTypeToString()
      << "\n" << setw(indent) << ""
      << "width=" << getWidth() << separator
      << "precision=" << getPrecision() << separator
      << "\n" << setw(indent) << ""
      << "nSamps:" << getNumSamples()
      << " Ave="
      << getPaddedAverageString()
      << separator << getUnits()
      << separator << separator
      << validityToString(getAveValidity())
      << separator
      << blankingFlaggingToString(getBlankingFlagging())
      << separator << separator
      << getSnapshotAverageToString()
      << '\n' << setw(indent) << "";
    if (!average) {
      s << "Samp" << sampleIndex << "="
        << valueString
        << separator << getUnits()
        << separator << separator
        << validityString
        << '\n' << setw(indent) << "";
    }
    if (getNumSamples() > 1) {
        s << dumpSamples(false, true)
          << '\n' << setw(indent) << "";
    }
    s << "CanonicalName=" << getCanonicalName() << separator
      << '\n' << setw(indent) << ""
      << "shortName=" << getShortName() << separator
      << "longName=\"" << getLongName() <<"\"" << separator
      << '\n' << setw(indent) << ""
      << "Comment=" << "'" << getComment() << "'"
      << " at MJD="<<setprecision(3) << getCommentTime() << ","
      << separator << Time::getDateTimeString(getCommentTime())
      << ", by " << getCommentUser()
      <<'\n';
  }
  return s.str();
}

string MonitorPoint::dumpSamples(bool includeAverage,
                                 bool includeValidity) const
{
  ostringstream o;
  o.setf(ios::fixed);
  const string separator = " ";

  const int numSamples = getNumSamples();
  for (int i=0; i<numSamples; i++) {
    o << getPaddedValueString(i) ;
    if (includeValidity) {
      if (isValid(i)) o << "g";
      else            o << "b";
    }
    o << separator;
  }
  if (includeAverage) {
    o << "Ave:" << getPaddedAverageString();
    if (isValid(getAveValidity())) o << "g";
    else                           o << "b";
  }
  return o.str();
}


string MonitorPoint::monitorPointTags(bool untagged) const
{
  ostringstream o;
  int id = getTagID();
  unsigned short ssID = dbms::TagIDAuthority::getSubsystemID(id);
  unsigned short mpID = dbms::TagIDAuthority::getPointID(id);
  bool hasMonitorPointID = (ssID != 0);
  if((untagged && hasMonitorPointID) ||
     (!untagged && !hasMonitorPointID)) return "";
  o << setw(3) << ssID
    << "/"
    << setw(3) << mpID
    << "(" << setw(7) << id << ")    "
    << getCanonicalName()
    << "\n";
  return o.str();
}


string
MonitorPoint::valuetypeToString( const MonitorValueType mvt )
{
    switch ( mvt ) {
        case MONITOR_VALUE_TYPE_BYTE:          return "BYTE";
        case MONITOR_VALUE_TYPE_SHORT:         return "SHORT";
        case MONITOR_VALUE_TYPE_INTEGER:       return "INTEGER";
        case MONITOR_VALUE_TYPE_BOOLEAN:       return "BOOLEAN";
        case MONITOR_VALUE_TYPE_FLOAT:         return "FLOAT";
        case MONITOR_VALUE_TYPE_DOUBLE:        return "DOUBLE";
        case MONITOR_VALUE_TYPE_COMPLEX:       return "COMPLEX";
        case MONITOR_VALUE_TYPE_STRING:        return "STRING";
        case MONITOR_VALUE_TYPE_SERIAL_NUMBER: return "SERIAL_NUMBER";
    }

    return "Unknown valuetype!!";
}

string MonitorPoint::valuetypeToString() const
{
  return valuetypeToString(getValuetype());
}


string
MonitorPoint::monitorPointTypeToString( const MONITOR_POINT_TYPE mpt )
{
    switch ( mpt ) {
        case MONITOR: return "MONITOR";
        case CONTROL: return "CONTROL";
    }

    return "Unknown monitor point type!!";
}

string
MonitorPoint::monitorPointTypeToString( ) const
{
    return monitorPointTypeToString( getMonitorPointType() );
}


string
MonitorPoint::validityToString( const VALIDITY validity )
{
    switch ( validity ) {
        case INVALID_NO_DATA:    return "INVALID_NO_DATA";
        case INVALID_NO_HW:      return "INVALID_NO_HW";
        case INVALID_HW_BAD:     return "INVALID_HW_BAD";
        case VALID:              return "VALID";
        case VALID_NOT_CHECKED:  return "VALID_NOT_CHECKED";
        case VALID_GOOD:         return "VALID_GOOD";
        case VALID_WARNING:      return "VALID_WARNING";
        case VALID_ERROR:        return "VALID_ERROR";
        case VALID_WARNING_LOW:  return "VALID_WARNING_LOW";
        case VALID_WARNING_HIGH: return "VALID_WARNING_HIGH";
        case VALID_ERROR_LOW:    return "VALID_ERROR_LOW";
        case VALID_ERROR_HIGH:   return "VALID_ERROR_HIGH";

        case MAX_VALIDITY: break;
    }

    return "No validity string!!";
}

string
MonitorPoint::blankingFlaggingToString( const BLANKING_FLAGGING f )
{
    switch ( f ) {
        case OK:              return "OK";
        case UNDETERMINED:    return "UNDETERMINED";
        case BLANKED:         return "BLANKED";
        case FLAGGED:         return "FLAGGED";
        case BLANKED_FLAGGED: return "BLANKED_FLAGGED";

        case MAX_BLANKING_FLAGGING: break;
    }

    return "No blanking/flagging string!!";
}


string MonitorPoint::archivePriorityToString() const
{
  return MonitorComponent::archivePriorityToString(getArchivePriority());
}

void MonitorPoint::setArchivePriority(
    MonitorComponent::ARCHIVE_PRIORITY archivePriority)
{
    // If it's already set to DONTARCHIVE, quietly ignore requests to change it.
    if(archivePriority_ != MonitorComponent::DONTARCHIVE) {
        archivePriority_ = archivePriority;
    }
}

void MonitorPoint::setDefaultArchivePriority(
    MonitorComponent::ARCHIVE_PRIORITY archivePriority)
{
    if(archivePriority_ == MonitorComponent::DEFAULT) {
        archivePriority_ = archivePriority;
    }
}

void MonitorPoint::setUpdateInterval(int interval)
{
  updateInterval_ = interval;
}

// Must be overridden is classes that support spectra
void MonitorPoint::setTimeSeries(bool timeSeries)
{
  if (!timeSeries) {
    ostringstream o;
    o << "MonitorPoint: Can't have non-timeSeries data for this type ("
      << valuetypeToString() << ")" ;
    throw CARMA_ERROR(o);
  }
  timeSeries_ = timeSeries;
}


bool MonitorPoint::isEqualTo (const MonitorPoint& mp) const
{
  // Check tagID
  if (mp.getTagID() != getTagID())return false;

  // Check data type
  if (mp.getValuetype() != getValuetype())return false;

  // Check name
  if (mp.getName() != getName())return false;

  return true;
}


// Checks tagID, type and name
bool MonitorPoint::operator==(const MonitorComponent& component) const
{
  // Make sure its a monitor point and not a monitor container
  const MonitorPoint* mp = dynamic_cast<const MonitorPoint*>(&component);
  if ( mp == 0 ) return false;

  const MonitorPoint& m = *mp;

  return this->isEqualTo (m);
}

// Checks tagID and name
bool MonitorPoint::operator==(const MonitorPoint& mpoint) const
{
  // Make sure its a monitor point and not a monitor container

  // Check tagID
  if (mpoint.getTagID() != getTagID())return false;

  // Check name
  if (mpoint.getName() != getName())return false;

  return true;
}

// Orders by tagID
bool MonitorPoint::operator<(const MonitorPoint& mpoint) const
{
  // Check tagID
  if (getTagID() < mpoint.getTagID())return true;

  return false;
}


// This default implementation returns false
bool MonitorPoint::isMonitorPoint() const
{
  return true;
}

bool MonitorPoint::hasAllData() const
{
  // Check all samples
  const int numSamples = getNumSamples();
  for (int i=0; i<numSamples; i++) {
    if (getValidity(i) == INVALID_NO_DATA)return false;
  }
  return true;
}


// ------------------------ Get methods for samples-------------------
char        MonitorPoint::getValueChar(int sampleIndex) const
{
  return getMonitorPointSample(sampleIndex).getMonitorValue().byte;
}

short       MonitorPoint::getValueShort(int sampleIndex) const
{
  return getMonitorPointSample(sampleIndex).getMonitorValue().sh;
}

long        MonitorPoint::getValueLong(int sampleIndex) const
{
  return getMonitorPointSample(sampleIndex).getMonitorValue().lo;
}

bool        MonitorPoint::getValueBoolean(int sampleIndex) const
{
  return getMonitorPointSample(sampleIndex).getMonitorValue().bo;
}

float       MonitorPoint::getValueFloat(int sampleIndex) const
{
  return getMonitorPointSample(sampleIndex).getMonitorValue().fl;
}

double      MonitorPoint::getValueDouble(int sampleIndex) const
{
  return getMonitorPointSample(sampleIndex).getMonitorValue().db;
}

complex<float>  MonitorPoint::getValueComplex(int sampleIndex) const
{
  float* c = getMonitorPointSample(sampleIndex).getMonitorValue().complex;
  return complex<float>(c[0], c[1]);
}


MonitorValueStringChunk
MonitorPoint::getValueStringChunk( const int sampleIndex ) const
{
    MonitorValueStringChunk chunk;

    const char * const s =
        getMonitorPointSample( sampleIndex ).getMonitorValue().str;

    for ( size_t i = 0; i < 8; ++i )
        chunk.chunkChars[ i ] = s[ i ];

    return chunk;
}


void
MonitorPoint::setValuesStringChunksAndValidities(
    const MonitorValueStringChunk * const chunks,
    const int                             numChunks,
    const VALIDITY                        validity ) const
{
    const uchar saneVf =
        sanityCheckValidityFlags(
            static_cast< unsigned char >( validity ),
            this,
            true,
            "MonitorPoint::setValuesStringChunksAndValidities" );

    for ( int i = 0; i < numChunks; ++i ) {
        MonitorPointSample m = getMonitorPointSample( i );

        m.setValueStringChunkAndKnownSaneValidityFlags( chunks[ i ], saneVf );
    }
}


long        MonitorPoint::getValueSerialNo() const
{
  return getMonitorPointSample(0).getMonitorValue().sn;
}


// --------------------- Sample set methods --------------------
// The validity is always set *after* the value

void MonitorPoint::setValue(char d,    int sampleIndex) const
{
  MonitorPointSample m = getMonitorPointSample(sampleIndex);
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setValue(short d,   int sampleIndex) const
{
  MonitorPointSample m = getMonitorPointSample(sampleIndex);
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setValue(long d,    int sampleIndex) const
{
  MonitorPointSample m = getMonitorPointSample(sampleIndex);
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setValue(bool d, int sampleIndex) const
{
  MonitorPointSample m = getMonitorPointSample(sampleIndex);
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setValue(float d,   int sampleIndex) const
{
  MonitorPointSample m = getMonitorPointSample(sampleIndex);
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setValue(double d,  int sampleIndex) const
{
  MonitorPointSample m = getMonitorPointSample(sampleIndex);
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setValue(const complex<float>& c,  int sampleIndex) const
{
  float f[2] = {c.real(), c.imag()};
  MonitorPointSample m = getMonitorPointSample(sampleIndex);
  m.setMonitorValueAndKnownSaneValidityFlags( f, VALID_NOT_CHECKED );
}


void MonitorPoint::setValueSerialNo(long d) const
{
  MonitorPointSample m = getMonitorPointSample(0);
  m.setValueSerialNoAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

//===================================================================
// --------------------- Average set methods --------------------
// The validity is always set *after* the value

void MonitorPoint::setAve(char d) const
{
  MonitorPointSample m = getSampleAverage();
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setAve(short d) const
{
  MonitorPointSample m = getSampleAverage();
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setAve(long d) const
{
  MonitorPointSample m = getSampleAverage();
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setAve(bool d) const
{
  MonitorPointSample m = getSampleAverage();
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setAve(float d) const
{
  MonitorPointSample m = getSampleAverage();
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setAve(double d) const
{
  MonitorPointSample m = getSampleAverage();
  m.setMonitorValueAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}

void MonitorPoint::setAve(const complex<float>& c) const
{
  float f[2] = {c.real(), c.imag()};
  MonitorPointSample m = getSampleAverage();
  m.setMonitorValueAndKnownSaneValidityFlags( f, VALID_NOT_CHECKED );
}

void MonitorPoint::setAveSerialNo(long d) const
{
  MonitorPointSample m = getSampleAverage();
  m.setValueSerialNoAndKnownSaneValidityFlags( d, VALID_NOT_CHECKED );
}


string
MonitorPoint::hierarchyToString( const bool canonical,
                                 const bool verbose,
                                 const bool value,
                                 const int  sampleIndex,
                                 const int  indent,
                                 const int  levels ) const
{
    ostringstream o;

    o << toString( canonical, verbose, value, sampleIndex, indent );
    if (!verbose) o << "\n";

    return o.str();
}


void
MonitorPoint::hierarchyToVector( vector< string > & hierarchyList,
                                 const bool         canonical,
                                 const bool         verbose,
                                 const int          sampleIndex ) const
{
    hierarchyList.push_back( toString( canonical, verbose, sampleIndex, 0 ) );
}


string
MonitorPoint::hierarchyToStringAverage( const bool canonical,
                                        const bool verbose,
                                        const bool value,
                                        const int  indent,
                                        const int  levels ) const
{
    ostringstream o;

    o << toStringAverage( canonical, verbose, value, indent );
    if (!verbose) o << "\n";

    return o.str();
}


string
MonitorPoint::leafToString( const bool verbose,
                            const bool value,
                            const int  sampleIndex ) const
{
    ostringstream o;

    o << toString(false, verbose, value, sampleIndex, 0) ;
    if (!verbose) o << "\n";

    return o.str();
}

void MonitorPoint::setSnapshotAverage(bool state) 
{
    snapshotAverage_ = state;
}

