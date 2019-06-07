#include "carma/dbms/SyslogMMAPOperator.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"

#include <iosfwd>
#include <iostream>
#include <map>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

using namespace ::std;
using namespace carma;
using namespace carma::dbms;
using namespace carma::util;


SyslogMMAPOperator::SyslogMMAPOperator( SyslogMMAPFile &smapf ) 
  :
  _smapf( smapf )
{
  CPTRACE( Trace::TRACE6, "      c'tor" );

  _setSize = 0;
  _pos = _cpos = _smapf.getCurrentPos();
  _current = false;
  _hasFilter = false;
  _opGreater = true;
  _fieldCheck = SyslogMMAPOperator::ALL;
  _regex = (regex_t *)NULL;

  CPTRACE( Trace::TRACE6, "      c'tor exiting successfully" );
}

SyslogMMAPOperator::~SyslogMMAPOperator()
{
  CPTRACE( Trace::TRACE6, "      d'tor" );
  CPTRACE( Trace::TRACE6, "      d'tor exiting successfully" );
}

void SyslogMMAPOperator::setWayBackMachine( double mjdOffset )
{
  _mjdOffset = mjdOffset;

  // set position  so that it points to whatever message
  // is found via going back through time...
  // this is basically a binary search
  CPTRACE( Trace::TRACE1, " before nearest, _pos:" << _pos << " _cpos:" << _cpos );
  _pos = _smapf.getNearestAgedPos( Time::MJD() - _mjdOffset );
  // bookend
  _cpos = _smapf.getCurrentPos();
  CPTRACE( Trace::TRACE1, " after nearest, _pos:" << _pos << " _cpos:" << _cpos );
}

SyslogMessage *SyslogMMAPOperator::nextMessage()
{
  SyslogMessage *msg = (SyslogMessage *)NULL;

  CPTRACE( Trace::TRACE4, "  nextMessage _pos:" << _pos << " _cpos:" << _cpos );
  // display of most up to date message...
  if ( _pos == _cpos && _current == false )
  {
    _current = true;
    msg = _smapf.getMessageAt( _pos );
  }
  else if ( _pos != _cpos )
  {
    msg = _smapf.getMessageAtAndUpdateNext( _pos );

    if ( _current )
    {
      // throw away this  message, because
      // it's already been sent back...
      delete msg;
      msg = (SyslogMessage *)NULL;
    }

    _current = false;
  }

  _cpos = _smapf.getCurrentPos();

  // Now filter this message
  if ( msg != (SyslogMessage *)NULL )
  {
    CPTRACE( Trace::TRACE3, " hasFilter: " << (boolalpha) << hasFilter() );
    if ( hasFilter() )
    {

      if ( doesMatch( msg ) == false )
      {
	CPTRACE( Trace::TRACE2, "  FAILED match on: " << msg->toString() );
	delete msg;
	msg = (SyslogMessage *)NULL;
      }
    } // hasFilter
  } // if msg NULL

  return msg;
}

bool SyslogMMAPOperator::doesMatch( SyslogMessage *msg )
{
  bool match = false;
  const char *text = _textString.c_str();

  // Any match that explicitly fails, immediately return that
  // condition

  if ( _opGreater )
  {
    if ( msg->getIntPrio() >= _prioInt )
      match = true;
    else
      return false;
  }
  else
  {
    if ( msg->getIntPrio() == _prioInt )
      match = true;
    else
      return false;
  }

  if ( _textString.length() > 0 )
  {
    CPTRACE( Trace::TRACE1, " ** field: " << _fieldCheck );

    if ( _fieldCheck == SyslogMMAPOperator::PROGRAM 
	|| _fieldCheck == SyslogMMAPOperator::ALL )
    {

      if ( _regex != (regex_t *)NULL )
      {
	if ( strstr( msg->getProgram(), text ) != NULL ) 
	  return true; // kick out immediately
      }
      else
      {
	if ( regexec( _regex, msg->getProgram(), 0, NULL, 0 ) == 0 )
	  return true;
      }
      match = false;
    }
    else if ( _fieldCheck == SyslogMMAPOperator::LOGNAME 
	|| _fieldCheck == SyslogMMAPOperator::ALL )
    {
      if ( _regex != (regex_t *)NULL )
      {
	if ( strstr( msg->getLog(), text ) != NULL ) 
	  return true; // kick out immediately
      }
      else
      {
	if ( regexec( _regex, msg->getLog(), 0, NULL, 0 ) == 0 )
	  return true;
      }
      match = false;
    }
    else if ( _fieldCheck == SyslogMMAPOperator::NDC 
	|| _fieldCheck == SyslogMMAPOperator::ALL )
    {
      if ( _regex != (regex_t *)NULL )
      {
	if ( strstr( msg->getFullyQualifiedProgramName(), text ) != NULL ) 
	  return true; // kick out immediately
      }
      else
      {
	if ( regexec( _regex, msg->getFullyQualifiedProgramName(), 0, NULL, 0 ) == 0 )
	  return true;
      }
      match = false;
    }
    else if ( _fieldCheck == SyslogMMAPOperator::MESSAGE 
	|| _fieldCheck == SyslogMMAPOperator::ALL )
    {
      if ( _regex != (regex_t *)NULL )
      {
	if ( strstr( msg->getMessage(), text ) != NULL ) 
	  return true; // kick out immediately
      }
      else
      {
	if ( regexec( _regex, msg->getMessage(), 0, NULL, 0 ) == 0 )
	  return true;
      }
    }
    match = false;
  }
  else
    match = true; // if zero length it matches

  return match;
}

void SyslogMMAPOperator::setFilter( ::std::string &myFilter )
{
  _filter = string(myFilter);

  // parse filter
  unsigned char d = '|';

  // find mjd
  string::size_type p = myFilter.find_first_of( d, 0 ), l;
  _mjdOffsetString = myFilter.substr( 0, p );
  l = p+1;

  p = myFilter.find_first_of( d, l+1 );
  _opString = myFilter.substr( l, p - l );
  _opGreater = _opString == ">=";
  l = p+1;


  p = myFilter.find_first_of( d, l );
  _valString = myFilter.substr( l, p - l );
  l = p+1;

  p = myFilter.find_first_of( d, l );
  _textString = myFilter.substr( l, p - l );
  // Create regex based on this filter
  // TODO, have a bad filter response if
  // string does not compile
  if ( _regex != (regex_t *)NULL )
    regfree( _regex );

  _regex = new regex_t;
  CPTRACE( Trace::TRACE1, "*** Compiling regex *** -> " << _textString );
  int s = regcomp( _regex, _textString.c_str(), REG_ICASE|REG_NOSUB|REG_EXTENDED);
  if ( s != 0 )
  {
    char err[256];
    regerror( s, _regex, err, 255 );
    CPTRACE( Trace::TRACE1, " regex compilation error: " << err );
    regfree( _regex );
    _regex = (regex_t *)NULL;
  }
  l = p+1;

  p = myFilter.find_first_of( d, l );
  _fieldString = myFilter.substr( l, p - l );
  if ( _fieldString == "all fields" )
    _fieldCheck = SyslogMMAPOperator::ALL;
  else if ( _fieldString == "program" )
    _fieldCheck = SyslogMMAPOperator::PROGRAM;
  else if ( _fieldString == "log name" )
    _fieldCheck = SyslogMMAPOperator::LOGNAME;
  else if ( _fieldString == "ndc" )
    _fieldCheck = SyslogMMAPOperator::NDC;
  else if ( _fieldString == "message" )
    _fieldCheck = SyslogMMAPOperator::MESSAGE;
  else
    _fieldCheck = SyslogMMAPOperator::ALL; // default

  // It's parsed, now lets turn parts of it into usefulness
  _mjdOffset = ::strtod( _mjdOffsetString.c_str(), NULL );
  if ( _mjdOffset != 0.0 )
    setWayBackMachine( _mjdOffset );

  _prioInt = (int)::strtol( _valString.c_str(), NULL, 10 );

  // TODO, proper error handling of strtod (make a util function!)

  _hasFilter = true;
}

void SyslogMMAPOperator::resetFilter()
{
  _hasFilter = false;
}

void SyslogMMAPOperator::resetWayBackMachine()
{
  _mjdOffset = 0.0;
  // reset current position...
  _pos = _smapf.getCurrentPos();
}

string SyslogMMAPOperator::toString()
{
  ostringstream oss;

  oss << "[mjd:"<<_mjdOffsetString<<",op:"<<_opString<<",val:"<<_valString<<",text:"<<_textString
    <<",field:"<<_fieldString<<"]";

  return string(oss.str());
}

