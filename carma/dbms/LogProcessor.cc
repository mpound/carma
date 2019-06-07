
#include "carma/dbms/LogProcessor.h"
#include "carma/dbms/SyslogMessage.h"
#include "carma/dbms/MalformedSyslogLineException.h"
#include "carma/dbms/Syslog2DBMSConversions.h"
#include "carma/util/EOFException.h"
#include "carma/util/FileUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/IllegalStateException.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Program.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Logger.h"
#include "carma/util/Trace.h"

#include <fstream>
#include <iostream>
#include <sstream>
#include <stdarg.h>
#include <cstdlib>
#include <sys/types.h>
#include <unistd.h>

using namespace ::std;
using namespace carma;
using namespace carma::dbms;
using namespace carma::util;


LogProcessor::LogProcessor()
{
}

LogProcessor::~LogProcessor() 
{
    // stream is closed by ofstream's destructor which
    // is called when it goes out of scope.
}


SyslogMessage *LogProcessor::fromMMAP( SyslogMMAPFile &smapf, unsigned char *ptr )
{
  SyslogMessage *msg = new SyslogMessage();
  double *dateMJD;
  
  dateMJD = (double *)ptr;
  CPTRACE( Trace::TRACE4, "  fromMMAP.dateMJD: " << *dateMJD );

  size_t len;
  len = msg->setDate( *dateMJD );
  ptr += len;
  len = msg->setLog( (char *)ptr );
  CPTRACE( Trace::TRACE4, "  fromMMAP.log: " << ptr );
  ptr += len;
  len = msg->setHost( (char *)ptr );
  CPTRACE( Trace::TRACE4, "  fromMMAP.host: " << ptr );
  ptr += len;
  len = msg->setProgram( (char *)ptr );
  CPTRACE( Trace::TRACE4, "  fromMMAP.program: " << ptr );
  ptr += len;
  len = msg->setPrio( (char *)ptr );
  CPTRACE( Trace::TRACE4, "  fromMMAP.prio: " << ptr );
  ptr += len;
  len = msg->setFullyQualifiedProgramName( (char *)ptr );
  CPTRACE( Trace::TRACE4, "  fromMMAP.fqpn: " << ptr );
  ptr += len;
  len = msg->setThreadInfo( (char *)ptr );
  CPTRACE( Trace::TRACE4, "  fromMMAP.thread: " << ptr );
  ptr += len;
//  cout << "  p:{" << ptr << "}" << endl;
  len = msg->setMessage( (char *)ptr );
  CPTRACE( Trace::TRACE4, "  fromMMAP.msg: " << ptr );
  
  return msg;
}

SyslogMessage *LogProcessor::fromString( string *aMessage )
{
  SyslogMessage *msg = new SyslogMessage();
  double dateMJD;

  dateMJD = 0.0;

  // Hmmmm yeah, this is not all that nice.
  decomposeSyslogMessage( aMessage, msg );

  return msg;
}

void LogProcessor::decomposeSyslogMessage( string *aMessage,
    SyslogMessage *msg )
{
  int i, holder, tot;
  string::size_type lmr = string::npos; // used to detect last message repeated
  string::size_type yearStart = 0;

  char dateStr[SyslogMessage::S_STD_LEN+1];
  CPTRACE( Trace::TRACE3, "  decomposing: '" << *aMessage << "'" );

  
  string::size_type logNameEnd = aMessage->find_first_of( ' ' );
  if ( logNameEnd == string::npos )
  {
    // OH NOOOOOES
    throw CARMA_EXCEPTION( MalformedSyslogLineException, "no spaces!");
  }
  msg->setLog( (char *)aMessage->substr( 0, logNameEnd ).c_str() );
  logNameEnd++;

  // Now drop the logName so that we can carry on
  string noLogNameMessage = aMessage->substr( logNameEnd );

  int hostStart, hostEnd, programEnd;
  getHostBounds( &noLogNameMessage, hostStart, hostEnd );
  msg->setHost( (char *)noLogNameMessage.substr( hostStart, hostEnd-hostStart ).c_str() );

  CPTRACE( Trace::TRACE4, "  host: '" << msg->getHost() << "'" );

  // Now get the program
  string::size_type p = noLogNameMessage.find_first_of( ':', hostEnd );
  if ( p == string::npos )
  {
    CPTRACE( Trace::TRACE4, "   no program" );
    // huh, no program... weird, but okay!
    msg->setProgram( (char *)"unknown" );
    programEnd = hostEnd+1;
  }
  else
  {
    msg->setProgram( noLogNameMessage.substr( hostEnd+1, p - hostEnd - 1 ).c_str() );
    programEnd = p+1;
  }

  CPTRACE( Trace::TRACE4, "  program: '" << msg->getProgram() << "' " );

  yearStart = noLogNameMessage.find_first_of( '{', 0 );

  // Now, you might ask, why this much code to parse such a small string
  // I'll tell you why... To have it do something sensible if there's
  // a problem parsing the info.  Also, to construct a reasonable date
  // string, which syslog does not do on its own. (jerk)
  // also, it's fast.  Well, except for all this extra memsetting and
  // strncpy's... god I've gotta cut down on those.
  holder = i = tot = 0;
  // 
  // Make a copy of the full message sans logName at the start, e.g.
  // an exact copy of the raw message
  const char *buf = noLogNameMessage.c_str();

  // first copies the year into a more useful place
  if ( yearStart != string::npos )
  {
    yearStart++;
    while ( i < 4 )
    {
      if ( ! ::isalnum( buf[yearStart+i] ) )
      {
	// this captures NULL as well as odd characters
	throw CARMA_EXCEPTION( MalformedSyslogLineException, "year");
      }
      dateStr[i] = buf[yearStart+i];
      i++; tot++;
    }
  }
  else
  {
    // get the current year
    struct tm *tms;
    time_t tt = time(NULL);
    tms = gmtime(&tt);
    int year = tms->tm_year+1900;

    sprintf( dateStr, "%d", year );
    i = 4;
  }

  dateStr[i] = ' ';
  holder = i+1;

  // then copies the month and day and time, e.g. "Nov 14 00:00:00"
  i = 0;
  while ( i < 15 )
  {
    if ( ! ::isprint(buf[i]) && buf[i] != 10  )
    {
      // this captures NULL as well as odd characters
      throw CARMA_EXCEPTION( MalformedSyslogLineException, "date");
    }
    dateStr[holder+i] = buf[i];
    i++; tot++;
  }

  // We have the date supposedly, now to turn it into MJD
  // The format is the default, but unfortunately, the default
  // tz is "LOCAL"...
  dateStr[holder+i] = '\0';
  msg->setDate(
      Time::computeMJD( string( dateStr ),
	string( "%Y %b %d %H:%M:%S" ),
	Time::GMT ) );

  // Done with making sure the date string is setup properly.

  // lets get "last message repeated"'s and non-CARMA
  // conforming messages out of the way
  if ( yearStart == string::npos )
  {
    CPTRACE( Trace::TRACE4, "  Non-conformal log message" );
    // Either this is one of those "last message repeated" ones
    // or some other non-CARMA conforming message
    lmr = noLogNameMessage.find( "last message repeated", 0 );
    if ( lmr != string::npos )
    {
      CPTRACE( Trace::TRACE4, "   last message repeated!" );
      msg->setMessage( buf+hostEnd+1 );
    }
    else
    {
      CPTRACE( Trace::TRACE4, "   huh, some other message" );
      // Huh, it's some other message
      msg->setMessage( buf+programEnd+1 );
    }

    msg->setPrio( "INFO" );
    msg->setFullyQualifiedProgramName( "carma.dbms.syslog2carmalog" );
    msg->setThreadInfo( " " );

    CPTRACE( Trace::TRACE4, "   leaving decompose early with"
	<< " date:'" << msg->getDate() << "' program:'"
	<< msg->getProgram() << "' msg:'" << msg->getMessage() << "'" );

    return;
  } // last message repeat or non-CARMA conforming messages

  // Now move onto priority
  string::size_type prioStart = noLogNameMessage.find_first_of( '{', yearStart+4 );
  string::size_type prioEnd = noLogNameMessage.find_first_of( '}', prioStart );
  if ( prioStart == string::npos || prioEnd == string::npos )
  {
    throw CARMA_EXCEPTION( MalformedSyslogLineException, "priority");
  }
  else
  {
    prioStart++;

    msg->setPrio( noLogNameMessage.substr( prioStart, prioEnd - prioStart ).c_str() );
  }

  CPTRACE( Trace::TRACE4, "  prio: '" << msg->getCharPrio() << "'" );

  // Now move onto fqsysname
  string::size_type fqsStart = noLogNameMessage.find_first_of( '{', prioEnd );
  string::size_type fqsEnd = noLogNameMessage.find_first_of( '}', fqsStart );
  if ( fqsStart == string::npos || fqsEnd == string::npos )
  {
    throw CARMA_EXCEPTION( MalformedSyslogLineException, "fqs");
  }
  else
  {
    fqsStart++;

    msg->setFullyQualifiedProgramName(
	noLogNameMessage.substr( fqsStart, fqsEnd - fqsStart ).c_str() );
  }

  CPTRACE( Trace::TRACE4, "  fqs: '" << msg->getFullyQualifiedProgramName() << "'" );

  // Now move onto thread
  string::size_type threadStart = noLogNameMessage.find_first_of( '{', fqsEnd );
  string::size_type threadEnd = noLogNameMessage.find_first_of( '}', threadStart );
  if ( threadStart == string::npos || threadEnd == string::npos )
  {
    throw CARMA_EXCEPTION( MalformedSyslogLineException, "thread");
  }
  else
  {
    threadStart++;

    msg->setThreadInfo(
	noLogNameMessage.substr( threadStart, threadEnd - threadStart ).c_str() );
  }

  CPTRACE( Trace::TRACE4, "  thread: '" << msg->getThreadInfo() << "'" );

  // Now move onto THE message
  string::size_type msgStart = noLogNameMessage.find_first_of( '{', threadEnd );
  string::size_type msgEnd = noLogNameMessage.find_first_of( '}', msgStart );
  // Let's just assume the rest of the string is the message...
  if ( msgStart == string::npos )
    throw CARMA_EXCEPTION( MalformedSyslogLineException, "message");

  if ( msgEnd == string::npos )
    msgEnd = noLogNameMessage.length();

  msgStart++;
  msg->setMessage(
	noLogNameMessage.substr( msgStart, msgEnd - msgStart ).c_str() );

  CPTRACE( Trace::TRACE3, "  msg: '" << msg << "'" );

}

void LogProcessor::getHostBounds( string *message, int &start, int &end )
{
  // host info always starts on character 17 in current version of syslogd output
  start = 16;
  end = message->find_first_of( ' ', 17 );
  if ( (string::size_type)end == string::npos )
    end = (int)message->length()-1;
}
