//
// @file        SyslogListenerThread.cc
// @version     $Id: SyslogListenerThread.cc,v 1.4 2011/12/21 22:56:43 mpound Exp $
// @author      Colby Gutierrez-Kraybill
//
// @description
//


// System includes
//#include <string>
#include <errno.h>
#include <unistd.h>
#include <climits>

// C++ includes
#include <iostream>
#include <sstream>

// CARMA tools includes
#include "log4cpp/Category.hh"

// CARMA software includes
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ScopedPthreadMutexLock.h"

// CARMA includes specific to this file
#include "carma/dbms/SyslogListenerThread.h"

using namespace std;
using namespace carma::util;
using namespace carma::dbms;


SyslogListenerThread::SyslogListenerThread(
    string pipeFileName,
    queue<string * > *rawBufferQueue,
    PthreadMutex *queueGuard,
    PthreadMutex *readyGuard,
    PthreadCond *readyCond)
: _log( Program::getLogger() ),
  _pipeFileName( pipeFileName ),
  _rawBufferQueue( rawBufferQueue ),
  _queueGuard( queueGuard ),
  _readyGuard( readyGuard ),
  _readyCond( readyCond )
{
  CPTRACE( Trace::TRACE3, "   SyslogListenerThread c'tor start" );
  CPTRACE( Trace::TRACE4, "    c'tor opening: " << _pipeFileName );

  if ( (_pipeFD = open( _pipeFileName.c_str(), O_RDONLY )) < 0 ) 
  {
    ostringstream oss;
    oss << "Unable to open " << _pipeFileName << ": " << strerror( errno );
    CPTRACE( Trace::TRACE3, "    c'tor error: " << oss.str() );

    throw CARMA_ERROR( oss.str().c_str() );
  }
  CPTRACE( Trace::TRACE4, "    c'tor fifo opened" );


  // get the filename... this assumes / is the file system path separator...
  string::size_type n = _pipeFileName.find_last_of( '/' );
  if ( n == string::npos )
    n = 0;
  else
    n++;

  // This can remain npos if that's what happens
  string::size_type p = _pipeFileName.find_last_of( '.' );
  string::size_type l = _pipeFileName.length();

  _logName = _pipeFileName.substr( n, l - n - (l - p) );
  CPTRACE( Trace::TRACE4, "    _logName: " << _logName );

  CPTRACE( Trace::TRACE3, "   SyslogListenerThread c'tor end" );
}


void SyslogListenerThread::run()
{
  _timeOfStart = Time::MJD();
  _messageCountFromStartOfRun = 0L;
  _totalBytesReceived = 0L;

  ssize_t bytesReceived;
  char rawBuffer[(LINE_MAX*10)+1];
  int rawBufferPlace = 0;

  try
  {

    while ( ( bytesReceived = read( _pipeFD, rawBuffer+rawBufferPlace,
	    ((LINE_MAX*10)-rawBufferPlace) ) ) > -1 )
    {

      _totalBytesReceived += bytesReceived;
      _messageCountFromStartOfRun++;

      // Last character read not being 10 is an indication of end of line not
      // having an atomic syslog line
      // In this case, we should tack on this most recent read to the end of
      // the full buffer, then, go through the buffer, producing individual lines
      // for anyone paying attention to the SyslogListener
      if ( rawBuffer[bytesReceived-1] != 10 )
      {
	if ( rawBufferPlace+bytesReceived < (LINE_MAX*10) )
	{
	  rawBufferPlace = rawBufferPlace+bytesReceived;
	}
	else
	{
	  ScopedPthreadMutexLock scopelock( *_queueGuard );
	  _rawBufferQueue->push(
	      new string( _logName + 
  string(" Jan 31 00:00:00 acc syslog2carmalog: {9999} {ERROR} {} {Read buffer overflow!}\10"
	      )));
	  rawBufferPlace = 0;
	}
      }
      else
      {
	rawBuffer[bytesReceived] = '\0';
	rawBufferPlace = 0;
	// Now break up the rawBuffer into new strings
	// Guard the buffer queue with the lock
	string::size_type s = 0, n = 0;
	string *rawBufferCopy = new string( rawBuffer );
	{
	  {
	    ScopedPthreadMutexLock scopelock( *_queueGuard );

	    while ( (s = rawBufferCopy->find_first_of( (char)10, n )) != string::npos )
	    {
	      s++;
	      _rawBufferQueue->push( new string(_logName
		    + string(" ") + rawBufferCopy->substr( n, s - n - 1 )));
	      n = s;
	    }
	  } // scopedlock

	  ready();
	}

	delete rawBufferCopy; // free up space.
      } // if rawBuffer[bytesReceived-1] != 10 else

    } // while read()

    if ( bytesReceived == -1 )
    {
      ostringstream oss;
      oss << "Error reading from pipe: " << strerror( errno );
      throw std::runtime_error( oss.str().c_str() );
    }
  }
  catch ( ErrorException &eex )
  {
  }

} // void run


void SyslogListenerThread::thread( SyslogListenerThread & This )
{
  programLogInfoIfPossible( "Calling SyslogListenerThread::run" );

  This.run();

  programLogInfoIfPossible( "SyslogListenerThread::run completed" );
}



void SyslogListenerThread::stop()
{
}

void SyslogListenerThread::ready()
{
  _readyCond->Signal();
}

