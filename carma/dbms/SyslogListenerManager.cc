//
// @file        SyslogListenerManager.cc
// @version     $Id: SyslogListenerManager.cc,v 1.2 2007/11/28 07:57:53 colby Exp $
// @author      Colby Gutierrez-Kraybill
//
// @description
//


// System includes
#include <string.h>
#include <errno.h>
#include <unistd.h>

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
#include "carma/util/StartPthread.h"

// CARMA includes specific to this file
#include "carma/dbms/SyslogListenerManager.h"

using namespace ::std;
using namespace carma::util;
using namespace carma::dbms;

queue<string * > *SyslogListenerManager::_rawBufferQueue = (queue<string * > *)0;
PthreadMutex *SyslogListenerManager::_queueGuard = (PthreadMutex *)0;
PthreadMutex *SyslogListenerManager::_readyGuard = (PthreadMutex *)0;
PthreadCond *SyslogListenerManager::_readyCond = (PthreadCond *)0;


SyslogListenerManager::SyslogListenerManager( string pipeFileNames )
: _log( Program::getLogger() )
{
  CPTRACE( Trace::TRACE3, "   SyslogListenerManager c'tor start" );

  //** Instantiate mutexes, conditionals and the buffer queue
  _rawBufferQueue = new queue<string * >();
  _queueGuard = new PthreadMutex();
  _readyGuard = new PthreadMutex();
  _readyCond = new PthreadCond();

  //** tokenize FIFO/pipe filenames
  string::size_type comma = 0, distance;

  while ( (distance = pipeFileNames.find_first_of( ',', comma )) != string::npos )
  {
    _pipeFileNames.push_back( pipeFileNames.substr( comma, distance-comma ) );
    comma = distance+1;
  }

  _pipeFileNames.push_back( pipeFileNames.substr( comma ));


  //** Now instantiate listener threads
  CPTRACE( Trace::TRACE3, "    c'tor instantiating SyslogListenerThreads" );
  vector<string>::iterator name;
  for ( name = _pipeFileNames.begin(); name != _pipeFileNames.end(); ++name )
  {
    _listeners[*name] = new SyslogListenerThread(
	*name, _rawBufferQueue, _queueGuard, _readyGuard, _readyCond);

    // Someday we might wanna use the thread ID's to stop/start
    // reading on pipes, but can't think of immediate reason to right
    // now.
    StartPthreadWithRef(
	SyslogListenerThread::thread,
	*(_listeners[*name]), "hi" /*string( string("SLM::") + string(*name))*/ );
  }

  CPTRACE( Trace::TRACE3, "   c'tor end" );
}


void SyslogListenerManager::run()
{
  _timeOfStart = Time::MJD();

  try
  {
    while (1)
      sleep(1000000);
  }
  catch ( ErrorException &eex )
  {
  }

} // void run


void SyslogListenerManager::thread( SyslogListenerManager & This )
{
  programLogInfoIfPossible( "Calling SyslogListenerManager::run" );

  This.run();

  programLogInfoIfPossible( "SyslogListenerManager::run completed" );
}



void SyslogListenerManager::stop()
{
}


string *SyslogListenerManager::waitForNextMessage()
{
  string *msg = NULL;

  // First, check if there're are messages already
  // sitting in the buffer queue
  // if so, pop it
  {
    ScopedPthreadMutexLock scopelock( *_queueGuard );
    if ( _rawBufferQueue->size() > 0 )
    {
      msg = _rawBufferQueue->front();
      _rawBufferQueue->pop();
    }
  }

  // If a message was waiting in the queue already,
  // go ahead and immediately return it.
  // Otherwise block on the listener until there is
  // something in the queue
  if ( msg == NULL )
  {
    _readyGuard->Lock();
    _readyCond->Wait( *_readyGuard );
    _readyGuard->Unlock();

    {
      ScopedPthreadMutexLock scopelock( *_queueGuard );
      if ( _rawBufferQueue->size() > 0 )
      {
	msg = _rawBufferQueue->front();
	_rawBufferQueue->pop();
      }
    }
  }

  return ( msg );
}


