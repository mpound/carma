

#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"
#include "carma/antenna/bima/SemaphoreOperator.h"

#include <iostream>

#include <unistd.h>

using namespace std;
using namespace carma::util;
using namespace carma::antenna::bima;

const key_t SemaphoreOperator::_key[4] = { 0xCEADA4F1, 0xCEADA4F2, 0xCEADA4F3, 0xCEADA4F4 };

SemaphoreOperator::SemaphoreOperator( bool server )
{
  int flags = 0777;
  ostringstream errMsg;

  _staleCheck = 0;

  // Check if we're running as the server...
  // i.e. bimaTelemetryHost
  for ( int i = 0; i < 4; i++ )
  {
    _semid[i] = semget( _key[i], MAX_SEMS, flags );

    if ( server == true && _semid[i] == -1 ) // most likely the sems don't exist...
    {                                        // and we're the server...
      int cflags = flags | IPC_CREAT;

      _semid[i] = semget( _key[i], MAX_SEMS, cflags );

      if ( _semid[i] == -1 )
      {
        errMsg << "Unable to acquire id to semaphore set: "
                     << strerror( errno );
        throw CARMA_ERROR( errMsg.str() );
      }
    }
    else if ( _semid[i] == -1 )
    {
      errMsg << "Has bimaTelemetryHost been run? "
	     << "Unable to open semaphore set: "
	     << strerror( errno ) ;
      throw CARMA_ERROR( errMsg.str() );
    }
  }
}

void SemaphoreOperator::addNameAndId( const string & name, unsigned short id )
{
  CPTRACE( Trace::TRACE6, "name: " << name << " sem id: " << id );
  _nameToSemOpId[name] = id; 
}

void SemaphoreOperator::update( const string & name )
{
  int semid, id;
  unsigned short semnum;
  int waiters;
  ostringstream errMsg;

  CPTRACE( Trace::TRACE6, "Looking up semOp for name: " << name );

  id = _nameToSemOpId[name];

  semOpIdToSemIdAndNum( id, semid, semnum );

  CPTRACE( Trace::TRACE5,
           "update, sem name: "
           << name << " - id: " << id
           << " semid: " << semid << " semnum: " << semnum );

  // First, find out how many processes are waiting for
  // for an update...
  waiters = semctl( semid, semnum, GETNCNT, _arg );
 
  if ( waiters == -1 )
  {
    errMsg << "Unable to get process count on semid: "
           << semid << " semnum: " << semnum
           << " - " << strerror(errno);
    throw CARMA_ERROR( errMsg.str() );
  }
                  
  if ( waiters > 0 )
  {
    // Now, set the semaphore up value so that those
    // processes can stop waiting...
    _arg.val = waiters;
    CPTRACE( Trace::TRACE5,
             "Updating " << name << " " << waiters << " processes waiting" );

    if ( semctl( semid, semnum, SETVAL, _arg ) < 0 )
    {
      errMsg << "Unable to set count to " << waiters << " on semid: "
             << semid << " semnum: " << semnum
             << " - " << strerror(errno);
  
      throw CARMA_ERROR( errMsg.str() );
    }
  }
  else
  {
    // This means we got a message back before the other process actually
    // started waiting.  Therefore, we should mark this update as one
    // that will lead to a stale semaphore, locking another process.
    // This list of stale sems is checked every 100 messages to make sure
    // as soon as we see a process waiting for the semaphore, we
    // mark it as read.
 
    if ( _staleList.find( name ) != _staleList.end() )
      _staleList[name] = _staleList[name] + 1;
    else
      _staleList[name] = 0;
  }

  // Et Voila!
}

void SemaphoreOperator::waitOn( const string & name )
{
  int semid, id;
  unsigned short semnum;
  struct sembuf op;

//cout << "waitOn: " << *name << endl;
//cout.flush();

  id = _nameToSemOpId[name];

//cout << "id: " << id << endl; cout.flush();

  semOpIdToSemIdAndNum( id, semid, semnum );

  CPTRACE( Trace::TRACE5,
           "waitOn, sem name: "
           << name << " - id: " << id
           << " semid: " << semid << " semnum: " << semnum );
//cout << "name: " << *name << " id: " << id << " semid: " << semid << " semnum: " << semnum << endl;
//cout.flush();

  op.sem_num = semnum;
  op.sem_op = -1; 
  op.sem_flg = 0;
  
  // Wait for semaphore to increase..
  int semstat;

// If HAVE_SEMTIMEDOP is not defined, this means a reconfig has not happened
// and it is better to assume we don't have it rather than error when we
// try to compile with it...
#ifndef HAVE_SEMTIMEDOP
#define HAVE_SEMTIMEDOP 0
#endif

#if (HAVE_SEMTIMEDOP > 0)
  struct timespec timeout;
  timeout.tv_sec = 1; // always timeout after 1 second, no reason to wait longer
  timeout.tv_nsec = 0;
  semstat = semtimedop( semid, &op, 1, &timeout );
#else
  semstat = semop( semid, &op, 1 );
#endif

  if ( semstat < 0 && semstat != EAGAIN )
  {
    ostringstream errMsg;
    errMsg << "Unable to decrement/wait on semid: "
           << semid << " semnum: " << semnum
           << " - " << strerror(errno);
    throw CARMA_ERROR( string( errMsg.str() ) );
  }

  CPTRACE( Trace::TRACE5, "Got update of memlocation: " << name );

  // Et Voila!
}

// Really only used for debugging purposes.
unsigned short SemaphoreOperator::getSemNum ( const string & name )
{
  return _nameToSemOpId[name];
}


void SemaphoreOperator::semOpIdToSemIdAndNum( int semOpId,
			   int &semid,
			   unsigned short &semnum )
{
  int i = 0;

  semnum = semOpId;

  while ( semnum > MAX_SEMS - 1 ) // 250 semnums per semid by default...
  {
    semnum = semOpId - MAX_SEMS;
    i++;
  }

  semid = _semid[i];
}

void SemaphoreOperator::updateStale()
{
  
  if ( _staleCheck % 100 == 0 && _staleList.size() > 0 )
  {
    for ( map<string,int>::iterator i = _staleList.begin(); i != _staleList.end(); ++i )
    {
      CPTRACE( Trace::TRACE7, "Stale Semaphore found: " << i->first );

      if ( i->second < 50 )
      {
        update( i->first );
      }
      else
      {
        CPTRACE( Trace::TRACE6, "Stale semaphore has exceeded update 50 retries, erasing" );
        _staleList.erase( i->first );
      }
    }
  }
  _staleCheck++;
}

