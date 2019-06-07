#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Mutex.h"
#include "carma/szautil/MutexException.h"

#include <errno.h>
#include <cstring>

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Mutex::Mutex() 
{
  LogStream logStr;
  //  pthread_mutexattr_t attributes;

  // (void) pthread_mutexattr_init(&attributes);

  // Initialize this mutex to be recursive.  Note that this is a
  // non-portable extension, supported under Linux.  This is so that a
  // calling thread can lock the same mutex more than once.  On a
  // recursive mutex, unlock must be called as many times as lock has
  // been called before the mutex is restored to its free state.

  //  if(pthread_mutexattr_settype(&attributes, 
  //			       PTHREAD_MUTEX_RECURSIVE_NP) != 0) 
  //  {
  //    logStr.appendSysError(true, "pthread_mutexattr_settype()");
  //    throw Error(logStr);
  //  }

  who_ = 0;

  mutexIsReady_ = false;
  
  //  if(pthread_mutex_init(&mutex_, &attributes)) {
  if(pthread_mutex_init(&mutex_, NULL)) {
    ThrowSysError("pthread_mutex_init()");
  };
  
  mutexIsReady_ = true;
}

/**.......................................................................
 * Destructor.
 */
Mutex::~Mutex() 
{
  int status;

  unlock();

  if(mutexIsReady_) {
    errno=0;
    if((status=pthread_mutex_destroy(&mutex_)) != 0) {
      if(errno != 0) {
	LogStream errStr;
	errStr.appendSysError(true, "pthread_mutex_destroy()");
	errStr.report(); // Don't throw errors in destructors
      }
    }
    mutexIsReady_ = false;
  }
}

/**.......................................................................
 * Lock the mutex.
 */
void Mutex::lock()
{
  if(mutexIsReady_) {

    // If the mutex is already locked by us, don't try to lock it again!

    if(who_ == pthread_self())
      return;

    // Comment this out for now.  This is causing an interrupted
    // system call in trylock, which I don't fully understand.  For
    // the time being, I have set the mutex type to be recursive to
    // avoid deadlocking if the thread already owning the mutex tries
    // to lock it again.
    //
    //    if(isLocked() && isItMe()) {
    //      throw MutexException();
    //    }

    if(pthread_mutex_lock(&mutex_)) {
      ThrowSysError("pthread_mutex_lock");
    }

    // And record the identity of the locking thread

    who_ = pthread_self();
  }
}

/**.......................................................................
 * Unlock the mutex.
 */
void Mutex::unlock()
{
  LogStream errStr;

  if(mutexIsReady_) {

    // Reset who_ indicating that no-one currently has the lock.  This
    // is ok, because any threads which might modify this in lock()
    // will be blocked until the call to pthread_mutex_unlock(), and
    // it cannot cause this thread to deadlock because this thread
    // won't do anything else until it has exited this method call

    who_ = 0;

    if(pthread_mutex_unlock(&mutex_)) {
      ThrowSysError("pthread_mutex_unlock");
    }
  }
}

/**.......................................................................
 * Get a lock if the mutex is available.  Return true if the lock was
 * successful
 */
bool Mutex::tryLock()
{
  if(mutexIsReady_) {
    if(pthread_mutex_trylock(&mutex_)) {

      COUT(strerror(errno));

      if(errno != EBUSY) {
	ThrowSysError("pthread_mutex_trylock");
      } else // mutex is busy
	return false;
    } else // trylock() returned ok.  mutex is locked
      return true;
  }
  return false;
}

/**.......................................................................
 * Return true if the mutex is locked.
 */
bool Mutex::isLocked()
{
  if(mutexIsReady_) {
    if(pthread_mutex_trylock(&mutex_)) {
      if(errno != EBUSY) {
	ThrowSysError("pthread_mutex_trylock");
      } else // mutex is busy
	return true;
    } else {// trylock() returned ok.  mutex is locked
      unlock();
      return false;
    }
  }
  return false;
}

/**.......................................................................
 * Return true if the calling thread is the one already locking this mutex.
 */
bool Mutex::isItMe()
{
  // Since I'm not sure how to initialize a pthread_t variable, we
  // should just return false if the mutex isn't locked.  Otherwise we
  // might return true if the uninitialized pthread_t happens to match
  // our id, or if we have previously locked this mutex and no-one
  // else has in the meantime.

  if(!isLocked())
    return false;
  else {
    pthread_t who = pthread_self();
    return pthread_equal(who, who_)==1;
  }
}
