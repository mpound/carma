#include "carma/szautil/Exception.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/IoLock.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Thread.h"

#include <signal.h>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Thread constructor function
 */
Thread::Thread(THREAD_START(*startFn), THREAD_CLEAN(*cleanFn),
	       THREAD_PING(*pingFn), string name, unsigned startOrder, unsigned cancelOrder)
{
  privateConstructor(startFn, cleanFn, pingFn, name, startOrder, cancelOrder);
}

/**.......................................................................
 * Thread constructor function
 */
Thread::Thread(THREAD_START(*startFn), THREAD_CLEAN(*cleanFn),
	       THREAD_PING(*pingFn), string name)
{
  privateConstructor(startFn, cleanFn, pingFn, name, 0, 0);
}

/**.......................................................................
 * Thread constructor function
 */
void Thread::privateConstructor(THREAD_START(*startFn), THREAD_CLEAN(*cleanFn),
				THREAD_PING(*pingFn), string name, 
				unsigned startOrder, unsigned cancelOrder)
{
  wasError_ = false;

  startOrder_ = startOrder;
  cancelOrder_ = cancelOrder;

  // Sanity check arguments.

  if(startFn == 0)
    throw Error("Thread::Thread: No startup function supplied.\n");

  // Initialize the startup and shutdown functions of this thread

  userStartFn_  = startFn;
  userCleanFn_  = cleanFn;
  userPingFn_   = pingFn;

  startupArg_ = 0;

  // Initialize the name of this thread

  name_ = name;

  // Initialize the guard mutexes of this thread

  pthread_mutex_init(&ready_.guard, NULL);
  pthread_mutex_init(&done_.guard, NULL);
  pthread_mutex_init(&runningGuard_, NULL);

  // Initialize the ready_ condition variables for this thread

  pthread_cond_init(&ready_.cond, NULL);
  pthread_cond_init(&done_.cond, NULL);

  // We initialize this thread to block all (blockable) signals.  I
  // have confirmed that sigfillset() adds all signals up to SIGRTMAX
  // into the signal mask.

  //  sigset_t allSignals;
  //  sigfillset(&allSignals);
  //  pthread_sigmask(SIG_BLOCK, &allSignals, NULL);
    
  // Initialize the state of this thread to not running: this will be
  // set to true when the resources for this thread have been
  // allocated and the thread's message service is running

  running_ = false;
}

/**.......................................................................
 * Thread destructor
 */
Thread::~Thread()
{
  // Destroy the guard mutex and condition variables

  pthread_mutex_destroy(&ready_.guard);
  pthread_mutex_destroy(&done_.guard);
  pthread_mutex_destroy(&runningGuard_);

  pthread_cond_destroy(&ready_.cond);
  pthread_cond_destroy(&done_.cond);
}

/**.......................................................................
 * A public interface to setRunStatePrivate()
 */
void Thread::setRunState(bool state)
{
  if(setRunStatePrivate(state))
    throw Error("Thread::setRunState: Error in setRunStatePrivate()");
}

/**.......................................................................
 * Change the boolean variable that reflects the running state of this
 * thread.
 */
bool Thread::setRunStatePrivate(bool state)
{
  int oldtype;
  LogStream ls;

  // Before locking the mutex, we want to set pthread_mutex_unlock()
  // as a cleanup handler.  This guarantees that the mutex will be
  // unlocked even if this thread is cancelled before we reach the
  // next pthread_mutex_unlock() statement below.
  //
  // Note that if this thread was created with canceltype
  // PTHREAD_CANCEL_ASYNCHRONOUS, we need to switch to
  // PTHREAD_CANCEL_DEFERRED so that a cancellation cannot occur
  // between cleanup_push() and mutex_lock() below, resulting in the
  // cleanup handler trying to unlock a mutex that isn't locked by the
  // current thread

  INSTALL_MUTEX_CLEANUP(runningGuard_, ls);

  // The calling thread must lock the mutex before altering the
  // thread's running state; we don't want multiple threads to try and
  // change the state at the same time.

  if(pthread_mutex_lock(&runningGuard_) != 0)
    ls.appendSysError(true, "pthread_mutex_lock");

  // Now change the state of this Thread object

  running_ = state;

  // Release the guard mutex

  if(pthread_mutex_unlock(&runningGuard_) != 0)
    ls.appendSysError(true, "pthread_mutex_unlock");

  // Now remove the handler, but don't execute it, so we can test the
  // return value, below

  UNINSTALL_MUTEX_CLEANUP(ls);

  return ls.isError();
}

/**.......................................................................
 * Thread run function
 */
void Thread::start(void* arg) 
{
  bool waserr=false;
  int oldtype;
  LogStream ls;

  startupArg_ = arg;

  // We call pthread_create with a pointer to each thread's startup
  // function.  Each startup function initializes the subsystem and
  // sets the appropriate pointer of ant pointing to its resources.

  // The calling thread must lock the mutex before pthread_cond_wait()
  // can be called

  INSTALL_MUTEX_CLEANUP(ready_.guard, ls);

  // The calling thread must lock the mutex before altering the
  // thread's running state; we don't want multiple threads to try and
  // change the state at the same time.

  if(pthread_mutex_lock(&ready_.guard) != 0)
    ls.appendSysError(true, "pthread_mutex_lock");
  
  // Create the thread, with startup function pointed to by this
  // thread's startfn
  
  if(pthread_create(&id_, NULL, &startThread, this) != 0)
    ls.appendSysError(true, "pthread_create");
  
  // Suspend execution of the calling thread until the ready variable
  // is signalled.  This is kind of dangerous, in that it requires the
  // user startup function explicitly to call broadcastReady(), but it
  // allows the user to synchronize startup precisely.
  
  if(pthread_cond_wait(&ready_.cond, &ready_.guard) != 0)
    ls.appendSysError(true, "pthread_cond_wait");
  
  // And unlock the guard mutex
  
  if(pthread_mutex_unlock(&ready_.guard) != 0)
    ls.appendSysError(true, "pthread_mutex_unlock");
  
  // Now remove the handler, but don't execute it.
  
  UNINSTALL_MUTEX_CLEANUP(ls);

  if(ls.isError()) {
    ThrowError("Error starting thread " 
	       << id_ << " (" << name_ << "): " << ls);
  }
};

/**.......................................................................
 * Thread cancellation function.  It is expected that the exiting
 * thread will call a function established by pthread_cleanup_push()
 * which will signal ready when it is done.
 */
void Thread::cancel()
{
  bool waserr=false;
  int oldtype;
  LogStream ls;

  // Only proceed if the thread is currently running.  isRunning()
  // will return false if cancel has already been called on this
  // thread

  if(!isRunning())
    return;
  
  // Before locking the mutex, we want to set pthread_mutex_unlock()
  // as a cleanup handler.  This guarantees that the mutex will be
  // unlocked even if this thread is cancelled before we reach the
  // next pthread_mutex_unlock() statement below.
  //
  // Note that if this thread was created with canceltype
  // PTHREAD_CANCEL_ASYNCHRONOUS, we need to switch to
  // PTHREAD_CANCEL_DEFERRED so that a cancellation cannot occur
  // between cleanup_push() and mutex_lock() below, resulting in the
  // cleanup handler trying to unlock a mutex that isn't locked by the
  // current thread

  // Push cleanup handler onto the stack to unlock the mutex in case
  // we are cancelled while the mutex is locked.

  INSTALL_MUTEX_CLEANUP(done_.guard, ls);

  if(pthread_mutex_lock(&done_.guard) != 0)
    ls.appendSysError(true, "pthread_mutex_lock");

  
  DBPRINT(true, Debug::DEBUG7, "Cancelling thread id: " << id_ 
	  << " (" << name_ << ")");

  if(pthread_cancel(id_) != 0)
    ls.appendSysError(true, "pthread_cancel");

  DBPRINT(true, Debug::DEBUG7, "About to wait on done cond var");

  // Suspend execution of the calling thread until it is signalled
  // that the cancellation function is done.

  if(pthread_cond_wait(&done_.cond, &done_.guard) != 0)
    ls.appendSysError(true, "pthread_cond_wait");

  DBPRINT(true, Debug::DEBUG7, "Signalled done");

  // Set our running state to false

  if(setRunStatePrivate(false))
     ls.appendMessage(true, "Error in setRunStatePrivate");

  // Unlock the guard mutex

  if(pthread_mutex_unlock(&done_.guard) != 0)
    ls.appendSysError(true, "pthread_mutex_unlock");

  // And remove the handler, but don't execute it!

  UNINSTALL_MUTEX_CLEANUP(ls);

  if(ls.isError())
    throw Error(ls);
}

/**.......................................................................
 * Thread ping function
 */
void Thread::ping(void* arg) 
{
  if(isPingable())  // Don't ping if no ping method was passed
    (*userPingFn_)(arg);
}

/**.......................................................................
 * Return true if this thread is running
 */
bool Thread::isRunning()
{
  return running_;
}

/**.......................................................................
 * Broadcast to any waiting threads that we are ready
 */
void Thread::broadcastReady()
{
  bool waserr=false;
  int oldtype;
  LogStream ls;

  // Before locking the mutex, we want to set pthread_mutex_unlock()
  // as a cleanup handler.  This guarantees that the mutex will be
  // unlocked even if this thread is cancelled before we reach the
  // next pthread_mutex_unlock() statement below.
  //
  // Note that if this thread was created with canceltype
  // PTHREAD_CANCEL_ASYNCHRONOUS, we need to switch to
  // PTHREAD_CANCEL_DEFERRED so that a cancellation cannot occur
  // between cleanup_push() and mutex_lock() below, resulting in the
  // cleanup handler trying to unlock a mutex that isn't locked by the
  // current thread

   INSTALL_MUTEX_CLEANUP(ready_.guard, ls);

   // Now lock the mutex.  We have to explicitly lock here, to avoid a
   // race condition in which we broadcast ready before another thread
   // begins to wait, which will cause the other thread to wait
   // forever.  The model here is that a thread waiting on this
   // condition variable locks the mutex first, and doesn't release it
   // until pthread_cond_wait() gets called.  This means that the
   // following line will cause us to block until the other thread is
   // actually waiting.

   if(pthread_mutex_lock(&ready_.guard) != 0)
     ls.appendSysError(true, "pthread_mutex_lock");

   // The meat of the function -- just broadcast to threads waiting on
   // this condition variable.
   
   if(pthread_cond_broadcast(&ready_.cond) != 0)
     ls.appendSysError(true, "pthread_cond_broadcast");   

   // Unlock the mutex.
   
   if(pthread_mutex_unlock(&ready_.guard) != 0)
      ls.appendSysError(true, "pthread_mutex_unlock");

   // And remove the handler, but don't execute it!
   
   UNINSTALL_MUTEX_CLEANUP(ls);
   
   if(ls.isError())
     throw Error(ls);
}

/**.......................................................................
 * Broadcast to any waiting threads that we are done
 */
void Thread::broadcastDone()
{
  if(pthread_cond_broadcast(&done_.cond) != 0)
    throw Error("Error in pthread_cond_broadcast()");
}

/**.......................................................................
 * Return true if the passed name matches
 */
bool Thread::matchName(string compname)
{
  return (name_ == compname);
}

/**.......................................................................
 * Return the name string of this thread
 */
string Thread::strName()
{
  return name_;
}

//-----------------------------------------------------------------------
// Static definitions

/**.......................................................................
 * Define a wrapper around pthread_mutex_unlock() suitable for passing to
 * pthread_cleanup_push(), which expects a void (*fn)(void* arg)
 */
THREAD_CLEAN(Thread::unlockMutex)
{
  pthread_mutex_t* mut = (pthread_mutex_t*) arg;

  // Ignore the return value for now -- Anyway, can we throw an
  // exception in a cleanup handler??

  pthread_mutex_unlock(mut);
}

/**.......................................................................
 * Return true if a ping function has been installed for this thread.
 */
bool Thread::isPingable()
{
  return (userPingFn_ != 0);
}

/**.......................................................................
 * Raise a signal to this thread.
 */
void Thread::raise(int sigNo)
{
  pthread_kill(id_, sigNo);
}

/**.......................................................................
 * Wrapper around the user-supplied startup function.  This is the
 * method which is actually passed to pthread_create().  
*/
THREAD_START(Thread::startThread)
{
  Thread* thread = (Thread*) arg;
  LogStream errStr;

  // Set up this thread to block all signals

  sigset_t allSignals;
  sigfillset(&allSignals);
  pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

  // Set the cancel type to asynchronous, which means that the thread
  // will immediately call its cleanup handlers and exit when
  // cancelled.

  pthread_setcancelstate(PTHREAD_CANCEL_ENABLE, NULL);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS, NULL);

  // Install a cleanup handler to be called on thread cancellation.
  //
  // We have to enclose pthread_cleanup_push() and
  // pthread_cleanup_pop() in the same block, at the same level of
  // nesting, since they are macros which introduce and close,
  // respectively, a pair of {} braces.

  pthread_cleanup_push(&cleanThread, arg);

  // Now call the user-supplied startup function, catching any
  // exceptions that may be thrown
  
  try {
    thread->setRunState(true);
    thread->userStartFn_(thread->startupArg_);
  } catch(Exception& err) {
    errStr.initMessage(true);
    errStr << "Caught an exception in thread: "
	   << thread->name_ << " (" 
	   << thread->id_ << "): "
	   << err.what();
    errStr.report();
    thread->wasError_ = true;
  } catch(...) {
    errStr.initMessage(true);
    errStr << "Caught an exception in thread: "
	   << thread->name_ << " (" 
	   << thread->id_ << "): "
	   << "unknown exception";
    errStr.report();
    thread->wasError_ = true;
  }
  
  // Remove and execute our cleanup handler.
    
  pthread_cleanup_pop(1);
}

/**.......................................................................
 * A wrapper around the user-supplied cleanup function.  This is the
 * method which is actually passed to pthread_cleanup_push().  
*/
THREAD_CLEAN(Thread::cleanThread)
{

  Thread* thread = (Thread*) arg;

  try {

    if(thread->userCleanFn_ != 0)
      thread->userCleanFn_(thread->startupArg_);

    // In case we were called because of an error during startup,
    // broadcast to other threads that we are ready, else the calling
    // thread will block forever.

    thread->broadcastReady();

    // Broadcast to other threads that we are done

    thread->broadcastDone();

    // Set the running state to false so that subsequent calls to
    // cancel on this thread will do nothing.

    thread->setRunState(false);

  } catch(...) {

    CERR("Caught an exception in Thread::cleanThread "
	 << "(thread: " << thread->name_ << ", (" 
	 << thread->id_ << ").");
  }
}

void Thread::waitUntilReady()
{
  // Suspend execution of the calling thread until the ready variable
  // is signalled.  
  
  if(pthread_cond_wait(&ready_.cond, &ready_.guard) != 0)
    ThrowError("A test");
}
