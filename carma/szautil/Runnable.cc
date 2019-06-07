#include "carma/szautil/Runnable.h"

#include <csignal>
#include <sys/select.h>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Runnable::Runnable(bool spawnThread, RUN_FN(*runFn))
{
  spawnedThread_ = 0;

  // If spawning in a separate thread, start up now.

  if(spawnThread) {

    // Block all signals but SIGINT in the calling thread

    sigset_t allSignals;
    sigfillset(&allSignals);
    sigdelset(&allSignals, SIGINT);
    pthread_sigmask(SIG_BLOCK, &allSignals, NULL);
    
    // Set up the function to be called

    runFn_ = runFn;

    // And start up a new thread in which this object will run
    //
    // Note: the thread won't be started until spawn() is called

    spawnedThread_ = new Thread(startUp, 0, 0, "");
  }
}

/**.......................................................................
 * Destructor.
 */
Runnable::~Runnable() 
{
  if(spawnedThread_ != 0) {

    // Thread destructor will call cancel on this thread

    delete spawnedThread_;
    spawnedThread_ = 0;
  }
}

THREAD_START(Runnable::startUp)
{
  Runnable* runnable = (Runnable*)arg;
  runnable->spawnedThread_->broadcastReady();
  runnable->runFn_(arg);
}

void Runnable::spawn(void* arg)
{
  if(spawnedThread_ != 0)
    spawnedThread_->start(arg);
}

void Runnable::spawn()
{
  spawn(this);
}

void Runnable::blockForever()
{
  while(true) 
    select(0, NULL, NULL, NULL, NULL);
}
