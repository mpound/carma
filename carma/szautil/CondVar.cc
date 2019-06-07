#include "carma/szautil/CondVar.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"

#include <errno.h>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
CondVar::CondVar() 
{
  errno = 0;

  if(pthread_cond_init(&cond_, 0) != 0) {
    ThrowSysError("initializing cond var");
  }
}

void CondVar::lock()
{
  mutex_.lock();
}

void CondVar::wait()
{
  errno = 0;

  // pthread_cond_wait() requires a locked mutex to be passed in with
  // the condition variable.  The system will release the lock on the
  // caller's behalf when the wait begins

  mutex_.lock();

  if(pthread_cond_wait(&cond_, mutex_.getPthreadVarPtr()) != 0) {
    ThrowSysError("Waiting");
  }
}

void CondVar::waitNoLock()
{
  errno = 0;

  // pthread_cond_wait() requires a locked mutex to be passed in with
  // the condition variable.  The system will release the lock on the
  // caller's behalf when the wait begins

  if(pthread_cond_wait(&cond_, mutex_.getPthreadVarPtr()) != 0) {
    ThrowSysError("Broadcasting");
  }
}

void CondVar::broadcast()
{
  errno = 0;

  // Lock the mutex before calling broadcast.  This ensures that the
  // broadcast is not called before a thread is waiting for it

  mutex_.lock();

  if(pthread_cond_broadcast(&cond_) != 0) {
    mutex_.unlock();
    ThrowSysError("Broadcasting");
  }

  mutex_.unlock();
}

/**.......................................................................
 * Destructor.
 */
CondVar::~CondVar() 
{
  errno = 0;
  if(pthread_cond_destroy(&cond_) != 0) {
    ReportSysError("Destroying cond var");
  }
}
