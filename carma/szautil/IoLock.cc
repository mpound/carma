#include <iostream>

#include "carma/szautil/Debug.h"
#include "carma/szautil/IoLock.h"

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
IoLock::IoLock() {}

// Initialize static variables

Mutex IoLock::coutMutex_;
Mutex IoLock::cerrMutex_;

/**.......................................................................
 * Destructor.
 */
IoLock::~IoLock() {}

void IoLock::lockCout()
{
  coutMutex_.lock();
}

void IoLock::unlockCout()
{
  coutMutex_.unlock();
}

void IoLock::lockCerr()
{
  cerrMutex_.lock();
}

void IoLock::unlockCerr()
{
  cerrMutex_.unlock();
}
