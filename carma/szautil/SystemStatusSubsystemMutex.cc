#include "carma/szautil/SystemStatusSubsystemMutex.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
SystemStatusSubsystemMutex::SystemStatusSubsystemMutex() 
{
  ss_ = 0;
  ss_ = new carma::monitor::SystemStatusSubsystem();
}

/**.......................................................................
 * Destructor.
 */
SystemStatusSubsystemMutex::~SystemStatusSubsystemMutex() 
{
  if(ss_) {
    delete ss_;
    ss_ = 0;
  }
}

void SystemStatusSubsystemMutex::lock()
{
  guard_.lock();
}

void SystemStatusSubsystemMutex::unlock()
{
  guard_.unlock();
}
