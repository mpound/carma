#include "carma/szautil/TimeOut.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/Logger.h"

#include<iostream>

using namespace std;

using namespace sza::util;

// Constructor which defaults to an interval, and remains inactive

TimeOut::TimeOut() {
  setIntervalInSeconds(defaultInterval_);
  activate(false);
  resetPending_ = true;
}

/**.......................................................................
 * Set the timeout, in seconds
 */
void TimeOut::setIntervalInSeconds(unsigned int seconds) {
  timeOut_.setTime(seconds,0);
  resetPending_ = true;
}

void TimeOut::setInterval(unsigned int seconds, unsigned int nanoSeconds) {
  timeOut_.setTime(seconds, nanoSeconds);
  resetPending_ = true;
}

/**.......................................................................
 * Activate the timeout
 */
void TimeOut::activate(bool active) 
{
  active_ = active;
}


/**.......................................................................
 * Return a pointer suitable for use in select()
 */
struct timeval* TimeOut::tVal() {
  
  if(active_) {
    
    // If the pending flag was set, reset the timeout now
    
    if(resetPending_) {
      timeOut_.reset();
      resetPending_ = false;
    }

    return timeOut_.timeVal();
    
  } else {
    return NULL;
  }
}

/**.......................................................................
 * Reset the timeout
 */
void TimeOut::reset() 
{
  timeOut_.reset();
}

