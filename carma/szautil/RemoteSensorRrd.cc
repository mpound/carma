#include "carma/szautil/Exception.h"
#include "carma/szautil/RemoteSensorRrd.h"
#include "carma/szautil/String.h"
#include "carma/szautil/Temperature.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructors.
 */
RemoteSensorRrd::RemoteSensorRrd(std::string host,
				 unsigned timeoutIntervalInSeconds) :
  RemoteSensorCoProc(host, timeoutIntervalInSeconds)
{
  mpSetTemp_ = 0;
}

RemoteSensorRrd::RemoteSensorRrd(std::string host, 
				 SystemStatusSubsystemMutex*            ms,
				 carma::monitor::MonitorPointBool&      mpIsReachable,
				 carma::monitor::MonitorPointDouble&    mpCurrentTemp,
				 carma::monitor::MonitorPointDouble&    mpSetTemp,
				 unsigned timeoutIntervalInSeconds) :
  RemoteSensorCoProc(host, ms, mpIsReachable, mpCurrentTemp, timeoutIntervalInSeconds)
{
  mpSetTemp_ = &mpSetTemp;
}

RemoteSensorRrd::RemoteSensorRrd(std::string host, 
				 SystemStatusSubsystemMutex*            ms,
				 carma::monitor::MonitorPointBool&      mpHostIsReachable,
				 carma::monitor::MonitorPointBool&      mpHostIsOk,
				 carma::monitor::MonitorPointBool&      mpPlaceIsOk,
				 carma::monitor::MonitorPointEnum&      mpPlaceStatus,
				 carma::monitor::MonitorPointAbstime&   mpSampleTime,
				 carma::monitor::MonitorPointDouble&    mpCurrentTemp,
				 carma::monitor::MonitorPointDouble&    mpSetTemp,
				 sza::util::Temperature&                minTemp,
				 sza::util::Temperature&                maxTemp,
				 unsigned timeoutIntervalInSeconds) :
  RemoteSensorCoProc(host, ms, mpHostIsReachable, mpHostIsOk, mpPlaceIsOk, mpPlaceStatus, mpSampleTime, mpCurrentTemp,
		     minTemp, maxTemp, timeoutIntervalInSeconds)
{
  mpSetTemp_ = &mpSetTemp;
}

/**.......................................................................
 * Destructor.
 */
RemoteSensorRrd::~RemoteSensorRrd() {}

/**.......................................................................
 * Compile the state machine we will use to get the device status
 */
void RemoteSensorRrd::
compileGetDeviceStatusStateMachine()
{
  // Clear the stack of sent commands and expected responses

  sentStrings_.clear();
  rcvdStrings_.clear();

  // We only expect one response

  rcvdStrings_.push_back(RcvdStr(":", parseDeviceStatus, this));

  // And set the iterators pointing to the head of the stack
  
  sentStringIter_ = sentStrings_.begin();
  rcvdStringIter_ = rcvdStrings_.begin();
}

/**.......................................................................
 * Process the device status received from a remote server
 */
void RemoteSensorRrd::processDeviceStatus()
{
  COUT("Inside RSR:: processDeviceStatus");

  Temperature temp1, temp2;
  String str(os_.str());

  String substr = str.findNextInstanceOf(": ", " ");
  currentTemp_.setC(substr.toFloat());

  substr = str.findNextInstanceOf(" ", " ");
  setTemp_.setC(substr.toFloat());

  // Finally, write monitor points

  writeMonitorPoints(true);
}

/**.......................................................................
 * Return the command string that will initiate communications
 */
std::string RemoteSensorRrd::getCommandString()
{
  std::ostringstream os;
  os << "ssh "
     << host_ 
     << " rrdtool fetch /opt/sysAdmin/Health/Temperature/ovrotemp.rrd LAST -s now-2min -e now-2min";

  //os << "ssh nfs2.fileserver.pvt rrdcmd";
  
  return os.str();
}

void RemoteSensorRrd::printTemps()
{
  COUT("curr = " << currentTemp_.F() << " set = " << setTemp_.F());
}

void RemoteSensorRrd::writeMonitorPoints(bool isReachable)
{
  if(haveMonitorSystem_) {
    if(isReachable) {

      CTOUT(pthread_self() << " About to lock host = " << host_);
      ms_->guard_.lock();

      try {
	mpSetTemp_->setValue(setTemp_.C());
      } catch(...) {
      }

      CTOUT(pthread_self() << " About to unlock host = " << host_);
      ms_->guard_.unlock();
    }
  }

  // Always call the base-calss method too; this actually writes the
  // buffered samples to the monitor system

  RemoteSensor::writeMonitorPoints(isReachable);
}
