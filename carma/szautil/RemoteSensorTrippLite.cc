#include "carma/szautil/Exception.h"
#include "carma/szautil/RemoteSensorTrippLite.h"
#include "carma/szautil/String.h"
#include "carma/szautil/Temperature.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RemoteSensorTrippLite::RemoteSensorTrippLite(std::string host,
					     unsigned timeoutIntervalInSeconds) :
  RemoteSensorCoProc(host, timeoutIntervalInSeconds)
{
  mpLowTemp_  = 0;
  mpHighTemp_ = 0;
}

RemoteSensorTrippLite::RemoteSensorTrippLite(std::string host, 
					     SystemStatusSubsystemMutex*            ms,
					     carma::monitor::MonitorPointBool&      mpIsReachable,
					     carma::monitor::MonitorPointDouble&    mpCurrentTemp,
					     carma::monitor::MonitorPointDouble&    mpLowTemp,
					     carma::monitor::MonitorPointDouble&    mpHighTemp,
					     unsigned timeoutIntervalInSeconds) :
  RemoteSensorCoProc(host, ms, mpIsReachable, mpCurrentTemp, timeoutIntervalInSeconds)
{
  mpLowTemp_  = &mpLowTemp;
  mpHighTemp_ = &mpHighTemp;
}

RemoteSensorTrippLite::RemoteSensorTrippLite(std::string host, 
					     SystemStatusSubsystemMutex*            ms,
					     carma::monitor::MonitorPointBool&      mpHostIsReachable,
					     carma::monitor::MonitorPointBool&      mpHostIsOk,
					     carma::monitor::MonitorPointBool&      mpPlaceIsOk,
					     carma::monitor::MonitorPointEnum&      mpPlaceStatus,
					     carma::monitor::MonitorPointAbstime&   mpSampleTime,
					     carma::monitor::MonitorPointDouble&    mpCurrentTemp,
					     carma::monitor::MonitorPointDouble&    mpLowTemp,
					     carma::monitor::MonitorPointDouble&    mpHighTemp,
					     sza::util::Temperature&                minTemp,
					     sza::util::Temperature&                maxTemp,
					     unsigned timeoutIntervalInSeconds) :
  RemoteSensorCoProc(host, ms, mpHostIsReachable, mpHostIsOk, mpPlaceIsOk, mpPlaceStatus, mpSampleTime, mpCurrentTemp, 
		     minTemp, maxTemp, timeoutIntervalInSeconds)
{
  mpLowTemp_  = &mpLowTemp;
  mpHighTemp_ = &mpHighTemp;
}

/**.......................................................................
 * Destructor.
 */
RemoteSensorTrippLite::~RemoteSensorTrippLite() {}

/**.......................................................................
 * Compile the state machine we will use to get the device status
 */
void RemoteSensorTrippLite::
compileGetDeviceStatusStateMachine()
{
  // Clear the stack of sent commands and expected responses

  sentStrings_.clear();
  rcvdStrings_.clear();

  rcvdStrings_.push_back(RcvdStr("login:", sendNextString, this));
  sentStrings_.push_back("admin");

  rcvdStrings_.push_back(RcvdStr("Password:", sendNextString, this));
  sentStrings_.push_back("power");

  rcvdStrings_.push_back(RcvdStr(">", sendNextString, this));
  sentStrings_.push_back("1");

  rcvdStrings_.push_back(RcvdStr(">", sendNextString, this));
  sentStrings_.push_back("2");

  rcvdStrings_.push_back(RcvdStr(">", sendNextString, this));
  sentStrings_.push_back("1");

  rcvdStrings_.push_back(RcvdStr(">", parseDeviceStatus, this));
  sentStrings_.push_back("x");

  rcvdStrings_.push_back(RcvdStr(">", sendNextString, this));
  sentStrings_.push_back("x");

  rcvdStrings_.push_back(RcvdStr(">", sendNextString, this));
  sentStrings_.push_back("x");

  // And set the iterators pointing to the head of the stack
  
  sentStringIter_ = sentStrings_.begin();
  rcvdStringIter_ = rcvdStrings_.begin();
}

/**.......................................................................
 * Process the device status received from a remote server
 */
void RemoteSensorTrippLite::processDeviceStatus()
{
  Temperature curr, low, high;
  String str(os_.str());
  String substr = str.findNextInstanceOf("Temperature (F)", "Humidity");

  String currTemp = substr.findNextInstanceOf(":", "F");
  currentTemp_.setF(currTemp.toFloat());

  String lowTemp = substr.findNextInstanceOf(":", "F");
  lowTemp_.setF(lowTemp.toFloat());

  String highTemp = substr.findNextInstanceOf(":", "F");
  highTemp_.setF(highTemp.toFloat());

  // Write monitor points

  writeMonitorPoints(true);

  // And send the termination string to the server

  sendNextString();
}

/**.......................................................................
 * Return the command string that will initiate communications
 */
std::string RemoteSensorTrippLite::getCommandString()
{
  std::ostringstream os;
  os << "telnet " << host_;
  
  return os.str();
}

void RemoteSensorTrippLite::printTemps()
{
  COUT("curr = " << currentTemp_.F() << " low = " << lowTemp_.F() << " high = " << highTemp_.F());
}

void RemoteSensorTrippLite::writeMonitorPoints(bool isReachable)
{
  if(haveMonitorSystem_) {
    if(isReachable) {

      CTOUT(pthread_self() << " About to lock host = " << host_);
      ms_->lock();

      try {
	mpLowTemp_->setValue(lowTemp_.C());
	mpHighTemp_->setValue(highTemp_.C());
      } catch(...) {
      }

      CTOUT(pthread_self() << " About to unlock host = " << host_);
      ms_->unlock();
    }
  }

  // Always call the base-class method too; this actually writes the
  // buffered samples to the monitor system

  RemoteSensor::writeMonitorPoints(isReachable);
}
