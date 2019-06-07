#include "carma/szautil/Exception.h"
#include "carma/szautil/RrdCollectorPing.h"
#include "carma/szautil/String.h"
#include "carma/szautil/Temperature.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructors.
 */
RrdCollectorPing::RrdCollectorPing(std::string host,
				   std::string rrdName,
				   unsigned timeoutIntervalInSeconds) :
  RrdCollector(host, rrdName, 0, timeoutIntervalInSeconds)
{
  mpPingTime_       = 0;
  mpPingSampleTime_ = 0;
  mpDeviceStatus_   = 0;
  mpDeviceOk_       = 0;

  maxPingTime_.setSeconds(0.5);
}

RrdCollectorPing::RrdCollectorPing(std::string host, 
				   std::string rrdName,
				   SystemStatusSubsystemMutex*          ms,
				   carma::monitor::MonitorPointDouble&  mpPingTime,
				   carma::monitor::MonitorPointAbstime& mpPingSampleTime,
				   carma::monitor::MonitorPointEnum&    mpDeviceStatus,
				   carma::monitor::MonitorPointBool&    mpDeviceOk,
				   unsigned timeoutIntervalInSeconds) :
  RrdCollector(host, rrdName, ms, timeoutIntervalInSeconds) 
{
  mpPingTime_       = &mpPingTime;
  mpPingSampleTime_ = &mpPingSampleTime;
  mpDeviceStatus_   = &mpDeviceStatus;
  mpDeviceOk_       = &mpDeviceOk;

  maxPingTime_.setSeconds(0.5);
}

/**.......................................................................
 * Destructor.
 */
RrdCollectorPing::~RrdCollectorPing() {}

/**.......................................................................
 * Process the device status received from a remote server
 */
void RrdCollectorPing::processDeviceStatus()
{
  COUT("Device status is:  " << os_.str());
  String str(os_.str());
  String absTimeStr  = str.findNextInstanceOf("ping", true, ":", true);
  String pingTimeStr = str.findNextInstanceOf(" ", true, " ", false);

  COUT("absTimeStr is now:  " << absTimeStr);
  COUT("pingTimeStr is now: " << pingTimeStr);

  pingSampleTime_.setMjd(absTimeStr.toDouble()/86400 + 40587);
  pingTime_.setSeconds(pingTimeStr.toFloat());

  // Finally, write monitor points

  writeMonitorPoints();
}

/**.......................................................................
 * Return the command string that will initiate communications
 */
std::string RrdCollectorPing::getCommandString()
{
  std::ostringstream os;

  // If we are running on the machine specified in host_, just fetch
  // the data from the database.  Else fetch via ssh.

  if(isLocal_) {
    os << "rrdtool fetch /misc/array/utilities/apache2.0.54/cacti/rra/" 
       << rrdName_ << " AVERAGE -s now-5min -e now-5min";
  } else {
    //    os << "ssh -o StrictHostKeyChecking=\"no\" " 
    os << "ssh "
       << host_ 
       << " rrdtool fetch /misc/array/utilities/apache2.0.54/cacti/rra/" 
       << rrdName_ << " AVERAGE -s now-5min -e now-5min";
  }

  COUT("(2) Command string is: " << os.str());

  return os.str();
}

void RrdCollectorPing::writeMonitorPoints()
{
  if(haveMonitorSystem_) {

    ms_->lock();

    try {

      mpPingTime_->setValue(pingTime_.seconds());
      mpPingSampleTime_->setValue(pingSampleTime_.getMjd());

      COUT("pingTime = " << pingTime_.seconds() << " max time = " << maxPingTime_.seconds());

      if(pingTime_.seconds() > maxPingTime_.seconds()) {
	mpDeviceStatus_->setValue(static_cast<int>(carma::monitor::StatusMonitorPointEnum::TIME_TOO_LONG), 0);
	mpDeviceOk_->setValue(false);
      } else {
	mpDeviceStatus_->setValue(static_cast<int>(carma::monitor::StatusMonitorPointEnum::OK), 0);
	mpDeviceOk_->setValue(true);
      }

    } catch(...) {
    }

    ms_->unlock();
  }

  RrdCollector::writeMonitorPoints();
}
