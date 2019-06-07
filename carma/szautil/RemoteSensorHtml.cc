#include "carma/szautil/CurlUtils.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/RemoteSensorHtml.h"
#include "carma/szautil/Temperature.h"

#include <sstream>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RemoteSensorHtml::RemoteSensorHtml(std::string host, 
				   unsigned    timeoutIntervalInSeconds,
				   std::string query) :
  RemoteSensor(host, timeoutIntervalInSeconds)
{
  query_ = query;
}

RemoteSensorHtml::RemoteSensorHtml(std::string host, 
				   SystemStatusSubsystemMutex*            ms,
				   carma::monitor::MonitorPointBool&      mpIsReachable,
				   carma::monitor::MonitorPointDouble&    mpCurrentTemp,
				   unsigned    timeoutIntervalInSeconds,
				   std::string query) :
  RemoteSensor(host, ms, mpIsReachable, mpCurrentTemp, timeoutIntervalInSeconds)
{
  query_ = query;
}

RemoteSensorHtml::RemoteSensorHtml(std::string host, 
				   SystemStatusSubsystemMutex*            ms,
				   carma::monitor::MonitorPointBool&      mpHostIsReachable,
				   carma::monitor::MonitorPointBool&      mpHostIsOk,
				   carma::monitor::MonitorPointBool&      mpPlaceIsOk,
				   carma::monitor::MonitorPointEnum&      mpPlaceStatus,
				   carma::monitor::MonitorPointAbstime&   mpSampleTime,
				   carma::monitor::MonitorPointDouble&    mpCurrentTemp,
				   sza::util::Temperature&                minTemp,
				   sza::util::Temperature&                maxTemp,
				   unsigned timeoutIntervalInSeconds,
				   std::string query) :
  RemoteSensor(host, ms, mpHostIsReachable, mpHostIsOk, mpPlaceIsOk, mpPlaceStatus, mpSampleTime, mpCurrentTemp, 
	       minTemp, maxTemp, timeoutIntervalInSeconds)
{
  query_ = query;
}


/**.......................................................................
 * Destructor.
 */
RemoteSensorHtml::~RemoteSensorHtml() {}

void RemoteSensorHtml::executeReadSensor()
{
  getUrl();
  parseUrl();
}

void RemoteSensorHtml::getUrl()
{
  CurlUtils curl;

  std::ostringstream os;
  os << "http://" << host_ << "/" << query_;

  url_ = curl.getUrl(os.str());
}

void RemoteSensorHtml::parseUrl()
{
}
