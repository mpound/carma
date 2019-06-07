#include "carma/szautil/Exception.h"
#include "carma/szautil/RemoteSensorIServer.h"
#include "carma/szautil/String.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RemoteSensorIServer::RemoteSensorIServer(std::string host, 
					 unsigned    timeoutIntervalInSeconds,
					 std::string query) :
  RemoteSensorHtml(host, timeoutIntervalInSeconds, query)
{}

RemoteSensorIServer::RemoteSensorIServer(std::string host, 
					 SystemStatusSubsystemMutex*            ms,
					 carma::monitor::MonitorPointBool&      mpIsReachable,
					 carma::monitor::MonitorPointDouble&    mpCurrentTemp,
					 unsigned    timeoutIntervalInSeconds,
					 std::string query) :
  RemoteSensorHtml(host, ms, mpIsReachable, mpCurrentTemp, timeoutIntervalInSeconds, query)
{}

RemoteSensorIServer::RemoteSensorIServer(std::string host, 
					 SystemStatusSubsystemMutex*            ms,
					 carma::monitor::MonitorPointBool&      mpHostIsReachable,
					 carma::monitor::MonitorPointBool&      mpHostIsOk,
					 carma::monitor::MonitorPointBool&      mpPlaceIsOk,
					 carma::monitor::MonitorPointEnum&      mpPlaceStatus,
					 carma::monitor::MonitorPointAbstime&   mpSampleTime,
					 carma::monitor::MonitorPointDouble&    mpCurrentTemp,
					 sza::util::Temperature&                minTemp,
					 sza::util::Temperature&                maxTemp,
					 unsigned    timeoutIntervalInSeconds,
					 std::string query) :
  RemoteSensorHtml(host, ms, mpHostIsReachable, mpHostIsOk, mpPlaceIsOk, mpPlaceStatus, mpSampleTime, mpCurrentTemp,
		   minTemp, maxTemp, timeoutIntervalInSeconds, query)
{}


/**.......................................................................
 * Destructor.
 */
RemoteSensorIServer::~RemoteSensorIServer() {}

void RemoteSensorIServer::parseUrl()
{
  String response(url_);
  String substr = response.findNextInstanceOf("dev_nd", ")");
  String temp   = substr.findNextInstanceOf("X01", "\"");

  currentTemp_.setF(temp.toFloat());

  // Update monitor points

  writeMonitorPoints(true);
}

void RemoteSensorIServer::printTemps()
{
  COUT("curr = " << currentTemp_.F());
}
