#include "carma/szautil/Exception.h"
#include "carma/szautil/RrdCollector.h"
#include "carma/szautil/String.h"
#include "carma/szautil/Temperature.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructors.
 */
RrdCollector::RrdCollector(std::string host,
			   std::string rrdName,
			   SystemStatusSubsystemMutex* ms,
			   unsigned timeoutIntervalInSeconds) :
  RemoteSensorCoProc(host, timeoutIntervalInSeconds)
{ 
  rrdName_ = rrdName;

  if(ms) {
    ms_                   = ms;
    haveMonitorSystem_    = true;
  } else {
    ms_                   = 0;
    haveMonitorSystem_    = false;
  }

  char hostName[HOST_NAME_MAX+1];

  if(gethostname(hostName, HOST_NAME_MAX+1)) {
    isLocal_ = false;
  } else {
    String hostStr(hostName);
    isLocal_ = hostStr.contains(host_);
  }
}

/**.......................................................................
 * Destructor.
 */
RrdCollector::~RrdCollector() {}

/**.......................................................................
 * Compile the state machine we will use to get the device status
 */
void RrdCollector::
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
void RrdCollector::processDeviceStatus()
{
  COUT("Called RrdCollector::processDeviceStatus() stub");
}

/**.......................................................................
 * Return the command string that will initiate communications
 */
std::string RrdCollector::getCommandString()
{
  std::ostringstream os;
  os << "ssh " << host_ << " rrdtool fetch /opt/sysAdmin/Health/Temperature/ovrotemp.rrd LAST -s now-1min -e now-1min";
  //  os << "ssh nfs2.fileserver.pvt rrdcmd";
  
  COUT("(1) Command string is: " << os.str());

  return os.str();
}

void RrdCollector::writeMonitorPoints()
{
  // We don't write monitor points here anymore.  They are written
  // once by a separate thread.

  if(haveMonitorSystem_) {
    //    ms_->ss_->write();
  }
}
