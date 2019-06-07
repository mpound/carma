#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/IoLock.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Temperature.h"
#include "carma/szautil/TimeOut.h"
#include "carma/szautil/RemoteSensorCoProc.h"

#include <fstream>
#include <cerrno>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Initialize pertinent members of this class to sensible defaults
 */
void RemoteSensorCoProc::initialize()
{
  coProc_   = 0;

  // This is necessary or else the returned string may contain
  // characters like '\r' which will cause subsequent string
  // comparisons by the likes of strstr(), etc., to fail.

  readPort_.stripUnprintable(true);

  // And will append \r\0 to everything we send to the strip's port.

  writePort_.append("\r\0");
}

/**.......................................................................
 * Constructors.
 */
RemoteSensorCoProc::RemoteSensorCoProc(std::string host, 
				   unsigned timeoutIntervalInSeconds) :
  RemoteSensor(host, timeoutIntervalInSeconds)
{
  initialize();
}

RemoteSensorCoProc::RemoteSensorCoProc(std::string host, 
				       SystemStatusSubsystemMutex*         ms,
				       carma::monitor::MonitorPointBool&   mpIsReachable,
				       carma::monitor::MonitorPointDouble& mpCurrentTemp,
				       unsigned timeoutIntervalInSeconds) :
  RemoteSensor(host, ms, mpIsReachable, mpCurrentTemp, timeoutIntervalInSeconds)
{
  initialize();
}

RemoteSensorCoProc::RemoteSensorCoProc(std::string host, 
				       SystemStatusSubsystemMutex*            ms,
				       carma::monitor::MonitorPointBool&      mpHostIsReachable,
				       carma::monitor::MonitorPointBool&      mpHostIsOk,
				       carma::monitor::MonitorPointBool&      mpPlaceIsOk,
				       carma::monitor::MonitorPointEnum&      mpPlaceStatus,
				       carma::monitor::MonitorPointAbstime&   mpSampleTime,
				       carma::monitor::MonitorPointDouble&    mpCurrentTemp,
				       sza::util::Temperature&                minTemp,
				       sza::util::Temperature&                maxTemp,
				       unsigned timeoutIntervalInSeconds) :
  RemoteSensor(host, ms, mpHostIsReachable, mpHostIsOk, mpPlaceIsOk, mpPlaceStatus, mpSampleTime, mpCurrentTemp,
	       minTemp, maxTemp, timeoutIntervalInSeconds)
{
  initialize();
}

/**.......................................................................
 * Destructor.
 */
RemoteSensorCoProc::~RemoteSensorCoProc() {}

/**.......................................................................
 * Initiate sending commands to the server.
 */
void RemoteSensorCoProc::
initiateGetDeviceStatusCommSequence()
{
  // Compile the command/response stack associated with this command

  compileGetDeviceStatusStateMachine();

  // Establish a connection to the server

  connect();
}

void RemoteSensorCoProc::connect()
{
  // Terminate any previous connection

  disconnect();

  // Initiate a connection to the client
  
  try {

    // Now attempt to connect.
    
    coProc_ = new sza::util::CoProc(getCommandString());
    
    if(coProc_->stdOut()->readFd() < 0 || coProc_->stdIn()->writeFd() < 0) {
      ThrowError("Unable to connect to telnet server at host: " << host_);
    }
    
    // If coProc_ successfully returned an fd, mark us as connected.
    
    connected_ = true;
    
    errPort_.setFd(coProc_->stdErr()->readFd());
    readPort_.setFd(coProc_->stdOut()->readFd());
    writePort_.setFd(coProc_->stdIn()->writeFd());
    
    // And register the readfd to be watched for readability
    
    fdSet_.registerReadFd(readPort_.getFd());
    fdSet_.registerReadFd(errPort_.getFd());

  } catch(Exception& err) {
    COUT(err.what() << " " << pthread_self());
  }
}

void RemoteSensorCoProc::disconnect()
{
  fdSet_.clear();

  errPort_.setFd(-1);
  readPort_.setFd(-1);
  writePort_.setFd(-1);

  if(coProc_) {
    delete coProc_;
    coProc_ = 0;
  }

  fdSet_.registerReadFd(msgq_.fd());
}

/**.......................................................................,
 * React to a failure to reply
 */
void RemoteSensorCoProc::registerTimeOut()
{
  // Register our failure to communicate

  LogMessage(true, "Timed out waiting for a response from the server on host: " << host_);
  terminateCommSequence(true);
}

/**.......................................................................
 * Static parser method to respond to disconnect from the server
 */
COMM_PARSER_FN(RemoteSensorCoProc::parseDeviceStatus)
{
  RemoteSensorCoProc* comm = (RemoteSensorCoProc*) arg;
  comm->processDeviceStatus();
}

/**.......................................................................
 * Main Task event loop: when this is called, the task blocks forever
 * in select(), or until a stop message is received.
 */
void RemoteSensorCoProc::serviceMsgQ()
{
  try {
    bool stop=false;
    int nready; // number of file descriptors ready for reading
    
    if(msgq_.fd() < 0) {
      ThrowError("Received NULL file descriptor");
    }
    
    while(!stop) {
      

      try {

	nready = select(fdSet_.size(), fdSet_.readFdSet(), 
			NULL, NULL, timeOut_.tVal());
	
	switch (nready) {
	  
	  // If no file descriptors were ready, it is time to contact the
	  // server and retrieve the device status
	  
	case 0:

	  processReadSensorMsg();
	  timeOut_.reset();
	  break;
	  
	case -1:
	  
	  disconnect();
	  
	  //	stop = true;
	  //	COUT("Throwing system error: " << pthread_self());
	  //	ThrowSysError("select()");
	  break;
	  
	default:
	  
	  // If this communicator's read fd was set, read the next line
	  // from the server
	  
	  if(getFd() > 0 && fdSet_.isSetInRead(getFd())) {
	    processClientMessage();
	  }
	  
	  if(errPort_.getFd() > 0 && fdSet_.isSetInRead(errPort_.getFd())) {
	    errPort_.concatenateString(os_);
	    ThrowError("Read: " << os_.str());
	  }
	  
	  // If a message is waiting to be read, process it now
	  
	  if(fdSet_.isSetInRead(msgq_.fd())) {
	    processTaskMsg(&stop);
	  }
	  
	  break;
	}
      } catch(Exception& err) {
	CTOUT("Caught an exception (1): " << pthread_self() << " " << err.what());
      }
    }
  } catch(Exception& err) {
    CTOUT("Caught an exception (2): " << pthread_self() << " " << err.what());
  }

}


/**.......................................................................
 * Overloaded function to write a string
 */
void RemoteSensorCoProc::writeString(std::string str)
{
  if(writePort_.getFd() > 0)
    writePort_.writeString(str);
}

/**.......................................................................
 * Overloaded function to concatenate a string
 */
void RemoteSensorCoProc::concatenateString(std::ostringstream& os)
{
  if(readPort_.getFd() > 0)
    readPort_.concatenateString(os_);
}

/**.......................................................................
 * Overloaded function to return the read file descriptor
 */
int RemoteSensorCoProc::getFd()
{
  return readPort_.getFd();
}

/**.......................................................................
 * Terminate a command sequence to the power strip.
 */
void RemoteSensorCoProc::terminateCommSequence(bool error)
{
  if(readPort_.getFd() > 0)
    fdSet_.clearFromReadFdSet(readPort_.getFd());

  if(errPort_.getFd() > 0)
    fdSet_.clearFromReadFdSet(errPort_.getFd());
}

/**.......................................................................
 * What we will do in response to a message to read our sensor
 */
void RemoteSensorCoProc::executeReadSensor()
{
  initiateGetDeviceStatusCommSequence();
}
