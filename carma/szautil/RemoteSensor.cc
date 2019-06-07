#include "carma/szautil/Connection.h"
#include "carma/szautil/RemoteSensor.h"

using namespace std;
using namespace sza::util;

void RemoteSensor::initialize(std::string host, unsigned timeoutIntervalInSeconds,
			      SystemStatusSubsystemMutex* ms)
{
  host_                 = host;
  ms_                   = ms;

  if(ms) {
    haveMonitorSystem_    = true;
  } else {
    haveMonitorSystem_    = false;
  }

  mpHostIsReachable_    = 0;
  mpCurrentTemperature_ = 0;
  mpHostIsOk_           = 0;
  mpPlaceIsOk_          = 0;
  mpPlaceStatus_        = 0;
  mpSampleTime_         = 0;

  minTemp_.setF(0.0);
  maxTemp_.setF(120.0);

  firstSample_ = true;
  deltaSampleTime_.setSeconds(120);

  initializeTimeout(timeoutIntervalInSeconds);
}

/**.......................................................................
 * Constructors.
 */
RemoteSensor::RemoteSensor(std::string host, 
			   unsigned timeoutIntervalInSeconds) :
  SpawnableTask<RemoteSensorMsg>(true)
{
  initialize(host, timeoutIntervalInSeconds, 0);
}

RemoteSensor::RemoteSensor(std::string host, 
			   SystemStatusSubsystemMutex*         ms,
			   carma::monitor::MonitorPointBool&   mpHostIsReachable,
			   carma::monitor::MonitorPointDouble& mpCurrentTemperature,
			   unsigned timeoutIntervalInSeconds) :
  SpawnableTask<RemoteSensorMsg>(true)
{
  initialize(host, timeoutIntervalInSeconds, ms);

  mpHostIsReachable_    = &mpHostIsReachable;
  mpCurrentTemperature_ = &mpCurrentTemperature;
}

RemoteSensor::RemoteSensor(std::string host, 
			   SystemStatusSubsystemMutex*            ms,
			   carma::monitor::MonitorPointBool&      mpHostIsReachable,
			   carma::monitor::MonitorPointBool&      mpHostIsOk,
			   carma::monitor::MonitorPointBool&      mpPlaceIsOk,
			   carma::monitor::MonitorPointEnum&      mpPlaceStatus,
			   carma::monitor::MonitorPointAbstime&   mpSampleTime,
			   carma::monitor::MonitorPointDouble&    mpCurrentTemperature,
			   sza::util::Temperature&                minTemp,
			   sza::util::Temperature&                maxTemp,
			   unsigned timeoutIntervalInSeconds) :
  SpawnableTask<RemoteSensorMsg>(true)
{
  initialize(host, timeoutIntervalInSeconds, ms);

  mpHostIsReachable_    = &mpHostIsReachable;
  mpHostIsOk_           = &mpHostIsOk;
  mpPlaceIsOk_          = &mpPlaceIsOk;
  mpPlaceStatus_        = &mpPlaceStatus;
  mpCurrentTemperature_ = &mpCurrentTemperature;
  mpSampleTime_         = &mpSampleTime;

  minTemp_              = minTemp;
  maxTemp_              = maxTemp;
}

/**.......................................................................
 * Destructor.
 */
RemoteSensor::~RemoteSensor() {}

/**.......................................................................
 * Public interface to this object: send a message to read our sensor
 */
void RemoteSensor::readSensor()
{
  RemoteSensorMsg msg;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  msg.type = RemoteSensorMsg::READ_SENSOR;
  sendTaskMsg(&msg);
}

/**.......................................................................
 * Process a message to read out the sensor
 */
void RemoteSensor::processReadSensorMsg()
{
  Connection conn;

  CTOUT(pthread_self() << " Inside processReadSensorMsg 0 host = " << host_);
  if(!conn.isReachable(host_)) {
    CTOUT(pthread_self() << " Inside processReadSensorMsg 1 host = " << host_);
    RemoteSensor::writeMonitorPoints(false);
    return;
  } else {
    CTOUT(pthread_self() << " Inside processReadSensorMsg 2 host = " << host_);
    executeReadSensor();
    CTOUT(pthread_self() << " Inside processReadSensorMsg 3 host = " << host_);
  }
}

/**.......................................................................
 * Respond to a message to read our sensor
 */
void RemoteSensor::processMsg(RemoteSensorMsg* msg)
{
  switch (msg->type) {
  case RemoteSensorMsg::READ_SENSOR:
    processReadSensorMsg();
    break;
  default:
    ThrowError("Unrecognized message type: " << msg->type);
    break;
  }
}

void RemoteSensor::printTemps()
{
}

/**.......................................................................
 * Write CARMA monitor points
 */
void RemoteSensor::writeMonitorPoints(bool isReachable)
{
  TimeVal currTime, diffTime;
  currTime.setToCurrentTime();

  if(firstSample_) {
    lastSampleTime_ = currTime;
    firstSample_ = false;
  }

  diffTime = currTime - lastSampleTime_;

  CTOUT(pthread_self() << " About to lock host = " << host_);
  ms_->lock();

  try {

    COUT("Inside wMP for host = " << host_ << " reachable = " << isReachable);

    // Only write if we have a monitor system

    if(haveMonitorSystem_) {

      // Write the reachability state of the host

      if(mpHostIsReachable_) {
	mpHostIsReachable_->setValue(isReachable);
      }

      // Only write the current temperature if the sensor was reachable

      if(isReachable) {

	if(mpSampleTime_) {
	  mpSampleTime_->setValue(currTime.getMjd());
	}

	if(mpCurrentTemperature_) {

	  CTOUT("Setting current Temp for host " << host_ << " to " << currentTemp_.F() << " (F), " << currentTemp_.C() << " (C)");
	  mpCurrentTemperature_->setValue(currentTemp_.C());
	
	  // And write the ok status for this device if the temperature
	  // was within limits
	
	  // If enough time has elapsed to expect this sensor readout
	  // to have changed, check the delta from the previous stored
	  // value

	  double tempDiff = 0.0;

	  if(diffTime.getTimeInSeconds() > deltaSampleTime_.seconds()) {
	    tempDiff        = fabs(currentTemp_.F() - lastTemp_.F());
	    lastTemp_       = currentTemp_;
	    lastSampleTime_ = currTime;
	  }

	  if(diffTime.getTimeInSeconds() > deltaSampleTime_.seconds() && tempDiff < 1e-12) {
	    if(mpPlaceIsOk_)
	      mpPlaceIsOk_->setValue(false);
	    if(mpPlaceStatus_)
	      mpPlaceStatus_->setValue(static_cast<int>(carma::monitor::StatusMonitorPointEnum::STATIC_VALUE), 0);
	  } else if(currentTemp_.F() < minTemp_.F()) {
	    if(mpPlaceIsOk_)
	      mpPlaceIsOk_->setValue(false);
	    if(mpPlaceStatus_)
	      mpPlaceStatus_->setValue(static_cast<int>(carma::monitor::StatusMonitorPointEnum::TEMP_TOO_LOW), 0);
	  } else if(currentTemp_.F() > maxTemp_.F()) {
	    if(mpPlaceIsOk_)
	      mpPlaceIsOk_->setValue(false);
	    if(mpPlaceStatus_)
	      mpPlaceStatus_->setValue(static_cast<int>(carma::monitor::StatusMonitorPointEnum::TEMP_TOO_HIGH), 0);
	  } else {
	    if(mpPlaceIsOk_)
	      mpPlaceIsOk_->setValue(true);
	    if(mpPlaceStatus_)
	      mpPlaceStatus_->setValue(static_cast<int>(carma::monitor::StatusMonitorPointEnum::OK), 0);
	  }
	}

	// If the host was reachable, it is OK, regardless of the state
	// of the thing that it is monitoring

	if(mpHostIsOk_)
	  mpHostIsOk_->setValue(true);

	// If the host wasn't reachable, it is NOT ok

      } else {

	if(mpHostIsOk_)
	  mpHostIsOk_->setValue(false);

      }

    }

    printTemps();

  } catch(Exception& err) {
    COUT("Caught an exception: " << err.what());
  } catch(carma::util::ErrorException& err) {
    COUT("Caught a CARMA exception: " << err.what());
  } catch(...) {
    COUT("Caught an unknown exception");
  }

  CTOUT(pthread_self() << " About to unlock host = " << host_);
  ms_->unlock();
}

/**.......................................................................
 * Overloaded method from GenericTask.  This is the run method for
 * this thread.
 */
void RemoteSensor::serviceMsgQ()
{
  bool stop=false;
  int nready; // number of file descriptors ready for reading
  
  if(msgq_.fd() < 0)
    ThrowError("Received NULL file descriptor");
  
  // Loop, checking the message queue file descriptor for readability
  
  while(!stop) {

    try {

      nready=select(fdSet_.size(), fdSet_.readFdSet(), NULL, NULL, timeOut_.tVal());
    
      switch(nready) {
	
	// If no file descriptors were ready, it is time to read out the sensor
	
      case 0:

	processReadSensorMsg();
	timeOut_.reset();
	break;
	
	// If an error occurred, report it, and wait for the next
	// timeout

      case -1:

	ThrowSysError("select()");
	break;

      default:
	  
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
}

/**.......................................................................
 * Initialize any timeout requested by the calling thread
 */
void RemoteSensor::initializeTimeout(unsigned timeoutIntervalInSeconds)
{
  if(timeoutIntervalInSeconds > 0) {
    timeOut_.setIntervalInSeconds(timeoutIntervalInSeconds);
    timeOut_.activate(true);
  } else {
    timeOut_.activate(false);
  }
}
