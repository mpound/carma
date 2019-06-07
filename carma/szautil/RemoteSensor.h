// $Id: RemoteSensor.h,v 1.4 2011/08/01 18:14:35 eml Exp $

#ifndef SZA_UTIL_REMOTESENSOR_H
#define SZA_UTIL_REMOTESENSOR_H

/**
 * @file RemoteSensor.h
 * 
 * Tagged: Tue Nov  2 14:28:38 PDT 2010
 * 
 * @version: $Revision: 1.4 $, $Date: 2011/08/01 18:14:35 $
 * 
 * @author tcsh: username: Command not found.
 */
#include "carma/szautil/Communicator.h"
#include "carma/szautil/GenericTaskMsg.h"
#include "carma/szautil/SpawnableTask.h"
#include "carma/szautil/SystemStatusSubsystemMutex.h"
#include "carma/szautil/Temperature.h"
#include "carma/szautil/Time.h"
#include "carma/szautil/TimeOut.h"
#include "carma/szautil/TimeVal.h"

#include "carma/monitor/SystemStatus.h"
#include "carma/monitor/monitorPointSpecializations.h"

#include <string>

namespace sza {
  namespace util {

    //-----------------------------------------------------------------------
    // A utility class for sending messages to the RemoteSensorCoProc task
    //-----------------------------------------------------------------------

    class RemoteSensorMsg : public GenericTaskMsg {
    public:

      enum MsgType {
	READ_SENSOR,
      };
      
      union {
	bool enable;
      } body;
      
      // A type for this message

      MsgType type;
    };

    //-----------------------------------------------------------------------
    // RemoteSensor class definition
    //-----------------------------------------------------------------------

    class RemoteSensor : public SpawnableTask<RemoteSensorMsg> {

    public:

      /**
       * Constructor.
       */
      RemoteSensor(std::string host,
		   unsigned    timeoutIntervalInSeconds=0);

      RemoteSensor(std::string host, 
		   SystemStatusSubsystemMutex*            ms,
		   carma::monitor::MonitorPointBool&      mpIsReachable,
		   carma::monitor::MonitorPointDouble&    mpCurrentTemperature,
		   unsigned timeoutIntervalInSeconds=0);

      RemoteSensor(std::string host, 
		   SystemStatusSubsystemMutex*            ms,
		   carma::monitor::MonitorPointBool&      mpHostIsReachable,
		   carma::monitor::MonitorPointBool&      mpHostIsOk,
		   carma::monitor::MonitorPointBool&      mpPlaceIsOk,
		   carma::monitor::MonitorPointEnum&      mpPlaceStatus,
		   carma::monitor::MonitorPointAbstime&   mpSampleTime,
		   carma::monitor::MonitorPointDouble&    mpCurrentTemperature,
		   sza::util::Temperature&                minTemp,
		   sza::util::Temperature&                maxTemp,
		   unsigned timeoutIntervalInSeconds=0);

      void initialize(std::string host, unsigned timeoutIntervalInSeconds,
		      SystemStatusSubsystemMutex* ms);

      /**
       * Destructor.
       */
      virtual ~RemoteSensor();

      void readSensor();

      virtual void printTemps();

    protected:

      Time deltaSampleTime_;
      TimeVal lastSampleTime_;
      bool firstSample_;

      std::string host_;
      Temperature currentTemp_;
      Temperature lastTemp_;
      Temperature minTemp_;
      Temperature maxTemp_;
      TimeOut timeOut_;

      bool haveMonitorSystem_;

      SystemStatusSubsystemMutex*            ms_;
      carma::monitor::MonitorPointBool*      mpHostIsReachable_;
      carma::monitor::MonitorPointBool*      mpHostIsOk_;
      carma::monitor::MonitorPointBool*      mpPlaceIsOk_;
      carma::monitor::MonitorPointEnum*      mpPlaceStatus_;
      carma::monitor::MonitorPointDouble*    mpCurrentTemperature_;
      carma::monitor::MonitorPointAbstime*   mpSampleTime_;

      // Run method for this object

      void serviceMsgQ();

      // Set up our internal timeout

      void initializeTimeout(unsigned timeoutIntervalInSeconds);

      // Process a request to read the sensor

      void processReadSensorMsg();

      // Generic method to process messages received on our queue

      void processMsg(RemoteSensorMsg* msg);

      // Inheritors should define what it means to read a particular
      // sensor

      virtual void executeReadSensor() = 0;

      // Inheritors may define what it means to write monitor points
      // for a particular sensor

      virtual void writeMonitorPoints(bool isReachable);

    }; // End class RemoteSensor

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REMOTESENSOR_H
