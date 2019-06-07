// $Id: RemoteSensorTrippLite.h,v 1.3 2011/07/19 20:53:21 eml Exp $

#ifndef SZA_UTIL_REMOTESENSORTRIPPLITE_H
#define SZA_UTIL_REMOTESENSORTRIPPLITE_H

/**
 * @file RemoteSensorTrippLite.h
 * 
 * Tagged: Tue Nov  2 15:47:23 PDT 2010
 * 
 * @version: $Revision: 1.3 $, $Date: 2011/07/19 20:53:21 $
 * 
 * @author tcsh: username: Command not found.
 */
#include "carma/szautil/RemoteSensorCoProc.h"

namespace sza {
  namespace util {

    class RemoteSensorTrippLite : public RemoteSensorCoProc {
    public:

      /**
       * Constructor.
       */
      RemoteSensorTrippLite(std::string host,
			    unsigned timeoutIntervalInSeconds=0);

      RemoteSensorTrippLite(std::string host, 
			    SystemStatusSubsystemMutex*            ms,
			    carma::monitor::MonitorPointBool&   mpIsReachable,
			    carma::monitor::MonitorPointDouble& mpCurrentTemp,
			    carma::monitor::MonitorPointDouble& mpLowTemp,
			    carma::monitor::MonitorPointDouble& mpHighTemp,
			    unsigned timeoutIntervalInSeconds=0);

      RemoteSensorTrippLite(std::string host, 
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
			    unsigned timeoutIntervalInSeconds=0);
      
      /**
       * Destructor.
       */
      virtual ~RemoteSensorTrippLite();

      void printTemps();

      Temperature lowTemp_;
      Temperature highTemp_;

    protected:

      carma::monitor::MonitorPointDouble* mpLowTemp_;
      carma::monitor::MonitorPointDouble* mpHighTemp_;

      // Compile the list of communication/responses needed for
      // retrieving the device status from the remote server

      void compileGetDeviceStatusStateMachine();

      // Assemble the command string to be issued

      virtual std::string getCommandString();

      // Define what it means to parse the return message

      virtual void processDeviceStatus();

      void writeMonitorPoints(bool isReachable);
    }; // End class RemoteSensorTrippLite

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REMOTESENSORTRIPPLITE_H
