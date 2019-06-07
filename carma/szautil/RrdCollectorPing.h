// $Id: RrdCollectorPing.h,v 1.2 2011/07/19 20:53:21 eml Exp $

#ifndef SZA_UTIL_RRDCOLLECTORRRDPING_H
#define SZA_UTIL_RRDCOLLECTORRRDPING_H

/**
 * @file RrdCollectorPing.h
 * 
 * Tagged: Tue Nov  2 15:47:23 PDT 2010
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/07/19 20:53:21 $
 * 
 * @author tcsh: username: Command not found.
 */
#include "carma/szautil/RrdCollector.h"
#include "carma/szautil/Time.h"

namespace sza {
  namespace util {

    class RrdCollectorPing : public RrdCollector {
    public:

      /**
       * Constructor.
       */
      RrdCollectorPing(std::string host,
		       std::string rrdName,
		       unsigned timeoutIntervalInSeconds=0);

      RrdCollectorPing(std::string host, 
		       std::string rrdName,
		       SystemStatusSubsystemMutex*          ms,
		       carma::monitor::MonitorPointDouble&  pingTime,
		       carma::monitor::MonitorPointAbstime& pingSampleTime,
		       carma::monitor::MonitorPointEnum&    mpDeviceStatus,
		       carma::monitor::MonitorPointBool&    mpDeviceOk,
      		       unsigned timeoutIntervalInSeconds=0);
      /**
       * Destructor
       */
      virtual ~RrdCollectorPing();

    protected:

      carma::monitor::MonitorPointDouble*  mpPingTime_;
      carma::monitor::MonitorPointAbstime* mpPingSampleTime_;
      carma::monitor::MonitorPointEnum*    mpDeviceStatus_;
      carma::monitor::MonitorPointBool*    mpDeviceOk_;

      Time    pingTime_;
      TimeVal pingSampleTime_;

      Time maxPingTime_;

      // Assemble the command string to be issued

      virtual std::string getCommandString();

      // Define what it means to parse the return message

      virtual void processDeviceStatus();
      
      // Write monitor points once data have been processed

      void writeMonitorPoints();

    }; // End class RrdCollectorPing

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RRDCOLLECTORRRDPING_H
