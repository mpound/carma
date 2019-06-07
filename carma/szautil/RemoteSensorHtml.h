// $Id: RemoteSensorHtml.h,v 1.3 2011/07/19 20:53:20 eml Exp $

#ifndef SZA_UTIL_REMOTESENSORHTML_H
#define SZA_UTIL_REMOTESENSORHTML_H

/**
 * @file RemoteSensorHtml.h
 * 
 * Tagged: Tue Nov  2 14:30:13 PDT 2010
 * 
 * @version: $Revision: 1.3 $, $Date: 2011/07/19 20:53:20 $
 * 
 * @author tcsh: username: Command not found.
 */
#include "carma/szautil/RemoteSensor.h"
#include "carma/szautil/Temperature.h"

#include <string>

namespace sza {
  namespace util {

    class RemoteSensorHtml : public RemoteSensor {
    public:

      /**
       * Constructor.
       */
      RemoteSensorHtml(std::string host, 
		       unsigned    timeoutIntervalInSeconds=0,
		       std::string query="main?PW=&PAGENAME=1");


      RemoteSensorHtml(std::string host, 
		       SystemStatusSubsystemMutex*            ms,
		       carma::monitor::MonitorPointBool&      mpIsReachable,
		       carma::monitor::MonitorPointDouble&    mpCurrentTemp,
		       unsigned timeoutIntervalInSeconds=0,
		       std::string query="main?PW=&PAGENAME=1");


      RemoteSensorHtml(std::string host, 
		       SystemStatusSubsystemMutex*            ms,
		       carma::monitor::MonitorPointBool&      mpHostIsReachable,
		       carma::monitor::MonitorPointBool&      mpHostIsOk,
		       carma::monitor::MonitorPointBool&      mpPlaceIsOk,
		       carma::monitor::MonitorPointEnum&      mpPlaceStatus,
		       carma::monitor::MonitorPointAbstime&   mpSampleTime,
		       carma::monitor::MonitorPointDouble&    mpCurrentTemp,
		       sza::util::Temperature&                minTemp,
		       sza::util::Temperature&                maxTemp,
		       unsigned timeoutIntervalInSeconds=0,
		       std::string query="main?PW=&PAGENAME=1");


      /**
       * Destructor.
       */
      virtual ~RemoteSensorHtml();

    protected:
      
      void executeReadSensor();
      void getUrl();
      virtual void parseUrl();
      
      std::string query_;
      std::string url_;

    }; // End class RemoteSensorHtml

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REMOTESENSORHTML_H
