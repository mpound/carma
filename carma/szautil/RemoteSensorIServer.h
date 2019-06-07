// $Id: RemoteSensorIServer.h,v 1.3 2011/07/19 20:53:21 eml Exp $

#ifndef SZA_UTIL_REMOTESENSORISERVER_H
#define SZA_UTIL_REMOTESENSORISERVER_H

/**
 * @file RemoteSensorIServer.h
 * 
 * Tagged: Tue Nov  2 15:47:23 PDT 2010
 * 
 * @version: $Revision: 1.3 $, $Date: 2011/07/19 20:53:21 $
 * 
 * @author tcsh: username: Command not found.
 */
#include "carma/szautil/RemoteSensorHtml.h"

namespace sza {
  namespace util {

    class RemoteSensorIServer : public RemoteSensorHtml {
    public:

      /**
       * Constructor.
       */
      RemoteSensorIServer(std::string host, 
			  unsigned    timeoutIntervalInSeconds=0,
			  std::string query="main?PW=&PAGENAME=1");

      RemoteSensorIServer(std::string host, 
			  SystemStatusSubsystemMutex*            ms,
			  carma::monitor::MonitorPointBool&   mpIsReachable,
			  carma::monitor::MonitorPointDouble& mpCurrentTemp,
			  unsigned    timeoutIntervalInSeconds=0,
			  std::string query="main?PW=&PAGENAME=1");

      RemoteSensorIServer(std::string host, 
			  SystemStatusSubsystemMutex*            ms,
			  carma::monitor::MonitorPointBool&      mpHostIsReachable,
			  carma::monitor::MonitorPointBool&      mpHostIsOk,
			  carma::monitor::MonitorPointBool&      mpPlaceIsOk,
			  carma::monitor::MonitorPointEnum&      mpPlaceStatus,
			  carma::monitor::MonitorPointAbstime&   mpSampleTime,
			  carma::monitor::MonitorPointDouble&    mpCurrentTemp,
			  sza::util::Temperature&                minTemp,
			  sza::util::Temperature&                maxTemp,
			  unsigned    timeoutIntervalInSeconds=0,
			  std::string query="main?PW=&PAGENAME=1");

      /**
       * Destructor.
       */
      virtual ~RemoteSensorIServer();

    protected:

      void parseUrl();

      void printTemps();

    }; // End class RemoteSensorIServer

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_REMOTESENSORISERVER_H
