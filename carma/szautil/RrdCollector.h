// $Id: RrdCollector.h,v 1.2 2011/07/19 20:53:21 eml Exp $

#ifndef SZA_UTIL_RRDCOLLECTORRRD_H
#define SZA_UTIL_RRDCOLLECTORRRD_H

/**
 * @file RrdCollector.h
 * 
 * Tagged: Tue Nov  2 15:47:23 PDT 2010
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/07/19 20:53:21 $
 * 
 * @author tcsh: username: Command not found.
 */
#include "carma/szautil/RemoteSensorCoProc.h"

namespace sza {
  namespace util {

    class RrdCollector : public RemoteSensorCoProc {
    public:

      /**
       * Constructor.
       */
      RrdCollector(std::string host,
		   std::string rrdName,
		   SystemStatusSubsystemMutex* ms=0,
		   unsigned timeoutIntervalInSeconds=0);

      /**
       * Destructor
       */
      virtual ~RrdCollector();

    protected:

      // True if we are running on the host where the RRD is located

      bool isLocal_;

      // The name of the RRD database

      std::string rrdName_;

      // Compile the list of communication/responses needed for
      // retrieving the device status from the remote server

      void compileGetDeviceStatusStateMachine();

      // Assemble the command string to be issued

      virtual std::string getCommandString();

      // Define what it means to parse the return message

      virtual void processDeviceStatus();
      
      void writeMonitorPoints();

    }; // End class RrdCollector

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RRDCOLLECTORRRD_H
