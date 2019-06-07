// $Id: RtdClientTask.h,v 1.1 2013/07/10 15:35:15 eml Exp $

#ifndef SZA_UTIL_RTDCLIENTTASK_H
#define SZA_UTIL_RTDCLIENTTASK_H

/**
 * @file RtdClientTask.h
 * 
 * Tagged: Mon Mar 25 19:32:30 PDT 2013
 * 
 * @version: $Revision: 1.1 $, $Date: 2013/07/10 15:35:15 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/ClientTask.h"
#include "carma/szautil/GenericTaskMsg.h"
#include "carma/szautil/RtdClientData.h"

namespace sza {
  namespace util {

    //------------------------------------------------------------
    // A message class for use with this task
    //------------------------------------------------------------

    class RtdClientMsg : public GenericTaskMsg {
    public:

      enum MsgType {
	ADD_REG = 0x1,
	REM_REG = 0x2,
      };
      
      union {
	unsigned regId_;
      } body;
      
      // A type for this message

      MsgType type;
    };

    //------------------------------------------------------------
    // Test class for communicating with RtdServer
    //------------------------------------------------------------

    class RtdClientTask : public ClientTask<RtdClientMsg> {
    public:

      /**
       * Constructor.
       */
      RtdClientTask(bool spawn, std::string host, unsigned connectPort);

      /**
       * Destructor.
       */
      virtual ~RtdClientTask();

      virtual void processServerData();
      virtual void processMsg(RtdClientMsg* msg);

      void sendAddRegMsg(unsigned id);
      void sendRemRegMsg(unsigned id);

    private:

      RtdClientData data_;

    }; // End class RtdClientTask

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_RTDCLIENTTASK_H
