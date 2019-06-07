// $Id: FastPdbClientTask.h,v 1.1 2014/05/05 22:51:57 eml Exp $

#ifndef SZA_UTIL_FASTPDBCLIENTTASK_H
#define SZA_UTIL_FASTPDBCLIENTTASK_H

/**
 * @file FastPdbClientTask.h
 * 
 * Tagged: Wed Feb 19 09:25:23 PST 2014
 * 
 * @version: $Revision: 1.1 $, $Date: 2014/05/05 22:51:57 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/ClientTask.h"
#include "carma/szautil/FastPdbData.h"
#include "carma/szautil/GenericTaskMsg.h"

#define MAX_NAME_LEN 100

#define FASTPDB_CALLBACK_FN(fn) void (fn)(std::string s)

namespace sza {
  namespace util {

    class CondVar;

    //------------------------------------------------------------
    // A message class for use with this task
    //------------------------------------------------------------

    class FastPdbClientMsg : public GenericTaskMsg {
    public:

      enum MsgType {
	SRCLIST  = 0x1,
	PROJLIST = 0x2,
      };
      
      union {
	char source[MAX_NAME_LEN+1];
	char project[MAX_NAME_LEN+1];
      } body;

      FASTPDB_CALLBACK_FN(*callback);
      
      // A type for this message

      MsgType type;
    };

    //------------------------------------------------------------
    // Test class for communicating with RtdServer
    //------------------------------------------------------------

    class FastPdbClientTask : public ClientTask<FastPdbClientMsg> {
    public:


      /**
       * Constructor.
       */
      FastPdbClientTask(bool spawn, std::string host, unsigned connectPort, CondVar* condVar=0);

      /**
       * Destructor.
       */
      virtual ~FastPdbClientTask();

      virtual void processServerData();
      virtual void processMsg(FastPdbClientMsg* msg);

      void sendListSourceMsg(std::string source,   FASTPDB_CALLBACK_FN(*callback)=0);
      void sendListProjectMsg(std::string project, FASTPDB_CALLBACK_FN(*callback)=0);

      void serviceMsgQ();

    private:

      CondVar* condVar_;
      FastPdbData data_;
      FASTPDB_CALLBACK_FN(*callback_);

    }; // End class FastPdbClientTask

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_FASTPDBCLIENTTASK_H
