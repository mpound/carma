#ifndef SZA_UTIL_CCNETMSG_H
#define SZA_UTIL_CCNETMSG_H

/**
 * @file CcNetMsg.h
 * 
 * Tagged: Mon Mar 15 15:29:07 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/NetSendStr.h"

#include "carma/szaarrayutils/netobj.h"
#include "carma/szaarrayutils/control.h"

namespace sza {
  namespace util {
    
    class CcNetMsg {
    public:
      
      /**
       * Enumerate supported message types.
       */
      enum MsgType {
	LOG      = sza::array::CC_LOG_MSG,   // A message to be logged
	REPLY    = sza::array::CC_REPLY_MSG, // A reply to a CC_INPUT_CMD
	SCHED    = sza::array::CC_SCHED_MSG, // A message from the scheduler
	ARC      = sza::array::CC_ARC_MSG,   // A message from the archiver
	PAGE     = sza::array::CC_PAGE_MSG,  // A message regarding the pager
	PAGECOND = sza::array::CC_PAGECOND_MSG, // A message regarding the pager
	CONFIG   = sza::array::CC_CONFIG_MSG, // A message regarding
					      // the array
					      // configuration
      };
      
      /**
       * A type for this message
       */
      MsgType type;
      
      /**
       * The contents of the message.
       */
      sza::array::CcNetMsg body;

      //------------------------------------------------------------
      // Methods to pack Network messages
      //------------------------------------------------------------

    }; // End class CcNetMsg
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_CCNETMSG_H
