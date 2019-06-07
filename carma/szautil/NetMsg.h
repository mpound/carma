#ifndef SZA_UTIL_NETMSG_H
#define SZA_UTIL_NETMSG_H

/**
 * @file NetMsg.h
 * 
 * Tagged: Mon Mar 15 15:29:07 UTC 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/NetSendStr.h"

#include "carma/szaarrayutils/netobj.h"
#include "carma/szaarrayutils/rtcnetcoms.h"

#include <cstring>

namespace sza {
  namespace util {
    
    class NetMsg {
    public:
      
      /**
       * Enumerate supported message types.
       */
      enum MsgType {

	// A message to be logged

	GREETING = sza::array::NET_GREETING_MSG, 

	// A message to be logged

	LOG = sza::array::NET_LOG_MSG, 

	// An antenna ID

	ID  = sza::array::NET_ID_MSG,  

	// A request for updates of phemeris positions from the
	// navigator thread

	NAV_UPDATE = sza::array::NET_NAV_UPDATE_MSG,  

	// Report the completion of a pmac transaction

	PMAC_DONE =  sza::array::NET_PMAC_DONE_MSG,   

	// Report that a source has set

	SOURCE_SET =  sza::array::NET_SOURCE_SET_MSG,   

	// Report the completion of a caltert transaction

	CALTERT_DONE =  sza::array::NET_CALTERT_DONE_MSG,   

	// Report the completion of a caltert transaction

	IFMOD_DONE =  sza::array::NET_IFMOD_DONE_MSG,   

	// Report the completion of a CAN transaction

	CAN_DONE =  sza::array::NET_CAN_DONE_MSG,   

	// Report the completion of a Noise source transaction

	NOISE_DONE =  sza::array::NET_NOISE_DONE_MSG,   
      };
      
      /**
       * A type for this message
       */
      MsgType type;
      
      /**
       * The contents of the message.
       */
      sza::array::RtcNetMsg body;

      //------------------------------------------------------------
      // Set the antenna id.
      //------------------------------------------------------------

      inline void setAntId(AntNum::Id antId) {
	body.antenna = antId;
      }

      //------------------------------------------------------------
      // Methods to pack Network messages
      //------------------------------------------------------------

      //------------------------------------------------------------
      // Method to pack a greeting to an antenna

      inline void packGreetingMsg(unsigned int id, 
				  unsigned revision,
				  unsigned nReg,
				  unsigned nByte) {

	type = GREETING;

	body.antenna = id;
	body.msg.greeting.revision = revision;
	body.msg.greeting.nReg     = nReg;
	body.msg.greeting.nByte    = nByte;
      }

      //------------------------------------------------------------
      // Method to pack a log message
      
      inline unsigned maxMsgLen() {
	return  sza::array::NET_LOG_MAX;
      }

      //------------------------------------------------------------
      // Method to pack a log message
      
      inline void packLogMsg(std::string message, bool isError, 
			     unsigned seq=0, bool end=0) {
	
	int length = message.length();
	
	length = (length > sza::array::NET_LOG_MAX) ? 
	  sza::array::NET_LOG_MAX : length;
	
	type = LOG;
	
	strncpy(body.msg.log.text, message.c_str(), length);
	
	// Make sure the string is properly terminated
	
	body.msg.log.text[length] = '\0';
	body.msg.log.bad   = isError;
	body.msg.log.seq   = seq;
	body.msg.log.end   = end;
      }

      //------------------------------------------------------------
      // Method to pack an antenna id

      inline void packAntennaIdMsg(unsigned int id) {

	type = ID;
	body.antenna = id;
      }

      //------------------------------------------------------------
      // Method to pack a request for ephemeris updates from the
      // navigator thread

      inline void packNavUpdateMsg() {
	type = NAV_UPDATE;
      }

      //------------------------------------------------------------
      // Method to pack a pmacxo transaction completion message

      inline void packPmacDoneMsg(unsigned seq) {
	type = PMAC_DONE;
	body.msg.pmac_done.seq = seq;
      }

      //------------------------------------------------------------
      // Method to report that a source has set

      inline void packSourceSetMsg(unsigned seq) {
	type = SOURCE_SET;
	body.msg.source_set.seq = seq;
      }

      //------------------------------------------------------------
      // Method to pack a CalTert transaction completion message

      inline void packCalTertDoneMsg(unsigned seq) {
	type = CALTERT_DONE;
	body.msg.calTertDone.seq = seq;
      }

      //------------------------------------------------------------
      // Method to pack a IFMod transaction completion message

      inline void packIFModDoneMsg(unsigned seq) {
	type = IFMOD_DONE;
	body.msg.IFModDone.seq = seq;
      }

      //------------------------------------------------------------
      // Method to pack a CAN transaction completion message

      inline void packCanCommandDoneMsg(unsigned seq) {
	type = CAN_DONE;
	body.msg.canDone.seq = seq;
      }

      //------------------------------------------------------------
      // Method to pack a Noise transaction completion message

      inline void packNoiseCommandDoneMsg(unsigned seq) {
	type = NOISE_DONE;
	body.msg.noiseDone.seq = seq;
      }

    }; // End class NetMsg
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETMSG_H
