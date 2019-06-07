#ifndef SZA_ANTENNA_CANBUS_CANMONITORCONDITION_H
#define SZA_ANTENNA_CANBUS_CANMONITORCONDITION_H

/**
 * @file CanMonitorCondition.h
 * 
 * Tagged: Sun Oct 24 17:05:09 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/DataType.h"
#include "carma/szautil/DataTypeTruthFn.h"

namespace sza {
  namespace antenna {
    namespace canbus {
      
      /**
       * A class for handling a condition for a monitor point.
       */
      class CanMonitorCondition {
      public:
	
	/**
	 * Constructor for no condition.  Matches any condition.
	 */
	CanMonitorCondition(unsigned packetCount       = defaultPacketCount_, 
			    unsigned stablePacketCount = defaultStablePacketCount_,
			    unsigned giveUpPacketCount = defaultGiveUpPacketCount_);

	/**
	 * Constructor for a single-valued condition.
	 *
	 * @param op          operator to check with
	 *
	 * @param val         value to check
	 *
	 * @param packetCount start checking when the packet count after 
	 *                    issuing this condition reaches this count.
	 *
	 * @param stablePacketCount After we start checking, the
	 *                          condition must hold true for this
	 *                          many consecutive packets before we will 
	 *                          report it as true
	 *
	 * @param giveUpPacketCount If the condition has not become true this 
	 *                          many packets _after we start checking_, 
	 *                          give up (0 == keep trying forever)
	 */
	CanMonitorCondition(sza::util::DataTypeTruthFn fn, 
			    sza::util::DataType op1, 
			    unsigned packetCount       = defaultPacketCount_,
			    unsigned stablePacketCount = defaultStablePacketCount_,
			    unsigned giveUpPacketCount = defaultGiveUpPacketCount_);

	/**
	 * Constructor for a dual-valued condition
	 */
	CanMonitorCondition(sza::util::DataTypeTruthFn fn,
			    sza::util::DataType op1, 
			    sza::util::DataType op2, 
			    unsigned packetCount       = defaultPacketCount_,
			    unsigned stablePacketCount = defaultStablePacketCount_,
			    unsigned giveUpPacketCount = defaultGiveUpPacketCount_);


	// Equivalent setTo() methods to the constructor versions

	void setTo(unsigned packetCount       = defaultPacketCount_, 
		   unsigned stablePacketCount = defaultStablePacketCount_, 
		   unsigned giveUpPacketCount = defaultGiveUpPacketCount_);

	void setTo(sza::util::DataTypeTruthFn fn, 
		   sza::util::DataType op1, 
		   unsigned packetCount       = defaultPacketCount_, 
		   unsigned stablePacketCount = defaultStablePacketCount_, 
		   unsigned giveUpPacketCount = defaultGiveUpPacketCount_);

	void setTo(sza::util::DataTypeTruthFn fn,
		   sza::util::DataType op1, 
		   sza::util::DataType op2, 
		   unsigned packetCount       = defaultPacketCount_, 
		   unsigned stablePacketCount = defaultStablePacketCount_, 
		   unsigned giveUpPacketCount = defaultGiveUpPacketCount_);

	void operator=(const CanMonitorCondition& condition);

	/**
	 * Destructor.
	 */
	virtual ~CanMonitorCondition();
	
	// Return true if the condition is satisfied by the passed value

	bool isSatisfiedBy(sza::util::DataType& dataType);

	/**
	 * Check the packet count to see if it makes sense
	 */
	void checkPacketCount(unsigned packetCount, unsigned stablePacketCount, 
			      unsigned giveUpPacketCount);

	sza::util::DataTypeTruthFn fn_;
	sza::util::DataType op1_;
	sza::util::DataType op2_;

	unsigned packetCount_;
	unsigned stablePacketCount_;
	unsigned giveUpPacketCount_;

	static const unsigned defaultPacketCount_       =  1;
	static const unsigned defaultStablePacketCount_ =  0;
	static const unsigned defaultGiveUpPacketCount_ = 20;

      }; // End class CanMonitorCondition
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_CANMONITORCONDITION_H
