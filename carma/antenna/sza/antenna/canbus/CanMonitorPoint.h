#ifndef SZA_ANTENNA_CANBUS_CANMONITORPOINT_H
#define SZA_ANTENNA_CANBUS_CANMONITORPOINT_H

/**
 * @file CanMonitorPoint.h
 * 
 * Tagged: Sat Oct 23 22:31:40 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/canbus/CanMonitorCondition.h"
#include "carma/szaarrayutils/regmap.h"
#include "carma/szautil/DataType.h"

#define CAN_MONITOR_CONDITION_HANDLER(fn) void (fn)(void* arg, bool satisfied, std::string message)

namespace sza {

  namespace util {
    class Coord;
    class CoordRange;
  }
  
  namespace antenna {

    namespace control {
      class SzaShare;
    }

    namespace canbus {
      
      class CanMonitorPoint {
      public:
	
	/**
	 * Constructor.
	 */
	CanMonitorPoint(sza::antenna::control::SzaShare* share, 
			RegMapBlock* block);
	
	/**
	 * Destructor.
	 */
	virtual ~CanMonitorPoint();
	
	inline RegMapBlock* block() {
	  return block_;
	}

	void writeReg(bool isSim, bool b, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, unsigned char uc, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, char ch, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, unsigned short us, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, short s, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, unsigned int ui, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, int i, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, float f, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, double d, sza::util::CoordRange* range=0);

	void writeReg(bool isSim, bool* b, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, unsigned char* uc, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, char* ch, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, unsigned short* us, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, short* s, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, unsigned int* ui, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, int* i, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, float* f, sza::util::CoordRange* range=0);
	void writeReg(bool isSim, double* d, sza::util::CoordRange* range=0);

	// Register a callback associated with a condition

	void registerConditionHandler(CAN_MONITOR_CONDITION_HANDLER(*handler), 
				      void* arg,
				      std::string& message,
				      CanMonitorCondition& condition, 
				      sza::util::Coord* coord=0);

	// This struct will manage condition-handler associations.

	struct ConditionHandler {
	  CanMonitorCondition condition_;
	  CAN_MONITOR_CONDITION_HANDLER(*handler_);
	  void* arg_;
	  std::string message_;

	  unsigned packetCount_;
	  unsigned stablePacketCount_;
	  unsigned giveUpPacketCount_;

	  ConditionHandler();
	  void callHandler(bool conditionWasMet);
	};

      private:

	// The register block that represents this monitor point

	RegMapBlock* block_;

	// A pointer to the shared memory object

	sza::antenna::control::SzaShare* share_;

	// The value of this monitor point

	sza::util::DataType dataType_;

	// A vector of handlers

	std::vector<ConditionHandler> handlers_;

	/**
	 * See if a handler has been registered for this monitor point
	 */
	void checkHandler(bool isSim, sza::util::CoordRange* range);

      }; // End class CanMonitorPoint
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza


#endif // End #ifndef SZA_ANTENNA_CANBUS_CANMONITORPOINT_H
