#ifndef SZA_UTIL_MONITORPOINT_H
#define SZA_UTIL_MONITORPOINT_H

/**
 * @file MonitorPoint.h
 * 
 * Tagged: Sat Oct 23 22:31:40 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AxisRange.h"
#include "carma/szautil/DataType.h"
#include "carma/szautil/MonitorCondition.h"

#include "carma/szautil/ArrayMapDataFrameManager.h"
#include "carma/szautil/RegMapDataFrameManager.h"

#include "carma/szaarrayutils/szaregs.h"

#include <list>

#define MONITOR_CONDITION_HANDLER(fn) void (fn)(void* arg, bool satisfied, std::string message, std::string comment)

namespace sza {
  namespace util {
    
    class ArrayMapDataFrameManager;
    class Coord;
    class CoordRange;
    class RegMapDataFrameManager;
    
    class MonitorPoint {
    public:
      
      /**
       * Constructors.
       */
      MonitorPoint(ArrayMapDataFrameManager* frame, ArrRegMap* regMap, 
		   RegMapBlock* block, CoordRange* range=0);
      MonitorPoint(ArrayMapDataFrameManager* frame, ArrRegMap* regMap, 
		   RegMapBlock* block);
      MonitorPoint(ArrayMapDataFrameManager* frame, ArrRegMap* regMap, 
		   RegMapBlock* block, int index);

      void setTo(ArrayMapDataFrameManager*   frame, ArrRegMap* regMap, 
		 RegMapBlock* block, int index);
      void setTo(ArrayMapDataFrameManager*   frame, ArrRegMap* regMap, 
		 RegMapBlock* block, CoordRange* range=0);
      
      MonitorPoint(RegMapDataFrameManager*   frame, RegMapBlock* block, CoordRange* range=0);
      MonitorPoint(RegMapDataFrameManager*   frame, RegMapBlock* block);
      MonitorPoint(RegMapDataFrameManager*   frame, RegMapBlock* block, int index);

      void setTo(RegMapDataFrameManager*     frame, RegMapBlock* block, int index);
      void setTo(RegMapDataFrameManager*     frame, RegMapBlock* block, CoordRange* range=0);

      void reset();

      /**
       * Destructor.
       */
      virtual ~MonitorPoint();
      
      /**
       * Return the block associated with this monitor point
       */
      inline RegMapBlock* block() {
	return block_;
      }
      
      /**.......................................................................
       * Template write method for monitor points
       */
      template <class type>
      void writeReg(bool isSim, type var,  sza::util::CoordRange* range=0)
	{
	  CoordRange* rng = range==0 ? &coordRange_ : range;
	  
	  if(isArrMap_)
	    arrMapFm_->writeReg(regMap_, block_, var, rng);
	  else
	    regMapFm_->writeReg(block_, var, rng);
	  
	  // Only check handlers if the handler count is non-zero
	  
	  if(handlerCount_ > 0) {
	    dataType_ = var;
	    
	    // Only construct an AxisRange object if a non-default coordinate
	    // range was specified
	    
	    if(range==0)
	      checkHandlers(isSim, axisRange_);
	    else {
	      AxisRange axisRange(block_->axes_, range);
	      checkHandlers(isSim, axisRange);
	    }
	  }
	}
      
      /**.......................................................................
       * Write dcon register values
       */
      void writeDcReg(bool isSim, float f,         sza::util::CoordRange* range=0);

      /**.......................................................................
       * Template read method
       */
      template <class type>
	void readReg(type ptr, sza::util::CoordRange* range=0)
	{
	  CoordRange* rng = range==0 ? &coordRange_ : range;
	  
	  if(isArrMap_)
	    arrMapFm_->readReg(regMap_, block_, ptr, rng);
	  else
	    regMapFm_->readReg(block_, ptr, rng);
	}
      
      // Register a callback associated with a condition
      
      void registerConditionHandler(MONITOR_CONDITION_HANDLER(*handler), 
				    void* arg,
				    std::string message,
				    std::string comment,
				    MonitorCondition& condition, 
				    bool persistent,
				    CoordRange* coordRange=0);
      
      //-----------------------------------------------------------------------
      // This struct will manage condition-handler associations.
      //-----------------------------------------------------------------------

      struct ConditionHandler 
      {
	MonitorCondition condition_;
	MONITOR_CONDITION_HANDLER(*handler_);
	void* arg_;
	std::string message_;
	std::string comment_;
	
	unsigned packetCount_;
	unsigned stablePacketCount_;
	unsigned giveUpPacketCount_;
	bool isPersistent_;

	ConditionHandler();
	ConditionHandler(const ConditionHandler& handler);
	ConditionHandler(ConditionHandler& handler);
	void operator=(ConditionHandler& handler);
	void operator=(const ConditionHandler& handler);
	bool operator==(const ConditionHandler& handler);
	void callHandler(bool conditionWasMet);
	bool checkCondition(bool isSim, DataType& dataType);
	void reset();

	std::string format(std::string& reg);

      };
      

      inline CoordRange coordRange() {
	return coordRange_;
      }

      inline AxisRange axisRange() {
	return axisRange_;
      }

      /**
       * An operator for printing this object
       */
      std::string format(std::string& reg);

      // Query the (first) condition for this monitor point.  This is
      // only meaningful if a single condition is registered for this
      // monitor point

      bool isDelta();
      unsigned nFrame();
      double min();
      double max();
      
    private:
      
      // The register map that represents this monitor point
      
      ArrRegMap* regMap_;

      // The register block that represents this monitor point
      
      RegMapBlock* block_;
      
      // Where applicable, a coordinate representation of the index
      // into the register corresponding to this monitor point
      
      CoordRange coordRange_;
      
      // And an axis range iterator
      
      AxisRange axisRange_;
      
      // Where applicable, a pointer to the shared memory objects
      
      ArrayMapDataFrameManager* arrMapFm_;
      RegMapDataFrameManager*   regMapFm_;

      // True if this object is managing monitor points for an array
      // map.  False if managing monitor points for a reg map

      bool isArrMap_;
      
      // A utility object which will store the values of this monitor
      // point
      
      DataType dataType_;
      
      // A vector of lists of handlers.  The first dimension will
      // index which element of this register, the second will be a
      // list of handlers attached to this monitor point.
      
    public:
      std::vector<std::list<ConditionHandler> > handlers_;
      
    private:

      // True if handlers are registered for this monitor point
      
      int handlerCount_;
      
      Mutex guard_;
      
      // See if a handler has been registered for this monitor point

      void checkHandlers(bool isSim, AxisRange& range);

      // Update the count of handlers for this monitor point
      
      void updateHandlerCount(int count);
      
    }; // End class MonitorPoint
    
  } // End namespace util
} // End namespace sza


#endif // End #ifndef SZA_UTIL_MONITORPOINT_H
