#ifndef SZA_ANTENNA_CORBA_CARMAMONITORPOINTHANDLER_H
#define SZA_ANTENNA_CORBA_CARMAMONITORPOINTHANDLER_H

/**
 * @file CarmaMonitorPointHandler.h
 * 
 * Tagged: Tue Aug 11 10:42:58 PDT 2009
 * 
 * @version: $Revision: 1.8 $, $Date: 2011/09/06 23:22:25 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/DataType.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/GenericTaskMsg.h"

#include "carma/szaarrayutils/regmap.h"

#if DIR_USE_ANT_CORBA
#include "carma/antenna/sza/antenna/corba/Corba.h"
#include "carma/antenna/sza/antenna/corba/CarmaSeqNum.h"

#include "carma/monitor/MonitorPoint.h"

#include <map>

namespace carma {
  namespace monitor {
    class SzaSubsystem;
  };
};
#endif

#define MP_PACK_FN(fn) void (fn)(CarmaMonitorPointHandler::CarmaMonitorPoint& cmp)

namespace sza {
  namespace antenna {
    namespace corba {

      class AntennaCorba;
      class SzaShareCorba;

      class SzaCanReceivedMonitorPoint {
      public:
	std::string boardName_;
	RegMapBlock* block_;
	bool valid_;

	//------------------------------------------------------------
	// Read the current value of the register managed by this
	// object
	//------------------------------------------------------------

	void read(SzaShareCorba* share);

	//------------------------------------------------------------
	// Return true if any MPs for which this is the controlling
	// monitor point should be written
	//------------------------------------------------------------

	bool isValid() {
	  return valid_;
	}

	//------------------------------------------------------------
	// Invalidate this monitor point
	//------------------------------------------------------------

	void invalidate(SzaShareCorba* share);

	//------------------------------------------------------------
	// Constructor
	//------------------------------------------------------------

	SzaCanReceivedMonitorPoint(RegMapBlock* block, std::string boardName) {
	  block_     = block;
	  boardName_ = boardName;
	  valid_     = true;
	};

	//------------------------------------------------------------
	// Copy constructors and = operators
	//------------------------------------------------------------

	SzaCanReceivedMonitorPoint(SzaCanReceivedMonitorPoint& mp) {
	  *this = mp;
	}

	SzaCanReceivedMonitorPoint(const SzaCanReceivedMonitorPoint& mp) {
	  *this = mp;
	};

	void operator=(const SzaCanReceivedMonitorPoint& mp) {
	  *this = (SzaCanReceivedMonitorPoint&) mp;
	}

	void operator=(SzaCanReceivedMonitorPoint& mp) {
	  block_     = mp.block_;
	  boardName_ = mp.boardName_;
	  valid_     = mp.valid_;
	};

      };

      class CarmaMonitorPointHandler {
      public:

#if DIR_USE_ANT_CORBA
	struct CarmaMonitorPoint {

	  SzaShareCorba* share_;
	  RegMapBlock* szaBlock_;        // The SZA register corresponding to this CARMA monitor point
	  SzaCanReceivedMonitorPoint* szaControlMp_; // If non-null,
						     // points to the
						     // register block
					 // controlling whether or not
					 // this mp should be written
	  carma::monitor::MonitorPoint* carmaMp_;
	  std::vector<carma::monitor::MonitorPoint*> carmaMpVec_;
	  MP_PACK_FN(*packFn_);

	  CarmaMonitorPoint() {
	    share_        = 0;
	    szaBlock_     = 0;
	    szaControlMp_ = 0;
	    carmaMp_      = 0;
	    carmaMpVec_.resize(0);
	    packFn_       = 0;
	  }

	  // To be used only if a) the types are the same, and b) no
	  // unit conversion needs to happen to convert from SZA
	  // monitor point to CARMA monitor point

	  void packData();

	};
#endif

	// CarmaMonitorPoint constructors

	CarmaMonitorPointHandler(AntennaCorba* parent);
	CarmaMonitorPointHandler();
	    
	// CarmaMonitorPoint desstructor

	virtual ~CarmaMonitorPointHandler();
	
	// Write a CARMA sequence number into the monitor stream

	void stageCarmaSeqNo(unsigned long seq, 
			     sza::util::GenericTaskMsg::CarmaSeqNoType type, 
			     bool success);
	
	// Write Common monitor points

	void stageCommonMonitorPoints();

	// Write Specific monitor points

	void stageSpecificMonitorPoints();

	// Stage all other SZA monitor points

	void stageSzaAllMonitorPoints();

	// Return the monitor point (if any) controlling whether
	// another MP should be written

	SzaCanReceivedMonitorPoint* getControlMp(std::string boardName, std::string blockName);

	// Write staged monitor points to the monitor stream

	void writeMonitorPoints();

	// Initialize internal members of this object

	void initialize(sza::antenna::corba::AntennaCorba* parent);

	// Low-level functions to pack data into the monitor stream

	void packData(RegMapBlock* blk, void* data, 
		      sza::util::CoordRange* range, sza::util::DataType::Type type);

	void packValue(RegMapBlock* blk, void* data, 
		       sza::util::CoordRange* range, sza::util::DataType::Type type);

	static MP_PACK_FN(packBool);
	static MP_PACK_FN(packChar);
	static MP_PACK_FN(packUchar);
	static MP_PACK_FN(packShort);
	static MP_PACK_FN(packUshort);
	static MP_PACK_FN(packInt);
	static MP_PACK_FN(packUint);
	static MP_PACK_FN(packFloat);
	static MP_PACK_FN(packDouble);
	static MP_PACK_FN(packString);
	static MP_PACK_FN(packUtc);
	
      private:
	  
	void writeTrackingStateMonitorPoints();
	void writeRxStateMonitorPoint();
	void writeLoStateMonitorPoint();

	void extractCommonMonitorPoints();
	void extractSpecificMonitorPoints();
	void extractSzaCanRcvdMonitorPoints();
	void extractAllSzaMonitorPoints();

	void createMonitorPointMap();

	void assignMpPackFn(CarmaMonitorPoint& mp);

	bool newCaltert_;

	SzaShareCorba* share_;
	  
	bool initialized_;

#if DIR_USE_ANT_CORBA
	
	CarmaSeqNum driveSeqNo_;
	CarmaSeqNum tuneSeqNo_;
	CarmaSeqNum calSeqNo_;
	CarmaSeqNum opticsSeqNo_;
	CarmaSeqNum opticalTelSeqNo_;

	// An object which will serve as our entry point into the
	// antenna monitor system

	carma::monitor::SzaSubsystem* antennaMonitor_;

	// A map of SZA blocks to CARMA monitor points

	std::map<RegMapBlock*, carma::monitor::MonitorPoint*> monitorMap_;

	// Control-system variables

	CarmaMonitorPoint time_;
	CarmaMonitorPoint online_;
	CarmaMonitorPoint antennaInitialized_;

	// Drive monitor points
	  
	CarmaMonitorPoint sourcename_;
	CarmaMonitorPoint rightAscension_;
	CarmaMonitorPoint declination_;
	CarmaMonitorPoint errorSky_;
	CarmaMonitorPoint state_;
	CarmaMonitorPoint safeState_;
	CarmaMonitorPoint safeAzLow_;
	CarmaMonitorPoint safeAzHigh_;
	CarmaMonitorPoint safeElLow_;
	CarmaMonitorPoint safeElHigh_;
	CarmaMonitorPoint mode_;

	// Track monitor points

	CarmaMonitorPoint requestedAzimuth_;
	CarmaMonitorPoint actualAzimuth_;
	CarmaMonitorPoint errorAzimuth_;
	CarmaMonitorPoint errorAzimuthSky_;
	CarmaMonitorPoint azimuthRate_;
	CarmaMonitorPoint requestedElevation_;
	CarmaMonitorPoint actualElevation_;
	CarmaMonitorPoint errorElevation_;
	CarmaMonitorPoint elevationRate_;
	CarmaMonitorPoint wrapLogic_;
	CarmaMonitorPoint emergencyOff_;
	CarmaMonitorPoint manualSwitch_;
	CarmaMonitorPoint trackTolerance_;

	// Point monitor points

	CarmaMonitorPoint offsetAz_;
	CarmaMonitorPoint offsetEl_;
	CarmaMonitorPoint mountOffsetAz_;
	CarmaMonitorPoint mountOffsetEl_;
	CarmaMonitorPoint refraction_;
	CarmaMonitorPoint refractionModel_;
	CarmaMonitorPoint magnitude_;
	CarmaMonitorPoint direction_;
	CarmaMonitorPoint coefChange_;

	CarmaMonitorPoint optElCollErr_;
	CarmaMonitorPoint optCrossElCollErr_;
	CarmaMonitorPoint optSag_;

	CarmaMonitorPoint rx1cmElCollErr_;
	CarmaMonitorPoint rx1cmCrossElCollErr_;
	CarmaMonitorPoint rx1cmSag_;

	CarmaMonitorPoint rx3mmElCollErr_;
	CarmaMonitorPoint rx3mmCrossElCollErr_;
	CarmaMonitorPoint rx3mmSag_;

	CarmaMonitorPoint rx1mmElCollErr_;
	CarmaMonitorPoint rx1mmCrossElCollErr_;
	CarmaMonitorPoint rx1mmSag_;

	CarmaMonitorPoint selectedApert_;

	// Limit monitor points

	CarmaMonitorPoint azSwLimit_;
	CarmaMonitorPoint elSwLimit_;
	CarmaMonitorPoint azHwLimit_;
	CarmaMonitorPoint elHwLimit_;
	CarmaMonitorPoint azLowSwLimitVal_;
	CarmaMonitorPoint azHighSwLimitVal_;
	CarmaMonitorPoint azLowHwLimitVal_;
	CarmaMonitorPoint azHighHwLimitVal_;
	CarmaMonitorPoint elLowSwLimitVal_;
	CarmaMonitorPoint elHighSwLimitVal_;
	CarmaMonitorPoint elLowHwLimitVal_;
	CarmaMonitorPoint elHighHwLimitVal_;

	// LO monitor points

	CarmaMonitorPoint oscFreq_;
	CarmaMonitorPoint yigFreq_;
	CarmaMonitorPoint yigIFLevel_;
	CarmaMonitorPoint yigError_;
	CarmaMonitorPoint yigState_;
	CarmaMonitorPoint yigSweep_;
	CarmaMonitorPoint loFreq_;
	CarmaMonitorPoint loSweep_;
	CarmaMonitorPoint loState_;

	// Receivers monitor points

	CarmaMonitorPoint currentRx_;
	CarmaMonitorPoint rxState_;
	CarmaMonitorPoint rxTsysState_;
	CarmaMonitorPoint rxTsys_;
	CarmaMonitorPoint rxOffsetAz_;
	CarmaMonitorPoint rxOffsetEl_;
	CarmaMonitorPoint compressorState_;
	CarmaMonitorPoint tuneSeqNum_;
	CarmaMonitorPoint dewarTemp_;

	// Location monitor points

	CarmaMonitorPoint latitude_;
	CarmaMonitorPoint longitude_;
	CarmaMonitorPoint altitude_;

	// Optical telescope monitor points

	CarmaMonitorPoint sizeX_;
	CarmaMonitorPoint sizeY_;
	CarmaMonitorPoint offsetX_;
	CarmaMonitorPoint offsetY_;
	CarmaMonitorPoint azFov_;
	CarmaMonitorPoint elFov_;
	CarmaMonitorPoint imageRotation_;
	CarmaMonitorPoint cover_;
	CarmaMonitorPoint centroidSeqNum_;

	// Optics monitor points

	CarmaMonitorPoint polarization_;
	CarmaMonitorPoint opticsSeqNum_;

	// Calibrator monitor points

	CarmaMonitorPoint calState_;
	CarmaMonitorPoint skyTemp_;
	CarmaMonitorPoint ambTemp_;
	CarmaMonitorPoint fixedTemp_;
	CarmaMonitorPoint partialTrans_;
	CarmaMonitorPoint spillOver_;
	CarmaMonitorPoint calSeqNum_;

	// Secondary monitor points

	CarmaMonitorPoint focusState_;
	CarmaMonitorPoint focusZ_;

	// PAM monitor points

	CarmaMonitorPoint totalPamAtten_;
	CarmaMonitorPoint inputPamAtten_;
	CarmaMonitorPoint outputPamAtten_;
	CarmaMonitorPoint ifTotalPower_;

	// Varactor monitor points

	CarmaMonitorPoint varLockStatus_;
	CarmaMonitorPoint varSweepEnabled_;
	CarmaMonitorPoint varGunnEnabled_;
	CarmaMonitorPoint varNoiseMeter_;
	CarmaMonitorPoint varIfLevel_;
	CarmaMonitorPoint varLoopGain_;
	CarmaMonitorPoint varGunnCurrent_;

	// Bias monitor points

	CarmaMonitorPoint biasLockStatus_;
	CarmaMonitorPoint biasSweepStatus_;

	// A vector of CARMA monitor points corresponding to all SZA
	// monitor points

	std::vector<CarmaMonitorPoint> szaSpecificMonitorPoints_;

	// A vector of registers controlling the writing of other
	// registers

	std::vector<SzaCanReceivedMonitorPoint> szaCanRcvdMonitorPoints_;
#endif

      }; // End class CarmaMonitorPointHandler

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_CARMAMONITORPOINTHANDLER_H
