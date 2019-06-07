// $Id: SzaShareCorba.h,v 1.4 2011/08/31 04:50:32 eml Exp $

#ifndef SZA_ANTENNA_CORBA_SZASHARECORBA_H
#define SZA_ANTENNA_CORBA_SZASHARECORBA_H

/**
 * @file SzaShareCorba.h
 * 
 * Tagged: Fri Aug 14 11:32:46 PDT 2009
 * 
 * @version: $Revision: 1.4 $, $Date: 2011/08/31 04:50:32 $
 * 
 * @author username: Command not found.
 */
#include "carma/antenna/sza/antenna/control/SzaShare.h"

#include "carma/szautil/Angle.h"
#include "carma/szautil/CoordRange.h"
#include "carma/szautil/DataType.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/Rx.h"
#include "carma/szautil/Temperature.h"

#include "carma/antenna/sza/antenna/corba/Corba.h"

#include "carma/services/AstroTime.h"
#include "carma/antenna/common/DriveControl.h"

namespace sza {
  namespace antenna {
    namespace corba {

      class AntennaCorba;
      class CarmaMonitorPointHandler;

      class SzaShareCorba : public sza::antenna::control::SzaShare {
      public:

	enum Position {
	  NONE,    // No position has been requested
	  AZEL,    // A generic AZEL position command
	  EQUAT,   // Equatorial tracking
	  SERVICE, // Service position
	  STOW,    // stow position
	  SNOW,    // snow track position
	};

	/**
	 * Constructor.
	 */
	SzaShareCorba(std::string host);

	/**
	 * Destructor.
	 */
	virtual ~SzaShareCorba();

	void initialize(AntennaCorba* antCorba);

	double getLst(double utc);

	void setSite(double longitude, double latitude, double altitude);

	//------------------------------------------------------------
	// Methods to handle current receiver logic
	//------------------------------------------------------------

	void setRx(sza::util::Rx::Id rxId);
	void getRx(sza::util::Rx::Id& rxId);
	sza::util::Rx::Id getRx();

	//------------------------------------------------------------
	// Methods to handle tuning logic
	//------------------------------------------------------------

	void setTuningPending(bool pending);
	bool getTuningPending();

	//------------------------------------------------------------
	// Methods to handle current requested position logic
	//------------------------------------------------------------

	void setPosition(Position position);
	void getPosition(Position& position, unsigned& nFrame, bool& pending);

	void incrementFrame(bool acquired);
	void setPending(bool pending);

	void setAmbientTemperature(sza::util::Temperature temp);
	sza::util::Temperature getAmbientTemperature();

	void setCarmaAzOffset(sza::util::Angle azOff);
	void setCarmaElOffset(sza::util::Angle elOff);

	sza::util::Angle getCarmaAzOffset();
	sza::util::Angle getCarmaElOffset();
	double getLastMjdCoefficientsChanged();

	void setCarmaMountAzOffset(sza::util::Angle azOff);
	void setCarmaMountElOffset(sza::util::Angle elOff);

	sza::util::Angle getCarmaMountAzOffset();
	sza::util::Angle getCarmaMountElOffset();

	void setCarmaApertureAzOffset(sza::util::Rx::Id rxId, sza::util::Angle azOff);
	void setCarmaApertureElOffset(sza::util::Rx::Id rxId, sza::util::Angle elOff);
	void setCarmaApertureFlexure(sza::util::Rx::Id rxId, sza::util::Angle flexureSin, sza::util::Angle flexureCos);

	sza::util::Angle getCarmaApertureAzOffset(sza::util::Rx::Id rxId);
	sza::util::Angle getCarmaApertureElOffset(sza::util::Rx::Id rxId);
	void getCarmaApertureFlexure(sza::util::Rx::Id rxId, sza::util::Angle& flexureSin, sza::util::Angle& flexureCos);

	void getFlexure(sza::util::Angle& flexureSin, sza::util::Angle& flexureCos);

	sza::util::Angle getTotalXCollimationOffset();
	sza::util::Angle getTotalYCollimationOffset();

	void setInitialized(bool initialized);
	void getInitialized(bool& initialized);

      private:

	friend class CarmaMonitorPointHandler;

	CarmaMonitorPointHandler* monitorPointHandler_;

	carma::services::AstroTime astroTime_;

	Position lastRequestedPosition_;
	sza::util::Mutex positionGuard_;
	unsigned nSinceLastRequest_;
	bool positionPending_;

	sza::util::Rx::Id rxId_;
	sza::util::Mutex rxGuard_;

	bool initialized_;
	sza::util::Mutex initGuard_;
	
	bool tuningPending_;
	sza::util::Mutex tuningPendingGuard_;

	sza::util::Temperature ambientTemperature_;
	sza::util::Mutex tempGuard_;

	// Offsets as CARMA defines them

	sza::util::Angle azOffset_;
	sza::util::Angle elOffset_;

	sza::util::Angle mountAzOffset_;
	sza::util::Angle mountElOffset_;

	sza::util::Angle rx30GHzApertureAzOffset_;
	sza::util::Angle rx30GHzApertureElOffset_;

	sza::util::Angle rx90GHzApertureAzOffset_;
	sza::util::Angle rx90GHzApertureElOffset_;

	sza::util::Angle rx30GHzApertureFlexureSin_;
	sza::util::Angle rx30GHzApertureFlexureCos_;

	sza::util::Angle rx90GHzApertureFlexureSin_;
	sza::util::Angle rx90GHzApertureFlexureCos_;

	sza::util::TimeVal lastTimeCoefficientsChanged_;
	sza::util::Mutex offsetGuard_;

	// Overloaded methods of RegMapDataFrameManager to pack data
	// of an arbitrary type into the underlying frame.  
	//
	// These will allow us to pack the data into CARMA monitor
	// points under the hood at the same time that they are packed
	// into the SZA data frame

	void packData(RegMapBlock* blk, void* data, sza::util::CoordRange* range, 
		      sza::util::DataType::Type type, bool lock=true);
	
	void packData(std::string board, std::string block, void* data, 
		      sza::util::CoordRange* range, sza::util::DataType::Type type, bool lock=true);
	
	void packValue(RegMapBlock* blk, void* data, sza::util::CoordRange* range, 
		       sza::util::DataType::Type type, bool lock=true);
	
	void packValue(std::string board, std::string block, void* data, 
		       sza::util::CoordRange* range, sza::util::DataType::Type type, bool lock=true);

      }; // End class SzaShareCorba
      
    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_SZASHARECORBA_H
