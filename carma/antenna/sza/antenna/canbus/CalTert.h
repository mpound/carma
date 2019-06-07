#ifndef SZA_ANTENNA_CANBUS_CALTERT_H
#define SZA_ANTENNA_CANBUS_CALTERT_H

/**
 * @file CalTert.h
 * 
 * Tagged: Thu Jun 17 20:58:48 UTC 2004
 * 
 * @author 
 */
#include "carma/szautil/Angle.h"
#include "carma/szautil/CalPos.h"
#include "carma/szautil/CalTertTypes.h"
#include "carma/szautil/Rx.h"

#include "carma/antenna/sza/antenna/canbus/CanDevice.h"

namespace sza {
  namespace antenna {
    
    namespace control {
      class AntennaRx;
    }
    
    namespace canbus {
      
      class CalTert : public CanDevice {
      public:
	/**
	 * Constructor.
	 */
	CalTert(sza::antenna::control::AntennaRx* parent,
		sza::antenna::control::SzaShare* share, 
		std::string boardName,
		carma::canbus::nodeType node, 
		carma::canbus::CanOutput& io);
	
	/**
	 * Destructor.
	 */
	virtual ~CalTert();
	
	/**
	 * Interface with a 1-wire device
	 */
	virtual std::vector<carma::canbus::Message>
	  oneWireInterface(sza::util::CalTertTypes::OwDevice device, 
			   sza::util::CalTertTypes::OwCommand command, 
			   bool send=true);
	
	/**
	 * Request a calibrator position
	 */
	virtual std::vector<carma::canbus::Message>
	  positionCalibrator(sza::util::CalPos::Pos position, bool send=true);
	
	virtual std::vector<carma::canbus::Message>
	  positionCalibrator(sza::util::CalPos::Pos position, unsigned seq, bool send=true);

	/**
	 * Home the tertiary
	 */
	virtual std::vector<carma::canbus::Message>
	  homeTertiary(bool send=true);
	
	/**
	 * Move the tertiary
	 */
	virtual std::vector<carma::canbus::Message>
	  positionTertiary(sza::util::Rx::Id id, bool send=true);

	virtual std::vector<carma::canbus::Message>
	  positionTertiary(sza::util::Rx::Id id, unsigned seq, bool send=true);
	
	/**
	 * Move the tertiary
	 */
	virtual std::vector<carma::canbus::Message>
	  positionTertiary(unsigned short position, bool send=true);
	
	virtual std::vector<carma::canbus::Message>
	  positionTertiary(sza::util::Angle position, unsigned seq, bool send=true);

	/**
	 * Enable the tertiary
	 */
	virtual std::vector<carma::canbus::Message>
	  enableTertiary(bool enable, bool send=true);
	
	/**
	 * Reset the stepper driver
	 */
	virtual std::vector<carma::canbus::Message>
	  resetStepper(bool send=true);
	
	/**
	 * Write the current encoder position to one of the positions
	 * indexed as 30GHz, 90GHz or 230GHz
	 */
	virtual std::vector<carma::canbus::Message>
	  indexCurrentEncoderPosition(sza::util::Rx::Id id, bool send=true);
	
	/**
	 * Write the current encoder position to one of the positions
	 * indexed as 30GHz, 90GHz or 230GHz
	 */
	virtual std::vector<carma::canbus::Message>
	  setEncoderPositionIndex(sza::util::Rx::Id id, 
				  unsigned short index, bool send=true);
	
	virtual std::vector<carma::canbus::Message>
	  setDefaultEncoderPositionIndex(sza::util::Rx::Id id, bool send=true);
	
	/**
	 * Register the receipt of a control-program command that needs to be
	 * acknowledged when it has taken effect.
	 */
	virtual void registerRequest(unsigned seq);

	virtual void storeEncoderPositionIndex(sza::util::Rx::Id id, 
					       unsigned short index);

      protected:
	
	sza::antenna::control::AntennaRx* parent_;

      }; // End class CalTert
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_CALTERT_H
