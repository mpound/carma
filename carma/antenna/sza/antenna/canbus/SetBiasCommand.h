#ifndef SZA_ANTENNA_CANBUS_SETBIASCOMMAND_H
#define SZA_ANTENNA_CANBUS_SETBIASCOMMAND_H

/**
 * @file SetBiasCommand.h
 * 
 * Tagged: Mon Oct 25 23:55:05 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/AntNum.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/Rx.h"

#include "carma/antenna/sza/antenna/canbus/CanCommand.h"
#include "carma/antenna/sza/antenna/canbus/Receiver.h"

namespace sza {
  namespace antenna {
    
    namespace control {
      class AntennaRx;
    }
    
    namespace canbus {
      
      class Receiver;
      class CanMonitorPoint;
      
      class SetBiasCommand : public CanCommand {
      public:
	
	/**
	 * Constructor.
	 */
	SetBiasCommand();
	
	SetBiasCommand(sza::antenna::control::AntennaRx* parent, 
		       sza::util::Rx::Id rxId, unsigned seq=0);
	
	/**
	 * Destructor.
	 */
	virtual ~SetBiasCommand();
	
	void compile(sza::util::Rx::Id rxId, unsigned seq);
	void compile(sza::antenna::control::AntennaRx* parent, 
		     sza::util::Rx::Id rxId, unsigned seq);
	void run(sza::antenna::control::AntennaRx* parent, 
		 sza::util::Rx::Id rxId, unsigned seq);
	
	void setDefaultBias(sza::util::AntNum::Id antId,
			    sza::antenna::canbus::Receiver::Amp amp, 
			    short bias);

      private:
	
	static const short biasLimit_ = 50; // No clue!
	
	static const unsigned nBias_ = 8;
	
	static sza::antenna::canbus::Receiver::Amp biasTypes30GHz_[nBias_];
	
	static short biasVals30GHz_[nBias_][sza::util::AntNum::NANT];
	
	static Receiver::Amp biasTypes90GHz_[nBias_];
	
	static short biasVals90GHz_[nBias_][sza::util::AntNum::NANT];
	
	unsigned iAnt_;
	
	// 30 GHz monitor points
	
	CanMonitorPoint* drainCurrent30GHz_;
	CanMonitorPoint* gateVoltage30GHz_;
	CanMonitorPoint* gateCurrent30GHz_;
	CanMonitorPoint* ifAmpVoltage30GHz_;
	CanMonitorPoint* ifAmpCurrent30GHz_;
	CanMonitorPoint* mixerCurrent30GHz_;
	CanMonitorPoint* ledCurrent30GHz_;
	
	// 90 GHz monitor points
	
	CanMonitorPoint* gateCurrent90GHz_;
	CanMonitorPoint* drainCurrent90GHz_;
	CanMonitorPoint* ifAmpDrainCurrent90GHz_;
	CanMonitorPoint* ifAmpGateCurrent90GHz_;
	
	friend class sza::antenna::control::AntennaRx;

	sza::antenna::control::AntennaRx* parent_;
	
	Receiver* receiver_;
	
	// Return a pointer to the location in the appropriate bias
	// array, of this amplifier

	Receiver::Amp biasTypeOf(sza::util::Rx::Id rxId, unsigned iBias);
	short biasValOf(sza::util::Rx::Id rxId, unsigned iBias);
	short biasLimitOf(sza::util::Rx::Id rxId, unsigned iBias);
	CanMonitorPoint* monitorPointOf(sza::util::Rx::Id rxId, 
					unsigned iBias);
	
	void initialize(sza::antenna::control::AntennaRx* parent=0);
	void compile(sza::util::Rx::Id rxId);
      };
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza

#endif // End #ifndef SZA_ANTENNA_CANBUS_SETBIASCOMMAND_H
