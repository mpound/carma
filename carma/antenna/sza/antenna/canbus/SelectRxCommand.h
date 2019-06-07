#ifndef SZA_ANTENNA_CANBUS_SELECTRXCOMMAND_H
#define SZA_ANTENNA_CANBUS_SELECTRXCOMMAND_H

/**
 * @file SelectRxCommand.h
 * 
 * Tagged: Sat Oct 23 13:49:19 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Directives.h"
#include "carma/szautil/Rx.h"

#if DIR_HAVE_CARMA
#include "carma/antenna/sza/antenna/canbus/CanCommand.h"
#endif

namespace sza {
  namespace antenna {
    
    namespace control {
      class AntennaRx;
    }
    
    namespace canbus {
      
      class CalTert;
      class Yig;
      class IFMod;
      class IntMod;
      class BiasTunedGunn;
      class VaractorTunedGunn;
      
      class SelectRxCommand : public CanCommand {
	
      public:
	
	friend class sza::antenna::control::AntennaRx;
	
	/**
	 * Constructors.
	 */
	SelectRxCommand();
	
	/**
	 * Constructor with initialization
	 */
	SelectRxCommand(sza::antenna::control::AntennaRx* parent, 
			sza::util::Rx::Id rxId, unsigned seq, bool moveTertiary=true);
	
	/**
	 * Initialize data members
	 */
	void initialize(sza::antenna::control::AntennaRx* parent=0);
	
	/**
	 * Compile this command
	 */
	void compile(sza::util::Rx::Id rxId, unsigned seq, bool moveTertiary=true);
	void compile(sza::antenna::control::AntennaRx* parent, 
		     sza::util::Rx::Id rxId, unsigned seq, bool moveTertiary=true);
	void run(sza::antenna::control::AntennaRx* parent, 
		 sza::util::Rx::Id rxId, unsigned seq, bool moveTertiary=true);
	
	/**
	 * Destructor.
	 */
	virtual ~SelectRxCommand();
	
      private:
	
	sza::antenna::control::AntennaRx* parent_;
	
	CalTert*            calTert_;
	Yig*                yig_;
	IFMod*              ifMod_;
	IntMod*             intMod_;
	BiasTunedGunn*      gunn_;
	VaractorTunedGunn*  varactor_;
	
      }; // End class SelectRxCommand
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_SELECTRXCOMMAND_H
