#ifndef SZA_ANTENNA_CANBUS_RECEIVER_H
#define SZA_ANTENNA_CANBUS_RECEIVER_H

/**
 * @file Receiver.h
 * 
 * Tagged: Mon Mar 29 11:46:02 PST 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/Amp.h"
#include "carma/szautil/Rx.h"
#include "carma/szautil/Voltage.h"

#include "carma/antenna/sza/antenna/canbus/CanDevice.h"

namespace sza {
  namespace antenna {
    namespace canbus {
      
      class Receiver : public CanDevice {
      public:
	
	enum Amp {
	  Amp30GHzRFStage1Vg  =  0,
	  Amp30GHzRFStage2Vg  =  1,
	  Amp30GHzRFStage3Vg  =  2,
	  Amp30GHzRFStage4Vg  =  3,

	  Amp30GHzRFStage1Id  =  4,
	  Amp30GHzRFStage2Id  =  5,
	  Amp30GHzRFStage3Id  =  6,
	  Amp30GHzRFStage4Id  =  7,
	  		    
	  Amp30GHzIF1         =  8,
	  		    
	  // 90 GHz Amp codes

	  Amp90GHzRF1Stage1Vg =  9,
	  Amp90GHzRF1Stage2Vg = 10,
	  Amp90GHzRF2Stage1Vg = 11,
	  Amp90GHzRF2Stage2Vg = 12,
	  		    
	  Amp90GHzRF1Vd       = 13,
	  Amp90GHzRF2Vd       = 14,
	  		    
	  Amp90GHzIFVd        = 15,
	  Amp90GHzIFVg        = 16
	};

	/**
	 * Constructor.
	 */
	Receiver(sza::antenna::control::SzaShare* share, 
		 std::string boardName,
		 carma::canbus::nodeType node, 
		 carma::canbus::CanOutput& io);
	
	/**
	 * Destructor.
	 */
	virtual ~Receiver();
	
	/**
	 * Set an amplifier input voltage
	 */
	std::vector<carma::canbus::Message>
	  setAmpBias(Amp amp, short bias, bool send=true);

	/**
	 * Set an amplifier input voltage
	 */
	std::vector<carma::canbus::Message>
	  setAmpBias(sza::util::Rx::Id rxType, 
		     sza::util::Amp::Type ampType, 
		     sza::util::Amp::Stage stage, 
		     sza::util::Amp::Bias biasType,
		     short bias, bool send=true);
	
	/**
	 * Start/stop fast sampling.
	 */
	std::vector<carma::canbus::Message>
	  toggleFastSampling(unsigned channel, bool start, bool send=true);

      private:

	//------------------------------------------------------------
	// Blanking-frame (half-second) monitor members.
	//------------------------------------------------------------
	
	/**
	 * Return a map of half-second monitor points.
	 */ 
	std::map<carma::canbus::msgType, std::string> 
	  getHalfSecMonitors() const;

	// Methods to deal with monitor packets for this device.

	void processBlankingFrameMonitor1(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor2(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor3(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor4(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor5(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor6(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor7(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor8(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor9(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor10(std::vector<carma::canbus::byteType>& data, bool isSim);
	void processBlankingFrameMonitor11(std::vector<carma::canbus::byteType>& data, bool isSim);

	//------------------------------------------------------------
	// Commands for this device.
	//------------------------------------------------------------
	
	/**
	 * Engineering commands specific to the YIG
	 */
	enum engineeringCommands 
	  {
	    ENGCMD_SET_AMP_BIAS = 0x080,
	  };

      }; // End class Receiver
      
    } // End namespace canbus
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CANBUS_RECEIVER_H
