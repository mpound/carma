#ifndef CARMA_ANTENNA_BIMA_CMRECEIVER_H
#define CARMA_ANTENNA_BIMA_CMRECEIVER_H

/**
 * @file CMReceiver.h
 * 
 * 
 * @author Doug Friedel
 */

#include "carma/antenna/common/CMReceiver.h"
#include "carma/antenna/bima/BimaCMRxControl_skel.h"

namespace carma{

namespace monitor {
    class RxBias;
}

namespace antenna {
namespace bima {

class Rx;
      
class CMReceiver : public carma::antenna::common::CMReceiver {
public:
	
	/**
	 * Constructor.
	 */
	CMReceiver( carma::canbus::nodeType node,
                carma::canbus::CanOutput & io,
                carma::monitor::RxBias & rxMon,
                carma::monitor::BimaSubsystem::RxBiasTemps & rxBiasTemps,
                carma::antenna::bima::Rx & rx );
	
	/**
	 * Destructor.
	 */
	~CMReceiver( );

	void setDrainVoltage();
	void setDrainCurrent();
	void setIFCurrent();

	void setDrainVoltage(CORBA::Short biasNo, CORBA::Double voltage);
	void setDrainCurrent(CORBA::Short biasNo, CORBA::Double current);
	void setIFCurrent(CORBA::Double current);

private:

	carma::antenna::bima::Rx & rx_;
      
}; // CMReceiver

} } } // namespace carma::antenna::bima

#endif // #ifndef CARMA_ANTENNA_BIMA_CMRECEIVER_H
