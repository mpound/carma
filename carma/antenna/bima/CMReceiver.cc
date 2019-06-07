#include "carma/antenna/bima/CMReceiver.h"

#include "carma/antenna/bima/Rx.h"

#include "carma/util/programLogging.h"

using namespace std;
using namespace carma;
using namespace carma::antenna;

namespace { 
    
  const carma::canbus::msgType SET_DRAIN_VOLTAGE           = 0x081;
  const carma::canbus::msgType SET_DRAIN_CURRENT           = 0x082;
  const carma::canbus::msgType SET_IF_DRAIN_CURRENT        = 0x083;
  
} // namespace < unnamed >


bima::CMReceiver::CMReceiver( carma::canbus::nodeType node,
                              carma::canbus::CanOutput & io,
                              carma::monitor::RxBias & rxMon,
                              monitor::BimaSubsystem::RxBiasTemps & rxBiasTemps,
                              carma::antenna::bima::Rx & rx ) :
    carma::antenna::common::CMReceiver( node, io, rxMon, &rxBiasTemps ),
    rx_( rx )
{
    // Nothing
}

bima::CMReceiver::~CMReceiver( ) 
{
    // Nothing
}

void 
bima::CMReceiver::setDrainVoltage( )
{
    for( unsigned short int biasNo = 1; biasNo <= 4; ++biasNo ) {
        const double dv = rx_.getGateVoltage(biasNo);
	sendGuardMessage();
	CARMALOGINFO("BIMA: setting cm drain current biasNo = " << biasNo << " dv = " << dv);
        antenna::common::CMReceiver::set30GHzDrainVoltage( biasNo, dv );
    }
}

void 
bima::CMReceiver::setDrainCurrent( )
{
    for( unsigned short biasNo = 1; biasNo <= 4; ++biasNo ) {
        const double dc = rx_.getDrainCurrent( biasNo );
	sendGuardMessage();
	CARMALOGINFO("BIMA: setting cm drain current biasNo = " << biasNo << " dc = " << dc);
        antenna::common::CMReceiver::set30GHzDrainCurrent( biasNo, dc );
    }
}

void 
bima::CMReceiver::setIFCurrent( )
{
    sendGuardMessage();
    antenna::common::CMReceiver::set30GHzIFDrainCurrent( rx_.getIFCurrent( ) );
}

void
bima::CMReceiver::setDrainVoltage(CORBA::Short biasNo, CORBA::Double voltage){
  sendGuardMessage();
  antenna::common::CMReceiver::set30GHzDrainVoltage( biasNo, voltage );
}

void
bima::CMReceiver::setDrainCurrent(CORBA::Short biasNo, CORBA::Double current){
  sendGuardMessage();
  antenna::common::CMReceiver::set30GHzDrainCurrent( biasNo, current );
}

void
bima::CMReceiver::setIFCurrent(CORBA::Double current){
  sendGuardMessage();
  antenna::common::CMReceiver::set30GHzIFDrainCurrent( current );
}
