#ifndef CARMA_ANTENNA_COMMON_CMFRONTENDCONTROLIMPL_H
#define CARMA_ANTENNA_COMMON_CMFRONTENDCONTROLIMPL_H

#include "carma/antenna/common/IVCurve.h"
#include "carma/antenna/common/FrontEndControl.h"
#include "carma/corba/corba.h"

namespace carma {
namespace antenna {
namespace common {

class CMReceiver;

class CMFrontEndControlImpl {
public:

    CMFrontEndControlImpl( carma::antenna::common::CMReceiver & cmrx );

    ~CMFrontEndControlImpl( );

    void setFrequency( CORBA::Double gHz );

    void setSISVj( CORBA::Float milliVolts );

    void setSISIj( CORBA::Float microAmps );

    void doIVcurve( CORBA::Float startVjInMv,
                    CORBA::Float stopVjInMv,
                    CORBA::Float stepVjInMv,
                    CORBA::UShort deltaInMs,
                    CORBA::Boolean doPower,
                    CORBA::ULong seqNo );

    carma::antenna::common::IVCurve * getIVCurve( );

    void setVG( carma::antenna::common::FrontEndControl::Amp amplifier,
                carma::antenna::common::FrontEndControl::Stage feStage,
                CORBA::Float volts );

    void setVD( carma::antenna::common::FrontEndControl::Amp amplifier,
                carma::antenna::common::FrontEndControl::Stage feStage,
                CORBA::Float volts );
    
    void setID( carma::antenna::common::FrontEndControl::Amp amplifier,
                carma::antenna::common::FrontEndControl::Stage feStage,
                CORBA::Float milliAmps );

    void setMixer( CORBA::Float volts );

protected:

    // No protected members

private:

    carma::antenna::common::CMReceiver & cmRx_;

}; // class CMFrontEndControlImpl

} } } // namespace carma::antenna::common

#endif
