#ifndef CARMA_ANTENNA_BIMA_SISRECEIVER_H
#define CARMA_ANTENNA_BIMA_SISRECEIVER_H

#include "carma/antenna/common/SisReceiver.h"

namespace carma {
namespace antenna {
namespace bima {

class AntennaNameResolver;

class SisReceiver : public carma::antenna::common::SisReceiver {
public:

    SisReceiver( carma::canbus::nodeType node,
                 carma::canbus::CanOutput & io,
                 carma::monitor::AntennaCommon & antCommon,
                 carma::monitor::StateMonitorPointEnum & state,
                 carma::monitor::SisReceiver & sis,
                 carma::monitor::Xac & xac,
                 carma::antenna::common::AntennaIF & antIF,
                 AntennaNameResolver & anr );

    virtual ~SisReceiver( );

    virtual void processIVCurvePoint( carma::canbus::DataVector & data );
    virtual void processBlankingFramePacket1( carma::canbus::DataVector &data ) ;
    virtual void processBlankingFramePacket3( carma::canbus::DataVector &data ) ;

protected:

    // Dick - you'll need to put your shared memory classes here.
    SharedMemory * _bimaShm;
    static const int NMAX = 300;
    float rcpVj[NMAX];
    float rcpIj[NMAX];
    AntennaNameResolver & anr_;

}; // class SisReceiver
}}} // namespace carma::antenna::bima
#endif
