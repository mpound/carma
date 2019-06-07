#include "carma/monitor/AstroSubsystemExt.h"

using namespace carma::monitor;

namespace {

    void 
    setSnapshotAverage( AstroSubsystem::SelfCal & selfCal )
    {

        selfCal.antVis().setSnapshotAverage( true );
        selfCal.antVisErr().setSnapshotAverage( true );
        selfCal.snr().setSnapshotAverage( true );

    }; // setSnapshotAverage

} // namespace < unnamed >


AstroSubsystem::AstroSubsystem( SystemFrameBuffer * const buffer ) :
    AstroSubsystemBase( buffer )
{
    // Set snapshot average attribute on all integration derived MPs.
    // This is done in order to avoid averaging mps which are already based
    // on correlator data timescales.  
    for ( int a = 0; a < antennaCount(); ++a ) {

        Antenna & ant = antenna( a );

        for ( int b = 0; b < ant.bandCount(); ++b ) {

            Band & band = ant.band( b );

            setSnapshotAverage( band.leftPol().usb().selfCal() );
            setSnapshotAverage( band.leftPol().lsb().selfCal() );
            setSnapshotAverage( band.rightPol().usb().selfCal() );
            setSnapshotAverage( band.rightPol().lsb().selfCal() );
        }
    }
} // AstroSubsystem::AstroSubsystem

AstroSubsystem::~AstroSubsystem( ) 
{

}
