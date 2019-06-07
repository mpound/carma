
#include "carma/monitor/CorrelatorPipelineInfoSim.h"
#include "carma/monitor/SlPipelineSubsystemExt.h"
#include "carma/pipeline/util/TsysPipelineInfo.h"
#include "carma/util/Program.h"

#include <iostream>

using namespace carma::util;
using namespace carma::monitor;
using namespace carma::pipeline::util;
using namespace std;

/**
 * @version $Revision: 1.1 $
 * 
 * @usage tTsysPipelineInfo
 *
 * @description Test application for TsysPipelineInfo class.
 * 
 * @key annoy false bool Prints calculated tsys values to console when true.
 *
 * @logger TEST_FACILITY carma.test.pipeline.util.tTsysPipelineInfo
 */

int Program::main ( ) {

    try {

        const bool annoy = Program::getBoolParameter( "annoy" );
        TsysPipelineInfo tsys( carma::monitor::CORRELATOR_SPECTRAL_LINE );
        
        CorrelatorPipelineInfoSim * simSky = new CorrelatorPipelineInfoSim( );
        CorrelatorPipelineInfoSim * simAmb = new CorrelatorPipelineInfoSim( ); 
        
        simAmb->setFrameCount( 1 );
        simSky->setFrameCount( 2 );

        int i = 1;
        for ( ; i <= SlPipelineSubsystem::inputCount( ); ++i ) {

            simSky->setLoadState( 
                    i, 
                    AntennaCommon::CalStateMonitorPointEnum::SKY, 
                    MonitorPoint::VALID );
            simAmb->setLoadState(
                    i,
                    AntennaCommon::CalStateMonitorPointEnum::AMB,
                    MonitorPoint::VALID );

            int b = 1;
            for ( ; b <= SlPipelineSubsystem::Input::bandCount( ); ++b ) { 
                // Create such that Tsys will be monotonically increasing.
                const float skyTp = -15.0;
                const float ambTp = skyTp
                    + 0.3f * ( ( i - 1 ) 
                    * SlPipelineSubsystem::Input::bandCount() + b );
                simSky->setTotalPower( i, b, skyTp, MonitorPoint::VALID ); 
                simAmb->setTotalPower( i, b, ambTp, MonitorPoint::VALID );
            }
        }

        // Use only named shared_ptr objects
        CorrelatorPipelineInfoPtr simSkyInfoPtr( simSky );
        CorrelatorPipelineInfoPtr simAmbInfoPtr( simAmb );

        tsys.updateWithCorrelatorPipelineInfo( simAmbInfoPtr ); 

        // Verify that tsys is still invalid...
        for ( i = 1; i <= SlPipelineSubsystem::Input::bandCount( ); ++i ) {
            int b = 1;
            for ( ; b <= SlPipelineSubsystem::Input::bandCount( ); ++b ) {
                assert( ! tsys.getTsys( b, i ).valid( ) );
            }
        }
                
        tsys.updateWithCorrelatorPipelineInfo( simSkyInfoPtr ); 

        for ( i = 1; i <= SlPipelineSubsystem::inputCount( ); ++i ) {
            int b = 1;
            for ( ; b <= SlPipelineSubsystem::Input::bandCount( ); ++b ) { 
                assert( tsys.getTsys( b, i ).valid( ) );
                if ( annoy ) {
                    cout << "Input " << i << " Band " << b
                        << " Tdsb=" << tsys.getTsys( b, i ).getTsysDsb( )
                        << " Tusb=" << tsys.getTsys( b, i ).getTsysUsb( )
                        << " Tlsb=" << tsys.getTsys( b, i ).getTsysLsb( )
                        << endl;
                }
            }
        }

    } catch ( const ::std::exception & ex ) {
        cerr << ex.what( ) << endl;
    } catch ( ... ) {
        return 1;
    }
    
    return 0;
}
