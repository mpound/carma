#include <cassert>
#include <iostream>

#include "carma/monitor/AntennaCommon.h"
#include "carma/pipeline/Tsys.h"
#include "carma/util/Program.h"

using namespace carma::util;
using namespace carma::pipeline;
using namespace std;

namespace {
    
    typedef carma::monitor::AntennaCommon::CalStateMonitorPointEnum CalStateEnum;

    void printTsys( const Tsys & tsys ) 
    {
        const bool annoy = Program::getProgram().getBoolParameter( "annoy" );
        if ( annoy ) { 
            cout << "Tsys=" << tsys.getTsysDsb( ) << ", "
                 << "Tusb=" << tsys.getTsysUsb( ) << ", "
                 << "Tlsb=" << tsys.getTsysLsb( ) << "." << endl;
        }
    }
} // End namespace <unnamed>

/**
 * @version $Id: tTsys.cc,v 1.1 2011/08/18 23:25:54 abeard Exp $
 *
 * @usage tTsys
 *
 * @description
 * Test application for Tsys class.
 *
 * @key annoy false bool Prints calculated tsys values to console when true.
 *
 * @logger TEST_FACILITY carma.test.pipeline.util.tTsys
 */
int Program::main( ) {

    {
        Tsys tsys;

        assert( !tsys.valid( ) ); // Verify default object isn't valid.

        // Calculate in dB a Y factor of 2.0.
        double Y = 10.0 * log10( 2.0 );

        // Verify that in the case of no atmospheric loss and equal sideband
        // receiver response, Tsys values match those from the first example 
        // in CARMA Memo #33. Results are verified to within a tenth of a K.
        tsys.setReceiverSidebandRatio( 1.0 );
        tsys.setAtmosphericOpacity ( 0.0, 0.0 );
        tsys.setTotalPower( CalStateEnum::AMB, -17.0, 1 );
        assert( !tsys.valid( ) ); // Tsys should still be invalid
        tsys.setTotalPower( CalStateEnum::SKY, ( -17.0 - Y ), 2 ); 
        tsys.calculateTsys( );
        assert( tsys.valid( ) );  // Should finally be valid
        assert( tsys.getTsysDsb( ) > 297.26 && tsys.getTsysDsb( ) < 297.28 );
        printTsys( tsys );
    }
    
    {
        // Verify that Tsys calculates DSB, USB and LSB Tsys values which
        // match those from the example on Pgs 6-8 of CARMA Memo #33. This
        // still assumes equal sideband receiver response. Results are 
        // verified to within a tenth of a K.
        Tsys tsys;
        double Y = 10.0 * log10( 1.79711378 );
        tsys.setLoadTemperature( 300.0 );
        tsys.setOutsideAmbientTemperature( 260.0 );
        tsys.setAtmosphericTemperature( 0.94 * 260.0 );
        tsys.setGroundOpacity( 0.05 );
        // tsys.setDoubleSidebandAtmosphericLoss( 0.533233755 );
        tsys.setReceiverSidebandRatio( 2.0 );
        tsys.setAtmosphericOpacity( 0.5, 0.7 ); // 0.2 dtau
        tsys.setTotalPower( CalStateEnum::SKY, -20.0, 3 );
        tsys.setTotalPower( CalStateEnum::AMB, -20.0 + Y, 4 );
        tsys.calculateTsys( );
        printTsys( tsys );
        assert( tsys.getTsysDsb( ) > 438.81 && tsys.getTsysDsb( ) < 438.82 );
    }

    { 
        // Same as above but verify solution for both USB and LSB and a slightly
        // different Tatmosphere (Memo #33 pg 10).
        Tsys tsys;
        double Y = 10.0 * log10( 1.949800 );
        tsys.setLoadTemperature( 300.0 );
        tsys.setOutsideAmbientTemperature( 260.0 );
        tsys.setAtmosphericTemperature( 260.0 );
        tsys.setGroundOpacity( 0.05 );
        // tsys.setDoubleSidebandAtmosphericLoss( 0.533233755 );
        tsys.setReceiverSidebandRatio( 2.0 );
        tsys.setAtmosphericOpacity( 0.5, 0.7 ); // 0.2 dtau
        tsys.setTotalPower( CalStateEnum::SKY, -20.0, 5 );
        tsys.setTotalPower( CalStateEnum::AMB, -20.0 + Y, 6 );
        tsys.calculateTsys( );
        printTsys( tsys );
        assert( tsys.getTsysDsb( ) > 353.89 && tsys.getTsysDsb( ) < 353.90 );
        assert( tsys.getTsysUsb( ) > 570.0 && tsys.getTsysUsb( ) < 570.1 );
        assert( tsys.getTsysLsb( ) > 933.38 && tsys.getTsysLsb( ) < 933.39 );
    }
    {
        // Ok, let's try to break things now... 
        // TODO: Try harder
        Tsys tsys;
        // Invert SKY and AMB total power measurements. 
        tsys.setTotalPower( CalStateEnum::SKY, -20.0, 7 );
        tsys.setTotalPower( CalStateEnum::AMB, -22.0, 8 );
        tsys.calculateTsys( );
        printTsys( tsys );
        // Identical SKY and AMB 
        tsys.setTotalPower( CalStateEnum::SKY, -20.0, 9 );
        tsys.setTotalPower( CalStateEnum::AMB, -20.0, 10 );
        tsys.calculateTsys( );
        printTsys( tsys );
        // Invalid temperatures...
    } 

    return 0;
}
