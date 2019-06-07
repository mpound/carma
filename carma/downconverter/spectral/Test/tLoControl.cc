
#include "carma/canbus/JanzCanIo.h"
#include "carma/downconverter/spectral/LoControl.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/util/Program.h"

#include <cassert>
#include <iostream>

using namespace carma::canbus;
using namespace carma::downconverter;
using namespace carma::monitor;
using namespace carma::util;
using namespace std;

/**
 * @version $Revision: 1.10 $
 *
 * @usage \nUsage: tLoControl
 *
 * @description
 * Basic tests of the carma::downconverter::LoControl class. No hardware is 
 * poked.
 *
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.downconverter.tLoControl
 */
int Program::main( ) {
    
    try {

        SldcSubsystem monitorSys;
        JanzCanIo canIo;

        { // Test creation and tear down.
            carma::downconverter::LoControl loControl( 
                canIo,
                monitorSys.loControlContainer( ).state( ),
                monitorSys.loControlContainer( ).loControl( ),
                monitorSys.loControlContainer( ).xac( ) );
        }

        { // Test processMsg and simulateMsg.
            carma::canbus::Message msg;
            carma::downconverter::LoControl loControl( 
                canIo,
                monitorSys.loControlContainer( ).state( ),
                monitorSys.loControlContainer( ).loControl( ),
                monitorSys.loControlContainer( ).xac( ) );

            MsgIdInfoMap halfSecMids = loControl.getHalfSecMonitors( );
            MsgIdInfoMap::const_iterator i = halfSecMids.begin( );
            for ( ; i != halfSecMids.end( ); ++i ) {
                msgType mid = i->first;
                msg = loControl.simulateMsg( mid );
                DataVector data = msg.getData( );
                loControl.processMsg( mid, data, true );
            }
        }

        { // Test control commands 
            carma::downconverter::LoControl loControl( 
                canIo,
                monitorSys.loControlContainer( ).state( ),
                monitorSys.loControlContainer( ).loControl( ),
                monitorSys.loControlContainer( ).xac( ) );

            assert( loControl.getApiId( ) == loControl.getApi( ) );
            assert( loControl.getApiId( ) == 200 );

            loControl.setLoFrequency( 1, 2.0 );

            try { // Verify invalid frequency throws exception
                loControl.setLoFrequency( 1, 88.5 );
                assert( false );
            } catch ( const UserException & ex ) {
                assert( true );
            } catch ( ... ) {
                assert( false );
            }
        }
            
    } catch ( const std::exception & ex ) {
        cerr << ex.what( );
        return 1;
    } catch ( ... ) {
        return 1;
    }
    return 0;
}
