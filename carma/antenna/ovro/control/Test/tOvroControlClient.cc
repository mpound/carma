#include "carma/antenna/common/CalibratorControl.h"
#include "carma/antenna/common/CryoControl.h"
#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/RxSelector.h"
#include "carma/antenna/common/SwitchState.h"
#include "carma/antenna/ovro/control/EnvironmentalControl.h"
#include "carma/antenna/ovro/control/ovroAntennaControl.h"
#include "carma/antenna/ovro/control/ovroCryoControl.h"
#include "carma/antenna/ovro/control/ovroFocusControl.h"
#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/util/Program.h"

#include <cassert>

using namespace carma;
using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::util;
using namespace std;

/**
 * @version $Id: tOvroControlClient.cc,v 1.20 2013/05/13 21:08:38 abeard Exp $
 *
 * @usage tOvroControlClient
 *
 * @description
 * Test client for ovro control classes.  
 *
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.antenna.ovro.tOvroControlClient
 */
int Program::main( ) 
try {
    const string context = "carma.ovro1.";

    corba::Client & client = getCorbaClient();

    { // Exercise Calibrator DO
        CalibratorControl_var cal = 
            client.resolveName< CalibratorControl >(context + CALIBRATOR_NAME);

        const CORBA::ULong dummySeqNo = 0;

        cout << "Exercising the CalibratorControl DO." << endl;
        cal->setPos( CalibratorControl::SKY, dummySeqNo );
        cal->setPos( CalibratorControl::AMBIENT, dummySeqNo );
        cal->setPos( CalibratorControl::FIXEDTEMP, dummySeqNo );
    } 

    { // Exercise the Ovro cryo control DO
        carma::antenna::ovro::CryoControl_var cryo = 
            client.resolveName< ovro::CryoControl >( context + CRYO_NAME );

        cout << "Exercising the ovro::CryoControl DO." << endl;

        cryo->turnCompressor( ON );
        cryo->resetCompressor( );
        cryo->fillCompressor( );
        cryo->purgeCompressor( );
        cryo->turnTempServoLoop( ON );
        cryo->setInletLouverPosition( 1.0 );
        cryo->setOutletLouverPosition( 1.0 );
        cryo->reset( );
    } 

    { // Excercise the Ovro Environment DO.
        ovro::EnvironmentalControl_var enviro = 
            client.resolveName< ovro::EnvironmentalControl >( 
                context + ovro::ENVIRONMENT_NAME );

        cout << "Exercising the ovro::Environment DO." << endl;

        enviro->enableCamera( false );
        enviro->turnSidecabPowerOff( );
        enviro->enable24vPs( 0, false ); // TODO: Verify exception
    }
        
    { // Focus DO
        ovro::FocusControl_var focus = 
            client.resolveName<ovro::FocusControl>( context + FOCUS_NAME );

        cout << "Exercising the ovro::FocusControl DO." << endl;

        focus->setX( 0.51, 1 );
        focus->setY( 0.52, 2 );
        focus->setZ( 0.53, 3 );
        focus->doZTracking( true, 4 );
        focus->cycleLvdtPower( );
        focus->stopMotion( );
        focus->reset( );
    }

    { // Rx

        RxSelector_var rxSelect = 
            client.resolveName< RxSelector >( context + RXSELECTOR_NAME );
        
        cout << "Exercising the RxSelector and Rx DOs." << endl;

        RxControl_var rx = rxSelect->Rx( RxControl::RX1MM );
        
        rx = rxSelect->Rx( RxControl::RX3MM );

        CORBA::ULong seqNo = 0;

        rx->setFrequency( 8.1, 85.0, true, false, false, seqNo++ );

        try {
            rx->setFrequency( 7.9, 85.0, true, true, false, seqNo++ );
            assert( false );
        } catch ( ... ) {
            assert( true );
        }

        try {
            rx->setFrequency( 8.1, 69.0, false, false, false, seqNo++ );
            assert( false );
        } catch ( ... ) {
            assert( true );
        }

        rx->setObservingFrequency( 85.0, seqNo++ );

        rx->measureTotalPower( CalibratorControl::AMBIENT, seqNo++ );

        rx->toggleFastSampling( 1, true );

    }

    { // LO
        LOControl_var lo = client.resolveName< RxSelector >( 
            context + RXSELECTOR_NAME )->Rx( RxControl::RX3MM )->LO( );
        
        cout << "Exercising the LOControl DO." << endl;
    }

    { // IF
        IFControl_var ifc = client.resolveName<RxSelector>( 
            context+RXSELECTOR_NAME)->Rx(RxControl::RX3MM)->IF(RxControl::IF1);

        cout << "Exercising the IFControl DO." << endl;
    }

    { // Front End
        FrontEndControl_var fe = client.resolveName<RxSelector>( 
            context + RXSELECTOR_NAME )->Rx( RxControl::RX3MM )->FrontEnd(
                carma::antenna::common::RxControl::SINGLE );
        
        cout << "Exercising the FrontEndControl DO." << endl;
    } 

    { // Optics
        OpticsControl_var optics = client.resolveName<RxSelector>( 
            context + RXSELECTOR_NAME )->Rx( RxControl::RX3MM )->Optics( );
        
        cout << "Exercising the OpticsControl DO." << endl;
    }

    { // Polarization
        PolarizationControl_var pol = client.resolveName<RxSelector>( 
            context + RXSELECTOR_NAME )->Rx( RxControl::RX3MM )->
            Polarization( );
        
        cout << "Exercising the PolarizationControl DO." << endl;
    }
        
    { // Rx Temperatures


    }

    { // Tiltmeter


    }

    { // Antenna DO and quit.
        ovro::AntennaControl_var ant = 
            client.resolveName<ovro::AntennaControl>( context + ANTENNA_NAME );

        ant->quit( ); // This actually terminates the application.
    }

    return 0;

} catch ( const std::exception & ex ) {
    cerr << ex.what( ) << endl;
    return 1;
} catch ( const CORBA::SystemException & ex ) {
    cerr << ex << endl;
    return 1;
} catch ( const CORBA::Exception & ex ) {
    cerr << ex << endl;
    return 1;
}
