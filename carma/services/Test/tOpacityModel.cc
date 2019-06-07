
#include "carma/services/Atmosphere.h"
#include "carma/services/SimpleOpacityModel.h"
#include "carma/util/Program.h"

#include <iostream>

using namespace std;
using namespace carma::services;
using namespace carma::util;

namespace {

    void
    testOpacityModel( const OpacityModel * model, 
                      const double freqResolution,
                      const bool print,
                      const double freqLo,
                      const double freqHi ) 
    {
        const double temperature = Atmosphere::DEFAULT_AIR_TEMP;
        const double dewpoint = Atmosphere::DEFAULT_DEW_POINT;
        const double pressure = Atmosphere::DEFAULT_ATM_PRESSURE;
        const double humidity = Atmosphere::DEFAULT_RH;
        double opacity = 0.0;

        for ( double freq = freqLo; freq <= freqHi; freq += freqResolution ) {
            opacity = model->calculateOpacityAtZenith( freq,
                                                       temperature,
                                                       dewpoint,
                                                       pressure,
                                                       humidity );
            if ( print ) {
                cout << freq << " " << opacity << endl;
            }
        } 
    } // testOpacityModel
    
} // namespace < unnamed >


/**
 * @version $Id: tOpacityModel.cc,v 1.1 2010/02/18 22:14:09 abeard Exp $
 *
 * @usage tOpacityModel
 *
 * @description
 * Test application for OpacityModel class.
 *
 * @key res 0.1 double Frequency resolution.
 * @key print false bool Prints calculated opacity values to console when true.
 *
 * @logger TEST_FACILITY carma.test.environment.tOpacityModel
 */
int Program::main( ) {
    
    const bool print = getBoolParameter( "print" ); 
    const double freqResolution = getDoubleParameter( "res" );
    // Verify creation and teardown.
    {
        Waters90GHz waters;
        Woody119GHzO2Line woody;
        SimpleOpacityModel simple;
    }

    { // Calculate opacities for a typical range of observing frequencies.
        Waters90GHz waters;
        if ( print ) cout << "Waters90GHz" << endl;
        testOpacityModel( &waters, freqResolution, print, 80.0, 130.0 );
    }

    { // Calculate opacities for a typical range of observing frequencies.
        Woody119GHzO2Line woody;
        if ( print ) cout << "Woody119GHzO2Line" << endl;
        testOpacityModel( &woody, freqResolution, print, 80.0, 130.0 );
    }

    { // Calculate opacities for a typical range of observing frequencies.
        SimpleOpacityModel simple;
        if ( print ) cout << "Simple" << endl;
        testOpacityModel( &simple, freqResolution, print, 80.0, 270.0 );
    }
    
    return 0;
}
