/**
 *
 * Interactive testing program for the LO chain calcs
 *
 * @author: Steve Scott
 *
 * $Id: tLOchain.cc,v 1.10 2011/07/22 20:17:04 scott Exp $
 *
 * $CarmaCopyright$
 *
 */
 
// Keyword setup...
// @version     $Revision: 1.10 $ $Date: 2011/07/22 20:17:04 $
// @usage tLOchain freq=xxx (GHz)
// @key freq 100 double Oscillator freq in GHz
// @key sweep false bool Check preferred for all freqs from 68 to 114 GHz
// @key mode  0 int Special modes: 0=normal, 1=SZA
//
// @logger TEST_FACILITY carma.test.control.tLOchain
//

 
#include <iostream>
#include <sstream>
#include <iomanip>

#include "carma/control/LOchain.h"
#include "carma/util/Program.h"

                                                                                 
using namespace std;
using namespace carma::control;
using namespace carma::util;


int Program::main()
{
    double f = getDoubleParameter("freq");
    int mode = getIntParameter("mode");
    cout << "Mode=" << mode << "\n" << endl;
    LOchain lo(Freq(f, Freq::GHz), mode);
    
    cout << "All" << endl;
    cout << lo.toStringAll() << endl;
    
    cout << "Odd only" << endl;
    cout << lo.toString() << endl;
    
    cout << "Preferred" << endl;
    cout << lo.toStringPreferred() << endl;
    cout << endl;
    cout << "Preferred at +/- 0.1 GHz" << endl;
    lo.updateFreq(Freq(f+0.1, Freq::GHz));
    cout << lo.toStringPreferred() << endl;
    lo.updateFreq(Freq(f-0.1, Freq::GHz));
    cout << lo.toStringPreferred() << endl;
    cout << endl;
    
    if (getBoolParameter("sweep")) {
        for (double flo=68; flo<114; flo+=0.1) {
            LOchain loc(Freq(flo, Freq::GHz), mode);
            cout << loc.toStringPreferred() << endl;
        }
    }
    
    
    return EXIT_SUCCESS;
 
}


