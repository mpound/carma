/**
 * $Id: tInterpolator.cc,v 1.1 2008/02/01 22:08:21 mpound Exp $
 *
 * @usage tInterpolator
 * @description
 *   test to see if Inteprolator works
 *
 * @noKeys
 *
 * @logger DEFAULT_FACILITY carma.services.Test.tInterpolator
 */

#include "carma/services/DecAngle.h"
#include "carma/services/Interpolator.h"
#include "carma/services/Frequency.h"
#include "carma/services/Types.h"
#include "carma/services/UvTrack.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include <vector>
#include <cmath>

using namespace carma::services;
using namespace carma::util;
using namespace std;

int Program::main() {
    try {
	vector<double> x;
	vector<double> y;
	for (double i = 0 ;i < 100; i++ ) {
	    x.push_back( i + 0.5 * sin (i) );
	    y.push_back( i + cos (i * i) );
	}
	Interpolator interp(x, y, CSPLINE);
	Interpolator interpline(x, y, LINEAR);
	for( double xi = x.at(0); xi < x.at(9); xi+=0.1) {
	   double yi = interp.evaluate( xi );
	   double lyi = interpline.evaluate( xi );
	   printf ("%.3f   %.3f  %.3f\n", xi, yi, lyi);
        }

	// periodic test.
	x.clear();
	y.clear();
	x.push_back(0.00);
        x.push_back(0.10);
        x.push_back(0.27);
        x.push_back(0.30);

	/* Note: first = last for periodic data */
	y.push_back(0.15);
	y.push_back(0.70);
	y.push_back(-0.10);
	y.push_back(0.15);
        const size_t N = x.size();
	Interpolator interp2(x, y, CSPLINE_PERIODIC);
	const double max = 25.0;
	for (double i = 0; i <= max ; i++) {
	   double xi = (1 - i / max) * x[0] + (i / max) * x[N-1];
	   double yi = interp2.evaluate( xi );
	   printf ("%.3f   %.3f\n", xi, yi);
	}

	cout << "UVTRACK C 90: " <<endl;
	UvTrack uvt("C",Frequency(90.0,"GHz") );
	DecAngle dec( 13, "degrees");
	cout << uvt.optimalLength(  dec )
	     << "   "
	     << uvt.maxLength()
	     << "   "
	     << uvt.policyLength( dec )
	     << endl;

	cout << " DEC   SPLINE   MAX POLICY " << endl;
	for( double d = -29;d < 90; d+=1.234 ) {
	    cout << d << "   "
		 << uvt.optimalLength( d )
		 << "   "
		 << uvt.maxLength()
		 << "   "
		 << uvt.policyLength( d )
		 << endl;
	}

	try {
	    uvt.optimalLength( -45 );
	} catch ( const util::ErrorException & domainError ) {
	    cout << " Correctly caught domain error: " 
		<< domainError.getMessage() << endl;
	}

	cout << "UVTRACK E 115.3: " <<endl;
	UvTrack uvt1("E",Frequency(115.271,"GHz") );
	cout << uvt1.optimalLength(  dec )
	     << "   "
	     << uvt1.maxLength()
	     << "   "
	     << uvt1.policyLength( dec )
	     << endl;

	cout << " DEC   SPLINE   MAX POLICY " << endl;
	for( double d = -29;d < 90; d+=1.234 ) {
	    cout << d << "   "
		 << uvt1.optimalLength( d )
		 << "   "
		 << uvt1.maxLength()
		 << "   "
		 << uvt1.policyLength( d )
		 << endl;
	}


    } catch ( const util::ErrorException& ee ) {
      cerr << "Program::main : " << ee.getMessage() << endl;
      return EXIT_FAILURE;
    } catch ( ... ) {
      cerr << "Program::main : unclassified exception" << endl;
      return EXIT_FAILURE;
    }


    return EXIT_SUCCESS;
}
