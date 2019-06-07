
/**
 * $Id: tmathFunctions.cc,v 1.1 2005/12/19 19:58:39 mpound Exp $
 *
 * @usage tmathFunctions
 * @description quick test to see if mathFunctions work
 * @logger DEFAULT_FACILITY carma.services.Test.tmathFunctions
 * @key arg 1.0 d argument for functions
 * @key nsteps 10000 i loop value for timing.
 */

#include "carma/services/mathFunctions.h"
#include "carma/util/BaseException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include <cmath>
#include <iostream>

using namespace carma::services;
using namespace carma::util;
using namespace std;

int carma::util::Program::main() {
    try {
	double arg = getDoubleParameter("arg");
	int nsteps= getIntParameter("nsteps");
	if (nsteps <= 0) nsteps =1 ;
	cout << "jinc(arg)=" << jinc(arg) << endl;
	cout << "j1(arg)=" << j1(arg) << endl;
	cout << "bracewell(arg)=" << bracewellJinc(arg) << endl;
	cout << "chat(arg)=" << chat(arg) << endl;
	cout << "pchat(arg)=" << pchat(arg) << endl;

	// test timing.
        double val;
	double loopEnd = static_cast<double>(nsteps);
	double x;

	///=========================
	/// FOR J1(X < 1)
	///=========================
	double start1 = Time::MJD();
	for( double i=1.0;i <= loopEnd ;i+=1.0) {
	    x = 1.0/i;
	    val = jinc(x);
	}
	double end1= Time::MJD();

	double start2 = Time::MJD();
	for( double i=1.0;i <= loopEnd ;i+=1.0) {
	    x = 1.0/i;
	    val = i*j1(x);
	}
	double end2= Time::MJD();

	val = 0.0;
	for( double i=1.0;i <= loopEnd ;i+=1.0) {
	    x = 1.0/i;
	    val += fabs( jinc(x) - i*j1(x) );
	}
	double averageError = val/loopEnd;

	// subtract off the time it takes to compute 1/i
	double startCorrection = Time::MJD();
	for( double i=1.0;i <= loopEnd ;i+=1.0) {
	    x = 1.0/i;
	}
	double endCorrection = Time::MJD();
	double correction = endCorrection - startCorrection;

	///=========================
	/// FOR J1(X > 1)
	///=========================
	double start3 = Time::MJD();
	for( double i=1.0;i <= loopEnd ;i+=1.0) {
	    val = jinc(i);
	}
	double end3= Time::MJD();

	double start4 = Time::MJD();
	for( double i=1.0;i <= loopEnd ;i+=1.0) {
	    val = j1(i)/i;
	}
	double end4 = Time::MJD();

	val = 0.0;
	for( double i=1.0;i <= loopEnd ;i+=1.0) {
	    val += fabs( jinc(i) - j1(i)/i );
	}
	double averageError1 = val/loopEnd;

	cout << " Time for jinc(x<1) per call (microsec): " 
	    << 86400*1000000*(end1-start1-correction)/loopEnd << endl;
	cout << " Time for j1(x<1)/x per call (microsec): " 
	    << 86400*1000000*(end2-start2-correction)/loopEnd << endl;
	cout << " Time for jinc(x>1) per call (microsec): " 
	    << 86400*1000000*(end3-start3)/loopEnd << endl;
	cout << " Time for j1(x>1)/x per call (microsec): " 
	    << 86400*1000000*(end4-start4)/loopEnd << endl;
	cout << " Average error (x<1): " << averageError << endl;
	cout << " Average error (x>1): " << averageError1 << endl;
    } catch ( const carma::util::BaseException& be) {
	cout << "Caught: "<< be.getMessage() << endl;
	return EXIT_FAILURE;
    } catch ( ... ) {
	cout << "caught unspecified error in tmathFunctions " << endl;
	return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
