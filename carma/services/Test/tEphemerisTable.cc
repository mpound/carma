#include "carma/util/Program.h"
#include "carma/services/Types.h"
#include "carma/services/EphemerisTable.h"
#include "carma/util/ErrorException.h"
#include <cstdio>
#include <math.h>
#include <vector>

using namespace std;
using namespace carma::util;
using namespace carma::services;

//
// @version	$Revision: 1.6 $ $Date: 2008/10/22 12:12:25 $ 
//
// @usage	testing some very basic Table I/O routines
//
// @description
//      testing CARMA ephemeris tables. Although 7 column SZA tables can be
//      automatically detected, their RA/DEC are in current epoch, not J2000.
//
//
// @key in      @noDefault s   input filename
// @key mjd     0          d   time
// @key maxrows 0          i   maximum number of rows to read from ephemeris table
//
// @logger DEFAULT_FACILITY carma.services.Test.tTable


int Program::main()
{
  // the following line is silly
  if (!parameterWasSpecified("in")) {
    cout << "need in=\n";
    return 0;
  }

  string fileName = getStringParameter("in");
  int maxRows = getIntParameter("maxrows");
  EphemerisTable et;


  try {
    et.open(fileName,maxRows);

    double mjd = getDoubleParameter("mjd");
    et.setMJD(mjd);
    double ra = et.getRa();
    double dec = et.getDec();
    double dist = et.getDistance();
    double dop = et.getDoppler();
    cout << "ra: " << ra << " dec: " << dec << " dist: " << dist << " dop: " << dop << endl;
   
  } catch (const carma::util::BaseException& e) {
    cerr << "Progam::main : exception caught: " << e.getMessage() << endl;
  } catch (...) {
    cerr << "Program::main : an unknown error occured" << endl;
    return 1;
  }

  return 0;
}


