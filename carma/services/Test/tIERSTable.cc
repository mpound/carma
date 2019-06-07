#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/services/Table.h"
#include "carma/services/IERSTable.h"
#include "carma/util/ErrorException.h"
#include <cstdio>
#include <math.h>
#include <vector>

using namespace std;
using namespace carma::util;
using namespace carma::services;

//
// @version	$Revision: 1.8 $ $Date: 2012/01/25 20:18:00 $ 
//
// @usage	testing IERS tables. 
//
// @description
//      testing IERS tables. By default it will display the
//      range of valid MJD's what this IERS table is good for.
//      For mjd<0 the range in MJD is given.
//      For mjd=0 the current values are given
//      For mjd>0 the values at the requested mjd are given
//      Output is :
//           mjd    dut1[sec] xpolar[arcsec] ypolar[arcsec]        date
//          53535.7 -0.62106     -0.0648         0.3725       Tue Jun 14 00:00:00 2005
//
// @key in      conf/catalogs/IERS.tab s   input filename
// @key mjd     -1                     d   mjd to test for 
// @logger DEFAULT_FACILITY carma.services.Test.tIERSTable
//
//


int Program::main()
{
  string filename, line;
  Table t;

  try {
    IERSTable iers(getStringParameter("in"));
    double mjd = getDoubleParameter("mjd");

    // @todo Time::etDateString() 
    // appears to have a bug : the output time is always 00:00:00

    if (mjd < 0) {
      // only report range of table
      cout << "Min MJD: " << iers.getMinMJD() << " = " 
	   << Time::getDateString(iers.getMinMJD(),"%c") << endl;
      cout << "Max MJD: " << iers.getMaxMJD() << " = " 
	   << Time::getDateString(iers.getMaxMJD(),"%c") << endl;
      double mjd = Time::MJD();
      if (mjd < iers.getMinMJD()) {
	throw CARMA_EXCEPTION(ErrorException,"Current time too early.");
      }
      if (mjd < iers.getMinMJD()) {
	throw CARMA_EXCEPTION(ErrorException,"Current time too late.");
      }
      if ( iers.isOutOfDate() ) 
	cout << "IERS table is overdue for an update; yours is "
	     << iers.age() << " days old" << endl;
    } else {
      // interpolate into the table
      if (mjd == 0) mjd = Time::MJD();      // get current time
      if (mjd >= iers.getMinMJD() && mjd <= iers.getMaxMJD()) {
	double dut1 =  iers.dut1(mjd);
	double xpol =  iers.xpolar(mjd);
	double ypol =  iers.ypolar(mjd);
	cout << "# mjd dut1[s] xpol[as] ypol[as] date" << endl;
	cout << mjd << " " << dut1 << " " << xpol << " " << ypol << " " 
	     << Time::getDateString(mjd,"%c") << endl;
      } else {
	// @todo the upper edge point is counted invalid, it should not
	cout << "MJD " << mjd << " not in range of the IERS table: " << 
	  iers.getMinMJD() << " .. "  << iers.getMaxMJD() << endl;
      }
    }

  } catch (const BaseException & e) {
    cerr << "Progam::main : exception caught: " << e.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (...) {
    cerr << "Program::main : an unknown error occured" << endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}


