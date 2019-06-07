#include "carma/util/Program.h"
#include "carma/services/Table.h"
#include "carma/util/ErrorException.h"
#include <cstdio>
#include <math.h>
#include <vector>

using namespace std;
using namespace carma::util;
using namespace carma::services;

//
// @version	$Revision: 1.3 $ $Date: 2006/04/29 16:43:34 $ 
//
// @usage	access to the TAI-UTC table
//
// @description
//      testing access to the TAI-UTC table. We don't use it
//      yet, the leap second boundaries are hardcoded in 
// 	the AstroTime class.
//
// @key in      tai-utc.dat s   input filename
// @logger DEFAULT_FACILITY carma.services.Test.tLeap
//


int Program::main()
{
  string filename, line;
  Table t;

  try {
    filename = getStringParameter("in");
    t.open(filename);
    vector<double> jd = t.getDoubleColumn(16,24);
    vector<double> ls = t.getDoubleColumn(36,40);

    for (unsigned int i=0; i<jd.size(); i++) 
      cout << "JD=" << jd[i] << " Leap=" << ls[i] << endl;

  } catch (const carma::util::BaseException& e) {
    cerr << "Progam::main : exception caught: " << e.getMessage() << endl;
  } catch (...) {
    cerr << "Program::main : an unknown error occured" << endl;
    return 1;
  }

  return 0;
}


