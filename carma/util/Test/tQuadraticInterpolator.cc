#include <iostream>

#include "carma/util/Program.h"
#include "carma/util/QuadraticInterpolatorNormal.h"
// 
// @version  $Id: tQuadraticInterpolator.cc,v 1.2 2006/02/23 19:45:38 tcosta Exp $
// 
// 
// @usage  testing QuadraticInterpolator(Normal) class
// 
// @description
//  currently only the command line work.
//  An offset (x0) can be given to test when this breaks down,
//  since the interpolator does not use relative offsets from any
//  of the points, but from absolutel 0 !!!
//  When x0 > 1e7 one can expect rounding errors to creep up.
//
// 
// @key x    1.23456789  d       Value to be interpolated against (default: interactive mode)
// @key x0   0           d       Offset, to test accuracy 
// 
// @logger TEST_FACILITY carma.test.util.tQuadraticInterpolator
//

using namespace std;
using namespace carma::util;

int Program::main()
{
  QuadraticInterpolatorNormal quad(0.0);
  double x;
  double x0 = getDoubleParameter("x0");

  cout << "Using offset:   " << x0 << endl;

  quad.extend(0+x0,  0);
  quad.extend(1+x0,  2);
  quad.extend(2+x0,  2);

  //  if (parameterWasSpecified("x")) 
  //  if (hasValue("x"))
  if (1) {
    x = getDoubleParameter("x");
      cout << "Value at    " << x << " is: " << quad.evaluate(x+x0) << endl;
      cout << "Gradient at " << x << " is: " << quad.gradient(x+x0) << endl;
  } else {
      while (cin >> x) {
        cout << "Value at    " << x << " is: " << quad.evaluate(x+x0) << endl;
        cout << "Gradient at " << x << " is: " << quad.gradient(x+x0) << endl;
      }
  }
  return 0;
}
