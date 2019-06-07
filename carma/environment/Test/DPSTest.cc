/**
 * @file carma/environment/Test/DPSTest.cc
 *
 * $Id: DPSTest.cc,v 1.2 2011/05/04 20:36:23 iws Exp $
 *
 * CppUnit test fixture for carma::environment::DPS
 *
 * Author: Peter Teuben
 * Version: $Revision: 1.2 $
 * $Date: 2011/05/04 20:36:23 $
 *
 */
#include <cmath>
#include <limits>
#include <iostream>
using namespace std;

#include "DPSTest.h"

#include <carma/services/Angle.h>
#include <carma/services/Physical.h>
#include <carma/services/Units.h>
#include <carma/services/Pressure.h>
#include <carma/services/Temperature.h>
using namespace carma::services;

using namespace CppUnit;

void DPSTest::setUp()
{
  const std::string dev = "bogus";
  const bool reopen = false;
  const bool emulate = true;

  this->tDPS = new carma::environment::DPS(dev, reopen, emulate);
  numeric_limits<float> floatLimits;
  epsilon = floatLimits.epsilon();
}

void DPSTest::tearDown()
{
  delete tDPS;
}

/* vim: set ts=2 sts=2 sw=2 et: */
