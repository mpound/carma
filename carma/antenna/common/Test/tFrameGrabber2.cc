/**
 * @file
 * CppUnit Test harness to test FrameGrabber operations.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2014/02/19 22:08:53 $
 * $Id: tFrameGrabber2.cc,v 1.2 2014/02/19 22:08:53 eml Exp $
 */

#include "carma/corba/corba.h"

#include <iostream>

#include <memory>
#include <vector>

#include <cppunit/TextTestRunner.h>
#include <cppunit/TestFixture.h>
#include <cppunit/extensions/HelperMacros.h>

#include "carma/util/ErrorException.h"

#include "carma/antenna/common/FrameContext.h"
#include "carma/antenna/common/FrameGrabber.h"
#include "carma/antenna/common/OpticalTelCommon.h"
#include "carma/antenna/common/OpticalTelControl.h"
#include "carma/util/Program.h"

#include "carma/szapgutil/PgUtil.h"
#include "carma/szautil/Exception.h"

using namespace carma::antenna::common;
using namespace carma::util;
using namespace sza::util;
using namespace std;

#if 0
#define COUT(statement) \
  {\
  }
#else
#define COUT(statement) \
  {\
    std::ostringstream _macroOs; \
    _macroOs << statement << std::endl; \
    std::cout << _macroOs.str();	\
  }
#endif

void grabAndDisplay(FrameGrabber& grabber);

//
// @key emulate false bool
//      Whether or not to emulate frame grabber
//
// @logger TEST_FACILITY carma.test.antenna.common.tFrameGrabber
//
int Program::main() 
{
  COUT("About to allocate grabber");

  try {
    FrameGrabber grabber("/dev/video0", 0, Program::getBoolParameter("emulate"));

    COUT("About to allocate grabber...done");
    
    grabber.setResolution(FrameGrabber::LO_RES);
    grabAndDisplay(grabber);
    grabber.setResolution(FrameGrabber::MID_RES);
    grabAndDisplay(grabber);
    grabber.setResolution(FrameGrabber::HI_RES);
    grabAndDisplay(grabber);
  } catch(carma::util::ErrorException& err) {
    COUT("Caught an error: " << err.what());
    return 1;
  } catch(sza::util::Exception& err) {
    COUT("Caught an error: " << err.what());
    return 1;
  }

  return 0;
}

void grabAndDisplay(FrameGrabber& grabber)
{
  pair<short, short> res = grabber.getResolution();

  unsigned width_  = res.first;
  unsigned height_ = res.second;

  std::vector<float> dataf(width_ * height_);
  std::vector<float> data(width_ * height_);

  grabber.grabFrame(1, &data);

  for(unsigned iH=0; iH < height_; iH++) {
    for(unsigned iW=0; iW < width_; iW++) {
      unsigned ind     = width_*iH + iW;
      unsigned indflip = width_*(height_-iH) + iW;
      dataf[indflip] = data[ind];
    }
  }

  PgUtil::greyScale(dataf.size(), &dataf[0], width_, height_);
}
