
// $Id: tBaselineCopy.cc,v 1.22 2011/05/04 18:15:36 abeard Exp $

// tBaselineCopy.cc

// test Copy of a Baseline object using Copy constructor and assignment


#include <netinet/in.h>
#include <math.h>
#include <unistd.h>           // for usleep
#include <stdlib.h>
#include <time.h>
#include <complex>
#include "carma/correlator/lib/CorrelatorBaseline.h"
#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;

//
// @version $Revision: 1.22 $
//
// @usage Usage: tCorrelatorData
//
// @description
// Time serialization.
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.correlator.lib.tBaselineCopy
//

int runIt();

int Program::main() {
  cerr << "Running..." << endl;
  string file = "conf/correlator/correlator.conf";
  CorrelatorConfigChecker* ccc = 
    CorrelatorConfigChecker::getInstance(file);
  ccc->start();
  for (int idx = 0; idx < 10; ++idx)
    runIt();
  return 0;
}


int runIt() {
  // check copy of baseline object
  CorrelatorBaseline cb2;
  CorrelatorBaseline cb5;
  {
    // create initial baseline objects
    CorrelatorBaseline cb1; // Auto
    cb1.setInput1Number(1);
    cb1.setInput2Number(1);
    CorrelatorSideband asb( CorrelatorSideband::AUTO_FLAVOR );
    asb.setNumberOfLags(1);
    vector<complex<float> > da;
    da.resize(1);
    da[0] = complex<float>(1.,2.);
    asb.setData(da);
    cb1.addSideband(asb);

    CorrelatorBaseline cbc; // Cross
    cbc.setInput1Number(1);
    cbc.setInput2Number(2);
    CorrelatorSideband usb( CorrelatorSideband::UPPER_FLAVOR );
    CorrelatorSideband lsb( CorrelatorSideband::LOWER_FLAVOR );
    usb.setNumberOfLags(1);
    lsb.setNumberOfLags(1);

    da[0] = complex<float>(3.,4.);
    usb.setData(da);
    da[0] = complex<float>(5.,6.);
    lsb.setData(da);
    cbc.addSideband(lsb);
    cbc.addSideband(usb);

    // output initial Baseline
    cout << "initial Baseline(Auto):" << endl;
    cout << "   cb1 input1Number= " << cb1.getInput1Number() << endl;
    cout << "   cb1 input2Number= " << cb1.getInput2Number() << endl;
    cout << "   cb1 is AutoSB= " << cb1.getAutoSideband().isAuto() << endl;
    cout << "   cb1 complex= " << da[0] << endl;

    CorrelatorBaseline cb3(cb1);
    CorrelatorBaseline cb4(cbc);

    // print out some values
    const vector<complex<float> >& da2 = cb3.getAutoSideband().getData();
    cout << "Baseline using Copy Constructor:" << endl;
    cout << "   cb3.input1Number= " << cb3.getInput1Number() << endl;
    cout << "   cb3.input2Number= " << cb3.getInput2Number() << endl;
    cout << "   cb3 is AutoSB= " << cb3.getAutoSideband().isAuto() << endl;
    cout << "   cb3 complex= " << da2[0] << endl;

    const vector<complex<float> >& da2a = cb4.getUpperSideband().getData();
    cout << "Baseline using Copy Constructor:" << endl;
    cout << "   cb4 input1Number= " << cb4.getInput1Number() << endl;
    cout << "   cb4 input2Number= " << cb4.getInput2Number() << endl;
    cout << "   cb4(USB) is USB= " << cb4.getUpperSideband().isUSB() << endl;
    cout << "   cb4 USB complex= " << da2a[0] << endl;

    const vector<complex<float> >& da2b = cb4.getLowerSideband().getData();
    cout << "   cb4(LSB) is LSB= " << cb4.getLowerSideband().isLSB() << endl;
    cout << "   cb4 LSB complex= " << da2b[0] << endl;

    cb2 = cb1;
    cb5 = cbc;
  }
  const vector<complex<float> >& da2 = cb2.getAutoSideband().getData();
  cout << "Baseline Copy using assignment:" << endl;
  cout << "   cb2 input1Number= " << cb2.getInput1Number() << endl;
  cout << "   cb2 input2Number= " << cb2.getInput2Number() << endl;
  cout << "   cb2 is AutoSB= " << cb2.getAutoSideband().isAuto() << endl;
  cout << "   cb2 complex= " << da2[0] << endl;

  const vector<complex<float> >& da2a = cb5.getUpperSideband().getData();
  cout << "Baseline Copy using assignment:" << endl;
  cout << "   cb5 input1Number= " << cb5.getInput1Number() << endl;
  cout << "   cb5 input2Number= " << cb5.getInput2Number() << endl;
  cout << "   cb5(USB) is USB= " << cb5.getUpperSideband().isUSB() << endl;
  cout << "   cb5 USB complex= " << da2a[0] << endl;

  const vector<complex<float> >& da2b = cb5.getLowerSideband().getData();
  cout << "   cb5(LSB) is LSB= " << cb5.getLowerSideband().isLSB() << endl;
  cout << "   cb5 LSB complex= " << da2b[0] << endl;

  // now add cb5 to cb5
  cb5.addIn( cb5 );
  cout << "   cb5+=cb5 input1Number= " << cb5.getInput1Number() << endl;
  cout << "   cb5+=cb5 input2Number= " << cb5.getInput2Number() << endl;
  cout << "   cb5+=cb5(USB) is USB= " << cb5.getUpperSideband().isUSB() << endl;
  cout << "   cb5+=cb5 USB complex= " << da2a[0] << endl;
  cout << "   cb5+=cb5(LSB) is LSB= " << cb5.getLowerSideband().isLSB() << endl;
  cout << "   cb5+=cb5 LSB complex= " << da2b[0] << endl;
  return 0;
}
