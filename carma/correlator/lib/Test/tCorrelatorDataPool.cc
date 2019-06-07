
// $Id: tCorrelatorDataPool.cc,v 1.5 2007/08/26 16:02:21 tcosta Exp $

// tCorrelatorDataPool.cc

// test obtaining CorrelatorData object from a pool.


#include <netinet/in.h>
#include <sys/time.h>
#include <math.h>
#include <unistd.h>           // for usleep
#include <stdlib.h>
#include <time.h>
#include <complex>
#include "carma/correlator/lib/CorrelatorDataPool.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;

//
// @version $Revision: 1.5 $
//
// @usage Usage: tCorrelatorDataPool
//
// @description
// Time serialization.
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.correlator.lib.tCorrelatorDataPoolTest
//

void runIt();

int Program::main() {
  cerr << "Running..." << endl;
  string filename = "conf/correlator/correlator.conf";
  CorrelatorConfigChecker* ccc = 
    CorrelatorConfigChecker::getInstance(filename);
  ccc->start();
  runIt();
  return 0;
}


void runIt() {
  Time time;
  CorrelatorDataPool cdp;
  CorrelatorData* cd; 
  CorrelatorData* cd2;

  cerr << "Size of pool= " << cdp.getPoolSize() << endl;
  cerr << "getting 1 CorrelatorData object" << endl;
  cd = cdp.getCorrelatorData();
  cerr << "   Size of pool= " << cdp.getPoolSize() << endl;
  cerr << "   Number of in use objects= " << cdp.getInUseCount() << endl;
  cerr << "   Number of available objects= " << cdp.getAvailableCount()
       << endl;
  cerr << endl << "releasing CorrelatorData object" << endl;
  cd->decrementRefCount();
  cerr << "   Size of pool= " << cdp.getPoolSize() << endl;
  cerr << "   Number of in use objects= " << cdp.getInUseCount() << endl;
  cerr << "   Number of available objects= " << cdp.getAvailableCount()
       << endl;
  cerr << endl << "getting 2 CorrelatorData objects" << endl;
  cd = cdp.getCorrelatorData();
  cd2 = cdp.getCorrelatorData();
  cerr << "   Size of pool= " << cdp.getPoolSize() << endl;
  cerr << "   Number of in use objects= " << cdp.getInUseCount() << endl;
  cerr << "   Number of available objects= " << cdp.getAvailableCount()
       << endl;
  cerr << endl << "releasing 1 CorrelatorData object" << endl;
  try {
    cd->decrementRefCount();
  } catch (const ErrorException& err) {
    cerr << err << endl;
  }
  cerr << "   Size of pool= " << cdp.getPoolSize() << endl;
  cerr << "   Number of in use objects= " << cdp.getInUseCount() << endl;
  cerr << "   Number of available objects= " << cdp.getAvailableCount()
       << endl;
  cerr << endl << "Incrementing refCount" << endl;
  cd2->incrementRefCount();
  cerr << "   RefCount= " << cd2->getRefCount() << endl;
  cerr << endl << "Decrementing refCount" << endl;
  try {
    cd2->decrementRefCount();
  } catch (const ErrorException& err) {
    cerr << err << endl;
  }
  cerr << "   RefCount= " << cd2->getRefCount() << endl;
  cerr << "   Size of pool= " << cdp.getPoolSize() << endl;
  cerr << "   Number of in use objects= " << cdp.getInUseCount() << endl;
  cerr << "   Number of available objects= " << cdp.getAvailableCount()
       << endl;
  cerr << endl << "Decrementing refCount" << endl;
  try {
    cd2->decrementRefCount();
  } catch (const ErrorException& err) {
    cerr << err << endl;
  }
  cerr << "   RefCount= " << cd2->getRefCount() << endl;
  cerr << "   Size of pool= " << cdp.getPoolSize() << endl;
  cerr << "   Number of in use objects= " << cdp.getInUseCount() << endl;
  cerr << "   Number of available objects= " << cdp.getAvailableCount()
       << endl;
  cerr << "Decrementing RefCount...Should throw an error" << endl;
  try {
    cd->decrementRefCount();
  } catch (const ErrorException& err) {
    cerr << err << endl;
  }
}
