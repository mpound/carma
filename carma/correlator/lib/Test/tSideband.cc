
// $Id: tSideband.cc,v 1.10 2007/08/25 22:37:07 tcosta Exp $

// tSideband.cc

// test addIn of Sideband object.
// test serial, deserial

#include <netinet/in.h>
#include <sys/time.h>
#include <math.h>
#include <unistd.h>           // for usleep
#include <stdlib.h>
#include <time.h>
#include <complex>
#include <string>
#include "carma/util/ErrorException.h"
#include "carma/correlator/lib/CorrelatorSideband.h"
#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;


void runIt(CorrelatorConfigChecker* ccc);

/**
  *  @description
  *  \nTest addIn method
  *
  *  @usage \nUsage: tSideband
  *
  *  @key f correlator/correlator.conf   s Correlator Config Filename(install or build conf dir path will be prepended)
  *
  *  @author Rick Hobbs
  *  @version $Revision:#
  *
  * @logger TEST_FACILITY carma.test.correlator.lib.tSideband
  */
int Program::main() {
  try {
    string file = getConfFile(getStringParameter("f"));
    CorrelatorConfigChecker* ccc = CorrelatorConfigChecker::getInstance(file);
    ccc->start();
    cerr << "Running..." << endl;
    runIt(ccc);
  } catch (ErrorException& err) {
    cerr << err << endl;
  } catch (...) {
    cerr << "Exception caught" << endl;
    return 1;
  }
  return 0;
}


void runIt(CorrelatorConfigChecker* ccc) {
  cerr << "Inside runIt" << endl;
  CorrelatorSideband * const asb1 =
    new CorrelatorSideband( CorrelatorSideband::AUTO_FLAVOR );
  CorrelatorSideband * const asb2 =
    new CorrelatorSideband( CorrelatorSideband::AUTO_FLAVOR );
  vector<complex<float> > data;
  vector<complex<float> > data2;
  int nChans = 8;
  data.resize(nChans);
  data2.resize(nChans);
  for (int idx = 0; idx < nChans; ++idx) {
    data[idx] = complex<float>(idx, idx + 1);
    data2[idx] = complex<float>(idx, idx + 1);
    cerr << data[idx] << endl;
  }
  cerr << "*****" << endl;
  asb1->setValidAll(true);
  asb1->setData(data);
  CorrelatorStats st1;
  st1.setIntegrationTime(.5);
  asb1->setStats(st1);

  asb2->setData(data2);
  asb2->setValidAll(true);
  st1.setIntegrationTime(.7);
  asb2->setStats(st1);
  // set channel 3 bad
  asb1->setValid(3, false);
  const vector<int>& vd1 = asb1->getDataValid();
  const vector<int>& vd2 = asb2->getDataValid();
  for (int idx = 0; idx < nChans; ++idx) {
    cerr << " asb1[]= " << data[idx]  << " vd1[]= " << vd1[idx]
         << " asb2[]= " << data2[idx] << " vd2[]= " << vd2[idx] << endl;
  }
  cerr << "starting addIn, with channel 3 is bad" << endl;
  asb1->addIn( *asb2 );
  cerr << "finished addIn" << endl;
  const vector<complex<float> >& d = asb1->getData();
  cerr << "asb1 integTime= " << asb1->getStats().getIntegrationTime() << endl;
  for (int idx = 0; idx < nChans; ++idx) {
    cerr << "asb1[]= " << d[idx] << " vd1[]= " << vd1[idx] << endl;
  }
  cerr << "starting addIn again, with channel 3 now good" << endl;
  asb2->setValid(3, true);
  asb1->addIn( *asb2 );
  cerr << "finished addIn" << endl;
  const vector<complex<float> >& d1 = asb1->getData();
  cerr << "asb1 integTime= " << asb1->getStats().getIntegrationTime() << endl;
  for (int idx = 0; idx < nChans; ++idx) {
    cerr << "asb1[]= " << d1[idx] << " vd1[]= " << vd1[idx] << endl;
  }
  // now normalize
  asb1->normalize();
  const vector<complex<float> >& dn = asb1->getData();
  cerr << "normalized..." << endl;
  cerr << "asb1 integTime= " << asb1->getStats().getIntegrationTime() << endl;
  for (int idx = 0; idx < nChans; ++idx) {
    cerr << "asb1[]= " << dn[idx] << " vd1[]= " << vd1[idx] << endl;
  }

  // Test serial, deserial
  //asb1->setNumberOfChans(8);
  cerr << "Starting serialization..." << endl;
  cerr << "numChans= " << asb1->getNumberOfChans() << endl;
  cerr << "size of asb1= " << asb1->getSizeInBytes() << endl;
  vector< char > byteArray;
  asb1->serialIntoByteVec( byteArray );
  cerr << "Finished serialization..." << endl;
  cerr << "size of byteArray= " << byteArray.size() << endl;
  cerr << "Starting deserialization..." << endl;
  CorrelatorSideband * const asb3 =
    new CorrelatorSideband( CorrelatorSideband::AUTO_FLAVOR );
  asb3->deserial(byteArray);
  cerr << "Finished deserialization..." << endl;
  const vector<complex<float> >& d3 = asb3->getData();
  cerr << "size of d3= " << d3.size() << endl;
  for (int idx = 0; idx < nChans; ++idx) {
    cerr << d3[idx] << endl;
  }
}
