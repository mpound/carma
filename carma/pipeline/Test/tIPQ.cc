
// $Id: tIPQ.cc,v 1.1 2011/08/18 23:25:54 abeard Exp $

// tIPQ.cc

// test reading CorrelatorData IPQ


#include <netinet/in.h>
#include <math.h>
#include <unistd.h>           // for usleep
#include <stdlib.h>
#include <time.h>
#include <iomanip>
#include <complex>
#include <vector>
#include <exception>
#include "carma/util/BaseException.h"
#include "carma/util/IPQbasicTypeBuffer.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;

// define strings for Program::main
//
// @version $Revision: 1.1 $
//
// @usage Usage: tIPQ f=<ipq filename>
//
// @description
// IPQ read test
//
// @key config "conf/correlator/slcorrelator.conf" string input correlator conf.
//
// @logger TEST_FACILITY carma.test.pipeline.util.tIPQ
//


void runIt(CorrelatorConfigChecker* ccc);

int Program::main() {
  cerr << "Running..." << endl;
  string filename = getStringParameter( "config" );
  CorrelatorConfigChecker* ccc = 
    CorrelatorConfigChecker::getInstance(filename);
  ccc->start();
  runIt(ccc);
  return 0;
}

void runIt(CorrelatorConfigChecker* ccc) {
  Time time;
  CorrelatorData cd2;

  const int ipqMaxsize = ccc->getIPQmaxsize();
  const int ipqNumberOfElements = ccc->getCatchDataIPQnumberOfElements();
  const string ipqFilename = ccc->getCatchDataIPQfilename();
  cerr << "runIt(): ipqMaxsize= " << ipqMaxsize
       << " ipqNumberOfElements= " << ipqNumberOfElements
       << endl;
  char* byteArray = new char[ipqMaxsize];
  try {
    carma::util::IPQbasicTypeBuffer* ipq = 
        new carma::util::IPQbasicTypeBuffer( byteArray,
                                    ipqMaxsize,
                                    ipqFilename,
                                    false,
                                    ipqNumberOfElements);


    //    for (int idx = 0; idx < 2; ++idx) {
    while(1) {
      try {
        ipq->read();
        cerr << "runIt(): finished ipq read" << endl;
        vector<char> data;
        for (int idx = 0; idx < ipqMaxsize; ++idx)
          data.push_back(byteArray[idx]);
        cerr << "Starting deserialization..." << endl;
        cd2.deserial(data);

        cout << "cd2: numberOfBands= " << cd2.getNumberOfBands() << endl;
        const vector<CorrelatorBand>& bands = cd2.getBands();


        for (unsigned int idx = 0; idx < bands.size(); ++idx) {
          cout << "Band Number: " << bands[idx].getBandNumber() << endl;
        }

        const CorrelatorHeader& head = cd2.getHeader();

        cout << setprecision(12) << "head mjd= " << head.getMJD() << endl;
        cout << "head asmmjd= " << head.getAssembledMJD() << endl;
        cout << "head txmjd= " << head.getTransmissionMJD() << endl;
        cout << "head rxmjd= " << head.getReceivedMJD() << endl;
        cout << "head seq= " << head.getSequenceNumber() << endl;

      } catch ( const BaseException & ex ) {
        cerr << "Base exception caught: " << ex.what() << endl;
      } catch ( const exception & ex ) {
        cerr << "Error while reading ipq: " << ex.what() << endl;
      }
    }
  } catch ( const BaseException & ex ) {
    cerr << "Error creating ipq: " << ex.what() << endl;
  }
}
