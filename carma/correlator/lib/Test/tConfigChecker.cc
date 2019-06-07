// $Id: tConfigChecker.cc,v 1.9 2006/04/26 16:28:45 tcosta Exp $

#include <string>
#include <iostream>
#include <unistd.h> // for sleep
#include "carma/util/ErrorException.h"
#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma::util;
using namespace carma::correlator::lib;

//
// @version $Revision: 1.9 $
//
// @usage Usage: tConfigChecker
//
// @description
// Test for ConfigChecker
//
// @noKeys
//

int runIt(string& filename);

int Program::main() {
  //  string filename = getStringParameter("f");
  string filename("correlator.conf");
  int errRtn = runIt(filename);
  return errRtn;
}

int runIt(string& filename) {
  try {
  string configFile("conf/correlator/" + filename);
  cerr << "configFile=" << configFile << endl;
  // ConfigChecker configChecker(configFile);
  CorrelatorConfigChecker* configChecker = 
    CorrelatorConfigChecker::getInstance(configFile);
  configChecker->start();
  for (int i = 0; i < 4; ++i) {
    cerr << "isDebug(All)= " << configChecker->isDebug("All") << endl;
    // should always be 0 since tConfigChecker is not in file
    cerr << "isDebug(tConfigChecker)= "
         << configChecker->isDebug("tConfigChecker") << endl;
    sleep(3);
  }
  } catch (...) {
    cerr << "caught an exception" << endl;
  }
  return 0;
}
