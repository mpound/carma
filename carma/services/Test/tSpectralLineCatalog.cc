/** * $Id: tSpectralLineCatalog.cc,v 1.8 2006/10/03 21:16:30 mpound Exp $
 *
 * @usage tSpectralLineCatalog catalog=conf/catalogs/SpectralLine.cat line=CO2 trans="" freqHi=0 freqLo=0
 * @description
 *    quick test to see if SpectralLineCatalog works
 *
 * @key catalog conf/catalogs/SpectralLine.cat s File containing line info
 * @key line     "co"             s name of line
 * @key trans    ""               s transition causing line
 * @key freqLo    50              d lower limit on frequency of line in GHz
 * @key freqHi   300              d upper limit on frequency of line in GHz
 *
 * @logger DEFAULT_FACILITY carma.services.Test.tSpectralLineCatalog
 *
 */

#include "carma/services/SpectralLineCatalog.h"
#include "carma/services/SpectralLineNotFoundException.h"
#include "carma/services/Frequency.h"
#include "carma/util/Program.h"

using namespace std;
using namespace carma;
using namespace carma::util;
using namespace carma::services;

int Program::main() {

  const string fileName = getStringParameter("catalog");
  const string lineName = getStringParameter("line");
  const string transitionName = getStringParameter("trans");

  SpectralLineCatalog catalog;
  try {
      catalog.open(fileName);
  } catch (const ErrorException & ex) {
    cerr << "Problem opening " << fileName << endl;
    cerr << ex.getMessage() << endl;
    return EXIT_FAILURE;
  }

  if (parameterWasSpecified("trans")) {
    SpectralLine line;

    try {
      line = catalog.lookup(lineName, transitionName);
      cout << "Spectral Line: " 
		<< line.getName() << "; "
		<< line.getTransition() << "; "
		<< setprecision(12) 
		<< line.getFrequency() << "; "
		<< endl;
    } catch (const SpectralLineNotFoundException &slex) {
      cerr << slex.getMessage() << endl;
      return EXIT_FAILURE;
    } catch (const ErrorException &ex) {
      cerr << ex.getMessage() << endl;
      return EXIT_FAILURE;
    }
    
  } else {
    double freqHi = getDoubleParameter("freqHi");
    double freqLo = getDoubleParameter("freqLo");
    vector<SpectralLine> lines;

    try {
      lines = catalog.lookup(lineName,
			     Frequency( freqLo, "GHz"), 
			     Frequency( freqHi, "GHz"));
    } catch (const SpectralLineNotFoundException & ex) {
      cerr << ex.getMessage() << endl;
    } catch (const ErrorException & ex) {
      cerr << ex.getMessage() << endl;
    }

    int nLines = lines.size();
    for (int i = 0; i < nLines; i++) {
      cout << "Spectral Line: "
		<< lines[i].getName() << "; "
		<< lines[i].getTransition() << "; "
		<< setprecision(8) 
		<< lines[i].getFrequency() << "; "
		<< endl;
    }
  }

  return EXIT_SUCCESS;
}
