
#include "carma/util/Program.h"
#include "carma/util/BaseException.h"
#include "carma/util/ErrorException.h"
#include "carma/util/FileNotFoundException.h"
#include "carma/util/IllegalArgumentException.h"

#include "carma/services/Selfcal.h"

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>


//
// @version	$Revision: 1.16 $ $Date: 2007/05/02 15:21:48 $
//
// @usage  test program for Selfcal
//
// @description
//	Test program for Selfcal calculations. The input ascii table
//      needs to have 4 or 5 columns:   
//            ant1 ant2 amp phase [weight]
//      where the weight column is optional. See weight= keyword.
//  Set test=1 to see solutions
//
// @logger DEFAULT_FACILITY carma.services.Test.tSelfCal
//
//
// @key   in              "" s  Input file name
// @key   iter             1 i  Number of benchmarks iterations to run
// @key   flux             1 d  Flux of pointsource
// @key   refant           1 i  Reference antenna (use 0 to compute it from largest weight)
// @key   maxant           0 i  Maximum number of antenna ot use
// @key   amp          false b  full Amplitude solution? Defaults to a phase-only solution
// @key   weight       false b  Use weight column (default=false, all weights=1)
// @key   test             0 i  Test (Debug) level (debug= is a system keyword)


//
// Some benchmark comments:
// 9-ant array             phase   amp
// with file I/O:  10,000 in 5.0"   6.0"
// only getGains():          0.6"   1.5"

using namespace carma::util;
using namespace carma::services;
using namespace std;

int carma::util::Program::main()
{
  string fileName = getStringParameter("in");
  int niter   = getIntParameter("iter");
  double flux = getDoubleParameter("flux");
  int refant  = getIntParameter("refant");
  int maxant  = getIntParameter("maxant");
  bool useAmp = getBoolParameter("amp");
  bool useWt  = getBoolParameter("weight");
  int debug   = getIntParameter("test");

  if (fileName.size() == 0) 
    return EXIT_SUCCESS;

  cout.setf(ios::fixed);
  
  try {
    Selfcal s;
    s.setReferenceAntenna(refant);
    s.setDebug(debug);
    s.setPointSourceModel(flux);
    s.setMaxAnt(maxant);

    while (niter-- > 0) {      // iterate the same to get enough for benchmark
    
      std::ifstream inFile(fileName.c_str());
      std::string line;
      if (!inFile) {
        cerr << "Trouble opening file " << fileName << endl;
        exit(1);
      }

      s.zero();

      while (1) {                // read lines and add these as visibilities
        getline(inFile,line);
        if (inFile.eof()) break;
        if (line[0] == '#') continue;
        istringstream iss(line);
        int a1,a2;
        double vr,vi,wt;
        if (useWt)
            iss >> a1 >> a2 >> vr >> vi >> wt;
        else {
            iss >> a1 >> a2 >> vr >> vi;
            wt = 1.0;
        }
        if (debug>1) cout << "LINE: " << a1 << " " << a2 << " " << vr << " " << vi << " " << wt << endl;
            Complex vis(vr,vi);
            s.setVis(a1,a2,vis,wt);
        }
        inFile.close();

        std::vector<Complex> g = s.getVis(useAmp);
        std::vector<Complex> e = s.getVisErrors();
        if (g.size() > 0) {
            for (unsigned int i=0; i<g.size(); i++) 
                if (debug) {
		    double snr = abs(e[i]) > 0.0 ? abs(g[i])/abs(e[i]) : 0.0;
                    cout << "gain[" << i << "]=" 
                         << setprecision(2) << setw(15) << g[i] 
                         //<< " norm=" << norm(g[i]) 
                         << " abs=" << abs(g[i]) 
                         << " error: " << e[i] << " abs=" << abs(e[i])  
                         //<< " arg=" << arg(e[i])* 57.2957795130 
                         << " SNR=" << snr << endl;
                }
            if (niter == 0) {
                double rmsPhase = s.getRMSPhase();
                cout << "RMS Phase=" << rmsPhase << endl;
                if (useAmp) {
                    double rmsAmp = s.getRMSAmp();
                    cout << "RMS Amp=" << rmsAmp << endl;
                }
                cout << "Iters=" << (int)s.getIter() << endl;
            }
        } else {
            cout << "Failed to converge...." << endl;
        }
    }
  } catch (const BaseException& be) {
    cout << "### Program exception: " << be.getMessage() << endl;
    return EXIT_FAILURE;
  } catch (...) {
    cout << "### Program exception: an unspecified error" << endl;
    return EXIT_FAILURE;
  }
  
  return EXIT_SUCCESS;
}
