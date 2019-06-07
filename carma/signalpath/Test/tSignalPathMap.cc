#include <iostream>
#include <iomanip>

#include <cmath>

#include "carma/signalpath/SignalPathMap.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/Program.h"
#include "carma/szautil/XtermManip.h"

using namespace std;
using namespace sza::util;
using namespace carma::signalpath;

PROGRAM_KEYWORDS = {
  { "conf",      "carma23",  "s", USAGE "Configuration to select"},
  { "selectconf","t",        "b", USAGE "True to select the named configuration"},
  { "printconf", "t",        "b", USAGE "True to print configuration"},
  { "ant",       "c1",       "s", USAGE "Antenna to print"},
  { "printant",  "f",        "b", USAGE "True to print antenna"},
  { "printdown", "t",        "b", USAGE "True to print down"},
  { "ifspec",    "C1R",      "s", USAGE "Antenna mapping to test"},
  { "corrspec",  "SLCOR1",   "s", USAGE "Correlator mapping to test"},
  { "corrtype",  "any",      "s", USAGE "Correlator type to assert"},
  { "base",      "1",        "i", USAGE "Base index for implicit mapping"},
  { "corrmaptest", "f",      "b", USAGE "True to test IF --> correlator mapping"},
  { "abtest",      "f",      "b", USAGE "True to test Corr --> AstroBand mapping"},
  { "astrospec", "AB1",      "s", USAGE "AstroBand spec to test"},
  { "indtest",     "f",      "b", USAGE "True to test index expression"},
  { "indexpr",     "2*ODD",  "s", USAGE "expression to test"},
  { END_OF_KEYWORDS}
};

PROGRAM_INITIALIZE_USAGE {}

int Program::main()
{
  try {

    SignalPathMap swMap;
    XtermManip xtm;

    swMap.initializeCableMap(getConfFile("signalpath/cableMap.txt"));

    //  swMap.getSwitch("SL11L")->selectChannel(SW_CHAN_3);

    //    swMap.mapAntennaIFToBdc("C20L", "BD12:[1,3,5]");
    //    swMap.mapAntennaIFToBdc("C12R", "BD12:[2,4,6]");
    //    swMap.selectConfiguration("default");
    //    swMap.loadConfiguration("c23", "c23.txt");

    if(Program::getbParameter("selectconf")) {
      swMap.selectConfiguration(Program::getParameter("conf"),
				Program::getiParameter("base"),
				SignalPathMap::corrNameToCorrType(Program::getParameter("corrtype")));
    }

    if(Program::getbParameter("printant")) {
      COUT(swMap.printDownAntenna(Program::getParameter("ant")));
    }

    if(Program::getbParameter("corrmaptest")) {
      swMap.mapAntennaIFToCorr(Program::getParameter("ifspec"), Program::getParameter("corrspec"), true, 
			       Program::getiParameter("base"));
    }

    if(Program::getbParameter("abtest")) {

      swMap.configureAstroBand(Program::getiParameter("base"),
			       Program::getParameter("conf"),
			       SA_1,
			       SignalPathMap::corrNameToCorrType(Program::getParameter("corrtype")));

#if 1
      swMap.configureAstroBand(9,
			       "default",
			       SA_1,
			       SignalPathMap::corrNameToCorrType(Program::getParameter("corrtype")));
      //      swMap.setWalshColumn("C9", 12);
#endif

    }


    if(Program::getbParameter("printconf")) {
      //    swMap.mapAntennaIFToBdc("C6L:D",  "BD6:[1-7;2]");
      //    swMap.mapAntennaIFToBdc("C14L:D", "BD6:[2-8;2]");
      COUT("About to print");

      if(Program::getbParameter("printdown")) {
	COUT(swMap.printDown());
      } else {
	COUT(swMap.printUp());
      }
      COUT("About to print done");
    } 

  } catch(Exception& err) {
    ReportError(err.what());
  }

  return 0;
}

