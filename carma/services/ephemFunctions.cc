#include "carma/services/ephemFunctions.h"
#include "carma/services/stringConstants.h"
#include "carma/util/StringUtils.h"
#include "carma/util/FileUtils.h"
#include "carma/util/Program.h"
// $Id: ephemFunctions.cc,v 1.2 2008/10/01 13:53:09 teuben Exp $
using namespace carma::services;
using namespace carma::util;
using namespace std;

bool
carma::services::isEphem(const std::string& sourceName)
{
  std::string efile = EphemFile(sourceName);
  if (efile.empty()) return false;
  return true;
}


std::string
carma::services::EphemFile(const std::string& sourceName)
{
  // we're mapping the sourcename to upper case (by lack of some function
  // to find filenames independant of case)
  // but we're also nice and try the originally supplied name
  string ucSource = StringUtils::lowASCIIAlphaNumericToUpper(sourceName);
  string retval;

  vector <std::string> files;
  vector <std::string>::iterator ifile;

  // push ephem files you want to test on the stack
  files.push_back("/array/rt/catalogs/" + ucSource + ".ephem");
  // files.push.back(Program::getConfDir() + "catalogs/observer/" + sourceName + ".ephem");
  files.push_back(Program::getConfDir() + "catalogs/" + ucSource + ".ephem");
  files.push_back(ucSource + ".ephem");
  // and push the original name as well in these three variants
  files.push_back("/array/rt/catalogs/" + sourceName + ".ephem");
  files.push_back(Program::getConfDir() + "catalogs/" + sourceName + ".ephem");
  files.push_back(sourceName + ".ephem");


  // check which of the files existed, if none, return false
  for (ifile=files.begin(); ifile != files.end(); ifile++)
    if (FileUtils::exists(*ifile)) {
      return *ifile;
    }

  return retval;   // a blank name
}

bool
carma::services::isPlanet(const std::string& sourceName)
{
  std::string lcSource = StringUtils::lowASCIIAlphaNumericToLower(sourceName);

  return (lcSource == "sun"     ||
	  lcSource == "mercury" ||
	  lcSource == "venus"   ||
	  lcSource == "earth"   ||
	  lcSource == "moon"    ||
	  lcSource == "mars"    ||
	  lcSource == "jupiter" ||
	  lcSource == "saturn"  ||
	  lcSource == "uranus"  ||
	  lcSource == "neptune" ||
	  lcSource == "pluto"); // yes, pluto will remain a planet for us
}

bool
carma::services::isFixed(const std::string& sourceName)
{
  // check Ephemeris.cc isFixed()
  string lcSource = StringUtils::lowASCIIAlphaNumericToLower(sourceName);
  if (lcSource == TRANSMITTER ) return true;
  return false;
}
