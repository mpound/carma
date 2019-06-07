/**
 * $Id: tSourceCatalog.cc,v 1.22 2008/11/24 14:22:06 mpound Exp $
 *
 * @usage tSourceCatalog catalog="" source=""
 * @description
 *   quick test to see if SourceCatalog (and Table, indirectly) work
 *
 * @key  catalog conf/catalogs/SystemSource.cat  s File containing source information
 * @key  source  3c273 s Source name from file listed in `file`
 *
 * @logger DEFAULT_FACILITY carma.services.Test.tSourceCatalog
 */

#include "carma/services/SourceCatalog.h"
#include "carma/services/Types.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"

using namespace carma::services;
using namespace carma::util;
using namespace std;

int Program::main() {
    try {
	if (parameterWasSpecified("source") == false) 	 
	  return 0;

	const string sourceName = getStringParameter("source");

	string fileName = getStringParameter("catalog");

	if ( fileName.empty( ) )
	    fileName = getConfFile("catalogs/SystemSource.cat");
      
	SourceCatalog sourceCatalog;
	Source source;

	  sourceCatalog.open(fileName);
	  source = sourceCatalog.lookup(sourceName);

	  cout << "filename is " 
	       << sourceCatalog.getFileName() 
	       << "[" <<fileName<<"]"<< endl;

	  cout << "Jupiter a planet?" << boolalpha 
	       << Source::isPlanet("jUpiTEr") <<endl;

	  cout << source.getName() << " a planet? " 
	       << boolalpha << source.isPlanet() << endl;

	// should probably make this switch a method in Source.
	string xStr, yStr;
	coordSysType ctype = source.getCoordSysType();

	switch (ctype) {
	    case COORDSYS_RADEC:
	      xStr = "RA";
	      yStr = "Dec";
	      break;
	    case COORDSYS_AZEL:
	      xStr = "AZ";
	      yStr = "EL";
	      break;
	    case COORDSYS_GALACTIC:
	      xStr = "l";
	      yStr = "b";
	      break;
	    default:
	      xStr = "(unknown coordinate type)";
	      yStr = "(unknown coordinate type)";
	      break;
	}

	cout <<
	  "SOURCE " << source.getName() << " " 
	     << xStr << "="
	     << source.getXCoordinate() << " "
	     << yStr << "="
	     << source.getYCoordinate()
	     << "; Comments: "
	     << source.getComments()
	     << endl;

    } catch ( const SourceNotFoundException & snfe ) {
      cerr << "Program::main : " << snfe.getMessage() << endl;
      return EXIT_FAILURE;
    } catch ( const util::NotFoundException & nfe ) {
      cerr << "Program::main : " << nfe.getMessage() << endl;
      return EXIT_FAILURE;
    } catch ( const util::IllegalArgumentException & iae ) {
      cerr << "Program::main : " << iae.getMessage() << endl;
      return EXIT_FAILURE;
    } catch ( ... ) {
      cerr << "Program::main : unclassified exception" << endl;
      return EXIT_FAILURE;
    }


    return EXIT_SUCCESS;
}
