/**
 * @file
 * stream test
 *
 * @author: Marc Pound
 * $CarmaCopyright$
 *
 */

// @usage  tStream 
// @description 
// Test to see what streams do if the file is moved
//

#include "carma/util/Program.h"

#include <iostream>
#include <fstream>
#include <unistd.h>

using namespace std;
using namespace carma::dbms;
using namespace carma::util;

int Program::main() {
    try {
	string file="/tmp/junk";
	ifstream inStream(file.c_str());
	if ( ! fileStream_ ) {
	    std::ostringstream os;
	    os << "LogProcessor: cannot open file " << fileName_ ;
	    throw CARMA_EXCEPTION(carma::util::NotFoundException,
		    os.str().c_str());
	}
	std::string line;
	for (int i = 0; i< 5;i ++ ) {
	    getline(fileStream_, line);
	    if ( fileStream_.eof() )  {
		ostringstream err;
		err << "LogProcessor: EOF in file "
		    <<  fileName_
		    << " has been reached.";
		throw CARMA_EXCEPTION(carma::util::EOFException,err.str());
	    }
	    cout << "got line: " << line << end;
	    cout << " you have 10 seconds to move the file and"
		 << " replace it with another one" 
		 << endl;
	    sleep (10);
	}
    } catch (ErrorException& e) {
	cerr < "Caught exception:\n" << e << "\nExiting with failure." <<endl;
	return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;

}

