/**
 * @author Harold Ravlin
 *
 * @usage binToAscii filename=<binfile>
 *
 * @description
 * Converts frame binary files to ASCII format.
 *
 * @key filename file.mpdat.bin string File name to be converted.
 *
 * @version $Id: binToAscii.cc,v 1.2 2007/03/12 10:11:24 tcosta Exp $
 * @logger DEFAULT_FACILITY carma.dbms.binToAscii
 */

// Standard C/C++ includes
#include <iostream>
#include <set>
#include <sstream>
#include <string>
#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>

// Carma includes
#include "carma/dbms/dbFFIO.h"
#include "carma/util/ErrorException.h"
#include "carma/util/FileUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Namespace directives
using namespace ::std;
using namespace carma::dbms;
using namespace carma::util;


// If the fullName is a link pointing to a file with a name ending in ".bin":
//  Create an ASCII version of the file with the same name, but no ".bin".
static const string binSuffix(".bin");


static void removeSuffix(string &f, const string &suffix)
{  string::size_type n = f.rfind(suffix);
    if(n != string::npos)
      f.erase(n, string::npos);
}


int convertFile(string &binFile)
{ string asciiFile = binFile;   // Name of ASCII file.


  if(binFile.rfind(binSuffix) == string::npos)
  { cerr << "binFile name does not end in .bin!\n";
      return -1;
  }
  
  removeSuffix(asciiFile, binSuffix);


  // If the ASCII file already exists, don't recreate it.
  if(carma::util::FileUtils::exists(asciiFile))
    cout << asciiFile << " already exists.\n";
  else
    dbFFIOb::copyToASCII(binFile, asciiFile, true);


  return 0;
}



int Program::main()
{ 
  try {
    // Get input parameter values
    std::string filename = getStringParameter("filename");

    // Convert file
    convertFile(filename);
    return EXIT_SUCCESS;

  } catch (const carma::util::ErrorException& exc) {
    CARMA_CPTRACE(carma::util::Trace::TRACE3, "ErrorException" << exc);
    return EXIT_FAILURE;
  }
}
