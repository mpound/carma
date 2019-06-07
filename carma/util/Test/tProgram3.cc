
/**
 *
 * Simple test for the Program methods related to getting the
 * directory from which the program was run. 
 *
 * @author: Steve Scott
 * $Id: tProgram3.cc,v 1.18 2006/03/21 22:06:57 tcosta Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <iostream>
#include <vector>
 
#include "carma/util/Program.h"
#include "carma/util/StringUtils.h"
#include "carma/util/ErrorException.h"

//
// @version $Revision: 1.18 $ $Date: 2006/03/21 22:06:57 $ 
//
// @usage test of methods to get root and conf directories in Program
//
// @logger TEST_FACILITY  carma.test.util.tProgram3
//
// @key antennaNo 1 int  which one am I?
//
// @description
//  Test program to get root, conf and related directories in Program.
//  Just prints them to stdout.
//

using namespace ::std;
using namespace carma::util;


int
Program::main( ) {
    try {
        const int antennaNo = getIntParameter( "antennaNo" );
        // getDoubleParameter( "antennaNo" );
        
        {
            ostringstream oss;
    
            oss << getLogname() << antennaNo << ".testProgram" ;
    
            setInstanceLogname( oss.str( ) );
        }
        
        cerr << " exe:     " << getExecutable() << endl;
        cerr << " exedir:  " << getExecutableDir() << endl;
        cout << " root:    " << getRootDir() << endl;
        cout << " confDir: " << getConfDir() << endl;
        cout << " confFile:" << getConfFile("bogusFile") << endl;
        cout << " confFile:" << getConfFile("-bogusFile") << endl;
        cout << " confFile:" << getConfFile("/bogusFile") << endl;
    
        /*
        vector<string> v = StringUtils::tokenize("Oct 28 10:16:20 teddi Trace:  {INFO} {carma.test.util.tProgram3} {} {Starting, pid=6062}","{"); 
        for(int i = 0; i < v.size(); i++ ) 
        cout << v[i] << endl;
        */
    } catch ( const ErrorException & e ) {
        cerr << "Caught exception\n " << e << "Exiting with failure" << endl;
        exit(EXIT_FAILURE);
    }

    return EXIT_SUCCESS;
}
