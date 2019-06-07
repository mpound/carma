
//
// @version	$Revision: 1.5 $ $Date: 2014/11/03 18:52:47 $
// @author Marc Pound
//
// @usage	test the project database manager utilities 
//
// @description
//	testing the pdmUtils methods
//
// @logger TEST_FACILITY carma.test.observertools.tPdmUtils
// @noKeys
//

#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ErrorException.h"
#include "carma/observertools/ItemValue.h"
#include "carma/observertools/PDB_Util.h"
#include <iostream>

using namespace ::std;
using namespace carma;
using namespace carma::util;
using namespace carma::observertools;


namespace {


// test conversion both ways between obsblockId string and ProjectId struct
bool
testConversions( )
{
    
    bool OK = true ;
    const string s("cx123.dr.who.5");
    const ProjectId pid = obsblockIdToProjectId( s );
    const string t = projectIdToObsblockId( pid );
    if ( t != s ) {
	cerr << " conversion failure on " << s << endl;
	OK = false;
    }

    ProjectId dip;
    dip.project     = "a923x";
    dip.obsblock    = "yabba";
    dip.subobsblock = "dabba";
    dip.trial       = 171;
    const string q("a923x.yabba.dabba.171");
    const string v = projectIdToObsblockId( dip );
    if ( q != v ) {
	cerr << " conversion failure on " << q << endl;
	OK = false;
    }

    // try with empty subobsblock
    dip.subobsblock = "";
    const string w("a923x.yabba.171");
    const string x = projectIdToObsblockId( dip );
    if ( w != x ) {
	cerr << " conversion failure on " << w << endl;
	OK = false;
    }

    return OK;
}

// make sure exceptions are thrown under for invalid input
bool
testExceptions( )
{
    bool OK = true;
    const string s("abcdec");
    try {
	ProjectId pid = obsblockIdToProjectId( s );
	OK = false;
    } catch ( util::ErrorException & ex  ) {
	cerr << "Correctly caught: " << ex.getMessage() << endl;
    } catch ( std::exception & foo ) {
	cerr << " caught " <<  foo.what() << endl;
    } catch ( ... ) {
	cerr << " caught unknown in 1 " << endl;
    }
    if ( ! OK ) {
	cerr << " Did not catch too few tokens: " << s << " " <<endl;
    }

    const string s1("abc.dec.0");
    try {
	ProjectId pid = obsblockIdToProjectId( s1 );
	OK = false;
    } catch ( util::ErrorException & ex  ) {
	cerr << "Correctly caught: " << ex.getMessage() << endl;
    } catch ( ... ) {
	cerr << " caught unknown in 2 " << endl;
    }
    if ( ! OK ) {
	cerr << " Did not catch bad trial number: " << s1 << " " <<endl;
    }

    const string s2("abc.dec.asd.asdasd.asdasd.1");
    try {
	ProjectId pid = obsblockIdToProjectId( s2 );
	OK = false;
    } catch ( util::ErrorException & ex  ) {
	cerr << "Correctly caught: " << ex.getMessage() << endl;
    } catch ( ... ) {
	cerr << " caught unknown in 3 " << endl;
    }

    if ( ! OK ) {
	cerr << " Did not catch too many tokens: " << s2 << " " <<endl;
    }
    
    return OK;
}



}  // namespace < anonymous >


int
Program::main( )
{
    bool status = false;
    try {
	status =    testConversions()
		 && testExceptions()
		 ;

    } catch (...) {
      cerr << getArg0() << " caught an exception. bye." <<endl;
      return EXIT_FAILURE;
    }
    cerr << getArg0() << " returning with" 
	 << (status ? " success!" : " FAILURE :-(" )
	 << endl;
    return ( status ? EXIT_SUCCESS : EXIT_FAILURE );

}
