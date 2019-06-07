/**
 *
 * Carma subarray control program.
 * This is the server for all of the carma commands.
 * The interface to control clients is implemented in CORBA.
 *
 * @author: Steve Scott
 *
 * $Id: subarrayControl.cc,v 1.45 2012/03/06 23:58:19 scott Exp $
 *
 * $CarmaCopyright$
 *
 */

//
// @version $Revision: 1.45 $ $Date: 2012/03/06 23:58:19 $
//
// @usage control
//
// @key runSAT true bool
//      if true, run the SAT thread
//
// @key verbose false bool
//      something or other verbose
//
// @key subarrayNo 1 int 
//      Number of the subarray to manage. Should be 1 through 5
//
// @key scriptStateDir /tmp string 
//      Directory for script state files
//
// @logger CONTROL_FACILITY carma.control.subarrayControl
//

#include <iostream>
#include <iomanip>

#include "carma/control/SubarrayControlImpl.h" // this must be first?
#include "carma/control/SubarrayControl.h"
#include "carma/control/SubarrayControl_skel.h"
#include "carma/control/SubarrayControl_skel_tie.h"
#include "carma/control/Subarray.h"

#include "carma/corba/Server.h"

#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ExceptionUtils.h"

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::util;

namespace {

string
getInstanceLogName( const int saNo ) {
    return "carma.control.subarrayControl." +
           SubarrayControlImpl::getAlphanumericSubarrayName( saNo );
}


}  // namespace < anonymous >


int
Program::main( )
try {
    const ScopedLogNdc ndc( "Main Thread" );
    
    const int saNo = getIntParameter( "subarrayNo" );
    
    setInstanceLogname( getInstanceLogName( saNo ) );
    
    const bool runSAT  = getBoolParameter( "runSAT" );
    const bool verbose = getBoolParameter( "verbose" );
    const string scriptStateDir = getStringParameter("scriptStateDir");

    SubarrayControlImpl servant(saNo, verbose, scriptStateDir);

    // start the SAT so tracking is regular
    if ( runSAT ) servant.startSAT( );

    servant.startHSU();

    carma::corba::Server & server= Program::getCorbaServer();
    server.addServant<POA_carma::control::SubarrayControl_tie>
        ( servant, Subarray::makeName(control::SUBARRAY_CONTROL_NAME,saNo) );

    server.run(false);
    
    return EXIT_SUCCESS;
} catch ( ... ) {
    try {
        programLogError( "Coming out of Program::main() on an exception" );
        return EXIT_FAILURE;
    } catch ( ... ) {
        // Just stifle any exception
    }
    
    throw;
}
