#include <ace/Log_Msg.h>

#include "carma/correlator/obsRecord2/aceUtils.h"
#include "carma/util/Program.h"

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::obsRecord2;
using namespace carma::util;


//
// @author Tom Costa
// @version $Revision: 1.1 $
// @description Plays with ACE logging redirection.
//
// @usage use it.
//
// @key redirectAceLogging false bool
//                         Whether to redirect ACE logging
//
// @logger TEST_FACILITY carma.test.correlator.obsrecord2.tAceLoggingRedirect
//


int
Program::main( )
{
    const bool redirectAceLogging = getBoolParameter( "redirectAceLogging" );

    if ( redirectAceLogging )
        installAceLoggingBackend();

    ACE_ERROR((LM_DEBUG, "Debug level message\n"));
    ACE_ERROR((LM_INFO, "Info level message\n"));
    ACE_ERROR((LM_WARNING, "Warning level message\n"));
    ACE_ERROR((LM_ERROR, "Error level message\n"));

    return 0;
}
