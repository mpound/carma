#include <iostream>
#include "carma/corba/corba.h"
#include "carma/corba/Server.h"
#include "carma/loref/LOReferenceControl_skel.h"
#include "carma/loref/LOReferenceControl_skel_tie.h"
#include "carma/loref/LOReferenceControlImpl.h"

#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/programLogging.h"

using namespace std;
using namespace carma::loref;
using namespace carma::util;

/** @version $Revision: 1.19 $
 *
 * @usage carmaLORefHost [imr=imr] [-- [-ORBDefaultInitRef defref] [-ORBInitRef initref] [-ORBServerId serverid]]
 *
 * @key counter_gpib -1 i GPIB Address of frequency counter, <0 means nonexistant
 * @key switch_gpib -1 i GPIB Address of RF switch, <0 means nonexistant
 * @key synth1_gpib 1 i GPIB Address of synth 1, <0 means nonexistant
 * @key synth1_mux   0 i Mux input for synth 1, 0 means N/A.  Must be 112, 113, 114, 121, 122, 123, or 124
 * @key synth1_8662 false b Indicates whether synth1 is an HP 8662
 * @key synth2_gpib 2 i GPIB Address of synth 2, <0 means nonexistant
 * @key synth2_mux   0 i Mux input for synth 1, 0 means N/A.  Must be 112, 113, 114, 121, 122, 123, or 124
 * @key synth2_8662 false b Indicates whether synth2 is an HP 8662
 * @key synth3_gpib 3 i GPIB Address of synth 0, <0 means nonexistant
 * @key synth3_mux   0 i Mux input for synth 1, 0 means N/A.  Must be 112, 113, 114, 121, 122, 123, or 124
 * @key synth3_8662 false b Indicates whether synth0 is an HP 8662
 * @key emulate false b Emulate hardware
 * @key awdelay  0.20  d Monitor system autowrite delay in seconds.
 *
 * @logger ENVIRONMENT_FACILITY carma.loref.LoReference
 */

int
Program::main()
{
    // Get emulate parameter
    bool emulate = getBoolParameter("emulate");

    CARMA_CPTRACE(Trace::TRACEALL, "carmaLORefHost starting.");

    try {
        // Get keyword values
        int counter_gpib = getIntParameter("counter_gpib");
        int switch_gpib = getIntParameter("switch_gpib");
        int synth_gpib[3];
        synth_gpib[0] = getIntParameter("synth1_gpib");
        synth_gpib[1] = getIntParameter("synth2_gpib");
        synth_gpib[2] = getIntParameter("synth3_gpib");
        int synth_8662[3];
        synth_8662[0] = getBoolParameter("synth1_8662");
        synth_8662[1] = getBoolParameter("synth2_8662");
        synth_8662[2] = getBoolParameter("synth3_8662");
        int synth_mux[3];
        synth_mux[0] = getIntParameter("synth1_mux");
        synth_mux[1] = getIntParameter("synth2_mux");
        synth_mux[2] = getIntParameter("synth3_mux");
        const double autoWriteDelayInS = getDoubleParameter( "awdelay" );
        
        // Create implementation instance
        LOReferenceControlImpl servant(
            counter_gpib,
            switch_gpib,
            synth_gpib,
            synth_8662,
            synth_mux,
            emulate,
            autoWriteDelayInS );

        carma::corba::Server & server = Program::getCorbaServer();
        server.addServant<POA_carma::loref::LOReferenceControl_tie>
            ( servant, loref::LOREF_NAME );
        server.run(false);
        
        return EXIT_SUCCESS;


    } catch (CORBA::Exception &ex) {
        ostringstream os;
        os << "CORBA::UserException "
           << ex._name() << " raised."
           << " Reason - "
           << ex._info().c_str(); // Tao specific

        programLogCriticalIfPossible( os.str() );
        return EXIT_FAILURE;
    } catch (carma::util::ErrorException &eex) {
        programLogCriticalIfPossible( eex.getErrorMessage() );
        return EXIT_FAILURE;
    } catch (...) {
        programLogCriticalIfPossible( "Unknown exception caught in Program::main()") ;
        return EXIT_FAILURE;
    }
    // Should "never" get here, so if we do, return failure
    return EXIT_FAILURE;
}

// vim: set expandtab sw=4 ts=4 :
