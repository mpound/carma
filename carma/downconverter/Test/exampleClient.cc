/**@file
 * Example client application for carmaWbdcHost application.
 * Demonstrates, proper setup, retrieval and usage of DOs as well as
 * reasonable error handling mechanisms.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.17 $
 * $Date: 2012/03/13 05:18:07 $
 * $Id: exampleClient.cc,v 1.17 2012/03/13 05:18:07 abeard Exp $
 */
#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/downconverter/common/downconverterSystem.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <fstream>
#include <iostream>
#include <signal.h>
#include <string>
#include <unistd.h>

using namespace std;
using namespace carma::downconverter;
using namespace carma::util;

// Helper function declaration
void setSigAction(); // Needed to bypass carma signal handler.

/** 
 * @version $Revision: 1.17 $
 * 
 * @usage exampleClient [-- [-ORBInitRef initref] [-ORBDefaultInitRef defref] [-ORBServerId serverid]]
 *
 * @description 
 * \nexampleClient is an example Corba client application for the Carma Wideband
 * Downconverter CAN host/CORBA server application.  The example demonstrates 
 * how to retrieve an initial reference to the carma::downconverter::System DO,
 * and then demonstrates how to use this DO to retrieve subsytem DOs. It then
 * operates on subsystem DOs by invoking various request on them.
 * In addition, it demonstrates reasonable error handling mechanisms for a
 * CORBA client application.  It is important to note two things: a) This 
 * example requires that the carmaWbdcHost application be running and b) Both
 * applications use the ORBacus Name Service to publish/retrieve DO references.
 * Name Server parameters are passed via the command line.  
 * \nExample: Using a NameServer on host inyo port 4000.
 * \texampleClient -- -ORBInitRef NameService=corbaloc::inyo:4000/NameService
 * \nExample: If NameService is configured via IMR running on imrhost.
 * \texampleClient -- -ORBDefaultInitRef corbaloc::imrhost.
 * \nSee exampleClient --usage and carmaWbdcHost --description for more info.
 *
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.downconverter.exampleClient
 */
int Program::main() {
    setSigAction(); // Bypass Carma signal handlers
    System_var sys;
    DownconverterControl_var dc;
    QuadModControl_var qm;
    NoiseSourceControl_var ns;
    LoMonitorControl_var lo;

    // Resolve and retrieve a carma::downconverter::System reference
    // and use it to retrieve subobjects.  If we fail to get any of this 
    // stuff on the first try we assume a fatal error and exit.
    try {
        sys = getCorbaClient().resolveName<System>(
            "carma.downconverter.WbdcControl");
        dc = sys->GlobalDownconverter();
        qm = sys->GlobalQuadMod();
        ns = sys->GlobalNoiseSource();
        lo = sys->GlobalLoMonitor();
     } catch (CORBA::UserException &uex) {
        cerr << "Corba user exception caught - exiting" << endl;
        exit (EXIT_FAILURE);
     } catch (CORBA::SystemException &sex) {
        cerr << sex << endl;
        exit (EXIT_FAILURE);
     }

     cout << "ORB set, carma.downconverter.WbdcControl resolved." << endl;

     // OK, We've got our orb and our system reference, now enter an infinite 
     // loop to retrieve objects resolve names, etc...  The rational here is 
     // that if for some reason we do receive a TRANSIENT or other such 
     // exception, we try try again until the guy comes back online.
     try {

         // Do work with our DOs. 

         cerr << "Invoking methods on DOs." << endl;

         // After first invocation of below, dc object reference is 
         // directly binded to the server (if an IMR is being used).
         // This call causes ALL Downconverter modules to setPsysPreset.
         dc->setPsysPreset();

         cerr << "Done setting psys preset." << endl;
         // If the server is started using the IMR, this call rebinds to 
         // the server on every invocation! This is because the below
         // call must retrieve the Downconverter(1,1) IOR on every call 
         // which is then indirectly binded to the server - basically it 
         // is very slow and grows increasingly slow with the nesting level.
         sys->Downconverter(1,1)->setPsysPreset();

         // Act on the downconverter extensively.
         dc->setPsysPreset();
         dc->setPsys(20.0);
         dc->setPsysAtten(2.0);
         dc->setIfOutPreset();
         dc->setIfOut(2.0);
         dc->setIfOutAtten(10.0);
         dc->enableRfInputAmp(true);
         dc->enableIfOutAlc(true);
         dc->reset();

         cerr << "Quad mods" << endl;
         // Act on the Qm extensively.
         qm->setPoutPreset();
         qm->setPout(-50.0);
         qm->setPoutAtten(8);
         qm->enableQuadMod(true);
         qm->reset();

         cerr << "Noise source" << endl;
         // Act on Ns extensively.
         ns->setNoiseOutputToPreset();
         ns->setNoiseOutput(2.0);
         ns->setNoiseAttenuation(1);
         ns->setToneAttenuation(1);
         ns->enableNoiseSource(true);
         ns->enableToneSource(true);
         ns->reset();

         // Act on the Lo monitor extensively.
         lo->initializePowerMeter();
         lo->reset();

         cerr << "System itself." << endl;
         // Reset the entire bus for the hell of it.
         sys->reset();  // Via DIO.
         cerr << "Reset" << endl;
         sleep(1);
         sys->softReset(); // Via CAN RESET message.
         cout << "Quitting." << endl;
         sleep(1);
         sys->quit();

     } catch (...) {
        cerr << getStringForCaught() << endl;
        return 1;
     }

     return 0;
}

// -----------------------------------------------------------------------------
void setSigAction()
{
    int SIGNUM = 2; // SIGINT
    struct sigaction action;
    // action.sa_flags = SA_SIGINFO;
    sigfillset (&(action.sa_mask));
    action.sa_handler = SIG_DFL; // Specify Default action - abort().
    action.sa_restorer = NULL;
    sigaction (SIGNUM, &action, NULL);
}
