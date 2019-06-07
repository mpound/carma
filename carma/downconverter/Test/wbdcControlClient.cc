/**@file
 * canlab2 quadmod and downconverter control application.
 *
 * canlab2 (located in Hawkins' lab in building 7) requires
 * a simple tool to
 *
 *  - turn on the quadmods
 *  - enable quadmod phase switching
 *  - set the quadmod to a particular phase state
 *  - set the downconverter output power level 
 *    (nominally its 6dBm, but if splitters are used during
 *     a test, the power needs to be boosted).
 * 
 * The canlab2 setup does not have a CAN bus noise source,
 * so no control of that is currently requred.
 * 
 * This file is derived from Andy's exampleClient.cc
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard/Dave Hawkins </dl>
 * $Revision: 1.21 $
 * $Date: 2012/03/13 05:18:07 $
 * $Id: wbdcControlClient.cc,v 1.21 2012/03/13 05:18:07 abeard Exp $
 */
#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/downconverter/common/downconverterSystem.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/UserException.h"

#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <signal.h>
#include <string>
#include <unistd.h>

using namespace std;
using namespace carma::downconverter;
using namespace carma::util;

// Helper function declaration
void setSigAction(); // Needed to bypass carma signal handler.
void sleepWithProgress(string msg, unsigned int secs) {
    cout << msg << flush;
    for (unsigned i = 0; i < secs; i++) {
        sleep(1);
        cout << "." << flush;
    } 
};

/** 
 * @version $Revision: 1.21 $
 * 
 * @usage Usage: wbdcControlClient [imr=imr] [-- <extraargs] \n See wbdcControlClient --keywords for a complete list of keywords.
 *
 * @description 
 * \nwbdcControlClient is a Corba client application to the Carma Wideband
 * Downconverter CAN host/CORBA server application. This application
 * provides a simple command line tool for enabling and controlling
 * the Wbdc noise source, quadmods and downconverters. \n\nNOTES:
 * a) This program requires that the carmaWbdcHost application be 
 *    running.
 * b) This application must contact the ORBacus Naming Service to 
 *    retrieve DO references.  It is easiest to access this through
 *    the imr via the imr= keyword.\n
 * \nExample: Using an imr on host inyo.ovro.caltech.edu
 * \t wbdcControlClient imr=inyo 
 * \nSee wbdcControlClient --usage, --description and --keywords for more info.
 *
 * @key phase        -1 i Set phase state value to 0,1,2, or 3.
 * @key ifout       6.0 double Set downconverter IF Output power (Default: 6dBm).
 * @key psys @noDefault double Set downconverter Psys power level (Default: nothing)
 * @key band         -1 i Only run on specified band (Default: All bands).
 * @key nsatten     0 i NoiseSource output power attenuation level (Default 0).
 * @key qmatten     0 i QuadMod output power attenuation level (Default 0).
 * @key noise    true b Enable/Disable output noise from QuadMods and NoiseSource.
 * @key rf       true b Enable/Disable RF input signal (via amplifier and atten).
 * @key sl      false b Communicate with the spectral line downconverters. 
 *
 * @logger DEFAULT_FACILITY carma.downconverter.controlClient
 */
int Program::main() {
    setSigAction(); // Bypass Carma signal handlers
    
    const unsigned int WAIT_SLEEP = 2;
    System_var sys;
    DownconverterControl_var dc;
    QuadModControl_var qm;
    NoiseSourceControl_var ns;
    double ifOutPower = getDoubleParameter("ifout"); 
    int nsAtten = getIntParameter("nsatten");
    int qmAtten = getIntParameter("qmatten");
    int phaseState = getIntParameter("phase");
    int band = getIntParameter("band"), bStart, bEnd;
    bool noise = getBoolParameter("noise"), result;
    bool rf = getBoolParameter("rf");
    bool sl = getBoolParameter("sl");
    int aEnd = (sl ? 15 : 8);

    // Resolve DO references from nameservice
    try {

        // Retrieve DO references...
        if ( sl ) {
            CPTRACE(Trace::TRACE6, "Resolving SldcControl DO.");
            sys = getCorbaClient().resolveName<System>(
                    "carma.downconverter.SldcControl");
        } else { 
            CPTRACE(Trace::TRACE6, "Resolving WbdcControl DO.");
            sys = getCorbaServer().client().resolveName<System>(
                    "carma.downconverter.WbdcControl");
        }

        CPTRACE(Trace::TRACE6, "Resolving sub DOs.");
        dc = sys->GlobalDownconverter();
        CPTRACE(Trace::TRACE6, "Resolving GlobalQuadMod.");
        qm = sys->GlobalQuadMod();
        CPTRACE(Trace::TRACE6, "Resolving NoiseSource."); 
        ns = sys->NoiseSource();
        CPTRACE(Trace::TRACE6, "Done resolving sub DOs.");
    } catch (CORBA::UserException &uex) {
        cout << "Error initializing ORBacus - make sure that the proper "
             << "imr was input on the command line (via imr= keyword), "
             << "that the carmaWbdcHost application is running and that "
             << "the imr is up: " << endl << uex << endl;
        exit (EXIT_FAILURE);
    } catch (CORBA::SystemException &sex) {
        cout << "Error initializing ORBacus - make sure that the proper "
             << "imr was input on the command line (via imr= keyword), "
             << "that the carmaWbdcHost application is running and that "
             << "the imr is up: " << endl << sex << endl;
        exit (EXIT_FAILURE);
    }

    cout << "--------------------------------" << endl
         << "wbdcControlClient client started" << endl
         << "--------------------------------" << endl;

    try {
        // Enable the NoiseSource
        cout << (noise ? "En" : "Dis") << "abling noise source." << flush;
        ns->enableNoiseSource(noise);
        sleepWithProgress("", WAIT_SLEEP);

        if (sys->NoiseSource()->isEnabled()) {
            cout << "Enabled" << endl;
        } else {
            cout << "Disabled" << endl;
        }

        if (noise) {
            ns->setPoutAtten(nsAtten);
            sleepWithProgress("Setting NoiseSource attenuation.", WAIT_SLEEP); 
            cout << "Done" << endl;
        }

        // Enable the quadmod
        cout << (noise ? "En" : "Dis") << "abling quadmods." << flush;
        qm->enableQuadMod(noise);
        sleepWithProgress("", WAIT_SLEEP);
        cout << " " << flush;
        // Verify that quad mods are enabled
        for (int a = 1; a <= aEnd; a++) {
            if (sys->QuadMod(a)->isEnabled()) {
                cout << a << "E " << flush;
            } else {
                cout << a << "D " << flush;
            }
        }
        cout << endl;
        
        if (noise) {
            cout << "Setting QuadMod output power attenuation to "
                << qmAtten << "." << flush;
            qm->setPoutAtten(qmAtten);
            sleepWithProgress("", WAIT_SLEEP);
            cout << "Done" <<  endl;
        }

        if ((phaseState >= 0 && phaseState < 4) && noise) {
            cout << "Setting Quadmod phase state to " << phaseState 
                << flush;
            qm->setPhaseState(phaseState);
            sleepWithProgress("", WAIT_SLEEP);
            cout << "Done" << endl;
        }

        // Either set psys power levels OR turn off the amplifier and 
        // add full attenuation to RF input.
        if (rf) {
            if (parameterWasSpecified("psys")) {
                double psys = getDoubleParameter("psys");
                dc->setPsys(psys);
                cout << "Setting Psys to " << psys << flush;
                sleepWithProgress("", WAIT_SLEEP);
            } else {
                dc->setPsysPreset();
                sleepWithProgress("Setting psys to PRESET value.", WAIT_SLEEP);
            }
        } else {
            cout << "Disabling RF amplifier." << flush;
            dc->enableRfInputAmp(false);
            sleepWithProgress("", WAIT_SLEEP);
            cout << "Done" << endl << "Fully attenuation RF input signal." 
                 << flush;
            dc->setPsysAtten(31.50);
            sleepWithProgress("", WAIT_SLEEP);
        }
        cout << "Done" << endl;

        // Call setPsysPreset on all downconverters.  
        if (!parameterWasSpecified("ifout")) {
            cout << "Setting dowconverter IF output power to preset." << flush;
            dc->setIfOutPreset();
        } else {        
            cout << "Setting downconverter IF output power to " 
                << ifOutPower << flush;
            dc->setIfOut(ifOutPower);
        }
        sleepWithProgress("", WAIT_SLEEP); 
        cout << "Done" << endl;

        // Verify that the if output level was set properly...
        cout << "Verifying that Downconverters output power level is within "
            << "0.5 dBm of " << ifOutPower << "." << endl;
        cout << "V = valid (within range), I = invalid, O = Offline" << endl;
        if (band == -1) {
            bStart = 1;
            if (sl)
                bEnd = 8;
            else 
                bEnd = 16;
            
        } else if (band >= 1 && ((sl && band <= 16) || (!sl && band <= 8))) {
            bStart = bEnd = band;
        } else {
            cerr << "Invalid band: " << endl 
                << getUsageString() << endl;
            return EXIT_FAILURE;
        }
        
        cout << "Antenna: ";
        for (int ant = 1; ant <= aEnd; ant++) 
            cout << setw(3) << ant;
        cout << endl;
        if (sl) cout << "---------------------";
        cout << "---------------------------------" << endl;
        for (int b = bStart; b <= bEnd; b++) {
            cout << "Band " << setw(2) << b << ":   ";
            for (int a = 1; a <= aEnd ; a++) {

                if (sys->Downconverter(a, b)->isOnline()) {
                    // Check that ifOutputPower is within bounds...
                    result = sys->Downconverter(a, b)->
                        checkIfOutputPower(ifOutPower, 0.5);
                    if (result) { 
                        cout << "V" << flush;
                    } else {
                        cout << "I" << flush;
                    }
                } else {
                    cout << "O" << flush;
                }
                cout << "  " << flush;
            }
            cout << endl;
        }
        
    } catch (const carma::util::UserException &eex) {
         cout << eex.errorMsg << endl;
         exit(EXIT_FAILURE);
     } catch (const CORBA::UserException &uex) {
         cout << uex << endl;
         exit(EXIT_FAILURE);
     } catch (const CORBA::Exception& ex) {
         cout << ex << endl;
         exit(EXIT_FAILURE);
     }
     return EXIT_SUCCESS;
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
