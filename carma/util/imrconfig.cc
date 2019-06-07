/** @file 
 * Application to configure IMR from CARMA XML configuration files.  
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.33 $
 * $Date: 2012/03/13 05:18:24 $
 * $Id: imrconfig.cc,v 1.33 2012/03/13 05:18:24 abeard Exp $
 */
// System includes
#include <unistd.h>

// Carma includes
#include "carma/corba/Client.h"
#include "carma/corba/Server.h"
#include "carma/util/ImrConfigHandlers.h"
#include "carma/util/ConfigureImr.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"

#include <iostream>

using namespace std;
using namespace carma::util;

/**
 * @version $Revision: 1.33 $
 *
 * @usage \nUsage: imrconfig [oad=oadhostname] [file=config.xml] [clean=false]\n\t\t [verbose=false] [validate=false] [priority=\"0-9\"]\n
 *
 * @description
 * \nimrconfig is responsible for reading a CARMA XML configuration file,
 * parsing it and configuring the IMR accordingly. See 
 * carma/conf/exampleCarmaConfig.xml for details on the format and contents
 * of the configuration files.  This program configures only one OAD at 
 * a time and the OAD must be described in the XML config file.
 *
 * @key file       @mandatory string Name of CARMA XML configuration file.
 * @key oad        @noDefault string OAD hostname this program is running on. 
 * @key start      false      bool   Start an OAD's shared mode servers.
 * @key stop       false      bool   Stop an OAD's registered servers.
 * @key priority   "0-9"      string 
 *      Only start or stop servers with the specified startup priority.\n\t 
 *      Valid priority levels are 0-9 inclusive with 0 indicating the \n\t
 *      highest priority.  Ranges may be specified with a '-'.  The below
 *      wait parameter specifies the longest amount of time to wait for
 *      servers in a priority range to start.
 * @key wait        5000      int    
 *      Wait msecs for servers of specified priority range to startup.
 * @key waitOnEachPriority false bool Wait 'wait' ms for servers to start/stop 
 *      for each priority level rather than all at the end.
 * @key clean      false      bool   Unregister all OAD servers from the IMR.
 * @key hosts      false      bool   Print all OAD hosts - don't configure! 
 * @key validate   false      bool   Validate XML file only - don't configure!
 * @key verbose    false      bool   Be verbose - lists all servers and OADs.
 * @key printTimes true       bool   Print server startup times.
 * @key addSleep   0          int    Sleep msecs between adding server to OAD.
 * @key startSleep 0          int    Sleep msecs between starting servers.
 * @key priorityWaitMs 5000   int    Sleep msecs between priority stages.
 * @logger DEFAULT_FACILITY carma.imrconfig
 */
    
void printHosts(domainType domain);

int Program::main() {
    
    string filename = getConfFile(getStringParameter("file"));
    string imrHostname;
    string oad;
    if ( parameterWasSpecified("oad") ) {
        oad = getStringParameter("oad");
        string lowercaseOad;
        lowercaseOad.resize(oad.size());
        for (string::size_type i=0; i<oad.size(); i++) {
             lowercaseOad[i] = tolower(oad[i]);
        }
        if (lowercaseOad == "localhost") {
            const int len = 300;
            char localhostname[len+1];
            gethostname(localhostname, len);
            oad = localhostname;
        }
    }

    const bool validate = getBoolParameter("validate"); // Validate only
    const bool verbose = getBoolParameter("verbose");
    const bool printServerStartTimes = getBoolParameter("printTimes"); 
    const bool clean = getBoolParameter("clean");
    const bool start = getBoolParameter("start");
    const bool hosts = getBoolParameter("hosts");
    const int priorityWaitMs = getIntParameter("priorityWaitMs");
    const bool waitOnEachPriorityStage = getBoolParameter("waitOnEachPriority");
    bool stop = getBoolParameter("stop");

    const unsigned long addSleepMs = getIntParameter("addSleep");
    const unsigned long startSleepMs = getIntParameter("startSleep");
    const unsigned long waitMs = getIntParameter("wait");
    const string prioritySpec = getStringParameter("priority");

    domainType domain;

    // Validate command line parameters...
    // Make sure oad was specified if either cleaning starting or stopping.
    if ( (!parameterWasSpecified("oad")) && (clean || start || stop) ) {
        cerr << "OAD not specified!" << endl;
        cerr << getUsageString() << endl;
        return EXIT_FAILURE;
    }
        
    // Make sure user didn't specify non-configuring options with ones that
    // perform configuration tasks.
    if ( (hosts || validate) && (clean || start || stop) ) {
        cerr << "Ambiguous command line options." << endl;
        cerr << getUsageString() << endl;
        return EXIT_FAILURE;
    }

    // Make sure user didn't specify multiple tasks such as validating and 
    // printing hosts or stopping and starting.
    if ( (hosts && validate) || (start && stop) ) {
        cerr << "Ambiguous command line options." << endl;
        cerr << getUsageString() << endl;
        return EXIT_FAILURE;
    }

    // Parse XML File.
    try {
        domain = parseXmlConfig(filename, validate, verbose);
    } catch ( const ErrorException& ex) {
        const string errMsg = ex.getErrorMessage();
        programLogErrorIfPossible(errMsg);
        cerr << errMsg << endl;
        return EXIT_FAILURE;
    } catch (...) {
        const string 
            errMsg( "Caught unspecified exception trying to parse config file");
        programLogErrorIfPossible(errMsg);
        cerr << errMsg << endl;
        return EXIT_FAILURE;
    }
    
    // If we are only validating the input XML file, then we're done.
    if (validate) {
        return EXIT_SUCCESS;
    } 
    
    // If we're only printing the hostnames, do so, then get out.
    if (hosts) {
        printHosts(domain);
        return EXIT_SUCCESS;
    }
   
    // If we made it this far, we actually want to configure the imr and must
    // initialize an orb to begin doing so.  But first make sure imr was
    // specified on the command line. 
    if ( haveImrHostname() ) {
        imrHostname = getImrHostname();
    } else {
        cerr << "Must specify IMR with imr=X." << endl;
        return EXIT_FAILURE;
    }
        
    // Proceed with command line specific action. 
    try {
    
        ConfigureImr imrAdmin( getCorbaClient(), imrHostname, domain );

        if (start) {
            imrAdmin.addServers( oad, prioritySpec, addSleepMs );
            sleep(1);
            imrAdmin.startServers( oad, 
                                   prioritySpec, 
                                   waitMs, 
                                   startSleepMs, 
                                   waitOnEachPriorityStage, 
                                   priorityWaitMs,
                                   printServerStartTimes );
        } else if (stop) {
            imrAdmin.stopServers( oad, prioritySpec, 
                                  waitMs, waitOnEachPriorityStage  );
            if ( clean ) imrAdmin.clean(oad);
            sleep(1);
        } else {
            imrAdmin.addServers( oad, prioritySpec, addSleepMs );
            sleep(1);
        }
    } catch (...) {
        cerr << "Unable to contact IMR!" << endl;
        cerr << "Verify that specified imr is valid, reachable and up." << endl;
        cerr << getStringForCaught() << endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

void printHosts(domainType domain)
{
    // Loop through the domain and print out all the oad hosts.
    vector<OADConfig>::iterator oad;
    for (oad = domain.oads.begin(); oad < domain.oads.end(); oad++)
        cout << oad->hostname << " ";
    cout << endl;
}
    
