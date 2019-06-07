/** @file
 * Firmware loader for CARMA CANbus modules.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.3 $
 * $Date: 2012/07/25 17:51:26 $
 * $Id: canfwloader.cc,v 1.3 2012/07/25 17:51:26 abeard Exp $
 */

/** 
 * @version $Revision: 1.3 $
 * 
 * @usage fwloader [fwfile=<string>] [api=<int>] [OPTION]
 *
 * @description 
 * \nDownloads firmware to a module or group of modules.  This program can
 * be used either locally or remotely via CAN-Over-Ip.  When used locally,
 * this program must have exclusive access to the CANbus, and thus the rest
 * of the system must be disrupted.  When used remotely via CAN-Over-Ip, only
 * the module(s) receiving firmware will go OFFLINE.  Keep in mind that this
 * requires a CAN-Over-Ip server to be running on the host systemm.
 * \nExample: downcvtr.hex file with a new api id of 130 on all nodes remotely.
 * \tfwloader fwfile=./downcvtr.hex api=130 hostname=sldc.carma.pvt
 * \nExample: downcvtr.hex file with api of 130 on node 12 on bus 1 locally.
 * \tfwloader fwfile=./downcvtr.hex api=130 node=12 canbus=1
 * \nExample: downcvtr.hex file, api 130, board 1 canbus 0 locally.
 * \tfwloader fwfile=./downcvtr.hex api=130 board=1
 *
 * @key fwfile     @mandatory string 
 *      Firmware filename.
 * @key api        @mandatory int    
 *      Api id corresponding to modules to download to.
 * @key hostname   @noDefault string 
 *      Use to download firmware remotely using CAN-Over-Ip.  This is a far \n
 *    \tless disruptive way to download firmware on a running system.
 * @key board      0          int    
 *      Modulbus number of Janz board: Range [0,15].  This option is \n 
 *    \tincompatible with the hostname option.
 * @key canbus     0          int    
 *      Modulbus slot id of CAN card (0, 1 or -1 for both busses).  If  \n
 *    \tdownloading to both busses MAKE SURE THAT THE TWO BUSSES AREN'T \n
 *    \tCONNECTED!  This option is incompatible with the hostname option. 
 * @key node       0          int    
 *      Node id of a single module to download to (default all).
 * @key busid     -1          int 
 *      Only send firmware to modules on the specified bus rather than \n
 *    \tall busses. This is useful when you know which bus the module \n
 *    \tresides on and don't want to saturate other busses with firmware \n 
 *    \tpackets.  Generally, a module's busid is displayed on its XAC \n 
 *    \tRTD page.
 * @key dummys     3          int 
 *      Number of dummy messages to send between legitimate fw packets. \n
 *    \tWarning: Debug only - more than 3 dummy packets may result in \n
 *    \tcorrupted firmware!
 * @key initdelay  10         int    
 *      Seconds to wait for module initialization (reset). 
 * @key oldstyle   false      bool 
 *      Use old style of downloading firmware. The original method used  \n
 *    \tPOSIX realtime scheduling and required that the user become root. 
 * @logger DEFAULT_FACILITY carma.canbus.fwloader
 */

#include "carma/canbus/CanDio.h"
#include "carma/canbus/DeviceNames.h"
#include "carma/canbus/InetCan.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"

#include <ctime>
#include <cerrno>
#include <fstream>
#include <iostream>
#include <unistd.h>

using namespace std;
using namespace carma::canbus;
using namespace carma::util;

namespace { // Anonymous namespace for local constants
    const long MSG_DELAY           = 500; // Wait 500us between messages.
    const apiType FW_PKT_API       = 0x0E0;
    const nodeType FW_PKT_NODE     = 0x0E0;
    const msgType FW_PKT_MID       = 0x3F3;
    Program & program              = Program::getProgram();
} // End namespace <unnamed>

// ----------------
void scheduleRealtime()
{
    // We use POSIX realtime scheduling to get high time resolution
    // when sending CAN messages.  If we send them all at once, the 
    // XAC drops packets as it can't service them fast enough.  If we 
    // send them one at a time and sleep in-between the scheduler kicks 
    // in and separates the sent messages by 20ms (5 minutes to send
    // a 200k file!).  With realtime scheduling the sleeps are very 
    // accurate as they are implemented with busy loops and thus we 
    // can fine tune the uploading to less than 20 seconds.  Unfortunately
    // this kind of scheduling requires root access.

    int thread_policy = SCHED_FIFO, status;
    struct sched_param thread_param;
    thread_param.sched_priority = 25;

    status = pthread_setschedparam(
            pthread_self(), thread_policy, &thread_param);
    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "main() - Error changing thread priority " 
                + (string)strerror(errno));
    }
}

// ----------------
bool userIntentVerified(int api, int node)
{

    string ans;

    // Firmware is serious make sure they know what they want.
    if (node == 0) 
        cerr << "Are you sure you want to download new firmware to "
            << "ALL " << DeviceNames::getName(api) << " modules (api " 
            << api << ")? (type yes or no)" << endl;
    else
        cerr << "Are you sure you want to download new firmware to "
            << DeviceNames::getName(api) << " module (api " <<  api 
            << ") node " << node << "? (type yes or no)" << endl;

    // Force user to type yes or no exactly
    while (true) {
        cin >> ans;
        if (ans == "yes")
            break;
        if (ans == "no")
            return false; 
        cerr << "What? Type yes or no only please." << endl;
    }
    return true;
}

// --------------------
bool commandLineOptionsVerified()
{
    carma::util::Program & program = Program::getProgram();

    // Make sure the command line doesn't contradict itself
    if ( (program.parameterWasSpecified("board") || 
          program.parameterWasSpecified("canbus") ) &&
          program.parameterWasSpecified("hostname") ) 
    {
        cerr << program.getArg0() 
             << ": Conflicting command line options." << endl;
        return false;
    } 

    // Make sure API is valid.
    if (program.getIntParameter("api") == 0) {
        cerr << "Sorry, but downloading a single firmware file to all api's"
            << " doesn't make sense.  Please use a valid api." << endl;
        return false;
    }
    return true;
}
    
// --------------------
void sleepWithProgress(int secs) 
{
    for (int i = 0; i < secs; i++) {
        cerr << "." << flush;
        sleep(1);
    }
    cerr << "Done" << endl;
}

// --------------------
carma::canbus::Message createInitMsg(apiType api, nodeType node, busIdType busId )
{
    unsigned char magic[8] = {0x1E, 0xA5, 0x69, 0xC3, 0x3C, 0x96, 0x5A, 0xE1};
    carma::canbus::Message msg;
    msg.setId( createId(false, api, node, 0x3fd) );
    msg.setBusId(busId);

    for (unsigned i = 0; i < 8; ++i)
        msg << magic[i];

    return msg;
}

// -----------------------------------------------------------------------------
int Program::main() 
{

    try {
        string clFilename = getStringParameter("fwfile");
        int clApi = getIntParameter("api");
        int clNode = getIntParameter("node");
        int clBoard = getIntParameter("board");
        int clCanbus = getIntParameter("canbus");
        int clInitDelay = getIntParameter("initdelay");
        int clDummys = getIntParameter("dummys");
        bool clOldStyle = getBoolParameter("oldstyle");
        int clBusId = getIntParameter("busid");
        string clHostname; 

        int nMsgs = 0, byteCount = 0;
        char c;
        CanIo * can;
        timespec ts, rem;
        time_t start, end;
        const busIdType BUS_ID = ( clBusId == -1 ? ALL_BUSSES : clBusId );

        carma::canbus::Message fwPacketMsg( 
            createId(false, FW_PKT_API , FW_PKT_NODE, FW_PKT_MID), 
            BUS_ID );
        carma::canbus::Message dummyMsg( 
            createId(false, FW_PKT_API, FW_PKT_NODE, DUMMY_PKT_MID), 
            DataVector(8),
            BUS_ID ); 
        DataVector noData;

        ts.tv_sec = 0;
        ts.tv_nsec = MSG_DELAY * 1000; // us

        // Check permissions and schedule realtime if we're running old style.
        if (clOldStyle) {
            if (getuid() != 0) { // Make sure we have proper permissions.
                cerr << getArg0() 
                    << ": You must be root to load firmware!" << endl;
                return EPERM;
            }
            scheduleRealtime(); // Schedule ourself with RT policy.
        }

        // Verify command line options and print usage if bad.
        if ( !commandLineOptionsVerified() ) {
            cerr << "Usage: " << getUsageString() << endl;
            cerr << "Try `" << getArg0() << " --keywords` for more info on "
                << "options." << endl;
            return EXIT_FAILURE;
        }

        // Make sure user is downloading to proper module(s).
        if ( !userIntentVerified(clApi, clNode) )
            return EXIT_FAILURE;

        // Open CAN interface depending on command line options.
        if ( parameterWasSpecified("hostname") ) {
            clHostname = getStringParameter("hostname");
            // Create can over ip session - note that the acceptance filter is
            // 0 which prevents all messages except a global reset from 
            // coming back.
            can = new InetCan(clHostname, 0, 0);
        } else if (clCanbus == -1) {
            CanDio * candio = new CanDio(clBoard);
            candio->reset();
            can = candio;
        } else {
            CanDio * candio = new CanDio(clBoard, clCanbus);
            candio->reset();
            can = candio;
        }
        
        cerr << "Initializing CAN interface." << flush;
        if ( !parameterWasSpecified("hostname") ) 
            sleepWithProgress(clInitDelay);
        else
            sleepWithProgress(0);

        cerr << "Initializing board for download." << flush;
        can->postMessage( createInitMsg(clApi, clNode, BUS_ID) );
        sleepWithProgress(5);

        cerr << "Downloading firmware." << flush;

        time (&start);

        // Open file stream in binary mode...
        ifstream fw(clFilename.c_str(), ios::in|ios::binary);
        while ( fw ) {  // Do while the stream is good.
            fwPacketMsg.setData( noData ); // Reset data
            for (unsigned int i = 0; i < 8; i++) {
                fw.get( c ); // Read from file
                if ( fw ) 
                    fwPacketMsg << static_cast<byteType>(c);

                     // data.insert(data.end(), buff[i]);
                else 
                    break;
                ++byteCount;   
            } // End loop over bytes in CAN message.
                
            if ( !fw && !fw.eof() ) // Bail if error
                throw carma::util::ErrorException((string)"Error reading from "
                        "firmware file.", __FILE__, __LINE__);

            if ( fw.eof() && fwPacketMsg.getData().size() == 0 )
                break;  // We're done

            // Post the message to the bus
            can->postMessage(fwPacketMsg);

            // Send out dummy messages to compromise for crap timing
            if (!clOldStyle) {
                for (int i = 0; i < clDummys; ++i) 
                    can->postMessage(dummyMsg);
            }

            // Print out progress
            if (++nMsgs % 100 == 0) 
                cerr << "." << flush;

            if (clOldStyle)
                nanosleep(&ts, &rem);
        } // End while stream is good

        cerr << "Done" << endl;

        time (&end);
                
        cerr << "Firmware download successful." << endl;
        cerr << " Sent " << byteCount << " bytes in " << (end - start);
        cerr << " seconds." << endl;
        cerr << "Waiting 80 seconds for module(s) to flash new firmware.";
        cerr << endl;
        sleepWithProgress(80); 

        delete can;

    } catch (carma::canbus::JanzFailException &jfe) {
        cerr << "Failed to initialize CAN/DIO interface.  You "
            << "must have exclusive access to the CAN interface in "
            << "order to load firmware.  Kill any applications that "
            << "may be accessing the bus. Error msg: " << jfe.what()
            << endl;
    } catch (carma::util::ErrorException &eex) {
        cerr << eex.what() << endl;
    } catch (...) {
        cerr << "Unknown exception caught - exiting." << endl;
    }
    return EXIT_SUCCESS;
}
