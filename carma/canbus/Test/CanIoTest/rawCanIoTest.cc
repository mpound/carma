/** @file
 * rawCanIoTest - mimics canIoTest but uses raw janz calls thus allowing me
 * to characterize overhead of the CanIo class wrappers. 
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.6 $
 * $Date: 2006/04/14 20:34:49 $
 * $Id: rawCanIoTest.cc,v 1.6 2006/04/14 20:34:49 tcosta Exp $
 */

// Carma includes
#include "carma/util/Program.h"

// Janz includes
#include "janz/bcan.h"
#include "janz/can_lib.h"
#include "janz/defs.h"
#include "janz/dpm.h"
#include "janz/mitop.h"
#include "janz/vmod.h"

// C++ stl
#include <iostream>

#include <unistd.h>

using namespace std;
using namespace carma::util;

static int rxfd, txfd; // Rx and Tx file descriptors

// Helper functions - defined at end of file...
bool initialize();

// Post nMsgs to bus using CANId beginning at startIndex 
// returns number of messages posted...
int postMsgs(unsigned int nMsgs, unsigned int startIndex);

// Read nMsgs and check their index starting at startIndex, returns number
// of messages dropped...
int readMsgs(unsigned int nMsgs, unsigned int startIndex);

// Form up the raw fast message
void initFastMsg(::FastMessage &fmsg);

/**
 * @version $Revision: 1.6 $
 *
 * @description
 * CAN bus 1 must be connected to CAN bus 2. This program tests CPU usage and 
 * CANbus throughput using raw janz c-lib calls.  This test was mainly designed
 * to characterize any overhead of the carma::canbus::CanIo classes. 
 *
 * @usage rawCanIoTest can1=<device> can2=<device> \n Example:
 *  rawCanIoTest can1=/dev/dpm_00 can2=/dev/dpm_01
 *
 * @key can1 /dev/dpm_00 s First CAN device name.
 * @key can2 /dev/dpm_01 s Second CAN device name.
 *
 * @logger TEST_FACILITY carma.test.canbus.CanIoTest.rawCanIoTest
 */
int Program::main()
{
    const unsigned int postBatch = 250; 
    const unsigned int postRounds = 1000;
    int posted = 0, retrieved = 0;
    
    if (!initialize()) return EXIT_FAILURE;

    for (unsigned int i = 0; i < postRounds; i++) {
        cerr << "." << flush;
        posted += postMsgs(postBatch, 0);
        // cerr << posted << "," << flush;
        retrieved += readMsgs(postBatch, 0);
        // cerr << retrieved << ">" << flush;
    }
   
    cerr << "Posted/Retrieved: " << posted << "/" << retrieved << endl;
    return EXIT_SUCCESS;
    
}

// --------------
// Initialize Janz cards.
// --------------
bool initialize() 
{
    string dev1 = Program::getProgram().getStringParameter("can1");
    string dev2 = Program::getProgram().getStringParameter("can2");
    
    ::Message msg;  // Janz CAN message.

    // Open the can devices;
    rxfd = can_open(const_cast<char *>(dev1.c_str()));
    txfd = can_open(const_cast<char *>(dev2.c_str()));

    if (rxfd < 0 || txfd < 0) {
        cerr << "Unable to open CAN devices: check that devices exist and "
            << "dpm modules are loaded." << endl;
        return false;
    }

    // Terminate both busses.
    SwitchCanTermination(&msg, 1);
    while (can_send(rxfd, &msg) <= 0);
    while (can_send(txfd, &msg) <= 0);

    // Set baud rate to 1Mbit/s
    IcWriteBtrBCAN(&msg, 0x2300);
    while (can_send(rxfd, &msg) <= 0);
    while (can_send(txfd, &msg) <= 0);
    
    // Set Software acceptance mask
    IcRangeSetAfil(&msg, 0x00, 0x7ff, 2);
    while (can_send(rxfd, &msg) <= 0);
    while (can_send(txfd, &msg) <= 0);
    
    // Switch devices to new style host interface
    ican2_select_hostif(rxfd, 20, 20);
    ican2_select_hostif(txfd, 20, 20);

    // Initialize 'fast interface'
    if (ican2_init_fast_can(rxfd, 1632, 1632) < 0 ||
        ican2_init_fast_can(txfd, 1632, 1632) < 0) {
         cerr << "Unable to initialize fast interface!" << endl;
         return false;
    }
       
    // Turn the bus on
    IcBusOnBCAN(&msg);
    while (can_send(rxfd, &msg) <= 0);
    while (can_send(txfd, &msg) <= 0);

    return true;
}

// -------------
// postMsgs to rx CAN interface
// -------------
int postMsgs(unsigned int nMsgs, unsigned int startIndex)
{
    int result;
    int nPosted = 0;
    ::FastMessage fmsg;

    initFastMsg(fmsg);

    for (unsigned i = 0; i < nMsgs; i++) {
        // Form message - and place msg index in data.
        result = can_fast_send(txfd, &fmsg);
        if (result >= 0) nPosted++;
    }
    return nPosted;
}

// ------------------------
// Read msgs...
// ------------------------
int readMsgs(unsigned int nMsgs, unsigned int startIndex) 
{
  
    int nRead = 0; // # of read messages.
    int bytesRead;
    unsigned char charBuf;
    ::FastMessage fmsg;

    for (unsigned i = 0; i < nMsgs; i++) {
        bytesRead = read(rxfd, &charBuf, 1);

          if (bytesRead == 1 && charBuf == FAST_QUEUE) 
            // Retrieve the message.
            if (!(can_recv_fast(rxfd, &fmsg) < 0))
                nRead++;
    }
    return nRead;
}
        
// -----------------------------
// Initialize the fast message
// -----------------------------
void initFastMsg(::FastMessage &fmsg)
{
    unsigned int id = 0x10000000;
    fmsg.data[0] = (1 << 7);
    fmsg.data[1] = 0;
    fmsg.data[2] = id >> 21;
    fmsg.data[3] = (id >> 13) & 0x000000ff;
    fmsg.data[4] = (id >> 5) & 0x000000ff;
    fmsg.data[5] = (id & 0x0000001f) << 3;
};


    
