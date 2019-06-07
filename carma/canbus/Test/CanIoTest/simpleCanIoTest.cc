#include "carma/canbus/JanzCanIo.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Types.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"

#include <iostream>

using namespace carma::canbus;
using namespace carma::util;
using namespace std;

/**
 * @version $Revision: 1.7 $
 *
 * @description 
 * Simple program to send and receive messages from two connected cards. 
 *
 * @usage 
 * simpleCanIoTest can1=/dev/dpm_00 can2=/dev/dpm_01
 *
 * @key can1 /dev/dpm_00 s First CAN device name.
 * @key can2 /dev/dpm_01 s Second CAN device name.
 *
 * @logger TEST_FACILITY carma.test.canbus.CanIoTest.simpleCanIoTest
 */
int Program::main()
{
    int posted = 0;
    int retrieved = 0;
    string can1 = getStringParameter("can1");
    string can2 = getStringParameter("can2");
    
    try {
        JanzCanIo txIo( can1.c_str(), true );
        JanzCanIo rxIo( can2.c_str(), true );
        canbus::Message txMsg(createId(true, 0, 0, 0), ALL_BUSSES);
        canbus::Message rxMsg;
        
        for (unsigned i = 0; i < 600; i++) {
         cout << "." << flush;
         for (unsigned j = 0; j < 250; j++) {
            txIo.postMessage(txMsg);
            posted++;
         }
         for (unsigned k = 0; k < 250; k++) {
            rxMsg = rxIo.getMessage();
            retrieved++;
         }
        }
        cout << "Posted/Retrieved:" << posted << "/" << retrieved << endl;
    } catch (util::ErrorException &ex) {
        cerr << ex << endl;
    }
    return EXIT_SUCCESS;
}           
