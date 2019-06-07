#include "carma/canbus/DirectCan.h"
#include "carma/canbus/JanzCanIo.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"

#include <unistd.h>

#include <iostream>

using namespace carma::canbus;
using namespace std;
using namespace log4cpp;

/**
 * @version $Revision: 1.20 $
 *
 * @description
 * Test program for DirectCan class. Device 1 should be connected to device 2
 * as they send CAN messages to each other.
 * \nExample: directCanTest d1=/dev/dpm_00 d2=/dev/dpm_01
 * 
 * @usage directCanTest d1=<device 1> d2=<device 2> \n
 *
 * @key d1 /dev/dpm_00 s Device 1 name.
 * @key d2 /dev/dpm_01 s Device 2 name.
 *
 * @logger TEST_FACILITY carma.test.canbus.DirectCanTest.directCanTest
 */
int carma::util::Program::main() 
{
    string dev0 = getStringParameter("d1");
    string dev1 = getStringParameter("d2");

    // Create an instance of CanIo to service DirectCan requests... 
    // DirectCan is intended to be used concurrently with a CanMaster,
    // however, the actual servicing is done in CanIo (of which Master is
    // a derivative), thus we can get away with declaring a CanIo object
    // to handle the servicing.
    try {
        DirectCan dc;
        JanzCanIo cio( dev0.c_str(), true, dev1.c_str(), true );

        // Extract the txBusId from the device name so we know where to
        // send messages to.
        busIdType txBusId = extractBusId(dev0);
        carma::canbus::Message msg;
        short runs = 0, rxRuns = 0;
        const short MAX_RUNS = 20;
        vector<byteType> data;
        timespec st, ft, diff; // Start, finish and difference times.
        double msgRate;

        cout << endl << "Testing DirectCan." << endl;
        clock_gettime(CLOCK_REALTIME, &st);
        while (runs < MAX_RUNS) {

            // Alternately post and retrieve messages from the bus.
            // To maximize the CAN bandwidth send large batches of 
            // messages, however, note that if we send batches larger
            // than IPQ_BUFFER_SIZE (see Types.h), messages will be
            // lost.
            for (int i = 0; i < 2500; i++) {
                data.clear();
                sShortToData(data, runs);
                msg.setBusId(txBusId);
                msg.setId(i);
                msg.setData(data);
                dc.postMessage(msg);
            }

            // Retrieve a message using DirectCan. 
            // Here we wait for the last message sent to prevent us from
            // overflowing the IPQs.  This is an exceptional situation but
            // there is currently no way to handle it using IPQbuffer.
            // If we were to wait on the 1st message instead, we would
            // quickly return to the top of this loop and then overflow
            // the IPQ.
            msg = dc.getMessage(2499);
            data = msg.getData();
            rxRuns = dataToShort(data);
            cout << "." << flush;
            runs++;
        }
        clock_gettime(CLOCK_REALTIME, &ft);

        // Calculate time difference and message rate.
        // The msg rates for DirectCan will be slower due to context
        // switching and such.
        diff.tv_sec = ft.tv_sec - st.tv_sec;
        if (ft.tv_nsec >= st.tv_nsec) {
            diff.tv_nsec = ft.tv_nsec - st.tv_nsec;
        } else {
            diff.tv_sec -= 1;
            diff.tv_nsec = 1000000000 - abs(ft.tv_nsec - st.tv_nsec);
        }

        msgRate = (2500*MAX_RUNS) /
            (double)(diff.tv_sec + ((1.0e-9)*diff.tv_nsec));

        cout << endl << "Results:" << endl;
        cout << "Messages sent: " << (2500*MAX_RUNS) << endl;
        cout << "Message rate: " << msgRate << " msgs/s." << endl;
    } catch (carma::util::ErrorException &err) {
        err.log(Priority::ALERT);
        err.report();
    }
    cerr << "Test completed and successful." << endl;
    return EXIT_SUCCESS;
}
    
    
