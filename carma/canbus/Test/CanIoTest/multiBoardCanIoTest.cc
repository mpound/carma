// To perform this test tye bus 0 to bus 1 on a CARMA canbus card.

#include "carma/canbus/JanzCanIo.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <iostream>
#include <iomanip>

#include <sys/times.h>
#include <unistd.h>

using namespace carma::canbus;
using namespace carma::util;
using namespace std;

/** 
 * @version $Revision: 1.2 $
 * 
 * @description
 * Test program for multi board CanIo class instance. The devices specified 
 * by bus1 and bus2 must be connected to bus3 and bus4 respectively as the
 * two send CAN messages to each other.
 * \nExample: canIoTest b1=/dev/dpm_00 b2=/dev/dpm_00 b3=/dev/dpm_10 
 * \n\t b4=/dev/dpm_11
 *
 * @usage multiBoardCanIoTest b1=<dev name> b2=<dev name> b3=<dev name> b4=<dev name>
 * 
 * @key b1 /dev/dpm_00 s CAN device bus 1 name (connected to bus 3).
 * @key b2 /dev/dpm_01 s CAN device bus 2 name (connected to bus 4).
 * @key b3 /dev/dpm_10 s CAN device bus 3 name (connected to bus 1).
 * @key b4 /dev/dpm_11 s CAN device bus 4 name (connected to bus 2).
 * @key prio 2 i Tx message priority (1 = HIGH, 2 = NORMAL, 3 = LOW).
 * @key batchSize 400 i Size of batches of messages to send and receive.
 *
 * @logger TEST_FACILITY carma.test.canbus.CanIoTest.multiBoardCanIoTest
 */
int Program::main ()
{
    const string bus1 = getStringParameter("b1");
    const string bus2 = getStringParameter("b2");
    const string bus3 = getStringParameter("b3");
    const string bus4 = getStringParameter("b4");

    int prio = getIntParameter("prio");
    const int MSG_BATCH_SIZE = getIntParameter("batchSize");
    
    vector< pair< string, bool > > canIoArgs;
    canIoArgs.push_back( make_pair( bus1, true ) );
    canIoArgs.push_back( make_pair( bus2, true ) );
    canIoArgs.push_back( make_pair( bus3, true ) );
    canIoArgs.push_back( make_pair( bus4, true ) );

    // Note we swap which board sends and receives.
    const busIdType rxBus1Id = extractBusId( bus1 );
    const busIdType txBus2Id = extractBusId( bus2 );
    const busIdType txBus3Id = extractBusId( bus3 );
    const busIdType rxBus4Id = extractBusId( bus4 );
    
    try {

        JanzCanIo io( canIoArgs );

        io.echoAll(false);

        vector<byteType> data;
        idType txId = 0x01, rxId = 0x00;
        carma::canbus::Message rxMsg, txMsg;  // Tx and rx CAN msgs.
        int missedMsgs = 0;
        double msgRate = 0;
        carma::util::Time time;

        timespec st, ft, sleep, diff;  // start, finish, sleep and elapsed time.

        bool msgCorrect = false;
        bool integrityPass = true;

        txPriorityType priority = static_cast<txPriorityType>(prio);

        sleep.tv_sec = 0;
        sleep.tv_nsec = 5000000; // Sleep for 50ms

        // Perform the following tests...
        // a) Construction of CanIo object.
        // b) Verify send and receipt of messages.
        // c) Verify message rates.
        // d) Verify message contents and integrity.
        // d) Retrieve bus status.


        // Send and receive 20000 msgs each with a different payload.
        cout << "Testing msg rates with " << MSG_BATCH_SIZE << " msg batches."
            << endl << endl;
        cout << " Bytes .......... Sustained ";
        map<busIdType, busStatusType> busStatus = io.getBusStatus();
        map<busIdType, busStatusType>::iterator bsi;
        for ( bsi = busStatus.begin(); bsi != busStatus.end(); ++bsi ) 
            cout << " | Bus" << bsi->first << " rx/tx";
        cout << endl;
        cout << "--------------------------------------------------------"
             << "--------" << endl;
        for (vector<byteType>::size_type i = 0; i < 9; i++) {

            rxId = 0x00;
            txId = 0x01;

            cout << i << " byte payload." << flush;

            // Insert a byte of data...
            if (i != 0) {
                data.insert(data.end(), static_cast<byteType>(i));
            }

            // Get the start time...
            clock_gettime(CLOCK_REALTIME, &st);	

            while (rxId < 20900) {

                // Post the messages in batches large enough
                // to fill the onboard buffers...
                for (int j = 0; j < MSG_BATCH_SIZE; j++) {	
                    txMsg.setId(txId);
                    txMsg.setData(data);
                    txMsg.setBusId(txBus2Id);
                    io.postMessage(txMsg, priority); // odd
                    ++txId;

                    txMsg.setId(txId);
                    txMsg.setData(data);
                    txMsg.setBusId(txBus3Id);
                    io.postMessage(txMsg, priority); // even
                    ++txId;

                    if ((txId % 1499 ) == 0) {
                        cout << "." << flush;
                    }
                }

                // Retrieve them in equivalent batches...
                for (int k = 0; k < 2 * MSG_BATCH_SIZE; k++) {

                    rxMsg = io.getMessage();	
                    rxId = rxMsg.getId();
                    data = rxMsg.getData();

                    // Verify message integrity every 1000 messages 
                    if ((rxId % 2 ) == 0) // Even id come from bus1
                        msgCorrect = ( rxMsg.getBusId() == rxBus1Id );
                    else // Odd id come from bus4
                        msgCorrect = ( rxMsg.getBusId() == rxBus4Id );

                    // Verify that rx bus and data size are correct...
                    msgCorrect = ( msgCorrect && (data.size() == i));

                    // Verify that data is correct...
                    for (unsigned int l = 0; l < i; l++) {
                        msgCorrect = (data[l] == l+1);
                        if (!msgCorrect) {
                            break;
                        }
                    }

                    // Output '.' for correct '!' for incorrect.
                    // cout << (msgCorrect ? "." : "!") << flush;
                    integrityPass = (integrityPass ? msgCorrect : false);
                }
            }

            // Get finish time...
            clock_gettime(CLOCK_REALTIME, &ft);

            // Calculate elapsed time.
            diff.tv_sec = ft.tv_sec - st.tv_sec;
            if (ft.tv_nsec >= st.tv_nsec) {
                diff.tv_nsec = ft.tv_nsec - st.tv_nsec;
            } else {
                diff.tv_sec -= 1;
                diff.tv_nsec = 1000000000 - abs(ft.tv_nsec - st.tv_nsec);
            }

            // Calculate message rate...
            msgRate = rxId / 
                2.0 * (diff.tv_sec + ((1.0e-9)*diff.tv_nsec));
            cout << " " << msgRate << " msgs/s " << flush;

            // Print out the CanIo determined statistics (1/2 s updates)...
            map<busIdType, busStatusType> busStat = io.getBusStatus();
            map<busIdType, busStatusType>::iterator si;
            for ( si = busStat.begin(); si != busStat.end(); ++si ) {
               
                cout << setprecision( 3 )
                     << si->second.rxMsgRate / 1000.0 << "k/"
                     << si->second.txMsgRate / 1000.0 << "k "; 
            }
            cout << endl; 
        }

        // Output results...	
        cout << endl << "Results" << endl;
        cout << "Messages sent " << (rxId * 9) << endl;
        cout << "Messages lost " << missedMsgs << endl;
        cout << "Message integrity tests " 
            << (integrityPass ? "passed." : "failed.") << endl << endl;

    } catch (const carma::canbus::TxBufferFullException &tbfex) {
        cerr << "TxBufferFullException caught()!" << endl;
        return 1;
    } catch (carma::util::ErrorException &err) {
		cerr << endl << err.what() << endl;
        cerr << "Exception caught in main - make sure Bus 1 connects to Bus 2"
            << endl;
		return 0;
	} catch (...) {
		cout << "Caught an exception." << endl;
		return 0;
	}
    return 0;
}
