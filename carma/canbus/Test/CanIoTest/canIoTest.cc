// To perform this test tye bus 0 to bus 1 on a CARMA canbus card.

#include "carma/canbus/exceptions.h"
#include "carma/canbus/JanzCanIo.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <iostream>

#include <sys/times.h>
#include <unistd.h>

using namespace carma::canbus;
using namespace carma::util;
using namespace std;

/** 
 * @version $Revision: 1.24 $
 * 
 * @description
 * Test program for CanIo class. CAN bus 0 must be connected to CAN bus 1 as 
 * the two send CAN messages to each other.
 * \nExample: canIoTest d1=/dev/dpm_00 d2=/dev/dpm_00
 *
 * @usage canIoTest [s (single device-to-object version)] d1=<device name> d2=<device name> 
 * 
 * @key s false b Use 1 CanIo object per device - default 1 object both devs.
 * @key d1 /dev/dpm_00 s CAN device 1 name.
 * @key d2 /dev/dpm_01 s CAN device 2 name
 * @key prio 2 i Tx message priority (1 = HIGH, 2 = NORMAL, 3 = LOW).
 * @key batchSize 400 i Size of batches of messages to send and receive.
 * @key txRetryTest false b Test if Message::disableTxRetry works (disconnect busses).
 *
 * @logger TEST_FACILITY carma.test.canbus.CanIoTest.canIoTest
 */
int Program::main ()
{
    string dev0 = getStringParameter("d1");
    string dev1 = getStringParameter("d2");
    bool txRetryTest = getBoolParameter("txRetryTest");
    int prio = getIntParameter("prio");
    const int MSG_BATCH_SIZE = getIntParameter("batchSize");
    
    bool singleMode = getBoolParameter("s");
    auto_ptr< JanzCanIo > rxIo, txIo;
    busIdType rxBusId, txBusId;
    
    try {
        if (singleMode) {
            rxIo = auto_ptr< JanzCanIo >( new JanzCanIo( dev0.c_str(), true ) );
            txIo = auto_ptr< JanzCanIo >( new JanzCanIo( dev1.c_str(), true ) );
        } else {
            rxIo = auto_ptr< JanzCanIo >( new JanzCanIo( dev0.c_str(), true , 
                                                         dev1.c_str(), true ) );
            txIo = rxIo;
        }

        rxIo->echoAll(false);
        txIo->echoAll(false);

        rxBusId = extractBusId( dev0 );
        txBusId = extractBusId( dev1 );

        vector<byteType> data;
        idType txId = 0x01, prevId = 0x00, rxId = 0x00;
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

        if ( txRetryTest ) {
            // Just send a crapload of messages out both busses.
            for (unsigned int i = 0; i < 50000; i++) {
                txMsg.setId(txId);
                txMsg.setData(data);
                txMsg.setBusId(txBusId);
                txIo->postMessage(txMsg, priority);
                txId++;
                if ((txId %1500) == 0) {
                    cout << "." << flush;
                }
            }

            cout << endl << "DisableTxRetry works successfully - 50000 msgs"
                << " sent." << endl;
            return 0;
        }
                

        // Send and receive 20000 msgs each with a different payload.
        cout << "Testing msg rates with " << MSG_BATCH_SIZE << " msg batches."
            << endl << endl;
        cout << " Byte Payload ...............   Sustained   |   .5s Tx   |"
            << " .5s Rx " << endl;
        cout << "--------------------------------------------------------"
            << "--------" << endl;
        for (vector<byteType>::size_type i = 0; i < 9; i++) {

            rxId = 0x00;
            txId = 0x01;
            prevId = 0x00;

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
                    txMsg.setBusId(txBusId);
                    txIo->postMessage(txMsg, priority);
                    txId++;
                    if ((txId % 1500) == 0) {
                        cout << "." << flush;
                    }

                }

                // Retrieve them in equivalent batches...
                for (int k = 0; k < MSG_BATCH_SIZE; k++) {

                    rxMsg = rxIo->getMessage();	
                    rxId = rxMsg.getId();
                    data = rxMsg.getData();

                    // Did we miss any? This is a stupid test, if I'd missed
                    // any then I'd still be blocked in getMessage waiting for
                    // it.
                    if ((rxId - prevId) != 1) {
                        missedMsgs += (rxId - prevId);
                        cout << "Missed msg " << rxId << endl;
                    }
                    prevId = rxId;

                    // Verify message integrity every 1000 messages 
                    if ((rxId % 2000) == 0) {

                        // Verify that rx bus and data size are correct...
                        msgCorrect = ((rxMsg.getBusId() == rxBusId) 
                                && (data.size() == i));

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
            msgRate = rxId/(double)(diff.tv_sec + ((1.0e-9)*diff.tv_nsec));
            cout << " " << msgRate << " msgs/s " << flush;

            // Print out the CanIo determined statistics (1/2 s updates)...
            map<busIdType, busStatusType> busStat = txIo->getBusStatus();
            map<busIdType, busStatusType>::iterator si;
            si = busStat.find(txBusId);
            cout << si->second.txMsgRate << " msg/s " << flush;
            busStat = rxIo->getBusStatus();
            si = busStat.find(rxBusId);
            cout << si->second.rxMsgRate << " msg/s " << endl;
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
