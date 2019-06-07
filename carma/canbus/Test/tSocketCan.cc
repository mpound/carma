/** 
 * Test application for SocketCan class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2012/08/03 22:59:49 $
 * $Id: tSocketCan.cc,v 1.1 2012/08/03 22:59:49 abeard Exp $
 */

#include "carma/canbus/SocketCan.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <boost/foreach.hpp>
#include <iostream>
#include <sys/times.h>
#include <unistd.h>

using namespace boost;
using namespace carma::canbus;
using namespace carma::util;
using namespace std;

/** 
 * @version $Revision: 1.1 $
 * 
 * @description
 * Test for SocketCan class. Connect busses together to use.
 * \nExample: tSocketCan bus0=can0 bus1=can1 
 *
 * @usage tSocketCan [s=false] [bus0=can0] [bus1=can1]
 * 
 * @key s false b Use 1 SocketCan instance per bus (default shared instance).
 * @key bus0 can0 s First CAN interface.
 * @key bus1 can1 s Second CAN interface.
 * @key batchSize 200 i Size of batches of messages to send and receive.
 *
 * @logger TEST_FACILITY carma.test.canbus.tSocketCan
 */
int Program::main ()
{
    const bool instancePerBus( getBoolParameter("s") );
    const string bus0If( getStringParameter( "bus0") );
    const string bus1If( getStringParameter( "bus1") );
    const int msgBatchSize( getIntParameter( "batchSize" ) );

    typedef boost::shared_ptr< SocketCan > SocketCanPtr;

    SocketCanPtr rxIo, txIo;
    
    try {

        if ( instancePerBus ) {
            vector< string > rxIfs, txIfs;
            rxIfs.push_back( bus0If );
            txIfs.push_back( bus1If );
            rxIo = SocketCanPtr( new SocketCan( rxIfs ) );
            txIo = SocketCanPtr( new SocketCan( txIfs ) );
        } else {
            vector< string > canIfs;
            canIfs.push_back( bus0If );
            canIfs.push_back( bus1If );
            rxIo = SocketCanPtr( new SocketCan( canIfs ) );
            txIo = rxIo;
        }

        rxIo->echoAll( false );
        txIo->echoAll( false );
    
        const busIdType rxBusId = SocketCan::parseBusId( bus0If );
        const busIdType txBusId = SocketCan::parseBusId( bus1If );

        idType txId = 0x01, prevId = 0x00, rxId = 0x00;
        carma::canbus::Message rxMsg, txMsg;  // Tx and rx CAN msgs.
        DataVector data;
        int missedMsgs = 0;

        bool integrityPass = true;

        cout << "Testing msg rates with " << msgBatchSize << " msg batches.";
        cout << endl << endl;
        cout << " Byte Payload ...............   Sustained   |   .5s Tx   |"
            << " .5s Rx " << endl;
        cout << "--------------------------------------------------------"
            << "--------" << endl;
        for ( DataVector::size_type i = 0; i < 9 && !missedMsgs; ++i ) {

            rxId = 0x00;
            txId = 0x01;
            prevId = 0x00;

            cout << i << " byte payload." << flush;

            if (i != 0) 
                txMsg << static_cast<byteType>( i );

            const double startMJD( Time::MJD() );

            while ( rxId < 20900 && !missedMsgs ) {

                // Utilize onboard buffers with large batches....
                for ( int j = 0; j < msgBatchSize; j++ ) {	
                    txMsg.setId( txId++ );
                    txMsg.setBusId( txBusId );
                    txIo->postMessage( txMsg );
                    if ( (txId % 1500) == 0 ) 
                        cout << "." << flush;
                }

                // Retrieve them in equivalent batches...
                for ( int k = 0; k < msgBatchSize && !missedMsgs; ++k ) {

                    rxMsg = rxIo->getMessage();	
                    rxId = rxMsg.getId();
                    data = rxMsg.getData();

                    // Did we miss any? 
                    if ( ( rxId - prevId ) != 1 ) 
                        missedMsgs += ( rxId - prevId );

                    prevId = rxId;

                    // Verify that rx bus and data size are correct...
                    bool msgCorrect = ( ( rxMsg.getBusId() == rxBusId ) && 
                                        ( data.size() == i ) );
                    for ( unsigned int l = 0; l < i && msgCorrect; ++l ) 
                        msgCorrect = ( data[l] == l+1 );

                    integrityPass = ( integrityPass ? msgCorrect : false );
                }
            }

            const double elapsedSecs( ( Time::MJD() - startMJD ) * 
                                      Time::SECONDS_PER_DAY );
            const double msgRate( rxId / elapsedSecs );

            cout << " " << msgRate << " msgs/s " << flush;

            // Print out the CanIo determined statistics (1/2 s updates)...
            BusStatusMap busStat = txIo->getBusStatus();
            BusStatusMap::iterator si = busStat.find( txBusId );
            cout << si->second.txMsgRate << " msg/s " << flush;
            busStat = rxIo->getBusStatus();
            si = busStat.find(rxBusId);
            cout << si->second.rxMsgRate << " msg/s " << endl;
        }

        // Output results...	
        cout << endl << "Results" << endl;
        cout << "Messages sent " << (rxId * 9) << endl;
        cout << "Messages lost " << missedMsgs << " total" << flush;
        const BusStatusMap rxBusStats = rxIo->getBusStatus( );
        BOOST_FOREACH( const BusStatusMap::value_type & val, rxBusStats ) {
            cout << ", bus " << val.first << flush;
            cout << " - " << val.second.fastMsgsLost << flush;
        }
        if ( rxIo != txIo ) {
            const BusStatusMap txBusStats = txIo->getBusStatus( );
            BOOST_FOREACH( const BusStatusMap::value_type & val, txBusStats ) {
                cout << ", bus " << val.first << flush;
                cout << " - " << val.second.fastMsgsLost << flush;
            }
        }
        cout << "." << endl;
        cout << "Message integrity tests " 
            << (integrityPass ? "passed." : "failed.") << endl << endl;

        if ( missedMsgs ) {
            cout << "Try decreasing the batch size "
                 << "to prevent missed messages." << endl;
            return 1;
        }

    } catch ( ... ) {
        cerr << getStringForCaught() << endl;
		return 1;
	} 

    return 0;
}
