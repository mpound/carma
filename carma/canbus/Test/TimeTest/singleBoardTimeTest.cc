/** @file
 * Program to characterize latency of posting a time stamp to the Janz board. 
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.6 $
 * $Date: 2012/07/19 18:41:50 $
 * $Id: singleBoardTimeTest.cc,v 1.6 2012/07/19 18:41:50 abeard Exp $
 */

// System includes
#include <unistd.h>

// C++ includes
#include <fstream>
#include <vector>
#include <iostream>

// Carma includes
#include "carma/canbus/CanDio.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"


using namespace carma::canbus;
using namespace carma::util;
using namespace std;

carma::canbus::Message getTime()
{
    idType address = createId(false, 0x0, 0x0, 0x001);

    double fullMjd;  // Full MJD.
    int mjd;         // Mean Julian Day and difference as microseconds
    unsigned int fracMjd;     // Billionths of a day

    busIdType busId = ALL_BUSSES;

    fullMjd = Time::MJD();
    mjd = (int)fullMjd;
    fracMjd = (int) ((fullMjd - mjd) * 1000000000);

    cerr << "Post mjd: " << mjd << " fmjd " << fracMjd << endl;
    vector<byteType> data;

    uShortToData(data, mjd);
    uLongToData(data, fracMjd);
    padWithZeros(data);
    carma::canbus::Message msg(address, data, busId);
    return msg;
};

/**
 * @version $Revision: 1.6 $
 *
 * @description Program to test latency of posting a time stamp to the CANbus.
 * this routine differs from timeTest in that it utilitizes the ECHO flag on 
 * a CAN msg which instructs the Janz board to echo the message back to the host
 * upon successful transmission.  
 *
 * @usage singleBoardTimeTest [board=0] [canbus=0]
 *
 * @key board 0 i The board number of the janz/carma cPCI carrier board.
 * @key canbus 0 i The canbus (or modulbus slot) of the CAN card.
 *
 * @logger TEST_FACILITY carma.test.canbus.TimeTest.singleBoardTimeTest
 */
int Program::main()
{

    try {
        int boardId = getIntParameter("board");
        int canbusId = getIntParameter("canbus");
        unsigned int mjd, fracmjd, asUs; 
        double timeStamp, rxMjd, diff;
        vector<byteType> data;
        carma::canbus::Message msg;
        ofstream out("sbttr.txt");
        CanDio io(boardId, canbusId); 
        
        // Set resetLo just incase...
        io.reset();

        while (true) {
            
            // Send a timestamp 
            cerr << "Sending: " << endl;
            msg = getTime();
            io.postMessage(msg);

            // Retrieve the echoed timestamp.
            msg = io.getMessage();
            cerr << "Received!" << endl;

            // Determine difference between rxMjd and timestamp
            data = msg.getData();
            mjd = dataToUshort(data);
            fracmjd = dataToUlong(data);

            cerr << "Get: mjd " << mjd << " fracmjd " << fracmjd << endl;

            timeStamp = static_cast<double>(mjd) +
                (static_cast<double>(fracmjd) * 1.0e-9);

            rxMjd = msg.getRxMjd();

            diff = rxMjd - timeStamp;
            asUs = static_cast<int>(diff * 86400.0 * (1000000.0));
            out << asUs << endl;
            // Sleep for 1 second.
            sleep(1);
        }

        return 0;

    } catch (carma::util::ErrorException &ex) {
        cerr << ex.what() << endl;
    } catch (...) {
        cerr << "Unknown exception caught in Program::main()" << endl;
    }
    return 1;
}
