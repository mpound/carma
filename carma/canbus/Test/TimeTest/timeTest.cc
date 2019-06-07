/** @file
 * timeTest main
 *
 * <dl><dt><b>Author: </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.3 $
 * $Date: 2006/04/14 20:34:49 $
 * $Id: timeTest.cc,v 1.3 2006/04/14 20:34:49 tcosta Exp $
 */

#include <fstream>
#include <iostream>

#include "carma/canbus/CanDio.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Types.h"
#include "carma/canbus/Utilities.h"
#include "carma/canbus/Test/TimeTest/TimeMaster.h"
#include "carma/util/Program.h"
#include "carma/util/ErrorException.h"

using namespace carma::canbus;
using namespace carma::canbus::test;
using namespace carma::util;

using namespace std;

/**
 * @version $Revision: 1.3 $
 * 
 * @description Small test application to measure time stamp latencies...
 *
 * @usage timeTest [board=0] 
 *
 * @key board 0 i The board number of the janz/carma cPCI carrier board
 *
 * @logger TEST_FACILITY carma.test.canbus.TimeTest.timeTest
 */
int Program::main()
{

    try {
        int boardId = getIntParameter("board");
        ofstream out("ttr.txt");
        double timeStamp, rxMjd, diff;
        vector<byteType> data;
        carma::canbus::Message msg;
        TimeMaster master(boardId, 0);
        CanDio     canDio(boardId, 1);
        unsigned int mjd, fracmjd, asUs; // days and frac/day respectively

        while (true) {
            
            // Get timesync message from bus 1 (it's sent on bus 0)
            msg = canDio.getMessage();

            // Determine the difference between the time stamp in the message
            // and the time it was received (as determined by the CanIo class).
            data = msg.getData();
            
            mjd = dataToUshort(data);
            fracmjd = dataToUlong(data);
            
            timeStamp = static_cast<double>(mjd) + 
                (static_cast<double>(fracmjd) * 1.0e-9);
            
            rxMjd = msg.getRxMjd();

            diff = rxMjd - timeStamp;
            asUs =  static_cast<int>(diff * 86400.0 * (1000000.0));
            out << asUs << endl;
            
        }

        return 0;
    } catch (carma::util::ErrorException &ex) {
        cerr << ex.what() << endl;
    } catch (...) {
        cerr << "Unknown exception caught in Program::main() - exiting" << endl;
    }
    return 1;
}
 
