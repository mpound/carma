#include <iostream>

#include "carma/canbus/CanDio.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

using namespace carma::canbus;
using namespace carma::util;
using namespace std;

carma::canbus::Message setTime()
{
    idType address = createId(false, 0x0, 0x0, 0x001);
    double fullMjd = Time::MJD();
    int mjd = static_cast<int>(fullMjd);
    unsigned int fracMjd = static_cast<int>((fullMjd - mjd) * 1000000000);

    vector<byteType> data;

    uShortToData(data, mjd);
    uLongToData(data, fracMjd);
    padWithZeros(data);
    carma::canbus::Message msg(address, data, ALL_BUSSES);
    return msg;
} // End setTime()

void analyzeTimeSync(const carma::canbus::Message & ts)
{
    vector<byteType> data = ts.getData();
    unsigned int mjd = dataToUshort(data);
    unsigned int fracmjd = dataToUlong(data);
    double timeStamp = static_cast<double>(mjd) + 
        (static_cast<double>(fracmjd) * 1.0e-9);

    double rxMjd = ts.getRxMjd();
    double diff = rxMjd - timeStamp;
    unsigned int microsec = static_cast<unsigned int>(diff * 86400.0 * 1.0e6);
    cout << microsec << endl;

} // End analyzeTimeSync

/**
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.canbus.TimeTest.timeStampLatency
 */
int Program::main()
{
    timespec req = {0, 1000000}, rem; // 1ms
    
    try {
        CanDio source(0, 0); // Time sync source
        CanDio sink(0, 1);   // Time sync sink
        carma::canbus::Message timeSync; 

        while (true) {
            source.postMessage( setTime(), HIGH );
            timeSync = sink.getMessage();
            analyzeTimeSync( timeSync );
            nanosleep(&req, &rem);
        } // End loop forever

    } catch (const std::exception & ex) {
        cerr << ex.what() << endl;
        return EXIT_FAILURE;
    } catch (...) {
        cerr << "No way to delay that trouble comin' everyday." << endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
} // End Program::main
