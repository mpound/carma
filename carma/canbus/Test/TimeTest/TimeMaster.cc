/** @file
 * Skeleton master class for timesync latency tests.  
 *
 * <dl><dt><b>Author: </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.4 $
 * $Date: 2005/11/04 17:50:24 $
 * $Id: TimeMaster.cc,v 1.4 2005/11/04 17:50:24 abeard Exp $
 */

#include "carma/canbus/Test/TimeTest/TimeMaster.h"
#include "carma/canbus/exceptions.h"
#include "carma/util/Time.h"
#include "carma/canbus/Utilities.h"
#include "carma/canbus/Types.h"

using namespace std;
using namespace carma::canbus;
using namespace carma::canbus::test;
using namespace carma::util;

// -----------------------------------------------------------------------------
TimeMaster::TimeMaster(int boardId, int modulbusId) : 
    Master(boardId, modulbusId), out_("tts.txt")
{
    // Start a separate thread to block on run.
    pthread_t runThreadId;
    int status = pthread_create(&runThreadId, NULL, runThreadEntry, 
        static_cast<void *>(this));
    if (status != 0) throw CARMA_EXCEPTION(
        carma::canbus::PthreadFailException,
        "TestMaster::TestMaster() - Unable to start run thread." +
        (const string)strerror(status));

}

// -----------------------------------------------------------------------------
void TimeMaster::updateStatus() 
{
    // Nothing for now
}

// -----------------------------------------------------------------------------
void TimeMaster::setTime() 
{
    
    idType address = createId(false, 0x0, 0x0, 0x001);

    double fullMjd, postMjd, diffMjd;  // Full MJD.
    int mjd, diffAsUs;     // Mean Julian Day and difference as microseconds
    unsigned int fracMjd;     // Billionths of a day
    map<busIdType, busStatusType> busStats = Master::getBusStatus();
    map<busIdType, busStatusType>::iterator bi = busStats.begin();

    // Make sure we're only dealing with a single bus...
    if (busStats.size() == 1) {
    
        busStateType state = bi->second.state;
        busIdType busId = bi->first;

        // Try to send messages unless the bus state is BUS_OFF
        if (state != BUS_OFF) {
            fullMjd = Time::MJD();
            mjd = (int)fullMjd;
            fracMjd = (int) ((fullMjd - mjd) * 1000000000);

            vector<byteType> data;

            uShortToData(data, mjd);
            uLongToData(data, fracMjd);
            padWithZeros(data);
            Message msg(address, data, busId);
            postMessage(msg);
        } else {
            return;
        }
    } else {
        cerr << "Number of busses is != 1!!!" << endl;
        return;
    }
    
    // Get current MJD 
    postMjd = Time::MJD();

    // Calculate the difference
    diffMjd = postMjd - fullMjd;

    // Convert it to microseconds = (%day)(sec/day)(s/10-6us).
    diffAsUs =  static_cast<int>(diffMjd * 86400.0 * (1000000.0));

    out_ << diffAsUs << endl;
}

// -----------------------------------------------------------------------------
void * TimeMaster::runThreadEntry(void *arg)
{
    try {
        TimeMaster *This = static_cast<TimeMaster *>(arg);
        This->run();
    } catch (carma::util::ErrorException &ex) {
        cerr << ex << endl;
    } catch (...) {
        cerr << "Unknown exception in TimeMaster::runThreadEntry." << endl;
    }
    exit(EXIT_FAILURE);
}
 

