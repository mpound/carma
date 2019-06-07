/** @file
 * Skeleton master class for timesync latency tests.  
 *
 * <dl><dt><b>Author: </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2004/07/09 21:20:49 $
 * $Id: TimeMaster.h,v 1.1 2004/07/09 21:20:49 abeard Exp $
 */

#ifndef CARMA_CANBUS_TEST_TIMEMASTER_H
#define CARMA_CANBUS_TEST_TIMEMASTER_H

#include "carma/canbus/Master.h"
#include <fstream>
#include <vector>

namespace carma {
namespace canbus {
namespace test {

class TimeMaster : public carma::canbus::Master {
public:

    /**
     * Constructor
     */
    TimeMaster(int boardId, int modulbusId);

    /**
     * Update status
     * Does nothing
     */
    void updateStatus();

    /**
     * Overloaded setTime
     */
    void setTime();

protected:

    

private:

    static void * runThreadEntry(void *arg);


    // File iostream to write data to.
    std::ofstream out_;
    
}; // End class TimeTest
}}} // End namespace carma::canbus::Test
#endif
