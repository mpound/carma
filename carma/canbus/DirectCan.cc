/** @file
 * Definition of carma::canbus::DirectCan class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.13 $
 * $Date: 2005/08/23 00:28:49 $
 * $Id: DirectCan.cc,v 1.13 2005/08/23 00:28:49 abeard Exp $
 */

// Carma includes
#include "carma/canbus/DirectCan.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Types.h"

using namespace std;
using namespace carma::canbus;
using namespace carma::util;

// -----------------------------------------------------------------------------
DirectCan::DirectCan() 
{
    // Open IPQs.
    // Only open IPQs that are already open.  If it isn't already open
    // then it means the CanMaster that will read and write DirectCan
    // messages is either not running or has experienced a fatal error.  
    try {
        writeIpq_ = new IPQwriter<carma::canbus::Message>(CAN_INPUT_IPQ,
                true, IPQ_BUFFER_SIZE);
        readIpq_ = new IPQreader<carma::canbus::Message>(CAN_OUTPUT_IPQ,
                true, IPQ_BUFFER_SIZE);
        
        // Set the number of elements available in ipqs to zero.  This
        // causes this DirectCan object to only look at messages received
        // in the IPQs after this call was made instead of the perhaps
        // thousands of old or stagnant messages.
        readIpq_->setNoneAvailable();
            
    } catch (carma::util::ErrorException &err) {
        ostringstream os;
        os << "DirectCan::DirectCan() - Exception caught while trying to "
            "open IPQs.  Check that a CanMaster process is running on the "
            "bus you wish to communicate with using DirectCan.  Also make "
            "sure you are running DirectCan with the same privledges as the "
            "CanMaster process.  Exiting." 
           << endl << (string)err.what() << ends;
        throw CARMA_ERROR (os);
    }
}

// -----------------------------------------------------------------------------
carma::canbus::DirectCan::~DirectCan()
{
    delete readIpq_;
    delete writeIpq_;
}

// -----------------------------------------------------------------------------
carma::canbus::Message DirectCan::getMessage() 
{
    // Check to make sure that the buffer isn't being overflowed 
    int msgsLost = readIpq_->read();
    if (msgsLost > 0) {
        ostringstream err;
        err << "DirectCan::getMessage() - CAN Message IPQ is "
            << "being written to much faster than you are reading from "
            << "it! " << msgsLost << " messages lost since last read." 
            << ends;
       throw CARMA_EXCEPTION(carma::canbus::BufferOverflowException, err);
    } 
    return *(Message *)readIpq_;
}

// -----------------------------------------------------------------------------
carma::canbus::Message DirectCan::getMessage(idType id) 
{
    bool got = false;
    carma::canbus::Message msg;
    while (!got) {
        msg = getMessage();
        got = (msg.getId() == id);
    }
    return msg;
}

// -----------------------------------------------------------------------------
void DirectCan::postMessage(
    const carma::canbus::Message &msg,
    carma::canbus::txPriorityType prio)
{
    *(Message *)writeIpq_ = msg;
    writeIpq_->write();
}
