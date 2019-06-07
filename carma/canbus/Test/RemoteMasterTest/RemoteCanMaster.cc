/** @file
 * carma::canbus::test::RemoteCanMaster class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2005/01/07 23:28:14 $
 * $Id: RemoteCanMaster.cc,v 1.1 2005/01/07 23:28:14 abeard Exp $
 */

#include "carma/canbus/Test/RemoteMasterTest/RemoteCanMaster.h"
#include "carma/canbus/Utilities.h"
 
using namespace carma::canbus;
using namespace carma::canbus::test;
using namespace std;

// -----------------------------------------------------------------------------
RemoteCanMaster::RemoteCanMaster(std::string hostname) : 
    Master(), 
    remoteIo_(hostname)
{
    // Now, everything else is exactly the same as a typical Master derivative.
    // Here you would addDevices, publish DOs, etc here.  
}

// -----------------------------------------------------------------------------
RemoteCanMaster::~RemoteCanMaster()
{
    // Nothing
}

// -----------------------------------------------------------------------------
void RemoteCanMaster::postMessage(
    const carma::canbus::Message &msg,
    carma::canbus::txPriorityType prio)
{
    remoteIo_.postMessage(msg, prio);
}

// -----------------------------------------------------------------------------
carma::canbus::Message RemoteCanMaster::getMessage()
{
    carma::canbus::Message msg = remoteIo_.getMessage();
    idType id = msg.getId();
    apiType api;
    nodeType node;
    msgType msgid;

    fromId(api, node, msgid,  id);
    
    cout << "CAN: api " << api << ", node " << node << ", msg 0x" << hex
        << msgid << "." << endl;
    return msg;
}

// -----------------------------------------------------------------------------
void RemoteCanMaster::updateStatus()
{
    // Nothing to update
}
