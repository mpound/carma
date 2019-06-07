/** @file
 * RemoteCanMaster class declaration.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2005/01/07 23:28:14 $
 * $Id: RemoteCanMaster.h,v 1.1 2005/01/07 23:28:14 abeard Exp $
 */
#ifndef CARMA_CANBUS_TEST_REMOTECANMASTER_H
#define CARMA_CANBUS_TEST_REMOTECANMASTER_H

// Stl
#include <iostream>

// Carma includes
#include "carma/canbus/Master.h"
#include "carma/canbus/InetCan.h"

namespace carma {
namespace canbus {
namespace test {

    /**
     * RemoteCanMaster class. 
     * This class is just a proof of concept for a clever trick I had.  
     * The trick is to use the InetCan Can-Over-Ip Client class as the 
     * underlying io communications layer for a canbus::Master derivative.  
     * This effectively provides a remote Can Master variant.  The concept
     * isn't incredibly useful in production Carma, but may find use 
     * on the sza and when debugging. 
     * This is possible due to the fact that InetCan (a CanOverIp client) and
     * CanIo, share the same interface (namely CanIoBase).
     */
    class RemoteCanMaster : public carma::canbus::Master {
    public:
    
        /**
         * Constructor
         * @param hostname Can-Over-Ip server host.
         */
        RemoteCanMaster(std::string hostname);

        /**
         * Destructor
         */
        ~RemoteCanMaster();

        /**
         * Post a message to CAN.
         */
        void postMessage(
            const carma::canbus::Message &msg,
            carma::canbus::txPriorityType prio = carma::canbus::NORMAL);

        /**
         * Retrieve a message from CAN.
         */
        carma::canbus::Message getMessage();

    protected:
        
        // There are no protected methods or data.
        
    private:

        void updateStatus();

        carma::canbus::InetCan remoteIo_; // CanOverIp client.

    }; // End class RemoteCanMaster
}}} // End namespace carma::canbus::test;
#endif
    
            
            
    

