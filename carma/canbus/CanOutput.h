/** @file
 * Declaration of carma::canbus::CanOutput interface.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.16 $
 * $Date: 2008/04/23 21:26:48 $
 * $Id: CanOutput.h,v 1.16 2008/04/23 21:26:48 abeard Exp $
 */
#ifndef CARMA_CANBUS_CANOUTPUT_H
#define CARMA_CANBUS_CANOUTPUT_H

#include "carma/canbus/Types.h"

namespace carma {
namespace canbus {

class Message;

/**
 * CanOutput interface.
 * CanOutput is only allowed to post messages to the CAN.  This is required
 * to provide enforcement that only canbus::Master can read and process 
 * messages while allowing canbus::Device derivatives to post messages to 
 * the canbus.
 */
class CanOutput {
public:

    virtual ~CanOutput( ) { };

    /**
     * Post a CAN message.
     * Send a carma::canbus::Message to a single or multiple CANbusses as 
     * specified by the Message::setBusId method.  To send to all CAN busses
     * use the carma::canbus::ALL_BUSSES constant.
     * @param msg The carma::canbus::Message to post.
     * @param prio Priority of message.
     */
    virtual void postMessage(
            const carma::canbus::Message & msg,
            carma::canbus::txPriorityType  prio = carma::canbus::NORMAL) = 0;

protected:
    
    // There are no protected methods or data.

private:

    // There are no private methods or data.

};  // End class CanOutputBase.
}} // End namespace carma::canbus
#endif
