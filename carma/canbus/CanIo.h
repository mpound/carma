/** @file
 *  Declaration of carma::canbus::CanIo interface.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.37 $
 * $Date: 2012/08/04 00:00:43 $
 * $Id: CanIo.h,v 1.37 2012/08/04 00:00:43 abeard Exp $
 */
#ifndef CARMA_CANBUS_CANIO_H
#define CARMA_CANBUS_CANIO_H

#include "carma/canbus/CanOutput.h"
#include "carma/canbus/Message.h"

namespace carma {
namespace canbus {

/**
 * CanIo interface.
 * Abstract base class to define core CANbus communication.  
 */
class CanIo : public CanOutput {
public:

    /**
     * Virtual destructor.
     * Assures that a call to destroy CanIo memory will actually destroy
     * the inherited class it came from.
     */
    virtual ~CanIo( );

    /**
     * Retrieve a CAN message.
     * This routine should block until a message is available.
     * @return carma::canbus::Message object.
     */
    virtual carma::canbus::Message getMessage( ) = 0;

    /**
     * Retrieve bus statistics.
     * The default implementation returns an empty BusStatusMap.
     * @return carma::canbus::BusStatusMap 
     */
    virtual BusStatusMap getBusStatus( ) const;

    /**
     * Enable or disable echoing sent messages back through the read interface.
     * The default implementation is a no-op.
     * @param enable Echo CAN messages if true, don't if false.
     */
    virtual void echoAll( bool enable );

    /**
     * Clear any underlying read buffers.
     * Useful for startup when we want to begin from a known reset state.
     * Default implementation is a no-op.
     */
    virtual void clearReadQueue( );

    /**
     * Set timestamp echo latency.
     * By default this method is a no-op.
     * @param tsLatency Timestamp latency in microseconds.
     * @param busId Bus identifier.
     */
    virtual void setTimestampEchoLatency( int tsLatency, busIdType busId );

    /**
     * Queue CAN message to be read for simulation purposes.
     * By default this method is a no-op.
     * @param msg Message object to queue. 
     */
    virtual void queueMessage( const Message & msg );

protected:

private:
    
    // There are no private methods or data.

}; // End class CanIo
}} // End namespace carma::canbus
#endif
