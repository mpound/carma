/** @file
 * Declaration of carma::canbus::SocketCan class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.6 $
 * $Date: 2012/08/04 00:00:45 $
 * $Id: SocketCan.h,v 1.6 2012/08/04 00:00:45 abeard Exp $
 */
#ifndef CARMA_CANBUS_SOCKETCAN_H
#define CARMA_CANBUS_SOCKETCAN_H

#include "carma/canbus/CanIo.h"
#include "carma/canbus/Message.h"
#include "carma/canbus/Types.h"

#include <boost/thread.hpp>
#include <boost/thread/mutex.hpp>
#include <vector>

namespace carma {
namespace canbus {

/**
 * Implements the CanIo interface using the socketcan framework.
 */
class SocketCan : public carma::canbus::CanIo {
public:

    /**
     * Constructor for use with all busses on the system.
     */
    SocketCan( );

    /**
     * Constructor for use with specific busses.
     * @param canInterfaces Vector of interface names (e.g. ["can0", "can1"]).
     */
    explicit SocketCan( const ::std::vector< ::std::string > & canInterfaces );

    /**
     * Destructor.
     */
    virtual ~SocketCan( );

    /**
     * Wait and retrieve a CAN message.
     * This routine is boost::thread interruptable and carma thread quitable.
     */
    carma::canbus::Message getMessage( );

    /**
     * Post a message to a single or multiple CAN busses.
     */
    void postMessage( const carma::canbus::Message & msg,
                      carma::canbus::txPriorityType p = carma::canbus::NORMAL );

    /**
     * Retrieve bus statistics indexed by bus id.
     */
    BusStatusMap getBusStatus() const;

    /**
     * Echo all CAN messages.
     * This method causes sent messages to be echoed back through the
     * read interface.
     */
    void echoAll( bool enable ); 

    /**
     * Parse bus id given an interface name (e.g. can0).
     */
    static busIdType parseBusId( const std::string & canInterfaceName );

protected:

    // No protected methods.

private:

    void 
    commonConstruction( const ::std::vector< ::std::string > & canInterfaces );

    void updateThread( ); // Update bus statistics.

    struct BusInfo {
        const ::std::string name;  /**< CAN interface name (e.g. can0). */ 
        const busIdType id;        /**< Bus identifier.*/
        const int wsfd;            /**< Write socket file descriptor. */
        int rxCount;               /**< Running rx count for msg rates. */
        int txCount;               /**< Running tx count for msg rates. */
        int oneMinRxCount;         /**< One Minute rx count for msg rates. */
        int oneMinTxCount;         /**< One Minute tx count for msg rates. */
        double lastUpdateMJD;      /**< Time of last msg rate update.*/
        double lastOneMinUpdateMJD; /**< Last 1min msg rate update.*/
        carma::canbus::busStatusType status; /**< Bus status. */

        BusInfo( const ::std::string & name,
                 const busIdType id,
                 const int wsfd );

        void updateBusStatus( );

    };

    ::std::vector< int > fileDescriptors_;
    int maxFileDescriptor_;

    typedef ::std::map< int, BusInfo > BusInfoMap; // Indexed by interface index
    typedef ::std::map< int, int > BusIdToInterfaceMap;

    BusInfoMap busses_; 
    BusIdToInterfaceMap busIdToIf_;

    mutable boost::mutex mutex_;
    boost::thread updateThread_;

}; // class SocketCan

} } // namespace carma::canbus
#endif
