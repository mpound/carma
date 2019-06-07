
/** @file
 * Declaration of carma::canbus::InetCan class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.6 $
 * $Date: 2012/07/25 17:51:26 $
 * $Id: InetCan.h,v 1.6 2012/07/25 17:51:26 abeard Exp $
 */
#ifndef CARMA_CANBUS_INETCAN_H
#define CARMA_CANBUS_INETCAN_H

#include "carma/canbus/CanIo.h"

namespace carma {
namespace canbus {

/**
 * InetCan class.
 * A CAN-over-tcp/ip client class which conforms to the CanIo interface.
 */
class InetCan : public carma::canbus::CanIo {
public:

    /**
     * Constructor.
     * Establish a client session with a CanOverIp server.  By default,
     * the acceptance filter is set so that ALL messages are sent over the 
     * connection.  Keep in mind that this may be a very large number 
     * of messages.  Also keep in mind that the CanOverIp server ignores
     * certain critical messages such as time stamps and resets. 
     * @param hostname Hostname of machine running CanOverIp server.
     * @param ac Can message filter acceptance code (default all).
     * @param am Can message filter acceptance mask (default all).
     * @param busac Can bus filter acceptance code (default ALL_BUSSES).
     */
    InetCan(
        const std::string hostname, 
        unsigned int ac = 0xffffffff,
        unsigned int am = 0xffffffff,
        unsigned short busac = 0xffff);

    /**
     * Destructor.
     */
    ~InetCan();

    /**
     * Post a CAN Message.
     * @param msg canbus::Message to post.
     * @param prio Message priority - has no effect for InetCan!
     */
    void postMessage(
        const carma::canbus::Message& msg,
        carma::canbus::txPriorityType prio = carma::canbus::NORMAL);

    /**
     * Get a CAN Message.
     * This method blocks until a message is received.
     * @return carma::canbus::Message.
     */
    carma::canbus::Message getMessage();

    /**
     * Add an additional acceptance filter.
     * @param ac Can message filter acceptance code.
     * @param am Can message filter acceptance mask.
     * @param busac Can Bus filter acceptance code.
     */
    void addFilter(unsigned int ac, unsigned int am, unsigned short busac);

    /**
     * Clear all acceptance filters.
     * After this method is called, no messages will be received.
     */
    void clearFilters();

protected:

    // There are no protected methods.

private:

    // Message received received from server (contains CAN message).
    struct MsgFromServer {
        unsigned int id;
        unsigned short busId;
        unsigned char size;
        unsigned char data[8];
    }; // 15 Bytes

    // Message for posting to CANbus or setting filter.
    struct MsgToServer {
        unsigned char mode;
        /**
         * @union id_or_ac
         * CAN id or Acceptance code in Network Byte Order.
         */
        union {
            unsigned int id;
            unsigned int ac;
        };
        /**
         * @union
         * busId or busFilter in Network Byte Order.
         * 0xff = All busses.
         */
        union {
            unsigned short busId;
            unsigned short busFilter;
        };
        unsigned char size;
        /**
         * @union
         * CAN message data or 4 byte acceptance mask in NBO
         */
        union {
            unsigned char data[8];   // Data
            unsigned int am;         // Acceptance mask 
        };
    }; // 16 bytes

    // MsgToServer mode
    enum mode {
        SET_READ_FILTER    = 0x00,
        CLEAR_READ_FILTERS = 0x01,
        WRITE_CAN_MSG      = 0x02
    };

    // Private data.
    static const unsigned int READ_PORT = 15000;
    static const unsigned int WRITE_PORT = 15001;

    int writeSock_; // Write socket file descriptor.
    int readSock_;  // Read socket file descriptor.    
    const std::string hostname_; // Who are we connected to?

    // Private methods.
    void sendToServer(const MsgToServer &msg); // Posts to write socket.
     
}; // End class InetCan
}} // End namespace carma::canbus
#endif
