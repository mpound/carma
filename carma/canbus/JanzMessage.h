/** @file
 * Declaration of carma::canbus::JanzMessage class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2012/07/13 18:55:26 $
 * $Id: JanzMessage.h,v 1.1 2012/07/13 18:55:26 abeard Exp $
 */

#ifndef CARMA_CANBUS_JANZMESSAGE_H
#define CARMA_CANBUS_JANZMESSAGE_H

#include "carma/canbus/Types.h"

namespace carma {
namespace canbus {

    class Message;

    // Typedefs to mimic Janz internal FastMessage structure
    typedef unsigned char BYTE_t;

    typedef struct {
        BYTE_t    unused;      /* driver internal flags */
        BYTE_t    cmd;         /* command specifier */
        BYTE_t    data[14];    /* data array */
    } FastMessage;

    /** 
     * This class encapsulate a Janz CAN message.
     * A CAN message consists of a 29 bit message Id and up to 8
     * bytes of raw data.  This class wraps the CAN message structure
     * defined for the Janz drivers which contains the actual bytes.
     * Since this class may be used to post/send messages to multiple
     * CANbusses controlled by multiple processes on a single linux host,
     * it was necessary to define the concept of a busId which
     * uniquely identifies which bus a message was read from or will be 
     * posted to.  BusIds are generally assigned by the master class and 
     * are assured to be unique by deriving the busId number from the 
     * CAN device filename (e.g. /dev/dpm_00 = busId 0 whereas 
     * /dev/dpm_f0 = busId 31).  Any user of this class must be careful 
     * to use the proper logic to assure that the BusId of a message exists 
     * and is being controlled by a CAN Master.  This is generally handled 
     * by making sure you set the BusId of a tx msg to the same as a message 
     * received from the node you wish to communicate with 
     * (i.e. txMsg.setBusId(rxMsg.getBusId)).  If this is not possible, 
     * but the device name is accessible, one can use the 
     * carma::canbus::extractBusId() routine, or the busId
     * canbus::ALL_BUSSES. If a Message is addressed to a bus that doesn't exist
     * an exception will be thrown when trying to post that message.
     * The 29 bit CAN id itself is represented via a typedef 
     * carma::canbus::idType (a 4 byte unsigned integer), and the data is 
     * represented as a vector<carma::canbus::byteType>, where byteType 
     * is an unsigned one-byte character. 
     */
    class JanzMessage {
    public:

        /**
         * Message type definition.
         * Yes you will wonder 'messageTypeType?' - In CANbus parlance there 
         * are two message Types STANDARD and EXTENDED - this is a typedef to 
         * describe the CAN Message Type thus messageTypeType.
         */
        typedef enum {
            STANDARD = 0,
            EXTENDED = 1
        } messageTypeType;

        /** 
         * Default constructor.
         * Creates a Message with an id of 0 (all bits 0), busId 0,
         * and no data. 
         */
        JanzMessage( JanzMessage::messageTypeType msgType = EXTENDED );

        /**  
         * Create an extended message with specified id and busId.
         * @exception CAN::Error if the id is illegal (more than 29 bits). 
         */
        JanzMessage(carma::canbus::idType id, 
                carma::canbus::busIdType busId);

        /** 
         * Create an extended message with the specified ids and data.
         * @exception CAN::Error if the id is illegal or the data contains 
         * more than 8 bytes. 
         */
        JanzMessage(carma::canbus::idType id, 
                const std::vector<carma::canbus::byteType> &data,
                carma::canbus::busIdType busId);

        /**
         * Create a message from a Janz FastMessage.
         */
        JanzMessage(carma::canbus::FastMessage, 
                    carma::canbus::busIdType busId);

        /**
         * Create a JanzMessage from a Message.
         */
        JanzMessage( const carma::canbus::Message & message );

        /**
         * Copy constructor.
         */
        JanzMessage(const JanzMessage &other);

        /**
         * Assignment operator.
         */
        JanzMessage &operator=(const JanzMessage &other);

        /**
         * Check for equality.
         */
        bool operator==( const JanzMessage & other ) const;
        
        /**
         * Check for inequality.
         */
        bool operator!=( const JanzMessage & other ) const;

        /**
         * Get message type. 
         * CAN messages exist in two flavors: STANDARD (11 bit Id) and
         * EXTENDED (29 bit Id).  CARMA will use EXTENDED Ids by default 
         * but STANDARD functionality is included for completeness. Note 
         * the type must be established at object creation (no set 
         * type methods).
         */
        carma::canbus::JanzMessage::messageTypeType getMessageType() const;

        /**
         * Get CAN message id.
         * @return CAN message id.
         */
        carma::canbus::idType getId() const;

        /**
         * Get CAN bus Id.  
         */
        carma::canbus::busIdType getBusId() const;

        /** 
         * Get the data as a vector of bytes (raw).
         */
        std::vector<carma::canbus::byteType> getData() const;

        /** 
         * Get port number.
         * Only valid on multi-bus modules (E.G. CAN-PCI2),
         * returns 0 on all others.
         */
        carma::canbus::portType getPort() const;

        /**
         * Return the underlying drivers raw message.
         * Needed so that BusIo can post a message.
         */
        carma::canbus::FastMessage getRawMessage() const;

        /**
         * Return the Rx MJD time.
         */
        double getRxMjd() const;

        /**
         * Return simulated flag.
         * This method should be used to determine if the Message
         * was artificially constructed for simulation purposes (i.e.
         * not retrieved from or posted to a CAN bus).
         * @return True if message is simulated, false otherwise.
         */
        bool isSimulated() const;


        /** 
         * Set CAN message id. 
         * @throws carma::canbus::BadParameterException if id is illegal 
         * (more than 29 bits). 
         */
        void setId(carma::canbus::idType id);

        /**
         * Set bus Id.
         */
        void setBusId(carma::canbus::busIdType busId);

        /** 
         * Set the data.
         * @throws carma::canbus::BadDataSizeException if the data contains 
         * more than 8 bytes. 
         */
        void setData(const std::vector<carma::canbus::byteType> &data);

        /** 
         * Set port number.
         * Also only for use on multi-bus modules like CAN-PCI2.
         * @throws carma::canbus::BadParameterException if invalid port.
         */
        void setPort(carma::canbus::portType port);	

        /**
         * Set the rxMjd time.
         */
        void setRxMjd(double mjd);

        /**
         * Set the simulated flag.
         * This flag should be set if the message is being artificially 
         * created for simulation purposes (i.e. it isn't actually retrieved
         * from or sent to the CAN bus). For the most part, it is set 
         * internally by the canbus::Master class.
         * @param sim True if message is simulated, false if not.
         */
        void setSimFlag(bool sim);

        /**
         * Set retry mode for message.
         * The Janz driver allows one to specify if the card should attempt
         * to resend a message upon a transmit failure due to bus-errors
         * or arbitration.  By default, the CAN card will continuously 
         * try to send a message until either it succeeds or fails due to 
         * bus errors triggering the controller to shutdown.  However, this
         * isn't always desirable.  In particular, we do not want the CAN
         * controller to continuously try to resend a time sync if nobody is
         * there to receive it (i.e. nobody is on the bus or the bus is 
         * disconnected from the linux host machine).  The reasons for this
         * are twofold. First, the timesyncs will continue to queue up until
         * the controller is able to send them.  If this continues long enough,
         * the onboard queue will overflow.  Second, when a module is connected
         * to the bus, it will receive all queued timesync messages (which are
         * nearly all bad) which in turn may overload and confuse the XACs 
         * timekeeping algorithms.  In this case, it is desirable to disable 
         * retries using this method.  Note also that the timesync message 
         * will almost never lose arbitration since it is (almost) the highest 
         * priority message in the system.  The single caveat to this is the 
         * reset message (highest prio) in which case it doesn't matter anyway.
         */
        void disableTxRetry();

        /**
         * Echo this message.
         * Set the Echo flag on a message.  This will cause the Janz board
         * to echo the message back through the read queue once it has
         * been sent on the bus.  For the most part, this is useful for
         * debugging only. 
         */
        void enableEcho();

    protected:

        // There are no protected methods or data.

    private:

        /** Mask for 11 bit CAN id. */
        static const idType MAX_STANDARD_ID    = 0x7FF;
        /** Mask for 29 bit CAN id. */
        static const idType MAX_EXTENDED_ID    = 0x1FFFFFFF;

        /**
         * Get current data length in bytes.
         */
        int getDataLength() const;

        /**
         * Set the message type.
         */
        void setMessageType( JanzMessage::messageTypeType messageType);

        /**
         * Validate CAN message id.
         */
        bool validateId(carma::canbus::idType id) const;

        // Private variables.
        carma::canbus::busIdType busId_;  // Bus id.
        carma::canbus::FastMessage rawMsg_;              // Janz raw message
        bool simulated_;      // Flag to indicate if msg is for sim purposes.
        double rxMjd_;        // Rx time in Mean Julian Day format.
    };
} // namespace canbus
} // namespace carma
#endif
