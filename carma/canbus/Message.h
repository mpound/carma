/** @file
 * Declaration of carma::canbus::Message class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.22 $
 * $Date: 2012/07/13 18:55:26 $
 * $Id: Message.h,v 1.22 2012/07/13 18:55:26 abeard Exp $
 */

#ifndef CARMA_CANBUS_MESSAGE_H
#define CARMA_CANBUS_MESSAGE_H

#include "carma/canbus/Types.h"

namespace carma {
namespace canbus {

    /** 
     * Class to encapsulate a CAN message.
     */
    class Message {
    public:

        /** 
         * Default constructor.
         * Creates a Message with an id of 0, busId 0, and no data. 
         */
        Message( );

        /**  
         * Create an extended message with specified id and busId.
         * @exception CAN::Error if the id is illegal (more than 29 bits). 
         */
        Message( carma::canbus::idType id, 
                 carma::canbus::busIdType busId );

        /** 
         * Create an extended message with the specified ids and data.
         * @exception CAN::Error if the id is illegal or the data contains 
         * more than 8 bytes. 
         */
        Message( carma::canbus::idType id, 
                 const DataVector & data,
                 carma::canbus::busIdType busId ); 

        /**
         * Copy constructor.
         */
        Message( const Message &other );

        /**
         * Get CAN message id.
         * @return CAN message id.
         */
        carma::canbus::idType getId( ) const;

        /**
         * Get CAN bus Id.  
         */
        carma::canbus::busIdType getBusId( ) const;

        /** 
         * Get the data as a vector of up to 8 bytes.
         */
        DataVector getData( ) const;
        
        /**
         * Return the Rx MJD time.
         */
        double getRxMjd( ) const;

        /**
         * Return simulated flag.
         * This method should be used to determine if the Message
         * was artificially constructed for simulation purposes (i.e.
         * not retrieved from or posted to a CAN bus).
         * @return True if message is simulated, false otherwise.
         */
        bool isSimulated( ) const;

        /** 
         * Set CAN message id. 
         * @throws carma::canbus::BadParameterException if id is illegal 
         * (more than 29 bits). 
         */
        void setId( carma::canbus::idType id );

        /**
         * Set bus Id.
         */
        void setBusId( carma::canbus::busIdType busId );

        /** 
         * Set the data.
         * @throws carma::canbus::BadDataSizeException if the data contains 
         * more than 8 bytes. 
         */
        void setData( const carma::canbus::DataVector & data );

        /**
         * Set the rxMjd time.
         */
        void setRxMjd( double mjd );

        /**
         * Set the simulated flag.
         * This flag should be set if the message is being artificially 
         * created for simulation purposes (i.e. it isn't actually retrieved
         * from or sent to the CAN bus). For the most part, it is set 
         * internally by the canbus::Master class.
         * @param sim True if message is simulated, false if not.
         */
        void setSimFlag( bool sim );

        /**
         * Assignment operator.
         */
        Message & operator=( const Message & other );

        /** 
         * Returns true if the id's, data and busId's are identical 
         * (including data length).
         */ 
        bool operator==( const Message & other );

        /** 
         * Returns false if the id's, busId's or data differs 
         * (including length). 
         */
        bool operator!=( const Message & other );

        /**
         * Add an unsigned char to CAN message data.
         * @throws carma::canbus::BadDataSizeException if resulting data
         * size is > 8 bytes.
         */
        carma::canbus::Message & operator<<( unsigned char rvalue );

        /**
         * Add an unsigned short to CAN message data.
         * @throws carma::canbus::BadDataSizeException if resulting data
         * size is > 8 bytes.
         */
        carma::canbus::Message & operator<<( unsigned short rvalue );
        
        /**
         * Add a signed short to CAN message data.
         * @throws carma::canbus::BadDataSizeException if resulting data
         * size is > 8 bytes.
         */
        carma::canbus::Message & operator<<( short rvalue );

        /**
         * Add an unsigned long integer to CAN message data.
         * @throws carma::canbus::BadDataSizeException if resulting data
         * size is > 8 bytes.
         */
        carma::canbus::Message & operator<<( unsigned long int rvalue );
        
        /**
         * Add a long integer to CAN message data.
         * @throws carma::canbus::BadDataSizeException if resulting data
         * size is > 8 bytes.
         */
        carma::canbus::Message & operator<<( long int rvalue );

        /**
         * Add an IEEE 754-1990 float to CAN message data.
         * @throws carma::canbus::BadDataSizeException if resulting data
         * size is > 8 bytes.
         */
        carma::canbus::Message & operator<<( float rvalue );

        /**
         * Add an IEEE 754-1990 double float to CAN message data.
         * @throws carma::canbus::BadDataSizeException if resulting data
         * size is > 8 bytes.
         */
        carma::canbus::Message & operator<<( double rvalue );

        /**
         * Dump everything we know about this message in CARMAesque.
         * Prints the bus, api and node (or board type and serial number), 
         * message id, whether it was intended for the host or nodes and 
         * the raw data in hex.  
         * @param abbreviate Only output data if true.
         * @param ascii Output data as ascii text if true.
         * @return string containing dumped data.
         */
        ::std::string dump( bool abbreviate, bool ascii ) const;

    protected:

        // There are no protected methods or data.

    private:

        carma::canbus::idType id_;
        carma::canbus::busIdType busId_;  
        // Use a hard sized data array as opposed to a vector as these
        // will be put in shared memory IPQs which can't contain pointers.
        carma::canbus::byteType data_[8];
        size_t dlc_;
        bool simulated_;      
        double rxMjd_;       

    };
} // namespace canbus
} // namespace carma
#endif
