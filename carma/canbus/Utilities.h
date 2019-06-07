/** @file
 * Declarations of carma::canbus utility functions. 
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.22 $
 * $Date: 2005/07/07 00:01:26 $
 * $Id: Utilities.h,v 1.22 2005/07/07 00:01:26 abeard Exp $
 */
#ifndef CARMA_CANBUS_UTILITIES_H
#define CARMA_CANBUS_UTILITIES_H

// C++ Standard Library includes 
#include <vector>

// Carma includes
#include "carma/canbus/Types.h"

namespace carma {

    /**
     * This namespace contains all canbus library related code.
     */
    namespace canbus {

        // CAN Id Utilities.

        /** @fn modeType getMode(idType id)
         *  @brief Extract the address mode from a 29 bit can id.  
         *  @param id 29 bit CAN address.
         *  @return address mode ENGINEERING or APPLICATION.
         */ 
        modeType getMode(idType id);

        /**
         * Extract the host bit from a 29 bit CAN id. 
         * @param id 29 bit CAN address.
         * @return host bit (true = message addressed to host).
         */
        bool isToHost(idType id);

        /**
         * Constant for bool host parameter of createId indicating that the
         * message is to be addressed to the linux host.
         */
        const bool TO_HOST = true;

        /** 
         * Constant for bool host parameter of createId indicating that the
         * message is to be addressed to the can nodes on the bus.
         */
        const bool TO_NODES = false;

        /** 
         * Create a CAN id from api, node and mid.
         * Creates an application message from the api, node and message id 
         * addressed to either the host or nodes.
         * @param host addressed to host or nodes.
         * @param api id.
         * @param node message node location id. 
         * @param mid of message.  
         */
        idType createId(bool host, apiType api, nodeType node, msgType mid);

        /**
         * Produce a CAN message from a board type and serial number 
         * (engineering message). 
         * @param host send to host. 
         * @param board the board id being addressed. 
         * @param serialNumber of board being addressed. 
         * @param mid message id.
         * @return 29 bit CAN id. 
         */
        idType createEngId(bool host, boardType board, 
                serialNumberType serialNumber, msgType mid);

        /** 
         * Convert a raw CAN id to api code, node location and mid.
         * @param api api id.
         * @param node node id.
         * @param mid message id.
         * @param id full CAN message id.
         */
        void fromId(apiType &api, nodeType &node, 
                msgType &mid, idType id);

        /** 
         * Convert a raw CAN id to board type, serial number and mid.
         * @param bt board type id.
         * @param sn serial number id.
         * @param mid message id.
         * @param engId full Engineering CAN message id.
         */
        void fromEngId(boardType &bt, serialNumberType &sn, 
                msgType &mid, idType engId);

        // CAN data utilities...
        // The toData and dataTo routines correspond to the basic
        // CAN data types that will be used and are defined in the hardware
        // APIs...  The names reflect the corresponding C basic data types.

        /**
         * Convert a unsigned char into raw CAN bytes.
         * This routine takes an input unsigned char value and adds it
         * to the input vector of raw CAN bytes.  This enbables one to 
         * build a full CAN data packet by consecutively calling the
         * appropriate typeToData routine.
         * @param data is a reference to a vector which the data will be
         * appended to.
         * @param data is the vector to append the data to.
         * @param value is the unsigned char to be appended to data.
         * @throws carma::canbus::BadDataSizeException if resulting vector
         * is > than 8 bytes.
         */
        void uByteToData(std::vector<byteType> &data, unsigned char value);

        /**
         * Convert an unsigned short integer (2 bytes) into raw CAN bytes.
         * This routine takes an input unsigned short integer value and
         * adds it to the input vector of raw CAN bytes in network byte
         * order.  This enables one to build a full CAN data packet by 
         * consecutively calling the appropriate typeToData routine.
         * @param data is the vector to append the data to.
         * @param value is the unsigned short int to append to data.
         * @throws carma::canbus::BadDataSizeException if resulting vector
         * is > than 8 bytes.
         */
        void uShortToData(std::vector<byteType> &data, unsigned short value);

        /**
         * Convert an unsigned long integer (4 bytes) into raw CAN bytes.
         * This routine takes an input unsigned long integer value and
         * adds it to the input vector of raw CAN bytes in network byte
         * order.  This enables one to build a full CAN data packet by 
         * consecutively calling the appropriate typeToData routine.
         * @param data is the vector to append the data to.
         * @param value is the unsigned long int to append to data.
         * @throws carma::canbus::BadDataSizeException if resulting vector
         * is > than 8 bytes.
         */
        void uLongToData(std::vector<byteType> &data, unsigned long value);

        /**
         * Convert a signed short integer (2 bytes) into raw CAN bytes.
         * This routine takes an input signed short integer value and
         * adds it to the input vector of raw CAN bytes in network byte
         * order.  This enables one to build a full CAN data packet by 
         * consecutively calling the appropriate typeToData routine.
         * @param data is the vector to append the data to.
         * @param value is the signed short int to append to data.
         * @throws carma::canbus::BadDataSizeException if resulting vector
         * is > than 8 bytes.
         */
        void sShortToData(std::vector<byteType> &data, short value);

        /**
         * Convert a signed long integer (4 bytes) into raw CAN bytes.
         * This routine takes an input signed long integer value and
         * adds it to the input vector of raw CAN bytes in network byte
         * order.  This enables one to build a full CAN data packet by 
         * consecutively calling the appropriate typeToData routine.
         * @param data is the vector to append the data to.
         * @param value is the signed long int to append to data.
         * @throws carma::canbus::BadDataSizeException if resulting vector
         * is > than 8 bytes.
         */
        void sLongToData(std::vector<byteType> &data, long value);

        /**
         * Convert a IEEE 754-1990 float (4 bytes) into raw CAN bytes.
         * This routine takes an input floating point value and
         * adds it to the input vector of raw CAN bytes in network byte
         * order.  This enables one to build a full CAN data packet by 
         * consecutively calling the appropriate typeToData routine.
         * @param data is the vector to append the data to.
         * @param value is the float to append to the data.
         * @throws carma::canbus::BadDataSizeException if resulting vector
         * is > than 8 bytes.
         */
        void floatToData(std::vector<byteType> &data, float value);

        /**
         * Convert a IEEE 754-1990 double float (8 bytes) into raw CAN bytes.
         * This routine takes an input double float value and
         * adds it to the input vector of raw CAN bytes in network byte
         * order.  This enables one to build a full CAN data packet by 
         * consecutively calling the appropriate typeToData routine.
         * @param data is the vector to append the data to.
         * @param value is the double to append to the data.
         * @throws carma::canbus::BadDataSizeException if resulting vector
         * is > than 8 bytes.
         */
        void doubleToData(std::vector<byteType> &data, double value);

        /**
         * Convert data vector to unsigned byte.
         * This routine interprets the first byte of a vector of
         * raw CAN data as an unsigned byte and returns the byte.
         * @param data is the vector to extract the unsigned byte from.
         * @return unsigned byte.
         * @throws carma::canbus::BadDataSizeException if data vector is
         * empty.
         */
        unsigned char dataToUbyte(std::vector<byteType> &data);

        /**
         * Convert data vector to unsigned short integer.
         * This routine interprets the first 2 bytes of a vector of
         * raw CAN data as an unsigned short integer in network byte order.
         * It then extracts, converts and returns the unsigned int.
         * @param data is the vector to extract the unsigned short from.
         * @return unsigned short.
         * @throws carma::canbus::BadDataSizeException if data vector
         * is < than 2 bytes.
         */
        unsigned short dataToUshort(std::vector<byteType> &data);

        /**
         * Convert data vector to unsigned long integer.
         * This routine interprets the first 4 bytes of a vector of
         * raw CAN data as an unsigned long integer in network byte order.
         * It then extracts, converts and returns the unsigned int.
         * @param data is the vector to extract the unsigned long from.
         * @return unsigned long.
         * @throws carma::canbus::BadDataSizeException if data vector
         * is < than 4 bytes.
         */
        unsigned long dataToUlong(std::vector<byteType> &data);

        /**
         * Convert data vector to signed short integer.
         * This routine interprets the first 2 bytes of a vector of
         * raw CAN data as an signed short integer in network byte order.
         * It then extracts, converts and returns the signed int.
         * @param data is the vector to extract the signed short from.
         * @return unsigned long.
         * @throws carma::canbus::BadDataSizeException if data vector
         * is < than 2 bytes.
         */
        short dataToShort(std::vector<byteType> &data);

        /**
         * Convert data vector to signed long integer.
         * This routine interprets the first 4 bytes of a vector of
         * raw CAN data as an signed long integer in network byte order.
         * It then extracts, converts and returns the signed int.
         * @param data is the vector to extract the signed long from.
         * @return signed long.
         * @throws carma::canbus::BadDataSizeException if data vector
         * is < than 4 bytes.
         */
        long dataToLong(std::vector<byteType> &data);

        /**
         * Convert data vector to IEEE float.
         * This routine interprets the first 4 bytes of a vector of
         * raw CAN data as an IEEE floating point value.
         * It then extracts, converts and returns the float.
         * @param data is the vector to extract the float from.
         * @return float.
         * @throws carma::canbus::BadDataSizeException if data vector
         * is < than 4 bytes.
         */
        float dataToFloat(std::vector<byteType> &data);

        /**
         * Convert data vector to IEEE double.
         * This routine interprets the first 8 bytes of a vector of
         * raw CAN data as an IEEE double floating point value.
         * It then extracts, converts and returns the double.
         * @param data is the vector to extract the double from.
         * @return double.
         * @throws carma::canbus::BadDataSizeException if data vector
         * is < than 8 bytes.
         */
        double dataToDouble(std::vector<byteType> &data);

        /**
         * Pad an input vector with up to 8 bytes of zeros.
         * This routine takes the input data, calculates it's size
         * and fills the remaining space with up to 8 bytes of '0's.
         * For example a single byte data vector, after being input
         * to this routine, will contain the original data and 7 '0's.
         * @param data reference to vector to be stuffed with '0's.
         * @throws carma::canbus::BadDataSizeException if data vector
         * is > than 8 bytes.
         */
        void padWithZeros(std::vector<byteType> &data);

        // Timing routines 
        // Useful for sleeping and waking on 1/2 second boundaries
        // or Slow monitor boundaries (5 second).

        /**
         * Calculate the time to the next five second boundary.
         * This routine calculates the amount of time necessary to
         * sleep until the next five second boundary as determined
         * by absolute time.
         * @return timespec containing amount of time to sleep
         * until next five second boundary.
         */
        timespec calculateTimeToNextSlowBoundary();

        /**
         * Calculate the time to the next half second boundary.
         * This routine calculates the amount of time necessary
         * to sleep until the next half second boundary as
         * determined by absolute time.
         * @return timespec containing amount of time to 
         * sleep until next half second boundary.
         */
        timespec calculateTimeToNextHalfSec();

        /**
         * Extract the busId from a string containing
         * the device filename.  In order to maintain unique 
         * busIds on a multiprocess multi canbus system, I use
         * the can device filename to determine the bus Id.  
         * @throws carma::canbus::BadParameterException if input
         * filename is invalid.
         */
        busIdType extractBusId(std::string filename);

        /**
         * Validate a modulbusNo.  
         * The janz modulbus hardware has a hex switch to designate the
         * modulbus id of the board.  This routine, checks to make sure
         * that a modulbusNo is within the range of 0x00 to 0x0f.
         * @param modulbusNo to validate.
         * @return modulbusNo if validated.
         * @throws carma::canbus::BadParameterException if invalid modulbusNo.
         */
        int validateModulbusNo(int modulbusNo);

        /**
         * Validate slot number for CANbus slot.
         * The specialized janz CANDio card contains CANbus cards in modulbus
         * slots 0 and 1 only.  This routine checks to make sure that a 
         * input slot number is within that range and throws an exception 
         * otherwise.
         * @param slotNo to validate.
         * @return slotNo if valid.
         * @throws carma::canbus::BadParameterException if invalid slotNo.
         */
        int validateSlotNo(int slotNo);

    }; // namespace canbus 
}; // namespace carma
#endif // CARMA_CANBUS_UTILITIES_H
