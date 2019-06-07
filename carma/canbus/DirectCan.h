/** @file
 * Declaration of carma::canbus::DirectCan class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.10 $
 * $Date: 2012/07/25 17:51:26 $
 * $Id: DirectCan.h,v 1.10 2012/07/25 17:51:26 abeard Exp $
 */
#ifndef CARMA_CANBUS_DIRECTCAN_H
#define CARMA_CANBUS_DIRECTCAN_H

// Carma Includes
#include "carma/canbus/CanIo.h"
#include "carma/canbus/Message.h"
#include "carma/util/IPQwriter.h"
#include "carma/util/IPQreader.h"

namespace carma {
namespace canbus {

    /** 
     * The DirectCan class is intended for use as a way to access
     * the CANbus from multiple separate processes in conjunction with 
     * a process implementing the DO model. It makes use of shared 
     * memory queues of CAN messages to get and post messages to the CAN bus.
     * It is required that a CAN host process be running on the 
     * CANbus(ses) requiring direct CAN access.  This is due to 
     * the fact that DirectCAN was designed to concurrently run
     * with the DO model.  If no DOs exist on the system in question
     * then the user may be better off directly accessing the CAN 
     * bus with the CanIo or CanDio classes.  May it be noted that
     * this class may be extremely useful for debugging DOs.
     * 
     * @see carma::canbus::CanIo
     * @see carma::canbus::CanDio
     * 
     * Since CAN is a finite bandwidth bus and DirectCan depends on 
     * shared memory and a CAN Master (carma::canbus::Master) process
     * to access the CAN bus, read and write buffer overflow errors 
     * are possible.  These errors will not always be detectable from
     * DirectCan directly.  The getMessage routines will throw a
     * carma::canbus::BufferOverflowException when overflows occur.  
     * However, the situation for posting messages to the CAN bus is
     * more difficult.  Please see DirectCan::postMessage for a detailed
     * explanation of how write overflow errors can be detected.
     * 
     * BusId:  It is important that the user of this class understand 
     * how BusIds are used in the system.  Since multiple CAN Masters
     * each controlling multiple CAN busses but all requiring Direct CAN
     * access may be running on any given target host, it was necessary
     * to devise a system assuring unique busIds.  This was accomplished
     * by parsing the input device name (i.e. /dev/dpm_00) into a busId.
     * As a result, it is not directly obvious which busId a DirectCan 
     * user will need to access (what is the BusId of the device you are
     * controlling).  Thus in general a user will need to determine the 
     * busId of the device he or she is controlling by calling msg.getBusId()
     * from a msg known to come from his/her device.  The blanket busId
     * carma::canbus::ALL_BUSSES is provided to allow a DirectCan user to 
     * send to all busses and this may be used initially to assure 
     * communication with your device until the BusId is retrieved.
     * @see carma::canbus::busIdType
     * @see carma::canbus::Message
     * @see carma::canbus
     */   
    class DirectCan : public CanIo {
        public:

            /**
             * Constructor
             * @throw carma::util::ErrorException on failure to open 
             * shared memory required for DirectCan.  This exception
             * generally signals that a CanMaster host process is not 
             * running on the system, or that you are running a DirectCan
             * application with less priveledge than the CanMaster process.
             */
            DirectCan();

            /**
             * Destructor
             */
            ~DirectCan();

            /**
             * Get a message from the CAN Message IPQ.
             * Blocks until next message is retrieved from
             * CAN bus.  This routine will throw an exception if it
             * trys to retrieve a message that has been overwritten
             * by the writer.  In this case, the user is not reading
             * from the interprocess queues quickly enough and messages
             * are being lost.  
             * @return Message CAN Message object.
             * @throw carma::canbus::BufferOverflowException if messages 
             * were lost since the last getMessage call.
             */
            Message getMessage();
            
            /**
             * Get a specific message from the CAN Message IPQ.
             * Blocks until next message with a matching id is retrieved 
             * from the CAN bus.  Note that if the message is never 
             * received, this will block forever.  Also note that
             * any messages received between the time we call getMessage
             * and the time when the desired message is received will
             * be lost to this object!
             * @param id Full 29 bit CAN id to await.
             * @return Message CAN Message object.
             * @throw carma::canbus::BufferOverflowException if messages 
             * were lost since the last getMessage call.
             * @see carma::canbus::getMessage
             */
            Message getMessage(idType id);

            /**
             * Post a message to the CAN bus. 
             * Unfortunately, there is no direct way to indicate a 
             * write buffer overflow error (write overflows) from DirectCan.  
             * This is due to the design of the IPQ classes and would require 
             * a major overhaul of these classes.  Buffer overflows can 
             * be and are detected within the CAN Master process however.  
             * When they are detected, the message is dropped, the error 
             * is logged and execution will continue as normal.  If a user 
             * of this routine suspects write overflows are occurring, he 
             * should first check that the CAN Master process is running 
             * and then monitor the syslog for 
             * carma::canbus::TxBufferFullException and 
             * carma::canbus::BufferOverflowException messages reported
             * from the CAN Master.
             * @param msg to CAN bus.
             */
            void postMessage(
                const carma::canbus::Message &msg,
                carma::canbus::txPriorityType prio = carma::canbus::NORMAL);

        private:

            carma::util::IPQreader<carma::canbus::Message> *readIpq_;
            carma::util::IPQwriter<carma::canbus::Message> *writeIpq_;

    }; // End of class DirectCan
} // End of namespace canbus
} // End of namespace carma
#endif
