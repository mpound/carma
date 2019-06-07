/** @file 
 * Declaration of carma::canbus::CanDio class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.17 $
 * $Date: 2012/08/03 23:08:24 $
 * $Id: CanDio.h,v 1.17 2012/08/03 23:08:24 abeard Exp $
 *
 */

#ifndef CARMA_CANBUS_CANDIO_H
#define CARMA_CANBUS_CANDIO_H

#include "carma/canbus/CanIo.h"
#include "carma/canbus/Dio.h"

#include <memory>
#include <utility>
#include <vector>

namespace carma {
namespace canbus {

    /**
     * Class to control the CARMA specialized Janz CAN/DIO card.
     *
     * This class controls a very specific hardware setup used in CARMA and 
     * described below.  For more general usage of arbitrary CAN or TTL
     * devices, use the CanIo and/or Dio variants.
     *
     * This class controls the composite Janz CARMA compact PCI carrier board.
     * The cPCI carrier board contains 4 mezanine daughter boards - from 
     * top to bottom, these are 2 x vmod-ican3 CAN cards, an in-house RJ45 
     * breakout board and a vmod-ttl digital IO board. Signals from the CAN
     * and DIO boards are rerouted and combined through the backplane to the 
     * RJ45 card which serves as the primary physical interface. The Dio 
     * lines control hardware reset functionality of CARMA CAN modules.
     *
     * The cPCI carrier board is identified by it's unique 'modulbus' number.  
     * The modulbus number is set via a hex switch on the carrier board.   
     * Older carrier boards instead contain a rom chip clearly labeled 
     * 'Modbus X'.  Particular mezanine slots are labeled on the front panel as 
     * 'MODULbus N'. Janz ambiguously uses the term modulbus to refer to both 
     * the board and/or a specific slot.  We take care to use the term 
     * 'board' to refer to the cPCI carrier board id (e.g. the hex switch 
     * identifier) and 'slot' to refer to the specific mezanine slot.
     */
    class CanDio : public CanIo {
        public:

            /**
             * Default constructor.
             * This constructor is for use when emulating the underlying 
             * Janz hardware.  If no Janz hardware exists, use this constructor.
             * All Can or Dio write routines will write to /dev/null.
             */
            CanDio();

            /**
             * Constructor to control both CAN busses on a single board.
             * @param boardId Janz cPCI carrier board modulbus number (0x0-0xf).
             * @param reset Place modules in a hardware reset state by default. 
             * @param terminate Terminate both busses if true.
             * @throw carma::util::ErrorException on failure.
             */
            CanDio( int boardId, 
                    bool reset = true, 
                    bool terminate = true );

            /** 
             * Constructor to control a single CAN bus.
             * @param boardId Janz cPCI carrier board modulbus number (0x0-0xf).
             * @param slotId Mezanine slot id of CAN card (0 or 1).
             * @param reset Place modules in a hardware reset state by default. 
             * @param terminate Terminate bus if true.
             * @throw carma::util::ErrorException on failure.
             */
            CanDio( int boardId, 
                    int slotId, 
                    bool reset = true, 
                    bool terminate = true );

            /**
             * Pair containing boardId (first) and slotId (second).
             */
            typedef ::std::pair< int, int > BoardSlotPair;

            /**
             * Pair for board & slot (first) and termination setting (second).
             */
            typedef ::std::pair< BoardSlotPair, bool > DevTermPair;

            /** 
             * Constructor to control arbitrary number of CAN busses.
             * @param devTermPairs Bus identifier and termination vector.
             * @param reset Place modules in a hardware reset state by default. 
             * @throw carma::util::ErrorException on failure.
             */
            CanDio( const std::vector< DevTermPair > & devTermPairs, 
                    bool reset = true );

            /**
             * Reset.
             * Resets the CANbus using Dio and clears the CAN message read 
             * queue. This assures that users will retrieve only messages 
             * that were sent after this method was called (i.e. no old 
             * messages will remain in CAN read queue).
             * @throws JanzFailException if unable to write to dio board.
             */
            void reset();

            /**
             * Return termination state.
             * Returns true if terminated, false otherwise.
             */
            bool isTerminated();
            
            carma::canbus::Message getMessage();

            void 
            postMessage( 
                const carma::canbus::Message & msg,
                carma::canbus::txPriorityType prio = carma::canbus::NORMAL );

            std::map<busIdType, carma::canbus::busStatusType>  
            getBusStatus( );

            void echoAll( bool enable );

        protected:
            
            void setTimestampEchoLatency(int tsLatency, busIdType busId);
            void queueMessage(const Message &msg);
            void clearReadQueue( );

            /**
             * Reset pulsewidth.
             * In most cases the reset dio lines must be written hi
             * for a minimal amount of time in order for a module's reset 
             * circuitry to detect it.  This variable defines that minimal 
             * pulsewidth time in ms.
             */ 
            const long pulseWidth_;
            const bool terminate_;
            std::auto_ptr< carma::canbus::Dio > dio_;
            std::auto_ptr< carma::canbus::CanIo > cio_;

    };  // End class CanDio
  }  // End namespace canbus
}  // End namespace carma
#endif // CARMA_CANBUS_CANDIO_H
