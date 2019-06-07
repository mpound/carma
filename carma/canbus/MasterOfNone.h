/** @file
 * Declaration of carma::canbus::MasterOfNone class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2004/12/16 22:47:07 $
 * $Id: MasterOfNone.h,v 1.1 2004/12/16 22:47:07 abeard Exp $
 */
#ifndef CARMA_CANBUS_MASTEROFNONE_H
#define CARMA_CANBUS_MASTEROFNONE_H

// Carma includes
#include "carma/canbus/Master.h"

namespace carma {
namespace canbus {

    /**
     * Carma CAN MasterOfNone class.
     *
     * This class is a dummy implementation of the carma::canbus::Master class.
     * It contains no Device derivatives and contains no control commands or
     * other methods.  As such, it is functionally useful only to send
     * timestamps and to pass incoming CAN messages on to the DirectCan 
     * queues (and subsequently canOverIp). This is useful if one needs an 
     * application which only acts as an intermediary for canOverIp but doesn't
     * need to process messages or expose control commands on the machine
     * hosting the CANbus(ses).  One downside of this is that there is no
     * way to control the hardware reset lines (i.e. hard reset all modules), 
     * although these lines will always be driven when this class is used.
     */
    class MasterOfNone : public carma::canbus::Master {
    public:

        /**
         * Constructor for single CANbus.
         * For use with only a single CANbus on a multi bus board.
         * @param boardId Modulbus board id [0-15].
         * @param canbus Canbus or modulbusNo [0-1].
         */
        MasterOfNone(int boardId, int canbus);

        /**
         * Constructor for two CAN busses.
         * For use with both busses on a single Janz/Carma CAN/DIO board.
         * @param boardId Modulbus board id [0-15].
         */
        MasterOfNone(int boardId);

        /**
         * Destructor
         */
        ~MasterOfNone();

    protected:

        // There are no protected methods.

    private:

        // No-op
        void updateStatus();

        // Again, disallow copying and assignment.
        MasterOfNone(const MasterOfNone &);
        MasterOfNone &operator=(const MasterOfNone &);
        
    }; // End class MasterOfNone.
}} // End namespace carma::canbus
#endif
