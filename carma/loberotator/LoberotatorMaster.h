/**@file
 * Master class declaration for the LoberotatorMaster CANbus control system
 *
 * @author Colby Kraybill
 *
 * $Id: LoberotatorMaster.h,v 1.31 2014/06/24 21:46:00 scott Exp $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_LOBEROTATORMASTER_H
#define CARMA_LOBEROTATORMASTER_H

// System includes
#include <pthread.h>

// Carma includes
#include "carma/canbus/Master.h"
#include "carma/loberotator/Chassis.h"
#include "carma/loberotator/Loberotator.h"
#include "carma/switchyard/Switchyard.h"
#include "carma/monitor/LoberotatorSubsystem.h"
#include "carma/monitor/SignalPathSubsystem.h"
#include "carma/util/Time.h"

#include <utility>

namespace carma {

namespace corba {

    class Server;

}


namespace loberotator {

    /**
     * Carma Loberotator CAN Master class.
     */
    class LoberotatorMaster :
        public carma::loberotator::Chassis,
        public carma::canbus::Master {
    public:

        /**
         * Constructor for emulation mode
         * Runs without accessing any hardware, directing all
         * canbus writes to dev/null.
         * Automatically does simulation.
         * @param holdoff delay (msec) to send commands to the XAC after halfsec
         * tick
         * @param autoWriteDelayInS Monitor system autowrite delay in seconds.
         * @param syautoWriteDelayInS Switchyeard monitor system autowrite delay in seconds.
         */
        LoberotatorMaster(int holdoff, double autoWriteDelayInS,
                          double syautoWriteDelayInS);

        /**
         * LoberotatorMaster constructor.
         * @param board Janz modulBus board designator (0 - 15).
         * @param bus Janz modulBus slot # of CAN card to use (0 or 1).
         * @param simulate Produce simulated monitor packets
         * @param holdoff delay (msec) to send commands to the XAC after halfsec
         * tick
         * @param autoWriteDelayInS Monitor system autowrite delay in seconds.
         * @param syautoWriteDelayInS Switchyeard monitor system autowrite delay in seconds.
         */
        LoberotatorMaster( int board, int bus,
                           int holdoff, bool simulate,
                           double autoWriteDelayInS,
                           double syautoWriteDelayInS);

        /**
         * Get 'global' (node 0 control device) loberotator.
         */
        carma::loberotator::Loberotator & getGlobalLoberotator( );
        
        /**
         * Retrieve reference to LO Switchyard instance.
         */
        carma::switchyard::Switchyard & getLoSwitchyard();

        /**
         * Retrieve reference to LL Switchyard instance.
         */
        carma::switchyard::Switchyard & getLlSwitchyard();

        /**
         * Query to see if the System::quit() method has been invoked.
         * This routine is for debug use to explicitly determine if a user
         * has invoked the carma::loberotator::System::quit() method
         */
        bool isDone();

    protected:

        /**
         * Update the status of the Loberotator CAN Master.
         * This routine is responsible for retrieving values specific
         * to the state of the CanMaster object and the CANbus(ses)
         * themselves such as number of online nodes, CANbus errors, etc.
         * It retrieves these values and places them into the monitor stream.
         * The method is called internally my the canbus::Master base class
         * every half second as described in carma::canbus::Master.
         */
        void updateStatus();

        /**
         * Destructor.
         * By declaring the LoberotatorMaster destructor as protected we force
         * a user to only allocate it on the heap.  This is done in order
         * for proper use with CORBA reference counting.
         */
        virtual ~LoberotatorMaster();

    private:

       // The timer delay after the frame boundary for updating
        // the phase and rate
        // There is an inherent statistical delay of about 1.5msec
        // before the timer is serviced. It also takes on average
        // 0.7msec to actually do the update. Additionally, the messages take
        // about 3msec on the wire to go out. So we have a fixed delay of
        // 1.5 + 0.7 + 1.5 = 4,
        static const int FIXED_HOLDOFF = 4;
        int holdoff_;

        // Copying and assignment are not allowed
        LoberotatorMaster(const LoberotatorMaster &);
        LoberotatorMaster &operator=(const LoberotatorMaster &);

        // Helper routines to consolidate initialization among 3 constructors.
        void initialize(double autoWriteDelayInS, double syautoWriteDelayInS);
        void addDevices( );

        // Consolidate removal of devices - used on destruction.
        void removeDevices();

        // DEBUG
        void quit();

        // Run thread entry point.  Entry points are needed in order to assure
        // that the actual run method is not subject to the c-style linkage
        // rules (static) that are required of the pthread entry point.
        static void *runThreadEntry(void *arg);

        // Redefine run such that it is private. Since the carmaLoberotatorHost app
        // is a CORBA server, it will block on runOrb - thus we call run in
        // a separate thread in the LoberotatorMaster constructor to get the
        // CANbus side of things moving independently.
        void run();

        // Keep the run thread id around so that we can properly destroy the
        // thread upon destruction.
        pthread_t runThreadId_;

        // Node 0 'Global control' device objects. (Aren't added to Master
        // device map via addDevice).
        carma::loberotator::Loberotator* globalLrb_;
        carma::util::Time time_;
        carma::monitor::LoberotatorSubsystem* mon_;

        carma::monitor::SignalPathSubsystem signalPathMon_;
        carma::switchyard::Switchyard loSwitchyard_;
        carma::switchyard::Switchyard llSwitchyard_;

        // Are we emulating the underlying Janz hardware?
        const bool emulate_;

        std::string hostname_;

        // Static needed to call update in thread
        static void updateThreadStaticWrapper(LoberotatorMaster& lrm);
        // Thread scheduled update of phase & rate
        void updateThread();
        pthread_t updateThread_;
        void updatePhaseAndRate() const;

        // DEBUG
        pthread_mutex_t doneMutex_;
        bool done_;

    }; // End LoberotatorMaster class
 }; // End loberotator namespace
}; // End carma namespace

#endif // CARMA_LOBEROTATORMASTER_H
