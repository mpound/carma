/**@file
 * Master class declaration for the Master Clock CANbus system.
 *
 * <dl><dt><b>Author </b></dt><dd>Chul Gwon </dl>
 * $Revision: 1.8 $
 * $Date: 2011/08/25 20:54:37 $
 * $Id: ClockMaster.h,v 1.8 2011/08/25 20:54:37 abeard Exp $
 */
#ifndef CARMA_CLOCK_CLOCKMASTER_H
#define CARMA_CLOCK_CLOCKMASTER_H

// Carma includes
#include "carma/corba/corba.h"
#include "carma/canbus/Master.h"
#include "carma/clock/Clock.h"
#include "carma/util/UserException.h"

#include <memory>
#include <pthread.h>

namespace carma {
namespace clock {

    /**
     * Carma Master Clock CAN Master class.
     *
     * This class realizes the carma::canbus::Master class for the Carma
     * Master Clock CANbus system.
     *
     */
    class ClockMaster : public carma::canbus::Master {
    public:

        /**
         * Default constructor for emulation.
         * This constructor is for emulation only when it is not desired or not
         * possible to communicate with the underlying Janz hardware (i.e. the
         * Janz hardware doesn't exist on the system). In this mode
         * all DOs are still published, and remote method invocations are still
         * possible. However, CANbus writes will go to /dev/null rather than
         * the CANbus.
         * @param autoWriteDelayInS Monitor system autowrite delay in seconds.
         * @see carma::canbus::Master::Master()
         */
        ClockMaster( bool enableAutowriter,
                     double autoWriteDelayInS );

        /**
         * Clock Master constructor.
         * Constructor for use with a single CANbus.  This constructor should
         * only be used during initial integration and testing when there are
         * not yet enough CAN modules to utilize two busses.
         * @param modulBusNo Janz modulBus board designator (0 - 15).
         * @param slotNo Janz modulBus slot # of CAN card to use (0 or 1).
         * @param autoWriteDelayInS Monitor system autowrite delay in seconds.
         */
        ClockMaster( int modulBusNo,
                     int slotNo,
                     double autoWriteDelayInS );

        /**
         * Clock Master constructor.
         * Constructor for use with two CANbusses. It
         * controls both CANbusses on the Carma specialized Janz Can/Dio board.
         * @param modulBusNo Janz modulBus board designator (0 - 15).
         * @param autoWriteDelayInS Monitor system autowrite delay in seconds.
         */
        ClockMaster( int modulBusNo,
                     double autoWriteDelayInS );

        /**
         * Destructor.
         */
        virtual ~ClockMaster();

        /**
         * Query to see if the System::quit() method has been invoked.
         * This routine is for DEBUG USE ONLY to explicitly determine if a user
         * has invoked the carma::clock::System::quit() method
         */
        bool isDone();

        // Control routines (see IDL for documentation).
        carma::clock::ClockControl_ptr GlobalClock();
        carma::clock::ClockControl_ptr Clock();
        void reset();
        void softReset();
        void quit();

    protected:

        /**
         * Update the status of the Clock CAN Master.
         * This routine is responsible for retrieving values specific
         * to the state of the CanMaster object and the CANbus(ses)
         * themselves such as number of online nodes, CANbus errors, etc.
         * It retrieves these values and places them into the monitor stream.
         * The method is called internally my the canbus::Master base class
         * every half second as described in carma::canbus::Master.
         */
        void updateStatus();

    private:

        // Copying and assignment are not allowed
        ClockMaster(const ClockMaster &);
        ClockMaster &operator=(const ClockMaster &);

        // Helper routines to consolidate initialization among 3 constructors.
        void initialize();
        void addDevices();

        // Consolidate removal of devices - used on destruction.
        void removeDevices();

        // Run thread entry point.  Entry points are needed in order to assure
        // that the actual run method is not subject to the c-style linkage
        // rules (static) that are required of the pthread entry point.
        static void *runThreadEntry(void *arg);

        // Redefine run such that it is private. Since the carmaClockHost app
        // is a CORBA server, it will block on runOrb - thus we call run in
        // a seperate thread in the ClockMaster constructor to get the CANbus
        // side of things moving independently.
        void run();

        // Keep the run thread id around so that we can properly destroy the
        // thread upon destruction.
        pthread_t runThreadId_;

        // Node 0 'Global control' device objects. (Aren't added to Master
        // device map via addDevice).
        std::auto_ptr< carma::clock::Clock > clock_;
        std::auto_ptr< carma::clock::Clock > globalClock_;
        
        carma::clock::ClockControl_ptr clockControlPtr_;
        carma::clock::ClockControl_ptr globalClockControlPtr_;

        std::string hostname_;
        std::auto_ptr< carma::monitor::MasterClockSubsystem > mon_;

        // Are we emulating the underlying Janz hardware?
        const bool emulate_;

        // DEBUG
        pthread_mutex_t doneMutex_;
        bool done_;

    }; // End ClockMaster class
}; // End clock namespace
}; // End carma namespace
#endif
