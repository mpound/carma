/**@file
 * Master class declaration for the Carma Wideband Downconverter CANbus system.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.38 $
 * $Date: 2012/08/28 21:43:11 $
 * $Id: WbdcMaster.h,v 1.38 2012/08/28 21:43:11 abeard Exp $
 */
#ifndef CARMA_DOWNCONVERTER_WBDCMASTER_H
#define CARMA_DOWNCONVERTER_WBDCMASTER_H

#include "carma/canbus/Master.h"
#include "carma/downconverter/common/DownconverterControl.h"
#include "carma/downconverter/common/QuadModControl.h"
#include "carma/downconverter/common/LoMonitorControl.h"
#include "carma/downconverter/common/NoiseSourceControl.h"
#include "carma/downconverter/common/SldcLoControl.h"
#include "carma/downconverter/spectral/BlockDownconverterControl.h"
#include "carma/downconverter/spectral/SpectralDownconverterControl.h"

#include <map>
#include <pthread.h>
#include <tao/Basic_Types.h>

/**
 * @page WbdcHost Wideband Downconverter CAN Host Application
 *
 * This documentation describes the Carma Wideband Downconverter Linux Host
 * application - carmaWbdcHost.
 *
 * The Carma Wideband Downconverter application is responsible for monitoring
 * and controlling the Carma Wideband Downconverter system.  This system
 * consists of 128 Downconverter CAN modules, 8 Quadrature Modulator CAN
 * modules, a Noise Source CAN module and finally an LO Monitor CAN module.
 * The carmaWbdcHost application is responsible for managing the CAN networks
 * these devices reside on, processing standard monitor packets received
 * from the modules and placing them into the Carma Monitor System (CAM) while
 * simultaneously acting as a CORBA server to field control requests via CORBA,
 * encode the corresponding CAN messages and send them to the appropriate CAN
 * devices.  This application is a textbook implementation of the CARMA
 * CANbus Library DO model.
 *
 * Related Documentation:
 * - @ref WbdcControl
 * - @ref Canbus
 * - <A HREF="http://www.mmarray.org/project/WP/SpectralDownconverter/sw/">
 * Design docs. </A>
 *
 */

namespace carma {

namespace corba {
    class Server;
}

namespace monitor {
    // Forward dec
    class WbdcSubsystem;
}

namespace downconverter {

    class Downconverter;
    class LoMonitor;
    class NoiseSource;
    class QuadratureModulator;

    /**
     * Carma Wideband Downconverter CAN Master class.
     *
     * This class realizes the carma::canbus::Master class for the Carma
     * Wideband %Downconverter CANbus system.  It instantiates 128 Downconverter
     * devices, 8 QuadratureModulator devices and a NoiseSource device which
     * reside on the Wideband Downconverter CAN network.  It is then
     * responsible for managing them, placing device and CANbus monitor data
     * into the monitor stream and simulating OFFLINE devices.
     *
     * It simultaneously incarnates the CORBA server for the
     * carma::downconverter::System interface. This interface is documented
     * extensively in the @ref WbdcControl.
     */
    class WbdcMaster : public carma::canbus::Master {
    public:

        /**
         * Default constructor for emulation.
         * This constructor is for emulation only when it is not desired or not
         * possible to communicate with the underlying Janz hardware (i.e. the
         * Janz hardware doesn't exist on the system). In this mode
         * all DOs are still published, and remote method invocations are still
         * possible. However, CANbus writes will go to /dev/null rather than
         * the CANbus.
         * @param server Reference to corba::Server instance.
         * @param mon Reference to WbdcSubsystem.
         * @see carma::canbus::Master::Master()
         */
        WbdcMaster( carma::corba::Server & server, 
                    carma::monitor::WbdcSubsystem & mon );

        /**
         * Wideband %Downconverter Master constructor.
         * Constructor for use with a single CANbus.  This constructor should
         * only be used during initial integration and testing when there are
         * not yet enough CAN modules to utilize two busses.
         * @param server Reference to corba::Server instance.
         * @param modulBusNo Janz modulBus board designator (0 - 15).
         * @param slotNo Janz modulBus slot # of CAN card to use (0 or 1).
         * @param mon Reference to WbdcSubsystem.
         */
        WbdcMaster( carma::corba::Server & server, 
                    int modulBusNo, int slotNo,
                    carma::monitor::WbdcSubsystem & mon );

        /**
         * Wideband %Downconverter Master constructor.
         * Constructor for use with two CANbusses.  This constructor should
         * be used with the final wideband %downconverter implementation.  It
         * controls both CANbusses on the Carma specialized Janz Can/Dio board.
         * @param server Reference to corba::Server instance.
         * @param modulBusNo Janz modulBus board designator (0 - 15).
         * @param mon Reference to WbdcSubsystem.
         */
        WbdcMaster( carma::corba::Server & server, 
                    int modulBusNo,
                    carma::monitor::WbdcSubsystem & mon );

        /**
         * Destructor.
         * By declaring the WbdcMaster destructor as protected we force
         * a user to only allocate it on the heap.  This is done in order
         * for proper use with CORBA reference counting.
         */
        virtual ~WbdcMaster();

        /**
         * Query to see if the System::quit() method has been invoked.
         * This routine is for DEBUG USE ONLY to explicitly determine if a user
         * has invoked the carma::downconverter::System::quit() method
         * (defined in the Downconverter API).
         */
        bool isDone();

    protected:

        /**
         * Update the status of the Cwdc CAN Master.
         * This routine is responsible for retrieving values specific
         * to the state of the CanMaster object and the CANbus(ses)
         * themselves such as number of online nodes, CANbus errors, etc.
         * It retrieves these values and places them into the monitor stream.
         * The method is called internally my the canbus::Master base class
         * every half second as described in carma::canbus::Master.
         */
        void updateStatus();

    public: 

        // Our CORBA IDL Defined control methods.  These implement the IDL
        // defined System interface. They are declared private to assure that
        // they are only accessible via CORBA (doesn't care about privacy).
        carma::downconverter::DownconverterControl_ptr GlobalDownconverter();
        carma::downconverter::DownconverterControl_ptr Downconverter(
            short input, short band);
        carma::downconverter::SpectralDownconverterControl_ptr
        SpectralDownconverter( CORBA::Short input, CORBA::Short band );
        carma::downconverter::QuadModControl_ptr GlobalQuadMod();
        carma::downconverter::QuadModControl_ptr QuadMod(short input);
        carma::downconverter::NoiseSourceControl_ptr GlobalNoiseSource();
        carma::downconverter::NoiseSourceControl_ptr NoiseSource();
        carma::downconverter::LoMonitorControl_ptr GlobalLoMonitor();
        carma::downconverter::LoMonitorControl_ptr LoMonitor();
        carma::downconverter::SldcLoControl_ptr LoControl();
        carma::downconverter::BlockDownconverterControl_ptr
        BlockDownconverter( CORBA::Short inputNo );
        carma::downconverter::BlockDownconverterControl_ptr
        GlobalBlockDownconverter( );

        void selectSidebandFrequency(
            carma::downconverter::DownconverterControl::SidebandType sideband,
            CORBA::Double lofreq,
            CORBA::UShort bandNo);
        void selectFilter(
            carma::downconverter::DownconverterControl::FilterType filter,
            CORBA::UShort bandNo );
        void setPsysPreset( CORBA::UShort inputNo, CORBA::UShort bandNo );
        void setPsys( CORBA::Float psys,
                      CORBA::UShort inputNo,
                      CORBA::UShort bandNo );
        void reset();
        void softReset();
        // DEBUG
        void quit();
    
    private:

        // Copying and assignment are not allowed
        WbdcMaster(const WbdcMaster &);
        WbdcMaster &operator=(const WbdcMaster &);

        // Helper routines to consolidate initialization among 3 constructors.
        void initialize();
        void addDevices();

        // Consolidate removal of devices - used on destruction.
        void removeDevices();

        // Run thread entry point.  Entry points are needed in order to assure
        // that the actual run method is not subject to the c-style linkage
        // rules (static) that are required of the pthread entry point.
        static void *runThreadEntry(void *arg);

        // Redefine run such that it is private. Since the carmaWbdcHost app
        // is a CORBA server, it will block on runOrb - thus we call run in
        // a seperate thread in the WbdcMaster constructor to get the CANbus
        // side of things moving independently.
        void run();

        // Keep the run thread id around so that we can properly destroy the
        // thread upon destruction.
        pthread_t runThreadId_;

        // Node 0 'Global control' device objects. (Aren't added to Master
        // device map via addDevice).
        typedef std::map< carma::canbus::nodeType,
                          carma::downconverter::Downconverter *> DcByNodeMap;

        DcByNodeMap downconverters_;
        carma::downconverter::Downconverter * globalDc_;
        carma::downconverter::QuadratureModulator * globalQm_;
        carma::downconverter::LoMonitor * globalLoMon_;
        
        typedef std::pair< short, short > InputBandNoPair;
        typedef std::map< InputBandNoPair, 
                          carma::downconverter::DownconverterControl_ptr >
            DownconverterControlsMap;
        typedef std::map< short, 
                          carma::downconverter::QuadModControl_ptr >
            QuadModControlsMap;

        carma::downconverter::DownconverterControl_ptr globalDcControl_;
        carma::downconverter::QuadModControl_ptr globalQmControl_;
        carma::downconverter::LoMonitorControl_ptr globalLoMonControl_;

        DownconverterControlsMap dcControls_;
        QuadModControlsMap qmControls_;
        carma::downconverter::NoiseSourceControl_ptr nsControl_;
        carma::downconverter::LoMonitorControl_ptr loMonControl_;
        carma::corba::Server & server_;

        carma::monitor::WbdcSubsystem & mon_;

        const bool emulate_;            // Emulate Janz Hardware
        const std::string hostname_;    // Hostname we're running on

        // DEBUG
        pthread_mutex_t doneMutex_;
        bool done_;

    }; // End WbdcMaster class
}; // End downconverter namespace
}; // End carma namespace
#endif
