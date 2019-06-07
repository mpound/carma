/**@file
 * SldcMaster class declaration.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.13 $
 * $Date: 2012/01/31 00:22:04 $
 * $Id: SldcMaster.h,v 1.13 2012/01/31 00:22:04 abeard Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_DOWNCONVERTER_SLDCMASTER_H
#define CARMA_DOWNCONVERTER_SLDCMASTER_H

#include "carma/canbus/Master.h"
#include "carma/downconverter/common/LoMonitor.h"
#include "carma/downconverter/common/NoiseSource.h"
#include "carma/downconverter/common/QuadratureModulator.h"
#include "carma/downconverter/spectral/BlockDownconverter.h"
#include "carma/downconverter/spectral/SpectralDownconverter.h"
#include "carma/downconverter/spectral/LoControl.h"
#include "carma/switchyard/Switchyard.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/PthreadMutex.h"

#include <map>
#include <string>

namespace carma {

namespace monitor {
    class SignalPathSubsystem;
    class SldcSubsystem;
}

namespace canbus {
    class Device;
} 

namespace downconverter {

    /**
     * Spectral Downconverter System CAN Master.
     * This class is responsible for creating and controlling the Spectral
     * Line Downconverter CAN system.
     */
    class SldcMaster : public carma::canbus::Master {
    public:

        /** 
         * Default constructor for emulation.
         * This constructor should be used when no Janz hardware is available
         * on the system (during testing for instance).  All writes to the 
         * CANbus will go to /dev/null instead.
         * @see carma::canbus::Master::Master()
         */
        SldcMaster( ::carma::monitor::SldcSubsystem & sldcMon,
                    ::carma::monitor::SignalPathSubsystem & signalPathMon );

        /**
         * Constructor for use with an arbitrary number of Carma CAN/DIO cards.
         * @param devTermPairs Vector of pairs of board id, bus id pairs and
         * bus termination state.  The carrier board identified by a boardId
         * is the cPCI board which contains four separate mezzanine modules 
         * (2 - CAN, 1 - DIO and 1 - RJ45 Breakout Board), each on a separate 
         * 'modulbus'.  The boardId is set via a hex switch near the back of
         * the cPCI board.  On older models, it is determined via a clearly 
         * labeled PLD chip near the back of the board.  The chip will be 
         * labeled "mbus X" - do not confuse this with the mobulbus numbers on 
         * the front panel (labeled 'MODULbus[0..3]). The bus id will be either
         * 0 or 1 corresponding to CAN cards in MODULbus slots 0 and 1.
         * @param simOfflineNodes Simulate offline nodes.
         * @throws carma::util::ErrorException derivatives on a variety of 
         * failures.     
         */
        SldcMaster( const ::std::vector< CanDio::DevTermPair > & devTermPairs, 
                    bool simOfflineNodes,
                    ::carma::monitor::SldcSubsystem & sldcMon,
                    ::carma::monitor::SignalPathSubsystem & signalPathMon );

        /**
         * Destructor
         */
        virtual ~SldcMaster();

        /**
         * Add a device to the master.
         * This is kind of a hack but this method has been redeclared public
         * to allow SldcControlServer to add Devices after SldcMaster creation.
         * @param device pointer to base Device class of a device.
         * @see Device::setState
         * @throws carma::canbus::BadParameterException if device has
         * already been added.
         */
        void addDevice(carma::canbus::Device *device);

        /**
         * Retrieve a reference to a SpectralDownconverter residing on this
         * system.
         * @param inputIndex Input index [1..15].
         * @param bandIndex Band index [1..8].
         * @return Reference to specified Spectral Line Downconverter object.
         */
        SpectralDownconverter & getSldc(
            unsigned short inputIndex, 
            unsigned short bandIndex);

        /**
         * Retrieve a reference to the global (node 0) SpectralDownconverter.
         * @return Reference to node 0 (global control) Downconverter.
         */
        SpectralDownconverter & getGlobalSldc();

        /**
         * Retrieve a reference to the LoControl module.
         */
        LoControl & getLoControl( );

        /**
         * Retrieve a reference to the LoMonitor module.
         */
        LoMonitor & getLoMonitor( );

        /**
         * Retrieve a reference to the NoiseSource module.
         */
        NoiseSource & getNoiseSource( );

        /**
         * Retrieve a reference to specified QuadMod module.
         * @param inputNo Input number (range 1..15).
         */
        QuadratureModulator & getQuadMod( unsigned short inputNo );

        /**
         * Retrieve a reference to the global (node 0) quad mod.
         */
        QuadratureModulator & getGlobalQuadMod( );

        /** 
         * Get a reference to a block downconverter module.
         * @param inputNo Input number (range 1..15).
         * @return Reference to block downconverter for desired input.
         */
        BlockDownconverter & getBlockDownconverter( unsigned short inputNo );

        /**
         * Retrieve a reference to the global (node 0) block downconverter.
         * @return Reference to global block downconverter.
         */
        BlockDownconverter & getGlobalBlockDownconverter( );

        /**
         * Retrieve a reference to the switchyard.
         * @return Reference to the switchyard.
         */
        ::carma::switchyard::Switchyard & getSwitchyard( );

        /**
         * Retrieve a reference to the downconverter LO switchyard.
         * @return Reference to the downconverter LO switchyard.
         */
        ::carma::switchyard::Switchyard & getDcLoSwitchyard( );


        /**
         * Reset all modules on all controlled busses via the DIO lines.
         */
        void reset();

        /**
         * Reset all Xac modules on all controlled busses via the RESET
         * message.
         */
        void softReset();

        /**
         * Start sldc master in a separate thread.
         * Calls Master::run() but spawns it in a separate thread.
         */
        void start();

        /**
         * Stop the sldc master.
         */
        void stop();
        

    protected:

        /** 
         * Retrieve a map of CAN controls provided by this class.
         */
        std::map<carma::canbus::msgType, std::string> getControls() const;

        /**
         * Update the status of the Sldc CAN Master.
         * This routine retrieves values specific to the CANbus(ses)
         * for the Sldc system and places them into the monitor stream.
         * It is called automatically by the Master class every
         * frame (1/2 second) as described in carma::canbus::Master.
         * @see carma::canbus::Master::updateStatus
         */
        void updateStatus();

    private:

        // Disallow assignment and copy construction.
        SldcMaster(const SldcMaster &);
        SldcMaster &operator=(const SldcMaster &);

        // Helper routine for common initialization
        void initialize();
        static void * runThreadEntry(void *arg);

        pthread_t runThreadId_;
        bool isRunning_;
        carma::util::PthreadMutex isRunningGuard_;
        carma::util::PthreadCond isRunningCond_;

        carma::monitor::SldcSubsystem & sldcMon_;
        carma::monitor::SignalPathSubsystem & signalPathMon_;
        
        typedef ::std::map<
            ::carma::canbus::nodeType, 
            ::carma::downconverter::SpectralDownconverter *> SldcDeviceMap;

        typedef ::std::map<
            ::carma::canbus::nodeType,
            ::carma::downconverter::BlockDownconverter *> BlockDcDeviceMap;

        typedef ::std::map<
            ::carma::canbus::nodeType,
            ::carma::downconverter::QuadratureModulator *> QuadModDeviceMap;
            
        LoControl loControl_;
        LoMonitor loMonitor_;
        NoiseSource noiseSource_;
        QuadModDeviceMap quadMods_;
        BlockDcDeviceMap blockDownconverters_;
        SldcDeviceMap sldcDevices_;
        ::carma::switchyard::Switchyard switchyard_;
        ::carma::switchyard::Switchyard dcLoSwitchyard_;

        QuadratureModulator globalQuadMod_; // Node 0 QuadMod
        SpectralDownconverter globalSldc_; // Node 0 Downconverter
        BlockDownconverter globalBlockDownconverter_;

        const std::string hostname_;

    }; // End class SldcMaster
}} // End namespace carma::downconverter
#endif
