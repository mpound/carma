/** @file
 * Declaration of Ovro antenna canbus master class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.47 $
 * $Date: 2012/10/26 23:17:32 $
 * $Id: OvroMaster.h,v 1.47 2012/10/26 23:17:32 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_OVROMASTER_H
#define CARMA_ANTENNA_OVRO_OVROMASTER_H

// Carma includes
#include "carma/antenna/common/CMReceiver.h"
#include "carma/antenna/common/LOReferenceMonitor.h"
#include "carma/antenna/common/SisReceiver.h"
#include "carma/antenna/common/Tiltmeter.h"
#include "carma/antenna/common/Varactor.h"
#include "carma/antenna/ovro/canbus/AntennaIF.h"
#include "carma/antenna/ovro/canbus/CryoCompressor.h"
#include "carma/antenna/ovro/canbus/CryoTemperatures.h"
#include "carma/antenna/ovro/canbus/Drive.h"
#include "carma/antenna/ovro/canbus/Encoder.h"
#include "carma/antenna/ovro/canbus/EnvironmentalMonitor.h"
#include "carma/antenna/ovro/canbus/GunnPll.h"
#include "carma/antenna/ovro/canbus/Optics.h"
#include "carma/antenna/ovro/canbus/RxTemperatures.h"
#include "carma/antenna/ovro/canbus/SecondaryMirror.h"
#include "carma/antenna/ovro/canbus/SharedOpticsSeqNo.h"
#include "carma/antenna/ovro/canbus/YigPll.h"
#include "carma/antenna/ovro/control/DriveEngine.h"
#include "carma/canbus/Master.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/PthreadMutex.h"

// STL includes
#include <map>
#include <memory>

namespace log4cpp {
    // Forward declaration.
    class Category;
} // End namespace log4cpp

namespace carma {

namespace monitor {
    // Forward declaration.
    class OvroSubsystem;
} // End namespace monitor

namespace antenna {
namespace ovro {

/**
 * Ovro Antenna canbus Master class.
 * This class is responsible for declaring and maintaining all ovro antenna 
 * CAN devices. 
 */
class OvroMaster : public carma::canbus::Master {
public:

    /**
     * Constructor for emulation only.
     * @param antNo Antenna number this process is running on.
     * @param simOfflineNodes Simulate offline nodes if set true.
     * @param ovroSubsystem Reference to instance of Ovro Monitor Subsystem.
     */ 
    explicit OvroMaster( unsigned short antNo, 
                         bool simOfflineNodes,
                         carma::monitor::OvroSubsystem & ovroSubsystem );

    /**
     * OvroMaster constructor.
     * This constructor is for use when controlling both CANbusses on the
     * Janz cPCI carrier board.  
     *
     * @param board Board number of Janz cPCI carrier board
     * (0-15). The carrier board is the cPCI board which contains
     * four separate mezzanine modules (2 - CAN, 1 - DIO and 1 - RJ45
     * Breakout Board), each on a separate 'modulbus'.  The boardId is
     * set via a hex switch near the back of the cPCI board.  On older
     * models, it is determined via a clearly labeled PLD chip near
     * the back of the board.  The chip will be labeled "mbus X" - do
     * not confuse this with the mobulbus numbers on the front panel.
     * @param antNo Antenna number this process is running on.
     * @param simOfflineNodes Simulate offline nodes if set true.
     * @param reset If true, perform a hard reset upon construction.
     * @param terminate Terminate both CAN busses if true.
     * @throws carma::util::ErrorException derivatives on a variety of failures.
     * @see carma::canbus::CanDio::reset
     * @see carma::canbus::CanDio::isTerminated
     * @param ovroSubsystem Reference to instance of Ovro Monitor Subsystem.
     */
    explicit OvroMaster(
        int board, 
        unsigned short antNo, 
        bool simOfflineNodes,
        bool reset,
        bool terminate, 
        carma::monitor::OvroSubsystem & ovroSubsystem );

    /**
     * OvroMaster single bus constructor.
     * This constructor should be used when it is desireable to control
     * only a single CANbus on the Janz cPCI carrier board.
     * @param board Board number of Janz cPCI carrier board
     * (0-15). The carrier board is the cPCI board which contains
     * four separate mezzanine modules (2 - CAN, 1 - DIO and 1 - RJ45
     * Breakout Board), each on a separate 'modulbus'.  The boardId is
     * set via a hex switch near the back of the cPCI board.  On older
     * models, it is determined via a clearly labeled PLD chip near
     * the back of the board.  The chip will be labeled "mbus X" - do
     * not confuse this with the mobulbus numbers on the front panel.
     * @param bus Canbus to use [0..1].
     * @param antNo Antenna number this process is running on.
     * @param simOfflineNodes Simulate offline nodes if set true.
     * @param reset If true, perform a hard reset upon construction.
     * @param terminate Terminate both CAN busses if true.
     * @param ovroSubsystem Reference to instance of Ovro Monitor Subsystem.
     * @throws carma::util::ErrorException derivatives on a variety of
     * failures.
     * @see carma::canbus::CanDio::reset
     * @see carma::canbus::CanDio::isTerminated
     */
    explicit OvroMaster(
        int board, 
        int bus, 
        unsigned short antNo, 
        bool simOfflineNodes,
        bool reset,
        bool terminate,
        carma::monitor::OvroSubsystem & ovroSubsystem );

    /**
     * Destructor.
     */
    virtual ~OvroMaster();

    /**
     * Start the CAN Master
     * This routine does not block! It is responsible for kicking off a
     * new thread which in turn runs the main Master::run() routine.
     * @see carma::antenna::ovro::OvroMaster::stop
     */
    void start();

    /**
     * Stop the CAN Master
     * This routine terminates the main run thread in the CAN Master.
     * It is safe to call this routine even if start() has not yet been
     * called (i.e. when handling an exception). 
     */
    void stop();

    enum AntennaIFType {
        IF1,
        IF2
    };

    /**
     * Retrieve a reference to underlying AntennaIF CAN device.
     * @return AntennaIF reference.
     */
    AntennaIF& getAntennaIF( enum AntennaIFType ifType );

    /**
     * Retrieve pointer to underlying CryoCompressor CAN device.
     * @return CryoCompressor pointer.
     */
    CryoCompressor& getCryoCompressor();

    /**
     * Retrieve pointer to underlying CryoTemperatures CAN device (dewar).
     * @return CryoTemperatures pointer.
     */
    CryoTemperatures& getCryoTemperatures();

    /**
     * Get a reference to the drive engine
     */
    DriveEngine & getDriveEngine( );

    /**
     * Retrieve pointer to underlying Tiltmeter CAN device.
     * @return Tiltmeter pointer.
     */
    Tiltmeter& getTiltmeter();

    /**
     * Retrieve a pointer to the underlying SecondaryMirror CAN device.
     * @return SecondaryMirror pointer.
     */
    SecondaryMirror& getSecondary();

    /**
     * Retrieve a pointer to the underlying Optics CAN device.
     * @return Optics pointer.
     */
    Optics& getOptics();

    /**
     * Retrieve a pointer to the underlying EnvironmentalMonitor CAN device.
     * @return EnvironmentalMonitor pointer.
     */
    EnvironmentalMonitor& getEnvironmentalMonitor();

    /**
     * Retrieve reference to the underlying YigPll CAN device.
     * @return YigPll reference.
     */
    YigPll& getYigPll();

    /**
     * Retrieve a reference to the underlying Rx Electronics Temperature
     * Controller device.
     * @return RxTemperatures reference.
     */
    RxTemperatures& getRxTemperatureController();

    /**
     * Sis Receiver types.
     */
    enum SisRxType {
        SIS_RX1MM,
        SIS_RX3MM
    };

    enum SisRxPolType {
        SINGLE, // Single Pol, doesn't matter which kind of polarization.
        LEFT_CIRCULAR,
        RIGHT_CIRCULAR
    };

    /**
     * Retrieve a reference to underlying Sis Receiver module.
     */
    carma::antenna::common::SisReceiver& 
    getSisReceiver( enum SisRxType rx,
                    enum SisRxPolType pol );

    /**
     * Retrieve a reference to underlying CM Receiver module.
     */
    carma::antenna::common::CMReceiver &
    getCMReceiver( );

    /**
     * Gunn types.
     */
    enum GunnType {
        LO1CM,
        LO1MM,
        LO3MM,
        LOANY
    };

    /**
     * Retrieve a reference to the specified Bias Tuned Gunn CAN device.
     * @return GunnPll reference.
     */
    GunnPll& getGunn(enum GunnType gunn);

    /**
     * Retrieve a reference to the varactor module.
     * @return Varactor reference.
     */
    carma::antenna::common::Varactor & getVaractor( );

    /**
     * Retrieve a reference to the underlying LO Reference CAN device.
     * @return carma::antenna::common::LOReferenceMonitor reference.
     */
    carma::antenna::common::LOReferenceMonitor& getLOReferenceMonitor();

    /**
     * Reset ALL modules on CAN via DIO.
     * This routine performs what is commonly known as a hard reset.
     */
    void reset(); 
    
    /**
     * Set state of antenna initialization.
     */
    void setInitialization( bool state );

protected:

    /**
     * Get a map of controls provided by the OvroMaster
     * Master controls correspond to global CAN Message Ids.
     * For the most part this routine won't be used since
     * control commands are hard coded into the below routines and
     * our Antenna API has no mechanism to return values via corba.
     */ 
    carma::canbus::MsgBriefMap getControls() const;

    /**
     * Update the status of the Antenna CAN Master.
     * This routine retrieves values specific to the CANbus(ses) 
     * for the antenna and places them into the monitor stream.
     * It is called automatically by the Master class every 
     * frame (1/2 second) as described in carma::canbus::Master.
     * @see carma::canbus::Master::updateStatus
     */
    void updateStatus();

    /**
     * Reset ALL modules on CAN via RESET CAN msg.
     * This routine triggers a software reset in XAC modules. 
     */
    void softReset();

private:

    // Disallow assignment and copy construction.
    OvroMaster(const OvroMaster &);
    OvroMaster &operator=(const OvroMaster &);

    // Run thread entry point.
    static void *runThreadEntry(void *arg);

    // Redefine run such that it is private.
    void run();

    // Initialize helper routine to perform common creation tasks 
    // (i.e. those common among all three constructors).
    void initialize(unsigned short antNo);

    log4cpp::Category& log_;

    // Keep id around so that we can destroy it when need be.
    pthread_t runThreadId_;  
    bool isRunning_;
    carma::util::PthreadMutex isRunningGuard_;
    carma::util::PthreadCond isRunningCond_;

    // Define the devices that will exist on this network.
    // Also, since these aren't CORBA servants, they don't need to be
    // allocated on the heap.  This is a bit different than in the past 
    // and it avoids the mismanagement of the memory...
    SharedOpticsSeqNo sharedOpticsSeqNo_;
    CryoCompressor compressor_;
    CryoTemperatures dewar_;
    EnvironmentalMonitor enviro_;
    AntennaIF ifLeftPol_;
    AntennaIF ifRightPol_;
    carma::antenna::common::LOReferenceMonitor loref_;
    Optics optics_;
    RxTemperatures rxtemp_;
    SecondaryMirror secondary_;
    Tiltmeter tiltmeter_;
    YigPll yig_;
    GunnPll gunn1mm_;
    GunnPll gunn3mm_;
    carma::antenna::common::Varactor gunn1cm_;
    carma::antenna::common::SisReceiver rx1mmLeftPol_;
    carma::antenna::common::SisReceiver rx1mmRightPol_;
    carma::antenna::common::SisReceiver rx3mm_;
    carma::antenna::common::CMReceiver rx1cm_;
    
    // WARNING: the following are initialization order dependent on each other.
    Drive drive_; 
    Encoder azEncoder_; 
    Encoder elEncoder_; 
    DriveEngine driveEngine_; 
    // Initialization order dependent
    
    // Monitor system reference
    carma::monitor::OvroSubsystem & mon_;

    const ::std::string hostname_;
    bool initialized_;

}; // End class OvroMaster    
}}} // End namespace carma::antenna::ovro
#endif
