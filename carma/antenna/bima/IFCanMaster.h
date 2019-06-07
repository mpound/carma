/** @file
 * Implementation for BIMA AntennaIF canbus master class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.18 $
 * $Date: 2012/10/17 14:54:00 $
 * $Id: IFCanMaster.h,v 1.18 2012/10/17 14:54:00 friedel Exp $
 */
#ifndef CARMA_ANTENNA_BIMA_IFCANMASTER_H
#define CARMA_ANTENNA_BIMA_IFCANMASTER_H

// Carma includes
#include "carma/corba/corba.h"
#include "carma/antenna/bima/CMReceiver.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/antenna/bima/TelemetryCommand.h"
#include "carma/antenna/bima/AntennaIFClient.h"
#include "carma/antenna/bima/control/AntennaControlImpl.h"
#include "carma/antenna/bima/Rx.h"
#include "carma/canbus/Master.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Program.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/PthreadCond.h"
#include "carma/util/PthreadMutex.h"
#include "carma/antenna/bima/SharedMemory.h"
#include "carma/antenna/bima/ProcessMonitor.h"

#include "carma/antenna/bima/AntennaIF.h"

// NOTE, this puts itself in the carma::antenna::ovro
// name space to utilize ovro's written code
#include "carma/antenna/bima/Tiltmeter.h"

// STL includes
#include <map>

namespace log4cpp {
    // Forward declaration.
    class Category;
} // End namespace log4cpp

namespace carma
{
  namespace monitor
  {
    class BimaSubsystem; // Forward declaration.
  }

  namespace antenna
  {

    namespace common 
    {
      class Varactor;
    }

    namespace bima
    {
      class AntennaNameResolver;
      class SisReceiver;
      class CMReceiver;

/**
 * BIMA Antenna IF canbus Master class.
 * This class is responsible for declaring and maintaining the Antenna IF
 * CAN device on BIMA antennas.
 */
class IFCanMaster : public carma::canbus::Master
{
 public:

    /**
     * Constructor for emulation only.
     */ 
    IFCanMaster(carma::util::IPQreader<TelemetryCommand> &ifr,
                Rx &rx, 
                carma::monitor::BimaSubsystem &mon,
                AntennaNameResolver & anr,
		Configuration &config);

    /**
     * IFCanMaster single bus constructor.
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
     * @throws carma::util::ErrorException derivatives on a variety of
     * failures.
     */
    IFCanMaster( int board, int bus,
                 carma::util::IPQreader<TelemetryCommand> &ifr,
                 Rx &rx,
                 carma::monitor::BimaSubsystem &mon,
                 AntennaNameResolver & anr,
		 Configuration &config);

    ~IFCanMaster();

    /**
     * Start the CAN Master
     * This routine does not block! It is responsible for kicking off a
     * new thread which in turn runs the main Master::run() routine.
     * @see carma::antenna::bima::IFCanMaster::stop
     */
    void start();

    /**
     * Stop the CAN Master
     * This routine terminates the main run thread in the CAN Master.
     * It is safe to call this routine even if start() has not yet been
     * called (i.e. when handling an exception). 
     */
    void stop();

    static void *startWriterThread( void *arg );
    void writerThread( void );

    AntennaIF& getAntennaIFPol1() { return *_aifPol1; };
    AntennaIF& getAntennaIFPol2() { return *_aifPol2; };
    AntennaControlImpl& getAntennaControl() { return *_ant; };
    CMReceiver& getCmRxControl() { return *_cmrx; };
    carma::antenna::ovro::Tiltmeter& getTiltmeter() { return *_tiltMeter; };
    
    /**
     * Reset the canbusses via dio.
     * This routine performs what is commonly known as a hard reset.
     */
    void reset(); 
    
    /** 
     * Set initialization flag.
     */
    void setInitialized( bool state );

   protected:

    /**
     * Get a map of controls provided by the IFCanMaster
     * Master controls correspond to global CAN Message Ids.
     * For the most part this routine won't be used since
     * control commands are hard coded into the below routines and
     * our Antenna API has no mechanism to return values via corba.
     */ 
    std::map<carma::canbus::msgType, std::string> getControls() const;

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
     * Reset modules via software RESET msg.
     * A soft reset.
     */
    void softReset();


private:

    // Disallow assignment and copy construction.
    IFCanMaster(const IFCanMaster &);
    IFCanMaster &operator=(const IFCanMaster &);

    // Run thread entry point.
    static void *runThreadEntry(void *arg);

    // Redefine run such that it is private.
    void run();

    // Initialize helper routine to perform common creation tasks 
    // (i.e. those common among all three constructors).
    void initialize(Configuration &config);

    // Keep id around so that we can destroy it when need be.
    pthread_t runThreadId_;  
    bool isRunning_;
    carma::util::PthreadMutex isRunningGuard_;
    carma::util::PthreadCond isRunningCond_;

    // Define the devices that will exist on this network.
    // Also, since these aren't CORBA servants, they don't need to be
    // allocated on the heap.  This is a bit different than in the past 
    // but it avoids the mismanagement of the memory...
    carma::antenna::bima::AntennaIF *_aifPol1;
    carma::antenna::bima::AntennaIF *_aifPol2;

    // OVRO type of tiltmeter, currently only on carma7/bima1
    // This is picked up from bima/Tiltmeter.h above!
    // that has modifications to its interface (namely a different
    // monitor subsystem..
    carma::antenna::ovro::Tiltmeter *_tiltMeter;

    // This is used to talk to the SIS Receiver CAN module.
    carma::antenna::bima::SisReceiver *_sisrx;

    carma::antenna::common::Varactor *_varactor;

    carma::antenna::bima::CMReceiver *_cmrx;
    
    // The sole reason for AntennaControl's existence anymore
    // is apparently to provide a way to reset the primary
    // CAN bus...  The IF system on the BIMA antennas is
    // the only shared system between OVRO and BIMA and
    // it is the only class that has a copy of the "master"
    // which provides the method to reset... Hence this is
    // where AntennaControl ends up... Oh.  My.  God.
    carma::antenna::bima::AntennaControlImpl *_ant;

    carma::monitor::BimaSubsystem& mon_;
    log4cpp::Category& log_;
    carma::util::IPQreader<TelemetryCommand> &_ifReader;

    AntennaNameResolver & anr_;
    SharedMemory *_bimaShm;

   }; // End class IFCanMaster    

 }
}
} // End namespace carma::antenna::bima

#endif // CARMA_ANTENNA_BIMA_IFCANMASTER_H

