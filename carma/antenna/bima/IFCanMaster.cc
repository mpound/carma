/** @file 
 * IFCanMaster class implementation.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.33 $
 * $Date: 2012/12/18 18:50:08 $
 * $Id: IFCanMaster.cc,v 1.33 2012/12/18 18:50:08 abeard Exp $
 */

// Carma Includes
#include "carma/antenna/bima/AntennaNameResolver.h"
#include "carma/antenna/bima/IFCanMaster.h"
#include "carma/antenna/common/RxTypeInfo.h"
#include "carma/antenna/common/Varactor.h"
#include "carma/antenna/bima/SisReceiver.h"
#include "carma/antenna/bima/CMReceiver.h"

// STL Includes
#include <iostream>

#define POL1_NODE_ID 1
#define POL2_NODE_ID 2
#define DEFAULT_NODE_ID 1
#define CMRX_NODE_ID 1
#define RCP_NODE_ID 4		

using namespace std;
using namespace log4cpp;
using namespace carma::antenna::bima;
using namespace carma::antenna::common;
using namespace carma::canbus;
using namespace carma::monitor;
using namespace carma::util;

namespace CAC = carma::antenna::common;

// -----------------------------------------------------------------------------
IFCanMaster::IFCanMaster( int board, int bus,
                          IPQreader<TelemetryCommand> &ifr,
                          Rx &rx,
                          carma::monitor::BimaSubsystem& mon,
                          AntennaNameResolver & anr,
			  Configuration &config) : 
    Master(board, bus, false),
    mon_( mon ),
    log_(Program::getLogger()),
    _ifReader( ifr ),
    anr_( anr )
{
  CPTRACE( Trace::TRACE4,
           "IFCanMaster( board=" << board << ", bus=" << bus << ", mon" );


  CPTRACE( Trace::TRACE2, " Instantiating _aifPol1" );
    _aifPol1 = new AntennaIF(POL1_NODE_ID, *this, rx, 
                 mon_.antennaIfContainer(POL1_NODE_ID - 1).state(),
                 mon_.antennaIfContainer(POL1_NODE_ID - 1).antennaIF(),
                 mon_.antennaIfContainer(POL1_NODE_ID - 1).xac());

  CPTRACE( Trace::TRACE2, " Instantiating _aifPol2" );
    _aifPol2 = new AntennaIF(POL2_NODE_ID, *this, rx, 
                 mon_.antennaIfContainer(POL2_NODE_ID - 1).state(),
                 mon_.antennaIfContainer(POL2_NODE_ID - 1).antennaIF(),
                 mon_.antennaIfContainer(POL2_NODE_ID - 1).xac());

    _tiltMeter = new carma::antenna::ovro::Tiltmeter( DEFAULT_NODE_ID, *this, mon );

    _sisrx = new carma::antenna::bima::SisReceiver (
        RCP_NODE_ID, *this, 
	mon.antennaCommon(),			// carma::monitor::AntennaCommon & antCommon
        mon_.rx1mm().state(),                   // carma::monitor::StateSensePointEnum &, ok
	mon_.rx1mm().sisReceiver(),
	mon_.rx1mm().xac(),
	* _aifPol2, // carma::antenna::common::AntennaIF & antIF
    anr_ );	

    _varactor = new carma::antenna::common::Varactor( 
        DEFAULT_NODE_ID, *this,
        mon_.gunn1cm().varactor(),
        mon_.gunn1cm().xac(),
        mon_.gunn1cm().state() );

    _cmrx = new carma::antenna::bima::CMReceiver(
	CMRX_NODE_ID, *this,
	mon_.rx(),
    mon_.rxBiasTemps(),
	rx);

    _ant = new AntennaControlImpl( *this );

    

    initialize(config);
}

// -----------------------------------------------------------------------------
IFCanMaster::IFCanMaster( IPQreader<TelemetryCommand> &ifr,
                          Rx &rx,
                          carma::monitor::BimaSubsystem& mon,
                          AntennaNameResolver & anr,
			  Configuration &config) : 
    Master(), 
    mon_( mon ),
    log_(Program::getLogger()),
    _ifReader( ifr ),
    anr_( anr )
{
  CPTRACE( Trace::TRACE4,
           "IFCanMaster( board=0, bus=0, mon" );

    _aifPol1 = new AntennaIF(POL1_NODE_ID, *this, rx,
                 mon_.antennaIfContainer(POL1_NODE_ID - 1).state(),
                 mon_.antennaIfContainer(POL1_NODE_ID - 1).antennaIF(),
                 mon_.antennaIfContainer(POL1_NODE_ID - 1).xac());

    _aifPol2 = new AntennaIF(POL2_NODE_ID, *this, rx,
                 mon_.antennaIfContainer(POL2_NODE_ID - 1).state(),
                 mon_.antennaIfContainer(POL2_NODE_ID - 1).antennaIF(),
                 mon_.antennaIfContainer(POL2_NODE_ID - 1).xac());

    _tiltMeter = new carma::antenna::ovro::Tiltmeter( DEFAULT_NODE_ID, *this, mon );

    _sisrx = new carma::antenna::bima::SisReceiver (
        RCP_NODE_ID, *this, 
	mon.antennaCommon(),			// carma::monitor::AntennaCommon & antCommon
        mon_.rx1mm().state(),
	mon_.rx1mm().sisReceiver(),
	mon_.rx1mm().xac(),
	* _aifPol2, // carma::antenna::common::AntennaIF & antIF
    anr_ ) ;				

    _varactor = new carma::antenna::common::Varactor( 
        DEFAULT_NODE_ID, *this,
        mon_.gunn1cm().varactor(),
        mon_.gunn1cm().xac(),
        mon_.gunn1cm().state() );

    _cmrx = new carma::antenna::bima::CMReceiver(
	CMRX_NODE_ID, *this,
	mon_.rx(),
    mon_.rxBiasTemps(),
	rx);

    _ant = new AntennaControlImpl( *this );

    initialize(config);
}

// -----------------------------------------------------------------------------
IFCanMaster::~IFCanMaster()
{
    cerr << "IFCanMaster::~IFCanMaster()" << endl;
    // Remove devices from Master - this prevents the run or timer thread
    // from calling processMsg or simulateMsg on deleted devices...
    Master::removeDevice(_cmrx->getApi(), _cmrx->getNode());
    Master::removeDevice(_varactor->getApi(), _varactor->getNode());
    Master::removeDevice(_sisrx->getApi(), _sisrx->getNode());
    Master::removeDevice(_aifPol1->getApi(), _aifPol1->getNode());
    Master::removeDevice(_aifPol2->getApi(), _aifPol2->getNode());
}

// -----------------------------------------------------------------------------
void IFCanMaster::initialize(Configuration &config)
{
  isRunning_ = false;
  // Add all of our device to the device map.
  Master::addDevice(_aifPol1);
  Master::addDevice(_aifPol2);
  // This is really gonna totally blow your freaking mind!
  Master::addDevice(_tiltMeter);
  Master::addDevice(_varactor);
  Master::addDevice(_sisrx);
  Master::addDevice(_cmrx);

  int startattempts = 0;
  while ( true )
    {
    try
      {
	_bimaShm = new SharedMemory( config.getAntenna().c_str() );
	break;
      }
    catch ( ... )
      {
	if ( startattempts++ < 10 ) 
	  {
	    log_ << Priority::WARN << "Unable to open shared memory file, retrying in 1 second";
	    sleep(1);
	  }
	else
	  throw CARMA_ERROR( "Unable to open shared memory file after 9 attempts!" );
      }
    }
	// Start new thread 	 
	//int status = pthread_create( &runThreadId_, NULL, runThreadEntry, (void *)this ); 	 

	//if ( status != 0 ) 	 
	//	throw CARMA_EXCEPTION( carma::canbus::PthreadFailException, 	 
	//			"IFCanMaster::initialize() " 	 
	//			"- Unable to start run thread. " 	 
	//			+ (const string)strerror( status ) );

}

// -----------------------------------------------------------------------------
void IFCanMaster::setInitialized( const bool state )
 {
   if(state){
     _bimaShm->putData( "INITIAL", &RUNNING);
   }
   else{
     _bimaShm->putData( "INITIAL", &STOPPED);
   }
 }
 
 // -----------------------------------------------------------------------------
map<msgType, string> IFCanMaster::getControls() const
{
    // Return an empty map for now...
    map<msgType, string> empty;
    return empty;
}

// -----------------------------------------------------------------------------
void IFCanMaster::updateStatus()
{
    map<canbus::busIdType, canbus::busStatusType> busStatus;
    map<canbus::busIdType, canbus::busStatusType>::iterator busIter;
    busIter = busStatus.begin();

    mon_.can().host().nActiveNodes().setValue( getOnlineNodeCount() );
    mon_.can().host().nOfflineNodes().setValue( getOfflineNodeCount() );
    mon_.can().host().nLatePackets().setValue( getLatePacketCount() );
    mon_.can().host().nDonglelessPackets().setValue(getDonglelessPacketCount());
    mon_.can().host().nUnknownPackets().setValue( getUnknownPacketCount() );
    mon_.can().host().hostname().setValue( "somewhere" );

}

// -----------------------------------------------------------------------------
void IFCanMaster::reset() 
{
    // Perform a hardware reset.
    CanDio::reset();
}

// -----------------------------------------------------------------------------
void IFCanMaster::softReset() 
{
    // Perform a software reset to all modules.
    Master::softwareReset();
}

// -----------------------------------------------------------------------------
void IFCanMaster::run()
{
    Master::run();
}

// -----------------------------------------------------------------------------
void IFCanMaster::start()
{
    // Start Master::run thread...
    if (!isRunning_) {

        int status;

        // Basic method here is to use a condition variable to ping-pong with
        // the runThread which will then signal when it has indeed started. 
        // After that this routine can return.  Care is taken not to block this
        // guy forever.  There is a small performance hit here but it avoids
        // the start/stop race.
        ScopedPthreadMutexLock lock(isRunningGuard_);

        // Create separate thread to block on Master::run()...
        status = pthread_create(&runThreadId_, NULL,
                runThreadEntry, static_cast<void *>(this));
        if (status != 0)
            throw CARMA_EXCEPTION(ErrorException,
                    "IFCanMaster::start() - Unable to create "
                    "run thread - " + (string) strerror(status));

        // Block on condition variable until the runThread signals.  In
        // pthreadanese - lock mutex, test predicate, wait (returns with mutex
        // locked), test predicate again, unlock mutex, continue...  Note that
        // the runThreadStartedGuard_ is unlocked via scoped lock.
        while (!isRunning_)
            isRunningCond_.Wait(isRunningGuard_);

    } else {
        throw CARMA_EXCEPTION(ErrorException,
                "IFCanMaster::start() - start() called twice.  Must call only "
                "once or call stop() prior to start().");
    }
}

// -----------------------------------------------------------------------------
void IFCanMaster::stop()
{
    // This whole stop/start thing is pretty shoddy.  Really I need a 
    // better way to hide and partition threads in the canbus::Master.
    // Perhaps someday in the future...
    void *result;
    int status;
    ScopedPthreadMutexLock lock(isRunningGuard_);

    if (isRunning_) {

        // OK, here I must cancel the runThread prior to stopping master.
        // The reason for this is that Master::run() blocks on pthread_join
        // with the timer thread.  In addition Master::stop(), calls 
        // cancel and then pthread_join on the timer thread as well.  This
        // is an error if stop() is called prior to cancellation of the
        // run thread because only one thread can be joined on a thread!
        // This problem is not trivial and needs a more general fix in 
        // the Master base class.

        // Cancel run thread...
        status = pthread_cancel(runThreadId_);
        if (status != 0)
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "IFCanMaster::stop() - Error cancelling run thread."
                + (string)strerror(status));
              
        // Block on it for the return value...
        status = pthread_join(runThreadId_, &result);
        if (status != 0)
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "IFCanMaster::stop() - Error joining on run thread."
                + (string)strerror(status));
        
        // Finally, double check that the result is what we want.
        if (result != PTHREAD_CANCELED)
            throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
                "IFCanMaster::stop() = Run thread returned with invalid "
                "value.");
            
        // Stop base Master - See above note...
        Master::stop();
            
        isRunning_ = false;

    } else {
        // Thread wasn't running in the first place.
    }
}

// -----------------------------------------------------------------------------
void *IFCanMaster::runThreadEntry(void *arg)
{
    IFCanMaster *This;

    // Cat arg (this) to This
    This = static_cast<IFCanMaster *>(arg);

    try {
        // start() method is blocked here and waiting for this thread to 
        // signal back to it. I'm pretty sure that it is safe to not disable
        // cancellation here as start() is blocked until it is signalled below.

        // Lock mutex, set predicate and signal to waiters.
        This->isRunningGuard_.Lock();
        This->isRunning_ = true;
        This->isRunningGuard_.Unlock();
        This->isRunningCond_.Signal();

        This->run();
        return EXIT_SUCCESS;
    } catch (...) {
        try {
            // Things went south somewhere...
            // Make sure we've signalled to the object creator as it is 
            // absolutely essential that we unblock start() to prevent
            // deadlock.
            if (This->isRunningGuard_.TryLock()) {
                if (!This->isRunning_) {
                    This->isRunning_ = true;
                    This->isRunningCond_.Signal();
                }
                This->isRunningGuard_.Unlock();
            }
            This->log_ << log4cpp::Priority::WARN
                << "IFCanMaster::runThreadEntry() - Unknown exception caught. "
                << "Thread is exsiting but successfully signalled "
                << "IFCanMaster::start() (main should continue on).";
        } catch (...) { 
            // Another exception!!!  Must have come from Pthread objects...
            // Not much we can do here - get out of dodge!
            This->log_ << log4cpp::Priority::ERROR
                << "IFCanMaster::runThreadEntry() - Exception caught within "
                << "retry block.  Cannot guarantee that IFCanMaster::start "
                << "has been signalled to avoid deadlock. Aborting.";
            exit(EXIT_FAILURE);
        }
    }
    return EXIT_SUCCESS;
}

// -----------------------------------------------------------------------------
void *IFCanMaster::startWriterThread( void *anObj )
{
  IFCanMaster *ifcm = static_cast<IFCanMaster *>(anObj);

  CPTRACE( Trace::TRACE2, "Starting IFCanMaster Writer Thread tied to canbus" );
  ifcm->writerThread();

  return NULL;
}


void IFCanMaster::writerThread( void )
{
  CPTRACE( Trace::TRACE1, "Entering Writer Thread..." );

  _ifReader.setNoneAvailable();

  try
  {
    // default values for SISRX_IVCURVE
    float scanv1 = 0.;
    float scanv2 = 14.;
    float scandv = 0.05;	

    while ( true )
    {
      _ifReader.read();


      switch ( _ifReader.getAddr() )
      {
	case AntennaIFClient::SELECT_IF_BAND: 
	  {
	    using namespace carma::antenna::common;
	    short band;
	    unsigned short pamband;
	    pamband = RxTypeInfo::ifSwitchPositionFromRxType( RxControl::RX3MM );

	    _ifReader.getPayload( band );
	    if ( band != 4 ) {
	      _varactor->enableGunn(false);	// turn off the cm varactor-tuned Gunn
            }

	    if ( band == 0 ) // bima A band, 1mm
	      pamband = RxTypeInfo::ifSwitchPositionFromRxType( RxControl::RX1MM );
	    else if ( band == 1 ) // bima B band, 3mm
	      pamband = RxTypeInfo::ifSwitchPositionFromRxType( RxControl::RX3MM );
	    else if ( band == 2 ) // bima C band, ??
	      ;
	    else if ( band == 3 ) // bima D band, 1mm
	      pamband = RxTypeInfo::ifSwitchPositionFromRxType( RxControl::RX1MM );
	    else if ( band == 4 ){ // bima E band, 1cm
	      pamband = RxTypeInfo::ifSwitchPositionFromRxType( RxControl::RX1CM );
	      _cmrx->setDrainVoltage();
	      _cmrx->setDrainCurrent();
	      _cmrx->setIFCurrent();
	      _varactor->enableGunn(true);	// turn on the cm varactor-tuned Gunn
	    }

	    _aifPol1->selectBand( pamband );
	  }
	  break;
	case AntennaIFClient::SET_IF_LEVEL : 
	case AntennaIFClient::SET_IF1_LEVEL : 
	  {
	    float power;
	    _ifReader.getPayload( power );
	    _aifPol1->setPower( power );
	  }
	  break;
	case AntennaIFClient::SET_IF2_LEVEL : 
	  {
	    float power;
	    _ifReader.getPayload( power );
	    _aifPol2->setPower( power );
	  }
	  break;
	case AntennaIFClient::SET_IF_ATTEN : 
	case AntennaIFClient::SET_IF1_ATTEN : 
	  {
	    float atten;
	    _ifReader.getPayload( atten );
	    _aifPol1->setAtten( atten );
	  }
	  break;
	case AntennaIFClient::SET_IF2_ATTEN : 
	  {
	    float atten;
	    _ifReader.getPayload( atten );
	    _aifPol2->setAtten( atten );
	  }
	  break;
	case AntennaIFClient::START_IF1_FASTSAMP :
	  {
	    _aifPol1->startFastSample( 3 );
	  }
	  break;
	case AntennaIFClient::START_IF2_FASTSAMP :
	  {
	    _aifPol2->startFastSample( 3 );
	  }
	  break;
	case AntennaIFClient::STOP_IF1_FASTSAMP :
	  {
	    _aifPol1->stopFastSample();
	  }
	  break;
	case AntennaIFClient::STOP_IF2_FASTSAMP :
	  {
	    _aifPol2->stopFastSample();
	  }
	  break;
	case AntennaIFClient::SISRX_SET_VDRAIN :
	  {
            short stage = 1;
	    float VdInVolts;
	    _ifReader.getPayload( VdInVolts );
	    _sisrx->setVd( stage, static_cast<float>(VdInVolts) );
	  }
	  break;
	case AntennaIFClient::SISRX_SET_VGATE1 :
	  {
	    float Vg;
            short stage = 1;
	    _ifReader.getPayload( Vg );
	    _sisrx->setVg( stage, static_cast<float>(Vg) );
	  }
	  break;
	case AntennaIFClient::SISRX_SET_VGATE2 :
	  {
	    float Vg;
            short stage = 2;
	    _ifReader.getPayload( Vg );
	    _sisrx->setVg( stage, static_cast<float>(Vg) );
	  }
	  break;
	case AntennaIFClient::SISRX_SET_VJ :
	  {
	    float vj;
	    _ifReader.getPayload( vj );
	    _sisrx->setVj ( static_cast<float>(vj) );
	  }
	  break;
	case AntennaIFClient::SISRX_SET_LOOP_MODE :
	  {
	    short mode;
	    _ifReader.getPayload( mode );
	    if (mode == 0) _sisrx->setVjLoopMode( carma::antenna::common::SisReceiver::VJ_CLOSED ) ;
	    if (mode == 1) _sisrx->setVjLoopMode( carma::antenna::common::SisReceiver::VJ_OPEN );
	    if (mode == 2) _sisrx->setVjLoopMode( carma::antenna::common::SisReceiver::VJ_FINITE );
	  }
	  break;
	case AntennaIFClient::SISRX_TUNE :
	  {
	    float freqGHz;
	    _ifReader.getPayload( freqGHz );
	    _sisrx->tuneMixer ( static_cast<float>(freqGHz) );
	  }
	  break;
	case AntennaIFClient::SISRX_GETVGAP :
	  {
	    _sisrx->getVgap ( CAC::SisReceiver::STORED, 0.0 );
	  }
	  break;
	case AntennaIFClient::SISRX_SCANV1 :
	  {
	    _ifReader.getPayload( scanv1 );
          }
	  break;
	case AntennaIFClient::SISRX_SCANV2 :
	  {
	    _ifReader.getPayload( scanv2 );
          }
	  break;
	case AntennaIFClient::SISRX_SCANDV :	
	  {
	    _ifReader.getPayload( scandv );
          }
	  break;
	case AntennaIFClient::SISRX_IVCURVE :
	  {
            int stepmsec = 100;
	    int seqno = 0;
	    bool doPower = 1;
	    _sisrx->doIVCurve ( scanv1, scanv2, scandv, stepmsec, seqno, doPower  );
	  }
	  break;
	default:
	  {
	    log_ << Priority::WARN << "Received unrecognized IF command";
	  }
	  break;
      } // switch
    } // while
  }
  catch ( ErrorException &eex )
  {
    log_ << Priority::ERROR << "IF writer thread exiting, " << eex.what();
  }
  catch ( ... )
  {
    log_ << Priority::ERROR
      << "Telemetry writer thread exiting, uncaught default exception";
  }

}

