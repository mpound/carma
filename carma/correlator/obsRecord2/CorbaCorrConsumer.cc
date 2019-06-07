#include "carma/correlator/obsRecord2/CorbaCorrConsumer.h"

#include "carma/corba/Client.h"
#include "carma/corba/corba.h"
#include "carma/corba/Server.h"

#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/correlator/lib/CorrelatorDataPool.h"
#include "carma/correlator/lib/CorrelatorListener.h"
#include "carma/correlator/obsRecord2/CorDataBase_skel.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/NotificationConsumer.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/Trace.h"
#include "carma/util/Orb.h"

#include <orbsvcs/CosNotifyCommS.h>

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::correlator::obsRecord2;
using namespace carma::util;


namespace {

typedef ScopedLock< PthreadMutex > CorbaCorrConsumerStatsGuardLock;

}  // namespace < anonymous >

class CorbaCorrConsumer::CorrDataConsumer : public util::NotificationConsumer { 
public:

    CorrDataConsumer( CorbaCorrConsumer::Impl & impl,
                      Orb * localOrb,
                      const std::string channelName,
                      const std::string consumerName );

    ~CorrDataConsumer( );

    /**
     * Callback function that is called by the notification server
     * when notrifications arrive for this consumer.
     */
    void push_structured_event( const CosNotification::StructuredEvent & event )
        throw ( CosEventComm::Disconnected, CORBA::SystemException );

private:

    CorbaCorrConsumer::Impl & impl_;

}; // class CorbaCorrConsumer::CorrDataConsumer

class CorbaCorrConsumer::Impl {
    public:

  Impl( Orb* localOrb,
    const string & notificationChannelName, 
	Listener & listener );
  
        virtual ~Impl( );

        //! @brief method to start receiving events.
        //!
        //! Does not return
        void getData3( );

        //! @brief Method called when events arrive
        void processEvent( const CosNotification::StructuredEvent & event );

        //! @brief Retrieve deserialization stats.
        CorbaCorrConsumerStats getCorbaCorrConsumerStats( );

    private:
  
  Orb* localOrb_;

      // No copying
        Impl( const Impl & rhs );
        Impl & operator=( const Impl & rhs );

        static const string className_;

        CorrelatorConfigChecker * const ccc_;

        CorrelatorDataPool              cdp_;
        string                          notificationChannelName_;
        Listener &                      listener_;
        CorbaCorrConsumerStats          corbaCorrConsumerStats_;
        double                          lastCdRxMJD_;
        mutable PthreadMutex            corbaCorrConsumerStatsGuard_;

};

const string CorbaCorrConsumer::Impl::className_( "CorbaCorrConsumer::Impl" );

CorbaCorrConsumer::Impl::Impl( Orb* orb,
                               const string & notificationChannelName,
                               Listener & listener ) :
localOrb_( orb ),
ccc_( CorrelatorConfigChecker::getInstance() ),
cdp_(),
notificationChannelName_( notificationChannelName ),
listener_( listener ),
corbaCorrConsumerStats_( notificationChannelName ),
lastCdRxMJD_( 0.0 )
{
    Program::getProgram().orbInit( localOrb_ );
}


CorbaCorrConsumer::Impl::~Impl( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}

void
CorbaCorrConsumer::Impl::getData3( )
{
  try {
    
    CorrDataConsumer * cdc = 0;
    
    cdc = new CorrDataConsumer( *this,
                  localOrb_,
                  notificationChannelName_,
                  (notificationChannelName_ + string("1")) );
      
    if ( cdc != 0 ) {
      cdc->run();
    } else {
      CARMA_CPTRACE(Trace::TRACE1, "CorrDataConsumer is null");
      LOGEXCEPTION(ccc_, className_, "CorrDataConsumder is null");
    }
  } catch(...) {
    logCaughtAsError( );
  }
}


void 
CorbaCorrConsumer::Impl::processEvent( 
    const CosNotification::StructuredEvent & event )
{
    CARMA_CPTRACE(Trace::TRACE1, "processEvent() " + notificationChannelName_ );

  const double cdRxMJD = Time::MJD();
  static unsigned nullDataCounter = 0;

  // unpack data from event
  const CorData_s * cordata = 0;
  event.filterable_data[0].value >>= cordata;

  // EML added check for NULL data returned from the CarmaCorrelatorServer

  if(cordata == 0) {

    ++nullDataCounter;

    if(nullDataCounter % 10 == 0) {
      std::ostringstream os;
      os << "Intercepted a null CorrData object from " << notificationChannelName_ << " counter = " << nullDataCounter;
      programLogInfo(os.str());
    }

    return;
  }

  const double deMarshMJD = Time::MJD();

  compileTimeCheck< (sizeof( CORBA::Octet ) == sizeof( char )) >();

  if(cordata == 0) {
    return;
  }

  const int correlatorDataLength = cordata->correlatorData.length();

  const char * const byteArray =
    reinterpret_cast< const char * >( cordata->correlatorData.get_buffer() );


  // reconstruct Correlator Data object
  CorrelatorData * const cd = cdp_.getCorrelatorData();

  // Deserialize byte array into correlator data while collecting stats.
  bool deserializationError = false;
  try {
    cd->deserial( byteArray, correlatorDataLength );
  } catch (...) {
    deserializationError = true;   
    logCaughtAsError( );
    // For now just continue on - if we find we have more or any of these
    // we can do something else later.
  }

  const double deserMJD = Time::MJD();

  cd->setHeaderReceivedMJD( cdRxMJD );

  { // Fill in CorbaCorrConsumerStats object.
    const CorbaCorrConsumerStatsGuardLock lock( corbaCorrConsumerStatsGuard_ );

    const double oneFrameMJD = 0.5 / 86400.0; 
    const double endOfSampleMJD = cd->getHeader().getMJD() + oneFrameMJD;

    const double assemblyMJD = cd->getHeader().getAssembledMJD();
    const double transmissionMJD = cd->getHeader().getTransmissionMJD();
    const double receiveMJD = cd->getHeader().getReceivedMJD();

    const double assemblyLatencyInMs = 
        ( assemblyMJD - endOfSampleMJD ) * Time::MILLISECONDS_PER_DAY;
    const double transmissionLatencyInMs = 
        ( transmissionMJD - endOfSampleMJD ) * Time::MILLISECONDS_PER_DAY;
    const double receiveLatencyInMs =
        ( receiveMJD - endOfSampleMJD ) * Time::MILLISECONDS_PER_DAY;

    const double demarshalTimeInMs = 
        ( deMarshMJD - cdRxMJD ) * Time::MILLISECONDS_PER_DAY;
    const double deserialTimeInMs = 
        ( deserMJD - deMarshMJD ) * Time::MILLISECONDS_PER_DAY;
    const double totalTimeInMs = 
        ( Time::MJD() - cdRxMJD ) * Time::MILLISECONDS_PER_DAY;

    lastCdRxMJD_ = cdRxMJD;
    corbaCorrConsumerStats_.active = true;
    corbaCorrConsumerStats_.errorOnLastDeserialization = deserializationError;
    corbaCorrConsumerStats_.assemblyLatencyInMs = assemblyLatencyInMs;
    corbaCorrConsumerStats_.transmitLatencyInMs = transmissionLatencyInMs;
    corbaCorrConsumerStats_.receiveLatencyInMs = receiveLatencyInMs; 
    corbaCorrConsumerStats_.corbaDemarshalingTimeInMs = demarshalTimeInMs; 
    corbaCorrConsumerStats_.deserializationTimeInMs = deserialTimeInMs;
    corbaCorrConsumerStats_.totalProcTimeInMs = totalTimeInMs;
    if ( deserializationError ) 
        corbaCorrConsumerStats_.deserializationErrorCount += 1;
  }

  // This call is allowed to, and in generally does (i.e. DataCollectorN),
  // modify the CorrelatorData instance

  listener_.processData( cd );
  
  try {
    cd->decrementRefCount();
  } catch (const ErrorException& err) {
    cerr << err << endl;
  }
  CARMA_CPTRACE(Trace::TRACE1,
         "CorrelatorDataPool size= " << cdp_.getPoolSize() << endl <<
         " Number of in use objects= " << cdp_.getInUseCount() << endl <<
         " Number of available objects= " << cdp_.getAvailableCount());

}

CorbaCorrConsumerStats
CorbaCorrConsumer::Impl::getCorbaCorrConsumerStats( ) 
{
    static const double activeThreshMjd = 1000.0 / Time::MILLISECONDS_PER_DAY;

    const CorbaCorrConsumerStatsGuardLock lock( corbaCorrConsumerStatsGuard_ );

    if ( Time::MJD() - lastCdRxMJD_ > activeThreshMjd ) 
        corbaCorrConsumerStats_.active = false;

    return corbaCorrConsumerStats_;
}


CorbaCorrConsumer::CorrDataConsumer::CorrDataConsumer( 
    CorbaCorrConsumer::Impl & impl,
    Orb * localOrb,
    const std::string channelName,
    const std::string consumerName ) :
        NotificationConsumer( localOrb, channelName, consumerName ),
        impl_( impl )
{
    // Nothing
}

CorbaCorrConsumer::CorrDataConsumer::~CorrDataConsumer( )
{
    // Nothing
}
                      
void 
CorbaCorrConsumer::CorrDataConsumer::push_structured_event( 
    const CosNotification::StructuredEvent & event )
        throw ( CosEventComm::Disconnected, CORBA::SystemException )
try {
    impl_.processEvent( event );
} catch (...) {
    logCaughtAsError();
}


CorbaCorrConsumer::CorbaCorrConsumer( Orb* localOrb, 
                                      const string & notificationChannelName,
                                      Listener & listener ) :
  impl_( new Impl(localOrb, notificationChannelName, listener ) )
{
    // Nothing
}


CorbaCorrConsumer::~CorbaCorrConsumer( )
try {
    // Nothing
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void
CorbaCorrConsumer::getData( )
{
    // CPTRACE( Trace::TRACE5, "calling getData3()");

    // don't use lock, since getData3 never returns
    impl_->getData3();
}

CorbaCorrConsumerStats
CorbaCorrConsumer::getCorbaCorrConsumerStats( ) 
{
    if ( impl_.get() == 0 ) 
        return CorbaCorrConsumerStats( "<ERROR>" );
    else 
        return impl_->getCorbaCorrConsumerStats();
}
