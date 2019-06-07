#include "carma/correlator/obsRecord2/CorrDataConsumer.h"

#include "carma/corba/Client.h"
#include "carma/corba/corba.h"
#include "carma/corba/Server.h"
#include "carma/correlator/lib/CorrelatorDataPool.h"
#include "carma/correlator/lib/CorrelatorListener.h"
#include "carma/correlator/obsRecord2/CorDataBase.h"
#include "carma/util/compileTimeCheck.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <boost/thread.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

using namespace boost;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::correlator::obsRecord2;
using namespace carma::util;
using namespace std;

namespace {

typedef ScopedLock< PthreadMutex > CorbaCorrConsumerStatsGuardLock;

}  // namespace < anonymous >

class CorrDataConsumer::Impl {
    public:
        //! @brief Constructor
        Impl( corba::Server & server,
              const string & notificationChannelName,
              Listener & listener );

        virtual ~Impl( );

        //! @brief method to start receiving events.
        //!
        //! Does not return
        void getData3( );

        //! @brief Method called when events arrive
        void operator()( const CorData_s & cordata );

        //! @brief Retrieve deserialization stats.
        CorbaCorrConsumerStats getCorbaCorrConsumerStats( );

    private:
  
      corba::Server * server_;

      // No copying
        Impl( const Impl & rhs );
        Impl & operator=( const Impl & rhs );

        static const string className_;

        CorrelatorDataPool              cdp_;
        string                          notificationChannelName_;
        Listener &                      listener_;
        CorbaCorrConsumerStats          corbaCorrConsumerStats_;
        double                          lastCdRxMJD_;
        mutable PthreadMutex            corbaCorrConsumerStatsGuard_;

};

const string CorrDataConsumer::Impl::className_( "CorrDataConsumer::Impl" );

CorrDataConsumer::Impl::Impl( corba::Server & server,
                               const string & notificationChannelName, 
                               Listener &    listener ) :
server_( &server ),
cdp_(),
notificationChannelName_( notificationChannelName ),
listener_( listener ),
corbaCorrConsumerStats_( notificationChannelName ),
lastCdRxMJD_( 0.0 )
{
    // Nothing
}

CorrDataConsumer::Impl::~Impl( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}

void
CorrDataConsumer::Impl::getData3( )
{
  try {
    
    server_->addNotificationServantFunctor<
      CorrDataConsumer::Impl, CorData_s >( 
          *this, 
          notificationChannelName_,
          (notificationChannelName_ + string("1")) );

    // I use the polling version of Server::run here to expedite shutdown
    while ( !server_->terminated( ) ) {
      server_->work();
      boost::this_thread::sleep( boost::posix_time::milliseconds( 10 ) );
    }

  } catch(...) {
    logCaughtAsError( );
  }
}

void
CorrDataConsumer::Impl::operator()( const CorData_s & cordata )
{
  const double cdRxMJD = Time::MJD();

  const int correlatorDataLength = cordata.correlatorData.length();

  const char * const byteArray =
    reinterpret_cast< const char * >( cordata.correlatorData.get_buffer() );

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

    // With corba::Server implementation, we can no longer monitor the
    // demarshalling time.  This is OK though, it was always less than 100us.
    const double demarshalTimeInMs = 0.0;
    const double deserialTimeInMs = 
        ( deserMJD - cdRxMJD ) * Time::MILLISECONDS_PER_DAY;
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

  // This call is allowed to, and in general does (i.e. DataCollectorN),
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
  
  compileTimeCheck< (sizeof( CORBA::Octet ) == sizeof( char )) >();
}

CorbaCorrConsumerStats
CorrDataConsumer::Impl::getCorbaCorrConsumerStats( ) 
{
    static const double activeThreshMjd = 1000.0 / Time::MILLISECONDS_PER_DAY;

    const CorbaCorrConsumerStatsGuardLock lock( corbaCorrConsumerStatsGuard_ );

    if ( Time::MJD() - lastCdRxMJD_ > activeThreshMjd ) 
        corbaCorrConsumerStats_.active = false;

    return corbaCorrConsumerStats_;
}

CorrDataConsumer::CorrDataConsumer( carma::corba::Server & server,
                                      const string & notificationChannelName,
                                      Listener &     listener ) :
impl_( new Impl( server, notificationChannelName, listener ) )
{
    // Nothing
}

CorrDataConsumer::~CorrDataConsumer( )
try {
    // Nothing
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void
CorrDataConsumer::getData( )
{
    // CPTRACE( Trace::TRACE5, "calling getData3()");

    // don't use lock, since getData3 never returns
    impl_->getData3();
}

CorbaCorrConsumerStats
CorrDataConsumer::getCorbaCorrConsumerStats( ) 
{
    if ( impl_.get() == 0 ) 
        return CorbaCorrConsumerStats( "<ERROR>" );
    else 
        return impl_->getCorbaCorrConsumerStats();
}
