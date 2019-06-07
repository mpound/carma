
// $Id: NotificationConsumer.cc,v 1.42 2013/01/03 21:19:53 abeard Exp $

/**
 * A base class to accept notifications from a Push supplier. This class
 * is usually extended and the push method overridden.
 *
 *
 *  @author Chul Gwon
 *  @version $Revision: 1.42 $, $Date: 2013/01/03 21:19:53 $
 */

#include "carma/util/NotificationConsumer.h"

#include "carma/util/UserException.h"
#include "carma/util/programExtras.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Trace.h"
#include "carma/util/Logger.h"
#include "carma/util/demangle.h"
#include "carma/util/loggingUtils.h"
#include "carma/util/Backtrace.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/programLogging.h"
#include "carma/util/Orb.h"
#include "carma/util/ScopedLock.h"

#include <unistd.h>

using namespace ::std;
using namespace carma::util;

using ::log4cpp::Priority;
using ::log4cpp::Category;

namespace {
  ::pthread_mutex_t gOrbRunGuard = PTHREAD_MUTEX_INITIALIZER;
}


// ----------------------------------------------------------------------
// Based on the PushConsumer_impl implementation
// ----------------------------------------------------------------------


NotificationConsumer::NotificationConsumer(Orb* localOrb,
					   string channelName,
					   string consumerName) :
  orb_(localOrb->duplicateOrb()),
  poa_(localOrb->getPOA(channelName+"."+consumerName+"."+"PushConsumer")),
  channelName_ (channelName),
  consumerName_ (consumerName),
  isActive_(false)
{
  localOrb_    = localOrb;
  setProxyPushSupplier();
  activatePoa();
}

NotificationConsumer::~NotificationConsumer( )
try {
    try {
        Category * logger = getProgramLoggerIfAvailable( );
        
        if ( logger != 0 ) {
            *logger << Priority::WARN
                    << "NotificationConsumer::~NotificationConsumer called (\""
                    << channelName_ << "\", \"" << consumerName_ << "\").";
               
            string btText;
            
            {
                Backtrace bt;
                
                bt.captureNoThrow( );
                
                btText = bt.formattedAsString( "    ", "\n" );
            }
    
            *logger << Priority::WARN << "Backtrace is:";
    
            logMultipleLines( *logger, Priority::WARN, btText );
        }
    } catch ( ... ) {
        // Just stifle any exceptions
    }
    
    deactivateConsumer( );
    PortableServer::ObjectId_var oid = poa_->servant_to_id(this);
    poa_->deactivate_object(oid.in());

    if ( !CORBA::is_nil( proxyPushSupplier_ ) ) {
        proxyPushSupplier_->disconnect_structured_push_supplier( );
    }

    // Let the xxx_var manage the ref counting
    proxyPushSupplier_ =
        CosNotifyChannelAdmin::StructuredProxyPushSupplier::_nil();
} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in"
            " NotificationConsumer::~NotificationConsumer - " +
            getStringForCaught() );
    } catch ( ... ) {
        // Just stifle any exceptions
    }

    // Just stifle any exceptions
    return;
}


void
NotificationConsumer::activatePoa() {

  try {

    // connect consumer to same POA each time

    const string objString = string( "pushConsumerObject." ) + consumerName_;

    PortableServer::ObjectId_var objectId =
      PortableServer::string_to_ObjectId( objString.c_str( ) );

    poa_->activate_object_with_id( objectId.in( ), this );
    
    localOrb_->activatePOA(localOrb_->getPersistentPOAName(channelName_ + "." + consumerName_ + "." + "PushConsumer"));
  } catch ( const PortableServer::POAManager::AdapterInactive & ) {
    programLogErrorIfPossible( "NotificationConsumer::run - caught "
        "PortableServer::POAManager::AdapterInactive" ); 
    throw;
  }  catch ( const CORBA::SystemException & cse )  {
      ostringstream oss;

      oss << "NotificationConsumer::run - caught CORBA::SystemException ("
          << demangleTypeName( typeid( cse ) ) << ") - " << cse
          << "; reason = " << cse._info().c_str()
          << "; minor code = " << cse.minor();

      programLogErrorIfPossible( oss.str() );
      throw;
  }
}


// IDL to C++ mapping
void
NotificationConsumer::push_structured_event(
    const CosNotification::StructuredEvent & event )
throw ( CORBA::SystemException, CosEventComm::Disconnected )
try {
  /*
    Trace * trace = getProgramTraceObjectIfAvailable( );

    if ( trace != 0 )
        trace->write( Trace::TRACE1, "Event data received!!" );
  */
} catch ( const CORBA::SystemException & ) {
    throw;
} catch ( const CosEventComm::Disconnected & ) {
    throw;
} catch ( ... ) {
    // BULLSHIT_TWC - 14 Sept 2005 - Figure out what to do here
}

void NotificationConsumer::disconnect_structured_push_consumer()
throw ( CORBA::SystemException )
try {

  // the only reason this method is called is because the cleanup from
  // the notification server is trying to shut down the consumer.
  // however, the cleanup is just for "unclean" exits of the consumer,
  // not if it's still actually up.  so if it tries to call this, we
  // create a new consumer and continue to go.

  ScopedLock< ::pthread_mutex_t > lock( gOrbRunGuard );
  if ( !CORBA::is_nil( proxyPushSupplier_ ) ) {
    proxyPushSupplier_->disconnect_structured_push_supplier( );
  }

  // don't release - let the _var handle management
  //  CORBA::release( proxyPushSupplier_ );
  setProxyPushSupplier();
  CosNotifyComm::StructuredPushConsumer_var spc = _this( );
  proxyPushSupplier_->connect_structured_push_consumer( spc );
  // - we don't want a shutdown here since NC can be run in
  //   multi-threaded process
  //orb_->shutdown(false);
} catch ( const CORBA::SystemException & ) {
    throw;
} catch ( ... ) {
    // BULLSHIT_TWC - 14 Sept 2005 - Figure out what to do here
}


void
NotificationConsumer::offer_change(
    const CosNotification::EventTypeSeq & added,
    const CosNotification::EventTypeSeq & removed )
throw( CORBA::SystemException, CosNotifyComm::InvalidEventType )
try {
  try {
    Category *logger = getProgramLoggerIfAvailable();

    if ( logger != 0) {
      *logger << Priority::WARN
	      << "NotificationConsumer::offer_change called (\""
	      << channelName_ << "\" , \"" << consumerName_ << "\").";

      string btText;
      {
	Backtrace bt;
	bt.captureNoThrow( );
	btText = bt.formattedAsString( "    ", "\n" );
      }
      
      *logger << Priority::WARN << "Backtrace is: ";
      logMultipleLines( *logger, Priority::WARN, btText );
    }
  } catch (...) {
    // stifle logging exceptions
  }
  // Intentionally left blank
} catch ( const CORBA::SystemException & ) {
    throw;
} catch ( const CosNotifyComm::InvalidEventType & ) {
    throw;
} catch ( ... ) {
    // BULLSHIT_TWC - 14 Sept 2005 - Figure out what to do here
}


PortableServer::POA_ptr
NotificationConsumer::_default_POA( ) {
    return PortableServer::POA::_duplicate( poa_ );
}


/**.......................................................................
 * A method for calling run if eveng the ProxyPushSupplier object -
 * represents the instance of the notification queue for this consumer
 */
void
NotificationConsumer::setProxyPushSupplier( ) 
{
    proxyPushSupplier_ = localOrb_->getNotifyProxyPushSupplier(channelName_, consumerName_);
}


/**.......................................................................
 * A method to stop the consumer DO
 */
void
NotificationConsumer::deactivateConsumer( ) {
    isActive_ = false;
}


/**.......................................................................
 * A method to test status of the consumer DO
 */
bool
NotificationConsumer::isActive( ) {
    return isActive_ ;
}


/**.......................................................................
 * A method for calling run if event channel is already defined
 */
void
NotificationConsumer::run( )
try {
    Category *logger = getProgramLoggerIfAvailable();
  
    // force servant destruction when out of scope
    // BULLSHIT_TWC - 19 Sept 2005 Figure out what it up with this
    // BULLSHIT_TWC - 19 Sept 2005 Should this at least use _this( )?
    // PortableServer::ServantBase_var tempConsumer = this;

    try {
        CosNotifyComm::StructuredPushConsumer_var spc = _this( );

        proxyPushSupplier_->connect_structured_push_consumer( spc );

        // Now serve the objects (normally blocks here forever).
        isActive_ = true;

        while ( isActive( ) ) {
            // Retrieve and activate the ORB
            if ( CORBA::is_nil( orb_ ) )
              throw CARMA_ERROR("ORB pointer is nil!");

            try {
		// EML: Putting global mutex locks around the global
		// orb already forces serial execution of ANY
		// multi-threaded program using CORBA, and we probably
		// don't want that either, but I certainly don't want
		// it around our private orb

		if(localOrb_->workPending())
                  localOrb_->performWork();

		// EML: For now, I'm putting a 10ms sleep to reduce
		// the load, since we don't expect data on any
		// channel any more frequently than once per
		// half-sec.  We should figure out if there's a
		// better general way to handle the limitations of
		// the CORBA implementation

        struct timespec ts;
                ts.tv_sec  = 0;
                ts.tv_nsec = 10000000;
        nanosleep(&ts, 0);

            } catch (CORBA::BAD_INV_ORDER &) {
                deactivateConsumer();
                programLogWarnIfPossible( "NotificationConsumer::run() - "
                        "Received BAD_INV_ORDER, most likely"
                        " on termination. Deactivating." );
            }

            // 1s sleep exists in work_pending call of OB-4.1.2, but not
            // OB-4.2.0 ... adding another 1ms to this just in case
            // using unix sleep, and upping to 10ms (since that's what
            // it used to be with the old linux scheduler)
            usleep( 10 );

        }

	try {
	  if ( logger != 0) {
	    *logger << Priority::WARN
		    << "NotificationConsumer::run() returning (\""
		    << channelName_ << "\" , \"" << consumerName_ << "\").";
	    
	    string btText;
	    {
	      Backtrace bt;
	      bt.captureNoThrow( );
	      btText = bt.formattedAsString( "    ", "\n" );
	    }
	    
	    *logger << Priority::WARN << "Backtrace is: ";
	    logMultipleLines( *logger, Priority::WARN, btText );
	  }
	} catch (...) {
	  // stifle logging exception
	}
    }  catch ( const CORBA::SystemException & cse )  {
      if ( logger != 0 ) {
        ostringstream oss;

        oss << "NotificationConsumer::run - caught CORBA::SystemException ("
            << demangleTypeName( typeid( cse ) ) << ") - " << cse
            << "; reason = " << cse._info().c_str()
            << "; minor code = " << cse.minor();

        const string msg = oss.str( );

        *logger << Priority::ERROR << msg;
      }

      throw;
    }
} catch ( ... ) {
    try {
        string msg = "NotificationConsumer::run - exception escaping - ";

        msg += getStringForCaught( );

        getProgramLogger( ) << Priority::ERROR << msg;
    } catch ( ... ) {

    }

    throw;
}

