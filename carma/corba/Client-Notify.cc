#include "carma/corba/Client.h"

#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedPthreadMutexLock.h"

#include <tao/TimeBaseC.h>

/**
 * Notes
 * 
 * A notification 'channel' is a communications path between one or more
 * 'suppliers' which create and send data, the 'notification server' which both
 * receives and resends the data and one or more 'consumers' which finally 
 * receive and process the data.  All communications are through the 
 * notification server itself - suppliers and consumers do not communicate
 * directly with each other but rather through consumer and producer 'proxies' 
 * hosted by the notification service.  
 *
 * In CORBA there is a distinction between 'push' and 'pull' consumers 
 * and producers.  We do not concern ourselves with the pull model as it is 
 * not used in CARMA nor supported by TAO. With the push model, data is pushed 
 * to the notification service from a producer and subsequently pushed to the 
 * consumer from the notification service. So a producer is a remote client to 
 * the notification server while a consumer is a server which fields remote 
 * calls from the notification server.  
 *
 * Both producers and consumers are clients to the notification server with 
 * respect to creating and connecting to a channel. The Client::Notify class
 * focuses on handling these mundane connection details.  The 
 * CosEventChannelAdmin module contains the many interfaces which are used 
 * to create and communicate with a channel.  In summary these are:
 * 
 * EventChannel - Interface for retrieving new ConsumerAdmin and SupplierAdmin
 *         objects which are used to create consumers and suppliers.
 * ConsumerAdmin - Interface for retrieving a 'ProxyPushSupplier' from the 
 *         notification server. 
 * SupplierAdmin - Interface for retrieving a 'ProxyPushConsumer' from the 
 *         notification server.
 * ProxyPushSupplier - Interface for connecting a consumer to the notification
 *         server.  This is used for telling the notification server which 
 *         servant your consumer application will use for processing particular
 *         notifications.
 * ProxyPushConsumer - Interface for connecting a supplier to the notification
 *         server.  This is used for telling the notification server which
 *         servant, if any, your supplier application will use for handling
 *         administrative calls related to the channel. It is also the client
 *         through which notification are sent from the supplier.
 * 
 * Each connection can receive callbacks from the notification server informing
 * them of administrative changes to the channel such as shutdown information
 * or changes to the events being offered.  In this manner both a PushSupplier 
 * and a PushConsumer is a servant in the traditional corba sense.  
 *
 * In practice, this is a bloated and complicated mess that epitomizes 
 * everything I hate about corba. Things are orders of magnitude worse if 
 * you start thinking about multiple threads, multiple processes, shutdown, 
 * interaction with the IMR, exception safety, etc.  Nonetheless, you'll be 
 * hard pressed to find a better summary than what I've written above and 
 * this will be needed to understand the implementation below <insert thankless
 * praise here>.
 * 
 * The implementation below is largely legacy from util/NotifyUtils.h created
 * by Chul Gwon, Tom Costa, Rick Hobbs, Myself and others over the years.  It 
 * works and that is its only saving grace.  The implementation uses the 
 * name service and locally cached references to create channels, connect to 
 * channels and optimize sending of notifications.  This scheme is not 
 * the safest route to go; notification channels and proxies are guaranteed to
 * be unique by the notification server and handled via a unique ID.  By 
 * associating names to these instead, we create artificial races between procs,
 * the notification service and the nameservice if multiple processes try to
 * establish the same name.  It also leaks resources on the notification 
 * service as there is no way to guarantee proxy shutdown and destruction. 
 * A better way to handle this would simply be to let each process establish 
 * their own connection with the notification service independent of the 
 * nameservice.  The shutdown and resource leak still persists however. 
 *
 * How does this differ from NotifyUtils of old and/or improve upon it?  The 
 * below implementation is hidden from the world as a whole. Users
 * of the notification server never have to learn the below interface
 * nor call it directly - this was not the case with NotifyUtils.h.
 * Notification implementation at the user level has been reduced to 
 * Server::addNotificationServantFunctor() and Client::sendNotification().
 * In addition the implementation has been classified, the nameserver naming 
 * scheme simplified and global data replaced with class variables.
 */
using namespace carma::corba;
using namespace carma::util;
using namespace std;

namespace {

void rethrowNotifyExceptionAsCarmaException( )
{
    try {
        throw;
    } catch ( const CosNotification::UnsupportedQoS & ) {
        throw CARMA_EXCEPTION( ErrorException,
                               "Notification channel initialized with "
                               "unsupported QoS policies." ); 
    } catch ( const CosNotifyChannelAdmin::AdminLimitExceeded & ) {
        throw CARMA_EXCEPTION( ErrorException,
                               "AdminLimit exceeded." );
    } catch ( const CosNotification::UnsupportedAdmin & ) {
        throw CARMA_EXCEPTION( ErrorException,
                               "UnsupportedAdmin." );
    } catch ( const CosEventComm::Disconnected & ) {
        throw CARMA_EXCEPTION( ErrorException,
                               "Disconnected." );
    }
} 

void verifyChannelName( const string & channel ) 
{
    if ( channel.find('\0') != string::npos )
        throw CARMA_ERROR( "Channel contains NULL characters (possibly caused "
            "by ends)" );
}

} // namespace < unnamed >

Client::Notify::Notify( Client::Naming & naming,
                        CosNotifyChannelAdmin::EventChannelFactory_var ecf ) :
    notifyChannelFactory_( ecf ),
    naming_( naming )
{
}

Client::Notify::~Notify( ) 
{
}

CosNotifyChannelAdmin::EventChannel_var
Client::Notify::getNotifyChannel( const string & channelName )
{
    CosNotifyChannelAdmin::EventChannel_var notifyChannel;
    try { 
        notifyChannel = naming_.resolveName<
            CosNotifyChannelAdmin::EventChannel>( channelName );
    } catch ( const CosNaming::NamingContext::NotFound &err ) {
        notifyChannel = createNotifyChannel( channelName );
    }

    return notifyChannel;
}

CosNotifyChannelAdmin::SupplierAdmin_var
Client::Notify::getNotifySupplierAdmin( const string & channelName )
{
    const string adminName( channelName + "Channel.supplierAdmin.name" );

    CosNotifyChannelAdmin::SupplierAdmin_var supplierAdmin;
    try { 
        supplierAdmin = naming_.resolveName<
            CosNotifyChannelAdmin::SupplierAdmin>( adminName );
    } catch ( const CosNaming::NamingContext::NotFound & ) {
        supplierAdmin = createNotifySupplierAdmin( channelName );
    }

    return supplierAdmin._retn();
}

CosNotifyChannelAdmin::ConsumerAdmin_var
Client::Notify::getNotifyConsumerAdmin( const string & channelName )
{
    const string adminName( channelName + "Channel.consumerAdmin.name" );

    CosNotifyChannelAdmin::ConsumerAdmin_var consumerAdmin;
    try {
        consumerAdmin = naming_.resolveName<
            CosNotifyChannelAdmin::ConsumerAdmin>( adminName );
    } catch ( const CosNaming::NamingContext::NotFound & ) {
        consumerAdmin = createNotifyConsumerAdmin( channelName );
    }
  
    return consumerAdmin._retn();
} 

CosNotifyChannelAdmin::StructuredProxyPushConsumer_var
Client::Notify::getNotifyProxyPushConsumer( const string & channelName,
                                            const string & proxyName )
{
    // Proxy references are cached locally in order to speed up the sending
    // of notifications. If this wasn't the case, each notification would 
    // require at least one call to the nameserver.  So we first check for a 
    // cache hit then check the nameserver. If all else fails, create a new one.
    const string proxyFullName( channelName + "Channel.supplierAdmin." + 
                                proxyName );
    
    const ScopedPthreadMutexLock lock( proxyConsumersGuard_ );

    const ProxyPushConsumerMap::const_iterator iProxyConsumer =
        proxyConsumers_.find( proxyFullName );

    if ( iProxyConsumer != proxyConsumers_.end() )
        return iProxyConsumer->second; // Sweet - cache hit.

    // No cache hit - check if we have a proxy in the nameserv
    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var proxyConsumer; 
    try {  
        proxyConsumer = naming_.resolveName< 
            CosNotifyChannelAdmin::StructuredProxyPushConsumer >
                ( proxyFullName );
    } catch ( const CosNaming::NamingContext::NotFound & ) {
        proxyConsumer = createNotifyProxyPushConsumer( channelName, proxyName );
    }
    
    // We do, cache it locally 
    const pair< ProxyPushConsumerMap::const_iterator, bool > insertResult = 
        proxyConsumers_.insert( make_pair( proxyFullName, proxyConsumer ) );

    if ( !insertResult.second ) 
        throw CARMA_EXCEPTION( ErrorException, "Unable to insert proxy consumer"
            "for " + proxyFullName + " into proxyConsumers_ map." );

    return insertResult.first->second;

} 

CosNotifyChannelAdmin::StructuredProxyPushSupplier_var
Client::Notify::getNotifyProxyPushSupplier( const string & channelName,
                                            const string & proxyName )
try {
    // Unlike proxy push consumers, we do not store suppliers on the nameserver.
    // This insures that only one consumer exists for each process, even if they
    // have the same channel and proxy name.  Unfortunately, these are leaked 
    // on the notification server (see detail::~StructurePushConsumerImplT).
    const string proxyFullName( channelName + "Channel.consumerAdmin." + 
                                proxyName );
    
    verifyChannelName( proxyFullName );

    ScopedPthreadMutexLock lock( proxySuppliersGuard_ );

    const ProxyPushSupplierMap::const_iterator iProxySupplier =
        proxySuppliers_.find( proxyFullName );

    if ( iProxySupplier != proxySuppliers_.end() )
        return iProxySupplier->second; // cache hit

    // proxy supplier wasn't cached - create a new one then cache it.
    CosNotifyChannelAdmin::StructuredProxyPushSupplier_var proxySupplier =
        createNotifyProxyPushSupplier( channelName, proxyName );

    pair< ProxyPushSupplierMap::const_iterator, bool > insertResult = 
        proxySuppliers_.insert( make_pair( proxyFullName, proxySupplier ) );

    if ( !insertResult.second )
        throw CARMA_EXCEPTION( ErrorException, "Unable to insert proxySupplier."
            "for " + proxyFullName + " into proxySuppliers_ map." );

    return insertResult.first->second;

} catch (...) {
    rethrowNotifyExceptionAsCarmaException( );
    return CosNotifyChannelAdmin::StructuredProxyPushSupplier::_nil();
}

CosNotifyChannelAdmin::EventChannel_var
Client::Notify::createNotifyChannel( const string & channelName )
{
    CosNotification::QoSProperties initialQoS;
    CosNotification::AdminProperties initialAdmin; // Not used
    CosNotifyChannelAdmin::ChannelID channelId;  // Not used

    initialQoS.length(6);
    // - connection reliability is necessary for use with IMR
    initialQoS[0].name = CORBA::string_dup("ConnectionReliability");
    initialQoS[0].value <<= CosNotification::Persistent; 
    // - delivery policy - get rid of the oldest ones first
    initialQoS[1].name = CORBA::string_dup("OrderPolicy");
    initialQoS[1].value <<= CosNotification::FifoOrder;
    // - set number of times a failed event will be resent
    // note: this is only for use with a push consumer ... will
    // not work with pull consumer.	 if a notification retries 10
    // times without success, it will destroy the proxy (thus
    // cleaning up the memory)
    initialQoS[2].name = CORBA::string_dup("MaxRetries");
    initialQoS[2].value <<= static_cast< CORBA::ULong >( 10 );
    // - set maximum number of events allowed to queue up in a proxy... 
    // expect 200 at the _most_, so use 500 just to be safe
    initialQoS[3].name = CORBA::string_dup("MaxEventsPerConsumer");
    initialQoS[3].value <<= static_cast< CORBA::Long >( 2000 );
    // - discard policy - get rid of the oldest ones first
    //   - only works with a defined MaxEventsPerConsumer policy
    initialQoS[4].name = CORBA::string_dup("DiscardPolicy");
    initialQoS[4].value <<= CosNotification::FifoOrder;
    // - set timeout for events here in the channel (in units of 100ns)
    //   - only works with a defined MaxEventsPerConsumer policy
    initialQoS[5].name = CORBA::string_dup("Timeout");
    initialQoS[5].value <<= static_cast< TimeBase::TimeT >( 10000000 ); // 1s
    
    CosNotifyChannelAdmin::EventChannel_var notifyChannel =
        notifyChannelFactory_->create_channel( initialQoS, 
                                               initialAdmin, 
                                               channelId );

    // Bind to nameserver under both channelName and id number.
    try { 
        ostringstream idName;
        idName << channelName << "Channel.id." << channelId;
        naming_.addObject( channelName, 
                          CORBA::Object::_duplicate( notifyChannel ) );
        naming_.addObject( idName.str(), 
                          CORBA::Object::_duplicate( notifyChannel ) );
    } catch ( const CosNaming::NamingContext::AlreadyBound &ex ) {
        // if it gets here, it's because of a race condition between processes,
        // so destroy the channel and re-resolve. 
        notifyChannel->destroy();
	
	    notifyChannel = 
            naming_.resolveName< CosNotifyChannelAdmin::EventChannel >(
                channelName );
    } 
    
    return notifyChannel;
}
    
CosNotifyChannelAdmin::SupplierAdmin_var
Client::Notify::createNotifySupplierAdmin( const string & channelName )
{
    const string adminName( channelName + "Channel.supplierAdmin.name" );

    CosNotifyChannelAdmin::EventChannel_var notifyChannel = 
        getNotifyChannel(channelName);
    CosNotifyChannelAdmin::InterFilterGroupOperator groupOperator =
        CosNotifyChannelAdmin::OR_OP;
    CosNotifyChannelAdmin::AdminID adminId;

    // need to create new admin to bind with nameserver (since
    // binding the default admin would be bad)
    CosNotifyChannelAdmin::SupplierAdmin_var supplierAdmin = 
        notifyChannel->new_for_suppliers( groupOperator, adminId );

    try {
        naming_.addObject( adminName, 
                           CORBA::Object::_duplicate(supplierAdmin) );
    } catch ( const CosNaming::NamingContext::AlreadyBound & ) {
        // if it gets here, it's because we lost a race with another process 
        // trying to get an admin with the same name, so destroy everything
        supplierAdmin->destroy();

        // at this point, just try to resolve again
        supplierAdmin = naming_.resolveName<
            CosNotifyChannelAdmin::SupplierAdmin>( adminName );
    }

    return supplierAdmin._retn( );
}

CosNotifyChannelAdmin::ConsumerAdmin_var
Client::Notify::createNotifyConsumerAdmin( const string & channelName )
{
    const string adminName( channelName + "Channel.consumerAdmin.name" );

    CosNotifyChannelAdmin::EventChannel_var notifyChannel = 
        getNotifyChannel(channelName);

    CosNotifyChannelAdmin::InterFilterGroupOperator groupOperator =
        CosNotifyChannelAdmin::OR_OP;
    CosNotifyChannelAdmin::AdminID adminId;

    // create new consumer admin for binding to nameserver, rather
    // than binding the default one
    CosNotifyChannelAdmin::ConsumerAdmin_var consumerAdmin = 
        notifyChannel->new_for_consumers( groupOperator, adminId );

    try {
        naming_.addObject(adminName, CORBA::Object::_duplicate(consumerAdmin));
    } catch ( const CosNaming::NamingContext::AlreadyBound & ) {
        // if it gets here, it's because we lost a race with another process
        // trying to create the same consumer admin name so destroy everything
        consumerAdmin->destroy();

        // just try to resolve again
        consumerAdmin = 
            naming_.resolveName< CosNotifyChannelAdmin::ConsumerAdmin >(
                adminName );
    }

    return consumerAdmin._retn( );
}

CosNotifyChannelAdmin::StructuredProxyPushConsumer_var
Client::Notify::createNotifyProxyPushConsumer( const string & channelName,
                                               const string & proxyName )
{
    const string proxyFullName( channelName + "Channel.supplierAdmin." + 
            proxyName );

    CosNotifyChannelAdmin::SupplierAdmin_var supplierAdmin = 
        getNotifySupplierAdmin( channelName );

    CosNotifyChannelAdmin::ProxyID proxyId;
    CosNotifyChannelAdmin::ProxyConsumer_var proxyConsumer = 
        supplierAdmin->obtain_notification_push_consumer(
                CosNotifyChannelAdmin::STRUCTURED_EVENT, proxyId );

    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var
        structuredProxyPushConsumer =
        CosNotifyChannelAdmin::StructuredProxyPushConsumer::_narrow( 
                proxyConsumer );
    

    if ( CORBA::is_nil( structuredProxyPushConsumer ) ) {
        throw CARMA_EXCEPTION( ErrorException, 
                "Client::Notify::createNotifyProxyPushConsumer( ) - Could not "
                "obtain ProxyPushConsumer - reference is nil!" );
    }

    // connect push supplier to proxy push consumer passing nil
    // as argument since we are not interested (i believe) in
    // the following:
    // 1. when it is about to be disconnected
    // 2. when there is a change in the set of events to which
    //    consumers are currently subscribed
    structuredProxyPushConsumer->connect_structured_push_supplier(
            CosNotifyComm::StructuredPushSupplier::_nil() );

    // add proxy to map and nameserv
    naming_.addObject(
        proxyFullName, 
        CORBA::Object::_duplicate( structuredProxyPushConsumer ) );

    return structuredProxyPushConsumer;
} 

CosNotifyChannelAdmin::StructuredProxyPushSupplier_var
Client::Notify::createNotifyProxyPushSupplier( const string & channelName,
                                               const string & proxyName )
{
    const string proxyFullName( channelName + "Channel.consumerAdmin." + 
            proxyName );

    // need Consumer Admin to obtain Proxy Supplier
    CosNotifyChannelAdmin::ConsumerAdmin_var consumerAdmin = 
        getNotifyConsumerAdmin( channelName );

    // obtain Proxy Push Supplier
    CosNotifyChannelAdmin::ProxyID proxyId;
    CosNotifyChannelAdmin::ProxySupplier_var proxySupplier = 
        consumerAdmin->obtain_notification_push_supplier(
                CosNotifyChannelAdmin::STRUCTURED_EVENT, proxyId );

    CosNotifyChannelAdmin::StructuredProxyPushSupplier_var 
        structuredProxyPushSupplier = 
            CosNotifyChannelAdmin::StructuredProxyPushSupplier::_narrow(
                proxySupplier );

    if ( CORBA::is_nil( structuredProxyPushSupplier ) ) {
        throw CARMA_EXCEPTION( ErrorException,
                "createNotifyProxyPullSupplier() - Obtained "
                "ProxyPullSupllier is nil!");
    }

    return structuredProxyPushSupplier;
}

CosNotification::StructuredEvent_var
Client::Notify::createEventForm( const string & typeName,
                                 const string & eventName )
try {
    const string domainName( "CARMA" );

    // create new structured event
    CosNotification::StructuredEvent_var event =
        new CosNotification::StructuredEvent;

    // fill header information
    event->header.fixed_header.event_type.domain_name =
        CORBA::string_dup( domainName.c_str() );
    event->header.fixed_header.event_type.type_name =
        CORBA::string_dup( typeName.c_str() );
    event->header.fixed_header.event_name =
        CORBA::string_dup( eventName.c_str() );

    // apply QoS

    event->header.variable_header.length(1);
    event->header.variable_header[0].name = CORBA::string_dup( "Timeout" );
    event->header.variable_header[0].value <<=
        static_cast< TimeBase::TimeT >( 10000000 ); // discard events after 1s. 

    return event;

} catch (...) {
    rethrowNotifyExceptionAsCarmaException( );
    CosNotification::StructuredEvent_var blankEvent =
        new CosNotification::StructuredEvent;
    return blankEvent;
}
    
void 
Client::Notify::sendNotification( const string & channelName,
                                  const string & proxyName,
                                  CosNotification::StructuredEvent_var event )
try {
    verifyChannelName( channelName );

    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var
        notifyProxyPushConsumer = Client::Notify::getNotifyProxyPushConsumer( 
            channelName, proxyName );

    notifyProxyPushConsumer->push_structured_event( event );

} catch (...) {
    programLogErrorIfPossible( "Unable to send event to channel \"" + 
        channelName + "\", proxy \"" + proxyName + "\"." );
    rethrowNotifyExceptionAsCarmaException( );
}
