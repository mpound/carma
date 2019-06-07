#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/PthreadMutex.h"

#include <iostream>
#include <map>
#include <orbsvcs/CosNamingC.h>
#include <orbsvcs/CosNotifyChannelAdminC.h>
#include <orbsvcs/NotifyExtC.h>
#include <sstream>

namespace carma {
namespace corba {
namespace detail { // Used to hide private templatized functions.

    template< typename T, typename S >
    typename T::_var_type getNarrowedVar( const S & s );

    // The main reason for this is to avoid polluting this header with corba.h.
    // While any user will generally still get headers fromt the IDL generated 
    // proxy inclusion - this will be a subset of the world pulled in by 
    // corba.h.  This in turn further limits visibility and will speed up 
    // compile times as there are fewer header files being parsed repetitively 
    // by everybody who includes Client.h.
    bool isNil( CORBA::Object_ptr objPtr );

} } } // namespace carma::corba::detail

// The seemingly excessive inlining and privatization is meant to not only hide
// as much CORBA from the user as possible, but to also minimize exosure
// to TAO/CORBA headers and data structures which quickly get out of control.
class carma::corba::Client::PrivateInline {
public:

    PrivateInline( carma::corba::Client::Private & privInst );
    
    template< typename T >
    typename T::_var_type resolveInitRef( const ::std::string & name );

private:

    CORBA::Object_ptr resolveInit( const ::std::string & name );

    carma::corba::Client::Private & private_;

}; // struct Client::Private

/**
 * Class for retrieving, binding and deleting contexts and objects to the
 * CORBA Name Service.
 *
 * Note that this class contains no state information regarding objects added
 * or existing on the CORBA Name Service.  It merely communicates with the 
 * Name Service.  As such, if you want to remove your name from the service
 * on shutdown, you'll need to call removeObject explicitly.  Common CARMA 
 * usage orphans the references, allowing a client to retrieve one and
 * then causing  the IMR to restart the server, so be careful about removal.
 */
class carma::corba::Client::Naming {
public:

    /**
     * Constructor.
     */
    Naming( CosNaming::NamingContext_var rootNamingContext );

    /**
     * Resolve a CORBA object from a hierarchical name on the nameserver.
     * @param hierarchicalName Name which object is published.
     */
    template< typename T >
    typename T::_var_type 
    resolveName( const ::std::string & hierarchicalName ) const;

    /**
     * Add a CORBA object to the Name Service.
     * @param hierarchicalName Name under which object is to be published.
     * @param objectPtr CORBA Object to be bound to hierarchicalName. 
     */
    void addObject( const ::std::string & hierarchicalName,
                    CORBA::Object_ptr objectPtr );

    /**
     * Remove object from the Name Service.
     * @param hierarchicalName Name of object to be removed.
     */
    void removeObject( const ::std::string & hierarchicalName );

    /**
     * Remove context from the Name Service.
     * Context must be empty or an exception will be thrown.
     * @param hierarchicalName Name of context to be removed.
     * @param recurse Recursively destroy the whole tree (still must be empty).
     */
    void removeContext( const ::std::string & hierarchicalName, bool recurse );

private:

    CORBA::Object_ptr resolve( const ::std::string & hierarchicalName ) const;

    CosNaming::NamingContext_var 
    getNamingContext( const CosNaming::Name & name );

    void
    destroyContext( CosNaming::Name & name, const bool recurse );

    CosNaming::NamingContext_var rootNamingContext_;
}; // class Client::Naming

template< typename T >
typename T::_var_type 
carma::corba::Client::Naming::resolveName( 
    const ::std::string & hierarchicalName ) const
{
    return detail::getNarrowedVar< T >( resolve( hierarchicalName ) );
}


class carma::corba::Client::Notify {
public:

    Notify( carma::corba::Client::Naming & naming,
            CosNotifyChannelAdmin::EventChannelFactory_var channelFactory );

    ~Notify( );
    
    CosNotifyChannelAdmin::StructuredProxyPushSupplier_var
    getNotifyProxyPushSupplier( const std::string & channelName,
                                const std::string & proxyName );

    CosNotification::StructuredEvent_var
    createEventForm( const std::string & typeName,
                     const std::string & eventName );

    void sendNotification( const std::string & channelName,
                           const std::string & proxyName,
                           CosNotification::StructuredEvent_var event );

private: 

    CosNotifyChannelAdmin::EventChannel_var
    getNotifyChannel( const std::string & channelName );

    CosNotifyChannelAdmin::SupplierAdmin_var
    getNotifySupplierAdmin( const std::string & channelName ); 

    CosNotifyChannelAdmin::ConsumerAdmin_var
    getNotifyConsumerAdmin( const std::string & channelName );
    
    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var
    getNotifyProxyPushConsumer( const std::string & channelName,
                                const std::string & proxyName );
    
    CosNotifyChannelAdmin::EventChannel_var
    createNotifyChannel( const std::string & channelName );

    CosNotifyChannelAdmin::SupplierAdmin_var
    createNotifySupplierAdmin( const std::string & channelName );

    CosNotifyChannelAdmin::ConsumerAdmin_var
    createNotifyConsumerAdmin( const std::string & channelName );

    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var
    createNotifyProxyPushConsumer( const std::string & channelName,
                                   const std::string & proxyName );

    CosNotifyChannelAdmin::StructuredProxyPushSupplier_var
    createNotifyProxyPushSupplier( const std::string & channelName,
                                   const std::string & proxyName );

    // Proxy maps are both keyed by fully qualified proxy name.
    typedef
    std::map< std::string, 
              CosNotifyChannelAdmin::StructuredProxyPushConsumer_var >
    ProxyPushConsumerMap;
    
    typedef
    std::map< std::string, 
              CosNotifyChannelAdmin::StructuredProxyPushSupplier_var > 
    ProxyPushSupplierMap;

    ProxyPushConsumerMap proxyConsumers_;
    ProxyPushSupplierMap proxySuppliers_;
    carma::util::PthreadMutex proxyConsumersGuard_;
    carma::util::PthreadMutex proxySuppliersGuard_;
    CosNotifyChannelAdmin::EventChannelFactory_var notifyChannelFactory_;
    carma::corba::Client::Naming & naming_;
}; // class Client::Notify

template< typename T, typename S >
typename T::_var_type
carma::corba::detail::getNarrowedVar( const S & s ) 
{
    typename T::_var_type tVar;

    if ( detail::isNil( s ) )
        throw CARMA_EXCEPTION( carma::util::IllegalArgumentException, 
                               "Input CORBA object is nil!" );

    tVar = T::_narrow( s );

    if ( detail::isNil( s ) ) 
        throw CARMA_EXCEPTION( carma::util::ErrorException,
                               "Narrowing of input CORBA object failed." );

    return tVar._retn();
}

template< typename T >
typename T::_var_type
carma::corba::Client::PrivateInline::resolveInitRef( 
    const ::std::string & name ) 
{
    return detail::getNarrowedVar< T >( resolveInit( name ) );
}

template< typename T >
typename T::_var_type 
carma::corba::Client::resolveName( const ::std::string & name, 
                                   const bool initialRef ) 
{
    if ( initialRef ) {
        return privateInline_->resolveInitRef< T >( name );
    } 

    if ( naming_.get() == 0 )
        throw CARMA_EXCEPTION( carma::util::ErrorException,
                               "NamingService is not available." );

    return naming_->resolveName< T >( name );
}

template< typename N >
bool
carma::corba::Client::sendNotification( const ::std::string & channelName,
                                        const ::std::string & proxyName,
                                        const N & notification )
{
    if ( notify_.get() == 0 ) 
        throw CARMA_EXCEPTION( carma::util::ErrorException,
            "Client::sendNotification() - The notification service is "
            "not available but you are trying to send notifications! "
            "Make sure the notification service is running and/or properly "
            "initialized via command line options." );

    try {
        ::CosNotification::StructuredEvent_var event =
            notify_->createEventForm( typeid( N ).name(), proxyName );

        event->filterable_data.length( 1 );
        event->filterable_data[0].name = CORBA::string_dup( proxyName.c_str() );
        event->filterable_data[0].value <<= notification;
        notify_->sendNotification( channelName, proxyName, event );
    } catch (...) {
        return false;
    }
    return true;
}
