#include "carma/util/Trace.h"

#include <orbsvcs/CosNotifyCommS.h>
#include <string>
#include <tao/PortableServer/PortableServer.h>
#include <utility>

#define TRACE_DO_LIFECYCLE carma::util::Trace::TRACE3
#define TRACE_EVENTS carma::util::Trace::TRACE7
#define TRACE_REF_COUNTS carma::util::Trace::TRACE5

namespace carma {
namespace corba {
namespace detail {

template< typename T, typename S > // T stands for Tie, S stands for Servant.
class TiedRefCountServant : 
    public virtual T, 
    public virtual PortableServer::RefCountServantBase 
{
public:

    TiedRefCountServant( S & servant,
                         PortableServer::POA_ptr poa );

    virtual ~TiedRefCountServant( );

    typedef typename T::_stub_var_type var_type;

};

}}} // namespace carma::corba::detail

template< typename T, typename S >
carma::corba::detail::TiedRefCountServant<T,S>::TiedRefCountServant( 
        S & servant,
        PortableServer::POA_ptr poa ) :
    T( servant, poa )
{
    using namespace carma::util;
    using namespace std;

    CARMA_CPTRACE( TRACE_DO_LIFECYCLE, "Creating TiedRefCountServant" );
}

template< typename T, typename S >
carma::corba::detail::TiedRefCountServant<T,S>::~TiedRefCountServant( )
{
    using namespace carma::util;
    using namespace std;
    CARMA_CPTRACE( TRACE_DO_LIFECYCLE, "Destroying TiedRefCountServant" );
}

struct carma::corba::Server::PrivateInline 
{
    PrivateInline( carma::corba::Server::PrivateImplementation & pimpl );
    
    PortableServer::POA_ptr poa();
    void activate( PortableServer::Servant servant ); 
    void publish( const std::string & name, CORBA::Object_ptr objectPtr );
    void connectConsumerToProxySupplier( 
        const std::string & channelName,
        const std::string & proxyName,
        CosNotifyComm::StructuredPushConsumer_var spc ); 

    PrivateImplementation & pimpl_;
};


template< template< typename > class T , typename S >
void 
carma::corba::Server::addServant( 
    S & servant, 
    typename T< S >::_stub_ptr_type & corbaClientPtr ) 
{
    using namespace carma::corba;
    using namespace carma::util;
    using namespace std;

    CARMA_CPTRACE( TRACE_DO_LIFECYCLE, "Creating corbaServant" );

    // Create a tie servant using the passed in servant.  
    // This gives us complete decoupling from user defined implementation
    // class and the Corba object.  
    detail::TiedRefCountServant<T<S>, S> * corbaServant = 
        new detail::TiedRefCountServant<T<S>, S>( servant, privateInl_->poa() );

    CARMA_CPTRACE( TRACE_REF_COUNTS,
                   __PRETTY_FUNCTION__ << " initial refCount is " 
                   << corbaServant->_refcount_value() );

    privateInl_->activate( corbaServant );

    CARMA_CPTRACE( TRACE_REF_COUNTS, __PRETTY_FUNCTION__ 
                   << " refcount is now " << corbaServant->_refcount_value() );

    // Since the servant is explicitly activated above, _this() does NOT 
    // increase the reference count.
    corbaClientPtr = corbaServant->_this();
    
    CARMA_CPTRACE( TRACE_REF_COUNTS, __PRETTY_FUNCTION__ 
                   << " client refcount is " 
                   << corbaClientPtr->_refcount_value() );

    // Call remove_ref on corba object leaving memory management to the CORBA 
    // runtime.  When the server shuts down, it will delete the object.
    // Note that it does NOT delete or otherwise manage the passed 
    // servant (S) - it's owned and managed by the caller.  We only manage
    // the TiedRefCountServant which incarnates this servant. This 
    // trick COMPLETELY decouples the user's servant from the incarnated 
    // corba servant, a distinction which CORBA highlights but is rarely done 
    // in C++ due mostly to the terrible inheritance based mappings.
    corbaServant->_remove_ref();
    
    CARMA_CPTRACE( TRACE_REF_COUNTS,
                   __PRETTY_FUNCTION__ << " final refcount is now " 
                   << corbaServant->_refcount_value() );
}

template< template< typename > class T, typename S >
void 
carma::corba::Server::addServant( S & servant, 
                                  const ::std::string & nameserverName )
{
    using namespace carma::util;
    using namespace std;
    
    typename T<S>::_stub_ptr_type corbaClientPtr;

    addServant<T, S>( servant, corbaClientPtr );

    CARMA_CPTRACE( TRACE_DO_LIFECYCLE, __PRETTY_FUNCTION__ 
                   << " publishing servant as '" << nameserverName << "'." );
    
    privateInl_->publish( nameserverName, corbaClientPtr );
}


namespace carma {
namespace corba { 
namespace detail {

template< typename S, typename N > // S servant, N notification type
class StructuredPushConsumerImplT : 
    public virtual PortableServer::RefCountServantBase,
    public virtual POA_CosNotifyComm::StructuredPushConsumer {
private:

    const std::string channelName_;
    const std::string proxyName_;

    S & servantFunctor_;

    PortableServer::POA_ptr poa_;

    unsigned int erroneousNotifications_;

public:

    StructuredPushConsumerImplT( const std::string & channelName,
                                 const std::string & proxyName,
                                 S & servantFunctor,
                                 PortableServer::POA_ptr poa ) :
        channelName_( channelName ),
        proxyName_( proxyName ),
        servantFunctor_( servantFunctor ),
        poa_( poa ),
        erroneousNotifications_( 0 ) 
        {
        };

    ~StructuredPushConsumerImplT( ) 
    { 
        CARMA_CPTRACE( TRACE_DO_LIFECYCLE, 
                       "~StructuredPushcConsumerImplT - D'tor." );

        // Ideally we would call disconnect_structured_push_supplier here to 
        // tell the notification server we are done with the proxy and to 
        // force it to cleanup resources associated with it.  In practice this
        // is problematic as most of the time the requisite runtime is either
        // too far shutdown or otherwise doesn't exist in a state which allows
        // remote invocation.  I do not know of an easy way to solve this 
        // problem and guarantee that the notification service cleans up 
        // proxies. Anybody looking at this should be aware that proxy suppliers
        // are leaked on the notification server.
    };

    // Implements the CosNotifyComm.idl interface.
    void push_structured_event( const CosNotification::StructuredEvent & e );

    // Called when somebody externally disconnects our push consumer. 
    // We currently have no reason to do this.
    void disconnect_structured_push_consumer( ) { };

    // Called when their is a change to the kind of events the channel receives.
    // We do not use this functionality but it's still part of the interface.
    void offer_change( const CosNotification::EventTypeSeq & added,
                       const CosNotification::EventTypeSeq & removed ) { };

    PortableServer::POA_ptr _default_POA( )
    {
        return PortableServer::POA::_duplicate( poa_ );
    };

}; // StructuredPushConsumerImplT

}}} // Namespace carma::corba::detail

template< typename S, typename N >
void
carma::corba::detail::StructuredPushConsumerImplT<S, N>::push_structured_event( 
    const CosNotification::StructuredEvent & event )
try {

    CARMA_CPTRACE( TRACE_EVENTS, "StructuredPushConsumerImplT<S, N>::"
        "push_structured_event - Event received on channel "
        << channelName_ << " proxy " << proxyName_ << "." );

    const N * N_ptr = 0;

    if ( !( event.filterable_data[0].value >>= N_ptr ) ) {
        ++erroneousNotifications_;
        return;
    }

    if ( N_ptr == 0 ) {
        ++erroneousNotifications_;
        return;
    }

    servantFunctor_( *N_ptr ); 
        
} catch (...) {

}

template< typename S, typename N >
void 
carma::corba::Server::addNotificationServantFunctor( 
    S & servant, 
    const ::std::string & channelName,
    const ::std::string & proxyName )
{
    using namespace carma::corba;
    using namespace carma::util;
    using namespace std;
    
    CARMA_CPTRACE( TRACE_DO_LIFECYCLE, "Creating notification servant" );

    detail::StructuredPushConsumerImplT< S, N > * notificationServant =
        new detail::StructuredPushConsumerImplT< S, N >( channelName,
                                                         proxyName,
                                                         servant,
                                                         privateInl_->poa() );

    CARMA_CPTRACE( TRACE_REF_COUNTS,
                   __PRETTY_FUNCTION__ << " initial refCount is " 
                   << notificationServant->_refcount_value() );

    privateInl_->activate( notificationServant );

    CARMA_CPTRACE( TRACE_REF_COUNTS, 
                   __PRETTY_FUNCTION__ << " refcount is now " 
                   << notificationServant->_refcount_value() );

    privateInl_->connectConsumerToProxySupplier( channelName,
                                                 proxyName, 
                                                 notificationServant->_this() );

    // Call remove_ref on corba object leaving memory management to the CORBA 
    // runtime.  When the server shuts down, it will delete the object.
    // Note that it does NOT delete or otherwise manage the passed 
    // servant (S) - it's owned and managed by the caller.  We only manage
    // the StructuredPushConsumerImplT which incarnates this servant. This 
    // trick COMPLETELY decouples the user's servant from the incarnated 
    // corba servant, a distinction which CORBA highlights but is rarely done 
    // in C++ due mostly to the terrible inheritance based mappings.
    notificationServant->_remove_ref();
    
    CARMA_CPTRACE( TRACE_REF_COUNTS,
                   __PRETTY_FUNCTION__ << " final refcount is now " 
                   << notificationServant->_refcount_value() );
}
