#ifndef CARMA_CORBA_CLIENT_H
#define CARMA_CORBA_CLIENT_H

#include <memory>
#include <string>

namespace CORBA {

class ORB;
typedef ORB * ORB_ptr;

} // namespace CORBA

namespace carma {
namespace corba {

class Server;

/**
 * Class to encapsulate CORBA client functionality in CARMA.
 *
 * For examples see the Test directory.
 *
 * Author: Andrew Beard
 */
class Client {
public:
    
    /** 
     * Initialize CORBA client runtime.  
     * @param argc Command line arguments to initialize ORB from.
     * @param argv Command line arguments to initialized ORB from.
     * @param rrtt Relative round-trip timeout for commands in seconds. 
     * @see carma::util::Program::getCorbaClient()
     */
    Client( int argc, char * argv[], int rrtt = 60 );

    /**
     * Destructor
     * Note: doesn't cleanup remote resources allocated with this class. This
     * is NOT a shortcoming of this class, but a fundamental limitation of the
     * ownership concept in distributed environments in general.
     */
    ~Client( );

    /**
     * Resolve specified name to a CORBA object of the templatized output type.
     * Nameserver names have the format "context.name" where a context contains 
     * multiple contexts and/or objects.
     * @param name Hierarchical nameserver name OR initial service name.
     * @param initialRef True if name is an initial service name.
     * @return Templatized CORBA var type from generated proxy class.
     */
    template< typename T >
    typename T::_var_type 
    resolveName( const ::std::string & name, bool initialRef = false );
    
    /**
     * Send a notification of type N to specified notification server channel.
     * @param channelName Name of notification channel.
     * @param proxyName Name of notification proxy connection.
     * @param notification IDL defined structure of notification.
     */
    template< typename N >
    bool sendNotification( const ::std::string & channelName,
                           const ::std::string & proxyName,
                           const N & notification );
                           
    /**
     * Apply client timeout policies.
     * This is only exposed here so that we apply the same timeout policies
     * to util::CorbaUtils and util::Orb (both call this). 
     * @param rrtt Relative round-trip timeout for commands. 
     */ 
    static void applyTimeoutPolicies( CORBA::ORB_ptr orb, int rrtt = 60 );

private:

    friend class Server;
    Client( CORBA::ORB_ptr orb, int rrtt );

    // No copy
    Client( const Client & );
    Client & operator=( const Client & );
    
    // Aggressively hide everything
    struct Private; 
    mutable ::std::auto_ptr< Private > private_;

    class PrivateInline; // Visible to Inline
    mutable ::std::auto_ptr< PrivateInline > privateInline_;

    class Naming;
    mutable ::std::auto_ptr< Naming > naming_;

    class Notify;
    mutable ::std::auto_ptr< Notify > notify_;

}; // class Client

}} // namespace carma::corba
    
// Hide the templatized portion of the implementation
#include "carma/corba/Client.inl"

#endif
