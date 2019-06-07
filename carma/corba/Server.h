#ifndef CARMA_CORBA_SERVER_H
#define CARMA_CORBA_SERVER_H

#include "carma/util/Program.h"

#include <memory>
#include <string>

namespace carma {
namespace corba {

    class Client;

    /** 
     * Class for creating, managing and serving requests to CORBA servants.
     *
     * This class encapsulates all CORBA server functionality in CARMA. 
     * For examples ranging from trivial to complex, see the Test directory.
     *
     * Author: Andrew Beard
     */
    class Server {
    public:

        /** 
         * Initialize CORBA server runtime.  
         * @param argc Command line arguments to initialize ORB from.
         * @param argv Command line arguments to initialized ORB from.
         * @param rrtt Relative round-trip timeout for commands. 
         * @see carma::util::Program::getCorbaServer()
         */
        explicit Server( int argc, char * argv[], int rrtt = 60 );

        /** 
         * Destructor
         */
        virtual ~Server( );

        /**
         * Add servant S, incarnate with IDL generated tie T and publish. 
         * @param servant Instance defining how to process requests.
         * @param nameserverName Published name in "context.object" form. 
         */
        template< template< typename > class T, typename S >
        void addServant( S & servant,
                         const ::std::string & nameserverName );
        
        /**
         * Add servant S, incarnate with generated tie T and set client pointer.
         * @param servant Instance defining how to process requests.
         * @param corbaClientPtr Client pointer to incarnated CORBA servant. 
         */
        template< template< typename > class T , typename S >
        void addServant( S & servant, 
                         typename T< S >::_stub_ptr_type & corbaClientPtr );

        /**
         * Add servant to process IDL defined notifications on given channel.
         * Servant must define functor void S::operator()( N & notification ).
         * Notifications are IDL defined structures. Multiple consumers on the 
         * same channel must have distinct proxy names.
         * @param servant Servant functor to process notifications of type N.
         * @param channeName Name of notification channel.
         * @param proxyName Notification channel proxy name.
         */
        template< typename S, typename N >
        void addNotificationServantFunctor( S & servant, 
                                            const ::std::string & channelName,
                                            const ::std::string & proxyName );

        /**
         * Service remote requests if pending (non blocking).
         */
        void work( );

        /**
         * Service remote requests continuously (blocks).
         * @param inSeparateThread Run in newly spawned separate thread.
         */
        void run( bool inSeparateThread = false );

        /**
         * Stop serving remote requests permanently.
         */
        void stop( );
        
        /**
         * Check if server has terminated, including via the IMR.
         * @post Servant instances no longer used and can be safely destructed. 
         */
        bool terminated( ) const; 

    private:

        friend Client & carma::util::ProgramBase::getCorbaClient();
        Client & client(); 

        // No copy
        Server( const Server & );
        Server & operator=( const Server & );

        // Aggressively hide everything
        struct PrivateImplementation; // Visible to .cc implementation only.
        std::auto_ptr< PrivateImplementation > privateImpl_;
        
        struct PrivateInline; // Visible to .inl templatized implementation 
        std::auto_ptr< PrivateInline > privateInl_; 
    };

}} // namespace carma::corba

// Include the implementation in a separate file to avoid clutter. 
#include "carma/corba/Server.inl"

#endif
