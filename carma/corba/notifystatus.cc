#include "carma/corba/Client.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"

#include <boost/foreach.hpp>
#include <iostream>
#include <map>
#include <orbsvcs/CosNamingC.h>

using namespace boost;
using namespace carma::corba;
using namespace carma::util;
using namespace std;

namespace {

struct Context {
    string name;  // Fully qualified (e.g. carma.ovro1)
    CosNaming::NamingContext_var nameContextVar;
    vector< string > objects;
    vector< Context * > contexts;

    void print( const string & prefix = "", const bool condense = false ) const;

};

void
Context::print( const string & prefix, const bool condense ) const 
{
    string cprefix( prefix );
    if ( cprefix.rbegin() != cprefix.rend() ) 
        (*cprefix.rbegin()) = '-';
    cout << cprefix << "Context " << ( name == "" ? "(Root)" : name ) << flush;
    
    if ( !contexts.empty() ) 
        cout << " " << contexts.size() << " subcontexts" << flush;
    else if ( contexts.size() == 1 )
        cout << " " << "1 subcontext" << flush;

    if ( objects.empty() ) 
        cout << "." << endl;
    else {
        cout << ", " << flush;
        if ( objects.size() == 1 ) 
            cout << "1 object" << flush;
        else 
            cout << "" << objects.size() << " objects" << flush;

        if ( condense )
            cout << " (not shown)." << endl;
        else 
            cout << "." << endl;
    }

    BOOST_FOREACH ( const Context * context, contexts ) {
        context->print( prefix + "| ", condense );
    }

    if ( !condense ) {
        BOOST_FOREACH( const string & object, objects ) {
            cout << prefix << "|-> Object " << object << endl; 
        }
    }
    cout << prefix << "`--- End " << name << " ---" << endl;
}
    
// Use a map so that contexts are somewhat (lexicographically) sorted.
typedef map< string, const Context * > NamedContextMap;
map< string, const Context * >
findNotificationContexts( const Context * root )
{
    // CARMAs use of the notification service uses a naming context scheme
    // which I exploit here.  A harder but more fool proof way to do this 
    // would be to search through all objects and find the notification objects
    // by narrowing. 
    const string channelStr( "Channel" ); // Context ends in 'Channel'.
    map< string, const Context * > found;
    BOOST_FOREACH( const Context * context, root->contexts ) {
        const string::size_type channelIdx = context->name.find( 
            channelStr, context->name.size() - channelStr.size() );
        if ( channelIdx != string::npos ) 
        {
            found.insert( make_pair( context->name, context ) );
        } else {
            map< string, const Context * > tmp = 
                findNotificationContexts( context );
            BOOST_FOREACH( NamedContextMap::value_type & v, tmp ) {
                found.insert( v );
            }
        }
    }
    return found;
}

void
processNotificationContext( const Context * context )
{
    // Also exploits CARMA notification service naming scheme 
    const string channel("Channel");
    const string::size_type channelIdx = context->name.find( channel );
    if ( channelIdx == string::npos )
        throw CARMA_ERROR( "Context " + context->name + " is not a "
            "notification context." );
    const string channelName( context->name.substr( 0, channelIdx ) );

    // Loop through subcontexts and extract info
    vector< string > ids; // Should only be 1 but track if more
    vector< string > consumers; 
    vector< string > suppliers; 
    BOOST_FOREACH( const Context * c, context->contexts ) {

        if ( c->name == context->name + ".id" ) {
            ids.insert( ids.begin(), c->objects.begin(), c->objects.end() );
        } else if ( c->name == context->name + ".consumerAdmin" ) {
            consumers.insert( consumers.begin(), c->objects.begin(), 
                              c->objects.end() );
        } else if ( c->name == context->name + ".supplierAdmin" ) {
            suppliers.insert( suppliers.begin(), c->objects.begin(),
                              c->objects.end() );
        }
    }

    cout << "Channel " << channelName << " (ID " << flush;
    const vector< string >::size_type numIds = ids.size();
    if ( numIds <= 0 ) 
        cout << "NONE!)" << flush;
    else if ( numIds > 1 ) 
        cout << "<error>)" << flush;
    else 
        cout << ids[0] << ")" << flush;

    cout << " contains " << flush;
    if ( consumers.size() == 1 ) 
        cout << "1 consumer" << flush;
    else 
        cout << consumers.size() << " consumers" << flush;
    
    cout << " and " << flush;

    if ( suppliers.size() == 1 ) 
        cout << "1 supplier." << endl;
    else
        cout << suppliers.size() << " suppliers." << endl;
}

// Derived from show_chunk and list_context H&V pg 806
Context *
processContext( const string fqname, Client & client );

void
addComponents( Context * context,
               const CosNaming::BindingList & bl, 
               Client & client )
{
    for ( CORBA::ULong i = 0; i < bl.length(); ++i ) {

        const string leafName( bl[i].binding_name[0].id );
        const string fullName( context->name == "" ? leafName :
                               context->name + '.' + leafName );

        if ( bl[i].binding_type == CosNaming::ncontext ) {
            context->contexts.push_back( processContext( fullName, client ) );
        } else {
            context->objects.push_back( leafName );
        }
    }
}

Context *
processContext( const string fqname, Client & client ) 
{
    CosNaming::NamingContext_var nc = 
        client.resolveName< CosNaming::NamingContext >( fqname );

    Context * context = new Context;
    context->name = fqname;
    context->nameContextVar = nc;

    CosNaming::BindingIterator_var it;
    CosNaming::BindingList_var bl;
    const CORBA::ULong CHUNK =- 100;

    nc->list( CHUNK, bl, it );
    addComponents( context, bl, client );

    if ( !CORBA::is_nil( it ) ) {
        while ( it->next_n( CHUNK, bl ) )
            addComponents( context, bl, client );
        it->destroy( );
    }

    return context;
}

}  // namespace < unnamed >

// @version $Revision: 1.3 $ $Date: 2013/01/28 23:28:39 $ 
//
// @usage notifystatus imr=<imr hostname> [details=t|f] [connect=<channel>]
//
// @logger DEFAULT_FACILITY carma.corba.notifystatus
// 
// @key details false bool Print details about notification channel contexts.
// @key allnames false bool Print details about all nameservice contexts.
//
// @description Search nameserver for notification channels and output status.
//
int Program::main( )
try {
    const bool printDetails( getBoolParameter( "details" ) );
    const bool printAllNames( getBoolParameter( "allnames" ) );

    Client & client = getCorbaClient();

    // Retrieve root naming context and it's contents.
    Context * root = processContext( "", client );

    if ( printAllNames ) 
        root->print();

    map< string, const Context * > notificationContexts = 
        findNotificationContexts( root );

    cout << "Found " << notificationContexts.size() << " notification channels."
        << endl;
    BOOST_FOREACH( NamedContextMap::value_type & v, notificationContexts ) {
        processNotificationContext( v.second );
        if ( printDetails ) {
            v.second->print();
            cout << endl;
        }
    }

    return 0;
} catch (...) {
    cerr << getStringForCaught() << endl;
    return 1;
}
