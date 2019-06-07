#include "carma/corba/Client.h"

#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"

#include <boost/foreach.hpp>
#include <string>

using namespace carma;
using namespace carma::corba;
using namespace carma::util;
using namespace std;

namespace {

const carma::util::Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE2;

string
sanitizeHierarchicalName( const string & hierarchicalName ) 
{
    string sanitized( hierarchicalName );

    unsigned nullCount = 0;
    string::size_type nullIdx = sanitized.find( '\0' );
    while ( nullIdx != string::npos ) {
        ++nullCount;
        sanitized.erase( nullIdx, 1 );
        nullIdx = sanitized.find( '\0' );
    }
        
    if ( nullCount > 0 ) {
        ostringstream info;
        info << "sanitizeHierarchicalName() - Removed " << nullCount 
            << " null chars from hierarchical name " << sanitized << ".";
        programLogInfoIfPossible( info.str() );
    }

    return sanitized;
}

vector< string >
getHierarchyIds( const string & hierarchicalName ) {
    // ids are separated by '.' e.g. "carma.ovro1.antenna.cryocompressor"
    // has 3 context ids (carma, ovro1, antenna) 
    // and 1 leaf object id (cryocompressor).

    vector< string > ids;

    string::size_type nextIdBeginPos = 0;

    while ( true ) {
        const string::size_type nextIdEndPos =
            hierarchicalName.find( '.', nextIdBeginPos );

        if ( nextIdEndPos == string::npos ) {
            ids.push_back( hierarchicalName.substr( nextIdBeginPos ) );

            break;
        }

        const string::size_type nextIdLength =
            nextIdEndPos - nextIdBeginPos;
        ids.push_back( hierarchicalName.substr( nextIdBeginPos, nextIdLength ) )
            ;

        nextIdBeginPos = nextIdEndPos + 1;
    }

    return ids;
}

CosNaming::Name
createCosNamingName( const string & hierarchicalName )
{
    const string sanitizedHierarchicalName( 
        sanitizeHierarchicalName( hierarchicalName ) );

    CosNaming::Name name;

    const vector<string> ids = getHierarchyIds( sanitizedHierarchicalName );

    if ( ids.empty( ) ) return name;

    BOOST_FOREACH( const string id, ids ) {
        const CORBA::ULong length = name.length( );
        name.length( length + 1 );
        name[ length ].id = CORBA::string_dup( id.c_str( ) );
        name[ length ].kind = CORBA::string_dup( "" );
    }

    return name;
}

CosNaming::Name
trimLeaf( CosNaming::Name & name )
{
    if ( name.length() == 0 ) 
        throw CARMA_EXCEPTION( IllegalArgumentException, 
                               "Input name is empty." );

    const CORBA::ULong leafIdx = name.length() - 1;

    CosNaming::Name leaf;
    leaf.length( 1 );
    leaf[ 0 ].id = name[ leafIdx ].id;
    leaf[ 0 ].kind = CORBA::string_dup( "" );

    name.length( leafIdx );

    return leaf;
}

void
rethrowNamingExceptionAsCarmaException( const std::string & fqn )
{
    throw; // ADB - Don't remove this - dependent exception handlers in Notify
    try {
        throw;
    } catch ( const CosNaming::NamingContext::NotFound & nfe ) {
        throw;  // Logic in NotifyUtils depends on this.
        throw CARMA_EXCEPTION( IllegalArgumentException,
                               "Context in " + fqn + " not found." );
    } catch ( const CosNaming::NamingContext::CannotProceed & cpe ) {
        // Not used in TAO.
        throw CARMA_EXCEPTION( ErrorException,
                               "Cannot resolve " + fqn + "." );
    } catch ( const CosNaming::NamingContext::InvalidName & ine ) {
        throw CARMA_EXCEPTION( IllegalArgumentException,
                               "Invalid name in " + fqn + "." );
    }
}

} // namespace < unnamed >


Client::Naming::Naming( CosNaming::NamingContext_var rootNamingContext ) :
    rootNamingContext_( rootNamingContext )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Client::Naming - Ctor." );
}

CORBA::Object_ptr 
Client::Naming::resolve( const ::std::string & hierarchicalName ) const
try {
    CosNaming::Name name = createCosNamingName( hierarchicalName );

    CORBA::Object_var objectVar;
    if ( hierarchicalName == "" )
        objectVar = CosNaming::NamingContext::_duplicate( rootNamingContext_ );
    else
        objectVar = rootNamingContext_->resolve( name );

    return objectVar._retn( );  // Relinquish ownership of objectVar
} catch (...) {
    rethrowNamingExceptionAsCarmaException( hierarchicalName );
    throw;
}

void
Client::Naming::addObject( const ::std::string & hierarchicalName,
                           CORBA::Object_ptr objectPtr )
try {
    CosNaming::Name name = createCosNamingName( hierarchicalName );

    CosNaming::Name leaf = trimLeaf( name );

    // I might need to do this recursively
    CosNaming::Name nameToBind;
    for ( CORBA::ULong idx = 0; idx < name.length(); ++idx ) {
        nameToBind.length( idx + 1 );
        nameToBind[ idx ].id = name[ idx ].id;
        nameToBind[ idx ].kind = name[ idx ].kind;
        try {
            rootNamingContext_->bind_new_context( nameToBind );
        } catch ( const CosNaming::NamingContext::AlreadyBound & ) {
            // Nothing - it's already bound. 
        }
    }

    CosNaming::NamingContext_var context = getNamingContext( name );

    context->rebind( leaf, objectPtr );
} catch (...) {
    rethrowNamingExceptionAsCarmaException( hierarchicalName );
}

void
Client::Naming::removeObject( const ::std::string & hierarchicalName )
try {
    CosNaming::Name name = createCosNamingName( hierarchicalName );

    CosNaming::Name leaf = trimLeaf( name );

    CosNaming::NamingContext_var context = getNamingContext( name );

    context->unbind( leaf );
} catch (...) {
    rethrowNamingExceptionAsCarmaException( hierarchicalName );
}

void
Client::Naming::removeContext( const ::std::string & hierarchicalName,
                               const bool recurse )
try {
    CosNaming::Name name = createCosNamingName( hierarchicalName );

    destroyContext( name, recurse );
} catch (...) {
    rethrowNamingExceptionAsCarmaException( hierarchicalName );
}

CosNaming::NamingContext_var 
Client::Naming::getNamingContext( const CosNaming::Name & name )
{
    if ( name.length() == 0 ) 
        return rootNamingContext_;
    else 
        return detail::getNarrowedVar< CosNaming::NamingContext >( 
                rootNamingContext_->resolve( name ) );
}

void
Client::Naming::destroyContext( CosNaming::Name & name, const bool recurse )
{
    if ( name.length() == 0 ) 
        return;

    CosNaming::NamingContext_var dieingContext = getNamingContext( name );

    rootNamingContext_->unbind( name );

    dieingContext->destroy( );

    if ( recurse ) {
        name.length( name.length() - 1 );
        destroyContext( name, true );
    }
}
