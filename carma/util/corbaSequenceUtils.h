#ifndef CARMA_UTIL_CORBASEQUENCEUTILS_H
#define CARMA_UTIL_CORBASEQUENCEUTILS_H

#include <set>
#include <string>
#include <vector>

#include "carma/corba/corba.h"
#include "carma/util/SeqTypedefs.h"

namespace carma {
namespace util {


template < typename T, typename S >
void assignVectorToSequence( const ::std::vector< T > & tVector,
                             S &                        sequence );

template < typename T, typename S >
void assignSetToSequence( const ::std::set< T > & tSet,
                          S &                     sequence );

template < typename T, typename S >
void assignMultisetToSequence( const ::std::multiset< T > & tMultiset,
                               S &                          sequence );

template < typename S, typename T >
void assignSequenceToVector( const S &            sequence,
                             ::std::vector< T > & tVector );

void assignSequenceToVector( 
    const carma::util::SeqString & sequence,
    ::std::vector< std::string > & tVector );

template < typename S, typename T >
void assignSequenceToSet( const S &            sequence,
                             ::std::set< T > & tSet);

template < typename S, typename T >
S convertVectorToSequence( const ::std::vector< T > & tVector );


template < typename S, typename T >
S convertSetToSequence( const ::std::set< T > & tSet );


template < typename S, typename T >
S convertMultisetToSequence( const ::std::multiset< T > & tMultiset );


template < typename T, typename S >
::std::vector< T > convertSequenceToVector( const S & sequence );

template < typename T, typename S >
::std::set< T > convertSequenceToSet( const S & sequence );

}  // namespace carma::util
}  // namespace carma


// NOTE: Below here is simply implementation

template < typename T, typename S >
void
carma::util::assignVectorToSequence( const ::std::vector< T > & tVector,
                                     S &                        sequence )
{
    const typename ::std::vector< T >::size_type count = tVector.size();

    sequence.length( count );
    
    typename ::std::vector< T >::const_iterator i = tVector.begin();
    const typename ::std::vector< T >::const_iterator iEnd = tVector.end();

    for ( CORBA::Long j = 0; i != iEnd; ++j, ++i )
        sequence[ j ] = *i;
}


template < typename T, typename S >
void
carma::util::assignSetToSequence( const ::std::set< T > & tSet,
                                  S &                     sequence )
{
    const typename ::std::set< T >::size_type count = tSet.size();

    sequence.length( count );
    
    typename ::std::set< T >::const_iterator i = tSet.begin();
    const typename ::std::set< T >::const_iterator iEnd = tSet.end();

    for ( CORBA::Long j = 0; i != iEnd; ++j, ++i )
        sequence[ j ] = *i;
}


template < typename T, typename S >
void
carma::util::assignMultisetToSequence( const ::std::multiset< T > & tMultiset,
                                       S &                          sequence )
{
    const typename ::std::multiset< T >::size_type count = tMultiset.size();

    sequence.length( count );
    
    typename ::std::multiset< T >::const_iterator i = tMultiset.begin();
    const typename ::std::multiset< T >::const_iterator iEnd = tMultiset.end();

    for ( CORBA::Long j = 0; i != iEnd; ++j, ++i )
        sequence[ j ] = *i;
}


template < typename S, typename T >
void
carma::util::assignSequenceToVector( const S &            sequence,
                                     ::std::vector< T > & tVector )
{
    const CORBA::Long count = sequence.length();

    tVector.clear();
    tVector.reserve( count );
    
    for ( CORBA::Long i = 0; i < count; ++i )
        tVector.push_back( sequence[ i ] );
}

template < typename S, typename T >
void
carma::util::assignSequenceToSet( const S & sequence,
                                  ::std::set< T > & tSet )
{
    const CORBA::Long count = sequence.length();

    tSet.clear();
    
    for ( CORBA::Long i = 0; i < count; ++i )
        tSet.insert( sequence[ i ] );
}


template < typename S, typename T >
S
carma::util::convertVectorToSequence( const ::std::vector< T > & tVector )
{
    S result;

    assignVectorToSequence( tVector, result );
    
    return result;
}


template < typename S, typename T >
S
carma::util::convertSetToSequence( const ::std::set< T > & tSet )
{
    S result;

    assignSetToSequence( tSet, result );
    
    return result;
}


template < typename S, typename T >
S
carma::util::convertMultisetToSequence( const ::std::multiset< T > & tMultiset )
{
    S result;

    assignMultisetToSequence( tMultiset, result );
    
    return result;
}


template < typename T, typename S >
::std::vector< T >
carma::util::convertSequenceToVector( const S & sequence )
{
    ::std::vector< T > result;

    assignSequenceToVector( sequence, result );
    
    return result;
}

template < typename T, typename S >
::std::set< T >
carma::util::convertSequenceToSet( const S & sequence )
{
    ::std::set< T > result;

    assignSequenceToSet( sequence, result );
    
    return result;
}

#endif
