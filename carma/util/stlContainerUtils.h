#ifndef CARMA_UTIL_STLCONTAINERUTILS_H
#define CARMA_UTIL_STLCONTAINERUTILS_H

//! @file
//! Interface file for various utlities for use with STL containers.

#include <algorithm>
#include <iterator>
#include <map>
#include <set>


namespace carma {
namespace util {


//! @brief Constant time check that a container has exactly one element
//!
//! Useful because the STL container size() method is only required to be
//! linear time complexity with respect to the number of elements in the
//! container instance
template < typename C >
bool sizeIsExactlyOne( const C & );


//! @brief Constant time check that a container has no more than one element
//!
//! Useful because the STL container size() method is only required to be
//! linear time complexity with respect to the number of elements in the
//! container instance
template < typename C >
bool sizeIsOneOrLess( const C & );


template < typename K, typename V >
::std::multimap< V, K > invertToMultimap( const ::std::map< K, V > & in );


template < typename K, typename V >
::std::multimap< V, K > invertToMultimap( const ::std::multimap< K, V > & in );


//! @brief Return the keys of a map as a set.
//! Keys must be comparable with less-than operator.
template < typename K, typename V >
::std::set< K > keys( const ::std::map< K, V > & in );

//! @brief Return the unique keys of a multimap as a set. 
//! Keys must be comparable with less-than operator.
template < typename K, typename V >
::std::set< K > keys( const ::std::multimap< K, V > & in );

//! Struct which operators on a container key-value pair to retrieve the key.
//@see carma::util::keys()
struct RetrieveKey {
    template< typename T >
    typename T::first_type operator()(T keyValuePair ) const {
	return keyValuePair.first;
    }
};
}  // namespace carma::util
}  // namespace carma


/// Below is implementation

template < typename C >
bool
carma::util::sizeIsExactlyOne( const C & c )
{
    if ( c.empty() )  // constant time
        return false;

    typename C::const_iterator i = c.begin();  // constant time

    ++i;  // constant time

    return (i == c.end());  // constant time
}


template < typename C >
bool
carma::util::sizeIsOneOrLess( const C & c )
{
    if ( c.empty() )  // constant time
        return true;

    typename C::const_iterator i = c.begin();  // constant time

    ++i;  // constant time

    return (i == c.end());  // constant time
}


template < typename K, typename V >
::std::multimap< V, K >
carma::util::invertToMultimap( const ::std::map< K, V > & in )
{
    ::std::multimap< V, K > out;

    typename ::std::map< K, V >::const_iterator i = in.begin();
    const typename  ::std::map< K, V >::const_iterator iEnd = in.end();

    for ( ; i != iEnd; ++i )
        out.insert( ::std::make_pair( i->second, i->first ) );

    return out;
}


template < typename K, typename V >
::std::multimap< V, K >
carma::util::invertToMultimap( const ::std::multimap< K, V > & in )
{
    ::std::multimap< V, K > out;

    typename ::std::multimap< K, V >::const_iterator i = in.begin();
    const typename ::std::multimap< K, V >::const_iterator iEnd = in.end();

    for ( ; i != iEnd; ++i )
        out.insert( ::std::make_pair( i->second, i->first ) );

    return out;
}

template < typename K, typename V >
::std::set< K > carma::util::keys( const ::std::map< K, V > & in ) 
{
    ::std::set< K > out;
    transform( in.begin(), in.end(), 
	       inserter( out, out.begin() ), RetrieveKey() 
	     );
    return out;
}

template < typename K, typename V >
::std::set< K > carma::util::keys( const ::std::multimap< K, V > & in ) 
{
    ::std::set< K > out;
    // this is somewhat inefficient since identical keys are multiply
    // retrieved.  
    transform( in.begin(), in.end(), 
	       inserter( out, out.begin() ), RetrieveKey() 
	     );
    return out;
}


#endif
