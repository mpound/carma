#include "carma/util/rangeFormatting.h"

#include <sstream>
#include <limits>

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


template < typename V, bool parenthesizeNegatives >
class ValueStreamer {
    public:
        static void streamValue( ostream &      os,
                                 const V &      v,
                                 const string & prefix,
                                 const string & suffix );
};


template < typename V >
class ValueStreamer< V, true > {
    public:
        static void streamValue( ostream &      os,
                                 const V &      v,
                                 const string & prefix,
                                 const string & suffix );
};


template < typename V >
class ValueStreamer< V, false > {
    public:
        static void streamValue( ostream &      os,
                                 const V &      v,
                                 const string & prefix,
                                 const string & suffix );
};


template < typename V >
void ValueStreamer< V, true >::streamValue( ostream &      os,
                                            const V &      v,
                                            const string & prefix,
                                            const string & suffix )
{
    if ( (v < 0) && prefix.empty() )
        os << "(" << v << suffix << ")";
    else
        os << prefix << v << suffix;
}


template < typename V >
void ValueStreamer< V, false >::streamValue( ostream &      os,
                                             const V &      v,
                                             const string & prefix,
                                             const string & suffix )
{
    os << prefix << v << suffix;
}


template < typename V >
void
streamValue( ostream &      os,
             const V &      v,
             const string & prefix,
             const string & suffix )
{
    ValueStreamer< V, numeric_limits< V >::is_signed >::streamValue( os,
                                                                     v,
                                                                     prefix,
                                                                     suffix );
}


template < >
void
streamValue( ostream &      os,
             const char &   c,
             const string & prefix,
             const string & suffix )
{
    os << prefix << c << suffix;
}


template < >
void
streamValue( ostream &             os,
             const unsigned char & uc,
             const string &        prefix,
             const string &        suffix )
{
    os << prefix << uc << suffix;
}


template < typename V >
bool
valuesAreOrderedAdjacent( const V & lhs,
                          const V & rhs )
{
    V lhsNext = lhs;
    ++lhsNext;
    
    return (lhsNext == rhs);
}


template < typename C >
string
formatSortedContainerAsRanges( const C &      sortedValues,
                               const string & prefix,
                               const string & suffix )
{
    ostringstream oss;
    
    bool firstOne = true;
    
    typename C::const_iterator i = sortedValues.begin();
    const typename C::const_iterator iEnd = sortedValues.end();
    
    while ( i != iEnd ) {
        if ( firstOne )
            firstOne = false;
        else
            oss << ",";

        streamValue( oss, *i, prefix, suffix );
        
        // Now try to extend the range as far as possible
        bool haveARange = false;
        typename C::const_iterator lastRangeIter = i;
        
        while ( true ) {
            ++i;
        
            if ( i == iEnd ) {
                // Done with the whole set
                
                if ( haveARange ) {
                    oss << "-";
                    
                    streamValue( oss, *lastRangeIter, prefix, suffix );
                }
                
                break;
            }
        
            if ( valuesAreOrderedAdjacent( *lastRangeIter, *i ) == false ) {
                // Can't extend the range any more

                if ( haveARange ) {
                    oss << "-";
                    
                    streamValue( oss, *lastRangeIter, prefix, suffix );
                }
                
                break;
            }
            
            lastRangeIter = i;
            haveARange = true;
        }
    }
    
    return oss.str();
}


template < typename C >
string
formatUnsortedContainerAsSortedRanges( const C &      values,
                                       const string & prefix,
                                       const string & suffix )
{
    multiset< typename C::value_type > sortedValues;
    {
        typename C::const_iterator i = values.begin();
        const typename C::const_iterator iEnd = values.end();
    
        for ( ; i != iEnd; ++i )
            sortedValues.insert( *i );
    }
    
    return formatSortedContainerAsRanges( sortedValues, prefix, suffix );
}


}  // namespace < anonymous >


string
carma::util::formatAsRanges( const multiset< char > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const multiset< short > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const multiset< int > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const multiset< long > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const multiset< long long > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const multiset< unsigned char > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const multiset< unsigned short > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const multiset< unsigned int > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const multiset< unsigned long > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const multiset< unsigned long long > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const set< char > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const set< short > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const set< int > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const set< long > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const set< long long > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const set< unsigned char > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const set< unsigned short > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const set< unsigned int > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const set< unsigned long > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const set< unsigned long long > & values )
{
    return formatSortedContainerAsRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const vector< char > & values )
{
    return formatUnsortedContainerAsSortedRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const vector< short > & values )
{
    return formatUnsortedContainerAsSortedRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const vector< int > & values )
{
    return formatUnsortedContainerAsSortedRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const vector< long > & values )
{
    return formatUnsortedContainerAsSortedRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const vector< long long > & values )
{
    return formatUnsortedContainerAsSortedRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const vector< unsigned char > & values )
{
    return formatUnsortedContainerAsSortedRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const vector< unsigned short > & values )
{
    return formatUnsortedContainerAsSortedRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const vector< unsigned int > & values )
{
    return formatUnsortedContainerAsSortedRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const vector< unsigned long > & values )
{
    return formatUnsortedContainerAsSortedRanges( values, string(), string() );
}


string
carma::util::formatAsRanges( const vector< unsigned long long > & values )
{
    return formatUnsortedContainerAsSortedRanges( values, string(), string() );
}


// fooz

string
carma::util::formatAsRanges( const multiset< char > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const multiset< short > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const multiset< int > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const multiset< long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const multiset< long long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const multiset< unsigned char > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const multiset< unsigned short > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const multiset< unsigned int > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const multiset< unsigned long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const multiset< unsigned long long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const set< char > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const set< short > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const set< int > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const set< long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const set< long long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const set< unsigned char > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const set< unsigned short > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const set< unsigned int > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const set< unsigned long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const set< unsigned long long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatSortedContainerAsRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const vector< char > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatUnsortedContainerAsSortedRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const vector< short > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatUnsortedContainerAsSortedRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const vector< int > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatUnsortedContainerAsSortedRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const vector< long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatUnsortedContainerAsSortedRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const vector< long long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatUnsortedContainerAsSortedRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const vector< unsigned char > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatUnsortedContainerAsSortedRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const vector< unsigned short > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatUnsortedContainerAsSortedRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const vector< unsigned int > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatUnsortedContainerAsSortedRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const vector< unsigned long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatUnsortedContainerAsSortedRanges( values, prefix, suffix );
}


string
carma::util::formatAsRanges( const vector< unsigned long long > & values,
                             const string &           prefix,
                             const string &           suffix )
{
    return formatUnsortedContainerAsSortedRanges( values, prefix, suffix );
}
