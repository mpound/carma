#ifndef CARMA_UTIL_RANGEFORMATTING_H
#define CARMA_UTIL_RANGEFORMATTING_H

#include <set>
#include <string>
#include <vector>


namespace carma {
namespace util {


::std::string formatAsRanges(
    const ::std::multiset< char > & vals );
    
::std::string formatAsRanges(
    const ::std::multiset< short > & vals );
    
::std::string formatAsRanges(
    const ::std::multiset< int > & vals );

::std::string formatAsRanges(
    const ::std::multiset< long > & vals );

::std::string formatAsRanges(
    const ::std::multiset< long long > & vals );

::std::string formatAsRanges(
    const ::std::multiset< unsigned char > & vals );

::std::string formatAsRanges(
    const ::std::multiset< unsigned short > & vals );

::std::string formatAsRanges(
    const ::std::multiset< unsigned int > & vals );

::std::string formatAsRanges(
    const ::std::multiset< unsigned long > & vals );

::std::string formatAsRanges(
    const ::std::multiset< unsigned long long > & vals );


::std::string formatAsRanges(
    const ::std::multiset< char > & vals,
    const ::std::string &           prefix,
    const ::std::string &           suffix );

::std::string formatAsRanges(
    const ::std::multiset< short > & vals,
    const ::std::string &            prefix,
    const ::std::string &            suffix );

::std::string formatAsRanges(
    const ::std::multiset< int > & vals,
    const ::std::string &          prefix,
    const ::std::string &          suffix );

::std::string formatAsRanges(
    const ::std::multiset< long > & vals,
    const ::std::string &           prefix,
    const ::std::string &           suffix );

::std::string formatAsRanges(
    const ::std::multiset< long long > & vals,
    const ::std::string &                prefix,
    const ::std::string &                suffix );

::std::string formatAsRanges(
    const ::std::multiset< unsigned char > & vals,
    const ::std::string &                    prefix,
    const ::std::string &                    suffix );

::std::string formatAsRanges(
    const ::std::multiset< unsigned short > & vals,
    const ::std::string &                     prefix,
    const ::std::string &                     suffix );

::std::string formatAsRanges(
    const ::std::multiset< unsigned int > & vals,
    const ::std::string &                   prefix,
    const ::std::string &                   suffix );

::std::string formatAsRanges(
    const ::std::multiset< unsigned long > & vals,
    const ::std::string &                    prefix,
    const ::std::string &                    suffix );

::std::string formatAsRanges(
    const ::std::multiset< unsigned long long > & vals,
    const ::std::string &                         prefix,
    const ::std::string &                         suffix );


::std::string formatAsRanges(
    const ::std::set< char > & vals );

::std::string formatAsRanges(
    const ::std::set< short > & vals );

::std::string formatAsRanges(
    const ::std::set< int > & vals );

::std::string formatAsRanges(
    const ::std::set< long > & vals );

::std::string formatAsRanges(
    const ::std::set< long long > & vals );

::std::string formatAsRanges(
    const ::std::set< unsigned char > & vals );

::std::string formatAsRanges(
    const ::std::set< unsigned short > & vals );

::std::string formatAsRanges(
    const ::std::set< unsigned int > & vals );

::std::string formatAsRanges(
    const ::std::set< unsigned long > & vals );

::std::string formatAsRanges(
    const ::std::set< unsigned long long > & vals );



::std::string formatAsRanges(
    const ::std::set< char > & vals,
    const ::std::string &      prefix,
    const ::std::string &      suffix );

::std::string formatAsRanges(
    const ::std::set< short > & vals,
    const ::std::string &       prefix,
    const ::std::string &       suffix );

::std::string formatAsRanges(
    const ::std::set< int > & vals,
    const ::std::string &     prefix,
    const ::std::string &     suffix );

::std::string formatAsRanges(
    const ::std::set< long > & vals,
    const ::std::string &      prefix,
    const ::std::string &      suffix );

::std::string formatAsRanges(
    const ::std::set< long long > & vals,
    const ::std::string &           prefix,
    const ::std::string &           suffix );

::std::string formatAsRanges(
    const ::std::set< unsigned char > & vals,
    const ::std::string &               prefix,
    const ::std::string &               suffix );

::std::string formatAsRanges(
    const ::std::set< unsigned short > & vals,
    const ::std::string &                prefix,
    const ::std::string &                suffix );

::std::string formatAsRanges(
    const ::std::set< unsigned int > & vals,
    const ::std::string &              prefix,
    const ::std::string &              suffix );

::std::string formatAsRanges(
    const ::std::set< unsigned long > & vals,
    const ::std::string &               prefix,
    const ::std::string &               suffix );

::std::string formatAsRanges(
    const ::std::set< unsigned long long > & vals,
    const ::std::string &                    prefix,
    const ::std::string &                    suffix );


::std::string formatAsRanges(
    const ::std::vector< char > & vals );

::std::string formatAsRanges(
    const ::std::vector< short > & vals );

::std::string formatAsRanges(
    const ::std::vector< int > & vals );

::std::string formatAsRanges(
    const ::std::vector< long > & vals );

::std::string formatAsRanges(
    const ::std::vector< long long > & vals );

::std::string formatAsRanges(
    const ::std::vector< unsigned char > & vals );

::std::string formatAsRanges(
    const ::std::vector< unsigned short > & vals );

::std::string formatAsRanges(
    const ::std::vector< unsigned int > & vals );

::std::string formatAsRanges(
    const ::std::vector< unsigned long > & vals );

::std::string formatAsRanges(
    const ::std::vector< unsigned long long > & vals );


::std::string formatAsRanges(
    const ::std::vector< char > & vals,
    const ::std::string &         prefix,
    const ::std::string &         suffix );

::std::string formatAsRanges(
    const ::std::vector< short > & vals,
    const ::std::string &          prefix,
    const ::std::string &          suffix );

::std::string formatAsRanges(
    const ::std::vector< int > & vals,
    const ::std::string &        prefix,
    const ::std::string &        suffix );

::std::string formatAsRanges(
    const ::std::vector< long > & vals,
    const ::std::string &         prefix,
    const ::std::string &         suffix );

::std::string formatAsRanges(
    const ::std::vector< long long > & vals,
    const ::std::string &              prefix,
    const ::std::string &              suffix );

::std::string formatAsRanges(
    const ::std::vector< unsigned char > & vals,
    const ::std::string &                  prefix,
    const ::std::string &                  suffix );

::std::string formatAsRanges(
    const ::std::vector< unsigned short > & vals,
    const ::std::string &                   prefix,
    const ::std::string &                   suffix );

::std::string formatAsRanges(
    const ::std::vector< unsigned int > & vals,
    const ::std::string &                 prefix,
    const ::std::string &                 suffix );

::std::string formatAsRanges(
    const ::std::vector< unsigned long > & vals,
    const ::std::string &                  prefix,
    const ::std::string &                  suffix );

::std::string formatAsRanges(
    const ::std::vector< unsigned long long > & vals,
    const ::std::string &                       prefix,
    const ::std::string &                       suffix );


}  // namespace carma::util
}  // namespace carma


#endif
