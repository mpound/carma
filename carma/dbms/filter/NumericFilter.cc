#include "carma/dbms/filter/NumericFilter.h"

#include <string>


namespace {


void
verifyItWorks( ) {
    using namespace carma::dbms;
    
    const NumericFilter< char > cNumFil( 1, NumericFilter< char >::GREATER_THAN );
    const NumericFilter< unsigned char > ucNumFil( 1, NumericFilter< unsigned char >::GREATER_THAN );

    const NumericFilter< short > sNumFil( 1, NumericFilter< short >::GREATER_THAN );
    const NumericFilter< unsigned short > usNumFil( 1, NumericFilter< unsigned short >::GREATER_THAN );

    const NumericFilter< int > iNumFil( 1, NumericFilter< int >::GREATER_THAN );
    const NumericFilter< unsigned int > uiNumFil( 1, NumericFilter< unsigned int >::GREATER_THAN );

    const NumericFilter< long > lNumFil( 1, NumericFilter< long >::GREATER_THAN );
    const NumericFilter< unsigned long > ulNumFil( 1, NumericFilter< unsigned long >::GREATER_THAN );

    const NumericFilter< long long > llNumFil( 1, NumericFilter< long long >::GREATER_THAN );
    const NumericFilter< unsigned long long > ullNumFil( 1, NumericFilter< unsigned long long >::GREATER_THAN );

    const NumericFilter< float > fNumFil( 1, NumericFilter< float >::GREATER_THAN );
    
    const NumericFilter< double > dNumFil( 1, NumericFilter< double >::GREATER_THAN );

    const NumericFilter< long double > ldNumFil( 1, NumericFilter< long double >::GREATER_THAN );

    // const NumericFilter< char * > cpNumFil( 0, NumericFilter< char * >::GREATER_THAN );

    // const NumericFilter< ::std::string > ssNumFil( 0, NumericFilter< ::std::string >::GREATER_THAN );

    // struct foozball;
    // const NumericFilter< foozball > fbNumFil( 0, NumericFilter< foozball >::GREATER_THAN );
}


}  // namespace < anonymous >
