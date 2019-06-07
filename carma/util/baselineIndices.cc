#include "carma/util/baselineIndices.h"

#include <algorithm>
#include <sstream>
#include <cmath>

#include "carma/util/combinatorics.h"
#include "carma/util/ErrorException.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


pair< int, int >
computeLittleBigForIndexWithAutos( const int index )
{
    const int bigA =
        static_cast< int >( floor( 0.5 + sqrt( 2.0 * index + 0.25 ) ) );
        
    const int bigAPiece = sumOf1ToN( bigA - 1 );
        
    // Make us robust to sqrt impl errors that compute bigA slightly too big
    if ( bigAPiece > index ) {
        const int big = bigA - 1;
        const int little = index - sumOf1ToN( big - 1 ) + 1;
        
        return make_pair( little, big );
    }
    
    const int littleA = index - bigAPiece + 1;

    // Make us robust to sqrt impl errors that compute bigA slightly too small
    if ( littleA > bigA ) {
        const int big = bigA + 1;
        const int little = index - sumOf1ToN( big - 1 ) + 1;
        
        return make_pair( little, big );
    }
    
    return make_pair( littleA, bigA );
}


int
computeIndexForLittleBigWithAutos( const int little,
                                   const int big )
{
    // Note: that I am relying on the fact that sumOf1ToN(0) returns 0
    //       to get the correct answer for a little-big pair of (1,1)
    return (sumOf1ToN( big - 1 ) + (little - 1));
}


int
computeIndexCountForMaxInputNoWithAutos( const int maxInputNo )
{
    return sumOf1ToN( maxInputNo );
}


class LittleBigTableWithAutos {
    public:
        explicit LittleBigTableWithAutos( int maxInputNo );

        ~LittleBigTableWithAutos( );
        
        pair< int, int > getLittleBigForIndexWithAutos( int index ) const;
        
    private:
        const int tableSize_;
        vector< pair< int, int > > table_;
};


LittleBigTableWithAutos::LittleBigTableWithAutos( const int maxInputNo ) :
tableSize_( computeIndexCountForMaxInputNoWithAutos( maxInputNo ) ),
table_()
{
    table_.reserve( tableSize_ );

    for ( int index = 0; index < tableSize_; ++index )
        table_.push_back( computeLittleBigForIndexWithAutos( index ) );
}


LittleBigTableWithAutos::~LittleBigTableWithAutos( )
try {
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}


pair< int, int >
LittleBigTableWithAutos::getLittleBigForIndexWithAutos( const int index ) const
{
    if ( index < tableSize_ )
        return table_.at( index );
        
    return computeLittleBigForIndexWithAutos( index );
}


const LittleBigTableWithAutos gLittleBigTableWithAutos( 23 );


}  // namespace < anonymous >


pair< int, int >
carma::util::inputNoPairForBaselineIndexWithAutos( const int index )
{
    return gLittleBigTableWithAutos.getLittleBigForIndexWithAutos( index );
}


int
carma::util::baselineIndexForInputNosWithAutos( const int inputNo1,
                                                const int inputNo2 )
{
    const int little = ::std::min( inputNo1, inputNo2 );
    const int big = ::std::max( inputNo1, inputNo2 );
    
    return computeIndexForLittleBigWithAutos( little, big );
}


int
carma::util::baselineCountForMaxInputNoWithAutos( const int maxInputNo )
{
    return computeIndexCountForMaxInputNoWithAutos( maxInputNo );
}


pair< int, int >
carma::util::inputNoPairForBaselineIndex( const int index )
{
    pair< int, int > littleBig =
        gLittleBigTableWithAutos.getLittleBigForIndexWithAutos( index );
        
    ++(littleBig.second);

    return littleBig;
}


int
carma::util::baselineIndexForInputNos( const int inputNo1,
                                       const int inputNo2 )
{
    if ( inputNo1 == inputNo2 ) {
        ostringstream oss;
        
        oss << "Input number pair (" << inputNo1 << ", " << inputNo2
            << ") is an auto correlation";
            
        throw CARMA_ERROR( oss.str() );
    }

    const int little = ::std::min( inputNo1, inputNo2 );
    const int big = ::std::max( inputNo1, inputNo2 );
    
    return computeIndexForLittleBigWithAutos( little, (big - 1) );
}


int
carma::util::baselineCountForMaxInputNo( const int maxInputNo )
{
    if ( maxInputNo == 0 )
        return 0;
        
    return computeIndexCountForMaxInputNoWithAutos( maxInputNo - 1 );
}

