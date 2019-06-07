#include "carma/util/simpleStats.h"

#include <algorithm>
#include <cmath>

#include "carma/util/ErrorException.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


template < typename T >
void
commonCalcStatsInplace( vector< T > & values,
                        T * const     minValue,
                        T * const     maxValue,
                        T * const     medianValue,
                        T * const     meanValue,
                        T * const     stdDev )
{
    // NOTE: Clearly I could do much better for large sets of values by
    //       using something like a Hoare kth-selection algorithm to find
    //       the min, max, and median values and just do the mean
    //       computation via a linear scan (the same one used to run the
    //       kth-selection possibly). However, thus far I don't need
    //       this code to run in O(N) (instead of O(N log N)) enough to
    //       go get/make a kth-selection algorithm since I didn't see an
    //       obvious one in the STL.
    stable_sort( values.begin(), values.end() );

    const size_t numValues = values.size();

    if ( numValues < 1 )
        throw CARMA_ERROR( "Values vector is empty" );

    if ( minValue != 0 )
        *minValue = values[0];

    if ( maxValue != 0 )
        *maxValue = values[numValues - 1];

    if ( medianValue != 0 ) {
        const size_t i = (numValues / 2);

        if ( (2 * i) != numValues )
            *medianValue = values[i];
        else
            *medianValue = ((values[i - 1] + values[i]) / 2);
    }

    if ( (meanValue != 0) || (stdDev != 0) ) {
        double sum = 0;

        for ( size_t i = 0; i < numValues; ++i )
            sum += values[i];

        const double localMeanValue = (sum / numValues);

        if ( stdDev != 0 ) {
            double sumDiffSquared = 0;

            for ( size_t i = 0; i < numValues; ++i ) {
                const double diff = values[i] - localMeanValue;

                sumDiffSquared += (diff * diff);
            }

            const double variance = (sumDiffSquared / numValues);

            *stdDev = sqrt( variance );
        }

        if ( meanValue != 0 )
            *meanValue = localMeanValue;
    }
}


}  // namespace < anonymous >


void
carma::util::calcSimpleStats( const vector< float > & values,
                              float * const     minValue,
                              float * const     maxValue,
                              float * const     medianValue,
                              float * const     meanValue,
                              float * const     stdDev )
{
    vector< float > valuesCopy( values );

    commonCalcStatsInplace( valuesCopy,
                            minValue,
                            maxValue,
                            medianValue,
                            meanValue,
                            stdDev );
}


void
carma::util::calcSimpleStats( const vector< double > & values,
                              double * const     minValue,
                              double * const     maxValue,
                              double * const     medianValue,
                              double * const     meanValue,
                              double * const     stdDev )
{
    vector< double > valuesCopy( values );

    commonCalcStatsInplace( valuesCopy,
                            minValue,
                            maxValue,
                            medianValue,
                            meanValue,
                            stdDev );
}


void
carma::util::calcSimpleStatsInplace( vector< float > & values,
                                     float * const     minValue,
                                     float * const     maxValue,
                                     float * const     medianValue,
                                     float * const     meanValue,
                                     float * const     stdDev )
{
    commonCalcStatsInplace( values,
                            minValue,
                            maxValue,
                            medianValue,
                            meanValue,
                            stdDev );
}


void
carma::util::calcSimpleStatsInplace( vector< double > & values,
                                     double * const     minValue,
                                     double * const     maxValue,
                                     double * const     medianValue,
                                     double * const     meanValue,
                                     double * const     stdDev )
{
    commonCalcStatsInplace( values,
                            minValue,
                            maxValue,
                            medianValue,
                            meanValue,
                            stdDev );
}
