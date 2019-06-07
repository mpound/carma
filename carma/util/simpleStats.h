#ifndef CARMA_UTIL_SIMPLESTATS_H
#define CARMA_UTIL_SIMPLESTATS_H

#include <vector>


namespace carma {
namespace util {


void calcSimpleStats( const ::std::vector< float > & values,
                      float *                        minValue,
                      float *                        maxValue,
                      float *                        medianValue,
                      float *                        meanValue,
                      float *                        stdDev );

void calcSimpleStats( const ::std::vector< double > & values,
                      double *                        minValue,
                      double *                        maxValue,
                      double *                        medianValue,
                      double *                        meanValue,
                      double *                        stdDev );


void calcSimpleStatsInplace( ::std::vector< float > & values,
                             float *                  minValue,
                             float *                  maxValue,
                             float *                  medianValue,
                             float *                  meanValue,
                             float *                  stdDev );

void calcSimpleStatsInplace( ::std::vector< double > & values,
                             double *                  minValue,
                             double *                  maxValue,
                             double *                  medianValue,
                             double *                  meanValue,
                             double *                  stdDev );


}  // namespace carma::util
}  // namespace carma

#endif
