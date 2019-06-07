#ifndef CARMA_UTIL_SIMPLESTATISTICSACCUMULATORS_H
#define CARMA_UTIL_SIMPLESTATISTICSACCUMULATORS_H

#include <boost/accumulators/numeric/functional/complex.hpp>
#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/count.hpp>
#include <boost/accumulators/statistics/max.hpp>
#include <boost/accumulators/statistics/mean.hpp>
#include <boost/accumulators/statistics/min.hpp>
#include <boost/accumulators/statistics/rolling_count.hpp>
#include <boost/accumulators/statistics/rolling_sum.hpp>
#include <boost/accumulators/statistics/rolling_mean.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/error_of.hpp>
#include <boost/accumulators/statistics/error_of_mean.hpp>
#include <complex>

namespace carma {
namespace util {


// See http://boost-sandbox.sourceforge.net/libs/accumulators/doc/html/accumulators/user_s_guide.html#accumulators.user_s_guide.the_accumulators_framework.using___accumulator_set___
// for examples of how to use these.  They are very useful and convenient.

typedef boost::accumulators::stats< 
    boost::accumulators::tag::count,
    boost::accumulators::tag::max,
    boost::accumulators::tag::min,
    boost::accumulators::tag::mean,
    boost::accumulators::tag::error_of< 
        boost::accumulators::tag::mean> > SimpleStats;

typedef boost::accumulators::stats<
    boost::accumulators::tag::rolling_count,
    boost::accumulators::tag::rolling_sum,
    boost::accumulators::tag::rolling_mean > RollingStats;

typedef 
boost::accumulators::accumulator_set< double, SimpleStats >
DoubleStatAccumulator;

typedef 
boost::accumulators::accumulator_set< double, RollingStats >
DoubleRollingStatAccumulator;

typedef 
boost::accumulators::accumulator_set< float, SimpleStats > 
FloatStatAccumulator;

typedef 
boost::accumulators::accumulator_set< float, RollingStats > 
FloatRollingStatAccumulator;

typedef 
boost::accumulators::accumulator_set< short, SimpleStats >
ShortStatAccumulator;

typedef 
boost::accumulators::accumulator_set< short, RollingStats >
ShortRollingStatAccumulator;

typedef 
boost::accumulators::accumulator_set< int, SimpleStats >
IntStatAccumulator;

typedef 
boost::accumulators::accumulator_set< int, RollingStats >
IntRollingStatAccumulator;

typedef 
boost::accumulators::accumulator_set< long, SimpleStats > 
LongStatAccumulator;

typedef 
boost::accumulators::accumulator_set< long, RollingStats > 
LongRollingStatAccumulator;

typedef 
boost::accumulators::accumulator_set< unsigned short, SimpleStats >
UnsignedShortStatAccumulator;

typedef 
boost::accumulators::accumulator_set< unsigned short, RollingStats >
UnsignedShortRollingStatAccumulator;

typedef 
boost::accumulators::accumulator_set< unsigned int, SimpleStats >
UnsignedIntStatAccumulator;

typedef 
boost::accumulators::accumulator_set< unsigned int, RollingStats >
UnsignedIntRollingStatAccumulator;

typedef 
boost::accumulators::accumulator_set< unsigned long, SimpleStats >
UnsignedLongStatAccumulator;

typedef 
boost::accumulators::accumulator_set< unsigned long, RollingStats >
UnsignedLongRollingStatAccumulator;

typedef
boost::accumulators::accumulator_set< std::complex<double>, SimpleStats >
ComplexDoubleStatAccumulator;

typedef 
boost::accumulators::accumulator_set< std::complex< double >, RollingStats >
ComplexDoubleRollingStatAccumulator;

typedef
boost::accumulators::accumulator_set< std::complex<float>, SimpleStats >
ComplexFloatStatAccumulator;

typedef 
boost::accumulators::accumulator_set< std::complex< float >, RollingStats >
ComplexFloatRollingStatAccumulator;

}} // namespace carma::util
#endif
