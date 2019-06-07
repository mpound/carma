/**
 *
 * Implementation of the numeric monitor point class.
 *
 * @author: Steve Scott
 *
 * $Id: MonitorPointNumeric.cc,v 1.28 2010/07/21 17:58:27 abeard Exp $
 * $CarmaCopyright$
 *
 */

#include <cmath>
#include <iostream>

#include "carma/util/checking.h"
#include "carma/monitor/MonitorPointAverageT.h"
#include "carma/monitor/MonitorPointNumeric.h"

using namespace ::std;
using namespace carma;
using namespace carma::monitor;


MonitorPointNumeric::MonitorPointNumeric(
    const string &     name,
    MonitorValueType   valuetype,
    MONITOR_POINT_TYPE monitorPointType ) :
MonitorPoint( name, valuetype, monitorPointType ),
sum_( 0.0 ),
nValidSamples_( 0 ),
aveValidity_( INVALID_NO_DATA ),
aveBlankingFlagging_( UNDETERMINED )
{
}


MonitorPointNumeric::~MonitorPointNumeric( )
{
    if ( debug_ ) cout << "MonitorPointNumeric destructor" << endl;
}


double
MonitorPointNumeric::computeFrameAverage(
    MonitorPointAverageNumeric & scratchAvg )
{
    resetAccumulator( scratchAvg );
    accumulate( scratchAvg );

    sum_ = scratchAvg.getAccumulator();
    aveValidity_ = scratchAvg.getValidity();
    aveBlankingFlagging_ = scratchAvg.getBlanking();
    nValidSamples_ = scratchAvg.getNumValidSamples();

    return getAccumulatedAverageDouble( scratchAvg );
}


enum MonitorPoint::VALIDITY
MonitorPointNumeric::accumulateSample(
    MonitorPointAverageNumeric & avg,
    const int                    index ) const
{
    const enum MonitorPoint::VALIDITY v = getValidity( index );

    if ( isValid( v ) ) {
        const double value = getValueNumeric( index );
        avg.setAccumulator( avg.getAccumulator() + value );
        avg.incrementNumValidSamples();
        avg.setMaxValue( ::std::max( avg.getMaxValue(), value ) );
        avg.setMinValue( ::std::min( avg.getMinValue(), value ) );
    }

    avg.incrementNumTotalSamples();

    return v;
}


void
MonitorPointNumeric::accumulate( MonitorPointAverageNumeric & avg ) const
{
    const int nSamples = getNumSamples();

    enum MonitorPoint::VALIDITY v = INVALID_NO_DATA;
    for ( int i = 0; i < nSamples; ++i )
        v = accumulateSample( avg, i );

    enum MonitorPoint::VALIDITY aveValidity = avg.getValidity();
    if ( v > aveValidity )
        aveValidity = v;

    if ( avg.getNumValidSamples() == 0 ) {
        if ( avg.getNumTotalSamples() == 0 )
            aveValidity = INVALID_NO_DATA;
        else
            aveValidity = v;
    } else if ( avg.getNumValidSamples() != avg.getNumTotalSamples() ) {
        // Some (but not all) samples are good
        aveValidity = VALID_NOT_CHECKED;
    }

    avg.setValidity( aveValidity );
}


void
MonitorPointNumeric::accumulateAverage( MonitorPointAverageNumeric & accum )
{
    const int numSamps = getNumSamples();
    const int numValidSamps = getNumValidSamples();
    
    nValidSamples_ = numValidSamps;
    sum_ = nValidSamples_ * getAveNumeric();  // What is this doing? -TWC
    accum.setAccumulator (accum.getAccumulator()+sum_);
    accum.setBlanking (getBlankingFlagging());
    accum.incrementNumTotalSamples( numSamps );
    accum.incrementNumValidSamples( nValidSamples_ );

    if ( numValidSamps > 0 ) {
        // At least 1 sample is valid
        
        double accumMaxValue = accum.getMaxValue();
        double accumMinValue = accum.getMinValue();

        if ( numValidSamps == numSamps ) {
            // All samples are valid
            
            for ( int i = 0; i < numSamps; ++i ) {
                const double value = getValueNumeric( i );
                
                accumMaxValue = ::std::max( accumMaxValue, value );
                accumMinValue = ::std::min( accumMinValue, value );
            }
        } else {
            // Some (but not all) samples are valid
            
            for ( int i = 0; i < numSamps; ++i ) {
                const MonitorPoint::VALIDITY v = getValidity(i);
        
                if ( isValid( v ) ) {
                    const double value = getValueNumeric( i );
                    
                    accumMaxValue = ::std::max( accumMaxValue, value );
                    accumMinValue = ::std::min( accumMinValue, value );
                }
            }
        }
        
        accum.setMaxValue( accumMaxValue );
        accum.setMinValue( accumMinValue );
    }
    
    const int accumValidSamps = accum.getNumValidSamples();
    const int accumTotalSamps = accum.getNumTotalSamples();

    if ( accumValidSamps == 0 ) {
        if ( accumTotalSamps == 0 )
            accum.setValidity( INVALID_NO_DATA );
        else
            accum.setValidity( getAveValidity() );
    } else if ( accumValidSamps != accumTotalSamps ) {
        // Some (but not all) samples are valid
        accum.setValidity( VALID_NOT_CHECKED );
    } else {
        const enum MonitorPoint::VALIDITY v = getAveValidity();

        if ( v > accum.getValidity() )
            accum.setValidity( v );
    }
}


void
MonitorPointNumeric::resetAccumulator( MonitorPointAverageNumeric & avg ) const
{
    avg.setAccumulator (0.0);
    avg.setMaxValue (-(HUGE_VAL));
    avg.setMinValue (HUGE_VAL);
    avg.resetAveProperties();
}


double
MonitorPointNumeric::getAccumulatedAverageDouble(
    const MonitorPointAverageNumeric & avg ) const
{
    double averageValue;

    if (avg.getNumValidSamples() == 0)  {
        CARMA_CHECK (avg.getAccumulator() == 0.0);
        averageValue = 0.0;
    } else if (avg.getNumValidSamples() == 1)  {
        averageValue = avg.getAccumulator();
    }  else  {
        averageValue = avg.getAccumulator()/avg.getNumValidSamples();
    }

    return averageValue;
}


// Checks type, name and value
bool
MonitorPointNumeric::operator==(const MonitorComponent& component) const
{
    // Make sure its a monitor point and not a monitor container
    const MonitorPointNumeric* mp =
            dynamic_cast<const MonitorPointNumeric*>(&component);
    if ( mp == 0) return false;

    const MonitorPointNumeric& m = *mp;

    // Check base type
    if (! this->MonitorPoint::isEqualTo (m)) return false;

    // Check values
    // Values are checked independent of status flags -- is this
    // correct ? - Amar
    const int nSamples = getNumSamples();
    for ( int i = 0; i < nSamples; ++i ) {
        if ( m.getValueNumeric( i ) != getValueNumeric( i ) )
            return false;
    }

    return true;
}
