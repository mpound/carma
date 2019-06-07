/* MonitorPointThreshold.cc - Contains methods for monitor point 
 * threshold class.
 *
 * @CarmaCopyright@
 */

#include <math.h>
#include <numeric>

#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/MonitorPointThreshold.h"

using namespace carma;
using namespace carma::monitor;


void                
MonitorPointThreshold::setRangeValue (ThresholdValueEnum lowHigh, const MonitorValue value)
{
    thresholds_.thresholdRange[lowHigh] = value;
    ThresholdSetState setState = THRESHOLD_NONE_SET;
    switch (lowHigh)  {
        case THRESHOLD_LOW_ERROR_VALUE:  
                               setState = THRESHOLD_ERROR_LOW_SET;
                               break;

        case THRESHOLD_HIGH_ERROR_VALUE:
                               setState = THRESHOLD_ERROR_HIGH_SET;
                               break;

        case THRESHOLD_LOW_WARN_VALUE:
                               setState = THRESHOLD_WARN_LOW_SET;
                               break;

        case THRESHOLD_HIGH_WARN_VALUE:
                               setState = THRESHOLD_WARN_HIGH_SET;
                               break;

        default:               break;
    }
    thresholds_.flags |= setState;
}



void                
MonitorPointThreshold::setRangeValue (ThresholdValueEnum lowHigh, const char value)
{
    MonitorValue val;
    val.lo = 0;
    val.byte = value;
    setRangeValue (lowHigh, val);
}



void                
MonitorPointThreshold::setRangeValue (ThresholdValueEnum lowHigh, const short value)
{
    MonitorValue val;
    val.lo = 0;
    val.sh = value;
    setRangeValue (lowHigh, val);
}



void                
MonitorPointThreshold::setRangeValue (ThresholdValueEnum lowHigh, const long value)
{
    MonitorValue val;
    val.lo = value;
    setRangeValue (lowHigh, val);
}



void                
MonitorPointThreshold::setRangeValue (ThresholdValueEnum lowHigh, const bool value)
{
    // Booleans are a speial case - set low/high range values to value passed in.
    MonitorValue val;
    val.bo = value;
    switch (lowHigh)  {
        case THRESHOLD_LOW_ERROR_VALUE:  
        case THRESHOLD_HIGH_ERROR_VALUE:
                               setRangeValue (THRESHOLD_LOW_ERROR_VALUE, val);
                               setRangeValue (THRESHOLD_HIGH_ERROR_VALUE, val);
                               unset (THRESHOLD_LOW_WARN_VALUE);
                               unset (THRESHOLD_HIGH_WARN_VALUE);
                               break;

        case THRESHOLD_LOW_WARN_VALUE:
        case THRESHOLD_HIGH_WARN_VALUE:
                               setRangeValue (THRESHOLD_LOW_WARN_VALUE, val);
                               setRangeValue (THRESHOLD_HIGH_WARN_VALUE, val);
                               unset (THRESHOLD_LOW_ERROR_VALUE);
                               unset (THRESHOLD_HIGH_ERROR_VALUE);
                               break;

        default:               break;
    }
}




void                
MonitorPointThreshold::setRangeValue (ThresholdValueEnum lowHigh, const float value)
{
    MonitorValue val;
    val.fl = value;
    setRangeValue (lowHigh, val);
}



void                
MonitorPointThreshold::setRangeValue (ThresholdValueEnum lowHigh, const double value)
{
    MonitorValue val;
    val.db = value;
    setRangeValue (lowHigh, val);
}



void                
MonitorPointThreshold::setRangeValue (ThresholdValueEnum lowHigh, const std::string& value)
{
    MonitorValue val;
    strncpy (val.str,  value.c_str(), sizeof (thresholds_.thresholdRange[lowHigh])-1);
    setRangeValue (lowHigh, val);
}



void                
MonitorPointThreshold::setRangeValue (ThresholdValueEnum lowHigh, const std::complex<float> value)
{
    MonitorValue val;
    val.complex[0] = value.real();
    val.complex[1] = value.imag();
    setRangeValue (lowHigh, val);
}



void                
MonitorPointThreshold::setRangeValueSerialNo (ThresholdValueEnum lowHigh, const long value)
{
    MonitorValue val;
    val.sn = value;
    setRangeValue (lowHigh, val);
}


void    
MonitorPointThreshold::setThresholdValuesFromDefaults (const MonitorPoint& mp)
{
    MonitorValue value;

    // set warn low
    if (mp.warnLowDefaultIsSet())  {
	value = mp.getWarnLowDefault ();
	setRangeValue (THRESHOLD_LOW_WARN_VALUE, value);
    }

    // set warn high
    if (mp.warnHighDefaultIsSet())  {
	value = mp.getWarnHighDefault ();
	setRangeValue (THRESHOLD_HIGH_WARN_VALUE, value);
    }

    // set error low
    if (mp.errorLowDefaultIsSet())  {
	value = mp.getErrorLowDefault ();
	setRangeValue (THRESHOLD_LOW_ERROR_VALUE, value);
    }

    // set error high
    if (mp.errorHighDefaultIsSet())  {
	value = mp.getErrorHighDefault ();
	setRangeValue (THRESHOLD_HIGH_ERROR_VALUE, value);
    }
}


void
MonitorPointThreshold::unset (const ThresholdValueEnum lowHigh)
{
    long flag = 1 << lowHigh;
    flag = ~flag;
    thresholds_.flags &= flag;
}


