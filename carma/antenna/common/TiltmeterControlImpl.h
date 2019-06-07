/**
 * @file
 * TiltmeterControl implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.5 $
 * $Date: 2012/02/21 21:06:59 $
 * $Id: TiltmeterControlImpl.h,v 1.5 2012/02/21 21:06:59 abeard Exp $
 */
#ifndef CARMA_ANTENNA_COMMON_TILTMETERCONTROLIMPL_H
#define CARMA_ANTENNA_COMMON_TILTMETERCONTROLIMPL_H

#include "carma/antenna/common/TiltmeterControl.h"
#include "carma/corba/corba.h"

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {

namespace ovro {
    // Forward declaration
    class Tiltmeter;
}

namespace common {

    /**
     * CORBA control implementation for the TiltmeterControl interface.
     */
    class TiltmeterControlImpl 
    {
    public:

        TiltmeterControlImpl( 
            carma::antenna::ovro::Tiltmeter & tiltmeter );

        ~TiltmeterControlImpl(); 

        void setTemperature(float temp);

        void regulateTemperature(
            carma::antenna::common::TiltmeterControl::OpMode mode,
            float pwrfract);

        void setLoopGain(float gain);

        void setLoopIntegrationConstant(float loopInteg);

        void setLoopRateConstant(float rateConst);

        void setLoopBandwidth(float bw);

        void writeLoopParametersToEEPROM();
    
    private:

        carma::antenna::ovro::Tiltmeter &tilt_;
        log4cpp::Category &log_;

    }; // End class TiltmeterControlImpl
}}} // End namespace carma::antenna::common
#endif
