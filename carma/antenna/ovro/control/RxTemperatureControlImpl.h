/**
 * @file
 * RxTemperatureControl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.7 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: RxTemperatureControlImpl.h,v 1.7 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_RXTEMPERATURECONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_RXTEMPERATURECONTROLIMPL_H

#include "carma/antenna/ovro/control/RxTemperatureControl.h"

namespace log4cpp {
    // Forward dec
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace ovro {

    // Forward declaration
    class RxTemperatures;

    /**
     * Corba Rx Electronics Thermal Controller implementation.
     */
    class RxTemperatureControlImpl {
    public:

        /**
         * Constructor
         */
        RxTemperatureControlImpl(
            RxTemperatures& rxtemp );

        ~RxTemperatureControlImpl(); 

        void setTemperature(
            carma::antenna::ovro::RxTemperatureControl::LoopId loop,
            float temp);

        void regulateTemperature(
            carma::antenna::ovro::RxTemperatureControl::LoopId loop,
            carma::antenna::ovro::RxTemperatureControl::OpMode mode,
            float pwr);

        void setLoopGain(
            carma::antenna::ovro::RxTemperatureControl::LoopId loop,
            float gain);

        void setLoopIntegrationConstant(
            carma::antenna::ovro::RxTemperatureControl::LoopId loop,
            float integration);

        void setLoopRateConstant(
            carma::antenna::ovro::RxTemperatureControl::LoopId loop,
            float rate);

        void setLoopBandwidth(
            carma::antenna::ovro::RxTemperatureControl::LoopId loop,
            float bandwidth);

        void writeParametersToEEPROM();

        void reset();
    
    private:

        RxTemperatures &rxtemp_;
        log4cpp::Category &log_;

    }; // End class RxTemperatureControlImpl
}}} // End namespace carma::antenna::ovro
#endif
