#ifndef CARMA_PIPELINE_TSYS_H
#define CARMA_PIPELINE_TSYS_H

#include "carma/monitor/AntennaCommon.h"
#include "carma/util/Time.h"

#include <boost/shared_ptr.hpp>
#include <iostream>

namespace carma {

    namespace pipeline {

        /** 
         * Class to calculate, store and update tsys and associated parameters.
         * All calculations are based on 
         * <A HREF="http://www.mmarray.org/memos/carma_memo33.pdf">CARMA 
         * Memo #33</A>. 
         */
        class Tsys {
        public:

            /** 
             * Default Constructor
             */
            explicit Tsys( );

            /**
             * Copy Constructor
             */
            Tsys( const Tsys & other );

            /**
             * Destructor
             */
            /* virtual */ ~Tsys( );

            /**
             * Is Tsys Valid? 
             */
            bool valid( ) const;

            /**
             * Invalid tsys until the next complete ambient/sky cycle.
             */
            void invalidate( );

            /**
             * Retrieve double sideband system temperature.
             */
            double getTsysDsb( ) const;

            /**
             * Retrieve upper sideband system temperature.
             */
            double getTsysUsb( ) const;

            /**
             * Retrieve lower sideband system temperature.
             */
            double getTsysLsb( ) const;

            /** 
             * Set receiver sideband ratio.
             * This is the receiver sideband gain ratio.  This is 
             * not to be confused with the observed sbr measured on a flat 
             * continuum astronomical source.  
             */
            void setReceiverSidebandRatio( double sbr );

            /**
             * Set atmospheric opacity.
             * These are used to determine the observed sideband ratio and
             * double sideband atmospheric loss. 
             * @see carma::environment::OpacityModel
             */
            void setAtmosphericOpacity( double tauLSB,
                    double tauUSB );

            /**
             * Set the effective load temperature.  
             * This is the temperature of either a temperature controlled
             * hot load or the ambient temperature for a non temperature 
             * controlled hot load (e.g. absorber). 
             */
            void setLoadTemperature( double calTemp );

            /**
             * Set outside ambient temperature.
             * This is temperature as recorded by the weather station.
             * This temperature is also referred to as the ground temperature
             * in CARMA Memo #33. 
             */
            void setOutsideAmbientTemperature( double ambTemp );

            /** 
             * Set atmospheric temperature.
             * This temperature is commonly referred to as the sky temperature.
             * It is often assumed to be 0.94 * the outside air temperature.
             */
            void setAtmosphericTemperature( double atmosphericTemp );

            /**
             * Set effective opacity for ground spillover losses.
             */
            void setGroundOpacity( double opacity );

            /**
             * Set total power data for specified load state.
             * @param calState Calibration wheel state.
             * @param totalPower Total power in DBm
             * @param frame Frame time associated with total power.
             */
            void setTotalPower( 
                carma::monitor::AntennaCommon::CalStateMonitorPointEnum::CALSTATE calState,
                double totalPower,
                carma::util::frameType frame );

            /**
             * Set Janskys per Kelvin for flux calibration.
             * @param jyPerK value.
             */
            void setJanskysPerKelvin( double jyPerK );

            /** 
             * Retrieve janskys per kelvin for flux calibration.
             * @return JyPerK
             */
            double getJyPerK( ) const;

            /** 
             * Calculate tsys.
             */
            void calculateTsys( );

            /**
             * Retrieve effective calibration temperature.
             * Debugging.
             */
            double getEffectiveTcal( ) const;

            /**
             * Retrieve ambient load psys.
             * For deugging and system diagnostics.
             */
            double getAmbPsysdBm( ) const;

            /**
             * String of all calibration calculation information.
             */
            ::std::string getDetails( ) const;

        private:

            struct TsysInfo; // Hide the many details
            ::boost::shared_ptr< TsysInfo > info_;

        }; // End class Tsys
    } // End namespace pipeline
} // End namespace carma
#endif
