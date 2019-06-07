/**
 * Declaration of carma::pipeline::TsysPipelineInfo class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.3 $
 * $Date: 2013/05/15 15:23:00 $
 * $Id: TsysPipelineInfo.h,v 1.3 2013/05/15 15:23:00 abeard Exp $
 *
 * $CarmaCopyright$
 */
#ifndef CARMA_PIPELINE_TSYSPIPELINEINFO_H
#define CARMA_PIPELINE_TSYSPIPELINEINFO_H

// Note: PipelineMonitorInput can't be forward declared here (though it 
// typically can be) due to use of typedefed AntPolPair below.
#include "carma/monitor/PipelineMonitorInput.h" 
#include "carma/pipeline/pipelineUtils.h"

#include <memory>

namespace carma {

    namespace pipeline {


        class Tsys;

        /**
         * Calculates and stores Tsys values.
         */
        class TsysPipelineInfo {
        public:

            explicit TsysPipelineInfo( carma::pipeline::PipelineType plType );

            /* virtual */ ~TsysPipelineInfo( );

            void resetTsys( const std::vector< int > & carmaAntNoVec,
                            const carma::monitor::PipelineMonitorInput & plmi );

            void updateWithPipelineMonitorInput(
                const carma::monitor::PipelineMonitorInput & plmi );

            double getTsysDsb( unsigned int inputNo, 
                               unsigned int bandNo ) const;

            double getTsysDsb( const carma::monitor::AntPolPair & antPol,
                               unsigned int bandNo ) const;
            
            /**
             * Retrieve geometrically averaged tsys for given inputs and band.
             */
            double getBaselineTsys( unsigned int inputNo1, 
                                    unsigned int inputNo2,
                                    unsigned int bandNo,
                                    bool usb,
                                    std::pair< bool, bool > & validity ) const;

            double getBaselineTsys( const carma::monitor::AntPolPair & antPol1,
                                    const carma::monitor::AntPolPair & antPol2,
                                    unsigned int bandNo,
                                    bool usb ) const;

            double getBaselineTsysUsb( unsigned int inputNo1, 
                                       unsigned int inputNo2,
                                       unsigned int bandNo,
                                       std::pair<bool, bool> & validity ) const;

            double getBaselineTsysLsb( unsigned int inputNo1,
                                       unsigned int inputNo2,
                                       unsigned int bandNo,
                                       std::pair<bool, bool> & validity ) const;
            
            double getBaselineTsysUsb( 
                const carma::monitor::AntPolPair & antPol1,
                const carma::monitor::AntPolPair & antPol2,
                unsigned int bandNo ) const;

            double getBaselineTsysLsb( 
                const carma::monitor::AntPolPair & antPol1,
                const carma::monitor::AntPolPair & antPol2,
                unsigned int bandNo ) const;

            /**
             * Retrieve geometrically averaged Jy/K for given inputs and band.
             */
            double getBaselineJanskysPerKelvin( unsigned int inputNo1,
                                                unsigned int inputNo2,
                                                unsigned int bandNo ) const;

            double getBaselineJanskysPerKelvin( 
                const carma::monitor::AntPolPair & antPol1,
                const carma::monitor::AntPolPair & antPol2,
                unsigned int bandNo ) const;
            /**
             * Retrieve Tsys object for a particular band and input. 
             */
            const Tsys & getTsys( unsigned int bandNo, 
                                  unsigned int inputNo ) const;

            const Tsys & getTsys( const carma::monitor::AntPolPair & antPol,
                                  unsigned int bandNo ) const;
                                

        private:
          
            // Disallow assigment and copy construction
            TsysPipelineInfo( const TsysPipelineInfo & );
            TsysPipelineInfo & operator=( const TsysPipelineInfo & );

            class TsysInput;

            class Pimpl; // Details, details, details...
            ::std::auto_ptr< TsysPipelineInfo::Pimpl > pimpl_;

        }; // class TsysPipelineInfo

    } // namespace pipeline  
} // namespace carma
#endif 

