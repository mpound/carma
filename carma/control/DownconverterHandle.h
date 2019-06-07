#ifndef CARMA_CONTROL_DOWNCONVERTERHANDLE_H
#define CARMA_CONTROL_DOWNCONVERTERHANDLE_H

/**
 * @file
 *
 * Carma control interface to the downconverter.
 *
 * @author: Amar Amarnath
 *
 * $CarmaCopyright$
 *
 */
 

#include "carma/corba/corba.h"
#include "carma/control/SubarrayControl_skel.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/downconverter/common/NoiseSourceControl.h"
#include "carma/downconverter/common/QuadModControl.h"
#include "carma/downconverter/common/DownconverterControl.h"
#include "carma/downconverter/common/downconverterSystem.h"
#include "carma/downconverter/spectral/SpectralDownconverterControl.h"
#include "carma/downconverter/spectral/BlockDownconverterControl.h"


namespace carma {
namespace control {


typedef RemoteObjHandleT< carma::downconverter::System >
        DownconverterSystemRemoteObjHandle;


//! @brief Manages downconverter system DO connections
class DownconverterHandle : public DownconverterSystemRemoteObjHandle {
    public:
        /**
         * Constructor
         *
         * @param carmaMonitor ::carma::monitor::MonitorSystem&
         *              monitor system, which allows delay engine to get a 
         *              reference to its own monitor stream.
         * @param subarrayMonitor 
         *        const ::carma::monitor::ControlSubsystemBase:Subarray&
         *              monitor system, which allows delay engine to set 
         *              monitor points for the subarray within control monitor 
         *              subsystem .
         */
        DownconverterHandle(
            bool                                                spectral,
            ::carma::monitor::MonitorSystem &                   carmaMonitor, 
            ::carma::monitor::ControlSubsystemBase::Reachable & reachable );
        
        
        /**
        * Destructor - releases object (DO) references.
        */
        virtual ~DownconverterHandle();
        
        bool isSpectral( ) const;
        
        /**
         * Turns noise source on or off based on the value of state.
         * If state == true, the noiseSource is on, else off.
         *
         * @param state bool if true, switch noise source on, else put it off.
         */
        void noiseSource (bool state) ;
        
        /**
         * Set noise source power output level to the preset value stored
         * in the noise source module's EEPROM.
         */
        void noisePreset( ); 

        /**
         * Sets noise source attenuation level
         * @param atten integer attenuation.
         */
        void noiseAtten (short atten) ;
               
        /**
         * Sets quadmod attenuation level
         * @param atten integer attenuation.
         */
        void quadmodAtten (short atten) ;
        
        /**
         * Set input power level to preset level for all downconverters.
         * @param inputNo vector of input numbers, zero means all
         * @param bandNo band number (first band is 1)
         */
        void psysPreset(std::vector<short> inputNo, short bandNo);
        
        /**
         * Set if output power level to preset level for all downconverters.
         */
        void ifoutPreset () ;
        
        /**
         * Set input power level to preset level for a single downconverters.
         * @param level power level in dBm
         * @param inputNo starting at one
         * @param bandNo starting at one
         */
        void psysLevel(double level, short inputNo, short bandNo) ;
        
        /**
         * Set input power level to requested level for all downconverters.
         * @param level power level in dBm
         */
        void psysLevel(double level) ;
        
        /**
         * Set output power level to requested level for all downconverters.
         * @param  level power level in dBm
         */
        void ifoutLevel(double level) ;
        
        /**
         * Turn on/off rf power for all downconverters.
         * @param  state
         */
        void rfPower(bool state) ;
        
        /**
         * Select the 2nd LO downconversion sideband for a band
         * @param  sideband
         * @param  band number, starting with 1
         */
        void selectSlSideband(
            downconverter::DownconverterControl::SidebandType sideband, 
            unsigned short bandNo) ;
        
        /**
         * Provide the 2nd LO frequency
         * @param  freq - 2nd LO frequency in GHz
         * @param  band number, starting with 1
         */
        void setSlLOFrequency(float loFreq, unsigned short bandNo) ;        
        
        /**
         * Set sideband and the 2nd LO frequency
         * @param  sideband
         * @param  freq - 2nd LO frequency in GHz
         * @param  band number, starting with 1
         */
        void setSlSidebandFrequency(
            downconverter::DownconverterControl::SidebandType sideband, 
            double loFreq, unsigned short bandNo) ;

        /**
         * Select the filter for all module in a band
         * @param  filter
         * @param  band number, starting with 1
         */
        void selectFilter(
            downconverter::DownconverterControl::FilterType filter, 
            unsigned short bandNo) ;

        void selectFilter(
            downconverter::DownconverterControl::FilterType filter, 
            unsigned short inputNo,
            unsigned short bandNo) ;
        
        /**
         * Select the block downconverter block (upper or lower )
         * and polarization (pol1 or pol2)
         * @param block The enumerated block value
         * @param polarization The enumerated polarization value
         */
        void setBlockAndPolarization(
             const downconverter::BlockDownconverterControl::Block block,
             const downconverter::BlockDownconverterControl::Polarization polarization,
             const unsigned short bandNo);

        void setBlockAndPolarization(
             const downconverter::BlockDownconverterControl::Block block,
             const downconverter::BlockDownconverterControl::Polarization polarization,
             const unsigned short inputNo,
             const unsigned short bandNo);

    protected:
        virtual bool resolveObjRef( );
        
    private:
        typedef carma::downconverter::DownconverterControl ControlType;
        typedef carma::downconverter::QuadModControl       QuadModType;
        typedef carma::downconverter::NoiseSourceControl   NoiseSourceType; 
        typedef carma::downconverter::BlockDownconverterControl BlockDCType; 
        
        bool isControlReachable( bool logIfNotReachable );
        bool isQuadModReachable( bool logIfNotReachable );
        bool isNoiseSourceReachable( bool logIfNotReachable );
        bool isBlockDownconverterReachable( bool logIfNotReachable );
        
        bool isControlReachable( );
        bool isQuadModReachable( );
        bool isNoiseSourceReachable( );
        bool isBlockDownconverterReachable( );
        
        const bool isSpectral_;
        
        ::carma::monitor::MonitorPointBool & controlReachableMp_;
        ::carma::monitor::MonitorPointBool & quadModReachableMp_;
        ::carma::monitor::MonitorPointBool & noiseSourceReachableMp_;
        ::carma::monitor::MonitorPointBool & blockDCReachableMp_;
        
        ControlType::_var_type     cachedControl_;
        QuadModType::_var_type     cachedQuadMod_;
        NoiseSourceType::_var_type cachedNoiseSource_;
        BlockDCType::_var_type     cachedBlockDC_;
};


}
}  // End namespace carma/control


#endif
