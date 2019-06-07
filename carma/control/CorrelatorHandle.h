#ifndef CARMA_CONTROL_CORRELATOR_HANDLE_H
#define CARMA_CONTROL_CORRELATOR_HANDLE_H

/**
 * @file
 *
 * Control system interface to the correlator control distributed object.
 *
 * $CarmaCopyright$
 *
 */
 

#include <vector>

#include "carma/corba/corba.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/control/CorrelatorInfo.h"
#include "carma/control/CorrDefs.h"
#include "carma/control/SubarrayControlImpl.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/NoiseSource.h"
#include "carma/correlator/obsRecord2/CorDataBase.h"
#include "carma/correlator/obsRecord2/obsRecordUtils.h"
#include "carma/util/corrUtils.h"


namespace carma {
namespace control {


typedef RemoteObjHandleT< correlator::obsRecord2::Correlator_I > 
        CorrelatorControlRemoteObjHandle;


//! @brief Manages correlator control DO connections
class CorrelatorHandle : public CorrelatorControlRemoteObjHandle {
    public:
        /**
         * Constructor
         *
         * @param monitorSystem monitor system reference which allows this
         *                      handle to get a reference to its own monitor
         *                      stream.
         * This constructor will throw ErrorException if
         * the Band hardware type comes back as HARDWARE_TYPE_UNKNOWN.
         * @see util/corrUtils.h
         */
        CorrelatorHandle(
            unsigned int                               astroBandNo,
            carma::util::CorrelatorType                corrType,
            monitor::MonitorSystem &                   monitorSys,
            monitor::ControlSubsystemBase::Reachable & reachable,
            bool                                       defaultLogIfNotReach );
    
        virtual ~CorrelatorHandle( );

        //! set input delay triplets for a single polarization.
        //! @note The vector argument is passed by value so we can safely
        //!       insure that it is fully and deeply copied across the thread
        //!       barrier when this method is forked out to the worker pool
        void setInputDelayTriplets(
            ::std::vector< correlator::obsRecord2::DelayTriplet > triplets );

        //! @return True if the band has been marked offline, false if
        //! it is marked online,
        //! REGARDLESS OF WHETHER OR NOT THE REMOVE OBJECT IS REACHABLE.
        bool isOffline() const { return !online_ ; }

        //! Mark a band offline or online.  An offline band will
        //! ignore all commands.
        //@param offline True if the band is online, false if offline
        void setOnline( const bool online )  {
            online_ = online;
        }
        
        //! Set bandwidth with a preferred sequence number.
        //! Sets spectral bandwidth mode for the band.
        //! The process takes several seconds and is spawned off in
        //! a thread.
        //! @param bw bandwidth indicator (500MHZ, 250MHZ_3BIT, etc)
        //! @param preferredSeqNo - The preferred sequence number for 
        //! this command. If the preferred sequence number has already 
        //! been used, the actual sequence number is set to preferred + 10. 
        //! @param The astroBand number to set.  Default of -1 means the
        //! cobra library will set it to the Correlator Band number.
        void setBandwidth( 
            carma::correlator::obsRecord2::BandWidthType bw,
            const int preferredSeqNo,
            const unsigned int astroBandNo = 0 );

        //! Vectorized version of setBandwidth, for support of C3G Correlator
        //! @param bw vector of bandwidth indicators (500MHZ, 250MHZ_3BIT, etc)
        //! @param bw vector of fpgmmode indicators (LL, FULLPOL, etc)
        //! @param preferredSeqNo - The preferred sequence number for 
        //! this command. If the preferred sequence number has already 
        //! been used, the actual sequence number is set to preferred + 10. 
        //! @param astrobandNo vector of astroBand numbers for, one for 
        //! each bandwidth indicator
        void setBandwidth( 
            ::std::vector<carma::correlator::obsRecord2::BandWidthType> bw,
            ::std::vector<carma::correlator::obsRecord2::FpgaModeType> fm,
            const int preferredSeqNo,
            ::std::vector<unsigned int> astroBandNo );

        //! Optimize thresholds on the band.  The process on
        //! the remote object takes ~10 seconds
        //! @precondition Noise source must be OFF.
        //! @param preferredSeqNo - The preferred sequence number for 
        //! this command. If the preferred sequence number has already 
        //! been used, the actual sequence number is set to preferred + 10. 
        void optimizeThresholds( const int preferredSeqNo );

        //! Flatten phases on the band.  The process on the
        //! remote object takes ~30 seconds
        //! @precondition Noise source must be ON
        //! @param preferredSeqNo - The preferred sequence number for 
        //! this command. If the preferred sequence number has already 
        //! been used, the actual sequence number is set to preferred + 10. 
        void flattenPhases( const int preferredSeqNo );
     
        //! Calibrate spectra (bandpass) on the band.  The process on the
        //! remote object takes ~30(?) seconds
        //! @precondition Noise source must be in requested state
        //! @param enable Whether or not to enable bandpass (spectral) 
        //! calibration (can be used to toggle it on and off)
        //! @param noiseEnabled Whether or not to enable the noise source. For 
        //! instance, you would set this to false if doing bandpass 
        //! calibration on an astronomical source.
        //! @param cache Whether or not to cache the result for 
        //! subsequent division.
        //! @param intTime Integration time in seconds for the calibration.
        //! @param preferredSeqNo - The preferred sequence number for 
        //! this command. If the preferred sequence number has already 
        //! been used, the actual sequence number is set to preferred + 10. 
        void calibrateSpectra( const bool noiseEnabled,
                               const float intTime,
                               const bool cache,
                               const bool enable,
                               const int preferredSeqNo
                              );

        //! Vectorized version of call to set the downconverter settings.
        //! There is no handle wrapper for the non-vector call since 
        //! no sequence number needs to be set and the cobra library call returns
        //! quickly.  This method must convert vector to CORBA sequence before
        //! the remote call.
        //! @param dcFreq - vector of downconverter frequencies in GHz.
        void setDownconverterSettings( ::std::vector<float> dcFreqGHz );

        
        //! Enable or disable correlations (COBRA boards only).
        //! This is to be used when the correlator room is overheating.
        //!  @param enable Enable correlation or not.
        void enableCorrelation( const bool enable );

        //! Compare next sequence number with one returned from monitor system.
        //! If they are the same then the last drive action is complete.
        //! @param monsys monitor system from which to retrieve completion
        //! @param monitorDataErrorLimit number of consecutive monitor data
        //! invalid limit before thowing an exception
        //! @return true if last action is complete
        //! @throws if number of consecutive monitor data errors is exceeed
        bool isActionComplete( const monitor::MonitorSystem & monsys,
                               int monDataErrorLimit );

        //! Set the Astroband (FPGA) mode of this band. This will typically
        //! happen from within configband().
        //! @param astroBandMode The FPGA configuration mode: SINGLEPOL, DUALPOL, FULLPOL, or CARMA23
        void setAstroBandMode( util::CorrelatorFpgaModeType astroBandMode ) { astroBandMode_ = astroBandMode; }

        //! Get the FPGA mode of this band.
        //! @return The FPGA configuration mode: SINGLEPOL, FULLPOL, or CARMA23
        util::CorrelatorFpgaModeType getFpgaMode( ) const { return astroBandMode_; }

        unsigned int correlatorBandNo() const {return bandNo_;}
        unsigned int astroBandNo() const {return astroBandNo_ ;}

         /**
          * @brief Sets correlator input Walsh column indices
          * @param cols90   Walsh columns used for 90-deg demodulation.
          * @param cols180     Walsh columns used for 180-deg demodulation.
          * @param nStates90   Number of states for 90-deg demodulation
          *                    (zero maintains existing value).
          * @param nStates180  Number of states for 180-deg demodulation
          *                    (zero maintains existing value).
          * @param noiseEnabled True if the columns are for the noise source
          *                     False if RF.
          */
        void setWalshColumns(
                ::std::vector<int> cols90,
                ::std::vector<int> cols180,
                const int nStates90,
                const int nStates180,
                const bool noiseEnabled);

        /**
         * @ return the underlying hardware type of correlator that this handle controls
         */
        carma::util::hardwareType hardwareType() const {return carma::util::hwType(astroBandNo_); }

        /**
         * @return True if the underlying hardware type is HARDWARE_TYPE_CARMA, false otherwise.
         */
        bool isSpectral() const { return hardwareType() == carma::util::HARDWARE_TYPE_CARMA; }

        /**
         * @return True if the underlying hardware type is HARDWARE_TYPE_COBRA, false otherwise.
         */
        bool isWideband() const { return hardwareType() == carma::util::HARDWARE_TYPE_COBRA; }

        /**
         * @return True if the underlying hardware type is HARDWARE_TYPE_C3G, false otherwise.
         */
        bool isC3g() const { return hardwareType() == carma::util::HARDWARE_TYPE_C3G; }

        // @FIXME NO LONGER NEEDED.
        /**
         * For C3G correlator, assert configurations on a group of bands
         * @param cabmap curretn ConfigAstroBand map from SubarrayControl.
         * @param preferredSeqNo - The preferred sequence number for 
         * this command. If the preferred sequence number has already 
         * been used, the actual sequence number is set to preferred + 10. 
        void assertConfiguration( const SubarrayControlImpl::ConfigAstroBandMap  * cabmap, 
                                  const int preferredSeqNo );
         */


    private:
        
        bool isNoiseSourceEnabled( void );
        bool isNoiseSourceDisabled( void );
        bool isNoiseSource( 
            monitor::NoiseStatusMonitorPointEnum::NOISESTATUS status 
                );
        void setNextSequenceNo( int preferredSequenceNo );

        bool online_;
        int nextSequenceNo_; 
        int consecutiveErrors_;
        int errLimit_;
        monitor::MonitorSystem & monitorSys_;
        unsigned int bandNo_;
        unsigned int astroBandNo_;
        util::CorrelatorFpgaModeType astroBandMode_;
        carma::util::CorrelatorType corrType_;
    
};


}  // namespace carma::control
}  // namespace carma


#endif
