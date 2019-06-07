#ifndef CARMA_CONTROL_VLBI_HANDLE_H
#define CARMA_CONTROL_VLBI_HANDLE_H

/**
 * @file
 *
 * Carma control interface to the VLBI beam former
 *
 * This class copies too much of the functionality of
 * carma::control::CorrelatorHandle.  It would be cleaner to create an
 * intermediate base class from which both VlbiHandle and CorrelatorHandle
 * inherit.
 *
 * $CarmaCopyright$
 *
 */

#include <vector>

#include "carma/corba/corba.h"
#include "carma/control/RemoteObjHandleT.h"
#include "carma/control/CorrelatorInfo.h"
#include "carma/monitor/ControlSubsystem.h"
#include "carma/monitor/NoiseSource.h"
#include "carma/correlator/obsRecord2/CorDataBase.h"
#include "carma/util/corrUtils.h"


namespace carma {
namespace control {


// Vlbi handle implements the Correlator_I interface per
// DaveM's email of March 11, 2011
// Note the Correlator_I interface gets its remote object name
// from carma/conf/slcorrelator.conf.   Your DO will have to
// set its remove name
typedef RemoteObjHandleT< correlator::obsRecord2::Correlator_I > 
        CorrelatorControlRemoteObjHandle;


//! @brief Manages correlator control DO connections
class VlbiHandle : public CorrelatorControlRemoteObjHandle {
    public:
        /**
         * Constructor
         *
         * @param monitorSystem monitor system reference which allows this
         *                      handle to get a reference to its own monitor
         *                      stream.
         * This constructor will throw ErrorException if
         * the Band hardware type comes back as HARDWARE_TYPE_UNKNOWN.
         * @see utils/corrUtils.h
         */
        VlbiHandle(
// Dave - 
// I removed the bool spectral argument here, assuming you don't need it.  
// -- Marc
            unsigned int                               band,
            monitor::MonitorSystem &                   monitorSys,
            monitor::ControlSubsystemBase::Reachable & reachable,
            bool                                       defaultLogIfNotReach );
    
        virtual ~VlbiHandle( );

        //! set input delay triplets for a single polarization.
        //! @note The vector argument is passed by value so we can safely
        //!       insure that it is fully and deeply copied across the thread
        //!       barrier when this method is forked out to the worker pool
        void setInputDelayTriplets(
            ::std::vector< correlator::obsRecord2::DelayTriplet > triplets );

        //! @return True if the band has been marked offline, false if
        //! it is marked online,
        //! REGARDLESS OF WHETHER OR NOT THE REMOTE OBJECT IS REACHABLE.
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
        //! @param astroBandMode The FPGA configuration mode: CORR_SINGLEPOL, CORR_DUALPOL, CORR_FULLPOL, or CORR_CARMA23
        void setAstroBandMode( util::CorrelatorFpgaModeType astroBandMode ) { astroBandMode_ = astroBandMode; }

        //! Get the FPGA mode of this band.
        //! @return The FPGA configuration mode: CORR_SINGLEPOL, CORR_FULLPOL, or CORR_CARMA23
        util::CorrelatorFpgaModeType getFpgaMode( ) const { return astroBandMode_; }

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
        const unsigned int bandNo_;
        // Needed??
        util::CorrelatorFpgaModeType astroBandMode_;
    
};


}  // namespace carma::control
}  // namespace carma


#endif
