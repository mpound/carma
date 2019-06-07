#ifndef CARMA_CORRELATOR_OBSRECORD2_DEFAULTCORRCONTROL_H
#define CARMA_CORRELATOR_OBSRECORD2_DEFAULTCORRCONTROL_H

//! @file
//! @brief Interface file for DefaultCorrControl class.
//! @author Rick Hobbs

#include "carma/corba/corba.h"
#include "carma/correlator/obsRecord2/CorDataBase.h"
#include "carma/util/PhaseSwitchingImpl.h"
#include "carma/util/PthreadMutex.h"

#include <memory>
#include <vector>


namespace carma {

namespace corba {
    class Server;
}

namespace correlator {


namespace lib {
    class CorrelatorConfigChecker;
    class CorrelatorData;
}


namespace obsRecord2 {


class BandStatus;

//! @brief Class responsible for handling all CORBA type functionality
//!        related to the correlator.
//! Method calls to correlator library are dispatched through delegation
//! in the CobraClient class, defined in DefaultCorrControl.cc.
//! When adding a new operation, follow the existing pattern of
//! creating a subclass for that operation request, e.g.
//! DefaultCorrControl::CobraClient::MyOperationRequest : public OpReq
class DefaultCorrControl : public carma::util::PhaseSwitchingImpl
{
    public:
        //! @brief Constructor
        DefaultCorrControl( int controlPort );

        //! @brief Destructor
        virtual ~DefaultCorrControl( );

        //! @brief Start the control server (doesn't block).
        void startControlServer( carma::corba::Server & server,
                                 const ::std::string & servedObjName,
                                 void (*shutdownCallback)( void * ),
                                 void * shutdownCallbackArg );

        //! @brief Sets the spectral bandwidth mode for the band.
        //!
        //! @param bandwidth New spectral bandwidth mode.
        //! @param seqNo Sequence number (for monitor system  feedback)
        //! @param astroBandNo The astroband number
        //!
        //! @throw carma::util::UserException
        void setBandwidth( obsRecord2::BandWidthType bandwidth,
                           obsRecord2::FpgaModeType  fpgaMode,
                           CORBA::Long               seqNo ,
                           CORBA::Long               astroBandNo);

        //! @brief Vectorized version of setBandwidth command for CARMA3G correlator.
        //! The C3G correlator requires all bands be configured simultaneously,
        //! hence a vectorized setBandwidth is needed.
        //! @throw carma::util::UserException
        void setBandwidthVector(const BandWidthSeq & bandwidth,
                          const FpgaModeSeq  & fpgaMode,
                          CORBA::Long          seqNo ,
                          const carma::util::SeqLong      & astroBandNo );

        //! @brief Updates the current status of the noise source.
        //!
        //! @param isOn Whether the noise source is presently on.
        //!
        //! @throw carma::util::UserException
        void setNoiseSourceState( CORBA::Boolean isOn );


        //! @brief Set Input Delay coefficients
        //!
        //! @throw carma::util::UserException
        void setInputDelayTriplets( const DelayTripletSeq & tripletSeq );

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
                const carma::util::SeqLong & cols90,
                const carma::util::SeqLong & cols180,
                CORBA::Long nStates90,
                CORBA::Long nStates180,
                CORBA::Boolean noiseEnabled);

        //! @brief Specifies the downconverter frequency and sideband
        //!        in use for the band.
        //!
        //! @param freq Downconverter LO frequency (GHz).
        //! @param sb Sideband in use (upper or lower).
        //! @param bdcEnabled Block downconverter enabled
        //!
        //! @throw carma::util::UserException
        void setDownconverterSettings(
          CORBA::Double                               freq,
          carma::correlator::obsRecord2::SidebandType sb,
          CORBA::Boolean  bdcEnabled );

        /**
         * @brief Vectorized version of setDownconverterSettings command for CARMA3G correlator.
         * @param freq Sequence of downconverter LO frequencies (GHz).
         * @throw carma::util::UserException
         */
        void setDownconverterSettingsVector( const carma::util::SeqFloat & freq );

        //! @brief Optimize CARMA digitizer thresholds.
        //! CARMA boards only; no-op for COBRA boards
        //!
        //! @throw carma::util::UserException
        void optimizeThresholds( CORBA::Long seqNo );

        //! @brief Initiates the correlator hardware phase flattening routine.
        //! CARMA boards only; no-op for COBRA boards
        //! @throw carma::util::UserException
        //! @pre Noise source must be on
        void flattenPhases( CORBA::Long seqNo );

        //! @brief Initiates the correlator hardware spectra
        //! calibration routine.
        // CARMA boards only; No-op for COBRA hardware boards.
        // @param enable Enable calibration or not
        // @param cache  Whether to cache new calibration spectra.
        // @param count  Integration time for calibration spectra (in frames).
        // @param seqNo  Command sequence number for monitor stream reporting.
        void calibrateSpectra( CORBA::Boolean enable,
                               CORBA::Boolean cache,
                               CORBA::Long count,
                               CORBA::Long seqNo  );

        //! @brief Enables or disables correlations on COBRA boards.
        //! This is for thermal protection when the correlator room.
        //! gets too hot.  No-op for CARMA boards.
        // @param enable True to enable correlation, false to disable.
        void enableCorrelation( CORBA::Boolean enable );

 protected:

        class CobraClient;
        struct AccumDelaySamps;

        typedef enum {
            INVALID_NOISE_SOURCE_STATE,
            ON_NOISE_SOURCE_STATE,
            OFF_NOISE_SOURCE_STATE
        } NoiseSourceState;

        util::PthreadMutex guard_;

        // Control connection to the correlator
        ::std::auto_ptr< CobraClient > cobraClient_;

        NoiseSourceState   requestedNoiseSourceState_;
        ::std::auto_ptr< AccumDelaySamps > accumDelaySamps_;
};


}  // namespace carma::correlator::obsRecord2
}  // namespace carma::correlator
}  // namespace carma


#endif
