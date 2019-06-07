#ifndef CARMA_MONITOR_PIPELINESUBSYSTEMTEMPLATE_H
#define CARMA_MONITOR_PIPELINESUBSYSTEMTEMPLATE_H

#include "carma/monitor/PipelineSubsystem.h"

#include "carma/util/Trace.h"

namespace carma {
namespace monitor {

    /**
     * Templatized implementation of PipelineSubsystem interface.
     */
    template < typename S >
    class PipelineSubsystemTemplate : public PipelineSubsystem {
    public:

        explicit PipelineSubsystemTemplate( );

        explicit PipelineSubsystemTemplate( S & pl );

        virtual ~PipelineSubsystemTemplate( );

        /**
         * Retrieve band count - differs among wb and sl systems.
         */
        int getBandCount( ) const;

        carma::monitor::PipelineStatus &
        getPipelineStatus( ) const;

        carma::monitor::StageStats &
        getCatchDataStageStats( ) const;

        carma::monitor::CatchDataStage & 
        getCatchDataStage( ) const;

        carma::monitor::CatchDataBand &
        getCatchDataBand( int bandIdx ) const;

        carma::monitor::StageStats & 
        getCoherenceStageStats( ) const;

        carma::monitor::StageStats &
        getDecimationStageStats( ) const;

        /**
         * Retrieve a reference to common Decimation container.
         */
        carma::monitor::Decimation &
        getDecimation( int bandIdx ) const;

        carma::monitor::SelfCal &
        getSelfCal( int bandIdx, bool usb ) const;

        carma::monitor::StageStats &
        getPassBandStageStats( ) const;

        carma::monitor::StageStats &
        getTsysStageStats( ) const;

        carma::monitor::TsysStage &
        getTsysStage( ) const;

        carma::monitor::StageStats &
        getBlankFlagStageStats( ) const;

        carma::monitor::BlankFlagStage &
        getBlankFlagStage( ) const;

        carma::monitor::StageStats &
        getLinelengthStageStats( ) const;

        carma::monitor::StageStats &
        getIFcorrectionStageStats( ) const;

        carma::monitor::StageStats &
        getWvrStageStats( ) const;

        carma::monitor::StageStats &
        getSelfCalStageStats( ) const;

        carma::monitor::StageStats &
        getCorrelatorPublisherStageStats( ) const;

        carma::monitor::CorrelatorPublisherStage &
        getCorrelatorPublisherStage( ) const;

        carma::monitor::StageStats &
        getIntegratorStageStats( ) const;

        carma::monitor::IntegratorStage &
        getIntegratorStage( ) const;

        carma::monitor::StageStats &
        getVisBrickStageStats( ) const;

        carma::monitor::VisBrickStage &
        getVisBrickStage( ) const;

       /**
         * Retrieve reference to lastIntegration container
         */
        carma::monitor::LastIntegration& lastIntegration() const;

        /**
         * Start the monitor system auto writer (automatically writes 
         * data to the Frame Scriber Publisher every half second).
         */
        void startAutoWriter( float delay ); 

        /**
         * Stop the autowriter.
         */
        void stopAutoWriter( );

        /**
         * Check to see if the autowriter is alive.
         */
        bool autoWriterIsAlive() const;

        /**
         * Write frame data
         * Use when not using autowriter.
         */
        void write( );

    protected:

        // Nothing protected

    private:

        // Disallow copying - especially important with auto_ptr.
        PipelineSubsystemTemplate( const PipelineSubsystemTemplate & ); 
        PipelineSubsystemTemplate & operator=(
            const PipelineSubsystemTemplate & );

        // ORDER DEPENDENCY .
        ::std::auto_ptr<S> pipelineStorage_;
        S & pipeline_;
        // END ORDER DEPENDENCY .

    }; // End class PipelineSubsystem
} // End namespace monitor
} // End namespace carma
            
// ========================== Implementation Only ==============================

template< typename S>
inline
carma::monitor::PipelineSubsystemTemplate< S >::PipelineSubsystemTemplate( ) 
    : // ORDER DEPENDENCY
        pipelineStorage_( new S ), 
        pipeline_( *( pipelineStorage_.get( ) ) )
      // END ORDER DEPENDENCY
{ }

template< typename S>
inline
carma::monitor::PipelineSubsystemTemplate< S >::PipelineSubsystemTemplate(
    S & pl ) : pipelineStorage_( ), pipeline_( pl )
{ 
    CARMA_CPTRACE( carma::util::Trace::TRACE3, "~PipelineSubsystemTemplate" );
}
    
template< typename S>
inline
carma::monitor::PipelineSubsystemTemplate< S >::~PipelineSubsystemTemplate( ) 
{ }
        
template< typename S>
inline
carma::monitor::PipelineStatus &
carma::monitor::PipelineSubsystemTemplate< S >::getPipelineStatus( ) const
{
    return pipeline_.pipelineStatus( );
}

template< typename S>
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::getCatchDataStageStats( ) const
{
    return pipeline_.catchDataStageContainer( ).stageStats( );
}

template< typename S>
inline
carma::monitor::CatchDataStage & 
carma::monitor::PipelineSubsystemTemplate< S >::getCatchDataStage( ) const
{
    return pipeline_.catchDataStageContainer( ).catchDataStage( );
}

template< typename S >
inline
carma::monitor::CatchDataBand &
carma::monitor::PipelineSubsystemTemplate< S >::getCatchDataBand( 
    const int bandIdx ) const
{
    return pipeline_.catchDataBandContainer( bandIdx ).catchDataBand();
}
        
template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::
    getCoherenceStageStats( ) const
{
    return pipeline_.coherenceStageContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::
    getDecimationStageStats( ) const
{
    return pipeline_.decimatorStageContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::Decimation &
carma::monitor::PipelineSubsystemTemplate< S >::getDecimation( 
    const int bandIdx ) const
{
    return pipeline_.decimationBandContainer( bandIdx ).decimation( );
}

template< typename S >
inline
carma::monitor::SelfCal &
carma::monitor::PipelineSubsystemTemplate< S >::getSelfCal( 
    const int bandIdx, const bool usb ) const
{
    if ( usb )
        return pipeline_.selfCalUsbIntegContainer( bandIdx ).selfCal( ); 
    else
        return pipeline_.selfCalLsbIntegContainer( bandIdx ).selfCal( );
}

template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::getPassBandStageStats( ) const
{
    return pipeline_.passBandStageContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::getTsysStageStats( ) const
{
    return pipeline_.tsysStageContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::TsysStage &
carma::monitor::PipelineSubsystemTemplate< S >::getTsysStage( ) const
{
    return pipeline_.tsysStageContainer( ).tsysStage( );
}

template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::getBlankFlagStageStats( ) const
{
    return pipeline_.blankFlagStageContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::BlankFlagStage &
carma::monitor::PipelineSubsystemTemplate< S >::getBlankFlagStage( ) const
{
    return pipeline_.blankFlagStageContainer( ).blankFlagStage( );
}

template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::getLinelengthStageStats( ) const
{
    return pipeline_.linelengthStageContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::
    getIFcorrectionStageStats( ) const
{
    return pipeline_.iFcorrectionStageContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::getWvrStageStats( ) const
{
    return pipeline_.wvrStageContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::getSelfCalStageStats( ) const
{
    return pipeline_.selfCalIntegStageContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::
    getCorrelatorPublisherStageStats( ) const
{
    return pipeline_.correlatorPublisherContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::CorrelatorPublisherStage &
carma::monitor::PipelineSubsystemTemplate< S >::
    getCorrelatorPublisherStage( ) const
{
    return pipeline_.correlatorPublisherContainer().correlatorPublisherStage();
}

template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::getIntegratorStageStats( ) const
{
    return pipeline_.integratorStageContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::IntegratorStage &
carma::monitor::PipelineSubsystemTemplate< S >::getIntegratorStage( ) const
{
    return pipeline_.integratorStageContainer( ).integratorStage( );
}

template< typename S >
inline
carma::monitor::StageStats &
carma::monitor::PipelineSubsystemTemplate< S >::getVisBrickStageStats( ) const
{
    return pipeline_.visBrickStageContainer( ).stageStats( );
}

template< typename S >
inline
carma::monitor::VisBrickStage &
carma::monitor::PipelineSubsystemTemplate< S >::getVisBrickStage( ) const
{
    return pipeline_.visBrickStageContainer( ).visBrickStage( );
}

template< typename S >
inline
int 
carma::monitor::PipelineSubsystemTemplate< S >::getBandCount( ) const 
{
    return S::catchDataBandContainerCount( );
}

template< typename S >
inline
carma::monitor::LastIntegration &
carma::monitor::PipelineSubsystemTemplate< S >::lastIntegration() const
{
    return pipeline_.lastIntegration();
}

template< typename S>
inline
void 
carma::monitor::PipelineSubsystemTemplate< S >::startAutoWriter( float delay ) 
{
    pipeline_.startAutoWriter( delay );
}

template< typename S>
inline
void 
carma::monitor::PipelineSubsystemTemplate< S >::stopAutoWriter( ) 
{
    pipeline_.stopAutoWriter( );
}

template< typename S>
inline
bool
carma::monitor::PipelineSubsystemTemplate< S >::autoWriterIsAlive( ) const 
{
    return pipeline_.autoWriterIsAlive( );
}

template< typename S>
inline
void 
carma::monitor::PipelineSubsystemTemplate< S >::write( ) 
{
    pipeline_.writeWithoutResettingValidities( );
}

#endif
