// $Id: TsysStage.cc,v 1.17 2014/06/04 17:09:27 mpound Exp $

#include "carma/pipeline/TsysStage.h"

#include "carma/correlator/lib/CorrelatorSideband.h"
#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/monitor/AstroSubsystem.h"
#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/ControlCorrelEnum.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/PipelineCommon.h"
#include "carma/monitor/PipelineMonitorInput.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/pipeline/Tsys.h"
#include "carma/services/Global.h"

#include "carma/util/CorrelatorSet.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedExclusiveLock.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <algorithm>
#include <boost/foreach.hpp>
#include <iostream>
#include <set>

using namespace boost;
using namespace carma;
using namespace carma::correlator::lib;
using namespace carma::pipeline;
using namespace carma::services;
using namespace carma::util;
using namespace std;


// EML: WTF?  Had to add this or else code doesn't compile with
// EML_NEW_CORR defined, even though the definition of median() below
// in the anonymous namespace, or the code that uses it, hasn't
// changed

double medianLocal( vector< double > & data ) 
{
  const vector< double >::size_type size = data.size();
  
  if ( !size ) 
    throw CARMA_EXCEPTION( ErrorException, 
               "Median of empty data set is undefined." );
  
  const set< double >::size_type n = size / 2; 
  std::sort( data.begin(), data.end() );
  if ( size % 2 == 0 ) // Even
    return ( data[ n ] + data[ n - 1 ] ) / double( 2 );
  else // Odd
    return data[ n ]; 
} // median

namespace { // Anonymous

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;
    const Trace::TraceLevel TRACE_EXCEPTIONS = Trace::TRACE2;
    const Trace::TraceLevel TRACE_TIMING = Trace::TRACE2;
    const Trace::TraceLevel TRACE_CALIBRATION = Trace::TRACE5;

    const string CLASS_NAME( "TsysStage" );

    const enum monitor::MonitorPoint::VALIDITY NO_HW =
        monitor::MonitorPoint::INVALID_NO_HW;
    const enum monitor::MonitorPoint::VALIDITY NO_DATA =
        monitor::MonitorPoint::INVALID_NO_DATA;

    typedef carma::monitor::NoiseStatusMonitorPointEnum NoiseStatusEnum; 
    typedef carma::monitor::NoiseStatusMonitorPointEnum NSE; 
    typedef carma::monitor::MonitorPoint MP;

    monitor::AstroSubsystemBase::Rx &
    getAstroRx( const int bandNo, 
                const monitor::AntPolPair & antPol,
                monitor::AstroSubsystem & astro )
    {
        const int antIdx = antPol.first - 1;
        const int bandIdx = bandNo - 1;

        if ( antPol.second == monitor::PolMPE::L ||
             antPol.second == monitor::PolMPE::V ||
             antPol.second == monitor::PolMPE::H )
        {
            return astro.antenna( antIdx ).band( bandIdx ).leftPol();
        } else if ( antPol.second == monitor::PolMPE::R ) {
            return astro.antenna( antIdx ).band( bandIdx ).rightPol();
        } else {
            throw CARMA_EXCEPTION( ErrorException, "Invalid Pol." );
        }
    }


    void applyCalibrationToBand( 
        CorrelatorBand & cb,
        const TsysPipelineInfo & tsysInfo,
        const bool applyTsys,
        const bool applyFlux,
        const NoiseStatusEnum::NOISESTATUS noiseStatus,
        const bool log )
    {
        if ( ( !applyTsys && !applyFlux ) || noiseStatus == NSE::CHANGING ) 
            return;

        double calFactor = 1.0;

        const int bandNo = cb.getBandNumber( );
        BaselineVector & cba = cb.getBaselines( );
        const BaselineVector::size_type numBaselines = cba.size( );

        std::set< int > seenInputs;

        BaselineVector::size_type baselineIdx = 0;
        for ( ; baselineIdx < numBaselines; ++baselineIdx ) { // Baselines

            const int input1 = cba.at( baselineIdx ).getInput1Number();
            const int input2 = cba.at( baselineIdx ).getInput2Number();

            if ( log ) {

                if ( seenInputs.find( input1 ) == seenInputs.end() ) {
                    ostringstream oss;
                    oss << "Calibration info for astroband " << bandNo 
                        << " astroinput " << input1 << ": "
                        << tsysInfo.getTsys( bandNo, input1 ).getDetails() 
                        << ".";
                    programLogInfoIfPossible( oss.str() );
                    seenInputs.insert( input1 );
                }
                
                if ( seenInputs.find( input2 ) == seenInputs.end() ) {
                    ostringstream oss;
                    oss << "Calibration info for astroband " << bandNo 
                        << " astroinput " << input2 
                        << tsysInfo.getTsys( bandNo, input2 ).getDetails() 
                        << ".";
                    programLogInfoIfPossible( oss.str() );
                    seenInputs.insert( input2 );
                }

            } // if ( log ) 

            SidebandVector & sidebands = cba.at( baselineIdx ).getSidebands();

            BOOST_FOREACH( CorrelatorSideband & sideband, sidebands ) {

                if ( sideband.isAuto() ) 
                    continue; // Don't calibrate autospectra

                if ( noiseStatus == NSE::ENABLED && applyFlux ) {
                    calFactor = 100.0;
                } else if ( noiseStatus == NSE::DISABLED && applyTsys ) {

                    std::pair< bool, bool > blValidity;

                    calFactor = tsysInfo.getBaselineTsys( 
                        input1,
                        input2,
                        bandNo,
                        sideband.isUSB( ),
                        blValidity );

                    if ( applyFlux )  
                        calFactor *= tsysInfo.getBaselineJanskysPerKelvin( 
                            input1, 
                            input2,
                            bandNo );

                    if ( !blValidity.first )
                        sideband.flagData( CorrelatorSideband::A1_TSYS_BAD );

                    if ( !blValidity.second )
                        sideband.flagData( CorrelatorSideband::A2_TSYS_BAD );

                 } else {
                    continue;
                 }

                ::std::transform( 
                        sideband.getData( ).begin( ),  
                        sideband.getData( ).end( ),
                        sideband.getData( ).begin( ),
                        ::std::bind2nd( ::std::multiplies< complex<float> >( ),
                            complex<float>( calFactor ) ) );

                // Recompute stats for this sideband (avg, variance, etc)
                sideband.computeStats( );

            } // End loop over sidebands
        } // End loop over baselines.
    }

    template< typename T >
    T median( vector< T > & data ) 
    {
        const typename vector< T >::size_type size = data.size();

        if ( !size ) 
            throw CARMA_EXCEPTION( ErrorException, 
                                   "Median of empty data set is undefined." );

        const typename set< T >::size_type n = size / 2; 
        std::sort( data.begin(), data.end() );
        if ( size % 2 == 0 ) // Even
            return ( data[ n ] + data[ n - 1 ] ) / T( 2 );
        else // Odd
            return data[ n ]; 
    } // median
        
} // End namespace <unnamed>

/**.......................................................................
 * Constructor.
 */
TsysStage::TsysStage( 
            carma::monitor::PipelineSubsystem & monitor, 
            const carma::monitor::PipelineMonitorInput & plmi, 
            const enum carma::pipeline::PipelineType plType,
            carma::monitor::AstroSubsystem & astroMonitor ) : 
    Stage( monitor.getTsysStageStats( ), "Tsys" ), 
    plmi_( plmi ),
    tsysInfo_( plType ),
    applyTsys_( true ),
    applyFlux_( true ),
    resetPendingAntNoVec_( ),
    noiseStatus_( NoiseStatusEnum::DISABLED ),
    noiseStatusValid_( false ),
    monitorData_( monitor ),
    astroMonitor_( astroMonitor ),
    plType_( plType ),
    logBandOnce_( 0 )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "TsysStage::TsysStage( ) - Constructor." );
}

/**.......................................................................
 * Destructor.
 */
TsysStage::~TsysStage() 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "TsysStage::~TsysStage( ) - Destructor." );
}

void TsysStage::fillMonitorData() 
{
  carma::monitor::TsysStage & tsysStageMon = monitorData_.getTsysStage( );

  {
    ScopedSharedLock< PthreadRWLock > scopelock( applyTsysAndFluxRWLock_ ); 
    tsysStageMon.tsysCalActive().setValue( applyTsys_ );
    tsysStageMon.fluxCalActive().setValue( applyFlux_ );
  } 

  if ( noiseStatusValid_ ) 
      tsysStageMon.noiseStatus().setValue( noiseStatus_ );

  // fill in computed Tsys quantities
  typedef map< int, vector< double > > MedianTsysMap;
  MedianTsysMap medianTsysMap;

  vector< monitor::PolType > pols;
  pols.push_back( monitor::PolMPE::L );
  pols.push_back( monitor::PolMPE::R );

  const AstrobandRange abRange( getAstrobandRange( plType_ ) ); 
  for ( unsigned ab = abRange.first; ab <= abRange.second; ++ab ) {
    
      const int antCount = monitor::AstroSubsystemBase::antennaCount();
      for ( int antNo = 1; antNo <= antCount; ++antNo ) {

        BOOST_FOREACH( const monitor::PolType pol, pols ) { 

            const monitor::AntPolPair antPol( antNo, pol );

            monitor::AstroSubsystem::Rx & rx 
                = getAstroRx( ab, antPol, astroMonitor_ );

            if ( !plmi_.signalPathMapped( ab, antPol ) ) {

              rx.usb().tsys().setValidity( NO_HW );
              rx.lsb().tsys().setValidity( NO_HW );
              rx.tdsb().setValidity( NO_HW );
              rx.effectiveTcal().setValidity( NO_HW );
              rx.ambPsys().setValidity ( NO_HW );

            } else {

              const int astroInputNo = plmi_.getAstroInputNo( ab, antPol );

              const pipeline::Tsys & tsys = 
                tsysInfo_.getTsys( ab, astroInputNo );

              rx.usb().tsys().setValue( tsys.getTsysUsb() );
              rx.lsb().tsys().setValue( tsys.getTsysLsb() );
              rx.tdsb().setValue( tsys.getTsysDsb() );
              rx.effectiveTcal().setValue( tsys.getEffectiveTcal() );
              rx.ambPsys().setValue( tsys.getAmbPsysdBm() );

              if ( !plmi_.isRxInSideband( ab, antPol, true ).second ) 
                rx.usb().tsys().setValidity( NO_HW );

              if ( !plmi_.isRxInSideband( ab, antPol, false ).second ) 
                rx.lsb().tsys().setValidity( NO_HW );
              
              // Add this to our median map too if valid.
              if ( tsys.valid() ) 
                  medianTsysMap[ antPol.first ].push_back( tsys.getTsysDsb() );
            }
        } // Loop over Pols
      } // Loop over antenna #s 
  } // Loop over astrobands

  // Loop over antennas once again and set median Tsys
  const int antCount = monitor::AstroSubsystemBase::antennaCount();
  for ( int antNo = 1; antNo <= antCount; ++antNo ) {

    const int antIdx = antNo - 1;

    typedef carma::monitor::CorrelatorDesignationMonitorPointEnum CorrDesMPE;
    typedef CorrDesMPE::CORRELATORDESIGNATION CorrDesEnum;

    monitor::MonitorPointFloat & medTdsb = 
        astroMonitor_.antenna( antIdx ).medianTdsb();

    // Though I calculate a median tsys in both pipelines, only one
    // can be the canonical value to display on the composite page. So, 
    // we check that the antenna's owning subarray's correlator designation
    // matches that of this pipeline in order to write it. Note that I can't
    // just check antenna correlator designation because this can be 'ANY'
    // in some oddball signal path configurations despite the fact that the
    // subarray does not contain both correlators.  If the antenna's owning 
    // subarray's corr designation is still ANY, I only write the 
    // SPECTRAL LINE version.  Got it? 
    
    const MonitorCorrelatorDesignation antCorrDes = plmi_.getAntSubarrayCorrDes( antNo );

    const MonitorCorrelatorDesignation plCorrDes( getMonCorrDes( plType_ ) );
    carma::util::CorrelatorSet antCorrSet(antCorrDes);

    // This version generalizes Andy's logic to check whether the
    // antenna's correlator set includes this pipeline's
    // correlator, and to publish the coherence only if this
    // correlator is the first one in the antenna's correlator
    // set

    if(antCorrSet == plCorrDes ||
       (antCorrSet.includes( plCorrDes ) && antCorrSet.firstCorrelatorIs( plCorrDes )))
    {
      const MedianTsysMap::iterator tsysAccIt = medianTsysMap.find( antNo );
      if ( tsysAccIt == medianTsysMap.end( ) ) {
        medTdsb.setValue( 10000.0 );
      } else {
        if ( !tsysAccIt->second.empty() )
          medTdsb.setValue( medianLocal( (std::vector<double>&)tsysAccIt->second ) );
      }
    } else if(antCorrSet.isEmpty()) {
      medTdsb.setValidity( NO_HW );
    }

  }
    
}

void
TsysStage::resetTsys( const std::vector< int > & resetPendingAntNos )
{
    ScopedPthreadMutexLock scopelock( resetPendingMutex_ );
    BOOST_FOREACH( const short antNo, resetPendingAntNos ) {

        if ( antNo == 0 ) {
            throw CARMA_EXCEPTION( ErrorException, 
                "0 is not a valid antenna number here as subarray membership "
                "must be determined by caller (the control system)!" );
        }
            
        // Ignore duplicates - just results in resetting twice.

        resetPendingAntNoVec_.push_back( antNo );
    } 
}

void
TsysStage::applyTsysToData( const bool apply ) 
{
    ScopedExclusiveLock< PthreadRWLock > scopelock( applyTsysAndFluxRWLock_ ); 
    applyTsys_ = apply;
}

void
TsysStage::applyFluxCalibrationToData( const bool apply ) 
{
    ScopedExclusiveLock< PthreadRWLock > scopelock( applyTsysAndFluxRWLock_ ); 
    applyFlux_ = apply;
}

bool
TsysStage::isTsysBeingApplied( ) const
{
    ScopedSharedLock< PthreadRWLock > scopelock( applyTsysAndFluxRWLock_ );
    return applyTsys_;
}

void
TsysStage::logCalibration( const short astroband )
{
    logBandOnce_ = astroband;
}

bool
TsysStage::isFluxBeingApplied( ) const
{
    ScopedSharedLock< PthreadRWLock > scopelock( applyTsysAndFluxRWLock_ );
    return applyFlux_;
}

const TsysPipelineInfo &
TsysStage::getTsysPipelineInfo( ) const
{
    return tsysInfo_;
}

void
TsysStage::preprocess( CorrelatorDataPtr cd )
{
    CARMA_CPTRACE( TRACE_TIMING, "TsysStage::preprocessCorrelatorData( )." );
    
    { 
        // Note we don't mess around with low level per band per input locks
        // so anything which effects state at that resolution must be done
        // atomically at a known time in processing rather than completely 
        // asynchronously, hence the resetPending logic.
        ScopedPthreadMutexLock scopelock( resetPendingMutex_ );
        if ( !resetPendingAntNoVec_.empty() ) {
            tsysInfo_.resetTsys( resetPendingAntNoVec_, plmi_ );
            resetPendingAntNoVec_.clear( );
        }
    }

    // Locking applyTsysMutex here assures that either all the data for
    // this frame is calibrated or none of it is.
    ScopedSharedLock< PthreadRWLock > scopelock( applyTsysAndFluxRWLock_ );

    const int numBands   = cd->getNumberOfBands();

    const vector<CorrelatorBand>& cb = cd->getBands();

    if ( static_cast<int>( cb.size( ) ) != numBands ) {
        ostringstream msg;
        msg << "TsysStage::processTheData( ) "
            << " - cd->getNumberOfBands() returned " << numBands 
            << " but the vector contains " << cb.size( ) << " elements!";
        programLogErrorIfPossible( msg.str( ) ); 
    }

    const frameType monFC = getDataFrameCount( );

    CARMA_CPTRACE( TRACE_TIMING, "TsysStage::processTheData() - "
            "Retrieving monitor info for correlator band FC=" 
            << static_cast<long int>( getDataFrameCount( ) ) 
            << ", monitor data FC=" << monFC << " @"
            << Time::getTimeString( 2 ) << " ." );


    // Update noiseStatus
    const NoiseStatusEnum & noiseStat = 
        plmi_.noiseStatus( getMonCorrDes( plType_ ) );
    noiseStatus_ = noiseStat.getValue();
    noiseStatusValid_ = noiseStat.isValid();
    if ( noiseStatusValid_ && noiseStatus_ == NoiseStatusEnum::DISABLED )
        tsysInfo_.updateWithPipelineMonitorInput( plmi_ );
}

void
TsysStage::processBand( CorrelatorBand * cb )
{
    CARMA_CPTRACE( TRACE_TIMING, "TsysStage::processCorrelatorBand( )." );
    bool localApplyTsys; // Keep the shared lock as little as possible.
    bool localApplyFlux; // Keep the shared lock as little as possible.
    {
        ScopedSharedLock< PthreadRWLock > scopelock( applyTsysAndFluxRWLock_ );
        localApplyTsys = applyTsys_;
        localApplyFlux = applyFlux_;
    }
    
    applyCalibrationToBand( *cb, tsysInfo_,
                            localApplyTsys, 
                            localApplyFlux,
                            noiseStatus_,
                            cb->getBandNumber() == logBandOnce_ );

    logBandOnce_ = -1;
}

CorrelatorDataPtr
TsysStage::postprocess( const CorrelatorDataPtr cd )
{
    CARMA_CPTRACE( TRACE_TIMING, "TsysStage::postprocessCorrelatorData( )." );
    return cd;
}
