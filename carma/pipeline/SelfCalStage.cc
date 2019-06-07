// $Id: SelfCalStage.cc,v 1.14 2014/08/19 16:54:48 control Exp $

#include "carma/pipeline/SelfCalStage.h"

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/monitor/AstroSubsystemExt.h"
#include "carma/monitor/ControlCorrelEnum.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/CorrDesignation.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/pipeline/Tsys.h"
#include "carma/pipeline/TsysStage.h"

#include "carma/util/CorrelatorSet.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/programLogging.h"
#include "carma/util/rangeFormatting.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/services/Global.h"

#include <boost/accumulators/accumulators.hpp>
#include <boost/accumulators/statistics/stats.hpp>
#include <boost/accumulators/statistics/median.hpp>
#include <boost/foreach.hpp>
#include <complex>
#include <limits>

using namespace boost;
using namespace boost::accumulators;
using namespace carma;
using namespace carma::correlator::lib;
using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::services;
using namespace carma::util;
using namespace log4cpp;
using namespace std;
    
namespace {


    typedef ::std::vector< ::std::complex<float> > VisDataVector;
    typedef ::std::vector< Selfcal > SelfCalBandVector;

    typedef stats< accumulators::tag::median > MedianStats;
    typedef accumulator_set< double, MedianStats > DoubleMedianAccumulator;
    typedef ::std::map< int, DoubleMedianAccumulator > DoubleMedianAccumMap;

    const enum MonitorPoint::VALIDITY NO_HW = MonitorPoint::INVALID_NO_HW;
    const enum MonitorPoint::VALIDITY NO_DATA = MonitorPoint::INVALID_NO_DATA;

    const string CLASS_NAME( "SelfCalStage" );

    const complex< double > MAX_ERROR( 100.0, 100.0 );
    const float MAX_SNR = 100.0;
    const double S_PER_MS = 1.e-3;

    const Trace::TraceLevel TRACE_SELF_CAL = Trace::TRACE7;
    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE7;

    const int ERROR_THRESH = 36000; // Once every half hour @ 10 errs/frame.
    const int MAX_ITER = 100;
        
    void
    tallyAntSelfCalMedians( 
        DoubleMedianAccumMap & ampMedAccumMap,
        DoubleMedianAccumMap & snrMedAccumMap,
        const vector< Complex > & solutions,
        const vector< double > & snrs,
        const PipelineMonitorInput & plmi,
        const pair< int, carma::monitor::PolType > astroBandPol )
    {
        // We have numerous checks to make sure these match up so if not return.
        if ( solutions.size() != snrs.size() ) return;

        for ( size_t ai = 0; ai < solutions.size(); ++ai ) {
            const int antNo = ai + 1;
            AntPolPair antPol( antNo, astroBandPol.second );
            if ( plmi.signalPathMapped( astroBandPol.first, antPol ) ) {
                ampMedAccumMap[ antNo ]( std::abs( solutions.at( ai ) ) );  
                snrMedAccumMap[ antNo ]( snrs.at( ai ) );
            }
        }
    }

    monitor::AstroSubsystem::SelfCal &
    getAstroSelfCal( AstroSubsystem & astro, 
                     const int astroBandNo, 
                     const AntPolPair & antPol,
                     const bool usb )
    {
        if ( !( antPol.second == PolMPE::L ||
                antPol.second == PolMPE::R ) ) 
        {
            ostringstream msg;
            msg << __PRETTY_FUNCTION__ << " " 
                << PolMPE::valueToString( antPol.second ) 
                << " is not supported by selfcal.";
            throw CARMA_EXCEPTION( IllegalArgumentException, msg.str() );
        }

        if ( ( antPol.first <= 0 || 
               antPol.first > AstroSubsystem::antennaCount() ) ||
             ( astroBandNo <= 0 ||
                astroBandNo > AstroSubsystem::Antenna::bandCount() ) ) {
            ostringstream msg;
            msg << __PRETTY_FUNCTION__ << " ant " << antPol.first 
                << " astroband " << astroBandNo << " does not have an "
                << "associated Astro subsystem component.";
            throw CARMA_EXCEPTION( IllegalArgumentException, msg.str() );
        }

        const bool leftPol = ( antPol.second == PolMPE::L ); 
        const int antIdx = antPol.first - 1;
        const int bandIdx = astroBandNo - 1;
        AstroSubsystem::Band & ab = astro.antenna( antIdx ).band( bandIdx );

        if ( leftPol ) 
        {
            if ( usb ) 
                return ab.leftPol().usb().selfCal();
            else
                return ab.leftPol().lsb().selfCal();
        }
        else
        {
            if ( usb )
                return ab.rightPol().usb().selfCal();
            else
                return ab.rightPol().lsb().selfCal();
        }
    } // getAstroSelfCal
        
} // End namespace

SelfCalStage::SelfCalSidebandInfo::SelfCalSidebandInfo( ) :
    nAnts_( 0 ),
    maxIter_( 0 ),
    usb_( false )
{
    // Container semantics suck
}

SelfCalStage::SelfCalSidebandInfo::SelfCalSidebandInfo( 
        const int antennas,
        const int maxIterations,
        const bool usb,
        monitor::SelfCal * mon,
        SelfCalStage * mom ) :
    selfCal_(),
    solutionValid_( false ),
    vis_( antennas, Complex(0, 0) ),
    visErrors_( antennas, Complex( 0, 0 ) ),
    snr_( antennas, 0.0 ),
    seenAnts_(),
    referenceAntennaNo_( 0 ),
    explicitRefAnt_( false ),
    bandNo_( 0 ),
    pol_( PolMPE::UNKNOWN ),
    nAnts_( antennas ),
    maxIter_( maxIterations ),
    usb_( usb ),
    mon_( mon ),
    mom_( mom )
{
    selfCal_.setMaxAnt( antennas ); 
    selfCal_.setMaxIter( maxIterations ); 

    CARMA_CPTRACE( TRACE_CTOR_DTOR, "SelfCalSidebandInfo( "  
        << "antennas=" << antennas << " )." );
}

SelfCalStage::SelfCalSidebandInfo::SelfCalSidebandInfo( 
    const SelfCalSidebandInfo & rhs ) :
    selfCal_(),
    solutionValid_( false ),
    vis_( rhs.nAnts_, Complex(0, 0) ),
    visErrors_( rhs.nAnts_, Complex( 0, 0 ) ),
    snr_( rhs.nAnts_, 0.0 ),
    seenAnts_(),
    referenceAntennaNo_( rhs.referenceAntennaNo_ ),
    explicitRefAnt_( rhs.explicitRefAnt_ ),
    bandNo_( 0 ),
    pol_( PolMPE::UNKNOWN ),
    nAnts_( rhs.nAnts_),
    maxIter_( rhs.maxIter_ ),
    usb_( rhs.usb_ ),
    mon_( rhs.mon_ ),
    mom_( rhs.mom_ )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "SelfCalSidebandInfo::SelfCalSidebandInfo Copy Ctor" );
    selfCal_.setMaxAnt( nAnts_ ); 
    selfCal_.setMaxIter( maxIter_ ); 
}

SelfCalStage::SelfCalSidebandInfo &
SelfCalStage::SelfCalSidebandInfo::operator=( 
    const SelfCalSidebandInfo & rhs )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "SelfCalSidebandInfo::operator=" );

    if ( &rhs == this ) 
        return *this;

    if ( rhs.nAnts_ != nAnts_ || rhs.usb_ != usb_ || 
         rhs.maxIter_ != maxIter_ ) 
        throw CARMA_EXCEPTION( ErrorException, "Incompatible SelfCalSbInfos." );

    solutionValid_ = false;
    vis_ = rhs.vis_;
    visErrors_ = rhs.visErrors_;
    snr_ = rhs.snr_;
    seenAnts_.clear();
    referenceAntennaNo_ = rhs.referenceAntennaNo_;
    explicitRefAnt_ = rhs.explicitRefAnt_; 
    bandNo_ = rhs.bandNo_;
    pol_ = rhs.pol_;
    mon_ = rhs.mon_;
    mom_ = rhs.mom_;
    
    selfCal_.setMaxAnt( nAnts_ ); 
    selfCal_.setMaxIter( maxIter_ ); 

    return *this;
}

void
SelfCalStage::SelfCalSidebandInfo::reset( )
{
    selfCal_.zero( ); 
    seenAnts_.clear();
    solutionValid_ = false; 
    
    // Fill monitor data in order to invalidate previous solutions in the 
    // monitor stream.  Since this is integrated data, the MPs are declared
    // persistent and there is no way to invalidate persistent data once
    // written.  Here we fillMonitorData with solutionValid = false so it 
    // will write invalid mps - if they are overwritten by the subsequent
    // solutions, great, if not, they stay invalid rather than valid.  
    fillMonitorData();

    bandNo_ = 0;
    pol_ = PolMPE::UNKNOWN;
}

void
SelfCalStage::SelfCalSidebandInfo::setVis( 
    const int antenna1No, 
    const int antenna2No,
    const complex< float > & avgVis,
    double weight )
{
    selfCal_.setVis( 
        antenna1No,
        antenna2No,
        avgVis,
        weight );

    CARMA_CPTRACE( TRACE_SELF_CAL, "setVis( antenna1No=" << antenna1No 
        << ", antenna2No=" << antenna2No << " )" );

    seenAnts_.insert( antenna1No );
    seenAnts_.insert( antenna2No );
}

void
SelfCalStage::SelfCalSidebandInfo::setBandPol(
    const int bandNo,
    const carma::monitor::PolType pol )
{
    bandNo_ = bandNo;
    pol_ = pol;
}

SelfCalStage::AstroBandPolPair
SelfCalStage::SelfCalSidebandInfo::getBandPol( ) const
{
    return AstroBandPolPair( bandNo_, pol_ );
}

void
SelfCalStage::SelfCalSidebandInfo::calculateSelfCal( double rootbtau )
{
    // Do nothing if we don't have any candidate inputs.
    if ( seenAnts_.empty() ) {
        solutionValid_ = false;
        return; 
    }

    ScopedPthreadMutexLock scopelock( refAntMutex_ );

    // We use a simple algorithm for determining the reference ant:
    // If the ref ant wasn't explicitly set and the current ref ant 
    // (which can also be 0) has not been seen - pick the next seen ant.
    // For all other cases, continue to use the current ref ant.
    if ( !explicitRefAnt_ ) {
        
        const set< int >::const_iterator refAntIt = 
            seenAnts_.lower_bound( referenceAntennaNo_ );

        if ( refAntIt == seenAnts_.end() ) 
            referenceAntennaNo_ = *(seenAnts_.begin( ) );
        else
            referenceAntennaNo_ = *refAntIt;

    } 

    selfCal_.setReferenceAntenna( referenceAntennaNo_ );
        
    vis_ = selfCal_.getVis( );
    solutionValid_ = !vis_.empty();

    ComplexVector fitErrs;

    // set gains to zero and errors to MAX_ERROR if no solution.
    if ( !solutionValid_ ) {
        vis_ = ComplexVector( nAnts_, complex<double>( 0, 0 ) );
        fitErrs = ComplexVector( nAnts_, MAX_ERROR );
    } else {
        fitErrs = selfCal_.getVisErrors( );
    }

    visErrors_.resize( fitErrs.size() ); 

    // Calculate SNRs
    const ComplexVector & gains = vis_;
    const ComplexVector & errs  = fitErrs;
    SnrVector & snrs = snr_;

    // Make sure all the sizes match up.
    // Should probably check that gains match nInputs
    if ( ( gains.size( ) > 0 ) &&  
         ( gains.size( ) != errs.size( ) ||
           gains.size( ) != snrs.size( ) ) ) {
        ostringstream msg;
        msg << "Gains vector size of "
            << gains.size( ) << " does not match either errors size "
            << errs.size( ) << " or snr vector size " << snrs.size( );
        throw CARMA_EXCEPTION( ErrorException, msg );
    }

    for ( ComplexVector::size_type i = 0; i < gains.size( ); ++i ) { 
        
        const double noise = mom_->calculateSystemNoise( rootbtau,
                                                        i + 1,
                                                        bandNo_,
                                                        usb_ );
        const complex<double> err = errs.at(i); 

        const complex<double> totalError = 
            polar( 1.0, arg( err ) ) * hypot( abs( err ), noise );

        visErrors_.at(i) = totalError;

        const double error = abs( totalError );
        if ( error > numeric_limits< float >::epsilon( ) ) {
            snrs.at( i ) = ( abs( gains[i] ) / error );
        } else {
            snrs.at( i ) = MAX_SNR;
        }

    } // Loop over inputs
}

void
SelfCalStage::SelfCalSidebandInfo::fillMonitorData( ) 
{
    string ndcMsg;
    {
        ostringstream oss;
        oss << "SelfCalStage::writeSolutionsToMonitorSystem( bandNumber="
            << bandNo_ << " )";
        ndcMsg = oss.str();
    }

    ScopedLogNdc ndc( ndcMsg );

    try {
    
        ScopedPthreadMutexLock scopelock( refAntMutex_ );
        
        // First write convergence stats now
        mon_->astroBandNo().setValue( bandNo_ );
        mon_->polarization().setValue( pol_ );
        mon_->convergence().setValue( solutionValid_ );
        mon_->iterations().setValue( static_cast<int>( selfCal_.getIter() ) );
        mon_->maxIterations().setValue( mom_->maxIterations_ );
        mon_->refAnt().setValue( static_cast<int>( referenceAntennaNo_ ) );
        const string rangeString = formatAsRanges( seenAnts_ );
        mon_->seenAnts().setValue( rangeString );

        if ( bandNo_ == 0 ) return;

        const ComplexVector::size_type visSize = vis_.size( );

        if ( visSize != vis_.size() || 
             visSize != visErrors_.size() ||
             visSize != snr_.size() ) {

            ++mom_->errors_;
            if ( mom_->errors_ % ERROR_THRESH == 1 ) {

                ostringstream errmsg;
                errmsg << "Vector sizes do not match for band " 
                       << bandNo_ << "!  " << "visSize = " << visSize 
                       << ", vis_.size() = " << vis_.size() 
                       << ", visErrors_.size() = " << visErrors_.size() 
                       << ", snr_.size() = " << snr_.size() 
                       << ".  Write will not take place.";

                programLogErrorIfPossible( errmsg.str() );
            }

            return; // Don't write!
        }

        for ( ComplexVector::size_type i = 0; i < visSize; ++i ) {

            // Now write out the Astro subsystem provided it exists
            const AntPolPair antPol( i + 1, pol_ ); 

            AstroSubsystem::SelfCal & astroSelfCal = 
                getAstroSelfCal( mom_->astroMonitor_, bandNo_, antPol, usb_ );

            if ( !mom_->plmi_.signalPathMapped( bandNo_, antPol ) ) {
                astroSelfCal.antVis().setValidity( NO_HW );
                astroSelfCal.antVisErr().setValidity( NO_HW );
                astroSelfCal.snr().setValidity( NO_HW );
                astroSelfCal.valid().setValue( false ); 
            } else if ( !mom_->plmi_.isRxInSideband( bandNo_, 
                                                     antPol, 
                                                     usb_ ).second ) {
                astroSelfCal.antVis().setValidity( NO_HW );
                astroSelfCal.antVisErr().setValidity( NO_HW );
                astroSelfCal.snr().setValidity( NO_HW );
                astroSelfCal.valid().setValue( false ); 
            } else if ( !solutionValid_ ) {
                astroSelfCal.antVis().setValue( complex< float >( 0., 0. ) );
                astroSelfCal.antVisErr().setValue(
                    static_cast< complex<float> >( MAX_ERROR ) );
                astroSelfCal.snr().setValue( 0. );
                astroSelfCal.valid().setValue( false ); 
            } else {
                astroSelfCal.antVis().setValue( 
                        static_cast< complex< float > >( vis_.at( i ) ) );
                astroSelfCal.antVisErr().setValue( 
                        static_cast< complex< float > >( visErrors_.at( i ) ) );
                astroSelfCal.snr().setValue( snr_.at( i ) );
                astroSelfCal.valid().setValue( true ); 
            } 
        }

    } catch (...) {

        ++mom_->errors_;
        if ( mom_->errors_ % ERROR_THRESH == 1 ) {
            ostringstream err;
            err << getStringForCaught() << " [" << mom_->errors_ 
                << " exceptions triggered, output every " 
                << ERROR_THRESH << " invocations]";

            programLogErrorIfPossible( err.str() );
        }

        // Stifle
    }
}

std::pair< bool, SelfCalStage::ComplexVector >
SelfCalStage::SelfCalSidebandInfo::getVis( ) const
{
    return std::make_pair( solutionValid_, vis_ );
}

SelfCalStage::SnrVector
SelfCalStage::SelfCalSidebandInfo::getSnr( ) const
{
    return snr_;
}

void
SelfCalStage::SelfCalSidebandInfo::setRefAnt( const int antNo ) 
{
    ScopedPthreadMutexLock scopelock( refAntMutex_ );
    explicitRefAnt_ = ( antNo != 0 );
    referenceAntennaNo_ = antNo;
}

SelfCalStage::SelfCalStage( 
    PipelineSubsystem& monitor,
    const TsysStage & tsys,
    carma::monitor::AstroSubsystem & astro,
    const carma::monitor::PipelineMonitorInput & plmi,
    const carma::pipeline::PipelineType plType ) :
        Stage( monitor.getSelfCalStageStats( ), "SelfCal" ),
        mon_( monitor ),
        astroMonitor_( astro ),
        tsys_( tsys ),
        plmi_( plmi ),
        maxIterations_( MAX_ITER ),
        nBands_( monitor.getBandCount() ),
        nAnts_( Global::maxAntennas() ),
        errors_( 0 ),
        plType_( plType )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "SelfCalStage( ) - Ctor." );

    for ( int monBand = 0; monBand < mon_.getBandCount(); ++monBand ) {
            
        monitor::SelfCal * usbMon = &( mon_.getSelfCal( monBand, true ) );
        monitor::SelfCal * lsbMon = &( mon_.getSelfCal( monBand, false ) );

        selfCalInfo_.push_back( UsbLsbInfoPair( 
                SelfCalSidebandInfo( nAnts_, 
                                     maxIterations_,
                                     true,
                                     usbMon,
                                     this ),
                SelfCalSidebandInfo( nAnts_, 
                                     maxIterations_,
                                     false,
                                     lsbMon,
                                     this ) ) );
    } // for band 
}

SelfCalStage::~SelfCalStage( ) 
{
    // Nothing
}

void 
SelfCalStage::setReferenceAntNo( const int antNo ) 
{
    BOOST_FOREACH( SelfCalInfoVec::value_type & info, selfCalInfo_ ) 
    {
        info.first.setRefAnt( antNo );
        info.second.setRefAnt( antNo );
    }
}

void
SelfCalStage::preprocess( const CorrelatorDataPtr cd )
{
    BOOST_FOREACH( SelfCalInfoVec::value_type & info, selfCalInfo_ ) 
    {
        info.first.reset();
        info.second.reset();
    }

    typedef multimap< int, PolType > BandPolMultimap;

    // Determine set of available SelfCalInfo indices based on currently 
    // mapped astrobands. 
    bandPolToInfoIdx_.clear();

    SelfCalInfoVec::size_type infoIdx  = 0;

    std::vector< int > astrobands = plmi_.getMappedAstroBandNumbers( );

    BOOST_FOREACH( const int astroband, astrobands )
    {
        const set< PolType > pols = plmi_.getMappedPols( astroband );
        BOOST_FOREACH( const PolType pol, pols ) {

            if ( infoIdx >= selfCalInfo_.size() ) 
                throw CARMA_EXCEPTION( ErrorException, "Too many pols!" ); 

            bandPolToInfoIdx_.insert( make_pair( make_pair( astroband, pol ), 
                                                 infoIdx ) );

            selfCalInfo_.at( infoIdx ).first.setBandPol( astroband, pol );
            selfCalInfo_.at( infoIdx ).second.setBandPol( astroband, pol );

            ++infoIdx;
        }
    }

    // Finally, determine which reference antenna to use.  We use the same
    // reference antenna for all MPs.
}

void
SelfCalStage::processBand( CorrelatorBand * cb )
{
    const int bandNo = cb->getBandNumber( );

    const double bw = 1.0e6*cb->getBandwidth(); // From MHz to Hz

    // Loop over baselines 
    const BaselineVector & baselineVec = cb->getBaselines( );

    set< SelfCalInfoVec::size_type > seenInfos;

    // Get the max integ time for any bline to use for an integ time
    // Really should be done on a per input basis
    double maxItime = 0.0;
    BOOST_FOREACH( const CorrelatorBaseline & baseline, baselineVec ) {
        processBaseline( bandNo, baseline, seenInfos, maxItime );
    }

    double rootbtau = sqrt( bw*maxItime );
    if (rootbtau < 1.0) rootbtau = 1.0;

    // Retrieve solutions and cache away for writing to monitor system. 
    // If solutions fail on the first attempt, try again with a different
    // reference ant on the second attempt when integrating...
    BOOST_FOREACH( SelfCalInfoVec::size_type infoIdx, seenInfos ) 
    {
        selfCalInfo_.at( infoIdx ).first.calculateSelfCal( rootbtau );
        selfCalInfo_.at( infoIdx ).second.calculateSelfCal( rootbtau );
    }
}

CorrelatorDataPtr
SelfCalStage::postprocess( CorrelatorDataPtr cd ) 
{
    return cd; // Pass correlator data on to next stage.
}

void SelfCalStage::fillMonitorData( ) 
{
    BOOST_FOREACH( SelfCalInfoVec::value_type & usbLsbPair, selfCalInfo_ ) {
        usbLsbPair.first.fillMonitorData();
        usbLsbPair.second.fillMonitorData();
    }

    // Now calculate medians for each antenna...
    DoubleMedianAccumMap ampMedianMap, snrMedianMap;

    BOOST_FOREACH( SelfCalInfoVec::value_type & usbLsbPair, selfCalInfo_ ) {
        const pair< bool, ComplexVector > usbVis = usbLsbPair.first.getVis();
        const pair< bool, ComplexVector > lsbVis = usbLsbPair.second.getVis();
        const SnrVector usbSnr = usbLsbPair.first.getSnr();
        const SnrVector lsbSnr = usbLsbPair.second.getSnr();

        if ( usbVis.first ) {
            tallyAntSelfCalMedians( ampMedianMap, 
                                    snrMedianMap, 
                                    usbVis.second,
                                    usbSnr,
                                    plmi_,
                                    usbLsbPair.first.getBandPol() );
        }

        if ( lsbVis.first ) {
            tallyAntSelfCalMedians( ampMedianMap, 
                                    snrMedianMap, 
                                    lsbVis.second,
                                    lsbSnr,
                                    plmi_,
                                    usbLsbPair.second.getBandPol() );
        }

    }

    // Now loop over ALL antennas - if antenna is connected to this pipeline's 
    // correlator, write out either INVALID_NO_HW (if not signal path mapped) 
    // or the median self cal amps and snrs to the astro monitor subsystem. 
    const int antCount = monitor::AstroSubsystemBase::antennaCount();
    for ( int antNo = 1; antNo <= antCount; ++antNo ) {

        const int antIdx = antNo - 1;

        // If this ant is not connected to this pipeline's correlator, move on
        typedef CM::CorrelatorDesignationMonitorPointEnum CorrDesMPE;
        typedef CorrDesMPE::CORRELATORDESIGNATION CorrDesEnum;

        AstroSubsystem::Antenna & antMon = astroMonitor_.antenna( antIdx );

        // Though I calculate selfcal in both pipelines, only one
        // can be the canonical value to display on the composite page. So, 
        // we check that the antenna's owning subarray's correlator designation
        // matches that of this pipeline in order to write it. Note that I can't
        // just check antenna correlator designation because this can be 'ANY'
        // in some oddball signal path configurations despite the fact that the
        // subarray does not contain both correlators.  If the antenna's owning 
        // subarray's corr designation is still ANY, I only write the 
        // SPECTRAL LINE version.  Got it?

        const MonitorCorrelatorDesignation antCorrDes = plmi_.getAntSubarrayCorrDes( antNo );

    const MonitorCorrelatorDesignation pipelineCorrDes = getMonCorrDes( plType_ );

    carma::util::CorrelatorSet antCorrSet(antCorrDes);
    
    // This version generalizes Andy's logic to check whether the
    // antenna's correlator set includes this pipeline's
    // correlator, and to publish the coherence only if this
    // correlator is the first one in the antenna's correlator
    // set
        
        if(antCorrSet == pipelineCorrDes ||
           (antCorrSet.includes(pipelineCorrDes) && antCorrSet.firstCorrelatorIs(pipelineCorrDes)))
        {  
          const DoubleMedianAccumMap::const_iterator ait = 
            ampMedianMap.find( antNo );
          if ( ait != ampMedianMap.end() ) {
            antMon.medianAmp().setValue( median( ait->second ) );
          } else {
            antMon.medianAmp().setValue( 0.0 );
          }
          
          const DoubleMedianAccumMap::const_iterator sit = 
            snrMedianMap.find( antNo );
          if ( sit != snrMedianMap.end() ) {
            antMon.medianSnr().setValue( median( sit->second ) );
          } else {
            antMon.medianSnr().setValue( 0.0 );
          }
        } else if(antCorrSet.isEmpty()) {
          antMon.medianAmp().setValidity( NO_HW );
          antMon.medianSnr().setValidity( NO_HW );
        }
    } // for antNo
}

void
SelfCalStage::processBaseline( 
    const int bandNo, 
    const CorrelatorBaseline & baseline, 
    set< SelfCalInfoVec::size_type > & seenInfos,
    double & iTime )
{
    const int input1Num = baseline.getInput1Number( );
    const int input2Num = baseline.getInput2Number( );

    const AntPolPair antPol1 = plmi_.getAntPolPair( bandNo, input1Num );
    const AntPolPair antPol2 = plmi_.getAntPolPair( bandNo, input2Num );

    const AntPolPair corAntPol1( baseline.getAnt1Number( ), 
                                 corrPolToMonPol(baseline.getPolarization1()));
    const AntPolPair corAntPol2( baseline.getAnt2Number( ), 
                                 corrPolToMonPol(baseline.getPolarization2()));

    // Verify that SPM and correlator data agree on AntPol
    if ( antPol1 != corAntPol1 || antPol2 != corAntPol2 ) 
        return;

    // Self cal is only calculated on like polarizations
    if ( antPol1.second != antPol2.second )
        return;
    
    // Skip if no signal path is mapped
    if ( !plmi_.signalPathMapped( bandNo, antPol1 ) ||
         !plmi_.signalPathMapped( bandNo, antPol2 ) )
        return; 
        
    const SelfCalInfoVec::size_type infoIdx = getInfoIdx( bandNo, 
                                                          antPol1.second );
    UsbLsbInfoPair & sbInfo = selfCalInfo_.at( infoIdx );

    seenInfos.insert( infoIdx );

    // Loop over sidebands - note there are three types: USB, LSB and AUTO. 
    const SidebandVector & sidebands = baseline.getSidebands( );

    BOOST_FOREACH( const CorrelatorSideband & sideband, sidebands ) {

        const bool usb = sideband.isUSB( );
        const bool lsb = sideband.isLSB( );

        if ( !usb && !lsb ) continue; // Skip if auto sideband 
        
        if ( !( sideband.isValid() ) ) {
            CARMA_CPTRACE( TRACE_SELF_CAL, "Sideband INVALID" );
            continue; // Ditto if sideband invalid
        }

        const double intTime = 
            sideband.getStats( ).getIntegrationTime( ) * S_PER_MS;
        const double weight = calculateWeight( 
            input1Num, 
            input2Num,
            bandNo,
            usb, 
            intTime );

        if (weight <= 0) {
            CARMA_CPTRACE( TRACE_SELF_CAL, "Weight CRAP" );
            continue; // Ditto if weight is crap
        }

        SelfCalSidebandInfo & info = ( usb ? sbInfo.first : sbInfo.second );
        info.setVis( antPol1.first,
                     antPol2.first,
                     sideband.getStats( ).getAvg( ),
                     weight );

        iTime = std::max( intTime , iTime); 
    } // End loop over sidebands
}

// This is sigma**2 of the visibility data
double
SelfCalStage::calculateWeight( const int input1Num,
                               const int input2Num,
                               const int bandNum,
                               const bool usb,
                               const double integTime ) const
{
    double tsys   = 1.0;
    double jyperk = 1.0;
    double weight = 1.0;

    if ( tsys_.isStageActive( ) && tsys_.isTsysBeingApplied( ) ) {

        const TsysPipelineInfo& tpi = tsys_.getTsysPipelineInfo();
        if (tpi.getTsysDsb(input1Num, bandNum) > 9999) return 0.0;
        if (tpi.getTsysDsb(input2Num, bandNum) > 9999) return 0.0;
        std::pair< bool, bool > validity;
        if ( usb ) {
            tsys = tpi.getBaselineTsysUsb( input1Num, input2Num, bandNum, validity );
        } else {
            tsys = tpi.getBaselineTsysLsb( input1Num, input2Num, bandNum, validity );
        }

        if ( !validity.first || !validity.second ) return 0.0;

        if (tsys > 9999) return 0.0;

        if ( tsys_.isStageActive( ) && tsys_.isFluxBeingApplied( ) ) {
            jyperk = tpi.getBaselineJanskysPerKelvin( input1Num, 
                                                      input2Num,
                                                      bandNum );
        }

        double noise = tsys * jyperk; // In Janskies
        if ( noise < numeric_limits< double >::epsilon( ) ) {
            ostringstream msg;
            msg << "SelfCalStage::calculateWeight( ) - Tsys ( " << tsys 
                << " ) * jyperk ( " << jyperk << " ) is less than epsilon.";
            programLogErrorIfPossible( msg.str( ) );
            weight = 1.0;
        } else {
            weight = integTime / (noise*noise);
        }
    } 

    return weight;
}

double
SelfCalStage::calculateSystemNoise( const double rootbtau, 
                                    const int inputNum,
                                    const int bandNum,
                                    const bool usb ) const
{
    const TsysPipelineInfo & tpi = tsys_.getTsysPipelineInfo();
    const double tsys = usb ? tpi.getTsys(bandNum, inputNum).getTsysUsb() : tpi.getTsys(bandNum, inputNum).getTsysLsb();
    const double jyperk = tpi.getBaselineJanskysPerKelvin( inputNum,  
                                                           inputNum, 
                                                           bandNum );

    return ( tsys*jyperk/rootbtau ); // System noise in Jy
}

SelfCalStage::SelfCalInfoVec::size_type
SelfCalStage::getInfoIdx( const int astroBandNo,
                          const carma::monitor::PolType pol ) const
{
    const AstroBandPolPair bandPol( astroBandNo, pol );
    const AstroBandPolInfoIdxMap::const_iterator bpiIt = 
        bandPolToInfoIdx_.find( bandPol );
    if ( bpiIt == bandPolToInfoIdx_.end( ) ) {
        ostringstream msg;
        msg << "Astroband/pol " << astroBandNo 
            << PolMPE::valueToString( pol ) << " is invalid.";
        throw CARMA_EXCEPTION( IllegalArgumentException, msg.str() );
    }
        
    return bpiIt->second;
}
