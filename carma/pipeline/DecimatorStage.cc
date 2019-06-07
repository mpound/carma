// $Id: DecimatorStage.cc,v 1.5 2013/05/15 15:23:00 abeard Exp $

#include "carma/pipeline/DecimatorStage.h"

#include "carma/correlator/lib/CorrelatorData.h"
#include "carma/monitor/PipelineSubsystem.h"
#include "carma/monitor/PipelineMonitorInput.h"
#include "carma/util/FftwRealToRealPlan.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <boost/assign/list_of.hpp>
#include <boost/foreach.hpp>
#include <cmath>

using namespace std;
using namespace carma::pipeline;
using namespace carma::util;
using namespace carma::correlator::lib;
using namespace carma::monitor;

namespace {

    typedef ScopedLock< ::pthread_mutex_t > ScopedPthreadMutexLock;
    
    /**
     * VisData consists of the DC channel and positive output spectra only!
     * The negative portion of the spectra can be duplicated by taking
     * the complex conjugate of the positive elements (i.e. the spectrum
     * is symetric about zero).
     * Note that vis data is assumed to be derived from an even number of lags.
     */
    typedef ::std::vector< complex< float > > VisDataVector;

    /**
     * Both the GSL and FFTW real FFT routines use an array of N real numbers 
     * as the input to the forward fft and as the output from the inverse fft. 
     * The output from the forward transform and input to the inverse transform
     * are arrays in 'half-complex' form.  Half complex form is described below
     * where the input 'in' vector is an array of complex numbers:
     *  halfcplx[0] = in[0].real();
     *  halfcplx[1] = in[1].real();
     *  ... 
     *  halfcplx[N/2] = in[N/2].real(); // Nyquist (only if even in time domain)
     *  halfcplx[N-(N2-1)] = in[N/2-1].imag();
     *  ...
     *  halfcplx[N-1]=in[1].imag();
     * 
     * Note that the same N elements are used for both the input and output
     * forward and reverse FFTs (i.e. all calculations are done in-place).
     *
     * This should be enough info but see either the GSL or FFTW manuals if 
     * you require more.
     */
    typedef FftwRealVector FftWorkVector;

    const Trace::TraceLevel TRACE_DEMEAN = Trace::TRACE5;
    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE3;
    const Trace::TraceLevel TRACE_PLAN_COPY = Trace::TRACE3;

    const FftWorkVector::size_type MAX_PLAN_SIZE = 4096;

    // Populate with known FFT sizes and kinds from Memo #46. 
    // Additional channel sizes will cause on-the-fly plan creation with
    // a possibly large one-time performance hit.
    const ::std::set< FftWorkVector::size_type > 
    DEFAULT_PLAN_SIZES = boost::assign::list_of
        // COBRA Bands
        ( 32 )  // 500 MHz - 17 channels 
        ( 120 ) // 62 MHz - 61 channels
        ( 128 ) // 31 MHz, 8 MHz, 2 MHz - 65 channels
        // CARMA Bands [2-bit samples]
        ( 256 ) // 500 MHz - 129 channels (old)
        ( 192 ) // 500 MHz - 97 channels (new)
        ( 384 ) // 250 MHz - 193 channels
        ( 576 ) // 125 MHz - 289 channels
        ( 768 ) // 62 MHz, 31 MHz, 8 MHz, 2 MHz - 385 channels
        // CARMA Bands [3-bit samples] 
        ( 160 ) // 500 MHz - 81 channels (old)
        ( 80 )  // 500 MHz - 41 channels (new)
        ( 448 ) // 125 MHz - 225 channels (old)
        ( 320 ) // 125 MHz - 161 channels (new)
        ( 640 ) // 62 MHz (old), 31 MHz, 8 MHz, 2 MHz - 321 channels
        ( 512 ) // 62 MHz - 257 channels (new)
        // CARMA 23 station modes (inferred from logs) 
        ( 96 ); 

    FftWorkVector::size_type 
    computeHalfComplexSize( const VisDataVector::size_type nChans )
    {
        // Verify that we have a sufficient number of channels (3).
        if ( nChans < 3 ) {
            throw std::logic_error( "computeHalfComplexSize( nChans < 2 )!" );
        }

        // Verify that the number of channels is odd and hence corresponds to 
        // an even number of lags (equal positive and negative).
        if ( nChans % 2 == 0 ) {
            throw std::logic_error( 
                "computeHalfComplexSize() - Holy Moly nChans is even!" );
        }

        return ( ( nChans * 2 ) - 2 ); 
    }

    void
    convertVisDataToHalfComplex( const VisDataVector & visData,
                                 FftWorkVector & halfComplex )
    {
        const FftWorkVector::size_type N = 
            computeHalfComplexSize( visData.size( ) );
        
        halfComplex.resize( N );

        halfComplex.at( 0 ) = visData.at( 0 ).real( ); // DC, real only

        FftWorkVector::size_type idx = 1;
        for ( ; idx < N / 2; ++idx ) {
            halfComplex.at( idx ) = visData.at( idx ).real( );
            halfComplex.at( N - idx ) = visData[idx].imag( );
        }
        
        halfComplex.at( N/2 ) = visData.at( N/2 ).real( ); // Nyquist, real only
    }
        
    void
    convertHalfComplexToVisData( const FftWorkVector & halfComplex,
                                 VisDataVector & visData )
    {
        const VisDataVector::size_type hcSize = halfComplex.size( );
        const VisDataVector::size_type nOutChans = ( 1 + ( hcSize / 2 ) );
            
        visData.resize( nOutChans ); // Should already be this size.

        visData.at( 0 ) = complex<float>( halfComplex.at( 0 ) ); // DC

        VisDataVector::size_type i = 1;
        for ( ; i < nOutChans - 1; ++i ) {
            visData.at( i ) = complex<float>( 
                    halfComplex.at(  i ),
                    halfComplex.at( hcSize - i ) );
        } 
        
        visData.at( nOutChans - 1 ) = 
            complex<float>( halfComplex.at( hcSize / 2 ) ); // Nyquist
    }
        
    void
    decimate( VisDataVector & visData )
    {
        // Decimation keeps odd chans and hence implicitly drops end chans
        VisDataVector::size_type size = visData.size( );
        VisDataVector::size_type i = 0;
        VisDataVector::size_type j = 1; 
        for ( i = 0; i < size/2; ++i ) {
            visData.at( i ) = visData.at( j );
            j += 2;
        }

        visData.resize( size/2 );
    }

    void
    dropEndChannels( VisDataVector & visData )
    {
        visData.pop_back( );
        visData.erase( visData.begin( ) );
    }

} // namespace < unnamed >

    void
    Decimator::cacheHannFactorsIfNotAlready( const int numLags )
    {
        if ( hannFactors_.find( numLags ) == hannFactors_.end( ) ) {

            vector< double > hannVector( numLags );

            const double factor = 2.0 * M_PI / numLags;
            double wt;

            // Recompute hanning factors 
            for ( int idx = 0; idx < numLags; ++idx ) {
                wt = 0.5 + 0.5 * cos(factor * idx);
                hannVector.at( idx ) = wt;
            }

            hannFactors_[numLags] = hannVector;
        }
    }

namespace {

    void 
    applyHannWindow( FftWorkVector & td, const vector< double > & hannFactors )
    {
        if ( td.size() != hannFactors.size() )
            throw CARMA_ERROR( "Lags and Hann factor vector size mismatch" );

        const FftWorkVector::size_type tdSize = td.size();
        for ( FftWorkVector::size_type idx = 0; idx < tdSize; ++idx ) 
            td.at( idx ) *= hannFactors.at( idx );
    }
    
    void 
    demeanPaddedLags( FftWorkVector & td, 
                      const FftWorkVector::size_type numLags );

    void 
    demeanUnpaddedLags( FftWorkVector & td, 
                        const FftWorkVector::size_type numLags );

    void 
    demean( FftWorkVector & td, const FftWorkVector::size_type numLags )
    {
        const bool padded = ( td.size() != numLags );

        if ( padded ) {
            demeanPaddedLags( td, numLags );
        } else {
            demeanUnpaddedLags( td, numLags );
        }

    }
                      
    // Set padded lags to zero
    // This wants the real number of lags (e.g. 119, not 120)
    void
    zeroPaddedLags(FftWorkVector & td, 
                      const FftWorkVector::size_type numLags ) {
        if ((numLags && 1) != 1) 
            throw CARMA_EXCEPTION( IllegalArgumentException, 
                                   "Number of lags must be odd" ); 
        int pad = td.size() - numLags;  // num lags that must be padded
        int idx = (td.size() - pad)/2 + 1;   // starting index for padding           
        for (int i = 0;  i< pad; i++, idx++) td.at(idx) = 0.0;          
    }
    
    // td is time domain data (lags)                  
    void 
    demeanPaddedLags( FftWorkVector & td, 
                      const FftWorkVector::size_type numLags )
    {
        // Note on padding: A correlation function produced by a DFT is
        // real and has an even number of lags.  Because one of these lags 
        // is lag=0, the function does not have the same number of 
        // positive and negative lags. If such a function is used as input 
        // to a DFT it will work fine because the DFT assumes circular 
        // symmetry (cyclic) correlation function. However, if the 
        // correlation function is truncated to a length less than that 
        // expected by the DFT then the correlation function must have an equal 
        // number of plus and minus lags, and hence an odd number of lags.
        // For example time data with 8 elements and with 6 reported lags (thus 
        // really 3 are zero padded ('Z')) can be represented as: 
        // | 0 | 1 | 2 | Z | Z | Z | -2 | -1 |
        //
        // Things are more complicated however. In the only case
        // where we have padding the correlator reports 120 lags.
        // Unfortunately, that is how many it really used, so we mast
        // drop things to 119 to get a correct correlation function.
        const FftWorkVector::size_type tdSize = td.size();
        if (numLags == tdSize)
            throw CARMA_EXCEPTION( IllegalArgumentException, 
                                   "demeanPaddedLags() - Lags aren't padded!" );

        // Make number of lags odd. If even lags comes in, make it one smaller.
        int nLags = 2*((numLags - 1)/2) + 1;

        zeroPaddedLags(td, nLags); // Must do before we compute the mean
        
        double sum = 0;
        // compute the mean
        for ( FftWorkVector::size_type idx = 0; idx < tdSize; ++idx ) {
            sum += td.at( idx ) ;
        }
        // Normalize by number of valid (non-zero set) lags
        const double mean = sum / nLags;

        // Remove mean
        for ( FftWorkVector::size_type idx = 0; idx < tdSize; idx++ ) {
            td.at( idx ) -= mean;
        }
        // Yes, do it again to fix mean removal for padded lags
        zeroPaddedLags(td, nLags); // Must do before we compute the mean

    }

    void
    demeanUnpaddedLags( FftWorkVector & td, 
                        const FftWorkVector::size_type numLags )
    {
        const FftWorkVector::size_type tdSize = td.size();

        if ( tdSize != numLags ) 
            throw CARMA_EXCEPTION( IllegalArgumentException, 
                                   "demeanUnpaddedLags() - Lags are padded!" );
        
        double sum = 0.0;

        const FftWorkVector::size_type halfLags = numLags / 2;
        for ( FftWorkVector::size_type idx = 0; idx < halfLags; ++idx ) {
            sum += td.at( idx ) + td.at( tdSize - idx - 1 );
        }

        const double mean = sum / numLags;

        for ( FftWorkVector::size_type idx = 0; idx < halfLags; ++idx ) {
            td.at( idx ) -= mean;
            td.at( tdSize - idx - 1 ) -= mean;
        }
    }


    void
    removePaddedLags( FftWorkVector & td, 
                      const FftWorkVector::size_type numLags )
    {
        if ( numLags < td.size( ) ) {
            // Remove zero padded lags by overwritting them with negative lags.
            FftWorkVector::size_type padIdx = numLags / 2;
            FftWorkVector::size_type lagIdx = td.size( ) - padIdx;
            for ( ; padIdx < numLags; ++padIdx, ++lagIdx ) {
                td.at( padIdx ) = td.at( lagIdx );
            }

            td.resize( numLags );
        }
    }

} // namespace < unnamed >


    void
    Decimator::processSideband( 
        CorrelatorSideband & sb,
        DecimationBandInfo & decBand ) 
    {
        const int numLags = sb.getNumberOfLags( );
        const VisDataVector::size_type numChans = sb.getData( ).size( );
        const VisDataVector::size_type numPaddedLags = ( numChans - 1 ) * 2;

        // If channel count isn't what we expect, blank and move on.
        if ( static_cast< int >( numChans ) != decBand.expectedChans ) {
            sb.blankData( CorrelatorSideband::BAD_CHANNEL_COUNT );
            return;
        }

        FftwRealToRealPlan & inversePlan = decBand.fftwPlans.retrievePlan( 
            numPaddedLags, 
            FftwRealToRealPlan::HALFCOMPLEX_TO_REAL );

        FftWorkVector & td = 
            decBand.fftwPlans.getFftwRealVector( ); // time domain
            
        convertVisDataToHalfComplex( sb.getData( ), td );

        inversePlan.execute( ); // Inverse transform back to lag space
    
        demean( td, numLags );

        if ( decBand.decimate ) {
            const HannFactorsMap::const_iterator hfi = 
                hannFactors_.find( numLags );
            if ( hfi == hannFactors_.end() ) 
                throw CARMA_ERROR( "Precached Hann factors not found." );
            else 
                applyHannWindow( td, hfi->second );
        }
            
        FftwRealToRealPlan & forwardPlan = decBand.fftwPlans.retrievePlan( 
            numPaddedLags, FftwRealToRealPlan::REAL_TO_HALFCOMPLEX );

        forwardPlan.execute( ); // Forward transform back to frequency space

        convertHalfComplexToVisData( td, sb.getData( ) );
        
        if ( decBand.decimate ) { 
            decimate( sb.getData( ) ); // Implicitly drops end channels
        } else if ( !decBand.keepEndChannels ) {
            dropEndChannels( sb.getData( ) );
        } 
            
        sb.setData( sb.getData( ) ); // Required as setData sets other crap too.

    } // processSideband 
    
    void 
    Decimator::updateAdditionalSidebandParameters( 
        CorrelatorSideband & sb,
        const DecimationBandInfo & decBand ) 
    {
        const float inRxOutFreq   = sb.getRxOutFrequency( );
        const float inDelFreq = sb.getDeltaFrequency( );
        float newDeltaFreq = decBand.outDeltaFreq;

        if ( inDelFreq < 0.0 ) {
            newDeltaFreq *= -1.0;
        }

        sb.setDeltaFrequency( newDeltaFreq );

        if ( decBand.decimate ) {
            sb.setOffsetFrequency( inDelFreq );
            sb.setRxOutFrequency( inRxOutFreq + ( inDelFreq ) / 1e3 );
        } else { // Don't decimate
            if ( decBand.keepEndChannels ) {
                sb.setOffsetFrequency( 0.0 );
                sb.setRxOutFrequency( inRxOutFreq );
            } else { // drop end channels
                sb.setOffsetFrequency( inDelFreq );
                sb.setRxOutFrequency( inRxOutFreq + ( inDelFreq ) / 1e3 );
            }
        }

        sb.computeStats( ); // Recompute stats
    } // updateAdditionalSidebandParameters

Decimator::DecimationBandInfo::DecimationBandInfo() : 
    expectedChans( 0 ),
    outputChans( -1 ),
    numLags( 0 ),
    outBandwidth( 0.0 ),
    outDeltaFreq( 0.0 ),
    keepEndChannels( false ),
    decimate( false ),
    decimateType( Decimation::DecimateMonitorPointEnum::NO ),
    windowType( Decimation::WindowMonitorPointEnum::NONE ),
    fftwPlans( MAX_PLAN_SIZE )
{
    pthread_mutex_init( &mutex, 0 );
}

Decimator::Decimator( PipelineSubsystem & monitor,
                      const carma::monitor::PipelineMonitorInput & plmi,
                      const carma::pipeline::PipelineType pipelineType ) : 
    Stage( monitor.getDecimationStageStats( ), "Decimator" ),
    monitorData_( monitor ),
    plType_( pipelineType ),
    plmi_( plmi ),
    hannFactors_( )
{
    const unsigned short bandNoBegin = getAstrobandRange( plType_ ).first;

    const unsigned short bandNoEnd = monitorData_.getBandCount( ) + bandNoBegin;
    for ( unsigned short bandNo = bandNoBegin; bandNo < bandNoEnd; ++bandNo ) {

        DecimationBandInfo defaultBandInfo;

        FftwRealToRealPlanManager & fftwPlans = defaultBandInfo.fftwPlans;

        // Populate with known FFT sizes and kinds from Memo #46. 
        // Additional channel sizes will cause on-the-fly plan creation with
        // a noticeable one-time performance hit.
        BOOST_FOREACH( const FftWorkVector::size_type & ps, DEFAULT_PLAN_SIZES )
        {
            fftwPlans.createPlan( ps, FftwRealToRealPlan::REAL_TO_HALFCOMPLEX );
            fftwPlans.createPlan( ps, FftwRealToRealPlan::HALFCOMPLEX_TO_REAL );
        }

        CARMA_CPTRACE( TRACE_PLAN_COPY, "Decimator - Pushing band info copy "
            "onto vector." );
        decimationInfo_.insert ( 
            DecimationInfoMap::value_type( bandNo, defaultBandInfo ) );
    }
}

Decimator::~Decimator() 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Decimator::~Decimator() - D'tor." );
}

void
Decimator::preprocess( const CorrelatorDataPtr cd )
{
    resetDecimationBandInfo( );

    // Prep per band info.  This is done here rather than in parallel 
    // to avoid destroying per band parallelization on shared data
    // such as the cached Hann windows.  We save the expensive stuff
    // for parallel band processing (e.g. FFTS and the like).
    const BandVector & bands = cd->getBands();
    BOOST_FOREACH( const CorrelatorBand & band, bands )
    {
        if ( !band.isValid() ) continue;
    
        const int abNo = band.getBandNumber( );

        DecimationInfoMap::iterator infoIt = decimationInfo_.find( abNo );

        if ( infoIt == decimationInfo_.end() ) continue;

        DecimationBandInfo & decBand = infoIt->second;

        ScopedPthreadMutexLock scopelock( decBand.mutex );

        const int nChans = plmi_.getExpectedChannels( abNo );     

        decBand.expectedChans = nChans + 2;
        decBand.numLags = ( decBand.expectedChans - 1 ) * 2;
        calculateNewFrequencyParams( band.getBandwidth( ), decBand );
        cacheHannFactorsIfNotAlready( decBand.numLags ); 
     }
}

void
Decimator::processBand( CorrelatorBand * cb )
{
    if ( cb == 0 ) return;

    if ( !cb->isValid() ) return;

    const int decBandNum = cb->getBandNumber( );

    DecimationInfoMap::iterator decBandIt = decimationInfo_.find( decBandNum );

    if ( decBandIt == decimationInfo_.end() ) return;

    DecimationBandInfo & decBand = decBandIt->second;

    ScopedPthreadMutexLock scopelock( decBand.mutex );

    cb->setBandwidth( decBand.outBandwidth );

    vector< CorrelatorBaseline > & baselines = cb->getBaselines(); 
    BOOST_FOREACH( CorrelatorBaseline & baseline, baselines ) {
        processBaseline( baseline, decBand );
    }

    return;
}

CorrelatorDataPtr
Decimator::postprocess( const CorrelatorDataPtr cd )
{
    return cd;
}
                
void 
Decimator::keepEndChannels( const bool keep ) 
{
    BOOST_FOREACH( DecimationInfoMap::value_type & entry, decimationInfo_ ) {
        keepEndChannels( keep, entry.first ); 
    }
}

void 
Decimator::keepEndChannels( const bool keep, const int astroBandNo ) 
{
    DecimationInfoMap::iterator decBandIt = decimationInfo_.find( astroBandNo );
    if ( decBandIt == decimationInfo_.end() ) 
        throw CARMA_ERROR( "Invalid astroband." );

    ScopedPthreadMutexLock lock( decBandIt->second.mutex );

    if ( keep ) {
        decBandIt->second.decimate = false;
        decBandIt->second.decimateType = 
            Decimation::DecimateMonitorPointEnum::NO;
    }

    decBandIt->second.keepEndChannels = keep;
}

void 
Decimator::decimation( const bool on ) 
{
    BOOST_FOREACH( DecimationInfoMap::value_type & entry, decimationInfo_ ) {
        decimation( on, entry.first );
    }
}

::std::set< FftwRealVector::size_type > 
Decimator::defaultFftwPlanSizes( ) 
{
    return DEFAULT_PLAN_SIZES;
}

void 
Decimator::decimation( const bool on, const int astroBandNo ) 
{
    DecimationInfoMap::iterator decBandIt = decimationInfo_.find( astroBandNo );
    if ( decBandIt == decimationInfo_.end() ) 
        throw CARMA_ERROR( "Invalid astroband." );

    ScopedPthreadMutexLock lock( decBandIt->second.mutex );

    if ( on ) {
        decBandIt->second.keepEndChannels = false;
        decBandIt->second.decimateType = 
            Decimation::DecimateMonitorPointEnum::YES;
    } else {
        decBandIt->second.decimateType = 
            Decimation::DecimateMonitorPointEnum::NO;
    }

    decBandIt->second.decimate = on;
}

void 
Decimator::fillMonitorData( ) 
{
    const unsigned short astroBandNoBegin = getAstrobandRange( plType_ ).first;

    const unsigned short bands = monitorData_.getBandCount( );
    for ( unsigned short bandIdx = 0; bandIdx < bands; ++bandIdx ) {
        Decimation & decMon = monitorData_.getDecimation( bandIdx );

        const unsigned short astroBandNo = astroBandNoBegin + bandIdx; 

        DecimationInfoMap::iterator decBandIt = decimationInfo_.find( astroBandNo );
        if ( decBandIt == decimationInfo_.end() ) 
            throw CARMA_ERROR( "Invalid astroband." );

        DecimationBandInfo & decInfo = decBandIt->second;
        ScopedPthreadMutexLock lock( decInfo.mutex );
        decMon.decimate( ).setValue( decInfo.decimateType );
        decMon.expectedChannels( ).setValue( decInfo.expectedChans );
        decMon.outputChannels( ).setValue( decInfo.outputChans );
        decMon.numLags( ).setValue( decInfo.numLags );
        decMon.keepEndChannels( ).setValue( decInfo.keepEndChannels );
        decMon.window( ).setValue( decInfo.windowType );
    }
}

void 
Decimator::resetDecimationBandInfo( ) 
{
    // Since data input into the Decimator can come in out-of-band order
    // and/or partially, it is easiest to simply reset decimation band info 
    // and refill it as we process the input bands.
    BOOST_FOREACH( DecimationInfoMap::value_type & entry, decimationInfo_ ) {
        ScopedPthreadMutexLock scopelock( entry.second.mutex );
        entry.second.expectedChans = 0;
        entry.second.outputChans = 0;
    }
}

void
Decimator::calculateNewFrequencyParams( const float inBandwidth,
                                        DecimationBandInfo & decBand )
{
    // Note: End channels are special in that channel width is half other chans.
    // Example: 9 input channels contain 7 full width chans and 2 half-width. 
    const float inDeltaFreq = inBandwidth / (decBand.expectedChans - 1);
    
    if ( decBand.decimate ) {
        // Decimation keeps every other (odd) channel - which implies removing
        // the end channels. This leaves (N - 1)/2 channels with 2x chan width.
        decBand.outputChans = decBand.expectedChans / 2; // Truncate on purpose.
        decBand.outDeltaFreq = 2.0 * inDeltaFreq;
        decBand.outBandwidth = decBand.outputChans * decBand.outDeltaFreq;
        decBand.decimateType = Decimation::DecimateMonitorPointEnum::YES;
        decBand.windowType = Decimation::WindowMonitorPointEnum::HANNING;
    } else {
        if ( decBand.keepEndChannels ) {
            decBand.outputChans = decBand.expectedChans;
            decBand.outDeltaFreq = inDeltaFreq;
            decBand.outBandwidth = inBandwidth;
        } else { // Drop the end channels
            // When dropping the end channels, note again that end channel
            // width is only half of input delta frequency.  When removed,
            // it leaves N - 2 channels all with their original width.
            decBand.outputChans = decBand.expectedChans - 2;
            decBand.outDeltaFreq = inDeltaFreq; 
            decBand.outBandwidth = decBand.outputChans * decBand.outDeltaFreq;
        }
        decBand.decimateType = Decimation::DecimateMonitorPointEnum::NO;
        decBand.windowType = Decimation::WindowMonitorPointEnum::NONE;
    }
}

void
Decimator::processBaseline(
    CorrelatorBaseline & baseline,
    DecimationBandInfo & decBand )
{
    SidebandVector &  sidebands = baseline.getSidebands( );
    BOOST_FOREACH( CorrelatorSideband & sideband, sidebands ) {

        if ( sideband.isValid( ) ) {

            processSideband( sideband, decBand );

            updateAdditionalSidebandParameters( sideband, decBand );
        }
    } // Loop over sidebands
}
        
