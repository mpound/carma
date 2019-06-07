#include "carma/pipeline/TsysPipelineInfo.h"

#include "carma/services/Atmosphere.h"
#include "carma/services/SimpleOpacityModel.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/ControlBandCommon.h"
#include "carma/monitor/ControlSubsystemExt.h"
#include "carma/monitor/PipelineMonitorInput.h"
#include "carma/monitor/WeatherSubsystem.h"
#include "carma/pipeline/Tsys.h"
#include "carma/services/Global.h"
#include "carma/services/Physical.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Trace.h"

#include <boost/foreach.hpp>
#include <iostream>
#include <map>
#include <vector>

using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;
using namespace std;

namespace {

    const Trace::TraceLevel TRACE_ASTRO = Trace::TRACE7;
    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE4;
    const Trace::TraceLevel TRACE_TSYS_CALC = Trace::TRACE5;
    const Trace::TraceLevel TRACE_SET_GET = Trace::TRACE6;

    const double DEFAULT_GROUND_OPACITY = 0.05; // Nepers

    typedef AntennaCommon::CalStateMonitorPointEnum CalStateEnum;

    double computeGeometricAverage( const double x, const double y )
    {
        return ::sqrt( x * y );
    }

} // namespace < unnamed >


    class TsysPipelineInfo::TsysInput {
    public:

        explicit TsysInput( unsigned short astroInputNo, 
                            Atmosphere & atmosphere, 
                            carma::pipeline::PipelineType plType );
        
        TsysInput( const TsysInput & from );

        /* virtual */ ~TsysInput( );

        TsysInput & operator=(const TsysInput & from );

        Tsys & astroBand( unsigned int astroBandNo );

        void updateWithPipelineMonitorInput( 
                    const PipelineMonitorInput & plmi ); 

    private:

        typedef ::std::map< unsigned short, Tsys > TsysAstroBandMap;

        TsysAstroBandMap astroBands_;
        Atmosphere & atmosphere_;

        mutable unsigned int astroInputNo_;
        mutable carma::pipeline::PipelineType plType_;

    }; // class TsysPipelineInfo::TsysInput

    TsysPipelineInfo::TsysInput::TsysInput( 
        const unsigned short astroInputNo,
        Atmosphere & atmosphere, 
        const carma::pipeline::PipelineType plType ) : 
            atmosphere_( atmosphere ),
            astroInputNo_( astroInputNo ),
            plType_( plType )
    {
        CARMA_CPTRACE( TRACE_CTOR_DTOR, "TsysInput::TsysInput( astroInputNo="
            << astroInputNo_ << " ) - C'tor. " );

        // Populate the map of astroband tsys
        const AstrobandRange abRange( getAstrobandRange( plType_ ) );
        for ( unsigned abNo = abRange.first; abNo <= abRange.second; ++abNo ) {
            astroBands_.insert( TsysAstroBandMap::value_type( abNo, Tsys() ) );
        }

    }

    TsysPipelineInfo::TsysInput::TsysInput( const TsysInput & from ) 
        :
        astroBands_( from.astroBands_ ),
        atmosphere_( from.atmosphere_ ),
        astroInputNo_( from.astroInputNo_ ),
        plType_( from.plType_ )
    {
        CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                       "TsysPipelineInfo::TsysInput::TsysInput() - "
                       "Copy c'tor." );
    }

    TsysPipelineInfo::TsysInput & 
    TsysPipelineInfo::TsysInput::operator=( const TsysInput & from ) 
    {
        CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                       "TsysPipelineInfo::TsysInput::operator=() - "
                       "Operator = called." );

        if ( &from == this ) {
            return *this;
        } else {
            astroBands_ = from.astroBands_;
            astroInputNo_ = from.astroInputNo_;
            plType_ = from.plType_;
            return *this;
        }
    }
        
    TsysPipelineInfo::TsysInput::~TsysInput( ) 
    { 
        CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                       " TsysPipelineInfo::TsysInput::~TsysInput( ) - D'tor." );
    }

    Tsys & TsysPipelineInfo::TsysInput::astroBand( 
        const unsigned int astroBandNo )
    {
        const TsysAstroBandMap::iterator ab = astroBands_.find( astroBandNo ); 
        if ( ab == astroBands_.end() ) {
            ostringstream oss;
            oss << "Requested invalid astroband " << astroBandNo << " for "
                << "this correlator type.";
            throw CARMA_ERROR( oss.str() );
        }

        return ab->second;
    }

    void
    TsysPipelineInfo::TsysInput::updateWithPipelineMonitorInput( 
        const PipelineMonitorInput & plmi ) 
    {
        // The rules: 
        // 0) Signal path must be mapped.
        // 1) Cal state and total powers must be valid or we exit immediately.
        // 2) Temperatures are given more leeway, if they aren't valid, 
        //    the previous valid values (originally initialized to 'safe'
        //    weather values), are used.
        BOOST_FOREACH( TsysAstroBandMap::value_type & abTsys, astroBands_ ) {

            const unsigned short astroBandNo = abTsys.first;
            Tsys & tsys = abTsys.second;

            const AntPolPair antPol = plmi.getAntPolPair( astroBandNo, 
                                                          astroInputNo_ );

            if ( !plmi.signalPathMapped( astroBandNo, antPol ) ) 
                continue;

            CARMA_CPTRACE( TRACE_ASTRO, 
                "TsysPipelineInfo::TsysInfo::updateWithPipelineMonitorInput()"
                " astroInput=" << astroInputNo_ << " astroBand=" << astroBandNo
                << "." );

            const AntennaCommon & antCom = plmi.antennaCommon( antPol.first );

            if ( !antCom.calibrator().calState().isValid() ) {
                CARMA_CPTRACE( TRACE_ASTRO, 
                "TsysPipelineInfo::TsysInfo::updateWithPipelineMonitorInput()"
                " astroInput=" << astroInputNo_ << " astroBand=" << astroBandNo
                << " INVALID load state." );
                continue;
            }

            const CalStateEnum::CALSTATE calState = 
                antCom.calibrator().calState().getValue();

            bool loadTempValid = false;
            double loadTempInK = 0.0;
            switch ( calState ) {
                case CalStateEnum::AMB:
                    if ( antCom.calibrator().ambTemp().isValid() ) {
                        loadTempValid = true;
                        loadTempInK = antCom.calibrator().ambTemp().getValue();
                    }
                    break;
                case CalStateEnum::FIXED:
                    if ( antCom.calibrator().fixedTemp().isValid() ) {
                        loadTempValid = true;
                        loadTempInK = 
                            antCom.calibrator().fixedTemp().getValue();
                    }
                    break;
                case CalStateEnum::SKY:
                    if ( antCom.calibrator().skyTemp().isValid() ) {
                        loadTempValid = true;
                        loadTempInK = antCom.calibrator().skyTemp().getValue();
                    }
                    break;
                default:
                    CARMA_CPTRACE( TRACE_ASTRO, 
                        "TsysPipelineInfo::TsysInfo::"
                        "updateWithPipelineMonitorInput()"
                        " astroInput=" << astroInputNo_ 
                        << " astroBand=" << astroBandNo 
                        << " INVALID cal state." );
                    continue; // We don't care about any other cal wheel state.
            }

            loadTempValid = ( loadTempValid && 
                              atmosphere_.isSafeAirTemperature( loadTempInK ) );

        
            const double ambientTempInK =  // Ambient outside temp, not load
                plmi.weather().ambientTemperature().getValue() - 
                Physical::ABS_ZERO;

            const bool ambientTempValid = 
                ( plmi.weather().ambientTemperature().isValid() &&
                  atmosphere_.isSafeAirTemperature( ambientTempInK ) );

            carma::util::frameType frame = plmi.frameCount( );

            if ( !plmi.totalPower( astroBandNo, antPol ).isValid() ) {
                CARMA_CPTRACE( TRACE_ASTRO, 
                "TsysPipelineInfo::TsysInfo::updateWithPipelineMonitorInput()"
                " astroInput=" << astroInputNo_ << " astroBand=" << astroBandNo
                << " INVALID total power." );
                continue; // If less than valid, skip to next band.
            }

            const double totalPower = 
                plmi.totalPower( astroBandNo, antPol ).getValue();

            // Hack: If we're doing cm observations, boost power by
            // 9dB when the ambient load is in.
            const int lsbIdx = 1;
            const ControlBandPoints & cbp = plmi.controlBandPoints(astroBandNo);
            if ( !cbp.sideband(lsbIdx).skyFreq().isValid() ) 
                continue;

            const double freqLsb = cbp.sideband(lsbIdx).skyFreq().getValue();

            if ( CalStateEnum::AMB == calState && freqLsb < 50.0 ) { 
                tsys.setTotalPower( calState, totalPower + 8.0, frame );
            } else { 
                tsys.setTotalPower( calState, totalPower, frame );
            }

            const double sbr = ( freqLsb <= 50.0 ? 0.0 : 
                                 ( antPol.first < 16 ? 1.0 : 1000.0 ) );

            tsys.setReceiverSidebandRatio( sbr );
            
            const ControlSubsystem::Antenna & antControl = 
                plmi.control().antenna( antPol.first - 1 );

            if ( antControl.jyperk().isValid() ) 
                tsys.setJanskysPerKelvin( antControl.jyperk().getValue() );

            tsys.setGroundOpacity( DEFAULT_GROUND_OPACITY );

            if ( ( CalStateEnum::AMB == calState || 
                   CalStateEnum::FIXED == calState ) && loadTempValid ) {
                tsys.setLoadTemperature( loadTempInK );
            } else if ( CalStateEnum::SKY == calState && loadTempValid ) {
                tsys.setAtmosphericTemperature( loadTempInK );
            }

            if ( ambientTempValid ) {
                tsys.setOutsideAmbientTemperature( ambientTempInK );
            }

            tsys.calculateTsys( );

        } // End loop over bands


    } // End TsysPipelineInfo::TsysInput::updateWithPipelineMonitorInput

// ========================== TsysPipelineInfo::Pimpl ==========================

class TsysPipelineInfo::Pimpl {
public:

    Pimpl( carma::pipeline::PipelineType plType );

    ~Pimpl( ); 

    TsysInput & astroInput( unsigned int astroInputNo );

    void
    updateWithPipelineMonitorInput( const PipelineMonitorInput & plmi );

    void
    resetTsys( const vector< int > & carmaAntNoVec,
               const PipelineMonitorInput & plmi );

private:
    
    void updateAtmosphericParameters( const PipelineMonitorInput & plmi );

    void updateAtmosphericOpacity( const PipelineMonitorInput & plmi );

    typedef ::std::vector< TsysInput > TsysInputVector;

    TsysInputVector astroInputs_;
    Atmosphere atmosphere_;
    SimpleOpacityModel opacityModel_;
    carma::pipeline::PipelineType plType_;

}; // End class TsysPipelineInfo::Pimpl

TsysPipelineInfo::Pimpl::Pimpl( carma::pipeline::PipelineType plType ) :
    astroInputs_( ),
    atmosphere_( ),
    opacityModel_( ),
    plType_( plType )
{
    const unsigned int maxAstroInputs = Global::nAstroInputs( );

    astroInputs_.reserve( maxAstroInputs );
    for ( unsigned int ai = 1; ai <= maxAstroInputs; ++ai ) 
        astroInputs_.push_back( TsysInput( ai, atmosphere_, plType_ ) );
}

TsysPipelineInfo::Pimpl::~Pimpl( ) { }

TsysPipelineInfo::TsysInput &
TsysPipelineInfo::Pimpl::astroInput( unsigned int astroInputNo )
{
    TsysInputVector::size_type astroInputIdx = astroInputNo - 1; 
    return astroInputs_.at( astroInputIdx );
}

void
TsysPipelineInfo::Pimpl::updateWithPipelineMonitorInput( 
    const PipelineMonitorInput & plmi ) 
{
    carma::util::frameType frame = plmi.frameCount( );
    
    CARMA_CPTRACE( TRACE_SET_GET, "TsysPipelineInfo::Pimpl::"
        "updateWithPipelineMonitorInput( ) - updating info for frame "
        << frame << "." ); 

    updateAtmosphericParameters( plmi );

    BOOST_FOREACH( TsysInput & input, astroInputs_ ) {
        input.updateWithPipelineMonitorInput( plmi );
    }
}

void
TsysPipelineInfo::Pimpl::resetTsys( const vector< int > & carmaAntNoVec,
                                    const PipelineMonitorInput & plmi )
{
    BOOST_FOREACH( const int antNo, carmaAntNoVec ) {
        const vector< BandInputPair > astroPairs = 
            plmi.getMappedAstroBandInputPairs( antNo );
        
        BOOST_FOREACH( const BandInputPair & abi, astroPairs ) {
            astroInput( abi.second ).astroBand( abi.first ).invalidate( );
        }
    }
}

void
TsysPipelineInfo::Pimpl::updateAtmosphericParameters(
    const PipelineMonitorInput & plmi )
{
    const monitor::WeatherSubsystem & weather = plmi.weather();

    if ( weather.pressure().isValid() ) 
        atmosphere_.lastSafeAtmPressure( weather.pressure().getValue() );

    if ( weather.ambientTemperature().isValid() )
        atmosphere_.lastSafeAirTemperature( 
            Temperature( weather.ambientTemperature().getValue(), "C" ) );

    if ( weather.humidity().isValid() )
        atmosphere_.lastSafeRelativeHumidity( weather.humidity().getValue() );

    if ( weather.dewpointTemperature().isValid() )
        atmosphere_.lastSafeDewPoint( 
            Temperature( weather.dewpointTemperature().getValue(), "C" ) );

    updateAtmosphericOpacity( plmi );
}

void
TsysPipelineInfo::Pimpl::updateAtmosphericOpacity( 
    const PipelineMonitorInput & plmi ) 
{
    const int usbIdx = 0;
    const int lsbIdx = 1;

    double opacityUsb, opacityLsb; // Zenith opacities 
    double elevationInDeg, elevationInRad, elevationFactor;

    const AstrobandRange abRange( getAstrobandRange( plType_ ) );
    for ( unsigned abNo = abRange.first; abNo <= abRange.second; ++abNo ) {

        const ControlBandPoints & cbp = plmi.controlBandPoints( abNo );

        if ( !cbp.sideband( usbIdx ).skyFreq().isValid() ||
             !cbp.sideband( lsbIdx ).skyFreq().isValid() ) 
            continue;

        // Calculate opacity for each band and sideband
        opacityUsb = opacityModel_.calculateOpacityAtZenith( 
                                cbp.sideband(usbIdx).skyFreq().getValue(),
                                atmosphere_.lastSafeAirTemperature( ).kelvin( ),
                                atmosphere_.lastSafeDewPoint( ).kelvin( ),
                                atmosphere_.lastSafeAtmPressure( ).millibar( ),
                                atmosphere_.lastSafeRelativeHumidity( ) );

        opacityLsb = opacityModel_.calculateOpacityAtZenith( 
                                cbp.sideband(lsbIdx).skyFreq().getValue(), 
                                atmosphere_.lastSafeAirTemperature( ).kelvin( ),
                                atmosphere_.lastSafeDewPoint( ).kelvin( ),
                                atmosphere_.lastSafeAtmPressure( ).millibar( ),
                                atmosphere_.lastSafeRelativeHumidity( ) );
        
        const TsysInputVector::size_type maxAstroInputs = astroInputs_.size();
        for ( unsigned int aiNo = 1; aiNo <= maxAstroInputs; ++aiNo ) {

            const AntPolPair antPol = plmi.getAntPolPair( abNo, aiNo );

            if ( ! plmi.signalPathMapped( abNo, antPol ) ) 
                continue;

            const AntennaCommon & antCommon = plmi.antennaCommon(antPol.first);

            if ( ! antCommon.drive().track().actualElevation().isValid() ) 
                continue;

            elevationInDeg = 
                antCommon.drive().track().actualElevation().getValue();

            elevationInRad = atmosphere_.safeElevation( 
                Angle( elevationInDeg, "degrees" ).radians( ) );

            elevationFactor = 1.0 / ( sin( elevationInRad ) );

            CARMA_CPTRACE( Trace::TRACE5, "TsysPipelineInfo::"
                "setAtmosphericOpacity( ) - input " << aiNo 
                << " band " << abNo << " opacity LSB/USB " 
                << opacityLsb << "/" << opacityUsb 
                << " elevation in deg/rad " 
                << elevationInDeg << "/" << elevationInRad << "." );

            astroInput( aiNo ).astroBand( abNo ).setAtmosphericOpacity( 
                opacityLsb * elevationFactor,
                opacityUsb * elevationFactor );
        }
    }
}

// ============================= TsysPipelineInfo ==============================

TsysPipelineInfo::TsysPipelineInfo( 
    const carma::pipeline::PipelineType plType ) 
    : pimpl_( new TsysPipelineInfo::Pimpl( plType ) )
{

}

TsysPipelineInfo::~TsysPipelineInfo( ) 
{

}

void
TsysPipelineInfo::resetTsys( const vector< int > & carmaAntNoVec,
                             const PipelineMonitorInput & plmi )
{
    pimpl_->resetTsys( carmaAntNoVec, plmi );
}

void 
TsysPipelineInfo::updateWithPipelineMonitorInput(
    const PipelineMonitorInput & plmi )
{
    pimpl_->updateWithPipelineMonitorInput( plmi );
}

double
TsysPipelineInfo::getTsysDsb( const unsigned int inputNo,
                              const unsigned int bandNo ) const
{
    return pimpl_->astroInput( inputNo ).astroBand( bandNo ).getTsysDsb( );
}

double
TsysPipelineInfo::getBaselineTsys( const unsigned int inputNo1,
                                   const unsigned int inputNo2,
                                   const unsigned int bandNo,
                                   const bool usb,
                                   pair< bool, bool > & validity ) const
{
    if ( usb ) 
        return getBaselineTsysUsb( inputNo1, inputNo2, bandNo, validity );
    else
        return getBaselineTsysLsb( inputNo1, inputNo2, bandNo, validity );
}
        
double
TsysPipelineInfo::getBaselineTsysUsb( const unsigned int inputNo1,
                                      const unsigned int inputNo2,
                                      const unsigned int bandNo,
                                      pair< bool, bool > & validity ) const
{
    const double tsys1 = pimpl_->astroInput(inputNo1).astroBand(bandNo).getTsysUsb( );
    const double tsys2 = pimpl_->astroInput(inputNo2).astroBand(bandNo).getTsysUsb( );
    const double avg = computeGeometricAverage( tsys1, tsys2 );
    validity.first = pimpl_->astroInput(inputNo1).astroBand(bandNo).valid();
    validity.second = pimpl_->astroInput(inputNo2).astroBand(bandNo).valid();
    return avg;
}

double
TsysPipelineInfo::getBaselineTsysLsb( const unsigned int inputNo1,
                                      const unsigned int inputNo2,
                                      const unsigned int bandNo,
                                      pair< bool, bool > & validity ) const
{
    const double tsys1 = pimpl_->astroInput(inputNo1).astroBand(bandNo).getTsysLsb( );
    const double tsys2 = pimpl_->astroInput(inputNo2).astroBand(bandNo).getTsysLsb( );
    const double avg = computeGeometricAverage( tsys1, tsys2 );
    validity.first = pimpl_->astroInput(inputNo1).astroBand(bandNo).valid();
    validity.second = pimpl_->astroInput(inputNo2).astroBand(bandNo).valid();
    return avg;
}

double 
TsysPipelineInfo::getBaselineJanskysPerKelvin( const unsigned int inputNo1,
                                               const unsigned int inputNo2,
                                               const unsigned int bandNo ) const
{
    const double jyperki = pimpl_->astroInput(inputNo1).astroBand(bandNo).getJyPerK( );
    const double jyperkj = pimpl_->astroInput(inputNo2).astroBand(bandNo).getJyPerK( );
    return computeGeometricAverage( jyperki, jyperkj );
}
    
const Tsys &
TsysPipelineInfo::getTsys( const unsigned int bandNo,
                           const unsigned int inputNo ) const
{
    return pimpl_->astroInput( inputNo ).astroBand( bandNo );
}
