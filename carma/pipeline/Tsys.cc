#include "carma/pipeline/Tsys.h"

#include "carma/services/Atmosphere.h"
#include "carma/util/Trace.h"

#include <cmath>

using namespace carma::monitor;
using namespace carma::pipeline;
using namespace carma::services;
using namespace carma::util;
using namespace std;

namespace {
    
    typedef AntennaCommon::CalStateMonitorPointEnum CalStateEnum; 

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE4;
    const Trace::TraceLevel TRACE_TSYS_CALC = Trace::TRACE7;
    const Trace::TraceLevel TRACE_TSYS_CORRECTIONS = Trace::TRACE5;
    const Trace::TraceLevel TRACE_SET_GET = Trace::TRACE6;

    const double MAX_TSYS            = 10000.0; // Kelvin
    const double MIN_TSYS            = 1.0; // Kelvin
    const int    VALID_FRAME_COUNT   = 72000; // 1 hour
    const double Tcmb                = 2.73; // Kelvin (may Rayleigh Jeans crct)

    double
    boundTsys( const double tsys ) // Provide reasonable bounds 
    {
        if ( tsys > MIN_TSYS && tsys < MAX_TSYS )
            return tsys;
        else 
            return MAX_TSYS;
    }

    double
    dBm2mW( double dBm )
    {
        return pow( 10.0, dBm / 10.0 );
    }

    double
    mw2dBm( double mw )
    {
        if ( mw == 0.0 )
            return 0.0;
        else 
            return 10.0 * log10( mw );
    }
    
} // End namespace < unnamed >

struct Tsys::TsysInfo {
    
    TsysInfo();

    ~TsysInfo();

    void calculateTsys( );
    void calculateTdsb( );
    void calculateTssb( );
    double calculateTsysAtmosphericCorrection( );
    double calculateTsysGroundSpilloverCorrection( );
    void updateNetSidebandRatio( );
    void updateAtmosphericGainDSB( );

    double                 totalPowerAmbMw_; // Psys Ambient Load in mW 
    double                 totalPowerAmbDBm_;
    bool                   totalPowerAmbValid_;
    carma::util::frameType totalPowerAmbFrame_;
    double                 totalPowerSky_; // Psys Sky Load in mW
    bool                   totalPowerSkyValid_;
    carma::util::frameType totalPowerSkyFrame_;
    double                 tsysDoubleSideband_;
    double                 tsysUpperSideband_;
    double                 tsysLowerSideband_;
    double                 effectiveCalTemp_;
    double                 rxSidebandRatio_;  // Receiver sideband ratio  
    double                 netSidebandRatio_; // Effective or observed sbr
    double                 atmosphericOpacityLsb_;
    double                 atmosphericOpacityDiff_; // tauUSB - tauLSB
    double                 atmosphericGainDoubleSideband_; // gDSBa
    double                 groundOpacity_; 
    double                 ambientTemperature_; // Outside ambient temperature
    double                 atmosphericTemperature_;
    double                 loadTemperature_; // Kelvin 
    double                 janskysPerKelvin_; // Jy Per k
    bool                   valid_;

}; // End Tsys::TsysInfo
    
Tsys::TsysInfo::TsysInfo( ) :
    totalPowerAmbMw_( 0.0 ),
    totalPowerAmbDBm_( 0.0 ),
    totalPowerAmbValid_( false ),
    totalPowerAmbFrame_( 0 ),
    totalPowerSky_( 0.0 ),
    totalPowerSkyValid_( false ),
    totalPowerSkyFrame_( 0 ),
    tsysDoubleSideband_( MAX_TSYS ),
    tsysUpperSideband_( MAX_TSYS ),
    tsysLowerSideband_( MAX_TSYS ),
    effectiveCalTemp_( 0.0 ),
    rxSidebandRatio_( 1.0 ),
    netSidebandRatio_( 1.0 ),
    atmosphericOpacityLsb_( 0.0 ),
    atmosphericOpacityDiff_( 0.0 ),
    atmosphericGainDoubleSideband_( 1.0 ),
    groundOpacity_( 0.0 ),
    ambientTemperature_( Atmosphere::DEFAULT_AIR_TEMP ),
    atmosphericTemperature_( 0.94 * Atmosphere::DEFAULT_AIR_TEMP ),
    loadTemperature_( Atmosphere::DEFAULT_AIR_TEMP ),
    janskysPerKelvin_( 1.0 ),
    valid_( false )
{
    CARMA_CPTRACE( Trace::TRACE7, "TsysInfo::TsysInfo @ " << this );
}

Tsys::TsysInfo::~TsysInfo( ) 
{
}

void
Tsys::TsysInfo::calculateTsys( )
{
    calculateTdsb( );
    calculateTssb( );
}

void 
Tsys::TsysInfo::calculateTdsb( )
{
    double diff = 0.0;    // Total power diff amb - sky
    double tdsb = MAX_TSYS;
    double Tcal = 0.0;    // Effective calibration temperature 
    double temperature = 0.0;
    
    Tcal  = loadTemperature_; 
    Tcal += calculateTsysAtmosphericCorrection( );
    Tcal += calculateTsysGroundSpilloverCorrection( );
     
    temperature = Tcal - Tcmb;

    effectiveCalTemp_ = temperature;

    if ( totalPowerAmbValid_ && totalPowerSkyValid_ ) {
            
        diff = totalPowerAmbMw_ - totalPowerSky_;
        if ( diff > 0.0 ) {
            tdsb = temperature * ( totalPowerSky_ / diff );
            CARMA_CPTRACE( TRACE_TSYS_CALC, "Tsys::calculateTdsb( ) "
                    "- Calculated tdsb with"
                    " tpAmb=" << mw2dBm( totalPowerAmbMw_ ) << 
                    ", tpSky=" << mw2dBm( totalPowerSky_ ) << 
                    ", temp=" << temperature << " and tdsb=" << tdsb << "." );
            tdsb = boundTsys( tdsb );
        }
        valid_ = true;
    } else {
        valid_ = false;
    }

    tsysDoubleSideband_ = tdsb;
    
    CARMA_CPTRACE( TRACE_TSYS_CALC, "Tsys::calculateTdsb( temp="
            << temperature << " ) - Done tdsb=" << tdsb << " " 
            << ( valid_ ? "VALID " : "INVALID" ) << "." );
}

void
Tsys::TsysInfo::calculateTssb( ) 
{
    // Peg to all upper sb, all lower sb or somewhere in-between
    if ( netSidebandRatio_ < 0.00001 ) {       // Peg to all lsb
        tsysLowerSideband_ = tsysDoubleSideband_;
        tsysUpperSideband_ = MAX_TSYS;
    } else if ( netSidebandRatio_ > 1000.0 ) { // Peg to all usb
        tsysUpperSideband_ = tsysDoubleSideband_;
        tsysLowerSideband_ = MAX_TSYS;
    } else {  // Calculate based on observed sideband ratio
        double obsSBR = netSidebandRatio_;

        tsysUpperSideband_ = boundTsys( 
                ( 1.0 + ( 1.0 / obsSBR ) ) * tsysDoubleSideband_ );
        tsysLowerSideband_ = boundTsys(
                ( 1.0 + obsSBR ) * tsysDoubleSideband_ );
    }
    CARMA_CPTRACE( TRACE_TSYS_CALC, "Tsys::calculateTssb( ) - "
        << " Done Tusb=" << tsysUpperSideband_ << " " 
        << "Tlsb=" << tsysLowerSideband_ << "." );
} 

double
Tsys::TsysInfo::calculateTsysAtmosphericCorrection( ) 
{
    double correction = 0.0;    
    correction = ( 1.0 / atmosphericGainDoubleSideband_ ) - 1.0;
    correction *= ( loadTemperature_ - atmosphericTemperature_ ); 
    CARMA_CPTRACE( TRACE_TSYS_CALC, 
                   "Tsys atmospheric correction = " << correction << "K w/ " 
                   << " aDSBgain = " << atmosphericGainDoubleSideband_ 
                   << " loadTemp = " << loadTemperature_ 
                   << " atmosphericTemperature = " << atmosphericTemperature_
                   << "." );
                   
    return correction;
}

double
Tsys::TsysInfo::calculateTsysGroundSpilloverCorrection( )
{
    double correction = 0.0;
    correction = ( 1.0 / atmosphericGainDoubleSideband_ );
    correction *= ( exp( groundOpacity_ ) - 1 );
    correction *= ( loadTemperature_ - ambientTemperature_ ); 
    CARMA_CPTRACE( TRACE_TSYS_CALC, 
                   "Tsys ground spill correction = " << correction << "K." );
    return correction;
}

void
Tsys::TsysInfo::updateNetSidebandRatio( )
{
    netSidebandRatio_ = rxSidebandRatio_ * 
                        exp( -1.0 * atmosphericOpacityDiff_ );
}

void
Tsys::TsysInfo::updateAtmosphericGainDSB( )
{
    atmosphericGainDoubleSideband_ = 
        ( ( 1.0 + rxSidebandRatio_ * exp( -1.0 * atmosphericOpacityDiff_ ) )
        / ( 1.0 + rxSidebandRatio_ ) ) * exp( -1.0 * atmosphericOpacityLsb_ );

    CARMA_CPTRACE( TRACE_TSYS_CORRECTIONS, 
        "TsysInfo::updateAtmosphericGainDSB( ) - "
        "With rxSidebandRAtio_ = " << rxSidebandRatio_ <<
        " atmosphericOpacityDiff_ = " << atmosphericOpacityDiff_ <<
        " and atmosphericOpacityLsb_ = " << atmosphericOpacityLsb_ << "." );
        
}
 
// =========================== Tsys Implementation =============================
Tsys::Tsys( ) : info_( new Tsys::TsysInfo( ) ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Tsys::Tsys() - C'tor." );
}

Tsys::Tsys( const Tsys & other ) : info_( new Tsys::TsysInfo( ) )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Tsys::Tsys() - Copy C'tor." );
    *( info_ ) = *( other.info_ );
}

Tsys::~Tsys( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, "Tsys::~Tsys() - D'tor." );
}

bool
Tsys::valid( ) const
{
    return info_->valid_;
}

void 
Tsys::invalidate( )
{
    info_->totalPowerAmbValid_ = false;
    info_->totalPowerSkyValid_ = false;
}

double
Tsys::getTsysDsb( ) const
{
    return info_->tsysDoubleSideband_;
}
    
double
Tsys::getTsysUsb( ) const
{
    return info_->tsysUpperSideband_;
}

double
Tsys::getTsysLsb( ) const
{
    return info_->tsysLowerSideband_;
}

void
Tsys::setReceiverSidebandRatio( const double sbr ) 
{
    info_->rxSidebandRatio_ = sbr;
    info_->updateNetSidebandRatio( );
    info_->updateAtmosphericGainDSB( );
}

void 
Tsys::setAtmosphericOpacity( const double tauLSB,
                             const double tauUSB )
{
    CARMA_CPTRACE( TRACE_TSYS_CORRECTIONS, 
        "Tsys::setAtmosphericOpacity( tauLSB = " << tauLSB << ", "
        << "tauUSB = " << tauUSB << " )." );

    info_->atmosphericOpacityLsb_ = tauLSB;
    info_->atmosphericOpacityDiff_ = tauUSB - tauLSB;
    info_->updateNetSidebandRatio( );
    info_->updateAtmosphericGainDSB( );
}

void
Tsys::setLoadTemperature( const double calTemp )
{
    info_->loadTemperature_ = calTemp;
}

void
Tsys::setOutsideAmbientTemperature( const double ambTemp )
{
    info_->ambientTemperature_ = ambTemp;
}

void
Tsys::setAtmosphericTemperature( const double atmosphericTemp )
{
    info_->atmosphericTemperature_ = atmosphericTemp;
}

void
Tsys::setGroundOpacity( const double opacity )
{
    info_->groundOpacity_ = opacity;
}

void
Tsys::setTotalPower( const CalStateEnum::CALSTATE calState,
                     const double totalPower,
                     const frameType frame )
{
    CARMA_CPTRACE( TRACE_SET_GET, "Tsys::updateTotalPower(calState="
            << calState << ", totalPower=" << totalPower << ", frame="
            << frame << ")." );
    switch ( calState ) {
        case CalStateEnum::SKY:
            info_->totalPowerSky_ = dBm2mW( totalPower );
            info_->totalPowerSkyValid_ = true;
            info_->totalPowerSkyFrame_ = frame;
            break;
        case CalStateEnum::AMB:
            info_->totalPowerAmbDBm_ = totalPower;
            info_->totalPowerAmbMw_ = dBm2mW( totalPower );
            info_->totalPowerAmbValid_ = true;
            info_->totalPowerAmbFrame_ = frame;
            break;
        case CalStateEnum::FIXED:
        case CalStateEnum::REFLEC:
        case CalStateEnum::MOVING:
        case CalStateEnum::ERROR:
        default:
            break;
    }
}

void
Tsys::setJanskysPerKelvin( const double jyPerK ) 
{
    info_->janskysPerKelvin_ = jyPerK;
}

double
Tsys::getJyPerK( ) const
{
    return info_->janskysPerKelvin_;
}

void
Tsys::calculateTsys( )
{
    info_->calculateTsys( );
}
            
double 
Tsys::getEffectiveTcal( ) const
{
    return info_->effectiveCalTemp_;
}

double
Tsys::getAmbPsysdBm( ) const
{
    return info_->totalPowerAmbDBm_;
}

std::string
Tsys::getDetails() const
{
    std::ostringstream oss;
    oss << "TPAMB=" << info_->totalPowerAmbMw_ << " mw "
        << "(" << (info_->totalPowerAmbValid_ ? "valid" : "invalid") << "),"
        << "TPSKY=" << info_->totalPowerSky_ << " mw "
        << "(" << ( info_->totalPowerSkyValid_ ? "valid" : "invalid" ) << "),"
        << "Tdsb=" << info_->tsysDoubleSideband_ << ", "
        << "Tusb=" << info_->tsysUpperSideband_ << ", "
        << "Tlsb=" << info_->tsysLowerSideband_ << ", "
        << "Teff=" << info_->effectiveCalTemp_ << ", "
        << "RxSbr=" << info_->rxSidebandRatio_ << ", "
        << "NetSbr=" << info_->netSidebandRatio_ << ", "
        << "TauLsb=" << info_->atmosphericOpacityLsb_ << ", "
        << "AtmGainDsb=" << info_->atmosphericGainDoubleSideband_ << ", "
        << "GroundOpacity=" << info_->groundOpacity_ << ", "
        << "Tamb= " << info_->ambientTemperature_ << ", "
        << "Tatmo=" << info_->atmosphericTemperature_ << ", "
        << "Tload=" << info_->loadTemperature_ << ", "
        << "JyPerK=" << info_->janskysPerKelvin_ << ", "
        << (info_->valid_ ? "VALID" : "INVALID" );
    return oss.str();
}

