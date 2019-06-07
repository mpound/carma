/* $Id: AntennaControls.cc,v 1.63 2012/02/15 15:18:39 mpound Exp $ */
#include "carma/control/AntennaControls.h"

#include "carma/control/antennaHandleUtils.h"
#include "carma/control/AntennaHandle.h"
#include "carma/control/DriveHandle.h"
#include "carma/control/CalibratorHandle.h"
#include "carma/control/CryoHandle.h"
#include "carma/control/FocusHandle.h"
#include "carma/control/OpticalTelHandle.h"
#include "carma/control/RxSelectorHandle.h"
#include "carma/services/AntennaCoordinates.h"
#include "carma/services/Observatory.h"
#include "carma/services/padUtils.h"
#include "carma/services/Physical.h"
#include "carma/services/Vector.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Logger.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedQILockManager.h"

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::monitor;
using namespace carma::services;
using namespace carma::services::constants;
using namespace carma::util;
using namespace log4cpp;


AntennaControls::PersistentInfo::PersistentInfo( ) :
pad(),
eastPadOffset( 0.0, "mm" ),
northPadOffset( 0.0, "mm" ),
upPadOffset( 0.0, "mm" ),
eastAntennaOffset( 0.0, "mm" ),
northAntennaOffset( 0.0, "mm" ),
upAntennaOffset( 0.0, "mm" ),
totalEast( 0.0, "mm" ),
totalNorth( 0.0, "mm" ),
totalUp( 0.0, "mm" ),
axisMisalignment(0.0, "mm")
{
}

AntennaControls::AntennaControls(
    const unsigned short            carmaAntNo,
    const PersistentInfo &          persistentInfo,
    MonitorSystem &                 monitorSys,
    ControlSubsystemBase::Antenna & antenna ) :
carmaAntNo_( carmaAntNo ),
antennaHandle_( new AntennaHandle( carmaAntNo, monitorSys, antenna ) ),
calibratorHandle_( new CalibratorHandle( carmaAntNo, monitorSys, antenna) ),
cryoHandle_( new CryoHandle( carmaAntNo, monitorSys, antenna) ),
driveHandle_( new DriveHandle( carmaAntNo, persistentInfo.pad.getLocation(), monitorSys, antenna) ),
focusHandle_( new FocusHandle( carmaAntNo, monitorSys, antenna ) ),
opticalTelHandle_( new OpticalTelHandle( carmaAntNo, monitorSys, antenna) ),
rxSelectorHandle_( new RxSelectorHandle( carmaAntNo, monitorSys, antenna ) ),
persistentInfo_( persistentInfo )
{
    try {
        ostringstream oss;
        
        oss << "ANTENNACONTROLS constructed with "
            << " AntType="
            << computeAntennaTypeName( carmaAntNo_ )
            << " CarmaAnt#="
            << carmaAntNo_
            << " Drive carmaAntName="
            << driveHandle_->getCarmaAntennaName()
            << " Drive typed antName="
            << driveHandle_->getTypedAntennaName()
            << " Drive carmaAnt#="
            << driveHandle_->getCarmaAntennaNo()
            << " persistentInfo.pad.getLocation()="
            << persistentInfo.pad.getLocation()
            << " axisMisalignment=" 
            << persistentInfo.axisMisalignment;
        if(false) Program::getLogger() << Priority::INFO 
            << "C" << carmaAntNo << ": "
            << " persistentInfo.axisX:" 
            << persistentInfo.axisMisalignment
            << " persistentInfo.totalEast:"
            << persistentInfo.totalEast;
           
        // initialize the UVW interpolators
        Uinterp_.empty();
        Vinterp_.empty();
        Winterp_.empty();
        CARMA_CPTRACE( Trace::TRACE1, oss.str( ) );
    } catch ( ... ) {
        // Just stifle any exception
    }

    // To ensure we have a consistent absolute location.
    computeNewLocation(); 
}

AntennaControls::AntennaControls(
    const unsigned short            carmaAntNo,
    const PersistentInfo &          persistentInfo) :
carmaAntNo_( carmaAntNo ),
antennaHandle_( 0 ),
calibratorHandle_( 0 ),
cryoHandle_( 0 ),
driveHandle_( 0 ),
focusHandle_( 0 ),
opticalTelHandle_( 0 ),
rxSelectorHandle_( 0 ),
persistentInfo_( persistentInfo )
{
    try {
        // initialize the UVW interpolators
        Uinterp_.empty();
        Vinterp_.empty();
        Winterp_.empty();
    } catch ( ... ) {
        // Just stifle any exception
    }

    // To ensure we have a consistent absolute location.
    computeNewLocation(); 
}

AntennaControls::~AntennaControls( )
try {
} catch ( ... ) {
    // Just stifle any exception

    return;
}


void
AntennaControls::forceFullReconnect( )
{
    antennaHandle_->forceFullReconnect( );
    calibratorHandle_->forceFullReconnect( );
    cryoHandle_->forceFullReconnect( );
    driveHandle_->forceFullReconnect( );
    focusHandle_->forceFullReconnect( );
    opticalTelHandle_->forceFullReconnect( );
    rxSelectorHandle_->forceFullReconnect( );
}


void
AntennaControls::attemptToReconnectIfNeeded( )
{
    antennaHandle_->attemptToReconnectIfNeeded( );
    calibratorHandle_->attemptToReconnectIfNeeded( );
    cryoHandle_->attemptToReconnectIfNeeded( );
    driveHandle_->attemptToReconnectIfNeeded( );
    focusHandle_->attemptToReconnectIfNeeded( );
    opticalTelHandle_->attemptToReconnectIfNeeded( );
    rxSelectorHandle_->attemptToReconnectIfNeeded( );
}

AntennaHandle *
AntennaControls::antennaHandle( ) const
{
    return antennaHandle_.get( );
}


CalibratorHandle *
AntennaControls::calibratorHandle( ) const
{
    return calibratorHandle_.get( );
}


CryoHandle *
AntennaControls::cryoHandle( ) const
{
    return cryoHandle_.get( );
}


DriveHandle *
AntennaControls::driveHandle( ) const
{
    return driveHandle_.get( );
}


FocusHandle *
AntennaControls::focusHandle( ) const
{
    return focusHandle_.get( );
}


OpticalTelHandle *
AntennaControls::opticalTelHandle( ) const
{
    return opticalTelHandle_.get( );
}


RxSelectorHandle *
AntennaControls::rxSelectorHandle( ) const
{
   return rxSelectorHandle_.get( );
}

void 
AntennaControls::setPad( const Pad & pad )
{
    persistentInfo_.pad = pad;
    computeNewLocation();
}


Pad 
AntennaControls::getPad( ) const
{
    return persistentInfo_.pad;
}


void
AntennaControls::setPadOffsets( const Length & eastOffset,
                                const Length & northOffset,
                                const Length & upOffset )
{
    persistentInfo_.eastPadOffset  = eastOffset;
    persistentInfo_.northPadOffset = northOffset;
    persistentInfo_.upPadOffset    = upOffset;

    computeNewLocation();
}


vector< Length > 
AntennaControls::getPadOffsets( ) const
{
    vector< Length > v;
    
    v.reserve( 3 );
    
    v.push_back( persistentInfo_.eastPadOffset );
    v.push_back( persistentInfo_.northPadOffset );
    v.push_back( persistentInfo_.upPadOffset );
    
    return v;
}


void
AntennaControls::setAntennaOffsets( const Length & eastOffset,
                                const Length & northOffset,
                                const Length & upOffset )
{
    persistentInfo_.eastAntennaOffset  = eastOffset;
    persistentInfo_.northAntennaOffset = northOffset;
    persistentInfo_.upAntennaOffset    = upOffset;

    computeNewLocation();
}

vector< Length > 
AntennaControls::getAntennaOffsets( ) const
{
    vector< Length > v;
    
    v.reserve( 3 );
    
    v.push_back( persistentInfo_.eastAntennaOffset );
    v.push_back( persistentInfo_.northAntennaOffset );
    v.push_back( persistentInfo_.upAntennaOffset );
    
    return v;
}

vector< Length > 
AntennaControls::getTotalEnu( ) const
{
    vector< Length > v;
    
    v.reserve( 3 );
    
    v.push_back( persistentInfo_.totalEast );
    v.push_back( persistentInfo_.totalNorth );
    v.push_back( persistentInfo_.totalUp );
    
    return v;
}

services::Location
AntennaControls::persistentInfoLocation() const
{
    Length totalUpOffset    
        = persistentInfo_.upPadOffset    + persistentInfo_.upAntennaOffset;
    Length totalEastOffset  
        = persistentInfo_.eastPadOffset  + persistentInfo_.eastAntennaOffset;
    Length totalNorthOffset 
        = persistentInfo_.northPadOffset + persistentInfo_.northAntennaOffset;

    // Compute the new location. Be sure to use the
    // services routine which does it correctly!!
    return services::adjustedLocation( persistentInfo_.pad,
            totalEastOffset, totalNorthOffset, totalUpOffset
            );
}

void 
AntennaControls::computeNewLocation()
{
    const string METERS( "meters" );  
    const Location location = persistentInfoLocation();

    // update the total ENU values for this location with respect
    // to the array reference point
    AntennaCoordinates locCoord( location );
    AntennaCoordinates arrayRef( persistentInfo_.pad.getReference() );
    Vector<double> totalUen = arrayRef.getUen( locCoord );
    persistentInfo_.totalUp.reset( totalUen[0], METERS  );
    persistentInfo_.totalEast.reset( totalUen[1], METERS );
    persistentInfo_.totalNorth.reset( totalUen[2], METERS );

    {
        ostringstream os;
        os << "Antenna Controls setting new pad+offsets location: " 
           << persistentInfo_.pad.getName() << "   "
           << location << "  "
           << setprecision(4)
           << " East [m]: "  << totalUen[1]
           << " North [m]: " << totalUen[2]
           << " Up [m]: "    << totalUen[0]
           << " East [ns]: "  << totalUen[1]*Physical::NANOSEC_PER_METER
           << " North [ns]: " << totalUen[2]*Physical::NANOSEC_PER_METER
           << " Up [ns]: "    << totalUen[0]*Physical::NANOSEC_PER_METER
           ;

        CARMA_CPTRACE( Trace::TRACE5, os.str() );
        //programLogNoticeIfPossible( os.str() );
    }
                                  
    if ( driveHandle_.get() != 0 )
        driveHandle_->setLocation( location );
}

Location
AntennaControls::getAbsoluteLocation( ) const
{
    if ( driveHandle_.get() != 0 )
        return driveHandle_->getLocation( );
    else
        return persistentInfoLocation();
}


Length
AntennaControls::getAxisMisalignment( ) const
{
   return persistentInfo_.axisMisalignment;
}

void
AntennaControls::setAxisMisalignment( const Length& axisMis )
{
    persistentInfo_.axisMisalignment = axisMis;
}

unsigned short
AntennaControls::getCarmaAntennaNo( ) const
{
    return carmaAntNo_;
}


string
AntennaControls::getCarmaAntennaName( ) const
{
    return computeCarmaAntennaName( carmaAntNo_ );
}


string
AntennaControls::getTypedAntennaName( ) const
{
    return computeTypedAntennaName( carmaAntNo_ );
}

void 
AntennaControls::updateUVWInterpolators(
        const float U, 
        const float V, 
        const float W, 
        const double mjd,
        const bool discontinuity) 
{
    ScopedQILockManager uLockManager( Uinterp_, true );
    ScopedQILockManager vLockManager( Vinterp_, true );
    ScopedQILockManager wLockManager( Winterp_, true );

    uLockManager.lockQI();
    vLockManager.lockQI();
    wLockManager.lockQI();

    if ( discontinuity )
        Uinterp_.empty();
    Uinterp_.extend( mjd, U );

    if ( discontinuity )
        Vinterp_.empty();
    Vinterp_.extend( mjd, V );

    if ( discontinuity )
        Winterp_.empty();
    Winterp_.extend( mjd, W );
    
    wLockManager.unlockQI();
    vLockManager.unlockQI();
    uLockManager.unlockQI();

    ostringstream tmpStream;
    tmpStream << "AntennaControls - Successfully updated UVW "
              << " interpolators for antenna " 
              << carmaAntNo_ << " and MJD " << setprecision(12) << mjd;
    CARMA_CPTRACE(Trace::TRACE6, tmpStream);
    //programLogInfoIfPossible( tmpStream.str() );
}


AntennaControls::Uvw
AntennaControls::interpolateUVWfor( const double mjd )
{
    bool exceptionOccurred = false;
    ostringstream warning;
    warning << " AntennaControls::interpolatUVWfor( MJD ) - ";

    ScopedQILockManager uLockManager( Uinterp_, true );
    ScopedQILockManager vLockManager( Vinterp_, true );
    ScopedQILockManager wLockManager( Winterp_, true );

    uLockManager.lockQI();
    vLockManager.lockQI();
    wLockManager.lockQI();

    if ( !Uinterp_.canBracket(mjd) ) {
         warning << " !! Could not interpolate U because the MJD "
                 << setprecision(12)
                 << mjd << " is out of range of the U interpolator "
                 << "["
                 << setprecision(12)
                 << Uinterp_.getXmin()
                 << ","
                 << setprecision(12)
                 << Uinterp_.getXmax()
                 << "]";

         exceptionOccurred = true;
    }

    if ( !Vinterp_.canBracket(mjd) ) {
         warning << " !! Could not interpolate V because the MJD "
                 << setprecision(12)
                 << mjd << " is out of range of the V interpolator "
                 << "["
                 << setprecision(12)
                 << Vinterp_.getXmin()
                 << ","
                 << setprecision(12)
                 << Vinterp_.getXmax()
                 << "]";

         exceptionOccurred = true;
    }


    if ( !Winterp_.canBracket(mjd) ) {
         warning << " !! Could not interpolate W because the MJD "
                 << setprecision(12)
                 << mjd << " is out of range of the W interpolator "
                 << "["
                 << setprecision(12)
                 << Winterp_.getXmin()
                 << ","
                 << setprecision(12)
                 << Winterp_.getXmax()
                 << "]";

         exceptionOccurred = true;
    }

    if ( exceptionOccurred ) {
        wLockManager.unlockQI();
        vLockManager.unlockQI();
        uLockManager.unlockQI();

        throw CARMA_EXCEPTION(util::IllegalArgumentException, warning );
    }

    const Uvw uvw( Uinterp_.evaluate( mjd ),
                   Vinterp_.evaluate( mjd ),
                   Winterp_.evaluate( mjd ) );

    wLockManager.unlockQI();
    vLockManager.unlockQI();
    uLockManager.unlockQI();

    {
        ostringstream tmpStream;
        
        tmpStream << " AntennaControls::interpolatUVWfor( MJD ) - [u,v,w] = ["
                  << uvw.u << "," 
                  << uvw.v << "," 
                  << uvw.w << "]";
                          
        CARMA_CPTRACE( Trace::TRACE6, tmpStream.str() );
    }
    
    return uvw;
}

