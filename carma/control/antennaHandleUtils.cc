#include "carma/control/antennaHandleUtils.h"

#include <stdexcept>
#include <sstream>

#include "carma/corba/corba.h"
#include "carma/control/SubarrayControl.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/BimaSubsystem.h"
#include "carma/monitor/OvroSubsystem.h"
#include "carma/monitor/SzaSubsystem.h"


using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::monitor;


namespace {

struct TypedAntInfo {
    const AntennaType    antType;
    const unsigned short typedAntNo;
    
    TypedAntInfo( AntennaType inAntType, unsigned short inTypedAntNo );
};


TypedAntInfo::TypedAntInfo( const AntennaType    inAntType,
                            const unsigned short inTypedAntNo ) :
antType( inAntType ),
typedAntNo( inTypedAntNo ) {
}


TypedAntInfo
getTypedAntInfoForCarmaAntNo( const unsigned short carmaAntNo ) {
    if ( (carmaAntNo >= 1) && (carmaAntNo <= 6) )
        return TypedAntInfo( ANTENNA_TYPE_OVRO, carmaAntNo );
    
    if ( (carmaAntNo >= 7) && (carmaAntNo <= 15) )
        return TypedAntInfo( ANTENNA_TYPE_BIMA, (carmaAntNo - 6) );
    
    if ( (carmaAntNo >= 16) && (carmaAntNo <= 23) )
        return TypedAntInfo( ANTENNA_TYPE_SZA, (carmaAntNo - 15) );
    
    throw runtime_error( "Invalid carma antenna number" );
}


string
getAntTypeName( const AntennaType antType ) {
    switch ( antType )  {
        case ANTENNA_TYPE_BIMA:  return "bima";
        case ANTENNA_TYPE_OVRO:  return "ovro";
        case ANTENNA_TYPE_SZA :  return "sza";
    }

    throw runtime_error( "Invalid antenna type" );
}


}  // namespace < anonymous >


MonitorSubsystem &
carma::control::getAntennaSubsystem( const unsigned short   carmaAntNo,
                                     const MonitorSystem &  carma ) {
    const TypedAntInfo info = getTypedAntInfoForCarmaAntNo( carmaAntNo );
    
    MonitorSubsystem * antSubsystem = 0;

    switch ( info.antType ) {
        case ANTENNA_TYPE_BIMA:
            antSubsystem = &(carma.bima( info.typedAntNo - 1 ));
            break;
            
        case ANTENNA_TYPE_OVRO:
            antSubsystem = &(carma.ovro( info.typedAntNo - 1 ));
            break;
            
        case ANTENNA_TYPE_SZA:
            antSubsystem = &(carma.sza( info.typedAntNo - 1 ));
            break;
    }

    if ( antSubsystem == 0 )
        throw runtime_error( "NULL antenna subsystem" );
        
    return *antSubsystem;
}


AntennaCommon&
carma::control::getAntennaCommon( const unsigned short   carmaAntNo,
                                     const MonitorSystem &  carma ) 
{
    const TypedAntInfo info = getTypedAntInfoForCarmaAntNo( carmaAntNo );
    
    AntennaCommon* antCommon = 0;

    switch (info.antType) {
        case ANTENNA_TYPE_BIMA:
            antCommon = &(carma.bima(info.typedAntNo - 1).antennaCommon());
            break;
            
        case ANTENNA_TYPE_OVRO:
            antCommon = &(carma.ovro(info.typedAntNo - 1).antennaCommon());
            break;
            
        case ANTENNA_TYPE_SZA:
            antCommon = &(carma.sza(info.typedAntNo - 1).antennaCommon());
            break;
    }

    if (antCommon == 0)
        throw runtime_error("NULL antenna common");
        
    return *antCommon;
}


string
carma::control::computeCarmaAntennaName( const unsigned short carmaAntNo ) {
    ostringstream oss;
    
    oss << "carma" << carmaAntNo;
    
    return oss.str( );
}


string
carma::control::computeTypedAntennaName( const unsigned short carmaAntNo ) {
    const TypedAntInfo info = getTypedAntInfoForCarmaAntNo( carmaAntNo );
    
    ostringstream oss;
    
    oss << getAntTypeName( info.antType ) << info.typedAntNo;
    
    return oss.str( );
}


AntennaType
carma::control::computeAntennaType( const unsigned short carmaAntNo ) {
    return getTypedAntInfoForCarmaAntNo( carmaAntNo ).antType;
}


string
carma::control::computeAntennaTypeName( const unsigned short carmaAntNo ) {
    return getAntTypeName( computeAntennaType( carmaAntNo ) );
}


string
carma::control::makeAntennaDoName( const unsigned short   carmaAntNo,
                                   const string &         leafName ) {
    return string( "carma." ) +
           computeTypedAntennaName( carmaAntNo ) +
           string( "." ) +
           leafName;
}
