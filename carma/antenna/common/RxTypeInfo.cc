
#include "carma/antenna/common/RxTypeInfo.h"

#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Trace.h"

using namespace carma::antenna::common;
using namespace carma::monitor;
using namespace carma::util;

namespace { 

    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE2;

}

    ::std::string
    RxTypeInfo::stringFromRxType( const RxControl::Type rxType ) 
    {
        switch ( rxType ) {
            case RxControl::RX1CM: return "RX1CM";
            case RxControl::RX1MM: return "RX1MM";
            case RxControl::RX3MM: return "RX3MM";
            default: return "UNKNOWN";
        }
        return "UNKNOWN";
    } // stringFromRxType
                
namespace {

    AntennaCommon::CurrentRxMonitorPointEnum::CURRENTRX
    currentRxMonitorPointEnumFromRxType( const RxControl::Type rxType )
    {
        typedef AntennaCommon::CurrentRxMonitorPointEnum CurrentRxMPE;
        switch ( rxType ) {
            case RxControl::RX1CM: return CurrentRxMPE::RX1CM;
            case RxControl::RX1MM: return CurrentRxMPE::RX1MM;
            case RxControl::RX3MM: return CurrentRxMPE::RX3MM;
            case RxControl::RXANY: return CurrentRxMPE::RXANY;
        }
        throw CARMA_EXCEPTION( IllegalArgumentException,
                               "Switch does not match any case!" );
    } // currentRxMonitorPointEnumFromRxType

    carma::monitor::AntennaIF::IfSwitchStatMonitorPointEnum::IFSWITCHSTAT
    ifSwitchStatFromRxType( const RxControl::Type rxType )     
    {
        typedef carma::monitor::AntennaIF::IfSwitchStatMonitorPointEnum IfStat;
        switch ( rxType ) {
            case RxControl::RX1CM: return IfStat::POS_1;
            case RxControl::RX3MM: return IfStat::POS_2;
            case RxControl::RX1MM: return IfStat::POS_3;
            case RxControl::RXANY: return IfStat::POS_4;         
        }         
        throw CARMA_EXCEPTION( IllegalArgumentException,
                               "Switch does not match any case!" );     
    } // ifSwitchStatFromRxType 

} // namespace <unnamed>

RxTypeInfo::RxTypeInfo( carma::antenna::common::RxControl::Type rxType ) :
    rxControlType_( rxType ),
    rxName_( stringFromRxType( rxType ) ),
    currentRxMonitorPointEnum_( currentRxMonitorPointEnumFromRxType( rxType ) ),
    ifSwitchStatMonitorPointEnum_( ifSwitchStatFromRxType( rxType ) ),
    ifSwitchPosition_( ifSwitchPositionFromRxType( rxType ) )
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "RxTypeInfo::RxTypeInfo( rxType=" << rxName_ << " )." );
}

RxTypeInfo::~RxTypeInfo( ) 
{
    CARMA_CPTRACE( TRACE_CTOR_DTOR, 
                   "RxTypeInfo::~RxTypeInfo( ) - D'tor for " << rxName_ << " )." );
}

unsigned short
RxTypeInfo::ifSwitchPositionFromRxType( const RxControl::Type rxType )
{
    switch ( rxType ) {
        case RxControl::RX1CM: return 1; 
        case RxControl::RX3MM: return 2;
        case RxControl::RX1MM: return 3;
        case RxControl::RXANY: return 4;
    }
    throw CARMA_EXCEPTION( IllegalArgumentException,
            "Switch does not match any case!" );
} // ifSwitchPositionFromRxType
