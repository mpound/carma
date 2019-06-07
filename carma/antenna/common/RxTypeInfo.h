/**
 * @file
 * Declaration for carma::antenna::common::RxTypeInfo class.
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.6 $
 * $Date: 2011/01/03 18:48:05 $
 * $Id: RxTypeInfo.h,v 1.6 2011/01/03 18:48:05 iws Exp $
 */
#ifndef CARMA_ANTENNA_COMMON_RXTYPEINFO_H
#define CARMA_ANTENNA_COMMON_RXTYPEINFO_H

#include "carma/corba/corba.h"

#include "carma/antenna/common/RxControl.h"
#include "carma/antenna/common/IFControl.h"
#include "carma/monitor/AntennaCommon.h"
#include "carma/monitor/AntennaIF.h"

#include <string>

namespace carma {
namespace antenna {
namespace common {

    /**
     * Maps and provides conversions between the various receiver type 
     * definitions from the monitor system, control system and canbus
     * module.  Most enumeration mappings are obvious from the enumeration
     * name, however, for the PAM module, this class maps Rx to IF Switch
     * Position which is a numbered switch position (1,2,3,4).
     */
    class RxTypeInfo {
    public:
        
        /**
         * Constructor
         */
        explicit RxTypeInfo( carma::antenna::common::RxControl::Type rxType );

        /**
         * Destructor
         */
        /* virtual */ ~RxTypeInfo( );

        /**
         * Return receiver name as a string. 
         */
        ::std::string rxAsString( ) const;
        
        static ::std::string stringFromRxType( const RxControl::Type rxType );

        /**
         * Return receiver as RxControl::Type type.
         */
        carma::antenna::common::RxControl::Type
        rxAsRxControlType( ) const;

        /**
         * Return the IF Switch position (aka IF band) associated with this
         * receiver.
         */
        unsigned short rxAsIfSwitchPosition( ) const;

        /**
         * Get receiver as AntennaIF::IfSwitchStatMonitorPointEnum.
         */
        carma::monitor::AntennaIF::IfSwitchStatMonitorPointEnum::IFSWITCHSTAT 
        rxAsIfSwitchStatMonitorPointEnum( ) const;

        /**
         * Get receiver as AntennaCommon::CurrentMonitorPointEnum type.
         */
        carma::monitor::AntennaCommon::CurrentRxMonitorPointEnum::CURRENTRX
        rxAsCurrentRxMonitorPointEnum( ) const;

        static unsigned short
        ifSwitchPositionFromRxType( const RxControl::Type rxType );

    private:

        const carma::antenna::common::RxControl::Type rxControlType_;

        const ::std::string rxName_;
        
        const 
        carma::monitor::AntennaCommon::CurrentRxMonitorPointEnum::CURRENTRX 
        currentRxMonitorPointEnum_;

        const 
        carma::monitor::AntennaIF::IfSwitchStatMonitorPointEnum::IFSWITCHSTAT
        ifSwitchStatMonitorPointEnum_;

        const unsigned short ifSwitchPosition_;

    }; // End RxTypeInfo

}}} // namespace carma::antenna::common

// ====================Inline Implementation Follows ===========================
inline 
::std::string
carma::antenna::common::RxTypeInfo::rxAsString( ) const 
{
    return rxName_;
}
        
inline
carma::antenna::common::RxControl::Type
carma::antenna::common::RxTypeInfo::rxAsRxControlType( ) const
{
    return rxControlType_;
}

inline
unsigned short 
carma::antenna::common::RxTypeInfo::rxAsIfSwitchPosition( ) const
{
    return ifSwitchPosition_;
}

inline
carma::monitor::AntennaCommon::CurrentRxMonitorPointEnum::CURRENTRX
carma::antenna::common::RxTypeInfo::rxAsCurrentRxMonitorPointEnum( ) const
{
    return currentRxMonitorPointEnum_;
}

inline
carma::monitor::AntennaIF::IfSwitchStatMonitorPointEnum::IFSWITCHSTAT 
carma::antenna::common::RxTypeInfo::rxAsIfSwitchStatMonitorPointEnum( ) const
{
    return ifSwitchStatMonitorPointEnum_;
}
#endif 
