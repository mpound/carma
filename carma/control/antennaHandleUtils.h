#ifndef CARMA_CONTROL_ANTENNA_HANDLE_UTILS_H
#define CARMA_CONTROL_ANTENNA_HANDLE_UTILS_H

#include <string>

namespace carma {
    // Forward declarations
    namespace monitor {
        class AntennaCommon;
        class MonitorSystem;
        class MonitorSubsystem;
    }  // namespace carma::monitor

namespace control {


// TODO? Replace this with services::AntennaType
//! @brief Enumeration of possible types for antennas.
typedef enum {
    ANTENNA_TYPE_BIMA,
    ANTENNA_TYPE_OVRO,
    ANTENNA_TYPE_SZA
} AntennaType;


//! @brief Extracts antenna's monitor subsystem from the CARMA monitor system,
//!        given a carma antenna number.
//!
//! @param carmaAntNo carma antenna number
//!
//! @param carmaMonitor CARMA monitor system
//!
//! @return antenna monitor subsystem
monitor::MonitorSubsystem &
getAntennaSubsystem( unsigned short                 carmaAntNo,
                     const monitor::MonitorSystem & carmaMonitor);


/**
 * Get the antennaCommon container for an antenna
 * @param carmaAntNo carma (physical) antenna number
 */
monitor::AntennaCommon& 
        getAntennaCommon(const unsigned short carmaAntNo,
                         const monitor::MonitorSystem& carmaMonitor);
        

::std::string makeAntennaDoName( unsigned short        carmaAntNo,
                                 const ::std::string & leafName );
                                 
                                 
AntennaType computeAntennaType( unsigned short carmaAntNo );

::std::string computeAntennaTypeName( unsigned short carmaAntNo );

::std::string computeCarmaAntennaName( unsigned short carmaAntNo );
::std::string computeTypedAntennaName( unsigned short carmaAntNo );


}  // namespace carma::control
}  // namespace carma


#endif
