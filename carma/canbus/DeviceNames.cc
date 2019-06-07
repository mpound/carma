/** @file
 * Definition of carma::canbus::DeviceNames class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.8 $
 * $Date: 2009/09/02 23:10:35 $
 * $Id: DeviceNames.cc,v 1.8 2009/09/02 23:10:35 abeard Exp $
 */

#include <map>

#include "carma/canbus/DeviceNames.h"

using namespace carma::canbus;
using namespace std;

namespace {

    typedef map<apiType, string> deviceNameMapType;

    deviceNameMapType::value_type _deviceNames[] = 
    {
        deviceNameMapType::value_type(4, "Block Downconverter"),
        deviceNameMapType::value_type(8, "SZA Caltert"),
        deviceNameMapType::value_type(16, "Bias-tuned Gunn"),
        deviceNameMapType::value_type(24, "Spectral-line Downconverter"),
        deviceNameMapType::value_type(32, "OVRO Cryo Compressor"),
        deviceNameMapType::value_type(40, "Tiltmeter"),
        deviceNameMapType::value_type(48, "Varactor-tuned Gunn"),
        deviceNameMapType::value_type(56, "OVRO Secondary Mirror"),
        deviceNameMapType::value_type(64, "Quadrature Noise Modulator"),
        deviceNameMapType::value_type(65, "Quadrature Noise Modulator"),
        deviceNameMapType::value_type(66, "Quadrature Noise Modulator"),
        deviceNameMapType::value_type(72, "OVRO Optics Control"),
        deviceNameMapType::value_type(80, "YIG Reference Oscillator"),
        deviceNameMapType::value_type(88, "OVRO Environmental Monitor"),
        deviceNameMapType::value_type(96, "Correlated Noise Source"),
        deviceNameMapType::value_type(97, "Correlated Noise Source"),
        deviceNameMapType::value_type(104, "SZA Rx Box Thermal Controller"),
        deviceNameMapType::value_type(112, "Loberotator"),
        deviceNameMapType::value_type(113, "Loberotator"),
        deviceNameMapType::value_type(120, "SZA Rack Fan Controller"),
        deviceNameMapType::value_type(128, "COBRA Downconverter"),
        deviceNameMapType::value_type(129, "COBRA Downconverter II"),
        deviceNameMapType::value_type(130, "Wideband Downconverter"),
        deviceNameMapType::value_type(136, "SZA Compressor Louver Controller"),
        deviceNameMapType::value_type(144, "BIMA Telemetry"),
        deviceNameMapType::value_type(160, "OVRO Cryo Temperatures"),
        deviceNameMapType::value_type(168, "OVRO Rx Electronics Temperature "
                "Controller"),
        deviceNameMapType::value_type(176, "SZA Receiver Control"),
        deviceNameMapType::value_type(184, "LO Reference Monitor Module"),
        deviceNameMapType::value_type(192, "Downconverter LO Monitor"),
        deviceNameMapType::value_type(200, "Spectral-line Downconverter LO "
                "Control"),
        deviceNameMapType::value_type(208, "OVRO SIS Receiver"),
        deviceNameMapType::value_type(209, "OVRO SIS Receiver"),
        deviceNameMapType::value_type(216, "Low-level I/O CANbus Module"),
        deviceNameMapType::value_type(224, "Antenna IF"),
        deviceNameMapType::value_type(232, "Encoder Module"),
        deviceNameMapType::value_type(240, "Master Clock"),
        deviceNameMapType::value_type(248, "Drive Module")
    };

    // Initialize Device::deviceNames_ map with _deviceNames array...
    // This is a bit cumbersome but it allows me to use a map rather than a 
    // 2-d array.
    const deviceNameMapType deviceNames_(
                _deviceNames, 
                _deviceNames + sizeof(_deviceNames) / sizeof(_deviceNames[0])
                );

} // End anonymous namespace


// -----------------------------------------------------------------------------
bool DeviceNames::isRegistered ( apiType api )
{
    return ( !( deviceNames_.find( api )  == deviceNames_.end( ) ) );
}

// -----------------------------------------------------------------------------
std::string DeviceNames::getName( apiType api )
{
    // Since our map is const we must use a const_iterator here.
    map<apiType, string>::const_iterator dev = deviceNames_.find( api );

    if ( dev == deviceNames_.end( ) )
        return "Unknown";
    else 
        return dev->second;

    // Should never reach this
    return "Unknown";
}
