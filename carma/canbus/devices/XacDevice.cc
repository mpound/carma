/**@file
 * carma::canbus::devices::XacDevice class definition.
 *
 *<dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.29 $
 * $Date: 2011/08/25 20:53:19 $
 * $Id: XacDevice.cc,v 1.29 2011/08/25 20:53:19 abeard Exp $
 */

#include "carma/canbus/devices/XacDevice.h"

#include "carma/canbus/DeviceNames.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"

#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

#include <iostream>
#include <iomanip>

using namespace carma::canbus;
using namespace carma::canbus::devices;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace { // Anonymous namespace for local constants, typedefs, etc.

    typedef ::std::map<msgType, ::std::string> CanMsgMap;

    const CanMsgMap::value_type slowMonArray[] = {
        CanMsgMap::value_type(XacDevice::SYSTEM_MONITOR_PACKET_1,
                              "SYSTEM_MONITOR_PACKET_1"),
        CanMsgMap::value_type(XacDevice::SYSTEM_MONITOR_PACKET_2,
                              "SYSTEM_MONITOR_PACKET_2"),
        CanMsgMap::value_type(XacDevice::SYSTEM_MONITOR_PACKET_3,
                              "SYSTEM_MONITOR_PACKET_3"),
        CanMsgMap::value_type(XacDevice::SYSTEM_MONITOR_PACKET_4,
                              "SYSTEM_MONITOR_PACKET_4"),
        CanMsgMap::value_type(XacDevice::SYSTEM_MONITOR_PACKET_5,
                              "SYSTEM_MONITOR_PACKET_5")
    };

    const CanMsgMap slowMonitors(
        slowMonArray,
        slowMonArray + sizeof(slowMonArray) / sizeof(slowMonArray[0]) );

    // Broken Down Time Build Epoch
    struct tm BD_BUILDTIME_EPOCH = {
        0, // 0 seconds
        0, // 0 minutes
        0, // 0 hour
        1, // 1 day
        0, // months since january
        105 // Years since 1900 (i.e. 2005)
        // Other variables get filled in by mktime.
    };

    // Time Build Epoch (in seconds since THE epoch 00:00:00 UTC 1/1/1970).
    const time_t BUILDTIME_EPOCH = ::mktime( &BD_BUILDTIME_EPOCH );

    struct tm
    convertMinutesSinceBuildEpochToBuildTime( const unsigned int minutes ) 
    {
        const time_t secondsSinceBTEpoch = BUILDTIME_EPOCH + ( minutes * 60 );
        struct tm buildtime;
        struct tm * answer = ::localtime( &secondsSinceBTEpoch );

        // Copy answer to local memory - this is not threadsafe.
        if ( 0 != answer ) {
            memcpy( &buildtime, answer, sizeof(struct tm) );
        } else {
            throw std::logic_error("::localtime failed");
        }
        return buildtime;
    }

    unsigned int
    convertBuildTimeToMinutesSinceBuildEpoch( struct tm & buildtime )
    {
        const time_t build = ::mktime( &buildtime );
        const double diff = ::difftime( build, BUILDTIME_EPOCH );

        return static_cast<unsigned int>( diff / 60.0 );
    }

    const string months[12] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

    string
    buildTimeToDateString( const struct tm & time ) 
    {
        ostringstream os;
        os << setw( 2 ) << setfill( '0' ) << time.tm_mday
           << months[time.tm_mon] << setw( 2 ) << setfill( '0' ) 
           << ( time.tm_year - 100 );
        return os.str( );
    }

    string
    buildTimeToTimeString( const struct tm & time )
    {
        const string ampm = ( ( time.tm_hour / 12 )  == 0 ? "AM" : "PM" );
        
        ostringstream os;
        os << setw( 2 ) << setfill( '0' ) << time.tm_hour % 12 << ":"
           << setw( 2 ) << setfill( '0' ) << time.tm_min << " " << ampm;
        return os.str();
    }


} // End namespace <unnamed>

const carma::canbus::msgType XacDevice::SYSTEM_MONITOR_PACKET_1;
const carma::canbus::msgType XacDevice::SYSTEM_MONITOR_PACKET_2;
const carma::canbus::msgType XacDevice::SYSTEM_MONITOR_PACKET_3;
const carma::canbus::msgType XacDevice::SYSTEM_MONITOR_PACKET_4;
const carma::canbus::msgType XacDevice::SYSTEM_MONITOR_PACKET_5;

// -----------------------------------------------------------------------------
XacDevice::XacDevice(apiType api, nodeType node, CanOutput &canOutput) : 
    Device(api, node, canOutput),
    startFrame_( Time::computeCurrentFrame( ) )
{
    // Nothing here
}

// -----------------------------------------------------------------------------
XacDevice::~XacDevice()
{
    // Nothing here
}

// -----------------------------------------------------------------------------
map<msgType, string> XacDevice::getSlowMonitors() const
{
    return slowMonitors;
}

// -----------------------------------------------------------------------------
void XacDevice::setState(deviceStateType state)
{
    // get logger, log and set state.
    Category & log = Program::getLogger();
    string strstate;

    switch (state) {
        case ONLINE:
            strstate = "ONLINE";
            break;
        case STARTING:
            strstate = "STARTING";
            break;
        case OFFLINE:
            strstate = "OFFLINE";
            break;
        case SIMULATED:
            strstate = "SIMULATED";
            break;
        default:
            strstate = "INVALID!";
            break;
    }

    log << Priority::INFO << DeviceNames::getName( getApi() ) << " CAN module "
        << "node " << getNode() << " " << strstate;

    Device::setState(state);
}

// -----------------------------------------------------------------------------
void XacDevice::initialize( )
{
    // Override for your own functionality.
}

// -----------------------------------------------------------------------------
bool XacDevice::isSystemMonitorPacket( const msgType mid ) 
{
    switch ( mid ) {
        case SYSTEM_MONITOR_PACKET_1:
        case SYSTEM_MONITOR_PACKET_2:
        case SYSTEM_MONITOR_PACKET_3:
        case SYSTEM_MONITOR_PACKET_4:
        case SYSTEM_MONITOR_PACKET_5:
            return true;
        default:
            return false;
    }
    return false;
}

// -----------------------------------------------------------------------------
void XacDevice::processSystemMonitorPacket( 
    msgType mid,
    DataVector & data,
    Xac & xac )
{
    switch ( mid ) {
        case SYSTEM_MONITOR_PACKET_1:
            processSystemMonitorPacket1( data, xac );
            break;
        case SYSTEM_MONITOR_PACKET_2:
            processSystemMonitorPacket2( data, xac );
            break;
        case SYSTEM_MONITOR_PACKET_3:
            processSystemMonitorPacket3( data, xac );
            break;
        case SYSTEM_MONITOR_PACKET_4:
            processSystemMonitorPacket4( data, xac );
            break;
        case SYSTEM_MONITOR_PACKET_5:
            processSystemMonitorPacket5( data, xac );
            break;
        default:
            break;
    }
}

// -----------------------------------------------------------------------------
void XacDevice::processSystemMonitorPacket1(
    std::vector<carma::canbus::byteType> &data,
    carma::monitor::Xac& xac)
{
    serialNumberType sn;
    boardType bt;
    byteType init, rxErrs, txErrs, memErrs;

    // Make sure data is 8 bytes long
    if (data.size() != 8) {
        throw CARMA_EXCEPTION(BadDataSizeException, 
            "XacDevice::processSystemMonitorPacket1() - Data is not "
            "8 bytes!");
    }

    // Unpack the data
    sn = dataToUshort(data);
    bt = dataToUbyte(data);
    init = dataToUbyte(data);
    rxErrs = dataToUbyte(data);
    txErrs = dataToUbyte(data);
    memErrs = dataToUshort(data);
    
    // Set device state and parameters
    setSerialNumber(sn);  // Sets the serial number for the device
    setBoardType(bt);     // Sets the board type for the device

    // Convert and place data into monitor stream
    xac.serialNo().setValue(sn);
    xac.modType().setValue(bt);
    xac.init().setValue((init != 0));
    xac.nCanRxErrs().setValue(rxErrs);
    xac.nCanTxErrs().setValue(txErrs);
    xac.nMemErrs().setValue(memErrs);
    
    if ( init ) {
        initialize( );
    }

    // Ugly Hack - the bus id doesn't come from a CAN message, but 
    // setting it from here is a quick way to make sure it get's set for all
    // devices.
    xac.busId().setValue( getBusId() );
}

// -----------------------------------------------------------------------------
void XacDevice::processSystemMonitorPacket2(
    std::vector<carma::canbus::byteType> &data,
    carma::monitor::Xac& xac)
{
    unsigned int soCnt, tsoCnt; // Scheduler and timed scheduler overflow cnts.
    byteType swVerMaj, swVerMin, swVerTest;  // Firmware version info.
    byteType testMode;
    ostringstream os;

    // Make sure data is 8 bytes long
    if (data.size() != 8) {
        throw CARMA_EXCEPTION(BadDataSizeException, 
            "XacDevice::processSystemMonitorPacket2() - Data is not "
            "8 bytes!");
    }

    // Unpack the data
    soCnt = dataToUshort(data);
    tsoCnt = dataToUshort(data);
    swVerMaj = dataToUbyte(data);
    swVerMin = dataToUbyte(data);
    swVerTest = dataToUbyte(data);
    testMode = dataToUbyte(data);
    

    // Convert and place into monitor stream
    os << static_cast<short>(swVerMaj) << "."
       << static_cast<short>(swVerMin) << "."
       << static_cast<short>(swVerTest);

    xac.nSchedOverflows().setValue(soCnt);
    xac.nTimeOverflows().setValue(tsoCnt);
    xac.fwVersion().setValue(os.str());
    xac.testMode().setValue(static_cast<short>(testMode));
    
}

// -----------------------------------------------------------------------------
void XacDevice::processSystemMonitorPacket3(
    std::vector<carma::canbus::byteType> &data,
    carma::monitor::Xac& xac)
{
    unsigned short comErrs, timeErrs, swErrs, hwErrs;

    // Make sure data is 8 bytes long
    if (data.size() != 8) {
        throw CARMA_EXCEPTION(BadDataSizeException, 
            "XacDevice::processSystemMonitorPacket3() - Data is not "
            "8 bytes!");
    }

    // Unpack the data
    comErrs = dataToUshort(data);
    timeErrs = dataToUshort(data);
    swErrs = dataToUshort(data);
    hwErrs = dataToUshort(data);

    // Place data into monitor stream
    xac.nCommErrs().setValue(static_cast<int>(comErrs));
    xac.nTimeErrs().setValue(static_cast<int>(timeErrs));
    xac.nSwErrs().setValue(static_cast<int>(swErrs));
    xac.nHwErrs().setValue(static_cast<int>(hwErrs));

}

// -----------------------------------------------------------------------------
void XacDevice::processSystemMonitorPacket4(
    std::vector<carma::canbus::byteType> &data,
    carma::monitor::Xac& xac)
{
    short timeJitter, sinceLastTs, tsDelta;
    unsigned char apiVersion;

    // Make sure data is atleast 7 bytes long
    if (data.size() < 7) {
        throw CARMA_EXCEPTION(BadDataSizeException, 
            "XacDevice::processSystemMonitorPacket4() - Data is greater "
            "than 7 bytes!");
    }

    // Unpack the data
    timeJitter = dataToShort(data);
    sinceLastTs = dataToShort(data);
    tsDelta = dataToShort(data);
    apiVersion = dataToUbyte(data);
    
    // Convert and place into monitor stream
    xac.timeJitter().setValue(timeJitter);
    xac.timeSinceLastTs().setValue(sinceLastTs);
    xac.tsDelta().setValue(tsDelta);
    xac.apiVer().setValue(static_cast<char>(apiVersion));
    
}

// -----------------------------------------------------------------------------
void XacDevice::processSystemMonitorPacket5(
    std::vector<carma::canbus::byteType> &data,
    carma::monitor::Xac& xac)
{
    long uptime;
    unsigned short bootloader;
    unsigned int buildtime;
    
    // Make sure data is atleast 5 bytes long
    if (data.size() < 5) {
        throw CARMA_EXCEPTION(BadDataSizeException, 
            "XacDevice::processSystemMonitorPacket5() - Data is less "
            "than the minimum 5 bytes!");
    }

    // Unpack

    uptime = dataToUlong(data);
    bootloader = dataToUbyte(data);
    // This is a dirty trick, the buildtime is a 3 byte number in network byte
    // order.  So I tack an empty byte at the beginning of the vector and 
    // process it like normal.
    data.insert( data.begin( ),  0x00 );
    buildtime = dataToUlong( data );
    
    const struct tm bt = convertMinutesSinceBuildEpochToBuildTime( buildtime );
    
    bootloader &= 0x01; // Mask off first bit only

    // Convert and place into monitor stream...
    xac.uptime().setValue(uptime);
    xac.bootloader().setValue( 
            (bootloader == 0x00 ? Xac::BootloaderMonitorPointEnum::ABSENT :
                Xac::BootloaderMonitorPointEnum::PRESENT) );
    xac.buildDate().setValue( buildTimeToDateString( bt ) );
    xac.buildTime().setValue( buildTimeToTimeString( bt ) ); 

}

// -----------------------------------------------------------------------------
carma::canbus::Message XacDevice::simSystemMonitorPacket( 
    const msgType mid )
{
    switch ( mid ) {
        case SYSTEM_MONITOR_PACKET_1:
            return simSystemMonitorPacket1( );
        case SYSTEM_MONITOR_PACKET_2:
            return simSystemMonitorPacket2( );
        case SYSTEM_MONITOR_PACKET_3:
            return simSystemMonitorPacket3( );
        case SYSTEM_MONITOR_PACKET_4:
            return simSystemMonitorPacket4( );
        case SYSTEM_MONITOR_PACKET_5:
            return simSystemMonitorPacket5( );
        default:
            ostringstream msg;
            msg << "XacDevice::simSystemMonitorPacket( mid=" << mid << " ) - "
                << "Message id is not system monitor packet.";
            throw CARMA_EXCEPTION( IllegalArgumentException, msg.str( ) ); 
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message XacDevice::simSystemMonitorPacket1()
{
    Message msg(
        createId(true, getApi(), getNode(), SYSTEM_MONITOR_PACKET_1),
        getBusId());
    vector<byteType> data;
    uShortToData(data, 0); // Sim with serial number of 0
    uByteToData(data, getApi());  // Sim with a board type that is = to api
    uByteToData(data, 0);  // Never set initreq when simulating
    uByteToData(data, 0);  // 0 Can Rx Errors
    uByteToData(data, 0);  // 0 Can Tx Errors
    uByteToData(data, 0);  // 0 Mem Error count
    uByteToData(data, 0);  // Set sys error flags to 0
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message XacDevice::simSystemMonitorPacket2()
{
    Message msg(
        createId(true, getApi(), getNode(), SYSTEM_MONITOR_PACKET_2),
        getBusId());
    vector<byteType> data;
    uShortToData(data, 0); // Sim with scheduled overflow count of 0
    uShortToData(data, 0); // Same with timed schedule overflow count
    uByteToData(data, 0); // Major minor and test versions are '0.0.0'
    uByteToData(data, 0); 
    uByteToData(data, 0);
    uByteToData(data, 0); // Test mode 0
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message XacDevice::simSystemMonitorPacket3()
{
    Message msg(
        createId(true, getApi(), getNode(), SYSTEM_MONITOR_PACKET_3),
        getBusId());
    vector<byteType> data;
    uShortToData(data, 0); // Sim with 0 comm errors
    uShortToData(data, 0); // Sim with 0 time errors
    uShortToData(data, 0); // Sim with 0 sw errors
    uShortToData(data, 0); // Sim with 0 hw errors
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message XacDevice::simSystemMonitorPacket4()
{
    Message msg(
        createId(true, getApi(), getNode(), SYSTEM_MONITOR_PACKET_4),
        getBusId());
    vector<byteType> data;
    sShortToData(data, 0);  // Sim with 0ms time offset
    sShortToData(data, 0);  // Sim with 0ms time period
    sShortToData(data, 0);  // Sim with 0ms time delta
    uByteToData(data, '0'); // API Version 0
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message XacDevice::simSystemMonitorPacket5()
{
    Message msg(
        createId(true, getApi(), getNode(), SYSTEM_MONITOR_PACKET_5),
        getBusId());

    vector<byteType> data;
    uLongToData(data, ( Time::computeCurrentFrame() - startFrame_ ) / 2 );
    uByteToData(data, 0); // Bootloader absent if sim.
    data.push_back( 0x10 ); // Jan-25-2007 22:58 (10:58 PM) MSB
    data.push_back( 0x96 ); // Jan-25-2007 22:58 (10:58 PM) 
    data.push_back( 0xa2 ); // Jan-25-2007 22:58 (10:58 PM) LSB
    msg.setData(data);
    return msg;
}

// -----------------------------------------------------------------------------
void XacDevice::reset()
{
    try {
        Device::reset();
    } catch (carma::util::ErrorException &eex) {
        throw CARMA_EXCEPTION(carma::util::UserException, eex.what());
    }
}

// -----------------------------------------------------------------------------
void XacDevice::stopChannelOneFastSampling()
{
    try {
        Device::stopChannelOneFastSampling();
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void XacDevice::stopChannelTwoFastSampling()
{
    try {
        Device::stopChannelTwoFastSampling();
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void XacDevice::startChannelOneFastSampling(unsigned short fastItem)
{
    try {
        Device::startChannelOneFastSampling(fastItem);
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void XacDevice::startChannelTwoFastSampling(unsigned short fastItem)
{
    try {
        Device::startChannelTwoFastSampling(fastItem);
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
bool XacDevice::isOnline()
{
    try {
        if (Device::getState() == ONLINE) {
            return true;
        } else {
            return false;
        }
    } catch (std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}


