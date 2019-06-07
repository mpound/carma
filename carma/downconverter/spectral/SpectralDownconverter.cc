/**@file
 * SpectralDownconverter class definition (CARMA CANbus API 024).
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.37 $
 * $Date: 2011/05/11 17:35:23 $
 * $Id: SpectralDownconverter.cc,v 1.37 2011/05/11 17:35:23 iws Exp $
 */

// Carma includes
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/downconverter/spectral/SpectralDownconverter.h"
#include "carma/monitor/SldcSubsystem.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/UserException.h"

// Carma tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

// STL includes
#include <stdexcept>

using namespace carma::canbus;
using namespace carma::downconverter;
using namespace carma::monitor;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

// Anonymous namespace for constants that aren't used elsewhere.
namespace {

    // API Number
    const apiType API_ID                                     = 25;

    // API Version.
    const char API_VERSION                                   = 'E';

    // Packet late threshold.
    const double PACKET_LATE_THRESHOLD                       = 175.0;

    // Control commands.
    const carma::canbus::msgType SET_PSYS_POWER_TO_PRESET    = 0x040;
    const carma::canbus::msgType SET_PSYS_POWER_TO_REQSTD    = 0x041;
    const carma::canbus::msgType SET_PSYS_ATTEN_TO_REQSTD    = 0x042;
    const carma::canbus::msgType SET_IFOUT_POWER_TO_PRESET	 = 0x043;
    const carma::canbus::msgType SET_IFOUT_POWER_TO_REQSTD   = 0x044;
    const carma::canbus::msgType SET_IFOUT_ATTEN_TO_REQSTD   = 0x045;
    const carma::canbus::msgType SET_AMP_POWER_SWITCH        = 0X046;
    const carma::canbus::msgType SET_IFOUT_ALC_LOOP_SWITCH   = 0x047;
    const carma::canbus::msgType SELECT_OUTPUT_FILTER        = 0x048;
    const carma::canbus::msgType SELECT_SIDEBAND             = 0x049;
    const carma::canbus::msgType SET_LO_FREQUENCY            = 0x04A;
    const carma::canbus::msgType SET_I_MODULATOR_VOLTAGE     = 0x04B;
    const carma::canbus::msgType SET_Q_MODULATOR_VOLTAGE     = 0x04C;

    // Fast monitors.
    const carma::canbus::msgType BLANKING_FRAME_PACKET_1     = 0x0E0;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_2     = 0x0E1;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_3     = 0x0E2;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_4     = 0x0E3;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_5     = 0x0E4;
    const carma::canbus::msgType BLANKING_FRAME_PACKET_6     = 0x0E5;

    // Other constants
    const unsigned short INPUT_MAX                           = 0xFFFF;
    const unsigned short BAND_MAX                            = 0xFFFF;

    typedef map<SpectralDownconverterControl::FilterType,
                string> OutputFilterToStringType;

    // Define an array of value_type from which we can initialize an
    // OutputFilterToStringType map statically.
    OutputFilterToStringType::value_type _filterToString[] =
    {
        OutputFilterToStringType::value_type(
            SpectralDownconverterControl::FILTER_490MHZ, "FILTER_490MHZ"),
        OutputFilterToStringType::value_type(
            SpectralDownconverterControl::FILTER_240MHZ, "FILTER_240MHZ"),
        OutputFilterToStringType::value_type(
            SpectralDownconverterControl::FILTER_119MHZ, "FILTER_119MHZ"),
        OutputFilterToStringType::value_type(
            SpectralDownconverterControl::FILTER_58MHZ, "FILTER_58MHZ"),
        OutputFilterToStringType::value_type(
            SpectralDownconverterControl::FILTER_280MHZ, "FILTER_280MHZ")
    };

    const OutputFilterToStringType filterToString(
        _filterToString,
        _filterToString + sizeof(_filterToString) / sizeof(_filterToString[0])
        );


    // Typesafe macros :-)
    unsigned short bandNumber(nodeType node)
    {
        if (node == 0)
            return BAND_MAX;
        else
            return (node % 0x10);
    }

    unsigned short inputNumber(nodeType node)
    {
        if (node == 0)
            return INPUT_MAX;
        else
            return (node / 0x10);
    }

} // End anonymous namespace

// -----------------------------------------------------------------------------
SpectralDownconverter::SpectralDownconverter(
        carma::canbus::nodeType node,
        carma::canbus::CanOutput & co,
        carma::monitor::SldcSubsystem & sldcMon ) :
    XacDevice( API_ID, node, co ),
    inputNo_( inputNumber( node ) ),
    bandNo_( bandNumber( node ) ),
    log_( Program::getLogger() ),
    lastLogWarnFrame_( Time::computeCurrentFrame( ) ),
    offlinesSinceLastLog_( 0 ),
    commandLoggingEnabled_( true )
{
    CPTRACE(Trace::TRACE6, "SpectralDownconverter::SpectralDownconverter() - "
        << "Device class created for api " << getApi()
        << ", node " << getNode() << ", input " << inputNo_
        << ", band " << bandNo_);
    if ( node != 0 ) {
        mon_ = &(sldcMon.band( bandNo_ - 1 ).input( inputNo_ - 1 ) );
        mon_->bandNo().setValue(bandNo_);
        mon_->inputNo().setValue(inputNo_);
    } else {
        mon_ = 0;
    }

    ifOutputPower_ = 0.0;
}

// -----------------------------------------------------------------------------
SpectralDownconverter::~SpectralDownconverter()
{
    CPTRACE(Trace::TRACE6, "SpectralDownconverter::~SpectralDownconverter() - "
        << "Device class destroyed for api " << getApi()
        << ", node " << getNode() << ", input " << inputNo_
        << ", band " << bandNo_);
}

// -----------------------------------------------------------------------------
map<msgType, string> SpectralDownconverter::getHalfSecMonitors() const
{
    static bool init = false;
    static map<msgType, string> mons;

    if (!init) {
        mons[BLANKING_FRAME_PACKET_1] = "BLANKING_FRAME_PACKET_1";
        mons[BLANKING_FRAME_PACKET_2] = "BLANKING_FRAME_PACKET_2";
        mons[BLANKING_FRAME_PACKET_3] = "BLANKING_FRAME_PACKET_3";
        mons[BLANKING_FRAME_PACKET_4] = "BLANKING_FRAME_PACKET_4";
        mons[BLANKING_FRAME_PACKET_5] = "BLANKING_FRAME_PACKET_5";
        mons[BLANKING_FRAME_PACKET_6] = "BLANKING_FRAME_PACKET_6";
        init = true;
    }
    return mons;
}

// -----------------------------------------------------------------------------
map<msgType, string> SpectralDownconverter::getSlowMonitors() const
{
    return XacDevice::getSlowMonitors();
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::processMsg(
    carma::canbus::msgType mid,
    std::vector<carma::canbus::byteType> &data,
    bool sim)
{
    if (getState() == ONLINE) {
        if (isPacketLate(PACKET_LATE_THRESHOLD)) {
            incrementLatePacketCount();
        }
    }

    CPTRACE(Trace::TRACEALL, "SpectralDownconverter::processMsg() - Processing "
            << (sim ? "simulated" : "real") << " msg 0x"
            << hex << mid << dec << " for node " << getNode() << ".");

    // Dispatch the data to the appropriate message processing routine
    switch (mid) {
        case BLANKING_FRAME_PACKET_1:
            processBlankingFramePacket1(data);
            break;
        case BLANKING_FRAME_PACKET_2:
            processBlankingFramePacket2(data);
            break;
        case BLANKING_FRAME_PACKET_3:
            processBlankingFramePacket3(data);
            break;
        case BLANKING_FRAME_PACKET_4:
            processBlankingFramePacket4(data);
            break;
        case BLANKING_FRAME_PACKET_5:
            processBlankingFramePacket5(data);
            break;
        case BLANKING_FRAME_PACKET_6:
            processBlankingFramePacket6(data);
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
            XacDevice::processSystemMonitorPacket1(data, mon_->xac());
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
            XacDevice::processSystemMonitorPacket2(data, mon_->xac());
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
            XacDevice::processSystemMonitorPacket3(data, mon_->xac());
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
            XacDevice::processSystemMonitorPacket4(data, mon_->xac());
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            XacDevice::processSystemMonitorPacket5(data, mon_->xac());
            break;
        default:
            // I don't know how to process this message id!
            // Not a problem, just log it.
            log_ << Priority::DEBUG << "SpectralDownconverter::processMsg() - "
                << "Switch does not match any case: Unknown mid "
                << mid << ". Node " << getNode();
            CPTRACE(Trace::TRACE6, "SpectralDownconverter::processMsg() - "
                    "Switch doesn't match any case.  mid 0x" << hex  << mid
                    << dec << " node " << getNode() << ".");
            break;
    }
}

// -----------------------------------------------------------------------------
carma::canbus::Message SpectralDownconverter::simulateMsg(msgType mid)
{
    carma::canbus::Message msg;
    CPTRACE(Trace::TRACEALL, "SpectralDownconverter::simulateMsg() - "
        "Simulating msg 0x" << hex << mid << dec << " for node "
        << getNode() << ".");

    switch (mid) {
        case BLANKING_FRAME_PACKET_1:
            msg = simBlankingFramePacket1();
            break;
        case BLANKING_FRAME_PACKET_2:
            msg = simBlankingFramePacket2();
            break;
        case BLANKING_FRAME_PACKET_3:
            msg = simBlankingFramePacket3();
            break;
        case BLANKING_FRAME_PACKET_4:
            msg = simBlankingFramePacket4();
            break;
        case BLANKING_FRAME_PACKET_5:
            msg = simBlankingFramePacket5();
            break;
        case BLANKING_FRAME_PACKET_6:
            msg = simBlankingFramePacket6();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_1:
            msg = XacDevice::simSystemMonitorPacket1();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_2:
            msg = XacDevice::simSystemMonitorPacket2();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_3:
            msg = XacDevice::simSystemMonitorPacket3();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_4:
            msg = XacDevice::simSystemMonitorPacket4();
            break;
        case XacDevice::SYSTEM_MONITOR_PACKET_5:
            msg = XacDevice::simSystemMonitorPacket5();
            break;
        default:
            // I don't know how to simulate this message!
            // Not a problem, just log it - but should I throw???
            log_ << Priority::DEBUG << "YigPll::simulateMsg - "
                << "Switch does not match any case: mid " << mid;
            CPTRACE(Trace::TRACE6, "YigPll::simulateMsg() - "
                    "Switch doesn't match any case.  mid 0x" << hex  << mid
                    << dec << " node " << getNode() << ".");
            break;
    }
    return msg;
}

// -----------------------------------------------------------------------------
apiType SpectralDownconverter::getApiId()
{
    return API_ID;
}

// -----------------------------------------------------------------------------
nodeType SpectralDownconverter::calculateNodeId(
    unsigned short input, unsigned short band)
{
    nodeType node = (input * 0x10) + (band);
    return node;
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::updateFrameData()
{
    // Set the state of the device...
    mon_->state().setValue(
        static_cast<StateMonitorPointEnum::STATE>(getState()));
    mon_->bandNo().setValue(bandNo_);
    mon_->inputNo().setValue(inputNo_);

    // Log how many times we've been taken offline in the last LOG_THRESHOLD
    static const unsigned int LOG_THRESHOLD = 3600; // 1/2 hour.
    const unsigned int framesSinceLastLog =
        Time::computeCurrentFrame( ) - lastLogWarnFrame_;
    if ( offlinesSinceLastLog_ > 0 && framesSinceLastLog > LOG_THRESHOLD ) {
        log_ << Priority::ERROR << "SpectralDownconverter input "
             << inputNo_ << " band " << bandNo_ << " has gone offline "
             << offlinesSinceLastLog_ << " time(s) in the last "
             << LOG_THRESHOLD << " frames.";
        offlinesSinceLastLog_ = 0;
        lastLogWarnFrame_ = Time::computeCurrentFrame( );
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::setPsysPreset() const
{
    try {
        carma::canbus::Message msg = createMsgToNode(SET_PSYS_POWER_TO_PRESET);
        io_.postMessage(msg);
        if ( commandLoggingEnabled_ ) {
            log_ << Priority::INFO << __PRETTY_FUNCTION__
                << " for " << printInputAndBand();
        }
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::setPsys(::CORBA::Float psys) const
{
    try {
        carma::canbus::Message msg = createMsgToNode(SET_PSYS_POWER_TO_REQSTD);
        msg << ( static_cast<short>( round( psys/0.01 ) ) );
        io_.postMessage(msg);
        if ( commandLoggingEnabled_ ) {
            log_ << Priority::INFO << __PRETTY_FUNCTION__
                << "( psys=" << psys << " dBm ) for " << printInputAndBand()
                << ".";
        }
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::setPsysAtten(::CORBA::Float atten) const
{
    if ( atten < 0.00 ) {
        throw CARMA_EXCEPTION(carma::util::UserException, "Invalid Input: "
            "Attenuation must be non-negative.");
    }

    try {
        carma::canbus::Message msg = createMsgToNode(SET_PSYS_ATTEN_TO_REQSTD);
        msg << ( static_cast<unsigned short>( round( atten/0.01 ) ) );
        io_.postMessage(msg);
        log_ << Priority::INFO << "SpectralDownconverter::setPsysAtten() - "
            << "Setting psys attenuation to " << atten << " dB for "
            << printInputAndBand();
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::setIfOutPreset() const
{
    try {
        carma::canbus::Message msg = createMsgToNode(SET_IFOUT_POWER_TO_PRESET);
        io_.postMessage(msg);
        log_ << Priority::INFO << "SpectralDownconverter::setIfOutPreset() - "
            << "Setting IF Output to preset level for "
            << printInputAndBand();
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::setIfOut(::CORBA::Float ifout) const
{
    try {
        carma::canbus::Message msg = createMsgToNode(SET_IFOUT_POWER_TO_REQSTD);
        msg << ( static_cast<short>( round( ifout/0.01 ) ) );
        io_.postMessage(msg);
        log_ << Priority::INFO << "SpectralDownconverter::setIfOut() - "
            << "Setting IF Output power to " << ifout << " dBm for "
            << printInputAndBand();
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::setIfOutAtten(::CORBA::Float atten) const
{
    if ( atten < 0.00 ) {
        throw CARMA_EXCEPTION(carma::util::UserException, "Invalid Input: "
            "Attenuation must be non-negative.");
    }

    try {
        carma::canbus::Message msg = createMsgToNode(SET_IFOUT_ATTEN_TO_REQSTD);
        msg << ( static_cast<unsigned short>( round( atten/0.01 ) ) );
        io_.postMessage(msg);
        log_ << Priority::INFO << "SpectralDownconverter::setIfOutAtten() - "
            << "Setting IF Output attenuation to " << atten << " dB "
            << "for " << printInputAndBand();
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::enableRfInputAmp(::CORBA::Boolean enable) const
{
    try {
        carma::canbus::Message msg = createMsgToNode(SET_AMP_POWER_SWITCH);
        msg << static_cast<unsigned char>( ( enable ? 0x01 : 0x00 ) );
        io_.postMessage(msg);
        log_ << Priority::INFO << "SpectralDownconverter::enableRfInputAmp() - "
            << (enable ? "Enabling " : "Disabling") << "RF Input Amplifier "
            << "for " << printInputAndBand();
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::enableIfOutAlc(::CORBA::Boolean enable) const
{
    try {
        carma::canbus::Message msg = createMsgToNode(SET_IFOUT_ALC_LOOP_SWITCH);
        msg << static_cast<unsigned char>( ( enable ? 0x01 : 0x00 ) );
        io_.postMessage(msg);
        log_ << Priority::INFO << "SpectralDownconverter::enableIfOutAlc() - "
            << (enable ? "Enabling" : "Disabling") << " IF Output ALC for "
            << printInputAndBand();
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::selectOutputFilter(
    enum SpectralDownconverterControl::FilterType filter) const
{
    try {
        carma::canbus::Message msg = createMsgToNode(SELECT_OUTPUT_FILTER);
        OutputFilterToStringType::const_iterator iFilter =
            filterToString.find( filter );
        if ( iFilter == filterToString.end() )
            throw std::logic_error( "Invalid filter in selectOutputFilter" );
        msg << ( static_cast<unsigned char>(filter) );
        io_.postMessage(msg);
        if ( commandLoggingEnabled_ ) {
            log_ << Priority::INFO << __PRETTY_FUNCTION__
                << "( filter=" << iFilter->second << " )"
                << " for " << printInputAndBand();
        }
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::selectSideband(
    enum SpectralDownconverterControl::SidebandType sideband) const
{
    try {
        carma::canbus::Message msg = createMsgToNode(SELECT_SIDEBAND);
        msg << ( static_cast<unsigned char>(sideband) );
        io_.postMessage(msg);
        if ( commandLoggingEnabled_ ) {
            log_ << Priority::INFO << __PRETTY_FUNCTION__
                << "( sideband="
                << ( sideband == SpectralDownconverterControl::UPPER_SIDEBAND ?
                     "Upper" :
                     (sideband == SpectralDownconverterControl::LOWER_SIDEBAND ?
                       "Lower" : "Invalid") ) << " sideband for "
                << printInputAndBand() << ").";
        }
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::setLOFrequency( ::CORBA::Double lofreq ) const
{
    try {
        unsigned short freq =
            static_cast<unsigned short>( round( lofreq * 1000.0 ) );
        carma::canbus::Message msg = createMsgToNode(SET_LO_FREQUENCY);
        msg << freq;
        io_.postMessage(msg);
        if ( commandLoggingEnabled_ ) {
        log_ << Priority::INFO << __PRETTY_FUNCTION__
            << "(lofreq=" << lofreq << " GHz ) for "
            << printInputAndBand() << ".";
        }
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::setIModulatorVoltage(::CORBA::Short I) const
{
    try {
        carma::canbus::Message msg = createMsgToNode(SET_I_MODULATOR_VOLTAGE);
        msg << I;
        io_.postMessage(msg);
        log_ << Priority::INFO << "SpectralDownconverter::"
            << "setIModulatorVoltage() - Setting I to " << I << " mv for "
            << printInputAndBand();
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::setQModulatorVoltage(::CORBA::Short Q) const
{
    try {
        carma::canbus::Message msg = createMsgToNode(SET_Q_MODULATOR_VOLTAGE);
        msg << Q;
        io_.postMessage(msg);
        log_ << Priority::INFO << "SpectralDownconverter::"
            << "setQModulatorVoltage() - Setting Q to " << Q << " mv for "
            << printInputAndBand();
    } catch (const std::exception &ex) {
        throw CARMA_EXCEPTION(carma::util::UserException, ex.what());
    }
}

// -----------------------------------------------------------------------------
::CORBA::Boolean SpectralDownconverter::isOnline()
{
    return (Device::getState() == ONLINE);
}

// -----------------------------------------------------------------------------
::CORBA::Boolean
SpectralDownconverter::checkIfOutputPower(
    ::CORBA::Float power,
    ::CORBA::Float delta)
{
    ::CORBA::Boolean answer;
    if ( getState() != ONLINE )
        throw CARMA_EXCEPTION( carma::util::UserException,
                "SpectralDownconverter::checkIfOutputPower() - Downconverter "
                "is not ONLINE.");

    answer = ( ( ifOutputPower_ >= (power - delta) )
              && ( ifOutputPower_ <= (power + delta) ) );
    return answer;
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::disableCommandLogging()
{
    commandLoggingEnabled_ = false;
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::enableCommandLogging()
{
    commandLoggingEnabled_ = true;
}

// -----------------------------------------------------------------------------
std::string SpectralDownconverter::filterAsString(
    const carma::downconverter::DownconverterControl::FilterType filter)
{
    const OutputFilterToStringType::const_iterator answer =
        filterToString.find( filter );
    if ( answer == filterToString.end() )
        return "< unknown >";
    else
        return answer->second;
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::processBlankingFramePacket1(vector<byteType> &data)
{
    short psys[4];

    if ( data.size() < 8 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "SpectralDownconverter::processBlankingFramePacket1 - "
            "Invalid data size.");

    // Unpack  the raw data
    for (unsigned int i = 0; i < 4; ++i) {
        psys[i] = dataToShort(data);

        // Convert and place into monitor stream.
        mon_->psys().setValue(psys[i] / 100.0, i);
    }
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::processBlankingFramePacket2(vector<byteType> &data)
{
    short psys5, psysAtten, ifOut, ifOutAtten;

    if ( data.size() < 8 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "SpectralDownconverter::processBlankingFramePacket2() - "
            "Invalid data size.");

    psys5 = dataToShort(data);
    psysAtten = dataToShort(data);
    ifOut = dataToShort(data);
    ifOutAtten = dataToShort(data);

    ifOutputPower_ = ifOut / 100.0;

    // Convert and place into monitor stream.
    mon_->psys().setValue(psys5 / 100.0, 4);
    mon_->psysAtten().setValue(psysAtten / 100.0);
    mon_->ifOutPower().setValue(ifOut / 100.0);
    mon_->ifOutAtten().setValue(ifOutAtten / 100.0);
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::processBlankingFramePacket3(vector<byteType> &data)
{
    short temp;
    unsigned char psysStat, ifOutStat, outputFilter;
    unsigned char sideband, invalidFilt, rfAmp;

    if ( data.size() < 8 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "SpectralDownconverter::processBlankingFramePacket3() - "
            "Invalid data size.");

    temp = dataToShort(data);
    psysStat = dataToUbyte(data);
    ifOutStat = dataToUbyte(data);
    outputFilter = dataToUbyte(data);
    sideband = dataToUbyte(data);
    invalidFilt = dataToUbyte(data);
    rfAmp = dataToUbyte(data);

    // Convert and place into monitor stream.
    mon_->temp().setValue(temp / 100.0);
    mon_->psysStat().setValue(
        static_cast<SldcSubsystem::PsysStatMonitorPointEnum::PSYSSTAT>(psysStat));
    mon_->ifOutStat().setValue(
        static_cast<SldcSubsystem::IfOutStatMonitorPointEnum::IFOUTSTAT>(
            ifOutStat));
    mon_->outputFilt().setValue(
        static_cast<SldcSubsystem::OutputFiltMonitorPointEnum::OUTPUTFILT>(
            outputFilter));
    mon_->sideband().setValue(
        static_cast<SldcSubsystem::SidebandMonitorPointEnum::SIDEBAND>(
            sideband));
    mon_->filter().setValue(
        static_cast<SldcSubsystem::FilterMonitorPointEnum::FILTER>(
            invalidFilt));
    mon_->rfAmp().setValue(
        static_cast<SldcSubsystem::RfAmpMonitorPointEnum::RFAMP>(rfAmp));
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::processBlankingFramePacket4(vector<byteType> &data)
{
    unsigned short loFreq;
    short iMod, qMod;
    unsigned char filter, valid;

    if ( data.size() < 8 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "SpectralDownconverter::processBlankingFramePacket4() - "
            "Invalid data size.");

    // Unpack the raw data.
    loFreq = dataToUshort(data);
    iMod = dataToShort(data);
    qMod = dataToShort(data);
    filter = dataToUbyte(data);
    valid = dataToUbyte(data);

    mon_->loFreq().setValue(loFreq / 1000.0);
    mon_->imod().setValue(iMod / 1000.0);
    mon_->qmod().setValue(qMod / 1000.0);
    mon_->rqstdFilt().setValue(
        static_cast<SldcSubsystem::RqstdFiltMonitorPointEnum::RQSTDFILT>(
            filter) );
    mon_->dataValid( ).setValue( ( valid == 0x01 ) );
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::processBlankingFramePacket5(vector<byteType> &data)
{
    short ps5v1, ps5v2, ps5v3, ps5v4;

    if ( data.size() < 8 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "SpectralDownconverter::processBlankingFramePacket5() - "
            "Invalid data size.");

    // Unpack raw data.
    ps5v1 = dataToShort(data);
    ps5v2 = dataToShort(data);
    ps5v3 = dataToShort(data);
    ps5v4 = dataToShort(data);

    // Convert and place into monitor stream.
    mon_->ps5vAnalog().setValue(ps5v1 / 1000.0);
    mon_->psNeg5vAnalog().setValue(ps5v2 / 1000.0);
    mon_->ps5vDigital().setValue(ps5v3 / 1000.0);
    mon_->ps5vAnalog2().setValue(ps5v4 / 1000.0);
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::processBlankingFramePacket6(vector<byteType> &data)
{
    short ps24v, ps7v1, ps7v2, ps7v3;

    if ( data.size() < 8 )
        throw CARMA_EXCEPTION(carma::canbus::BadDataSizeException,
            "SpectralDownconverter::processBlankingFramePacket6() - "
            "Invalid data size.");

    // Unpack raw data.
    ps24v = dataToShort(data);
    ps7v1 = dataToShort(data);
    ps7v2 = dataToShort(data);
    ps7v3 = dataToShort(data);

    mon_->ps24v().setValue(ps24v / 1000.0);
    mon_->ps7v1().setValue(ps7v1 / 1000.0);
    mon_->ps7v2().setValue(ps7v2 / 1000.0);
    mon_->ps7v3().setValue(ps7v3 / 1000.0);
}

// -----------------------------------------------------------------------------
carma::canbus::Message SpectralDownconverter::simBlankingFramePacket1()
{
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_1);
    for (unsigned i = 0; i < 4; i++)
        msg << static_cast<short>( ( bandNo_ + 0.01 * inputNo_ ) * 100 );
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SpectralDownconverter::simBlankingFramePacket2()
{
    short psys5 = static_cast<short>( (( bandNo_ + 0.01 * inputNo_ ) * 100) );
    short psysAtt    = 600;
    short ifOut      = 700;
    short ifOutAtten = 800;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_2);
    msg << psys5 << psysAtt << ifOut << ifOutAtten;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SpectralDownconverter::simBlankingFramePacket3()
{
    short temp = 500; // 50C
    unsigned char psysStat = 0, ifOutStat = 0, outputFilt = 0;
    unsigned char sideband = 0, invalidFilt = 1, rfAmp = 0;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_3);
    msg << temp << psysStat << ifOutStat << outputFilt;
    msg << sideband << invalidFilt << rfAmp;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SpectralDownconverter::simBlankingFramePacket4()
{
    unsigned short lofreq = 2000; // 2 GHz
    short iMod = 500, qMod = 1000;
    unsigned char reqstdFilt = 0;
    unsigned char valid = 0x00; // Invalid
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_4);
    msg << lofreq << iMod << qMod << reqstdFilt << valid;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SpectralDownconverter::simBlankingFramePacket5()
{
    short ps5v1 = 5001, ps5v2 = -5002, ps5v3 = 5003, ps5v4 = 5004;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_5);
    msg << ps5v1 << ps5v2 << ps5v3 << ps5v4;
    return msg;
}

// -----------------------------------------------------------------------------
carma::canbus::Message SpectralDownconverter::simBlankingFramePacket6()
{
    short ps24v = 24000, ps7v1 = 7001, ps7v2 = 7002, ps7v3 = 7003;
    carma::canbus::Message msg = createMsgToHost(BLANKING_FRAME_PACKET_6);
    msg << ps24v << ps7v1 << ps7v2 << ps7v3;
    return msg;
}

// -----------------------------------------------------------------------------
string SpectralDownconverter::printInputAndBand() const
{
    ostringstream os;
    if ( getNode() == 0 )
        os << "ALL modules.";
    else
        os << "input " << inputNo_ << ", band " << bandNo_ << ends;

    return os.str();
}

// -----------------------------------------------------------------------------
void SpectralDownconverter::setState( const deviceStateType state )
{
    if ( state == OFFLINE && getState( ) != OFFLINE )
        ++offlinesSinceLastLog_;

    XacDevice::setState( state );
}
