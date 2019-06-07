#include "carma/correlator/transport/CorrMonUpdater.h"

#include "carma/corba/corba.h"

#include "carma/correlator/lib/CorrelatorConfigChecker.h"
#include "carma/monitor/CarmaSlcBandSubsystem.h"
#include "carma/monitor/CarmaCorrelatorCommon.h"
#include "carma/monitor/WbcBandSubsystem.h"
#include "carma/monitor/Carma3GSubsystem.h"
#include "carma/util/ErrorException.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/FrameAlignedTimer.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/Sleeper.h"
#include "carma/util/StringUtils.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"

#include <algorithm>
#include <cerrno>
#include <cmath>
#include <cstdlib>
#include <iomanip>
#include <sstream>
#include <string>
#include <vector>
#include <map>

#include <pthread.h>
#include <unistd.h>
#include <signal.h>

// Network
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include <cobra/CobraVersion.h>
#include <cobra/DigitizerStatistics.h>

// COBRA data interface
#include <cobra/CorrelatorBandClient.h>
#include <cobra/CorrelatorConfigurationIniImporter.h>
#include <cobra/CorrelatorMonitorCDR.h>
#include <cobra/CorrelatorControlErrors.h>
#include <cobra/CorrelatorMonitorSimulator.h>
#include <cobra/Types.h>
#include <cobra/debugUtils.h>

// Boost
#include <boost/foreach.hpp>

using namespace ::std;
using namespace carma;
using namespace carma::correlator;
using namespace carma::correlator::lib;
using namespace carma::correlator::obsRecord2;
using namespace carma::correlator::transport;
using namespace carma::monitor;
using namespace carma::util;

typedef std::auto_ptr<cobra::CorrelatorMonitorSimulator> MonitorSimulatorPtr;
// This maps bandNumber to index in the monitor subsystem vector
typedef std::map<unsigned short,unsigned short> BandNoToMonSysMap;

namespace {

const Trace::TraceLevel TRACE_ALL = Trace::TRACE7;
const Trace::TraceLevel TRACE_MISC = Trace::TRACE4;

// ----------------------------------------------------------------
// MonitorServer
// ----------------------------------------------------------------

std::string
convertToHexString(const unsigned int num)
{
    std::ostringstream oss;
    oss << std::hex << std::uppercase << num;
    return oss.str();
}

double
convertVersion( const unsigned int ver )
{
    const unsigned int major = (ver >> 8);
    const unsigned int minor = ver & 0xFF;

    return static_cast< double >( major ) +
           static_cast< double >( minor ) / 100.0;
}

std::string convertVersionToString(const unsigned int ver)
{
    std::ostringstream oss;
    oss << convertVersion(ver);
    return oss.str();
}

/**
 * Maps the integer (bitpattern) command status to the CommandStatus
 * monitor point.  If there are multiple failures for the band, the
 * monitor point, then COMMAND_MULTIPLE_FAIL is set and the actual
 * integer values on the boards must be inspected.
 */
template <typename COBRA, typename CARMA>
void convertCommandStatus(const COBRA &board, CARMA &mon)
{
    typedef carma::monitor::CommandStatusMonitorPointEnum CSMP;
    CSMP &mp = mon.commandStatus();

    switch (board.commandStatusEnumValue()) {
    case cobra::COMMAND_OK:
        mp.setValue(CSMP::OK);
        break;
    case cobra::COMMAND_IN_PROGRESS:
        mp.setValue(CSMP::IN_PROGRESS);
        break;
    case cobra::COMMAND_CONFIGURE_FAIL:
        mp.setValue(CSMP::CONFIGURE_FAIL);
        break;
    case cobra::COMMAND_OPTIMIZE_FAIL:
        mp.setValue(CSMP::OPTIMIZE_FAIL);
        break;
    case cobra::COMMAND_FLATTEN_FAIL:
        mp.setValue(CSMP::FLATTEN_FAIL);
        break;
    case cobra::COMMAND_CALIBRATE_FAIL:
        mp.setValue(CSMP::CALIBRATE_FAIL);
        break;
    case cobra::COMMAND_MULTIPLE_FAIL:
        mp.setValue(CSMP::MULTIPLE_FAIL);
        break;
    default:
    case cobra::COMMAND_UNKNOWN:
        mp.setValue(CSMP::UNKNOWN);
        break;
    }
}

template <typename COBRA>
carma::monitor::ReceivedMonitorPointEnum::RECEIVED
convertRxStatus(const COBRA &board)
{
    if (board.status() < 0)
        return carma::monitor::ReceivedMonitorPointEnum::MISSING;

    if (board.late())
        return carma::monitor::ReceivedMonitorPointEnum::LATE;

    return carma::monitor::ReceivedMonitorPointEnum::ONTIME;
}

// template specialization for cobra::MonitorBand which lacks the
// late() function
template <>
carma::monitor::ReceivedMonitorPointEnum::RECEIVED
convertRxStatus<cobra::MonitorBand>(const cobra::MonitorBand &band)
{
    if (band.status() < 0)
        return carma::monitor::ReceivedMonitorPointEnum::MISSING;

    return carma::monitor::ReceivedMonitorPointEnum::ONTIME;
}

/*
 * Write generic monitor information available in the same location on all
 * board types. This should be called in the conversion function for each
 * board hardware type.
 */
template <typename COBRA, typename CARMA>
void convertHeader(const COBRA &board, CARMA &mon)
{
    typedef carma::monitor::ModeMonitorPointEnum ModeMPEnum;
    typedef carma::monitor::DconSidebandMonitorPointEnum DconSBMPEnum;
    typedef carma::monitor::LobeTrackSidebandMonitorPointEnum LobeMPEnum;

    const cobra::CorrelatorParameters &params = board.parameters();

    mon.received().setValue(convertRxStatus(board));

    mon.timestamp().setValue(board.timestamp().mjdDays());
    mon.bandNumber().setValue(params.bandNumber());

    // Enumeration conversion
    switch (params.mode()) {
    case 0:
        mon.mode().setValue(ModeMPEnum::IDLE);
        break;
    case 1:
        mon.mode().setValue(ModeMPEnum::CORRELATE);
        break;
    case 2:
        mon.mode().setValue(ModeMPEnum::HOST_SIM);
        break;
    case 3:
        mon.mode().setValue(ModeMPEnum::DSP_SIM);
        break;
    case 4:
        mon.mode().setValue(ModeMPEnum::PPC_SIM);
        break;
    default:
        break;
    }

    mon.sourceName().setValue(params.sourceName());
    mon.numberOfPhaseSwitchesIntegrated().setValue(params.numberOfPhaseSwitchesIntegrated());
    mon.phaseSwitchPeriod().setValue(params.phaseSwitchPeriod() * 1e3);
    mon.phaseSwitchIntegrationTime().setValue(params.phaseSwitchIntegrationTime() * 1e3);
    mon.totalTimeSpan().setValue(params.totalTimeSpan() * 1e3);
    mon.totalIntegrationTime().setValue(params.totalIntegrationTime() * 1e3);
    mon.bandwidth().setValue(params.bandwidth() / 1e6);
    mon.centerFrequency().setValue(params.centerFrequency() / 1e6);
    mon.dconFrequency().setValue(params.downconverterFrequency() / 1e6);

    if (params.downconverterSidebandIsUpper()) {
        mon.dconSideband().setValue(DconSBMPEnum::USB);
    } else {
        mon.dconSideband().setValue(DconSBMPEnum::LSB);
    }

    mon.bdcEnabled().setValue(params.blockDownconverterEnabled());
    mon.lobeResidualFrequency().setValue(params.lobeResidualFrequency() / 1e6);

    if (params.lobeResidualFrequency() != 0) {
        if (params.trackingSidebandIsUpper()) {
            mon.lobeTrackSideband().setValue(LobeMPEnum::USB);
        } else {
            mon.lobeTrackSideband().setValue(LobeMPEnum::LSB);
        }
    } else {
        mon.lobeTrackSideband().setValue(LobeMPEnum::DSB);
    }

    mon.scaling().setValue(params.scaling());
}

// Convert information present in the same place for both COBRA digitizers
// and COBRA correlators. We're trying to avoid some typing.
template <typename COBRA, typename CARMA>
void convertCobraGeneric(const COBRA &board, CARMA &mon)
{
    // common header information
    convertHeader(board, mon);

    // command status enumeration
    convertCommandStatus(board, mon);

    // sequence number / command status
    mon.corrSeqNo().setValue(board.sequenceNumber);
    mon.commandStatusBitPattern().setValue(board.commandStatus);

    // various enumerations
    {
        typedef carma::monitor::BitModeMonitorPointEnum::BITMODE BMEnum;
        typedef carma::monitor::CorrStatusMonitorPointEnum::CORRSTATUS CSEnum;
        typedef carma::monitor::AstroBandModeMonitorPointEnum::ASTROBANDMODE ABMEnum;

        // COBRA hardware doesn't support changing these values
        const BMEnum bm = static_cast<BMEnum>(cobra::CORR_2BIT);
        const CSEnum cs = static_cast<CSEnum>(cobra::READY);
        const ABMEnum abmode = static_cast<ABMEnum>(cobra::SINGLEPOL);

        mon.bitMode().setValue(bm);
        mon.corrStatus().setValue(cs);
        mon.astroBandMode().setValue(abmode);
    }
}

// Convert correlation info for COBRA hardware
template <typename COBRA, typename CARMA>
void convertCobraCorrelationInfo(const COBRA &board, CARMA &mon)
{
    // Correlation info
    const int numberOfCorrelations = board.input_info.size();
    const int maxCorrelations = mon.getNumCorrelations();
    for (int i = 0; i < maxCorrelations; i++) {
        if (i >= numberOfCorrelations) {
            mon.correlations(i).setValue("unused");
            continue;
        }

        const cobra::CorrelatorInputInfo &info = board.input_info.at(i);
        std::ostringstream oss;
        if (info.isAuto()) {
            oss << "auto " << info.input(0);
        } else if (info.isCross()) {
            oss << "cross " << info.input(0) << "-" << info.input(1);
        } else {
            oss << "unknown";
        }

        mon.correlations(i).setValue(oss.str());
    }

    // Spectral line mode deltas
    const int numSpectralDeltaMps = mon.getNumSpectralDelta();
    if (fabs(board.parameters().bandwidth() - 5e8) < 1e8) {
        // 500MHz bandwidth (+- 100MHz)
        for (int i = 0; i < numSpectralDeltaMps; i++)
            mon.spectralDelta(i).setValue(0);
    } else {
        const int iEnd = std::min(numSpectralDeltaMps, std::min(maxCorrelations, numberOfCorrelations));

        int i = 0;
        for (/* none */; i < iEnd; i++) {
            mon.spectralDelta(i).setValue(board.spectral_delta.at(i));
        }

        for (/* none */; i < numSpectralDeltaMps; i++) {
            mon.spectralDelta(i).setValue(0);
        }
    }
}

void convertDigitizerBoard(
        const cobra::MonitorDigitizer &board,
        carma::monitor::WbcBandSubsystem::Digitizer &mon)
{
    // Information common across both digitizer and correlator
    //
    // Things such as the header, sequence number, command status, etc.
    convertCobraGeneric(board, mon);

    // board location info
    const cobra::CorrelatorBoardInfo &info = board.boardInfo();
    mon.boardNumber().setValue(info.boardNumber());
    mon.slotNumber().setValue(info.slotNumber());

    // board type / revision
    mon.boardType().setValue(convertToHexString(board.board_type));
    mon.boardRevision().setValue(convertToHexString(board.board_rev));
    mon.boardSerialNumber().setValue(board.board_serial_num);

    // module revision
    mon.moduleRevision().setValue(convertVersionToString(board.module_rev));
    mon.moduleSerialNumber().setValue(board.module_serial_num);

    // version info
    mon.versionDspSoftware().setValue(convertVersionToString(board.ver_dsp_sw));
    mon.versionSysHdl().setValue(convertVersion(board.ver_sys_hdl));
    mon.versionFpgaHdl().setValue(convertVersion(board.ver_fpga_hdl));
    mon.versionModuleHdl().setValue(convertVersion(board.ver_module_hdl));

    // RTOS stats
    mon.osTaskCounter().setValue(board.os_task_ctr);
    mon.osCpuUsage().setValue(board.os_cpu_usage);
    mon.osContextSwitchCounter().setValue(board.os_ctx_sw_ctr);

    // Reference status
    mon.timeReferenceStatus().setValue(board.time_ref_status);
    mon.phaseReferenceStatus().setValue(board.phase_ref_status);

    // DSP time status
    mon.dspStatus().setValue(board.dsp_status);
    mon.timeOfLastUpdate().setValue(board.time_of_last_update.mjdDays());
    mon.uptime().setValue(board.uptime.utcSeconds());

    // Board monitor points
    mon.fpga1Temperature().setValue(board.temperature[0]);
    mon.fpga3Temperature().setValue(board.temperature[1]);
    mon.digATemperature().setValue(board.temperature[2]);
    mon.digBTemperature().setValue(board.temperature[3]);
    mon.phase().setValue(board.phase);
    mon.vcc().setValue(board.vcc);
    mon.vcc3().setValue(board.vcc3);
    mon.vint_sys().setValue(board.vint_sys);
    mon.vint_fpga().setValue(board.vint_fpga);
    mon.vecl().setValue(board.vecl);
    mon.vtt().setValue(board.vtt);

    // Digitizer monitor points
    mon.quantizationStatesValid().setValue(board.qstates_valid);
    mon.digA().states00().setValue(board.qstates[0][0]);
    mon.digA().states01().setValue(board.qstates[0][1]);
    mon.digA().states10().setValue(board.qstates[0][2]);
    mon.digA().states11().setValue(board.qstates[0][3]);

    mon.digB().states00().setValue(board.qstates[1][0]);
    mon.digB().states01().setValue(board.qstates[1][1]);
    mon.digB().states10().setValue(board.qstates[1][2]);
    mon.digB().states11().setValue(board.qstates[1][3]);

    mon.digA().vplus().setValue(board.thresholds[0][0]);
    mon.digA().vzero().setValue(board.thresholds[0][1]);
    mon.digA().vminus().setValue(board.thresholds[0][2]);
    mon.digA().voffset().setValue(board.thresholds[0][3]);

    mon.digB().vplus().setValue(board.thresholds[1][0]);
    mon.digB().vzero().setValue(board.thresholds[1][1]);
    mon.digB().vminus().setValue(board.thresholds[1][2]);
    mon.digB().voffset().setValue(board.thresholds[1][3]);

    mon.digA().digitalDelay().setValue(board.digital_delay[0]);
    mon.digB().digitalDelay().setValue(board.digital_delay[1]);

    // FPGA clock control/status
    mon.digA().coarseClockDelay().setValue(board.coarse_clock_delay[0]);
    mon.digB().coarseClockDelay().setValue(board.coarse_clock_delay[1]);
    mon.digA().fineClockDelay().setValue(board.fine_clock_delay[0]);
    mon.digB().fineClockDelay().setValue(board.fine_clock_delay[1]);

    // Common COBRA correlation info
    convertCobraCorrelationInfo(board, mon);
}

template <typename CARMA>
void convertCorrelatorBoard(
    const cobra::MonitorCorrelator &board,
    CARMA &mon)
{
    // Convert generic COBRA information
    convertCobraGeneric(board, mon);

    // board location info
    const cobra::CorrelatorBoardInfo &info = board.boardInfo();
    mon.boardNumber().setValue(info.boardNumber());
    mon.slotNumber().setValue(info.slotNumber());
    mon.boardSerialNumber().setValue(board.board_serial_num);

    // board type / revision
    mon.boardType().setValue(convertToHexString(board.board_type));
    mon.boardRevision().setValue(convertToHexString(board.board_rev));

    // version info
    mon.versionDspSoftware().setValue(convertVersionToString(board.ver_dsp_sw));

    mon.versionSysHdl().setValue(convertVersion(board.ver_sys_hdl));
    mon.versionFpgaHdl().setValue(convertVersion(board.ver_fpga_hdl));

    // RTOS stats
    mon.osTaskCounter().setValue(board.os_task_ctr);
    mon.osCpuUsage().setValue(board.os_cpu_usage);
    mon.osContextSwitchCounter().setValue(board.os_ctx_sw_ctr);

    // Reference status
    mon.timeReferenceStatus().setValue(board.time_ref_status);
    mon.phaseReferenceStatus().setValue(board.phase_ref_status);

    // DSP time status
    mon.dspStatus().setValue(board.dsp_status);
    mon.timeOfLastUpdate().setValue(board.time_of_last_update.mjdDays());
    mon.uptime().setValue(board.uptime.utcSeconds());

    // Board monitor points
    mon.fpga1Temperature().setValue(board.temperature[0]);
    mon.fpga7Temperature().setValue(board.temperature[1]);

    mon.phase0().setValue(board.phase[0]);
    mon.phase1().setValue(board.phase[1]);
    mon.phase2().setValue(board.phase[2]);
    mon.phase3().setValue(board.phase[3]);

    mon.vcc().setValue(board.vcc);
    mon.vcc3().setValue(board.vcc3);
    mon.vint_sys().setValue(board.vint_sys);
    mon.vint_fpga().setValue(board.vint_fpga);

    // FPGA clock control/status
    mon.clockDelay().setValue(board.clock_delay);

    // Correlation info
    convertCobraCorrelationInfo(board, mon);
}

// Convert data which is present on both CARMA digitizers and CARMA correlators
// in the same locations
template <typename COBRA, typename CARMA>
void convertCarmaGeneric(const COBRA &board, CARMA &mon)
{
    const cobra::MonitorCarmaCommon &common = board.common;
    const cobra::CorrelatorBoardInfo &info = board.boardInfo();

    // Common header information
    convertHeader(board, mon);

    // command status enumeration
    convertCommandStatus(common, mon);

    // sequence number / command status
    mon.corrSeqNo().setValue(common.sequenceNumber);
    mon.commandStatusBitPattern().setValue(common.commandStatus);

    // various enumerations
    {
        typedef carma::monitor::BitModeMonitorPointEnum::BITMODE BMEnum;
        typedef carma::monitor::CorrStatusMonitorPointEnum::CORRSTATUS CSEnum;
        typedef carma::monitor::AstroBandModeMonitorPointEnum::ASTROBANDMODE ABMEnum;

        const BMEnum bm = static_cast<BMEnum>(common.bitMode);
        const CSEnum cs = static_cast<CSEnum>(common.corrStatus);
        const ABMEnum abmode = static_cast<ABMEnum>(common.astroBandMode);

        mon.bitMode().setValue(bm);
        mon.corrStatus().setValue(cs);
        mon.astroBandMode().setValue(abmode);
    }

    // Board location info
    mon.boardNumber().setValue(info.boardNumber());
    mon.slotNumber().setValue(info.slotNumber());

    // Board revision/serial numbers
    mon.boardType().setValue(common.board_type);
    mon.boardRevision().setValue(common.board_rev);
    mon.boardSerialNumber().setValue(common.board_serial_num);

    // Version info
    mon.versionDspSoftware().setValue(common.ver_ppc_sw);
    mon.versionSysHdl().setValue(convertVersion(common.ver_sys_hdl));
    mon.versionFpgaHdl().setValue(convertVersion(common.ver_fpga_hdl));

    // Reference status
    mon.timeReferenceStatus().setValue(common.time_ref_status);
    mon.phaseReferenceStatus().setValue(common.phase_ref_status);

    // uptime
    mon.uptime().setValue(common.uptime.utcSeconds());

    // RTOS stats
    mon.osTaskCounter().setValue(common.taskCount);
    mon.osCpuUsage().setValue(static_cast<int>(common.cpuUsage));
    mon.osLoadAverage().setValue(common.loadAverage);
    mon.osFreeRam().setValue(common.freeRAM);
    mon.osEccErrors().setValue(common.eccErrors);
    mon.osContextSwitchCounter().setValue(common.contextSwitchCount);
}

// converts cobra::MonitorCarmaCommon to the monitor::CarmaMonitorCommon
// container.
// If the board data were missing, the
// sensor values as INVALID_NO_DATA. See bug #800
// http://www.mmarray.org/bugzilla/show_bug.cgi?id=800
template <typename CARMA>
void convertCarmaCommon(
        const cobra::MonitorCarmaCommon &common,
        CARMA &mon,
        const carma::monitor::MonitorPoint::VALIDITY &validity)
{
    const ScopedLogNdc ndc("CorrMonUpdater::convertCarmaCommon");

    // This method takes data from cobra::MonitorCarmaCommon
    // which are unique to the new CARMA boards and puts them
    // in the monitor::CarmaMonitorCommon container.
    // See monitor/CarmaCorrelatorCommon.mpml

    // CPCI voltages
    {
        const unsigned int cobraSamples = static_cast<unsigned int>(cobra::MonitorCarmaCommon::NUM_CPCI);
        const unsigned int carmaSamples = mon.power().getNumCpciVoltage();
        const unsigned int samples = ::std::min(cobraSamples, carmaSamples);

        if (carmaSamples != cobraSamples) {
            std::ostringstream oss;
            oss << " Number of CPCI voltage samples in board data ["
                << cobraSamples
                << "] does not match monitor point sample number ["
                << carmaSamples
                << "].  Using smaller number."
                ;
            programLogWarnIfPossible(oss.str());
        }

        for(unsigned int i = 0; i < samples; i++) {
            mon.power().cpciVoltage(i).setValue(common.cPciVoltage[i]);
            mon.power().cpciVoltage(i).setValidity(validity);
        }
    }

    // CPCI currents
    {
        const unsigned int cobraSamples = static_cast<unsigned int>(cobra::MonitorCarmaCommon::NUM_CPCI);
        const unsigned int carmaSamples = mon.power().getNumCpciCurrent();
        const unsigned int samples = ::std::min(cobraSamples, carmaSamples);

        if (cobraSamples != carmaSamples) {
            std::ostringstream oss;
            oss << " Number of CPCI current samples in board data ["
                << cobraSamples
                << "] does not match monitor point sample number ["
                << carmaSamples
                << "].  Using smaller number."
                ;
            programLogWarnIfPossible(oss.str());
        }

        for(unsigned int i = 0; i < samples; i++) {
            mon.power().cpciCurrent(i).setValue(common.cPciCurrent[i]);
            mon.power().cpciCurrent(i).setValidity(validity);
        }
    }

    // FPGA voltages
    {
        const unsigned int cobraSamples = static_cast<unsigned int>(cobra::MonitorCarmaCommon::NUM_FPGA);
        const unsigned int carmaSamples = mon.power().getNumFpgaVoltage();
        const unsigned int samples = ::std::min(cobraSamples, carmaSamples);

        if (cobraSamples != carmaSamples) {
            std::ostringstream oss;
            oss << " Number of FPGA voltage samples in board data ["
                << cobraSamples
                << "] does not match monitor point sample number ["
                << carmaSamples
                << "].  Using smaller number."
                ;
            programLogWarnIfPossible(oss.str());
        }

        for(unsigned int i = 0; i < samples; i++) {
            mon.power().fpgaVoltage(i).setValue(common.fpgaVoltage[i]);
            mon.power().fpgaVoltage(i).setValidity(validity);
        }
    }

    // FPGA currents
    {
        const unsigned int cobraSamples = static_cast<unsigned int>(cobra::MonitorCarmaCommon::NUM_FPGA);
        const unsigned int carmaSamples = mon.power().getNumFpgaCurrent();
        const unsigned int samples = ::std::min(cobraSamples, carmaSamples);

        if (cobraSamples != carmaSamples) {
            std::ostringstream oss;
            oss << " Number of FPGA current samples in board data ["
                << cobraSamples
                << "] does not match monitor point sample number ["
                << carmaSamples
                << "].  Using smaller number."
                ;
            programLogWarnIfPossible(oss.str());
        }

        for(unsigned int i = 0; i < samples; i++) {
            mon.power().fpgaCurrent(i).setValue(common.fpgaCurrent[i]);
            mon.power().fpgaCurrent(i).setValidity(validity);
        }
    }

    // System controller voltages
    {
        const unsigned int cobraSamples = static_cast<unsigned int>(cobra::MonitorCarmaCommon::NUM_SYS);
        const unsigned int carmaSamples = mon.power().getNumSysVoltage();
        const unsigned int samples = ::std::min(cobraSamples, carmaSamples);

        if (cobraSamples != carmaSamples) {
            std::ostringstream oss;
            oss << " Number of System Controller voltage samples in board data ["
                << cobraSamples
                << "] does not match monitor point sample number ["
                << carmaSamples
                << "].  Using smaller number."
                ;
            programLogWarnIfPossible(oss.str());
        }

        for(unsigned int i = 0; i < samples; i++) {
            mon.power().sysVoltage(i).setValue(common.sysVoltage[i]);
            mon.power().sysVoltage(i).setValidity(validity);
        }
    }

    // System controller currents
    {
        const unsigned int cobraSamples = static_cast<unsigned int>(cobra::MonitorCarmaCommon::NUM_SYS);
        const unsigned int carmaSamples = mon.power().getNumSysCurrent();
        const unsigned int samples = ::std::min(cobraSamples, carmaSamples);

        if (cobraSamples != carmaSamples) {
            std::ostringstream oss;
            oss << " Number of System Controller current samples in board data ["
                << cobraSamples
                << "] does not match monitor point sample number ["
                << carmaSamples
                << "].  Using smaller number."
                ;
            programLogWarnIfPossible(oss.str());
        }

        for(unsigned int i = 0; i < samples; i++) {
            mon.power().sysCurrent(i).setValue(common.sysCurrent[i]);
            mon.power().sysCurrent(i).setValidity(validity);
        }
    }

    // Digitizer voltages
    {
        const unsigned int cobraSamples = static_cast<unsigned int>(cobra::MonitorCarmaCommon::NUM_DIGITIZER);
        const unsigned int carmaSamples = mon.power().getNumDigitizerVoltage();
        const unsigned int samples = ::std::min(cobraSamples, carmaSamples);

        if (cobraSamples != carmaSamples) {
            std::ostringstream oss;
            oss << " Number of digitizer voltage samples in board data ["
                << cobraSamples
                << "] does not match monitor point sample number ["
                << carmaSamples
                << "].  Using smaller number."
                ;
            programLogWarnIfPossible(oss.str());
        }

        for(unsigned int i = 0; i < samples; i++) {
            mon.power().digitizerVoltage(i).setValue(common.digitizerVoltage[i]);
            mon.power().digitizerVoltage(i).setValidity(validity);
        }
    }

    // Board temperatures
    {
        mon.temperature().ppc().setValue(common.temperature.ppc);
        mon.temperature().ppc().setValidity(validity);
        mon.temperature().digitizer().setValue(common.temperature.digitizer);
        mon.temperature().digitizer().setValidity(validity);
        mon.temperature().sys().setValue(common.temperature.sys);
        mon.temperature().sys().setValidity(validity);

        // FPGA temperatures
        const unsigned int cobraSamples = static_cast<unsigned int>(cobra::MonitorCarmaCommon::NUM_FPGA);
        const unsigned int carmaSamples = mon.temperature().getNumFpga();
        const unsigned int samples = ::std::min(cobraSamples, carmaSamples);

        if (cobraSamples != carmaSamples) {
            std::ostringstream oss;
            oss << " Number of FPGA temperature samples in board data ["
                << cobraSamples
                << "] does not match monitor point sample number ["
                << carmaSamples
                << "].  Using smaller number."
                ;
            programLogWarnIfPossible(oss.str());
        }

        for(unsigned int i = 0; i < samples; i++) {
            mon.temperature().fpga(i).setValue(common.temperature.fpga[i]);
            mon.temperature().fpga(i).setValidity(validity);
        }
    }

    // Sensor temperatures
    {
        const unsigned int cobraSamples = static_cast<unsigned int>(cobra::MonitorCarmaCommon::NUM_SENSOR);
        const unsigned int carmaSamples = mon.temperature().getNumSensor();
        const unsigned int samples = ::std::min(cobraSamples, carmaSamples);

        if (cobraSamples != carmaSamples) {
            std::ostringstream oss;
            oss << " Number of temperature sensor samples in board data ["
                << cobraSamples
                << "] does not match monitor point sample number ["
                << carmaSamples
                << "].  Using smaller number."
                ;
            programLogWarnIfPossible(oss.str());
        }

        for(unsigned int i = 0; i < samples; i++) {
            mon.temperature().sensor(i).setValue(common.temperature.sensor[i]);
            mon.temperature().sensor(i).setValidity(validity);
        }
    }

    // power supply status
    {
        /// FIX THE REST OF THESE NOW USING CARMACORRELATORCOMMON.MPML
        // 12/21/2012 MWP
        typedef carma::monitor::CorrelatorPowerSupply::PowerSupplyStatusMonitorPointEnum PSSMPEnum;

        mon.power().powerSupplyStatus().setValidity(validity);

        switch (common.powerSupplyStatus) {
        case cobra::POWER_SUPPLY_ENABLED:
            mon.power().powerSupplyStatus().setValue(PSSMPEnum::ENABLED);
            break;
        case cobra::POWER_SUPPLY_DISABLED:
            mon.power().powerSupplyStatus().setValue(PSSMPEnum::DISABLED);
            break;
        default:
            // unknown status is bad.
            programLogWarnIfPossible("Got unknown power supply status -- setting status to BAD");
            /* fallthrough */
        case cobra::POWER_SUPPLY_BAD:
            mon.power().powerSupplyStatus().setValue(PSSMPEnum::BAD);
            break;
        }
    }

    // power failure reason
    {
        typedef carma::monitor::CorrelatorPowerSupply::PowerFailureReasonMonitorPointEnum PFRMPEnum;

        mon.power().powerFailureReason().setValidity(validity);

        switch(common.powerFailureReason) {
        ///< Supplies are OK, no failure
        case cobra::POWER_FAIL_OK:
            mon.power().powerFailureReason().setValue(PFRMPEnum::OK);
            break;
        ///< 1.2V supply hit its current limit
        case cobra::POWER_FAIL_1V2_OVERCURRENT:
            mon.power().powerFailureReason().setValue(PFRMPEnum::OVERCURRENT_1V2);
            break;
        ///< 1.2V supply bad
        case cobra::POWER_FAIL_1V2_POWERBAD:
            mon.power().powerFailureReason().setValue(PFRMPEnum::POWERBAD_1V2);
            break;
        ///< 2.5V supply hit its current limit
        case cobra::POWER_FAIL_2V5_OVERCURRENT:
            mon.power().powerFailureReason().setValue(PFRMPEnum::OVERCURRENT_2V5);
            break;
        ///< 2.5V supply bad
        case cobra::POWER_FAIL_2V5_POWERBAD:
            mon.power().powerFailureReason().setValue(PFRMPEnum::POWERBAD_2V5);
            break;
        ///< 3.3V supply hit its current limit
        case cobra::POWER_FAIL_3V3_OVERCURRENT:
            mon.power().powerFailureReason().setValue(PFRMPEnum::OVERCURRENT_3V3);
            break;
        ///< 3.3V supply bad
        case cobra::POWER_FAIL_3V3_POWERBAD:
            mon.power().powerFailureReason().setValue(PFRMPEnum::POWERBAD_3V3);
            break;
        ///< 12V supply hit its current limit
        case cobra::POWER_FAIL_12V_OVERCURRENT:
            mon.power().powerFailureReason().setValue(PFRMPEnum::OVERCURRENT_12V);
            break;
        ///< 12V supply hit its upper voltage limit
        case cobra::POWER_FAIL_12V_OVERVOLTAGE:
            mon.power().powerFailureReason().setValue(PFRMPEnum::OVERVOLTAGE_12V);
            break;
        ///< 12V supply hit its lower voltage limit
        case cobra::POWER_FAIL_12V_UNDERVOLTAGE:
            mon.power().powerFailureReason().setValue(PFRMPEnum::UNDERVOLTAGE_12V);
            break;
        ///< 12V supply bad
        case cobra::POWER_FAIL_12V_POWERBAD:
            mon.power().powerFailureReason().setValue(PFRMPEnum::POWERBAD_12V);
            break;
        ///< FGPA#0 supply hit its temperature limit
        case cobra::POWER_FAIL_FPGA0_OVERTEMPERATURE:
            mon.power().powerFailureReason().setValue(PFRMPEnum::OVERTEMPERATURE_FPGA0);
            break;
        ///< FGPA#1 supply hit its temperature limit
        case cobra::POWER_FAIL_FPGA1_OVERTEMPERATURE:
            mon.power().powerFailureReason().setValue(PFRMPEnum::OVERTEMPERATURE_FPGA1);
            break;
        ///< FGPA#2 supply hit its temperature limit
        case cobra::POWER_FAIL_FPGA2_OVERTEMPERATURE:
            mon.power().powerFailureReason().setValue(PFRMPEnum::OVERTEMPERATURE_FPGA2);
            break;
        ///< FGPA#3 supply hit its temperature limit
        case cobra::POWER_FAIL_FPGA3_OVERTEMPERATURE:
            mon.power().powerFailureReason().setValue(PFRMPEnum::OVERTEMPERATURE_FPGA3);
            break;
        ///< Unknown reason
        default:
        case cobra::POWER_FAIL_UNKNOWN:
            mon.power().powerFailureReason().setValue(PFRMPEnum::UNKNOWN);
            break;
        }
    }

    // board temperature status
    {
        typedef carma::monitor::CorrelatorTemperature::TemperatureStatusMonitorPointEnum TSMPEnum;

        mon.temperature().temperatureStatus().setValidity(validity);

        switch (common.temperatureStatus) {
        case cobra::TEMPERATURE_OK:
            mon.temperature().temperatureStatus().setValue(TSMPEnum::OK);
            break;
        case cobra::TEMPERATURE_WARNING:
            mon.temperature().temperatureStatus().setValue(TSMPEnum::WARNING);
            break;
        default: // unknown status is bad.
            programLogWarnIfPossible("Got unknown temperature status -- setting status to CRITICAL");
        case cobra::TEMPERATURE_CRITICAL:
            mon.temperature().temperatureStatus().setValue(TSMPEnum::CRITICAL);
            break;
        }
    }
}

// Convert monitor points specific to a CARMA digitizer
template <typename CARMA>
void convertCarmaDigitizer(
    const cobra::MonitorCarmaDigitizer &board,
    CARMA &mon)
{
    mon.digA().gain().setValue(board.thresholdGainA);
    mon.digA().offset().setValue(board.thresholdOffsetA);
    mon.digA().numSamplesIn().setValue(board.inStatsA.numSamples);
    mon.digA().meanIn().setValue(board.inStatsA.mean);
    mon.digA().varianceIn().setValue(board.inStatsA.variance);
    mon.digA().chiSquaredIn().setValue(board.inStatsA.chi2);
    mon.digA().numSamplesOut().setValue(board.outStatsA.numSamples);
    mon.digA().meanOut().setValue(board.outStatsA.mean);
    mon.digA().varianceOut().setValue(board.outStatsA.variance);
    mon.digA().chiSquaredOut().setValue(board.outStatsA.chi2);

    mon.digB().gain().setValue(board.thresholdGainB);
    mon.digB().offset().setValue(board.thresholdOffsetB);
    mon.digB().numSamplesIn().setValue(board.inStatsB.numSamples);
    mon.digB().meanIn().setValue(board.inStatsB.mean);
    mon.digB().varianceIn().setValue(board.inStatsB.variance);
    mon.digB().chiSquaredIn().setValue(board.inStatsB.chi2);
    mon.digB().numSamplesOut().setValue(board.outStatsB.numSamples);
    mon.digB().meanOut().setValue(board.outStatsB.mean);
    mon.digB().varianceOut().setValue(board.outStatsB.variance);
    mon.digB().chiSquaredOut().setValue(board.outStatsB.chi2);

    for (unsigned int i = 0 ; i < cobra::DigitizerStatistics::HIST_SIZE; ++i) {
        mon.digA().qStateIn(i).setValue(board.inStatsA.histogram[i]);
        mon.digA().qStateOut(i).setValue(board.outStatsA.histogram[i]);

        mon.digB().qStateIn(i).setValue(board.inStatsB.histogram[i]);
        mon.digB().qStateOut(i).setValue(board.outStatsB.histogram[i]);
    }
}


template <typename CARMA>
void convertCorrelatorBoard(
    const cobra::MonitorCarmaCorrelator &board,
    CARMA &mon)
{
    // CARMA board generic monitor points
    convertCarmaGeneric(board, mon);

    const ReceivedMonitorPointEnum::RECEIVED boardRxStatus = convertRxStatus(board);
    const MonitorPoint::VALIDITY validity = (boardRxStatus == ReceivedMonitorPointEnum::MISSING)
        ? MonitorPoint::INVALID_NO_DATA
        : MonitorPoint::VALID;

    // CARMA board common monitor points (voltages, etc.)
    convertCarmaCommon(board.common, mon, validity);

    // TODO FIXME: do we have any CARMA correlator specific stuff?
}

#if (( COBRA_COMPILETIME_VERSION_MINOR > 36 ) && ( COBRA_COMPILETIME_VERSION_PATCH > 7 ))
template <typename CARMA>
void convertCorrelatorBoard(
    const cobra::MonitorCarma3GCorrelator &board,
    CARMA &mon)
{
    // CARMA board generic monitor points
    convertCarmaGeneric(board, mon);

    const ReceivedMonitorPointEnum::RECEIVED boardRxStatus = convertRxStatus(board);
    const MonitorPoint::VALIDITY validity = (boardRxStatus == ReceivedMonitorPointEnum::MISSING)
        ? MonitorPoint::INVALID_NO_DATA
        : MonitorPoint::VALID;

    // CARMA board common monitor points (voltages, etc.)
    convertCarmaCommon(board.common, mon, validity);

    // TODO FIXME: do we have any CARMA correlator specific stuff?
}
#endif

void convertDigitizerBoard(
    const cobra::MonitorCarmaDigitizer &board,
    carma::monitor::CarmaSlcBandSubsystem::Digitizer &mon)
{
    // CARMA board generic monitor points
    convertCarmaGeneric(board, mon);

    const ReceivedMonitorPointEnum::RECEIVED boardRxStatus = convertRxStatus(board);
    const MonitorPoint::VALIDITY validity = (boardRxStatus == ReceivedMonitorPointEnum::MISSING)
        ? MonitorPoint::INVALID_NO_DATA
        : MonitorPoint::VALID;

    // CARMA board common monitor points (voltages, etc.)
    convertCarmaCommon(board.common, mon, validity);

    // CARMA digitizer specific monitor points
    convertCarmaDigitizer(board, mon);
}

void convertDigitizerBoard(
    const cobra::MonitorCarma3GDigitizer &board,
    carma::monitor::Carma3GBandSubsystem::Digitizer &mon)
{
    // CARMA board generic monitor points
    convertCarmaGeneric(board, mon);

    const ReceivedMonitorPointEnum::RECEIVED boardRxStatus = convertRxStatus(board);
    const MonitorPoint::VALIDITY validity = (boardRxStatus == ReceivedMonitorPointEnum::MISSING)
        ? MonitorPoint::INVALID_NO_DATA
        : MonitorPoint::VALID;

    // CARMA board common monitor points (voltages, etc.)
    convertCarmaCommon(board.common, mon, validity);

}

template <typename CARMA>
void convertInterpolatorSamples(
        const cobra::MonitorBand &band,
        CARMA &mon)
{
    {
        std::ostringstream oss;
        oss << "Number of interpolator samples received from astroband "
            << band.parameters().bandNumber()
            << " is " << band.delays().size();
        CARMA_CPTRACE(TRACE_ALL, oss.str());
    }

    const unsigned int cobraSamples = band.delays().size();
    const unsigned int carmaSamples = mon.interpolatorSamplesCount();

    if (carmaSamples < cobraSamples) {
        std::ostringstream oss;
        oss << "Received " << cobraSamples << " interpolator samples"
            << " from COBRA, but CARMA only has space for " << carmaSamples;
        CARMA_CPTRACE(TRACE_ALL, oss.str());
    }

    int i = 0;
    BOOST_FOREACH(const cobra::CorrelatorInterpolatorSamples::value_type &t, band.delays()) {
        const cobra::CorrelatorInterpolatorSamples::key_type &input = t.first;
        const cobra::CorrelatorInterpolatorSamples::mapped_type &delays = t.second;

        // grab the current CARMA monitor container for interpolator samples
        const carma::monitor::CorMonitorInterpSamps &interp = mon.interpolatorSamples(i);
        i++;

        interp.inputNumber().setValue(input);

        const unsigned int cobraDelays = delays.size();
        const unsigned int carmaDelays = interp.inputDelaysCount();
        const unsigned int maxDelays = ::std::min(cobraDelays, carmaDelays);

        for (unsigned int j = 0; j < maxDelays; j++) {
            interp.inputDelays(j).timestamp().setValue(delays.timestamp(j).mjdDays());
            interp.inputDelays(j).delay().setValue(delays.delay(j).nanoseconds());
        }
    }
}

void convertBoardMonitorData(
        const cobra::MonitorBand &band,
        carma::monitor::WbcBandSubsystem &mon)
{
    const ScopedLogNdc ndc("CorrMonUpdater::convertBoardMonitorData(COBRA)");

    {
        std::ostringstream oss;
        oss << "Band name: [" << band.getName() << "]";
        CARMA_CPTRACE(TRACE_ALL, oss.str());
    }

    const int digMax = mon.digitizerCount();
    const int corMax = mon.correlatorCount();

    int digIndex = 0;
    int corIndex = 0;

    const std::vector<cobra::MonitorDataPtr> &data = band.data();
    BOOST_FOREACH(const cobra::MonitorDataPtr &p, data) {
        const cobra::MonitorData * const monitorData = p.get();
        if (monitorData == NULL)
            continue;

        const cobra::monitorDataType monType = monitorData->monitorType();
        switch (monType) {
        case cobra::MONITOR_DATA_TYPE_COBRA_CORRELATOR:
            {
                // cannot handle more boards than the CARMA monitor system
                if (corIndex >= corMax) {
                    ostringstream os;
                    os << " Found too many COBRA-type correlators. Expected: "
                       << corMax << " Received: " << corIndex;
                    programLogErrorIfPossible( os.str() );
                    continue;
                }

                // convert correlator board data
                const cobra::MonitorCorrelator &corBoard =
                    *static_cast<const cobra::MonitorCorrelator *>(monitorData);
                convertCorrelatorBoard(corBoard, mon.correlator(corIndex));

                // increment CARMA monitor data index
                corIndex++;
            }
            break;
        case cobra::MONITOR_DATA_TYPE_COBRA_DIGITIZER:
            {
                // cannot handle more boards than the CARMA monitor system
                if (digIndex >= digMax) {
                    ostringstream os;
                    os << " Found too many COBRA-type digitizers. Expected: "
                       << digMax << " Received: " << digIndex;
                    programLogErrorIfPossible( os.str() );
                    continue;
                }

                // convert digitizer board data
                const cobra::MonitorDigitizer &digBoard =
                    *static_cast<const cobra::MonitorDigitizer *>(monitorData);
                convertDigitizerBoard(digBoard, mon.digitizer(digIndex));

                // increment CARMA monitor data index
                digIndex++;
            }
            break;
        case cobra::MONITOR_DATA_TYPE_BAND:
        case cobra::MONITOR_DATA_TYPE_BAND_PROCESSOR:
        case cobra::MONITOR_DATA_TYPE_BANDS:
        case cobra::MONITOR_DATA_TYPE_BOARD:
        case cobra::MONITOR_DATA_TYPE_CARMA_CORRELATOR:
        case cobra::MONITOR_DATA_TYPE_CARMA_DIGITIZER:
        case cobra::MONITOR_DATA_TYPE_CARMA_PROCESSOR:
        case cobra::MONITOR_DATA_TYPE_UNKNOWN:
            {
                std::ostringstream oss;
                oss << "Skipping unexpected monitor data type: "
                    << cobra::getNameForMonitorDataTypeEnum(monType)
                    << ". Mixed hardware types in a single band?";
                programLogErrorIfPossible(oss.str());
            }
            break;
        default:
            break;
        }
    }
}

void convertBoardMonitorData(
        const cobra::MonitorBand &band,
        carma::monitor::CarmaSlcBandSubsystem &mon)
{
    const ScopedLogNdc ndc("CorrMonUpdater::convertBoardMonitorData(CARMA)");

    {
        std::ostringstream oss;
        oss << "Band name: [" << band.getName() << "]";
        CARMA_CPTRACE(TRACE_ALL, oss.str());
    }

    const int digMax = mon.digitizerCount();
    const int corMax = mon.correlatorCount();

    int digIndex = 0;
    int corIndex = 0;

    const std::vector<cobra::MonitorDataPtr> &data = band.data();
    BOOST_FOREACH(const cobra::MonitorDataPtr &p, data) {
        const cobra::MonitorData * const monitorData = p.get();
        if (monitorData == NULL)
            continue;

        const cobra::monitorDataType monType = monitorData->monitorType();
        switch (monType) {
        case cobra::MONITOR_DATA_TYPE_CARMA_DIGITIZER:
            {
                // cannot handle more boards than the CARMA monitor system
                if (digIndex >= digMax) {
                    ostringstream os;
                    os << " Found too many CARMA-type digitizers. Expected: "
                       << digMax << " Received: " << digIndex;
                    programLogErrorIfPossible( os.str() );
                    continue;
                }

                // convert digitizer board data
                const cobra::MonitorCarmaDigitizer &digBoard =
                    *static_cast<const cobra::MonitorCarmaDigitizer *>(monitorData);
                convertDigitizerBoard(digBoard, mon.digitizer(digIndex));

                // increment CARMA monitor data index
                digIndex++;
            }
            break;
        case cobra::MONITOR_DATA_TYPE_CARMA_CORRELATOR:
            {
                // cannot handle more boards than the CARMA monitor system
                if (corIndex >= corMax) {
                    ostringstream os;
                    os << " Found too many CARMA-type correlators. Expected: "
                       << corMax << " Received: " << corIndex;
                    programLogErrorIfPossible( os.str() );
                    continue;
                }

                // convert correlator board data
                const cobra::MonitorCarmaCorrelator &corBoard =
                    *static_cast<const cobra::MonitorCarmaCorrelator *>(monitorData);
                convertCorrelatorBoard(corBoard, mon.correlator(corIndex));

                // increment CARMA monitor data index
                corIndex++;
            }
            break;
        case cobra::MONITOR_DATA_TYPE_BAND:
        case cobra::MONITOR_DATA_TYPE_BAND_PROCESSOR:
        case cobra::MONITOR_DATA_TYPE_BANDS:
        case cobra::MONITOR_DATA_TYPE_BOARD:
        case cobra::MONITOR_DATA_TYPE_COBRA_CORRELATOR:
        case cobra::MONITOR_DATA_TYPE_COBRA_DIGITIZER:
        case cobra::MONITOR_DATA_TYPE_CARMA_PROCESSOR:
        case cobra::MONITOR_DATA_TYPE_UNKNOWN:
            {
                std::ostringstream oss;
                oss << "Skipping unexpected monitor data type: "
                    << cobra::getNameForMonitorDataTypeEnum(monType)
                    << ". Mixed hardware types in a single band?";
                programLogErrorIfPossible(oss.str());
            }
            break;
        default:
            break;
        }
    }
}

void convertBoardMonitorData(
        const cobra::MonitorBand &band,
        carma::monitor::Carma3GBandSubsystem &mon)
{
    const ScopedLogNdc ndc("CorrMonUpdater::convertBoardMonitorData(CARMA3G)");

    {
        std::ostringstream oss;
        oss << "Band name: [" << band.getName() << "]";
        CARMA_CPTRACE(TRACE_ALL, oss.str());
    }

    int digIndex = 0;
    const int digMax = mon.digitizerCount();
#if (( COBRA_COMPILETIME_VERSION_MINOR > 36 ) && ( COBRA_COMPILETIME_VERSION_PATCH > 7 ))
    int corIndex = 0;
    const int corMax = mon.correlatorCount();
#endif

    const std::vector<cobra::MonitorDataPtr> &data = band.data();
    BOOST_FOREACH(const cobra::MonitorDataPtr &p, data) {
        const cobra::MonitorData * const monitorData = p.get();
        if (monitorData == NULL)
            continue;

        const cobra::monitorDataType monType = monitorData->monitorType();
        switch (monType) {
        case cobra::MONITOR_DATA_TYPE_CARMA3G_DIGITIZER:
            {
                // cannot handle more boards than the CARMA3G monitor system
                if (digIndex >= digMax) {
                    ostringstream os;
                    os << " Found too many CARMA3G-type digitizers. Expected: "
                       << digMax << " Received: " << digIndex;
                    programLogErrorIfPossible( os.str() );
                    continue;
                }

                // convert digitizer board data
                const cobra::MonitorCarma3GDigitizer &digBoard =
                    *static_cast<const cobra::MonitorCarma3GDigitizer *>(monitorData);
                convertDigitizerBoard(digBoard, mon.digitizer(digIndex));

                // increment CARMA monitor data index
                digIndex++;
            }
            break;
#if (( COBRA_COMPILETIME_VERSION_MINOR > 36 ) && ( COBRA_COMPILETIME_VERSION_PATCH > 7 ))
        case cobra::MONITOR_DATA_TYPE_CARMA3G_CORRELATOR:
            {
                // cannot handle more boards than the CARMA3G monitor system
                if (corIndex >= corMax) {
                    ostringstream os;
                    os << " Found too many CARMA3G-type correlators. Expected: "
                       << corMax << " Received: " << corIndex;
                    programLogErrorIfPossible( os.str() );
                    continue;
                }

                // convert correlator board data
                const cobra::MonitorCarma3GCorrelator &corBoard =
                    *static_cast<const cobra::MonitorCarma3GCorrelator *>(monitorData);
                convertCorrelatorBoard(corBoard, mon.correlator(corIndex));

                // increment CARMA monitor data index
                corIndex++;
            }
            break;
#endif
        default:
            {
                std::ostringstream oss;
                oss << "Skipping unexpected monitor data type: "
                    << cobra::getNameForMonitorDataTypeEnum(monType)
                    << ". Mixed hardware types in a single band?";
                programLogErrorIfPossible(oss.str());
            }
            break;
        }
    }
}

void convertMonitorData(
        const cobra::MonitorBand &band,
        carma::monitor::Carma3GBandSubsystem &mon)
{
    const ScopedLogNdc ndc("CorrMonUpdater::convertMonitorData(CARMA3G)");

    // common header information
    convertHeader(band, mon);

    // command status enumeration
    convertCommandStatus(band, mon);

    // sequence number / command status
    mon.corrSeqNo().setValue(band.sequenceNumber());
    mon.commandStatusBitPattern().setValue(band.commandStatus());

    // various enumerations
    {
        typedef carma::monitor::BitModeMonitorPointEnum::BITMODE BMEnum;
        typedef carma::monitor::CorrStatusMonitorPointEnum::CORRSTATUS CSEnum;
        typedef carma::monitor::AstroBandModeMonitorPointEnum::ASTROBANDMODE ABMEnum;

        const BMEnum bm = static_cast<BMEnum>(band.bitMode());
        const CSEnum cs = static_cast<CSEnum>(band.corrStatus());
        const ABMEnum abmode = static_cast<ABMEnum>(band.astroBandMode());

        mon.bitMode().setValue(bm);
        mon.corrStatus().setValue(cs);
        mon.astroBandMode().setValue(abmode);
    }

    // interpolator samples
    convertInterpolatorSamples(band, mon);

    // convert the monitor data for each board
    convertBoardMonitorData(band, mon);
}

void convertMonitorData(
        const cobra::MonitorBand &band,
        carma::monitor::CarmaSlcBandSubsystem &mon)
{
    const ScopedLogNdc ndc("CorrMonUpdater::convertMonitorData(CARMA)");

    // common header information
    convertHeader(band, mon);

    // command status enumeration
    convertCommandStatus(band, mon);

    // sequence number / command status
    mon.corrSeqNo().setValue(band.sequenceNumber());
    mon.commandStatusBitPattern().setValue(band.commandStatus());

    // various enumerations
    {
        typedef carma::monitor::BitModeMonitorPointEnum::BITMODE BMEnum;
        typedef carma::monitor::CorrStatusMonitorPointEnum::CORRSTATUS CSEnum;
        typedef carma::monitor::AstroBandModeMonitorPointEnum::ASTROBANDMODE ABMEnum;

        const BMEnum bm = static_cast<BMEnum>(band.bitMode());
        const CSEnum cs = static_cast<CSEnum>(band.corrStatus());
        const ABMEnum abmode = static_cast<ABMEnum>(band.astroBandMode());

        mon.bitMode().setValue(bm);
        mon.corrStatus().setValue(cs);
        mon.astroBandMode().setValue(abmode);
    }

    // interpolator samples
    convertInterpolatorSamples(band, mon);

    // convert the monitor data for each board
    convertBoardMonitorData(band, mon);
}

void convertMonitorData(
        const cobra::MonitorBand &band,
        carma::monitor::WbcBandSubsystem &mon)
{
    const ScopedLogNdc ndc("CorrMonUpdater::convertMonitorData(COBRA)");

    // common header information
    convertHeader(band, mon);

    // command status enumeration
    convertCommandStatus(band, mon);

    // sequence number / command status
    mon.corrSeqNo().setValue(band.sequenceNumber());
    mon.commandStatusBitPattern().setValue(band.commandStatus());

    // various enumerations
    {
        typedef carma::monitor::BitModeMonitorPointEnum::BITMODE BMEnum;
        typedef carma::monitor::CorrStatusMonitorPointEnum::CORRSTATUS CSEnum;
        typedef carma::monitor::AstroBandModeMonitorPointEnum::ASTROBANDMODE ABMEnum;

        // COBRA hardware doesn't support changing these values
        const BMEnum bm = static_cast<BMEnum>(cobra::CORR_2BIT);
        const CSEnum cs = static_cast<CSEnum>(band.corrStatus());
        const ABMEnum abmode = static_cast<ABMEnum>(cobra::SINGLEPOL);

        mon.bitMode().setValue(bm);
        mon.corrStatus().setValue(cs);
        mon.astroBandMode().setValue(abmode);
    }

    // interpolator samples
    convertInterpolatorSamples(band, mon);

    // convert the monitor data for each board
    convertBoardMonitorData(band, mon);
}

// ----------------------------------------------------------------
// Scoped AutoWriter
// ----------------------------------------------------------------

class ScopedAutoWriterManager {
    public:
        explicit ScopedAutoWriterManager( MonitorSubsystem & monSubsys );

        ~ScopedAutoWriterManager( );

        bool running( ) const;

        void start( double x );

        void stop( );

    private:
        MonitorSubsystem & monSubsys_;
        bool               autoWriterRunning_;
};


ScopedAutoWriterManager::ScopedAutoWriterManager(
    MonitorSubsystem & monSubsys ) :
monSubsys_( monSubsys ),
autoWriterRunning_( false )
{
}


ScopedAutoWriterManager::~ScopedAutoWriterManager( )
try {
    stop();
} catch ( ... ) {
    try {
        programLogErrorIfPossible(
            "Stifling exception in"
            " ScopedAutoWriterManager::~ScopedAutoWriterManager - " +
            getStringForCaught() );
    } catch ( ... ) {
    }

    return;
}


bool
ScopedAutoWriterManager::running( ) const
{
    return autoWriterRunning_;
}


void
ScopedAutoWriterManager::start( const double x )
{
    if ( autoWriterRunning_ != true ) {
        programLogInfoIfPossible( "starting autowriter..." );

        monSubsys_.startAutoWriter( x );

        autoWriterRunning_ = true;

        programLogInfoIfPossible( "autowriter running" );
    }
}


void
ScopedAutoWriterManager::stop( )
{
    if ( autoWriterRunning_ ) {
        programLogInfoIfPossible( "stopping autowriter..." );
        monSubsys_.stopAutoWriter();
        autoWriterRunning_ = false;
        programLogInfoIfPossible( "autowriter stopped" );
    }
}

// ----------------------------------------------------------------
// Simulator
// ----------------------------------------------------------------

/*
MonitorSimulatorPtr makeMonSimulator(
        const string & inifile,
        const string & bwText,
        const uint32_t bandNo,
        uint32_t &     bandIndex,
        const cobra::hardwareType hwType)
try {
    cobra::CorrelatorConfiguration config;
    {
        cobra::CorrelatorConfigurationIniImporter importer;

        const int importStatus =
            importer.import( inifile,
                             "CARMA",
                             bwText,
                             config );

        if ( importStatus != 0 ) {
            programLogErrorIfPossible("Cobra config import failure" );
            throw CARMA_ERROR( "Cobra config import failure" );
        }
    }

    {
        const vector< uint32_t > & bandNoVec = config.bandNumber();
        const size_t bandNoVecSize = bandNoVec.size();

        size_t i = 0;

        for ( ; i < bandNoVecSize; ++i ) {
            if ( bandNoVec.at( i ) == bandNo )
                break;
        }

        if ( i >= bandNoVecSize ) {
            programLogErrorIfPossible("Cobra config does not have band number" );
            throw CARMA_ERROR( "Cobra config does not have band number" );
        }

        bandIndex = i;
    }

    MonitorSimulatorPtr result(new cobra::CorrelatorMonitorSimulator);

    {
        ostringstream os;
        os  << "Simulating hardware type "
            << cobra::getNameForHardwareTypeEnum( hwType )
            << " for band "
            << bandIndex+1
            ;

        programLogNoticeIfPossible( os.str() );
    }

    result->configuration( config );

    return result;
} catch (carma::util::ErrorException &ex) {
    std::ostringstream oss;
    oss << "Creating the correlator monitor simulator failed because an "
        << "invalid configuration was requested: " << bwText << ". "
        << "A section may be missing in the INI file. "
        << "The error message was: " << ex.getErrorMessage() << ". "
        << "Ignoring configband command.";
    programLogErrorIfPossible(oss.str());
    return MonitorSimulatorPtr(NULL);
}

// Fixup various fields in the cobra::MonitorBand object that the
// cobra monitor simulator doesn't handle correctly yet
void fixupSimMonBand(
        cobra::MonitorBand &band,
        const cobra::CorrelatorInterpolatorSamples &simSamps)
{

    // set band sequence number
    band.sequenceNumber(bwSeqNo);

    // set band delays
    band.delays() = simSamps;

    // debugging
    {
        std::ostringstream oss;
        oss << "Band: " << band.getName()
        << " simulation sequence number is now "
        << bwSeqNo;
        CARMA_CPTRACE(Trace::TRACE7, oss.str());
    }
}
*/

void printIterationText(const size_t &connectionGoodReads)
{
    std::ostringstream oss;
    oss << std::string(20, '-')
        << " Iteration " << connectionGoodReads
        << std::string(20, '-');
    CARMA_CPTRACE(TRACE_ALL, oss.str());
}

bool hardwareTypeIsCorrect(const cobra::MonitorBand &band, const cobra::hardwareType &hwType)
{
    const cobra::hardwareType rxHwType = band.getHardwareType();
    if (rxHwType != hwType) {
        std::ostringstream oss;
        oss << "Expected band hardware type " << getNameForHardwareTypeEnum(hwType)
            << ", received " << getNameForHardwareTypeEnum(rxHwType) << " instead";
        programLogErrorIfPossible(oss.str());
        return false;
    }

    // everything is ok
    return true;
}

void performTimeChecks(
        const cobra::MonitorBand &band,
        unsigned long &consecutiveWrongTimes)
{
    const double nowMjdSecs = carma::util::Time::MJD() * 86400.0;
    const double tsMjdSecs = band.timestamp().mjdSeconds();
    // Note different test here than in CorrDataUpdater.cc, which
    // subtracts an extra 0.5 second from diffMjdSecs.  This is
    // the correlation data timestamp is expected to be the beginning
    // of the frame while the monitor data timestamp can be up to the
    // end of frame.  I think.
    const double diffMjdSecs = nowMjdSecs - tsMjdSecs;
    const double errMjdSecs = fabs(diffMjdSecs);

    // Major errors
    if (errMjdSecs > 0.5) {

        // Print an error message on first instance and every
        // 600 consecutive instances following
        if (consecutiveWrongTimes++ % 600 == 0) {
            std::ostringstream oss;
            oss << "timestamp appears to be wrong:"
                << std::fixed << std::setprecision(6)
                << " current time = " << nowMjdSecs
                << ", data timestamp = " << tsMjdSecs
                << ", diff (current minus data) = " << diffMjdSecs << ". ";

            if (consecutiveWrongTimes > 1) {
                oss << "There have been " << consecutiveWrongTimes
                    << " consecutive instances of this error.";
            }

            programLogWarnIfPossible(oss.str());
        }

        // leave immediately
        return;
    }

    // Minor problems, but not enough to do anything except warn
    if (errMjdSecs > 0.1) {

        // reset the error counter
        consecutiveWrongTimes = 0;

        std::ostringstream oss;
        oss << "WARNING: timestamp appears to be wrong:"
            << std::fixed << std::setprecision(6)
            << " current time = " << nowMjdSecs
            << ", data timestamp = " << tsMjdSecs
            << ", diff (current minus data) = " << diffMjdSecs;
        CARMA_CPTRACE(TRACE_ALL, oss.str());

        // leave immediately
        return;
    }

    // no problems
    {
        // reset the error counter
        consecutiveWrongTimes = 0;

        std::ostringstream oss;
        oss << "INFO: timestamp is normal:"
            << std::fixed << std::setprecision(6)
            << " current time = " << nowMjdSecs
            << ", data timestamp = " << tsMjdSecs
            << ", diff (current minus data) = " << diffMjdSecs;
        CARMA_CPTRACE(TRACE_ALL, oss.str());

        // leave immediately
        return;
    }
}

template <typename CARMA>
void takeCareOfMonitorData(
        const cobra::MonitorBand &band,
        CARMA &mon,
        CorrelatorConfigChecker &ccc,
        ScopedAutoWriterManager &corrAutoWriterManager)
{
    if (!ccc.isTransportCorrelatorMonitorData()) {
        corrAutoWriterManager.stop();
        return;
    }

    const double delay = ccc.getCorrelatorMonitorDelay();
    if (corrAutoWriterManager.running() != true)
        corrAutoWriterManager.start(delay);

    CARMA_CPTRACE(TRACE_ALL, "send monitor data");

    // the online monitor point is always true
    mon.online().setValue(true);
    mon.online().setValidity(MonitorPoint::VALID);

    // the received cobra::MonitorBand hardware type has already been checked
    // to make sure that it matches the hardware type we expect
    convertMonitorData(band, mon);
}

/*
template <typename CARMA>
void runSimulatedMonitorServer(
        vector<CARMA *> &corrMonSubsys,
        CorrelatorConfigChecker &ccc,
        const cobra::hardwareType &simHwType,
        const uint32_t simBandNo)
{
    const ScopedLogNdc ndc("CorrMonUpdater::runSimulatedMonitorServer");
    const long simTimerOffsetNanos = 100 * 1000 * 1000;

    MonitorSimulatorPtr monSimulator;

    std::string simBwText = "500MHz";
    std::string simSourceName = "rf";
    cobra::CorrelatorInterpolatorSamples simSamps;
    float simDcFreq = 0.0;
    bool simDcSbIsUpper = false;
    bool bdcEnabled = false;

    simInfo.getInfo(&simBwText,
                     &simSourceName,
                     &simSamps,
                     &simDcFreq,
                     &simDcSbIsUpper,
                     &bdcEnabled);

    FrameAlignedTimer simTimer(simTimerOffsetNanos, 1, false);
    cobra::CorrelatorTimestamp simTs;
    uint32_t simBandIndex = 0;

    ScopedAutoWriterManager corrAutoWriterManager(*corrMonSubsys[0]);
    unsigned long consecutiveWrongTimes = 0;
    unsigned long failedSimulationCount = 0;

    programLogInfoIfPossible("Simulating band client connect");

    // this might fail and return a NULL pointer
    monSimulator = makeMonSimulator(simInfo.getInifile(),
                                    simBwText,
                                    simBandNo,
                                    simBandIndex,
                                    simHwType);

    if (monSimulator.get())
        programLogInfoIfPossible("Connected to simulated band client");

    simTimer.ResetNextFireTime(2);

    // read until error
    size_t connectionGoodReads = 0;
    while (true) {
        ThreadQuitTestSelf();

        // debug
        printIterationText(connectionGoodReads);
        if (connectionGoodReads == 0)
            programLogInfoIfPossible("First read attempt on connection");

        std::string newBwText;
        simInfo.getInfo(&newBwText,
                         &simSourceName,
                         &simSamps,
                         &simDcFreq,
                         &simDcSbIsUpper,
                         &bdcEnabled);

        // configuration change
        if (newBwText != simBwText) {
            {
                std::ostringstream oss;
                oss << "BW change from " << simBwText << " to " << newBwText;
                programLogInfoIfPossible(oss.str());
            }

            // this might fail and return a NULL pointer
            uint32_t newBandIndex = 0;
            MonitorSimulatorPtr newSimulator;
            newSimulator = makeMonSimulator(simInfo.getInifile(),
                                            newBwText,
                                            simBandNo,
                                            newBandIndex,
                                            simHwType);

            // if the configuration change was successful, assign it
            if (newSimulator.get()) {
                monSimulator = newSimulator;
                simBandIndex = newBandIndex;
                simBwText = newBwText;
            }
        }

        {
            struct ::timespec ts = simTimer.getNextFireTime();
            if (ts.tv_nsec == simTimerOffsetNanos)
                ts.tv_nsec = 0;
            else
                ts.tv_nsec = 500 * 1000 * 1000;

            simTs.timespec(ts);
        }

        // make sure we have a simulator
        if (monSimulator.get() == NULL) {
            programLogInfoIfPossible("Invalid configuration, no simulator available.");
            simTimer.WaitForNextFireTime();
            continue;
        }

        CARMA_CPTRACE(TRACE_ALL, "Simulating band client read");

        const int status = monSimulator->simulate(simBandIndex);
        monSimulator->updateTimestamp(simTs);

        if (status == -2) {
            programLogErrorIfPossible("Simulation failed due to UNKNOWN hardware type in MonitorBand."
                                      " Check configuration file for 'Hardware' tag.");
            return;
        }

        if (status < 0) {
            programLogWarnIfPossible("Simulation failed.");
            failedSimulationCount++;
        }

        const unsigned long kMaxFailedSimulations = 10;
        if (failedSimulationCount == kMaxFailedSimulations) {
            std::ostringstream oss;
            oss << "Simulation failed " << kMaxFailedSimulations << " times. Quitting.";
            programLogErrorIfPossible(oss.str());
            return;
        }

        cobra::MonitorBand band = monSimulator->band();
        fixupSimMonBand(band, simSamps, seqNoInfo);

        // blocks the thread until the next fire time
        simTimer.WaitForNextFireTime();

        connectionGoodReads++;
        if (connectionGoodReads == 1) {
            programLogInfoIfPossible("First good read on connection");
        }

        // check for incorrect hardware type
        if (!hardwareTypeIsCorrect(band, simHwType))
            return;

        performTimeChecks(band, consecutiveWrongTimes);
        takeCareOfMonitorData(band, *corrMonSubsys[0], ccc, corrAutoWriterManager);
    }
}
*/

template <typename CARMA>
void runHardwareMonitorServer(
        vector<CARMA *> &corrMonSubsys,
        CorrelatorConfigChecker &ccc,
        const cobra::hardwareType &hwType,
        const std::string &controlHost,
        const unsigned short controlPort,
        const int            portOffset,
        const BandNoToMonSysMap & bandNoToMonSysMap)
{
    const ScopedLogNdc ndc("CorrMonUpdater::runHardwareMonitorServer");
    const int kFailedConnectStallSecs = 10;

    Sleeper failedConnectSleeper;
    unsigned long consecutiveWrongTimes = 0;
    vector<ScopedAutoWriterManager *> corrAutoWriterManager;
    for( unsigned i=0; i<corrMonSubsys.size(); ++i) {
        ScopedAutoWriterManager * writer = new ScopedAutoWriterManager( *corrMonSubsys[i] );
        corrAutoWriterManager.push_back( writer );
    }

    // Connection to the correlator
    cobra::CorrelatorBandClient<cobra::MonitorBand> bandClient(cobra::monitorServerPort);

    while (true) {
        ThreadQuitTestSelf();

        if (bandClient.connected())
            bandClient.close();

        programLogInfoIfPossible("Connecting to band client...");

        // the default timeout in cobra::CorrelatorBandClient
        // is 2 seconds
        const int connectStatus = bandClient.connect(controlPort, controlHost.c_str(), portOffset);
        if (connectStatus < 0) {
            const int savedErrno = errno;
            std::ostringstream oss;
            oss << "Connection to band monitor server failed - "
                << strerror(savedErrno)
                << ". Will retry in " << kFailedConnectStallSecs
                << " seconds.";

            programLogErrorIfPossible(oss.str());

            if (bandClient.connected())
                bandClient.close();

            failedConnectSleeper.waitForWholeSecDuration(kFailedConnectStallSecs);
            continue;
        }

        programLogInfoIfPossible("Successfully connected to band client");

        // read until error
        size_t connectionGoodReads = 0;
        while (true) {
            try {
                ThreadQuitTestSelf();
            } catch (...) {
                if (bandClient.connected())
                    bandClient.close();

                throw;
            }

            // debug
            printIterationText(connectionGoodReads);

            if (connectionGoodReads == 0)
                programLogInfoIfPossible("First read attempt on connection");

            // Monitor data received from the correlator
            cobra::MonitorBand band;

            const int readStatus = bandClient.read(band);
            if (readStatus < 0) {
                const int savedErrno = errno;
                std::ostringstream oss;
                oss << "Read error, closing connection: " << strerror(savedErrno);
                programLogErrorIfPossible(oss.str());

                // break out of read loop and try to reconnect
                if (bandClient.connected())
                    bandClient.close();

                break;
            }

            connectionGoodReads++;
            if (connectionGoodReads == 1) {
                programLogInfoIfPossible("First good read on connection");
            }

            // check for incorrect hardware type
            if (!hardwareTypeIsCorrect(band, hwType))
                return;

            // Time checks
            performTimeChecks(band, consecutiveWrongTimes);

            // If not CARMA3G, then the vectors are of length 1.
            unsigned short index = 0;
#if (( COBRA_COMPILETIME_VERSION_MINOR > 36 ) && ( COBRA_COMPILETIME_VERSION_PATCH > 6 ))
            // The astro band number encoded in the MonitorBand object in
            // cobra 2.36.7 and later.
            unsigned short astrobandNo = band.astroBandNumber();
            BandNoToMonSysMap::const_iterator ip = bandNoToMonSysMap.find( astrobandNo );
            if ( ip != bandNoToMonSysMap.end() ) {
                index = ip->second;
            }
#endif

            // Take care of monitor data
            takeCareOfMonitorData(band, *corrMonSubsys[index], ccc, *corrAutoWriterManager[index]);

        } // inner while
    } // outer while
}

}  // namespace < anonymous >

void CorrMonUpdater::runUpdateLoop(
        const int bandNo,
        CorrelatorConfigChecker &ccc,
        const unsigned short controlPort,
        const std::string &controlHost,
        const int portOffset,
        const std::string &hwStr)
{
    cobra::hardwareType hwType = cobra::findHardwareType(hwStr.c_str());
    unsigned numbands = 1;
    unsigned firstCarma3gBandNo = 0;
    if (   StringUtils::equalsIgnoreCase(hwStr,"c3gmax8") 
        || StringUtils::equalsIgnoreCase(hwStr,"c3gmax23") 
       )
    {
        // The following line can be removed when cobra 2.99 is installed.
        hwType = cobra::HARDWARE_TYPE_CARMA3G; 
        numbands = 8; // used in vectorization of monitor system
        //firstCarma3gBandNo = 25;
    }
    CARMA_CPTRACE(TRACE_MISC," MONUPDATER hardware type " << cobra::getNameForHardwareTypeEnum(hwType));

    BandNoToMonSysMap bmm;
    switch (hwType) {
    default:
    case cobra::HARDWARE_TYPE_UNKNOWN:
        {
            std::ostringstream oss;
            oss << "Unrecognized hardware type : " << hwStr
                << ".  Valid values are COBRA or CARMA."
                ;
            throw CARMA_EXCEPTION(carma::util::IllegalArgumentException, oss.str());
        }
        break;
    case cobra::HARDWARE_TYPE_CARMA:
        {
            // carma boards
            vector<CarmaSlcBandSubsystem *> mon;
            for (unsigned short i=0; i < numbands; ++i ) {
                // carma or carma3g boards
                // Note until the monsys is expanded/updated we can't run
                // both carma and carma3g bands simultaneously, as the
                // limit is 8 in mpml.dtd.  This ripples to number of RTD pages
                // etc.
                unsigned short astrobandNo = bandNo+i;
                CarmaSlcBandSubsystem * carmaSlcBandSubsystem = new CarmaSlcBandSubsystem( astrobandNo );

                // Set the initial online value to true, indicating
                // the monitor system publisher is online
                carmaSlcBandSubsystem->online().setValue(true);
                carmaSlcBandSubsystem->online().setValidity(MonitorPoint::VALID);

                mon.push_back(carmaSlcBandSubsystem);
                bmm.insert(make_pair<unsigned short, unsigned short>(astrobandNo, i));
            }

            runHardwareMonitorServer(mon, ccc, hwType, controlHost, controlPort, portOffset, bmm);

            // If runMonitorServer returns, then the monitor system
            // writer is no longer online
            for (unsigned i=0; i < numbands; ++i ) {
                mon.at(i)->online().setValue(false);
                mon.at(i)->online().setValidity(MonitorPoint::VALID);
            }
        }
        break;
    case cobra::HARDWARE_TYPE_COBRA:
        {
            // cobra boards
            vector<WbcBandSubsystem*> mon(1);
            WbcBandSubsystem wbcBandSubsystem( bandNo - 8 );
            mon.at(0) = &wbcBandSubsystem;

            // Set the initial online value to true, indicating
            // the monitor system publisher is online
            mon[0]->online().setValue(true);
            mon[0]->online().setValidity(MonitorPoint::VALID);
            bmm.insert(make_pair<unsigned short, unsigned short>(bandNo, 0));

            runHardwareMonitorServer(mon, ccc, hwType, controlHost, controlPort, portOffset, bmm);

            mon[0]->online().setValue(false);
            mon[0]->online().setValidity(MonitorPoint::VALID);
        }
        break;
    case cobra::HARDWARE_TYPE_CARMA3G:
        {
            CARMA_CPTRACE(TRACE_MISC,"C3G branch");
            // carma boards
            vector<Carma3GBandSubsystem *> mon;
            for (unsigned short i=0; i < numbands; ++i ) {
                // carma or carma3g boards
                // Note until the monsys is expanded/updated we can't run
                // both carma and carma3g bands simultaneously, as the
                // limit is 8 in mpml.dtd.  This ripples to number of RTD pages
                // etc.

                // If CARMA3G, bandNo will be identically zero.
                unsigned short astrobandNo = bandNo+firstCarma3gBandNo+i+1;
                CARMA_CPTRACE(TRACE_MISC,"TRYING c3gbandsubsystem " << astrobandNo);
                Carma3GBandSubsystem * carma3gBandSubsystem = new Carma3GBandSubsystem( astrobandNo );
                CARMA_CPTRACE(TRACE_MISC,"instantiated c3gbandsubsystem " << astrobandNo);

                // Set the initial online value to true, indicating
                // the monitor system publisher is online
                carma3gBandSubsystem->online().setValue(true);
                carma3gBandSubsystem->online().setValidity(MonitorPoint::VALID);

                mon.push_back(carma3gBandSubsystem);
                bmm.insert(make_pair<unsigned short, unsigned short>(astrobandNo, i));
            }

            runHardwareMonitorServer(mon, ccc, hwType, controlHost, controlPort, portOffset, bmm);

            // If runMonitorServer returns, then the monitor system
            // writer is no longer online
            for (unsigned i=0; i < numbands; ++i ) {
                mon.at(i)->online().setValue(false);
                mon.at(i)->online().setValidity(MonitorPoint::VALID);
            }
        }
        break;
   }
}
