/**@file
 * SpectralDownconverter Device class implementation (CARMA CANbus API 024).
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.20 $
 * $Date: 2011/05/11 17:35:23 $
 * $Id: SpectralDownconverter.h,v 1.20 2011/05/11 17:35:23 iws Exp $
 */

#ifndef CARMA_DOWNCONVERTER_SPECTRALDOWNCONVERTER_H
#define CARMA_DOWNCONVERTER_SPECTRALDOWNCONVERTER_H

// C++ Standard library includes
#include <map>
#include <string>
#include <vector>

// Carma includes
#include "carma/corba/corba.h"
#include "carma/canbus/devices/XacDevice.h"
#include "carma/canbus/Types.h"
#include "carma/downconverter/spectral/SpectralDownconverterControl.h"
#include "carma/monitor/SldcSubsystem.h"

namespace log4cpp {
    class Category;
}  // namespace log4cpp


namespace carma {
namespace downconverter {

    /**
     * SpectralDownconverter CAN Device class implementation.
     * This class implements the carma::canbus::Device base class for
     * Carma CAN API 024 describing the Spectral Downconverter CAN device.
     */
	class SpectralDownconverter : public carma::canbus::devices::XacDevice {
	public:

        /**
         * Constructor.
         * Creates a SpectralDownconverter device with the given node id.
         * @param node Node id of device.
         * @param io Reference to CanOutput class.
         * @param sldcMon Reference to sldc monitor subsystem.
         */
		SpectralDownconverter(
                carma::canbus::nodeType node,
                carma::canbus::CanOutput &co,
                carma::monitor::SldcSubsystem & sldcMon );

        /**
         * Destructor
         */
		virtual ~SpectralDownconverter();

        /**
         * Retrieve a map of this devices half second monitor points.
         * The monitor points returned from this routine will be
         * simulated if the device is in the OFFLINE state.
         * @return map of string description of half second monitors
         * keyed by msgType (message id).
         */
        std::map<carma::canbus::msgType, std::string>
            getHalfSecMonitors() const;

        /**
         * Retrieve a map of this devices slow monitor points.
         * These monitor points will be simulated every 5 seconds if
         * the device is in the OFFLINE state.
         * @return map of string description of slow (5 second) monitors
         * keyed by msgType (message id).
         */
        std::map<carma::canbus::msgType, std::string> getSlowMonitors() const;

        /**
         * Process a CAN message.
         * This routine is responsible for processing all CAN messages
         * addressed to this device.
         * @param mid the 10bit message id (carma::canbus::msgType)
         * @param data reference to the byte vector containing the raw data.
         */
        void processMsg(carma::canbus::msgType mid,
                std::vector<carma::canbus::byteType> &data, bool sim);

        /**
         * Produce a simulated CAN message for a given msgType.
         * This routine creates a Message with simulated data for the
         * input message id.  The returned message is automatically
         * placed in the CAN message queue for retrieval and processing
         * by the Master class.  When this device is OFFLINE, it is called for
         * each message type returned from
         * SpectralDownconverter::getSlowMonitors and
         * SpectralDownconverter::getHalfSecMonitors and thus tests the
         * processMsg()
         * method and monitor system. Note that this routine is called by
         * the carma::canbus::Master base class automatically.
         * @param mid Message id of CAN message to simulate.
         * @return Simulated CAN message.
         */
        carma::canbus::Message simulateMsg(carma::canbus::msgType mid);

        /**
         * Staticly retrieve the API Id.
         * This is a helper routine and does the same thing as the getApi
         * member function with the exception that it can be called staticly.
         * @return api id of device.
         */
        static carma::canbus::apiType getApiId();

        /**
         * Calculate node id based on the input and band id.
         * @param input Input (antenna) id (1-15) - not the index.
         * @param band Band id (1-8) - not the index.
         * @return node id of device.
         */
        static carma::canbus::nodeType calculateNodeId(
            unsigned short input,
            unsigned short band);

        /**
         * Update data on a frame time scale
         * This routine is called automatically by carma::canbus::Master
         * every half second.  It is used to set the state monitor point
         * among other things.
         */
        void updateFrameData();

        // Control commands

        /**
         * Set Psys power level to the preset value stored in the compensation
         * EEPROM.
         */
        void setPsysPreset() const;

        /**
         * Set Psys to requested power level.
         * Once this level is set, once this level is set, it is not adjusted
         * again until this message or a Set Psys to Preset Level command is
         * received.  The power in versus voltage out characteristics of the
         * Psys Log Detector are contained in the downconverter compensation
         * EEPROM.
         * @param psys Requested Psys level in dBm.
         */
        void setPsys(::CORBA::Float psys) const;

        /**
         * Set Psys attenuation level to the requested level.
         * Once this value is set, it is not adjusted again until this
         * message or a set Psys Level command is received.
         * @param atten Requested Psys attenuation level in dB.
         */
        void setPsysAtten(::CORBA::Float atten) const;

        /**
         * Set the IF Output Power level to the preset value stored in the
         * compensation EEPROM.  The downconverter adjusts the IFOUT VVA
         * voltage until the IF Output Log Detector voltage reaches the
         * required power level.  Once this level is set, it is not
         * adjusted again until this message or an IFOUT Level command
         * is received.  The hardware contains a closed loop system that
         * continuously keeps the IF Output Power level at this level without
         * any interaction from the microcontroller.  When this message is
         * sent, the hardware ALC is enabled.
         */
        void setIfOutPreset() const;

        /**
         * Set the IF Output power level to the requested level.
         * The downconverter adjusts the IF Output VVA voltage until the
         * IF Output Log Detector power level reaches the requested value.
         * Once this level is set, it is not adjusted again until this message i
         * or an IFOUT Level command is received.  The actual power in versus
         * voltage out characteristics of the IFOUT Log Detector are contained
         * in the downconverter Compensation EEPROM.  When this message is sent
         * the hardware ALC is enabled.
         * @param ifout Requested IF Output power level in dBm.
         */
        void setIfOut(::CORBA::Float ifout) const;

        /**
         * Set the downconverter IF Output Attenuation to the requested
         * value.  Once this value is set, it is not adjusted again until
         * this message or a Set IF Output to Requested Power Level command
         * is received (setIfOut).  When this message is sent, the
         * hardware ALC is disabled.
         * @param atten Requested IF Output attenuation value in dB.
         */
        void setIfOutAtten(::CORBA::Float atten) const;

        /**
         * Switch the RF Input Amplifier on or off.
         * @param enable True to enable, false to disable.
         */
        void enableRfInputAmp(::CORBA::Boolean enable) const;

        /**
         * Enable or disable the IF Output ALC (Automatic Level Control)
         * loop (normally on).
         * @param enable True to enable, falst to disable.
         */
        void enableIfOutAlc(::CORBA::Boolean enable) const;

        /**
         * Select which of the available output bandwidth defining filters
         * to use.  Different boards may have different filters available from
         * the OutputFilterType list.  However, if an unavailable filter
         * is requested, an error will be sent back in the monitor stream.
         * Upon reset, the module will use the same filter it used prior to
         * the reset. However, the INITREQ flag will be set since the high-level
         * system may have attempted to reset the filter.
         * @param filter Desired filter of OutputFilterType.
         */
        void selectOutputFilter(
            enum SpectralDownconverterControl::FilterType filter ) const;

        /**
         * Select which sideband the single sideband downconverter is to use.
         * Upon reset the module will use the same sideband it used prior to
         * reset but INITREQ will be set until this command is called again.
         * This is required since the control system may have changed the
         * sideband while the module was offline.
         * @param sideband Desired sideband.
         */
        void selectSideband(
            enum SpectralDownconverterControl::SidebandType sideband ) const;

        /**
         * Sends the 2nd LO frequency to this module which in turn uses it
         * to determine required I and Q modulator settings.
         * Again, upon reset the prior value is used but INITREQ is set in
         * case the control system changed this value while the module was
         * offline.
         * @param lofreq 2nd LO frequency in GHz.
         */
        void setLOFrequency( ::CORBA::Double lofreq ) const;

        /**
         * Manually set the differential voltage on the I channel of the 90
         * degree I/Q modulator to a particular value (used for testing).
         * @param I modulator voltage in mv.
         */
        void setIModulatorVoltage( ::CORBA::Short I ) const;

        /**
         * Manually set the differential voltage on the Q channel of the 90
         * degree I/Q modulator to a particular value.
         * @param Q modulator voltage in mv.
         */
        void setQModulatorVoltage( ::CORBA::Short Q ) const;

        /**
         * Is this module online?
         * @return True if online false if offline.
         */
        ::CORBA::Boolean isOnline();

        /**
         * Check that IF Output power is within a specified range.
         * This command is for engineering use only!
         * @param power center IF Output power in dBm
         * @param delta value around IF Output power in dBm
         * @return true if within boundaries false otherwise.
         * @throw carma::util::UserException if module is OFFLINE.
         */
        ::CORBA::Boolean checkIfOutputPower(
            ::CORBA::Float power,
            ::CORBA::Float delta);

        /**
         * Disable command logging.
         * Turn off command logging on commands which are called on a per-band
         * basis.  This allows the higher level wrapper to log once per
         * band rather than log for every call to every input for each band
         * thus reducing the log volume.
         */
        void disableCommandLogging();

        /**
         * Enable command logging.
         * Re-enable command logging for per-band commands.
         */
        void enableCommandLogging();

        /**
         * Return a string representation of the input filter.
         * @param filter Input filter.
         */
        static std::string filterAsString(
          const carma::downconverter::DownconverterControl::FilterType filter);

	private:

        // Copy and assignment not permitted for this class.
        SpectralDownconverter(const SpectralDownconverter &);
        SpectralDownconverter &operator=(const SpectralDownconverter &);

        // Methods to process individual CAN messages.  These routines
        // are called by processMsg upon receipt of any message addressed
        // to this device.
        void processBlankingFramePacket1(
            std::vector<carma::canbus::byteType> &data);
        void processBlankingFramePacket2(
            std::vector<carma::canbus::byteType> &data);
        void processBlankingFramePacket3(
            std::vector<carma::canbus::byteType> &data);
        void processBlankingFramePacket4(
            std::vector<carma::canbus::byteType> &data);
        void processBlankingFramePacket5(
            std::vector<carma::canbus::byteType> &data);
        void processBlankingFramePacket6(
            std::vector<carma::canbus::byteType> &data);

        // Methods to produce simulated CAN messages.  These routines are
        // called by simulateMsg and then placed in the CAN message queue
        // where they will eventually be processed by processMsg above.
        carma::canbus::Message simBlankingFramePacket1();
        carma::canbus::Message simBlankingFramePacket2();
        carma::canbus::Message simBlankingFramePacket3();
        carma::canbus::Message simBlankingFramePacket4();
        carma::canbus::Message simBlankingFramePacket5();
        carma::canbus::Message simBlankingFramePacket6();

        void setState( carma::canbus::deviceStateType state );

        // Helper routines
        ::std::string printInputAndBand() const;

		const unsigned short inputNo_;  // Input/antenna id/slot # (1..8)
		const unsigned short bandNo_;   // Band # (1..16)
        log4cpp::Category &log_; // Reference to the logger.
        carma::monitor::SldcSubsystem::Input * mon_; // Monitor system.
        double ifOutputPower_;

        unsigned int lastLogWarnFrame_;
        unsigned int offlinesSinceLastLog_;
        bool commandLoggingEnabled_;
	};
} // End namespace downconverter
} // End namespace carma
#endif
