/**@file
 * Downconverter Device class implementation for Carma CANbus API 130.
 * This class also serves as a base class to the spectral-line downconverter.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.34 $
 * $Date: 2012/01/31 00:22:03 $
 * $Id: Downconverter.h,v 1.34 2012/01/31 00:22:03 abeard Exp $
 */

#ifndef CARMA_DOWNCONVERTER_DOWNCONVERTER_H
#define CARMA_DOWNCONVERTER_DOWNCONVERTER_H

#include "carma/canbus/devices/XacDevice.h"
#include "carma/monitor/WbdcSubsystem.h"

#include <map>
#include <string>
#include <tao/Basic_Types.h>
#include <vector>

namespace carma {

/**
 * Contains all downconverter related code.
 */
namespace downconverter {


    /**
     * Downconverter Device class implementation.
     * This class implements the carma::canbus::Device base class for
     * Carma CAN API 130 describing the Wideband Downconverter CAN device.
     */
	class Downconverter : public carma::canbus::devices::XacDevice { 
	public:

        /**
         * Constructor.
         * Creates a Downconverter device with the given node id.
         * @param node Node id of device.
         * @param io Reference to CanOutput class.
         */
		Downconverter(
                carma::canbus::nodeType node,
                carma::canbus::CanOutput &io,
                carma::monitor::WbdcSubsystem & wbdcSubsys );

        /**
         * Destructor
         */
		virtual ~Downconverter();

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
        void processMsg( carma::canbus::msgType mid,
                         carma::canbus::DataVector & data, 
                         bool sim );

        /**
         * Produce a simulated CAN message for a given msgType.
         * This routine creates a Message with simulated data for the
         * input message id.  The returned message is automatically
         * placed in the CAN message queue for retrieval and processing
         * by the Master class.  When this device is OFFLINE, it is called for
         * each message type returned from Downconverter::getSlowMonitors and
         * Downconverter::getHalfSecMonitors and thus tests the processMsg()
         * method and monitor system. Note that this routine is called by
         * the carma::canbus::Master base class automatically.
         * @param mid Message id of CAN message to simulate.
         * @return Simulated CAN message.
         */
        carma::canbus::Message simulateMsg(carma::canbus::msgType mid);

        /**
         * Helper routine to calculate a downconverter node id from
         * an input and band id.  Note that both input parameters are ids
         * not indices thus they both start from 1 (input 1 - 8, band 1 - 16).
         * @param input Input (antenna) id (1-8).
         * @param band Band id (1-16) - not the index.
         * @return node id of device.
         */
        static carma::canbus::nodeType calculateNodeId(short input, short band);

        /**
         * Staticly retrieve the API Id.
         * This is a helper routine and does the same thing as the getApi
         * member function with the exception that it can be called staticly.
         * @return api id of device.
         */
        static carma::canbus::apiType getApiId();

        /**
         * Update data on a frame time scale
         * This routine is called automatically by carma::canbus::Master
         * every half second.  It is used to set the state monitor point.
         */
        void updateFrameData();

        /**
         * Disable command logging.
         */
        void disableCommandLogging( );

        /**
         * Enable command logging.
         */
        void enableCommandLogging( );

        // CORBA control methods implemented from idl defined interface
        void setPsysPreset();
        void setPsys(::CORBA::Float psys);
        void setPsysAtten(::CORBA::Float atten);
        void setIfOutPreset();
        void setIfOut(::CORBA::Float ifout);
        void setIfOutAtten(::CORBA::Float atten);
        void enableRfInputAmp(::CORBA::Boolean enable);
        void enableIfOutAlc(::CORBA::Boolean enable);

        // For Engineering use only
        ::CORBA::Boolean
        checkIfOutputPower(::CORBA::Float power, ::CORBA::Float delta);

	private:

        // Copy and assignment not permitted for this class.
        Downconverter(const Downconverter &);
        Downconverter &operator=(const Downconverter &);


        // Methods to process individual CAN messages.  These routines
        // are called by processMsg upon receipt of any message addressed
        // to this device.
        void processBlankingFramePacket1( carma::canbus::DataVector & data );
        void processBlankingFramePacket2( carma::canbus::DataVector & data );
        void processBlankingFramePacket3( carma::canbus::DataVector & data ); 
        void processBlankingFramePacket4( carma::canbus::DataVector & data );
        void processBlankingFramePacket5( carma::canbus::DataVector & data );
        void processSystemMonitorPacket4( carma::canbus::DataVector & data );


        // Methods to produce simulated CAN messages.  These routines are
        // called by simulateMsg and then placed in the CAN message queue
        // where they will eventually be processed by processMsg above.
        carma::canbus::Message simBlankingFramePacket1();
        carma::canbus::Message simBlankingFramePacket2();
        carma::canbus::Message simBlankingFramePacket3();
        carma::canbus::Message simBlankingFramePacket4();
        carma::canbus::Message simBlankingFramePacket5();
        carma::canbus::Message simSystemMonitorPacket4();

		const unsigned int inputNo_;  // Input/antenna id/slot # (1..8)
		const unsigned int bandNo_;   // Band # (1..16)
        const std::string stringId_;
        carma::monitor::WbdcSubsystem::Input * mon_; // Ref to monitor system

        // State variables for use with engineering commands only.
        double ifOutputPower_;

        bool commandLoggingEnabled_;

	};
} // Namespace downconverter
} // Namespace carma
#endif
