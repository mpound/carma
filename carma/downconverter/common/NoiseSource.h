/**@file
 * Device class declaration for Noise Source CAN device (CAN API 97).
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.30 $
 * $Date: 2012/08/28 21:43:06 $
 * $Id: NoiseSource.h,v 1.30 2012/08/28 21:43:06 abeard Exp $
 *
 */
#ifndef CARMA_DOWNCONVERTER_NOISESOURCE_H
#define CARMA_DOWNCONVERTER_NOISESOURCE_H

#include "carma/canbus/devices/XacDevice.h"
#include "carma/util/PthreadMutex.h"

#include <map>
#include <string>
#include <tao/Basic_Types.h>
#include <vector>

namespace carma {

namespace monitor {
    class NoiseSource;
    class NoiseSourceContainer;
    class StateMonitorPointEnum;
    class Xac;
}

namespace downconverter {

    /**
     * Noise Source Device Class.
     * This class implements the carma::canbus::Device base class for
     * <A HREF=http://www.mmarray.org/project/system/CanAPI/Docs/API_097_CorrelatedNoiseSourceVerA.pdf>Carma CANbus API 97 </A>
     * describing the Noise Source CAN device.
     */
    class NoiseSource : public carma::canbus::devices::XacDevice {
    public:

        /**
         * Constructor 
         * Creates a Noise Source device with the given node id.
         * @param node Node id of device.
         * @param io Reference to CanOutput class.
         */
        NoiseSource(
            carma::canbus::nodeType node,
            carma::canbus::CanOutput &io,
            carma::monitor::NoiseSourceContainer & nsMon );


        /**
         * Destructor
         */
        virtual ~NoiseSource();

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
         * Return a map of this devices slow monitor points.
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
         * This routine creates a Message with simulated data for an
         * input message id.  The returned message is automatically
         * placed in the CAN message queue for retrieval and processing
         * by the Master class.  When this device is OFFLINE, it is called for
         * each message type returned from NoiseSource::getSlowMonitors and
         * NoiseSource::getHalfSecMonitors and thus tests the processMsg()
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
         * Update data on a frame time scale
         * This routine is called automatically by carma::canbus::Master
         * every half second.  It is used to set the state monitor point.
         */
        void updateFrameData();

    private:

        // Prohibit copy and assignment.
        NoiseSource(const NoiseSource &);
        NoiseSource &operator=(const NoiseSource &);

    public:

        void setNoiseOutputToPreset();
        void setNoiseOutput(CORBA::Double pout);
        void setNoiseAttenuation(CORBA::UShort atten);
        void enableNoiseSource(CORBA::Boolean enable);
        void setToneOutputToPreset( );
        void setToneOutput( CORBA::Double powerIndBm );
        void setToneAttenuation( CORBA::UShort atten);
        void enableToneSource(CORBA::Boolean enable);
        bool isEnabled();

    private:

        // Methods to process individual CAN messages.  These routines
        // are called by processMsg.
        void processBlankingFramePacket1( carma::canbus::DataVector & data );
        void processBlankingFramePacket2( carma::canbus::DataVector & data );
        void processBlankingFramePacket3( carma::canbus::DataVector & data );

        // Methods to produce simulated CAN messages.  These routines are
        // called by simulateMsg.  The returned Message is then placed in
        // the CAN message queue where they will eventually be processed
        // by processMsg above.  See carma::canbus::Master for more info.
        carma::canbus::Message simBlankingFramePacket1();
        carma::canbus::Message simBlankingFramePacket2();
        carma::canbus::Message simBlankingFramePacket3();

        carma::monitor::StateMonitorPointEnum * state_;
        carma::monitor::NoiseSource * mon_;
        carma::monitor::Xac * xacMon_;

        // Shared status variables
        struct Shared {

            Shared( );

            bool noiseEnabled;
            bool simNoiseEnabled;
            carma::util::PthreadMutex mutex;

        } shared_;

    }; // End class NoiseSource
} // End namespace downconverter
} // End namespace carma
#endif
