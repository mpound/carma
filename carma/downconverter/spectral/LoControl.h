/**
 * @file
 * Spectral Line Downconverter LO Control declaration (API #200).
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard </dl>
 * $Revision: 1.7 $
 * $Date: 2011/01/03 18:48:21 $
 * $Id: LoControl.h,v 1.7 2011/01/03 18:48:21 iws Exp $
 */

#ifndef CARMA_DOWNCONVERTER_LOCONTROL_H
#define CARMA_DOWNCONVERTER_LOCONTROL_H

#include "carma/canbus/devices/XacDevice.h"

namespace log4cpp {
    class Category;
} // End namespace log4cpp

namespace carma {

  namespace monitor {
    class LoControl;
    class StateMonitorPointEnum;
    class Xac;
  }

  namespace downconverter {

    /**
     * Spectral Line Downconverter LO Control module class.
     * This class is responsible for setting "the LO frequency for 
     * each of the eight LOs in the spectral line downconverter
     * system". 
     */
    class LoControl : public carma::canbus::devices::XacDevice {
    public:

        /**
         * Constructor 
         * @param co Reference to CanOutput instance.
         * @param mon Reference to LoControl monitor system container.
         * @param xacmon Reference to Xac monitor system device.
         */
        explicit LoControl( 
            carma::canbus::CanOutput & co,
            carma::monitor::StateMonitorPointEnum & state,
            carma::monitor::LoControl & mon,
            carma::monitor::Xac & xacmon );

        /**
         * Destructor
         */
        ~LoControl( );

        /**
         * Retrieve a map of this devices half second monitor points.
         * @see carma::canbus::Device::getHalfSecMonitors
         */
        carma::canbus::MsgIdInfoMap getHalfSecMonitors() const;

        /**
         * Retrieve a map of this devices slow monitor points.
         * @see carma::canbus::Device::getSlowMonitors
         */
        carma::canbus::MsgIdInfoMap getSlowMonitors( ) const;

        /**
         * Process a CAN message.
         * @see carma::canbus::Device::processMsg
         */
        void processMsg( carma::canbus::msgType mid,
                         carma::canbus::DataVector & data,
                         bool sim );

        /**
         * Produce a simulated CAN message for the given message id.
         * @see carma::canbus::Device::simulateMsg
         */
        carma::canbus::Message simulateMsg( carma::canbus::msgType mid );

        /**
         * Retrieve API Id.
         */
        static carma::canbus::apiType getApiId( );
        
        /**
         * Update data on the half second ("frame") timescale.
         * @see carma::canbus::Device::updateFrameData
         */
        void updateFrameData( );

        /**
         * Set LO Frequency for the specified band.
         * @param bandNo Band we wish to tune.
         * @param loFrequency Desired lo frequency in GHz.
         * @throw InvalidArgumentException if band or frequency out of range.
         */
        void setLoFrequency( unsigned short bandNo,
                             double loFrequency ) const;

    protected:

        // Nothing is protected

    private:

        // Copy and assignment prohibited
        LoControl( const LoControl & );
        LoControl & operator=( const LoControl & );

        void processLoFreqAndStatMessage( carma::canbus::msgType mid,
                                          carma::canbus::DataVector & data );
        void processBlankingFramePacket9( carma::canbus::DataVector & data );
        void processBlankingFramePacket10( carma::canbus::DataVector & data );

        carma::canbus::Message simLoFreqAndStatMessage( 
            carma::canbus::msgType mid );
        carma::canbus::Message simBlankingFramePacket9( );
        carma::canbus::Message simBlankingFramePacket10( );

        log4cpp::Category & log_;
        carma::monitor::StateMonitorPointEnum & state_;
        carma::monitor::LoControl & mon_;
        carma::monitor::Xac & xacMon_;
        
    }; // End class LoControl
  } // End namespace downconverter
} // End namespace carma
#endif
