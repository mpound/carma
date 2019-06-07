/** @file
 * Declaration of carma::canbus::GpioDio class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2012/08/03 22:59:21 $
 * $Id: GpioDio.h,v 1.2 2012/08/03 22:59:21 abeard Exp $
 */

#ifndef CARMA_CANBUS_GPIODIO_H
#define CARMA_CANBUS_GPIODIO_H

#include "carma/canbus/Dio.h"

#include <boost/thread.hpp>
#include <vector>

namespace carma {
namespace canbus {

    /**
     * GpioDio class for use with gpio framework.
     */
    class GpioDio : public Dio {
    public:

        /**
         * Constructor for emulation.
         */
        GpioDio( );

        /**
         * Constructor
         * @param baseGpioPins Base gpio pin numbers, separate by atleast 8.
         * @param resetOnStart will write a resetHi to Dio immediately
         * following startup.  This is  useful for placing devices in
         * a known state until the user is ready to start them by reasserting
         * the line low.
         */
        GpioDio( std::vector< int > basePins, bool resetOnStart );

        /**
         * Destructor
         */
        ~GpioDio( );

        /**
         * Assert the 'power' line.
         * @throws carma::canbus::ErrorException on error.
         */	
        void powerOn();

        /**
         * Unassert the 'power' line.
         * @throws carma::canbus::ErrorException on error.
         */
        void powerOff();

        /**
         * Assert the reset line.
         * @throws carma::canbus::ErrorException on error.
         */
        void resetHi();

        /**
         * Unassert the 'reset' line.
         * @throws carma::canbus::ErrorException on error.
         */
        void resetLo();

        /**
         * Assert the 'reserved' line.
         * @throws carma::canbus::ErrorException on error.
         */
        void reservedHi();

        /**
         * Unassert the 'reserved' line.
         * @throws carma::canbus::ErrorException on error.
         */
        void reservedLo();

        /**
         * Clear (unassert) all lines.
         * @throws carma::canbus::ErrorException on error.
         */
        void clear();
        
    protected:

    private:
        
        void set( unsigned char bits );
        void unset( unsigned char bits );
        void writeHoldingLock( );

        struct GpioInfo {
            std::vector< int > fds; // File descriptors for all 8 pins
            int basePin;            // Base pin
        };
        
        unsigned char state_;
        std::vector< GpioInfo > gpioDevs_;
        boost::mutex mutex_;             // Coarse lock
        const bool emulate_;

    }; // class GpioDio

}} // namespace carma::canbus
#endif // #ifndef CARMA_CANBUS_GPIODIO_H
