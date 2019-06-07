/** @file
 * Declaration of carma::canbus::Dio class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2014/09/08 18:02:09 $
 * $Id: JanzDio.h,v 1.2 2014/09/08 18:02:09 iws Exp $
 */

#ifndef CARMA_CANBUS_JANZDIO_H
#define CARMA_CANBUS_JANZDIO_H

#include "carma/canbus/Dio.h"

#include <boost/thread/mutex.hpp>
#include <string>
#include <vector>

namespace carma {
namespace canbus {

    /**
     * Dio class for use with Janz char driver.
     * Dio provides digital input and output control for Janz Dio cards.
     * The Dio class is tailored for use with Janz CAN cards and the
     * CARMA RJ45 breakout board all contained on a Janz Compact PCI
     * carrier board.  As a result the methods available below represent
     * the uses of the dio pins in conjunction with those boards
     * rather than routines to write and read generic data to/from
     * the Dio card.
     */
    class JanzDio : public carma::canbus::Dio {
    public:

        /**
         * Default constructor for hardware emulation.
         * This constructor opens /dev/null as the 'Dio'
         * device.  It is included to allow writes to the Dio
         * class when Janz hardware does not exist on the target
         * system.
         * @throws carma::canbus::ErrorException if error opening /dev/null.
         */
        JanzDio( );

        /**
         * Single device/port contructor for Dio.
         * This constructor is for use with a single port on
         * the Janz TTL card.
         * @param dev Device name (e.g /dev/mttla_02 for port a
         * on a TTL card residing on modulbus 0 slot 2).
         * @param resetOnStart will write a resetHi to Dio immediately
         * following startup.  This is  useful for placing devices in
         * a known state until the user is ready to start them.
         * The user is responsible for reasserting the line low!
         * @throws carma::canbus::ErrorException on error.
         */
        JanzDio( const std::string & dev,
                 bool resetOnStart = false );

        /**
         * Multiple port constructor for Dio.
         * In practice this should be used to control different ports
         * on the same device although this constraint is not
         * necessary.
         * @param dev0 First device (e.g. /dev/mttla_02)
         * @param dev1 Second device (e.g. /dev/mttlb_02)
         * @param resetOnStart will write a resetHi to Dio immediately
         * following startup.  This is useful for placing devices
         * in a known state until the user is ready to start them.
         * The user is responsible for reasserting the line low!
         * @throws carma::canbus::ErrorException on error.
         */
        JanzDio( const std::string & dev0,
                 const std::string & dev1,
                 bool resetOnStart = false );

        /**
         * Constructor for an arbitrary number of Dio ports.
         * @param Vector of string device names.
         * @param resetOnStart will write a resetHi to Dio immediately
         * following startup.  This is useful for placing devices
         * in a known state until the user is ready to start them.
         * The user is responsible for reasserting the line low!
         * @throws carma::canbus::ErrorException on error.
         */
        JanzDio( const ::std::vector< ::std::string > & devs,
                 bool resetOnStart = false );

        /**
         * Destructor
         */
        ~JanzDio();

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

    private:

        struct DeviceInfo {
            int fd;			 	   // File descriptor.
            std::string name;	   // Name of ttl device.
            unsigned char state;   // Bit state.
        };

        // Common initialization
        void initializeDevice( const std::string & deviceName,
                               int flags,
                               bool resetOnStart );
        // Emulation hook.
        void mttlWrite(int fd, int data);

        // Twiddle bits.
        void set( int bit );
        void unset( int bit );

        // Member data
        std::vector< DeviceInfo > devs_; // List of our janz devices
        const bool emulate_;             // Emulate Janz Dio card?
        boost::mutex mutex_;             // Coarse lock

    }; // End class JanzDio
}}  // namespace carma::canbus
#endif // CARMA_CANBUS_JANZDIO_H
