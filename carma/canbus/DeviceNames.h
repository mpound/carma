/** @file
 * Declaration of carma::canbus::DeviceNames class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.3 $
 * $Date: 2005/03/31 23:37:53 $
 * $Id: DeviceNames.h,v 1.3 2005/03/31 23:37:53 abeard Exp $
 */

#ifndef CARMA_CANBUS_DEVICENAMES_H
#define CARMA_CANBUS_DEVICENAMES_H

// STL Includes
#include <string>

// Carma include
#include "carma/canbus/Types.h"

namespace carma {
namespace canbus {

    /**
     * Completely static class to retrieve string names of 
     * CAN devices based on API Id.  This class cannot be 
     * instantiated.
     */
    class DeviceNames {
    public:

        /**
         * Check to see if a device has been registered with 
         * a specific api.
         */
        static bool isRegistered(carma::canbus::apiType api);
        
        /**
         * Get the Device name for an input API Id.
         * If a device name doesn't exist for an input API id,
         * the string "Unknown" is output.  Use DeviceNames::isRegistered
         * to check first.
         * @param api API Id of device.
         */
        static std::string getName(carma::canbus::apiType api);

    private:
        
        // Creation, assignment, copy construction, destruction all prevented.
        DeviceNames();
        ~DeviceNames();
        DeviceNames(const DeviceNames &);
        DeviceNames &operator=(const DeviceNames &);

    }; // End class DeviceNames

}} // End namespace carma::canbus
#endif
