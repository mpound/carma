#ifndef CARMA_SERVICES_PAD_UTILS_H
#define CARMA_SERVICES_PAD_UTILS_H

#include <vector>
#include <string>

namespace carma {
namespace services {
    class Pad;
    class Location;
    class Length;

    /**
     * Compute the new location of a Pad plus offsets.
     * @param pad The Pad from which to compute the new location.
     * @param east The east coordinate, relative to the Pad location.
     * @param up   The up coordinate, relative to the Pad location.
     * @param north The north coordinate, relative to the Pad location.
     * @return The Location of the input pad plus offsets.
     * @see adjustedLocation(Location, Length, Length, Length);
     */
    Location adjustedLocation(Pad pad, Length east, Length north, Length up);

    /**
     * Compute the new location of a Location plus offsets.
     * @pararm loc The Location from which to compute the new location.
     * @param east The east coordinate, relative to the Pad location.
     * @param up   The up coordinate, relative to the Pad location.
     * @param north The north coordinate, relative to the Pad location.
     * @return The Location equivalent to the input location plus offsets.
     */
    Location adjustedLocation(Location loc, 
	                      Length east, Length north, Length up);

    /**
     * Take an X, Y, Z topographic position and convert it to
     * ENU offsets, given a Pad location.  <b>The reference position
     * for the array (i.e. for X, Y and Z) is assumed to be that
     * returned by pad.getReference().</b>
     * @pararm pad The Pad from which to compute the new location.
     * @param X The X coordinate, relative to the Pad reference position.
     * @param Y The Y coordinate, relative to the Pad reference position.
     * @param Z The Z coordinate, relative to the Pad reference position.
     * @return A vector containing ENU offsets relative to the input pad
     * location, which is returned from pad.getLocation().
     * @see ArrayCoordinates for description of various antenna coordinate 
     * systems.
     */
    std::vector<Length*> convertBaseline(Pad pad, Length X, Length Y, Length Z);

    /**
     * @ return the default pad name for this CARMA pad.  e.g. 
     *   "pad#22" for padNo = 22.
     */
    std::string defaultPadName(const unsigned short padNo);
}
}
#endif // CARMA_SERVICES_PAD_UTILS_H
