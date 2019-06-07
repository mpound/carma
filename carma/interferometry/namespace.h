
#ifndef CARMA_INTERFEROMETRY_NAMESPACE_H 
#define CARMA_INTERFEROMETRY_NAMESPACE_H

/**
 * @file
 *
 * The documentation for the carma interferometry namespace.
 * This file is documentation only and has no other use.
 *
 * @author: Marc Pound
 *
 * $Id: namespace.h,v 1.3 2004/08/04 18:03:29 scott Exp $
 * $CarmaCopyright$
 *
 */

namespace carma {

    /**
     * The interferometry subsystem calculates delay values
     * for all antennas based on station location, weather,
     * pointing and phase centers.
     *
     * The relevant classes containing public interfaces of 
     * interest to client programs:
     * <UL>
     * <LI> <b>DelayEngineControl</b> is the distributed object 
     * interface. This the what the SubArrayTracker will talk to.
     * </LI>
     *
     * <LI> <b>DelayEngine</b> is the Program class to start
     * the Delay Engine server.
     * </LI>
     * </UL>
     *
     * @see http://www.mmarray.org/project/WP/Interferometry
     *
     */

    namespace interferometry {

} }  // End namespace carma::interferometry


#endif  // CARMA_INTERFEROMETRY_NAMESPACE_H









