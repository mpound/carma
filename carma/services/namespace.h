
#ifndef CARMA_SERVICES_NAMESPACE_H 
#define CARMA_SERVICES_NAMESPACE_H

/**
 * @file
 *
 * The documentation for the carma services namespace.
 * This file is documentation only and has no other use.
 *
 * @author: Marc Pound
 *
 * $Id: namespace.h,v 1.4 2005/08/17 01:22:21 mpound Exp $
 * $CarmaCopyright$
 *
 */

namespace carma {

    /**
     *
     * Auxiliary Services is intended to provide routines commonly needed
     * for array control and observational planning.  These routines are
     * mostly, but not exclusively, astronomical in nature.  Auxiliary 
     * Services contains a mixture of new code, reused code, and
     * wrappers for external libraries. Briefly, these services are
     * <ul>
     * <li>Astronomical Timekeeping</li>
     * <li>Conformable Quantities and Unit Conversion</li>
     * <li>Ephemerides and Planetary Brightness</li>
     * <li>Source Coordinates and Velocities</li>
     * <li>Tables and Catalog I/O</li>
     * <li>Antenna positions</li>
     * </ul>
     */

    namespace services { 
	/** 
	 * Standard physical and astronomical constants.
	 */
         namespace constants { }
    } 

}  


#endif  // CARMA_SERVICES_NAMESPACE_H









