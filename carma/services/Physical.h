// $Id: Physical.h,v 1.7 2006/01/20 16:03:22 mpound Exp $

/**
 * @file carma/services/Physical.h
 * Standard physical constants used across CARMA. 
 *
 * @author Marc Pound
 * @version $Revision: 1.7 $
 * @todo (someday) Change this to use the Units class which has all this
 * stuff defined.
 */

#ifndef CARMA_SERVICES_CONSTANTS_PHYSICAL_H
#define CARMA_SERVICES_CONSTANTS_PHYSICAL_H

namespace carma  {
  namespace services {
    namespace constants {

    /** 
     * Standard physical constants CARMA-wide. Values are taken from 
     * the National Institute of Standards. Units follow 
     * CARMA Standard Units where applicable, SI otherwise.
     *
     * @see http://physics.nist.gov
     * @see http://www.mmarray.org/workinggroups/computing/CarmaStdUnits.html
     *
     */
	class Physical {
	public:
	    /**
	     * Speed of light 
	     * Units: m/s
	     */
	    static const double C = 299792458.0;

	    /**
	     * Newton's gravitational extern constant
	     * Units: m^3 kg^-1 s^-1
	     */
	    static const double G = 6.6742E-11;

	    /**
	     * Boltzmann's extern constant
	     * Units: Joule/K
	     */
	    static const double K = 1.3806505E-23;

	    /**
	     * Planck's constant
	     * Units: J*s
	     */
	    static const double H = 6.6260693E-34;

	    /**
	     * Absolute zero.
	     * Units: degrees Celsius
	     */
	    static const double ABS_ZERO = -273.15;
	
	    /**
	     * Individual gas constant for water
	     * Units: Joule/(kg*K)
	     */
	    static const double R_WATER = 461.5;

	    /**
	     * conversion of nanoseconds of delay to 
	     * meters of delay
	     */
	    static const double NANOSEC_PER_METER = 3.33564095198;


	// destructor is public because compiler will complain about 
	// private destructor with no friends
	    ~Physical();

	private: /* no subclassing allowed */
	    Physical();
	};
    }
  }
}

#endif //CARMA_SERVICES_CONSTANTS_PHYSICAL_H
