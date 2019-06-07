
/**
 * @file 
 * Representation of Planet
 *
 * @author Marc Pound
 * @version $Revision: 1.6 $ $Date: 2013/07/16 19:08:20 $
 * $CarmaCopyright$
 */

#ifndef CARMA_SERVICES_PLANET_H
#define CARMA_SERVICES_PLANET_H

#include "carma/services/Astro.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/PlanetTemperature.h"
#include "carma/services/Types.h"
#include "carma/util/Time.h"
#include <memory>
#include <string>
#include <vector>

namespace carma  {
  namespace services {

      class Angle;
      class Distance;
      class Frequency;
      class FluxDensity;
      class Length;
      class Location;
      class Temperature;
      class Velocity;

/**
 * Proposed planet class.  Note brightnessTemperature() and
 * axis methods will differ from planet to planet.  So we
 * would need a way to handle this.  Axis aspect ratios are easy 
 * since those are constant (if we ignore Saturn's rings).
 * Temperature models are trickier. Could be static arrays
 * or stored in external Tables.
 * Note MIRIAD and BIMA have differing temperature models. These need to
 * be reconciled, pick the best of each.
 */
  class Planet {
    public:

	/**
	 * Construct a Planet given the name.
	 * @param planetName  case insensitive solar system body name.
	 * @throws SourceNotFoundException if the planet name is
	 * not recognized. 
	 */
	explicit Planet( const std::string& planetName );

	/** Destructor */
	virtual ~Planet();

	const std::string getName() const;

	/**
	 * Set the Modified Julian Date to use in calculations.  
	 * If this method has not been called, all other calculation 
	 * methods will throw an EphemerisException because the
	 * ephemeris mjd has not been set.
	 *
	 * @param mjd The MJD, defaults to current time.
	 */
	void setMJD( double mjd = carma::util::Time::MJD() );

	/**
	 * Set the location to use in calculations.  The Planet constructor
	 * defaults this to CARMA's reference position.
	 */
	void setLocation( const Location& location );

	/**
	 * @return The distance to the planet from the Earth (geocentric) , at the set MJD
	 * @throw EphemerisException if mjd is not set.
	 * @see setMJD(double)
	 */
	const Distance earthDistance() const;
	
	/**
	 * @return the average distance to the planet from the Sun
	 */
	const Distance avgSunDistance() const;

	/**
	 * @return The current distance to the planet from the Sun.
	Distance sunDistance() const;
	 */

	/**
	 * @return The apparent planetary major axis size, at the set MJD
	 * @throw EphemerisException if mjd is not set.
	 * @see setMJD(double)
	 */
	const Angle majorAxis() const;

	/**
	 * @return The apparent planetary minor axis size, at the set MJD.
	 * @throw EphemerisException if mjd is not set.
	 * @see setMJD(double)
	 */
	const Angle minorAxis() const;

	/**
	 * @return The angle of the planetary polar axis with respect 
	 * to equatorial coordinate system.  This is calculated
	 * for the apparent RA, DEC of date of the planetary north pole.
	 * 
	 * @see <i>Report of the IAU/IAG
	 * Working Group on Cartographic Coordinates and
	 * Rotational Elements of the Planets and Satellites:2000</i>,
	 * P. K. Seidelmann et al., 2002, 
	 * Celestial Mechanics & Dynamical Astronomy, <b>82</b>, 83.
	 *
	 * @throw EphemerisException if mjd is not set.
	 */
	const Angle axisAngle() const;

	/**
	 * @return the angle of the planetary polar axis with respect
	 * to the sky plane. 
	 *
	 * @throw EphemerisException if mjd is not set.
	 */

	const Angle tiltAngle() const;

	/**
	 * @return The brightness temperature at the given frequency.
	 * If a table exists in conf/data of brightness temperature as a 
	 * function of time and frequency, it will be interpolated
	 * at the input frequency and MJD set via setMJD.  Otherwise
	 * a time-invariant value that is only a power-law function of 
	 * frequency will be returned.
	 *
	 * @param frequency The frequency at which to calculate the
	 * brightness temperature.
	 */
	const Temperature brightnessTemperature(const Frequency& frequency);

	/**
	 * @return The flux density at the given frequency.
	 * @param frequency The frequency at which to calculate the
	 *   flux density
	 */
	const FluxDensity flux(const Frequency& frequency);

	/**
	 * @return The flux density at the given frequency and visibility point.
	 * @param frequency The frequency at which to calculate the
	 *   flux density
	 * @param u The u length of the visibility, e.g. the u-distance between two antennas
	 * @param v The v length of the visibility, e.g. the v-distance between two antennas
	 */
	FluxDensity uvFlux(const Frequency& frequency, const Length& u, const Length& v);

	/**
	 * @return A vector of flux density at the associated frequency and visibility point.
	 * @param frequency The frequency at which to calculate the flux densities
	 * @param u The u length of the visibility, e.g. the u-distance between two antennas
	 * @param v The v length of the visibility, e.g. the v-distance between two antennas
	 */
	std::vector<FluxDensity*> uvFluxes(const Frequency& frequency, 
				         const std::vector<Length*>& u,
				         const std::vector<Length*>& v);

	/**
	 * @return the velocity with respect to location
	 * @throw EphemerisException if mjd is not set.
	 */
	const Velocity velocity(velocityFrameType frameType);


    const Table getTable() const {return planetTemperature_->getTable();}

    private:
	/** Struct that holds some constant planet data */
	const carma::services::constants::Astro::planetType pType_;

	// the planet name.  store here b/c I don't want to
	// call Ephemeris::getSource().getName() in a catch block.
	std::string name_;

	std::auto_ptr<carma::services::PlanetTemperature> planetTemperature_;

	/** used for calculations, defaults to CARMA reference position */
	// This is mutable so that its internal data may change even in
	// const methods of Planet.  I.e. we want to call non-const methods
	// of Ephemeris inside const methods of Planet.
	mutable Ephemeris ephemeris_;


	const Temperature powerLawTb(const Frequency& frequency) const;

	/**
	 * helper function for axisAngle and tiltAngle.
	 * true: (axisAngle) the positionangle of the polar axis
	 *       Positive is counter clock wise
	 * false (tiltAngle  the angle out of the sky, inclination for 
	 *                   you galaxian types.
	 *       Positive is pointing towards the observer.
	 */

	const Angle planetAngle(bool positionAngle) const;

    };

  }
}

#endif //CARMA_SERVICES_PLANET_H
