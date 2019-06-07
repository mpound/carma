// $Id: Astro.h,v 1.14 2013/07/16 13:41:30 mpound Exp $

/**
 * @file carma/services/Astro.h
 * Astronomical Constants used across CARMA.  
 *
 * @author Marc Pound
 * @version $Revision: 1.14 $
 */

#ifndef CARMA_SERVICES_CONSTANTS_ASTRO_H
#define CARMA_SERVICES_CONSTANTS_ASTRO_H

#include <string>
#include <map>
namespace carma  {
  namespace services {
    namespace constants {

    /**
     * Astronomical Constants used across CARMA.
     * Units follow 
     * CARMA Standard Units where applicable, SI otherwise.
     *
     * Planetary data come from http://nineplanets.org and Hat Creek code.
     * They are not meant to supplant ephemeris values, but are simply 
     * for quick calculations that do not need high accuracy.
     *
     * @see http://www.mmarray.org/workinggroups/computing/CarmaStdUnits.html
     * @see http://nineplanets.org
     */
    class Astro {
    public:

        /**
         * Astronomical Unit
         * Units: m
         */
        static const double AU = 1.49597870691E11;

        /**
         * A struct to hold simple planetary data.
         * <i>These are not meant to supplant ephemeris
         * values, but are simply for quick calculations that
         * do not need high accuracy.</i>
         * Earth data is of most interest, but
         * it isn't crazy to store other planets,
         * too.  Structures of this type should
         * be declared static const.
         *
         * // this needs to become its own class for, e.g.
         * // planetary brightess & angular size calculations,
         * // brightness temperature as a function of frequency
         */
        typedef struct {

        /**
         * Lower case name
         */
        char name[32];

        /** 
         * Planetary radius
         * Unit: m
         */
        double radius;  

        /** 
         * Planetary mass
         * Unit: kg
         */
        double mass;    

        /**
         * Major/Minor axis aspect ratio (1 = spherical)
         */
        double aspectRatio;  

        /**
         * Average distance to Sun
         * Unit: AU
         */
        double avgDist; 

        /**
         * Planetary brightess temperature at ~ 100 GHz.
         * taken from BIMA's ephplanet.f
         * Unit: K
         */
        double brightnessTemp;

        /**
         * Power law index of temperature. 
         * taken from BIMA's ephplanet.f
         * NOTE $MIRSUBS/planet.for and ephplanet.f
         * need to be reconciled.  
         */
        double tempIndex;

        /**
         * Coordinates of the North pole of the planet
         * in J2000, as defined by the IAU.  See
         * <i>Report of the IAU/IAG
         * Working Group on Cartographic Coordinates and
         * Rotational Elements of the Planets and Satellites:2000</i>,
         * P. K. Seidelmann et al., 2002, 
         * Celestial Mechanics & Dynamical Astronomy, 
         * <b>82</b>, 83.
         *
         * poleRa  in J2000 at current time = poleRa2000  +  dRaDt * T
         * <br>
         * poleDec in J2000 at current time = poleDec2000 + dDecDt * T
         *
         * where T is elapsed Julian Centuries since 1.5 Jan  2000.
         * == JD 2451545.0 TDT
         */

        /**
         * Right ascension of planet pole in J2000 epoch, degrees.
         */
        double poleRa2000; // degrees

        /**
         * Declination of planet pole in J2000 epoch, degrees.
         */
        double poleDec2000; // degrees

        /** 
         * Value of pole Ra change with time,
         * (linear term)
         * degrees per Julian Century
         */
        double dRaDt;

        /** 
         * Value of pole Dec change with time,
         * (linear term)
         * degrees per Julian Century
         */
        double dDecDt;

            double W;   // degrees

            double dWDt;


        } planetType;


        /** Basic data for Sun */
        static const planetType SUN;

        /** Basic data for Mercury */
        static const planetType MERCURY;

        /** Basic data for Venus */
        static const planetType VENUS;

        /** Basic data for Earth */
        static const planetType EARTH;

        /** Basic data for Moon */
        static const planetType MOON;

        /** Basic data for Mars */
        static const planetType MARS;

        /** Basic data for Jupiter */
        static const planetType JUPITER;

        /** Basic data for Saturn */
        static const planetType SATURN;

        /** Basic data for Uranus */
        static const planetType URANUS;

        /** Basic data for Neptune */
        static const planetType NEPTUNE;

        /** Basic data for Pluto */
        static const planetType PLUTO;

        /**
         * @return planetType struct given planet name
         * @param planetName Planet name, case insensitive.
         */

        static planetType getPlanet(const std::string& planetName);

        /**
         * @return true if input planet name is a recognized planet,.
         * false otherwise
         * @param planetName Planet name, case insensitive.
         */
        static bool isPlanet(const std::string& planetName);

        /*
        static std::vector<Angle*> 
        angularSize(const planetType, const Distance& distance);
        */

        /** Destructor */
    // compiler will complain about private destructor with no friends
        ~Astro();
        
    private: /** no subclassing allowed. */
        /** No-arg Constructor */
        Astro();

      static void initPlanets(void);
      static std::map<std::string, planetType>* planets_;
          static const unsigned int NUM_PLANETS = 11;
      static const std::string PLANETNAME_[NUM_PLANETS];
      static const planetType PLANETDATA_[NUM_PLANETS];


    };
    }
  }
}

#endif //CARMA_SERVICES_CONSTANTS_ASTRO_H
