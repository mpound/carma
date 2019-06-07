/**
 * @file carma/interferometry/DelayInfo.h
 *
 * Contains a self-consistent set of values for delay
 * parameters.  These values are computed from DelayEngine
 * input parameters, rather than just set to constant
 * values via mutators. Constant delays, like offsets, are
 * set directly in the DelayEngine API.
 * 
 * $Id: DelayInfo.h,v 1.26 2013/05/22 23:08:38 scott Exp $
 * @author Marc Pound
 *
 * $CarmaCopyright:$
 */

#ifndef CARMA_INTERFEROMETRY_DELAYINFO_H
#define CARMA_INTERFEROMETRY_DELAYINFO_H

#include <vector>
#include "carma/monitor/Subarray.h"

namespace carma {
  namespace interferometry {

 /**
  * <p>
  * This class keeps a self-consistent values for delay
  * parameters.  It contains parameters related to antenna
  * locations, source coordinates, weather information,
  * delay values, and delay usage indicators (turn on/off
  * certain delays).
  * The boolean arrays <code>use...</code> indicate whether or
  * not the Delay Engine should include certain
  * delays in the total delay calculation (per antenna).
  * The constructor will set all these to true.
  * </p><p>
  * The Delay Engine can safely use one of
  * an instance of this class to calculate delays.
  * </p>
  */
    class DelayInfo
    {
    public: 
        /** 
         * no-arg constructor 
         */
         //@param nAntennas The number of antennas this object represents 
        DelayInfo();

        /** destructor */
        virtual ~DelayInfo();


        /** identifying name for this object */
        std::string name;


        /** 
         * Following are the parameters related to antenna locations
         * (X,Y,Z,latitude,longitude, axisMis).
         * The constructor will set all these values to zero.
         */

        // Why std::vectors instead of arrays? don't have to know
        // size at compile time, even though we do (23=max_ant).
        // Also range checking done for you.
        
        /** 
         * sourcename for which delays are being calculated
         */
        std::vector<std::string> source;
        
        /** Antenna X ground position, meters */
        std::vector<double> X;

        /** Antenna Y ground position, meters */
        std::vector<double> Y;

        /** Antenna Z ground position, meters */
        std::vector<double> Z;

        /** Source U position in equinox of date (not J2000), meters */
        std::vector<double> U;

        /** Source V position in equinox of date (not J2000), meters */
        std::vector<double> V;

        /** Soruce W position in equinox of date (not J2000), meters */
        std::vector<double> W;

        //std::vector<AntennaCoordinates> antCoords;

        /** Antenna station geocentric latitude, radians */
        std::vector<double> latitude; 

        /** Antenna station geocentric longitude, radians */
        std::vector<double> longitude; 

        /** Antenna station altitude, meters */
        std::vector<double> altitude; 

        /** Antenna axis misalignment, meters*/
        std::vector<double> axisMis;  

        /** 
         * Following are the parameters of the source 
         * towards which the antenna is pointing
         * The constructor will set all these values to zero.
         */

        /** Pointing center right ascension, radians */
        std::vector<double> pntRa;     

        /** Pointing center declination, radians*/
        std::vector<double> pntDec;    

        /** Phase center right ascension, radians */
        std::vector<double> phsRa;     

        /** Phase center declination, radians */
        std::vector<double> phsDec;   

        /** Pointing center azimuth, radians */
        std::vector<double> pntAz;     

        /** Pointing center elevation, radians */
        std::vector<double> pntEl;    

        /** Phase center azimuth, radians */
        std::vector<double> phsAz;     

        /** Phase center elevation, radians */
        std::vector<double> phsEl;    

        /** 
         * Used to identify whether an antenna
         * had its pointing position set as RA,Dec or
         * Az,El.
         */
        typedef enum pntStateEnum {
            RADEC, AZEL,
        } pntStateType;

        /** 
         * Pointing center state: AZEL or RADEC 
         */
        std::vector<pntStateType> pntState;    

        /**
         * Array containing axis misalignment delay 
         * for each antenna.
         */
        std::vector<double> axisDelay; 

        /**
         * Array containing geometric delay  (including axis 
         * misalignment delay) for each antenna.
         */
        std::vector<double> geometricDelay; 

        /**
         * Array containing additional refractive 
         * height delay for each antenna. Not this is
         * the difference between
         * this antenna and the array reference point.
         */
        std::vector<double> heightDelay; 

        /**
         * Array containing ionospheric delay
         * for each antenna.
         */
        std::vector<double> ionosphericDelay; 

        /**
         * Array containing tropospheric pathlength
         * for each antenna (meters)
         */
        std::vector<double> pathlength; 

        /**
         * Array containing zeinith refractivity
         * for each antenna (dimensionless)
         */
        std::vector<double> refractivity; 

        /**
         * Array containing thermal delay
         * for each antenna.
         */
        std::vector<double> thermalDelay; 

        /** 
         * Array containing the total calculated fixed delay for each antenna
         * The constructor will set this to zero.
         */
        std::vector<double> totalFixedDelay; 
        
        /** 
         * Array containing the total calculated delay for each antenna
         * The constructor will set this to zero.
         */
        std::vector<double> totalDelay; 

        /** 
         * Total calculated delay per antenna for polarization state 1
         */
        std::vector<double> totalDelayPol1; 

        /** 
         * Total calculated delay per antenna for polarization state 2
         */
        std::vector<double> totalDelayPol2; 

        /**
         * Array containing tropospheric delay
         * for each antenna.
         */
        std::vector<double> troposphericDelay; 

        /** 
         * The time when the delays were calculated, MJD units. 
         * This is NOT the time for which they were calculated.
         * @todo mpml must change to vecto, too.
         */
        std::vector<double> calculatedAt; 

        /** 
         * The time when the delays validity expires, MJD units. 
         * @todo mpml must change to vecto, too.
         */
        std::vector<double> validUntil;
        
        /** 
         * Time for which these delays were calculated, MJD units
         * @see carma::util::Time
         * This vector allows
         * each antenna to have a different timestamp.
         */
        std::vector<double> timestamp;

        /** 
         * Air temperature, Kelvin. The constructor will
         * set to Atmosphere.DEFAULT_AIR_TEMP
         * @see carma::environment::Atmosphere
         */
        double airTemp;

        /** 
         * Atmospheric pressure, mbar.  The constructor will
         * set to Atmosphere.DEFAULT_ATM_PRESSURE
         * @see carma::environment::Atmosphere
         */
        double atmPressure;

        /**
         * Relative humidity, percent.   The constructor will
         * set to Atmosphere.DEFAULT_RH;
         * @see carma::environment::Atmosphere
         */
        double relHumid;

        /**
         * Zero out this object (all antennas). Set 
         * discontinuity parameters to true, reset all member variables,
         * set all vector values to zero.
         */
        void reset();

        /**
         * Zero the values for the given antenna, set its 
         * discontinuity parameter to true.  Member variables (e.g.
         * weather) are untouched since they are the same for all
         * antennas.
         * @param antid Antenna identification 
         */
        void reset(unsigned short antid);

        /**
         * Copy the values from one DelayInfo to this one.
         * @param antid Antenna identification, the antenna to copy
         * @param from the DelayInfo object from which to copy.
         */
        void copy(unsigned short antid, DelayInfo& from);

        /**
         * Number of antennas
         */
        static unsigned short numAntennas() {
        
        return static_cast<unsigned short>(
            carma::monitor::Subarray::totalNumAntennas
            );
        }

    private:

        /** 
         * Reset all non-vector member variables to an initial state
         */
        void initializeMembers();

        /** 
         * Set all vector values to zero.
         */
        void initializeVectors();
    
    };

  } // namespace interferometry
} // namespace carma


#endif //CARMA_INTERFEROMETRY_DELAYINFO_H
