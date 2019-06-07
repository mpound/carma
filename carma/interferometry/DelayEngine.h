/**
 * @file 
 *
 * The Delay Engine
 *
 * $Id: DelayEngine.h,v 1.22 2013/10/01 23:27:45 scott Exp $
 *
 */

#ifndef CARMA_INTERFEROMETRY_DELAYENGINE_H
#define CARMA_INTERFEROMETRY_DELAYENGINE_H

#include "carma/services/Atmosphere.h"
#include "carma/interferometry/DelayInfo.h"
#include "carma/interferometry/DelayStatus.h"
#include "carma/monitor/DelayEngineSubsystem.h"
#include "carma/services/AntennaCoordinates.h"
#include "carma/services/AstroTime.h"
#include "carma/services/Ephemeris.h"
#include "carma/services/Frequency.h"
#include "carma/services/Types.h"

#include <vector>


namespace carma {

  namespace services {
      // forward declarations
      class Length;
      class Location;
      class Pressure;
      class Temperature;
  }

  namespace interferometry {

    typedef ::std::vector< const carma::monitor::DelayEngineSubsystem * >
             DelayFrameVec;
/**
**
 ** %DelayEngine interface.  
 ** <p>
 ** Interferometry information is generated as delays
 ** for altering the total path length (in terms of nanoseconds) of the
 ** light wave front from a source.  The delays are determined by
 ** information given to the Delay Engine from the Sub-Array Tracker.
 ** </p><p>
 ** Delays are then computed for wave front altering parameters such as
 ** atmospheric refraction (including ionospheric and tropospheric
 ** effects), thermal displacement of antenna heights and geometric
 ** corrections based the positions of the antennas on the ground.
 ** </p><p>
 ** The time-tagged, antenna-based delay values are passed to 
 ** LobeRotator and Correlator subsystems every 20 seconds (the rate 
 ** is determined by the SAT). When
 ** discontinuity occurs, due to source or frequency change, three
 ** sets of delays are sent in rapid succession.
 ** </p><p>
 ** The timing of this delay loop is not critical, as long as
 ** values are published approximately every 20 seconds, the
 ** client subsystem interpolators will function accurately.
 ** </p><p>
 ** A typical invocation is as follows:<br><br>
 ** <code>
    using namespace carma::interferometry;<br>
    using namespace carma::util;          <br>
    <br>
    DelayEngine delayEngine <br>
    delayEngine.setArrayReferencePoint( location );<br>
    delayEngine.setWeather( temperature, pressure, relhumid );<br>
    delayEngine.setAntennaLOFreq( antNumber, frequency )
    delayEngine.setAntennaCoordinates( antNumber, x, y, z,    <br>
                                      acType, axisMis );     <br>
    delayEngine.setDelayOffset( antNumber, offset );         <br>
    delayEngine.setAntennaRaDec( antNumber, mjd, pntRa, pntDec, <br>
                                phsRa, phsDec ); <br>
    delayEngine.setDelayOffset( offset, antNumber );
    delayEngine.computeDelays();                           <br>
   </code>
   <br>
 ** Note setArrayReferencePoint and setAntennaLOFreq <b>must</b>
 ** be called before setAntennaSource or setAntennaRaDec because
 ** the reference point and frequency are needed to computed the
 ** actual current sky position (refraction included). An exception
 ** will be thrown if the methods are called out of order.<p>
 ** For AZ/EL tracking the method setAntennaAzEl() is provided.
 ** </p>

 *
 * @see http://www.mmarray.org/project/WP/Interferometry
 * @author Marc Pound
 *
 * @Todo Check that OVRO and BIMA define axis misalignment D_a
 * using the same +/- convention.
 * @Todo change compute elevation to allow EL > 90. Have callers
 * modify the result if necessary, since elevation always used in 
 * trig function:
 * sin(EL) = sin(180-EL). 
 * cos(EL) = - cos(180-EL).  
  */

    class DelayEngine
    {

        public:
    /** constructor */
    DelayEngine();

    /** destructor */
    ~DelayEngine();

      /**
       * <p>Tell the DelayEngine the position for the given antenna,
       * either absolute or relative to the array center.  Axis misalignment 
       * is also stuck in here because we need it.  
       * </p><p>
       * The first three parameters specify the location of the antenna.
       * They may be in one of the three common coordinate systems:
       * Topocentric (X,Y,Z); (Longitude, Latitude, Altitude); or 
       * (Up, East, North).  The fourth parameter acType indicates which
       * coordinate system was entered.  You <b>must</b>
       * first specify the array reference position with a previous call to
       * setArrayReferencePoint().   If you do not, an exception is
       * is thrown because there is not enough info to calculate 
       * needed quantities.
       *
       * Antenna (X,Y,Z) positions are in the Equatorial coordinate system
       * defined in Thompson, Moran, and Swension, 1st edition, equation 4.15.
       * To wit: X in the plane defined by the terrestrial poles
       * and the reference point of the array, Y toward the east and Z
       * toward the North Celestial Pole. 
       * The XY plane is parallel to the earth's equator, Z is parallel
       * to the earth's spin axis.
       * 
       * Antenna (L,L,A) are The longitude/latitude/altitude coordinate 
       * system.  Longitude increases from 0 at Greenwich, England in an
       * easterly direction, and decreases in a westerly direction.
       * East longitudes are correctly described as positive 
       * numbers between 0 and 180 degrees.  West longitudes
       * are negative numbers between 0 and -180 degrees.
       *
       * Antenna (U, E, N): This Up, East, North coordinate system is 
       * useful for specifying
       * locations relative to a given LLA point.  Coordinates are in a
       * tangent plane at the specified LLA point, with N pointing due
       * north, E pointing due east, and U pointing straight up along a
       * radial vector from the center of the earth, and passing through
       * the given LLA point.  
       *
       * @param antennaNo The unique id for the antenna. Must be > 0 && < 23.
       * @param x Antenna X position in meters relative to array 
       *   reference point OR the Longitude in radians OR the Up in meters
       * @param y Antenna Y position in meters relative to array 
       *   reference point OR the Latitude in radians OR the East in meters
       * @param z Antenna Z position in meters relative to array
       *   reference point OR the Altitude in meters OR the North in meters
       * @param AntennaCoordinateType acType Specifies what you put in
       * for the first 3 parameters, one of: ANTCOORD_TOPO_XYZ, 
       * ANTCOORD_GEO_XYZ, ANTCOORD_LLA, ANTCOORD_UEN.
       * @param axisMis the axis misalignment value in meters
       * @return none.
       * @see services::AntennaCoordinateType
       *
       * @see carma::services::setArrayReferencePoint
       * @see setArrayReferencePoint
       * @throw carma::util::ErrorException
       */
      void setAntennaCoordinates( unsigned short antennaNo,
                                  double x, double y, double z,
                                  services::AntennaCoordinateType acType,
                                  double axisMis );
      /**
       * Set the antenna coordinates.
       * @param antennaNo The unique id for the antenna. Must be > 0 && < 23.
       * @param location The antenna coordinates as a Location
       * @param axisMis the axis misalignment value as a Length
       */
      void setAntennaCoordinates( unsigned short antennaNo,
              const services::Location & location, 
          const services::Length &   axisMis);
      /**
       * Set the axis misalignment value
       * @param antennaNo The unique id for the antenna. Must be > 0 && < 23.
       * @param axisMis the axis misalignment value as a Length
       */
      void setAxisMisalignment( unsigned short antennaNo,
                                const services::Length & axisMis);
      /**
       * <p>
       * Change the array reference point as
       * Antenna (Longitude, Latitude, Altitude) coordinates.  This
       * coordinate is also by definition (X=0, Y=0, Z=0).
       * Antenna coordinates in setAntennaCoordinates are with respect 
       * to the array reference point.  
       * <b>This method must be called prior to setAntennaCoordinates()</b>
       * or an exception will be thrown by that method.
       * </p><p>
       * A call to setArrayReferencePoint() will result in the discontinuity
       * flag being set to true. That is, this call will be treated like
       * a call to setAntennaCoordinates and a new triplet will be sent
       * automatically to clients on the next call to computeDelays.
       * </p><p>
       * <br>
       *
       * @param location the location object giving the geographic coordinates
       * of the reference point.
       * @see setAntennaCoordinates
       */
      void setArrayReferencePoint(const services::Location & location);

      /**
       * <p>
       * Another way of specifying source position.
       * This method tells the DelayEngine the 
       * position parameters required to determine delay
       * information for an antenna which is tracking an RA,Dec position.
       * Positions are FK5 for the current epoch, with any proper motion 
       * applied. Precession, nutation, polar motion, aberration, 
       * parallax, and any equatorial offsets have also been applied. 
       * Refraction has not been applied.
       * The topocentric RA and DEC in the current equinox of the source 
       * are used to compute the pointing
       * elevation of the antennas which is then also used in tropospheric
       * and geometric delay computations.
       * The Ephemeris class is used internally to convert RA,DEC to AZ,EL
       * coordinates.
       * </p>
       * <p>
       * Calling this method turns resets the delay computation for the
       * given antenna to use RA and DEC, rather than AZ and EL.
       * It also sets the TrackState monitor point for this antenna to 
       * the string "RADEC".
       * To change back, setAntennaAzEl must be called.
       * </p><p>
       * This method should be called about 
       * once per 20 seconds by the SubArray Tracker.
       * </p>
       *
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param mjd Modified Julian day number for input parameters
       * @param pntra The topocentric Pointing center RA in the current equinox
       *        for the source being observed, radians
       * @param pntdec The topocentric Pointing center Dec in the current 
       *        equinox for the source being observed, radians
       * @param phsra The topocentric Phase center RA in the current equinox
       *        for the source being observed, radians
       * @param phsdec The topocentric Phase center Dec in the current 
       *        equinox for the source being observed, radians
       * input parameter since the last call for this antenna
       * @param source The source name
       * @return none.
       */
      void setAntennaRaDec( unsigned short antennaNo,
                            double mjd,
                            double pntra, double pntdec,
                            double phsra, double phsdec,
                            bool logAction, const std::string& source
                           );

      /**
       * This is the primary method for telling the DelayEngine about
       * the source being tracked.
       * </p><p>
       * Separate pointing and phase centers are allowed; these are specified
       * as offsets to the nominal source position.  These offsets
       * are sky offsets not coordinate offsets, following the
       * same convention as SubarrayControl and Ephemeris. 
       * To convert an RA coordinate offset to a sky offset, 
       * multiply by cos(dec) The pointing offsets
       * naturally provide for mosaicking.
       * </p><p>
       * Calling this method turns resets the delay computation for the
       * given antenna to use RA and DEC, rather than AZ and EL.
       * It also sets the TrackState monitor point for this antenna to 
       * the string "RADEC".
       * To change back, setAntennaAzEl must be called.
       * </p><p>
       * This method should be called about 
       * once per 20 seconds by the SubArray Tracker.
       * </p>
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param name A string indicating the source name. The source
       * must be in the CARMA catalog or be a known solar system body.
       * @param mjd Modified Julian day number for input parameters
       * @param raPointingOffset the right ascension <b>pointing</b> 
       *        center offset from the source position, in radians. 
       * @param decPointingOffset the declination <b>pointing</b> 
       *        center offset from the source position, in radians
       * @param raPhaseOffset the right ascension <b>phase</b> 
       *        center offset from the source position, in radians
       * @param decPhaseOffset the declination <b>phase</b> 
       *        center offset from the source position, in radians
       * input parameter since the last call for this antenna.
       * @return none.
       * @throw the Ephemeris class will throw an exception if the source
       * is not in the CARMA catalog.
      void setAntennaSource( unsigned short antennaNo, 
                             const std::string& name,
                             double mjd,
                             double raPointingOffset,
                             double decPointingOffset,
                             double raPhaseCenterOffset,
                             double decPhaseCenterOffset
                           );

       */
      /**
       * <p>
       * Set the LO1 frequency, the multiplier and divisor to
       * be passed to the lobe rotator.  The frequency is also
       * used in the calculation of refraction effects.
       * </p><p>
       * <b>This method must be called by Control for delay computation
       * to succeed.  The LO1 frequency does not default.</b>
       * </p>
       *
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param LO1freq The LO1 frequency, represented by a Frequency object
       */
      void setAntennaLOFreq( unsigned short antennaNo, 
                             const services::Frequency & LO1freq);

      /**
       * Set the LO1 frequency for all antennas to the same value.
       * @param LO1freq The LO1 frequency, represented by a Frequency object
       */
      void setAllAntennaLOFreqs( const services::Frequency & LO1freq );
      
      /**
       * Tell the DelayEngine parameters required to determine delay
       * information for an antenna pointed to a fixed azimuth and
       * elevation.  This method should be used when looking at the
       * transmitter or when doing drift scans of celestial objects.
       *
       * The phase center AZ and EL are assumed to be identical to
       * the pointing center AZ and EL.
       *
       * The frequency is used to compute refraction effects.
       * The azimuth and elevation are used 
       * in tropospheric and geometric delay computations.
       * and delay computations.
       *
       * Calling this method turns resets the delay computation for the
       * given antenna to use AZ and EL , rather than RA and DEC.
       * It also sets the TrackState monitor point for this antenna to 
       * the string "AZEL".
       * To change back, setAntennaRaDec must be called.
       *
       * This method should be called about 
       * once per 30 seconds by the SubArray Tracker.
       *
       * No distance is given in this method call because we don't
       * do horizontal parallax correction for AZEL pointing mode.
       *
       * NOTE: pntaz is never used by the DelayEngine code, should
       * we eliminate it as a parameter or keep it for symmetry?
       *
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param mjd Modified Julian day number for input parameters
       * @param pntaz The pointing center azimuth for the antenna, radians
       * @param pntel The pointing center elevation for the antenna, radians
       * @param phsaz The phase center azimuth for the antenna, radians
       * @param phsel The phase center elevation for the antenna, radians
       * input parameter since the last call for this antenna
       * @return none.
       */
      void setAntennaAzEl( unsigned short antennaNo,
                           double mjd,
                           double pntaz, double pntel,
                           double phsaz, double phsel
                          );

      /**
       * Set the current weather parameters, needed to
       * calculated the tropospheric delay.  Note the air temperature
       * is in Celsius so that this method signature matches
       * other CARMA **weather(...) methods (e.g. DriveControl).
       *
       * @param airTemp The ambient air temperature in Celsius
       * @param atmPressure The atmospheric pressure in millibars
       * @param relHumid The relative humidity in percent.
       * @return none.
       */
      void setWeather( double airTemp,
                       double atmPressure,
                       double relHumid );

      /**
       * Set weather using conformable quantities.
       * @param airTemp The ambient air temperature.
       * @param atmPressure The atmospheric pressure.
       * @param relHumid The relative humidity in percent.
       */
      void setWeather( const services::Temperature & airTemp,
                       const services::Pressure & atmPressure,
                       double relHumid ) ;


      /**
       * Set the value for the antenna's delay offset.
       * This can be used to "tweaking up" the delays after,
       * adjustable delay, pad delay, and antennaDelay are set.
       * The delay offset is always used in calculating the total
       * delay; there is no bool "use" parameter to toggle it
       * on and off.
       * @param antennaNo The unique id for the antenna. Must be non-negative.
       * Zero means all antennas.
       * @param delay Delay offset in nanoseconds.
       * @return none.
       * @see Equation 23 of interferometry design document.
       */
      void setDelayOffset( unsigned short antennaNo, double delay );

      /**
       * Set the value for the array-center-to-pad delay.
       * The pad delay is always used in calculating the total
       * delay; there is no bool "use" parameter to toggle it
       * on and off.
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param delay Delay value in nanoseconds.
       * @return none.
       */
      void setPadDelay( unsigned short antennaNo, double delay );

      /**
       * Set the value for the pad-to-antenna, and other antenna specific
       * delays.
       * The antenna delay is always used in calculating the total
       * delay; there is no bool "use" parameter to toggle it
       * on and off.
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param antDelay Delay value in nanoseconds.
       * @param opticsDelayMM Optics delay for mm receivers in nanoseconds.
       * @param opticsDelayCM Optics delay for cm receiver in nanoseconds.
       * @param loCableDelayMM LO cable delay for mm receivers in nanoseconds.
       * @param loCableDelayCM LO cable delay for cm receiver in nanoseconds.
       * @return none.
       */
      void setAntennaDelays(unsigned short antennaNo, double antDelay,
            double opticsDelayMM, double opticsDelayCM, 
            double loCableDelayMM, double loCableDelayCM);

      /**
       * Set the value for the user-definable delay offset.
       * By convention (10/2008), this parameter will be used
       * for the "global" delay centering term used for all 
       * antennas in a subarray.
       *
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param delay Delay value in nanoseconds.
       * @return none.
       * @see Equation 23 of interferometry design document.
       */
      void setAdjustableDelay( unsigned short antennaNo, double delay );

      /**
       * Set the receiver offset for polarization state 1
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param delay Delay value in nanoseconds.
       * @return none.
       */
      void setRxDelayPol1(unsigned short antennaNo, double delay);

      /**
       * Set the receiver offset for polarization state 2
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param delay Delay value in nanoseconds.
       * @return none.
       */
      void setRxDelayPol2(unsigned short antennaNo, double delay);

      /**
       * Tell the DelayEngine whether to use the adjustable delay for a given
       * antenna.
       *
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param useit True or false value.
       * @return none.
       * @see Equation 23 of interferometry design document.
       */
      void useAdjustableDelay( unsigned short antennaNo, bool useit );

      /**
       * Tell the DelayEngine whether to use the geometric delay for a given
       * antenna.
       *
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param useit True or false value.
       * @return none.
       * @see Equation 23 of interferometry design document.
       */
      void useGeometricDelay( unsigned short antennaNo, bool useit );

      /**
       * Tell the DelayEngine whether to use the additional refractive
       * height delay for a given antenna.
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param useit True or false value.
       * @return none.
       */
      void useHeightDelay( unsigned short antennaNo, bool useit );

      /**
       * Tell the DelayEngine whether to use the ionospheric delay for a given
       * antenna.
       *
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param useit True or false value.
       * @return none.
       * @see Equation 23 of interferometry design document.
       */
      void useIonosphericDelay( unsigned short antennaNo, bool useit );

      /**
       * Tell the DelayEngine whether to use the tropospheric delay for a given
       * antenna.
       *
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param useit True or false value.
       * @return none.
       * @see Equation 23 of interferometry design document.
       */
      void useTroposphericDelay( unsigned short antennaNo, bool useit );

      /**
       * Tell the DelayEngine whether to use the thermal delay for a given
       * antenna.
       *
       * @param antennaNo The unique id for the antenna. Must be > 0.
       * @param useit True or false value.
       * @return none.
       * @see Equation 23 of interferometry design document.
       */
      void useThermalDelay( unsigned short antennaNo, bool useit );

      /**
       * Initiation a delay calculation.
       * To keep interpolations downstream valid, this method must be 
       * called by the Subarray Tracker at least every 20 seconds, normally
       * after the appropriate set or use calls.
       */
      DelayFrameVec computeDelays(void);

      /**
       * Perform an internal selftest to compute a geometric
       * delay for an antenna configuration with a known answer.
       * @param verbose true if you want some intermediate messages
       * @return true if self test passed, false if not.
       */
      bool selfTest (bool verbose );

      /**
       * Print out the delays from CARMA and BIMA code
       */
      void testAgainstBima(void);

    private: 

        // prevent copying and assignment
        DelayEngine( const DelayEngine& );
        DelayEngine &operator=( const DelayEngine& );

        /**
         * Called during construction
         */
        void initialize(void) ;

        /**
         * This actually does the work of the coordinate setup.
         * Since we have two methods of setting a source,
         * the common work is moved to a single method.
         */
        void finishCoordinates(unsigned short antennaNo,
                              double mjd,
                              double raPointingOffset,
                              double decPointingOffset,
                              double raPhaseOffset,
                              double decPhaseOffset,
                              bool isJ2000,
                              const std::string& source
                  );

        // subroutines to compute various delays
        
        /**
         * Compute the total fixed delay for the given antenna.
         * This can be considered as the part of the path in the IF.
         * @param antennaNo Antenna identification
         * use parameters for delay calculation.
         * @return Total fixed delay, nanosec
         */
        double computeTotalFixedDelay(unsigned short antennaNo);
                                      
        /**
         * Compute the total delay for the given antenna
         * @param antennaNo Antenna identification
         * @param delayInfo The DelayInfo object from which to
         * use parameters for delay calculation.
         * @param refPtInfo The reference point DelayInfo object,
         * used for subtraction in tropospheric and height
         * delay calcs.
         * Timestamp must match that of delayInfo parameter!
         * @return Total delay, nanosec
         */
        double computeTotalDelay(unsigned short antennaNo, 
                                 DelayInfo & delayInfo,
                                 DelayInfo & refPtInfo);

        /**
         * Compute the axis misaligmnent delay, Da/cos(elevation),
         * where Da is the axis offset in meters
         * @param antennaNo Antenna identification
         * @param delayInfo The DelayInfo object from which to
         * use parameters for delay calculation.
         * @return Delay due to axis misalignment, nanosec
         * @see TSW chapter 4 and ref (19xx)
         */
        double computeAxisDelay(unsigned short antennaNo, 
                                DelayInfo & delayInfo);

        /**
         * Compute the geometric delay, standard Thompson,
         * Moran, and Swenson formula: -W plus axis
         * misalignment term.
         * U and V will also be computed here, so that
         * (U,V,W) may be provided as monitor points for the
         * Filler.
         * @param antennaNo Antenna identification
         * @param delayInfo The DelayInfo object from which to
         * use parameters for delay calculation.
         * @return -W plus axis misalignment term, nanosecs
         */ 
        double computeGeometricDelay(unsigned short antennaNo, 
                                     DelayInfo & delayInfo);

        /**
         * Compute additional refractive delay due
     * to antennas at differing heights. 
     * Return value will be the total height
     * delay for the input antenna (from sea level): 
     * must be differenced with that from the array reference 
     * point. i.e. call this method on (antennaNo, arrayRefPt)
         * @param antennaNo Antenna identification
         * @param delayInfo The DelayInfo object from which to
         * @return R0*dH/sin(el)
         */ 
        double computeHeightDelay(unsigned short antennaNo, 
                                  DelayInfo & delayInfo);


        /**
         * Compute the delay due to propogation through
         * the ionospheric plasma.
         * This is a negative delay. See design document.
         * @param antennaNo Antenna identification
         * @param delayInfo The DelayInfo object from which to
         * use parameters for delay calculation.
         * @return Ionospheric delay, nanosec
         */
        double computeIonosphericDelay(unsigned short antennaNo, 
                                       DelayInfo & delayInfo);

        /*
         * Compute the delay due to thermal changes in the
         * antenna structure.
         * @param antennaNo Antenna identification
         * @param delayInfo The DelayInfo object from which to
         * use parameters for delay calculation.
         * @return Thermal delay, nanosec
         */
        double computeThermalDelay(unsigned short antennaNo, 
                                   DelayInfo & delayInfo);

        /*
         * Compute the delay due to propogation through the
         * troposphere. Along the way, this method also calculates
         * the pathlength and zenith refractivity, which are
         * Delay Engine monitor points.
         *
         * @param antennaNo Antenna identification
         * @param delayInfo The DelayInfo object from which to
         * use parameters for delay calculation.
         * @return Tropospheric delay, nanosec
         */
        double computeTroposphericDelay(unsigned short antennaNo, 
                                        DelayInfo & delayInfo);

        /**
         * Compute the hour angle of the pointing center
         * for the given antenna. Guaranteed to
         * be between -PI and PI radians. 
         *
         * @param antennaNo Antenna identification
         * @param delayInfo The DelayInfo object from which to
         * use parameters for delay calculation.
         * @return the hour angle in radians
         */
        double computePointingCenterHourAngle(unsigned short antennaNo, 
                                              const DelayInfo & delayInfo);

        /**
         * Compute the hour angle of the phase center
         * for the given antenna. Guaranteed to
         * be between -PI and PI radians. 
         *
         * @param antennaNo Antenna identification
         * @param delayInfo The DelayInfo object from which to
         * use parameters for delay calculation.
         * @return the hour angle in radians
         */
        double computePhaseCenterHourAngle(unsigned short antennaNo,
                                           const DelayInfo & delayInfo);

        
        /**
         * Set the Location in the delay engine's AstroTime
         * object so that AstroTime methods can be called
         * for a given antenna.
         * @param antennaNo Antenna identification
         * @param delayInfo The DelayInfo object from which to
         * use get location info.
         */
        void setLocation( unsigned short antennaNo, 
                          const DelayInfo & delayInfo );

        /**
         * Fill in one of the samples for the monitor points.
         * This method should be called for each of the
         * three MP samples before calling publishMonitorPoints.
         *
         * @param delayInfo A DelayInfo object which has been fully
         * computed. The delay data from this object will be passed
         * along to the Monitor stream.
         */
        void fillInMonitorPoints(
            const DelayInfo & delayInfo,
            monitor::DelayEngineSubsystem & monitorFrame );


        /** @return debugging info to pass to, e.g. a Trace object 
         * @param delayInfo The DelayInfo object to print
         */
        std::string debugPrint(const DelayInfo & delayInfo);

        /**
         * For comparison, compute the delay from the BIMA code
         * @param antennaNo Antenna identification
         * @param delayInfo The DelayInfo object from which to
         * @return Total delay, nanosec
         */
        double computeBimaDelay(unsigned short antennaNo, 
                                const DelayInfo & delayInfo);

 
        /**
         * compute the delays for just one antenna
         */
        void computeDelays(unsigned short antennaNo);

    //-------------- MEMBER VARIABLES -----------------
    
    // units conversion helper
        carma::services::Units units_;

        /** Use for J2000->Current Ra,Dec calculation */
        carma::services::Ephemeris ephemeris_;

        /** Used for LST calculation */
        carma::services::AstroTime astroTime_;

        /** Used for atmospheric calculations.*/
        carma::services::Atmosphere atm_;


        /**
         * Was setArrayReferencePoint called?
         */
        bool arrayRefPtSet_;  // false

        /**
         * The reference point for the array, specified by 
         * longitude, latitude, altitude.
         * Defaults to CARMA reference position from
         * Observatory.cat
         */
        carma::services::AntennaCoordinates arrayRefPt_;

        /**
         * Each DelayInfo object keeps a self-consistent set of delay
         * parameters.  They are separated by 20 seconds in time.
         * The current will contain data for the calculation
         * currently underway, which has a timestamp
         * roughly 40 seconds from the current time; the
         * Delay Engine normally works 40 seconds ahead.
         * If a discontinuity occurs (e.g. source change),
         * all three of these are recalculated.
         */ 
        DelayInfo current_;          /* 40 seconds from now */
        DelayInfo penultimate_;      /* 20 seconds from now */
        DelayInfo antepenultimate_;  /* now */
        DelayInfo modified_;         /* intermediate, temporary, to keep
                                     * current pristine so that current
                                     * can be copied to penultimate when
                                     * it is superceded.
                                     * modifed always
                                     * gets copied to current when 
                                     * computeDelays() 
                                     * is called 
                                     */

    /* For calculating various delays at the array
        * at the array reference point, which will be
    * subtracted from the associated delays at the
    * antenna.
        */
    std::vector<DelayInfo> refPtDelay_;

    // a container for the current, pen, ante DelayInfos
    std::vector<DelayInfo> dtriplet_;

    // for testing purposes against bima code
    DelayInfo bima_;

    /**
     * Holds information on which delays are turned on and off
     */
    DelayStatus* delayStatus_;

    /** 
     * Array containing the total calculated fixed delay for each antenna
     * The constructor will set this to zero.
     */
    std::vector<double> totalFixedDelay_; 
        
    /**
     * Array containing an additional adjustable delay offset 
     * for each antenna.
     */
    std::vector<double> adjustableDelay_; 

    /**
     * Array containing the fixed "global" delay offset from the
     * array center every antenna in a given subarray.
     * This will be identical for every antenna in a subarray
     * except when the receivers are tuned to 1mm when there will
     * be small differences attributable to the 3mm-1mm receiver
     * delay offsets.
     */
    std::vector<double> delayOffset_; 

    /**
     * Array containing array-center-to-pad
     * delay for each antenna
     */
    std::vector<double> padDelay_; 

    /**
     * Array containing pad-to-antenna
     * delay for each antenna
     */
    std::vector<double> antDelay_; 
    /**
     * Arrays containing optics delays for each antenna
     */
    std::vector<double> opticsDelayMM_; 
    std::vector<double> opticsDelayCM_; 
    /**
     * Arrays containing lo cable delays for each antenna
     */
    std::vector<double> loCableDelayMM_; 
    std::vector<double> loCableDelayCM_; 

    /** Receiver offset delay per antenna, for polarization 1 */
    std::vector<double> rxDelayPol1_; 

    /** Receiver offset delay per antenna, for polarization 2 */
    std::vector<double> rxDelayPol2_; 


    static const std::string RaDec;
    static const std::string AzEl;
        
    /**
     * ensure allocation of the auto_ptrs below
     */
        static monitor::DelayEngineSubsystem &
        ensureAllocated( ::std::auto_ptr< monitor::DelayEngineSubsystem > & );
        
        ::std::auto_ptr< monitor::DelayEngineSubsystem > des0_;
        ::std::auto_ptr< monitor::DelayEngineSubsystem > des1_;
        ::std::auto_ptr< monitor::DelayEngineSubsystem > des2_;

    static const unsigned short ANTEPENULTIMATE = 0;
    static const unsigned short PENULTIMATE     = 1;
    static const unsigned short CURRENT         = 2;

    bool freqSet_;
    services::Frequency freq_; // GHz
    };
  } // namespace interferometry
} // namespace carma


#endif //CARMA_INTERFEROMETRY_DELAYENGINE_H
