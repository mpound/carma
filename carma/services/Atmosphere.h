// $Id: Atmosphere.h,v 1.2 2014/06/04 17:09:38 mpound Exp $
/**
 * @file carma/services/Atmosphere.h
 *
 * Constants and methods having to do with the earth's atmosphere 
 * (e.g. refraction). These are used by interferometry package,
 * Auxiliary Services, and may be used by Weather Station.
 * @author Marc Pound
 * @version $Revision: 1.2 $
 *
 */

#include "carma/services/Units.h"
#include "carma/services/Angle.h"
#include "carma/services/Pressure.h"
#include "carma/services/Temperature.h"
#include "carma/services/Velocity.h"

#ifndef CARMA_SERVICES_ATMOSPHERE_H
#define CARMA_SERVICES_ATMOSPHERE_H
namespace carma  {
  namespace services {
  /**
   * This class is used to calculate a variety of quantities related 
   * to the earth's atmosphere, such as refractivity, saturated pressure,
   * and pathlength (refractivity integrated through the atmosphere). 
   * Many of these depend upon the
   * weather conditions such as air temperature, pressure, humidity.
   * The class also contains reasonable default, 
   * minimum, and maximum values for these weather parameters.
   * The current user of this class is the Delay Engine, however
   * it is expected that Auxiliary Services will use this class
   * to calculate antenna refraction pointing corrections.
   * The Weather Station might also use this class.
   *
   * @todo 
   * <ul>
   * <li>Verify the min, max, default values for atmospheric 
   * quantities are sensible.
   * </li>
   * <li>Possibly modify computeHorizonPathlength() to take 
   * into account all 3 scale heights.
   * </li>
   * </ul>
   */
  class Atmosphere {
      public:

      //------------------------------------------------------
      //             PUBLIC STATIC CONSTANTS 
      //------------------------------------------------------
      

      /** 
       * Default atmospheric pressure, in millibars 
       * For when weather station measurement unavailable.
       * The default is based on measurements at Cedar Flat.V
       */
      static const double DEFAULT_ATM_PRESSURE = 780.0;

      /**
       * US standard 1 atm of pressure
       */
      static const double STANDARD_ATM_PRESSURE = 1013.25;

      /** 
       * Minimum believable atmospheric pressure, in millibars. 
       * If weather station gives value below this, there is
       * probably something wrong, so fall back to default.
       */
      static const double MIN_ATM_PRESSURE = 680; // 0.9*DEFAULT for CARMA

      /** 
       * Maximum believable atmospheric pressure, in millibars 
       * If weather station gives value above this, there is
       * probably something wrong, so fall back to default.
       */
      static const double MAX_ATM_PRESSURE = 1115.0; //1.1atm at sealevel

      /** 
       * Default air temperature, in Kelvin 
       * For when weather station measurement unavailable.
       */
      //"room temperature"
      static const double DEFAULT_AIR_TEMP = 300.0; //80 Fahrenheit

      /** 
       * Minimum believable air temperature, in Kelvin
       * If weather station gives value below this, there is
       * probably something wrong, so fall back to default.
       */
      //??need reasonable limits
      static const double MIN_AIR_TEMP = 242.0; // -25 Fahrenheit

      /** 
       * Maximum believable air temperature, in Kelvin
       * If weather station gives value above this, there is
       * probably something wrong, so fall back to default.
       */
      //??need reasonable limits
      static const double MAX_AIR_TEMP = 316.5; // 110 Fahrenheit 

      /** 
       * Default relative humidity, in percent 
       * For when weather station measurement unavailable.
       */
      //??need reasonable limits
      static const double DEFAULT_RH = 25.0; 

      /** 
       * Minimum believable relative humidity, in percent 
       * If weather station gives value below this, there is
       * probably something wrong, so fall back to default.
       */
      //??need reasonable limits
      static const double MIN_RH = 5.0; 

      /** 
       * Maximum believable relative humidity, in percent
       * If weather station gives value above this, there is
       * probably something wrong, so fall back to default.
       */
      static const double MAX_RH = 100.0; 

      /** 
       * Default dew point, in Kelvin
       * For when weather station measurement unavailable.
       */
      // This is the dewpoint for the DEFAULT_AIRTEMP (300K)
      // and DEFAULT_RH (50%)
      //  carma/environment/Test/tAtmosphere RH=50 Ta=26.85
      // 25% = 277.388
      //  carma/environment/Test/tAtmosphere RH=25 Ta=26.85
      // 50% = 288.761; 
      static const double DEFAULT_DEW_POINT = 278.388;

      /** 
       * Minimum believable dew point, in Kelvin
       * If weather station gives value below this, there is
       * probably something wrong, so fall back to default.
       */
      //For MIN_AIRTEMP, MIN_RH
      static const double MIN_DEW_POINT = 221.9;

      /** 
       * Maximum believable dew point, in Kelvin
       * If weather station gives value above this, there is
       * probably something wrong, so fall back to default.
       * Ideally, this should be non-const and
       * set to the current Ambient temperature, since Tdp 
       * is "never" greater than Tambient.
       * Maybe I need a private maxdewpoint() method.
       */
      //dewpoint at 100% RH is the airtemp
          //value is initialized with the storage declaration in the .cc file
      static const double MAX_DEW_POINT;

      /**
       * The minimum elevation at which pathlength will be directly 
       * calculated, in radians.  Below this elevation,
       * computeHorizonPathLength() is used. Currently set to 
       * 1 degree.
       */
      static const double MIN_ELEVATION = 0.0174533; // radians

      /** 
       * Default wind speed, in mph
       */
      static const double DEFAULT_WIND_SPEED = 0.0;

      /** 
       * Minimum wind speed, in mph
       * If weather station gives value below this, there is
       * probably something wrong, so fall back to default.
       */
      static const double MIN_WIND_SPEED = 0.0;

      /** 
       * Maximum reasonable wind speed, in mph
       * If weather station gives value above this, there is
       * probably something wrong, so fall back to default.
       */
      static const double MAX_WIND_SPEED = 200.0;

      typedef enum dewPointMethodEnum { 
          CLAUSIUS_CLAPEYRON, 
          MAGNUS_TETENS,
          HOFFMAN_WELCH 
      } dewPointMethodType ;

      //------------------------------------------------------
      //            PUBLIC METHODS
      //------------------------------------------------------
      
      /** no-arg constructor */
      Atmosphere();

      /** destructor */
      virtual ~Atmosphere();

    /**
     * The surface saturated water vapor pressure is
     * used in the calculation of the refractivity.
     * This method will NOT check that 
     * the air temperature is "safe", use safeAirTemperature() to do
     * that.
     *
     * @param airTemp The ambient air temperature in Kelvin
     * @return the surface saturated water vapor pressure
     * in millibars (equation 13 in interferometry design document)
     *
     * @see #safeAirTemperature(double airTemp)
     */
    double computeSaturatedPressure(double airTemp) const;

    /**
     * The water column is derived from the water vapor density
     * and an assumed scaleheight for the wet atmosphere
     * (assumed to be about 2.1km)
     *
     * @param airTemp    the ambient air temperature in Kelvin
     * @param relHumid   the relative humidity, in percent (0..100)
     * @return water column in mm
     */
    double waterColumn(double airTemp, double relHumid) const;

    /**
     * Compute the partial pressure of water given the ambient
     * air temperature and relative humidity.  The partial pressure
     * is the saturated pressure times the relative humidity.
     *
     * @param airTemp    the ambient air temperature in Kelvin
     * @param relHumid   the relative humidity, in percent (0..100)
     * @return partial pressure of water in millibars
     *
     * @see #computeSaturatedPressure(double airTemp, double relHumid)
     */
    double waterPartialPressure(double airTemp, double relHumid) const;

    /**
     * The water vapor density is derived from
     * the relative humidity and saturated pressure, and is the
     * amount of water in the atmosphere.  We choose the return
     * units to be g/m^3 rather than g/cm^3 because typical
     * values in g/m^3 will be of order unity.
     *
     * @param airTemp    the ambient air temperature in Kelvin
     * @param relHumid   the relative humidity, in percent (0..100)
     * @return water vapor density in g/m^3
     */
    double waterVaporDensity(double airTemp, double relHumid) const;


    /**
     * The dewpoint temperature defines the temperature to which a
     * parcel of air would need to be cooled at constant pressure
     * and constant moisture content in order for the vapor pressure
     * and saturation vapor pressure of the parcel to be equal. In
     * other words, it's the temperature at which a parcel of air
     * would be saturated (i.e., 100% relative humidity)
     *
     * The dewpoint temperature is computed from relative
     * humidity and air temperature.
     * There are a few alternate methods available.
     * <ol>
     * <li><b>Modified Clausius-Clapeyron equation<b>
     * </li>
     * <li><b>Magnus-Tetens formula</b>
     * </li>
     * <li><b>Latent Heat of Vaporization</b>
     * </li>
     * </ol>
     * <br>
     * @see <a href=http://meted.ucar.edu/awips/validate/dewpnt.htm>
     * Further discussion of the C-C equation</a>
     * @see <a href=http://www.paroscientific.com/dewpoint.htm
     * Further discussion of the Magnus-Tetens formula</a>
     *
     * @param airTemp  the ambient air temperature in Kelvin
     * @param relHumid the relative humidity, in percent (0..100)
     * @param method   compute method 
     * @return the dewpoint temperature, in Kelvin
     *
     */

    double computeDewPoint(double airTemp, 
        double relHumid, 
        dewPointMethodType method = CLAUSIUS_CLAPEYRON ) const;


    /**
     * The relative humidity is computed from the ambient and dew point
     * temperatures.
     * There are a few alternate methods available.
     *
     * @param airTemp  the ambient air temperature in Kelvin
     * @param dewTemp  dew point temperature, in Kelvin
     * @param method   compute method  (see above)
     * @return relHumid the relative humidity, in percent (0..100)
     * @see #computeDewPoint(double airTemp, 
     *                       double relHumid, 
     *                       dewPointmethodType method) 
     * for explanation of compute method
     */

    double computeHumidity(double airTemp, 
        double dewTemp, 
        dewPointMethodType method = CLAUSIUS_CLAPEYRON ) const;

    /**
     * The refractivity, N_O, at zenith. See equation 14 of
     * the interferometry design document. Its value depends on 
     * weather conditions. This method will NOT check that 
     * the air temperature, atmospheric pressure,  and RH 
     * are "safe"; use safeAirTemperature(), safeAtmPressure(), 
     * and safeRelativeHumidity() to do that.
     *
     * @param airTemp The ambient air temperature in Kelvin
     * @param atmPressure The atmospheric pressure in millibars
     * @param relHumid The relative humidity in percent
     * @param frequency The observing frequency, Hz
     * @return zenith refractivity.
     *
     * @see #safeAirTemperature(double airTemp)
     * @see #safeAtmPressure(double airTemp)
     * @see #safeRelativeHumidity(double relHumid)
     */
    double computeZenithRefractivity(double airTemp, 
                     double atmPressure,
                     double relHumid,
                     double frequency) const;

    /**
     * Compute the refraction pointing correction, given
     * the <em>in vacuo</em> elevation and weather
     * parameters. The return value should be <b>added</b>
     * to the <em>in vacuo</em> elevation.
     * The frequency parameter indicates whether
     * the optical or radio correction is returned. If frequency
     * is greater than 3 THz, the optical correction is returned,
     * otherwise the radio correction is returned.  The refraction
     * correction is done using the mapping function of Yan (1996)
     * as written by Mangum ALMA Memo 366.
     *
     * @param airTemp The ambient air temperature in Kelvin
     * @param atmPressure The atmospheric pressure in millibars
     * @param relHumid The relative humidity in percent
     * @param elevation The <em>in vacuo</em> 
     *        (uncorrected) elevation in radians
     * @param frequency The observing frequency, Hz
     * @param  the altitude (station height) of the antenna above 
     *
     * @return the refraction pointing correction in radians
     * to add to the uncorrected elevation.
     */
    double computeRefractionCorrection(double airTemp, 
                       double atmPressure,
                       double relHumid,
                       double elevation,
                       double frequency,
                       double altitude) const;
    /**
     * This is to test against the Ulich (1981)
     * radio refraction formulation
     * NOTE: RADIO ONLY
     * @param  atmPressure The atmospheric pressure in millibars
     * @param  airTemp The ambient air temperature in Kelvin
     * @param  relHumid The relative humidity in percent
     * @param frequency The observing frequency, Hz
     * @param elevation The <em>in vacuo</em> 
     *        (uncorrected) elevation in radians
     */

    double ulichRefractionCorrection(double airTemp, 
                     double atmPressure, 
                     double relHumid,
                     double elevation,
                     double frequency) const;
    /**
     * <p>
     * This method calculates
     * the refractivity integrated through the atmosphere,
     * that is, the pathlength.  See equation 18 of
     * the interferometry design document. This expression
     * for pathlength diverges as elevation approaches zero degrees.
     * Therefore, if the source elevation is below some minimum, 
     * the maximum (horizon) pathlength as defined by 
     * TMS equation 13.40 is returned.  
     * </p><p>
     * The pathlength depends on 
     * weather conditions. This method will NOT check that 
     * the air temperature, atmospheric pressure,  and RH 
     * are "safe"; use safeAirTemperature(), safeAtmPressure(), 
     * and safeRelativeHumidity() to do that.
     * </p>
     * <p>
     * Note the refraction correction to the elevation is
     * not applied internally by this method. If you want
     * the pathlength for the refraction-corrected elevation,
     * you must first add the the elevation the return value 
     * computeRefractionCorrection().
     * 
     * @param  atmPressure The atmospheric pressure in millibars
     * @param  airTemp The ambient air temperature in Kelvin
     * @param  relHumid The relative humidity in percent
     * @param  elevation The source elevation in radians
     * @param  frequency The observing frequency, Hz
     * @param  the altitude (station height) of the antenna above 
     *         the array reference plane, meters.
     * @return the pathlength, in meters
     *
     * @see #computeZenithRefractivity(double airTemp, 
     *       double atmPressure, double relHumid)
     * @see #computeHorizonPathlength()
     * @see #computeRefractionCorrection()
     * @see #safeAirTemperature(double airTemp)
     * @see #safeAtmPressure(double airTemp)
     * @see #safeRelativeHumidity(double relHumid)
     */
    double computePathlength(double airTemp,
                 double atmPressure,
                 double relHumid,
                 double elevation,
                 double frequency,
                 double altitude = 0.0) const;


    /**
     * Calculate the maximum possible pathlength, toward
     * the horizon. This is defined in terms of the
     * zenith refractivity and Earth constants in
     * TMS equation 13.40.  Its value depends on 
     * weather conditions. This method will NOT check that 
     * the air temperature, atmospheric pressure,  and RH 
     * are "safe"; use safeAirTemperature(), safeAtmPressure(), 
     * and safeRelativeHumidity() to do that.
     *
     * @param  atmPressure The atmospheric pressure in millibars
     * @param  airTemp The ambient air temperature in Kelvin
     * @param  relHumid The relative humidity in percent
     * @param  frequency The observing frequency, Hz
     * @return The horizon pathlength, in meters
     *
     * @see #computeZenithRefractivity(double airTemp, 
     *       double atmPressure, double relHumid)
     */
    double computeHorizonPathlength(double airTemp, 
                    double atmPressure,
                    double relHumid,
                    double frequency) const;

    /**
     * This method is meant to check that a given air temperature
     * is within reasonable bounds for the CARMA site. It will return 
     * a sensible default air temperature if the input temperature
     * is outside those bounds.  Both methods store the last safe 
     * air temperature if it is safe.
     *
     * @param airTemp The ambient air temperature to check, in Kelvin
     * @return airTemp if (MIN_AIR_TEMP <= airTemp <= MAX_AIR_TEMP),
     * otherwise return DEFAULT_AIR_TEMP
     */
    static double safeAirTemperature(double airTemp);

    /**
     * @return True if the air temperaure is within reasonable
     * bounds.
     * @param airTemp The ambient air temperature to check, in Kelvin
     * @see #safeAirTemperature(double airTemp)
     */
    static bool isSafeAirTemperature(double airTemp);

    /**
     * This method is meant to check that a given air temperature
     * is within reasonable bounds for the CARMA site. It will return 
     * a sensible default air temperature inside a Tempurature object 
     * if the input temperature is outside those bounds.
     *
     * @param airTemp The ambient air temperature to check
     * (any units)
     * @return airTemp if its value is within the reasonable bounds
     * otherwise return DEFAULT_AIR_TEMP in a Temperature object.
     * @see #safeAirTemperature(double airTemp)
     */
    static carma::services::Temperature safeAirTemperature(
        carma::services::Temperature airTemp);
    /**
     * @return True if the air temperaure is within reasonable
     * bounds.
     * @param airTemp The ambient air temperature to check
     * @see #safeAirTemperature(services::Temperature airTemp)
     */
    static bool isSafeAirTemperature( carma::services::Temperature airTemp);
    
    /**
     * This method checks if the input air temperature is safe and   
     * if so saves and returns the value.  If the input temperature isn't 
     * safe, the last safe temperature value is returned.  If there is
     * no previous value, DEFAULT_AIR_TEMP is returned.  This 
     * technique provides alternative air temperature values 
     * which more accurately reflect current weather conditions.
     * @param airTemp Air temperature in Kelvin.
     */
    carma::services::Temperature lastSafeAirTemperature( double airTemp );

    /**
     * This method checks if the input air temperature is safe and   
     * if so saves and returns the value.  If the input temperature isn't 
     * safe, the last safe temperature value is returned.  If there is
     * no previous value, DEFAULT_AIR_TEMP is returned.  This 
     * technique provides alternative air temperature values 
     * which more accurately reflect current weather conditions.
     * @param airTemp Air temperature
     */
    carma::services::Temperature lastSafeAirTemperature( 
       carma::services::Temperature airTemp );

    /**
     * @return the most recently stored safe air temperature
     */
    carma::services::Temperature lastSafeAirTemperature( ) const;

    /**
     * This method is meant to check that a given atmospheric pressure
     * is within reasonable bounds for the CARMA site. It will return 
     * a sensible default atmospheric pressure if the input pressure
     * is outside those bounds.
     *
     * @param atmPressure The atmospheric pressure to check, 
     *                    in millibars
     * @return atmPressure if 
     * (MIN_ATM_PRESSURE <= atmPressure <= MAX_ATM_PRESSURE),
     * otherwise return DEFAULT_ATM_PRESSURE
     */
    static double safeAtmPressure(double atmPressure);

    /**
     * @return True if the atmospheric pressure is within reasonable
     * bounds.
     * @param atmPressure The atmospheric pressure to check, 
     *                    in millibars
     * @see #safeAirTemperature(double airTemp)
     */
    static bool isSafeAtmPressure(double atmPressure);

    /**
     * This method is meant to check that a given atmospheric pressure
     * is within reasonable bounds for the CARMA site. It will return 
     * a sensible default atmospheric pressure inside a Pressure object 
     * if the input pressure is outside those bounds.
     *
     * @param atmPressure The atmospheric pressure to check, 
     *                    in a Pressure object.
     * @return atmPressure if its value is within the reasonable bounds
     * otherwise return DEFAULT_ATM_PRESSURE in a Pressure object.
     * @see #safeAtmPressure(double atmPressure)
     */
    static carma::services::Pressure safeAtmPressure(
        carma::services::Pressure atmPressure );
    /**
     * @return True if the atmospheric pressure is within reasonable
     * bounds.
     * @param atmPressure The atmospheric pressure to check
     * @see #safeAtmPressure(double atmPressure)
     */
    static bool isSafeAtmPressure( carma::services::Pressure atmPressure );
    
    /**
     * This method checks if the input atmospheric pressure is safe and   
     * if so saves and returns the value.  If the input pressure isn't 
     * safe, the last safe pressure value is returned.  If there is
     * no previous value, DEFAULT_ATM_PRESSURE is returned.  This 
     * technique provides alternative atmospheric pressure values 
     * which more accurately reflect current weather conditions.
     * @param atmPressure Atmospheric pressure in mbar.
     */
    carma::services::Pressure lastSafeAtmPressure( double atmPressure );

    /**
     * This method checks if the input atmospheric pressure is safe and   
     * if so saves and returns the value.  If the input pressure isn't 
     * safe, the last safe pressure value is returned.  If there is
     * no previous value, DEFAULT_ATM_PRESSURE is returned.  This 
     * technique provides alternative atmospheric pressure values 
     * which more accurately reflect current weather conditions.
     * @param atmPressure Atmospheric pressure 
     */
    carma::services::Pressure lastSafeAtmPressure( 
       carma::services::Pressure atmPressure );

    /**
     * @return the most recently stored safe atmospheric pressure
     */
    carma::services::Pressure lastSafeAtmPressure( ) const;

    /**
     * This method is meant to check that a given relative humidity
     * is within reasonable bounds for the CARMA site. It will return 
     * a sensible default relative humidity if the input 
     * relative humidity is outside those bounds.
     *
     * @param relHumid The relative humidity to check, in percent.
     * @return relHumid if (MIN_RH <= relHumid <= MAX_RH),
     * otherwise return DEFAULT_RH
     */
    static double safeRelativeHumidity(double relHumid);

    /**
     * @return true if the relative humidity 
     * is within reasonable bounds for the CARMA site. 
     * @see #safeRelativeHumidity(double relHumid)
     */
    static bool  isSafeRelativeHumidity(double relHumid);

    /**
     * This method checks if the input relative humidity is safe and   
     * if so saves and returns the value.  If the input humidity isn't 
     * safe, the last safe humidity value is returned.  If there is
     * no previous value, DEFAULT_RH is returned.  This 
     * technique provides alternative humidity values 
     * which more accurately reflect current weather conditions.
     * @param relHumid Relative humidity in percent.
     */
    double lastSafeRelativeHumidity( double relHumid );

    /**
     * @return the most recently stored safe relative humidity
     */
    double lastSafeRelativeHumidity( ) const;
    
    /**
     * This method is meant to check that a given dew point temperature
     * is within reasonable bounds for the CARMA site. It will return 
     * a sensible default dew point if the input dew point
     * is outside those bounds.
     *
     * @param  dewPoint The dew point temperature to check, in Kelvin
     * @return dewPoint if 
     * (MIN_DEW_POINT <= dewPoint <= MAX_DEW_POINT),
     * otherwise return DEFAULT_DEW_POINT
     *
     * (NB: Actually MAX_DEW could be a variable set equal to 
     * Tambient, since Dewpoint rarely exceeds Tambient)
     */
    static double safeDewPoint(double dewPoint);

    /**
     * @return true if dewpoint temperature 
     * is within reasonable bounds for the CARMA site. 
     * @param dewPoint The dewpoint Temperature
     * @see #safeDewPoint(double dewPoint)
     */
    static bool  isSafeDewPoint(double dewPoint);

    /**
     * This method is meant to check that a given dew point  temperature
     * is within reasonable bounds for the CARMA site. It will return 
     * a sensible dew point temperature inside a Tempurature object 
     * if the input temperature is outside those bounds.
     *
     * @param  dewPoint The dew point temperature to check, 
     * in a Temperature object
     * @return dewPoint if its value is within the reasonable bounds
     * otherwise return DEFAULT_DEW_POINT in a Temperature object.
     */
    static carma::services::Temperature safeDewPoint(
        carma::services::Temperature dewPoint );

    /**
     * @return true if dewpoint temperature 
     * is within reasonable bounds for the CARMA site. 
     * @param dewPoint The dewpoint Temperature
     * @see #safeDewPoint(services::Temperature dewPoint)
     */
    static bool  isSafeDewPoint( carma::services::Temperature dewPoint );

    /**
     * Check that a given dewPoint value is consistent with
     * its paired ambient air temperature and relative
     * humidity.
     * @param  dewPoint The dew point temperature to check
     * @param airTemp The ambient air temperature to use in the
     * calculation
     * @param relHumid The relative humidity in percent
     * @param method   compute method 
     * @param consistencyPercent The sloppiness of the consistency check.
     * @return true if the dewPoint entered is consistent
     * with the air temp and relative humidity.
     * Consistency means input dewPoint is within 
     * <i>+/-consistencyPercent</i> (inclusive range) of
     * that returned by computeDewPoint(airTemp,relHumid)
     * @see #computeDewPoint(double airTemp, 
     *                       double relHumid, 
     *                       dewPointmethodType method) 
     * for explanation of compute method
     */
    bool isConsistentDewPoint(services::Temperature dewPoint,
                  services::Temperature airTemp, 
                  double relHumid,
        dewPointMethodType method = CLAUSIUS_CLAPEYRON,
            double consistencyPercent = 10.0 );

    /**
     * This method checks if the input dew point is safe and   
     * if so saves and returns the value.  If the input dew point isn't 
     * safe, the last safe dew point value is returned.  If there is
     * no previous value, DEFAULT_DEW_POINT is returned.  This 
     * technique provides alternative dew point values 
     * which more accurately reflect current weather conditions.
     * @param dewPoint Dew point in Kelvin.
     */
    carma::services::Temperature lastSafeDewPoint( double dewPoint );

    /**
     * This method checks if the input dew point is safe and   
     * if so saves and returns the value.  If the input dew point isn't 
     * safe, the last safe dew point value is returned.  If there is
     * no previous value, DEFAULT_DEW_POINT is returned.  This 
     * technique provides alternative dew point values 
     * which more accurately reflect current weather conditions.
     * @param dewPoint Dew point Temperature
     */
    carma::services::Temperature lastSafeDewPoint( 
       carma::services::Temperature dewPoint );

    /**
     * @return the most recently stored safe dew point.
     */
    carma::services::Temperature lastSafeDewPoint( ) const;
    
    /**
     *
     * @param  elevation The elevation to check, in radians
     * @return elevation if (elevation >= MIN_ELEVATION )
     * otherwise return MIN_ELEVATION
     */
    static double safeElevation(double elevation);

    /**
     * @return true if elevation is safe
     * @param elevation, in radians
     * @see #safeElevation(double elevation)
     */
    static bool isSafeElevation(double elevation);

    /**
     *
     * @param  elevation The elevation ANgle to check, in radians
     * @return An Angle representiing the elevation if 
     * (elevation >= MIN_ELEVATION )
     * otherwise return an Angle representing MIN_ELEVATION
     */
    static carma::services::Angle safeElevation(
        carma::services::Angle elevation );
    /**
     * @return true if elevation is safe
     * @param elevation, as Angle
     * @see #safeElevation(services::Angle elevation)
     */
    static bool isSafeElevation( carma::services::Angle elevation );

    /**
     * This method is meant to check that a given wind speed
     * is within reasonable bounds for the CARMA site. It will return 
     * a sensible default wind speed if the input speed
     * is outside those bounds.
     *
     * @param windSpeed The wind speed to check, represented by
     * a Velocity
     * @return windSpeed if 
     * (MIN_WIND_SPEED <= windSpeed.mph() <= MAX_WIND_SPEED),
     * otherwise return DEFAULT_WIND_SPEED in a Velocity object
     */
    static carma::services::Velocity safeWindSpeed(
        carma::services::Velocity windSpeed );

    /*
     * @return true is wind speed 
     * is within reasonable bounds for the CARMA site. 
     * @see #safeWindSpeed(services::Velocity windSpeed)
     */
    static bool isSafeWindSpeed( carma::services::Velocity windSpeed );
    
    /**
     * This method is meant to check that a given wind speed
     * is within reasonable bounds for the CARMA site. It will return 
     * a sensible default wind speed if the input speed
     * is outside those bounds.
     *
     * @param windSpeed The wind speed to check, in mph
     * @return windSpeed if 
     * (MIN_WIND_SPEED <= windSpeed <= MAX_WIND_SPEED),
     * otherwise return DEFAULT_WIND_SPEED
     */
    static double safeWindSpeed(double windSpeed);

    /*
     * @return true is wind speed 
     * is within reasonable bounds for the CARMA site. 
     * @see #safeWindSpeed(double windSpeed)
     */
    static bool isSafeWindSpeed( double windSpeed );

      private:
      //------------------------------------------------------
      //            PRIVATE STATIC CONSTANTS 
      //------------------------------------------------------
      /**
       * Coefficients of Clausius-Clapeyron equation approximation
       * as derived by Crane (1976). See, e.g. TMS equation 13.15
       */
      static const double CC_A = 6.105; // leading coeff.
      static const double CC_B = 25.22; // T coeff.
      static const double CC_C = 5.31;  // ln(T) coeff.

      /**
       * Coefficients for modified Clausius-Clapeyron equations
       * used in NOAA's Advanced Weather Interactive Processing System
       * Modified Clausius-Clapeyron is shown here (but not derived!):
       * http://meted.ucar.edu/awips/validate/dewpnt.htm
       */
      static const double AWIPS_C1  = 0.0091379024;
      static const double AWIPS_C2  = 6106.396;
      static const double AWIPS_C3  = 223.1986;
      static const double AWIPS_C4  = 0.0182758048;
      static const double AWIPS_C15 = 26.66082;


      /**
       * Coefficients of Magnus-Teton formula of dewpoint.
       */
      static const double MT_A = 17.27;
      static const double MT_B = 237.7;

      /**
       * Coefficients for Hoffman-Welch (BIMA) formulation
       * of dewpoint. From HatCreek's weatherman1.c code.
       */
      static const double HW_A = 1.598E9;
      static const double HW_B = 5370;

      /**
       * Coefficient for dry air pressure in Smith-Weintraub 
       * equation. See, e.g. TMS chapter 13., eqn 13.70
       * This is K1/Zd with Zd=1. Zd is the dry air compressibility
       * factor.
       */
      static const double SW_DRY_AIR = 77.6;

      /**
       * Coefficient for dry air pressure in Smith-Weintraub 
       * equation as measured by Brussaard & Watson (1995)
       */
      static const double BW_DRY_AIR = 77.6;

      /**
       * Coefficient for water vapor pressure in Smith-Weintraub 
       * equation. Accounts for molecules with induced dipole moments.
       * See, e.g. TMS chapter 13., eqn 13.70
       * This is (K2/Zv)/100 with Zv=1. Zv is the H2O gas 
       * compressibility factor.  Note we divide by 100 because we 
       * use relative humidity in the calculation of refractivity. 
       * See interferometry package design document equation 14.
       */
      static const double SW_INDUCED_DIPOLE = 0.128;
      /** 
       * as measured by Brussaard & Watson (1995)
       */
      static const double BW_INDUCED_DIPOLE = 5.6;

      /**
       * Coefficient for water vapor pressure in Smith-Weintraub 
       * equation. Accounts for molecules with permanent dipole 
       * moments.  See, e.g. TMS chapter 13., eqn 13.70
       * This is (K3/Zv)/100 with Zv=1. Zv is the H2O gas 
       * compressibility factor.  Note we divide by 100 because 
       * we use relative humidity in the calculation of refractivity. 
       * See interferometry package design document equation 14.
       */
      static const double SW_PERM_DIPOLE = 3.776E3;
      /** 
       * as measured by Brussaard & Watson (1995)
       */
      static const double BW_PERM_DIPOLE = 3.75E3;

      /**
       * Coefficient for first-order term of frequency
       * dependence of refractivity, as derived in 
       * interferometry package
       * design document (see equation 22 and Table 1)
       */
      static const double REFRAC_COEFF_A = -2.8354E-5;

      /**
       * Coefficient for second-order term of frequency
       * dependence of refractivity, as derived in 
       * interferometry package
       * design document (see equation 22 and Table 1)
       */
      static const double REFRAC_COEFF_B = 5.4012E-7;

      /**
       * Minimum frequency to apply the 
       * frequency dependence of the refractivity.
       * At or below this frequency the multiplicative
       * factor is unity.  This value is in GHz because
       * the equation coefficients are for GHz.
       */
      static const double MIN_REFRAC_FREQ = 50.0; //50 GHz

      /**
       * Refraction coefficients from Yan equations 13 and 17.
       * These are used in the calculation of the refraction
       * pointing correction. There are 4 equations, 2 for radio
       * refraction and 2 for optical refraction. Each of those
       * equations has several coefficients.  The naming scheme
       * for these coefficients is
       * 
       * A1_OPT_XX...etc, where <br>
       *  A1 means first "A1" expression<br>
       *  RAD means radio <br>
       *  OPT means optical<br>
       *  XX indicates the quantity to which 
       *  this coefficient applies:<br>
       *   "1"   means unity (i.e. its a constant)
       *   "P"   means atmospheric pressure (measured at ground)<br>
       *   "T"   means atmospheric temperature (measured at ground)<br>
       *   "PPW" means the partial pressure of water vapor<br>
       *   "WAVE" means wavelength<br>
       *   "SQ" means quantity squared<br>
       */
      // Equations 13. (radio)
       static const double A1_RAD_1     =  0.5753868; // constant
       static const double A1_RAD_P     =  0.5291E-4; // * P
       static const double A1_RAD_PPW   = -0.2819E-4; // * Pwater
       static const double A1_RAD_PPWSQ = -0.9381E-6; // * Pwater^2
       static const double A1_RAD_T     = -0.5958E-3; // * T
       static const double A1_RAD_TSQ   =  0.2657E-5; // * T^2

       static const double A2_RAD_1     =  1.301211;  // constant
       static const double A2_RAD_P     =  0.2003E-4; // * P
       static const double A2_RAD_PPW   = -0.7285E-4; // * Pwater
       static const double A2_RAD_PPWSQ =  0.2579E-5; // * Pwater^2
       static const double A2_RAD_T     = -0.2595E-2; // * T
       static const double A2_RAD_TSQ   =  0.8509E-5; // * T^2

      // Equations 17. (optical)
       static const double A1_OPT_1      =  0.5787089; // constant
       static const double A1_OPT_P      =  0.5609E-4; // * P
       static const double A1_OPT_T      = -0.6229E-3; // * T
       static const double A1_OPT_TSQ    =  0.2824E-5; // * T^2
       static const double A1_OPT_PPW    =  0.5177E-3; // * Pwater
       static const double A1_OPT_PPWSQ  =  0.2900E-6; // * Pwater^2
       static const double A1_OPT_WAVE   = -0.1644E-1; // * lambda
       static const double A1_OPT_WAVESQ =  0.4910E-1; // * lambda^2

       static const double A2_OPT_1      =  1.302474;  // constant
       static const double A2_OPT_P      =  0.2142E-4; // * P
       static const double A2_OPT_T      =  0.1287E-2; // * Pwater
       static const double A2_OPT_TSQ    =  0.6500E-6; // * Pwater^2
       static const double A2_OPT_PPW    = -0.2623E-2; // * T
       static const double A2_OPT_PPWSQ  =  0.8776E-5; // * T^2
       static const double A2_OPT_WAVE   = -0.6298E-2; // * lambda
       static const double A2_OPT_WAVESQ =  0.1890E-1; // * lambda^2

       // Zero point offsets used by Mangum (2001)
       // Same as Yan (1996) except temperature has been converted
       // to Kelvin. These are all to be ADDED to the measured
       // values.
       //
       // pressure offset, mbar
       static const double P_OFFSET    = -1013.25;
       // temperature offset, K
       static const double T_OFFSET    = -258.15; 
       // wavelength offset, micron
       static const double WAVE_OFFSET = -0.5320; 

       // coefficients for normalized effective zenith
       // Yan equation 12, Mangum equations 24 and 27.
       // R = 8314.14 J/(kmol K) = kg/(m^2 s^2 kmol K)
       // M = 28.970 kg/kmol
       // g = 9.7840 m/s^2 (in vertical column of air at tropopause)
       // R/(M*g) = 29.33895 meters
       // From units: 
       // You have: 1 K*gasconstant/(28.970 kg kmol^-1*9.7840 m s^-2)
       // You want: 
       //         Definition: 29.333895 m
       static const double RMG = 29.333895;

       // some constants used in the mapping function
       // see computeMappingFunction(...)
       static const double MAP1 = 13.24969;
       static const double MAP2 = 173.4233;

      //------------------------------------------------------
      //          PRIVATE NON-STATIC NON-CONSTANT DATA 
      //------------------------------------------------------
      carma::services::Temperature lastSafeAirTemperature_;
      carma::services::Pressure lastSafeAtmPressure_;
      carma::services::Temperature lastSafeDewPoint_;
      double lastSafeRelativeHumidity_;
       
      //------------------------------------------------------
      //            STRUCTS USED BY THIS CLASS
      //------------------------------------------------------
      
      /**
       * @typedef scaleHeightType
       * Structure to contain various atmospheric scale heights.
       */
      typedef struct {
          /**
           * Infrared scale height
           * Units: m
           */
          double ir;

          /**
           * dry scale height
           * Units: m
           */
          double dry;

          /**
           * water vapor scale height 
           * Units: m
           */
          double wet;

          /**
           * opacity scale height
           * Units: m
           */
          double opacity;

      } scaleHeightType;

      /** 
       * The assumed scaleheights, see interferometry 
       * design document, Table 1
       */
      static const scaleHeightType scaleHeight;

      /**
       * A unit conversion class, used in various places
       */
      const carma::services::Units units;

    //------------------------------------------------------
    //            PRIVATE METHODS
    //------------------------------------------------------
    /**
     * Computes the frequency dependence of the refractivity
     * as described in interferometry design document section 2.1.
     * @param frequency The frequency at which to calculate 
     *                  refractivity, in Hz
     * @return The multiplicative factor for base refractivity
     */
    double freqDependence(double frequency);

    /**
     * Returns the refraction mapping (generator) function
     * for given atmospheric parameters, frequency, and
     * source elevation. 
     * @param  atmPressure The atmospheric pressure in millibars
     * @param  airTemp The ambient air temperature in Kelvin
     * @param  relHumid The relative humidity in percent
     * @param elevation The <em>in vacuo</em> 
     *        (uncorrected) elevation in radians
     * @param frequency The frequency at which to calculate 
     *                  the function, in Hz
     * @param  the altitude (station height) of the antenna above 
     *         the array reference plane, meters.
     * @return the mapping function value
     * @see Mangum (2001) equation 25
     */
    double computeMappingFunction(
                   double airTemp, 
                   double atmPressure, 
                   double relHumid,
                   double elevation,
                   double frequency, 
                   double altitude) const;

    /*
     * This is to test against the Ulich (1981)
     * mapping function from NRAO.
     * @param elevation The <em>in vacuo</em> 
     *        (uncorrected) elevation in radians
     */
    double ulichMappingFunction(double elevation) const;

    /*
     * Calculates the normalized effective zenith parameter, I,
     * which is used in calculating the refraction mapping function.
     * This calculates I<sup>2</sup>csc(elevation).
     * @param elevation The <em>in vacuo</em> 
     *        (uncorrected) elevation in radians
     * @param  airTemp The ambient air temperature in Kelvin
     * @param  the altitude (station height) of the antenna above 
     *         the array reference plane, meters.
     * @return the normalized effective zenith parameter (unitless).
     * @see Mangum (2001) equation 27
     */
    double normalizedEffectiveZenith(double airTemp, 
                     double elevation, 
                     double altitude) const;

    /**
     * Decide whether to use radio or optical correction.
     * @return true if the frequency is above 3 THz and thus 
     * considered optical, false if radio
     */
    bool isOptical(double frequency) const;

    /**
     * Calculate the first "meterological parameter" for 
     * radio refraction correction (Mangum 2001 equation 28).
     * @param p0 normalized pressure, mbar
     * @param t0 normalized temperature, K
     * @param ppw partial pressure of water vapor, mbar
     * @return first radio parameter value, 
     *         A<sub>1</sub><sup>R</sup>
     */
    double a1Radio(double p0, double t0, double ppw) const;

    /**
     * Calculate the second "meterological parameter" for 
     * radio refraction correction (Mangum 2001 equation 28).
     * @param p0 normalized pressure, mbar
     * @param t0 normalized temperature, K
     * @param ppw partial pressure of water vapor, mbar
     * @return second radio parameter value, 
     *         A<sub>2</sub><sup>R</sup>
     */
    double a2Radio(double p0, double t0, double ppw) const;

    /**
     * Calculate the first "meterological parameter" 
     * for optical refraction correction (Mangum 2001 equation 29).
     * @param p0 normalized pressure, mbar
     * @param t0 normalized temperature, K
     * @param ppw partial pressure of water vapor, mbar
     * @return first optical parameter value, 
     *         A<sub>2</sub><sup>R</sup>
     */
    double a1Optical(double p0, double t0, double ppw, double w0) const;

    /**
     * Calculate the second "meterological parameter" 
     * for optical refraction correction (Mangum 2001 equation 29).
     * @param p0 normalized pressure, mbar
     * @param t0 normalized temperature, K
     * @param ppw partial pressure of water vapor, mbar
     * @return second optical parameter value, 
     *         A<sub>2</sub><sup>R</sup>
     */
    double a2Optical(double p0, double t0, double ppw, double w0) const;

  };

  }
}
#endif //CARMA_SERVICES_ATMOSPHERE_H

