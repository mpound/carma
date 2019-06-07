// $Id: Types.h,v 1.17 2013/01/29 14:16:23 teuben Exp $

/**
 * @file 
 * Various type definitions for services classes.
 *
 * @author Marc Pound
 * @version $Revision: 1.17 $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_SERVICES_TYPES_H
#define CARMA_SERVICES_TYPES_H

#include <iostream>
#include <string>
#include <map>

namespace carma  {
  namespace services {

    class Pad;
    class Source;
    class SpectralLine;
    /**
     * @typedef velocityFrameType
     * The Velocity Frame
     */
      typedef enum velocityFrameEnum {
          /** Kinematic local standard of rest */
          FRAME_LSR,

          /** 
           * With respect to a point on the earth's surface,
           * e.g antenna coordinates. The local oscillator is fixed, 
           * no tracking. */
          FRAME_TOPOGRAPHIC,

          /** With respect to the Solar sytem barycenter */
          FRAME_BARYCENTRIC,

          /** With respect to the Sun's barycenter */
          FRAME_HELIOCENTRIC,

          /** With respect to the a planet's barycenter */
          FRAME_PLANETARY

      } velocityFrameType;

      /**
       * @typedef velocityDefType
       * The Velocity Definition
       */
      typedef enum velocityDefEnum {
          /** 
           * Radio definition:<br> nu = nu<sub>0</sub>(1&nbsp;-&nbsp;V/c)
           */
          VEL_RADIO,

          /** 
           * Optical definition:<br> nu = nu<sub>0</sub>/(1&nbsp;+&nbsp;V/c)
           */
          VEL_OPTICAL,

	  /**
	   * Optical definition: nu = nu<sub>0</sub>/(1&nbsp;+&nbsp;z)
	   */
          VEL_Z,

          /**
           * Relativistic definition:<br>
           * nu = nu<sub>0</sub>sqrt(1&nbsp;-&nbsp;(V/c)<sup>2</sup>)
           * /(1&nbsp;+&nbsp;V/c)
           */
          VEL_RELATIVISTIC

      } velocityDefType;

      /**
       * @typedef coordSysType
       * The coordinate systems
       */
      typedef enum coordSysEnum {
          /**
           * Right Ascension, Declination 
           */
          COORDSYS_RADEC, 

          /**
           * Galactic longitude, Galactic latitude
           */
          COORDSYS_GALACTIC,

          /**
           * Azimuth and Elevation
           */
          COORDSYS_AZEL
      } coordSysType;


      /**
       * @typedef sourcePntType
       * Can this source be used for RADIO and/or OPTICAL pointing?
       */
      typedef enum sourcePntEnum {
          /**
           * Source can be used only for radio pointing
           */
          PNT_RADIO,

          /**
           * Source can be used only for optical pointing
           */
          PNT_OPTICAL,

          /**
           * Source can be used only for both radio and optical pointing
           */
          PNT_BOTH
     } sourcePntType;

      /**
       * @typedef ephemTableType
       * Ephemeris table type
       */
      typedef enum ephemTableEnum {
          /**
           * Old style SZA ephem files. Have no doppler. Not supported
           */
	  EPHEM_SZA,

          /**
           * New style VECTORS (X,Y,Z,VX,VY,VZ) from Horizon. We use these.
           */
          EPHEM_VECTORS,

          /**
           * New style RADEC from horizon. Of limited use.
           */
          EPHEM_RADEC,
          /**
           * unknown
           */
          EPHEM_UNKNOWN

      } ephemTableType;

     /**
      * @enum AntennaCoordinateType
      * @brief  Enumeration of possible specifications of antenna 
      * coordinates.  One of:<br>
      * <ul>
      * <li>LLA: Longitude, Latitude, Altitude</li>
      * <li>UEN: Up, East, North</li>
      * <li>TOPO_XYZ: <b>topocentric</b>X, Y, Z</li>
      * <li>GEO_XYZ: <b>geocentric</b>X, Y, Z</li>
      * </ul>
      */
      typedef enum AntennaCoordinateTypeEnum { 
          ANTCOORD_LLA, 
          ANTCOORD_UEN, 
          ANTCOORD_TOPO_XYZ, 
          ANTCOORD_GEO_XYZ 
      } AntennaCoordinateType;



      typedef enum TelescopeLimitsTypeEnum {
	  /** Object always below horizon */
	  LIMIT_NEVER_RISES,  

	  /** Telescope stopped by horizon limit */
	  LIMIT_HORIZON_STOP,

	  /** Telescope stopped by horizon limit on negative
	   *  wrap, by azimuth limit on positive wrap 
	   */
	  LIMIT_AZ_HORIZON_STOP,

	  /** Telescope stopped by azimuth limit on positive wrap,
	   * no limit on negative wrap.
	   */
	  LIMIT_AZ_STOP,

	  /** Telescope never stopped by a limit */
	  NO_LIMIT
      } TelescopeLimitsType;

       // these enums would allow computation of optimal wrap
       // completely inside SourceChecker class
       // instead of split between SourceChecker
       // and DriveHandle.
       // But need to make lots of control changes to move
       // AntennaType from control/antennaHandleUtils to services/Types.h.
       
      /** @typedef AzWrapType
       * mirror drive mode wrap types 
       */
      typedef enum AzWrapTypeEnum {
	  AZWRAP_ADD,
	  AZWRAP_SUB,
	  AZWRAP_ZERO
      } AzWrapType;

      /** @typedef AntennaType
       * Enumeration of possible types for antennas.
       * mirror control antenna types.
       */
      typedef enum {
	  ANT_TYPE_BIMA,
	  ANT_TYPE_OVRO,
	  ANT_TYPE_SZA
      } AntennaType;

      /**
       * @typedef interpType
       * Defines various interpolation types from the GSL library.
       * @see http://www.gnu.org/software/gsl/manual/html_node/Interpolation-Types.html 
       */
    typedef enum interpolationTypeEnum {

	/** Linear interpolation */
	LINEAR, 
	/** Polynomial interpolation. The number of terms in the 
	 * interpolating polynomial is equal to the number of points.
         */
	POLYNOMIAL, 
	/** Cubic spline with natural boundary conditions. */
	CSPLINE, 
	/** Cubic spline with periodic boundary conditions. */
	CSPLINE_PERIODIC,
	/** Non-rounded Akim spline with natural conditions. */
	AKIMA, 
	/** Non-rounded Akim spline with periodic boundary conditions. */
	AKIMA_PERIODIC

    } interpolationType;


      /**
       * @typedef HmsType
       * A string of form "hours:minutes:seconds", i.e., HH:MM:SS(.sss)
       */
      typedef std::string HmsType;

      /**
       * @typedef DmsType
       * A string of form "degrees:minutes:seconds", i.e., DD:MM:SS(.sss)
       */
      typedef std::string DmsType; 

    /**
     * A const iterator over a map of Pads
     */
    typedef ::std::map< ::std::string, carma::services::Pad >::const_iterator
            PadIterator;
     /**
      * A const iterator over a map of Sources
      */
    typedef ::std::map< ::std::string, carma::services::Source >::const_iterator
              SourceIterator;
      
     /**
      * A const iterator over a map of Spectral Lines
      */
    typedef ::std::multimap< ::std::string, carma::services::SpectralLine >::const_iterator SpectralLineIterator;

  }
}

#endif //CARMA_SERVICES_TYPES_H
