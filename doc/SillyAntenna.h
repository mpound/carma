// -*- c++ -*-
#ifndef CARMA_SILLY_ANTENNA_H
#define CARMA_SILLY_ANTENNA_H

#include "Drive.h" 
#include "Source.h"

/** @file
  * Sample code to illustrate use of the coding style guide.
  * The name is designed to ensure that no one uses this code for actual 
  * production software. Anyone who does so shall be labelled SillyBilly,
  * or some homonym thereof.
  * This section also serves as the block summarizing the whole set of
  * classes in this file. You should still summarize a class in the
  * first block on the class level
  * Also note the cute emacs hint -*- c++ -*- on the first line here.
  *
  * @author     N.A. Amarnath
  * @reviewer   Peter Teuben
  * @inspector  Tony Beasley
  * @see        AntennaDesign.pdf
  *
  * @todo   need to check the StyleGuide #'s, since it was updated recently
  *
  * $Id: SillyAntenna.h,v 1.5 2003/05/15 13:58:26 teuben Exp $
  */

// this code does all the musts, should do all the shoulds, and cans the cans....

namespace carma {
  
  namespace antenna {

    /**
     * @typedef skyPos
     * An in-convenient type that is merely introduced here
     * to show how doxygen documents them
     */
    typedef unsigned int skyPos;
    
   /** @class SillyAntenna
     *  Models antennas that waste observing time. 
     *  These antennas do not produce useful data, and should be used 
     *  with caution - if they produce valid data, they are probably in 
     *  error - in other words, not functioning correctly.
     *  This is the section where you need to describe an overview of
     *  your class.
     *
     */
    class SillyAntenna {
      
    public:
      
     /** Returns Right Ascension (RA) as a double precision number,
       * for the specified antenna. Value is returned in radians. 
       *
       * @pre     antenna has been set to a specified source, or has
       *          a set RA (see SillyAntenna::point).
       * @post	  double with valid RA returned, or BlankStareException raised. 
       * @exception BlankStareException raised if antenna's RA, DEC haven't 
       *            been set. 
       * @return  double, representing the RA set for the antenna.
       * @see     antenna::BlankStareException
       */
      double getRA (void)                    // #16 not followed
	throw (antenna::BlankStareException);

     /** Returns Declination (DEC) as a double precision number,
       * for the specified antenna. Value is returned in radians.
       *
       * @pre     antenna has been set to a specified source, or has
       *          a set DEC (see SillyAntenna::point).
       * @post	  double with valid DEC returned, or BlankStareException 
       *          raised.
       * @exception BlankStareException raised if antenna's RA, DEC haven't 
       *            been set. 
       * @return  a double, representing the DEC set for the antenna.
       * @see     antenna::BlankStareException
       */
      double getDEC (void)
	throw (antenna::BlankStareException);

     /** Points antenna at a specified RA and DEC, and tracks that position
       * by changing the azimuth and elevation of the antenna.
       *
       * @pre     antenna is not in collision, locked down (stowed), or
       *          in some other error state that precludes it from being 
       *          pointed at the specified position in the sky.
       * @post	  antenna is pointed at specified ra and dec, and tracks 
       *          that position across the sky.
       * @param ra   of type double, representing a Right Ascension, 
       *          presumably of some object of interest in the sky.
       * @param dec  of type double, representing a declination, presumably 
       *          of some object of interest in the sky.
       * @exception OutOfRangeException raised if specified ra, dec are out 
       *            of range. 
       * @exception FrozenException raised if antenna is locked down or 
       *            cannot move from some reason.
       * @return  nothing.
       * @see     AntennaAPI.html
       */
      void point (double ra, double dec)      // this would turn tracking on
	throw (antenna::OutOfRangeException, antenna::FrozenException);

     /** Points antenna at a specified source, and tracks that source
       * by changing the azimuth and elevation of the antenna.
       *
       * @pre     antenna is not in collision, locked down (stowed), or
       *          in some other error state that precludes it from being 
       *          pointed at the specified source.
       * @post	  antenna is pointed at specified source, and tracks 
       *          that object across the sky using an ephemeris.
       * @param source  of type carma::Source, representing a source 
       *          (an astronomical object of interest to the astronomer).
       * @exception SourceIsntUpYetException, raised if the object isn't 
       *            visible in the sky.
       * @exception FrozenException raised if antenna is locked down or 
       *            cannot move from some reason.
       * @return  nothing.
       */
      void point (Source source)
	throw (antenna::SourceIsntUpYetException, antenna::FrozenException);

     /** Returns azimuth of the antenna in radians. +0 is counter-clockwise
       * from North, and -0 is clockwise from North.
       *
       * @pre     antenna drive system sensors are functioning.
       * @post	  double with valid azimuth returned, or 
       *          SilentDriveException raised.
       * @exception SilentDriveException, raised if the antenna's drive 
       *            sensors aren't functioning.
       * @return  double precision representation for antenna azimuth, 
       *          in radians.
       * @see     AntennaAPI.html
       */
      const double getAZ (void) const                     // #.16/26
	throw (antenna::SilentDriveException);

     /** Returns elevation of the antenna in radians. +0 is counter-clockwise
       * (upwards) from the horizontal, and -0 is clockwise (downwards) from 
       * the horizontal.
       *
       * @pre     antenna drive system sensors are functioning.
       * @post	  double with valid elevation returned, or 
       *          SilentDriveException raised.
       * @exception SilentDriveException, raised if the antenna's drive 
       *            sensors aren't functioning.
       * @return  double precision representation for antenna elevation, 
       *          in radians.
       * @see     AntennaAPI.html
       */
      const double getEL (void) const
	throw (antenna::SilentDriveException);

     /** Sets the azimuth of the antenna to an angle specified in radians.
       *
       * @pre     antenna is not in collision, locked down (stowed), or
       *          in some other error state that precludes it from being 
       *          moved.
       * @post	  antenna azimuth is equal to the angle specified as input.
       * @param az  of type double, representing the azimuthal angle to which 
       *          antenna has to move.
       * @exception FrozenException raised if antenna is locked down or 
       *            cannot move from some reason.
       * @return  nothing.
       * @see     AntennaAPI.html
       */
      void setAZ (double az)
	throw (antenna::FrozenException); // when it snows, for example

     /** Sets the elevation of the antenna to an angle specified in radians.
       *
       * @pre     antenna is not in collision, locked down (stowed), or
       *          in some other error state that precludes it from being 
       *          moved.
       * @post	  antenna elevation is equal to the angle specified as input.
       * @param el  of type double, representing the angle to which 
       *          antenna elevation has to move.
       * @exception FrozenException raised if antenna is locked down or 
       *            cannot move from some reason.
       * @return  nothing.
       */
      void setEL (double el)
	throw (antenna::FrozenException);

     /** Tracks that portion of the sky pointed to by the antenna.
       *
       * @pre     antenna is not in collision, locked down (stowed), or
       *          in some other error state that precludes it's dish from 
       *          being moved.
       * @post	  current RA,DEC or current source is tracked across the sky.
       * @exception FrozenException raised if antenna is locked down or 
       *            cannot move from some reason.
       * @return  nothing.
       */
      void startTracking (void)                 // #26
	throw (antenna::FrozenException);

     /** Suspends tracking of the current RA, DEC or source across the sky.
       *
       * @pre     antenna is not in collision, locked down (stowed), or
       *          in some other error state that precludes it from being 
       *          moved.
       * @post	  antenna (dish) is fixed and does not move to track
       *          current RA,DEC or source across the sky.
       * @exception FrozenException raised if antenna is locked down or 
       *            cannot move from some reason.
       * @return  nothing.
       */
      void stopTracking (void)
	throw (antenna::FrozenException);

     /** Returns true if antenna is tracking current RA,DEC or current
       * source across the sky. Else returns false.
       *
       * @pre     None.
       * @post    System state unchanged.
       * @return  true if antenna is tracking current RA, DEC or source,
       *          false if unknown or not tracking.
       */
      bool isTracking (void);                  // #25

    protected:

    private:
     /** Drive object is a handle to the antenna drive.
       * Useful to access azimuth, elevation values and to alter them.
       *
       * @see    Drive.h
       */
      Drive az, el;

     /** az and el are double precision representations of the azimuth
       * and elevation angles of the antenna dish in radians. Updated from
       * the monitor stream.
       *
       * @see    Drive.h
       */
      double ra, dec;

     /** bool tracking is set to true when the dish is tracking the 
       * current RA, DEC (or source), and false otherwise. Updated by methods 
       * setRA, setDEC, point, startTracking and stopTracking. Accessed
       * by isTracking.
       *
       * @see point, setRA, setDEC, isTracking
       */
      bool tracking_;                         // #11 underscore for private class vars

    
    };

    inline double SillyAntenna::getAZ () {  return az.getRadianAngle(); }
    inline double SillyAntenna::getEL () {  return el.getRadianAngle(); }
  };
};


#endif // CARMA_SILLY_ANTENNA_H
