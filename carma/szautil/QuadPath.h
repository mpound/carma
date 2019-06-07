#ifndef SZA_UTIL_QUADPATH_H
#define SZA_UTIL_QUADPATH_H

/**
 * @file QuadPath.h
 * 
 * Tagged: Thu Nov 13 16:53:49 UTC 2003
 * 
 * @author Erik Leitch
 */
// Include the C-style struct and method definitions this class is
// based on.

#include "carma/szaarrayutils/quad.h"
#include "carma/szaarrayutils/szaconst.h" // pi and twopi

namespace sza {
  namespace util {
    
    /**
     * A class to handle quadrature interpolation of ephemerides
     * received from the control program.  
     *
     * This class is a wrapper around the C-style struct QuadData
     * which is required by certain sza::array functions.
     */
    class QuadPath {
      
    public:
      
      /**
       * Enumerate the various types of ephemeris types we might handle.
       */
      enum QuadType {
	/**
	 * A continuous function.
	 */
	QP_NORMAL,
	
	/**
	 * Angles defined modulo 2.pi between -pi <= v < pi.
	 */
	QP_SIGNED_ANGLE,
	
	/**
	 * Angles defined modulo 2.pi between 0 <= v < 2.pi.
	 */
	QP_POSITIVE_ANGLE
      };
      
      /**
       * The following object type is used to query or replace the
       * current contents of a QuadPath object.
       */
      struct QuadData {
	
	/**
	 * The C-struct this is based on.
	 */	
	sza::array::QuadData quaddata_;
	
	/**
	 * Empty a QuadData object and set all of its sample values
	 * to 0.
	 */
	void init();
      };
      
      /**
       * Constructor function.
       *
       *  @param empty_value  double  The value to return until at least one
       *                              coordinate pair has been added.
       *  @param type QuadType  The type of ordinate being interpolated.
       
       * @param
       * @throws Exception
       */
      QuadPath(double empty_value, QuadType type);
      
      /**
       * Destructor.
       */
      ~QuadPath();
      
      /**
       * Query the contents of a QuadPath object.
       *
       * @throws Exception
       */
      void get(QuadData* data);
      
      /**
       * Set the contents of a QuadPath object.
       *
       * Each time a new set of samples is received by this
       * function, the three quadratic polynomial coefficients a,b,c
       * (ie. a.x^2+b.x+c) are * recomputed for use by eval().
       *
       * The coefficients are initialized according to the number of
       * entries in the interpolation table. If just one coordinate
       * pair has been entered via this function, eval() returns its
       * y-value irrespective of the target x-value. If two samples
       * have been entered, the coefficients implement linear
       * interpolation of the two coordinate pairs. If all 3 points
       * are entered, the three coefficients implement a quadratic
       * interpolation.
       *
       * @throws Exception
       */
      void set(QuadData* data);
      
      /**
       * Empty the coordinate table of a QuadPath object.
       *
       * After this call the value of the empty_value argument that
       * was presented to QuadPath::QuadPath() will be returned
       * until extend() is next called.
       *
       * @throws Exception
       */
      void empty();
      
      /**
       * Append or prepend an x,y coordinate pair to the three-entry
       * circular table of a quadratic interpolation object. Entries
       * are kept in ascending order of x, so if the new x value is
       * larger than any currently in the table, it will be
       * appended, and if it is smaller it will be prepended. If
       * there are already three entries in the table the one at the
       * other end of the table will be discarded and the table
       * rotated over it to make room for the new sample.
       *
       * If the new x value is within the range of x values already
       * covered by the table, the interpolator will be left
       * unchanged.
       *
       * Each time a new entry is added, the three quadratic
       * polynomial coefficients a,b,c (ie. a.x^2+b.x+c) are
       * recomputed for use by eval().
       *
       * The coefficients are initialized according to the number of
       * entries in the interpolation table. After just one
       * coordinate pair has been entered via this function, eval()
       * returns its y-value irrespective of the target
       * x-value. After a second point has been * added, the
       * coefficients implement linear interpolation of the two *
       * coordinate pairs. After 3 or more points have been added,
       * the three * coefficients implement a quadratic
       * interpolation of the last three * points entered.
       *
       * Note that calls to this function and set_QuadPath() can be
       * interleaved.  In fact this function itself calls
       * set_QuadPath().
       *
       *  @param x double The X coordinate of the point to add.
       *  @param y double The Y coordinate of the point to add.
       */
      void extend(double x, double y);
      
      /**
       * Return the value of the quadratic equation at x.
       */
      double eval(double x);
      
      /**
       * Return the gradient of the quadratic equation at x.
       */
      double grad(double x);
      
    private:
      
      /**
       * This class is really just a wrapper around
       * sza::array::QuadPath.
       */
      sza::array::QuadPath* quadpath_;
      
      // Private methods
      
      /**
       * Given two angles A and B within the same 2.pi interval,
       * return whichever of B-2.pi, B, B+2.pi is within pi of A.
       */
      double extendAngle(double a, double b);
      
      /**
       * Wrap an angle into the range -pi <= v < pi.
       */
      QP_ANGLE_FN(angle_around_zero);
      
      /**
       * Wrap an angle into the range 0 <= v < 2.pi.
       */
      QP_ANGLE_FN(angle_around_pi);
      
    }; // End class QuadPath
    
  }; // End namespace util
}; // End namespace sza

#endif
