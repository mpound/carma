#ifndef SZA_UTIL_QUADRATICINTERPOLATOR_H
#define SZA_UTIL_QUADRATICINTERPOLATOR_H

/**
 * @file QuadraticInterpolator.h
 * 
 * Started: Mon Dec 15 23:34:51 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <pthread.h>

/**
 * Functions of the following type are used to wrap an angle into a
 * given 2.pi interval.
 */
#define QP_ANGLE_FN(fn)  double (fn)(double angle)

namespace sza {
  namespace util {
    
    /**
     * The QuadraticInterpolator class is used to perform quadrature
     * interpolation of arbitrary continuous functions, as well as
     * certain periodic functions (angles).
     *
     * This class will store up to three (x,y) pairs of a function to
     * be interpolated, and provide a public interface to return the
     * value and gradient of the function at arbitrary x, through the
     * eval() and grad() methods.  The behaviors of these methods
     * under different conditions are described in the documentation
     * below.
     *
     * The class is initialized with a value to be returned until
     * valid samples of a function have been loaded, as well as the
     * type of function being interpolated.
     *
     * Samples are loaded using the extend() method (see documentation
     * below).  As new samples are installed, they are appended or
     * prepended to an internal circular buffer of samples, according
     * to the x values of the pairs.
     *
     * At a discontinuity (say, when preparing to interpolate the
     * ephemerides of a new source), the user should discard old
     * samples via the empty() method.
     */
    class QuadraticInterpolator {
      
    public:

      /**
       * Define constants
       */
      static const double pi_;
      static const double twopi_;
      
      /**
       * Enumerate the various types of ephemeris types we might
       * handle.
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
      
      //------------------------------------------------------------
      // Public methods
      //------------------------------------------------------------
      
      /**
       * Destructor.
       */
      virtual ~QuadraticInterpolator();
      
      /**
       * Empty the coordinate table of a QuadraticInterpolator object.
       *
       * After this call the value of the empty_value argument that
       * was presented to
       * QuadraticInterpolator::QuadraticInterpolator() will be
       * returned until extend() is next called.
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
       * entries in the interpolation table. After just one coordinate
       * pair has been entered via this function, eval() returns its
       * y-value irrespective of the target x-value. After a second
       * point has been added, the coefficients implement linear
       * interpolation of the two coordinate pairs. After 3 or more
       * points have been added, the three coefficients implement a
       * quadratic interpolation of the last three points entered.
       *
       * Note that calls to this function and set() can be
       * interleaved.  In fact this function itself calls
       * set().
       *
       *  @param x double The X coordinate of the point to add.
       *  @param y double The Y coordinate of the point to add.
       */
      void extend(double x, double y);
      
      /**
       * Return the value of the function at x.
       */
      double evaluate(double x);
      
      /**
       * Return the gradient of the function at x.
       */
      double gradient(double x);

      /**
       * A public method to lock this container
       */
      void lock();

      /**
       * A public method to unlock this container
       */
      void unlock();

      /**
       * A public method to attempt to lock this container.
       * Returns true if the lock was successful.
       */
      bool tryLock();

      /**
       * A method to query if the container can bracket the requested
       * value
       */
      bool canBracket(double x);

      /**
       * A method to query the number of points currently in our
       * interpolation container.
       */
      unsigned getNpt();

      /**
       * A method to query the minimum x-value in our interpolation
       * container.
       */
      double getXmin();

      /**
       * A method to query the maximum x-value in our interpolation
       * container.
       */
      double getXmax();

    protected:

      /**
       * The type of ordinate we are interpolating.
       */
      QuadType type_;

      /**
       * Constructor function.  Making this protected prevents
       * instantiation of the base class.
       *
       *  @param emptyValue The value to return until at least one
       *                    coordinate pair has been added.
       *
       *  @throws Exception
       */
      QuadraticInterpolator();
      
      /**
       * A method to set the value to be returned while the
       * interpolation container is empty.
       */
      void setEmptyValue(double emptyValue);

    private:
      
      //------------------------------------------------------------
      // Resources for locking this container
      //------------------------------------------------------------

      /**
       * A flag which is set to true if the mutex has been initialized
       */
      bool mutexIsReady_;

      /**
       * A mutex for locking this container
       */
      pthread_mutex_t mutex_;

      //------------------------------------------------------------
      // Structs used by this class.
      //------------------------------------------------------------

      /**
       * One sample of a function to be interpolated
       */
      struct QuadSample {
        double x;
        double y;
      };
      
      /**
       * The following object type is used to query or replace the
       * current contents of a QuadraticInterpolator object.
       */
      struct QuadData {
        
        /** 
         * The number of samples in x[] and y[] (0..3).  
         *
         * If npt==0 eval(X) always returns the value of the
         * empty_value argument
         *
         * If npt==1 eval(X) always returns x[0],y[0].  
         *
         * If npt==2 eval(X) returns the linear interpolation of
         * x[0..1],y[0..1] at x==X.  
         *
         * If npt==3 eval(X) returns the quadratic
         * interpolation of x[0..1],y[0..1] at x==X.
         */
        int npt;          
        
        /**
         * The npt <= 3 samples of the function being approximated.
         */
        QuadSample s[3]; 
        
        /**
         * A method to initialize this struct
         */
        void init();
      };
      
      //------------------------------------------------------------
      // Private data members
      //------------------------------------------------------------
      
      double emptyValue_;      // The value to use when npt==0 
      double x0_;              // A value which will be subtracted off
			       // of the x-value to maintain accuracy
      double a_,b_,c_;         // The coefficients of the quadratic
                               // equation that interpolates x[],y[]
      QuadSample s_[3];        // The samples to interpolate between 
      int npt_;                // The number of values in x[] and y[]
      
      //------------------------------------------------------------
      // Private methods
      //------------------------------------------------------------

      /** 
       * A function to wrap angles into a particular 2.pi interval.
       *
       * This should be overwritten by inheritors to appropriately
       * deal with different varieties of angle ordinates.  
       *
       * In the base class, this is stubbed out to return the input
       * unmodified.
       */
      virtual double fixAngle(double angle); 

      /**
       * Query the contents of a QuadraticInterpolator object.
       *
       * @throws Exception
       */
      void get(QuadData* data);
      
      /**
       * Set the contents of a QuadraticInterpolator object.
       *
       * Each time a new set of samples is received by this function,
       * the three quadratic polynomial coefficients a,b,c
       * (ie. a.x^2+b.x+c) are recomputed for use by eval().
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
       * Given two angles A and B within the same 2.pi interval,
       * return whichever of B-2.pi, B, B+2.pi is within pi of A.
       */
      double extendAngle(double a, double b);
    };
  };
};

#endif
