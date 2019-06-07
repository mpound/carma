
/**
 * @file 
 *
 * $Id: Selfcal.h,v 1.13 2007/05/02 15:21:48 teuben Exp $
 *
 * @author Peter Teuben
 *
 */

#ifndef CARMA_SERVICES_SELFCAL_H
#define CARMA_SERVICES_SELFCAL_H

#include <complex>
#include <vector>


namespace carma {
  namespace services {

    /**
     * @typedef Complex
     * convenient definition of a complex number. Notice the current code
     * does not support a complex<float> style, neither is this adviced.
     * @todo it would be useful to fix code for complex<float>
     *
     */
    typedef std::complex<double> Complex;


    /**
     * Computes an amplitude or phase selfcal solution on 
     * a set of visibilities. It also computes some statistics
     * to determine a goodness-of-fit.
     * The code - at least at its inception - 
     * produced identical results to the MIRIAD equivalent
     * routines in selfcal.for.
     */

    class Selfcal {

    public:

      /**
       * Constructor
       */
      Selfcal();
      

      virtual ~Selfcal();

      /**
       * reset the accumulators for a new solution
       */
      void zero(void);

      /**
       * set sourcename
       * @param sourceName
       */
      void setSourceName(std::string& sourceName);

      /**
       * set the reference antenna. The default (0) signifies that
       * the one with the greatest weight is taken. A valid refAnt
       * needs to be a 1..maxAnt.
       * @param refAnt
       */
      void setReferenceAntenna(int refAnt = 0);

      /** set the maximum number of iterations. Code default is 100.
       *
       *  @param maxIter 
       */
      void setMaxIter(int maxIter = 100);

      /** set the maximum number of antennae. If you set this,
       *  the array of gains will always be of this length,
       *  instead of a length of the largest found antenna
       *  in the input visibilities that were entered via setVis()
       *  The default value (0) will cause it to be determined
       *  automatically.
       *
       *  @param  maxAnt   the max number of ants requested in returns gain
       */
      void setMaxAnt(int maxAnt = 0);

      /**
       *  set the relative accuracy levels that need to be achieved
       *  in the jacobi iteration.
       *
       *  @param epsi1 phase loop. 
       *  @param epsi2 amplitude loop 
       */
      void setEps(double epsi1 = 1.0e-8, double epsi2 = 1.0e-4);

      /**
       *  set a debug level (0=nothing, 1=a bit, 2=more)
       *
       *  @param debug
       */
      void setDebug(int debug = 0);

      /**
       *  set the flux of a point source model 
       *
       *  @param flux
       */
      void setPointSourceModel(double flux);

      /**
       *  set the flux and size of a (uniform disk) planet.
       *  The code has no slots for antenna coordinates, and would need this. 
       *  Note this routine has not been implemented.
       */
      void setPlanetDiskModel(double flux, double bmaj=0, double bmin=0, double bpa=0);

      /**
       * accumulate visibilities. It will automatically reject those whose 
       * antennae number is above maxAnt (if set via setMaxAnt).
       *
       * @param ant1     antennae number (1..maxAnt)
       * @param ant2     antennae number (1..maxAnt)
       * @param vis      complex visbility
       * @param weight   weight (as a 1/sigma^2 factor)
       */
      void setVis(int ant1, int ant2, const Complex& vis, double weight=1);
      
      /**
       * Return antenna based visibilities (gains really)
       * The returned array will have a length that contains the maximum 
       * antenna number, but visibilities for antennae that were missing in 
       * the input visibilities are set to 0 on output. It is also
       * possible to use setMaxAnt() and always get a predictable length.
       * If convergence failed, the vector will be of length 0.
       * Visibilities are defined such that |g_i * g_j - V_ij| is minimized
       *
       *
       * @param useAmp    if true, return an amplitude selfcal, else phase only.
       *
       */
      std::vector<Complex> getVis(bool useAmp = true);

      /**
       * Return visibility errors for all inputs.
       * If getVisErrors() is called before getVis(), unpredictable
       * things can happen.  
       * Also note that errors will be arbitrarely seto to 1 for those
       * antennae that were missing.
       */

      std::vector<Complex> getVisErrors(void);

      /**
       * get phase RMS error of the current solution
       *
       * @return RMS error in degrees
       */
      double getRMSPhase(void) const {
	return RMSPhase_;
      }

      /**
       * get amplitude RMS error of the current solution
       *
       * @return RMS error in whatever units amplitudes were given
       */
      double getRMSAmp(void) const {
	return RMSAmp_;
      }

      /**
       * get the number of iterations it took to get a solution
       *
       * @return number of iterations
       */
      double getIter(void) const {
	return Niter_;
      }

      /**
       * get the reference antenna. Useful when it was left at 0 to find
       * the one with the greatest weight.
       *
       * @return reference antenna, in the range  1..maxAnt
       */
      double getRefAnt(void) const {
	return refAnt_;
      }

      /**
       * get the maximum numbers of antennae. Useful when it was left at 0 to find
       * the largest one. It is also equal to the length of the returned gains
       * in getGains()
       *
       * @return maximum antenna number (1 or larger)
       */
      double getMaxAnt(void) const {
	return maxAnt_;
      }

    private:

      /*
       * Vis: a convenient container to keep all the baseline based visibilities
       *      together
       */

      typedef struct {
          Complex v;  // data
          Complex m;  // model
          double w;   // weight, typically contains 1/sigma^2, systemp, inttime
          int a1;     // a number 0...Nants-1  !!!
          int a2;     // a number 0...Nants-1 (but not equal to ant1)
          Complex sumvm;    // derived quantities, used internally
          Complex sumvmc;   // derived quantities, used internally 
          double sumvv;     // derived quantities, used internally 
      } Vis;

      void visset(Vis &data, const Complex &v, const Complex &m, double w, int a1, int a2);
      int guess_nants(int Nbaselines);
      std::vector<Complex> phasol(const std::vector<Vis> &data);
      std::vector<Complex> amphasol(const std::vector<Vis> &data);
      void computeRMS(const std::vector<Complex> &gains);

      /*
       *   Control parameters for selfcal..
       */

      double epsi1_;
      double epsi2_;
      int  maxIter_;
      std::string sourceName_;
      int debug_;
      double flux_;

      bool autoRef_;               // automatically find reference antenna
      int  refAnt_;                // reference antenna 

      /*
       * index for fast antenna lookup 
       *
       */

      bool autoAnt_;                // if true, automatically determine maxAnt_
      int  maxAnt_;                 // the maximum ant number (1..) used in the input 
      std::vector<int> antIndex_;   // index array into 0s or filled antenna number (Nant)


      /*
       * the visibility Data
       */
      std::vector<Vis> data_; 

      /*
       * Computed, and cashed until needed via the getXXX() routines
       */

      std::vector<Complex> Verrors_;
      double RMSPhase_;
      double RMSAmp_;
      int    Niter_;
      bool   solutionFailed_;
      
    }; // end class Selfcal

  } // end namespace services
} // end namespace carma


#endif
