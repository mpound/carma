/**
 * @file
 * Definition for FftwRealToRealPlan class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.3 $
 * $Date: 2011/12/21 22:57:06 $
 * $Id: FftwRealToRealPlan.h,v 1.3 2011/12/21 22:57:06 mpound Exp $
 */
#ifndef CARMA_UTIL_FFTWREALTOREALPLAN_H
#define CARMA_UTIL_FFTWREALTOREALPLAN_H

#include "carma/util/FftwAllocator.h"

#include <fftw3.h>

#include <vector>
#include <map>
#include <string>

namespace carma {

namespace util {
        
    typedef 
    std::vector< double, carma::util::FftwAllocator< double > > FftwRealVector;

    /**
     * C++ wrapper of real-to-real half-complex fftw_plans.
     * The FFTW package uses an 'fftw_plan' to calculate an fft.  The fftw
     * paradigm is to create a plan for each type of fft you will be 
     * doing (e.g. a 32 point, 1-dimensional forward real to complex fft), 
     * store it away somewhere and then call the execute( plan ) routine when 
     * you want to perform the actual fft.  Plans can be reused as often as
     * needed.
     *
     * Unlike fftw this class normalizes on reverse transforms.
     * 
     * The FftwRealToRealPlanManager class is useful for managing a
     * large collection of plan size and types married to a single vector. 
     */ 
    class FftwRealToRealPlan {
    public:

        /**
         * Fftw supports many different kinds of real-to-real transforms,
         * these are the ones supported by this wrapper.  Others can be 
         * easily added in the future.  See the fftw manual for a description
         * of the halfcomplex format.
         */ 
        enum Kind { // Supported plan kinds 
            REAL_TO_HALFCOMPLEX,
            HALFCOMPLEX_TO_REAL 
        };

        /**
         * Fftw has several options for creating plans which substantially 
         * impact creation time but can also have a significant impact
         * on fftw execution performance.  Care should be taken to use
         * a planning rigor appropriate for the performance needed at 
         * plan creation time.  Once calculated, plan 'wisdom' can be
         * saved and reused to avoid the one-time cost of creation.
         * Note that by default, we always attempt to use wisdom first.
         * @see importWisdom
         * @see exportWisdom
         * @see forgetWisdom
         */
        enum Rigor { // Supported plan creation rigors.
            ESTIMATE,  // Estimate the best plan via heuristics 
            MEASURE,   // Determine best plan by measuring several ffts 
            PATIENT,   // Same as measure, but tests more algorithms 
            EXHAUSTIVE // Same as patient, but tests even more algorithms 
        };

        typedef ::std::map< enum Rigor, ::std::string > RigorEnumMap;

        static RigorEnumMap rigors(); // Convenience for looping & debug

        /**
         * Create an FftwRealToRealPlan. 
         * Once created an Fftw plan is forever married to the input
         * data vector (as it is with the core fftw interface), and hence the 
         * vector must not reallocate underlying storage. Vector reallocation 
         * is checked for internally and an exception will be thrown if  
         * reallocation is detected.  Note that reallocation is not the same 
         * as resizing - you can resize the vector and reuse it with other 
         * instances of FftwRealToRealPlan, just make sure to reserve enough 
         * space with vector.reserve() prior to instantiating all plans. 
         *
         * The specified rigor dramatically impacts plan creation time (the
         * most rigorous can take many seconds).  This can be minimized
         * by taking advantage of saved wisdom (@see importWisdom). If a 
         * more rigorous plan already exists in wisdom it will be used 
         * instead of the specified rigor.
         * 
         * @param size Number of input points to transform.
         * @param data Reference to preallocated data vector.
         * @param transformKind Type of transform.
         * @param minimumRigor Specifies how rigorously to optimize plan. 
         *
         * @throw ErrorException if input data vector is empty.
         */
        explicit FftwRealToRealPlan( FftwRealVector::size_type size,
                                     FftwRealVector & data,
                                     enum Kind transformKind,
                                     enum Rigor minimumRigor = MEASURE );

        /**
         * Copy constructor.
         * This creates a new fftw plan and can be a large performance hit
         * if wisdom was forgotten (e.g. forgetWisdom called).
         */
        FftwRealToRealPlan( const FftwRealToRealPlan & from );
        
        /**
         * Destructor.
         */
        ~FftwRealToRealPlan( );

        /**
         * Execute transform for this plan.
         * @throw ErrorException if vector reallocation is detected or size
         * has been trimmed.
         */
        void execute( );

        /**
         * Retrieve size.
         */
        FftwRealVector::size_type getSize( ) const;

        /**
         * Retrieve kind.
         */
        enum Kind getKind( ) const;

        /**
         * Import Fftw wisdom from the given file.
         * Fftw wisdom is persistent optimization information for plans.  
         * Each time a plan is created, wisdom is accumulated and stored in
         * global memory.  This information can then be exported or imported
         * to facilitate quick plan creation in the future.  Using the most 
         * rigorous plan creation modes can take many seconds so this is a 
         * useful optimization if first time plan creation latency is a problem.
         * @param filename Name of file to import wisdom from.
         * @throw FileNotFoundException 
         * @throw ErrorException 
         * @see exportWisdom
         * @see forgetWisdom
         */
        static void importWisdom( const ::std::string & filename );

        /**
         * Export wisdom to the given file.
         * @param filename Name of file to export wisdom to.
         * @ErrorException on file error.
         * @see importWisdom
         * @see forgetWisdom
         */
        static void exportWisdom( const ::std::string & filename );

        /**
         * Forget all wisdom.
         * Useful for creating fresh new wisdom (contradiction in terms?).
         * @see importWisdom
         * @see seeWisdom
         */
        static void forgetWisdom();

    private:

        const FftwRealVector::size_type size_;
        const FftwRealVector::value_type * data0_; // For realloc checks
        const Kind kind_;
        const Rigor rigor_;
        FftwRealVector & data_;
        ::fftw_plan plan_;
    };
} // namespace util
} // namespace carma
#endif

