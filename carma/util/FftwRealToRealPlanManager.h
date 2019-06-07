/**
 * @file
 * Declaration of FftwRealToRealPlanManager class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.1 $
 * $Date: 2011/04/11 20:39:43 $
 * $Id: FftwRealToRealPlanManager.h,v 1.1 2011/04/11 20:39:43 abeard Exp $
 */
#ifndef CARMA_UTIL_FFTWREALTOREALPLANMANAGER_H
#define CARMA_UTIL_FFTWREALTOREALPLANMANAGER_H

#include "carma/util/FftwRealToRealPlan.h"

#include <map>

namespace carma {

namespace util {

    /** 
     * Facilitate the creation, storage and retrieval of FftwRealToRealPlans.
     * The idea is to create all perceived necessary Fftw plans at the outset 
     * and then retrieve them on the fly when needed.  Plans can be created
     * on the fly as well but with a noticeable performance hit.
     * Note this class is not threadsafe.
     */
    class FftwRealToRealPlanManager {
    public:

        /**
         * Constructor.
         * @param maxSize The maximum sized plan this class will hold. 
         */
        explicit FftwRealToRealPlanManager( FftwRealVector::size_type maxSize );

        /** 
         * Copy constructor.
         */
        FftwRealToRealPlanManager( const FftwRealToRealPlanManager & from );

        /**
         * Assignment operator.
         */
        FftwRealToRealPlanManager & 
        operator=( const FftwRealToRealPlanManager & from );

        /**
         * Destructor.
         */
        /* virtual */ ~FftwRealToRealPlanManager( );

        /**
         * Retrieve an FftwRealToRealPlan for a given size and kind.
         * If a plan doesn't exist, it is created on the fly.
         * @param size Size of plan to retrieve.
         * @param kind Kind of plan to retrieve.
         * @throw ErrorException if size > maxSize
         */
        FftwRealToRealPlan & retrievePlan( FftwRealVector::size_type size,
                                           FftwRealToRealPlan::Kind kind ); 

        /**
         * Explicitly create and store a new plan of a given type and size.
         * This is just a convenient way to explicitly create a specified
         * plan.  Retrieve plan behaves identically in the event that the 
         * plan doesn't already exist.
         * @param size Size of plan to create.
         * @param kind Kind of plan to create.
         * @throw ErrorException if size > maxSize
         */
        FftwRealToRealPlan & createPlan( FftwRealVector::size_type size,
                                         FftwRealToRealPlan::Kind kind );
        
        /**
         * Retrieve vector.
         * Note that the same rules apply to this vector as noted in 
         * FftwRealToRealPlan docs (i.e. don't force reallocation).
         */
        FftwRealVector & getFftwRealVector( );

    private:
        
        typedef std::multimap< FftwRealVector::size_type, FftwRealToRealPlan > 
            PlanMultimap;     

        const FftwRealVector::size_type maxSize_;
        FftwRealVector data_;
        PlanMultimap plans_;

    }; // FftwRealToRealPlan
        
} // namespace util

} // namespace carma
#endif
