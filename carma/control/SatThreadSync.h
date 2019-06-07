#ifndef CARMA_CONTROL_SAT_THREAD_SYNC_H
#define CARMA_CONTROL_SAT_THREAD_SYNC_H

//!
//! @file
//!
//! @brief Interface file for the
//!        carma::control::SubarrayControlImpl::TrackerThreadSync class.
//!
//! $CarmaCopyright$
//!

#include "carma/control/SubarrayControlImpl.h"


namespace carma {
namespace control {


class SubarrayControlImpl::TrackerThreadSync {
    public:
        explicit TrackerThreadSync( SubarrayControlImpl & sacImpl );
    
        virtual ~TrackerThreadSync( );
        
    private:
    
        const ::boost::recursive_mutex::scoped_lock scopedLock_;
};


}  // namespace carma::control
}  // namespace carma


#endif
