#ifndef CARMA_CORRELATOR_LIB_CORRELATORLISTENER_H
#define CARMA_CORRELATOR_LIB_CORRELATORLISTENER_H

#include <string>

//! @file CorrelatorListener.h
//!
//! @author Rick Hobbs

namespace carma {
namespace correlator {
namespace lib {

class CorrelatorData;

//! @brief Interface for those wishing to receive notification of
//!        arriving Correlator Data.
class CorrelatorListener {
    public:
        //! @brief Destructor
        virtual ~CorrelatorListener( ) { }
        
        //! @brief Called with Correlator Data Arrives.
        virtual void
        processData( carma::correlator::lib::CorrelatorData * cd ) = 0;
        
        //! @brief Return class name
        virtual const ::std::string & getName( ) const = 0;
}; // End class CorrelatorListener

}  // End namespace lib
}  // End namespace correlator
}  // End namespace carma

#endif
