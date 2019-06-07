#ifndef CARMA_CORRELATOR_OBSRECORD2_BANDSTATUS_H
#define CARMA_CORRELATOR_OBSRECORD2_BANDSTATUS_H

//! @file
//! @brief Interface file for the BandStatus class.

#include "carma/corba/corba.h"
#include "carma/correlator/obsRecord2/CorDataBase.h"
#include "carma/util/PthreadRWLock.h"

#include <map>
#include <string>


namespace carma {
namespace correlator {
namespace obsRecord2 {


//! @brief Singleton class used to hold a map of connected Correlator DO bands.
class BandStatus {
    public:
        //! @brief Get pointer to Singleton class
        static BandStatus * getInstance( );
        
        //! @brief Add a valid DO.
        //! @throws ErrorException("DO name already exists. Delete it first")
        void addCorrelatorDO(
            const std::string &                             name,
            carma::correlator::obsRecord2::Correlator_I_var doObject );
        
        //! @brief Remove a DO.
        //! @throws ErrorException("No such DO name");
        void removeCorrelatorDO( const ::std::string & name );
        
        //! @brief Get a valid correlator DO.
        //! @throws ErrorException("No such DO name");
        carma::correlator::obsRecord2::Correlator_I_var
        getCorrelatorDO( const ::std::string & name ) const;
        
    private:
        BandStatus( );

        virtual ~BandStatus( );
        
        typedef ::std::map< ::std::string,
                            carma::correlator::obsRecord2::Correlator_I_var >
                DOmap;
         
        mutable util::PthreadRWLock guard_;
        DOmap                       bands_;
};


} // End namespace obsRecord2
} // End namespace correlator
} // End namespace carma



#endif
