#ifndef CARMA_CORRELATOR_LIB_BANDMANAGER_H
#define CARMA_CORRELATOR_LIB_BANDMANAGER_H

#include <string>
#include <vector>

//! @file
//! 
//! @author Rick Hobbs

namespace carma {
namespace correlator {
namespace lib {


//! @brief Interface for storing and retrieving Correlator Band DO names.
class BandManager {
    public:
        //! @brief Constructor.
        explicit BandManager( ) {  }
        
        //! @brief Destructor.
        virtual ~BandManager( ) {  }
        
        //! @brief add a Band Name
        void addBandName( const ::std::string & name );
        
        //! @brief get number of band names
        int getNumberOfBandNames( ) const;
        
        //! @brief get a vector of band names
        ::std::vector< ::std::string > getBandNames( ) const;
    
    private:
        BandManager( const BandManager & rhs );
        BandManager & operator=( const BandManager & rhs );

        ::std::vector< ::std::string > bandNames_;
}; // End class BandManager


} // End namespace lib
} // End namespace correlator
} // End namespace carma


inline void
carma::correlator::lib::BandManager::addBandName( const ::std::string & name )
{
    bandNames_.push_back( name );
}


inline int
carma::correlator::lib::BandManager::getNumberOfBandNames( ) const
{
    return bandNames_.size();
}


inline ::std::vector< ::std::string >
carma::correlator::lib::BandManager::getBandNames( ) const
{
    return bandNames_;
}


#endif
