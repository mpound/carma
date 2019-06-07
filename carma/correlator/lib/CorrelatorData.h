/**
 * @file CorrelatorData.h
 * 
 * @author Rick Hobbs
 */

#ifndef CORRELATORDATA_H
#define CORRELATORDATA_H

#include "carma/correlator/lib/CorrelatorBand.h"
#include "carma/correlator/lib/CorrelatorHeader.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/Serializable.h"

#include <vector>

namespace carma {
namespace correlator {
namespace lib {

      class CorrelatorConfigChecker;

      /**
       * Class used to represents bands of Correlator Data
       */
      class CorrelatorData : public carma::util::Serializable {
      public:
        // need this to allow the use of overloaded method deserialize
        using carma::util::Serializable::deserializeVer0;
	using carma::util::Serializable::deserializeVer1;

        class ScopedRef;

        /**
         * Constructor.
         */
        explicit CorrelatorData( );

        /**
         *  Copy Constructor
         */
        CorrelatorData( const CorrelatorData & rhs );

        /**
         * Destructor.
         */
        virtual ~CorrelatorData( );

        /**
         *  Assignment
         */
        CorrelatorData & operator=( const CorrelatorData & rhs );

        /**
         * Used to serialize data
         */
        void mySerialize( char * byteArray, int * offset ) const;

        /**
         * Return size in bytes of object
         */
        int getSizeInBytes( ) const;

        /**
         * Used to reconstruct object. typically called on serializable
         * members of a class. Must be defined!
         */
        void deserializeVer0( const char * byteArray, int * offset, 
                              int byteArraySize );
        void deserializeVer1( const char * byteArray, int * offset, 
                              int byteArraySize );
        /**
         * Used to reconstruct object. typically called on serializable
         * members of a class. Must be defined!
         */
        void deserializeSwapVer0( const char * byteArray, int * offset,
                                  int byteArraySize );
        void deserializeSwapVer1( const char * byteArray, int * offset,
                                  int byteArraySize );

        /**
         * Get number of Correlator Bands
         */
        int getNumberOfBands( ) const;

        /**
         * Add a Correlator Band
         */
        void addBand( const carma::correlator::lib::CorrelatorBand & band );

        //! @brief Add a correlator band using a swap
        //!
        //! The passed in band will probably come back as the equivalent
        //! of a default constructed CorrelatorBand if the add succeeds.
        //! This way of adding a band avoid expensive memory copies when the
        //! CorrelatorBand instance being added can afford to donate its
        //! memory/members
        void addBandViaSwap( carma::correlator::lib::CorrelatorBand & band );

        /**
         * Get Correlator Bands
         */
        const std::vector<carma::correlator::lib::CorrelatorBand>&
          getBands() const;

        std::vector<carma::correlator::lib::CorrelatorBand> & getBands( );

        /**
         * Get a single Correlator Band
         * bandNumber is the number assigned to this band.
         * @throw NotFoundException if bandNumber missing
         */
        const carma::correlator::lib::CorrelatorBand &
          getBand(int bandNumber) const;

        carma::correlator::lib::CorrelatorBand &
          getBand( int bandNumber );

        bool hasBand( int bandNumber ) const;

        /**
         * Get Correlator Header
         */
        const carma::correlator::lib::CorrelatorHeader & getHeader( ) const;

        carma::correlator::lib::CorrelatorHeader & getHeader( );

        /**
         * Set Correlator Header
         */
        void setHeader( const carma::correlator::lib::CorrelatorHeader & h );

        void setHeaderMJD( double mjd );
        void setHeaderTransmissionMJD( double mjd );
        void setHeaderReceivedMJD( double mjd );
        
        /**
         *  Add a CorrelatorData into this one
         */
        void addIn( const CorrelatorData & rhs );

        /**
         * Add a CorrelatorBand into this one. 
         * If the band doesn't already exist in the correlator object, 
         * it is created and added in.  If it does, it's just added.
         */
        void addIn( const CorrelatorBand & band );

        /**
         * Add a CorrelatorBand but ignore duplicates.
         * If the band already exists and contains baselines which already
         * exist, ignore the duplicates and report false. Otherwise
         * return true.
         */
        bool addInIgnoringDups( const CorrelatorBand & band );

        /**
         *  Normalize data. Normally used after summing together
         *  a number of data records.
         */
        void normalize();

        /**
         *  Increment the reference count
         */
        void incrementRefCount( );

        /**
         *  Decrement the reference count
         *  @throw CARMA_ERROR if refCount < 0
         */
        void decrementRefCount( );

        bool incrementRefCountIfZero( );
        
        /**
         *  Return the refCount
         */
        int getRefCount( ) const;

        void reserveBands( size_t numBands );

        void setBandMJD( int bandNumber, double mjd );

        bool baselineCountsPhysicallyValid( ) const;

      private:
        CorrelatorBand & getBandNonconst( int bandNumber );

        int refCount_; // this variable is NOT serialized
        mutable carma::util::PthreadMutex refCountGuard_; // this variable is NOT serialized
        carma::correlator::lib::CorrelatorHeader header_;
        std::vector<carma::correlator::lib::CorrelatorBand> band_;
      }; // End class CorrelatorData

} // End namespace lib
} // End namespace correlator
} // End namespace carma


class carma::correlator::lib::CorrelatorData::ScopedRef {
    public:
        explicit ScopedRef( CorrelatorData & cd );
        
        /* virtual */ ~ScopedRef( );
        
    private:
        ScopedRef( const ScopedRef & rhs );
        ScopedRef & operator=( const ScopedRef & rhs );

        CorrelatorData & cd_;
};


inline
carma::correlator::lib::CorrelatorData::CorrelatorData( ) :
refCount_( 1 ),
refCountGuard_(),
header_(),
band_()
{
}


inline
carma::correlator::lib::CorrelatorData::CorrelatorData(
    const carma::correlator::lib::CorrelatorData & rhs ):
refCount_( 1 ),
refCountGuard_(),
header_( rhs.header_ ),
band_( rhs.band_ )
{
}


inline
carma::correlator::lib::CorrelatorData::~CorrelatorData( )
{
}


inline carma::correlator::lib::CorrelatorData &
carma::correlator::lib::CorrelatorData::operator=(
    const carma::correlator::lib::CorrelatorData & rhs )
{
    if ( &rhs != this ) {
        // Pretty sure we don't want to copy the refCount_ - TWC 30 July 2007
        // refCount_ = rhs.refCount_;
        header_ = rhs.header_;
        band_ = rhs.band_;
    }

    return *this;
}


inline int
carma::correlator::lib::CorrelatorData::getNumberOfBands( ) const
{
    return band_.size();
}



inline const carma::correlator::lib::CorrelatorHeader &
carma::correlator::lib::CorrelatorData::getHeader( ) const
{
    return header_;
}

inline carma::correlator::lib::CorrelatorHeader &
carma::correlator::lib::CorrelatorData::getHeader( ) 
{
    return header_;
}


inline const ::std::vector< carma::correlator::lib::CorrelatorBand > &
carma::correlator::lib::CorrelatorData::getBands( ) const
{
    return band_;
}

inline ::std::vector< carma::correlator::lib::CorrelatorBand > &
carma::correlator::lib::CorrelatorData::getBands( )
{
    return band_;
}


inline void
carma::correlator::lib::CorrelatorData::setHeader(
    const carma::correlator::lib::CorrelatorHeader & h )
{
    header_ = h;
}


inline void
carma::correlator::lib::CorrelatorData::setHeaderMJD( const double mjd )
{
    header_.setMJD( mjd );
}


inline void
carma::correlator::lib::CorrelatorData::setHeaderTransmissionMJD(
    const double mjd )
{
    header_.setTransmissionMJD( mjd );
}

inline void
carma::correlator::lib::CorrelatorData::setHeaderReceivedMJD(
    const double mjd )
{
    header_.setReceivedMJD( mjd );
}

inline void
carma::correlator::lib::CorrelatorData::reserveBands( const size_t numBands )
{
    band_.reserve( numBands );
}


inline
carma::correlator::lib::CorrelatorData::ScopedRef::ScopedRef(
    CorrelatorData & cd ) :
cd_( cd )
{
    cd_.incrementRefCount();
}


inline
carma::correlator::lib::CorrelatorData::ScopedRef::~ScopedRef( )
try {
    cd_.decrementRefCount();
} catch ( ... ) {
    // Just stifle any exceptions
    
    return;
}


#endif
