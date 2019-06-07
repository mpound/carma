#ifndef CORRELATORBAND_H
#define CORRELATORBAND_H

#include "carma/util/Serializable.h"
#include "carma/correlator/lib/CorrelatorBaseline.h"

#include <boost/foreach.hpp>
#include <vector>

/**
 * @file CorrelatorBand.h
 *
 * @author Rick Hobbs
 */
namespace carma {
  namespace correlator {
    namespace lib {

      class CorrelatorBand;

      typedef ::std::vector< CorrelatorBand > BandVector;

      /**
       * Class to hold a Band of Correlator Data.
       */
      class CorrelatorBand : public carma::util::Serializable {
      public:

        /**
         * Constructor.
         */
        explicit CorrelatorBand();

        /**
         *  Copy constructor
         */
        CorrelatorBand(const CorrelatorBand& cbIn);

        /**
         * Destructor.
         */
        virtual ~CorrelatorBand();

        /**
         * Swap this instance with another CorrelatorBand instance
         */
        void swap( CorrelatorBand & rhs );

        /**
         * Used to serialize data
         */
        void mySerialize( char * byteArray, int * offset ) const;

        /**
         * Used to reconstruct object. Initial value for offset is
         * typically set to zero.
         */
        void deserializeVer0( const char * byteArray, 
                              int * offset, 
                              int byteArraySize );

        void deserializeVer1( const char * byteArray, 
                              int * offset, 
                              int byteArraySize );

        /**
         * Used to reconstruct object. Initial value for offset is
         * typically set to zero. Swap bytes.
         */
        void deserializeSwapVer0( const char * byteArray, 
                                  int * offset,
                                  int byteArraySize );

        void deserializeSwapVer1( const char * byteArray, 
                                  int * offset,
                                  int byteArraySize );

        /**
         *  Return size in Bytes
         */
        int getSizeInBytes() const;

        /**
         * Set MJD for this Band
         */
        void setMJD(double mjd);

        /**
         * Get MJD for this Band
         */
        double getMJD() const;

        /**
         * Get Band Number.
         */
        int getBandNumber() const;

        /**
         * Set Band Number.
         */
        void setBandNumber(int bandNumber);

        /**
         *  true if in selftest mode.
         */
        bool isSelfTest() const;

        /**
         *  Set selftest mode.
         */
        void setSelfTest(bool selftest);

        /**
         *  true if in simulation mode.
         */
        bool isSimulation() const;

        /**
         *  Set simulation mode.
         */
        void setSimulation(bool sim);

        /**
         * Get sequence number.
         */
        long getSequenceNumber() const;

        /**
         *  Set sequence number.
         */
        void setSequenceNumber(long sn);

        /**
         *  set bandwidth in MHz
         */
        void setBandwidth(float bandwidth);

        /**
         *  get bandwidth in MHz
         */
        float getBandwidth() const;

        /**
         *  Get number of Inputs
         */
        int getNumberOfInputs() const;

        /**
         *  Get number of Baselines.
         */
        int getNumberOfBaselines() const;

        /**
         *  Set number of Inputs
         */
        void setNumberOfInputs(int na);

        /**
         *  true if band is valid.
         */
        bool isValid() const;

        /**
         * set band validity.
         */
        void setValid( bool v, 
                       unsigned int reason = CorrelatorSideband::NO_REASON );

        /**
         * Does band contain baseline?
         */
        bool hasBaseline( int input1Number,
                          int input2Number ) const;

        /**
         *  Add a baseline object to this band.
         */
        void addBaseline( const CorrelatorBaseline & baseline );

        /**
         *  Remove a baseline object from this band.
         */
        void removeBaseline(int ant1Number, int ant2Number);

        /**
         *  Get a baseline object from this band via input numbers.
         *  input1Number must be less than or equal to input2Number.
         *  @throw ErrorException("input1Number greater than input2Number");
         *  @throw NotFoundException("Baseline does not exist");
         */
        const CorrelatorBaseline & getBaseline( int input1Number,
                                                int input2Number ) const;
        CorrelatorBaseline & getBaseline( int input1Number,
                                          int input2Number );

        /**
         *  Get all baselines in this band.
         */
        const ::std::vector< CorrelatorBaseline > & getBaselines( ) const;

        ::std::vector< CorrelatorBaseline > & getBaselines( );

        /**
         *  Assignment
         */
        CorrelatorBand& operator=(const CorrelatorBand& cbIn);

        /**
         *  == operator. Bands are equal if bandNumbers are equal
         */
        friend bool operator==(const CorrelatorBand& a, const CorrelatorBand& b) {
          return (a.getBandNumber() == b.getBandNumber());
        };

        /**
         *  Add a CorrelatorBand into this one
         */
        void addIn( const CorrelatorBand & rhs );

        /**
         * Add a CorrelatorBand but ignore duplicates.
         * If the band already exists and contains baselines which already
         * exist, ignore the duplicates and report false. Otherwise
         * return true and add the band normally.
         */
        bool addInIgnoringDups( const CorrelatorBand & band );

        /**
         * Flag entire band of data.
         * @param reason Bit flag of CorrelatorSideband::ValidReasons.
         * @see CorrelatorSideband::ValidReason
         */
        void flagData( unsigned int reason );

private:
        /**
         * Add a CorrelatorBaseline to this band
         */
        void addIn( const CorrelatorBaseline & rhsBaseline );

        bool addInIgnoringDups( const CorrelatorBaseline & rhsBaseline );

public:

        /**
         *  Normalize data in the band. Normally called after summing
         *  together band objects.
         */
        void normalize();

        void reserveBaselines( int numBaselines );

        void swapBaselines( ::std::vector< CorrelatorBaseline > & baselines );

        std::string getSummary() const;

      private:
        double                              mjd_;
        int                                 bandNumber_;
        bool                                selfTest_;
        bool                                simulation_;
        long                                sequenceNumber_;
        float                               bandwidth_;
        int                                 numberOfAntennas_;
        bool                                valid_;
        ::std::vector< CorrelatorBaseline > baselines_;
      }; // End class CorrelatorBand

    } // End namespace lib
  } // End namespace correlator
} // End namespace carma


inline
carma::correlator::lib::CorrelatorBand::CorrelatorBand( ) :
mjd_( 0.0 ),
bandNumber_( 0 ),
selfTest_( false ),
simulation_( true ),
sequenceNumber_( 0 ),
bandwidth_( 0.0 ),
numberOfAntennas_( 0 ),
valid_( true )
{
}


inline
carma::correlator::lib::CorrelatorBand::CorrelatorBand(
    const CorrelatorBand & rhs ) :
mjd_( rhs.mjd_ ),
bandNumber_( rhs.bandNumber_ ),
selfTest_( rhs.selfTest_ ),
simulation_( rhs.simulation_ ),
sequenceNumber_( rhs.sequenceNumber_ ),
bandwidth_( rhs.bandwidth_ ),
numberOfAntennas_( rhs.numberOfAntennas_ ),
valid_( rhs.valid_ ),
baselines_( rhs.baselines_ )
{
}


inline
carma::correlator::lib::CorrelatorBand::~CorrelatorBand( )
{
}


inline void
carma::correlator::lib::CorrelatorBand::swap(
    carma::correlator::lib::CorrelatorBand & rhs )
{
    ::std::swap( mjd_, rhs.mjd_ );
    ::std::swap( bandNumber_, rhs.bandNumber_ );
    ::std::swap( selfTest_, rhs.selfTest_ );
    ::std::swap( simulation_, rhs.simulation_ );
    ::std::swap( sequenceNumber_, rhs.sequenceNumber_ );
    ::std::swap( bandwidth_, rhs.bandwidth_ );
    ::std::swap( numberOfAntennas_, rhs.numberOfAntennas_ );
    ::std::swap( valid_, rhs.valid_ );
    baselines_.swap( rhs.baselines_ );
}


inline carma::correlator::lib::CorrelatorBand &
carma::correlator::lib::CorrelatorBand::operator=(
    const carma::correlator::lib::CorrelatorBand & rhs )
{
    if ( &rhs != this ) {
        mjd_               = rhs.mjd_;
        bandNumber_        = rhs.bandNumber_;
        selfTest_          = rhs.selfTest_;
        simulation_        = rhs.simulation_;
        sequenceNumber_    = rhs.sequenceNumber_;
        bandwidth_         = rhs.bandwidth_;
        numberOfAntennas_  = rhs.numberOfAntennas_;
        bandNumber_        = rhs.bandNumber_;
        valid_             = rhs.valid_;
        baselines_         = rhs.baselines_;
    }
  
    return *this;
}


inline double
carma::correlator::lib::CorrelatorBand::getMJD( ) const
{
    return mjd_;
}


inline int
carma::correlator::lib::CorrelatorBand::getBandNumber( ) const
{
    return bandNumber_;
}


inline long
carma::correlator::lib::CorrelatorBand::getSequenceNumber( ) const
{
    return sequenceNumber_;
}


inline float
carma::correlator::lib::CorrelatorBand::getBandwidth( ) const
{
    return bandwidth_;
}


inline int
carma::correlator::lib::CorrelatorBand::getNumberOfInputs( ) const
{
    return numberOfAntennas_;
}


inline int
carma::correlator::lib::CorrelatorBand::getNumberOfBaselines( ) const
{
    return baselines_.size();
}


inline const ::std::vector< carma::correlator::lib::CorrelatorBaseline > &
carma::correlator::lib::CorrelatorBand::getBaselines( ) const
{
    return baselines_;
}


inline ::std::vector< carma::correlator::lib::CorrelatorBaseline > &
carma::correlator::lib::CorrelatorBand::getBaselines( )
{
    return baselines_;
}


inline bool
carma::correlator::lib::CorrelatorBand::isSelfTest( ) const
{
    return selfTest_;
}


inline bool
carma::correlator::lib::CorrelatorBand::isSimulation( ) const
{
    return simulation_;
}


inline bool
carma::correlator::lib::CorrelatorBand::isValid( ) const
{
    return valid_;
}


inline void
carma::correlator::lib::CorrelatorBand::setMJD( const double mjd )
{
    mjd_ = mjd;
}


inline void
carma::correlator::lib::CorrelatorBand::setBandNumber( const int bandNumber )
{
    bandNumber_ = bandNumber;
}


inline void
carma::correlator::lib::CorrelatorBand::setSelfTest( const bool selftest )
{
    selfTest_ = selftest;
}


inline void
carma::correlator::lib::CorrelatorBand::setSimulation( const bool sim )
{
    simulation_ = sim;
}


inline void
carma::correlator::lib::CorrelatorBand::setSequenceNumber( const long sn )
{
    sequenceNumber_ = sn;
}


inline void
carma::correlator::lib::CorrelatorBand::setBandwidth( const float bandwidth )
{
    bandwidth_ = bandwidth;
}


inline void
carma::correlator::lib::CorrelatorBand::setNumberOfInputs( const int na )
{
    numberOfAntennas_ = na;
}


inline void
carma::correlator::lib::CorrelatorBand::setValid( const bool v,
                                                  const unsigned int reason )
{
    valid_ = v;

    if ( v ) return;

    BOOST_FOREACH( CorrelatorBaseline & baseline, baselines_ ) {
        baseline.blankData( reason );
    }
}


inline void
carma::correlator::lib::CorrelatorBand::swapBaselines(
    ::std::vector< carma::correlator::lib::CorrelatorBaseline > & baselines )
{
    baselines_.swap( baselines );
}


#endif
