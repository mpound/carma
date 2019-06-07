#ifndef CORRELATORSTATS_H
#define CORRELATORSTATS_H

#include <complex>
#include "carma/util/Serializable.h"

/**
 * @file CorrelatorStats.h
 *
 * @author Rick Hobbs
 */
namespace carma {
namespace correlator {
namespace lib {

      /**
       * Class to hold some statistics related to Sideband data
       */
      class CorrelatorStats : public carma::util::Serializable {
      public:

        /**
         *  Constructor.
         */
        explicit CorrelatorStats( );

        /**
         *  Copy Constructor
         */
        CorrelatorStats( const CorrelatorStats & rhs );

        /**
         *  Destructor
         */
        virtual ~CorrelatorStats();

        /**
         * Swap this instance with another CorrelatorStats instance
         */
        void swap( CorrelatorStats & rhs );

        /**
         *  Called when object is serialized.
         */
        void mySerialize( char * byteArray, int * offset ) const;

        /**
         *  Called when object is de-serialized.
         */
        void deserializeVer0( const char * byteArray, 
                              int * offset,
                              int byteArraySize );

        void deserializeVer1( const char * byteArray, 
                              int * offset,
                              int byteArraySize );

        /**
         *  Called when object is de-serialized. Swap bytes.
         */
        void deserializeSwapVer0( const char * byteArray, 
                                  int * offset,
                                  int byteArraySize );

        void deserializeSwapVer1( const char * byteArray, 
                                  int * offset,
                                  int byteArraySize );

        /**
         *  Return size in bytes
         */
        int getSizeInBytes() const;

        /**
         * Get Integration time in msec.
         */
        float getIntegrationTime() const;

        /**
         * Get the number of samples in integration.
         */
        int getNumberOfSamples() const;

        /**
         *  Get Average
         */
        std::complex<float> getAvg() const;

        /**
         *  Get Variance.
         */
        std::complex<float> getVariance() const;

        /**
         *  Get Standard Deviation
         */
        std::complex<float> getStandardDeviation() const;

        /**
         * Set the number of samples in integration.
         */
        void setNumberOfSamples(int numberOfSamples);

        /**
         * Set Integration time in msec.
         */
        void setIntegrationTime(float integTime);

        /**
         *  Set Average
         */
        void setAvg(std::complex<float> avg);

        /**
         *  Set Variance.
         */
        void setVariance(std::complex<float> var);

        /**
         *  Set Standard Deviation.
         */
        void setStandardDeviation(std::complex<float> sd);

        /**
         *  Assignment
         */
        CorrelatorStats & operator=( const CorrelatorStats & rhs );

        /**
         *  Add in integration time from a CorrelatorStats
         */
        void addInIntegrationTime( const CorrelatorStats & rhs );

        /**
         * Provide a string summary of the contents.
         */
        std::string getSummary( ) const;

      private:
        float integrationTime_;
        int numberOfSamples_; // number of samples used(ie. data.size())
        // these quantities represent averages over the number of
        // elemets in the data vector
        std::complex<float> avg_;
        std::complex<float> var_;
        std::complex<float> sd_;

      }; // End class CorrelatorStats

} // End namespace lib
} // End namespace correlator
} // End namespace carma


inline
carma::correlator::lib::CorrelatorStats::CorrelatorStats( ) :
integrationTime_( 0.0 ),
numberOfSamples_( 0 ),
avg_( 0.0, 0.0 ),
var_( 0.0, 0.0 ),
sd_( 0.0, 0.0 )
{
}


inline
carma::correlator::lib::CorrelatorStats::CorrelatorStats(
    const CorrelatorStats & rhs ) :
integrationTime_( rhs.integrationTime_ ),
numberOfSamples_( rhs.numberOfSamples_ ),
avg_( rhs.avg_ ),
var_( rhs.var_ ),
sd_( rhs.sd_ )
{
}


inline void
carma::correlator::lib::CorrelatorStats::swap( CorrelatorStats & rhs )
{
    ::std::swap( integrationTime_, rhs.integrationTime_ );
    ::std::swap( numberOfSamples_, rhs.numberOfSamples_ );
    ::std::swap( avg_, rhs.avg_ );
    ::std::swap( var_, rhs.var_ );
    ::std::swap( sd_, rhs.sd_ );
}


inline
carma::correlator::lib::CorrelatorStats::CorrelatorStats::~CorrelatorStats( )
{
}


inline float
carma::correlator::lib::CorrelatorStats::getIntegrationTime( ) const
{
    return integrationTime_;
}


inline int
carma::correlator::lib::CorrelatorStats::getNumberOfSamples( ) const
{
    return numberOfSamples_;
}


inline ::std::complex< float >
carma::correlator::lib::CorrelatorStats::getAvg( ) const
{
    return avg_;
}


inline ::std::complex< float >
carma::correlator::lib::CorrelatorStats::getVariance( ) const
{
    return var_;
}


inline ::std::complex< float >
carma::correlator::lib::CorrelatorStats::getStandardDeviation( ) const
{
    return sd_;
}


inline void
carma::correlator::lib::CorrelatorStats::setIntegrationTime(
    const float integTime )
{
    integrationTime_ = integTime;
}


inline void
carma::correlator::lib::CorrelatorStats::setNumberOfSamples(
    const int ns )
{
    numberOfSamples_ = ns;
}


inline void
carma::correlator::lib::CorrelatorStats::setAvg(
    const ::std::complex< float > avg )
{
    avg_ = avg;
}


inline void
carma::correlator::lib::CorrelatorStats::setVariance(
    const ::std::complex< float > var )
{
    var_ = var;
}


inline void
carma::correlator::lib::CorrelatorStats::setStandardDeviation(
    const ::std::complex< float > sd )
{
    sd_ = sd;
}


inline void
carma::correlator::lib::CorrelatorStats::addInIntegrationTime(
    const carma::correlator::lib::CorrelatorStats & rhs )
{
    integrationTime_ += rhs.getIntegrationTime();
}


inline carma::correlator::lib::CorrelatorStats &
carma::correlator::lib::CorrelatorStats::operator=(
    const carma::correlator::lib::CorrelatorStats & rhs )
{
    integrationTime_ = rhs.integrationTime_;
    numberOfSamples_ = rhs.numberOfSamples_;
    avg_             = rhs.avg_;
    var_             = rhs.var_;
    sd_              = rhs.sd_;

    return *this;
}


#endif
