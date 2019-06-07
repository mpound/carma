#ifndef CORRELATORSIDEBAND_H
#define CORRELATORSIDEBAND_H

#include <complex>
#include <vector>
#include "carma/util/Serializable.h"
#include "carma/monitor/MonitorPoint.h"
#include "carma/correlator/lib/CorrelatorStats.h"

/**
 * @file CorrelatorSideband.h
 * 
 * @author Rick Hobbs
 */


namespace carma {
namespace correlator {
namespace lib {

      class CorrelatorSideband;

      typedef ::std::vector< CorrelatorSideband > SidebandVector;
      typedef ::std::vector< ::std::complex< float > > DataVector;
      typedef ::std::vector< int > DataValidVector;

      /**
       * Base class for Correlator Sideband data.
       */
      class CorrelatorSideband : public carma::util::Serializable {
      public:
        typedef enum {
            AUTO_FLAVOR,
            UPPER_FLAVOR,
            LOWER_FLAVOR
        } Flavor;
        
        typedef enum {
            NO_REASON                  = 0x00000000,
            A1_PHASELOCK               = 0x00000001,
            A2_PHASELOCK               = 0x00000002,
            A1_MAJOR_TRACKING          = 0x00000004, // > 20% of integration
            A2_MAJOR_TRACKING          = 0x00000008, // > 20% of integration
            A1_TSYS_BAD                = 0x00000010,
            A2_TSYS_BAD                = 0x00000020,
            A1_SHADOWED                = 0x00000040,
            A2_SHADOWED                = 0x00000080,
            A1_OFFLINE                 = 0x00000100,
            A2_OFFLINE                 = 0x00000200,
            A1_MINOR_TRACKING          = 0x00000400,
            A2_MINOR_TRACKING          = 0x00000800,
            A1_CALSTATE                = 0x00001000,
            A2_CALSTATE                = 0x00002000,
            UNKNOWN12                  = 0x00004000,
            UNKNOWN13                  = 0x00008000,
            UNKNOWN14                  = 0x00010000,
            UNKNOWN15                  = 0x00020000,
            UNKNOWN16                  = 0x00040000,
            UNKNOWN17                  = 0x00080000,
            UNKNOWN18                  = 0x00100000,
            UNKNOWN19                  = 0x00200000,
            UNKNOWN20                  = 0x00400000,
            MANUAL_FLAG                = 0x00800000,
            BAND_OFFLINE               = 0x01000000,
            UNMAPPED_SIGNAL            = 0x02000000,
            MONITOR_DATA_BAD           = 0x04000000,
            BAD_CHANNEL_COUNT          = 0x08000000,
            NO_RX_IN_SIDEBAND          = 0x10000000,
            CORR_DATA_MISSING          = 0x20000000,
            CORR_DATA_INVALID          = 0x40000000,
            DO_NOT_USE                 = 0x80000000,
        } ValidReason;

        /**
         * Constructor
         */
        explicit CorrelatorSideband( Flavor initialFlavor );

        /**
         *  Copy Constructor
         */
        CorrelatorSideband( const CorrelatorSideband & rhs );

        /**
         * Destructor
         */
        virtual ~CorrelatorSideband( );

        /**
         * Swap this instance with another CorrelatorSideband instance
         */
        void swap( CorrelatorSideband & rhs );

        /**
         * Use to serialize contents
         */
        void mySerialize( char * byteArray, int * offset ) const;

        /**
         * Uset to deserialize contents
         */
        void deserializeVer0( const char * byteArray, 
                              int * offset,
                              int byteArraySize );

        void deserializeVer1( const char * byteArray, 
                              int * offset,
                              int byteArraySize );

        /**
         * Uset to deserialize contents. Swaps bytes.
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
        int getSizeInBytes( ) const;

        /**
         * True if Sideband is an Auto-Spectra
         */
        bool isAuto( ) const;

        /**
         * True if Sideband is an UpperSideband Spectra
         */
        bool isUSB( ) const;

        /**
         * True if Sideband is an UpperSideband Spectra
         */
        bool isLSB( ) const;

        /**
         * Get flavor
         */
        Flavor getFlavor( ) const;

        /**
         * True if data at channel index is valid.
         * @throw CARMA_ERROR if channel index out of range
         */
        bool isValid(int channelIndex) const;

        /**
         *  True if this sideband is valid which means at least 1
         *  channel is valid. If all channels are invalid, then this
         *  will return false.
         */
        bool isValid( ) const;

        /**
         *  Set validity for channel index
         *  @throw CARMA_ERROR if channel index out of range
         */
        void setValid(int channelIndex, bool val);

        /**
         *  Set all channels to same validity
         */
        void setValidAll(bool val);

        /**
         *  Get Spectra
         */
        DataVector & getData( ) const;

        /**
         *  Get valid channel vector
         */
        const DataValidVector & getDataValid( ) const;

        /**
         * Get Number of Channels in Spectra
         */
        int getNumberOfChans( ) const;

        /**
         * Get Number of Lags used to create Spectra. This will be
         * before any zero padding is done to the data before FFT.
         * Userful for when spectra is decimated by inverse FFT and
         * Hanning windowed, then FFT'd again. The window function must
         * be applied to the non-zero padded data.
         */
        int getNumberOfLags( ) const;

        /**
         *  Set the number of Lags used to create Spectra. This will be
         * before any zero padding is done to the data before FFT.
         * Userful for when spectra is decimated by inverse FFT and
         * Hanning windowed, then FFT'd again. The window function must
         * be applied to the non-zero padded data.
         */
        void setNumberOfLags(int numLags);

        /**
         *  Return sky freq. of first channel[GHz]
         */
         float getSkyFrequency( ) const;

        /**
         *  Return receiver output freq. of first channel[GHz]
         */
         float getRxOutFrequency( ) const;

         /**
          *  Return the offset frquency the center of the first
          *  channel is from the edge of the band. [MHz]
          */
         float getOffsetFrequency( ) const;

        /**
         *  Set sky freq. of first channel[GHz]
         */
         void setSkyFrequency(float freq);

        /**
         *  Set receiver output freq. of first channel[GHz]
         */
         void setRxOutFrequency(float freq);

         /**
          *  Set the offset frquency the center of the first
          *  channel is from the edge of the band. [MHz]
          */
         void setOffsetFrequency(float offsetFreq);

        /**
         *  Get delta freq. for channels
         */
         float getDeltaFrequency( ) const;

        /**
         *  Set delta freq. for channels
         */
         void setDeltaFrequency(float freq);

        /**
         * Set Spectra
         */
        void setData( const ::std::vector< ::std::complex< float > > & data );

        void swapData( ::std::vector< ::std::complex< float > > & data );

        /**
         * Get the Stats object
         */
        const carma::correlator::lib::CorrelatorStats & getStats( ) const;

        /**
         * Set the Stats object
         */
        void setStats( const carma::correlator::lib::CorrelatorStats & stats );

        /**
         * Convenience method to compute the Stats from the internally
         * stored spectra
         */
        void computeStats(bool keepEndChannels = true);

        /**
         *  normalize object. Used if addIn is used on this object.
         *  will recompute stats using computeStats().
         */
        void normalize( );

        /**
         *  Assignment
         */
        CorrelatorSideband & operator=( const CorrelatorSideband & rhs );

        /**
         *  Add a CorrelatorSideband into this one
         */
        void addIn( const CorrelatorSideband & rhs );

        /** 
         * Flag data with specified reason.
         * @param reason Bitwise or of CorrelatorSideband::ValidReason.
         */
        void flagData( unsigned int reason );
        void unflagData( unsigned int reason );

        /**
         * Blank data with specified reason (removes all data).
         * @param reason Bitwise or of CorrelatorSideband::ValidReason.
         */
        void blankData( unsigned int reason );

        /** 
         * Get blank flag status for this baseline.
         * @see getValidReason
         */
        carma::monitor::MonitorPoint::BLANKING_FLAGGING
        getBlankFlagStatus() const;
        
        /** 
         * Get reason for blanking & flagging.
         */
	    unsigned int getValidReason() const;
        
        /** 
         * Retrieve the minor track blanking and flagging counters.
         * @return pair containing A1_MINOR_TRACKING count first, A2 second.
         */
        std::pair< unsigned int, unsigned int > 
        getMinorTrackBfCount( ) const;

        std::string getSummary() const;

        std::string getSpectra() const;

      private:

        void setDataValidAllPrivate(bool b);
        void setBlankFlagStatus( unsigned int reason );
        void unsetBlankFlagStatus( unsigned int reason );


        bool autoSideband_;
        bool usb_;
        bool dataHasBeenSet_;
        bool dataValidAllHasBeenSet_;
        bool dataValidAllValue_;
        int numberOfChans_;
	
        // Next variable is needed to indicated the actual number of lags
        // used before any zero padding (if needed) is done before the FFT.
        // This will be used later when decimation of the spectra is done
        // and the Hanning window must be applied to only the acutal data
        // points, not the zero padded values.
        int numberOfLags_;

        carma::correlator::lib::CorrelatorStats stats_;
        float frequency_; // sky freq. of 1st channel(GHz)
        float deltaFrequency_; // delta freq.(MHz)
        float offsetFrequency_; // band edge to center of 1st channel(MHz)
        mutable DataVector data_;
        // dataValid_ is an array which indicates which channels
        // of data_ are valid. This is most useful for the pseudo-band
        // comprised of all the real band averages. For example, 16 bands
        // of data will be band averaged to produce a pseudo-band of
        // 16 channels which represents the average of each band. These
        // 16 channles can be marked as valid(1) or invalid(0). This
        // vector will also be used to accumulate sums of the data for
        // proper normalization. Thus an entry of 0 means invalid and > 0
        // valid.
        // Note: not using a vector<bool> since this is a special
        // construct is C++ just to screw you up!!!
        DataValidVector dataValid_;
	unsigned int validReason_;
	monitor::MonitorPoint::BLANKING_FLAGGING bfStatus_;
        std::pair< unsigned int, unsigned int > minorTrackBfCountNoSerialize_;

      }; // End class CorrelatorSideband

} // End namespace lib
} // End namespace correlator
} // End namespace carma


inline
carma::correlator::lib::CorrelatorSideband::CorrelatorSideband(
    const Flavor initialFlavor ) :
  autoSideband_( initialFlavor == AUTO_FLAVOR ),
  usb_( initialFlavor == UPPER_FLAVOR ),
dataHasBeenSet_( false ),
dataValidAllHasBeenSet_( false ),
dataValidAllValue_( false ),
numberOfChans_( 0 ),
numberOfLags_( 0 ),
stats_(),
frequency_( 0.0 ),
deltaFrequency_( 0.0 ),
offsetFrequency_( 0.0 ),
data_(),
dataValid_(),
validReason_(0),
bfStatus_(monitor::MonitorPoint::UNDETERMINED ),
minorTrackBfCountNoSerialize_( 0, 0 )
{
}


inline
carma::correlator::lib::CorrelatorSideband::CorrelatorSideband(
    const CorrelatorSideband & rhs ):
autoSideband_( rhs.autoSideband_ ),
usb_( rhs.usb_ ),
dataHasBeenSet_( rhs.dataHasBeenSet_ ),
dataValidAllHasBeenSet_( rhs.dataValidAllHasBeenSet_ ),
dataValidAllValue_( rhs.dataValidAllValue_ ),
numberOfChans_( rhs.numberOfChans_ ),
numberOfLags_( rhs.numberOfLags_ ),
stats_( rhs.stats_ ),
frequency_( rhs.frequency_ ),
deltaFrequency_( rhs.deltaFrequency_ ),
offsetFrequency_( rhs.offsetFrequency_ ),
data_( rhs.data_ ),
dataValid_( rhs.dataValid_ ),
validReason_( rhs.validReason_ ),
bfStatus_(rhs.bfStatus_),
minorTrackBfCountNoSerialize_( rhs.minorTrackBfCountNoSerialize_ )
{
}


inline void
carma::correlator::lib::CorrelatorSideband::swap( CorrelatorSideband & rhs )
{
    ::std::swap( autoSideband_, rhs.autoSideband_ );
    ::std::swap( usb_, rhs.usb_ );
    ::std::swap( dataHasBeenSet_, rhs.dataHasBeenSet_ );
    ::std::swap( dataValidAllHasBeenSet_, rhs.dataValidAllHasBeenSet_ );
    ::std::swap( dataValidAllValue_, rhs.dataValidAllValue_ );
    ::std::swap( numberOfChans_, rhs.numberOfChans_ );
    ::std::swap( numberOfLags_, rhs.numberOfLags_ );
    stats_.swap( rhs.stats_ );
    ::std::swap( frequency_, rhs.frequency_ );
    ::std::swap( deltaFrequency_, rhs.deltaFrequency_ );
    ::std::swap( offsetFrequency_, rhs.offsetFrequency_ );
    data_.swap( rhs.data_ );
    dataValid_.swap( rhs.dataValid_ );
    ::std::swap( validReason_, rhs.validReason_ );
    ::std::swap( bfStatus_, rhs.bfStatus_ );
    ::std::swap( minorTrackBfCountNoSerialize_, 
                 rhs.minorTrackBfCountNoSerialize_ );
}


inline carma::correlator::lib::CorrelatorSideband &
carma::correlator::lib::CorrelatorSideband::operator=(
    const  CorrelatorSideband & rhs )
{
    if ( &rhs != this ) {
        autoSideband_           = rhs.autoSideband_;
        usb_                    = rhs.usb_;
        dataHasBeenSet_         = rhs.dataHasBeenSet_;
        dataValidAllHasBeenSet_ = rhs.dataValidAllHasBeenSet_;
        dataValidAllValue_      = rhs.dataValidAllValue_;
        numberOfChans_          = rhs.numberOfChans_;
        numberOfLags_           = rhs.numberOfLags_;
        frequency_              = rhs.frequency_;
        deltaFrequency_         = rhs.deltaFrequency_;
        offsetFrequency_        = rhs.offsetFrequency_;
        stats_                  = rhs.stats_;
        data_                   = rhs.data_;
        dataValid_              = rhs.dataValid_;
	    validReason_            = rhs.validReason_;
        bfStatus_               = rhs.bfStatus_;
        minorTrackBfCountNoSerialize_ = rhs.minorTrackBfCountNoSerialize_; 
    }
  
    return *this;
}


inline
carma::correlator::lib::CorrelatorSideband::~CorrelatorSideband( )
{
}


inline bool
carma::correlator::lib::CorrelatorSideband::isAuto( ) const
{
    return autoSideband_;
}


inline bool
carma::correlator::lib::CorrelatorSideband::isUSB( ) const
{
    return usb_;
}


inline bool
carma::correlator::lib::CorrelatorSideband::isLSB( ) const
{
    return ((usb_ != true) && (autoSideband_ != true));
}

inline carma::correlator::lib::CorrelatorSideband::Flavor 
carma::correlator::lib::CorrelatorSideband::getFlavor( ) const
{
    if ( autoSideband_ ) {
        return carma::correlator::lib::CorrelatorSideband::AUTO_FLAVOR;
    } else {
        if ( usb_ ) 
            return carma::correlator::lib::CorrelatorSideband::UPPER_FLAVOR;
        else
            return carma::correlator::lib::CorrelatorSideband::LOWER_FLAVOR;
    }
}

inline ::std::vector< ::std::complex< float > > &
carma::correlator::lib::CorrelatorSideband::getData( ) const
{
    return data_;
}


inline const ::std::vector< int > &
carma::correlator::lib::CorrelatorSideband::getDataValid( ) const
{
    return dataValid_;
}


inline int
carma::correlator::lib::CorrelatorSideband::getNumberOfChans( ) const
{
    return numberOfChans_;
}


inline int
carma::correlator::lib::CorrelatorSideband::getNumberOfLags( ) const
{
    return numberOfLags_;
}


inline float
carma::correlator::lib::CorrelatorSideband::getSkyFrequency( ) const
{
    return frequency_;
}


inline float
carma::correlator::lib::CorrelatorSideband::getRxOutFrequency( ) const
{
    return frequency_;
}


inline float
carma::correlator::lib::CorrelatorSideband::getOffsetFrequency( ) const
{
    return offsetFrequency_;
}


inline float
carma::correlator::lib::CorrelatorSideband::getDeltaFrequency( ) const
{
    return deltaFrequency_;
}


inline const carma::correlator::lib::CorrelatorStats &
carma::correlator::lib::CorrelatorSideband::getStats( ) const
{
    return stats_;
}


inline void
carma::correlator::lib::CorrelatorSideband::setNumberOfLags(
    const int numLags )
{
    numberOfLags_ = numLags;
}


inline void
carma::correlator::lib::CorrelatorSideband::setSkyFrequency(
    const float freq )
{
    frequency_ = freq;
}


inline void
carma::correlator::lib::CorrelatorSideband::setRxOutFrequency(
    const float freq )
{
    frequency_ = freq;
}


inline void
carma::correlator::lib::CorrelatorSideband::setOffsetFrequency(
    const float offsetFreq )
{
    offsetFrequency_ = offsetFreq;
}


inline void
carma::correlator::lib::CorrelatorSideband::setDeltaFrequency(
    const float delFreq )
{
    deltaFrequency_ = delFreq;
}


inline void
carma::correlator::lib::CorrelatorSideband::setStats(
    const carma::correlator::lib::CorrelatorStats & stats )
{
    stats_ = stats;
}

inline carma::monitor::MonitorPoint::BLANKING_FLAGGING
carma::correlator::lib::CorrelatorSideband::getBlankFlagStatus( ) const
{
    return bfStatus_;
}

#endif
