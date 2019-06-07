#ifndef CORRELATORHEADER_H
#define CORRELATORHEADER_H

#include "carma/util/Serializable.h"

/**
 * @file CorrelatorHeader.h
 *
 * @author Rick Hobbs
 */
namespace carma {
namespace correlator {
namespace lib {

      /**
       * Class containing header information for Correlator Data.
       */
      class CorrelatorHeader: public carma::util::Serializable {
      public:

        /**
         * Constructor.
         */
        explicit CorrelatorHeader();

        /**
         *  Copy constructor
         */
        CorrelatorHeader( const CorrelatorHeader & rhs );

        /**
         *  Assignment
         */
        CorrelatorHeader & operator=( const CorrelatorHeader & rhs );

        /**
         * Destructor.
         */
        virtual ~CorrelatorHeader();

        /**
         * Swap this instance with another CorrelatorHeader instance
         */
        void swap( CorrelatorHeader & rhs );

        /**
         *  Set the start time of the packet (integral 1/2 second)
         */
        void setMJD(double mjd);

        /**
         * Get the start time of the sample packet (integral 1/2 second)
         * Note this is the beginning time of the correlation sample and
         * is pegged to an integral half second value!!!  
         */
        double getMJD() const;

        /**
         * Set the time the packet was assembled.  
         * Packet assembly refers to the time this CorrelatorHeader
         * object was assembled.  This typically occurs after receipt of a 
         * cobra::CorrelatorBand object over a socket from the 
         * CorrelatorBandServer application and prior to complete
         * assembly of the CorrelatorBand object, serialization and 
         * transmission via the CORBA event service. 
         * @see setTransmissionMJD
         * @see setReceivedMJD
         */
        void setAssembledMJD(double mjd);

        /**
         * Get the time the packet was assembled.
         */
        double getAssembledMJD() const;

        /**
         *  Set the time the packet was serialized for transmission
         */
        void setTransmissionMJD(double mjd);

        /**
         * Get the time the packet was serialized for transmission
         */
        double getTransmissionMJD() const;

        /**
         *  Set the time the packet was received and deserialized
         */
        void setReceivedMJD(double mjd);

        /**
         *  Get the time the packet was received and deserialized
         */
        double getReceivedMJD() const;

        /**
         *  Set the sequence number
         */
        void setSequenceNumber(long seqNumber);

        /**
         *  Get the sequence number
         */
        long getSequenceNumber() const;

        /**
         * Used to serialize data
         */
        void mySerialize( char * byteArray, int * offset ) const;

        /**
         * Used to reconstruct object
         */
        void deserializeVer0( const char * byteArray, 
                              int * offset, 
                              int byteArraySize );

        void deserializeVer1( const char * byteArray, 
                              int * offset, 
                              int byteArraySize );

        /**
         * Used to reconstruct object. Swap bytes.
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

	void setVersionNumber( int32_t ver );

	static int32_t getVersionNumber();

        std::string getSummary() const;

      private:
        double mjd_;        // Start time of packet (integral 1/2 sec)
        double asmmjd_;     // Time packet was assembled
        double txmjd_;      // Time packet was transmitted by CORBA
        double rxmjd_;      // Time packet was received by CORBA
        long seq_;          // Sequence number
	static int32_t version_;
      }; // End class CorrelatorHeader

} // End namespace lib
} // End namespace correlator
} // End namespace carma


inline
carma::correlator::lib::CorrelatorHeader::CorrelatorHeader( ) :
mjd_( 0.0 ),
asmmjd_( 0.0 ),
txmjd_( 0.0 ),
rxmjd_( 0.0 ),
seq_( 0L )
{
}


inline
carma::correlator::lib::CorrelatorHeader::CorrelatorHeader(
    const CorrelatorHeader & rhs ) :
mjd_( rhs.mjd_ ),
asmmjd_( rhs.asmmjd_ ),
txmjd_( rhs.txmjd_ ),
rxmjd_( rhs.rxmjd_ ),
seq_( rhs.seq_ )
{
}


inline
carma::correlator::lib::CorrelatorHeader::~CorrelatorHeader( )
{
}


inline void
carma::correlator::lib::CorrelatorHeader::swap( CorrelatorHeader & rhs )
{
    ::std::swap( mjd_, rhs.mjd_ );
    ::std::swap( asmmjd_, rhs.asmmjd_ );
    ::std::swap( txmjd_, rhs.txmjd_ );
    ::std::swap( rxmjd_, rhs.rxmjd_ );
    ::std::swap( seq_, rhs.seq_ );
}


inline carma::correlator::lib::CorrelatorHeader &
carma::correlator::lib::CorrelatorHeader::operator=(
    const carma::correlator::lib::CorrelatorHeader & rhs )
{
    mjd_    = rhs.mjd_;
    asmmjd_ = rhs.asmmjd_;
    txmjd_  = rhs.txmjd_;
    rxmjd_  = rhs.rxmjd_;
    seq_    = rhs.seq_;

    return *this;
}


inline double
carma::correlator::lib::CorrelatorHeader::getMJD( ) const
{
    return mjd_;
}


inline double
carma::correlator::lib::CorrelatorHeader::getAssembledMJD( ) const
{
    return asmmjd_;
}


inline double
carma::correlator::lib::CorrelatorHeader::getTransmissionMJD( ) const
{
    return txmjd_;
}


inline double
carma::correlator::lib::CorrelatorHeader::getReceivedMJD( ) const
{
    return rxmjd_;
}


inline long
carma::correlator::lib::CorrelatorHeader::getSequenceNumber( ) const
{
    return seq_;
}


inline void
carma::correlator::lib::CorrelatorHeader::setMJD( const double mjd )
{
    mjd_ = mjd;
}


inline void
carma::correlator::lib::CorrelatorHeader::setAssembledMJD( const double mjd )
{
    asmmjd_ = mjd;
}


inline void
carma::correlator::lib::CorrelatorHeader::setTransmissionMJD( const double m )
{
    txmjd_ = m;
}


inline void
carma::correlator::lib::CorrelatorHeader::setReceivedMJD( const double mjd )
{
    rxmjd_ = mjd;
}


inline void
carma::correlator::lib::CorrelatorHeader::setSequenceNumber( const long seqNo )
{
    seq_ = seqNo;
}

inline void
carma::correlator::lib::CorrelatorHeader::setVersionNumber( const int32_t ver )
{
  version_ = ver;
}

#endif
