#ifndef SERIALIZABLE_H
#define SERIALIZABLE_H

#include <netinet/in.h>
#include <iostream>
#include <vector>
#include <complex>
#include <iostream>
#include <string.h>

#include "carma/util/ByteBuffer.h"
#include "carma/correlator/lib/CorrelatorPolarization.h"

/**
 * @file Serializable.h
 *
 * @author Rick Hobbs
 */
namespace carma {
  namespace util {

    /**
     * Abstract Class used to allow object to serialize themselves
     * into a byte array. This class uses the 'reader makes right'
     * paradigm in which the writer of the data will write in its
     * native endianess and the reader determines if it needs to
     * swap bytes. This allows high efficiency when writer and reader
     * are of the same architecture. The first byte of the packed
     * array indicates the endianess of the writer(true for little
     * endian and false for big-endian).
     */
    class Serializable {
    public:
      explicit Serializable( ) {  }
      virtual ~Serializable( ) {  }

      /**
       * Write objects data members into a byte vector.
       * The vector will be resized as needed for you.
       * Use this method on the top most object.
       */
      void serialIntoByteVec( ::std::vector< char > & byteVec ) const;

      void serialIntoByteBuffer( ByteBuffer & byteBuffer ) const;

      void serialIntoByteArray( char * byteArray,
                                int    byteArraySize,
                                int *  totalSerialBytes ) const;

      int getTotalSerialBytes( ) const;

      /**
       *  Call to initiate the reconstruction of the object from
       *  the byte Array. Subclasses should use the unpack() methods
       *  to reconstruct data.
       */
      void deserial( const char * byteArray, int byteArraySize );
      void deserial( const ::std::vector< char > & byteVec );
      void deserial( const ByteBuffer & byteBuffer );

      /**
       * Write objects data members into a byte array.
       */
      void serialize( char * const byteArray,
                      int * const  offset ) const;

    protected:

      /**
       *  Return size in bytes of object
       */
      virtual int getSizeInBytes( ) const = 0;

      /**
       *  Called by serialize(). Must be implemented by
       *  concrete classes.
       */
      virtual void mySerialize( char * byteArray,
                                int *  offset ) const = 0;

      /**
       *  Called to continue the reconstruction of member objects from
       *  the byte Array. This method should NOT swap bytes when
       *  reconstructing object. Subclasses should use the unpack() methods
       *  to reconstruct data. All classes implementing Serializable.h must
       *  define this. The version number goes hand in hand with version=
       *  in the serialIntoByteVec() method.
       */
      virtual void deserializeVer0( const char * byteArray,
                                    int *        offset,
                                    int          byteArraySize ) = 0;

      virtual void deserializeVer1( const char * byteArray,
                                    int *        offset,
                                    int          byteArraySize ) = 0;

      /**
       *  Called to continue the reconstruction of member objects from
       *  the byte Array. This method SHOULD swap bytes when
       *  reconstructing object. Subclasses should use the unpackSwap() methods
       *  to reconstruct data. All classes implementing Serializable.h must
       *  define this. The version number goes hand in hand with version=
       *  in the inline serialIntoByteVec() method.
       */
      virtual void deserializeSwapVer0( const char * byteArray,
                                        int *        offset,
                                        int          byteArraySize ) = 0;

      virtual void deserializeSwapVer1( const char * byteArray,
                                        int *        offset,
                                        int          byteArraySize ) = 0;

      template < typename T >
      static void pack( const ::std::vector< T > & tmpv,
                        char *                     byteArray,
                        int *                      offset );

      template < typename T >
      static void pack( const T tmp,
                        char *  byteArray,
                        int *   offset );

      template < typename T >
      static void unpack( T &          val,
                          const char * byteArray,
                          int *        offset,
                          int          byteArraySize );

      template < typename T >
      static void unpack( ::std::vector< T > & tmpv,
                          const char *         byteArray,
                          int *                offset,
                          int                  byteArraySize );

      static void unpackSwap(char& val,
                             const char * byteArray,
                             int* offset,
                             int  byteArraySize );

      static void unpackSwap(bool& val,
                             const char * byteArray,
                             int* offset,
                             int byteArraySize );

      static void unpackSwap(short& val,
                             const char * byteArray,
                             int* offset,
                             int byteArraySize );

      static void unpackSwap(int& val,
                             const char * byteArray,
                             int* offset,
                             int byteArraySize );

      static void unpackSwap(long& val,
                             const char * byteArray,
                             int* offset,
                             int byteArraySize );

      static void unpackSwap(float& val,
                             const char * byteArray,
                             int* offset,
                             int byteArraySize );

      static void unpackSwap(double& val,
                             const char * byteArray,
                             int* offset,
                             int byteArraySize );

      static void unpackSwap( ::std::complex< float > & tmpv,
                              const char *              byteArray,
                              int *                     offset,
                              int                       byteArraySize );

      static void unpackSwap( ::std::vector< short > & tmpv,
                              const char *             byteArray,
                              int *                    offset,
                              int                      byteArraySize );

      static void unpackSwap( ::std::vector< int > & tmpv,
                              const char *           byteArray,
                              int *                  offset,
                              int                    byteArraySize );

      static void unpackSwap( ::std::vector< float > & tmpv,
                              const char *             byteArray,
                              int *                    offset,
                              int                      byteArraySize );

      static void unpackSwap( ::std::vector< ::std::complex< float > > & tmpv,
                              const char *                               byteArray,
                              int *                                      offset,
                              int                                        byteArraySize );

      static void unpackSwap( unsigned int & tmpv,  const char * byteArray,
			      int* offset,
			      int byteArraySize );

      static void unpackSwap( carma::correlator::lib::Polarization & val,
			      const char * byteArray,
			      int* offset,
			      int byteArraySize );

      static int32_t getVersion();

      static int32_t version_;
      // These methods convert floats and doubles to network byte order.
      // It's required that the sizeof(float) = sizeof(long) and
      // sizeof(double) = 2 * sizeof(long); I didn't put check's in these
      // routines as I wanted to maximize speed for now.
    private:

      /**
       *  Return true if machine running this code
       *  stores data in little-endian format
       */
      static bool isLittleEndian( );

      static void swapAnyBytes( void * in, size_t size );

      template < typename T >
      static T swapBytes( T val );

      static void throwByteArrayTooSmallError( int neededBytes,
                                               int availBytes );

      static void throwByteArrayOverrunError( int bufferSize,
                                              int finalOffset );

      static void preemptivelyCheckForByteArrayOverrun( int maxBufferSize,
                                                        int futureOffset );

    }; // class Serializable
  } // End namespace util
} // End namespace carma

inline bool
carma::util::Serializable::isLittleEndian( )
{
    const int a = 5;
    const int b = htonl(a);
    if (a == b) // big-endian
        return false;
    else        // little-endian
        return true;
}


inline void
carma::util::Serializable::swapAnyBytes( void * const in,
                                         const size_t size )
{
    char * const tmp = static_cast< char * >( in );
    const size_t halfSize = (size >> 1);
    for ( size_t i = 0; i < halfSize; ++i ) {
        const size_t j = size - 1 - i;
        const char t = tmp[i];
        tmp[i] = tmp[j];
        tmp[j] = t;
    }
}


template < typename T >
inline T
carma::util::Serializable::swapBytes( T val )
{
    T tmp = val; // Make a copy so we don't overwrite original
    swapAnyBytes( static_cast< void * >( &tmp ), sizeof( T ) );

    return tmp;
}


template < typename T >
inline void
carma::util::Serializable::pack( const ::std::vector< T > & tmpv,
                                 char * const               byteArray,
                                 int *                      offset )
{
    const size_t size = tmpv.size() * sizeof( T );
    memcpy(&(byteArray[*offset]), &tmpv[0], size);
    *offset += size;
}


template < typename T >
inline void
carma::util::Serializable::pack( const T      tmp,
                                 char * const byteArray,
                                 int * const  offset )
{
    const size_t size = sizeof( T );
    memcpy( &(byteArray[*offset]), &tmp, size );
    *offset += size;
}


template < typename T >
inline void
carma::util::Serializable::unpack( T &                val,
                                   const char * const byteArray,
                                   int * const        offset,
                                   const int          byteArraySize )
{
    const int size = sizeof( val );
    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + size );
    memcpy( &val, &(byteArray[*offset]), size );
    *offset += size;
}


template < typename T >
inline void
carma::util::Serializable::unpack( ::std::vector< T > & tmpv,
        const char * const byteArray,
        int * const offset,
        const int byteArraySize )
{
    const int size = tmpv.size() * sizeof( T );
    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + size );
    memcpy(&tmpv[0], &(byteArray[*offset]), size);
    *offset += size;
}


inline void
carma::util::Serializable::unpackSwap( char& val,
            const char * const byteArray,
            int * const offset,
            const int byteArraySize )
{
    unpack(val, byteArray, offset, byteArraySize );
}


inline void
carma::util::Serializable::unpackSwap(bool& val,
         const char * const byteArray,
         int * const offset,
         const int byteArraySize )
{
    unpack(val, byteArray, offset, byteArraySize );
}


inline void
carma::util::Serializable::unpackSwap(short& val,
             const char * const byteArray,
             int * const offset,
             const int byteArraySize )
{
    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + sizeof(val) );
    memcpy(&val, &(byteArray[*offset]), sizeof(val));
    val = swapBytes(val);
    *offset += sizeof(val);
}


inline void
carma::util::Serializable::unpackSwap(int& val,
            const char * const byteArray,
            int * const offset,
            const int byteArraySize )
{
    long lval = static_cast< long >(val);
    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + sizeof(lval));
    memcpy(&lval, &(byteArray[*offset]), sizeof(lval));
    lval = swapBytes(lval);
    val = static_cast< int >(lval);
    *offset += sizeof(lval);
}

inline void
carma::util::Serializable::unpackSwap(unsigned int& val,
            const char * const byteArray,
            int * const offset,
            const int byteArraySize )
{
    long lval = static_cast< long >(val);
    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + sizeof(lval));
    memcpy(&lval, &(byteArray[*offset]), sizeof(lval));
    lval = swapBytes(lval);
    val = static_cast< int >(lval);
    *offset += sizeof(lval);
}

inline void
carma::util::Serializable::unpackSwap(long& val,
             const char * const byteArray,
             int * const offset,
             const int byteArraySize )
{
    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + sizeof(val));
    memcpy(&val, &(byteArray[*offset]), sizeof(val));
    val = swapBytes(val);
    *offset += sizeof(val);
}


inline void
carma::util::Serializable::unpackSwap(float& val,
             const char * const byteArray,
             int * const offset,
             const int byteArraySize )
{
    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + sizeof(val));
    memcpy(&val, &(byteArray[*offset]), sizeof(val));
    val = swapBytes(val);
    *offset += sizeof(val);
}

inline void
carma::util::Serializable::unpackSwap(double& val,
             const char * const byteArray,
             int * const offset,
             const int byteArraySize )
{
    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + sizeof(val));
    memcpy(&val, &(byteArray[*offset]), sizeof(val));
    val = swapBytes(val);
    *offset += sizeof(val);
}


inline void
carma::util::Serializable::unpackSwap(std::complex<float>& tmpv,
             const char * const byteArray,
             int * const offset,
             const int byteArraySize )
{
    int size = sizeof(std::complex<float>);
    int size2 = static_cast< int >(size * .5);

    float real;
    float imag;

    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + size );

    memcpy(&real, &(byteArray[*offset]), size2);
    real = swapBytes(real);
    *offset += size2;
    memcpy(&imag, &(byteArray[*offset]), size2);
    imag = swapBytes(imag);
    *offset += size2;

    tmpv = std::complex<float>(real, imag);
}


inline void
carma::util::Serializable::unpackSwap(std::vector<short>& tmpv,
             const char * const byteArray,
             int * const offset,
             const int byteArraySize )
{
    int size = tmpv.size() * sizeof(short);
    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + size );
    memcpy(&tmpv[0], &(byteArray[*offset]), size);
    for (unsigned int idx = 0; idx < tmpv.size(); ++idx)
        tmpv[idx] = swapBytes(tmpv[idx]);
    *offset += size;
}


inline void
carma::util::Serializable::unpackSwap(std::vector<int>& tmpv,
             const char * const byteArray,
             int * const offset,
             const int byteArraySize )
{
    int size = tmpv.size() * sizeof(int);
    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + size );
    memcpy(&tmpv[0], &(byteArray[*offset]), size);
    for (unsigned int idx = 0; idx < tmpv.size(); ++idx)
        tmpv[idx] = swapBytes(tmpv[idx]);
    *offset += size;
}


inline void
carma::util::Serializable::unpackSwap(std::vector<float>& tmpv,
             const char * const byteArray,
             int * const offset,
             const int byteArraySize )
{
    int size = tmpv.size() * sizeof(float);
    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + size );
    memcpy(&tmpv[0], &(byteArray[*offset]), size);
    for (unsigned int idx = 0; idx < tmpv.size(); ++idx)
        tmpv[idx] = swapBytes(tmpv[idx]);
    *offset += size;
}


inline void
carma::util::Serializable::unpackSwap(std::vector<std::complex<float> >& tmpv,
             const char * const byteArray,
             int * const offset,
             const int byteArraySize )
{
    int length = tmpv.size();
    int size = length * sizeof( std::complex<float> );
    int size2 = sizeof(float);

    preemptivelyCheckForByteArrayOverrun( byteArraySize, *offset + size );

    float real;
    float imag;
    for (int idx = 0; idx < length; ++idx) {
        memcpy(&real, &(byteArray[*offset]), size2);
        real = swapBytes(real);
        *offset += size2;
        memcpy(&imag, &(byteArray[*offset]), size2);
        imag = swapBytes(imag);
        *offset += size2;
        tmpv[idx] = std::complex<float>(real, imag);
    }
}


inline void
carma::util::Serializable::unpackSwap( carma::correlator::lib::Polarization& val,
				       const char * const byteArray,
				       int * const offset,
				       const int byteArraySize )
{
    unpack(val, byteArray, offset, byteArraySize );
}


inline void
carma::util::Serializable::deserial( const char * const byteArray,
                                     const int byteArraySize )
{
    if ( byteArraySize == 0 ) 
        throwByteArrayOverrunError( byteArraySize, 1 );

    int offset = 1;
    if ( byteArray[0] == isLittleEndian() ) {
        unpack( version_, byteArray, &offset, byteArraySize );
        switch ( version_ ) {
            case 0:
                deserializeVer0( byteArray, &offset, byteArraySize );
                break;
            case 1:
            case 2:
                deserializeVer1( byteArray, &offset, byteArraySize );
                break;
        }
    } else {
        unpackSwap( version_, byteArray, &offset, byteArraySize );
        switch ( version_ ) {
            case 0:
                deserializeSwapVer0( byteArray, &offset, byteArraySize );
                break;
            case 1:
            case 2:
                deserializeSwapVer1( byteArray, &offset, byteArraySize );
                break;
        }
    }
}

inline void
carma::util::Serializable::deserial( const ::std::vector< char > & byteVec )
{
    deserial( &(byteVec[0]), byteVec.size() );
}


inline void
carma::util::Serializable::deserial( const ByteBuffer & byteBuffer )
{
    deserial( byteBuffer.get(), byteBuffer.size() );
}


inline int
carma::util::Serializable::getTotalSerialBytes( ) const
{
    return (1 + sizeof( long ) + getSizeInBytes());
}


inline void
carma::util::Serializable::serialIntoByteVec(
    ::std::vector< char > & byteVec ) const
{
    byteVec.resize( getTotalSerialBytes() );

    char * const byteArray = &(byteVec[0]);

    // first byte = true if data is in little endian
    byteArray[0] = isLittleEndian();

    int offset = 1;

    // next comes the version tag
    const int32_t version = 2;
    pack( version, byteArray, &offset );

    // finally the data
    mySerialize( byteArray, &offset );
}


inline void
carma::util::Serializable::serialIntoByteBuffer(
    ByteBuffer & byteBuffer ) const
{
    byteBuffer.destructiveResize( getTotalSerialBytes() );

    char * const byteArray = byteBuffer.get();

    // first byte = true if data is in little endian
    byteArray[0] = isLittleEndian();

    int offset = 1;

    // next comes the version tag
    const int32_t version = 2;
    pack( version, byteArray, &offset );

    // finally the data
    mySerialize( byteArray, &offset );
}


inline void
carma::util::Serializable::serialIntoByteArray(
    char * const byteArray,
    const int    byteArraySize,
    int * const  totalSerialBytes ) const
{
    const int totalNeededBytes = getTotalSerialBytes();
    
    if ( totalNeededBytes > byteArraySize ) {
        throwByteArrayTooSmallError( totalNeededBytes, byteArraySize );
    } else {
        // first byte = true if data is in little endian
        byteArray[0] = isLittleEndian();

        int offset = 1;

        // next comes the version tag
        const int32_t version = 2;
        pack( version, byteArray, &offset );
    
        // finally the data
        mySerialize( byteArray, &offset );
        
        if ( totalSerialBytes != 0 )
            *totalSerialBytes = totalNeededBytes;
    }
}


inline void
carma::util::Serializable::serialize( char * const byteArray,
                                      int * const  offset ) const
{
    mySerialize( byteArray, offset );
}


inline void 
carma::util::Serializable::preemptivelyCheckForByteArrayOverrun( 
    const int maxBufferSize,
    const int futureOffset )
{
    if ( futureOffset > maxBufferSize ) 
      throwByteArrayOverrunError( maxBufferSize,
                                  futureOffset );
}

#endif
