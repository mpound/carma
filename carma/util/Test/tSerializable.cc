#include "carma/util/Program.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Serializable.h"

#include <assert.h>
#include <cmath>
#include <complex>
#include <vector>

using namespace carma::util;
using namespace std;

namespace {

    class SerializeTest : public carma::util::Serializable {
    public:

        void mySerialize( char * byteArray, int * offset ) const;

        void deserializeVer0( const char * byteArray, 
                              int * offset,
                              int byteArraySize);

        void deserializeSwapVer0( const char * byteArray, 
                                  int * offset,
                                  int byteArraySize);

        void deserializeVer1( const char * byteArray, 
                              int * offset,
                              int byteArraySize);

        void deserializeSwapVer1( const char * byteArray, 
                                  int * offset,
                                  int byteArraySize);

        int getSizeInBytes( ) const;

        bool operator==( const SerializeTest & with ) const;

        // Noahs ark - provide one of each data type which is serializable
        bool aBool;
        char aChar;
        short aShort;
        int anInt;
        long aLong;
        float aFloat;
        double aDouble;
        complex<float> aComplex;
        vector< short > shortVector;
        vector< int > intVector;
        vector< float > floatVector;
        vector< complex< float > > complexVector;

    private:

    }; // class SerializeTest

    void
    SerializeTest::mySerialize( char * const byteArray,
                                int * const  offset ) const
    {
        pack( aBool, byteArray, offset );
        pack( aChar, byteArray, offset );
        pack( aShort, byteArray, offset );
        pack( anInt, byteArray, offset );
        pack( aLong, byteArray, offset );
        pack( aFloat, byteArray, offset );
        pack( aDouble, byteArray, offset );
        pack( aComplex, byteArray, offset );
        pack( shortVector, byteArray, offset );
        pack( intVector, byteArray, offset );
        pack( floatVector, byteArray, offset );
        pack( complexVector, byteArray, offset );
    }

    void
    SerializeTest::deserializeVer1( const char * const byteArray,
                                    int * const        offset,
                                    const int          byteArraySize )
    {
        deserializeVer0( byteArray, offset, byteArraySize );
    }

    void
    SerializeTest::deserializeVer0( const char * const byteArray,
                                    int * const        offset,
                                    const int          byteArraySize )
    {
        unpack( aBool, byteArray, offset, byteArraySize );
        unpack( aChar, byteArray, offset, byteArraySize  );
        unpack( aShort, byteArray, offset, byteArraySize  );
        unpack( anInt, byteArray, offset, byteArraySize  );
        unpack( aLong, byteArray, offset, byteArraySize  );
        unpack( aFloat, byteArray, offset, byteArraySize  );
        unpack( aDouble, byteArray, offset, byteArraySize  );
        unpack( aComplex, byteArray, offset, byteArraySize  );
        shortVector.resize( anInt );
        unpack( shortVector, byteArray, offset, byteArraySize  );
        intVector.resize( anInt );
        unpack( intVector, byteArray, offset, byteArraySize  );
        floatVector.resize( anInt );
        unpack( floatVector, byteArray, offset, byteArraySize  );
        complexVector.resize( anInt );
        unpack( complexVector, byteArray, offset, byteArraySize  );
    }
    
    void
    SerializeTest::deserializeSwapVer1( const char * const byteArray,
                                        int * const        offset,
                                        const int          byteArraySize )
    {
        deserializeSwapVer0( byteArray, offset, byteArraySize );
    }

    void
    SerializeTest::deserializeSwapVer0( const char * const byteArray,
                                        int * const        offset,
                                        const int          byteArraySize )
    {
        unpackSwap( aBool, byteArray, offset, byteArraySize );
        unpackSwap( aChar, byteArray, offset, byteArraySize  );
        unpackSwap( aShort, byteArray, offset, byteArraySize  );
        unpackSwap( anInt, byteArray, offset, byteArraySize  );
        unpackSwap( aLong, byteArray, offset, byteArraySize  );
        unpackSwap( aFloat, byteArray, offset, byteArraySize  );
        unpackSwap( aDouble, byteArray, offset, byteArraySize  );
        unpackSwap( aComplex, byteArray, offset, byteArraySize  );
        shortVector.resize( anInt );
        unpackSwap( shortVector, byteArray, offset, byteArraySize  );
        intVector.resize( anInt );
        unpackSwap( intVector, byteArray, offset, byteArraySize  );
        floatVector.resize( anInt );
        unpackSwap( floatVector, byteArray, offset, byteArraySize  );
        complexVector.resize( anInt );
        unpackSwap( complexVector, byteArray, offset, byteArraySize  );
    }

    int
    SerializeTest::getSizeInBytes( ) const
    {
        int size = 0;
        size += sizeof( aBool );
        size += sizeof( aChar );
        size += sizeof( aShort );
        size += sizeof( anInt );
        size += sizeof( aLong );
        size += sizeof( aFloat );
        size += sizeof( aDouble );
        size += sizeof( aComplex );
        size += sizeof( short ) * shortVector.size( );
        size += sizeof( int ) * intVector.size( );
        size += sizeof( float ) * floatVector.size( );
        size += sizeof( complex<float> ) * complexVector.size( );
        return size;
    }

    bool
    SerializeTest::operator==( const SerializeTest & with ) const
    {
        return ( aBool == with.aBool &&
             aChar == with.aChar &&
             aShort == with.aShort &&
             anInt == with.anInt &&
             aLong == with.aLong &&
             aFloat == with.aFloat &&
             aDouble == with.aDouble &&
             aComplex == with.aComplex &&
             shortVector == with.shortVector &&
             intVector == with.intVector &&
             floatVector ==  with.floatVector &&
             complexVector == with.complexVector );
    }

} // namespace unnamed

/**
 * @noKeys
 *
 * @logger TEST_FACILITY carma.test.util.Serializable
 */
int Program::main( ) {

    // Verify that packing and unpacking routines function correctly
    vector< char > byteVec; // Generic for all packing and unpacking
    ByteBuffer byteBuffer;
    
    complex<float> testComplex( 2.7182818, 4.0 );

    SerializeTest original;

    original.aBool = true;
    original.aChar = 'Z';
    original.aShort = 0xBA;
    original.anInt = 10;
    original.aFloat = M_PI;
    original.aDouble = 2.0 * M_PI;
    original.aComplex = testComplex;

    for ( int i = 0; i < original.anInt; ++i ) {
       original.shortVector.push_back( i );
       original.intVector.push_back( i * 1000 );
       original.floatVector.push_back( i * M_PI );
       original.complexVector.push_back( complex<float>( i, i * M_PI ) );
    }

    original.serialIntoByteVec( byteVec );
    original.serialIntoByteBuffer( byteBuffer );

    SerializeTest reserialized;

    reserialized.deserial( byteVec );

    // Verify some of the magic numbers
    assert( reserialized.aChar == 'Z' );
    assert( reserialized.aShort == 0xBA );
    assert( reserialized.aComplex == testComplex );

    assert( reserialized == original );

    reserialized.deserial( byteBuffer );

    // Verify some of the magic numbers
    assert( reserialized.aChar == 'Z' );
    assert( reserialized.aShort == 0xBA );
    assert( reserialized.aComplex == testComplex );

    assert( reserialized == original );

    // Verify throw on deserialization overrun by forcing serialized 
    // data to disagree with deserialization scheme.
    byteVec.pop_back();

    SerializeTest deserialized;

    try {
        deserialized.deserial( byteVec );
    } catch (...) {
        const string errMsg = getStringForCaught();
        cout << errMsg << endl;
        cout << "Exception caught as expected." << endl;
    }

    original.complexVector.pop_back();

    original.serialIntoByteVec( byteVec );
    
    try {
        deserialized.deserial( byteVec );
    } catch (...) {
        const string errMsg = getStringForCaught();
        cout << errMsg << endl;
        cout << "Exception caught as expected." << endl;
    }

    return 0;
} // Program::main
