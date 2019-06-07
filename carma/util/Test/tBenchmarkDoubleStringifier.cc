//
// @version $Revision: 1.9 $ $Date: 2006/05/03 22:32:42 $
//
// @usage use it
//
// @description
//  Test program for benchmarking the DoubleStringifier class as used by the
//  monitor average writer process.
//
// @key singletons 90000 int
//                 Number of singlteon points to do every cycle
//
// @key heavys 10000 int
//             Number of heavy points to do every cycle
//
// @key heavySize 50 int
//                Number of samples for heavy points
//
// @key cycles 60 int
//                Number of half second aligned cycles to do
//
// @key technique carma string
//                Pick one of { bytes, snprintf, fp, longlong, carma }
//
// @logger TEST_FACILITY carma.test.util.tBenchmarkDoubleStringifier
//


#include <vector>
#include <fstream>
#include <iostream>
#include <sstream>
#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <cmath>

#include <sys/time.h>
#include <pthread.h>

#include "carma/util/DoubleStringifier.h"
#include "carma/util/Program.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


DoubleStringifier gDoubleStringifier;


typedef vector< double > DoubleVector;

typedef vector< DoubleVector > DoubleVectorVector;

typedef vector< char > OutputVector;


typedef enum {
    AS_BYTES,
    AS_STRING_VIA_SNPRINTF,
    AS_STRING_VIA_FAST_FP,
    AS_STRING_VIA_FAST_LONG_LONG,
    AS_STRING_VIA_CARMA_STRINGIFIER,
} OutputTechnique;


template< OutputTechnique outputTechnique >
::size_t
EmitDouble( double   value,
            char *   ovp,
            ::size_t ovMaxCount );

template< >
::size_t
EmitDouble< AS_BYTES >( const double   value,
                        char *         ovp,
                        const ::size_t ovMaxCount )
{
    const ::size_t kDoubleSize = sizeof( double );
    
    union {
        double d;
        char   bytes[ kDoubleSize ];
    } u;
    
    u.d = value;

    ::size_t i = 0;
    
    for ( ; i < kDoubleSize; ++i )
        ovp[ i ] = u.bytes[ i ];
    
    return i;
}


template< >
::size_t
EmitDouble< AS_STRING_VIA_SNPRINTF >( const double   value,
                                      char *         ovp,
                                      const ::size_t ovMaxCount ) {
    char buffer[ 64 ];
    
    ::snprintf( buffer, sizeof( buffer ), "%23.16e ", value );

    buffer[ sizeof( buffer ) - 1 ] = 0;

    ::size_t i = 0;
    
    for ( ; buffer[ i ] != 0; ++i )
        ovp[ i ] = buffer[ i ];
        
    return i;
}


template< >
::size_t
EmitDouble< AS_STRING_VIA_FAST_FP >( const double   value,
                                     char *         ovp,
                                     const ::size_t ovMaxCount )
{
    ::size_t i = 0;
    
    if ( value == 0.0 ) {
        ovp[ i++ ] = '0';
    } else {
        const double valueLogTen = std::log10( value );
        const int startTenExponent = static_cast< int >( valueLogTen );
        
        double powerOfTen = std::pow( 10.0, startTenExponent );
        double inversePowerOfTen = std::pow( 0.1, startTenExponent );
        
        double tail = value;
        
        while ( i < 15 ) {
            const int digit = static_cast< int >( tail * inversePowerOfTen );
            const char digitChar = static_cast< char >( '0' + digit );
            
            ovp[ i++ ] = digitChar;
            
            tail -= (digit * powerOfTen);

            powerOfTen *= 0.1;
            inversePowerOfTen *= 10.0; 
        }
    }
    
    ovp[ i++ ] = '\n';

    return i;
}


char gChunkToChars[ 10000 ][ 4 ];


template< >
::size_t
EmitDouble< AS_STRING_VIA_FAST_LONG_LONG >( const double   value,
                                            char *         ovp,
                                            const ::size_t ovMaxCount )
{
    ::size_t i = 0;
    
    if ( value == 0.0 ) {
        ovp[ i++ ] = '0';
    } else {
        const double absValue = std::fabs( value );
            
        const double valueLogTen = std::floor( std::log10( absValue ) );
        const double scale = std::pow( 10.0, (15.0 - valueLogTen) );
        const unsigned long long scaled = static_cast< unsigned long long >( absValue * scale );

        const unsigned long chunkFactor = 10000;
        const unsigned long twoChunkFactor = chunkFactor * chunkFactor;
        
        const unsigned long top2Chunks = scaled / twoChunkFactor;
        const unsigned long bottom2Chunks = scaled - (top2Chunks * twoChunkFactor);
        
#if 0
        if ( top2Chunks < 0 || top2Chunks >= twoChunkFactor ||
             bottom2Chunks < 0 || bottom2Chunks >= twoChunkFactor ) {
            std::cerr << "oops!: "
                      << value << " "
                      << scaled << " "
                      << top2Chunks << " "
                      << bottom2Chunks << std::endl;
        }
#endif

        const unsigned long chunk0 = top2Chunks / chunkFactor;
        const unsigned long chunk1 = top2Chunks - chunk0 * chunkFactor;
        const unsigned long chunk2 = bottom2Chunks / chunkFactor;
        const unsigned long chunk3 = bottom2Chunks - chunk2 * chunkFactor;
        
#if 0
        if ( chunk0 < 0 || chunk0 >= chunkFactor ||
             chunk1 < 0 || chunk1 >= chunkFactor ||
             chunk2 < 0 || chunk2 >= chunkFactor ||
             chunk3 < 0 || chunk3 >= chunkFactor ) {
            std::cerr << "oops!: "
                      << value << " "
                      << scaled << " "
                      << chunk0 << " "
                      << chunk1 << " "
                      << chunk2 << " "
                      << chunk3 << std::endl;
        }
#endif

        ovp[ i++ ] = gChunkToChars[ chunk0 ][ 0 ];
        ovp[ i++ ] = gChunkToChars[ chunk0 ][ 1 ];
        ovp[ i++ ] = gChunkToChars[ chunk0 ][ 2 ];
        ovp[ i++ ] = gChunkToChars[ chunk0 ][ 3 ];

        ovp[ i++ ] = gChunkToChars[ chunk1 ][ 0 ];
        ovp[ i++ ] = gChunkToChars[ chunk1 ][ 1 ];
        ovp[ i++ ] = gChunkToChars[ chunk1 ][ 2 ];
        ovp[ i++ ] = gChunkToChars[ chunk1 ][ 3 ];
        
        ovp[ i++ ] = gChunkToChars[ chunk2 ][ 0 ];
        ovp[ i++ ] = gChunkToChars[ chunk2 ][ 1 ];
        ovp[ i++ ] = gChunkToChars[ chunk2 ][ 2 ];
        ovp[ i++ ] = gChunkToChars[ chunk2 ][ 3 ];

        ovp[ i++ ] = gChunkToChars[ chunk3 ][ 0 ];
        ovp[ i++ ] = gChunkToChars[ chunk3 ][ 1 ];
        ovp[ i++ ] = gChunkToChars[ chunk3 ][ 2 ];
        ovp[ i++ ] = gChunkToChars[ chunk3 ][ 3 ];
    }
    
    ovp[ i++ ] = '\n';
    
    return i;
}


template< OutputTechnique outputTechnique >
::size_t
EmitDoubles( const DoubleVector & dv,
             char *               ovp,
             ::size_t             ovMaxCount )
{
    ::size_t usedCount = 0;
    
    DoubleVector::const_iterator iDv = dv.begin();
    const DoubleVector::const_iterator iDvEnd = dv.end();

    for ( ; iDv != iDvEnd; ++iDv ) {
        const ::size_t iUsedCount =
            EmitDouble< outputTechnique >( *iDv, ovp, ovMaxCount );
        
        ovMaxCount -= iUsedCount;
        usedCount += iUsedCount;
        ovp += iUsedCount;
    }
    
    return usedCount;
}


template< >
::size_t
EmitDoubles< AS_STRING_VIA_CARMA_STRINGIFIER >(
    const DoubleVector & dv,
    char *               ovp,
    const ::size_t       ovMaxCount )
{
    const ::size_t kMaxCharsPerValue =
        DoubleStringifier::MAX_CHARS_PER_VALUE;
        
    if ( ovMaxCount < (dv.size() * kMaxCharsPerValue) )
        throw logic_error( "Buffer too small" );
                                                
    ::size_t usedCount = 0;
    
    DoubleVector::const_iterator iDv = dv.begin();
    const DoubleVector::const_iterator iDvEnd = dv.end();

    for ( ; iDv != iDvEnd; ++iDv ) {
        const ::size_t iUsedCount =
            gDoubleStringifier.stringifyDouble( *iDv, ovp, kMaxCharsPerValue );
        
        usedCount += iUsedCount;
        ovp += iUsedCount;
    }
    
    return usedCount;
}


void
AddAverage( const DoubleVector & dv,
            DoubleVector &       odv )
{
    DoubleVector::const_iterator i = dv.begin();
    const DoubleVector::const_iterator iEnd = dv.end();

    if ( i == iEnd ) {
        // empty value set
        odv.push_back( 0.0 );
    } else {
        double sum = *i;
        ::size_t count = 1;
        
        ++i;
        
        if ( i == iEnd )
            odv.push_back( sum );
        else {
            for ( ; i != iEnd; ++i ) {
                sum += *i;

                ++count;
            }
        
            odv.push_back( sum / count );
        }
    }
}


void
AddAverages( const DoubleVectorVector & valueVectorVector,
             DoubleVector &             dv )
{
    DoubleVectorVector::const_iterator i = valueVectorVector.begin();
    const DoubleVectorVector::const_iterator iEnd = valueVectorVector.end();
    
    for ( ; i != iEnd; ++i )
        AddAverage( *i, dv );
}


DoubleVectorVector
FakeUp( const ::size_t numSingletonPoints, 
        const ::size_t numHeavyPoints,
        const ::size_t heavyPointSize )
{
    DoubleVectorVector result;
    
    result.reserve( numSingletonPoints + numHeavyPoints );
    
    for ( ::size_t i = 0; i < numSingletonPoints; ++i ) {
        DoubleVector dv;
        
        dv.push_back( 3.1416 * i );
        
        result.push_back( dv );
    }
        
    for ( ::size_t i = 0; i < numHeavyPoints; ++i ) {
        DoubleVector dv;
        
        dv.reserve( heavyPointSize );
        
        for ( ::size_t j = 0; j < heavyPointSize; ++j )
            dv.push_back( 3.1416 * i + 2.718 * j );
            
        result.push_back( dv );
    }
    
    return result;
}


pthread_mutex_t gMutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t gCond = PTHREAD_COND_INITIALIZER;


void
LookForAProblem( const double e )
{
    const double kBigVal = 1.0e+300;
    
    const double v = pow( 10.0, e );

    double bv = v;
    double av = v;
    
    for ( int i = 0; i < 16; ++i ) {
        const ::size_t kMaxCount =
            DoubleStringifier::MAX_CHARS_PER_VALUE;
            
        char bs[ kMaxCount ];
        char as[ kMaxCount ];
        
        gDoubleStringifier.stringifyDouble( bv, bs, kMaxCount );
        gDoubleStringifier.stringifyDouble( av, as, kMaxCount );

        bv = nextafter( bv, -kBigVal );
        av = nextafter( av, kBigVal );
    }
}


void
LookForAProblem( )
{
    LookForAProblem( 0.0 );

    for ( double i = -200.0; i <= 200.0; i += 0.5 )
        LookForAProblem( i );
}


void
DoIt( const size_t          numSingletonPoints,
      const size_t          numHeavyPoints,
      const size_t          heavyPointSize,
      const int             numCycles,
      const OutputTechnique outputTechnique )
{
    LookForAProblem();

    cout << "Streaming doubles ";
    
    switch ( outputTechnique ) {
        case AS_BYTES:
            cout << "as bytes";
            break;
            
        case AS_STRING_VIA_SNPRINTF:
            cout << "as strings via snprintf";
            break;

        case AS_STRING_VIA_FAST_FP:
            cout << "as strings via fast powers of FP based routine";
            break;

        case AS_STRING_VIA_FAST_LONG_LONG:
            cout << "as strings via fast log10 long long based routine";
            break;
            
        case AS_STRING_VIA_CARMA_STRINGIFIER:
            cout << "as strings via carma stringifier";
            break;
            
        default:
            ::exit( 1 );
            break;
    }
    
    cout << ".\nDoing " << numSingletonPoints << " singleton points and "
         << numHeavyPoints << " heavy points every aligned half second for "
         << numCycles << " cycles (" << (0.5 * numCycles) << " seconds)."
         << endl;
        
    ofstream avgFileStream( "avgFile" );

    const DoubleVectorVector dvv =
        FakeUp( numSingletonPoints, numHeavyPoints, heavyPointSize );
    
    DoubleVector avgs;
    avgs.reserve( dvv.size() );

    AddAverages( dvv, avgs );

    OutputVector ov;    
    ov.resize( (2 * DoubleStringifier::MAX_CHARS_PER_VALUE) * avgs.size() );

    for ( int i = 0; i < 10000; ++i ) {
        gChunkToChars[ i ][ 0 ] = static_cast< char >( '0' + ((i / 1000) % 10) );
        gChunkToChars[ i ][ 1 ] = static_cast< char >( '0' + ((i / 100) % 10) );
        gChunkToChars[ i ][ 2 ] = static_cast< char >( '0' + ((i / 10) % 10) );
        gChunkToChars[ i ][ 3 ] = static_cast< char >( '0' + ((i / 1) % 10) );
    }

    ::pthread_mutex_lock( &::gMutex );

    {
        struct ::timeval t;
    
        ::gettimeofday( &t, 0 );

        cout << "starting (" << t.tv_sec << " sec, " << t.tv_usec << " usec)..." << endl;
    }
    
    struct ::timespec absTime;

    {
        struct ::timeval now;
        
        ::gettimeofday( &now, 0 );
        
        absTime.tv_sec = (now.tv_sec + 2);
        absTime.tv_nsec = 0;
    }

    for ( int i = 0; i < numCycles; ++i ) {
        while ( true ) {
            const int err = ::pthread_cond_timedwait( &::gCond, &::gMutex, &absTime );
            
            if ( err == ETIMEDOUT )
                break;
                
            if ( err != EINTR )
                ::exit( 1 );
        }

        char * ovp = &(ov[ 0 ]);
        const ::size_t ovMaxCount = ov.size();
        ::size_t ovCount = 0;
        
        switch ( outputTechnique ) {
            case AS_BYTES:
                ovCount = EmitDoubles< AS_BYTES >( avgs, ovp, ovMaxCount );
                break;
                
            case AS_STRING_VIA_SNPRINTF:
                ovCount = EmitDoubles< AS_STRING_VIA_SNPRINTF >( avgs, ovp, ovMaxCount );
                break;
                
            case AS_STRING_VIA_FAST_FP:
                ovCount = EmitDoubles< AS_STRING_VIA_FAST_FP >( avgs, ovp, ovMaxCount );
                break;

            case AS_STRING_VIA_FAST_LONG_LONG:
                ovCount = EmitDoubles< AS_STRING_VIA_FAST_LONG_LONG >( avgs, ovp, ovMaxCount );
                break;

            case AS_STRING_VIA_CARMA_STRINGIFIER:
                ovCount = EmitDoubles< AS_STRING_VIA_CARMA_STRINGIFIER >( avgs, ovp, ovMaxCount );
                break;
        }
        
        avgFileStream.write( &(ovp[ 0 ]), ovCount );

        avgFileStream.flush();

        if ( absTime.tv_nsec == 0 )
            absTime.tv_nsec = 500L * 1000L * 1000L;
        else {
            absTime.tv_nsec = 0;
            ++(absTime.tv_sec);
        }
    }

    {
        struct ::timeval t;
    
        ::gettimeofday( &t, 0 );

        cout << "Done (" << t.tv_sec << " sec, " << t.tv_usec << " usec)." << endl;
    }

    ::pthread_mutex_unlock( &::gMutex );
}


OutputTechnique
stringToOutputTechnique( const string & s )
{
    OutputTechnique result = AS_STRING_VIA_CARMA_STRINGIFIER;

    if ( s == "bytes" )
        result = AS_BYTES;
    else if ( s == "snprintf" )
        result = AS_STRING_VIA_SNPRINTF;
    else if ( s == "fp" )
        result = AS_STRING_VIA_FAST_FP;
    else if ( s == "longlong" )
        result = AS_STRING_VIA_FAST_LONG_LONG;
    else if ( s == "carma" )
        result = AS_STRING_VIA_CARMA_STRINGIFIER;
        
    return result;
}


}  // anonymous namespace


int
Program::main( )
{
    const int singletons = getIntParameter( "singletons" );
    const int heavys = getIntParameter( "heavys" );
    const int heavySize = getIntParameter( "heavySize" );
    const int cycles = getIntParameter( "cycles" );
    
    const OutputTechnique outputTechnique =
        stringToOutputTechnique( getStringParameter( "technique" ) );
    
    DoIt( singletons,
          heavys,
          heavySize,
          cycles,
          outputTechnique );
    
    return 0;
}
