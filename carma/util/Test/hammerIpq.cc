//
// @version $Revision: 1.19 $
//
// @usage use it
//
// @description
//  Test program for trying to flood an IPQ with a large volume of
//  reads and writes (usually with a small size queue element).
//
// @key  writers  4  int
//       Number of writer threads
//
// @key  readers  2  int
//       Number of reader threads
//
// @key  duration  5  int
//       Approximate number of seconds to run the test
//
// @key  writeUsec  200  int
//       Period in usecs of writes. 0 means write with no delay.
//
// @key  logLost  false  bool
//       Whether or not to log lost elements in readers.
//
// @logger TEST_FACILITY carma.test.util.hammerIpq
//


#include <iomanip>
#include <iostream>
#include <string>
#include <sstream>
#include <memory>

#include "carma/canbus/Message.h"
#include "carma/util/AutoPthreadQuitAndJoinGroup.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IPQwriter.h"
#include "carma/util/IPQreader.h"
#include "carma/util/OneShotBarrier.h"
#include "carma/util/PeriodicTimer.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedUmask.h"
#include "carma/util/StartPthread.h"
#include "carma/util/ThreadQuit.h"

using namespace ::std;
using namespace carma;
using namespace carma::canbus;
using namespace carma::util;


namespace {

bool               gCorrectnessErrorDetected = false;
unsigned long long gWrites = 0;
unsigned long long gReads = 0;

const unsigned long kMicrosecsPerSec = 1000UL * 1000UL;

const string kIpqName = "hammer";
const int kIpqElements = 150000;

const size_t kWritesPerQuitCheck = 1000;

const string kWriterIpqsReadyMsg =
    "All writers in this process have their IPQs ready";


struct WriterArgs {
    OneShotBarrier &     writerIpqsReadyBarrier;
    OneShotBarrier &     writersAndReadersReadyBarrier;
    const size_t         writeUsec;
    const ::pid_t        writerPid;
    const unsigned short writerThreadId;

    WriterArgs( OneShotBarrier & inWriterIpqsReadyBarrier,
                OneShotBarrier & inWritersAndReadersReadyBarrier,
                size_t           inWriteUsec,
                ::pid_t          inWriterPid,
                unsigned short   inWriterThreadId );
};


WriterArgs::WriterArgs( OneShotBarrier &     inWriterIpqsReadyBarrier,
                        OneShotBarrier &     inWritersAndReadersReadyBarrier,
                        const size_t         inWriteUsec,
                        const ::pid_t        inWriterPid,
                        const unsigned short inWriterThreadId ) :
writerIpqsReadyBarrier( inWriterIpqsReadyBarrier ),
writersAndReadersReadyBarrier( inWritersAndReadersReadyBarrier ),
writeUsec( inWriteUsec ),
writerPid( inWriterPid ),
writerThreadId( inWriterThreadId )
{
}


unsigned long long
decodeIndex( const vector< unsigned char > & data )
{
    const unsigned int hi =
        (static_cast< unsigned long >( data[ 0 ] ) << 24) |
        (static_cast< unsigned long >( data[ 1 ] ) << 16) |
        (static_cast< unsigned long >( data[ 2 ] ) << 8) |
         static_cast< unsigned long >( data[ 3 ] );
        
    const unsigned int lo =
        (static_cast< unsigned long >( data[ 4 ] ) << 24) |
        (static_cast< unsigned long >( data[ 5 ] ) << 16) |
        (static_cast< unsigned long >( data[ 6 ] ) << 8) |
         static_cast< unsigned long >( data[ 7 ] );

    return ((static_cast< unsigned long long >( hi ) << 32) |
             static_cast< unsigned long long >( lo ));
}


void
encodeIndex( vector< unsigned char > & data,
             const unsigned long long  index )
{
    {
        const unsigned int hi =
            static_cast< unsigned int >( index >> 32 );
            
        data[ 0 ] = static_cast< unsigned char >( hi >> 24 );
        data[ 1 ] = static_cast< unsigned char >( hi >> 16 );
        data[ 2 ] = static_cast< unsigned char >( hi >> 8 );
        data[ 3 ] = static_cast< unsigned char >( hi );
    }
    
    {
        const unsigned int lo = static_cast< unsigned int >( index );
            
        data[ 4 ] = static_cast< unsigned char >( lo >> 24 );
        data[ 5 ] = static_cast< unsigned char >( lo >> 16 );
        data[ 6 ] = static_cast< unsigned char >( lo >> 8 );
        data[ 7 ] = static_cast< unsigned char >( lo );
    }
}


void
writerEp( const WriterArgs & args )
try {
    programLogInfoIfPossible( "Started" );

    {
        const size_t writeUsec = args.writeUsec;

        const double writerPidAsDouble =
            static_cast< double >( args.writerPid );
            
        const unsigned short writerThreadId = args.writerThreadId;
        
        IPQwriter< canbus::Message > ipq( kIpqName, true, kIpqElements );
        
        ipq.setRxMjd( writerPidAsDouble );
        ipq.setBusId( writerThreadId );
        
        args.writerIpqsReadyBarrier.wait();
        programLogInfoIfPossible( kWriterIpqsReadyMsg );
        
        struct ::timeval period;
        
        period.tv_sec = (writeUsec / kMicrosecsPerSec);
        period.tv_usec = (writeUsec % kMicrosecsPerSec);

        PeriodicTimer timer( period );
        
        if ( writeUsec != 0 ) {
            struct ::timeval firstFire;
            
            ::gettimeofday( &firstFire, 0 );
            
            timer.ResetNextFireAbsoluteTime( firstFire );
        }
        
        size_t writesUntilNextQuitCheck = kWritesPerQuitCheck;
        vector< unsigned char > data;
        
        data.resize( 8 );
        
        args.writersAndReadersReadyBarrier.wait();
        
        unsigned long long index =
            ((1ULL << (45 + (args.writerPid & 0x0F))) |
             (static_cast< unsigned long long >( args.writerPid ) << 25) |
             (static_cast< unsigned long long >( writerThreadId ) << 20));
            
        for ( ; true; ++index ) {
            if ( ipq.getRxMjd() != writerPidAsDouble ) {
                gCorrectnessErrorDetected = true;

                ipq.setRxMjd( writerPidAsDouble );

                programLogErrorIfPossible( "Writer PID was lost" );
            }
            
            if ( ipq.getBusId() != writerThreadId ) {
                gCorrectnessErrorDetected = true;

                ipq.setBusId( writerThreadId );

                programLogErrorIfPossible( "Writer thread ID was lost" );
            }
            
            if ( data.size() != 8 ) {
                gCorrectnessErrorDetected = true;

                data.resize( 8 );

                programLogErrorIfPossible( "Data size was lost" );
            }
            
            encodeIndex( data, index );
            
            ipq.setData( data );

            if ( writeUsec != 0 )
                timer.WaitForNextFireTime();
            else if ( writesUntilNextQuitCheck != 0 )
                --writesUntilNextQuitCheck;
            else {
                ThreadQuitTestSelf();
                writesUntilNextQuitCheck = kWritesPerQuitCheck;
            }
                
            ipq.write();
            
            ++gWrites;

            {
                const vector< unsigned char > dataOut = ipq.getData();
                    
                if ( (dataOut.size() != 8) ||
                     (decodeIndex( dataOut ) != index) ) {
                    gCorrectnessErrorDetected = true;

                    ostringstream oss;
                
                    oss << "Index encoding of " << index << " was lost";
                
                    programLogErrorIfPossible( oss.str() );
                }
            }
        }
    }

    programLogInfoIfPossible( "Done" );
} catch ( ... ) {
    if ( CaughtExceptionIsThreadQuitRequestedError() )
        programLogInfoIfPossible( "writerEp quitting as requested" );
    else {
        gCorrectnessErrorDetected = true;

        programLogErrorIfPossible( "Coming out of writerEp on an exception: " +
                                   getStringForCaught() );
    }
    
    throw;
}


struct ReaderArgs {
    OneShotBarrier & writerIpqsReadyBarrier;
    OneShotBarrier & writersAndReadersReadyBarrier;
    const bool       haveWriters;
    const bool       logLostElements;
    
    ReaderArgs( OneShotBarrier & inWriterIpqsReadyBarrier,
                OneShotBarrier & inWritersAndReadersReadyBarrier,
                bool             inHaveWriters,
                bool             inLogLostElements );
};


ReaderArgs::ReaderArgs( OneShotBarrier & inWriterIpqsReadyBarrier,
                        OneShotBarrier & inWritersAndReadersReadyBarrier,
                        const bool       inHaveWriters,
                        const bool       inLogLostElements ) :
writerIpqsReadyBarrier( inWriterIpqsReadyBarrier ),
writersAndReadersReadyBarrier( inWritersAndReadersReadyBarrier ),
haveWriters( inHaveWriters ),
logLostElements( inLogLostElements )
{
}


class WriterInfo {
    public:
        WriterInfo( double         writerPidAsDouble,
                    unsigned short writerThreadId );
    
        bool operator<( const WriterInfo & rhs ) const;
    
    private:
        double         writerPidAsDouble_;
        unsigned short writerThreadId_;
};


WriterInfo::WriterInfo( const double         writerPidAsDouble,
                        const unsigned short writerThreadId ) :
writerPidAsDouble_( writerPidAsDouble ),
writerThreadId_( writerThreadId )
{
}


bool
WriterInfo::operator<( const WriterInfo & rhs ) const
{
    if ( writerPidAsDouble_ < rhs.writerPidAsDouble_ )
        return true;
        
    if ( writerPidAsDouble_ > rhs.writerPidAsDouble_ )
        return false;
        
    return (writerThreadId_ < rhs.writerThreadId_);
}


struct IndexLims {
    unsigned long long min;
    unsigned long long max;
};


void
readerEp( const ReaderArgs & args )
try {
    programLogInfoIfPossible( "Started" );

    unsigned long long totalReads = 0;
    unsigned long long totalLossyReads = 0;
    unsigned long long totalLostElements = 0;

    try {
        if ( args.haveWriters ) {
            args.writerIpqsReadyBarrier.wait();
            programLogInfoIfPossible( kWriterIpqsReadyMsg );
        }
        
        const bool logLostElements = args.logLostElements;
        
        IPQreader< canbus::Message > ipq( kIpqName, false, kIpqElements );
        
        ipq.setNoneAvailable();
        
        map< WriterInfo, IndexLims > expectedNext;
        
        args.writersAndReadersReadyBarrier.wait();
        
        while ( true ) {
            const unsigned int lostElements = ipq.read();
            
            ++totalReads;
            ++gReads;

            const vector< unsigned char > data = ipq.getData();

            if ( data.size() != 8 ) {
                gCorrectnessErrorDetected = true;

                ostringstream oss;
                
                oss << "Read #" << totalReads
                    << " had a bad data size of " << data.size();
                    
                programLogErrorIfPossible( oss.str() );

                continue;
            }
            
            const unsigned long long index = decodeIndex( data );

            const double writerPidAsDouble = ipq.getRxMjd();
            const unsigned short writerThreadId = ipq.getBusId();
            
            if ( lostElements != 0 ) {
                if ( logLostElements ) {
                    ostringstream oss;
    
                    oss << "Read #" << totalReads
                        << " lost " << lostElements << " elements";
                    
                    programLogWarnIfPossible( oss.str() );
                }
                
                ++totalLossyReads;
                totalLostElements += lostElements;
                
                map< WriterInfo, IndexLims >::iterator i =
                    expectedNext.begin();

                const map< WriterInfo, IndexLims >::iterator iEnd =
                    expectedNext.end();
                    
                for ( ; i != iEnd; ++i )
                    i->second.max += lostElements;
            }

            const WriterInfo writerInfo( writerPidAsDouble, writerThreadId );
            
            map< WriterInfo, IndexLims >::iterator iExpectedNext =
                expectedNext.find( writerInfo );
                
            if ( iExpectedNext == expectedNext.end() ) {
                IndexLims lims;
                
                lims.min = index + 1;
                lims.max = lims.min;

                expectedNext.insert( make_pair( writerInfo, lims ) );

                ostringstream oss;
                
                oss << "Read #" << totalReads << " was a new writer "
                    << static_cast< int >( writerPidAsDouble )
                    << ":" << writerThreadId
                    << " with first index of 0x"
                    << setbase( 16 ) << index << " [";
                    
                for ( int i = 0; i < 8; ++i ) {
                    const unsigned char j = data[ i ];
                    
                    if ( i != 0 )
                        oss << ' ';

                    oss << static_cast< unsigned short >( j / 16 )
                        << static_cast< unsigned short >( j % 16 );
                }
                    
                oss << "]";

                programLogInfoIfPossible( oss.str() );
            } else {
                if ( (index < iExpectedNext->second.min) ||
                     (index > iExpectedNext->second.max) ) {
                    gCorrectnessErrorDetected = true;

                    ostringstream oss;
                    
                    oss << "Read #" << totalReads
                        << " index 0x" << setbase( 16 ) << index
                        << setbase( 10 ) << " for writer "
                        << static_cast< int >( writerPidAsDouble )
                        << ":" << writerThreadId;
                        
                    if ( iExpectedNext->second.min == iExpectedNext->second.max ) {
                        oss << " does not match expected index of 0x"
                            << setbase( 16 ) << iExpectedNext->second.min;
                    } else {
                        oss << " is outside expected index range of 0x"
                            << setbase( 16 ) << iExpectedNext->second.min
                            << "-0x" << iExpectedNext->second.max;
                    }
                    
                    oss << " [";
                        
                    for ( int i = 0; i < 8; ++i ) {
                        const unsigned char j = data[ i ];
                        
                        if ( i != 0 )
                            oss << ' ';
                            
                        oss << static_cast< unsigned short >( j / 16 )
                            << static_cast< unsigned short >( j % 16 );
                    }
                    
                    oss << "]";
                        
                    programLogErrorIfPossible( oss.str() );
                }
                
                iExpectedNext->second.min = index + 1;
                iExpectedNext->second.max = iExpectedNext->second.min;
            }
        }
    } catch ( ... ) {
        ostringstream oss;

        oss << totalReads << " reads";
        
        if ( (totalLossyReads != 0) || (totalLostElements != 0) ) {
            oss << " with " << totalLossyReads << " reads losing elements ("
                << totalLostElements << " total elements lost)";
        }
        
        programLogWarnIfPossible( oss.str() );

        throw;
    }

    programLogInfoIfPossible( "Done" );
} catch ( ... ) {
    if ( CaughtExceptionIsThreadQuitRequestedError() )
        programLogInfoIfPossible( "readerEp quitting as requested" );
    else {
        gCorrectnessErrorDetected = true;

        programLogErrorIfPossible( "Coming out of readerEp on an exception: " +
                                   getStringForCaught() );
    }

    throw;
}


void
runIt( const size_t writers,
       const size_t readers,
       const size_t duration,
       const size_t writeUsec,
       const bool   logLostElements )
{
    const ::pid_t pid = ::getpid();

    OneShotBarrier writerIpqsReadyBarrier( writers + readers );
    OneShotBarrier writersAndReadersReadyBarrier( writers + readers + 1 );
    
    AutoPthreadQuitAndJoinGroup autoQuitAndJoinGroup;
    
    for ( size_t i = 0; i < writers; ++i ) {
        string initNdc;
        {
            ostringstream oss;
            
            oss << "Writer " << pid << ":" << (i + 1);
            
            initNdc = oss.str();
        }
        
        const WriterArgs args( writerIpqsReadyBarrier,
                               writersAndReadersReadyBarrier,
                               writeUsec,
                               pid,
                               (i + 1) );
        
        const pthread_t t = StartPthreadWithCopy( writerEp, args, initNdc );

        autoQuitAndJoinGroup.insert( t );
    }

    for ( size_t i = 0; i < readers; ++i ) {
        string initNdc;
        {
            ostringstream oss;
            
            oss << "Reader " << pid;
            
            if ( readers > 1 )
                oss << ":" << (i + 1);
            
            initNdc = oss.str();
        }
        
        const ReaderArgs args( writerIpqsReadyBarrier,
                               writersAndReadersReadyBarrier,
                               (writers > 0),
                               logLostElements );
        
        const pthread_t t = StartPthreadWithCopy( readerEp, args, initNdc );

        autoQuitAndJoinGroup.insert( t );
    }
    
    writersAndReadersReadyBarrier.wait();
    
    sleep( duration );
}


}  // namespace < anonymous >


int
Program::main( )
try {
    const int writers = getIntParameter( "writers" );
    const int readers = getIntParameter( "readers" );
    const int duration = getIntParameter( "duration" );
    const int writeUsec = getIntParameter( "writeUsec" );
    const bool logLostElements = getBoolParameter( "logLost" );
    
    if ( writers < 0 )
        throw CARMA_ERROR( "Negative number of writers" );
        
    if ( readers < 0 )
        throw CARMA_ERROR( "Negative number of readers" );
        
    if ( (writers + readers) == 0 )
        throw CARMA_ERROR( "No readers or writers" );

    if ( duration < 1 )
        throw CARMA_ERROR( "Non-positive duration" );

    if ( writeUsec < 0 )
        throw CARMA_ERROR( "Negative writeUsec" );
        
    {
        ostringstream oss;
        
        oss << "sizeof(canbus::Message)=" << sizeof( canbus::Message );

        programLogInfoIfPossible( oss.str() );
    }
    
    {
        ostringstream oss;
        
        oss << "Running for " << duration << " seconds with ";
        
        if ( readers > 0 ) {
            if ( readers == 1 )
                oss << "one reader thread";
            else
                oss << readers << " reader threads";

            if ( writers > 0 )
                oss << " and ";
        }
        
        if ( writers > 0 ) {
            if ( writers == 1 )
                oss << "one writer thread ";
            else
                oss << writers << " writer threads ";
                
            if ( writeUsec == 0 )
                oss << "trying to write as fast as possible";
            else {
                oss << "trying to write every "
                    << writeUsec << " microseconds (~"
                    << ((1000UL * 1000UL) / writeUsec) << "/sec)";
            }
        }

        oss << "...";
        
        programLogInfoIfPossible( oss.str() );
        cout << oss.str() << endl;
    }
    
    runIt( writers, readers, duration, writeUsec, logLostElements );

    {
        const unsigned long long writes = gWrites;
        const unsigned long long reads = gReads;

        if ( writers > 0 ) {
            ostringstream oss;
            
            const unsigned long long writesPerSec = (writes / duration);
    
            const unsigned long long writesPerSecPerThread =
                (writes / (duration * writers));
    
            oss << "Actual writes: " << writes << " (~"
                << writesPerSec << "/sec or ~"
                << writesPerSecPerThread << "/sec per writer)";
    
            programLogInfoIfPossible( oss.str() );
            cout << oss.str() << "." << endl;
        }

        if ( readers > 0 ) {
            ostringstream oss;
            
            const unsigned long long readsPerSec = (reads / duration);
    
            const unsigned long long readsPerSecPerThread =
                (reads / (duration * readers));
    
            oss << "Actual reads:  " << reads << " (~"
                << readsPerSec << "/sec or ~"
                << readsPerSecPerThread << "/sec per reader)";
    
            programLogInfoIfPossible( oss.str() );
            cout << oss.str() << "." << endl;
        }
    }
    
    if ( gCorrectnessErrorDetected ) {
        const string msg = "A correctness error was detected";
        
        programLogErrorIfPossible( msg );
        cerr << "ERROR - " << msg << "." << endl;
        
        return EXIT_FAILURE;
    } else {
        const string msg = "No correctness errors were detected";
        
        programLogInfoIfPossible( msg );
        cout << msg << "." << endl;

        return EXIT_SUCCESS;
    }
} catch ( ... ) {
    const string msg =
        "Exiting main on an exception - " + getStringForCaught();
    
    programLogErrorIfPossible( msg );
    cerr << "ERROR - " << msg << "." << endl;
    
    throw;
}
