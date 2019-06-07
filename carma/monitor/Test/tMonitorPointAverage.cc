//
// @version $Revision: 1.2 $
//
// @usage use it
//
// @description
//  Test program for testing the MonitorPointAverage classes.
//
// @noKeys
//
// @logger TEST_FACILITY carma.test.monitor.tMonitorPointAverage
//

#include <iostream>
#include <sstream>

#include "carma/dbms/MonitorData2DBMSConversions.h"
#include "carma/monitor/MonitorPointAverageT.h"
#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/ScopedLogNdc.h"

using namespace ::std;
using namespace carma;
using namespace carma::dbms;
using namespace carma::monitor;
using namespace carma::util;


namespace {

const size_t kBufferSize = 16;
const size_t kExpectedResultLength = 6;
const int kSpeedReps = 1000 * 1000;


void
oldWriteAvePropsToBufSpeedTest( MonitorPointAverageBase & mpab )
{
    ScopedLogNdc ndc( "oldWriteAvePropsToBufSpeedTest" );
    
    programLogInfo( "Start..." );
    
    char buffer[kBufferSize];
    
    unsigned long long total = 0;
    
    for ( int k = 0; k < kSpeedReps; ++k ) {
        for ( int bf = 0; bf < MonitorPoint::MAX_BLANKING_FLAGGING; ++bf ) {
            const MonitorPoint::BLANKING_FLAGGING mpBlankingValue =
                static_cast< MonitorPoint::BLANKING_FLAGGING >( bf );
                
            mpab.setBlanking( mpBlankingValue );
    
            for ( int val = 0; val < MonitorPoint::MAX_VALIDITY; ++val ) {
                const MonitorPoint::VALIDITY mpValidityValue =
                    static_cast< MonitorPoint::VALIDITY >( val );
    
                mpab.setValidity( mpValidityValue );
                
                ushort dbBlankingValue;
                try {
                    dbBlankingValue = blankingFlagging2DB( mpBlankingValue );
                } catch ( ... ) {
                    dbBlankingValue =
                        blankingFlagging2DB( MonitorPoint::UNDETERMINED );
                }
    
                ushort dbValidityValue;
                try {
                    dbValidityValue = validity2DB( mpValidityValue );
                } catch ( ... ) {
                    dbValidityValue =
                        validity2DB( MonitorPoint::INVALID_NO_DATA );
                }
    
                sprintf( buffer,
                         "%2d\t%2d\t",
                         static_cast< int >( dbBlankingValue ),
                         static_cast< int >( dbValidityValue ) );
                          
                ++total;
            }
        }
    }

    ostringstream oss;

    oss << "Done with " << total;

    programLogInfo( oss.str() );
}


void
writeAvePropsToBufSpeedTest( MonitorPointAverageBase & mpab )
{
    ScopedLogNdc ndc( "writeAvePropsToBufSpeedTest" );
    
    programLogInfo( "Start..." );

    char buffer[kBufferSize];
    
    unsigned long long total = 0;
    
    for ( int k = 0; k < kSpeedReps; ++k ) {
        for ( int bf = 0; bf < MonitorPoint::MAX_BLANKING_FLAGGING; ++bf ) {
            const MonitorPoint::BLANKING_FLAGGING mpBlankingValue =
                static_cast< MonitorPoint::BLANKING_FLAGGING >( bf );
                
            mpab.setBlanking( mpBlankingValue );
    
            for ( int val = 0; val < MonitorPoint::MAX_VALIDITY; ++val ) {
                const MonitorPoint::VALIDITY mpValidityValue =
                    static_cast< MonitorPoint::VALIDITY >( val );
    
                mpab.setValidity( mpValidityValue );
                
                mpab.writeAvePropsToBuf( buffer );

                ++total;
            }
        }
    }

    ostringstream oss;

    oss << "Done with " << total;

    programLogInfo( oss.str() );
}


void
writeAvePropsToBufTest( MonitorPointAverageBase & mpa )
{
    ScopedLogNdc ndc( "writeAvePropsToBufTest" );
    
    const int bfFirst = -MonitorPoint::MAX_BLANKING_FLAGGING;
    const int bfLast = 2 * MonitorPoint::MAX_BLANKING_FLAGGING;
    
    const int valFirst = -MonitorPoint::MAX_VALIDITY;
    const int valLast = 2 * MonitorPoint::MAX_VALIDITY;
    
    char buffer1[kBufferSize];
    char buffer2[kBufferSize];
    
    for ( int bf = bfFirst; bf <= bfLast; ++bf ) {
        const MonitorPoint::BLANKING_FLAGGING mpBlankingValue =
            static_cast< MonitorPoint::BLANKING_FLAGGING >( bf );
            
        mpa.setBlanking( mpBlankingValue );

        for ( int val = valFirst; val <= valLast; ++val ) {
            const MonitorPoint::VALIDITY mpValidityValue =
                static_cast< MonitorPoint::VALIDITY >( val );

            mpa.setValidity( mpValidityValue );
            
            if ( mpa.getBlanking() != bf )
                throw CARMA_ERROR( "bad bf" );

            if ( mpa.getValidity() != val )
                throw CARMA_ERROR( "bad val" );
                
            mpa.writeAvePropsToBuf( buffer1 );
            
            if ( buffer1[kExpectedResultLength] != '\0' )
                throw CARMA_ERROR( "wrong buffer1 length" );

            ushort dbBlankingValue;
            try {
                dbBlankingValue = blankingFlagging2DB( mpBlankingValue );
            } catch ( ... ) {
                dbBlankingValue =
                    blankingFlagging2DB( MonitorPoint::UNDETERMINED );
            }

            if ( mpa.getDbBlanking() != dbBlankingValue )
                throw CARMA_ERROR( "wrong getDbBlanking() value" );

            ushort dbValidityValue;
            try {
                dbValidityValue = validity2DB( mpValidityValue );
            } catch ( ... ) {
                dbValidityValue =
                    validity2DB( MonitorPoint::INVALID_NO_DATA );
            }

            if ( mpa.getDbValidity() != dbValidityValue )
                throw CARMA_ERROR( "wrong getDbValidity() value" );

            snprintf( buffer2,
                      sizeof( buffer2 ),
                      "%2d\t%2d\t",
                      static_cast< int >( dbBlankingValue ),
                      static_cast< int >( dbValidityValue ) );
                     
            if ( buffer2[kExpectedResultLength] != '\0' )
                throw CARMA_ERROR( "wrong buffer2 length" );
            
            if ( strcmp( buffer1, buffer2 ) != 0 )
                throw CARMA_ERROR( "wrong result" );
        }
    }

    ostringstream oss;
    
    oss << "bf=[" << bfFirst << ", " << bfLast << "], "
        << "val=[" << valFirst << ", " << valLast << "] gave correct answers";

    programLogInfo( oss.str() );
}


void
test( )
{
    MonitorPointAverageT< string > mpaString;

    MonitorPointAverageT< bool > mpaBool;

    MonitorPointAverageT< char > mpaChar;
    MonitorPointAverageT< short > mpaShort;
    MonitorPointAverageT< int > mpaInt;
    MonitorPointAverageT< long > mpaLong;
    MonitorPointAverageT< long long > mpaLongLong;

    MonitorPointAverageT< unsigned char > mpaUnsignedChar;
    MonitorPointAverageT< unsigned short > mpaUnsignedShort;
    MonitorPointAverageT< unsigned int > mpaUnsignedInt;
    MonitorPointAverageT< unsigned long > mpaUnsignedLong;
    MonitorPointAverageT< unsigned long long > mpaUnsignedLongLong;

    MonitorPointAverageT< float > mpaFloat;
    MonitorPointAverageT< double > mpaDouble;
    MonitorPointAverageT< long double > mpaLongDouble;
    
    MonitorPointAverageT< complex< float > > mpaComplexFloat;
    MonitorPointAverageT< complex< double > > mpaComplexDouble;
    MonitorPointAverageT< complex< long double > > mpaComplexLongDouble;
    
    writeAvePropsToBufTest( mpaString );

    writeAvePropsToBufTest( mpaBool );

    writeAvePropsToBufTest( mpaChar );
    writeAvePropsToBufTest( mpaShort );
    writeAvePropsToBufTest( mpaInt );
    writeAvePropsToBufTest( mpaLong );
    writeAvePropsToBufTest( mpaLongLong );

    writeAvePropsToBufTest( mpaUnsignedChar );
    writeAvePropsToBufTest( mpaUnsignedShort );
    writeAvePropsToBufTest( mpaUnsignedInt );
    writeAvePropsToBufTest( mpaUnsignedLong );
    writeAvePropsToBufTest( mpaUnsignedLongLong );

    writeAvePropsToBufTest( mpaFloat );
    writeAvePropsToBufTest( mpaDouble );
    writeAvePropsToBufTest( mpaLongDouble );

    writeAvePropsToBufTest( mpaComplexFloat );
    writeAvePropsToBufTest( mpaComplexDouble );
    writeAvePropsToBufTest( mpaComplexLongDouble );

    // writeAvePropsToBufSpeedTest( mpaDouble );
    // oldWriteAvePropsToBufSpeedTest( mpaDouble );
}


}  // namespace < anonymous >


int
Program::main( )
try {
    programLogInfoIfPossible( "Starting tests..." );

    test();

    programLogInfoIfPossible( "All tests done" );

    return 0;
} catch ( ... ) {
    cerr << "Exiting on an exception - " + getStringForCaught() << endl;
    
    throw;
}
