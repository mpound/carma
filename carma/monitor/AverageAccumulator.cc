/*
 * AverageAccumulator.cc - Method definitions for accumulating monitor point
 * values of various types.
 *
 * $Id: AverageAccumulator.cc,v 1.15 2006/11/26 23:55:22 tcosta Exp $
 *
 * $CarmaCopyright$
 */

/*!
 * @file AverageAccumulator.h
 * This is the class file for accumulators of various types for
 * computing averages.
 *
 * @author N. S. Amarnath
 */

#include "carma/dbms/dbFFIO.h"
#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/AverageAccumulator.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/programLogging.h"

using namespace carma;
using namespace carma::dbms;
using namespace carma::monitor;
using namespace carma::util;


AverageAccumulator::AverageAccumulator(
    MonitorSystem &                      monitorSystem, 
    const MonitorPoint::ARCHIVE_PRIORITY priority ) :
monitorSystem_(monitorSystem),
aveCharAccumulator_(*new CharAccumulator (monitorSystem,priority)),
aveByteAccumulator_(*new ByteAccumulator (monitorSystem,priority)),
aveShortAccumulator_(*new ShortAccumulator (monitorSystem,priority)),
aveIntAccumulator_(*new IntAccumulator (monitorSystem,priority)),
aveBoolAccumulator_(*new BoolAccumulator (monitorSystem,priority)),
aveEnumAccumulator_(*new EnumAccumulator (monitorSystem,priority)),
aveFloatAccumulator_(*new FloatAccumulator (monitorSystem,priority)),
aveDoubleAccumulator_(*new DoubleAccumulator (monitorSystem,priority)),
aveComplexAccumulator_(*new ComplexAccumulator (monitorSystem,priority)),
aveAbstimeAccumulator_(*new AbstimeAccumulator (monitorSystem,priority)),
aveStringAccumulator_(*new StringAccumulator (monitorSystem,priority)),
aveSerialNoAccumulator_(*new SerialNoAccumulator (monitorSystem,priority))
{
    resetAccumulator();
}


AverageAccumulator::~AverageAccumulator( )
try { 
    delete &aveCharAccumulator_;
    delete &aveByteAccumulator_;
    delete &aveShortAccumulator_;
    delete &aveIntAccumulator_;
    delete &aveBoolAccumulator_;
    delete &aveEnumAccumulator_;
    delete &aveFloatAccumulator_;
    delete &aveDoubleAccumulator_;
    delete &aveComplexAccumulator_;
    delete &aveAbstimeAccumulator_;
    delete &aveStringAccumulator_;
    delete &aveSerialNoAccumulator_;
} catch ( ... ) {
    programLogErrorIfPossible(
        "Stifling expception in AverageAccumulator::~AverageAccumulator - " +
        getStringForCaught() );
}


/**
 * Method to accumulate values from the monitor stream.
 * Ensure that monitorSystem_.read() has been called before calling 
 * this method.
 */
void
AverageAccumulator::accumulate( ) 
{
    if ( frameCount_ == monitorSystem_.getFrameCount() )
        return;

    aveCharAccumulator_.accumulate();
    aveByteAccumulator_.accumulate();
    aveShortAccumulator_.accumulate();
    aveIntAccumulator_.accumulate();
    aveBoolAccumulator_.accumulate();
    aveEnumAccumulator_.accumulate();
    aveFloatAccumulator_.accumulate();
    aveDoubleAccumulator_.accumulate();
    aveComplexAccumulator_.accumulate();
    aveAbstimeAccumulator_.accumulate();
    aveStringAccumulator_.accumulate();
    aveSerialNoAccumulator_.accumulate();
    frameCount_ = monitorSystem_.getFrameCount();
}


void
AverageAccumulator::resetAccumulator( )
{
    aveCharAccumulator_.resetAccumulator();
    aveByteAccumulator_.resetAccumulator();
    aveShortAccumulator_.resetAccumulator();
    aveIntAccumulator_.resetAccumulator();
    aveBoolAccumulator_.resetAccumulator();
    aveEnumAccumulator_.resetAccumulator();
    aveFloatAccumulator_.resetAccumulator();
    aveDoubleAccumulator_.resetAccumulator();
    aveComplexAccumulator_.resetAccumulator();
    aveAbstimeAccumulator_.resetAccumulator();
    aveStringAccumulator_.resetAccumulator();
    aveSerialNoAccumulator_.resetAccumulator();
    frameCount_ = 0;
}


void
AverageAccumulator::writeInstAveragesToFile(
    const long   frameCount,
    FILE * const shortFile,
    FILE * const numericFile, 
    FILE * const stringFile,
    FILE * const complexFile ) const
{
    dumpInstAveragesToDBFile (MONITOR_DB_TYPE_NUMERIC, frameCount, 
                              numericFile);
    dumpInstAveragesToDBFile (MONITOR_DB_TYPE_SHORT, frameCount, 
                              shortFile);
    dumpInstAveragesToDBFile (MONITOR_DB_TYPE_STRING, frameCount, 
                              stringFile);
    dumpInstAveragesToDBFile (MONITOR_DB_TYPE_COMPLEX, frameCount, 
                              complexFile);
}    


void
AverageAccumulator::writeInstAveragesToFile(
    const long frameCount,
    dbFFIO &   shortFile,
    dbFFIO &   numericFile, 
    dbFFIO &   stringFile,
    dbFFIO &   complexFile ) const
{
    dumpInstAveragesToDBFile (MONITOR_DB_TYPE_NUMERIC, frameCount, 
                              numericFile);
    dumpInstAveragesToDBFile (MONITOR_DB_TYPE_SHORT, frameCount, 
                              shortFile);
    dumpInstAveragesToDBFile (MONITOR_DB_TYPE_STRING, frameCount, 
                              stringFile);
    dumpInstAveragesToDBFile (MONITOR_DB_TYPE_COMPLEX, frameCount, 
                              complexFile);
}    


void
AverageAccumulator::writeLongAveragesToFile(
    const long   frameCount,
    FILE * const shortFile,
    FILE * const numericFile, 
    FILE * const stringFile,
    FILE * const complexFile ) const
{
    char frameCountText[32];
    
    snprintf( frameCountText,
              sizeof( frameCountText ),
              "%11ld",
              frameCount );
    
    dumpLongAveragesToDBFile( MONITOR_DB_TYPE_NUMERIC,
                              frameCountText,
                              numericFile);

    dumpLongAveragesToDBFile( MONITOR_DB_TYPE_SHORT,
                              frameCountText,
                              shortFile);

    dumpLongAveragesToDBFile( MONITOR_DB_TYPE_STRING,
                              frameCountText,
                              stringFile);

    dumpLongAveragesToDBFile( MONITOR_DB_TYPE_COMPLEX,
                              frameCountText,
                              complexFile);
}    


void
AverageAccumulator::dumpInstAveragesToDBFile(
    const DBwriteType dbWriteType,
    const long        frameCount,
    FILE * const      file ) const 
{
    switch ( dbWriteType )  {
        case MONITOR_DB_TYPE_STRING:
            aveStringAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            return;
        
        case MONITOR_DB_TYPE_COMPLEX:
            aveComplexAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            return;
        
        case MONITOR_DB_TYPE_SHORT:
            aveShortAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveByteAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveCharAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveBoolAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            return;
        
        case MONITOR_DB_TYPE_NUMERIC:
            aveIntAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveEnumAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveFloatAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveDoubleAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveAbstimeAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveSerialNoAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            return;
            
        case MONITOR_DB_TYPE_NUM_TYPES:
            break;
    }

    programLogErrorIfPossible(
        "Bad dbWriteType in AverageAccumulator::dumpInstAveragesToDBFile" );
}


void
AverageAccumulator::dumpInstAveragesToDBFile(
    const DBwriteType dbWriteType,
    const long        frameCount,
    dbFFIO &          file ) const
{
    switch ( dbWriteType )  {
        case MONITOR_DB_TYPE_STRING:
            aveStringAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            return;
            
        case MONITOR_DB_TYPE_COMPLEX:
            aveComplexAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            return;
            
        case MONITOR_DB_TYPE_SHORT:
            aveShortAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveByteAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveCharAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveBoolAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            return;
            
        case MONITOR_DB_TYPE_NUMERIC:
            aveIntAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveEnumAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveFloatAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveDoubleAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveAbstimeAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            aveSerialNoAccumulator_.dumpInstAveragesToDBFile(frameCount,file);
            return;

        case MONITOR_DB_TYPE_NUM_TYPES:
            break;
    }

    programLogErrorIfPossible(
        "Bad dbWriteType in AverageAccumulator::dumpInstAveragesToDBFile" );
}
    

void
AverageAccumulator::dumpLongAveragesToDBFile(
    const DBwriteType  dbWriteType,
    const char * const frameCountText,
    FILE * const       f ) const
{
    switch ( dbWriteType ) {
        case MONITOR_DB_TYPE_STRING:
            aveStringAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            return;
            
        case MONITOR_DB_TYPE_COMPLEX:
            aveComplexAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            return;
            
        case MONITOR_DB_TYPE_SHORT:
            aveShortAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            aveByteAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            aveCharAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            aveBoolAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            return;
        
        case MONITOR_DB_TYPE_NUMERIC:
            aveIntAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            aveEnumAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            aveFloatAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            aveDoubleAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            aveAbstimeAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            aveSerialNoAccumulator_.dumpLongAveragesToDBFile(frameCountText,f);
            return;

        case MONITOR_DB_TYPE_NUM_TYPES:
            break;
    }

    programLogErrorIfPossible(
        "Bad dbWriteType in AverageAccumulator::dumpLongAveragesToDBFile" );
}
