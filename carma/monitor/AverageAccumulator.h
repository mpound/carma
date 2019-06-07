/*
 * AverageAccumulator.h - Class definition for accumulating monitor point
 * values of various types.
 */

#ifndef        CARMA_MONITOR_AVERAGE_ACCUMULATOR_H
#define        CARMA_MONITOR_AVERAGE_ACCUMULATOR_H

/**
 * $Id: AverageAccumulator.h,v 1.17 2007/08/03 21:47:27 tcosta Exp $
 */

/*!
 * @file AverageAccumulator.h
 * This is the class file for accumulators of various types for
 * computing averages.
 *
 * @author N. S. Amarnath
 *
 */

#include <map>

#include "carma/monitor/MonitorSystem.h"
#include "carma/monitor/TypedAverageAccumulatorT.h"
#include "carma/dbms/dbFFIO.h"


namespace carma {
namespace monitor {


typedef TypedAverageAccumulatorT< MonitorPointChar, double > CharAccumulator;
typedef TypedAverageAccumulatorT< MonitorPointByte, double > ByteAccumulator;
typedef TypedAverageAccumulatorT< MonitorPointShort, double > ShortAccumulator;
typedef TypedAverageAccumulatorT< MonitorPointInt, double > IntAccumulator;
typedef TypedAverageAccumulatorT< MonitorPointBool, long > BoolAccumulator;
typedef TypedAverageAccumulatorT< MonitorPointEnum, int > EnumAccumulator;
typedef TypedAverageAccumulatorT< MonitorPointFloat, double > FloatAccumulator;
typedef TypedAverageAccumulatorT< MonitorPointDouble, double > DoubleAccumulator;
typedef TypedAverageAccumulatorT< MonitorPointComplex, ::std::complex< float > > ComplexAccumulator;
typedef TypedAverageAccumulatorT< MonitorPointAbstime, double > AbstimeAccumulator;
typedef TypedAverageAccumulatorT< MonitorPointString, ::std::string > StringAccumulator;
typedef TypedAverageAccumulatorT< MonitorPointSerialNo, long > SerialNoAccumulator;


typedef enum {
    MONITOR_DB_TYPE_NUMERIC,
    MONITOR_DB_TYPE_SHORT,
    MONITOR_DB_TYPE_STRING,
    MONITOR_DB_TYPE_COMPLEX,
    // Always last
    MONITOR_DB_TYPE_NUM_TYPES
} DBwriteType;


/**
 * @class
 * @brief Manages set of average accumulators bound to a monitor system 
 * instance.
 *
 * Manages a set of average accumulators bound to a MonitorSystem object.
 * The set consists of an average accumulator associated with each monitor 
 * point in the MonitorSystem object. The average accumulator is defined 
 * using a template so it can be instantiated based on the type of the
 * monitor point. The average accumulator object (@see MonitorPointAverageT.h)
 * is associated with a monitor point using the MonitorPointAccumulatorT
 * template (@see MonitorPointAccumulatorT.h). Accumulators (the term is used
 * interchangeably for either a MonitorPointAverage or MonitorPointAccumulator
 * object) of a specified type (say int or string) are managed by a 
 * TypedAverageAccumulator object, instantiated using a 
 * TypedAverageAccumulatorT template. The AverageAccumulator object
 * instantiates a TypedAverageAccumulator object for each type of monitor point
 * in the MonitorSystem class. The basic methods every accumulator has
 * are:
 *
 * resetAccumulator() - resets accumulators for a fresh average calculation
 * accumulate() - accumulates sample values, and associated properties (such as
 *                validity, # of samples)
 * dump/write<something> - write methods for writing averages to ostreams, 
 *                         or as strings.
 *
 * @see MonitorPointAverageT, MonitorPointAccumulatorT, 
 *      TypedAverageAccumulatorT
 */
class AverageAccumulator {
  private:
    // frameCount when last accumulate() was performed.
    long              frameCount_;
    MonitorSystem&    monitorSystem_;

    CharAccumulator&  aveCharAccumulator_;
    ByteAccumulator&  aveByteAccumulator_;
    ShortAccumulator& aveShortAccumulator_;
    IntAccumulator&   aveIntAccumulator_;
    BoolAccumulator&  aveBoolAccumulator_;
    EnumAccumulator&  aveEnumAccumulator_;
    FloatAccumulator& aveFloatAccumulator_;
    DoubleAccumulator& aveDoubleAccumulator_;
    ComplexAccumulator& aveComplexAccumulator_;
    AbstimeAccumulator& aveAbstimeAccumulator_;
    StringAccumulator& aveStringAccumulator_;
    SerialNoAccumulator& aveSerialNoAccumulator_;

  public:

    /**
     * Constructor
     * 
     * @param monitorSystem MonitorSystem& monitor system to which the
     *        accumulators are bound.
     * @param priority accumulate points only of this archive priority or 
     *        higher
     */
    AverageAccumulator (MonitorSystem& monitorSystem, 
                        const MonitorPoint::ARCHIVE_PRIORITY priority) ;

    /**
     * Destructor.
     *
     * Deletes typed accumulators created in the destructor.
     */
    ~AverageAccumulator () ;

    /**
     * Method to accumulate values from the monitor stream.
     * Ensure that monitorSystem_.read() has been called before calling 
     * this method.
     */
    void accumulate () ;

    /**
     * Resets accumulators for fresh calculation of averages.
     */
    void resetAccumulator () ;

    void writeLongAveragesToStrings( long          timestamp,
                                     std::string & shortString, 
                                     std::string & numericString,
                                     std::string & stringString, 
                                     std::string & complexString,
                                     char *        scratchString ) const;

    void writeInstAveragesToFile( long   frameCount, 
                                  FILE * shortFile, 
                                  FILE * numericFile, 
                                  FILE * stringFile, 
                                  FILE * complexFile ) const;

    void writeInstAveragesToFile( long                  frameCount, 
                                  carma::dbms::dbFFIO & shortFile, 
                                  carma::dbms::dbFFIO & numericFile, 
                                  carma::dbms::dbFFIO & stringFile, 
                                  carma::dbms::dbFFIO & complexFile ) const;

    void dumpInstAveragesToDBFile( DBwriteType dbWriteType, 
                                   long        frameCount,
                                   FILE *      file ) const;
                                   
    void dumpInstAveragesToDBFile( DBwriteType           dbWriteType,
                                   long                  frameCount,
                                   carma::dbms::dbFFIO & file ) const;
    
    void writeLongAveragesToFile( long   frameCount,
                                  FILE * shortFile,
                                  FILE * numericFile, 
                                  FILE * stringFile,
                                  FILE * complexFile ) const;

private:
    void dumpLongAveragesToDBFile( DBwriteType  dbWriteType,
                                   const char * frameCountText,
                                   FILE *       file ) const;



}; // class AverageAccumulator


} // end namespace carma::monitor
} // end namespace carma


#endif
