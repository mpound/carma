#ifndef CARMA_DBMS_MONITORSYSTEMANDDBMSRELATIONSHIPS_H
#define CARMA_DBMS_MONITORSYSTEMANDDBMSRELATIONSHIPS_H

/**
 * @file
 * relationships between the monitor and dbms systems
 *
 * @author: Dave Mehringer
 * @version $Id: MonitorSystemAndDBMSRelationships.h,v 1.6 2011/12/21 22:56:43 mpound Exp $
 * $CarmaCopyright$
 *
 */

#include <string>

namespace carma {
namespace dbms {

    /**
     * describes to what class this monitor point belongs
     */
    typedef enum {
        MPTYPE_SENSE, 
        MPTYPE_SOFT,
        MPTYPE_CONTROL,
        MAX_MPTYPE // add new data types immediately before this entry
    } MonitorPointType;
    
    /**
     * type of the monitor point.  The first nine correspond to the types 
     * defined in carma::monitor::MonitorValueType.  The others are additional
     * types defined in MPML files and 
     * carma/monitor/monitorPointSpecializations.h
     */
    typedef enum  {
        DATATYPE_BYTE,
        DATATYPE_SHORT,
        DATATYPE_INTEGER,
        DATATYPE_BOOLEAN,
        DATATYPE_FLOAT,
        DATATYPE_DOUBLE,
        DATATYPE_COMPLEX,
        DATATYPE_STRING,
        DATATYPE_SERIAL_NUMBER,
        DATATYPE_CHAR,
        DATATYPE_ENUMERATION,
        DATATYPE_ABSTIME,
        MAX_DATATYPE // add new data types immediately before this entry
    } MonitorPointDataType;

    /**
     * aggregate data type of the monitor[point,data file, table]
     */
    typedef enum {
        NUMERIC_TYPE, 
        STRING_TYPE,
        SHORT_TYPE, 
        COMPLEX_TYPE,
        MAX_AGGREGATE_DATA_TYPE
    } 
    MonitorAggregateType;

    /**
     * average type of the monitor[point,data file,table]
     */
    typedef enum {
        FRAME_AVG, 
        MINUTE_AVG, 
        WBCORREL_AVG, 
        SLCORREL_AVG,
    } MonitorAverageType;


    /**
     * type describing a particular set of directories used in writing,
     * loading, and archiving monitor point data
     * WRITE_AREA    = where the monitor data are written
     * LOAD_AREA     = from where the monitor data are loaded into the db
     * TRANSFER_AREA = from where the monitor data are transferred to the long
     * term archive
     */
    typedef enum  {
        MP_WRITE_AREA, 
        MP_LOAD_AREA, 
        MP_TRANSFER_AREA,
	MP_SDP_AREA
    } 
    MonitorDataAreaType;


    /**
     * MonitorPointDataType -> string for error messages, etc
     * @return string representation of the data type
     */
    std::string toString(const MonitorPointDataType& dataType);
    
    /**
     * MonitorPointType -> string for error messages, etc
     * @return string representation of the monitor point type
     */
    std::string toString(const MonitorPointType& mpType);
    
    /**
     * MonitorAggregateDataType -> string for error messages, etc
     * @return string representation of the aggregate data type
     */
    std::string toString(const MonitorAggregateType& dataType);
    
    /**
     * MonitorDataAreaType -> string for error messages, etc
     * @return string representation of the data area type
     */
    std::string toString(const MonitorDataAreaType& areaType);

    std::string toString(const MonitorAverageType& avgType);

    /**
     * convert a MonitorPointDataType value to the value which is 
     * written in the db
     */
    unsigned short mpDataType2DB(const MonitorPointDataType& mpDataType);
    
    /**
     * convert a db value to its MonitorPointDataType counterpart
     */
    MonitorPointDataType db2mpDataType(const unsigned short& dbDataType);

    /**
     * convert a monitor point type to the value which is written in the db
     */
    unsigned short mpType2DB(const MonitorPointType& mpType);
    
    /**
     * convert a db value to its MonitorPointType counterpart
     */
    MonitorPointType db2mpType(const unsigned short& dbType);

    /**
     * get the aggregate data type that the specific data corresponds to
     */
    MonitorAggregateType dataType2AggregateType
        (const MonitorPointDataType& dataType);

    /**
     * convert an aggregate type to the value which is written in the db
     */
    unsigned short aggregateTypeToDB(const MonitorAggregateType& aggType);
    
    /**
     * convert a db value to its MonitorAggregateType counterpart
     */
    MonitorAggregateType dbToAggregateType(const unsigned short& dbType);

    /**
     * convert an average type to the value which is written in the db
     */
    unsigned short averageTypeToDB(const MonitorAverageType& avgType);
    
    /**
     * convert a db value to its MonitorAverageType counterpart
     */
    MonitorAverageType dbToAverageType(const unsigned short& dbType);

}}


#endif //CARMA_DBMS_MONITORSYSTEMANDDBMSRELATIONSHIPS_H
