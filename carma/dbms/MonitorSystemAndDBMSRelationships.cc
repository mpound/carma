/**
 * @file
 * implementation of MonitorSystemAndDBMSRelationships
 *
 * @author: Dave Mehringer
 * @version $Id: MonitorSystemAndDBMSRelationships.cc,v 1.6 2011/12/21 22:56:43 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */
#include <sstream>
#include "carma/dbms/MonitorSystemAndDBMSRelationships.h"
#include "carma/util/ErrorException.h"

namespace carma {
namespace dbms {

    /*
     * WARNING: IF THESE CONSTANT VALUES ARE ALTERED, THE DATABASE WILL LOSE
     * INTEGRITY. 
     */
    // monitor point data types
    static const unsigned short DB_DATATYPE_BYTE          =  1010;
    static const unsigned short DB_DATATYPE_SHORT         =  1020;
    static const unsigned short DB_DATATYPE_INTEGER       =  1030;
    static const unsigned short DB_DATATYPE_BOOLEAN       =  1040;
    static const unsigned short DB_DATATYPE_FLOAT         =  1050;
    static const unsigned short DB_DATATYPE_DOUBLE        =  1060;
    static const unsigned short DB_DATATYPE_COMPLEX       =  1070;
    static const unsigned short DB_DATATYPE_STRING        =  1080;
    static const unsigned short DB_DATATYPE_SERIAL_NUMBER =  1090;
    static const unsigned short DB_DATATYPE_CHAR          =  1100;
    static const unsigned short DB_DATATYPE_ENUMERATION   =  1110;
    static const unsigned short DB_DATATYPE_ABSTIME       =  1120;

    // monitor point types
    static const unsigned short DB_MPTYPE_SENSE           = 3200; 
    static const unsigned short DB_MPTYPE_SOFT            = 3210;
    static const unsigned short DB_MPTYPE_CONTROL         = 3220;

    // aggregate types
    static const unsigned short DB_NUMERIC_TYPE           = 20; 
    static const unsigned short DB_STRING_TYPE            = 22;
    static const unsigned short DB_SHORT_TYPE             = 24; 
    static const unsigned short DB_COMPLEX_TYPE           = 26;

    // average types
    static const unsigned short DB_FRAME_AVG              = 119; 
    static const unsigned short DB_MINUTE_AVG             = 122; 
    static const unsigned short DB_WBCORREL_AVG           = 125; 
    static const unsigned short DB_SLCORREL_AVG           = 128;

    std::string toString(const MonitorPointDataType& dataType) {
        switch(dataType) {
        case DATATYPE_BYTE:
            return "byte";
        case DATATYPE_SHORT:
            return "short";
        case DATATYPE_INTEGER:
            return "integer";
        case DATATYPE_BOOLEAN:
            return "boolean";
        case DATATYPE_FLOAT:
            return "float";
        case DATATYPE_DOUBLE:
            return "double";
        case DATATYPE_COMPLEX:
            return "complex";
        case DATATYPE_STRING:
            return "string";
        case DATATYPE_SERIAL_NUMBER:
            return "serial number";
        case DATATYPE_CHAR:
            return "char";
        case DATATYPE_ENUMERATION:
            return "enumeration";
        case DATATYPE_ABSTIME:
            return "abstime";
        default:
            std::ostringstream emsg;
            emsg << "Unknown data type with value " << dataType << ". This "
                 << "probably indicates an enumorator has been added but this "
                 << "method has not been updated.";
            throw CARMA_ERROR(emsg.str());
        }
    }

    std::string toString(const MonitorPointType& mpType) {
        switch(mpType) {
        case MPTYPE_SENSE:
            return "SensePoint";
        case MPTYPE_SOFT:
            return "SoftPoint";
        case MPTYPE_CONTROL:
            return "ControlPoint";
        default:
            std::ostringstream emsg;
            emsg << "Unknown monitor point type with value " << mpType 
                 << ". This probably indicates an enumorator has been added "
                 << "but this method has not been updated.";
            throw CARMA_ERROR(emsg.str());
        }
    }
    
    // DBConnection::getMonitorDataTableBaseName() uses this method and
    // so the return values shouldn't be altered unless you understand
    // the ramifications
    std::string toString(const MonitorAggregateType& agType) {
        switch(agType) {
        case COMPLEX_TYPE:
            return "Complex";
        case NUMERIC_TYPE:
            return "Numeric";
        case SHORT_TYPE:
            return "Short";
        case STRING_TYPE:
            return "String";
        default:
            std::ostringstream emsg;
            emsg << "Unknown aggregate type with value " << agType << ". This "
                 << "probably indicates an enumorator has been added but this "
                 << "method has not been updated.";
            throw CARMA_ERROR(emsg.str());

        }
    }

    std::string toString(const MonitorDataAreaType& areaType) {
        switch(areaType) {
            case MP_WRITE_AREA:
                return "mp";
        case MP_LOAD_AREA:
            return "dbload";
        case MP_TRANSFER_AREA:
            return "transfer";
	case MP_SDP_AREA:
            return "sdp";
        default:
            std::ostringstream emsg;
            emsg << "Unknown data area type with value " << areaType << ". "
                 << "This probably indicates an enumorator has been added "
                 << "but this method has not been updated.";
            throw CARMA_ERROR(emsg.str());
        }
    }
            
    // DBConnection::getMonitorDataTableBaseName() uses this method and
    // so the return values shouldn't be altered unless you understand
    // the ramifications
    std::string toString(const MonitorAverageType& avgType) {
        switch(avgType) {
        case FRAME_AVG:
            return "Frame";
        case MINUTE_AVG:
            return "MinuteIntegrated";
        case WBCORREL_AVG:
            return "WBCorrelIntegrated";
        case SLCORREL_AVG:
            return "SLCorrelIntegrated";
        default:
            std::ostringstream emsg;
            emsg << "Unknown average type with value " << avgType << ". "
                 << "This probably indicates an enumorator has been added "
                 << "but this method has not been updated.";
            throw CARMA_ERROR(emsg.str());
        }
    }

    unsigned short mpDataType2DB(const MonitorPointDataType& mpDataType) {
        switch(mpDataType) {
        case DATATYPE_BYTE: {
            return DB_DATATYPE_BYTE;
            break;
        }
        case DATATYPE_SHORT: {
            return DB_DATATYPE_SHORT;
            break;
        }
        case DATATYPE_INTEGER: {
            return DB_DATATYPE_INTEGER;
            break;
        }
        case DATATYPE_BOOLEAN: {
            return DB_DATATYPE_BOOLEAN;
            break;
        }
        case DATATYPE_FLOAT: {
            return DB_DATATYPE_FLOAT;
            break;
        }
        case DATATYPE_DOUBLE: {
            return DB_DATATYPE_DOUBLE;
            break;
        }
        case DATATYPE_COMPLEX: {
            return DB_DATATYPE_COMPLEX;
            break;
        }
        case DATATYPE_STRING: {
            return DB_DATATYPE_STRING;
            break;
        }
        case DATATYPE_SERIAL_NUMBER: {
            return DB_DATATYPE_SERIAL_NUMBER;
            break;
        }
        case DATATYPE_CHAR: {
            return DB_DATATYPE_CHAR;
            break;
        }
        case DATATYPE_ENUMERATION: {
            return DB_DATATYPE_ENUMERATION;
            break;
        }
        case DATATYPE_ABSTIME: {
            return DB_DATATYPE_ABSTIME;
            break;
        }
        default: {
            std::ostringstream ss;
            ss << "Unhandled monitor point data type value " << mpDataType;
            throw CARMA_ERROR(ss.str());
        }
        }
    }

    MonitorPointDataType db2mpDataType(const unsigned short& dbDataType) {
        switch(dbDataType) {
        case DB_DATATYPE_BYTE: {
            return DATATYPE_BYTE;
            break;
        }
        case DB_DATATYPE_SHORT: {
            return DATATYPE_SHORT;
            break;
        }
        case DB_DATATYPE_INTEGER: {
            return DATATYPE_INTEGER;
            break;
        }
        case DB_DATATYPE_BOOLEAN: {
            return DATATYPE_BOOLEAN;
            break;
        }
        case DB_DATATYPE_FLOAT: {
            return DATATYPE_FLOAT;
            break;
        }
        case DB_DATATYPE_DOUBLE: {
            return DATATYPE_DOUBLE;
            break;
        }
        case DB_DATATYPE_COMPLEX: {
            return DATATYPE_COMPLEX;
            break;
        }
        case DB_DATATYPE_STRING: {
            return DATATYPE_STRING;
            break;
        }
        case DB_DATATYPE_SERIAL_NUMBER: {
            return DATATYPE_SERIAL_NUMBER;
            break;
        }
        case DB_DATATYPE_CHAR: {
            return DATATYPE_CHAR;
            break;
        }
        case DB_DATATYPE_ENUMERATION: {
            return DATATYPE_ENUMERATION;
            break;
        }
        case DB_DATATYPE_ABSTIME: {
            return DATATYPE_ABSTIME;
            break;
        }
        default: {
            std::ostringstream ss;
            ss << "Unhandled db monitor point data type value " << dbDataType;
            throw CARMA_ERROR(ss.str());
        }
        }
    }        

    unsigned short mpType2DB(const MonitorPointType& mpType) {
        switch(mpType) {
        case MPTYPE_SENSE: {
            return DB_MPTYPE_SENSE;
            break;
        }
        case MPTYPE_SOFT: {
            return DB_MPTYPE_SOFT;
            break;
        }
        case MPTYPE_CONTROL: {
            return DB_MPTYPE_CONTROL;
            break;
        }
        default: {
            std::ostringstream ss;
            ss << "Unhandled monitor point type value " << mpType;
            throw CARMA_ERROR(ss.str());
        }
        }
    }

    MonitorPointType db2mpType(const unsigned short& dbType) {
        switch(dbType) {
        case DB_MPTYPE_SENSE: {
            return MPTYPE_SENSE;
            break;
        }
        case DB_MPTYPE_SOFT: {
            return MPTYPE_SOFT;
            break;
        }
        case DB_MPTYPE_CONTROL: {
            return MPTYPE_CONTROL;
            break;
        }
        default: {
            std::ostringstream ss;
            ss << "Unhandled monitor point type value " << dbType;
            throw CARMA_ERROR(ss.str());
        }
        }
    }

    MonitorAggregateType dataType2AggregateType(const MonitorPointDataType& dataType) {
        if(dataType == DATATYPE_BYTE || dataType ==  DATATYPE_SHORT 
           || dataType == DATATYPE_BOOLEAN || dataType == DATATYPE_CHAR) {
            return SHORT_TYPE;
        } else if (dataType == DATATYPE_INTEGER || dataType == DATATYPE_FLOAT
                   || dataType == DATATYPE_DOUBLE 
                   || dataType == DATATYPE_SERIAL_NUMBER 
                   || dataType == DATATYPE_ENUMERATION 
                   || dataType == DATATYPE_ABSTIME) {
            return NUMERIC_TYPE;
        } else if (dataType == DATATYPE_COMPLEX) {
            return COMPLEX_TYPE;
        } else if (dataType == DATATYPE_STRING) {
            return STRING_TYPE;
        } else {
            std::ostringstream os;
            os << "Unhandled data type " << dataType 
               << ". carma::dbms::dataType2AggregateType() needs to be updated";
            throw CARMA_ERROR(os.str());
        }
    }

    unsigned short aggregateTypeToDB(const MonitorAggregateType& aggType) {
        switch(aggType) {
        case NUMERIC_TYPE:
            return DB_NUMERIC_TYPE;
        case STRING_TYPE:
            return DB_STRING_TYPE;
        case SHORT_TYPE:
            return DB_SHORT_TYPE;
        case COMPLEX_TYPE:
            return DB_COMPLEX_TYPE;
        default:
            std::ostringstream ss;
            ss << "Unhandled monitor aggregate type " << aggType;
            throw CARMA_ERROR(ss.str());
        }
    }
    
    MonitorAggregateType dbToAggregateType(const unsigned short& dbType) {
        switch(dbType) {
        case DB_NUMERIC_TYPE:
            return NUMERIC_TYPE;
        case DB_STRING_TYPE:
            return STRING_TYPE;
        case DB_SHORT_TYPE:
            return SHORT_TYPE;
        case DB_COMPLEX_TYPE:
            return COMPLEX_TYPE;
        default:
            std::ostringstream ss;
            ss << "Unhandled monitor aggregate type value " << dbType;
            throw CARMA_ERROR(ss.str());
        }
    }        

    unsigned short averageTypeToDB(const MonitorAverageType& avgType) {
        switch(avgType) {
        case FRAME_AVG:
            return DB_FRAME_AVG;
            break;
        case MINUTE_AVG:
            return DB_MINUTE_AVG;
            break;
        case WBCORREL_AVG:
            return DB_WBCORREL_AVG;
            break;
        case SLCORREL_AVG:
            return DB_SLCORREL_AVG;
            break;
        default:
            std::ostringstream ss;
            ss << "Unhandled average type " << avgType;
            throw CARMA_ERROR(ss.str());
        }
    }
    
    MonitorAverageType dbToAverageType(const unsigned short& dbType) {
        switch(dbType) {
        case DB_FRAME_AVG:
            return FRAME_AVG;
            break;
        case DB_MINUTE_AVG:
            return MINUTE_AVG;
            break;
        case DB_WBCORREL_AVG:
            return WBCORREL_AVG;
            break;
        case DB_SLCORREL_AVG:
            return SLCORREL_AVG;
            break;
        default:
            std::ostringstream ss;
            ss << "Unknown average type value " << dbType;
            throw CARMA_ERROR(ss.str());
        }
    }        

}}

