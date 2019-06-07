#ifndef SZA_UTIL_MONITORDATATYPE_H
#define SZA_UTIL_MONITORDATATYPE_H

/**
 * @file MonitorDataType.h
 * 
 * Tagged: Thu Mar 31 00:12:30 PST 2005
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/DataType.h"

namespace sza {
  namespace util {
    
    class MonitorDataType {
    public:
      
      // Enumerate valid formats

      enum FormatType {
	FM_CHAR=0,
	FM_UCHAR,
	FM_BOOL,
	FM_DOUBLE,
	FM_FLOAT,
	FM_SHORT,
	FM_USHORT,
	FM_INT,
	FM_UINT,
	FM_LONG,
	FM_ULONG,
	FM_DATE,
	FM_COMPLEX_FLOAT,
	FM_STRING,
	FM_UNKNOWN,
	FM_DEFAULT = FM_UINT
      };

      FormatType nativeFormat;
      FormatType selectedFormat;
      RegAspect aspect;
      
      sza::util::DataType val;
      std::string stringVal;
      std::string formatStr;

      void print();

    }; // End class MonitorDataType
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_MONITORDATATYPE_H
