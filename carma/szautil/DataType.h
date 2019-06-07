// $Id: DataType.h,v 1.4 2013/07/09 17:12:00 eml Exp $

#ifndef SZA_UTIL_DATATYPE_H
#define SZA_UTIL_DATATYPE_H

/**
 * @file DataType.h
 * 
 * Tagged: Tue Jun 22 22:32:16 UTC 2004
 * 
 * @version: $Revision: 1.4 $, $Date: 2013/07/09 17:12:00 $
 * 
 * @author 
 */
#include "carma/szaarrayutils/regmap.h"
#include "carma/szaarrayutils/regtemplate.h"

#include "carma/szautil/Complex.h"
#include "carma/szautil/RegDate.h"
#include <ostream>

namespace sza {
  namespace util {
    
    /**
     * Enumerate data types
     */
    class DataType {
    public:
      
      enum Type {
	NONE          = 0x0,
	UNKNOWN       = 0x0,
	UCHAR         = 0x1,
	CHAR          = 0x2,
	BOOL          = 0x4,
	USHORT        = 0x8,
	SHORT         = 0x10,
	UINT          = 0x20,
	INT           = 0x40,
	ULONG         = 0x80,
	LONG          = 0x100,
	FLOAT         = 0x200,
	DOUBLE        = 0x400,
	DATE          = 0x800,
	COMPLEX_FLOAT = 0x1000,

        // Not handled yet in this object, but included for
        // compatibility with FITS format types

        COMPLEX_DOUBLE= 0x2000,
        STRING        = 0x4000,
        CARMASTRING   = 0x8000,
        BITMASK       = 0x10000,
      };

      
      DataType();
      DataType(bool b);
      DataType(unsigned char uc);
      DataType(char ch);
      DataType(unsigned short us);
      DataType(short s);
      DataType(unsigned int ui);
      DataType(int i);
      DataType(unsigned long ul);
      DataType(long l);
      DataType(float f);
      DataType(double d);
      DataType(sza::util::RegDate date);
      DataType(sza::util::Complex<float> cf);
      
      virtual ~DataType();
      
      /**
       * Return the size, in bytes, of the requested type
       */
      static unsigned sizeOf(Type type);
      
      /**
       * Return the size, in bytes, of the type given in RegFlags
       * specification
       */
      static unsigned sizeOf(RegMapBlock* blk);
      
      /**
       * Return the data type of a block
       */
      static Type typeOf(RegMapBlock* blk);
      static Type typeOf(RegBlockTemp* blk);
      
      /**
       * Return the data type of a block as a string appropriate for
       * the CARMA monitor system
       */
      static std::string carmaTypeString(RegMapBlock* blk);

      // Take the stored value and convert to the type corresponding
      // to a register block

      void convertToTypeOf(RegMapBlock* blk);
      double getValAsDouble();

      /**
       * Assignment operators
       */
      void operator=(bool b);
      void operator=(unsigned char uc);
      void operator=(char ch);
      void operator=(unsigned short us);
      void operator=(short s);
      void operator=(unsigned int ui);
      void operator=(int i);
      void operator=(unsigned long ul);
      void operator=(long l);
      void operator=(float f);
      void operator=(double d);
      void operator=(sza::util::RegDate date);
      void operator=(sza::util::Complex<float> cf);
      
      /**
       * Assignment operators for pointers
       */
      void operator=(bool* b);
      void operator=(unsigned char* uc);
      void operator=(char* ch);
      void operator=(unsigned short* us);
      void operator=(short* s);
      void operator=(unsigned int* ui);
      void operator=(int* i);
      void operator=(unsigned long* ul);
      void operator=(long* l);
      void operator=(float* f);
      void operator=(double* d);
      void operator=(sza::util::RegDate* date);
      void operator=(sza::util::Complex<float>* cf);

      void operator=(DataType& dataType);
      void operator=(const DataType& dataType);
      
      void operator-=(DataType& dataType);

      bool operator==(DataType& dataType);
      bool operator>(DataType& dataType);
      bool operator>=(DataType& dataType);
      bool operator<(DataType& dataType);
      bool operator<=(DataType& dataType);

      void operator++();

      void convertToAbs();

      friend std::ostream& operator<<(std::ostream& os, DataType& dataType);
      friend std::ostream& operator<<(std::ostream& os, DataType::Type type);

      /**                                                                                                           
       * Return a void ptr to the data for this data type
       */
      void* data();

      // The actual data in this DataType will be stored as a union

      union {
	bool b;
	unsigned char uc;
	char c;
	unsigned short us;
	short s;
	unsigned int ui;
	int i;
	unsigned long ul;
	long l;
	float f;
	double d;
	sza::util::RegDate::Data date;
	sza::util::Complex<float>::Data cf;
      } data_;
      
      Type type_;
      
      void checkType(DataType& dataType);

      // True if we are indexing an array of data

      bool isArray_;

      // A void ptr to the array

      void* ptr_;

      // Overloaded utility methods

      static Type typeOf(bool* obj);
      static Type typeOf(unsigned char* obj);
      static Type typeOf(char* obj);
      static Type typeOf(unsigned short* obj);
      static Type typeOf(short* obj);
      static Type typeOf(unsigned int* obj);
      static Type typeOf(int* obj);
      static Type typeOf(unsigned long* obj);
      static Type typeOf(long* obj);
      static Type typeOf(float* obj);
      static Type typeOf(double* obj);
      static Type typeOf(sza::util::RegDate* obj);
      static Type typeOf(sza::util::Complex<float>* obj);
      static Type typeOf(sza::util::RegDate::Data* obj);
      static Type typeOf(sza::util::Complex<float>::Data* obj);

    }; // End class DataType
    
  } // End namespace util
} // End namespace sza




#endif // End #ifndef SZA_UTIL_DATATYPE_H
