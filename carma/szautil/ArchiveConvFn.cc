#include "carma/szautil/ArchiveConvFn.h"

#include<iomanip>
#include<iostream>

using namespace std;
using namespace sza::util;

#define ASSIGN_VALS(incast, outcast) \
  {\
    outcast* oPtr = (outcast*)reOut;\
    incast*  iPtr = (incast*)in;\
    unsigned iIn, iOut;\
    for(unsigned i=0; i < n; i++) {\
      iIn  = *(inInds+i);\
      iOut = *(outInds+i) + iAdd;\
      *(oPtr + iOut) = (outcast)(cals[iIn].factor_ * (double)(*(iPtr+iIn)) + cals[iIn].offset_);\
    }\
  }

#define ASSIGN_COMPLEX_VALS(incast, outcast) \
  {\
    outcast* reOutPtr = (outcast*)reOut;\
    outcast* imOutPtr = (outcast*)imOut;\
    Complex<incast>::Data* iPtr = (Complex<incast>::Data*)in;\
    unsigned iIn, iOut;\
    for(unsigned i=0; i < n; i++) {\
      iIn  = *(inInds+i);\
      iOut = *(outInds+i) + iAdd;\
      *(reOutPtr + iOut) = (outcast)(cals[iIn].factor_ * (double)((iPtr+iIn)->real_) + cals[iIn].offset_);\
      *(imOutPtr + iOut) = (outcast)(cals[iIn].factor_ * (double)((iPtr+iIn)->imag_) + cals[iIn].offset_);\
    }\
  }

#define ASSIGN_DATE_VALS(outcast) \
  {\
    outcast* oPtr = (outcast*)reOut;\
    RegDate::Data*  iPtr = (RegDate::Data*)in;\
    for(unsigned i=0; i < n; i++) {\
      *(oPtr + *(outInds+i) + iAdd) = (outcast)((double)(iPtr+i)->dayNo_ + (double)(iPtr+i)->mSec_/(1000*86400));\
    }\
  }

STRING_DISPATCH_FN(*ArchiveConvFn::stringDispatchFn_) = 0;

#define PRINT_VALS(incast, outcast, os, width, precision)	\
  {\
    outcast val;\
    incast*  iPtr = (incast*)in;\
    for(unsigned i=0; i < n; i++) {\
      val = (outcast)(cals[i].factor_ * (double)(*(iPtr+i)) + cals[i].offset_);\
      os << std::setw(width) << fixed << std::setprecision(precision) << val; \
    }\
  }

#define PRINT_COMPLEX_VALS(incast, outcast, os, width, precision)	\
  {\
    outcast re, im;\
    Complex<incast>::Data* iPtr = (Complex<incast>::Data*)in;\
    for(unsigned i=0; i < n; i++) {\
      re = (outcast)(cals[i].factor_ * (double)((iPtr+i)->real_) + cals[i].offset_);\
      im = (outcast)(cals[i].factor_ * (double)((iPtr+i)->imag_) + cals[i].offset_);\
      os << std::setw(width) << fixed << std::setprecision(precision) << re; \
      if(im < (outcast)0) {\
	os << " - i";\
      } else {\
	os << " + i";\
      }\
      os << std::setw(width) << fixed << std::setprecision(precision) << im; \
    }\
  }

#define PRINT_DATE_VALS(outcast, os, width, precision)	\
    RegDate::Data*  iPtr = (RegDate::Data*)in;\
    for(unsigned i=0; i < n; i++) {\
      os << setw(width) << fixed << setprecision(precision) << (outcast)((double)(iPtr+i)->dayNo_ + (double)(iPtr+i)->mSec_/(1000*86400)); \
    }\

#define PRINT_DATE(os)		\
  {\
    RegDate::Data*  iPtr = (RegDate::Data*)in;\
    for(unsigned i=0; i < n; i++) {\
      RegDate date((iPtr+i)->dayNo_,(iPtr+i)->mSec_);\
      os << date;\
    }\
  }

#define PRINT_CARMA_DATE(os)		\
  {\
    RegDate::Data*  iPtr = (RegDate::Data*)in;\
    for(unsigned i=0; i < n; i++) {\
      RegDate date((iPtr+i)->dayNo_,(iPtr+i)->mSec_);\
      os << date.formatCarmaString();\
    }\
  }

#define PRINT_BIT_VALS(incast, os, width, precision)	\
  {\
    incast val;\
    incast*  iPtr = (incast*)in;\
    for(unsigned i=0; i < n; i++) {\
      val = (incast)(cals[i].factor_ * (double)(*(iPtr+i)) + cals[i].offset_);\
      unsigned nByte = sizeof(val);\
      unsigned char* cPtr = (unsigned char*)&val;\
      std::ostringstream bitOs;\
      bitOs.str("");	      \
      for(unsigned iByte=0; iByte < nByte; iByte++) {\
	unsigned char byte = cPtr[iByte];\
	for(unsigned iBit=0; iBit < 8; iBit++) {\
	  bitOs << setw(1) << ((byte >> iBit) & 0x1);\
	}\
      }\
      os << std::setw(width) << fixed << std::setprecision(precision) << bitOs.str(); \
    }\
  }

/**.......................................................................
 * Constructor
 */
ArchiveConvFn::ArchiveConvFn()
{
  stringDispatchFn_ = 0;
}

/**.......................................................................
 * Destructor
 */
ArchiveConvFn::~ArchiveConvFn() {}

/**.......................................................................
 * Dispatch a string
 */
void ArchiveConvFn::setStringDispatchFn(STRING_DISPATCH_FN(*fn))
{
  stringDispatchFn_ = fn;
}

//------------------------------------------------------------
// Conversions for bool
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::boolToBool)
{
  ASSIGN_VALS(bool, bool);
}

ARC_CONV_FN(ArchiveConvFn::boolToUchar)
{
  ASSIGN_VALS(bool, unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::boolToChar)
{
  ASSIGN_VALS(bool, char);
}

ARC_CONV_FN(ArchiveConvFn::boolToUshort)
{
  ASSIGN_VALS(bool, unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::boolToShort)
{
  ASSIGN_VALS(bool, short);
}

ARC_CONV_FN(ArchiveConvFn::boolToUint)
{
  ASSIGN_VALS(bool, unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::boolToInt)
{
  ASSIGN_VALS(bool, int);
} 

ARC_CONV_FN(ArchiveConvFn::boolToUlong)
{
  ASSIGN_VALS(bool, unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::boolToLong)
{
  ASSIGN_VALS(bool, long);
}

ARC_CONV_FN(ArchiveConvFn::boolToFloat)
{
  ASSIGN_VALS(bool, float);
}

ARC_CONV_FN(ArchiveConvFn::boolToDouble)
{
  ASSIGN_VALS(bool, double);
}

//------------------------------------------------------------
// Conversions for unsigned char
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::ucharToBool)
{
  ASSIGN_VALS(unsigned char, bool);
}

ARC_CONV_FN(ArchiveConvFn::ucharToUchar)
{
  ASSIGN_VALS(unsigned char, unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::ucharToChar)
{
  ASSIGN_VALS(unsigned char, char);
}

ARC_CONV_FN(ArchiveConvFn::ucharToUshort)
{
  ASSIGN_VALS(unsigned char, unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::ucharToShort)
{
  ASSIGN_VALS(unsigned char, short);
}

ARC_CONV_FN(ArchiveConvFn::ucharToUint)
{
  ASSIGN_VALS(unsigned char, unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::ucharToInt)
{
  ASSIGN_VALS(unsigned char, int);
} 
ARC_CONV_FN(ArchiveConvFn::ucharToUlong)
{
  ASSIGN_VALS(unsigned char, unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::ucharToLong)
{
  ASSIGN_VALS(unsigned char, long);
}

ARC_CONV_FN(ArchiveConvFn::ucharToFloat)
{
  ASSIGN_VALS(unsigned char, float);
}

ARC_CONV_FN(ArchiveConvFn::ucharToDouble)
{
  ASSIGN_VALS(unsigned char, double);
}

ARC_CONV_FN(ArchiveConvFn::ucharToString)
{
  if(stringDispatchFn_) {
    stringDispatchFn_((const char *)in, args, iAdd);
  }
}

//------------------------------------------------------------
// Conversions for char
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::charToBool)
{
  ASSIGN_VALS(char, bool);
}

ARC_CONV_FN(ArchiveConvFn::charToUchar)
{
  ASSIGN_VALS(char, unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::charToChar)
{
  ASSIGN_VALS(char, char);
}

ARC_CONV_FN(ArchiveConvFn::charToUshort)
{
  ASSIGN_VALS(char, unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::charToShort)
{
  ASSIGN_VALS(char, short);
}

ARC_CONV_FN(ArchiveConvFn::charToUint)
{
  ASSIGN_VALS(char, unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::charToInt)
{
  ASSIGN_VALS(char, int);
} 
ARC_CONV_FN(ArchiveConvFn::charToUlong)
{
  ASSIGN_VALS(char, unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::charToLong)
{
  ASSIGN_VALS(char, long);
}

ARC_CONV_FN(ArchiveConvFn::charToFloat)
{
  ASSIGN_VALS(char, float);
}

ARC_CONV_FN(ArchiveConvFn::charToDouble)
{
  ASSIGN_VALS(char, double);
}

ARC_CONV_FN(ArchiveConvFn::charToString)
{
  if(stringDispatchFn_) {
    stringDispatchFn_((const char*)in, args, iAdd);
  }
}

//------------------------------------------------------------
// Conversions for unsigned short
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::ushortToBool)
{
  ASSIGN_VALS(unsigned short, bool);
}

ARC_CONV_FN(ArchiveConvFn::ushortToUchar)
{
  ASSIGN_VALS(unsigned short, unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::ushortToChar)
{
  ASSIGN_VALS(unsigned short, char);
}

ARC_CONV_FN(ArchiveConvFn::ushortToUshort)
{
  ASSIGN_VALS(unsigned short, unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::ushortToShort)
{
  ASSIGN_VALS(unsigned short, short);
}

ARC_CONV_FN(ArchiveConvFn::ushortToUint)
{
  ASSIGN_VALS(unsigned short, unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::ushortToInt)
{
  ASSIGN_VALS(unsigned short, int);
} 
ARC_CONV_FN(ArchiveConvFn::ushortToUlong)
{
  ASSIGN_VALS(unsigned short, unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::ushortToLong)
{
  ASSIGN_VALS(unsigned short, long);
}

ARC_CONV_FN(ArchiveConvFn::ushortToFloat)
{
  ASSIGN_VALS(unsigned short, float);
}

ARC_CONV_FN(ArchiveConvFn::ushortToDouble)
{
  ASSIGN_VALS(unsigned short, double);
}

//------------------------------------------------------------
// Conversions for short
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::shortToBool)
{
  ASSIGN_VALS(short, bool);
}

ARC_CONV_FN(ArchiveConvFn::shortToUchar)
{
  ASSIGN_VALS(short, unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::shortToChar)
{
  ASSIGN_VALS(short, char);
}

ARC_CONV_FN(ArchiveConvFn::shortToUshort)
{
  ASSIGN_VALS(short, unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::shortToShort)
{
  ASSIGN_VALS(short, short);
}

ARC_CONV_FN(ArchiveConvFn::shortToUint)
{
  ASSIGN_VALS(short, unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::shortToInt)
{
  ASSIGN_VALS(short, int);
} 
ARC_CONV_FN(ArchiveConvFn::shortToUlong)
{
  ASSIGN_VALS(short, unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::shortToLong)
{
  ASSIGN_VALS(short, long);
}

ARC_CONV_FN(ArchiveConvFn::shortToFloat)
{
  ASSIGN_VALS(short, float);
}

ARC_CONV_FN(ArchiveConvFn::shortToDouble)
{
  ASSIGN_VALS(short, double);
}

//------------------------------------------------------------
// Conversions for unsigned int
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::uintToBool)
{
  ASSIGN_VALS(unsigned int, bool);
}

ARC_CONV_FN(ArchiveConvFn::uintToUchar)
{
  ASSIGN_VALS(unsigned int, unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::uintToChar)
{
  ASSIGN_VALS(unsigned int, char);
}

ARC_CONV_FN(ArchiveConvFn::uintToUshort)
{
  ASSIGN_VALS(unsigned int, unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::uintToShort)
{
  ASSIGN_VALS(unsigned int, short);
}

ARC_CONV_FN(ArchiveConvFn::uintToUint)
{
  ASSIGN_VALS(unsigned int, unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::uintToInt)
{
  ASSIGN_VALS(unsigned int, int);
} 
ARC_CONV_FN(ArchiveConvFn::uintToUlong)
{
  ASSIGN_VALS(unsigned int, unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::uintToLong)
{
  ASSIGN_VALS(unsigned int, long);
}

ARC_CONV_FN(ArchiveConvFn::uintToFloat)
{
  ASSIGN_VALS(unsigned int, float);
}

ARC_CONV_FN(ArchiveConvFn::uintToDouble)
{
  ASSIGN_VALS(unsigned int, double);
}

//------------------------------------------------------------
// Conversions for int
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::intToBool)
{
  ASSIGN_VALS(int, bool);
}

ARC_CONV_FN(ArchiveConvFn::intToUchar)
{
  ASSIGN_VALS(int, unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::intToChar)
{
  ASSIGN_VALS(int, char);
}

ARC_CONV_FN(ArchiveConvFn::intToUshort)
{
  ASSIGN_VALS(int, unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::intToShort)
{
  ASSIGN_VALS(int, short);
}

ARC_CONV_FN(ArchiveConvFn::intToUint)
{
  ASSIGN_VALS(int, unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::intToInt)
{
  ASSIGN_VALS(int, int);
} 
ARC_CONV_FN(ArchiveConvFn::intToUlong)
{
  ASSIGN_VALS(int, unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::intToLong)
{
  ASSIGN_VALS(int, long);
}

ARC_CONV_FN(ArchiveConvFn::intToFloat)
{
  ASSIGN_VALS(int, float);
}

ARC_CONV_FN(ArchiveConvFn::intToDouble)
{
  ASSIGN_VALS(int, double);
}

//------------------------------------------------------------
// Conversions for unsigned long
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::ulongToBool)
{
  ASSIGN_VALS(unsigned long, bool);
}

ARC_CONV_FN(ArchiveConvFn::ulongToUchar)
{
  ASSIGN_VALS(unsigned long, unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::ulongToChar)
{
  ASSIGN_VALS(unsigned long, char);
}

ARC_CONV_FN(ArchiveConvFn::ulongToUshort)
{
  ASSIGN_VALS(unsigned long, unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::ulongToShort)
{
  ASSIGN_VALS(unsigned long, short);
}

ARC_CONV_FN(ArchiveConvFn::ulongToUint)
{
  ASSIGN_VALS(unsigned long, unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::ulongToInt)
{
  ASSIGN_VALS(unsigned long, int);
} 
ARC_CONV_FN(ArchiveConvFn::ulongToUlong)
{
  ASSIGN_VALS(unsigned long, unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::ulongToLong)
{
  ASSIGN_VALS(unsigned long, long);
}

ARC_CONV_FN(ArchiveConvFn::ulongToFloat)
{
  ASSIGN_VALS(unsigned long, float);
}

ARC_CONV_FN(ArchiveConvFn::ulongToDouble)
{
  ASSIGN_VALS(unsigned long, double);
}

//------------------------------------------------------------
// Conversions for long
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::longToBool)
{
  ASSIGN_VALS(long, bool);
}

ARC_CONV_FN(ArchiveConvFn::longToUchar)
{
  ASSIGN_VALS(long, unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::longToChar)
{
  ASSIGN_VALS(long, char);
}

ARC_CONV_FN(ArchiveConvFn::longToUshort)
{
  ASSIGN_VALS(long, unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::longToShort)
{
  ASSIGN_VALS(long, short);
}

ARC_CONV_FN(ArchiveConvFn::longToUint)
{
  ASSIGN_VALS(long, unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::longToInt)
{
  ASSIGN_VALS(long, int);
} 
ARC_CONV_FN(ArchiveConvFn::longToUlong)
{
  ASSIGN_VALS(long, unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::longToLong)
{
  ASSIGN_VALS(long, long);
}

ARC_CONV_FN(ArchiveConvFn::longToFloat)
{
  ASSIGN_VALS(long, float);
}

ARC_CONV_FN(ArchiveConvFn::longToDouble)
{
  ASSIGN_VALS(long, double);
}

//------------------------------------------------------------
// Conversions for float
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::floatToBool)
{
  ASSIGN_VALS(float, bool);
}

ARC_CONV_FN(ArchiveConvFn::floatToUchar)
{
  ASSIGN_VALS(float, unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::floatToChar)
{
  ASSIGN_VALS(float, char);
}

ARC_CONV_FN(ArchiveConvFn::floatToUshort)
{
  ASSIGN_VALS(float, unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::floatToShort)
{
  ASSIGN_VALS(float, short);
}

ARC_CONV_FN(ArchiveConvFn::floatToUint)
{
  ASSIGN_VALS(float, unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::floatToInt)
{
  ASSIGN_VALS(float, int);
} 
ARC_CONV_FN(ArchiveConvFn::floatToUlong)
{
  ASSIGN_VALS(float, unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::floatToLong)
{
  ASSIGN_VALS(float, long);
}

ARC_CONV_FN(ArchiveConvFn::floatToFloat)
{
  ASSIGN_VALS(float, float);
}

ARC_CONV_FN(ArchiveConvFn::floatToDouble)
{
  ASSIGN_VALS(float, double);
}

//------------------------------------------------------------
// Conversions for double
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::doubleToBool)
{
  ASSIGN_VALS(double, bool);
}

ARC_CONV_FN(ArchiveConvFn::doubleToUchar)
{
  ASSIGN_VALS(double, unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::doubleToChar)
{
  ASSIGN_VALS(double, char);
}

ARC_CONV_FN(ArchiveConvFn::doubleToUshort)
{
  ASSIGN_VALS(double, unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::doubleToShort)
{
  ASSIGN_VALS(double, short);
}

ARC_CONV_FN(ArchiveConvFn::doubleToUint)
{
  ASSIGN_VALS(double, unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::doubleToInt)
{
  ASSIGN_VALS(double, int);
} 
ARC_CONV_FN(ArchiveConvFn::doubleToUlong)
{
  ASSIGN_VALS(double, unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::doubleToLong)
{
  ASSIGN_VALS(double, long);
}

ARC_CONV_FN(ArchiveConvFn::doubleToFloat)
{
  ASSIGN_VALS(double, float);
}

ARC_CONV_FN(ArchiveConvFn::doubleToDouble)
{
  ASSIGN_VALS(double, double);
}

//------------------------------------------------------------
// Conversions for complex float 
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::complexFloatToComplexFloat)
{
  ASSIGN_COMPLEX_VALS(float, float);
}

//------------------------------------------------------------
// Conversions for date
//------------------------------------------------------------

ARC_CONV_FN(ArchiveConvFn::dateToBool)
{
  ASSIGN_DATE_VALS(bool);
}

ARC_CONV_FN(ArchiveConvFn::dateToUchar)
{
  ASSIGN_DATE_VALS(unsigned char);
}

ARC_CONV_FN(ArchiveConvFn::dateToChar)
{
  ASSIGN_DATE_VALS(char);
}

ARC_CONV_FN(ArchiveConvFn::dateToUshort)
{
  ASSIGN_DATE_VALS(unsigned short);
}

ARC_CONV_FN(ArchiveConvFn::dateToShort)
{
  ASSIGN_DATE_VALS(short);
}

ARC_CONV_FN(ArchiveConvFn::dateToUint)
{
  ASSIGN_DATE_VALS(unsigned int);
}

ARC_CONV_FN(ArchiveConvFn::dateToInt)
{
  ASSIGN_DATE_VALS(int);
} 
ARC_CONV_FN(ArchiveConvFn::dateToUlong)
{
  ASSIGN_DATE_VALS(unsigned long);
}

ARC_CONV_FN(ArchiveConvFn::dateToLong)
{
  ASSIGN_DATE_VALS(long);
}

ARC_CONV_FN(ArchiveConvFn::dateToFloat)
{
  ASSIGN_DATE_VALS(float);
}

ARC_CONV_FN(ArchiveConvFn::dateToDouble)
{
  ASSIGN_DATE_VALS(double);
}

ARC_CONV_FN(ArchiveConvFn::dateToString)
{
  RegDate date;
  unsigned iOut;
  RegDate::Data* iPtr = (RegDate::Data*)in;

  for(unsigned i=0; i < n; i++) {
    date = *(iPtr+i);

#if 1
    iOut = *(outInds+i) + iAdd;

    if(stringDispatchFn_) {
      stringDispatchFn_(date.str().c_str(), args, iOut);
    }
#endif
  }
}

ARC_CONV_FN(ArchiveConvFn::dateToCarmaString)
{
  RegDate date;
  unsigned iOut;
  RegDate::Data* iPtr = (RegDate::Data*)in;

  for(unsigned i=0; i < n; i++) {
    date = *(iPtr+i);

    COUT(date);
#if 0
    iOut = *(outInds+i) + iAdd;

    if(stringDispatchFn_) {
      stringDispatchFn_(date.str().c_str(), args, iOut);
    }
#endif
  }
}

void ArchiveConvFn::setConvFn(DataType::Type inType, DataType::Type outType, 
			      ARC_CONV_FN(**fptr))
{
  // Initialize the convfn ptr to NULL, so that it can be checked by
  // return value

  *fptr = 0;

  switch (inType) {
  case DataType::BOOL:
    setBoolConvFn(outType, fptr);
    break;
  case DataType::UCHAR:
    setUcharConvFn(outType, fptr);
    break;
  case DataType::CHAR:
    setCharConvFn(outType, fptr);
    break;
  case DataType::STRING:
    setStringConvFn(outType, fptr);
    break;
  case DataType::USHORT:
    setUshortConvFn(outType, fptr);
    break;
  case DataType::SHORT:
    setShortConvFn(outType, fptr);
    break;
  case DataType::UINT:
    setUintConvFn(outType, fptr);
    break;
  case DataType::INT:
    setIntConvFn(outType, fptr);
    break;
  case DataType::ULONG:
    setUlongConvFn(outType, fptr);
    break;
  case DataType::LONG:
    setLongConvFn(outType, fptr);
    break;
  case DataType::FLOAT:
    setFloatConvFn(outType, fptr);
    break;
  case DataType::DATE:
    setDateConvFn(outType, fptr);
    break;
  case DataType::DOUBLE:
    setDoubleConvFn(outType, fptr);
    break;
  case DataType::COMPLEX_FLOAT:
    setComplexFloatConvFn(outType, fptr);
    break;
  default:
    break;
  }
}

void ArchiveConvFn::setBoolConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = boolToBool;
    break;
  case DataType::UCHAR:
    *fptr = boolToUchar;
    break;
  case DataType::CHAR:
    *fptr = boolToChar;
    break;
  case DataType::USHORT:
    *fptr = boolToUshort;
    break;
  case DataType::SHORT:
    *fptr = boolToShort;
    break;
  case DataType::UINT:
    *fptr = boolToUint;
    break;
  case DataType::INT:
    *fptr = boolToInt;
    break;
  case DataType::ULONG:
    *fptr = boolToUlong;
    break;
  case DataType::LONG:
    *fptr = boolToLong;
    break;
  case DataType::FLOAT:
    *fptr = boolToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = boolToDouble;
    break;
  default:
    break;
  }
}

void ArchiveConvFn::setStringConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  setUcharConvFn(outType, fptr);
}

void ArchiveConvFn::setUcharConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = ucharToBool;
    break;
  case DataType::UCHAR:
    *fptr = ucharToUchar;
    break;
  case DataType::CHAR:
    *fptr = ucharToChar;
    break;
  case DataType::USHORT:
    *fptr = ucharToUshort;
    break;
  case DataType::SHORT:
    *fptr = ucharToShort;
    break;
  case DataType::UINT:
    *fptr = ucharToUint;
    break;
  case DataType::INT:
    *fptr = ucharToInt;
    break;
  case DataType::ULONG:
    *fptr = ucharToUlong;
    break;
  case DataType::LONG:
    *fptr = ucharToLong;
    break;
  case DataType::FLOAT:
    *fptr = ucharToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = ucharToDouble;
    break;
  case DataType::STRING:
    *fptr = ucharToString;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::UCHAR 
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setCharConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = charToBool;
    break;
  case DataType::UCHAR:
    *fptr = charToUchar;
    break;
  case DataType::CHAR:
    *fptr = charToChar;
    break;
  case DataType::USHORT:
    *fptr = charToUshort;
    break;
  case DataType::SHORT:
    *fptr = charToShort;
    break;
  case DataType::UINT:
    *fptr = charToUint;
    break;
  case DataType::INT:
    *fptr = charToInt;
    break;
  case DataType::ULONG:
    *fptr = charToUlong;
    break;
  case DataType::LONG:
    *fptr = charToLong;
    break;
  case DataType::FLOAT:
    *fptr = charToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = charToDouble;
    break;
  case DataType::STRING:
    *fptr = charToString;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::CHAR 
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setUshortConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = ushortToBool;
    break;
  case DataType::UCHAR:
    *fptr = ushortToUchar;
    break;
  case DataType::CHAR:
    *fptr = ushortToChar;
    break;
  case DataType::USHORT:
    *fptr = ushortToUshort;
    break;
  case DataType::SHORT:
    *fptr = ushortToShort;
    break;
  case DataType::UINT:
    *fptr = ushortToUint;
    break;
  case DataType::INT:
    *fptr = ushortToInt;
    break;
  case DataType::ULONG:
    *fptr = ushortToUlong;
    break;
  case DataType::LONG:
    *fptr = ushortToLong;
    break;
  case DataType::FLOAT:
    *fptr = ushortToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = ushortToDouble;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::USHORT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setShortConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = shortToBool;
    break;
  case DataType::UCHAR:
    *fptr = shortToUchar;
    break;
  case DataType::CHAR:
    *fptr = shortToChar;
    break;
  case DataType::USHORT:
    *fptr = shortToUshort;
    break;
  case DataType::SHORT:
    *fptr = shortToShort;
    break;
  case DataType::UINT:
    *fptr = shortToUint;
    break;
  case DataType::INT:
    *fptr = shortToInt;
    break;
  case DataType::ULONG:
    *fptr = shortToUlong;
    break;
  case DataType::LONG:
    *fptr = shortToLong;
    break;
  case DataType::FLOAT:
    *fptr = shortToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = shortToDouble;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::SHORT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setUintConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = uintToBool;
    break;
  case DataType::UCHAR:
    *fptr = uintToUchar;
    break;
  case DataType::CHAR:
    *fptr = uintToChar;
    break;
  case DataType::USHORT:
    *fptr = uintToUshort;
    break;
  case DataType::SHORT:
    *fptr = uintToShort;
    break;
  case DataType::UINT:
    *fptr = uintToUint;
    break;
  case DataType::INT:
    *fptr = uintToInt;
    break;
  case DataType::ULONG:
    *fptr = uintToUlong;
    break;
  case DataType::LONG:
    *fptr = uintToLong;
    break;
  case DataType::FLOAT:
    *fptr = uintToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = uintToDouble;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::UINT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setIntConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = intToBool;
    break;
  case DataType::UCHAR:
    *fptr = intToUchar;
    break;
  case DataType::CHAR:
    *fptr = intToChar;
    break;
  case DataType::USHORT:
    *fptr = intToUshort;
    break;
  case DataType::SHORT:
    *fptr = intToShort;
    break;
  case DataType::UINT:
    *fptr = intToUint;
    break;
  case DataType::INT:
    *fptr = intToInt;
    break;
  case DataType::ULONG:
    *fptr = intToUlong;
    break;
  case DataType::LONG:
    *fptr = intToLong;
    break;
  case DataType::FLOAT:
    *fptr = intToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = intToDouble;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::INT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setUlongConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = ulongToBool;
    break;
  case DataType::UCHAR:
    *fptr = ulongToUchar;
    break;
  case DataType::CHAR:
    *fptr = ulongToChar;
    break;
  case DataType::USHORT:
    *fptr = ulongToUshort;
    break;
  case DataType::SHORT:
    *fptr = ulongToShort;
    break;
  case DataType::UINT:
    *fptr = ulongToUint;
    break;
  case DataType::INT:
    *fptr = ulongToInt;
    break;
  case DataType::ULONG:
    *fptr = ulongToUlong;
    break;
  case DataType::LONG:
    *fptr = ulongToLong;
    break;
  case DataType::FLOAT:
    *fptr = ulongToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = ulongToDouble;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::ULONG
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setLongConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = longToBool;
    break;
  case DataType::UCHAR:
    *fptr = longToUchar;
    break;
  case DataType::CHAR:
    *fptr = longToChar;
    break;
  case DataType::USHORT:
    *fptr = longToUshort;
    break;
  case DataType::SHORT:
    *fptr = longToShort;
    break;
  case DataType::UINT:
    *fptr = longToUint;
    break;
  case DataType::INT:
    *fptr = longToInt;
    break;
  case DataType::ULONG:
    *fptr = longToUlong;
    break;
  case DataType::LONG:
    *fptr = longToLong;
    break;
  case DataType::FLOAT:
    *fptr = longToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = longToDouble;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::LONG
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setFloatConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = floatToBool;
    break;
  case DataType::UCHAR:
    *fptr = floatToUchar;
    break;
  case DataType::CHAR:
    *fptr = floatToChar;
    break;
  case DataType::USHORT:
    *fptr = floatToUshort;
    break;
  case DataType::SHORT:
    *fptr = floatToShort;
    break;
  case DataType::UINT:
    *fptr = floatToUint;
    break;
  case DataType::INT:
    *fptr = floatToInt;
    break;
  case DataType::ULONG:
    *fptr = floatToUlong;
    break;
  case DataType::LONG:
    *fptr = floatToLong;
    break;
  case DataType::FLOAT:
    *fptr = floatToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = floatToDouble;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::FLOAT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setDoubleConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = doubleToBool;
    break;
  case DataType::UCHAR:
    *fptr = doubleToUchar;
    break;
  case DataType::CHAR:
    *fptr = doubleToChar;
    break;
  case DataType::USHORT:
    *fptr = doubleToUshort;
    break;
  case DataType::SHORT:
    *fptr = doubleToShort;
    break;
  case DataType::UINT:
    *fptr = doubleToUint;
    break;
  case DataType::INT:
    *fptr = doubleToInt;
    break;
  case DataType::ULONG:
    *fptr = doubleToUlong;
    break;
  case DataType::LONG:
    *fptr = doubleToLong;
    break;
  case DataType::FLOAT:
    *fptr = doubleToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = doubleToDouble;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::DOUBLE
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setComplexFloatConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::COMPLEX_FLOAT:
    *fptr = complexFloatToComplexFloat;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::COMPLEX_FLOAT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setDateConvFn(DataType::Type outType, ARC_CONV_FN(**fptr))
{
  switch (outType) {
  case DataType::UCHAR:
    *fptr = dateToUchar;
    break;
  case DataType::CHAR:
    *fptr = dateToChar;
    break;
  case DataType::USHORT:
    *fptr = dateToUshort;
    break;
  case DataType::SHORT:
    *fptr = dateToShort;
    break;
  case DataType::UINT:
    *fptr = dateToUint;
    break;
  case DataType::INT:
    *fptr = dateToInt;
    break;
  case DataType::ULONG:
    *fptr = dateToUlong;
    break;
  case DataType::LONG:
    *fptr = dateToLong;
    break;
  case DataType::FLOAT:
    *fptr = dateToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = dateToDouble;
    break;
  case DataType::STRING:
    *fptr = dateToString;
    break;
  case DataType::CARMASTRING:
    *fptr = dateToCarmaString;
    break;
  case DataType::DATE:
    *fptr = dateToDouble;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::DATE
	       << " to " << outType);
    break;
  }
}

//------------------------------------------------------------
// Conversions for bool
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printBoolToBool)
{
  PRINT_VALS(bool, bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printBoolToUchar)
{
  PRINT_VALS(bool, unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printBoolToChar)
{
  PRINT_VALS(bool, char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printBoolToUshort)
{
  PRINT_VALS(bool, unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printBoolToShort)
{
  PRINT_VALS(bool, short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printBoolToUint)
{
  PRINT_VALS(bool, unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printBoolToInt)
{
  PRINT_VALS(bool, int, os, width, precision);
} 

ARC_PRINT_FN(ArchiveConvFn::printBoolToUlong)
{
  PRINT_VALS(bool, unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printBoolToLong)
{
  PRINT_VALS(bool, long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printBoolToFloat)
{
  PRINT_VALS(bool, float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printBoolToDouble)
{
  PRINT_VALS(bool, double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printBoolToString)
{
  bool val;					
  bool*  iPtr = (bool*)in;			

  for(unsigned i=0; i < n; i++) {
    val = *iPtr;
    os << std::setw(width) << fixed << std::setprecision(precision) << (val ? "true" : " false");
  }
}

ARC_PRINT_FN(ArchiveConvFn::printBoolToBitmask)
{
  PRINT_BIT_VALS(bool, os, width, precision);
}

//------------------------------------------------------------
// Conversions for unsigned char
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printUcharToBool)
{
  PRINT_VALS(unsigned char, bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUcharToUchar)
{
  PRINT_VALS(unsigned char, unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUcharToChar)
{
  PRINT_VALS(unsigned char, char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUcharToUshort)
{
  PRINT_VALS(unsigned char, unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUcharToShort)
{
  PRINT_VALS(unsigned char, short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUcharToUint)
{
  PRINT_VALS(unsigned char, unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUcharToInt)
{
  PRINT_VALS(unsigned char, int, os, width, precision);
} 
ARC_PRINT_FN(ArchiveConvFn::printUcharToUlong)
{
  PRINT_VALS(unsigned char, unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUcharToLong)
{
  PRINT_VALS(unsigned char, long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUcharToFloat)
{
  PRINT_VALS(unsigned char, float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUcharToDouble)
{
  PRINT_VALS(unsigned char, double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUcharToString)
{
  std::string str((char*)in);
  os << std::setw(width) << fixed << std::setprecision(precision) << str;
}

ARC_PRINT_FN(ArchiveConvFn::printUcharToBitmask)
{
  PRINT_BIT_VALS(unsigned char, os, width, precision);
}

//------------------------------------------------------------
// Conversions for char
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printCharToBool)
{
  PRINT_VALS(char, bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printCharToUchar)
{
  PRINT_VALS(char, unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printCharToChar)
{
  PRINT_VALS(char, char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printCharToUshort)
{
  PRINT_VALS(char, unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printCharToShort)
{
  PRINT_VALS(char, short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printCharToUint)
{
  PRINT_VALS(char, unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printCharToInt)
{
  PRINT_VALS(char, int, os, width, precision);
} 
ARC_PRINT_FN(ArchiveConvFn::printCharToUlong)
{
  PRINT_VALS(char, unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printCharToLong)
{
  PRINT_VALS(char, long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printCharToFloat)
{
  PRINT_VALS(char, float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printCharToDouble)
{
  PRINT_VALS(char, double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printCharToString)
{
  std::string str((char*)in);
  os << std::setw(width) << fixed << std::setprecision(precision) << str;
}

ARC_PRINT_FN(ArchiveConvFn::printCharToBitmask)
{
  PRINT_BIT_VALS(char, os, width, precision);
}

//------------------------------------------------------------
// Conversions for unsigned short
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printUshortToBool)
{
  PRINT_VALS(unsigned short, bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUshortToUchar)
{
  PRINT_VALS(unsigned short, unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUshortToChar)
{
  PRINT_VALS(unsigned short, char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUshortToUshort)
{
  PRINT_VALS(unsigned short, unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUshortToShort)
{
  PRINT_VALS(unsigned short, short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUshortToUint)
{
  PRINT_VALS(unsigned short, unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUshortToInt)
{
  PRINT_VALS(unsigned short, int, os, width, precision);
} 
ARC_PRINT_FN(ArchiveConvFn::printUshortToUlong)
{
  PRINT_VALS(unsigned short, unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUshortToLong)
{
  PRINT_VALS(unsigned short, long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUshortToFloat)
{
  PRINT_VALS(unsigned short, float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUshortToDouble)
{
  PRINT_VALS(unsigned short, double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUshortToBitmask)
{
  PRINT_BIT_VALS(unsigned short, os, width, precision);
}

//------------------------------------------------------------
// Conversions for short
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printShortToBool)
{
  PRINT_VALS(short, bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printShortToUchar)
{
  PRINT_VALS(short, unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printShortToChar)
{
  PRINT_VALS(short, char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printShortToUshort)
{
  PRINT_VALS(short, unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printShortToShort)
{
  PRINT_VALS(short, short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printShortToUint)
{
  PRINT_VALS(short, unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printShortToInt)
{
  PRINT_VALS(short, int, os, width, precision);
} 
ARC_PRINT_FN(ArchiveConvFn::printShortToUlong)
{
  PRINT_VALS(short, unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printShortToLong)
{
  PRINT_VALS(short, long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printShortToFloat)
{
  PRINT_VALS(short, float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printShortToDouble)
{
  PRINT_VALS(short, double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printShortToBitmask)
{
  PRINT_BIT_VALS(short, os, width, precision);
}

//------------------------------------------------------------
// Conversions for unsigned int
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printUintToBool)
{
  PRINT_VALS(unsigned int, bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUintToUchar)
{
  PRINT_VALS(unsigned int, unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUintToChar)
{
  PRINT_VALS(unsigned int, char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUintToUshort)
{
  PRINT_VALS(unsigned int, unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUintToShort)
{
  PRINT_VALS(unsigned int, short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUintToUint)
{
  PRINT_VALS(unsigned int, unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUintToInt)
{
  PRINT_VALS(unsigned int, int, os, width, precision);
} 
ARC_PRINT_FN(ArchiveConvFn::printUintToUlong)
{
  PRINT_VALS(unsigned int, unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUintToLong)
{
  PRINT_VALS(unsigned int, long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUintToFloat)
{
  PRINT_VALS(unsigned int, float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUintToDouble)
{
  PRINT_VALS(unsigned int, double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUintToBitmask)
{
  PRINT_BIT_VALS(unsigned int, os, width, precision);
}

//------------------------------------------------------------
// Conversions for int
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printIntToBool)
{
  PRINT_VALS(int, bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printIntToUchar)
{
  PRINT_VALS(int, unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printIntToChar)
{
  PRINT_VALS(int, char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printIntToUshort)
{
  PRINT_VALS(int, unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printIntToShort)
{
  PRINT_VALS(int, short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printIntToUint)
{
  PRINT_VALS(int, unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printIntToInt)
{
  PRINT_VALS(int, int, os, width, precision);
} 
ARC_PRINT_FN(ArchiveConvFn::printIntToUlong)
{
  PRINT_VALS(int, unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printIntToLong)
{
  PRINT_VALS(int, long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printIntToFloat)
{
  PRINT_VALS(int, float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printIntToDouble)
{
  PRINT_VALS(int, double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printIntToBitmask)
{
  PRINT_BIT_VALS(int, os, width, precision);
}

//------------------------------------------------------------
// Conversions for unsigned long
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printUlongToBool)
{
  PRINT_VALS(unsigned long, bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUlongToUchar)
{
  PRINT_VALS(unsigned long, unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUlongToChar)
{
  PRINT_VALS(unsigned long, char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUlongToUshort)
{
  PRINT_VALS(unsigned long, unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUlongToShort)
{
  PRINT_VALS(unsigned long, short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUlongToUint)
{
  PRINT_VALS(unsigned long, unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUlongToInt)
{
  PRINT_VALS(unsigned long, int, os, width, precision);
} 
ARC_PRINT_FN(ArchiveConvFn::printUlongToUlong)
{
  PRINT_VALS(unsigned long, unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUlongToLong)
{
  PRINT_VALS(unsigned long, long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUlongToFloat)
{
  PRINT_VALS(unsigned long, float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUlongToDouble)
{
  PRINT_VALS(unsigned long, double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printUlongToBitmask)
{
  PRINT_BIT_VALS(unsigned long, os, width, precision);
}

//------------------------------------------------------------
// Conversions for long
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printLongToBool)
{
  PRINT_VALS(long, bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printLongToUchar)
{
  PRINT_VALS(long, unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printLongToChar)
{
  PRINT_VALS(long, char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printLongToUshort)
{
  PRINT_VALS(long, unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printLongToShort)
{
  PRINT_VALS(long, short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printLongToUint)
{
  PRINT_VALS(long, unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printLongToInt)
{
  PRINT_VALS(long, int, os, width, precision);
} 
ARC_PRINT_FN(ArchiveConvFn::printLongToUlong)
{
  PRINT_VALS(long, unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printLongToLong)
{
  PRINT_VALS(long, long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printLongToFloat)
{
  PRINT_VALS(long, float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printLongToDouble)
{
  PRINT_VALS(long, double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printLongToBitmask)
{
  PRINT_BIT_VALS(long, os, width, precision);
}

//------------------------------------------------------------
// Conversions for float
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printFloatToBool)
{
  PRINT_VALS(float, bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printFloatToUchar)
{
  PRINT_VALS(float, unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printFloatToChar)
{
  PRINT_VALS(float, char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printFloatToUshort)
{
  PRINT_VALS(float, unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printFloatToShort)
{
  PRINT_VALS(float, short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printFloatToUint)
{
  PRINT_VALS(float, unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printFloatToInt)
{
  PRINT_VALS(float, int, os, width, precision);
} 
ARC_PRINT_FN(ArchiveConvFn::printFloatToUlong)
{
  PRINT_VALS(float, unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printFloatToLong)
{
  PRINT_VALS(float, long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printFloatToFloat)
{
  PRINT_VALS(float, float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printFloatToDouble)
{
  PRINT_VALS(float, double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printFloatToBitmask)
{
  PRINT_BIT_VALS(float, os, width, precision);
}

//------------------------------------------------------------
// Conversions for double
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printDoubleToBool)
{
  PRINT_VALS(double, bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDoubleToUchar)
{
  PRINT_VALS(double, unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDoubleToChar)
{
  PRINT_VALS(double, char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDoubleToUshort)
{
  PRINT_VALS(double, unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDoubleToShort)
{
  PRINT_VALS(double, short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDoubleToUint)
{
  PRINT_VALS(double, unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDoubleToInt)
{
  PRINT_VALS(double, int, os, width, precision);
} 
ARC_PRINT_FN(ArchiveConvFn::printDoubleToUlong)
{
  PRINT_VALS(double, unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDoubleToLong)
{
  PRINT_VALS(double, long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDoubleToFloat)
{
  PRINT_VALS(double, float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDoubleToDouble)
{
  PRINT_VALS(double, double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDoubleToBitmask)
{
  PRINT_BIT_VALS(double, os, width, precision);
}

//------------------------------------------------------------
// Conversions for complex float 
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printComplexFloatToComplexFloat)
{
  PRINT_COMPLEX_VALS(float, float, os, width, precision);
}

//------------------------------------------------------------
// Conversions for date
//------------------------------------------------------------

ARC_PRINT_FN(ArchiveConvFn::printDateToBool)
{
  PRINT_DATE_VALS(bool, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDateToUchar)
{
  PRINT_DATE_VALS(unsigned char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDateToChar)
{
  PRINT_DATE_VALS(char, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDateToUshort)
{
  PRINT_DATE_VALS(unsigned short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDateToShort)
{
  PRINT_DATE_VALS(short, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDateToUint)
{
  PRINT_DATE_VALS(unsigned int, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDateToInt)
{
  PRINT_DATE_VALS(int, os, width, precision);
} 
ARC_PRINT_FN(ArchiveConvFn::printDateToUlong)
{
  PRINT_DATE_VALS(unsigned long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDateToLong)
{
  PRINT_DATE_VALS(long, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDateToFloat)
{
  PRINT_DATE_VALS(float, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDateToDouble)
{
  PRINT_DATE_VALS(double, os, width, precision);
}

ARC_PRINT_FN(ArchiveConvFn::printDateToString)
{
  PRINT_DATE(os);
}

ARC_PRINT_FN(ArchiveConvFn::printDateToCarmaString)
{
  PRINT_CARMA_DATE(os);
}

void ArchiveConvFn::setPrintFn(DataType::Type inType, DataType::Type outType, 
			       ARC_PRINT_FN(**fptr))
{
  // Initialize the convfn ptr to NULL, so that it can be checked by
  // return value

  *fptr = 0;

  switch (inType) {
  case DataType::BOOL:
    setBoolPrintFn(outType, fptr);
    break;
  case DataType::UCHAR:
    setUcharPrintFn(outType, fptr);
    break;
  case DataType::CHAR:
    setCharPrintFn(outType, fptr);
    break;
  case DataType::STRING:
    setStringPrintFn(outType, fptr);
    break;
  case DataType::USHORT:
    setUshortPrintFn(outType, fptr);
    break;
  case DataType::SHORT:
    setShortPrintFn(outType, fptr);
    break;
  case DataType::UINT:
    setUintPrintFn(outType, fptr);
    break;
  case DataType::INT:
    setIntPrintFn(outType, fptr);
    break;
  case DataType::ULONG:
    setUlongPrintFn(outType, fptr);
    break;
  case DataType::LONG:
    setLongPrintFn(outType, fptr);
    break;
  case DataType::FLOAT:
    setFloatPrintFn(outType, fptr);
    break;
  case DataType::DATE:
    setDatePrintFn(outType, fptr);
    break;
  case DataType::DOUBLE:
    setDoublePrintFn(outType, fptr);
    break;
  case DataType::COMPLEX_FLOAT:
    setComplexFloatPrintFn(outType, fptr);
    break;
  default:
    break;
  }
}

void ArchiveConvFn::setBoolPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printBoolToBool;
    break;
  case DataType::UCHAR:
    *fptr = printBoolToUchar;
    break;
  case DataType::CHAR:
    *fptr = printBoolToChar;
    break;
  case DataType::USHORT:
    *fptr = printBoolToUshort;
    break;
  case DataType::SHORT:
    *fptr = printBoolToShort;
    break;
  case DataType::UINT:
    *fptr = printBoolToUint;
    break;
  case DataType::INT:
    *fptr = printBoolToInt;
    break;
  case DataType::ULONG:
    *fptr = printBoolToUlong;
    break;
  case DataType::LONG:
    *fptr = printBoolToLong;
    break;
  case DataType::FLOAT:
    *fptr = printBoolToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printBoolToDouble;
    break;
  case DataType::STRING:
    *fptr = printBoolToString;
    break;
  case DataType::BITMASK:
    *fptr = printBoolToBitmask;
    break;
  default:
    break;
  }
}

void ArchiveConvFn::setUcharPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printUcharToBool;
    break;
  case DataType::UCHAR:
    *fptr = printUcharToUchar;
    break;
  case DataType::CHAR:
    *fptr = printUcharToChar;
    break;
  case DataType::USHORT:
    *fptr = printUcharToUshort;
    break;
  case DataType::SHORT:
    *fptr = printUcharToShort;
    break;
  case DataType::UINT:
    *fptr = printUcharToUint;
    break;
  case DataType::INT:
    *fptr = printUcharToInt;
    break;
  case DataType::ULONG:
    *fptr = printUcharToUlong;
    break;
  case DataType::LONG:
    *fptr = printUcharToLong;
    break;
  case DataType::FLOAT:
    *fptr = printUcharToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printUcharToDouble;
    break;
  case DataType::STRING:
    *fptr = printUcharToString;
    break;
  case DataType::BITMASK:
    *fptr = printUcharToBitmask;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::UCHAR 
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setStringPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  setUcharPrintFn(outType, fptr);
}

void ArchiveConvFn::setCharPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printCharToBool;
    break;
  case DataType::UCHAR:
    *fptr = printCharToUchar;
    break;
  case DataType::CHAR:
    *fptr = printCharToChar;
    break;
  case DataType::USHORT:
    *fptr = printCharToUshort;
    break;
  case DataType::SHORT:
    *fptr = printCharToShort;
    break;
  case DataType::UINT:
    *fptr = printCharToUint;
    break;
  case DataType::INT:
    *fptr = printCharToInt;
    break;
  case DataType::ULONG:
    *fptr = printCharToUlong;
    break;
  case DataType::LONG:
    *fptr = printCharToLong;
    break;
  case DataType::FLOAT:
    *fptr = printCharToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printCharToDouble;
    break;
  case DataType::STRING:
    *fptr = printCharToString;
    break;
  case DataType::BITMASK:
    *fptr = printCharToBitmask;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::CHAR 
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setUshortPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printUshortToBool;
    break;
  case DataType::UCHAR:
    *fptr = printUshortToUchar;
    break;
  case DataType::CHAR:
    *fptr = printUshortToChar;
    break;
  case DataType::USHORT:
    *fptr = printUshortToUshort;
    break;
  case DataType::SHORT:
    *fptr = printUshortToShort;
    break;
  case DataType::UINT:
    *fptr = printUshortToUint;
    break;
  case DataType::INT:
    *fptr = printUshortToInt;
    break;
  case DataType::ULONG:
    *fptr = printUshortToUlong;
    break;
  case DataType::LONG:
    *fptr = printUshortToLong;
    break;
  case DataType::FLOAT:
    *fptr = printUshortToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printUshortToDouble;
    break;
  case DataType::BITMASK:
    *fptr = printUshortToBitmask;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::USHORT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setShortPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printShortToBool;
    break;
  case DataType::UCHAR:
    *fptr = printShortToUchar;
    break;
  case DataType::CHAR:
    *fptr = printShortToChar;
    break;
  case DataType::USHORT:
    *fptr = printShortToUshort;
    break;
  case DataType::SHORT:
    *fptr = printShortToShort;
    break;
  case DataType::UINT:
    *fptr = printShortToUint;
    break;
  case DataType::INT:
    *fptr = printShortToInt;
    break;
  case DataType::ULONG:
    *fptr = printShortToUlong;
    break;
  case DataType::LONG:
    *fptr = printShortToLong;
    break;
  case DataType::FLOAT:
    *fptr = printShortToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printShortToDouble;
    break;
  case DataType::BITMASK:
    *fptr = printShortToBitmask;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::SHORT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setUintPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printUintToBool;
    break;
  case DataType::UCHAR:
    *fptr = printUintToUchar;
    break;
  case DataType::CHAR:
    *fptr = printUintToChar;
    break;
  case DataType::USHORT:
    *fptr = printUintToUshort;
    break;
  case DataType::SHORT:
    *fptr = printUintToShort;
    break;
  case DataType::UINT:
    *fptr = printUintToUint;
    break;
  case DataType::INT:
    *fptr = printUintToInt;
    break;
  case DataType::ULONG:
    *fptr = printUintToUlong;
    break;
  case DataType::LONG:
    *fptr = printUintToLong;
    break;
  case DataType::FLOAT:
    *fptr = printUintToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printUintToDouble;
    break;
  case DataType::BITMASK:
    *fptr = printUintToBitmask;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::UINT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setIntPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printIntToUchar;
    break;
  case DataType::UCHAR:
    *fptr = printIntToBool;
    break;
  case DataType::CHAR:
    *fptr = printIntToChar;
    break;
  case DataType::USHORT:
    *fptr = printIntToUshort;
    break;
  case DataType::SHORT:
    *fptr = printIntToShort;
    break;
  case DataType::UINT:
    *fptr = printIntToUint;
    break;
  case DataType::INT:
    *fptr = printIntToInt;
    break;
  case DataType::ULONG:
    *fptr = printIntToUlong;
    break;
  case DataType::LONG:
    *fptr = printIntToLong;
    break;
  case DataType::FLOAT:
    *fptr = printIntToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printIntToDouble;
    break;
  case DataType::BITMASK:
    *fptr = printIntToBitmask;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::INT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setUlongPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printUlongToBool;
    break;
  case DataType::UCHAR:
    *fptr = printUlongToUchar;
    break;
  case DataType::CHAR:
    *fptr = printUlongToChar;
    break;
  case DataType::USHORT:
    *fptr = printUlongToUshort;
    break;
  case DataType::SHORT:
    *fptr = printUlongToShort;
    break;
  case DataType::UINT:
    *fptr = printUlongToUint;
    break;
  case DataType::INT:
    *fptr = printUlongToInt;
    break;
  case DataType::ULONG:
    *fptr = printUlongToUlong;
    break;
  case DataType::LONG:
    *fptr = printUlongToLong;
    break;
  case DataType::FLOAT:
    *fptr = printUlongToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printUlongToDouble;
    break;
  case DataType::BITMASK:
    *fptr = printUlongToBitmask;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::ULONG
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setLongPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printLongToBool;
    break;
  case DataType::UCHAR:
    *fptr = printLongToUchar;
    break;
  case DataType::CHAR:
    *fptr = printLongToChar;
    break;
  case DataType::USHORT:
    *fptr = printLongToUshort;
    break;
  case DataType::SHORT:
    *fptr = printLongToShort;
    break;
  case DataType::UINT:
    *fptr = printLongToUint;
    break;
  case DataType::INT:
    *fptr = printLongToInt;
    break;
  case DataType::ULONG:
    *fptr = printLongToUlong;
    break;
  case DataType::LONG:
    *fptr = printLongToLong;
    break;
  case DataType::FLOAT:
    *fptr = printLongToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printLongToDouble;
    break;
  case DataType::BITMASK:
    *fptr = printLongToBitmask;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::LONG
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setFloatPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printFloatToBool;
    break;
  case DataType::UCHAR:
    *fptr = printFloatToUchar;
    break;
  case DataType::CHAR:
    *fptr = printFloatToChar;
    break;
  case DataType::USHORT:
    *fptr = printFloatToUshort;
    break;
  case DataType::SHORT:
    *fptr = printFloatToShort;
    break;
  case DataType::UINT:
    *fptr = printFloatToUint;
    break;
  case DataType::INT:
    *fptr = printFloatToInt;
    break;
  case DataType::ULONG:
    *fptr = printFloatToUlong;
    break;
  case DataType::LONG:
    *fptr = printFloatToLong;
    break;
  case DataType::FLOAT:
    *fptr = printFloatToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printFloatToDouble;
    break;
  case DataType::BITMASK:
    *fptr = printFloatToBitmask;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::FLOAT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setDoublePrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printDoubleToBool;
    break;
  case DataType::UCHAR:
    *fptr = printDoubleToUchar;
    break;
  case DataType::CHAR:
    *fptr = printDoubleToChar;
    break;
  case DataType::USHORT:
    *fptr = printDoubleToUshort;
    break;
  case DataType::SHORT:
    *fptr = printDoubleToShort;
    break;
  case DataType::UINT:
    *fptr = printDoubleToUint;
    break;
  case DataType::INT:
    *fptr = printDoubleToInt;
    break;
  case DataType::ULONG:
    *fptr = printDoubleToUlong;
    break;
  case DataType::LONG:
    *fptr = printDoubleToLong;
    break;
  case DataType::FLOAT:
    *fptr = printDoubleToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printDoubleToDouble;
    break;
  case DataType::BITMASK:
    *fptr = printDoubleToBitmask;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::DOUBLE
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setComplexFloatPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::COMPLEX_FLOAT:
    *fptr = printComplexFloatToComplexFloat;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::COMPLEX_FLOAT
	       << " to " << outType);
    break;
  }
}

void ArchiveConvFn::setDatePrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr))
{
  switch (outType) {
  case DataType::BOOL:
    *fptr = printDateToBool;
    break;
  case DataType::UCHAR:
    *fptr = printDateToUchar;
    break;
  case DataType::CHAR:
    *fptr = printDateToChar;
    break;
  case DataType::USHORT:
    *fptr = printDateToUshort;
    break;
  case DataType::SHORT:
    *fptr = printDateToShort;
    break;
  case DataType::UINT:
    *fptr = printDateToUint;
    break;
  case DataType::INT:
    *fptr = printDateToInt;
    break;
  case DataType::ULONG:
    *fptr = printDateToUlong;
    break;
  case DataType::LONG:
    *fptr = printDateToLong;
    break;
  case DataType::FLOAT:
    *fptr = printDateToFloat;
    break;
  case DataType::DOUBLE:
    *fptr = printDateToDouble;
    break;
  case DataType::STRING:
    *fptr = printDateToString;
    break;
  case DataType::CARMASTRING:
    *fptr = printDateToCarmaString;
    break;
  case DataType::DATE:
    *fptr = printDateToDouble;
    break;
  default:
    ThrowError("No conversion exists from type " << DataType::DATE
	       << " to " << outType);
    break;
  }
}

