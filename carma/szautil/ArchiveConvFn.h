// $Id: ArchiveConvFn.h,v 1.3 2012/04/05 22:35:51 eml Exp $

#ifndef SZA_UTIL_ARCHIVECONVFN_H
#define SZA_UTIL_ARCHIVECONVFN_H

/**
 * @file ArchiveConvFn.h
 * 
 * Tagged: Sat Jan 31 23:42:25 NZDT 2009
 * 
 * @version: $Revision: 1.3 $, $Date: 2012/04/05 22:35:51 $
 * 
 * @author username: Command not found.
 */

#include "carma/szautil/DataType.h"
#include "carma/szautil/RegCal.h"

#include <iostream>

#define ARC_CONV_FN(fn) void (fn)(void* in, void* reOut, void* imOut, void* args, RegCal::RegCalSlot* cals, unsigned& n, unsigned* inInds, unsigned* outInds, unsigned iAdd)

#define ARC_PRINT_FN(fn) void (fn)(void* in, std::ostringstream& os, void* args, RegCal::RegCalSlot* cals, unsigned& n, int width, int precision)

#define STRING_DISPATCH_FN(fn) void (fn)(const char* carr, void* args, unsigned& ind)

namespace sza {
  namespace util {

    class ArchiveConvFn {
    public:

      static ARC_CONV_FN(boolToBool);
      static ARC_CONV_FN(boolToUchar);
      static ARC_CONV_FN(boolToChar);
      static ARC_CONV_FN(boolToUshort);
      static ARC_CONV_FN(boolToShort);
      static ARC_CONV_FN(boolToUint);
      static ARC_CONV_FN(boolToInt);
      static ARC_CONV_FN(boolToUlong);
      static ARC_CONV_FN(boolToLong);
      static ARC_CONV_FN(boolToFloat);
      static ARC_CONV_FN(boolToDouble);

      static ARC_CONV_FN(ucharToBool);
      static ARC_CONV_FN(ucharToUchar);
      static ARC_CONV_FN(ucharToChar);
      static ARC_CONV_FN(ucharToUshort);
      static ARC_CONV_FN(ucharToShort);
      static ARC_CONV_FN(ucharToUint);
      static ARC_CONV_FN(ucharToInt);
      static ARC_CONV_FN(ucharToUlong);
      static ARC_CONV_FN(ucharToLong);
      static ARC_CONV_FN(ucharToFloat);
      static ARC_CONV_FN(ucharToDouble);
      static ARC_CONV_FN(ucharToString);

      static ARC_CONV_FN(charToBool);
      static ARC_CONV_FN(charToUchar);
      static ARC_CONV_FN(charToChar);
      static ARC_CONV_FN(charToUshort);
      static ARC_CONV_FN(charToShort);
      static ARC_CONV_FN(charToUint);
      static ARC_CONV_FN(charToInt);
      static ARC_CONV_FN(charToUlong);
      static ARC_CONV_FN(charToLong);
      static ARC_CONV_FN(charToFloat);
      static ARC_CONV_FN(charToDouble);
      static ARC_CONV_FN(charToString);

      static ARC_CONV_FN(ushortToBool);
      static ARC_CONV_FN(ushortToUchar);
      static ARC_CONV_FN(ushortToChar);
      static ARC_CONV_FN(ushortToUshort);
      static ARC_CONV_FN(ushortToShort);
      static ARC_CONV_FN(ushortToUint);
      static ARC_CONV_FN(ushortToInt);
      static ARC_CONV_FN(ushortToUlong);
      static ARC_CONV_FN(ushortToLong);
      static ARC_CONV_FN(ushortToFloat);
      static ARC_CONV_FN(ushortToDouble);

      static ARC_CONV_FN(shortToBool);
      static ARC_CONV_FN(shortToUchar);
      static ARC_CONV_FN(shortToChar);
      static ARC_CONV_FN(shortToUshort);
      static ARC_CONV_FN(shortToShort);
      static ARC_CONV_FN(shortToUint);
      static ARC_CONV_FN(shortToInt);
      static ARC_CONV_FN(shortToUlong);
      static ARC_CONV_FN(shortToLong);
      static ARC_CONV_FN(shortToFloat);
      static ARC_CONV_FN(shortToDouble);

      static ARC_CONV_FN(uintToBool);
      static ARC_CONV_FN(uintToUchar);
      static ARC_CONV_FN(uintToChar);
      static ARC_CONV_FN(uintToUshort);
      static ARC_CONV_FN(uintToShort);
      static ARC_CONV_FN(uintToUint);
      static ARC_CONV_FN(uintToInt);
      static ARC_CONV_FN(uintToUlong);
      static ARC_CONV_FN(uintToLong);
      static ARC_CONV_FN(uintToFloat);
      static ARC_CONV_FN(uintToDouble);

      static ARC_CONV_FN(intToBool);
      static ARC_CONV_FN(intToUchar);
      static ARC_CONV_FN(intToChar);
      static ARC_CONV_FN(intToUshort);
      static ARC_CONV_FN(intToShort);
      static ARC_CONV_FN(intToUint);
      static ARC_CONV_FN(intToInt);
      static ARC_CONV_FN(intToUlong);
      static ARC_CONV_FN(intToLong);
      static ARC_CONV_FN(intToFloat);
      static ARC_CONV_FN(intToDouble);

      static ARC_CONV_FN(ulongToBool);
      static ARC_CONV_FN(ulongToUchar);
      static ARC_CONV_FN(ulongToChar);
      static ARC_CONV_FN(ulongToUshort);
      static ARC_CONV_FN(ulongToShort);
      static ARC_CONV_FN(ulongToUint);
      static ARC_CONV_FN(ulongToInt);
      static ARC_CONV_FN(ulongToUlong);
      static ARC_CONV_FN(ulongToLong);
      static ARC_CONV_FN(ulongToFloat);
      static ARC_CONV_FN(ulongToDouble);

      static ARC_CONV_FN(longToBool);
      static ARC_CONV_FN(longToUchar);
      static ARC_CONV_FN(longToChar);
      static ARC_CONV_FN(longToUshort);
      static ARC_CONV_FN(longToShort);
      static ARC_CONV_FN(longToUint);
      static ARC_CONV_FN(longToInt);
      static ARC_CONV_FN(longToUlong);
      static ARC_CONV_FN(longToLong);
      static ARC_CONV_FN(longToFloat);
      static ARC_CONV_FN(longToDouble);

      static ARC_CONV_FN(floatToBool);
      static ARC_CONV_FN(floatToUchar);
      static ARC_CONV_FN(floatToChar);
      static ARC_CONV_FN(floatToUshort);
      static ARC_CONV_FN(floatToShort);
      static ARC_CONV_FN(floatToUint);
      static ARC_CONV_FN(floatToInt);
      static ARC_CONV_FN(floatToUlong);
      static ARC_CONV_FN(floatToLong);
      static ARC_CONV_FN(floatToFloat);
      static ARC_CONV_FN(floatToDouble);

      static ARC_CONV_FN(doubleToBool);
      static ARC_CONV_FN(doubleToUchar);
      static ARC_CONV_FN(doubleToChar);
      static ARC_CONV_FN(doubleToUshort);
      static ARC_CONV_FN(doubleToShort);
      static ARC_CONV_FN(doubleToUint);
      static ARC_CONV_FN(doubleToInt);
      static ARC_CONV_FN(doubleToUlong);
      static ARC_CONV_FN(doubleToLong);
      static ARC_CONV_FN(doubleToFloat);
      static ARC_CONV_FN(doubleToDouble);

      static ARC_CONV_FN(complexFloatToComplexFloat);

      static ARC_CONV_FN(dateToBool);
      static ARC_CONV_FN(dateToUchar);
      static ARC_CONV_FN(dateToChar);
      static ARC_CONV_FN(dateToUshort);
      static ARC_CONV_FN(dateToShort);
      static ARC_CONV_FN(dateToUint);
      static ARC_CONV_FN(dateToInt);
      static ARC_CONV_FN(dateToUlong);
      static ARC_CONV_FN(dateToLong);
      static ARC_CONV_FN(dateToFloat);
      static ARC_CONV_FN(dateToDouble);
      static ARC_CONV_FN(dateToString);
      static ARC_CONV_FN(dateToCarmaString);


      static void setConvFn(DataType::Type inType, DataType::Type outType, 
			    ARC_CONV_FN(**fptr));

      static void setBoolConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setUcharConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setStringConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setCharConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setUshortConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setShortConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setUintConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setIntConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setUlongConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setLongConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setFloatConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setDoubleConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setComplexFloatConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));
      static void setDateConvFn(DataType::Type outType, ARC_CONV_FN(**fptr));

      // A function that will be called to dispatch a string formatted
      // by one of the string conversion methods of this class

      static STRING_DISPATCH_FN(*stringDispatchFn_);
      static void setStringDispatchFn(STRING_DISPATCH_FN(*stringDispatchFn_));

      static ARC_PRINT_FN(printBoolToBool);
      static ARC_PRINT_FN(printBoolToUchar);
      static ARC_PRINT_FN(printBoolToChar);
      static ARC_PRINT_FN(printBoolToUshort);
      static ARC_PRINT_FN(printBoolToShort);
      static ARC_PRINT_FN(printBoolToUint);
      static ARC_PRINT_FN(printBoolToInt);
      static ARC_PRINT_FN(printBoolToUlong);
      static ARC_PRINT_FN(printBoolToLong);
      static ARC_PRINT_FN(printBoolToFloat);
      static ARC_PRINT_FN(printBoolToDouble);
      static ARC_PRINT_FN(printBoolToString);
      static ARC_PRINT_FN(printBoolToBitmask);

      static ARC_PRINT_FN(printUcharToBool);
      static ARC_PRINT_FN(printUcharToUchar);
      static ARC_PRINT_FN(printUcharToChar);
      static ARC_PRINT_FN(printUcharToUshort);
      static ARC_PRINT_FN(printUcharToShort);
      static ARC_PRINT_FN(printUcharToUint);
      static ARC_PRINT_FN(printUcharToInt);
      static ARC_PRINT_FN(printUcharToUlong);
      static ARC_PRINT_FN(printUcharToLong);
      static ARC_PRINT_FN(printUcharToFloat);
      static ARC_PRINT_FN(printUcharToDouble);
      static ARC_PRINT_FN(printUcharToString);
      static ARC_PRINT_FN(printUcharToBitmask);

      static ARC_PRINT_FN(printCharToBool);
      static ARC_PRINT_FN(printCharToUchar);
      static ARC_PRINT_FN(printCharToChar);
      static ARC_PRINT_FN(printCharToUshort);
      static ARC_PRINT_FN(printCharToShort);
      static ARC_PRINT_FN(printCharToUint);
      static ARC_PRINT_FN(printCharToInt);
      static ARC_PRINT_FN(printCharToUlong);
      static ARC_PRINT_FN(printCharToLong);
      static ARC_PRINT_FN(printCharToFloat);
      static ARC_PRINT_FN(printCharToDouble);
      static ARC_PRINT_FN(printCharToString);
      static ARC_PRINT_FN(printCharToBitmask);

      static ARC_PRINT_FN(printUshortToBool);
      static ARC_PRINT_FN(printUshortToUchar);
      static ARC_PRINT_FN(printUshortToChar);
      static ARC_PRINT_FN(printUshortToUshort);
      static ARC_PRINT_FN(printUshortToShort);
      static ARC_PRINT_FN(printUshortToUint);
      static ARC_PRINT_FN(printUshortToInt);
      static ARC_PRINT_FN(printUshortToUlong);
      static ARC_PRINT_FN(printUshortToLong);
      static ARC_PRINT_FN(printUshortToFloat);
      static ARC_PRINT_FN(printUshortToDouble);
      static ARC_PRINT_FN(printUshortToBitmask);

      static ARC_PRINT_FN(printShortToBool);
      static ARC_PRINT_FN(printShortToUchar);
      static ARC_PRINT_FN(printShortToChar);
      static ARC_PRINT_FN(printShortToUshort);
      static ARC_PRINT_FN(printShortToShort);
      static ARC_PRINT_FN(printShortToUint);
      static ARC_PRINT_FN(printShortToInt);
      static ARC_PRINT_FN(printShortToUlong);
      static ARC_PRINT_FN(printShortToLong);
      static ARC_PRINT_FN(printShortToFloat);
      static ARC_PRINT_FN(printShortToDouble);
      static ARC_PRINT_FN(printShortToBitmask);
      static ARC_PRINT_FN(printShortToString);

      static ARC_PRINT_FN(printUintToBool);
      static ARC_PRINT_FN(printUintToUchar);
      static ARC_PRINT_FN(printUintToChar);
      static ARC_PRINT_FN(printUintToUshort);
      static ARC_PRINT_FN(printUintToShort);
      static ARC_PRINT_FN(printUintToUint);
      static ARC_PRINT_FN(printUintToInt);
      static ARC_PRINT_FN(printUintToUlong);
      static ARC_PRINT_FN(printUintToLong);
      static ARC_PRINT_FN(printUintToFloat);
      static ARC_PRINT_FN(printUintToDouble);
      static ARC_PRINT_FN(printUintToBitmask);
      static ARC_PRINT_FN(printUintToString);

      static ARC_PRINT_FN(printIntToBool);
      static ARC_PRINT_FN(printIntToUchar);
      static ARC_PRINT_FN(printIntToChar);
      static ARC_PRINT_FN(printIntToUshort);
      static ARC_PRINT_FN(printIntToShort);
      static ARC_PRINT_FN(printIntToUint);
      static ARC_PRINT_FN(printIntToInt);
      static ARC_PRINT_FN(printIntToUlong);
      static ARC_PRINT_FN(printIntToLong);
      static ARC_PRINT_FN(printIntToFloat);
      static ARC_PRINT_FN(printIntToDouble);
      static ARC_PRINT_FN(printIntToBitmask);
      static ARC_PRINT_FN(printIntToString);

      static ARC_PRINT_FN(printUlongToBool);
      static ARC_PRINT_FN(printUlongToUchar);
      static ARC_PRINT_FN(printUlongToChar);
      static ARC_PRINT_FN(printUlongToUshort);
      static ARC_PRINT_FN(printUlongToShort);
      static ARC_PRINT_FN(printUlongToUint);
      static ARC_PRINT_FN(printUlongToInt);
      static ARC_PRINT_FN(printUlongToUlong);
      static ARC_PRINT_FN(printUlongToLong);
      static ARC_PRINT_FN(printUlongToFloat);
      static ARC_PRINT_FN(printUlongToDouble);
      static ARC_PRINT_FN(printUlongToBitmask);
      static ARC_PRINT_FN(printUlongToString);

      static ARC_PRINT_FN(printLongToBool);
      static ARC_PRINT_FN(printLongToUchar);
      static ARC_PRINT_FN(printLongToChar);
      static ARC_PRINT_FN(printLongToUshort);
      static ARC_PRINT_FN(printLongToShort);
      static ARC_PRINT_FN(printLongToUint);
      static ARC_PRINT_FN(printLongToInt);
      static ARC_PRINT_FN(printLongToUlong);
      static ARC_PRINT_FN(printLongToLong);
      static ARC_PRINT_FN(printLongToFloat);
      static ARC_PRINT_FN(printLongToDouble);
      static ARC_PRINT_FN(printLongToBitmask);
      static ARC_PRINT_FN(printLongToString);

      static ARC_PRINT_FN(printFloatToBool);
      static ARC_PRINT_FN(printFloatToUchar);
      static ARC_PRINT_FN(printFloatToChar);
      static ARC_PRINT_FN(printFloatToUshort);
      static ARC_PRINT_FN(printFloatToShort);
      static ARC_PRINT_FN(printFloatToUint);
      static ARC_PRINT_FN(printFloatToInt);
      static ARC_PRINT_FN(printFloatToUlong);
      static ARC_PRINT_FN(printFloatToLong);
      static ARC_PRINT_FN(printFloatToFloat);
      static ARC_PRINT_FN(printFloatToDouble);
      static ARC_PRINT_FN(printFloatToBitmask);
      static ARC_PRINT_FN(printFloatToString);

      static ARC_PRINT_FN(printDoubleToBool);
      static ARC_PRINT_FN(printDoubleToUchar);
      static ARC_PRINT_FN(printDoubleToChar);
      static ARC_PRINT_FN(printDoubleToUshort);
      static ARC_PRINT_FN(printDoubleToShort);
      static ARC_PRINT_FN(printDoubleToUint);
      static ARC_PRINT_FN(printDoubleToInt);
      static ARC_PRINT_FN(printDoubleToUlong);
      static ARC_PRINT_FN(printDoubleToLong);
      static ARC_PRINT_FN(printDoubleToFloat);
      static ARC_PRINT_FN(printDoubleToDouble);
      static ARC_PRINT_FN(printDoubleToBitmask);
      static ARC_PRINT_FN(printDoubleToString);

      static ARC_PRINT_FN(printComplexFloatToComplexFloat);

      static ARC_PRINT_FN(printDateToBool);
      static ARC_PRINT_FN(printDateToUchar);
      static ARC_PRINT_FN(printDateToChar);
      static ARC_PRINT_FN(printDateToUshort);
      static ARC_PRINT_FN(printDateToShort);
      static ARC_PRINT_FN(printDateToUint);
      static ARC_PRINT_FN(printDateToInt);
      static ARC_PRINT_FN(printDateToUlong);
      static ARC_PRINT_FN(printDateToLong);
      static ARC_PRINT_FN(printDateToFloat);
      static ARC_PRINT_FN(printDateToDouble);
      static ARC_PRINT_FN(printDateToString);
      static ARC_PRINT_FN(printDateToCarmaString);

      static void setPrintFn(DataType::Type inType, DataType::Type outType, 
			     ARC_PRINT_FN(**fptr));

      static void setBoolPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setUcharPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setStringPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setCharPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setUshortPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setShortPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setUintPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setIntPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setUlongPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setLongPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setFloatPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setDoublePrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setComplexFloatPrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));
      static void setDatePrintFn(DataType::Type outType, ARC_PRINT_FN(**fptr));

      virtual ~ArchiveConvFn();

    private:

      ArchiveConvFn();

    }; // End class ArchiveConvFn

  } // End namespace util
} // End namespace sza


#endif // End #ifndef SZA_UTIL_ARCHIVECONVFN_H
