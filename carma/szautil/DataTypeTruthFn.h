#ifndef SZA_UTIL_DATATYPETRUTHFN_H
#define SZA_UTIL_DATATYPETRUTHFN_H

/**
 * @file DataTypeTruthFn.h
 * 
 * Tagged: Mon Oct 25 18:41:34 PDT 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/DataType.h"

#define DATATYPE_TRUTH_FN0(fn) bool (fn)(DataType& compVal)
#define DATATYPE_TRUTH_FN1(fn) bool (fn)(DataType& compVal, DataType& op1)
#define DATATYPE_TRUTH_FN2(fn) bool (fn)(DataType& compVal, DataType& op1, DataType& op2)

namespace sza {
  namespace util {
    
    /**
     * A class for managing operations on DataType objects which
     * return booleans
     */
    class DataTypeTruthFn {
    public:
      
      enum Type {
	FN0 = 0x0,
	FN1 = 0x1,
	FN2 = 0x2
      };

      /**
       * Constructor.
       */
      DataTypeTruthFn(DATATYPE_TRUTH_FN0(*fn)=0);
      DataTypeTruthFn(DATATYPE_TRUTH_FN1(*fn));
      DataTypeTruthFn(DATATYPE_TRUTH_FN2(*fn));
      
      /**
       * Destructor.
       */
      virtual ~DataTypeTruthFn();
      
      bool evaluate(DataType& compVal);
      bool evaluate(DataType& compVal, DataType& op1);
      bool evaluate(DataType& compVal, DataType& op1, DataType& op2);

      static bool alwaysTrue(                DataType& compVal);

      static bool equals(                    DataType& compVal, DataType& op1);
      static bool greaterThan(               DataType& compVal, DataType& op1);
      static bool lessThan(                  DataType& compVal, DataType& op1);
      static bool greaterThanEq(             DataType& compVal, DataType& op1);
      static bool lessThanEq(                DataType& compVal, DataType& op1);

      static bool greaterThanAndLessThan(    DataType& compVal, DataType& op1, DataType& op2);
      static bool greaterThanAndLessThanEq(  DataType& compVal, DataType& op1, DataType& op2);
      static bool greaterThanEqAndLessThan(  DataType& compVal, DataType& op1, DataType& op2);
      static bool greaterThanEqAndLessThanEq(DataType& compVal, DataType& op1, DataType& op2);
      static bool lessThanOrGreaterThan(     DataType& compVal, DataType& op1, DataType& op2);
      static bool lessThanEqOrGreaterThan(   DataType& compVal, DataType& op1, DataType& op2);
      static bool lessThanOrGreaterThanEq(   DataType& compVal, DataType& op1, DataType& op2);
      static bool lessThanEqOrGreaterThanEq( DataType& compVal, DataType& op1, DataType& op2);

      bool isZeroValued();
      bool isSingleValued();
      bool isDualValued();

      Type type() {
	return type_;
      }

      std::string format(std::string& reg, DataType& op1, DataType& op2);

    private:

      union {
	DATATYPE_TRUTH_FN0(*fn0_);
	DATATYPE_TRUTH_FN1(*fn1_);
	DATATYPE_TRUTH_FN2(*fn2_);
      } fn_;

      Type type_;

      void checkType(Type);

      // Methods for formatting an outputted string

      std::string formatZeroValuedFn(std::string& reg);
      std::string formatSingleValuedFn (std::string& reg, DataType& op1_);
      std::string formatDualValuedFn(std::string& reg, DataType& op1_, DataType& op2_);

      std::string formatOp(DATATYPE_TRUTH_FN1(*fn_));
      std::string formatOp1(DATATYPE_TRUTH_FN2(*fn_));
      std::string formatOp2(DATATYPE_TRUTH_FN2(*fn_));
      std::string formatLogical(DATATYPE_TRUTH_FN2(*fn_));

      std::string formatVal(DataType& val);
      std::string formatReg(std::string& reg);

    }; // End class DataTypeTruthFn
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_DATATYPETRUTHFN_H
