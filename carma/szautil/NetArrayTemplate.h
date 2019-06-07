// $Id: NetArrayTemplate.h,v 1.1 2011/06/08 18:40:13 eml Exp $

#ifndef SZA_UTIL_NETARRAYTEMPLATE_H
#define SZA_UTIL_NETARRAYTEMPLATE_H

/**
 * @file NetArrayTemplate.h
 * 
 * Tagged: Tue Aug 24 16:09:09 PDT 2010
 * 
 * @version: $Revision: 1.1 $, $Date: 2011/06/08 18:40:13 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/NetDat.h"
#include "carma/szautil/DataType.h"

#include "carma/szaarrayutils/arraytemplate.h"

namespace sza {

  namespace array {
    class NetBuf;
  }

  namespace util {

    class NetArrayTemplate : public NetDat {
    public:

      /**
       * Constructor.
       */
      NetArrayTemplate();
      NetArrayTemplate(ArrayTemplate* arrayTemplate);

      /**
       * Destructor.
       */
      virtual ~NetArrayTemplate();

      unsigned size();
      void resize();
      void serialize();
      void deserialize(const unsigned char* bytes);
      void deserialize(const std::vector<unsigned char>& bytes);
      ArrayTemplate* getArrayTemplate();

    public:

      ArrayTemplate* arrayTemplate_;
      bool external_;

      void setTo(ArrayTemplate* arrayTemplate);

      unsigned sizeOf(ArrayTemplate* arrayTemplate);
      unsigned sizeOf(RegTemp& regTemp);
      unsigned sizeOf(RegBoardTemp& boardTemp);
      unsigned sizeOf(RegBlockTemp& blockTemp);

      void serialize(unsigned char*& destPtr, ArrayTemplate* arrayTemplate);
      void serialize(unsigned char*& destPtr, RegTemp& regTemp);
      void serialize(unsigned char*& destPtr, RegBoardTemp& boardTemp);
      void serialize(unsigned char*& destPtr, RegBlockTemp& blockTemp);

      void deserialize(unsigned char*& srcPtr, ArrayTemplate* arrayTemplate);
      void deserialize(unsigned char*& srcPtr, RegTemp& regTemp);
      void deserialize(unsigned char*& srcPtr, RegBoardTemp& boardTemp);
      void deserialize(unsigned char*& srcPtr, RegBlockTemp& blockTemp);

      void initialize(ArrayTemplate*& arrayTemplate);
      void initialize(RegTemp*& regTemps, unsigned nRegTemp);
      void initialize(RegBoardTemp*& boardTemps, unsigned nBoard);
      void initialize(RegBlockTemp*& blockTemps, unsigned nBlock);

      void serialize(unsigned char*& destPtr, bool srcVal);
      void serialize(unsigned char*& destPtr, bool* src, unsigned nEl);
      void serialize(unsigned char*& destPtr, unsigned char srcVal);
      void serialize(unsigned char*& destPtr, unsigned char* src, unsigned nEl);
      void serialize(unsigned char*& destPtr, char srcVal);
      void serialize(unsigned char*& destPtr, char* src, unsigned nEl);
      void serialize(unsigned char*& destPtr, unsigned short srcVal);
      void serialize(unsigned char*& destPtr, unsigned short* srcPtr, unsigned nEl);
      void serialize(unsigned char*& destPtr, short srcVal);
      void serialize(unsigned char*& destPtr, short* srcPtr, unsigned nEl);
      void serialize(unsigned char*& destPtr, unsigned int srcVal);
      void serialize(unsigned char*& destPtr, unsigned int* src, unsigned nEl);
      void serialize(unsigned char*& destPtr, int srcVal);
      void serialize(unsigned char*& destPtr, int* srcPtr, unsigned nEl);
      void serialize(unsigned char*& destPtr, float srcVal);
      void serialize(unsigned char*& destPtr, float* src, unsigned nEl);
      void serialize(unsigned char*& destPtr, double srcVal);
      void serialize(unsigned char*& destPtr, double* src, unsigned nEl);
      void serialize(unsigned char*& destPtr, unsigned char* src, DataType::Type type, unsigned nEl);

      void deserialize(unsigned char*& srcPtr, bool& destVal);
      void deserialize(unsigned char*& srcPtr, bool* dest, unsigned nEl);
      void deserialize(unsigned char*& srcPtr, unsigned char& destVal);
      void deserialize(unsigned char*& srcPtr, unsigned char* dest, unsigned nEl);
      void deserialize(unsigned char*& srcPtr, char& destVal);
      void deserialize(unsigned char*& srcPtr, char* dest, unsigned nEl);
      void deserialize(unsigned char*& srcPtr, unsigned short& destVal);
      void deserialize(unsigned char*& srcPtr, unsigned short* dest, unsigned nEl);
      void deserialize(unsigned char*& srcPtr, short& destVal);
      void deserialize(unsigned char*& srcPtr, short* dest, unsigned nEl);
      void deserialize(unsigned char*& srcPtr, unsigned int& destVal);
      void deserialize(unsigned char*& srcPtr, unsigned int* dest, unsigned nEl);
      void deserialize(unsigned char*& srcPtr, int& destVal);
      void deserialize(unsigned char*& srcPtr, int* dest, unsigned nEl);
      void deserialize(unsigned char*& srcPtr, float& destVal);
      void deserialize(unsigned char*& srcPtr, float* dest, unsigned nEl);
      void deserialize(unsigned char*& srcPtr, double& destVal);
      void deserialize(unsigned char*& srcPtr, double* dest, unsigned nEl);
      void deserialize(unsigned char*& srcPtr, unsigned char* dest, DataType::Type type, unsigned nEl);

    }; // End class NetArrayTemplate

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETARRAYTEMPLATE_H
