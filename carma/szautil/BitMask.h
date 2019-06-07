#ifndef SZA_UTIL_BITMASK_H
#define SZA_UTIL_BITMASK_H

/**
 * @file BitMask.h
 * 
 * Tagged: Tue Mar  6 16:39:02 PST 2012
 * 
 * @version: $Revision: 1.4 $, $Date: 2013/11/19 22:55:28 $
 * 
 * @author username: Erik Leitch
 */
#include <valarray>
#include <iostream>

namespace sza {
  namespace util {

    class BitMask {
    public:

      // Constructor.

      BitMask();

      // Destructor.

      virtual ~BitMask();

      // Resize this bit mask to be at least as large as the requested
      // number of bits

      void resize(unsigned nBit);

      // Set up this object to manage an externally provided byte
      // array

      void setTo(std::valarray<unsigned char>* byteVec);
      void fillFrom(unsigned char* bytePtr, unsigned size);

      // Set the requested bit high

      void setBitHigh(unsigned iBit);
      void setAllBitsHigh();

      // Set the requested bit low

      void setBitLow(unsigned iBit);
      void setAllBitsLow();

      // Return if the requested bit is high or low

      bool bitIsHigh(unsigned iBit);
      bool bitIsLow(unsigned iBit);

      // Return the requested bit value

      unsigned bitVal(unsigned iBit);

      // Return true if all bits are set low

      bool allBitsAreLow();
      bool allBitsAreLowBut(unsigned iBit);

      // Print operator

      friend std::ostream& operator<<(std::ostream& os, BitMask& bitMask);

      // Assignment operators: bm1 = bm2

      void operator=(const BitMask& bitMask);
      void operator=(BitMask& bitMask);

      // Operator for bm1 |= bm2, bm3 = bm1 | bm2

      void operator|=(BitMask& bitMask);
      void operator|=(const BitMask& bitMask);
      BitMask operator|(BitMask& bitMask);
      BitMask operator|(const BitMask& bitMask);

      // Operator for bm1 &= bm2, bm3 = bm1 & bm2

      void operator&=(BitMask& bitMask);
      void operator&=(const BitMask& bitMask);
      BitMask operator&(BitMask& bitMask);
      BitMask operator&(const BitMask& bitMask);

      // Operator for ~bm

      BitMask operator~ (void);

      void operator=(std::string bitMaskStr);

    public:

      void getBitAndByte(unsigned iBit);

      unsigned nBit_;
      unsigned iByte_;
      unsigned iBitInByte_;
      unsigned byteVal_;
      bool internalMemory_;

      std::valarray<unsigned char>* byteVecPtr_;

    }; // End class BitMask

    std::ostream& operator<<(std::ostream& os, BitMask& bitMask);

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_BITMASK_H
