#include <sstream>

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/DataArray.h"

using namespace sza::util;

/**
 * Prevents instantiation (singleton class)
 */
DataArray::DataArray() { }

/**
 * Create the singleton instance
 */
DataArray  DataArray::dataArray_;

/**.......................................................................
 * Pack data from an array of unsigned longs to arrays of different
 * types using different addressing modes.
 */
void DataArray::pack(RegAddrMode addrMode, unsigned int flags,
		     unsigned int* destination, unsigned int* source, 
		     unsigned short first, unsigned short nreg)
{
  unsigned int ireg;  // The loop index over register elements being written 

  // Handle the different register sizes. Note that the memory-mapped
  // addresses of these registers have been pre-calculated in
  // 'destination' so there is no need to treat the different
  // address formats separately here when writeing registers. Only the
  // different data sizes and signednesses need to be treated
  // individually.
  //
  // Also note that signed registers that are narrower than the elements
  // of the output buffer have to be sign-extended. This simply involves
  // accessing the register through a pointer with the same signedness
  // as the register, so that the appropriate conversion gets applied.
  // This requires different code for signed and unsigned registers.

  switch(addrMode) {

    // Write 8-bit registers from the 'nreg' odd addresses that follow
    // the first element requested. Note that destination specifies
    // the initial odd address of the register block.

  case ADDR_A16D08_O:
  case ADDR_A24D08_O:
  case ADDR_A32D08_O:
    if(flags & REG_CHAR) {
      volatile signed char *d8 = (signed char *) destination + first * 2;
      for(ireg=0; ireg < nreg; ireg++,d8+=2)
	*d8 = *(signed *) source++;
    } else {
      volatile unsigned char *d8 = (unsigned char *) destination + first*2;
      for(ireg=0; ireg < nreg; ireg++,d8+=2)
	*d8 = *(unsigned *) source++;
    };
    break;

    // Write 8-bit registers from both odd and even addresses.

  case ADDR_A16D08_EO:
  case ADDR_A24D08_EO:
  case ADDR_A32D08_EO:
    if(flags & REG_CHAR) {
      volatile signed char *d8 = (signed char *) destination + first;
      for(ireg=0; ireg < nreg; ireg++)
	*d8++ = *(signed *) source++;
    } else {
      volatile unsigned char *d8 = (unsigned char *) destination + first;
      for(ireg=0; ireg < nreg; ireg++)
	*d8++ = *(unsigned *) source++;
    };
    break;

    // Write 16-bit VME registers.

  case ADDR_A16D16:
  case ADDR_A24D16:
  case ADDR_A32D16:
    if(flags & REG_SHORT) {
      volatile signed short *d16 = (signed short *) destination + first;
      for(ireg=0; ireg < nreg; ireg++)
	*d16++ = *(signed *) source++;
    } else {
      volatile unsigned short *d16 = (unsigned short *) destination + first;
      for(ireg=0; ireg < nreg; ireg++)
	*d16++ = *(unsigned *) source++;
    }
    break;

    // Write 32-bit VME registers. Note that since the VME registers
    // have the same size as the elements of the output buffer, there
    // is no need to sign-extend signed numbers, so there is no need
    // to have different cases for signed and unsigned registers.

  case ADDR_A16D32:
  case ADDR_A24D32:
  case ADDR_A32D32:
  case ADDR_A16D16_L:
  case ADDR_A24D16_L:
  case ADDR_A32D16_L:
  case ADDR_DEFAULT:
    {
      volatile unsigned int *d32 = (unsigned int *) destination + first;
      for(ireg=0; ireg < nreg; ireg++)
	*d32++ = *(unsigned *) source++;
    };
    break;
  default:
    {
      LogStream logStr;
      logStr.setMessage(true, "Unrecognized address mode: ");
      logStr << addrMode;
      throw Error(logStr);
    };
  };
}

/**.......................................................................
 * Unpack data from an array of different types into an array of
 * unsigned longsm using different addressing modes.
 */
void DataArray::unpack(RegAddrMode addrMode, unsigned int flags,
		     unsigned int* destination, unsigned int* source, 
		     unsigned short first, unsigned short nreg)
{
  unsigned short ireg;
  
  // Handle the different register sizes. We assume source points to a
  // valid memory mapped address, so there is no need to treat the
  // different address formats separately here when reading
  // registers. Only the different data sizes and signednesses need to
  // be treated individually.
  //
  // Also note that signed registers that are narrower than the
  // regs of the output buffer have to be sign-extended. This
  // simply involves accessing the register through a pointer with the
  // same signedness as the register, so that the appropriate
  // conversion gets applied.  This requires different code for signed
  // and unsigned registers.
  
  switch(addrMode) {
    
    // Read 8-bit registers from the 'nreg' odd addresses that follow
    // the first reg requested. Note that source specifies
    // the initial odd address of the register block.
    
  case ADDR_A16D08_O:
  case ADDR_A24D08_O:
  case ADDR_A32D08_O:
    if(flags & REG_CHAR) {
      volatile signed char *d8 = (signed char*) source + first * 2;
      for(ireg=0; ireg < nreg; ireg++,d8+=2)
	*(signed *) destination++ = *d8;
    } else {
      volatile unsigned char *d8 = (unsigned char*) source + first*2;
      for(ireg=0; ireg < nreg; ireg++,d8+=2)
	*(unsigned *) destination++ = *d8;
    };
    break;
    
    // Read 8-bit registers from both odd and even addresses.
    
  case ADDR_A16D08_EO:
  case ADDR_A24D08_EO:
  case ADDR_A32D08_EO:
    if(flags & REG_CHAR) {
      volatile signed char *d8 = (signed char *) source + first;
      for(ireg=0; ireg < nreg; ireg++)
	*(signed *) destination++ = *d8++;
    } else {
      volatile unsigned char *d8 = (unsigned char *) source + first;
      for(ireg=0; ireg < nreg; ireg++)
	*(unsigned *) destination++ = *d8++;
    };
    break;
    
    // Read 16-bit registers.
    
  case ADDR_A16D16:
  case ADDR_A24D16:
  case ADDR_A32D16:
    if(flags & REG_SHORT) {
      volatile signed short *d16 = (signed short *) source + first;
      for(ireg=0; ireg < nreg; ireg++)
	*(signed *) destination++ = *d16++;
    } else {
      volatile unsigned short *d16 = (unsigned short *) source + first;
      for(ireg=0; ireg < nreg; ireg++)
	*(unsigned *) destination++ = *d16++;
    }
    break;
    
    // Read 32-bit registers. Note that since these registers have the
    // same size as the elements of the output buffer, there is no
    // need to sign-extend signed numbers, so there is no need to have
    // different cases for signed and unsigned registers.
    
  case ADDR_A16D32:
  case ADDR_A24D32:
  case ADDR_A32D32:
  case ADDR_A16D16_L:
  case ADDR_A24D16_L:
  case ADDR_A32D16_L:
  case ADDR_DEFAULT:
    {
      volatile unsigned int *d32 = (unsigned int *) source + first;
      for(ireg=0; ireg < nreg; ireg++)
	*(unsigned *) destination++ = *d32++;
    };
    break;
  default:
    {
      LogStream logStr;
      logStr.setMessage(true, "Unrecognized address mode: ");
      logStr << addrMode;
      throw Error(logStr);
    };
  }
}

/**.......................................................................
 * Return the length, in bytes of the requested packing operation.
 */
unsigned short DataArray::byteLength(RegAddrMode addrMode, unsigned short nreg)
{
  // Handle the different register sizes. We assume source points to a
  // valid memory mapped address, so there is no need to treat the
  // different address formats separately here when reading
  // registers. Only the different data sizes and signednesses need to
  // be treated individually.
  //
  // Also note that signed registers that are narrower than the
  // regs of the output buffer have to be sign-extended. This
  // simply involves accessing the register through a pointer with the
  // same signedness as the register, so that the appropriate
  // conversion gets applied.  This requires different code for signed
  // and unsigned registers.
  
  switch(addrMode) {
    
    // Read 8-bit registers from the 'nreg' odd addresses that follow
    // the first reg requested. Note that source specifies
    // the initial odd address of the register block.
    
  case ADDR_A16D08_O:
  case ADDR_A24D08_O:
  case ADDR_A32D08_O:
  case ADDR_A16D08_EO:
  case ADDR_A24D08_EO:
  case ADDR_A32D08_EO:
    return nreg;
    break;
    
    // Read 16-bit registers.
    
  case ADDR_A16D16:
  case ADDR_A24D16:
  case ADDR_A32D16:
    return nreg*2;
    break;
    
    // Read 32-bit registers. Note that since these registers have the
    // same size as the elements of the output buffer, there is no
    // need to sign-extend signed numbers, so there is no need to have
    // different cases for signed and unsigned registers.
    
  case ADDR_A16D32:
  case ADDR_A24D32:
  case ADDR_A32D32:
  case ADDR_A16D16_L:
  case ADDR_A24D16_L:
  case ADDR_A32D16_L:
  case ADDR_DEFAULT:
    return nreg*4;
    break;
  default:
    {
      LogStream logStr;
      logStr.setMessage(true, "Unrecognized address mode: ");
      logStr << addrMode;
      throw Error(logStr);
    };
  }
}

