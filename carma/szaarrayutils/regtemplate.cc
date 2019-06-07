#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "carma/szaarrayutils/arraymaprev.h"
#include "carma/szaarrayutils/regtemplate.h"
#include "carma/szaarrayutils/lprintf.h"

#include "carma/szautil/RegDate.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace std;
using namespace sza::array;
using namespace sza::util;

/*.......................................................................
 * Pack a register map template into a network buffer.
 *
 * Input:
 *  regtmp  RegTemplate *  The register map template to be packed.
 * Input/Output:
 *  net          NetBuf *  The network buffer in which to pack the
 *                         template. Note that it is the callers
 *                         responsibility to call net_start_put() and
 *                         net_end_put().
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */
int net_put_RegTemplate(RegTemplate *regtmp, NetBuf *net)
{
  int board;   // The index of the board being packed 
  int block;   // The index of the block being packed 
  int i;
  
  // Check arguments.

  if(!regtmp || !net) {
    lprintf(stderr, "net_put_RegTemplate: NULL %s argument.\n",
	    !regtmp ? "regtmp" : "net");
    return 1;
  };

  // Record the count of the number of boards.

  unsigned short nboard = regtmp->nboard;
  if(net_put_short(net, 1, &nboard))
    return 1;
  
  // Pack each board.

  for(board=0; board < (int)regtmp->nboard; board++) {
    RegBoardTemp* brd       = regtmp->boards + board;
    unsigned short name_len = strlen(brd->name_);
    unsigned short nblock   = brd->nblock_;
    
    // Record the name and size of the board.

    if(net_put_short(net, 1, &name_len) ||
       net_put_char(net, name_len, (unsigned char* )brd->name_) ||
       net_put_short(net, 1, &nblock))
      return 1;

    // Pack each block description.

    for(block=0; block < nblock; block++) {
      RegBlockTemp* blk = brd->blocks_ + block;
      
      // Assemble details of the block into appropriate datatypes for
      // packing.

      unsigned short name_len = strlen(blk->name_);
      unsigned long ltmp1[3];
      unsigned long ltmp2[3];

      ltmp1[0] = blk->flags_;
      ltmp1[1] = blk->addr_mode_;
      ltmp1[2] = blk->base_;

      ltmp2[0] = blk->address_;
      ltmp2[1] = blk->axes_->nEl(0);
      ltmp2[2] = blk->axes_->nEl(1);
      
      // Pack the details into the network buffer.

      if(net_put_short(net, 1, &name_len) ||
	 net_put_char(net, name_len, (unsigned char* )blk->name_) ||
	 net_put_long(net, 3, ltmp1) ||
	 net_put_long(net, 3, ltmp2))
	return 1;

      //------------------------------------------------------------
      // New as of revision 2, we pack the units string as well
      //------------------------------------------------------------

      if(ARRAYMAP_REVISION >= 2) {
	unsigned short units_len = blk->carmaUnits_->size();
	unsigned char* unitsPtr  = (unsigned char*)&(blk->carmaUnits_->at(0));
	
	// Pack the units length and string

	if(net_put_short(net, 1, &units_len) ||
	   net_put_char(net, units_len, unitsPtr))
	  return 1;

	// And pack the validity index

	if(net_put_int(net, 1, (unsigned int*)&blk->carmaValidityBitIndex_))
	  return 1;
      }

    }
    
    // Pack the array of VME base addresses.

    unsigned long bases_[NBASE];

    for(i=0; i<NBASE; i++)
      bases_[i] = brd->bases_[i];

    if(net_put_long(net, NBASE, bases_))
      return 1;

  }
  return 0;
}

/*.......................................................................
 * Unpack a register map template from a network buffer.
 *
 * Input:
 *  net          NetBuf *  The network buffer from which to unpack the
 *                         template. Note that it is the callers
 *                         responsibility to call net_start_get() and
 *                         net_end_get().
 * Output:
 *  return  RegTemplate *  The unpacked register map template, or NULL
 *                         on error. This can be deleted via a call to
 *                         del_RegTemplate().
 */
RegTemplate *net_get_RegTemplate(NetBuf *net, unsigned long arraymap_revision)
{
  RegTemplate *regtmp;   // The new template 
  unsigned board;        // The index of the board being processed 
  unsigned block;        // The index of the block being processed 
  int i;
  
  // Check arguments.

  if(!net) {
    lprintf(stderr, "net_get_RegTemplate: NULL net argument.\n");
    return NULL;
  };
  
  // Allocate the container of the template.

  regtmp = (RegTemplate *) malloc(sizeof(RegTemplate));

  if(!regtmp) {
    lprintf(stderr, "net_get_RegTemplate: Insufficient memory.\n");
    return NULL;
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_RegTemplate().

  regtmp->boards = NULL;
  regtmp->nboard = 0;
  regtmp->comment_[0] = '\0';

  // Unpack the count of the number of boards.

  unsigned short nboard;
  if(net_get_short(net, 1, &nboard))
    return del_RegTemplate(regtmp);

  regtmp->nboard = nboard;

  if(nboard < 1) {
    lprintf(stderr, "net_get_RegTemplate: nboard <= 0.\n");
    return del_RegTemplate(regtmp);
  };

  // Allocate the array of boards.

  regtmp->boards = (RegBoardTemp *) malloc(sizeof(RegBoardTemp)*regtmp->nboard);
  if(!regtmp->boards) {
    lprintf(stderr, "net_get_RegTemplate: Insufficient memory.\n");
    return del_RegTemplate(regtmp);
  };
  
  // Initialize the array.

  for(board=0; board<regtmp->nboard; board++) {
    RegBoardTemp *brd = regtmp->boards + board;
    brd->name_[0] = '\0';
    brd->blocks_ = NULL;
    brd->nblock_ = 0;
    brd->comment_[0] = '\0';
    for(i=0; i < NBASE; i++)
      brd->bases_[i] = 0;
  };
  
  // Un-pack each board.

  for(board=0; board<regtmp->nboard; board++) {
    RegBoardTemp* brd = regtmp->boards + board;
    unsigned short name_len;
    unsigned short nblock;
    
    // Un-pack the name and size of the board.

    if(net_get_short(net, 1, &name_len))
      return del_RegTemplate(regtmp);

    if(name_len > REG_NAME_LEN) {
      lprintf(stderr, "net_get_RegTemplate: Board name too long.\n");
      return del_RegTemplate(regtmp);
    };

    if(net_get_char(net, name_len, (unsigned char* )brd->name_) ||
       net_get_short(net, 1, &nblock))
      return del_RegTemplate(regtmp);

    brd->name_[name_len] = '\0';
    brd->nblock_ = nblock;

    if(brd->nblock_ < 1) {
      lprintf(stderr, "net_get_RegTemplate: nblock <= 0.\n");
      return del_RegTemplate(regtmp);
    };
    
    // Allocate the array of blocks.

    brd->blocks_ = (RegBlockTemp *) malloc(sizeof(RegBlockTemp) * brd->nblock_);
    if(!brd->blocks_) {
      lprintf(stderr, "net_get_RegTemplate: Insufficient memory.\n");
      return del_RegTemplate(regtmp);
    };
    
    // Initialize the array.

    for(block=0; block < (unsigned)brd->nblock_; block++) {
      RegBlockTemp* blk = brd->blocks_ + block;
      blk->name_[0]      = '\0';
      blk->flags_        =  0;
      blk->addr_mode_    = ADDR_DEFAULT;
      blk->base_         = REG_BASE0;
      blk->address_      = 0;
      blk->axes_         = 0;
      blk->comment_      = 0;
      blk->carmaUnits_   = 0;
      blk->carmaErrors_  = 0;
      blk->carmaValidityBitIndex_ = -1;

      blk->comment_      = new std::string("");
      blk->carmaUnits_   = new std::string("");
      blk->carmaErrors_  = new std::vector<pair<std::string, std::string> >;
    };
    
    // Un-pack each block description into the newly allocate array.

    for(block=0; block < (unsigned)brd->nblock_; block++) {
      RegBlockTemp *blk = brd->blocks_ + block;
      
      // Assemble details of the block into appropriate datatypes for
      // packing.

      unsigned short name_len;
      unsigned long ltmp1[3];
      unsigned long ltmp2[3];
      
      // Un-pack the details of the latest block.

      if(net_get_short(net, 1, &name_len))
	return del_RegTemplate(regtmp);

      if(name_len > REG_NAME_LEN) {
	lprintf(stderr, "net_get_RegTemplate: Block name too long.\n");
	return del_RegTemplate(regtmp);
      };

      if(net_get_char(net, name_len, (unsigned char* )blk->name_) ||
	 net_get_long(net, 3, ltmp1) ||
	 net_get_long(net, 3, ltmp2))
	return del_RegTemplate(regtmp);

      // Record the details.

      blk->name_[name_len] = '\0';
      blk->flags_     = ltmp1[0];
      blk->addr_mode_ = (RegAddrMode)ltmp1[1];
      blk->base_      = (RegBase)ltmp1[2];
      blk->address_   = ltmp2[0];
      blk->axes_      = new CoordAxes(ltmp2[1], ltmp2[2]);

      //      COUT("Read block = " << brd->name_ << "." << blk->name_);

      if(blk->nEl() < 1) {
	lprintf(stderr, "net_get_RegTemplate: nreg <= 0.\n");
	return del_RegTemplate(regtmp);
      };

      //------------------------------------------------------------
      // New as of revision 2, read the units string as well
      //------------------------------------------------------------

      if(arraymap_revision >= 2) {
	unsigned short units_len = blk->carmaUnits_->size();
	
	if(net_get_short(net, 1, &units_len))
	  return del_RegTemplate(regtmp);
	
	blk->carmaUnits_->resize(units_len);
	unsigned char* unitsPtr  = (unsigned char*)&(blk->carmaUnits_->at(0));
	
	if(net_get_char(net, units_len, unitsPtr))
	  return del_RegTemplate(regtmp);

	// Finally, get the validity index

	if(net_get_int(net, 1, (unsigned int*)&blk->carmaValidityBitIndex_))
	  return del_RegTemplate(regtmp);
      }

    }
    
    // Unpack the array of base addresses.

    unsigned long bases[NBASE];

    if(net_get_long(net, NBASE, bases))
      return del_RegTemplate(regtmp);

    for(i=0; i < NBASE; i++)
      brd->bases_[i] = bases[i];

  }

  return regtmp;
}

/*.......................................................................
 * Delete a register template returned previously by net_get_RegTemplate().
 *
 * Input:
 *  regtmp   RegTemplate *   The template to be deleted.
 * Output:
 *  return   RegTemplate *   The deleted template (ie. NULL).
 */
RegTemplate *del_RegTemplate(RegTemplate *regtmp)
{
  if(regtmp) {
    if(regtmp->boards) {
      for(unsigned i=0; i < regtmp->nboard; i++) {
	RegBoardTemp* board = regtmp->boards + i;
	if(board->blocks_) {

	  // We must properly destruct the CoordAxes pointer or the
	  // memory won't be freed

	  for(unsigned iblk=0; iblk < board->nblock_; iblk++) {
	    RegBlockTemp *block = board->blocks_ + iblk;

	    if(block->axes_ != 0)
	      delete block->axes_;

	    if(block->carmaUnits_ != 0)
	      delete block->carmaUnits_;

	    if(block->carmaErrors_ != 0)
	      delete block->carmaErrors_;
	  }

	  free(board->blocks_);
	}
      };
      free(regtmp->boards);
    };
    free(regtmp);
  };
  return NULL;
}

/*.......................................................................
 * Return the space needed to pack a given register-map template
 * into a NetBuf network buffer.
 *
 * Input:
 *  regtmp   RegTemplate *  The set to be characterized.
 * Output:
 *  return     long    The number of bytes needed to pack a full
 *                     set of register ranges.
 */
long net_RegTemplate_size(RegTemplate *regtmp)
{
  unsigned board;  // The index of the board being processed 
  int block;  // The index of the block being processed 
  long size = 0;   // The byte-count to be returned 

  // Reserve space for the count of the number of register boards.

  size += NET_SHORT_SIZE;
  
  // Reserve space for the details of each VME board.

  for(board=0; board<regtmp->nboard; board++) {
    RegBoardTemp *brd = regtmp->boards + board;
    size += NET_SHORT_SIZE +                  // name_len 
      NET_CHAR_SIZE * strlen(brd->name_) +    // name_[] 
      NET_SHORT_SIZE;                         // nblock 
    
    // Reserve space for the details of each VME register block on the
    // board.

    for(block=0; block < brd->nblock_; block++) {
      RegBlockTemp* blk = brd->blocks_ + block;
      size += NET_SHORT_SIZE +                 // name_len 
	NET_CHAR_SIZE * strlen(blk->name_) +   // name_[] 
	NET_LONG_SIZE * 3 +                   // flags_, addr_mode_, base_ 
	NET_LONG_SIZE * 3;                     // address, nel1, nel2

      //------------------------------------------------------------
      // New as of revision 2, we pack the units string as well, and
      // the CARMA validity index
      //------------------------------------------------------------

      if(ARRAYMAP_REVISION >= 2) {
	size += NET_SHORT_SIZE;
	size += NET_CHAR_SIZE * blk->carmaUnits_->size();

	size += NET_INT_SIZE; // carmaValidityBitIndex_
      }

    };
    
    // Reserve space for the array of board addresses.

    size += NET_LONG_SIZE * NBASE;
  };

  return size;
}

/*.......................................................................
 * Retrieve a register map template from a network buffer and convert
 * it to a register map.
 *
 * Input:
 *  net    NetBuf *  The network buffer from which to unpack the register
 *                   map. It is left to the caller to call
 *                   net_start_get() and net_end_get().
 * Output:
 *  return RegMap *  The register map, or NULL on error.
 */
RegMap *net_get_RegMap(NetBuf *net, unsigned arraymap_revision)
{
  RegTemplate* rt=0;      // The template of the register map 
  RegMap*      regmap=0;  // The corresponding register map 
  
  // Retrieve the register template from the network buffer.

  rt = net_get_RegTemplate(net, arraymap_revision);

  if(!rt)
    return NULL;
  
  // Construct the register map from the template.

  regmap = new RegMap(rt);
  
  // Discard the redundant template.

  rt = del_RegTemplate(rt);
  
  // Return the register map (this will be NULL if new_RegMap()
  // failed).

  return regmap;
}

//=======================================================================
// Methods of the RegBlockTemp struct
//=======================================================================

/**.......................................................................
 * Return the total size in bytes of a register
 */
RegBlockTemp::RegBlockTemp(std::string comment, std::string name, 
			   unsigned flags, unsigned address, 
			   unsigned nel0,  unsigned nel1, unsigned nel2, 
			   std::string carmaUnits, 
			   std::vector<std::pair<std::string, std::string> >& errPairs)
{
  initialize();
  setTo(comment, name, flags, address, nel0, nel1, nel2, carmaUnits, errPairs);
}

/**.......................................................................
 * Intialize internals of this object
 */
void RegBlockTemp::initialize()
{
  flags_   = 0;
  address_ = 0;

  // Set the addressing mode to the default
  
  addr_mode_ = ADDR_DEFAULT;

  // Set the base address to the default

  base_ = REG_BASE0;  

  // Initialize pointer variables

  axes_                  =  0;
  comment_               =  0;
  carmaUnits_            =  0;
  carmaErrors_           =  0;
  carmaValidityBitIndex_ = -1;
}

void RegBlockTemp::setTo(std::string comment, std::string name, 
			 unsigned flags, unsigned address, 
			 unsigned nel0,  unsigned nel1, unsigned nel2, 
			 std::string carmaUnits, 
			 std::vector<std::pair<std::string, std::string> >& errPairs,
			 int carmaValidityBitIndex)
{
  flags_   = flags;
  address_ = address;

  // Check that the string is not too long

  if(name.size() > REG_NAME_LEN) {
    ThrowError("Register name is too long: " << name);
  }

  // And copy the string

  strcpy(name_, name.c_str());

  // Store the comment

  comment_      = 0;
  carmaUnits_   = 0;
  carmaErrors_  = 0;

  if(comment_) {
    delete comment_;
    comment_ = 0;
  }

  comment_      = new std::string(comment);

  if(carmaUnits_) {
    delete carmaUnits_;
    carmaUnits_ = 0;
  }

  carmaUnits_   = new std::string(carmaUnits);

  if(carmaErrors_) {
    delete carmaErrors_;
    carmaErrors_ = 0;
  }

  carmaErrors_  = new std::vector<pair<std::string, std::string> >;

  for(unsigned i=0; i < errPairs.size(); i++) {
    carmaErrors_->push_back(errPairs[i]);
  }

  // Set the addressing mode to the default
  
  addr_mode_ = ADDR_DEFAULT;

  // Set the base address to the default

  base_ = REG_BASE0;  

  // Construct the axis specifier

  if(axes_) {
    delete axes_;
    axes_ = 0;
  }

  axes_ = new CoordAxes(nel0, nel1, nel2);

  carmaValidityBitIndex_ = carmaValidityBitIndex;
}

/**.......................................................................
 * Return the total size in bytes of a register
 */
RegBlockTemp::RegBlockTemp(std::string comment, std::string name, 
			   unsigned flags, unsigned address, 
			   unsigned nel0,  unsigned nel1, unsigned nel2, 
			   std::string carmaUnits, 
			   std::string carmaErrLow1, std::string carmaErrHigh1,
			   std::string carmaErrLow2, std::string carmaErrHigh2,
			   std::string carmaErrLow3, std::string carmaErrHigh3,
			   int carmaValidityBitIndex)
{
  std::vector<std::pair<std::string, std::string> > errPairs;
  errPairs.push_back(std::pair<std::string, std::string>(carmaErrLow1, carmaErrHigh1));
  errPairs.push_back(std::pair<std::string, std::string>(carmaErrLow2, carmaErrHigh2));
  errPairs.push_back(std::pair<std::string, std::string>(carmaErrLow3, carmaErrHigh3));

  initialize();
  setTo(comment, name, flags, address, nel0, nel1, nel2, carmaUnits, errPairs, carmaValidityBitIndex);
}

RegBlockTemp::RegBlockTemp(const RegBlockTemp& reg)
{
  initialize();
  *this = reg;
}

RegBlockTemp::RegBlockTemp(RegBlockTemp& reg)
{
  initialize();
  *this = reg;
}

void RegBlockTemp::operator=(const RegBlockTemp& reg)
{
  *this = (RegBlockTemp&)reg;
}

void RegBlockTemp::operator=(RegBlockTemp& reg)
{
  if(axes_) {
    delete axes_;
  }

  axes_ = 0;

  if(comment_) {
    delete comment_;
  }

  comment_      = 0;

  if(carmaUnits_) {
    delete carmaUnits_;
  }

  carmaUnits_ = 0;

  if(carmaErrors_) {
    delete carmaErrors_;
  }

  carmaErrors_ = 0;

  // Now copy pertinent members

  strcpy(name_, reg.name_);
  flags_      = reg.flags_;
  address_    = reg.address_;
  addr_mode_  = reg.addr_mode_;
  base_       = reg.base_;

  // Store the comment

  comment_      = new std::string(*reg.comment_);
  carmaUnits_   = new std::string(*reg.carmaUnits_);
  carmaErrors_  = new std::vector<pair<std::string, std::string> >;

  carmaValidityBitIndex_ = reg.carmaValidityBitIndex_;

  // Copy the error assignments

  std::vector<std::pair<std::string, std::string> >& errPairs = *reg.carmaErrors_;

  for(unsigned i=0; i < errPairs.size(); i++) {
    carmaErrors_->push_back(errPairs[i]);
  }

  // Finally, copy the axes descriptor

  axes_      = new CoordAxes(reg.axes_->nEl(0), reg.axes_->nEl(1), reg.axes_->nEl(2));
}

/**.......................................................................
 * Destructor
 */
RegBlockTemp::~RegBlockTemp()
{
  if(axes_ != 0) {
    delete axes_;
    axes_ = 0;
  }

  if(comment_ != 0) {
    delete comment_;
    comment_ = 0;
  }

  if(carmaUnits_ != 0) {
    delete carmaUnits_;
    carmaUnits_ = 0;
  }

  if(carmaErrors_ != 0) {
    delete carmaErrors_;
    carmaErrors_ = 0;
  }
}

/**.......................................................................
 * Return the total number of register elements
 */
unsigned RegBlockTemp::nEl()
{
  return axes_->nEl();
}

/**.......................................................................
 * Return the total size in bytes of a register
 */
unsigned RegBlockTemp::sizeInBytes()
{
  // Return the size of this register, in bytes.

  return axes_->nEl() * nBytePerEl();
}

/**.......................................................................
 * Return the size in bytes of a single element of a register
 */
unsigned RegBlockTemp::nBytePerEl()
{
  unsigned nBytePerReg = sizeof(unsigned int);

  // See if a type size specifier was given

  if((flags_ & REG_BOOL) || (flags_ & REG_CHAR) || (flags_ & REG_UCHAR))
    nBytePerReg = sizeof(unsigned char);

  else if((flags_ & REG_SHORT) || (flags_ & REG_USHORT))
    nBytePerReg = sizeof(unsigned short);

  else if((flags_ & REG_INT) || (flags_ & REG_UINT))
    nBytePerReg = sizeof(unsigned int);

  else if(flags_ & REG_FLOAT)
    nBytePerReg = sizeof(float);

  else if(flags_ & REG_DOUBLE)
    nBytePerReg = sizeof(double);

  else if(flags_ & REG_UTC)
    nBytePerReg = sizeof(sza::util::RegDate::Data);

  // Check for complex registers

  if(flags_ & REG_COMPLEX)
    nBytePerReg *= 2;
  
  // Return the size of a single element of this register, in bytes.

  return nBytePerReg;
}

bool RegBlockTemp::isBool()
{
  return (flags_ & REG_BOOL);
}

bool RegBlockTemp::isChar()
{
  return (flags_ & REG_CHAR);
}

bool RegBlockTemp::isUchar()
{
  return (flags_ & REG_UCHAR);
}

bool RegBlockTemp::isString()
{
  return (flags_ & REG_STRING);
}

bool RegBlockTemp::isUshort()
{
  return (flags_ & REG_USHORT);
}

bool RegBlockTemp::isShort()
{
  return (flags_ & REG_SHORT);
}

bool RegBlockTemp::isUint()
{
  return (flags_ & REG_UINT);
}

bool RegBlockTemp::isInt()
{
  return (flags_ & REG_INT);
}

bool RegBlockTemp::isFloat()
{
  return (flags_ & REG_FLOAT);
}

bool RegBlockTemp::isDouble()
{
  return (flags_ & REG_DOUBLE);
}

bool RegBlockTemp::isComplex()
{
  return flags_ & REG_COMPLEX;
}


