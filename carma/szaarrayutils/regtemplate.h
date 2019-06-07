#ifndef regtemplate_h
#define regtemplate_h

/*   ***** PLEASE READ BEFORE MAKING MODIFICATIONS *****
 * If you make ANY modifications to the register template
 * which make it incompatible with older archived register
 * map templates, you must increment the REGMAP_REVISION
 * macro at the top of regmap.h.
 */
#include <utility>
#include <string>
#include <vector>

/*
 * This file defines the templates required by new_RegMap() for
 * creating register maps. The resulting register map, and
 * accompanying objects are defined in regmap.h.
 */
#include "carma/szaarrayutils/regmap.h"

#define ARRAY_DIM(array) (sizeof(array)/sizeof((array)[0]))

/*
 * Each board can have up to NBASE base addresses. The choice of
 * NBASE is arbitrary. Note, however, that if you change it, you
 * must also change REGMAP_REVISION in regmap.h.
 */
typedef enum {
  REG_BASE0,  /* To select the base address in RegBoardTemp::bases[0] */
  REG_BASE1,  /* To select the base address in RegBoardTemp::bases[1] */
  REG_BASE2,  /* To select the base address in RegBoardTemp::bases[2] */
  REG_BASE3,  /* To select the base address in RegBoardTemp::bases[3] */
  NBASE       /* The dimension of RegBoardTemp::bases[] */
} RegBase;

/*
 * The VME registers of a given board are specified via an array
 * of the following type. This is a template for creation of a
 * RegBlock object.
 */
struct RegBlockTemp {
  char name_[REG_NAME_LEN+1];   // The name of the block of registers 
  unsigned flags_;              // A bit-set of RegFlags enumerators 
  RegAddrMode addr_mode_;       // The addressing mode of the register 
  RegBase base_;                // Use the base-address in board->bases[base] 
  unsigned address_;            // The address of the block in the address 
                                // space given by addr_mode (0 for REG_LOCAL) 
  sza::util::CoordAxes* axes_;  // An axis specifier for this register
  std::string* comment_;
  std::string* carmaUnits_;
  std::vector<std::pair<std::string, std::string> >* carmaErrors_;
  unsigned carmaFlags_;
  int carmaValidityBitIndex_; // Sequential index into the CARMA
			      // validity array of the start of this
			      // block

  //------------------------------------------------------------
  // Methods associated with this struct
  //------------------------------------------------------------

  // A constructor

  RegBlockTemp(std::string comment, std::string name, 
	       unsigned flags, unsigned address, 
	       unsigned nel0,  unsigned nel1, unsigned nel2,
	       std::string carmaUnits, 
	       std::vector<std::pair<std::string, std::string> >& errPairs);

  RegBlockTemp(std::string comment, std::string name, 
	       unsigned flags,  unsigned address=0, 
	       unsigned nel0=1, unsigned nel1=0, unsigned nel2=0,
	       std::string carmaUnits="", 
	       std::string carmaErrLow1="", std::string carmaErrHigh1="",
	       std::string carmaErrLow2="", std::string carmaErrHigh2="",
	       std::string carmaErrLow3="", std::string carmaErrHigh3="",
	       int carmaValidityBitIndex=-1);

  RegBlockTemp(const RegBlockTemp& reg);
  RegBlockTemp(RegBlockTemp& reg);

  void setTo(std::string comment, std::string name, 
	     unsigned flags, unsigned address, 
	     unsigned nel0,  unsigned nel1, unsigned nel2,
	     std::string carmaUnits, 
	     std::vector<std::pair<std::string, std::string> >& errPairs,
	     int carmaValidityBitIndex=-1);

  void initialize();

  void operator=(const RegBlockTemp& reg);
  void operator=(RegBlockTemp& reg);

  // Destructor

  ~RegBlockTemp();

  // Return the total number of register elements

  unsigned nEl();

  // Return the number of bytes per element of this register

  unsigned nBytePerEl();

  // Return the size in bytes of this register

  unsigned sizeInBytes();

  // Return true if the block type matches 
  
  bool isBool();
  bool isUchar();
  bool isString();
  bool isChar();
  bool isUshort();
  bool isShort();
  bool isUint();
  bool isInt();
  bool isFloat();
  bool isDouble();
  bool isComplex();
};

/*
 * Each board of registers is described by an element of the following
 * type.
 */
typedef struct {
  char name_[REG_NAME_LEN+1]; // An unambiguous name for the board 
  RegBlockTemp* blocks_;      // The registers of the board 
  int nblock_;                // The number of register blocks in blocks[] 
  unsigned bases_[NBASE];     // An array of up to NBASE base addresses 
  char comment_[100];
} RegBoardTemp;

/**.......................................................................
 * Collect the register map template information into a single object.
 */
typedef struct {
  RegBoardTemp *boards;  // The array of register-board maps 
  unsigned nboard;       // The number of elements in boards[] 
  char comment_[100];
} RegTemplate;

RegMap *net_get_RegMap(sza::array::NetBuf *net);

/* NB. del_RegMap() is prototyped in regmap.h */

int net_put_RegTemplate(RegTemplate *regtmp, sza::array::NetBuf *net);
RegTemplate *net_get_RegTemplate(sza::array::NetBuf *net, unsigned long arraymap_revision);
long net_RegTemplate_size(RegTemplate *regtmp);

/*
 * The following destructor should only be applied the dynamically
 * allocated templates that are returned by net_get_RegTemplate().
 */
RegTemplate *del_RegTemplate(RegTemplate *regtmp);

#endif
