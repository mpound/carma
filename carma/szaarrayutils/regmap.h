#ifndef regmap_h
#define regmap_h

#include "carma/szaarrayutils/netbuf.h"
#include "carma/szaarrayutils/input.h"
#include "carma/szaarrayutils/output.h"

#include "carma/szautil/CoordAxes.h"
#include "carma/szautil/Coord.h"

#include <map>

/*
 * List supported addressing modes.
 */
typedef enum {
  ADDR_A16D08_O,    /* A 16-bit address of an 8-bit-odd register */
  ADDR_A24D08_O,    /* A 24-bit address of an 8-bit-odd register */
  ADDR_A32D08_O,    /* A 32-bit address of an 8-bit-odd register */
  ADDR_A16D08_EO,   /* A 16-bit address of an 8-bit-even/odd register */
  ADDR_A24D08_EO,   /* A 24-bit address of an 8-bit-even/odd register */
  ADDR_A32D08_EO,   /* A 32-bit address of an 8-bit-even/odd register */
  ADDR_A16D16,      /* A 16-bit address of a 16-bit register */
  ADDR_A24D16,      /* A 24-bit address of a 16-bit register */
  ADDR_A32D16,      /* A 32-bit address of a 16-bit register */
  ADDR_A16D32,      /* A 16-bit address of a 32-bit register */
  ADDR_A24D32,      /* A 24-bit address of a 32-bit register */
  ADDR_A32D32,      /* A 32-bit address of a 32-bit register */
  ADDR_A16D16_L,    /* A 16-bit address of a 32-bit (aka longword) */
                    /*  register to be read via two D16 transfers */
  ADDR_A24D16_L,    /* A 24-bit address of a 32-bit (aka longword) */
                    /*  register to be read via two D16 transfers */
  ADDR_A32D16_L,    /* A 32-bit address of a 32-bit (aka longword) */
                    /*  register to be read via two D16 transfers */
  ADDR_DEFAULT      /* A local 32-bit array of 32-bit integers */
} RegAddrMode;

/*
 * The following bit-mask enumerators are used to specify many of the
 * characteristics of each register. When describing a register in
 * a RegBlkTmpl template, the value of the flags member is either 0,
 * or the bitwise OR of one or more of the following enumerators.
 * Note that REG_READABLE and REG_WRITABLE are ignored for REG_LOCAL
 * registers. Also note that when writing to unreadable vme registers,
 * a copy of the written value is recorded in a software shadow
 * register. This value will be returned on subsequent reads.
 * REG_EXC should be used for registers that are used internally
 * but shouldn't be archived. Note that reading from vme registers can
 * sometimes have side-effects such as acknowledging an interrupt, so
 * be careful to exclude such registers from the archive. 
 */
enum RegFlags {
  REG_NONE    = 0x0,     // None of the following.
  REG_COMPLEX = 0x1,     // Include if register contains real,imaginary
                         // pairs Exclude if register contains only real
                         // values
  REG_R       = 0x4,     // Include if the register's vme address is
		         // readable. Exclude to read written values from
		         // the shadow array.
  REG_W       = 0x8,     // Include if the register's vme address is
		         // writable. Exclude if the register can't be
		         // written to.
  REG_RW      = REG_R | REG_W,
  REG_PREAVG  = 0x10,    // Include for integrated registers.  Exclude if
  REG_POSTAVG = 0x20,    // Include for integrated registers.  Exclude if
  REG_SUM     = 0x40,    // Include for integrated registers.  Exclude if
		         // the register contains a snapshot value.
  REG_UNION   = 0x80,    // Including this tells the archiver to
		         // integrate this register by taking the
		         // bit-mask union of the values of the register
		         // that are to be combined.
  REG_EXC     = 0x100,   // Include to exclude the register from the
		         // archive.  Exclude to include the register in
		         // the archive.
  REG_UTC     = 0x200,   // Include for registers that represent times in
		         // UTC. Such registers must consist of one or
		         // more pairs of elements in which the first
		         // element contains a modified Julian day
		         // number, and the second element contains the
		         // corresponding time of day in milliseconds.
  REG_PCI     = 0x400,   // A PCI register.
  REG_BOOL    = 0x800,   // This register of single bytes
  REG_CHAR    = 0x1000,  // This register of single bytes
  REG_UCHAR   = 0x2000,  // This register of single bytes
  REG_SHORT   = 0x4000,  // This register of shorts
  REG_USHORT  = 0x8000,  // This register of shorts
  REG_INT     = 0x10000, // A register of ints
  REG_UINT    = 0x20000, // A register of ints
  REG_FLOAT   = 0x40000, // A register of floating point values
  REG_DOUBLE  = 0x80000, // A register of doubles
  REG_DEFAULT = REG_UINT,// The default is a register of unsigned ints
			 // which should be summed together on integration
  REG_DPRAM   = 0x100000,// Include if this is a register on the PMAC's
		         // DPRAM. These registers will correspond to an
		         // actual memory space in the PMAC DPRAM, but
		         // are read and written over a serial
		         // connection. Internally these registers are
		         // read from and written to shadow registers.
  REG_STRING  = 0x200000,// Include if this is a register should be
			 // interpreted as a string
  REG_FIRST   = 0x400000,// Include if this register should assume the
			 // first value when integrating it
};

/*
 * Record the locations at which VME address spaces are mapped
 * into VxWorks memory.
 */
#define VME_A16_BASE 0x0
#define VME_A24D16_BASE 0x0
#define VME_A24D32_BASE 0x0
#define VME_A32_BASE 0x0

/*
 * Set the max length of names of register blocks and boards.
 */
#define REG_NAME_LEN 100

/*
 * Define type aliases for the structures that are being declared
 * below.
 */
typedef struct RegMapBlock RegMapBlock;
typedef struct RegMapBoard RegMapBoard;
typedef struct RegMap RegMap;
 
/*
 * Describe an array of registers.
 */
struct RegMapBlock {
  RegMapBoard* brd_;          // The parent register board
  unsigned number_;           // The index of the block on its parent
			      // board
  char name_[REG_NAME_LEN+1]; // The name of the block of registers
  std::string* comment_;
  unsigned flags_;            // A bit-set of RegFlags enumerators
  RegAddrMode addr_mode_;     // The addressing mode of the register
  unsigned location_;         // The address of the first register in
                              // VxWorks address-space (0 for non-VME
                              // registers)
  unsigned ireg_;             // The sequential number of the first
			      // register
  unsigned nreg_;             // The total number of elements in the
			      // block
  int slot_;                  // The start index of the block in the
                              // archive array (or -1 if [flags &
                              // REG_EXC != 0].

  sza::util::CoordAxes* axes_;  // An axis specifier for this register

  unsigned iSlot_;            // The sequential slot index of the
			      // first register in parent slot array
  unsigned iArcSlot_;         // The sequential slot index of the
			      // first register in parent archived
			      // slot array
  unsigned iByte_;            // The sequential byte index of the
			      // first register in the parent register
			      // map
  int iArcByte_;              // The starting index of this block in
			      // the archive byte array, or -1 if this
			      // register is not archived
  unsigned nBytePerEl_;       // The number of bytes in each element
			      // of this register

  std::string* carmaUnits_;
  std::vector<std::pair<std::string, std::string> >* carmaErrors_;

  int carmaValidityBitIndex_; // Sequential index into the CARMA
			      // validity array of the start of this
			      // block

  // Methods of the RegMapBlock struct

  // Constructors

  RegMapBlock();

  RegMapBlock(RegMapBoard* parent, void* tbrd, void* tblk, 
	      unsigned iBlock, unsigned nper_brd_blocks);

  // Destructor

  ~RegMapBlock();

  // Return the number of bytes in this register block

  unsigned nByte();        

  // Return the total number of elements in this register

  unsigned nEl();

  // Return the number of bytes per element of this register

  unsigned nBytePerEl() {
    return nBytePerEl_;
  }

  // Return the offset, in bytes, of the requested element of this
  // register in the register map archive array

  int byteOffsetInWholeRegMapOf(sza::util::Coord* coord=0);
  int byteOffsetInArcRegMapOf(sza::util::Coord* coord=0);

  // Convenience method for the above

  int byteOffsetInRegMapOf(bool archivedOnly, sza::util::Coord* coord=0) {
    return archivedOnly ? byteOffsetInArcRegMapOf(coord) :
      byteOffsetInWholeRegMapOf(coord);
  }

  // Methods for returning the slot offset of an element of this
  // register

  int slotOffsetInArcRegMapOf(sza::util::Coord* coord=0) {
    return (int)(iArcSlot_ + elementOffsetOf(coord));
  }

  int slotOffsetInWholeRegMapOf(sza::util::Coord* coord=0) {
    return iSlot_ + (int)elementOffsetOf(coord);
  }

  // Return the start index of this block in the archived slot array

  int slotOffsetInRegMapOf(bool archivedOnly, sza::util::Coord* coord=0) {
    return archivedOnly ? slotOffsetInArcRegMapOf(coord) :
      slotOffsetInWholeRegMapOf(coord);
  }

  // Methods returning the element offset of the requested coordinate

  int elementOffsetOf(sza::util::Coord* coord);

  // Return true if this block is archived

  bool isArchived();

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
  bool isUtc();

  bool isSummed();       // True if summed on integration
  bool isUnioned();      // True if bitwise-unioned on integration
  bool isPreAveraged();  // True if averaged on integration
  bool isPostAveraged(); // True if averaged when read
  bool isFirst();        // True if assumes the first value when
			 // integrating
  bool isComplex();

  /**
   * Check flags for self-consistency
   */
  void checkConsistency();

  /**
   * Check the type specification
   */
  void checkType(unsigned flags);

  /**
   * Check the integration specification
   */
  void checkIntegration(unsigned flags);

  /**
   * Check if the combination of attributes is valid for this register
   */
  void checkAttributes();

  friend std::ostream& operator<<(std::ostream& os, RegMapBlock& block);
};

/*
 * Collect registers that reside on a given board.
 */
struct RegMapBoard {
  RegMap *regmap;            /* The parent register map */
  unsigned number;           /* The index of the board in the register map */
  char name[REG_NAME_LEN+1]; /* An unambiguous name for the board */
  std::string* comment_;
  std::vector<RegMapBlock*> blocks;       /* The registers of the board */
  std::map<std::string, RegMapBlock*> blockMap_;  /* The registers of the board */
  int nblock;                /* The number of register blocks in blocks[] */
  unsigned nreg;             /* The total number of registers on the board */
  unsigned narchive;         /* The number of archived registers on the board */

  unsigned nByte_;
  unsigned nArcByte_;

  unsigned iSlot_;          // The sequential index of the first
			    // register of this board in the parent
			    // register map slot array
  int iArcSlot_;            // The sequential index of the first
			    // register of this board in the parent
			    // archived slot array
  unsigned iByte_;          // The sequential index of the first byte
			    // of this board in the parent register map
  int iArcByte_;            // The sequential index of the first byte
			    // of this board in the archived parent
			    // register map

  // Return the byte offset of this board in the parent register map

  int byteOffsetInRegMap(bool archivedOnly) {
    return archivedOnly ? iArcByte_ : iByte_;
  }

  // Return the slot offset of this board in the parent register map

  int slotOffsetInRegMap(bool archivedOnly) {
    return archivedOnly ? iArcSlot_ : iSlot_;
  }

  // Return the number of bytes in this board

  unsigned nByte(bool archivedOnly) {
    return archivedOnly ? nArcByte_ : nByte_;
  }

  RegMapBoard();

  RegMapBoard(RegMap* parent, void* vbrd, unsigned iboard, 
	      void* vper_brd_blocks=0, unsigned nper_brd_blocks=0);

  ~RegMapBoard();

  // Return the named register block

  RegMapBlock* findRegMapBlock(std::string blockName);

  // Return a vector of blocks matching the input string

  std::vector<RegMapBlock*> matchRegMapBlock(std::string regExpStr);
  std::vector<RegMapBlock*> matchRegMapBlock(std::vector<std::string>& blocks);
};

/*
 * Collect information about all boards.
 */
struct RegMap {
  unsigned ref_count_;       // Reference counter 
  std::vector<RegMapBoard*> boards_; // All addressable boards 
  std::map<std::string, RegMapBoard*> boardMap_;
  int nboard_;               // The number of boards 
  unsigned nreg_;            // The total number of registers 
  unsigned narchive_;        // The total number of archived registers
  unsigned nByte_;           // The size in bytes of all registers in
			     // this register map
  unsigned nArcByte_;        // The size in bytes of all archived
			     // registers in this register map

  // A constructor

  RegMap(void *regtmp, bool old=false, bool addRegs=true);

  // Destructor

  ~RegMap();

  void privateConstructor(void* regtmp, bool addRegs=true);
  void privateConstructorOld(void* regtmp);

  // Return the offset, in bytes_ of the named board and block from
  // the start of the archived array

  int byteOffsetInArcRegMapOf(std::string board, std::string block, 
			       sza::util::Coord* coord=0);
  int byteOffsetInWholeRegMapOf(std::string board, std::string block, 
				sza::util::Coord* coord=0);

  // Convenience method for the above two

  int byteOffsetInRegMapOf(bool archivedOnly, 
			   std::string board, std::string block, 
			   sza::util::Coord* coord=0) {
    return archivedOnly ? byteOffsetInArcRegMapOf(board, block, coord) :
      byteOffsetInWholeRegMapOf(board, block, coord);
  }

  int byteOffsetInArcRegMapOf(RegMapBlock* blk, sza::util::Coord* coord=0);
  int byteOffsetInWholeRegMapOf(RegMapBlock* blk, sza::util::Coord* coord=0);

  // Convenience method for the above two

  int byteOffsetInRegMapOf(bool archivedOnly, 
			   RegMapBlock* blk,
			   sza::util::Coord* coord=0) {
    return archivedOnly ? byteOffsetInArcRegMapOf(blk, coord) :
      byteOffsetInWholeRegMapOf(blk, coord);
  }

  unsigned nByte() {return nByte_;};
  unsigned nArcByte() {return nArcByte_;};

  // Convenience method for the above two

  int nByte(bool archivedOnly) {
    return archivedOnly ? nArcByte() : nByte();
  }

  // Find the named block

  RegMapBlock* findRegMapBlock(std::string board_name, std::string block_name,
			       bool doThrow=false);

};

/* NB. new_RegMap() is prototyped in regtemplate.h */

/**.......................................................................
 * Delete a regmap, checking the ref counter.
 */
RegMap* del_RegMap(RegMap* regmap);

/* Allocate a readonly alias to a given register map. */
/*  When no longer required this should be discarded by calling */
/*  del_RegMap(). */

RegMap *alias_RegMap(RegMap *regmap);

/* Return non-zero if two register maps are equivalent. */

int equiv_RegMap(RegMap *regmap1, RegMap *regmap2);

RegMapBoard *find_RegMapBoard(RegMap *regmap, std::string board_name);
RegMapBlock *find_RegMapBoard_Block(RegMapBoard *board, std::string block_name);
RegMapBlock *find_RegMapBlock(RegMap *regmap, std::string board_name,
			      std::string block_name);

/*
 * Enumerate the register-specifications read by input_RegMapReg().
 * The comment next to each mode indicates the type of input expected,
 * where "board" is the name of a board, "register" is the name of
 * a block of registers, and index is the sequential number of
 * a register within a block.
 */
typedef enum {
  REG_INPUT_BLOCK,     /* board.register */
  REG_INPUT_ELEMENT,   /* board.register[index] */
  REG_INPUT_RANGE      /* board.register[index-index] */
} RegInputMode;

/*
 * When the extend argument of input_RegMapReg() is true, and a complex or
 * utc register-pair specification is read, a member of the following type
 * is used to record what aspect of the complex register has been selected
 * by the user. If the user doesn't specify anything then REG_PLAIN is
 * substituted. Where appropriate these enumerators can be used as
 * indexes of arrays of REG_NASPECT elements.
 */
typedef enum {
  REG_ASPECT_UNKNOWN,
  REG_PLAIN,          /* Treat complex register pairs as two registers */
  REG_REAL,           /* The real member of the complex register pair */
  REG_IMAG,           /* The imaginary member of the complex register pair */
  REG_AMP,            /* The amplitude of a complex register pair */
  REG_PHASE,          /* The phase of a complex register pair */
  REG_DATE,           /* The Modified Julian Date of a day/time utc pair */
  REG_TIME            /* The time-of-day of a day/time utc pair */
} RegAspect;

enum {REG_NASPECT = 8};  /* The number of enumerators in RegAspect */

char *name_RegAspect(RegAspect aspect);

typedef enum {
  REG_INT_UNKNOWN,
  REG_INT_PLAIN, // Treat the register normally
  REG_INT_INT,   // Integrate the register
  REG_INT_DER    // Take the derivative of the register
} RegInteg;

enum {REG_NINTEG = 4};  /* The number of enumerators in RegInteg */

char *name_RegInteg(RegInteg integ);


/*
 * A structure of the following form is filled by input_RegMapReg().
 * The meanings of reg and nreg depend on the input mode as follows:
 *
 *  REG_INPUT_BLOCK:
 *   index will be 0.
 *   slot will be the frame index of the first register of the block.
 *   nreg will be the number of registers in the block.
 *  REG_INPUT_ELEMENT:
 *   index will be the block index of the selected member of the block.
 *   slot will be the frame index of the selected member of the block.
 *   nreg will always be 1.
 *  REG_INPUT_RANGE:
 *   index will be the block index of the selected member of the block.
 *   slot will be the frame index of the first selected member of the block.
 *   nreg will be the number of registers in the range.
 *
 * Note that if the selected register is not an archived register, the
 * slot index will be -1.
 */
struct RegMapReg {
  int board;           // The index of the specified board in the
		       // register-map
  int block;           // The index of the specified block on the
		       // regmap board
  unsigned index;      // The block index of the first register
		       // specified
  signed slot;         // The frame index of the first register
		       // specified
  unsigned nreg;       // The number of registers specified. The
                       // associated regmap slots are: slot -> slot +
                       // nreg*size - 1
  unsigned size;       // The number of slots per register. This is 1
                       // unless aspect!=REG_PLAIN, in which case it
                       // becomes 2
  RegAspect aspect;    // The quantity to derive from complex or utc
                       // register pairs
  RegInteg integ;      // The integration status of this register
                       // register pairs
  int iByte_;          // The byte index into the register map of the
		       // first register specified
  int iArcByte_;       // The byte index into the archived register
		       // map of the first register specified
  unsigned nBytePerEl_;// The number of bytes per element of this register

  sza::util::CoordAxes* axes_;   // An axis specifier for this register
  sza::util::CoordRange* range_; // An index-range specifier
};

int find_RegMapReg(RegMap *regmap, std::string board_name, std::string block_name,
		   RegAspect facet, unsigned index, unsigned n,
		   RegMapReg *reg);

int init_RegMapReg(RegMap *regmap, unsigned board, unsigned block,
		   unsigned index, unsigned nreg, RegAspect aspect,
		   RegMapReg *reg);

typedef enum {
  REG_VALID=0,    /* A valid register was found */
  REG_UNKNOWN,    /* Valid specification syntax, but unknown register */
  REG_INVALID     /* Invalid register specification syntax */
} RegValidity;

RegValidity input_RegMapReg(InputStream *stream, int tell, RegMap *regmap,
			    RegInputMode mode, int extend, RegMapReg *reg);

/*
 * Save a register specification in the form read by input_RegMapReg().
 */
typedef enum {
  REG_OUTPUT_BLOCK,     /* board.register */
  REG_OUTPUT_ELEMENT,   /* board.register[index] */
  REG_OUTPUT_RANGE      /* board.register[index-index] */
} RegOutputMode;

int output_RegMapReg(OutputStream *stream, RegMap *regmap, RegOutputMode mode,
		     RegMapReg *reg);

void clr_RegMapReg(RegMapReg *reg);

/*
 * If registers are represented as 32-bit integers then we need a method
 * to pack a string into an array of 32-bit integers. The following function
 * does this.
 */
int pack_int_string(char *string, int ndata, unsigned *data);

/*
 * Unpack a string that was previously packed by pack_int_string().
 */
int unpack_int_string(unsigned *data, int ndata, int size, char *string);

/*
 * Unpack a string that was previously packed by pack_int_string() and
 * then assigned to a parallel array of double.
 */
int unpack_double_string(double *data, int ndata, int size, char *string);

#endif
