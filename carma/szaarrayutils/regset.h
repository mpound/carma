#ifndef regset_h
#define regset_h

#include "carma/szaarrayutils/regmap.h"
#include "carma/szaarrayutils/regdata.h"
#include "carma/szaarrayutils/netbuf.h"

#include "carma/szautil/DataFrameManager.h"
#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/RegDescription.h"

/*
 * RegSet objects contain a list of contiguous ranges of archive
 * register elements. They are designed for use by users of
 * MonitorStream objects to tell the stream which registers the user
 * is interested in receiving the values of.
 */
typedef struct RegSet RegSet;

/*
 * Create an empty register-selection set for a given register map.
 */
RegSet *new_RegSet(ArrayMap *arraymap);

/*
 * Discard a redundant register-selection object.
 */
RegSet *del_RegSet(RegSet *regset);

/*
 * A contiguous range of selected register elements is described by a
 * container of the following form. A range is represented by the
 * indexes of the first and last register elements of the range. These
 * indexes refer to elements of the array of all archived registers
 * in the associated register map. They can be used to index
 * RegCalData::slots[] and RegRawData::slots[] arrays (see regdata.h).
 * Disconnected ranges are represented by linking individual ranges
 * into a list of ranges, via the 'next' field.
 */
typedef struct RegSetRange RegSetRange;
struct RegSetRange {
  unsigned int ia;   /* The index of the first register in the range */
  unsigned int ib;   /* The index of the last register in the range */
  RegSetRange *next;   /* The next range in the list (NULL marks the end) */
};

/*
 * Return the head of the list of contiguous element ranges that are
 * currently selected. Each successive range in the list covers higher
 * index values. There are no overlapping or abutting ranges.  Note
 * that this list should be treated as readonly and ephemeral. It will
 * only remain valid until the next application of a RegSet function
 * to the same RegSet.
 */
RegSetRange *regset_range_list(RegSet *regset);

/*
 * Return non-zero if two RegSet objects have identical contents.
 */
int equiv_RegSet(RegSet *regset1, RegSet *regset2);

/*
 * Return the register map size that a RegSet object was allocated
 * for.
 */
unsigned size_RegSet(RegSet *regset, bool archivedOnly);

/*
 * Copy the selections of one register set into a compatible register
 * set (two RegSet's are compatible if they were both created for
 * the same register map). Note that this results in the previous
 * selections of the destination register set being discarded.
 */
int dup_RegSet(RegSet *into, RegSet *from);

/*
 * Empty a register-selection set so that no registers remain selected.
 */
int clr_RegSet(RegSet *regset);

/*
 * Return a register set to the state of a newly allocated register set.
 * This allows it to be reused with a new client.
 */
int renew_RegSet(RegSet *regset);

/*
 * Add a given subset of the elements of a given register to a
 * register selection set. This function is idempotent.
 *
 * NB. Functions for setting up RegMapReg objects are provided in regmap.h.
 */
int add_RegSetRange(RegSet *regset, ArrRegMapReg *arreg);

/**
 * Add to the set of ranges that are currently marked as selected.
 */
int addRegSetRange(RegSet* regset, sza::util::RegDescription& parser);

/**
 * Add to the set of registers that are currently marked as selected.
 * If the new range overlaps or is immediately adjacent to other selected
 * ranges then coalesce them all into a single range.
 *
 * Input:
 *
 *  regset   RegSet * The register set to be modified.
 *  int      ia       The start index
 *  int      ib       The stop index
 *
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int addRegSetRange(RegSet *regset, int ia, int ib);

/*
 * Remove a given subset of the elements of a given register from a
 * register selection set. The specified subset need not have been
 * previously selected, either in whole or in part.
 *
 * NB. Functions for setting up RegMapReg objects are provided in regmap.h.
 */
int rem_RegSetRange(RegSet *regset, RegMapReg *reg);

/*
 * Check whether the register set includes a given register specification.
 */
int in_RegSet(RegSet *regset, RegMapReg *reg);

/*-----------------------------------------------------------------------
 * Facilities for network transmission and receipt of register sets
 * and the register values.
 *.......................................................................
 *
 * The following functions are designed for the situation where a
 * server of register values is told what to send by a client who
 * wants to receive register values. Before data communications can
 * procede the client and the server must create new register
 * selection objects, which then each have revision counts of 0.
 * Before the client can receive any data, it has to compose a
 * selection of registers in its RegSet object, and send that to the
 * server. In the process of preparing the object for transmission,
 * the net_put_RegSet() function increments its revision count. When
 * the server receives the register set and its revision count, the server
 * records the new register selection and revision count in it's
 * RegSet object. It then procedes to transmit the values of the selected
 * registers, whenever a new record of registers is received. Along
 * with these messages it includes its RegSet revision count. The
 * client quietly discards any register values that it receives where
 * this revision count is less than that in the client's own RegSet
 * object.  This ensures that after the client has updated its RegSet
 * object, no further data will be accepted from the server until the
 * server has been sent a copy of that RegSet object and begins to
 * transmit data with its revision count.
 *
 * Note that a communications channel need not be a network connection.
 * Messages in NetBuf buffers can also be written to and read from
 * files etc.. [See netbuf.h].
 *---------------------------------------------------------------------*/

/*
 * Return the number of bytes required to pack a specified register
 * selection into a NetBuf network buffer. Note that this depends on what
 * registers are currently selected, as well as on the fixed dimensions
 * of the object.
 */
long net_RegSet_size(RegSet *regset);

/*
 * Pack a specified set of register selections into a network buffer.
 * You should bracket this call with calls to net_start_put()
 * and net_end_put() [see netbuf.h].
 */
int net_put_RegSet(RegSet *regset, sza::array::NetBuf *net);
int netPutRegSet(RegSet *regset, sza::array::NetBuf *net);

/*
 * Unpack a set of register selections from a network buffer into
 * a compatible register selection set object.
 * You should bracket this call with calls to net_start_get()
 * and net_end_get() [see netbuf.h].
 */
int net_get_RegSet(RegSet* registerSet, sza::array::NetBuf *net);
int netGetRegSet(RegSet* registerSet, sza::array::NetBuf *net);

/*
 * Return the number of bytes needed to pack a selected set of
 * register values into a NetBuf network buffer.
 */
long net_regs_size(RegSet* registerSet);

/*
 * Pack a selected set of raw register values into a network
 * buffer. You should bracket this call with calls to net_start_put()
 * and net_end_put() [see netbuf.h]. Note that the revision count
 * of the specified register selection set is also packed, so for
 * a given connection you should always use the same RegSet object.
 */
int net_put_regs(RegSet* registerSet, 
		 RegRawData *raw, sza::array::NetBuf *net);

int netPutRegs(RegSet* registerSet, 
	       RegRawData *raw, sza::array::NetBuf *net);

int netPutRegs(RegSet* registerSet, 
	       sza::util::NetMonitorFrame* nmf, sza::array::NetBuf *net);

/*
 * The return value of net_get_regs().
 */
typedef enum {
  NETREG_OK,        /* Registers read OK */
  NETREG_SKIP,      /* Registers of legacy regset ignored */
  NETREG_ERROR      /* Error reading registers */
} NetRegState;

/*
 * Unpack a selected set of registers from a network buffer.
 * If the specified RegSet has a higher revision count than the
 * revision count that is packed with the message, NETREG_SKIP
 * will be returned. If it has a revision count that is lower than
 * the sender then NETREG_ERROR will be returned. Otherwise
 * NETREG_OK will be returned to signify that 'raw' now contains
 * a valid set of values.
 */
NetRegState net_get_regs(RegSet *regset, RegRawData *raw, 
			 sza::array::NetBuf *net);
NetRegState netGetRegs(RegSet *regset, RegRawData *raw, 
		       sza::array::NetBuf *net);

#endif
