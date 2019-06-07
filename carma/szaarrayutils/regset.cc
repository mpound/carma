#include <stdlib.h>
#include <stdio.h>

#include "carma/szaarrayutils/regset.h"
#include "carma/szaarrayutils/lprintf.h"

#include "carma/szautil/Debug.h"
#include "carma/szautil/RegDescription.h"

using namespace sza::array;
using namespace sza::util;

/*
 * A RegSet object contain a list of contiguous ranges of archive
 * register elements.
 */
struct RegSet {
  unsigned revision;     /* A revision count that is incremented on calls */
                         /* to net_put_RegSet() */
  unsigned nArcSlot;     /* The number of archived registers in the */
                         /*  parent register map */
  unsigned nslot;        /* The total number of registers in the */
                         /*  parent register map */
  unsigned nByte_;       // The number of bytes in the parent register
                         //  map
  unsigned nArcByte_;    // The number of archived bytes in the parent register
                         //  map
  unsigned max_range;    /* The dimension of range_mem[] */
  RegSetRange *range_mem;/* Memory for up to max_range range nodes */
  RegSetRange *free_list;/* Free-list allocated from ranges[] */
  RegSetRange *ranges;   /* The current set of selected registers, expressed */
                         /*  as a list of contiguous ranges */
  unsigned nselected;    /* The number of currently selected registers */
  unsigned nByteSelected;// The number of currently selected bytes
  unsigned nrange;       /* The number of ranges in the 'ranges' list */

  unsigned nByte(bool archivedOnly=true) {
    return archivedOnly ? nArcByte_ : nByte_;
  }
};

static RegSetRange *insert_RegSetRange(RegSet *regset, RegSetRange *prev,
				       int ia, int ib);
static int remove_RegSetRange(RegSet *regset, RegSetRange *prev,
			      RegSetRange *last);
static int ngRS_error(RegSet *regset);

/*.......................................................................
 * Create a register set for use in selecting registers to be monitored.
 *
 * Input:
 *  regmap    RegMap *  The register map. The set will represent the
 *                      regmap->narchive registers that are marked for
 *                      archival.
 * Output:
 *  return    RegSet *  The new set container, or NULL on error.
 */
RegSet* new_RegSet(ArrayMap* arraymap)
{
  RegSet *regset;             /* The container to be returned */
  unsigned i;

  // Check arguments.

  if(!arraymap) {
    lprintf(stderr, "new_RegSet: NULL regmap argument.\n");
    return NULL;
  };
  
  // Create the container.

  regset = (RegSet *) malloc(sizeof(RegSet));
  if(!regset) {
    lprintf(stderr, "new_RegSet: Insufficient memory.\n");
    return NULL;
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_RegSet().

  regset->revision      = 0;
  regset->nArcSlot      = arraymap->narchive;
  regset->nslot         = arraymap->nreg;
  regset->nArcByte_     = arraymap->nArcByte_;
  regset->nByte_        = arraymap->nByte_;
  regset->max_range     = 0;
  regset->ranges        = NULL;
  regset->range_mem     = NULL;
  regset->free_list     = NULL;
  regset->nrange        = 0;
  regset->nselected     = 0;
  regset->nByteSelected = 0;
  
  // Determine the maximum number of ranges required.  Note that if
  // adjoining index ranges are coallesced the max number of possible
  // ranges is given by the total number of selectable register
  // elements divided by two and rounded up to the next integer.

  regset->max_range = (arraymap->narchive + 1) / 2;
  
  // Allocate an array of ranges for use in the free list.

  regset->range_mem = (RegSetRange *)malloc(sizeof(RegSetRange) * 
					    regset->max_range);
  if(!regset->range_mem) {
    lprintf(stderr, "new_RegSet: Insufficient memory for range list.\n");
    return del_RegSet(regset);
  };
  
  // Initialize the nodes of ranges.

  for(i=0; i<regset->max_range; i++) {
    RegSetRange *rr = regset->range_mem + i;
    rr->ia = 0;
    rr->ib = 0;
    rr->next = NULL;
  };
  
  // Link the nodes into a free-list.

  regset->free_list = regset->range_mem;
  for(i=0; i < regset->max_range-1; i++)
    regset->free_list[i].next = &regset->free_list[i+1];
  regset->free_list[i].next = NULL;

  return regset;
}

/*.......................................................................
 * Delete a register-set object.
 *
 * Input:
 *  regset    RegSet *  The register set container to be deleted.
 * Output:
 *  return    RegSet *  The deleted set container (always NULL).
 */
RegSet *del_RegSet(RegSet *regset)
{
  if(regset) {
    if(regset->range_mem)
      free(regset->range_mem);
    regset->range_mem = regset->free_list = regset->ranges = NULL;
    regset->nselected = 0;
    regset->nByteSelected = 0;
  };
  return NULL;
}

/**.......................................................................
 * Add to the set of ranges that are currently marked as selected.
 */
int addRegSetRange(RegSet* regset, sza::util::RegDescription& desc)
{
  try {
    std::vector<sza::util::Range<unsigned> > ranges;

    ranges = desc.getByteRanges();
    for(unsigned iRange=0; iRange < ranges.size(); iRange++) {
      addRegSetRange(regset, ranges[iRange].start(), ranges[iRange].stop());
    }
  } catch(...) {
    return 1;
  }

  return 0;
}

/*.......................................................................
 * Add to the set of registers that are currently marked as selected.
 * If the new range overlaps or is immediately adjacent to other selected
 * ranges then coalesce them all into a single range.
 *
 * Input:
 *  regset   RegSet *   The register set to be modified.
 *  reg   RegMapReg *   The register selection to add (see regmap.h).
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int addRegSetRange(RegSet *regset, int ia, int ib)
{
  RegSetRange *prev;   /* The range-node that precedes 'next' */
  RegSetRange *next;   /* The next range-node to be checked */
  RegSetRange *node;   /* The register range being initialized */
  
  // The channel ranges are stored in ascending order. Find existing
  // ranges that bracket the start index of the new range. When the
  // loop completes, the 'next' variable will point at the first
  // overlapping range, or NULL if the new range is completely
  // unsampled. The 'prev' variable will point at the node that
  // precedes 'next', or NULL if 'next' is the start of the list.

  prev = NULL;
  for(next=regset->ranges; next && ia > next->ib+1; prev=next,next=prev->next)
    ;
  
  // If there are no overlapping ranges, acquire a new node from the
  // free-list and assign the new range to it.

  if(!next || ib < (signed)next->ia-1) {
    if(insert_RegSetRange(regset, prev, ia, ib) == NULL)
      return 1;
    
    // An overlapping range has been found.

  } else {
    
    // Extend the lower limit of the overlapping range to accomodate
    // the union of the new and overlapped ranges.

    if(ia > next->ia)
      ia = next->ia;
    
    // Find the last node that is overlapped by the new range.  When
    // this loop completes, node will point at the last overlapping
    // node and thus next will point at the first non-overlapping
    // node.

    node=next;
    for(next=node->next; next && ib >= (signed)next->ia - 1;
	node=next,next=node->next)
      ;
    
    // Extend the upper limit of the overlapping range to accomodate
    // the union of the new and overlapped ranges.

    if(ib < node->ib)
      ib = node->ib;
    
    // Return the overlapped nodes to the free-list and re-link the
    // range list around them.

    if(remove_RegSetRange(regset, prev, node))
      return 1;
    
    // Insert the new node.

    if(insert_RegSetRange(regset, prev, ia, ib) == NULL)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Add to the set of registers that are currently marked as selected.
 * If the new range overlaps or is immediately adjacent to other selected
 * ranges then coalesce them all into a single range.
 *
 * Input:
 *  regset   RegSet *   The register set to be modified.
 *  reg   RegMapReg *   The register selection to add (see regmap.h).
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int add_RegSetRange(RegSet *regset, ArrRegMapReg *arreg)
{
  RegSetRange *prev;   /* The range-node that precedes 'next' */
  RegSetRange *next;   /* The next range-node to be checked */
  RegSetRange *node;   /* The register range being initialized */
  int ia,ib;           /* The indexes of the first and last selected elements */
  
  RegMapReg* reg = &arreg->reg;

  // Check the arguments.

  if(!regset || !reg) {
    lprintf(stderr, "add_RegSetRange: NULL argument(s).\n");
    return 1;
  };
  
  // Determine the range of elements to be added to the set.

  ia = reg->slot;
  ib = reg->slot + reg->nreg * reg->size - 1;
/*
 * Widen the selected range to meet alignment and atomicity constraints.
 */
  if(reg->size > 1) {
    int i0 = reg->slot - reg->index; /* The index of the first block element */
    ia -= (ia-i0) % reg->size;
    ib += (reg->size - 1) - ((ib-i0) % reg->size);
  };
  if(ia < 0 || ib >= (int)regset->nslot) {
    lprintf(stderr, "add_RegSetRange: The index selection is out of range.\n");
    return 1;
  };
/*
 * The channel ranges are stored in ascending order. Find existing ranges
 * that bracket the start index of the new range. When the loop completes,
 * the 'next' variable will point at the first overlapping range, or NULL
 * if the new range is completely unsampled. The 'prev' variable will point at
 * the node that precedes 'next', or NULL if 'next' is the start of the
 * list.
 */
  prev = NULL;
  for(next=regset->ranges; next && ia > next->ib+1; prev=next,next=prev->next)
    ;
/*
 * If there are no overlapping ranges, acquire a new node from the
 * free-list and assign the new range to it.
 */
  if(!next || ib < (signed)next->ia-1) {
    if(insert_RegSetRange(regset, prev, ia, ib) == NULL)
      return 1;
/*
 * An overlapping range has been found.
 */
  } else {
/*
 * Extend the lower limit of the overlapping range to accomodate
 * the union of the new and overlapped ranges.
 */
    if(ia > next->ia)
      ia = next->ia;
/*
 * Find the last node that is overlapped by the new range.
 * When this loop completes, node will point at the last overlapping
 * node and thus next will point at the first non-overlapping node.
 */
    node=next;
    for(next=node->next; next && ib >= (signed)next->ia - 1;
	node=next,next=node->next)
      ;
/*
 * Extend the upper limit of the overlapping range to accomodate
 * the union of the new and overlapped ranges.
 */
    if(ib < node->ib)
      ib = node->ib;
/*
 * Return the overlapped nodes to the free-list and re-link the range
 * list around them.
 */
    if(remove_RegSetRange(regset, prev, node))
      return 1;
/*
 * Insert the new node.
 */
    if(insert_RegSetRange(regset, prev, ia, ib) == NULL)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Subtract from the set of registers that are currently marked as
 * selected. The specified range need not exactly match any given range
 * and can overlap any number of ranges.
 *
 * Input:
 *  regset   RegSet *   The register set to be modified.
 *  reg   RegMapReg *   The register selection to remove (see regmap.h).
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int rem_RegSetRange(RegSet *regset, RegMapReg *reg)
{
  RegSetRange *prev;  /* The range-node that precedes 'next' */
  RegSetRange *next;  /* The next range-node to be checked */
  int ia,ib;          /* The indexes of the first and last elements to remove */
/*
 * Check the arguments.
 */
  if(!regset || !reg) {
    lprintf(stderr, "rem_RegSetRange: NULL argument(s).\n");
    return 1;
  };
/*
 * Determine the range of elements to be removed from the set.
 */
  ia = reg->slot;
  ib = reg->slot + reg->nreg * reg->size - 1;
/*
 * Widen the specified range to meet alignment and atomicity constraints.
 */
  if(reg->size > 1) {
    int i0 = reg->slot - reg->index; /* The index of the first block element */
    ia -= (ia-i0) % reg->size;
    ib += (reg->size - 1) - ((ib-i0) % reg->size);
  };
  if(ia < 0 || ib >= (int)regset->nslot) {
    lprintf(stderr,
	    "rem_RegSetRange: The index specification is out of range.\n");
    return 1;
  };
/*
 * The channel ranges are stored in ascending order. Find existing ranges
 * that bracket the start index of the new range. When the loop completes,
 * the 'next' variable will point at the first overlapping range, or NULL
 * if the range is completely unsampled. The 'prev' variable will point at
 * the node that precedes 'next', or NULL if 'next' is the start of the
 * list.
 */
  prev = NULL;
  for(next=regset->ranges; next && ia > next->ib+1; prev=next,next=prev->next)
    ;
/*
 * If there are no overlapping ranges, then nothing more needs to be
 * done. Were any overlapping ranges found? If so, start by coallescing
 * overlapping ranges and the range to be removed. This results in a
 * single overlapping range from which to remove the given range.
 */
  if(next && ib >= (signed)next->ia-1) {
    unsigned overlap_ia;  /* The lower limit of the first overlapping range */
    unsigned overlap_ib;  /* The upper limit of the last overlapping range */
    RegSetRange *node;    /* The last overlapping node */
/*
 * Record the lower limit of the first node that overlaps the range to
 * be removed.
 */
    overlap_ia = next->ia;
/*
 * Find the last node that is overlapped by the new range.
 * When this loop completes, node will point at the last overlapping
 * node and thus next will point at the first non-overlapping node.
 */
    node=next;
    for(next=node->next; next && (signed)ib >= (signed)next->ia - 1; node=next,next=node->next)
      ;
/*
 * Record the upper limit of the last node that overlaps the range to
 * be removed.
 */
    overlap_ib = node->ib;
/*
 * Return all overlapping nodes to the free-list and re-link the
 * remaining ranges around them.
 */
    if(remove_RegSetRange(regset, prev, node))
      return 1;
/*
 * If the removed ranges extended past the range that was supposed
 * to be removed, re-introduce the extensions.
 */
    if((int)overlap_ia < ia) {
      if(insert_RegSetRange(regset, prev, overlap_ia, ia-1) == NULL)
	return 1;
    };
    if((int)overlap_ib > ib) {
      if(insert_RegSetRange(regset, prev, ib+1, overlap_ib) == NULL)
	return 1;
    };
  };
  return 0;
}

/*.......................................................................
 * Acquire and insert a new register range at a given position in
 * the current list of ranges.
 *
 * Input:
 *  regset       RegSet *   The register set to be modified.
 *  prev    RegSetRange *   The node preceding the required position
 *                          for the new node. A value of NULL denotes
 *                          the beginning of the list.
 *  ia,ib           int     The new range indexes.
 * Output:
 *  return  RegSetRange *   The new range node, or NULL on error.
 */
static RegSetRange *insert_RegSetRange(RegSet *regset, RegSetRange *prev,
				       int ia, int ib)
{
  RegSetRange *node;   /* The new node */
/*
 * Acquire a new node from the free-list.
 */
  node = regset->free_list;
  if(!node) {
    lprintf(stderr, "insert_RegSetRange: Empty free-list.\n");
    return NULL;
  };
  regset->free_list = node->next;
/*
 * Assign the specified range to the new node.
 */
  node->ia = ia;
  node->ib = ib;
/*
 * Insert the new node into the list.
 */
  if(prev) {
    node->next = prev->next;
    prev->next = node;
  } else {
    node->next = regset->ranges;
    regset->ranges = node;
  };
/*
 * Maintain a count of the total number of selected register elements,
 * and the number of contiguous ranges.
 */
  regset->nselected     += ib - ia + 1;
  regset->nByteSelected += ib - ia + 1;
  regset->nrange++;
  return node;
}

/*.......................................................................
 * Remove a section of the current list of ranges and return the
 * removed nodes to the free-list.
 *
 * Input:
 *  regset     RegSet *  The register set to be modified.
 *  prev  RegSetRange *  The node preceding the first node to be
 *                       removed. A value of NULL denotes
 *                       the beginning of the list.
 *  last  RegSetRange *  The last node of the list segment to be
 *                       removed. A value of NULL denotes
 *                       the end of the list.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int remove_RegSetRange(RegSet *regset, RegSetRange *prev,
			      RegSetRange *last)
{
  RegSetRange *node;   /* One of the nodes that is to be removed */
  RegSetRange *first;  /* The first node to be removed */
  RegSetRange *next;   /* The node that follows 'last' */
/*
 * Is the list already empty?
 */
  if(!regset->ranges) {
    if(prev || last) {
      lprintf(stderr, "remove_RegSetRange: Empty list.\n");
      return 1;
    };
    return 0;
  };
/*
 * Deduce the node that follows the last node that is to be removed.
 */
  next = last ? last->next : NULL;
/*
 * Deduce the first node that is to be removed.
 */
  first = prev ? prev->next : regset->ranges;
/*
 * Is the first node at the end of the list?
 */
  if(!first || next==first) {
    if(last) {
      lprintf(stderr, "remove_RegSetRange: Inconsistent prev and last.\n");
      return 1;
    };
    return 0;
  };
/*
 * Update the count of the total number of selected registers slots
 * and the number of contiguous ranges. Also (re-)determine the value of
 * 'last', to cater for when last is entered as NULL.
 */
  for(node = first; node != next && node; node = node->next) {
    regset->nselected     -= node->ib - node->ia + 1;
    regset->nByteSelected -= node->ib - node->ia + 1;
    regset->nrange--;
    last = node;
  };
/*
 * Was the end node found?
 */
  if(node != next) {
    lprintf(stderr, "remove_RegSetRange: last does not follow prev.\n");
    return 1;
  };
/*
 * Remove the list of nodes from regset->ranges and return it to
 * the free-list.
 */
  last->next = regset->free_list;
  if(prev) {
    regset->free_list = prev->next;
    prev->next = next;
  } else {
    regset->free_list = regset->ranges;
    regset->ranges = next;
  };
  return 0;
}

/*.......................................................................
 * Clear a register set.
 *
 * Input:
 *  regset   RegSet *  The register set to be emptied of all selected
 *                     registers.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
int clr_RegSet(RegSet *regset)
{
/*
 * Remove all of the ranges.
 */
  return regset && remove_RegSetRange(regset, NULL, NULL);
}

/*.......................................................................
 * Return a register set to the state of a newly allocated register set.
 * This allows it to be reused with a new client.
 *
 * Input:
 *  regset   RegSet *  The register set to be emptied of all selected
 *                     registers.
 * Output:
 *  return      int    0 - OK.
 *                     1 - Error.
 */
int renew_RegSet(RegSet *regset)
{
/*
 * Check arguments.
 */
  if(!regset) {
    lprintf(stderr, "reset_RegSet: NULL argument.\n");
    return 1;
  };
/*
 * Clear the list of selected ranges.
 */
  clr_RegSet(regset);
/*
 * Clear the network revision count.
 */
  regset->revision = 0;
  return 0;
}

/*.......................................................................
 * Return the maximum space needed to pack a register set into a
 * NetBuf network buffer.
 *
 * Input:
 *  regset   RegSet *  The set to be characterized.
 * Output:
 *  return     long    The number of bytes needed to pack a full
 *                     set of register ranges.
 */
long net_RegSet_size(RegSet *regset)
{
  return NET_LONG_SIZE +                    /* The revision count */
    NET_SHORT_SIZE * regset->max_range * 2; /* Two indexes per register range */
}

/*.......................................................................
 * Pack a register set for transmission over a network.
 *
 * Input:
 *  regset   RegSet *   The local copy of the register set.
 *  net      NetBuf *   The network buffer from which to unpack the set.
 *                      Note it is left to the caller to bracket this
 *                      call with calls to net_start_put() and
 *                      net_end_put().
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int net_put_RegSet(RegSet *regset, NetBuf *net)
{
  unsigned long tmp[3];   /* Used to send data to net_put_short() */
  RegSetRange *range;       /* The range being packed */
/*
 * Check arguments.
 */
  if(!net) {
    lprintf(stderr, "net_put_RegSet: NULL argument.\n");
    return 1;
  };
/*
 * Increment the revision count and pack the result.
 */
  {
    unsigned long revision = ++regset->revision;
    if(net_put_long(net, 1, &revision))
      return 1;
  };
/*
 * Pack a record of the number of ranges being packed.
 */
  tmp[0] = regset->nrange;
  if(net_put_long(net, 1, tmp))
    return 1;
/*
 * Pack the limits of each range.
 */
  for(range=regset->ranges; range; range=range->next) {
    tmp[0] = range->ia;
    tmp[1] = range->ib;
    if(net_put_long(net, 2, tmp))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Pack a register set for transmission over a network.
 *
 * Input:
 *  regset   RegSet *   The local copy of the register set.
 *  net      NetBuf *   The network buffer from which to unpack the set.
 *                      Note it is left to the caller to bracket this
 *                      call with calls to net_start_put() and
 *                      net_end_put().
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int netPutRegSet(RegSet *regset, NetBuf *net)
{
  unsigned long ltmp[3];   /* Used to send data to net_put_short() */
  RegSetRange *range;       /* The range being packed */
/*
 * Check arguments.
 */
  if(!net) {
    lprintf(stderr, "net_put_RegSet: NULL argument.\n");
    return 1;
  };
/*
 * Increment the revision count and pack the result.
 */
  {
    unsigned long revision = ++regset->revision;
    if(net_put_long(net, 1, &revision))
      return 1;
  };
/*
 * Pack a record of the number of ranges being packed.
 */
  ltmp[0] = regset->nrange;
  if(net_put_long(net, 1, ltmp))
    return 1;
/*
 * Pack the limits of each range.
 */
  for(range=regset->ranges; range; range=range->next) {
    ltmp[0] = range->ia;
    ltmp[1] = range->ib;
    if(net_put_long(net, 2, ltmp))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Unpack a register set from a network buffer into a given register set.
 *
 * Input:
 *  regset   RegSet *   The register-set to initialize from the network
 *                      buffer. clr_RegSet() is called before reading
 *                      the new set.
 *  net      NetBuf *   The network buffer from which to unpack the set.
 *                      Note it is left to the caller to bracket this
 *                      call with calls to net_start_get() and
 *                      net_end_get().
 * Output:
 *  return      int     0 - 'regset' now contains the register-set that
 *                          was encoded in 'net'.
 *                      1 - Error.  'regset' will be left cleared if
 *                          clr_RegSet() works.
 */
int net_get_RegSet(RegSet *regset, NetBuf *net)
{
  unsigned long tmp[3];   /* Used to send data to net_get_short() */
  RegSetRange *last;        /* The last range added to the register set */
  int nrange;               /* The number of ranges to be read */
  int i;
/*
 * Check arguments.
 */
  if(!net) {
    lprintf(stderr, "net_get_RegSet: NULL argument.\n");
    return 1;
  };
/*
 * Clear the input register set.
 */
  if(clr_RegSet(regset))
    return 1;
/*
 * Get the revision count of the remote register set.
 */
  {
    unsigned long revision;
    if(net_get_long(net, 1, &revision))
      return ngRS_error(regset);
    regset->revision = revision;
  };
/*
 * Unpack the record of the number of ranges.
 */
  if(net_get_long(net, 1, tmp))
    return ngRS_error(regset);
  nrange = tmp[0];
/*
 * Read the selected ranges of the current block and
 * record them in the register set.
 */
  last = NULL;
  for(i=0; i < nrange; i++) {
/*
 * Unpack the register start and end indexes and add the new range
 * to the list.
 */
    if(net_get_long(net, 2, tmp) ||
       (last=insert_RegSetRange(regset, last, tmp[0], tmp[1])) == NULL)
      return ngRS_error(regset);

  };
  return 0;
}

/*.......................................................................
 * Unpack a register set from a network buffer into a given register set.
 *
 * Input:
 *  regset   RegSet *   The register-set to initialize from the network
 *                      buffer. clr_RegSet() is called before reading
 *                      the new set.
 *  net      NetBuf *   The network buffer from which to unpack the set.
 *                      Note it is left to the caller to bracket this
 *                      call with calls to net_start_get() and
 *                      net_end_get().
 * Output:
 *  return      int     0 - 'regset' now contains the register-set that
 *                          was encoded in 'net'.
 *                      1 - Error.  'regset' will be left cleared if
 *                          clr_RegSet() works.
 */
int netGetRegSet(RegSet *regset, NetBuf *net)
{
  unsigned long ltmp[3];   /* Used to send data to net_get_short() */
  RegSetRange *last;        /* The last range added to the register set */
  int nrange;               /* The number of ranges to be read */
  int i;
/*
 * Check arguments.
 */
  if(!net) {
    lprintf(stderr, "net_get_RegSet: NULL argument.\n");
    return 1;
  };
/*
 * Clear the input register set.
 */
  if(clr_RegSet(regset))
    return 1;
/*
 * Get the revision count of the remote register set.
 */
  {
    unsigned long revision;
    if(net_get_long(net, 1, &revision))
      return ngRS_error(regset);
    regset->revision = revision;
  };
/*
 * Unpack the record of the number of ranges.
 */
  if(net_get_long(net, 1, ltmp))
    return ngRS_error(regset);
  nrange = ltmp[0];
/*
 * Read the selected ranges of the current block and
 * record them in the register set.
 */
  last = NULL;
  for(i=0; i < nrange; i++) {
/*
 * Unpack the register start and end indexes and add the new range
 * to the list.
 */
    if(net_get_long(net, 2, ltmp) ||
       (last=insert_RegSetRange(regset, last, ltmp[0], ltmp[1])) == NULL)
      return ngRS_error(regset);

  };
  return 0;
}

/*.......................................................................
 * This is a private error-return function used to clean up a regset
 * after an error occurs in net_get_RegSet().
 *
 * Input:
 *  regset   RegSet *  The register set to be sanitized.
 * Output:
 *  return      int    The error code of net_get_RegSet() [always 1].
 */
static int ngRS_error(RegSet *regset)
{
  (void) clr_RegSet(regset);
  return 1;
}

/*.......................................................................
 * Return the space needed to pack all possible registers into a
 * NetBuf network buffer.
 *
 * Input:
 *  regset   RegSet *  The set to be characterized.
 * Output:
 *  return     long    The number of bytes needed to pack a full
 *                     set of register ranges.
 */
long net_regs_size(RegSet *regset)
{
  return NET_LONG_SIZE +           /* The regset revision count */
    NET_SHORT_SIZE * 1 +           /* The count of selected elements */
    NET_LONG_SIZE * regset->nslot; /* The register-slot values */
}

/*.......................................................................
 * Pack a set of registers for network transmission.
 *
 * Input:
 *  regset     RegSet *   The set of registers in raw[] to be packed.
 *  raw    RegRawData *   The array of monitor registers to be packed.
 *  net        NetBuf *   The network buffer in which to pack the selected
 *                        set of registers. Note that it is left to the
 *                        caller to bracket this call with calls to
 *                        net_start_put() and net_end_put().
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
int netPutRegs(RegSet *regset, RegRawData* raw, NetBuf *net)
{
  RegSetRange *range; /* The range being packed */
  
  // Check arguments.

  if(!regset || !raw || !net) {
    lprintf(stderr, "net_put_regs: NULL argument.\n");
    return 1;
  };

  if(regset->nByte(raw->fm->archivedOnly()) != raw->fm->sizeInBytesOfData()) {
    lprintf(stderr, "netPutRegs: Incompatible regset and data frame.\n");
    return 1;
  };
  
  // Record the revision count of the register set.

  {
    unsigned long revision = regset->revision;
    if(net_put_long(net, 1, &revision))
      return 1;
  };
  
  // Record the number of registers that are being packed.

  {
    unsigned short nByteSelected = regset->nByteSelected;
    if(net_put_short(net, 1, &nByteSelected))
      return 1;
  };
  
  // Pack the selected ranges of elements into the network buffer.

  for(range = regset->ranges; range; range=range->next) {

    unsigned offset = raw->fm->byteOffsetInFrameOfData() + range->ia;

    if(net_put_char(net, range->ib - range->ia + 1, 
		    raw->fm->frame()->getUcharPtr(offset)))
      return 1;
  };
  return 0;
}

int netPutRegs(RegSet *regset, sza::util::NetMonitorFrame* nmf, NetBuf *net)
{
  RegSetRange *range; /* The range being packed */
  
  // Check arguments.

  if(!regset || !nmf || !net) {
    lprintf(stderr, "net_put_regs: NULL argument.\n");
    return 1;
  };

  if(regset->nByte(nmf->nadfm_.archivedOnly()) != nmf->nadfm_.sizeInBytesOfData()) {

    COUT(nmf->nadfm_.archivedOnly());

    COUT(regset->nByte(nmf->nadfm_.archivedOnly()));

    COUT(nmf->nadfm_.sizeInBytesOfData());

    COUT(regset->nByte(false));
    COUT(regset->nByte(true));

    lprintf(stderr, "netPutRegs: Incompatible regset and data frame.\n");
    return 1;
  };
  
  // Record the revision count of the register set.

  {
    unsigned long revision = regset->revision;
    if(net_put_long(net, 1, &revision))
      return 1;
  };
  
  // Record the number of registers that are being packed.

  {
    unsigned short nByteSelected = regset->nByteSelected;
    if(net_put_short(net, 1, &nByteSelected))
      return 1;
  };
  
  // Pack the selected ranges of elements into the network buffer.

  for(range = regset->ranges; range; range=range->next) {

    unsigned offset = nmf->nadfm_.byteOffsetInFrameOfData() + range->ia;

    if(net_put_char(net, range->ib - range->ia + 1, 
		    nmf->nadfm_.frame()->getUcharPtr(offset)))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Pack a set of registers for network transmission.
 *
 * Input:
 *  regset     RegSet *   The set of registers in raw[] to be packed.
 *  raw    RegRawData *   The array of monitor registers to be packed.
 *  net        NetBuf *   The network buffer in which to pack the selected
 *                        set of registers. Note that it is left to the
 *                        caller to bracket this call with calls to
 *                        net_start_put() and net_end_put().
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
int net_put_regs(RegSet *regset, RegRawData *raw, NetBuf *net)
{
  RegSetRange *range; /* The range being packed */
  
  // Check arguments.

  if(!regset || !raw || !net) {
    lprintf(stderr, "net_put_regs: NULL argument.\n");
    return 1;
  };
  if(regset->nslot != raw->nslot) {
    lprintf(stderr, "net_put_regs: Incompatible regset (%d) and raw (%d) args.\n", regset->nslot, raw->nslot);
    return 1;
  };
  
  // Record the revision count of the register set.

  {
    unsigned long revision = regset->revision;
    if(net_put_long(net, 1, &revision))
      return 1;
  };
  
  // Record the number of registers that are being packed.

  {
    unsigned short nselected = regset->nselected;
    if(net_put_short(net, 1, &nselected))
      return 1;
  };
  
  // Pack the selected ranges of elements into the network buffer.

  for(range = regset->ranges; range; range=range->next) {

    if(net_put_long(net, range->ib - range->ia + 1, 
		    (long unsigned int*)(raw->slots + range->ia)))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Unpack a set of registers from a network buffer.
 *
 * Input:
 *  regset       RegSet *  The set of registers to retrieve.
 *  raw      RegRawData *  The output array for the register values.
 *  net          NetBuf *  The network buffer containing the packed
 *                         set of registers. Note that it is left to the
 *                         caller to bracket this call with calls to
 *                         net_start_get() and net_end_get().
 * Output:
 *  return  NetRegState    The register extraction status, from:
 *                          NETREG_OK    - Registers read OK.
 *                          NETREG_SKIP  - Registers of legacy regset ignored.
 *                          NETREG_ERROR - Error reading registers.
 */
NetRegState netGetRegs(RegSet* regset, RegRawData* raw, NetBuf *net)
{
  RegSetRange *range; // The element range that is being unpacked 

  // Check arguments.

  if(!regset || !raw || !net) {
    lprintf(stderr, "net_get_regs: NULL argument.\n");
    return NETREG_ERROR;
  };

  if(regset->nByte(raw->fm->archivedOnly()) != raw->fm->sizeInBytesOfData()) {
    lprintf(stderr, "netGetRegs: Incompatible regset and data frame.\n");
    return NETREG_ERROR;
  };
  
  // Read the revision count of the register set.

  {
    unsigned long revision;
    if(net_get_long(net, 1, &revision))
      return NETREG_ERROR;
    
    // Check the remote regset revision count against that of the
    // local regset.

    if(revision != regset->revision) {
      
      // If the remote register-set revision count is earlier than the
      // current revision count, ignore the call. This corresponds to
      // the case where we have sent a revised regset while a
      // previously packed set of registers is still to be received.

      if(revision < regset->revision) {
	return NETREG_SKIP;
      } else {
	lprintf(stderr, "netGetRegs: Inconsistent revision count.\n");
	lprintf(stderr, "revision=%lu regset->revision=%u.\n",
		revision, regset->revision);
	return NETREG_ERROR;
      };
    };
  };
  
  // Read the number of bytes that are to be unpacked.

  {
    unsigned short nByteSelected;
    if(net_get_short(net, 1, &nByteSelected))
      return NETREG_ERROR;
    
    // Check the the specified register set contains the same number
    // of register slots as the register set that was used to pack the
    // registers into the network buffer.

    if(nByteSelected != regset->nByteSelected) {
      lprintf(stderr, "netGetRegs: Incompatible register set.\n");
      lprintf(stderr, "nselected=%u regset->nselected=%u.\n",
	      nByteSelected, regset->nByteSelected);
      return NETREG_ERROR;
    };
  };
  
  // Unpack the selected ranges of registers from the network buffer
  // and into the raw register array.

  for(range = regset->ranges; range; range=range->next) {

    unsigned offset = raw->fm->byteOffsetInFrameOfData() + range->ia;

    if(net_get_char(net, range->ib - range->ia + 1, 
		    raw->fm->frame()->getUcharPtr(offset)))
      return NETREG_ERROR;
  };
  return NETREG_OK;
}

/*.......................................................................
 * Unpack a set of registers from a network buffer.
 *
 * Input:
 *  regset       RegSet *  The set of registers to retrieve.
 *  raw      RegRawData *  The output array for the register values.
 *  net          NetBuf *  The network buffer containing the packed
 *                         set of registers. Note that it is left to the
 *                         caller to bracket this call with calls to
 *                         net_start_get() and net_end_get().
 * Output:
 *  return  NetRegState    The register extraction status, from:
 *                          NETREG_OK    - Registers read OK.
 *                          NETREG_SKIP  - Registers of legacy regset ignored.
 *                          NETREG_ERROR - Error reading registers.
 */
NetRegState net_get_regs(RegSet *regset, RegRawData *raw, NetBuf *net)
{
  RegSetRange *range; // The element range that is being unpacked 
  
  // Check arguments.

  if(!regset || !raw || !net) {
    lprintf(stderr, "net_get_regs: NULL argument.\n");
    return NETREG_ERROR;
  };
  if(regset->nslot != raw->nslot) {
    lprintf(stderr, "net_get_regs: Incompatible regset and raw args.\n");
    return NETREG_ERROR;
  };
  
  // Read the revision count of the register set.

  {
    unsigned long revision;
    if(net_get_long(net, 1, &revision))
      return NETREG_ERROR;
    
    // Check the remote regset revision count against that of the
    // local regset.

    if(revision != regset->revision) {
      
      // If the remote register-set revision count is earlier than the
      // current revision count, ignore the call. This corresponds to
      // the case where we have sent a revised regset while a
      // previously packed set of registers is still to be received.

      if(revision < regset->revision) {
	return NETREG_SKIP;
      } else {
	lprintf(stderr, "net_get_regs: Inconsistent revision count.\n");
	lprintf(stderr, "revision=%lu regset->revision=%u.\n",
		revision, regset->revision);
	return NETREG_ERROR;
      };
    };
  };
  
  // Read the number of registers that are to be unpacked.

  {
    unsigned short nselected;
    if(net_get_short(net, 1, &nselected))
      return NETREG_ERROR;
    
    // Check the the specified register set contains the same number
    // of register slots as the register set that was used to pack the
    // registers into the network buffer.

    if(nselected != regset->nselected) {
      lprintf(stderr, "net_get_regs: Incompatible register set.\n");
      lprintf(stderr, "nselected=%u regset->nselected=%u.\n",
	      nselected, regset->nselected);
      return NETREG_ERROR;
    };
  };
  
  // Unpack the selected ranges of registers from the network buffer
  // and into the raw register array.

  for(range = regset->ranges; range; range=range->next) {
    if(net_get_long(net, range->ib - range->ia + 1, 
		    (long unsigned int*)(raw->slots + range->ia)))
      return NETREG_ERROR;
  };
  return NETREG_OK;
}

/*.......................................................................
 * Duplicate the contents of register set in another register set.
 * The two register sets must have been allocated wrt the same register
 * map.
 *
 * Input:
 *  into     RegSet *   The register set to copy into.
 *  from     RegSet *   The register set to duplicate.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int dup_RegSet(RegSet *into, RegSet *from)
{
  RegSetRange *into_range = NULL;  /* The last register range added */
  RegSetRange *range;              /* The range being copied */
/*
 * Check that the two register sets are compatible.
 */
  if(into->nslot != from->nslot) {
    lprintf(stderr, "dup_RegSet: Incompatible register sets.\n");
    return 1;
  };
/*
 * Clear the destination register set.
 */
  if(clr_RegSet(into))
    return 1;
/*
 * Copy the input ranges into the output register set.
 */
  for(range = from->ranges; range; range=range->next) {
    into_range = insert_RegSetRange(into, into_range, range->ia, range->ib);
    if(!into_range)
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Determine whether two register sets are equivalent.
 *
 * Input:
 *  regset1  RegSet *   The first of the register sets to compare.
 *  regset2  RegSet *   The second of the register sets to compare.
 * Ouput:
 *  return      int     0 - The two register sets are not equivalent.
 *                      1 - The two register sets are equivalent.
 */
int equiv_RegSet(RegSet *regset1, RegSet *regset2)
{
  RegSetRange *range1;   /* An element selection range from regset1 */
  RegSetRange *range2;   /* An element selection range from regset2 */

/*
 * Two NULL register sets are counted as being equivalent.
 */
  if(!regset1 || !regset2)
    return regset1 == regset2;
/*
 * See if the number of archive slots or ranges differ.
 */
  if(regset1->nslot != regset2->nslot || regset1->nrange != regset2->nrange)
    return 0;
/*
 * Compare the ranges in each each of the ranges.
 */
  for(range1=regset1->ranges, range2=regset2->ranges;
      range1 && range2;
      range1=range1->next, range2=range2->next) {
    if(range1->ia != range2->ia || range1->ib != range2->ib)
      return 0;
  };
/*
 * Check that the ends of both lists of ranges were reached.
 */
  if(range1 || range2)
    return 0;
/*
 * No differences were found.
 */
  return 1;
}

/*.......................................................................
 * Return the head of the list of selected element ranges.
 *
 * Input:
 *  regset       RegSet *  The register selection set.
 * Output:
 *  return  RegSetRange *  The head of the list of ranges.
 */
RegSetRange *regset_range_list(RegSet *regset)
{
  return regset ? regset->ranges : NULL;
}

/*.......................................................................
 * Return the register map size that a RegSet object was allocated
 * for.
 *
 * Input:
 *  regset    RegSet *  The register selection set to characterize.
 * Output:
 *  return  unsigned    The number of slots in the register map that
 *                      was passed to new_RegSet().
 */
unsigned size_RegSet(RegSet *regset, bool archivedOnly)
{
  return regset ? (archivedOnly ? regset->nArcSlot : regset->nslot) : 0;
}

/*.......................................................................
 * Return true if a given register specification is already included
 * in the specified register set.
 *
 * Input:
 *  regset    RegSet *  The register selection set to characterize.
 *  reg    RegMapReg *  The register specification to check for.
 * Output:
 *  return       int    0 - The specification is not covered by the set.
 *                      1 - The specification is in the set.
 */
int in_RegSet(RegSet *regset, RegMapReg *reg)
{
  RegSetRange *range; /* The element range being checked */
  int ia, ib;         /* The range of register map elements to check for */
/*
 * Check the arguments.
 */
  if(!regset || !reg) {
    lprintf(stderr, "add_RegSetRange: NULL argument(s).\n");
    return 1;
  };
/*
 * Determine the range of elements to be added to the set.
 */
  ia = reg->slot;
  ib = reg->slot + reg->nreg * reg->size - 1;
/*
 * Widen the selected range to meet alignment and atomicity constraints.
 */
  if(reg->size > 1) {
    int i0 = reg->slot - reg->index; /* The index of the first block element */
    ia -= (ia-i0) % reg->size;
    ib += (reg->size - 1) - ((ib-i0) % reg->size);
  };
  if(ia < 0 || ib >= (int)regset->nslot) {
    lprintf(stderr, "in_RegSet: The index selection is out of range.\n");
    return 1;
  };
/*
 * Find the first element range that includes the start index.
 */
  for(range=regset->ranges; range && ia > range->ib; range=range->next)
    ;
/*
 * Was a contiguous range found that encompases the required range
 * of elements?
 */
  return (range && ib <= range->ib) ? 1 : 0;
}
