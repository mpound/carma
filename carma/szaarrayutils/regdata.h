#ifndef regdata_h
#define regdata_h

#include "carma/szaarrayutils/arraymap.h"
#include "carma/szautil/ArrayDataFrameManager.h"
#include "carma/szautil/PagerMonitor.h"

/*
 * Archived data on disk are recorded as big-endian 2-s complement 4-byte
 * integers. In memory the same data is recorded in a container of the
 * following type. RegRawData containers are allocated based on the size
 * of the register map that is supplied to their constructor. The size
 * of the slots[] array is equal to the number of archived registers in
 * the register map.
 */
typedef struct {
  unsigned nslot;    /* The number of elements in slots[] */
  long *slots;       /* The array of monitored registers */
  sza::util::ArrayDataFrameManager* fm;
  sza::util::PagerMonitor* pm;
} RegRawData;

/*
 * Create a raw data container for a given register map.
 * This should not be used with any other register map.
 */
RegRawData *new_RegRawData(ArrayMap *arraymap, bool archivedOnly);

/*
 * Delete a redundant raw-data container.
 */
RegRawData *del_RegRawData(RegRawData *raw);

/*
 * The following functions allow one to pass the contents of
 * a RegRawData structure across a network (or equivalent byte-stream).
 *
 * net_RegRawData_size()
 *  returns the number of bytes needed to pack the contents of a
 *  given RegRawData object into a network buffer.
 *
 * net_put_RegRawData()
 *  packs the contents of a RegRawData container into a network
 *  buffer.
 *
 * net_get_RegRawData()
 *  unpacks the contents of a RegRawData container from a network
 *  buffer.
 */
long net_RegRawData_size(RegRawData *raw);
int net_put_RegRawData(sza::array::NetBuf *net, RegRawData *raw);
int net_get_RegRawData(sza::array::NetBuf *net, RegRawData *raw);

#endif
