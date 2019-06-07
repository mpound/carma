#ifndef CARMA_UTIL_IPQFILEBUFFER_H
#define CARMA_UTIL_IPQFILEBUFFER_H

/**
 * @file
 * IPQ (InterProcessQueue) provides a generic way for information to be
 * shared between processes or threads.
 * This class provides access for a persistent memory file
 * implementation of the generic base class.
 *
 * Original author: Steve Scott
 *                  24 Apr, 2003
 *
 */

#include "carma/util/IPQbufferBase.h"

namespace carma {
namespace util {


/**
 * Shared memory storage mechanism. for an IPQ buffer.
 *
 * This class is a  base class for reader and writer classes.
 * See for e.g.
 * @see IPQfileReader, IPQfileWriter
 */
class IPQfileBuffer: public IPQbufferBase {
protected:
    /**
    **  Constructor for persistent queue.
    **  @param localElement address of the data buffer for reads/writes
    **  @param elementSize of an individual queue element in bytes
    **  @param filename Memory mapped filename.
    **             File must be on a native file system and not
    **             on an NFS mounted filesystem, or extremely poor
    **             performance will result.
    **  @param isCreator If true, create a new file if one doesn't exist,
    **              and make its size match nElementsuse.
    **  @param nElements Number of elements to allocate (queue length);
    **             ignored if not a creator.
    **  @throw std::exception
    */
    IPQfileBuffer( void *                localElement,
                   int                   elementSize,
                   const ::std::string & filename,
                   bool                  isCreator = false,
                   int                   nElements = 0,
                   unsigned int          testOffset = 0);

public:


private:
    /**
     * Open an existing shared memory buffer
     */
    virtual bool openBuffer();

    /**
     * Create a new shared memory buffer
     */
    virtual bool createBuffer();
};


} } // End of namespace carma::util


#endif  // CARMA_UTIL_IPQFILEBUFFER_H
