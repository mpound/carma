#ifndef CARMA_UTIL_IPQBUFFER_H
#define CARMA_UTIL_IPQBUFFER_H

/**
 * @file
 * IPQ (InterProcessQueue) provides a generic way for information to be
 * shared between processes or threads.
 * This class provides access for a shared memory implementation of
 * the generic base class.
 *
 * Original author: Steve Scott
 *                  24 Apr, 2003
 *
 */

#include "carma/util/IPQbufferBase.h"

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/uio.h>


namespace carma {
    namespace util {


/**
 * Shared memory storage mechanism for an IPQ buffer.
 * Once the shared memory is formed it persists as a file in /dev/shm.
 * It can be manually deleted if so desired (say, if it corrupted).
 * On Linux 2.4 kernels it can have a maximum size of one half of physical memory.
 *
 * This class is a  base class for reader and writer classes.
 * See for e.g.
 * @see IPQreader, IPQwriter
 */
class IPQbuffer: public IPQbufferBase {
protected:
    /**
    **  Constructor
    **  @param localElement address of the data buffer for reads/writes
    **  @param elementSize of an individual queue element in bytes
    **  @param filename Shared memory filename.
    **         Must start with '/' and be less than 15 chars long.
    **  @param isCreator If true, create a new file if one doesn't exist,
    **              and make its size match nElements
    **  @param nElements Number of elements to allocate (queue length);
    **             ignored if not a creator.
    **  @param testOffset When shared memory is created, set the putoffset
    **             to have testOffset full queues before it wraps around.
    **             Obviously for testing wrap around; 
    **             zero (default) inhibits this feature.
    **  @throw std::exception
    */
    IPQbuffer( void *                localElement,
               int                   elementSize,
               const ::std::string & filename,
               bool                  isCreator = false,
               int                   nElements = 0,
               unsigned int          testOffset = 0 );

public:
    /**
     * Destructor
     */
    virtual ~IPQbuffer() ;

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

#endif  // CARMA_UTIL_IPQBUFFER_H
