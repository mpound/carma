#ifndef CARMA_UTIL_IPQBUFFERBASE_H
#define CARMA_UTIL_IPQBUFFERBASE_H

/**
 * @file
 * IPQ (InterProcessQueue) provides a generic way for information to be
 * shared between processes or threads.
 * This class holds the basic buffer and all of the access methods.
 *
 * @author Steve Scott
 *         08 Oct, 2002
 *         Based on MemoryMappedFile and CrossProcessBuffer
 *         by Scott Brumbaugh, circa 1999
 *
 * $CarmaCopyright$
 *
 */

#include "carma/util/IPQinterface.h"

#include <string>

namespace carma {
    namespace util {

    // Forward declaration
    class PthreadRWLock;
    class ScopedFlockManager;

/**
 *
 * IPQ (InterProcessQueue) provides a generic way for information to be
 * shared between processes or threads. The queues are fixed in length
 * (number of elements) and width (element size). The queues may be
 * transient in shared memory or persistent in memory mapped files with
 * these implementations provides by classes that implement the abstract
 * methods of this class.
 * This buffer class provides the actual storage mechanism.
 * It also provides read and write methods to be inherited
 * by the derived classes.
 * This class throws exceptions, in particular on the constructor and the
 * write methods, so make sure you catch them!
 * Because this is a circular queue, if the reader falls more than the
 * queue depth behind the writer then elements will be overwritten that
 * can never be read and are lost forever. The read methods returns the
 * number of lost elements.
 * <BR>
 * Note that each instance of reader has its own set of pointers to keep
 * track of where it is in the queue. A single instance shared by multiple
 * threads has this feature, so it may only be useful if one of the threads
 * is designated as the master reader. An exception is if the threads are
 * just reading the most recent element off the top of the queue, in other
 * words not really using the queue feature but more as a data sharing 
 * mechanism between writers and readers.
 *
 * <BR>
 * Some facts about Linux interactions with this class.
 * <UL>
 * <LI> Shared memory is limited in size to half the physical memory size.
 * <LI> Shared memory exists in /dev/shm and can be deleted just
 * like any file.
 * <LI> If you are using large objects (structures) for the elements,
 * you can run into limitations caused by stack size.
 * Increase stacksize with:
 * <UL>
 * <LI> csh(limit stacksize &lt;sizeKB&gt;) or
 * <LI> sh(ulimit [-S][-H] -s &lt;sizeKB&gt;
 * </UL>
 * <LI> Shared memory is much faster than a memory mapped file.
 * <LI> Memory mapped files off a fileserver are very slow.
 * <LI> Memory mapped files are limited to 2GB in size.
 * </UL>
 *
 *
 * @see IPQreader, IPQwriter
 */
class IPQbufferBase : public carma::util::IPQinterface {
protected:
    /**
    **  Constructor for a read/write queue.
    **  The read pointer is placed at the bottom of the queue.
    **  @param localElement address of the data buffer for reads/writes
    **  @param elementSize of an individual queue element in bytes
    **  @param filename Memory mapped or shared memory filename.
    **         Note that if a persistent file is used, it must be on a
    **         a native file system and not on an NFS mounted filesystem.
    **         On Linux the open will work, but information is not shared
    **         between processes, so you will just hang at some point.
    **  @param isCreator Controls file creation
    **              If true, create a new file, if one doesn't exist,
    **              and make its size match nElements
    **  @param nElements Number of elements to allocate (queue length);
    **             ignored if not a creator. Default is zero.
    **  @param testOffset When shared memory is created, set the putoffset
    **             to have testOffset full queues before it wraps around.
    **             Obviously for testing wrap around; 
    **             zero (default) inhibits this feature.
    **  @throw std::exception
    */
    IPQbufferBase( void *                localElement,
                   int                   elementSize,
                   const ::std::string&  filename,
                   bool                  isCreator = false,
                   int                   nElements = 0,
                   unsigned int          testOffset = 0 );
public:
    /**
     * Destructor
     * Making this d'tor virtual causes the d'tors of inheriting classes
     * to be called first when the d'tor is called on this base class
     * (polymorphic), followed by the base class d'tor.
     */
    virtual ~IPQbufferBase() ;

    /**
    ** Get the filename
    */
    ::std::string getFileName( ) const;

    /**
    ** Returns the allocated size of the queue.
    ** The queue may not be full, and some of it may be already read.
    ** @return the size of the queue (but it may not be full!)
    ** @see getNumAvailable()
    **/
    int getQueueSize() const ;

    /**
    ** Returns the size of each element in the queue in bytes.
    ** @return the size of each element in the queue in bytes.
    **/
    int getElementSize() const ;

    /**
    ** Gets the number of unread elements available in the queue
    ** @return the number of unread elements in the queue
    ** @see getQueueSize()
    **/
    int getNumAvailable() const ;

    /**
    ** Checks to see if the queue is empty
    ** @return true if the queue is empty
    **/
    bool isEmpty() const ;

    /**
    ** Checks to see if there are any unread elements
    ** (would a read not block?).
    ** @return true if a read would not block
    **/
    bool isDataAvailable() const;

    /**
    ** Sets the number of available (unread) elements to zero (catches up)
    **/
    void setNoneAvailable();

    /**
    ** Read the oldest unread element from the queue.
    ** This is a blocking read that will wait for new data if there is
    ** no unread data available.
    ** This method does a lock/copy/unlock on the queue.
    ** At the completion of the read the data is copied into the
    ** localElement buffer
    ** @return number of elements lost in the queue that can
    ** never be retrieved.
    ** @throw std::exception
    **/
    unsigned int read();


private:

    /**
    ** Get the oldest unread element from the queue without locking
    ** out other access. This is a blocking operation.
    ** No locking is done to prevent this routine from blocking a higher
    ** priority writer if this gets swapped out mid-transfer.
    ** It should be used when there is no chance of entanglement with
    ** the write pointer; e.g. when the reading is caught up and the queue
    ** is sufficiently large that there is no chance of wrap around before
    ** this read would be completed. This is not an uncommon circumstance.
    ** At the completion of this method the data in the queue is
    ** copied into the localElement buffer.
    ** @return number of elements lost in the queue that can
    ** never be retrieved.
    ** @throw std::exception
    ** @see read()
    **/
    unsigned int readNoLock();

public:

    /**
    ** Get the newest element from the queue (with locking).
    ** This is not a blocking operation.
    ** If the queue is empty false is returned, otherwise true.
    ** At the completion of this method, no elements will be unread
    ** and the data from the most recent element will be copied
    ** into the localElement buffer.
    ** @return true if the queue is not empty
    ** @throw std::exception
    **/
    bool readNewest();
    
    /**
    ** Reads and copies the newest element from the queue into
    ** the localElement buffer, but only if has not been 
    ** previously read. This method does not block.
    ** If the queue is empty no data are copied and false is returned.
    ** This method does a lock/copy/unlock on the queue if there is
    ** an unread element.
    ** @return true if data are copied
    ** @throw std::exception
    **/
    bool readNewestConditionalCopy();
    
private:

    /**
    ** Get the newest element from the queue without locking.
    ** This is not a blocking operation.
    ** If the queue is empty false is returned, otherwise true.
    ** At the completion of this method, no elements will be unread.
    ** Very useful for getting the current state of the element.
    ** @return true if the queue is not empty
    ** @throw std::exception
    **/
    bool readNewestNoLock();

public:

    /**
    ** Get the number of elements lost on the last read.
    ** If the writer gets too far ahead of the reader it can overwrite
    ** unread elements making them lost forever.
    ** @return Number of elements lost on last read
    **/
    unsigned int getLostElementCount() const;

    /// Used internally, but made public for debugging
    unsigned int getPutOffset() const ;
    /// Useful only for debugging
    unsigned int getGetOffset() const ;
    /// Useful only for debugging
    unsigned int getMaxOffset() const ;

protected:

    /**
     * This does all the real constructor work.
     * Mandatory: the child class *must* call this in its constructor.
     * @throw std::exception
     */
    void init() ;

    /**
     * Put an element into the queue.
     * This method does a lock/insert/unlock on the queue.
     * @throw std::exception
     */
    void write() ;

    /**
     * Open an existing file or shared memory.
     * The internal variable fileDescriptor must contain the descriptor
     * to the open file after this call.
     * Follow the return convention.
     * Don't throw an exception, the next layer up takes care of that.
     * @return true if file open, false if not
     */
    virtual bool openBuffer() = 0;

    /**
     * Create a new file or shared memory.
     * The internal variable fileDescriptor must contain the descriptor
     * to the open file after this call.
     * Follow the return convention.
     * Don't throw an exception, the next layer up takes care of that.
     * @return true if file open, false if not
     */
    virtual bool createBuffer() = 0;

   /**
    * Shared memory filenames must contain only one '/' and
    * it must be the first character; this routine fixes up the filename.
    */
    void trimShmemFilename();

    /**
     * Get the trimmed filename
     */
    ::std::string getTrimmedFilename();

private:
    void copyMemory( void * const       dest,
                     const void * const src,
                     const size_t       bytes ) const;

    /**
    * Read in the header and put it in our local buffer.
    * @param keepOpen if there is an error
    * @throws ErrorException if the header is not the correct size or
    * if it doesn't contain a magic number
    */
    bool readHeader(bool keepOpen) ;

    /**
    * Check that element size of existing IPQ matches requested size
    */
    bool doesElementSizeMatch() ;

    /**
    * Compare existing IPQ sizes with requested
    */
    bool hasQueueChanged() ;

    /**
    * Write local header buffer into the file
    */
    bool writeHeader() ;

    /**
     * Create semaphore filename from input filename.
     * The semaphore filename is created the same regardless of what kind of
     * shared memory we are creating.  However, it is still derived from the
     * input filename passed in on construction.
     */
    void createSemFilename();

    /**
     * Open existing write semaphore.
     */
    bool openSemaphore();

    /**
     * Create a new semaphore.
     */
    bool createSemaphore();
    
    // Internal routine that handles wraparound.
    // Will lock the flock manager if the read/write pointers are adjacent
    // returns adjusted readoffset
    unsigned int adjustReadOffset(bool& lock, ScopedFlockManager& flockManager, 
        unsigned int readOffset);

    unsigned int   internalRead(bool lock) ;
    bool           internalReadNewest(bool lock) ;

private:
    class BlockedOnSemaphoreQuitRequestHandler;

    friend class BlockedOnSemaphoreQuitRequestHandler;

    static const int MEMORYMAPPEDFILE_MAGIC = 0x00000100;

    struct ControlBlock {
        // An index for the next element to be written.
        // Steadily increments through the full range of a long,
        // with modulos used to put it into the finite queue
        volatile unsigned int putOffset;
    };

    struct MapfileHeader {
        int          magic;
        unsigned int headerSize;
        unsigned int controlBlockOffset;
        unsigned int controlBlockSize;
        // Offset in bytes from the start of the shmem to start of queue area
        unsigned int queueOffset;
        // The size of a single element in the queue, in bytes 
        unsigned int queueElementSize; 
        // The number of elements allocated for the queue
        unsigned int queueElements;
    };

protected:
    const int protectionMask_;
    const int openMask_;

private:
    const ::std::string filename_;         // As input

    // The local copy of the data in an element.
    // It has been read from the queue and placed here - or it is
    // the copy that will be transferred into the queue on writes.
    void * const        localElement_;
    const size_t        localElementSize_;
    const bool          isCreator_;

protected:
    bool                debug_;
    int                 fileDescriptor_;

private:
    int                 semDescriptor_;  // Semaphore descriptor...

    void*               mmapAddr_;
    size_t              mmapSize_;

    size_t                 controlBlockOffset_;
    volatile ControlBlock* controlBlock_;

    // Offset in bytes from the start of the shmem area to start of queue area
    size_t              queueOffset_;
    // Size of an element in bytes
    size_t              queueElementSize_;
    // Number of elements allocated for the queue
    unsigned int        queueElements_;
    char*               queue_; // Absolute pointe to beginning of the queue

    bool                isNewFile_;
    bool                isFileOpen_;

    // Offset where next data will be read
    unsigned int        getOffset_;
    unsigned int        nLostElements_;

    ::std::string       trimmedFilename_;
    ::std::string       displayFilename_;
    ::std::string       semFilename_;      // Semaphore filename

    MapfileHeader       header_;
    PthreadRWLock&      rwLock_;
    unsigned int        maxOffset_;  // Max for put/get pointers (wraparound)
    // To help resolve emptiness at wraparound time
    bool                empty_;
    unsigned int        testOffset_;
};

} }  // End namespace carma::util

#endif  // CARMA_UTIL_IPQBUFFERBASE_H
