#ifndef CARMA_UTIL_IPQINTERFACE_H
#define CARMA_UTIL_IPQINTERFACE_H

#include <string>

namespace carma {
namespace util {

class IPQinterface {
public:
    
    /**
     * Destructor
     * Making this d'tor virtual causes the d'tors of inheriting classes
     * to be called first when the d'tor is called on this base class
     * (polymorphic), followed by the base class d'tor.
     */
    virtual ~IPQinterface() = 0; // Must provide empty definition (in cc).
    
    /**
    ** Get the filename
    */
    virtual ::std::string getFileName( ) const = 0;

    /**
    ** Returns the allocated size of the queue.
    ** The queue may not be full, and some of it may be already read.
    ** @return the size of the queue (but it may not be full!)
    ** @see getNumAvailable()
    **/
    virtual int getQueueSize() const = 0;

    /**
    ** Returns the size of each element in the queue in bytes.
    ** @return the size of each element in the queue in bytes.
    **/
    virtual int getElementSize() const = 0;

    /**
    ** Gets the number of unread elements available in the queue
    ** @return the number of unread elements in the queue
    ** @see getQueueSize()
    **/
    virtual int getNumAvailable() const = 0;

    /**
    ** Checks to see if the queue is empty
    ** @return true if the queue is empty
    **/
    virtual bool isEmpty() const = 0;

    /**
    ** Checks to see if there are any unread elements
    ** (would a read not block?).
    ** @return true if a read would not block
    **/
    virtual bool isDataAvailable() const = 0;

    /**
    ** Sets the number of available (unread) elements to zero (catches up)
    **/
    virtual void setNoneAvailable() = 0;

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
    virtual unsigned int read() = 0;

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
    virtual bool readNewest() = 0;
    
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
    virtual bool readNewestConditionalCopy() = 0;

    /**
    ** Get the number of elements lost on the last read.
    ** If the writer gets too far ahead of the reader it can overwrite
    ** unread elements making them lost forever.
    ** @return Number of elements lost on last read
    **/
    virtual unsigned int getLostElementCount() const = 0;

    virtual unsigned int getPutOffset() const = 0; /// Useful only for debugging
    virtual unsigned int getGetOffset() const = 0; /// Useful only for debugging
    virtual unsigned int getMaxOffset() const = 0; /// Useful only for debugging

protected:
    
    /**
     * Put an element into the queue.
     * This method does a lock/insert/unlock on the queue.
     * @throw std::exception
     */
    virtual void write() = 0;
    
private:
}; // class IPQinterface

}} // namespace carma::util

#endif
