#ifndef CARMA_UTIL_IPQWRITER_H
#define CARMA_UTIL_IPQWRITER_H

/**
 * @file
 * @author: Steve Scott
 *          02 Oct, 2002
 *
 */



#include "carma/util/IPQbuffer.h"

namespace carma {
namespace util {


/**
 *
 * IPQ (InterProcessQueue) provides a generic way for information to be
 * shared between processes or threads. The queues are fixed in length
 * (number of elements) and width (element size). These queues are
 * non-persistent shared memory.
 *
 * A templatized class (or structure) is intimately associated with the
 * queue. All writes are done from the local copy, and all reads go into it.
 * Note that all methods of the input class become part of the methods of
 * of the IPQ class, effectively adding the queue methods to the class.
 * The queue storage is simple, with fixed memory allocation, so the input
 * class should have no pointers - just memory. Hierarchies are allowed,
 * as long as it is done with composition and not pointers. Methods
 * have no effect on the queue storage (it may be dangerous to use pointers to
 * the internal data because it is going to get moved around).
 *
 * ***** DANGER *****
 * ** Any class that is virtual (polymorphic = has a vtable = overrides
 * ** a virtual function) will screw up in subtle ways because
 * ** the pointer to the writer's vtable will get copied into
 * ** the reader's object -> not what you want!
 * *****************
 *
 * The queues have independent read and write pointers. The write pointers
 * are an intrinsic part of each queue, but the read pointers are unique
 * to each instance of a reader. The read pointer is initialized in the
 * constructor to point to the oldest data in the queue. A writer is also
 * a reader.
 *
 * The constructors can throw exceptions - make sure that you catch them!
 *
 * Example of use:
 * <PRE>
 * Class X {
 * public:
 *     double getA() { return A; }
 *     int    getB() { return B; }
 *     char   getC() { return C; }
 *     void   setB(int b) { B=b; }
 * private:
 *     char   C;
 *     double A;
 *     int    B;
 * };
 *
 * // Make a queue that can contain 100 X's
 * try {
 *    IPQwriter<X> Xwriter("a.ipq", 100);
 * } catch (const Error & e) { cout<<e; exit(); }
 * Xwriter.setB(123);  // Mess with X object
 * Xwriter.write();    // Write current X into queue
 * Xwriter.setB(456);
 * Xwriter.write();    // Write again
 * Xwriter.read()      // Read the first X from queue (into internal X)
 * cout<<Xwriter.getB()<<endl;  // Will print '123'
 *
 * </PRE>
 *
 *
 */

template < class E >
class IPQwriter : public IPQbuffer, public E {
public:

    /**
    **  Constructor for a writer that can create a queue.
    **  If one already exists that matches the call, then it is used.
    **  But if the existing file is not big enough,
    **  the file will be removed and recreated.
    **  Note that an IPQwriter is also a reader and has all of the
    **  methods of a reader.
    **
    **  @param filenameString Shared memory filename
    **  @param isCreator Controls file creation
    **         If true, create a new file, if one doesn't exist,
    **         and make its size match nElements
    **  @param nElements Number of elements to allocate (queue length);
    **         ignored if not a creator.
    **  @param testOffset When shared memory is created, set the putoffset
    **             to have testOffset full queues before it wraps around.
    **             Obviously for testing wrap around; 
    **             zero (default) inhibits this feature.
    **  @throw std::exception
    **  @see IPQbuffer
    */
    IPQwriter( const ::std::string & filenameString,
               bool                  isCreator = false,
               int                   nElements = 0,
               unsigned int          testOffset = 0);


    /**
     * Put an element into the queue.
     * This method does a lock/insert/unlock on the queue.
     * Moves up from protected in IPQbuffer to full public here.
     * @throw std::exception
     */
    void write( );
};


}}  // End namespace carma::util


template < class E >
carma::util::IPQwriter< E >::IPQwriter(
    const ::std::string & filenameString,
    const bool            isCreator,
    const int             nElements,
    const unsigned int    testOffset) :
IPQbuffer( static_cast< E * >( this ),
           sizeof( E ),
           filenameString,
           isCreator,
           nElements,
           testOffset)
{
    init();
}


template < class E >
void
carma::util::IPQwriter< E >::write( )
{
    IPQbuffer::write();
}


#endif // CARMA_UTIL_IPQWRITER_H
