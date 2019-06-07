/*
 * Implementation of interprocess communication queue buffer (storage)
 * base class.
 *
 * @author: Steve Scott
 *
 */


#include "carma/util/IPQbufferBase.h"

#include <iostream>
#include <sstream>
#include <cerrno>
#include <climits>

#include <unistd.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/uio.h>
#include <sys/ipc.h>
#include <sys/sem.h>

#include "carma/util/ErrorException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/ThreadQuit.h"
#include "carma/util/memoryUtils.h"
#include "carma/util/programExtras.h"
#include "carma/util/programLogging.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/PthreadRWLock.h"
#include "carma/util/ScopedLogNdc.h"
#include "carma/util/ScopedExclusiveLockManager.h"
#include "carma/util/ScopedFlockManager.h"
#include "carma/util/ScopedLock.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/ScopedSharedLock.h"
#include "carma/util/ScopedSharedLockManager.h"
#include "carma/util/Time.h"


#if defined(__GNU_LIBRARY__) && !defined(_SEM_SEMUN_UNDEFINED)
i   // union semun is defined by including sys/sem.h
#else
    // We need to define it ourselves.
    union semun {
        int val;
    };
#endif


using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {

typedef ScopedSharedLockManager< PthreadRWLock > ScopedPthreadSharedLockManager;

/**
 * The following class is necessary to provide a single mutex for multiple 
 * instances of IPQ classes using the same name. The mutex is needed because
 * file locks do not make a distinction between different threads, only 
 * processes.  As a result, critical sections are vulnerable to multiple
 * instances accessing the same memory mapped regions if they are in the same
 * process.
 */
typedef map< string, PthreadRWLock * > RWLockMap;

RWLockMap * gRWLockMap = 0;
::pthread_mutex_t gRWLockMapGuard = PTHREAD_MUTEX_INITIALIZER;

PthreadRWLock &
getReadWriteLock( const string & filename )
{
    const ScopedLock< ::pthread_mutex_t > lock( gRWLockMapGuard );

    if (gRWLockMap == 0) gRWLockMap = new RWLockMap;
        
    const RWLockMap::const_iterator i = gRWLockMap->find( filename );
    
    if (i != gRWLockMap->end()) return *(i->second);
    
    if ( false ) {
        programLogInfoIfPossible( "Inserting IPQ RWLock into map for \"" +
                                  filename + "\"" );
    }
    
    PthreadRWLock * const rwLock = new PthreadRWLock;
    
    const bool inserted = 
        gRWLockMap->insert(
            RWLockMap::value_type( filename, rwLock ) ).second;
    
    if ( inserted != true ) {
        delete rwLock;
        
        const string msg =
            "Inserting IPQ RWLock into map for \"" + filename + "\" failed";
        
        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }
    
    return *rwLock;
}

} // namespace < anonymous >


IPQbufferBase::IPQbufferBase(
    void * const   localElement,
    int            elementSize,
    const string&  filename,
    bool           isCreator,
    int            nElements,
    unsigned int   testOffset) :
protectionMask_( 0666 ),
openMask_( O_RDWR ),
filename_( filename ),
localElement_( localElement ),
localElementSize_( elementSize ),
isCreator_( isCreator ),
debug_( false ),
fileDescriptor_( -1 ),
semDescriptor_( -1 ),
mmapAddr_( MAP_FAILED ),
mmapSize_( 0 ),
controlBlockOffset_( 0 ),
controlBlock_( 0 ),
queueOffset_( 0 ),
queueElementSize_( elementSize ),
queueElements_( (nElements < 0) ? 0 : nElements ),
queue_( 0 ),
isFileOpen_( false ),
nLostElements_( 0 ),
rwLock_( getReadWriteLock( filename )),
empty_(true),
testOffset_(testOffset) 
{
    const ScopedLogNdc ndc( "IPQbufferBase::IPQbufferBase " + filename_ );

    if ( localElementSize_ >= getDefaultVmMemoryCopyMinWinBytes() ) {
        const bool ptrOkay = pointerIsVmPageAligned( localElement_ );
        const bool sizeOkay = valueIsVmPageMultiple( localElementSize_ );

        const bool allOkay = (ptrOkay && sizeOkay);

        if ( allOkay != true ) {
            ostringstream oss;

            oss << localElementSize_ << " byte local element";
                
            if ( ptrOkay != true ) {
                oss << " at address "
                    << static_cast< const void * >( localElement_ );
            }
            
            oss << " is suboptimal for memory copies:";

            if ( ptrOkay != true )
                oss << " Address is not aligned to a VM page.";

            if ( sizeOkay != true )
                oss << " Byte size is not a multiple of a VM page.";

            programLogWarnIfPossible( oss.str() );
        }
    }
}

void IPQbufferBase::init( )
{
    const ScopedLogNdc ndc( "IPQbufferBase::init " + filename_ );
    string errorMessage;

    debug_ = false;
    if(debug_) cout << "IPQbufferBase::init: input filename="
                    << filename_ << endl;

    // Initialize filenames
    trimmedFilename_ = filename_;
    displayFilename_ = filename_;

    // Semaphore filename
    createSemFilename();

    // File status
    isNewFile_  = false;
    isFileOpen_ = false;

    controlBlockOffset_ = roundUpToVmPageMultiple( sizeof( header_ ) );
    const size_t controlBlockSize = sizeof( *controlBlock_ );

    if ( valueIsVmPageMultiple( localElementSize_ ) ) {
        queueOffset_ =
            roundUpToVmPageMultiple( controlBlockOffset_ + controlBlockSize );
    } else {
        // The control block size must be a multiple of 8 so that
        // the data that follows will start on an 8 byte boundary.
        // Doubles (at least in Sparc/Solaris) must start on 8 byte
        // boundaries, so all structures are multiples of this size.
        queueOffset_ =
            roundUpToMultiple( (controlBlockOffset_ + controlBlockSize), 8 );
    }

    // Try to open an existing buffer
    if (!openBuffer()) {
        // No luck on open...
        if (!isCreator_) {
            ostringstream s;
            s << "Couldn't open existing file \""
              << displayFilename_ << "\":"<< strerror(errno);
            throw CARMA_ERROR(s);
        }
        // Couldn't open existing file, try to create a new one
        else if (createBuffer()) {
            isNewFile_ = true;
        }
        else {
            ostringstream s;
            s << "Couldn't create file:" << displayFilename_
              << "; " << strerror(errno);
            throw CARMA_ERROR(s);
        }
    }

    // Try to open an existing semaphore
    if (!openSemaphore()) {
        if (debug_) cerr << "Error opening semaphore" << endl;
        if (!createSemaphore()) {
            errorMessage = "Couldn't create semaphore: "
                + (string)strerror(errno);
            throw CARMA_ERROR (errorMessage);
        }
    }

    isFileOpen_ = true;

    programLogDebug( "Successfully opened " + displayFilename_  );

    /* Must have at least one element */
    if ( isCreator_ && (queueElements_ < 1) ) {
        if (debug_)cout << "creator with less than 1 element" << endl;
        throw CARMA_ERROR("IPQbufferBase: must have at least 1 element");
    }

    if (false && debug_) cout << "Ready for header..." << endl;

    // Check or create header and size the file
    if (isCreator_) {
        if (!isNewFile_) {
            try {
                readHeader(true);
            } catch (ErrorException& e) {
                writeHeader();
            }
            if (hasQueueChanged()) isNewFile_ = true;
        }
        writeHeader();
    }
    else {
        try {
            readHeader(true);
        } catch (ErrorException& e) {
            // Maybe a writer is creating the header...
            // Give 'em a chance to finish and try one last time.
            sleep(1);
            readHeader(false);
        }
        doesElementSizeMatch();  // throws exception if doesn't
    }

    controlBlockOffset_ = header_.controlBlockOffset;
    queueOffset_        = header_.queueOffset;
    queueElementSize_   = header_.queueElementSize;
    queueElements_      = header_.queueElements;

    if ( queueElementSize_ != localElementSize_ ) {
        string msg;
        {
            ostringstream oss;
            oss << "Element size mismatch  between header size of "
                << queueElementSize_
                << " and local size of "
                << localElementSize_;
            msg = oss.str();
        }

        programLogErrorIfPossible( msg );
    }

    // Map the file into memory
    if ( mmapAddr_ != MAP_FAILED ) {
        const string msg = "mmapAddr_ != MAP_FAILED";

        programLogErrorIfPossible( msg );
        throw CARMA_ERROR( msg );
    }

    if ( debug_ ) cout << "mapping memory..." << endl;
    mmapSize_ = queueOffset_ + queueElementSize_ * queueElements_;

    mmapAddr_ = mmap( 0,                         // arbitrary placement
                      mmapSize_,                 // total length to map
                      (PROT_READ | PROT_WRITE),  // protection
                      MAP_SHARED,                // shared between processes
                      fileDescriptor_,           // file descriptor
                      0 );                       // map file from the start

    if ( mmapAddr_ == MAP_FAILED ) {
        const int savedErrno = errno;

        size_t okaySize = 0;
        size_t failedSizes = 0;
        for (size_t trySize = (mmapSize_ >> 1); trySize >= 1; trySize >>= 1) {
            void * const addr =
                mmap( 0,                         // arbitrary placement
                      trySize,                   // total length to map
                      (PROT_READ | PROT_WRITE),  // protection
                      MAP_SHARED,                // shared between processes
                      fileDescriptor_,           // file descriptor
                      0 );                       // map file from the start

            if ( addr == MAP_FAILED ) {
                ++failedSizes;
            }
            else {
                okaySize = trySize;
                munmap( addr, trySize );
                break;
            }
        }

        string msg;
        {
            ostringstream oss;
            oss << "filename=" << displayFilename_
                << " mmap failed for size "
                << mmapSize_ << ": "
                << savedErrno << " " << strerror( savedErrno );

            if ( okaySize >= 1 ) {
                oss << ". However, mmap succeeded for a size of "
                    << okaySize << ".";
            } else if ( failedSizes > 0 ) {
                oss << ". mmap also failed for " << failedSizes
                    << " smaller sizes.";
            }
            msg = oss.str();
        }
        programLogErrorIfPossible( msg );
        throw CARMA_ERROR( msg );
    }

    if ( pointerIsVmPageAligned( mmapAddr_ ) == false ) {
        programLogWarnIfPossible( "mmap return is not VM page aligned" );
    }

    if ( debug_ ) cout << "finished mmap" << endl;

    // Now unlock the file descriptor
    //
    // This seems like a bad idea as we may be releasing a lock held by another
    // instance (in a different thread) - TWC 25 Jan 2007
    //
    // unlockFileDescriptor( fileDescriptor_ );

    // If a new file is being used, clear the non-header area.
    // This clears the control block and the data queue area
    if ( isNewFile_ ) {
        void* const addr = byteOffsetPointer(mmapAddr_, controlBlockOffset_);
        const size_t bytes = mmapSize_ - controlBlockOffset_;

        if ( debug_ ) {
            cout << "bzero'ing addr=" << addr << " bytes= " << bytes << endl;
        }        
        bzero( addr, bytes );
        if ( debug_ ) cout << "bzero finished" << endl;

    }

    /*
    ** The control block sits directly in front of the circular array
    ** that forms the data queue
    */
    controlBlock_ =
        static_cast<ControlBlock*>(byteOffsetPointer(mmapAddr_,
                                                          controlBlockOffset_));

    /*
    ** Start at the bottom of the queue.
    */
    {
        const unsigned int presentPutOffset = getPutOffset();
        
        if ( presentPutOffset <= queueElements_ ) getOffset_ = 0;
        else getOffset_ = presentPutOffset - queueElements_;        
    }
    
    /* Element queue will point to the first element in the circular
    ** array which is located immediately following the control block
    ** in the client area of the memory mapped region
    */
    queue_ = static_cast<char*>(byteOffsetPointer(mmapAddr_, queueOffset_));

    if ( valueIsVmPageMultiple( queueElementSize_ ) ) {
        if ( pointerIsVmPageAligned( queue_ ) == false )
            programLogWarnIfPossible( "Element queue is not VM page aligned" );
    }
    
    // Set the max value for the put pointer offset
    // This needs to contain an integral number of full queues or else the
    // the writes, which use a modulo, won't be in the correct spot
    maxOffset_ = (UINT_MAX/queueElements_)*queueElements_ -1;
    
    // In testOffset mode we advance the start of the queue
    if (testOffset_ > 0) {
        const unsigned int testStart = 
                    maxOffset_ - testOffset_*queueElements_ + 1;
        if (isNewFile_) controlBlock_->putOffset = testStart;
        getOffset_ = testStart;
    }

    if ( debug_ ) {
        cerr << "init(end): "
             << " elementSize=" << queueElementSize_
             << " nElements=" << queueElements_
             << endl;
    }
}


void
IPQbufferBase::trimShmemFilename( )
{
    string output = filename_;

    // Shmem filenames must start with '/', so find last one
    string::size_type index = output.rfind("/");
    if (index != string::npos) {
        output = output.substr((int)index, output.size() - (int)index);
    }
    else {
        output = "/" + output;
    }
#ifndef __linux__
    // Truncate name to 14 characters for max portability...
    output = output.substr(0, 14);
#endif
    if (debug_) cout << "Shmem filename orig:" << filename_
                     << "   used:" << output << endl;
    trimmedFilename_ = output;
    displayFilename_ = trimmedFilename_ + "(" + filename_ + ")";
}

string IPQbufferBase::getTrimmedFilename( )
{
    return trimmedFilename_;
}

bool IPQbufferBase::readHeader( const bool keepOpen )
{
    ssize_t size = ::read( fileDescriptor_, &header_, sizeof( header_ ) );

    if ( size != sizeof( header_ ) ) {
        string err = "IPQbuffer:Couldn't read header from " + filename_;
        if ( keepOpen == false ) {
            close (fileDescriptor_);
            isFileOpen_ = false;
        }
        throw CARMA_ERROR(err);
    }

    // the file must be recognized as a previously mapped file
    if ( header_.magic != MEMORYMAPPEDFILE_MAGIC ) {
        if ( keepOpen == false ) {
            close (fileDescriptor_);
            isFileOpen_ = false;
        }
        ostringstream err;
        err << "IPQbufferBase:Magic number error"
            << displayFilename_
            << ": got " + header_.magic << ", should have been "
            << MEMORYMAPPEDFILE_MAGIC;
        throw CARMA_ERROR(err);
    }

    if ( header_.headerSize != sizeof( header_ ) ) {
        const string msg = "Header size mismatch";

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    if ( header_.controlBlockSize != sizeof( *controlBlock_ ) ) {
        const string msg = "Control block size mismatch";

        programLogErrorIfPossible( msg );

        throw CARMA_ERROR( msg );
    }

    if ( (header_.controlBlockOffset < header_.headerSize) ||
         (header_.queueOffset < (header_.controlBlockOffset + header_.controlBlockSize)) ) {
         throw CARMA_ERROR( "IPQbufferBase region overlap or order problem" );
    }

    return true;
}

bool IPQbufferBase::doesElementSizeMatch()
{

    // Element size must match
    if (header_.queueElementSize != queueElementSize_) {
        close (fileDescriptor_);
        isFileOpen_ = false;
        ostringstream err;
        err << "IPQbufferBase, filename:" << displayFilename_
            << "/ has elemSize " << header_.queueElementSize
            << ", but buffer wants " << queueElementSize_;
        throw CARMA_ERROR (err);
    }
    return true;
}

// Records changes of new/old return true if changes
bool
IPQbufferBase::hasQueueChanged( )
{
    ostringstream s;

    if ( (header_.queueElementSize == queueElementSize_) &&
         (header_.queueElements == queueElements_) )
            return false;
    s << "filename:"
      << displayFilename_ << " has changed size: old/new ";

    // Element size
    if (header_.queueElementSize != queueElementSize_) {
        s  << "queueElementSize(" << header_.queueElementSize
           << "/" <<  queueElementSize_ << ") ";
    }
    if ( header_.queueElements != queueElements_ ) {
        s << "queueElements(" << header_.queueElements
          << "/" <<  queueElements_ << ")";
    }

    programLogDebug( s.str() );

    return true;
}


bool
IPQbufferBase::writeHeader( )
{
    if ( (controlBlockOffset_ < sizeof( header_ )) ||
         (queueOffset_ < (controlBlockOffset_ + sizeof( *controlBlock_ ))) ) {
         throw CARMA_ERROR( "IPQbufferBase region overlap or order problem" );
    }

    header_.magic               = MEMORYMAPPEDFILE_MAGIC;
    header_.headerSize          = sizeof( header_ );
    header_.controlBlockOffset  = controlBlockOffset_;
    header_.controlBlockSize    = sizeof( *controlBlock_ );
    header_.queueOffset         = queueOffset_;
    header_.queueElementSize    = queueElementSize_;
    header_.queueElements       = queueElements_;

    // write the new header to the file
    lseek( fileDescriptor_, 0, SEEK_SET );

    ssize_t size = ::write( fileDescriptor_, &header_, sizeof( header_ ) );
    if ( size != sizeof( header_ ) ) {
        close(fileDescriptor_);
        isFileOpen_ = false;
        string err = "IPQbufferBase filename = " + displayFilename_
            + ", Header write failed" ;
        throw CARMA_ERROR (err);
    }

    // truncate the file to the correct length
    const off_t truncOffset =
        header_.queueOffset + header_.queueElementSize * header_.queueElements;

    int rtn = ftruncate( fileDescriptor_, truncOffset );

    if ( debug_ ) {
        cout << "ftruncate rtn=" << rtn
             << "  truncOffset=" << truncOffset << endl;
    }

    if ( rtn < 0 ) {
        close(fileDescriptor_);
        isFileOpen_ = false;
        string err = "IPQbufferBase filename = " + displayFilename_
            + ",  ftruncate() failed" ;
        throw CARMA_ERROR (err);
    }

    return true;
}


IPQbufferBase::~IPQbufferBase( )
try {
    if ( debug_ ) {
        try {
            cout << "IPQbufferBase::~IPQbufferBase for "
                 << displayFilename_ << endl;
        } catch ( ... ) {
            // Just stifle any exception
        }
    }

    try {
        if ( mmapAddr_ != MAP_FAILED ) {
            const int munmapResult = munmap( mmapAddr_, mmapSize_ );

            if ( munmapResult == 0 )
                mmapAddr_ = MAP_FAILED;
            else {
                const int savedErrno = errno;

                ostringstream oss;

                oss << "IPQbufferBase::~IPQbufferBase for "
                    << displayFilename_
                    << " munmap("
                    << hex << mmapAddr_ << ", " << dec << mmapSize_ << ") "
                    << "failed:"
                    << munmapResult << " "
                    << savedErrno << strerror( savedErrno );

                programLogErrorIfPossible( oss.str() );
            }
        }
    } catch ( ... ) {
        // Just stifle any exception
    }

    // Release the file descriptor to avoid a resource leak (open files).
    close( fileDescriptor_ );
    isFileOpen_ = false;
} catch ( ... ) {
    // Just stifle any exception
}


string
IPQbufferBase::getFileName( ) const
{
    return filename_;
}


/// Used internally and for debugging
unsigned int IPQbufferBase::getPutOffset( ) const
{
    unsigned int result;
    {
        const ScopedSharedLock< PthreadRWLock > readLock( rwLock_ );    
        ScopedFlockManager flockManager( true );
        flockManager.lockRead( fileDescriptor_ );        
        result = controlBlock_->putOffset;        
        flockManager.unlock( fileDescriptor_ );
    }    
    return result;
}


/// Useful only for debugging
unsigned int IPQbufferBase::getGetOffset() const
{
    return getOffset_;
}

unsigned int IPQbufferBase::getMaxOffset() const
{
    return maxOffset_;
}

unsigned int IPQbufferBase::getLostElementCount( ) const
{
    return nLostElements_;
}

unsigned int
IPQbufferBase::read( )
{
    return internalRead( true );
}

unsigned int
IPQbufferBase::readNoLock( )
{
  return internalRead( false );
}

bool
IPQbufferBase::readNewest( )
{
    return internalReadNewest( true );
}

bool
IPQbufferBase::readNewestConditionalCopy( )
{
    bool copy = (getNumAvailable() != 0) ;
    if (copy)  copy = internalReadNewest(true);
    // Debugging output
    if (false) {
        ostringstream o;
        o << "IPQbufferBase::readNewestConditionalCopy copy " ;
        if (copy) {
            o << "done" ;
            programLogInfo(o.str());
        }    
        else {
            o << "not done" ;
            programLogInfo(o.str());
        }    
    }    
    return copy;
}

bool
IPQbufferBase::readNewestNoLock( )
{
    return internalReadNewest(false);
}

int
IPQbufferBase::getQueueSize( ) const
{
    return queueElements_;
}


int
IPQbufferBase::getElementSize( ) const
{
    return queueElementSize_;
}


int
IPQbufferBase::getNumAvailable( ) const
{
    const unsigned int putOffset = getPutOffset();
    unsigned int unread;
    if (getOffset_ > putOffset) {
        unread = putOffset + maxOffset_ - getOffset_ + 1;
    }
    else {
        unread = putOffset - getOffset_;
    } 
    unread = min(unread, queueElements_);
    const unsigned int z = 0;
    unread = max(unread, z);
    return unread;   
}


bool
IPQbufferBase::isEmpty( ) const
{
    return ((getPutOffset() == 0) && empty_);
}


bool
IPQbufferBase::isDataAvailable( ) const
{
    return (getPutOffset() != getOffset_);
}


void
IPQbufferBase::setNoneAvailable( )
{
    getOffset_ = getPutOffset();
}


void
IPQbufferBase::copyMemory( void * const       dest,
                           const void * const src,
                           const size_t       bytes ) const
{
    if ( false && (bytes >= getDefaultVmMemoryCopyMinWinBytes()) ) {
        const bool destOkay  = pointerIsVmPageAligned( dest );
        const bool srcOkay   = pointerIsVmPageAligned( src );
        const bool bytesOkay = valueIsVmPageMultiple( bytes );

        const bool allOkay = (destOkay && srcOkay && bytesOkay);

        if ( allOkay == false ) {
            ostringstream oss;
            oss << bytes << " byte memory copy is suboptimal:";

            if ( destOkay == false )
                oss << " Destination pointer is not aligned to a VM page.";

            if ( srcOkay == false )
                oss << " Source pointer is not aligned to a VM page.";

            if ( bytesOkay == false )
                oss << " Byte count is not a multiple of a VM page.";

            programLogWarnIfPossible( oss.str() );
        }
    }

    memcpy( dest, src, bytes );
}


void
IPQbufferBase::write( )
{
    const ScopedLogNdc ndc( "IPQbufferBase::write " + filename_ );
    
    if ( localElementSize_ > queueElementSize_ ) {
        const string msg = "localElementSize_ > queueElementSize_";
        programLogErrorIfPossible( msg );
        throw CARMA_ERROR( msg );
    }

    // If file is not open (may have been destructed), bail out
    if ( !isFileOpen_ ) {
        throw CARMA_ERROR( "IPQbufferBase::write(): file is not open" );
    }

    const size_t writeBytes = std::min(localElementSize_, queueElementSize_);

    ScopedExclusiveLockManager< PthreadRWLock > rwLockManager( rwLock_ );
    rwLockManager.lock();

    ScopedFlockManager flockManager( true );
    flockManager.lockWrite( fileDescriptor_ );

    /* The volatile declaration is a attempt to ensure that the shared
     * pointer 'controlBlock_->putOffset' is not corrupted by a process
     * unexpectedly exiting during an update.  The idea here is that a
     * memory location is loaded with the value of putOffset before an
     * update.  The volatile keyword guarantees that all accesses of the
     * value will be made from memory and will not be optimized.  If a
     * modifying process exits unexpectedly leaving garbage data in the
     * element buffer, putOffset will not have been modified and no
     * other process will see the bad element.
     */
    const unsigned int startPutOffset = controlBlock_->putOffset;

    char* const queueElementPtr =
        queue_ + queueElementSize_ * (startPutOffset % queueElements_);

    memcpy(queueElementPtr, localElement_, writeBytes);

    // Compute new putOffset
    const bool needToWrapPutOffset = (startPutOffset >= maxOffset_);

    // Update the putOffset
    unsigned int preupPutOffset;
    if ( needToWrapPutOffset ) {
        if ( (preupPutOffset = controlBlock_->putOffset) == startPutOffset )
            controlBlock_->putOffset = 0;
    } else {
        if ( (preupPutOffset = controlBlock_->putOffset) == startPutOffset )
            ++(controlBlock_->putOffset);
    }
    
    // Signal to waiting readers if any are waiting...
    if ( debug_ ) {
        cerr << "Write signaling to " << semctl( semDescriptor_, 0, GETZCNT )
             << " readers." << endl;
    }

    union semun arg;
    arg.val = 0;
    if ( semctl( semDescriptor_, 0, SETVAL, arg ) != 0 ) {
        const int savedErrno = errno;

        const string msg =
            "Semaphore signal failed: " + string( strerror( savedErrno ) );

        throw CARMA_ERROR( msg );
    }

    flockManager.unlock( fileDescriptor_ );
    
    rwLockManager.unlock();
    
    empty_ = false;  // We just wrote so we can't be empty anymore
    
    if ( preupPutOffset != startPutOffset ) {
        const unsigned int startCycle = (startPutOffset / queueElements_);
        const unsigned int startIndex = (startPutOffset % queueElements_);

        const unsigned int preupCycle = (preupPutOffset / queueElements_);
        const unsigned int preupIndex = (preupPutOffset % queueElements_);
        
        ostringstream oss;
        
        oss << "controlBlock_->putOffset moved unexpectedly during write from "
            << startPutOffset << " (" << startCycle << ":" << startIndex
            << ") to " << preupPutOffset << " (+";
        
        if ( preupCycle == startCycle )
            oss << (preupIndex - startIndex);
        else
            oss << (preupCycle - startCycle) << ":" << preupIndex;

        oss << ")";
        
        programLogErrorIfPossible( oss.str() );
    }
}


class IPQbufferBase::BlockedOnSemaphoreQuitRequestHandler :
public ThreadQuitRequestHandler {
    public:
        explicit BlockedOnSemaphoreQuitRequestHandler( IPQbufferBase & ipq );

        void HandleQuitRequest( ::pthread_t thread );

    private:
        IPQbufferBase & ipq_;
};


IPQbufferBase::BlockedOnSemaphoreQuitRequestHandler::BlockedOnSemaphoreQuitRequestHandler( IPQbufferBase & ipq ) :
ipq_( ipq )
{
}


void
IPQbufferBase::BlockedOnSemaphoreQuitRequestHandler::HandleQuitRequest(
    ::pthread_t thread )
{
    union semun arg;
    arg.val = 0;

    if ( semctl( ipq_.semDescriptor_, 0, SETVAL, arg ) != 0 ) {
        const int savedErrno = errno;
        const string msg =
            "Semaphore signal failed: " + string( strerror( savedErrno ) );

        throw CARMA_ERROR( msg );
    }
}

// returns adjusted read offsset
unsigned int IPQbufferBase::adjustReadOffset(bool& lock, 
    ScopedFlockManager& flockManager, unsigned int readOffset)
{
    // If the getOffset is more than queueElements behind, move it up
    // If the read offset is more than the put offset it is assumed that
    // it is because we have wrapped around the whole buffer.
    unsigned int toBeRead;
    const unsigned int putOffset = controlBlock_->putOffset;
    if (readOffset > putOffset) {
        // wraparound case
        toBeRead = putOffset + maxOffset_ - readOffset + 1;
    }
    else {
        // non-wraparound case
        toBeRead = putOffset - readOffset + 1;
    }
    // If the getOffset is more than queueElements behind, move it up
    if (toBeRead > queueElements_) {
        if ( lock != true ) {
            lock = true; // Always lock if get/put pointers will be adjacent
            flockManager.lockRead(fileDescriptor_);
        }
        if ((readOffset < putOffset) || (putOffset >= queueElements_)) {
            // Case where there will be no wraparound with new readOffset
            return putOffset - queueElements_ ;
        }
        else {
            // Case with wraparound after readOffset is moved
            return putOffset + maxOffset_ - queueElements_ + 1;
        }       
    }
    return readOffset;
}

unsigned int
IPQbufferBase::internalRead( bool lock )
{
    const ScopedLogNdc ndc( "IPQbufferBase::internalRead " + filename_ );
    
    if ( queueElementSize_ > localElementSize_ ) {
        const string msg = "queueElementSize_ > localElementSize_";
        programLogErrorIfPossible( msg );
        throw CARMA_ERROR( msg );
    }

    // If file is not open (may have been destructed), bail out
    if ( !isFileOpen_ ) {
        throw CARMA_ERROR ("IPQbufferBase::read(): file is not open");
    }
    
    const size_t readBytes = std::min(localElementSize_, queueElementSize_);

    const unsigned int initialGetOffset = getOffset_;
    unsigned int readOffset = initialGetOffset;
    
    ScopedPthreadSharedLockManager sharedLockManager( rwLock_ );    
    sharedLockManager.lock( );
    ScopedFlockManager flockManager( true );

    if ( lock ) flockManager.lockRead( fileDescriptor_ );

    if ( debug_ ) {
        cout << "IPQbufferBase::read: putOffset=" << controlBlock_->putOffset
             << " getOffset=" << readOffset
             << "  numElements=" << queueElements_
             << endl;
    }

    readOffset = adjustReadOffset(lock, flockManager, readOffset);
    
    struct sembuf sop; // Semaphore operation structure.

    while ( readOffset == controlBlock_->putOffset ) {
        if ( semctl( semDescriptor_, 0, GETZCNT ) != 1 ) {
            // Set semval to 1.  At least one reader must do this but it is ok
            // to have multiple readers do it (and they do).  To ensure
            // that only a single reader increments val, we would either need to
            // use another semaphore or write lock the file here (not worth it).
            if ( debug_ ) cerr << "Setting semval to 1." << endl;

            union semun arg;
            arg.val = 1;
            semctl( semDescriptor_, 0, SETVAL, arg );
        }

        sop.sem_num = 0;  // first and only semaphore in the set
        sop.sem_op = 0; // Wait for zero
        sop.sem_flg = SEM_UNDO; // UNDO if problems occur

        if ( lock ) flockManager.unlock( fileDescriptor_ );
        
        sharedLockManager.unlock( );

        // Block on the semaphore....
        {
            const double start = Time::MJD();

            if ( debug_ )
                cerr << "Blocking read on semaphore " << endl;

            BlockedOnSemaphoreQuitRequestHandler handler( *this );
            ScopedThreadQuitRequestHandlerSelf handlerInstall( handler );

            const int status = semop( semDescriptor_, &sop, 1 );
            const int savedErrno = errno;

            if ( (status == -1) && (savedErrno != EINTR) ) {
                throw CARMA_ERROR( "Wait on read semaphore failed: " +
                                   string( strerror( savedErrno ) ) );
            } else if ( (status != 0) && (savedErrno == EINTR) ) {
                if ( debug_ )
                    cerr << "Blocking read received EINTR - retry." << endl;
            } else {
                // Nothing
            }
            const double end = Time::MJD();
            const double elapsedS = ( end - start ) * Time::SECONDS_PER_DAY;
            if ( elapsedS > 1.0 ) {
                ostringstream msg;
                msg << "IPQbufferBase::internalRead - Semaphore acquired "
                    << "after " << elapsedS << " seconds.";
                programLogErrorIfPossible( msg.str( ) );
            }
        }

        ThreadQuitTestSelf();

        sharedLockManager.lock( );
        if ( lock ) flockManager.lockRead( fileDescriptor_ );
        // Must adjust again after unblocking because the putOffset changes
        readOffset = adjustReadOffset(lock, flockManager, readOffset);   

    }

    const char * const queueElementPtr =
        queue_ + queueElementSize_ * (readOffset % queueElements_);

    memcpy( localElement_, queueElementPtr, readBytes );

    if ( lock ) flockManager.unlock( fileDescriptor_ );
    sharedLockManager.unlock( );

    if ( readOffset < initialGetOffset )
        programLogErrorIfPossible( "readOffset <= initialGetOffset" );
        
    const unsigned int lostElements = readOffset - initialGetOffset;
    
    if (readOffset >= maxOffset_) {
        getOffset_ = 0; // wrap around
    }
    else {
        getOffset_ = readOffset + 1;
    }
    nLostElements_ = lostElements;
    empty_ = false; // Can't be empty if we jsut read from it

    return lostElements;
}


bool
IPQbufferBase::internalReadNewest( const bool lock )
{
    const ScopedLogNdc ndc( "IPQbufferBase::internalReadNewest " + filename_ );

    if ( queueElementSize_ > localElementSize_ ) {
        const string msg = "queueElementSize_ > localElementSize_";
        programLogErrorIfPossible( msg );
        throw CARMA_ERROR( msg );
    }

    const size_t readBytes =
        ::std::min( localElementSize_, queueElementSize_ );

    const ScopedSharedLock< PthreadRWLock > scopelock( rwLock_ );

    // Check to see if queue is empty for non-locking case
    if ( (lock == false) && (controlBlock_->putOffset == 0) && empty_) {
        //programLogError("internalReadNewest(nolock) returns false");
        return false;
    }

    unsigned int putOffset = 0;
    {
        ScopedFlockManager flockManager( true );

        // Check to see if queue is empty for locking case
        if ( lock ) {
            flockManager.lockRead( fileDescriptor_ );
            if ((controlBlock_->putOffset == 0) && empty_) {
                flockManager.unlock( fileDescriptor_ );                
                //programLogError("internalReadNewest(lock) returns false");
                return false;
            }
        }
        
        putOffset = controlBlock_->putOffset;

        const char * const queueElementPtr =
            queue_ + queueElementSize_ * ((putOffset - 1) % queueElements_);

        memcpy( localElement_, queueElementPtr, readBytes );

        if ( lock ) flockManager.unlock( fileDescriptor_ );

    }

    // Set get pointer to match data just read
    getOffset_ = putOffset;
    
    empty_ = false; // Cannot be empty if we just read from it

    return true;
}


void
IPQbufferBase::createSemFilename( )
{
    string output = filename_;  // Could have many or one '/'

    // Shmem filenames must start with '/', so find last one
    string::size_type index = output.rfind("/");
    if (index != string::npos) {
        output = output.substr((int)index, output.size() - (int)index);
    } else {
        output = "/" + output;
    }

#ifndef __linux__
    // Truncate name to 14 characters same as shm filename...
    output = output.substr(0, 14);
#endif
    // Add a '.sem' filetype - this deliberately makes name > 14 characters.
    output += ".sem";
    if (debug_) cout << "Semaphore filename: derived from " << filename_
                     << " used:" << output << endl;
    semFilename_ = output;
}


namespace {

const int kProjId = 0xabcd;

}  // namespace < anonymous >


bool
IPQbufferBase::openSemaphore( )
{
    const key_t key = ftok( semFilename_.c_str(), kProjId );

    semDescriptor_ = semget( key,
                             0,
                             (S_IRWXU | S_IRWXG | S_IRWXO) );

    if ( semDescriptor_ == -1 ) {
        const int savedErrno = errno;

        if ( debug_ ) {
            cerr << "Failed to open semaphore "
                 << strerror( savedErrno ) << endl;
        }
        
        programLogInfoIfPossible( "Failed to opened sem " + semFilename_ );

        return false;
    } else {
        if ( debug_ )
            cerr << "Semaphore open/init successful" << endl;

        programLogInfoIfPossible( "Successfully opened sem " + semFilename_ );

        return true;
    }
}

bool
IPQbufferBase::createSemaphore( )
{
    // Retrieve Sys V file token.
    const key_t key = ftok( semFilename_.c_str(), kProjId );

    semDescriptor_ = semget( key,
                             1,
                             (IPC_CREAT | S_IRWXU | S_IRWXG | S_IRWXO) );

    if ( semDescriptor_ == -1 ) {
        if ( debug_ ) {
            cerr << "Error creating semaphore from name "
                 << semFilename_ << endl;
        }

        return false;
    } else {
        union semun arg;

        arg.val = 1;

        const int status = semctl( semDescriptor_, 0, SETVAL, arg );

        if ( debug_ && (status == 0) )
            cerr << "Semaphore create/init successful" << endl;

        return (status == 0);
    }
}
