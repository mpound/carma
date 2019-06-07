/**
 *
 * Implementation for the IPQbuffer class.
 *
 * @author: Steve Scott
 *          24 Apr, 2003
 *
 * $Id: IPQbuffer.cc,v 1.12 2007/05/08 23:39:43 scott Exp $
 *
 */

// Solaris needs this POSIX defn for sys/mman
#define _POSIX_C_SOURCE 199506L

#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>


#include <fcntl.h>
#include <iostream>
#include <iomanip>

#include "carma/util/IPQbuffer.h"
#include "carma/util/ScopedUmask.h"
#include "carma/util/Trace.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


IPQbuffer::IPQbuffer(
    void * const   localElement,
    const int      elementSize,
    const string & filename,
    const bool     isCreator,
    const int      nElements,
    const unsigned int testOffset) :
IPQbufferBase( localElement,
               elementSize,
               filename,
               isCreator,
               nElements,
               testOffset)
{
}


IPQbuffer::~IPQbuffer( )
try {
    // Don't call shm_unlink here - the file in /dev/shm will go away
    // and new processes will not be able to connect to it.
    // Delete the file in /dev/shm when you want the file to go away
    // or create a special method to call:
    //   shm_unlink(getTrimmedFilename().c_str());
    if ( debug_ )
        cout << "IPQbuffer destructor" << endl;
} catch ( ... ) {
    // Just stifle any exception
}


bool
IPQbuffer::openBuffer( )
{
    trimShmemFilename();

    const string trimmedFilename = getTrimmedFilename();

    fileDescriptor_ = shm_open( trimmedFilename.c_str(),
                                openMask_,
                                protectionMask_ );

    if ( debug_ ) {
        cout << "openBuffer:" << trimmedFilename
             << " fd:" << fileDescriptor_
             << " openMask:"<< openMask_
             << " prot:" << oct << protectionMask_
             << endl;
    }
    
    if ( fileDescriptor_ == -1 )
        return false;

    CARMA_CPTRACE(Trace::TRACE4,
        "IPQBuffer openBuffer got file descriptor " << fileDescriptor_
        << " filename " << getTrimmedFilename());

    return true;
}


bool
IPQbuffer::createBuffer( )
{
    trimShmemFilename();
    
    const string trimmedFilename = getTrimmedFilename();

    // The process umask is used when the permissions are set in shm_open,
    // so we set it to zero to get it out of the picture
    {
        const ScopedUmask scopedUmask( 0 );

        fileDescriptor_ = shm_open( trimmedFilename.c_str(),
                                    (openMask_ | O_CREAT),
                                    protectionMask_ );
    }
    
    if ( debug_ ) {
        cout << "createBuffer:" << trimmedFilename
             << " fd:" << fileDescriptor_
             << " openMask:"<< openMask_
             << " prot:" << oct << protectionMask_
             << endl;
    }
    
    if ( fileDescriptor_ == -1 )
        return false;

    CARMA_CPTRACE(Trace::TRACE4,
        "IPQBuffer createBuffer got file descriptor " << fileDescriptor_
        << " filename " << getTrimmedFilename());

    return true;
}
