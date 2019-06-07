/**
 *
 * Implementation for the IPQfileBuffer class.
 *
 * @author: Steve Scott
 *          24 Apr, 2003
 *
 * $Id: IPQfileBuffer.cc,v 1.9 2007/05/08 23:39:43 scott Exp $
 *
 */

// Solaris needs this POSIX defn for sys/mman
#define _POSIX_C_SOURCE 199506L

#include <sys/mman.h>

#include <fcntl.h>
#include <iostream>
#include <iomanip>

#include "carma/util/IPQfileBuffer.h"
#include "carma/util/ScopedUmask.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


IPQfileBuffer::IPQfileBuffer(
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


bool
IPQfileBuffer::openBuffer( )
{
    const string trimmedFilename = getTrimmedFilename();

    fileDescriptor_ = open( trimmedFilename.c_str(),
                            openMask_,
                            protectionMask_ );
        
    if ( debug_ ) {
        cout <<"openBuffer(file):"<<trimmedFilename
             <<" fd:"<<fileDescriptor_<<endl;
    }
    
    if ( fileDescriptor_ == -1 )
        return false;
        
    return true;
}


bool
IPQfileBuffer::createBuffer( )
{
    const string trimmedFilename = getTrimmedFilename();

    // The process umask is used when the permissions are set in shm_open,
    // so we set it to zero to get it out of the picture
    {
        const ScopedUmask scopedUmask( 0 );

        fileDescriptor_ = open( trimmedFilename.c_str(),
                                (openMask_ | O_CREAT),
                                protectionMask_ );
    }
    
    if ( fileDescriptor_ == -1 )
        return false;
        
    return true;
}
