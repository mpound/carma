/**
 * @file
 * Implementation for the IPQfileBuffer class.
 * $Id: IPQbasicTypeFileBuffer.cc,v 1.1 2011/04/11 22:11:47 abeard Exp $
 */

#include "carma/util/IPQbasicTypeFileBuffer.h"

using namespace carma::util;
using namespace std;

IPQbasicTypeFileBuffer::IPQbasicTypeFileBuffer(
    void*         localElement, 
    int           elementSize, 
    const string& filename, 
    bool          isCreator,
    int           nElements) 
    : carma::util::IPQfileBuffer( 
        localElement, 
        elementSize, 
        filename,
        isCreator, 
        nElements) 
{ 
    init( );
}

IPQbasicTypeFileBuffer::~IPQbasicTypeFileBuffer()
{
    // Nothing
}
