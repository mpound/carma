/**
 * @file
 * Implementation for the IPQbuffer class.
 * $Id: IPQbasicTypeBuffer.cc,v 1.1 2011/04/11 22:11:47 abeard Exp $
 */

#include "carma/util/IPQbasicTypeBuffer.h"

using namespace carma::util;
using namespace std;

IPQbasicTypeBuffer::IPQbasicTypeBuffer(
    void*         localElement, 
    int           elementSize, 
    const string& filename, 
    bool          isCreator,
    int           nElements) 
    : carma::util::IPQbuffer( 
        localElement, 
        elementSize, 
        filename,
        isCreator, 
        nElements) 
{ 
    init( );
}

IPQbasicTypeBuffer::~IPQbasicTypeBuffer()
{
    // Nothing
}
