/**
 * @file 
 * Declaration of carma::util::IPQfileBuffer.
 * $Id: IPQbasicTypeFileBuffer.h,v 1.1 2011/04/11 22:11:47 abeard Exp $
 */
#ifndef CARMA_UTIL_IPQBASICTYPEFILEBUFFER_H
#define CARMA_UTIL_IPQBASICTYPEFILEBUFFER_H

#include "carma/util/IPQfileBuffer.h"

namespace carma {
        namespace util {
 
            /** 
             * carma::util::IPQfileBuffer classes for basic types.
             * This class differs from IPQfileBuffer only in the fact that it
             * directly exposes the read and write methods.  This is required
             * in order to provide an IPQ which supports basic types.  Note 
             * that the IPQ[file]reader/writer classes CAN'T be used with basic
             * types because they inherit from their containing class.
             */
            class IPQbasicTypeFileBuffer: public carma::util::IPQfileBuffer {
            public:

                /**
                 *  Constructor
                 *  @param localElement address of the buffer for reads/writes
                 *  @param elementSize of an individual queue element in bytes
                 *  @param filename Shared memory filename.
                 *         Must start with '/' and be less than 15 chars long.
                 *  @param isCreator If true, create a new file if one doesn't, 
                 *              exist and make its size match nElements         
                 *  @param nElements Number of elements to allocate (queue 
                 *             length); ignored if not a creator.
                 *  @throw std::exception
                 */
                IPQbasicTypeFileBuffer( void * localElement, 
                               int elementSize, 
                               const std::string & filename, 
                               bool isCreator = false,
                               int nElements = 0 );

                /**
                 * Destructor
                 */
                virtual ~IPQbasicTypeFileBuffer( );

                using carma::util::IPQfileBuffer::write;
                using carma::util::IPQfileBuffer::read;
            };

        } // End of namespace util
} // End of namespace carma
#endif // CARMA_UTIL_IPQBASICTYPEFILEBUFFER_H
