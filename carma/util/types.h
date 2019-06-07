
// $Id: types.h,v 1.2 2009/07/24 16:25:39 abeard Exp $

/**
 * @file 
 * Various type definitions for util classes.
 * Set apart so that they can be included separately from the
 * header that would normally live with. This is necessary because
 * you cannot do a forward declaration for a typedef.
 * Feel free to add other typedefs to this file.
 *
 * @author Steve Scott
 * @version $Revision: 1.2 $
 *
 * $CarmaCopyright$
 */

#ifndef CARMA_UTIL_TYPES_H
#define CARMA_UTIL_TYPES_H

namespace carma  {
  namespace util {

    /**
     * @typedef frameType
     * Half second frames since Jan 1, 2000.
     * Unsigned integer gives a range until 2068.
     */
    typedef unsigned int frameType ;

    /**
     * @typedef frameDiffType
     * Signed type for calculating frame differences.
     */
    typedef long int frameDiffType;

  }
}

#endif //CARMA_UTIL_TYPES_H
