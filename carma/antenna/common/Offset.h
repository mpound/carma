// $Id: Offset.h,v 1.2 2007/06/28 05:03:42 abeard Exp $

#ifndef CARMA_ANTENNA_COMMON_OFFSET_H
#define CARMA_ANTENNA_COMMON_OFFSET_H

#include "carma/services/Angle.h"

/**
 * @file Offset.h
 * 
 * Tagged: Wed Sep 15 16:17:39 PDT 2004
 * 
 * @version: $Revision: 1.2 $, $Date: 2007/06/28 05:03:42 $
 * 
 * @author Rick Hobbs
 */
namespace carma {
  namespace antenna {
    namespace common {

      /**
       *  Manages Offset parameters for a Drive Axis
       */
      class Offset {
      public:

        /**
         * Constructor.
         */
        Offset();

        /**
         * Destructor.
         */
        virtual ~Offset();

        /**
         *  Set offset to this angle
         */
        void set(const carma::services::Angle& offset);

        /**
         *  Return the angular offset
         */
        carma::services::Angle* getOffset();

      private:
        carma::services::Angle offset_;
      }; // End class Offset

    } // End namespace common
  } // End namespace antenna
} // End namespace carma



#endif // End #ifndef CARMA_ANTENNA_COMMON_OFFSET_H
