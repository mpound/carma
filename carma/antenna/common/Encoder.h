// $Id: Encoder.h,v 1.2 2005/04/27 20:56:48 rick Exp $

#ifndef CARMA_ANTENNA_COMMON_ENCODER_H
#define CARMA_ANTENNA_COMMON_ENCODER_H

#include "carma/services/Angle.h"
/**
 * @file Encoder.h
 * 
 * Tagged: Wed Sep 15 16:15:33 PDT 2004
 * 
 * @version: $Revision: 1.2 $, $Date: 2005/04/27 20:56:48 $
 * 
 * @author Rick Hobbs
 */
namespace carma {
  namespace antenna {
    namespace common {

      /**
       *  Manages Encoder parameters.
       *  (e.g. zero points, calibration, counts per turn)
       */
      class Encoder {
      public:

        /**
         * Constructor.
         */
        Encoder();

        /**
         * Destructor.
         */
        virtual ~Encoder();

        virtual void setZero(const carma::services::Angle& zero) = 0;
        virtual const carma::services::Angle& getZero() = 0;

      private:
      }; // End class Encoder

    } // End namespace common
  } // End namespace antenna
} // End namespace carma



#endif // End #ifndef CARMA_ANTENNA_COMMON_ENCODER_H
