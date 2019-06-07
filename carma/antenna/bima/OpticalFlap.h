/**@file
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill</dl>
 * $Revision: 1.1 $
 * $Date: 2005/07/28 23:37:16 $
 * $Id: OpticalFlap.h,v 1.1 2005/07/28 23:37:16 colby Exp $
 */



#ifndef CARMA_ANTENNA_BIMA_OPTICALFLAP_H
#define CARMA_ANTENNA_BIMA_OPTICALFLAP_H

// System includes
#include <string>
#include <unistd.h>

// CARMA includes
#include "carma/antenna/bima/TelemetryClient.h"

namespace carma
{
  namespace antenna
  {
    namespace bima
    {
      class OpticalFlap : public TelemetryClient
      {
      public:
        OpticalFlap( Configuration &config );

        typedef enum { CLOSED, OPEN, STUCK } OpticalFlapStatus;

        void open(); 
        void close(); 
        OpticalFlapStatus getStatus(); 

      private:
        Configuration &_config;

      }; // class OpticalFlap
    } // namespace bima
  } // namespace antenna
} // namespace carma

#endif // CARMA_ANTENNA_BIMA_OPTICALFLAP_H
