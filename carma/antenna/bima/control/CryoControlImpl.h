/**
 * @file
 * BIMA CryoControl implementation.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.7 $
 * $Id: CryoControlImpl.h,v 1.7 2012/02/21 21:06:58 abeard Exp $
 */

#ifndef CARMA_ANTENNA_BIMA_CRYOCONTROLIMPL_H
#define CARMA_ANTENNA_BIMA_CRYOCONTROLIMPL_H

#include "carma/antenna/common/SwitchState.h"

namespace log4cpp {
    class Category;
}

namespace carma
{
  namespace antenna
  {
    namespace bima
    {

      /**
       * CryoControlImpl CORBA control implementation.
       * This class implements the CryoControl interface defined in
       * carma::antenna::common.
       */
      class CryoControlImpl 
      {
      public:

        /**
         * Constructor
         * Inputs a CryoCompressor reference to allow delegation of control
         * commands to it.
         */
        CryoControlImpl( );

        /**
         * Destructor
         */
        virtual ~CryoControlImpl();

        void turnCompressor(carma::antenna::common::SwitchState state);

        void resetCompressor();

        void fillCompressor();

        void purgeCompressor();

        void reset();
    
    private:

        log4cpp::Category &log_;

      }; // End class CryoControlImpl

    }}}  // End namespace carma::antenna::bima

#endif // CARMA_ANTENNA_BIMA_CRYOCONTROLIMPL_H

