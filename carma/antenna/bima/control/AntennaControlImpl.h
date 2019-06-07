/**
 * AntennaControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.5 $
 * $Date: 2012/02/21 21:06:58 $
 * $Id: AntennaControlImpl.h,v 1.5 2012/02/21 21:06:58 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_ANTENNACONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_ANTENNACONTROLIMPL_H

// Carma includes
#include "carma/corba/corba.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/UserException.h"

namespace log4cpp {
    // Forward declaration
    class Category;
} // End namespace log4cpp

namespace carma {
namespace antenna {
namespace bima {

    class IFCanMaster;

    class AntennaControlImpl 
    {
        public:

            /**
             * Constructor
             * @param master Reference to OvroMaster class.
             */
            AntennaControlImpl(
                    IFCanMaster & master );

            ~AntennaControlImpl();

            void resetAllCanModules();

            void setInitialization( CORBA::Boolean state );

        private:

            IFCanMaster & master_;
            log4cpp::Category & log_;

    }; // End class AntennaControlImpl
}}} // End namespace carma::antenna::bima
#endif
