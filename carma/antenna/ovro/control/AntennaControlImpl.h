/**
 * AntennaControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.10 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: AntennaControlImpl.h,v 1.10 2012/02/15 21:05:00 abeard Exp $
 */
#ifndef CARMA_ANTENNA_OVRO_ANTENNACONTROLIMPL_H
#define CARMA_ANTENNA_OVRO_ANTENNACONTROLIMPL_H

#include "carma/corba/corba.h"

namespace log4cpp {
    class Category;
} // namespace log4cpp

namespace carma {

namespace corba {
    class Server;
}

namespace antenna {
namespace ovro {

    class OvroMaster;

    class AntennaControlImpl {
        public:

            /**
             * Constructor
             * @param master Reference to OvroMaster class.
             * @param server Reference to corba::Server class.
             */
            AntennaControlImpl(
                    carma::antenna::ovro::OvroMaster & master,
                    carma::corba::Server & server );

            ~AntennaControlImpl();

            void resetAllCanModules();

            void setInitialization( CORBA::Boolean state );

            void quit( );

        private:

            carma::corba::Server & server_;
            carma::antenna::ovro::OvroMaster & master_;
            log4cpp::Category & log_;
    }; // End class AntennaControlImpl
}}} // End namespace carma::antenna::ovro
#endif
