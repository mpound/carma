#ifndef NUTATOR_H
#define NUTATOR_H

/**
 * @file Nutator.h
 *
 * Tagged: Fri Nov 14 12:38:01 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Corba.h"

#include "carma/antenna/common/NutatorControl_skel.h"
/**
 * Define a class for Nutator control
 */
namespace sza {
  namespace antenna {
    namespace corba {


      class Nutator {

	public:

	void doNothing();
      }; // End namespace sza

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif

