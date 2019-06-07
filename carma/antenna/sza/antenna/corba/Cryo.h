#ifndef CRYO_H
#define CRYO_H

/**
 * @file Cryo.h
 *
 * Tagged: Fri Nov 14 12:37:52 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Proxy.h"

#include "carma/antenna/common/SwitchState.h"

#include <string>

/**
 * Define a class for Cryo control
 */
namespace sza {
  namespace antenna {
    namespace corba {


      class Cryo :  public Proxy {
	public:

	Cryo(sza::antenna::control::AntennaMaster* parent);

	void turnCompressor(carma::antenna::common::SwitchState state);

	void resetCompressor();

	void fillCompressor();

	void purgeCompressor();

	void reset();

      };

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif
