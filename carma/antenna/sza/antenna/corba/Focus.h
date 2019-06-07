#ifndef FOCUS_H
#define FOCUS_H

/**
 * @file Focus.h
 *
 * Tagged: Fri Nov 14 12:37:55 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Proxy.h"

#include "carma/antenna/common/FocusControl.h"

#include <string>

/**
 * Define a class for Focus control
 */
namespace sza {
  namespace antenna {
    namespace corba {


      class Focus : public Proxy {

      public:

	Focus(sza::antenna::control::AntennaMaster* parent);

	void doZTracking(bool tracking, CORBA::ULong seq);

	void setX(float position, CORBA::ULong seq);

	void setY(float position, CORBA::ULong seq);

	void setZ(float position, CORBA::ULong seq);

      }; // End namespace sza

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif

