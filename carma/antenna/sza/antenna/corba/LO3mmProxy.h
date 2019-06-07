#ifndef SZA_ANTENNA_CORBA_LO3MMPROXY_H
#define SZA_ANTENNA_CORBA_LO3MMPROXY_H

/**
 * @file LO3mmProxy.h
 *
 * Tagged: Thu Nov 13 16:53:40 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/LOProxy.h"

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * A class which will be served as the CORBA LO DO, whose
       * methods will send messages to the AntennaMaster message
       * queue.
       */
      class LO3mmProxy : public LOProxy {

      public:

	/**
	 * Constructor with a pointer to the parent AntennaMaster
	 */
	LO3mmProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~LO3mmProxy();

	virtual void setYigFrequency(double yigFreq);

	virtual void setLoFrequency(double frequency);

	virtual void toggleSweep(bool on);


      }; // End class LO3mmProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


