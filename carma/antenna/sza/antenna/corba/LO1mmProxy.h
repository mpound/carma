#ifndef SZA_ANTENNA_CORBA_LO1MMPROXY_H
#define SZA_ANTENNA_CORBA_LO1MMPROXY_H

/**
 * @file LO1mmProxy.h
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
      class LO1mmProxy : public LOProxy {

      public:

	/**
	 * Constructor with a pointer to the parent AntennaMaster
	 */
	LO1mmProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~LO1mmProxy();

	virtual void setYigFrequency(double yigFreq);

	virtual void setLoFrequency(double frequency);

	virtual void toggleSweep(bool on);

	virtual void toggleYigSweep(bool on);

	virtual void setLoTerminatorAttenuation(unsigned short atten);

      }; // End class LO1mmProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


