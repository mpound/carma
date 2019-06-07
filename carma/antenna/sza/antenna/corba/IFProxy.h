#ifndef SZA_ANTENNA_CORBA_IFPROXY_H
#define SZA_ANTENNA_CORBA_IFPROXY_H

/**
 * @file IFProxy.h
 *
 * Tagged: Thu Nov 13 16:53:40 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Proxy.h"

#include "carma/antenna/common/IFControl.h"

#include "carma/szautil/Rx.h"

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * A class which will be served as the CORBA IF DO, whose
       * methods will send messages to the AntennaMaster message queue.
       */
      class IFProxy : public Proxy {

      public:

	/**
	 * Constructor with a pointer to the parent AntennaMaster
	 */
	IFProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~IFProxy();

	// Select this receiver as the IF input

	virtual void selectRx();

	virtual void selectBand(unsigned short);

	// Set the IF attenuation for this receiver

	virtual void setAtten(float atten);

	// Iterate to a target power

	virtual void setPower(float power);

	// Set power to preset

	virtual void setPresetPower();

	virtual void reset();

	//------------------------------------------------------------
	// Local methods
	//------------------------------------------------------------

	void setDefaultAtten(sza::util::Attenuation& atten, sza::util::Rx::Id rxId, sza::util::CalPos::Pos pos);

      }; // End class IFProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


