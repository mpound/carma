#ifndef SZA_ANTENNA_CORBA_ANTENNAPROXY_H
#define SZA_ANTENNA_CORBA_ANTENNAPROXY_H

/**
 * @file AntennaProxy.h
 *
 * Tagged: Thu Nov 13 16:53:30 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Proxy.h"

#include "carma/antenna/sza/control/szaAntennaControl.h"

// Must undef macro SystemException in carma::util::BaseException.h,
// since this causes references to CORBA::SystemException below to be
// misinterpreted

#ifdef SystemException
#undef SystemException
#endif

namespace carma {
  namespace corba {
    class Server;
  }
}

namespace sza {
  namespace util {
    class AntNum;
  }
}

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * Incomplete specifications lets us use classes without
       * defining them
       */
      class AntennaInitializer;
      class CalibratorProxy;
      class Cryo;
      class DriveProxy;
      class Focus;
      class RxSelector;

      /**
       * Create a AntennaProxy class in namespace carma.  This will be
       * served as the CORBA Antenna DO, by which the ACC will send
       * commands to the AC.
       */
      class AntennaProxy : public Proxy {

      public:

	/**
	 * Constructor with pointer to the parent AntennaMaster
	 * object.
	 *
	 * @throws Exception
	 */
	AntennaProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor
	 */
	~AntennaProxy();

	// SZA-specific IDL method

	virtual void reloadBiasTables();

	/**
	 * Register this object with the CORBA name server
	 *
	 * @throw Exception
	 */
	void registerObject(const std::string& name, carma::corba::Server & server);

	virtual void resetAllCanModules();

	virtual void setInitialization(bool initialized);

	// Local method to initialize the antenna

	void initializeAntenna();

	sza::util::AntNum* getAnt();

      private:

	friend class AntennaInitializer;

	/**
	 * Drive subsystem served up by this object.
	 */
	Cryo*            cryo_;
	CalibratorProxy* cal_;
	DriveProxy*      drive_;
	Focus*           focus_;
	RxSelector*      rxSelector_;

	AntennaInitializer* initializer_;

      }; // End class AntennaProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


