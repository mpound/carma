#ifndef SZA_ANTENNA_CORBA_RX3MMPROXY_H
#define SZA_ANTENNA_CORBA_RX3MMPROXY_H

/**
 * @file Rx3mmProxy.h
 *
 * Tagged: Thu Nov 13 16:53:50 UTC 2003
 *
 * @author Erik Leitch
 */
// The CORBA class Rx3mmProxy inherits from

#include "carma/antenna/sza/antenna/corba/RxProxy.h"

namespace carma {
  namespace corba {
    class Server;
  }
}

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * A class which will be served as the CORBA Rx DO, whose
       * methods will send messages to the AntennaMaster message
       * queue.
       */
      class Rx3mmProxy : public RxProxy {

      public:

	/**
	 * Constructor with a pointer to the parent.
	 */
	Rx3mmProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~Rx3mmProxy();


	void setFrequency(double yigFreq, double LOFreq,
			  bool endWithAbsorberInBeam,
			  bool optimizeReceiver,
			  bool forceRelock, // Not used by sza
			  CORBA::ULong seq);

    void registerObject(const std::string& name, carma::corba::Server & server);

      }; // End class Rx3mmProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


