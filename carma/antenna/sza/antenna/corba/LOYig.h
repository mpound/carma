#ifndef SZA_ANTENNA_CORBA_LOYIG_H
#define SZA_ANTENNA_CORBA_LOYIG_H

#include "carma/antenna/sza/antenna/corba/Corba.h"

#include "carma/antenna/common/LOControl.h"

/**
 * @file LOYig.h
 *
 * Started: Wed Dec 10 23:01:10 UTC 2003
 *
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace control {
      class AntennaMaster;
    };
  };
};

namespace sza {
  namespace antenna {
    namespace corba {

      class LOYig { 
	public:

	/**
	 * Constructor.
	 */
	/**
	 * Constructor with a pointer to the parent AntennaRx
	 */
	LOYig(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	virtual ~LOYig();

	/**
	 * Command to set the frequency.
	 */
	virtual void setFrequency(double yigFreq, double LOfreq);

	/**
	 * Engineering command to turn the sweep on.
	 */
	virtual void toggleSweep(bool on);

	/**
	 * Engineering command to set the Yig frequency.
	 */
	virtual void setLoFrequency(double frequency);

	private:

	/**
	 * Pointer to the parent task, whose send() methods we will
	 * call.
	 */
	sza::antenna::control::AntennaMaster* parent_;

      }; // End class LOYig

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef


