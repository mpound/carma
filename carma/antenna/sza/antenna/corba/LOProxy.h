#ifndef SZA_ANTENNA_CORBA_LOPROXY_H
#define SZA_ANTENNA_CORBA_LOPROXY_H

/**
 * @file LOProxy.h
 *
 * Tagged: Thu Nov 13 16:53:40 UTC 2003
 *
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/corba/Proxy.h"

#include "carma/antenna/sza/control/szaLOControl.h"

#include "carma/szautil/LoOsc.h"
#include "carma/szautil/Rx.h"

namespace sza {
  namespace antenna {
    namespace corba {

      /**
       * A class which will be served as the CORBA LO DO, whose
       * methods will send messages to the AntennaMaster message
       * queue.
       */
      class LOProxy : public Proxy {

      public:

	/**
	 * Constructor with a pointer to the parent AntennaMaster
	 */
	LOProxy(sza::antenna::control::AntennaMaster* parent);

	/**
	 * Destructor.
	 */
	~LOProxy();

	virtual void setYigFrequency(double yigFreq);

	virtual void setLoFrequency(double frequency);

	virtual void toggleSweep(bool on);

	virtual void toggleYigSweep(bool on);

	virtual void setLoTerminatorAttenuation(unsigned short atten);

	//-----------------------------------------------------------------------
	// SZA-specific YIG methods
	//-----------------------------------------------------------------------

	virtual void setYigVoltage(unsigned short voltage);

	virtual void setYigLoopGain(unsigned short gain);

	virtual void setYigDampingResistance(unsigned short resistance);

	//-----------------------------------------------------------------------
	// SZA=specific varactor methods
	//-----------------------------------------------------------------------

	virtual void setVaractorLoopGain(unsigned short gain);

	//-----------------------------------------------------------------------
	// SZA-specific BTG methods
	//-----------------------------------------------------------------------

	virtual void setGunnVoltage(unsigned short voltage);

	virtual void setGunnLoopGain(unsigned short gain);

	virtual void setDefaultGunnVoltage(unsigned short voltage);

	//-----------------------------------------------------------------------
	// SZA-specific IntMod methods
	//-----------------------------------------------------------------------

	virtual void setPresetPower();

	//-----------------------------------------------------------------------
	// Local methods
	//-----------------------------------------------------------------------

	void setYigFrequency(sza::util::Rx::Id rxId, double yigFreqInGHz);
	void setDefaultYigFrequency(sza::util::Frequency& freq);
	void setDefaultLOTermAtten(sza::util::Attenuation& atten);
	void setDefaultGunnVoltage(sza::util::Voltage& volt);
	void setLoopGainResistance(sza::util::LoOsc::Osc osc, int loopGain);
	void setDampingGainResistance(int dampGain);

      }; // End class LOProxy

    }; // End namespace corba
  }; // End namespace antenna
}; // End namespace sza

#endif


