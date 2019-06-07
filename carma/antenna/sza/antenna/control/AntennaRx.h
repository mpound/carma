#ifndef ANTENNARX_H
#define ANTENNARX_H

/**
 * @file AntennaRx.h
 * 
 * Tagged: Thu Nov 13 16:53:30 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/GenericTask.h"

#include "carma/antenna/sza/antenna/control/AntennaRxMsg.h"
#include "carma/antenna/sza/antenna/control/SzaTask.h"
#include "carma/szautil/Directives.h"

#if DIR_HAVE_CARMA
#include "carma/antenna/sza/antenna/canbus/CanCommand.h"
#include "carma/antenna/sza/antenna/canbus/SelectRxCommand.h"
#include "carma/antenna/sza/antenna/canbus/SetBiasCommand.h"
#endif

#include <map>

namespace sza {
  namespace antenna {

#if DIR_HAVE_CARMA
    namespace canbus {
      class IFMod;
      class BiasTunedGunn;
      class CalTert;
      class IntMod;
      class Receiver;
      class Thermal;
      class TiltMeter;
      class VaractorTunedGunn;
      class Yig;
    }
#endif

    namespace control {

      /**
       * Incomplete type specification for AntennaMaster lets us
       * declare it as a friend below without defining it
       */
      class AntennaMaster;
      
      /**
       * AntennaRx class will handle all receiver functions.  This class
       * instantiates proxy objects for all receiver subsystems, which
       * can be served as CORBA distributed objects.
       */
      class AntennaRx : 
	public SzaTask,
	public sza::util::GenericTask<AntennaRxMsg> {
	
	public:
	
	/**
	 * Register completion of a sequence-number marked transactionxo
	 */
	void registerDone();

	void registerCarmaRequest(AntennaRxMsg* msg);
	void sendCarmaSeqNoMsg(bool success);

	/**
	 * Send a message to the parent that the caltert has completed
	 * a transaction
	 */
	void sendCalTertDoneMsg(unsigned int seq);

	/**
	 * Send a message to the parent that the IFMod has completed
	 * a transaction
	 */
	void sendIFModDoneMsg(unsigned int seq);

#if DIR_HAVE_CARMA
	/**
	 * Send a message to the parent that an instruction has
	 * completed
	 */
	static CAN_EXECUTE_DONE_HANDLER(sendExecuteNextCanInstructionMsg);

	/**
	 * Send a message to the parent that a CAN command has
	 * completed
	 */
	static CAN_COMMAND_DONE_HANDLER(sendCanCommandDoneMsg);

	/**
	 * Send a message to the parent that a selectRx command has
	 * completed
	 */
	static CAN_COMMAND_DONE_HANDLER(sendSelectRxCommandDoneMsg);

	/**
	 * Send a message to the parent that a select rx command has
	 * completed
	 */
	static CAN_COMMAND_FAILED_HANDLER(canCommandFailed);

	static CAN_COMMAND_FAILED_HANDLER(selectRxCommandFailed);

#endif

	sza::util::AntNum* getAnt();

	void setTuningPending(bool pending);

	AntennaMaster* parent() {
	  return parent_;
	}

	private:
	
	/**
	 * Maintain information about the receiver currently selected
	 */
	sza::util::Rx::Id rxId_;
	
	/**
	 * We declare AntennaMaster a friend because its
	 * startAntennaRx() method will call our private serviceMsgQ()
	 * method, as well as our private sendTaskMsg() method.
	 */
	friend class AntennaMaster;
	
	/**
	 * A pointer to the parent task resources
	 */
	AntennaMaster* parent_;
	
	static AntennaRx* antennaRx_;
	
	std::map<sza::util::GenericTaskMsg::CarmaSeqNoType, unsigned long> lastReqCarmaSeqNo_;
	std::map<sza::util::GenericTaskMsg::CarmaSeqNoType, unsigned long> lastAckCarmaSeqNo_;
	sza::util::GenericTaskMsg::CarmaSeqNoType carmaSeqNoType_;

	// Declare CAN commands this class will execute

#if DIR_HAVE_CARMA
	friend class sza::antenna::canbus::SelectRxCommand;
	class sza::antenna::canbus::SelectRxCommand selectRxCommand_;

	friend class sza::antenna::canbus::SetBiasCommand;
	sza::antenna::canbus::SetBiasCommand setBiasCommand_;
#endif

	/**
	 * Constructor. Making this private prevents instantiation by
	 * anyone but AntennaMaster.
	 *
	 * @throws Exception
	 */
	AntennaRx(AntennaMaster* parent);
	
	/**
	 * Destructor.
	 *
	 * @throws Exception
	 */
	~AntennaRx();
	
	/**
	 * Process a message received on the AntennaRx message queue.
	 *
	 * @throws Exception
	 */
	void processMsg(AntennaRxMsg* msg);

#if DIR_HAVE_CARMA
	/**
	 * CAN devices managed by this task.
	 */
	sza::antenna::canbus::IFMod*             IFMod_;
	sza::antenna::canbus::BiasTunedGunn*     gunn_;
	sza::antenna::canbus::CalTert*           calTert_;
	sza::antenna::canbus::IntMod*            intMod_;
	sza::antenna::canbus::Receiver*          receiver_;
	sza::antenna::canbus::Thermal*           thermal_;
	sza::antenna::canbus::TiltMeter*         tiltmeter_;
	sza::antenna::canbus::VaractorTunedGunn* varactor_;
	sza::antenna::canbus::Yig*               yig_;
	
	/**
	 * Install devices on the CAN network that we are interested
	 * in.
	 */
	void installCanDevices();

	/**
	 * Process a message for a CAN module
	 */
	void processCanMsg(AntennaRxMsg* msg);

	/**
	 * Process a message for the caltert module
	 */
	void processCalTertMsg(AntennaRxMsg* msg);

	/**
	 * Process a message for the antenna IF module
	 */
	void processIFModMsg(AntennaRxMsg* msg);

	/**
	 * Process a message for the interface module
	 */
	void processIntModMsg(AntennaRxMsg* msg);

	/**
	 * Process a message for the thermal control module
	 */
	void processThermalMsg(AntennaRxMsg* msg);

	/**
	 * Process a message for the tiltmeter control module
	 */
	void processTiltMeterMsg(AntennaRxMsg* msg);

	/**
	 * Process a message for an LO module
	 */
	void processLoMsg(AntennaRxMsg* msg);

	/**
	 * Toggle LO stages
	 */
	void toggleLo(sza::util::LoOsc::Osc osc, 
		      sza::util::LoStage::Stage stages,
		      bool on);


	void stepCanCommand(AntennaRxMsg::MsgType type);
#endif
	
	/**
	 * Forward a message to the tracker task that we are changing
	 * frequencies
	 */
	void sendTrackerRxMsg(sza::util::Rx::Id rxId);

	bool moveTertiary(sza::util::Rx::Id rxId);

      }; // End class AntennaRx
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif



