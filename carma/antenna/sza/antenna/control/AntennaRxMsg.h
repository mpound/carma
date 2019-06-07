#ifndef SZA_ANTENNA_CONTROL_ANTENNARXMSG_H
#define SZA_ANTENNA_CONTROL_ANTENNARXMSG_H

/**
 * @file AntennaRxMsg.h
 * 
 * Tagged: Thu Nov 13 16:53:31 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/GenericTaskMsg.h"

// Needed for NetEnum

#include "carma/szaarrayutils/netobj.h"

// Needed for CalTertMsg

#include "carma/szaarrayutils/rtcnetcoms.h"

#include "carma/szautil/Amp.h"
#include "carma/szautil/CalPos.h"
#include "carma/szautil/CanModule.h"
#include "carma/szautil/CalTertTypes.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/IFAtten.h"
#include "carma/szautil/LoOsc.h"
#include "carma/szautil/LoStage.h"
#include "carma/szautil/Rx.h"
#include "carma/szautil/Thermal.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * Class to encapsulate messages send to the Rx task
       */
      class AntennaRxMsg :
	public sza::util::GenericTaskMsg {
	
	public:
	
	/**
	 * Enumerate supported AntennaRx messages
	 */
	enum MsgType {
	  FLAG_BOARD,
	  FREQ, 
	  LO,
	  OFFSET, 
	  TOTALPOWER,
	  SET_INPUT_VOLTAGE,
	  SET_DRAIN_CURRENT,
	  TOGGLE_FAST_SAMPLING,
	  CALTERT,
	  INTMOD,
	  SET_BIAS,
	  IFMOD,
	  THERMAL,
	  TILTMETER,
	  SELECT_RX, // Select a receiver
	  RESET,     // Reset a CAN module   
	  EXECUTE_NEXT_CAN_INSTRUCTION
	};
	
	/**
	 * The type of this message
	 */
	MsgType type;
	
	/**
	 * Define a Message container
	 */
	union {
	  
	  /**------------------------------------------------------------
	   * Flag a board.
	   */
	  struct {
	    unsigned short board; // The register map index of the
	    // board to un/flag
	    bool flag;            // True to flag, false to unflag
	  } flagBoard;
	  
	  /**------------------------------------------------------------
	   * Control the LO chain
	   */
	  struct {
	    sza::array::LoMsgId       msgId;
	    sza::util::LoOsc::Osc     oscs;
	    sza::util::LoStage::Stage stages;
	    sza::util::Rx::Id         rxId;
	    bool                      on;
	    unsigned short            dampGain;
	    unsigned short            frequency;
	    unsigned short            loopGain;
	    unsigned short            voltage;
	    unsigned char             id;
	    unsigned char             day;
	    unsigned char             month;
	    unsigned char             year;
	    unsigned char             npt;
	    float                     coeff;
	    int                       tunerPos;
	    int                       backshortPos;
	    int                       attenPos;
	    int                       position;
	  } lo;
	  
	  /**------------------------------------------------------------
	   * Install offsets needed to bring this receiver on-axis
	   */
	  struct {
	    double az;
	    double el;
	  } offset;
	  
	  /**------------------------------------------------------------
	   * Set a input voltage
	   */
	  struct {
	    sza::util::Rx::Id rx;
	    sza::util::Amp::Type amp;
	    sza::util::Amp::Stage stage;
	    sza::util::Amp::Bias biasType;
	    short bias;
	  } inputVoltage;

	  /**------------------------------------------------------------
	   * Set a drain current
	   */
	  struct {
	    sza::util::Rx::Id rx;
	    sza::util::Amp::Type amp;
	    sza::util::Amp::Stage stage;
	    float current;
	  } drainCurrent;

	  /**------------------------------------------------------------
	   * Toggle fast sampling
	   */
	  struct {
	    unsigned int channel;
	    bool start;
	  } fastSampling;

	  /**------------------------------------------------------------
	   * Control the Calibrator/Tertiary CAN module
	   */
	  struct {
	    sza::array::CalTertMsg msgId;
	    sza::util::Rx::Id rxId;
	    short tertPosition;
	    sza::util::CalPos::Pos calPosition;
	    bool enable;
	    sza::util::CalTertTypes::OwDevice owDevice;
	    sza::util::CalTertTypes::OwCommand owCommand;
	    unsigned long seq;
	    double tertPosDegrees;
	  } calTert;

	  /**------------------------------------------------------------
	   * Control the Berkeley interface module
	   */
	  struct {
	    sza::array::IntModMsg msgId;
	    unsigned char atten;
	  } intMod;

	  /**------------------------------------------------------------
	   * Set a bias
	   */
	  struct {
	    unsigned amp;
	    short bias;
	    sza::array::BiasType biasType;
	    sza::util::Rx::Id rxId;
	    unsigned seq;
	    bool isDefault;
	  } setBias;

	  /**------------------------------------------------------------
	   * Control the Antenna IF module
	   */
	  struct {
	    sza::array::IFModMsg msgId;
	    sza::util::Rx::Id band;
	    float level;
	    sza::util::IFAtten::Type attenSet;
	    float total;
	    float input;
	    float output;
	    unsigned seq;
	    sza::util::CalPos::Pos pos;
	  } IFMod;

	  /**------------------------------------------------------------
	   * Control the Thermal Module
	   */
	  struct {
	    sza::array::ThermalMsg msgId;
	    sza::util::Thermal::Target target;
	    float value;
	    sza::util::Thermal::BoxMode mode;
	    bool eqState;
	  } thermal;

	  /**------------------------------------------------------------
	   * Control the Tiltmeter Module
	   */
	  struct {
	    sza::array::TiltmeterMsg msgId;
	    sza::array::TiltmeterMode mode;
	    float value;
	    float afZero;
	    float lrZero;
	  } tiltmeter;

	  // CAN module commands
	  
	  struct {
	    sza::util::CanModule::Id modules;
	    bool hardwareReset;
	  } reset;
	  
	  // Select a receiver

	  struct {
	    sza::util::Rx::Id rxBand;
	    unsigned seq;
	  } selectRx;

	  struct {
	    MsgType type; // The network command this corresponds to
	  } canCommand;

	} body;
	
	//------------------------------------------------------------
	// Methods for packing messages
	//------------------------------------------------------------
	
	inline void packFlagBoardMsg(unsigned short board, bool flag)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = FLAG_BOARD;
	    body.flagBoard.board = board;
	    body.flagBoard.flag = flag;
	  }
	
	inline void packLoMsg(sza::array::LoMsgId msgId,
			      sza::util::LoOsc::Osc oscs,
			      sza::util::LoStage::Stage stages, 
			      sza::util::Rx::Id rxId,
			      bool on,
			      unsigned short dampGain,
			      unsigned short frequency,
			      unsigned short loopGain,
			      unsigned short voltage,
			      unsigned char id,
			      unsigned char day,
			      unsigned char month,
			      unsigned char year,
			      unsigned char npt,
			      float coeff,
			      int tunerPos,
			      int backshortPos,
			      int attenPos,
			      int position)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    type              = LO;

	    body.lo.msgId     = msgId;
	    body.lo.oscs      = oscs;
	    body.lo.stages    = stages;
	    body.lo.rxId      = rxId;
	    body.lo.on        = on;
	    body.lo.dampGain  = dampGain;
	    body.lo.frequency = frequency;
	    body.lo.loopGain  = loopGain;
	    body.lo.voltage   = voltage;
	    body.lo.id        = id;
	    body.lo.day       = day;
	    body.lo.month     = month;
	    body.lo.year      = year;	  
	    body.lo.npt       = npt;	  
	    body.lo.coeff     = coeff;	  
	    body.lo.tunerPos  = tunerPos;	  
	    body.lo.backshortPos = backshortPos;	  
	    body.lo.attenPos  = attenPos;	  
	    body.lo.position  = position; 
	  }
	
	inline void packOffsetMsg(double az, double el)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = OFFSET;
	    body.offset.az = az;
	    body.offset.el = el;
	  }
	
	inline void packSetDrainCurrentMsg(sza::util::Rx::Id rx, 
					   sza::util::Amp::Type amp, 
					   sza::util::Amp::Stage stage, 
					   float current)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = SET_DRAIN_CURRENT;

	    body.drainCurrent.rx = rx;
	    body.drainCurrent.amp = amp;
	    body.drainCurrent.stage = stage;
	    body.drainCurrent.current = current;
	  }

	inline void packSetInputVoltageMsg(sza::util::Rx::Id rx, 
					   sza::util::Amp::Type amp, 
					   sza::util::Amp::Stage stage, 
					   sza::util::Amp::Bias biasType,
					   short bias)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = SET_INPUT_VOLTAGE;

	    body.inputVoltage.rx = rx;
	    body.inputVoltage.amp = amp;
	    body.inputVoltage.stage = stage;
	    body.inputVoltage.biasType = biasType;
	    body.inputVoltage.bias    = bias;
	  }

	inline void packToggleFastSamplingMsg(unsigned int channel,
					      bool start)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = TOGGLE_FAST_SAMPLING;

	    body.fastSampling.channel = channel;
	    body.fastSampling.start = start;
	  }

	inline void packSetBiasMsg(unsigned amp, short bias, 
				   sza::array::BiasType biasType,
				   sza::util::Rx::Id rxId,
				   unsigned seq, bool isDefault)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = SET_BIAS;

	    body.setBias.amp = amp;
	    body.setBias.bias = bias;
	    body.setBias.biasType = biasType;
	    body.setBias.rxId = rxId;
	    body.setBias.seq = seq;
	    body.setBias.isDefault = isDefault;
	  }

	inline void packIntModMsg(sza::array::IntModMsg msgId,
				  unsigned char atten=0)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = INTMOD;

	    body.intMod.msgId = msgId;
	    body.intMod.atten = atten;
	  }

	inline void packCalTertMsg(sza::array::CalTertMsg msgId, 
				   sza::util::Rx::Id rxId, 
				   short tertPosition, 
				   sza::util::CalPos::Pos calPosition, 
				   bool enable,
				   sza::util::CalTertTypes::OwDevice device,
				   sza::util::CalTertTypes::OwCommand command,
				   unsigned long seq)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    DBPRINT(true, sza::util::Debug::DEBUG5, "Seq = " << seq);

	    setCarmaSequenceNumber();
	    type = CALTERT;

	    body.calTert.msgId        = msgId;
	    body.calTert.rxId         = rxId;
	    body.calTert.tertPosition = tertPosition;
	    body.calTert.calPosition  = calPosition;
	    body.calTert.enable       = enable;
	    body.calTert.owDevice     = device;
	    body.calTert.owCommand    = command;
	    body.calTert.seq          = seq;
	  }

	inline void packNewCalTertMsg(sza::array::CalTertMsg msgId, 
				      sza::util::CalPos::Pos calPosition, 
				      sza::util::Rx::Id rxId, 
				      double tertPosDegrees,
				      unsigned long seq)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    DBPRINT(true, sza::util::Debug::DEBUG5, "Seq = " << seq);

	    setCarmaSequenceNumber();
	    type = CALTERT;

	    body.calTert.msgId          = msgId;
	    body.calTert.calPosition    = calPosition;
	    body.calTert.rxId           = rxId;
	    body.calTert.tertPosDegrees = tertPosDegrees;
	    body.calTert.seq            = seq;
	  }

	// Pack an antenna IF message

	inline void packIFModMsg(sza::array::IFModMsg msgId,
				 sza::util::Rx::Id band,
				 float level,
				 sza::util::IFAtten::Type attenSet,
				 float total,
				 float input,
				 float output,
				 unsigned seq,
				 sza::util::CalPos::Pos pos)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = IFMOD;
	    
	    body.IFMod.msgId    = msgId;
	    body.IFMod.band     = band;
	    body.IFMod.level    = level;
	    body.IFMod.attenSet = attenSet;
	    body.IFMod.total    = total;
	    body.IFMod.input    = input;
	    body.IFMod.output   = output;
	    body.IFMod.seq      = seq;
	    body.IFMod.pos      = pos;
	  }

	inline void packThermalMsg(sza::array::ThermalMsg msgId,
				   sza::util::Thermal::Target target,
				   float value,
				   sza::util::Thermal::BoxMode mode,
				   bool eqState)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = THERMAL;

	    body.thermal.msgId   = msgId;
	    body.thermal.target  = target;
	    body.thermal.value   = value;
	    body.thermal.mode    = mode;
	    body.thermal.eqState = eqState;
	  }

	inline void packTiltmeterMsg(sza::array::TiltmeterMsg msgId,
				     sza::array::TiltmeterMode mode,
				     float value,
				     float afZero=0.0,
				     float lrZero=0.0)

	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = TILTMETER;

	    body.tiltmeter.msgId   = msgId;
	    body.tiltmeter.mode    = mode;
	    body.tiltmeter.value   = value;
	    body.tiltmeter.afZero  = afZero;
	    body.tiltmeter.lrZero  = lrZero;
	  }

	// Generic message to reset a CAN module.
	
	inline void packResetMsg(sza::util::CanModule::Id modules, bool hard)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = RESET;
	    
	    body.reset.modules       = modules;
	    body.reset.hardwareReset = hard;
	  }
	
	// Generic message to reset a CAN module.
	
	inline void packSelectRxMsg(sza::util::Rx::Id rxBand, unsigned seq)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = SELECT_RX;
	    
	    body.selectRx.rxBand = rxBand;
	    body.selectRx.seq = seq;

	    COUT("packSelectRxMsg seq = " << seq);

	  }

	inline void packExecuteNextCanInstructionMsg(MsgType commandType)
	  {
	    genericMsgType_ = 
	      sza::util::GenericTaskMsg::TASK_SPECIFIC;
	    
	    setCarmaSequenceNumber();
	    type = EXECUTE_NEXT_CAN_INSTRUCTION;

	    body.canCommand.type = commandType;
	  }

	/**
	 * Allows cout << AntennaRxMsg
	 */
	friend std::ostream& operator<<(std::ostream& os, AntennaRxMsg* msg);

      }; // End class AntennaRxMsg
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
