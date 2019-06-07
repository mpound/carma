#ifndef CANMODULE_H
#define CANMODULE_H

/**
 * @file CanModule.h
 * 
 * Started: Tue Jan 13 18:57:18 PST 2004
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/String.h"

#define CARMA_LR_API_113
#define CARMA_QM_API_66

namespace sza {
  namespace util {
    
    /**
     * A class for enumerating recognized CAN modules
     */
    class CanModule {

    public:

      enum Id {
	NONE         =    0x0,
	BIAS         =    0x1,
	CALTERT      =    0x2,
	DCON         =    0x4,
	IFMOD        =    0x8,
	INTMOD       =   0x10,
	LOBE_ROTATOR =   0x20,
	LO_MONITOR   =   0x40, 
	NOISE_SOURCE =   0x80,
	QUAD_MOD     =  0x100,
	RECEIVER     =  0x200,
	THERMAL      =  0x400,
	TILTMETER    =  0x800,
	VARACTOR     =  0x1000,
	YIG          = 0x2000,
	ALL          = BIAS | CALTERT | DCON | IFMOD | INTMOD | LO_MONITOR | LOBE_ROTATOR | NOISE_SOURCE | QUAD_MOD | RECEIVER | THERMAL | TILTMETER | VARACTOR | YIG,
	HARD         = 0x4000 // Not a real module -- used to indicate a hardware reset
      } id_;

      static const short biasApiNo_;
      static const short caltertApiNo_;
      static const short dconApiNo_;
      static const short ifmodApiNo_;
      static const short intmodApiNo_;
      static const short loMonitorApiNo_;
      static const short lobeRotatorApiNo_;
      static const short noiseSourceApiNo_;
      static const short quadModApiNo_;
      static const short receiverApiNo_;
      static const short thermalApiNo_;
      static const short tiltMeterApiNo_;
      static const short varactorApiNo_;
      static const short yigApiNo_;

      static String biasApiStr_;
      static String caltertApiStr_;
      static String ifmodApiStr_;
      static String intmodApiStr_;
      static String loMonitorApiStr_;
      static String lobeRotatorApiStr_;
      static String noiseSourceApiStr_;
      static String quadModApiStr_;
      static String receiverApiStr_;
      static String thermalApiStr_;
      static String tiltMeterApiStr_;
      static String varactorApiStr_;
      static String yigApiStr_;

    }; // End class CanModule
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 

