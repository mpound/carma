#ifndef LOSTAGE_H
#define LOSTAGE_H

/**
 * @file LoStage.h
 * 
 * Started: Wed Dec 10 23:48:07 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    /**
     * Class to enumerate recognized LO stages.
     */
    class LoStage {
    public:
      
      enum Stage {
	LO_IF        = 0x1,
	LO_SWEEP     = 0x2,
	LO_GUNN      = 0x4,
	LO_RFMONITOR = 0x8,
	LO_IFMONITOR = 0x10,
	LO_ATTEN     = 0x20,
	LO_BACKSHORT = 0x40,
	LO_TUNER     = 0x80,
	LO_ALL       = LO_IF|LO_SWEEP|LO_GUNN|LO_RFMONITOR|LO_IFMONITOR|LO_ATTEN|LO_BACKSHORT|LO_TUNER
      };
      
    }; // End class LoStage
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 


