#ifndef ANTENNATASK_H
#define ANTENNATASK_H

/**
 * @file AntennaTask.h
 * 
 * Tagged: Thu Nov 13 16:53:31 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * The following class just enumerates known antenna tasks
       */
      class AntennaTask {
	
      public:
	
	/**
	 * An enumerator to identify a valid antenna task.
	 */
	enum Id {
	  UNKNOWN = 0x1,
	  DRIVE   = 0x2,
	  MASTER  = 0x4,
	  MONITOR = 0x8,
	  RX      = 0x16
	};
	
      }; // End class AntennaTask
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
