#ifndef DRIVETASK_H
#define DRIVETASK_H

/**
 * @file DriveTask.h
 * 
 * Tagged: Thu Nov 13 16:53:36 UTC 2003
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * The following class just enumerates known drive task.
       */
      class DriveTask {
	
      public:
	
	/**
	 * An enumerator to identify a valid drive task.
	 */
	enum Id {
	  UNKNOWN   = 0x1,
	  DRIVE     = 0x2,
	  PMACCOMMS = 0x4
	};
	
      }; // End class DriveTask
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
