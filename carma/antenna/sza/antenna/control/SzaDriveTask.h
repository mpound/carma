#ifndef SZADRIVETASK_H
#define SZADRIVETASK_H

/**
 * @file SzaDriveTask.h
 * 
 * Tagged: Thu Nov 13 16:53:53 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/szautil/GenericTask.h"
#include "carma/antenna/sza/antenna/control/SzaShare.h"
#include "carma/antenna/sza/antenna/control/AntennaDriveMsg.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      class SzaDriveTask : 
	public sza::util::GenericTask<AntennaDriveMsg> {
	
	public:
	
	/**
	 * Constructor just initializes the shared object pointer to
	 * NULL.
	 */
	inline SzaDriveTask() {
	  share_   = 0;
	};
	
	/**
	 * Make this virtual so that inheritors destructors are
	 * properly called even if they are upcast.
	 */
	inline virtual ~SzaDriveTask() {};
	
	/**
	 * Public method to get a pointer to our shared object.
	 */
	inline SzaShare* getShare();
	
	protected:
	
	SzaShare* share_;
	
      }; // End class SzaDriveTask
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
