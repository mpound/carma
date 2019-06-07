#ifndef SLOWWALSH_H
#define SLOWWALSH_H

/**
 * @file SlowWalsh.h
 * 
 * Tagged: Thu Nov 13 16:53:52 UTC 2003
 * 
 * @author Erik Leitch
 */
/**
 * The bit in the frame->walshstate register that specifies whether
 * slow walshing is turned on or off.  This is a bit redundant, since
 * the 13 receiver walshstate bits will be 0 anyway, but we will use
 * it as a convenient flag for the routine which toggles the walsh
 * states to tell if it should stop.
 */
#define WALSH_ON_BIT 14

/**
 * One full Walsh cycle will be 16 states.  This will also be the
 * number of Walsh functions available.
 */
#define WALSH_PERIOD 16

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * A class to encapsulate slow walsh functions
       */
      class SlowWalsh {
	
      public:
	
	/**
	 * Constructor with initialization. Throws exception if iwalsh
	 * is out of range.
	 *
	 * @throws Exception
	 */
	SlowWalsh(unsigned short iwalsh);
	
	/**
	 * Constructor without initialization
	 */
	SlowWalsh();
	
	/**
	 * Set the walsh function.  Throws exception if iwalsh is out
	 * of range.
	 *
	 * @throws Exception
	 */
	void changeFunction(unsigned short iwalsh);
	
	/**
	 * Return true when this object has been initialized.
	 */
	bool isInitialized();
	
	/**
	 * Pointer to a selected Walsh function
	 */
	const int* walshFunction_;
	
      private:
	
	/**
	 * Define a set of walsh functions.  These will be used to
	 * modulate the inputs for each receiver to the correlator.
	 * When the walsh state is 1, we will insert a 0-degree phase
	 * shift, when it is -1, we will insert 180.
	 */
	static const int walshFunctions_[WALSH_PERIOD][WALSH_PERIOD];
	
	/**
	 * True when this objecy has been initialized.
	 */
	bool initialized_;
	
	/**
	 * Return true if the passed index is in range.
	 */
	bool isValidIndex(unsigned short iwalsh);
	
      }; // End class SlowWalsh
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
