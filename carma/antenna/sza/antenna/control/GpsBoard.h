#ifndef GPSBOARD_H
#define GPSBOARD_H

/**
 * @file GpsBoard.h
 * 
 * Tagged: Thu Nov 13 16:53:40 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "carma/antenna/sza/antenna/control/Board.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      
      /**
       * Encapsulate details about the registers of the GPS time-code
       * reader.
       */
      class GpsBoard : public Board {
	
      public:
	
	/**
	 * Constructor function for this board
	 *
	 * @throws Exception
	 */
	GpsBoard(SzaShare* share, std::string name);
	
	/**
	 * Read the current date and time from the time code reader
	 * card, and return it as a Modified Julian Date. Be aware
	 * that this function is expensive and that it can fail.
	 *
	 * @param year int      The current year.
	 * @param mjd  long *   On output the Modified Julian Day 
	 *                      number will be assigned to *mjd.
	 * @param ms   long *   On output the time of day (milli-
	 *                      seconds) will be assigned to *ms.
	 *
	 * @throws Exception
	 */
	void readTime(int year, long* mjd, long* ms);
	
	/**
	 * Enable the leap-year flag of the GPS if the year is a leap
	 * year.
	 *
	 * @throws Exception
	 */
	void enableLeapYear(int year);
	
	/**
	 * Called to intialize.
	 *
	 * @throws Exception
	 */
	void reset();
	
	/**
	 * Activate the time-code-reader board.
	 */
	void connect(int year);
	
	/**
	 * Shutdown the time-code-reader board.
	 *
	 * @throws Exception
	 */
	void disconnect();
	
	/**
	 * A private function of GpsBoard::connect(), called on error.
	 */
	void failedConnect();
	
	/**
	 * Arm the gps time-code reader to output a start pulse at a
	 * given time.
	 *
	 * Input:
	 *  mjd        int     The Modified Julian Day number at which to
	 *                     output the start pulse.
	 *  sec        int     The number of seconds into the day at which to
	 *                     output the start pulse.
	 *
	 * @throws Exception
	 */
	void arm(int mjd, int sec);
	
      private:
	
	/**
	 * The 4 time-code-reader configuration registers 
	 */
	RegMapBlock *config_;  
	
	/**
	 * The register that freezes the time readout when read 
	 */
	RegMapBlock *freeze_;  
	
	/**
	 * The register that unfreezes the time readout when read 
	 */
	RegMapBlock *release_; 
	
	/**
	 * The first of the time readout registers 
	 */
	RegMapBlock *utc_reg0_;
	
	/**
	 * The rest of the time readout registers 
	 */
	RegMapBlock *utc_regs_;
	
	/**
	 * The pulse-rate period registers 
	 */
	RegMapBlock *period_;  
	
	/**
	 * The interrupt setup registers 
	 */
	RegMapBlock *intset_;  
	
	/**
	 * The interrupt std::vector registers 
	 */
	RegMapBlock *intvec_;  
	
	/**
	 * The register that sets the time of the start pulse 
	 */
	RegMapBlock *start_;   
	
	/**
	 * The register that sets the time of the stop pulse 
	 */
	RegMapBlock *stop_;    
	
	/**
	 * True after reporting a time-code-reader error 
	 */
	bool reported_error_;  
	
	/**
	 * True after reporting loss of time-code-reader lock 
	 */
	bool reported_unlock_; 
	
	/**
	 * True after reporting out-of-range day number 
	 */
	bool reported_badday_; 
	
	/**
	 * True after reporting out-of-range day number 
	 */
	bool reported_unreachable_; 
	
	/**
	 * Return true if the passed year is a leap year
	 */
	bool isLeapYear(int year);
	
      }; // End class GpsBoard
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif // End #ifndef 
