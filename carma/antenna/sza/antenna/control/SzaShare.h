#ifndef SZASHARE_H
#define SZASHARE_H

/**
 * @file SzaShare.h
 * 
 * Tagged: Thu Nov 13 16:53:54 UTC 2003
 * 
 * @author Erik Leitch
 */
#include <string>

#include <pthread.h>

#include "carma/szautil/TimeVal.h"

#include "carma/szautil/AntennaDataFrameManager.h"
#include "carma/szautil/QuadPath.h"

#include "carma/antenna/sza/antenna/control/PolarEncoderPos.h"
#include "carma/antenna/sza/antenna/control/Site.h"

// C header files from the array control code

#include "carma/szaarrayutils/szaregs.h"
#include "carma/szaarrayutils/astrom.h"

namespace sza {
  namespace antenna {
    namespace control {
      
      /**
       * An instance of this class is created by AntennaMaster and
       * passed to the constructors of other tasks. It contains
       * resources that are shared between all SZA tasks.
       */
      class SzaShare : public sza::util::AntennaDataFrameManager {
	
      public:
	
	/**
	 * Enumerate supported timeouts.
	 */
	enum TimeOut {
	  NO_WAIT, 
	  WAIT_FOREVER
	};
	
	//------------------------------------------------------------
	// SzaRegDb Object
	//------------------------------------------------------------
	
	/**
	 * Access to the register database and its shadow registers is
	 * provided through an object of the following type.
	 */
	class SzaRegDb {
	  
	public:
	  
	  
	  /**
	   * Constructor.
	   *
	   * @throws Exception
	   */
	  SzaRegDb();
	  
	  /**
	   * Destructor.
	   */
	  ~SzaRegDb();
	  
	  /**
	   * Acquire exclusive access to the database.
	   *
	   * @throws Exception
	   */
	  void grabRegs(TimeOut timeout);
	  
	  /**
	   * Release the database.
	   *
	   * @throws Exception
	   */
	  void ungrabRegs();
	  
	  /**
	   * Look up a board in the register map
	   */
	  RegMapBoard* findRegMapBoard(std::string boardName);
	  
	  /**
	   * Flag a given register board as unreachable. This sets the
	   * status register of the specified board to 1. Note that
	   * readReg() and writeReg() don't check the status of this
	   * register before accessing other registers, so if you want
	   * to avoid I/O errors, call verifyBoard() before calling
	   * readReg() or writeReg().
	   *
	   *  @param board int  The index of the board to flag.
	   *
	   *  @throws Exception
	   */
	  void flagBoard(int board);
	  
	  /**
	   * Mark a given register board as reachable. This sets the
	   * status register of the specified board to 0.
	   *
	   * @param board int The index of the board to unflag.
	   *
	   * @throws Exception
	   */
	  void unflagBoard(int board);
	  
	  /**
	   * Return the value of the status register of a given
	   * register board.  This will be zero if the board is
	   * reachable, or non-zero otherwise.
	   *
	   * returns    bool    true  - The board is marked as ok.
	   *                   false - The board is marked as unreachable, or
	   *                           an error occured in the function.
	   *
	   * @param board  int The index of the board to verify.
	   *
	   * @throws Exception
	   */
	  bool verifyBoard(int board);
	  
	private:
	  
	  /**
	   * SzaShare will access our members directly.
	   */
	  friend class SzaShare;
	  
	  /**
	   * The SZA register map
	   */
	  SzaRegMap *regmap_;      

	  /**
	   * Database mutual-exclusion guard semaphore
	   */
	  pthread_mutex_t guard_;  
	  
	  /**
	   * Shadow and local register buffer
	   */
	  unsigned *shadow_;       
	  /**
	   * The dimension of shadow[]
	   */
	  int nshadow_;            
	  
	  /**
	   * This is a private function that returns a pointer to the
	   * shadow status register of a given register board. The
	   * caller should lock the database and have checked the
	   * validity of share and board before calling this function.
	   *
	   * returns  unsigned *  The pointer alias of the shadow 
	   *                      status register.
	   * @param board  int  The index of the board.
	   */
	  unsigned* boardStatusReg(int board);
	  
	};
	
	//------------------------------------------------------------
	// SzaClock Object
	//------------------------------------------------------------
	
	/**
	 * The following structure is used by the SZA UTC clock.
	 */
	class SzaClock {
	  
	public:
	  
	  /** 
	   * Constructor.
	   */
	  SzaClock();
	  
	  /**
	   * Destructor.
	   *
	   * @throws Exception
	   */
	  ~SzaClock();
	  
	  /**
	   * Set the current time.
	   *
	   * @throws Exception
	   */
	  void setClock(unsigned long mjd, unsigned long sec, 
			unsigned long nanoSeconds);
	  
	  /**
	   * Set the current time via a TimeVal ref.
	   *
	   * @throws Exception
	   */
	  void setClock(sza::util::TimeVal& time);
	  
	  /**
	   * Fill the internal time representation with the current
	   * time.
	   *
	   * @throws Exception
	   */
	  void setClock();

	  /**
	   * Get the current UTC as a Modified Julian Date.
	   *
	   * @throws Exception
	   */
	  double getUtc();
	  
	private:
	  
	  /**
	   * The mutual exclusion guard of the clock
	   */
	  pthread_mutex_t guard_; 
	  
	  sza::util::TimeVal time_;
	};
	
	//----------------------------------------------------------------------
	// SzaAstrom Object
	//------------------------------------------------------------
	
	/**
	 * Encapsulate the astrometry details of the array in this
	 * class.
	 */
	class SzaAstrom {
	  
	public:
	  
	  /**
	   * Constructor.
	   *
	   * @throws Exception
	   */
	  SzaAstrom();
	  
	  /**
	   * Destructor.
	   */
	  virtual ~SzaAstrom();
	  
	  /**
	   * Record new site-location details in share->site.
	   *
	   * @throws Exception
	   */
	  virtual void setSite(double longitude, double latitude, double altitude);
	  
	  /**
	   * Get a copy of the SZA site-specification object.
	   *
	   * @throws Exception
	   */
	  void getSite(sza::antenna::control::Site *site);
	  
	  /**
	   * Extend the quadratic interpolation table of ut1 - utc
	   * versus MJD UTC.
	   *
	   * @throws Exception
	   */
	  void extendUt1Utc(double utc, double ut1utc);
	  
	  /**
	   * Extend the quadratic interpolation table of the equation
	   * of the equinoxes versus Terrestrial Time (as a Modified
	   * Julian Date).
	   *
	   * @throws Exception
	   */
	  void extendEqnEqx(double tt, double eqneqx);
	  
	  /**
	   * Get the value of UT1-UTC for a given UTC.
	   *
	   * @throws Exception
	   */
	  double getUt1Utc(double utc);
	  
	  /**
	   * Get the value of the equation of the equinoxes for a
	   * given terrestrial time.
	   *
	   * @throws Exception
	   */
	  double getEqnEqx(double tt);
	  
	private:
	  
	  /**
	   * The mutual exclusion guard of this shared object
	   */
	  pthread_mutex_t guard_;   
	  
	  /**
	   * The location of the SZA
	   */
	  sza::antenna::control::Site site_; 
	  
	  /**
	   * Quadratic interpolation table of UT1-UTC in seconds,
	   * versus UTC, expressed as a Modified Julian Date.
	   */
	  sza::util::QuadPath* ut1utc_;     
	  
	  /**
	   * Quadratic interpolation table of the equation of the
	   * equinoxes in radians, versus Terrestrial Time expressed
	   * as a Modified JulianDate.
	   */
	  sza::util::QuadPath* eqneqx_;     
	  
	}; // Class SzaAstrom
	
	//------------------------------------------------------------
	// SzaPmacLock Object
	//------------------------------------------------------------
	
	/**
	 * The following structure encapsulates the reader lock used
	 * to prevent clashes when freezing and unfreezing the pmac
	 * monitor registers.
	 */
	class SzaPmacLock { 
	  
	public:
	  
	  /**
	   * Constructor.
	   *
	   * @throws Exception
	   */
	  SzaPmacLock(SzaShare *share);
	  
	  /**
	   * Destructor.
	   *
	   * @throws Exception
	   */
	  ~SzaPmacLock();
	  
	private:
	  
	  /**
	   * The mutex that protects access to the counting semaphore
	   */
	  pthread_mutex_t guard_;  
	  
	  /**
	   * The number of readers
	   */
	  int count_;              
	  
	  /**
	   * The pmac register-map entry
	   */
	  RegMapBoard *pmac_;      
	  
	  /**
	   * The pmac.host_read register
	   */
	  RegMapBlock *host_read_; 

	}; // End class SzaPmacLock
	
	//------------------------------------------------------------
	// SzaShare Methods
	//------------------------------------------------------------
	
	/**
	 * Struct used to store names and IP addresses of trusted
	 * hosts.
	 */
	struct HostAddress {
	  char* name;
	  char* address;
	};
	
	// Static members and functions
	
	/**
	 * Static pointer for use in signal handlers.
	 */
	static SzaShare* share;
	
	/**
	 * An array of trusted hosts.
	 */
	static HostAddress host_address[];
	
	/**
	 * Validate a specified IP address or host-name alias, and
	 * return a malloc'd copy of the resulting IP address.
	 *
	 * returns  char *      The IP address of the host, or NULL on error.
	 *                      This should be free'd when no longer required.
	 *
	 * @param  host char *  The IP address or host-name alias of the host.
	 */
	static std::string hostIpAddress(std::string host);
	
	virtual void setTuningPending(bool pending) {};


	/**
	 * This is a signal handler for trapping bus and address
	 * errors while readReg() and writeReg() access PCI
	 * registers. It simply calls longjmp to abort the I/O
	 * operation and return to an error catching part of the
	 * originating function.
	 */
	static void pciBusErrorHandler(int sig);
	
	/**
	 * This is a signal handler for trapping bus and address
	 * errors while readReg() and writeReg() access PCI
	 * registers. It simply calls longjmp to abort the I/O
	 * operation and return to an error catching part of the
	 * originating function.
	 */
	static void pciSegvErrorHandler(int sig);
	
	/**
	 * Constructor.
	 *
	 * @throws Exception
	 */
	SzaShare(std::string host);
	
	/**
	 * Destructor.
	 */
	~SzaShare();
	
	/**
	 * Acquire exlusive use of the register database.  The
	 * following grab and ungrab calls can be used to bracket
	 * multiple calls to (raw_)read_sza_reg() and/or
	 * (raw_)write_sza_reg() to prevent other tasks from gaining
	 * access to the SZA registers between calls. These functions
	 * must be called by users of the raw_ read and write
	 * functions.
	 *
	 * @throws Exception
	 */
	void grabRegs(TimeOut timeout);
	
	/**
	 * Relinquish exclusive use to the register database.
	 *
	 * @throws Exception
	 */
	void ungrabRegs();
	
	/*
	 * Flag a board as unreachable. Note that this is called
	 * automatically by read_regdb() and write_regdb() if their
	 * 'check' argument is true and an exception occurs. If
	 * 'check' is false then the caller is expected to perform
	 * exception handling and to call the following function if an
	 * exception occurs.
	 *
	 * @throws Exception
	 */
	void flagBoard(int board);
	
	/*
	 * This function will be called by the scanner task after
	 * successfully reading all registers of a previously flagged
	 * board without incuring an exception. It marks the board as
	 * usable by other tasks.
	 *
	 * @throws Exception
	 */
	void unflagBoard(int board);
	
	/**
	 * Return false if a board is flagged as unreachable.
	 *
	 * @throws Exception
	 */
	bool verifyBoard(int board);
	
	/**
	 * Get the Local Sidereal Time that corresponds to a given MJD UTC.
	 *
	 * @throws Exception
	 */
	virtual double getLst(double utc);
	
	/**
	 * Get the terrestrial time as MJD
	 */
	double getTt(double lst);
	
	/**
	 * Return the Utc as MJD
	 *
	 * @throws Exception
	 */
	double getUtc();
	
	/**
	 * Set the current time.
	 *
	 * @throws Exception
	 */
	void setClock(unsigned long mjd, unsigned long sec, 
		      unsigned long nanoSeconds);
	
	/**
	 * Set the current time via a TimeVal ref.
	 *
	 * @throws Exception
	 */
	void setClock(sza::util::TimeVal& time);

	/**
	 * Set the current time.
	 *
	 * @throws Exception
	 */
	void setClock();

	/**
	 * Public function to set the site parameters
	 *
	 * @throws Exception
	 */
	virtual void setSite(double longitude, double latitude, double altitude);
	
	/**
	 * Get the value of UT1-UTC for a given UTC.
	 *
	 * @throws Exception
	 */
	double getUt1Utc(double utc);
	
	/**
	 * Get the value of the equation of the equinoxes for a given
	 * terrestrial time.
	 *
	 * @throws Exception
	 */
	double getEqnEqx(double tt);
	
	/**
	 * Extend the quadratic interpolation table of ut1 - utc
	 * versus MJD UTC.
	 *
	 * @throws Exception
	 */
	void extendUt1Utc(double utc, double ut1utc);
	
	/**
	 * Extend the quadratic interpolation table of the equation of
	 * the equinoxes versus Terrestrial Time (as a Modified Julian
	 * Date).
	 *
	 * @throws Exception
	 */
	void extendEqnEqx(double tt, double eqneqx);
	
	/**
	 * Return a pointer to the requested register
	 */
	RegMapBoard* findRegMapBoard(std::string boardName);
	
	/**
	 * Freeze the pmac readout
	 *
	 * @throws Exception
	 */
	void freezePmacReadout();
	
	/**
	 * Un-freeze the pmac readout
	 *
	 * @throws Exception
	 */
	void unfreezePmacReadout();
	
	/**
	 * Return the number of boards in the register map
	 */
	unsigned int getNboard();
	
	/**
	 * Return the number of archived registers in the register map
	 */
	unsigned int getNarchived();
	
	/**
	 * Return the total number of registers in the register map
	 */
	unsigned int getNreg();

	/**
	 * Return the number of bytes
	 */
	unsigned int getNbyte();

	/**
	 * Public method to pack a frame
	 *
	 * @throws Exception
	 */
	void packFrame(sza::util::DataFrameManager* frame);
	
	RegMap* getRegMap();

      private:
	
	/**
	 * A temporary buffer for use in read() methods
	 */
	unsigned* tmpbuf_;         
	
	/**
	 * The IP address of the control host
	 */
	std::string controlHost_;       
	
	/**
	 * The SZA register database
	 */
	SzaRegDb *regdb_;          
	
	/**
	 * The SZA UTC clock
	 */
	SzaClock *clock_;          
	
	/**
	 * Astrometry information
	 */
	SzaAstrom *astrom_;        
	
	/**
	 * PMAC dual-port-ram read-lock
	 */
	SzaPmacLock *pmac_lock_;   
	
      }; // class SzaShare
      
    }; // End namespace control
  }; // End namespace antenna
}; // End namespace sza

#endif
