// Standard includes 

#include <iostream>

#include <unistd.h>
#include <signal.h>
#include <pthread.h>
#include <time.h>

#include <setjmp.h>

// Include the following for inet_addr() 

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

// C includes

#include "carma/szaarrayutils/szaconst.h"
#include "carma/szaarrayutils/scanner.h" // SCAN_BUFF_SIZE etc.

// SZA includes 

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/TimeVal.h"
#include "carma/szautil/Thread.h"

#include "carma/antenna/sza/antenna/control/SzaShare.h"

using namespace std;
using namespace sza::util;
using namespace sza::antenna::control;

/**
 * While performing PCI I/O, bus-error and address-error signal
 * handlers will be installed. These will return the task to a safe
 * place via the environment stored in bus_error_env.
 */
static jmp_buf pci_error_env;

//-----------------------------------------------------------------------
// SzaShare::SzaRegDb Object methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Create the register database.
 */
SzaShare::SzaRegDb::SzaRegDb()
{
  int i;
  LogStream errStr;

  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_SzaRegDb().
  
  regmap_  = 0;
  shadow_  = 0;
  nshadow_ = 0;
  
  // Create the SZA antenna register map.
  
  regmap_ = new_SzaAntRegMap();
  
  if(!regmap_) {
    errStr.appendMessage(true, "Unable to allocate regmap");
    throw Error(errStr);
  }
  
  nshadow_ = regmap_->nreg_;
  
  // Allocate the mutual exclusion semaphore that will protect access
  // to the database.
  
  pthread_mutex_init(&guard_, NULL);
  
  // Allocate the shadow-register array.
  
  shadow_ = (unsigned *) malloc(sizeof(unsigned) * nshadow_);
  if(shadow_ == 0) {
    errStr.appendMessage(true, "Insufficient memory for shadow array.");
    throw Error(errStr);
  };
  
  // Clear the array. Note that this has the side effect of
  // clearing board flags.
  
  for(i=0; i < nshadow_; i++)
    shadow_[i] = 0;
}

/**.......................................................................
 * Destructor for a register-database object.
 */
SzaShare::SzaRegDb::~SzaRegDb()
{
  bool waserr=false;
  
  waserr = (pthread_mutex_destroy(&guard_) != 0);
  
  // This is a C binding
  
  if(regmap_ != 0)
    regmap_ = del_SzaAntRegMap(regmap_);
  
  // This was malloc'd
  
  if(shadow_ != 0)
    free(shadow_);
  
  if(waserr)
    throw Error("SzaShare::SzaRegDb::~SzaRegDb: "
		"Error in pthread_mutex_destroy");
}

/**.......................................................................
 * Acquire exclusive access to the database.
 *
 * Input:
 *  timeout      int    How long to wait for the database to become
 *                      available. This is one of the following special values:
 *
 *                       NO_WAIT       - Return immediately if another
 *                                       task is using the database.
 *                       WAIT_FOREVER  - Don't timeout.
 */
void SzaShare::SzaRegDb::grabRegs(TimeOut timeout)
{
  int status;
  
  // If requested to wait forever, block until the mutex becomes
  // available
  
  if(timeout==WAIT_FOREVER)
    status = pthread_mutex_lock(&guard_);

  // Else try the lock and return immediately if not available

  else  
    status = pthread_mutex_trylock(&guard_);
  
  if(status != 0)
    throw Error("SzaShare::SzaRegDb::grabRegs: Error locking mutex\n");
}

/**.......................................................................
 * Relinquish exclusive access to the database.
 */
void SzaShare::SzaRegDb::ungrabRegs()
{
  if(pthread_mutex_unlock(&guard_) != 0)
    throw Error("SzaShare::SzaRegDb::ungrabRegs: Error unlocking mutex\n");
}

/**.......................................................................
 * Look up a board in the register map
 */
RegMapBoard* SzaShare::SzaRegDb::findRegMapBoard(string boardName) 
{
  return find_RegMapBoard(regmap_, (char* )boardName.c_str());
}

/*.......................................................................
 * Flag a given register board as unreachable. This sets the status
 * register of the specified board to 1. Note that readReg() and
 * writeReg() don't check the status of this register before accessing
 * other registers, so if you want to avoid I/O errors, call
 * verifyBoard() before calling readReg() or writeReg().
 *
 * Input:
 *  board         int    The index of the board to flag.
 * Output:
 *  return        int    OK    - Board now flagged.
 *                       ERROR - Error.
 */
void SzaShare::SzaRegDb::flagBoard(int board)
{
  unsigned *flag;       // The board's flag register 
  bool already_flagged; // True if the board was already flagged 
  bool wasError=false;
  LogStream ls;

  if(board < 0 || board >= regmap_->nboard_) {
    ls.initMessage(true);
    ls << "Board index: " 
       << board 
       << " out of range.";
      throw Error(ls);
  };
  
  // Acquire exclusive access to the database.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  grabRegs(WAIT_FOREVER);
  
  // Get a pointer to the board's flag register.
  
  flag = boardStatusReg(board);
  
  // Is the board already flagged?
  
  if(*flag) {
    already_flagged = true;
  } else {
    already_flagged = false;
    *flag = 1;
  };
  
  // Relinquish exclusive access to the database.
  
  ungrabRegs();
  
  UNINSTALL_MUTEX_CLEANUP(ls);

  // If an error occurred, report it now.
  
  if(ls.isError())
    throw Error(ls);
  
  // Report the demise of the board if it wasn't already flagged.
  
  if(!already_flagged) {
    ls.initMessage(true);
    ls << "Board '"
       << regmap_->boards_[board]->name
       << "' has now been marked as unreachable.";
    ErrorDef(err, ls);
  }
}

/**.......................................................................
 * Mark a given register board as reachable. This sets the status
 * register of the specified board to 0.
 *
 * Input:
 *  board         int    The index of the board to unflag.
 */
void SzaShare::SzaRegDb::unflagBoard(int board)
{
  RegMap *regmap;  // The SZA register map 
  int oldtype;
  bool wasError=false;
  LogStream ls;

  // Check arguments.
  
  if(board < 0 || board >= regmap->nboard_) {
    ls.initMessage(true);
    ls << "SzaShare::flagBoard: Board index " << board 
       << " out of range.";
    throw Error(ls);
  };
  
  // Acquire exclusive access to the database.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  grabRegs(WAIT_FOREVER);
  
  // The first register of each board is a scalar "status" register.
  // This has the value 0 when the board is ok, or non-zero when
  // broken.
  
  *boardStatusReg(board) = 0;
  
  // Relinquish exclusive access to the database.
  
  ungrabRegs();
  
  UNINSTALL_MUTEX_CLEANUP(ls);
  
  // If an error occurred, report it now.
  
  if(ls.isError()) 
    throw Error(ls);
  
  // Report the recovery of the board.
  
  ls.initMessage(true);
  ls << "Board '"
     << regmap->boards_[board]->name
     << "' has now been marked as reachable.";
  ErrorDef(err, ls);
}

/**.......................................................................
 * Return the value of the status register of a given register board.
 * This will be zero if the board is reachable, or non-zero otherwise.
 *
 * Input:
 *  board         int    The index of the board to verify.
 * Output:
 *  return        int    1 - The board is marked as ok.
 *                       0 - The board is marked as unreachable, or
 *                           an error occured in the function.
 */
bool SzaShare::SzaRegDb::verifyBoard(int board)
{
  unsigned status; // The value of the board status register 
  int oldtype;
  bool wasError=true;
  LogStream ls;

  // Check arguments.
  
  if(board < 0 || board >= regmap_->nboard_) {
    ls.initMessage(true);
    ls << "SzaShare::verifyBoard: Board index " << board 
       << " out of range.";
    throw Error(ls);
  };
  
  // Acquire exclusive access to the database.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);
  
  grabRegs(WAIT_FOREVER);
  
  // The first register of each board is a scalar "status" register.
  // This has the value 0 when the board is ok, or non-zero when
  // broken.
  
  status = *boardStatusReg(board);
  
  // Relinquish exclusive access to the database.
  
  ungrabRegs();
  
  UNINSTALL_MUTEX_CLEANUP(ls);

  if(ls.isError())
    throw Error(ls);
  
  return status==0;
}

/**.......................................................................
 * This is a private function that returns a pointer to the shadow status
 * register of a given register board. The caller should lock the database
 * and have checked the validity of share and board before calling this
 * function. 
 *
 * Input:
 *  board             int    The index of the board.
 * Output:
 *  return       unsigned *  The pointer alias of the shadow status register.
 */
unsigned* SzaShare::SzaRegDb::boardStatusReg(int board)
{
  return shadow_ + regmap_->boards_[board]->blocks[0]->ireg_;
}

//-----------------------------------------------------------------------
// SzaShare::SzaClock Class methods
//

/**.......................................................................
 * Allocate an object to hold UTC clock information.
 */
SzaShare::SzaClock::SzaClock()
{
  // Allocate a mutual exclusion guard for the object.
  
  pthread_mutex_init(&guard_, NULL);
}

/**.......................................................................
 * Delete a SzaClock object.
 */
SzaShare::SzaClock::~SzaClock()
{
  LogStream ls;

  if(pthread_mutex_destroy(&guard_) != 0) {
    ls.appendSysError(true, "pthread_mutex_destroy()");
    throw Error(ls);
  }
}

/**.......................................................................
 * Adjust the SZA UTC clock.
 *
 * Input:
 *
 *  mjd            long    The MJD UTC day count.
 *  sec            long    The number of seconds into the above day
 */
void SzaShare::SzaClock::setClock(unsigned long mjd, unsigned long sec, 
				  unsigned long nanoSeconds)
{
  LogStream ls;
  
  // Acquire exclusive access to the clock.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  if(pthread_mutex_lock(&guard_) != 0)
    ls.appendSysError(true, "pthread_mutex_lock");

  // Record the new time relationship.
  
  time_.setMjd(mjd, sec, nanoSeconds);
  
  // Relinquish exclusive access to the astrometry object.
  
  if(pthread_mutex_unlock(&guard_) != 0) 
    ls.appendSysError(true, "pthread_mutex_unlock");

  UNINSTALL_MUTEX_CLEANUP(ls);

  // If an error occurred, report it now.

  if(ls.isError())
    throw Error(ls);
}

/**.......................................................................
 * Adjust the SZA UTC clock.
 *
 * Input:
 *
 *  mjd            long    The MJD UTC day count.
 *  sec            long    The number of seconds into the above day
 */
void SzaShare::SzaClock::setClock(TimeVal& time)
{
  LogStream ls;
  
  // Acquire exclusive access to the clock.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  if(pthread_mutex_lock(&guard_) != 0)
    ls.appendSysError(true, "pthread_mutex_lock");

  // Record the new time relationship.
  
  time_ = time;
  
  // Relinquish exclusive access to the astrometry object.
  
  if(pthread_mutex_unlock(&guard_) != 0) 
    ls.appendSysError(true, "pthread_mutex_unlock");

  UNINSTALL_MUTEX_CLEANUP(ls);

  // If an error occurred, report it now.

  if(ls.isError())
    throw Error(ls);
}

/**.......................................................................
 * Set the SZA UTC to the current time.
 */
void SzaShare::SzaClock::setClock(void)
{
  LogStream ls;
  
  // Acquire exclusive access to the clock.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  if(pthread_mutex_lock(&guard_) != 0)
    ls.appendSysError(true, "pthread_mutex_lock");

  // Record the new time relationship.
  
  time_.setToCurrentTime();
  
  // Relinquish exclusive access to the astrometry object.
  
  if(pthread_mutex_unlock(&guard_) != 0) 
    ls.appendSysError(true, "pthread_mutex_unlock");

  UNINSTALL_MUTEX_CLEANUP(ls);

  // If an error occurred, report it now.

  if(ls.isError())
    throw Error(ls);
}

/**.......................................................................
 * Return the current UTC as a Modified Julian Date.
 *
 * Output:
 *  return      double     The UTC as a Modified Julian Date 
 */
double SzaShare::SzaClock::getUtc()
{
  LogStream ls;
  double mjd;               // The time to be returned 
  
  // Acquire exclusive access to the clock.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  if(pthread_mutex_lock(&guard_) != 0) {
    ls.appendSysError(true, "pthread_mutex_lock");
    throw Error(ls);
  }
  
  // Compute the current MJD utc (including fractions of days).
  
  mjd = time_.getTimeInMjdDays();

  // Relinquish exclusive access to the clock
  
  if(pthread_mutex_unlock(&guard_) != 0) {
    ls.appendSysError(true, "pthread_mutex_unlock");
    throw Error(ls);
  }

  UNINSTALL_MUTEX_CLEANUP(ls);

  return mjd;
}

//-----------------------------------------------------------------------
// SzaPmacLock methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Allocate the object that maintains the integrity of the pmac.host_read
 * register.
 *
 * Input:
 *  share   SzaShare *   The SZA shared resource container.
 */
SzaShare::SzaPmacLock::SzaPmacLock(SzaShare* share)
{
  LogStream ls;
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_SzaPmacLock().
  
  count_     = 0;
  pmac_      = 0;
  host_read_  = 0;
  
  // Allocate a mutex to regulate access to this object.
  
  pthread_mutex_init(&guard_, NULL);
  
  // Lookup the register-map entry of the pmac board.
  
  pmac_ = share->findRegMapBoard("pmac");
  if(pmac_==0) {
    ls.appendMessage(true, "Lookup of pmac board failed");
    throw Error(ls);
  };
  
  // Look up the host_read register.
  
  host_read_ = find_RegMapBoard_Block(pmac_, "host_read");
  if(host_read_ == 0) {
    ls.appendMessage(true, "Lookup of pmac host_read register failed");
    throw Error(ls);
  }
}

/**.......................................................................
 * Destructor for a SzaPmacLock object
 */
SzaShare::SzaPmacLock::~SzaPmacLock()
{
  LogStream ls;
  if(pthread_mutex_destroy(&guard_) != 0) {
    ls.appendSysError(true, "pthread_mutex_destroy");
    throw Error(ls);
  }
}

//-----------------------------------------------------------------------
// SzaShare Class methods
//-----------------------------------------------------------------------

// Initialize our static pointer to NULL

SzaShare* SzaShare::share = 0;

/**.......................................................................
 * List the IP addresses of trusted hosts by name.  Note that VxWorks
 * doesn't support gethostbyname() so this is the only way to provide
 * name-service support.
 */
SzaShare::HostAddress SzaShare::host_address[] = 
  {
    {"motu",                    "127.0.0.1"},
    {"polestar.uchicago.edu",   "127.0.0.1"},
    {"szacntrl.uchicago.edu",   "127.0.0.1"},
    {"szanet.ovro.caltech.edu", "127.0.0.1"},
    {"szacntrl2.uchicago.edu",  "127.0.0.1"},
    {"cntrl",                   "127.0.0.1"},
    {"cntrl.localnet",          "127.0.0.1"},
    {"ant1",                    "127.0.0.1"},
    {"localhost",               "127.0.0.1"},
    {"ant1.uchicago.edu",       "127.0.0.1"},
    {"cobralab1",               "127.0.0.1"},
    {"cobralab1.uchicago.edu",  "127.0.0.1"},
  };

/**.......................................................................
 * Validate a specified IP address or host-name alias, and return a
 * malloc'd copy of the resulting IP address.
 *
 * Input:
 *  host    char *   The IP address or host-name alias of the host.
 * Output:
 *  return  char *   The IP address of the host, or NULL on error.
 *                   This should be free'd when no longer required.
 */
string SzaShare::hostIpAddress(string host)
{
  string address;  // The IP address string to be returned 
  int i;
  struct in_addr inp;
  char* hostname=0;
  LogStream ls;
  
  // Look up the host name in the alias table.
  
  for(i=0; i < sizeof(host_address) / sizeof(host_address[0]); i++) {
    if(strcmp(host_address[i].name, host.c_str()) == 0) {
      hostname = host_address[i].address;
      break;
    };
  };
  
#ifdef HOST_CHECK

  // Check that we found a match
  
  if(hostname == 0) {
    ls.initMessage(true);
    ls << "\"" << host << "\" is not a known host-name.";
    throw Error(ls);
  };
 
  // Ensure that specified host has valid syntax for an IP address.
  
  if(inet_aton(hostname, &inp) == 0) {
    ls.initMessage(true);
    ls << "\"" << host << "\" is neither an IP address nor a known host-name.";
    throw Error(ls);
  };

  // Attempt to allocate a copy of the IP address string.
  
  address = hostname;

#endif
  
  return address;
}

/**.......................................................................
 * Create an object to encapsulate resources that are shared
 * between SZA tasks.
 *
 * Input:
 *  host          char *  The IP address of the control host.
 */
SzaShare::SzaShare(string host) :
  sza::util::AntennaDataFrameManager(false)
{
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which ~SzaShare() can
  // safely be called
  
  regdb_        = 0;
  clock_        = 0;
  astrom_       = 0;
  pmac_lock_    = 0;
  tmpbuf_       = 0;
  
  // Keep a static pointer to ourselves; we will need this for access
  // to the AntennaMaster resources in signal handlers
  
  share = this;
  
  // Get the host address.
  
  controlHost_ = hostIpAddress(host);
  
  // Allocate the clock object.
  
  clock_ = new SzaClock();
  if(!clock_)
    throw Error("SzaShare::SzaShare: Unable to allocate clock");
  
  // Allocate the astrometry object.
  
  astrom_ = new SzaAstrom();
  if(!astrom_)
    throw Error("SzaShare::SzaShare: Unable to allocate astrom");
  
  // Create the register database.
  
  regdb_ = new SzaRegDb();
  if(!regdb_)
    throw Error("SzaShare::SzaShare: Unable to allocate regdb");
  
  // Create the pmac read lock.
  
  pmac_lock_ = new SzaPmacLock(this);
  if(!pmac_lock_)
    throw Error("SzaShare::SzaShare: Unable to allocate pmac_lock");
  
  unsigned int frameSize = SCAN_BUFF_SIZE(getNarchived())/sizeof(unsigned long);
  
  if((tmpbuf_ = (unsigned* )malloc(frameSize * sizeof(unsigned)))==0)
    throw Error("SzaShare::SzaShare: Unable to allocate tmpbuf_");
}

/**.......................................................................
 * Delete a SZA shared resource object.
 */
SzaShare::~SzaShare()
{
  if(pmac_lock_ != 0) {
    delete pmac_lock_;
    pmac_lock_ = 0;
  }
  
  if(regdb_ != 0) {  
    delete regdb_;
    regdb_ = 0;
  }
  
  if(clock_ != 0) {
    delete clock_;
    clock_ = 0;
  }
  
  if(astrom_ != 0) {
    delete astrom_;
    astrom_ = 0;
  }
  
  if(tmpbuf_ != 0) {
    free(tmpbuf_);
    tmpbuf_ = 0;
  }
}

/**.......................................................................
 * Acquire exclusive access to the database.
 *
 * Input:
 *  timeout      int    How long to wait for the database to become
 *                      available. This is one of the following special values:
 *
 *                       NO_WAIT       - Return immediately if another
 *                                       task is using the database.
 *                       WAIT_FOREVER  - Don't timeout.
 */
void SzaShare::grabRegs(TimeOut timeout)
{
  regdb_->grabRegs(timeout);
}

/**.......................................................................
 * Relinquish exclusive access to the database.
 */
void SzaShare::ungrabRegs()
{
  regdb_->ungrabRegs();
}

/**.......................................................................
 * A public wrapper around SzaRegDb::flagBoard()
 */
void SzaShare::flagBoard(int board)
{
  regdb_->flagBoard(board);
}

/**.......................................................................
 * A wrapper around SzaRegDb::unflagBoard()
 */
void SzaShare::unflagBoard(int board)
{
  regdb_->unflagBoard(board);
}

/**.......................................................................
 * A wrapper around SzaRegDb::verifyBoard()
 */
bool SzaShare::verifyBoard(int board)
{
  return regdb_->verifyBoard(board);
}


//-----------------------------------------------------------------------
// SzaShare::SzaAstrom methods
//------------------------------------------------------------

/**.......................................................................
 * Constructor for astrometry related information.
 */
SzaShare::SzaAstrom::SzaAstrom()
{
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // deleted in ~SzaAstrom().
  
  Angle lon, lat;

  site_.setFiducial(lon, lat, 0.0);
  ut1utc_ = NULL;
  eqneqx_ = NULL;
  
  // Allocate a mutual exclusion guard_ for the object.
  
  pthread_mutex_init(&guard_, NULL);
  
  // Allocate quadratic interpolation tables for ut1-utc and the
  // equation of the equinoxes.
  
  ut1utc_ = new QuadPath(0.0, QuadPath::QP_NORMAL);
  eqneqx_ = new QuadPath(0.0, QuadPath::QP_NORMAL);
}

/**.......................................................................
 * Destructor for a SzaAstrom object.
 */
SzaShare::SzaAstrom::~SzaAstrom()
{
  LogStream ls;

  if(pthread_mutex_destroy(&guard_) != 0)
    ls.appendSysError(true, "pthread_mutex_destroy");
  
  delete ut1utc_;
  delete eqneqx_;
  
  if(ls.isError())
    throw Error(ls);
}

/**.......................................................................
 * Change the SZA site-specification parameters.
 *
 * Input:
 *  longitude  double    The longitude of the site in radians (-pi..pi).
 *                       East is positive, West is negative.
 *  latitude   double    The latitude of the site in radians (-pi/2..pi/2).
 *  altitude   double    The height of the site in meters above sealevel.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error (the original site object remains
 *                                  unchanged).
 */
void SzaShare::SzaAstrom::setSite(double longitude, double latitude,
				  double altitude)
{
  bool waserr = false;  // True if an error occurred 
  LogStream ls;
  Angle lon, lat;

  lon.setRadians(longitude);
  lat.setRadians(latitude);
  
  // Acquire exclusive access to the astrometry object.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  if(pthread_mutex_lock(&guard_) != 0) {
    ls.appendSysError(true, "pthread_mutex_lock");
    throw Error(ls);
  }
  
  // Record the new site location.  Don't throw an exception here if
  // an error occurred, as this may leave guard locked.
  
  try {
    site_.setFiducial(lon, lat, altitude);
  } catch (Exception& err) {
    ls.appendMessage(true, err.what());
  }
  
  // Relinquish exclusive access to the astrometry object.
  
  if(pthread_mutex_unlock(&guard_) != 0) 
    ls.appendSysError(true, "pthread_mutex_unlock");
  
  UNINSTALL_MUTEX_CLEANUP(ls);

  // If an error occurred, throw an exception now
  
  if(ls.isError())
    throw Error(ls);
}

/**.......................................................................
 * Get a copy of the SZA site-specification object.
 *
 * Input/Output:
 *  site         Site *  On output *site will be contain a copy of the
 *                       site specification (see astrom.h).
 */
void SzaShare::SzaAstrom::getSite(sza::antenna::control::Site *site)
{
  LogStream ls;  

  // Acquire exclusive access to the astrometry object.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  if(pthread_mutex_lock(&guard_) != 0) {
    ls.appendSysError(true, "pthread_mutex_lock");
    throw Error(ls);
  }

  // Record the site for return.
  
  *site = site_;
  
  // Relinquish exclusive access to the astrometry object.
  
  if(pthread_mutex_unlock(&guard_) != 0) 
    ls.appendSysError(true, "pthread_mutex_unlock");
  
  UNINSTALL_MUTEX_CLEANUP(ls);

  if(ls.isError())
    throw Error(ls);
}

/*.......................................................................
 * Extend the quadratic interpolation table of ut1 - utc.
 *
 * Input:
 *  double        utc    The time to which the ut1-utc value pertains.
 *  double     ut1utc    The new value of ut1 - utc (seconds).
 */
void SzaShare::SzaAstrom::extendUt1Utc(double utc, double ut1utc)
{
  LogStream ls;
  
  // Acquire exclusive access to the astrometry object.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  if(pthread_mutex_lock(&guard_) != 0) {
    ls.appendSysError(true, "pthread_mutex_lock");
    throw Error(ls);
  }
  
  // Append the new ut1 - utc value to the interpolation table.  Catch
  // any exception thrown here, since we don't want to leave the guard
  // mutex locked if an exception was thrown.
  
  try {
    ut1utc_->extend(utc, ut1utc);
  } catch(Exception &err) {
    ls.appendMessage(true, err.what());
  }
  
  // Relinquish exclusive access to the astrometry object.
  
  if(pthread_mutex_unlock(&guard_) != 0) 
    ls.appendSysError(true, "pthread_mutex_unlock");
  
  // Remove the cleanup handler.

  UNINSTALL_MUTEX_CLEANUP(ls);

  // If an error occurred, report it now.

  if(ls.isError())
    throw Error(ls);
}

/*.......................................................................
 * Extend the quadratic interpolation table of the equation of the
 * equinoxes.
 *
 * Input:
 *  double        tt     The Terrestrial Time to which 'eqneqx'
 *                       pertains, expressed as a Modified Julian Date.
 *  double     eqneqx    The equation of the equinoxes (radians).
 */
void SzaShare::SzaAstrom::extendEqnEqx(double tt, double eqneqx)
{
  LogStream ls;
  
  // Acquire exclusive access to the astrometry object.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  if(pthread_mutex_lock(&guard_) != 0) {
    ls.appendSysError(true, "pthread_mutex_lock");
    throw Error(ls);
  }
  
  // Append the new ut1 - utc value to the interpolation table.  Catch
  // any exception thrown here, since we don't want to leave the guard
  // mutex locked if an exception was thrown.
  
  try {
    eqneqx_->extend(tt, eqneqx);
  } catch(Exception &err) {
    ls.appendMessage(true, err.what());
  }
  
  // Relinquish exclusive access to the astrometry object.
  
  if(pthread_mutex_unlock(&guard_) != 0) 
    ls.appendSysError(true, "pthread_mutex_unlock");
  
  // Remove the cleanup handler.

  UNINSTALL_MUTEX_CLEANUP(ls);

  // If an error occurred, report it now.

  if(ls.isError())
    throw Error(ls);
}

/*.......................................................................
 * Get the value of ut1 - utc at a given UTC.
 *
 * Input:
 *  utc        double    The utc at which to sample ut1-utc, expressed
 *                       as a Modified Julian Date.
 * Output:
 *  value      double *  The value of ut1 - utc for time 'utc' (seconds).
 */
double SzaShare::SzaAstrom::getUt1Utc(double utc)
{
  double value;
  LogStream ls;
  
  // Acquire exclusive access to the astrometry object.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  if(pthread_mutex_lock(&guard_) != 0) {
    ls.appendSysError(true, "pthread_mutex_lock");
    throw Error(ls);
  }
  
  // Interpolate the value of ut1utc for the current value of utc.
  // Catch any exception thrown here, since we don't want to leave the
  // guard mutex locked if an exception was thrown.
  
  try {
    value = ut1utc_->eval(utc);
  } catch(Exception &err) {
    ls.appendMessage(true, err.what());
  }

  // Relinquish exclusive access to the astrometry object.
  
  if(pthread_mutex_unlock(&guard_) != 0) 
    ls.appendSysError(true, "pthread_mutex_unlock");
  
  // Remove the cleanup handler.

  UNINSTALL_MUTEX_CLEANUP(ls);

  // If an error occurred, report it now.

  if(ls.isError())
    throw Error(ls);
  
  return value;
}

/*.......................................................................
 * Retrieve the cached value of the equation of the equinoxes.
 *
 * Input:
 *  tt         double    The Terrestrial Time at which to sample the
 *                       equation of the equinoxes, expressed as a
 *                       Modified Julian Date.
 * Output:
 *  value     double *   The equation of the equinoxes at time tt (radians).
 */
double SzaShare::SzaAstrom::getEqnEqx(double tt)
{
  double value;
  LogStream ls;
  
  // Acquire exclusive access to the astrometry object.
  
  INSTALL_MUTEX_CLEANUP(guard_, ls);

  if(pthread_mutex_lock(&guard_) != 0) {
    ls.appendSysError(true, "pthread_mutex_lock");
    throw Error(ls);
  }
  
  // Interpolate the value of ut1utc for the current value of utc.
  // Catch any exception thrown here, since we don't want to leave the
  // guard mutex locked if an exception was thrown.
  
  try {
    value = eqneqx_->eval(tt);
  } catch(Exception &err) {
    ls.appendMessage(true, err.what());
  }

  // Relinquish exclusive access to the astrometry object.
  
  if(pthread_mutex_unlock(&guard_) != 0) 
    ls.appendSysError(true, "pthread_mutex_unlock");
  
  // Remove the cleanup handler.

  UNINSTALL_MUTEX_CLEANUP(ls);

  // If an error occurred, report it now.

  if(ls.isError())
    throw Error(ls);
  
  return value;
}

/**.......................................................................
 * Return the local sidereal time that corresponds to a given UTC.
 * Note that the current UTC can be acquired by calling getUtc().
 *
 * Input:
 *  utc         double     The UTC expressed as a Modified Julian Date.
 *
 * Output:
 *  return      double     The Local Sidereal Time (radians)
 */
double SzaShare::getLst(double utc)
{
  sza::antenna::control::Site site;       // The SZA location 
  double ut1utc;   // The value of ut1 - utc 
  double eqneqx;   // The equation of the equinoxes 
  double tt;       // The Terrestrial Time equivalent of utc 
  LogStream ls;

  // Check arguments.
  
  if(utc < 0.0) {
    ls.appendMessage(true, "Illegal UTC.");
    throw Error(ls);
  }
  
  // Get the Terrestrial Time that corresponds to 'utc'.
  
  tt = sza::array::mjd_utc_to_mjd_tt(utc);
  if(tt < 0.0) {
    ls.appendMessage(true, "Received UTC < 0.");
    throw Error(ls);
  }
  
  // Lookup the location of the SZA and the current value of ut1-utc.
  
  astrom_->getSite(&site);
  ut1utc = astrom_->getUt1Utc(utc);
  eqneqx = astrom_->getEqnEqx(tt);
  
  // Convert to LST.
  
  return site.convertMjdUtcToLst(utc, ut1utc, eqneqx);
}

/**.......................................................................
 * Return the Terestrial time (aka Ephemeris Time), corresponding to a
 * given UTC (expressed as a Modified Julian date).
 */
double SzaShare::getTt(double utc)
{
  return sza::array::mjd_utc_to_mjd_tt(utc);
}

/**.......................................................................
 * A public wrapper around SzaShare's private regmap
 */
RegMapBoard* SzaShare::findRegMapBoard(string boardName) 
{
  return regdb_->findRegMapBoard(boardName);
}

/**.......................................................................
 * Get the current UTC as a Modified Julian Date.
 */
double SzaShare::getUtc()
{
  return clock_->getUtc();
}

/**.......................................................................
 * Get the value of the equation of the equinoxes for a
 * given terrestrial time.
 */
double SzaShare::getEqnEqx(double tt)
{
  return astrom_->getEqnEqx(tt);
}

/**.......................................................................
 * Get the value of UT1-UTC for a given UTC.
 *
 */
double SzaShare::getUt1Utc(double utc)
{
  return astrom_->getUt1Utc(utc);
}

/**.......................................................................
 * Set the current time.
 */
void SzaShare::setClock(unsigned long mjd, unsigned long sec, 
			unsigned long nanoSeconds)
{
  clock_->setClock(mjd, sec, nanoSeconds);
}

/**.......................................................................
 * Set the current time.
 */
void SzaShare::setClock(TimeVal& time)
{
  clock_->setClock(time);
}

/**.......................................................................
 * Set the current time.
 */
void SzaShare::setClock()
{
  clock_->setClock();
}

/**.......................................................................
 * Record new site-location details in share->site.
 */
void SzaShare::setSite(double longitude, double latitude, double altitude)
{
  astrom_->setSite(longitude, latitude, altitude);
}

/**.......................................................................
 * Extend the quadratic interpolation table of ut1 - utc versus
 * MJD UTC.
 */
void SzaShare::extendUt1Utc(double utc, double ut1utc)
{
  astrom_->extendUt1Utc(utc, ut1utc);
}

/**.......................................................................
 * Extend the quadratic interpolation table of the equation of the
 * equinoxes versus Terrestrial Time (as a Modified Julian Date).
 */
void SzaShare::extendEqnEqx(double tt, double eqneqx)
{
  astrom_->extendEqnEqx(tt, eqneqx);
}

/**.......................................................................
 * Return the number of boards in the register map
 */
unsigned int SzaShare::getNboard()
{
  return regdb_->regmap_->nboard_;
}

/**.......................................................................
 * Return the number of archived registers in the register map
 */
unsigned int SzaShare::getNarchived()
{
  return regdb_->regmap_->narchive_;
}

/**.......................................................................
 * Return the total number of registers in the register map
 */
unsigned int SzaShare::getNreg()
{
  return regdb_->regmap_->nreg_;
}

/**.......................................................................
 * Return the number of archived registers in the register map
 */
unsigned int SzaShare::getNbyte()
{
  return regdb_->regmap_->nByte_;
}

#if 0
/**.......................................................................
 * Function to pack a frame into a DataFrame object.
 *
 * We do not call grabRegs() explicitly in this function, since it is
 * called in verifyBoard() and packRegBoard()
 */
void SzaShare::packFrame(DataFrameManager* frame)
{
  bool wasError = false;
  LogStream ls;
  
  // Now attempt to read each board in turn into the frame buffer
  // Catch any errors here so that we don't leave the pmac readout
  // frozen
  
  DBPRINT(false, Debug::DEBUG7, "Inside SzaShare:: packFrame.  Frame is: "
	  << ((frame==0) ? "NULL" : "not NULL"));

  try {
    
    for(unsigned int iboard=0; iboard < regmap_->nboard_; iboard++) {
      RegMapBoard* brd = regmap_->boards_[iboard];
      
      // If the board is marked as reachable, attempt to copy the
      // contents of the board's registers into the frame buffer.
      
      if(brd->nByteArchive_ > 0) {
	if(verifyBoard(iboard)) {
	  
	  // If an error occurred accessing the board...
	  
	  try {
	    packRegBoard(brd, frame);
	  } catch(...) {
	    
	    // If the board is unreachable, zero the part of the frame
	    // buffer allocated to the board, then set the board's
	    // status register to record the problem.
	    
	    frame->fillBuffer(0, brd->nByteArchive_);
	    
	    // Mark the board as bad.
	    
	    flagBoard(iboard);
	    
	    // Record the error status in the frame-buffer copy of the
	    // status register.
	    
	    frame->setErrorStatus(true);
	  };
	};
	
	// Don't need to advance the frame pointer -- manager takes
	// care of this
	
      };
    };
  } catch (Exception& err) {
    ls.appendMessage(true, err.what());
  }

  // If any errors were caught, propagate them to the outside world
  
  if(ls.isError()) 
    throw Error(ls);
}
#endif

/**.......................................................................
 * Function to pack a frame into a DataFrame object.
 *
 * We do not call grabRegs() explicitly in this function, since it is
 * called in verifyBoard() and packRegBoard()
 */
void SzaShare::packFrame(DataFrameManager* frame)
{
  lock();

  // Use the assignment operator to copy ourselves in

  *frame = *this;

  unlock();
}

RegMap* SzaShare::getRegMap()
{
  return regMap_;
}
