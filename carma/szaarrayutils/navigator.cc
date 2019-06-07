#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <errno.h>
#include <time.h>
#include <math.h>

#include "lprintf.h"
#include "szacontrol.h"
#include "szaconst.h"
#include "astrom.h"
#include "cache.h"
#include "scan.h"
#include "source.h"
#include "navigator.h"

#include "carma/szautil/Axis.h"
#include "carma/szautil/CenterManager.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/FdSet.h"
#include "carma/szautil/TimeVal.h"

using namespace sza::array;
using namespace sza::util;

/*
 * The source that is currently being observed is maintained in
 * the source catalog under the following name.
 */
#define CURRENT_SOURCE "current"

/*
 * The scan that is currently being observed is maintained in
 * the scan catalog under the following name.
 */
#define CURRENT_SCAN "current"

/*
 * In order to keep the real-time tasks of the control system up to
 * date with various ephemerides, the navigator thread sends updates
 * of them to the real-time controller whenever they become out of date.
 * The following members record the times of pending updates.
 */
typedef struct {
  CacheWindow ut1utc;   /* The valid time range of the current
			   UT1-UTC */
                        /*  interpolation parameters. */
  CacheWindow eqneqx;   /* The valid time range of the current
			   Equation */
                        /*  of the Equinoxes interpolation
			    parameters. */
  CacheWindow phase;    /* The valid time range of the current phase
			   or 0,0 if no updates are needed */
  CacheWindow pointing; /* The valid time range of the current pointing
			   or 0,0 if no updates are needed */
  CacheWindow scan;     /* The valid time range of the current scan,
			   or 0,0 if no updates are needed */
  
} TimeWindows;

/*
 * The following type contains the cached values of ephemerides that were
 * last sent to the real-time controller.
 */
typedef struct {
  double ut1utc[3];       /* UT1-UTC quadratic interpolation times */
  double eqneqx[3];       /* Equation-of-the-equinoxes quadratic */
                          /*  interpolation times */
  double ephem[3];        /* Ephemeris source quadratic interpolation */
                          /*  times */
} LastCache;

// A Navigator object is used to maintain the state of the navigator
// thread.

struct Navigator {
  ControlProg *cp;          /* The state object of the control program */
  Pipe *pipe;               /* An externally allocated control-input pipe */
  int rtc_online;           /* True if the controller is connected */
  long wakeup_interval;     /* The minimum wakeup interval */
  int shutdown_pending;     /* True after receipt of a shutdown message */
  SourceCatalog *sc;        /* The source catalog */
  ScanCatalog *scanc;       /* The scan catalog */
  
  // Most of the time this thread is idle waiting for input from its
  // input pipe.  At such times the following guard mutex is released
  // to enable other threads to access the data that this thread
  // manages. All public accessor functions must acquire this mutex
  // before accessing the source catalog and release it when done.

  pthread_mutex_t guard;    /* The mutual exclusion guard of this object */
  int guard_ok;             /* True after initializing 'guard' */
  TimeWindows window;       /* The time windows for cached quantities */
  LastCache cache;          /* The last set of caches sent to the tracker */
  int current_source;       /* The catalog index of the current source */
  int current_scan;         /* The catalog index of the current scan */
  int lastScanIndex;        /* The index of the last scan offset sent
			       to the controller. */
  Site site;                /* The location of the SZA */
  CenterManager* phaseManager_; // An object which will keep track of
				// the phase tracking for antennas
  CenterManager* pointingManager_; // An object which will keep track of
  // the pointing tracking for antennas
};

static int nav_handle_message(Navigator *nav, NavigatorMessage *msg);
static int nav_check_caches(Navigator *nav);
static int nav_update_ut1utc(Navigator *nav, double utc, int reset);
static int nav_update_eqneqx(Navigator *nav, double tt, int reset);
static int nav_update_source(Navigator *nav, double tt, 
			     sza::util::Tracking::Type type);
static int nav_update_ephem_source(Navigator *nav, double tt, sza::array::Source *src,
				   SourceId *id, sza::util::Tracking::Type type, 
				   unsigned ant, signed seq);
static int nav_update_J2000_source(Navigator *nav, double tt, sza::array::Source *src,
				   SourceId *id, sza::util::Tracking::Type type, 
				   unsigned ant, signed seq);
static int nav_track_fixed_source(Navigator *nav, sza::array::Source *src, SourceId *id,
				  sza::util::Tracking::Type type, 
				  unsigned ant, unsigned seq);
static int nav_track_J2000_source(Navigator *nav, sza::array::Source *src, SourceId *id,
				  sza::util::Tracking::Type type, 
				  unsigned ant, unsigned seq);
static int nav_track_ephem_source(Navigator *nav, sza::array::Source *src, 
				  SourceId *id,
				  sza::util::Tracking::Type type, 
				  unsigned ant, unsigned seq);
// Scan functions

static int nav_start_normal_scan(Navigator *nav, sza::array::Scan *scan, 
				 ScanId *id,
				 unsigned seq);
static int nav_update_scan(Navigator *nav, double tt);
static int nav_update_normal_scan(Navigator *nav, double tt, 
				  sza::array::Scan *scan,
				  ScanId *id, unsigned seq);
static int nav_current_scan(Navigator *nav, sza::array::Scan* scan);

static int nav_set_site(Navigator *nav, NavSiteMsg *site);
static int nav_adopt_rtc_state(Navigator *nav, int online);
static int nav_dispatch_site_to_rtc(Navigator *nav);

/*
 * Record the interval at which to check the need for updating ephemerides
 * etc.  For now, we will update this once a minute.
 */
#define NAV_WAKEUP_INTERVAL 60000    /* (milli-seconds) */

/*.......................................................................
 * Create the context object of a navigator thread.
 *
 * Input:
 *  cp     ControlProg *   The state object of the control program.
 *  pipe          Pipe *   The pipe to receive control input from.
 * Output:
 *  return        void *   The new object, or NULL on error.
 */
CP_NEW_FN(new_Navigator)
{
  Navigator *nav;              /* The object to be returned */
  sza::array::Source* zenith;  /* The source entry for zenith */
  sza::array::Source* nadir;   /* The source entry for nadir */
  sza::array::Source* service; /* The source entry for SERVICE */
  sza::array::Source* stow;    /* The source entry for STOW */
  sza::array::Scan *currentScan; /* The scan entry for the current scan */
  sza::array::Scan *none;     /* The scan entry for none */
  int status;          /* The status return value of a pthread function */
  int i;
  
  // Allocate the container.
  
  nav = (Navigator* )malloc(sizeof(Navigator));
  if(!nav) {
    lprintf(stderr, "new_Navigator: Insufficient memory.\n");
    return NULL;
  };
  
  // Before attempting any operation that might fail, initialize the
  // container at least up to the point at which it can safely be
  // passed to del_Navigator().
  
  nav->cp = cp;
  nav->pipe = pipe;
  nav->rtc_online = 0;
  nav->wakeup_interval = NAV_WAKEUP_INTERVAL;
  nav->shutdown_pending = 0;
  nav->sc = NULL;
  nav->scanc = NULL;
  nav->phaseManager_ = 0;
  nav->pointingManager_ = 0;
  nav->guard_ok = 0;
  init_CacheWindow(&nav->window.ut1utc);
  init_CacheWindow(&nav->window.eqneqx);
  init_CacheWindow(&nav->window.phase);
  init_CacheWindow(&nav->window.pointing);
  init_CacheWindow(&nav->window.scan);
  
  for(i=0; i<3; i++) {
    nav->cache.ut1utc[i] = 0.0;
    nav->cache.eqneqx[i] = 0.0;
    nav->cache.ephem[i] = 0.0;
  };
  nav->current_source = -1;
  nav->current_scan = -1;
  set_Site(&nav->site, 0.0, 0.0, 0.0);
  
  // Allocate an empty source catalog.
  
  nav->sc = new_SourceCatalog();
  if(!nav->sc)
    return del_Navigator(nav);
  
  // Make sure that the source catalog contains entries for the zenith
  // and the nadir. Zenith is needed for parking the telescope, and
  // nadir is exploited in scripts as a source that is guaranteed
  // never to be visible.
  zenith  = add_FixedSource(nav->sc, "zenith",  Axis::EL, 0.0,  halfpi, 0.0);
  stow    = add_AliasSource(nav->sc, "stow", "zenith");

  nadir   = add_FixedSource(nav->sc, "nadir",   Axis::EL, 0.0, -halfpi, 0.0);
  service = add_FixedSource(nav->sc, "service", Axis::AZ|Axis::EL, pi,  0.0, 0.0);

  if(!zenith || !nadir || !service || !stow)
    return del_Navigator(nav);
  
  // Allocate new phase and pointing managers
  
  nav->phaseManager_ = new CenterManager();
  
  // Allocate new phase and pointing managers
  
  nav->pointingManager_ = new CenterManager();
  
  // Initialize the current phase and pointing centers for all
  // antennas to be the zenith.
  
  nav->phaseManager_->changeCenter(AntNum::ANTALL, zenith->id);
  nav->pointingManager_->changeCenter(AntNum::ANTALL, zenith->id);
  
  //-----------------------------------------------------------------------
  // And now the scan catalog
  //-----------------------------------------------------------------------
  
  // Allocate an empty scan catalog.
  
  nav->scanc = new_ScanCatalog();
  
  if(!nav->scanc)
    return del_Navigator(nav);
  
  
  // Make sure that the scan catalog contains an entry for a bogus
  // scan called "none"
  
  none = add_BogusScan(nav->scanc, "none");
  
  if(!none)
    return del_Navigator(nav);
  
  // Initialize the current scan to be none
  
  if(nav_current_scan(nav, none))
    return del_Navigator(nav);
  
  // Record the catalog slot of the "current" sscan
  
  currentScan = find_ScanByName(nav->scanc, CURRENT_SCAN);
  
  if(!currentScan)
    return del_Navigator(nav);
  
  nav->current_scan = get_Scan_number(currentScan, 0);
  
  if(nav->current_scan < 0)
    return del_Navigator(nav);
  
  // Allocate the guard mutex that is used to provide restricted
  // external access to the resources managed by this thread.
  
  status = pthread_mutex_init(&nav->guard, NULL);
  if(status) {
    lprintf(stderr, "pthread_mutex_init: %s\n", strerror(status));
    return del_Navigator(nav);
  };
  
  nav->guard_ok = 1;
  
  return nav;
}

/*.......................................................................
 * Delete the resource-object of a navigator thread.
 *
 * Input:
 *  obj     void *   The Navigator object to be deleted.
 * Output:
 *  return  void *   The deleted Navigator object (always NULL).
 */
CP_DEL_FN(del_Navigator)
{
  Navigator *nav = (Navigator* )obj;
  
  if(nav) {
    
    // Delete the source-catalog guard mutex. Note that del_Navigator
    // isn't called at any time when other client threads could be
    // running, so it is safe to do this.
    
    if(nav->guard_ok) {
      pthread_mutex_destroy(&nav->guard);
      nav->guard_ok = 0;
    };
    
    // Delete the source catalog.
    
    nav->sc = del_SourceCatalog(nav->sc);
    
    // Delete the source catalog.
    
    nav->scanc = del_ScanCatalog(nav->scanc);
    
    // Delete the phase and pointing center managers
    
    if(nav->phaseManager_ != 0)
      delete nav->phaseManager_;
    if(nav->pointingManager_ != 0)
      delete nav->pointingManager_;
    
    free(nav);
  };
    
  return NULL;
}

/*.......................................................................
 * Return the navigator resource object.
 *
 * Input:
 *  cp       ControlProg *   The control program resource object.
 * Output:
 *  return     Navigator *   The navigator resource object.
 */
Navigator *cp_Navigator(ControlProg *cp)
{
  return (Navigator* )cp_ThreadData(cp, CP_NAVIGATOR);
}

/*.......................................................................
 * Attempt to send a message to a navigator thread.
 *
 * Input:
 *  cp        ControlProg *  The state-object of the control program.
 *  msg  NavigatorMessage *  The message to be sent. This must have been
 *                           filled by one of the
 *                           pack_navigator_<type>() functions.
 *  timeou           long    The max number of milliseconds to wait for the
 *                           message to be sent, or -1 to wait indefinitely.
 * Output:
 *  return      PipeState    The status of the transaction:
 *                             PIPE_OK    - The message was read
 *                                          successfully.
 *                             PIPE_BUSY  - The send couldn't be accomplished
 *                                          without blocking (only returned
 *                                          when timeout=PIPE_NOWAIT).
 *                             PIPE_ERROR - An error occurred.
 */
PipeState send_NavigatorMessage(ControlProg *cp, NavigatorMessage *msg,
				long timeout)
{
#ifndef NEW_PIPE
  return write_pipe(cp_Navigator(cp)->pipe, msg, sizeof(*msg), timeout);
#else
  return cp_Navigator(cp)->pipe->write(msg, sizeof(*msg), timeout);
#endif
}

/*.......................................................................
 * Send a shutdown message to the navigator thread using non-blocking I/O.
 * Return 0 if the message was sent, 0 otherwise.
 *
 * Input:
 *  cp      ControlProg *  The control-program resource object.
 * Output:
 *  return          int    0 - Message sent ok.
 *                         1 - Unable to send message.
 */
CP_STOP_FN(stop_Navigator)
{
  NavigatorMessage msg;   /* The message to be sent */
  return pack_navigator_shutdown(&msg) ||
    send_NavigatorMessage(cp, &msg, PIPE_NOWAIT) != PIPE_OK;
}

/*.......................................................................
 * Prepare a shutdown message for subsequent transmission to the
 * navigator thread.
 *
 * Input:
 *  msg   NavigatorMessage *  The message object being prepared for
 *                            subsequent transmission.
 * Output:
 *  return             int     0 - OK.
 *                             1 - Error.
 */
int pack_navigator_shutdown(NavigatorMessage *msg)
{
  /*
   * Check arguments.
   */
  if(!msg) {
    lprintf(stderr, "pack_navigator_shutdown: NULL argument.\n");
    return 1;
  };
  msg->type = NAV_SHUTDOWN;
  return 0;
}

/*.......................................................................
 * Prepare a controller status message for subsequent transmission to the
 * navigator thread.
 *
 * Input:
 *  msg  NavigatorMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  online            int    True if the controller is connected.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
int pack_navigator_rtc_state(NavigatorMessage *msg, int online)
{
  /*
   * Check arguments.
   */
  if(!msg) {
    lprintf(stderr, "pack_navigator_rtc_state: NULL argument(s).\n");
    return 1;
  };
  msg->type = NAV_RTC_STATE;
  msg->body.rtc_online = online;
  return 0;
}

/*.......................................................................
 * Prepare a read-source-catalog message for subsequent transmission to
 * the navigator thread.
 *
 * Input:
 *  msg  NavigatorMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  path             char *  The full pathname of the file (Note
 *                           that pathname.h provides utilities for
 *                           composing/expanding pathnames). A copy
 *                           of this pathname will be made.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
int pack_navigator_catalog(NavigatorMessage *msg, char *path)
{
  /*
   * Check arguments.
   */
  if(!msg || !path) {
    lprintf(stderr, "pack_navigator_catalog: NULL argument(s).\n");
    return 1;
  };
  msg->type = NAV_CATALOG;
  strncpy(msg->body.catalog, path, CP_FILENAME_MAX);
  msg->body.catalog[CP_FILENAME_MAX] = '\0';
  return 0;
}

/*.......................................................................
 * Prepare a read-scan-catalog message for subsequent transmission to
 * the navigator thread.
 *
 * Input:
 *  msg  NavigatorMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  path             char *  The full pathname of the file (Note
 *                           that pathname.h provides utilities for
 *                           composing/expanding pathnames). A copy
 *                           of this pathname will be made.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
int pack_navigator_scan_catalog(NavigatorMessage *msg, char *path)
{
  /*
   * Check arguments.
   */
  if(!msg || !path) {
    lprintf(stderr, "pack_navigator_scan_catalog: NULL argument(s).\n");
    return 1;
  };
  msg->type = NAV_SCAN_CATALOG;
  strncpy(msg->body.scan_catalog, path, CP_FILENAME_MAX);
  msg->body.scan_catalog[CP_FILENAME_MAX] = '\0';
  return 0;
}

/*.......................................................................
 * Prepare a "read UT1-UTC ephemeris" message for subsequent transmission
 * to the navigator thread.
 *
 * Input:
 *  msg  NavigatorMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  path             char *  The full pathname of the file (Note
 *                           that pathname.h provides utilities for
 *                           composing/expanding pathnames). A copy
 *                           of this pathname will be made.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
int pack_navigator_ut1utc(NavigatorMessage *msg, char *path)
{
  
  // Check arguments.
  
  if(!msg || !path) {
    lprintf(stderr, "pack_navigator_ut1utc: NULL argument(s).\n");
    return 1;
  };
  msg->type = NAV_UT1UTC;
  strncpy(msg->body.ut1utc, path, CP_FILENAME_MAX);
  msg->body.ut1utc[CP_FILENAME_MAX] = '\0';
  return 0;
}

/*.......................................................................
 * Prepare a "set SZA location" message for subsequent transmission
 * to the navigator thread.
 *
 * Input:
 *  msg  NavigatorMessage *  The message object being prepared for
 *                           subsequent transmission.
 *  longitude      double    The longitude of the site in radians
 *                           (-pi..pi). East is positive.
 *  latitude       double    The latitude of the site in radians
 *                           (-pi/2..pi/2).
 *  altitude       double    The height of the site in meters above
 *                           sealevel.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
int pack_navigator_site(NavigatorMessage *msg, double longitude,
			double latitude, double altitude)
{
  /*
   * Check arguments.
   */
  if(!msg) {
    lprintf(stderr, "pack_navigator_site: NULL argument(s).\n");
    return 1;
  };
  msg->type = NAV_SITE;
  msg->body.site.longitude = longitude;
  msg->body.site.latitude = latitude;
  msg->body.site.altitude = altitude;
  return 0;
}

/*.......................................................................
 * This is the entry-point of the navigator thread.
 *
 * Input:
 *  arg          void *  A pointer to the Navigator resource object,
 *                       cast to (void *).
 * Output:
 *  return       void *  NULL.
 */
CP_THREAD_FN(navigator_thread)
{
  Navigator *nav = (Navigator* )arg;  /* The resource-object of the
					 current thread */
  NavigatorMessage msg;  /* A message received from another threads */
  PipeState state;       /* The completion status of a pipe read */
  int status;            /* The status return value of a pthread function */
  
  // Enable logging of the navigator's stdout and stderr streams.
  
  if(log_thread_stream(nav->cp, stdout) ||
     log_thread_stream(nav->cp, stderr)) {
    cp_report_exit(nav->cp);
    return NULL;
  };
  
  // Wait for commands from other threads.
  
#ifndef NEW_PIPE
  while(!nav->shutdown_pending &&
	(state=read_pipe(nav->pipe, &msg, sizeof(msg), nav->wakeup_interval)) != 
	PIPE_ERROR) {
#else

  sza::util::FdSet fdSet;
  fdSet.registerReadFd(nav->pipe->readFd());

  // Convert the wakeup interval (ms) to nano-seconds and set the
  // timeout

  unsigned int sec  = nav->wakeup_interval/1000;
  unsigned int nsec = (nav->wakeup_interval - sec*1000) * 1000000;
  sza::util::TimeVal timeOut(sec, nsec);
  int nready=0;
  while((nready=select(fdSet.size(), fdSet.readFdSet(), NULL, NULL, timeOut.timeVal())) >= 0) {

    // Now read the message off the queue

    if(nready > 0) {
      state = nav->pipe->read(&msg, sizeof(msg), PIPE_WAIT);
      if(state == PIPE_ERROR)
	break;

    // If we timed out, reset the timeout counter

    } else {
      timeOut.reset();
      state = PIPE_BUSY;
    } 

#endif
    
    // Acquire the resource-protection mutex before proceding.
    
    if((status=pthread_mutex_lock(&nav->guard))) {
      lprintf(stderr, "Navigator (mutex_lock): %s.\n", strerror(status));
      cp_report_exit(nav->cp);
      return NULL;
    };
    
    // Did we get a new message or is it time to run a new command?
    
    if(state == PIPE_OK) {
      if(nav_handle_message(nav, &msg))
	nav->shutdown_pending = 1;
    } else {
      if(nav_check_caches(nav))
	nav->shutdown_pending = 1;
    };
    
    // Relinquish exclusive access to the thread resources.
    
    pthread_mutex_unlock(&nav->guard);
  };
  
  // If a pipe error occured, report it.
  
  if(!nav->shutdown_pending)
    lprintf(stderr, "Aborting navigator-thread after pipe read-error.\n");
  
  // Report our exit to the control thread.
  
  cp_report_exit(nav->cp);
  return NULL;
}

/*.......................................................................
 * When a function that has acquired the navigator guard mutex suffers
 * an error, it should return via this function, to make sure that the
 * mutex is released under all circumstances.
 *
 * Input:
 *  nav     Navigator *   The resource object of the navigator task.
 * Output:
 *  return        int     Allways 1 (the error return value of most
 *                                   functions).
 */
static int nav_error_while_locked(Navigator *nav)
{
  pthread_mutex_unlock(&nav->guard);
  return 1;
}

/*.......................................................................
 * Interpret a message received from another thread.
 *
 * Input:
 *  nav         Navigator *  The resource object of the navigator thread.
 *  msg  NavigatorMessage *  The message to interpret.
 * Output:
 *  return            int    0 - OK.
 *                           1 - Error.
 */
static int nav_handle_message(Navigator *nav, NavigatorMessage *msg)
{
  /*
   * Interpret the message.
   */
  switch(msg->type) {
  case NAV_SHUTDOWN:
    nav->shutdown_pending = 1;
    lprintf(stdout, "Navigator thread exiting normally.\n");
    break;
  case NAV_RTC_STATE:
    nav_adopt_rtc_state(nav, msg->body.rtc_online);
    break;
  case NAV_CATALOG:
    (void) read_SourceCatalog(nav->sc, "", msg->body.catalog);
    break;
  case NAV_SCAN_CATALOG:
    (void) read_ScanCatalog(nav->scanc, "", msg->body.scan_catalog);
    break;
  case NAV_UT1UTC:
    (void) sc_set_ut1utc_ephemeris(nav->sc, "", msg->body.ut1utc);
    break;
  case NAV_SITE:
    (void) nav_set_site(nav, &msg->body.site);
    break;
  default:
    lprintf(stderr, "navigator_thread: Unknown message-type received.\n");
    break;
  };
  return 0;
}

/*.......................................................................
 * This is a private function of navigator_thread() used to check
 * for the need to send cache updates to the real-time controller.
 *
 * Input:
 *  nav      Navigator *   The resource object of this thread.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Fatal error.
 */
static int nav_check_caches(Navigator *nav)
{
  double utc;   /* The current Universal Coordinated Time */
  double tt;    /* The current Terrestrial Dynamic Time */
  
  // Get the valid-time windows.
  
  TimeWindows *w = &nav->window;
  
  // Ignore this call if the controller is off-line.
  
  if(!nav->rtc_online)
    return 0;
  
  // Get the current time as UTC and TT.
  
  if((utc = current_mjd_utc()) < 0.0 || (tt = mjd_utc_to_mjd_tt(utc)) < 0.0)
    return 1;
  
  // See if the UT1-UTC interpolation parameters need updating.
  
  if(w->ut1utc.tmax < utc && w->ut1utc.tmax > 0.0) {
    if(nav_update_ut1utc(nav, utc, 0))
      return 1;
  };
  
  // See if the Equation of the Equinoxes interpolation parameters
  // need updating.
  
  if(w->eqneqx.tmax < tt && w->eqneqx.tmax > 0.0) {
    if(nav_update_eqneqx(nav, tt, 0))
      return 1;
  };
  
  // See if any phase positional parameters need to be updated.
  
  if(w->phase.tmax < tt && w->phase.tmax > 0.0) {
    COUT("Updating phase");
    if(nav_update_source(nav, tt, Tracking::TRACK_PHASE))
      return 1;
  };
  
  // See if any pointing positional parameters need to be updated.
  
  if(w->pointing.tmax < tt && w->pointing.tmax > 0.0) {
    COUT("Updating pointing");
    if(nav_update_source(nav, tt, Tracking::TRACK_POINT))
      return 1;
  };
  
  return 0;
}

/*.......................................................................
 * Send an update of the current UT1-UTC interpolation parameters to the
 * real-time controller.
 *
 * Input:
 *  nav      Navigator *   The resource object of this thread.
 *  utc         double     The current UTC as a Modified Julian Date.
 *  reset          int     Normally the real-time controller is only
 *                         sent incremental updates, but if this
 *                         argument is set to non-zero, all three points
 *                         of the current quadratic interpolator will be
 *                         sent. This should be done whenever the
 *                         time controller is restarted.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
static int nav_update_ut1utc(Navigator *nav, double utc, int reset)
{
  ControlProg *cp = nav->cp; /* The control-program resource object */
  QuadData data;   /* The UT1-UTC interpolator contents */
  RtcNetCmd rtc;   /* A network object to be sent to the real-time */
                   /*  controller task */
  int n;           /* The number of new interpolation samples to be sent */
  int i;
  
  // Get the current UT1-UTC interpolation parameters.
  
  if(get_ut1utc_QuadData(nav->sc, utc, &data))
    return 1;
  
  // Determine how many samples of the tracker's 3-point interpolation
  // cache need to be updated.
  
  if(reset) {
    n = 3;
  } else {
    double earliest = data.s[0].x;
    for(n=0; n<3 && nav->cache.ut1utc[n] != earliest; n++)
      ;
  };
  
  // Send the n new samples to the tracker.
  
  for(i=3-n; i<3; i++) {
    QuadSample *s = data.s + i;
    
    // Convert the Modified Julian Date of the sample to an integral
    // day number and the number of milli-seconds into that day.
    
    double day, msec;
    msec = floor(modf(s->x, &day) * daysec * 1000.0);
    
    // Install the date and the new value of UT1-UTC in a network
    // message container.
    
    rtc.cmd.ut1utc.mjd    = (long int)day;
    rtc.cmd.ut1utc.utc    = (long int)msec;
    rtc.cmd.ut1utc.ut1utc = (long int)(s->y * 1.0e6);  /* micro-seconds */
    rtc.antennas          = AntNum::ANTALL;
    
    // Queue the message to be sent to the real-time-controller.
    
    if(nav->rtc_online && queue_rtc_command(cp, &rtc, NET_UT1UTC_CMD, false))
      return 1;
  };
  
  // Update the record of cached sample times.
  
  for(i=0; i<3-n; i++)
    nav->cache.ut1utc[i] = nav->cache.ut1utc[i+n];
  for(i=3-n; i<3; i++)
    nav->cache.ut1utc[i] = data.s[i].x;
  
  // Get the window of times for which the new parameters will remain
  // valid.
  
  if(get_ut1utc_window(nav->sc, utc, &nav->window.ut1utc))
    return 1;
  return 0;
}

/*.......................................................................
 * Send an update of the current Equation of the Equinoxes interpolation
 * parameters to the real-time controller.
 *
 * Input:
 *  nav      Navigator *   The resource object of this thread.
 *  tt          double     The current Terrestrial Dynamic Time as a
 *                         Modified Julian Date.
 *  reset          int     Normally the real-time controller is only
 *                         sent incremental updates, but if this
 *                         argument is set to non-zero, all three points
 *                         of the current quadratic interpolator will be
 *                         sent. This should be done whenever the
 *                         time controller is restarted.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
static int nav_update_eqneqx(Navigator *nav, double tt, int reset)
{
  ControlProg *cp = nav->cp; /* The control-program resource object */
  QuadData data;   /* The interpolator contents to be sent */
  RtcNetCmd rtc;   /* A network object to be sent to the real-time */
                   /*  controller task */
  int n;           /* The number of new interpolation samples to be sent */
  int i;
  
  // Get the current interpolation parameters.
  
  if(get_eqneqx_QuadData(nav->sc, tt, &data))
    return 1;
  
  // Determine how many samples of the tracker's 3-point interpolation
  // cache need to be updated.
  
  if(reset) {
    n = 3;
  } else {
    double earliest = data.s[0].x;
    for(n=0; n<3 && nav->cache.eqneqx[n] != earliest; n++)
      ;
  };
  
  // Send the n new samples to the tracker.
  
  for(i=3-n; i<3; i++) {
    QuadSample *s = data.s + i;
    
    // Convert the Modified Julian Date of the sample to an integral
    // day number and the number of milli-seconds into that day.
    
    double day, msec;
    msec = floor(modf(s->x, &day) * daysec * 1000.0);
    
    // Install the date and the new value of the equation of the
    // equinoxes in a network message container.
    
    rtc.cmd.eqneqx.mjd      = (long int) day;
    rtc.cmd.eqneqx.tt       = (long int) msec;
    
    // Milli-arcseconds 
    
    rtc.cmd.eqneqx.eqneqx   = (long int)floor(s->y * rtomas + 0.5); 
    rtc.antennas            = AntNum::ANTALL;
    
    // Queue the message to be sent to the real-time-controller.
    
    if(nav->rtc_online && queue_rtc_command(cp, &rtc, NET_EQNEQX_CMD, false))
      return 1;
  };
  
  // Update the record of cached sample times.
  
  for(i=0; i<3-n; i++)
    nav->cache.eqneqx[i] = nav->cache.eqneqx[i+n];
  for(i=3-n; i<3; i++)
    nav->cache.eqneqx[i] = data.s[i].x;
  
  // Get the window of times for which the new parameters will remain
  // valid.
  
  if(get_eqneqx_window(nav->sc, tt, &nav->window.eqneqx))
    return 1;
  return 0;
}

/*.......................................................................
 * Send an update of the positional parameters of the current scan
 * to the real-time controller.
 *
 * Input:
 *  nav      Navigator *   The rescan object of this thread.
 *  tt          double     The current Terrestrial Dynamic Time as
 *                         a Modified Julian Date.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
static int nav_update_scan(Navigator *nav, double tt)
{
  Scan *scan;     /* The current scan */
  ScanId id;     /* The type, name and number of the current scan */
  
  /*
   * Look up the current scan.
   */
  scan = find_ScanByNumber(nav->scanc, nav->current_scan);
  if(!scan)
    return 1;
  
  /*
   * Determine the type of the current scan.
   */
  if(get_ScanId(scan, 1, &id))
    return 1;
  
  /*
   * Different scans may need different forms of updating.
   */
  switch(id.type) {
  case SCAN_NORMAL:
    return nav_update_normal_scan(nav, tt, scan, &id, 0);
    break;
  case SCAN_BOGUS: /* Do nothing */
    return 0;
  default:
    lprintf(stderr, "nav_update_scan: Unknown scan type.\n");
    return 1;
    break;
  };
  return 0;
}

/*.......................................................................
 * Send an update of the positional parameters of all sources to the
 * real-time controller.
 *
 * Input:
 *  nav      Navigator *   The resource object of this thread.
 *  tt          double     The current Terrestrial Dynamic Time as
 *                         a Modified Julian Date.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
static int nav_update_source(Navigator *nav, double tt, 
			     sza::util::Tracking::Type type)
{
  sza::array::Source* src=0;   /* The current source */
  SourceId id;     /* The type, name and number of the current source */
  std::list<Center*>* centerList = 0;
  
  // Get the list of pointing centers
  
  COUT("nav_update_source called with tracking type: " << type);

  centerList = type==Tracking::TRACK_PHASE ? nav->phaseManager_->getCenterList() :
    nav->pointingManager_->getCenterList();
  
  // Loop over all known centers
  
  for(std::list<Center*>::iterator pc = centerList->begin();
      pc != centerList->end(); pc++) 
  {
    Center* pcPtr = *pc;
    
    // Look up the source associated with this pointing center
    
    src = find_SourceByNumber(nav->sc, pcPtr->getCatalogNumber());
    
    if(!src)
      return 1;
    
    // Determine the type of the current source.
    
    if(get_SourceId(src, 1, &id))
      return 1;
    
    // Different sources need different forms of updating.
    
    switch(id.type) {
    case SRC_J2000:
      return nav_update_J2000_source(nav, tt, src, &id, type,
				     pcPtr->getAntennas(), -1);
      break;
    case SRC_EPHEM:
      return nav_update_ephem_source(nav, tt, src, &id, type,
				     pcPtr->getAntennas(), -1);
      break;
    case SRC_FIXED:    /* Fixed sources don't need updating */
      break;
    default:
      lprintf(stderr, "nav_update_source: Unknown source type.\n");
      return 1;
      break;
    };
  }
  
  // Update any timing windows this command may have changed
  
  try {
    
    if(type & Tracking::TRACK_PHASE)
      nav->phaseManager_->updateCacheWindow(&nav->window.phase);
    
    if(type & Tracking::TRACK_POINT)
      nav->pointingManager_->updateCacheWindow(&nav->window.pointing);
    
  } catch(...) {
    return 1;
  }
  
  return 0;
}

/*.......................................................................
 * Send an update of the interpolation parameters of an ephemeris source
 * to the real-time controller.
 *
 * This command can legally be called with type set to TRACK_POINT,
 * TRACK_PHASE or TRACK_BOTH.  The latter case will only happen if a
 * track command is issued for phase and pointing simultaneously, in
 * which case it doesn't matter which ephemeris cache we use below.  
 *
 * Input:
 *  nav      Navigator *   The resource object of this thread.
 *  tt          double     The current Terrestrial Dynamic Time as
 *                         a Modified Julian Date.
 *  src         Source *   The ephemeris source to update.
 *  id        SourceId *   The identification header of 'src'.
 *  seq       unsigned     The pmac transaction sequence number for a
 *                         new track, or 0 to update the current source.
 * Output:
 *  return         int     0 - OK.
 *                         1 - Error.
 */
static int nav_update_ephem_source(Navigator *nav, double tt, 
				   sza::array::Source *src, SourceId *id, 
				   sza::util::Tracking::Type type,
				   unsigned antennas, signed seq)
{
  ControlProg *cp = nav->cp; /* The control-program resource object */
  QuadData ra;   /* The contents of the Right Ascension interpolator */
  QuadData dec;  /* The contents of the Declination interpolator */
  QuadData dist; /* The contents of the distance interpolator */
  RtcNetCmd rtc; /* A network object to be sent to the real-time */
                 /*  controller task */
  int n;         /* The number of new interpolation samples to be sent */
  int i;
  Center* phaseCenter=0;
  Center* pointingCenter=0;
  double* phaseEphemCache=0; // If phase tracking was requested, a  
			     // pointer to the phase ephemeris cache
  double* pointEphemCache=0; // If pointing tracking was requested, a
			     // pointer to the pointing ephemeris
			     // cache
  double* ephemCache=0;	     // A pointer to either one of the above.
			     // In the case of type==TRACK_BOTH, it
			     // doesn't matter which one it ends up
			     // pointing to.
  
  COUT("Inside nav_update_ephem_source()");
  
  // Get the current interpolation parameters.
  
  if(get_ephem_QuadData(nav->sc, src, tt, &ra, &dec, &dist))
    return 1;
  
  // Get the ephemeris cache for this source
  
  try {
    
    if(type & sza::util::Tracking::TRACK_PHASE) {
      phaseCenter = nav->phaseManager_->getCenter(id);
      phaseEphemCache = phaseCenter->getEphemerisCache();
      ephemCache = phaseEphemCache;
    }
    
    if(type & sza::util::Tracking::TRACK_POINT) {
      pointingCenter = nav->pointingManager_->getCenter(id);
      pointEphemCache = pointingCenter->getEphemerisCache();
      ephemCache = pointEphemCache;
    }
  } catch (...) {
    return 1;
  }
  
  // Determine how many samples of the tracker's 3-point interpolation
  // cache need to be updated.
  
  if(seq >= 0) {
    n = 3;
  } else {
    double earliest = ra.s[0].x;
    for(n=0; n<3 && ephemCache[n] != earliest; n++)
      ;
  };
  
  // Record the source name.
  
  strncpy(rtc.cmd.track.source, id->name, SRC_LEN-1);
  rtc.cmd.track.source[SRC_LEN-1] = '\0';
  rtc.cmd.track.number  = id->number;
  rtc.cmd.track.srcType = id->type;
  
  // Send the n new samples to the tracker.
  
  for(i=3-n; i<3; i++) {

    QuadSample *s_ra   = ra.s   + i; // Right Ascension sample
    QuadSample *s_dec  = dec.s  + i; // Declination sample 
    QuadSample *s_dist = dist.s + i; // Distance sample 
    
    // Convert the Modified Julian Date TT of the sample to an
    // integral day number and the number of milli-seconds into that
    // day.
    
    double day, msec;
    msec = floor(modf(s_ra->x, &day) * daysec * 1000.0);
    
    // Install the date and the new position in a network message
    // container. Note that only the first position of of track gets a
    // sequence number.
    
    rtc.cmd.track.seq  = seq >= 0 && i==0 ? seq : -1;
    rtc.cmd.track.mjd  = (long int)day;
    rtc.cmd.track.tt   = (long int)msec;
    rtc.cmd.track.ra   = (long int)floor(s_ra->y * rtomas + 0.5);  // Milli-arcseconds
    rtc.cmd.track.dec  = (long int)floor(s_dec->y * rtomas + 0.5); // Milli-arcseconds
    rtc.cmd.track.dist = (long int)floor(s_dist->y * 1.0e6 + 0.5); // Micro-AU
    rtc.cmd.track.type = type;
    rtc.antennas       = antennas;
    
    // Queue the message to be sent to the real-time-controller.

    if(queue_rtc_command(cp, &rtc, NET_TRACK_CMD, seq != -1))
      return 1;
  };
  
  // Update the record of cached sample times.
  
  if(type & sza::util::Tracking::TRACK_PHASE) {
    for(i=0; i<3-n; i++) 
      phaseEphemCache[i] = phaseEphemCache[i+n];
    for(i=3-n; i<3; i++) {
      phaseEphemCache[i] = ra.s[i].x;
    }
  }
  
  if(type & sza::util::Tracking::TRACK_POINT) {
    for(i=0; i<3-n; i++) 
      pointEphemCache[i] = pointEphemCache[i+n];
    for(i=3-n; i<3; i++) {
      pointEphemCache[i] = ra.s[i].x;
    }
  }
  
  // Get the window of times for which the new parameters will remain
  // valid.
  
  if(type & sza::util::Tracking::TRACK_PHASE) 
    if(get_ephem_window(nav->sc, src, tt, phaseCenter->getWindow()))
      return 1;
  if(type & sza::util::Tracking::TRACK_POINT) 
    if(get_ephem_window(nav->sc, src, tt, pointingCenter->getWindow()))
      return 1;
  
  return 0;
}

/*.......................................................................
 * Send an update of the precessed position of a J2000 source
 * to the real-time controller.
 *
 * Input:
 *  nav        Navigator*      The resource object of this thread.
 *  tt         double          The current Terrestrial Time as a Modified
 *                             Julian Date.
 *  src        Source*         The J2000 source to update.
 *  id         SourceId        The identification header of 'src'.
 *  type       Tracking::Type  The type of tracking command 
 *  antennas   AntNum::Id      A bitmask of antennas to which this command applies
 *  seq        unsigned        The pmac transaction sequence number of
 *                             a new track, or 0 to update the position of
 *                             the current source.
 * Output:
 *  return     int     0 - OK.
 *                          1 - Error.
 */
static int nav_update_J2000_source(Navigator *nav, double tt, sza::array::Source *src,
				   SourceId *id, sza::util::Tracking::Type type,
				   unsigned antennas, signed seq)
{
  double ra,dec;    // The updated geocentric RA and Dec of the source 
  double day, msec; // The TT date in days and milliseconds 
  RtcNetCmd rtc;    // The network object to be sent to the real-time 
                    // controller task 
  
  // Get the current geocentric Right Ascension and Declination.
  
  if(precess_J2000_source(nav->sc, src, tt, &ra, &dec))
    return 1;
  
  // Convert the Modified Julian Date TT of the sample to an integral
  // day number and the number of milli-seconds into that day.
  
  msec = floor(modf(tt, &day) * daysec * 1000.0);
  
  // Record the source name.
  
  strncpy(rtc.cmd.track.source, id->name, SRC_LEN-1);
  rtc.cmd.track.source[SRC_LEN-1] = '\0';
  rtc.cmd.track.number  = id->number;
  rtc.cmd.track.srcType = id->type;
  
  // Install the date and the new position in a network message
  // container.
  
  rtc.cmd.track.seq  = seq;
  rtc.cmd.track.mjd  = (long int)day;
  rtc.cmd.track.tt   = (long int)msec;
  rtc.cmd.track.ra   = (long int)floor(ra * rtomas + 0.5);   /* Milli-arcseconds */
  rtc.cmd.track.dec  = (long int)floor(dec * rtomas + 0.5);  /* Milli-arcseconds */
  rtc.cmd.track.dist = (long int)0.0;
  rtc.cmd.track.type = type;
  rtc.antennas       = antennas;
  
  // Queue the message to be sent to the real-time-controller.
  
  if(queue_rtc_command(nav->cp, &rtc, NET_TRACK_CMD, seq != -1))
    return 1;
  
  // Get the window of times for which the new position will remain
  // valid.
  
  try {
    
    if(type & sza::util::Tracking::TRACK_PHASE)
      if(get_mapqk_window(nav->sc, tt, 
			  nav->phaseManager_->getCenter(id)->getWindow()))
	return 1;
    
    if(type & sza::util::Tracking::TRACK_POINT)
      if(get_mapqk_window(nav->sc, tt, 
			  nav->pointingManager_->getCenter(id)->getWindow()))
	return 1;
    
  } catch(...) {
    return 1;
  }
  
  return 0;
}

/*.......................................................................
 * Send an update of the offsets of a normal scan to the real-time
 * controller.
 *
 * Input:
 *  nav      Navigator *   The resource object of this thread.
 *  tt       double        The current Terrestrial Dynamic Time as
 *                         a Modified Julian Date.
 *  src      Scan *        The ephemeris scan to update.
 *  id       ScanId *      The identification header of 'src'.
 *  seq      unsigned      The pmac transaction sequence number for a
 *                         new track, or 0 to update the current scan.
 * Output:
 *  return   int     0 - OK.
 *                   1 - Error.
 */
static int nav_update_normal_scan(Navigator *nav, double tt, Scan *scan,
				  ScanId *id, unsigned seq)
{
  ControlProg *cp = nav->cp; /* The control-program rescan object */
  RtcNetCmd rtc; /* A network object to be sent to the real-time */
                 /*  controller task */
  NormalScan* normal;
  bool done = false;
  int n;         /* The number of new offsets to be sent */
  int istart=0, istop=0, i;
  
  /*
   * Resolve source aliases.
   */
  scan = resolve_ScanAliases(scan);
  if(!scan)
    return 1;
  
  /*
   * Check that the scan is a normal scan.
   */
  if(scan->id.type != SCAN_NORMAL) {
    lprintf(stderr, "nav_update_normal_scan: \"%s\" is not a normal scan.\n",
	    scan->id.name);
    return 1;
  };
  
  /*
   * Get the type-specific container of the scan
   */
  normal = &scan->normal;
  
  /**
   * Update the indices of the scan cache that we will send on this command.
   */
  if(nav->lastScanIndex < 0) {
    istart = 0;
    istop = SCAN_NET_NPT <= normal->offsets.size ? 
      SCAN_NET_NPT-1 : normal->offsets.size-1;
  } else {
    if((unsigned)nav->lastScanIndex == normal->offsets.size-1)
      done = true;
    else {
      istart = nav->lastScanIndex + 1;
      istop = (unsigned)(istart + SCAN_NET_NPT) <= normal->offsets.size ? 
	istart + SCAN_NET_NPT-1 : normal->offsets.size-1;
    }
  }
  
  /**
   * If we reached the last index on the last sent command, we are
   * done with this scan.
   */
  if(!done) {
    n = istop - istart + 1;
    
    /*
     * Record the scan name.
     */
    strncpy(rtc.cmd.scan.name, id->name, SCAN_LEN-1);
    rtc.cmd.scan.name[SCAN_LEN-1] = '\0';
    
    /**
     * Install the sequence number in the command container.
     */
    rtc.cmd.scan.seq = seq;
    
    /**
     * Send to all antennas
     */
    rtc.antennas     = AntNum::ANTALL;
    
    /*
     * Send the n new samples to the tracker.
     */
    for(i=0; i < n; i++) {
      /*
       * Install the index and the new position in a network message
       * container.
       */
      rtc.cmd.scan.index[i] = normal->offsets.cache[istart+i].index;
      rtc.cmd.scan.azoff[i] = (long int)(normal->offsets.cache[istart+i].azoff * dtomas);
      rtc.cmd.scan.eloff[i] = (long int)(normal->offsets.cache[istart+i].eloff * dtomas);
      rtc.cmd.scan.dkoff[i] = (long int)(normal->offsets.cache[istart+i].dkoff * dtomas);
    }
    
    rtc.cmd.scan.npt = n;
    
    /*
     * Queue the message to be sent to the real-time-controller.
     */
    if(queue_rtc_command(cp, &rtc, NET_SCAN_CMD, seq != -1))
      return 1;
    
    /*
     * And increment the last sent index
     */
    nav->lastScanIndex += n;
    
    /*
     * And get the window of times for which the new parameters will
     * remain valid.
     */
    if(get_scan_window(nav->scanc, scan, tt, &nav->window.scan))
      return 1;
    
  } else { /* Done, turn off updates */
    /* To do -- unset current scan */
    nav->window.scan.tmin = nav->window.scan.tmax = 0.0;
    nav->lastScanIndex = -1;
  }
  
  return 0;
}

/*.......................................................................
 * This is a public thread-safe function, called upon to send a halt
 * command to the tracker task of the antenna control system and make
 * the current sources in the catalog for those antennas look like
 * fixed sources with approximately the coordinates at which the
 * telescopes were halted.
 *
 * Input:
 *  nav    Navigator *   The resource object of this thread.
 *  name        char *   The name of the new source.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
int nav_halt_telescope(Navigator *nav, unsigned antennas, unsigned seq)
{
  sza::array::Source *src;     // The new source 
  SourceInfo info; // Contemporary information about the current source 
  RtcNetCmd rtc;   // The network object to be sent to the real-time
                   // controller task
  int status;      // The return status of pthread functions 
  
  // Acquire exclusive access to the navigator resource object.
  
  if((status=pthread_mutex_lock(&nav->guard))) {
    lprintf(stderr, "nav_halt_telescope (mutex_lock): %s.\n", strerror(status));
    return 1;
  };
  
  try {
    
    // Fill the network message container.
    
    rtc.cmd.halt.seq = seq;
    rtc.antennas     = antennas;
    
    // Queue the message to be sent to the real-time controller task.
    
    if(queue_rtc_command(nav->cp, &rtc, NET_HALT_CMD))
      return nav_error_while_locked(nav);
    
    // Get the current sources associated with these antennas and
    // determine their types.
    
    std::vector<std::pair<SourceId, AntNum::Id> > srcAntAssoc = 
      nav->pointingManager_->getCenterAssociations((AntNum::Id)antennas);
    
    // Iterate over all distinct sources for te affected antennas
    
    for(unsigned isrc=0; isrc < srcAntAssoc.size(); isrc++) 
    {
      SourceId id       = srcAntAssoc[isrc].first;
      AntNum::Id antSet = srcAntAssoc[isrc].second;
      
      src = find_SourceByNumber(nav->sc, id.number);
      
      if(!src) {
	lprintf(stderr, "Error finding current source.\n");
	return nav_error_while_locked(nav);
      };
      
      // If the source is not already a fixed source, create a fixed
      // source at the last known coordinates of this source.
      
      if(id.type != SRC_FIXED) {
	
	// Determine the current coordinates of the current source.
	
	if(source_info(nav->sc, &nav->site, src, -1, 0.0, SIO_HORIZ, &info))
	  return nav_error_while_locked(nav);
	
	// Create a dummy source with the horizon coordinates of the
	// current source for this set of antennas.  This source will
	// always be called current, even though there may be mor than
	// one.
	
	src = add_FixedSource(nav->sc, CURRENT_SOURCE, info.axis_mask,
			      info.coord.az, info.coord.el, info.coord.pa);
	
	if(!src)
	  return nav_error_while_locked(nav);
	
	// Lastly, install this source as the new pointing center (and
	// phase center) for this set of antennas
	
	COUT("Installing " << src->id.name << " as pointing center for antennas: " << antSet);
	nav->pointingManager_->changeCenter(antSet, src->id);
	nav->phaseManager_->changeCenter(antSet, src->id);
      };
    }
    
    // And update the interpolation windows for the new set of current
    // sources.
    
    nav->pointingManager_->updateCacheWindow(&nav->window.pointing);
    nav->phaseManager_->updateCacheWindow(&nav->window.phase);

  } catch (...) {
    return nav_error_while_locked(nav);
  }
  
  // Relinquish exclusive access to the navigator resource object.
  
  pthread_mutex_unlock(&nav->guard);
  
  return 0;
}

/*.......................................................................
 * A public thread-safe function used to tell the navigator to
 * initiate a new scan.
 *
 * Input:
 *  nav    Navigator *   The resource object of this thread.
 *  name        char *   The name of the new scan.
 *  seq     unsigned     The pmac transaction sequence number to use
 *                       when sending the resulting scan command to the 
 *                       tracker.
 *
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
int nav_start_scan(Navigator *nav, char *name, unsigned seq)
{
  Scan *scan;  /* The new scan */
  ScanId id;  /* The identification header of the scan */
  int status;   /* The return status of pthread functions */
  
  /*
   * A sequence number of zero is illegal, both because this is the start
   * value of the tracker sequence before any transactions have been
   * started, and because zero is used to signify an update of the position
   * of the scan that the tracker is tracking, not a new scan.
   */
  if(!seq) {
    lprintf(stderr, "nav_track_scan: Illegal zero sequence number.\n");
    return 1;
  };
  /*
   * Acquire exclusive access to the navigator resource object.
   */
  if((status=pthread_mutex_lock(&nav->guard))) {
    lprintf(stderr, "nav_track_scan (mutex_lock): %s.\n", strerror(status));
    return 1;
  };
  /*
   * Look up the scan.
   */
  scan = find_ScanByName(nav->scanc, name);
  if(!scan) {
    lprintf(stderr, "Can't start unknown scan: %s\n", name);
    return nav_error_while_locked(nav);
  };
  
  /*
   * Look up the identification header of the scan.
   */
  if(get_ScanId(scan, 1, &id))
    return nav_error_while_locked(nav);
  
  /*
   * Different scan types may use different network commands.
   */
  switch(id.type) {
  case SCAN_NORMAL:
    if(nav_start_normal_scan(nav, scan, &id, seq))
      return nav_error_while_locked(nav);
    break;
  default:
    lprintf(stderr, "nav_track_scan: Unknown scan type.\n");
    return nav_error_while_locked(nav);
    break;
  };
  
  /*
   * Release the navigator resource object.
   */
  pthread_mutex_unlock(&nav->guard);
  
  return 0;
}

/*.......................................................................
 * A public thread-safe function used to tell the navigator to initiate
 * tracking a new source.
 *
 * Input:
 *  nav    Navigator *   The resource object of this thread.
 *  name        char *   The name of the new source.
 *  seq     unsigned     The pmac transaction sequence number to use
 *                       when sending the resulting track or slew
 *                       command to the tracker.
 * Output:
 *  return       int     0 - OK.
 *                       1 - Error.
 */
int nav_track_source(Navigator *nav, char *name, 
		     sza::util::Tracking::Type type, 
		     unsigned antennas, unsigned seq)
{
  sza::array::Source *src; // The new source 
  SourceId id; // The identification header of the source
  int status;  // The return status of pthread functions
  
  std::cout << "nav_track_source: sequence number is: seq" << seq << std::endl;
  
  // Look up the source.  We use the extended version of the lookup
  // function in case the source was specified as "current."  In this
  // case, we need to loop over all distinct antenna-source
  // associations, issuing separate commands to track the current
  // pointing sources for each association.  In this case, we search
  // for current pointing sources only, as there may be cases where we
  // wish to phase track the current pointing positions, as after a
  // slew, but never vice versa.
  
  std::vector<std::pair<SourceId, AntNum::Id> > sourceList;
  
  sourceList = 
    navLookupSourceExtended(nav, name, Tracking::TRACK_POINT, antennas, 1);
  
  // Acquire exclusive access to the navigator resource object.
  
  if((status=pthread_mutex_lock(&nav->guard))) {
    lprintf(stderr, "nav_track_source (mutex_lock): %s.\n", strerror(status));
    return 1;
  };
  
  // Enclose the rest in a try-catch clause, so that we do not exit
  // without releasing the guard mutex.
  
  try {
    
    // Now loop over all returned sources, issuing track commands for
    // each one.
    
    for(std::vector<std::pair<SourceId, AntNum::Id> >::iterator 
	  isrc = sourceList.begin(); isrc != sourceList.end(); isrc++) 
    {
      //      AntNum antSet(isrc->second);
      
      id = isrc->first;
      
      src = find_SourceByNumber(nav->sc, id.number);
      
      if(!src) {
	lprintf(stderr, "Can't track unknown source: %s\n", name);
	return nav_error_while_locked(nav);
      };
      
      // Inform the relevant tracking manager of the new source-antenna
      // association.
      
      if(type & Tracking::TRACK_PHASE)
	nav->phaseManager_->changeCenter((AntNum::Id) antennas, id);
      
      if(type & Tracking::TRACK_POINT)
	nav->pointingManager_->changeCenter((AntNum::Id) antennas, id);
      
      // Different source types use different network commands.
      
      switch(id.type) {
      case SRC_FIXED:
	if(nav_track_fixed_source(nav, src, &id, type, antennas, seq))
	  return nav_error_while_locked(nav);
	break;
      case SRC_J2000:
	if(nav_track_J2000_source(nav, src, &id, type, antennas, seq))
	  return nav_error_while_locked(nav);
	break;
      case SRC_EPHEM:
	if(nav_track_ephem_source(nav, src, &id, type, antennas, seq))
	  return nav_error_while_locked(nav);
	break;
      default:
	lprintf(stderr, "nav_track_source: Unknown source type.\n");
	return nav_error_while_locked(nav);
	break;
      };
    }
    
    // Update any timing windows this command may have changed
    
    if(type & Tracking::TRACK_PHASE)
      nav->phaseManager_->updateCacheWindow(&nav->window.phase);
    
    if(type & Tracking::TRACK_POINT)
      nav->pointingManager_->updateCacheWindow(&nav->window.pointing);
    
  } catch (...) {
    return nav_error_while_locked(nav);
  }
  
  // Release the navigator resource object.
  
  pthread_mutex_unlock(&nav->guard);
  
  return 0;
}

/*.......................................................................
 * This is a private function of nav_track_source(), called on to slew
 * the telescope to a given fixed source.
 *
 * Input:
 *  nav       Navigator * The resource object of this thread.
 *  src       Source    * The fixed source to slew to.
 *  id        SourceId  * The identification header of the source.
 *  seq       unsigned    The pmac transaction sequence number to
 *                        record in the NetSlewCmd packet.
 * Output:
 *  return    int         0 - OK.
 *                        1 - Error.
 */
static int nav_track_fixed_source(Navigator *nav, sza::array::Source *src, SourceId *id,
				  sza::util::Tracking::Type type, 
				  unsigned antennas, unsigned seq)
{
  RtcNetCmd rtc;       /* The network object to be sent to the real-time */
                       /*  controller task */
  SourceAzElPa coord;  /* The coordinates of the source */
  unsigned axis_mask;  /* A bitwise union of SourceAxes enumerators */
  
  // Get the coordinates of the source.
  
  if(describe_fixed_source(src, &coord, &axis_mask))
    return 1;
  
  // Fill the network message container.
  
  strncpy(rtc.cmd.slew.source, id->name, SRC_LEN-1);
  rtc.cmd.slew.source[SRC_LEN-1] = '\0';
  rtc.cmd.slew.number = id->number;
  rtc.cmd.slew.seq    = seq;
  rtc.cmd.slew.mask   = axis_mask;
  rtc.cmd.slew.az     = (long int)(coord.az * rtomas);   /* Milli-arcseconds */
  rtc.cmd.slew.el     = (long int)(coord.el * rtomas);   /* Milli-arcseconds */
  rtc.cmd.slew.dk     = (long int)(coord.pa * rtomas);   /* Milli-arcseconds */
  rtc.cmd.slew.type   = type;
  rtc.antennas        = antennas;
  
  // Queue the message to be sent to the real-time controller task.
  
  if(queue_rtc_command(nav->cp, &rtc, NET_SLEW_CMD))
    return 1;
  
  return 0;
}

/*.......................................................................
 * This is a private function of nav_track_source(), called upon to tell
 * the tracker to start tracking a new J2000 source.
 *
 * Input:
 *  nav     Navigator *  The resource object of this thread.
 *  src        Source *  The J2000 source to be tracked.
 *  id       SourceId *  The identification header of the source.
 *  seq      unsigned    The pmac transaction sequence number to
 *                       record in the NetTrackCmd packet.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int nav_track_J2000_source(Navigator *nav, sza::array::Source *src, SourceId *id,
				  sza::util::Tracking::Type type, 
				  unsigned antennas, unsigned seq)
{
  // Get the current Terrestrial Time.
  
  double tt = current_mjd_tt();
  
  if(tt < 0.0)
    return 1;
  
  // Send the source coordinates to the real-time controller, using
  // the incremented transaction number to indicate that this is a new
  // source.
  
  if(nav_update_J2000_source(nav, tt, src, id, type, antennas, seq))
    return 1;
  
  return 0;
}

/*.......................................................................
 * This is a private function of nav_track_source(), called upon to tell
 * an antennas to start tracking a new ephemeris source.
 *
 * Input:
 *  nav     Navigator *  The resource object of this thread.
 *  src        Source *  The ephemeris source to be tracked.
 *  id       SourceId *  The identification header of the source.
 *  seq      unsigned    The pmac transaction sequence number to
 *                       record in the NetTrackCmd packet.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int nav_track_ephem_source(Navigator *nav, sza::array::Source *src, SourceId *id,
				  sza::util::Tracking::Type type, 
				  unsigned ant, unsigned seq)
{
  // Get the current Terrestrial Time.
  
  double tt = current_mjd_tt();
  
  if(tt < 0.0)
    return 1;
  
  // Send the source coordinates to the real-time controller, using
  // the incremented transaction number to indicate that this is a new
  // source.
  
  if(nav_update_ephem_source(nav, tt, src, id, type, ant, seq))
    return 1;
  
  return 0;
}

/*.......................................................................
 * This is a private function of nav_start_scan(), called upon to tell
 * the tracker to start a new scan.
 *
 * Input:
 *  nav     Navigator *  The resource object of this thread.
 *  scan    Scan      *  The scan to start
 *  id      SourceId  *  The identification header of the scan.
 *  seq     unsigned     The pmac transaction sequence number to
 *                       record in the NetScanCmd packet.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int nav_start_normal_scan(Navigator *nav, sza::array::Scan *scan, 
				 ScanId *id,
				 unsigned seq)
{
  /*
   * Get the current Terrestrial Time.
   */
  double tt = current_mjd_tt();
  if(tt < 0.0)
    return 1;
  
  /*
   * Send the scan offsets to the real-time controller, using the
   * incremented transaction number to indicate that this is a new
   * scan.
   */
  if(nav_update_normal_scan(nav, tt, scan, id, seq))
    return 1;
  
  /*
   * Record the new scan as the current scan.
   */
  return nav_current_scan(nav, scan);
}

/*.......................................................................
 * This is a public thread-safe function used to tell the navigator to
 * initiate a telescope slew to a given az,el,dk position.
 *
 * Input:
 *  nav      Navigator  * The resource object of this thread.
 *  coord    NavSlewMsg * The mount position to slew to.
 *  seq      unsigned     The pmac transaction sequence number to
 *                        record in the resulting NetSlewCmd packet.
 * Output:
 *  return          int   0 - OK.
 *                        1 - Error.
 */
int nav_slew_telescope(Navigator *nav, unsigned mask, double az, double el,
		       double dk, unsigned antennas, unsigned seq)
{
  sza::array::Source *src; // The new source 
  SourceId id; // The identification header of the source 
  int status;  // The return status of pthread functions 
  
  // Acquire exclusive access to the navigator resource object.
  
  if((status=pthread_mutex_lock(&nav->guard))) {
    lprintf(stderr, "nav_slew_telescope (mutex_lock): %s.\n", strerror(status));
    return 1;
  };
  
  // Create a temporary current source at the specified coordinates.
  
  src = add_FixedSource(nav->sc, CURRENT_SOURCE, mask, az, el, dk);
  
  if(!src)
    return nav_error_while_locked(nav);
  
  // Look up the identification header of the source.
  
  if(get_SourceId(src, 1, &id))
    return nav_error_while_locked(nav);
  
  // Lastly, install this source as the new pointing (and phase)
  // center for this set of antennas.  We reset the phase center so
  // that if a phase center was previously set by a track command, it
  // gets reset by this command
  
  nav->pointingManager_->changeCenter((AntNum::Id)antennas, id);
  nav->phaseManager_->changeCenter((AntNum::Id)antennas, id);
  
  // Send the slew command.
  
  if(nav_track_fixed_source(nav, src, &id, Tracking::TRACK_POINT, antennas, 
			    seq))
    return nav_error_while_locked(nav);
  
  // And update the interpolation windows for the new set of current
  // sources.
  
  try {

    nav->pointingManager_->updateCacheWindow(&nav->window.pointing);
    nav->phaseManager_->updateCacheWindow(&nav->window.phase);

  } catch (...) {
    return nav_error_while_locked(nav);
  }
  
  // Release the navigator resource object.
  
  pthread_mutex_unlock(&nav->guard);
  return 0;
}

/*.......................................................................
 * Set the location of the SZA.
 *
 * Input:
 *  nav      Navigator *  The resource object of the task.
 *  site    NavSiteMsg *  A site-specification message.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int nav_set_site(Navigator *nav, NavSiteMsg *site)
{
  if(set_Site(&nav->site, site->longitude, site->latitude, site->altitude))
    return 1;
  
  // If we are currently connected to the real-time cpu, send it the
  // new site information.
  
  if(nav_dispatch_site_to_rtc(nav))
    return 1;
  return 0;
}

/*.......................................................................
 * Install a new scan as the current scan.
 *
 * Input:
 *  nav    Navigator *  The rescan object of this thread.
 *  src       Scan *  The scan to install as the current scan.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static int nav_current_scan(Navigator *nav, sza::array::Scan *scan)
{
  ScanId id;  /* The indentification header of the scan */
  
  /*
   * Disable further scan updates until we know what is needed.
   */
  init_CacheWindow(&nav->window.scan);
  
  /*
   * If the scan isn't in the slot reserved for the current scan,
   * create an alias in the current scan slot to point to it.
   */
  if(get_ScanId(scan, 0, &id) ||
     (strcmp(id.name, CURRENT_SCAN) !=0 &&
      add_AliasScan(nav->scanc, CURRENT_SCAN, id.name) == NULL))
    return 1;
  
  /*
   * Query the scan type.
   */
  if(get_ScanId(scan, 1, &id))
    return 1;
  
  /*
   * Set the update window according to the scan type.
   */
  switch(id.type) {
  case SCAN_NORMAL:     /* Set the update to be half the number of
			   1-second samples we just sent. */
    if(get_scan_window(nav->scanc, scan, -1.0, &nav->window.scan))
      return 1;
    break;
  case SCAN_BOGUS:     /* Do nothing */
    break;
  default:
    lprintf(stderr, "set_current_scan: Unknown scan type.\n");
    return 1;
  };
  return 0;
}

/*.......................................................................
 * Send the current site parameters to the real-time cpu.
 *
 * Input:
 *  nav      Navigator *  The resource object of the task.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int nav_dispatch_site_to_rtc(Navigator *nav)
{
  RtcNetCmd rtc;                 /* The command to send to the controller */
  
  // Compose the real-time controller network command.
  
  rtc.cmd.site.lon = (long int)(nav->site.longitude * rtomas);  /* mill-arcsec */
  rtc.cmd.site.lat = (long int)(nav->site.latitude * rtomas);   /* mill-arcsec */
  rtc.cmd.site.alt = (long int)(nav->site.altitude * 1000.0);   /* mm */
  rtc.antennas     = AntNum::ANTALL;
  
  // Send the command to the real-time controller.
  
  if(queue_rtc_command(nav->cp, &rtc, NET_SITE_CMD))
    return 1;
  return 0;
}

/*.......................................................................
 * Adapt to a change in the state of the connection to the real-time
 * controller.
 *
 * Input:
 *  nav     Navigator *   The resource object of the thread.
 *  online        int     True if the RTC has just become connected,
 *                        false if it has just disconnected.
 * Output:
 *  return        int     0 - OK.
 *                        1 - Error.
 */
static int nav_adopt_rtc_state(Navigator *nav, int online)
{
  
  // Record the new state.
  
  nav->rtc_online = online;
  
  // Clear all cache windows to prevent further updates until the
  // appropriate calls to nav_update_... are made.
  
  init_CacheWindow(&nav->window.ut1utc);
  init_CacheWindow(&nav->window.eqneqx);
  init_CacheWindow(&nav->window.phase);
  init_CacheWindow(&nav->window.pointing);
  
  // If we have just become connected to the controller, the
  // controller won't have any interpolation samples for UT1-UTC or
  // the equation of the equinoxes.
  
  if(online) {
    double utc;   /* The current Universal Coordinated Time */
    double tt;    /* The current Terrestrial Dynamic Time */
    
    // Get the current time as UTC and TT.
    
    if((utc = current_mjd_utc()) < 0.0 || (tt = mjd_utc_to_mjd_tt(utc)) < 0.0)
      return 1;
    
    // Send the current UT1-UTC samples.
    
    if(nav_update_ut1utc(nav, utc, 1))
      return 1;
    
    // Send the current equation-of-the-equinoxes samples.
    
    if(nav_update_eqneqx(nav, tt, 1))
      return 1;
    
    // Send the current site parameters.
    
    if(nav_dispatch_site_to_rtc(nav))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * An inter-thread accessor function to look up a source in the
 * navigator source catalog.
 *
 * Input:
 *  nav    Navigator *  The resource object of the navigator thread.
 *                      External threads should call
 *                      cp_ThreadData(cp, CP_NAVIGATOR) to acquire
 *                      this.
 *  name        char *  The name of the source to be looked up.
 *  resolve      int    If true resolve all aliases to return the
 *                      true source. Otherwise return what may be an
 *                      alias to another source.
 * Input/Output:
 *  id      SourceId *  If the source is found, its identification header
 *                      will be copied into *id.
 * Output:
 *  return       int    0 - Source found ok.
 *                      1 - Source not found (no error message).
 */
int nav_lookup_source(Navigator *nav, char *name, int resolve, 
		      SourceId* id)
{
  sza::array::Source *src=NULL; /* The located source */
  int status;       /* The return status of pthread_mutex_lock() */
  int waserr=0;     /* True if an error occurs while the mutex is held */
  
  // Check the arguments.
  
  if(!nav || !name || !id) {
    lprintf(stderr, "nav_lookup_source: NULL argument(s).\n");
    return 1;
  };
  
  // Acquire the resource-protection mutex before proceding.
  
  if((status=pthread_mutex_lock(&nav->guard))) {
    lprintf(stderr, "nav_lookup_source (mutex_lock): %s.\n", strerror(status));
    return 1;
  };
  
  // Lookup the source.
  
  waserr = waserr || (src = find_SourceByName(nav->sc, name)) == NULL;
  
  // Get a copy of its identification header.
  
  waserr = waserr || get_SourceId(src, resolve, id);
  
  // Relinquish exclusive access to the thread resources.
  
  pthread_mutex_unlock(&nav->guard);
  return waserr;
}

/*.......................................................................
 * An inter-thread accessor function to look up a source in the
 * navigator source catalog.
 *
 * Input:
 *  nav    Navigator *  The resource object of the navigator thread.
 *                      External threads should call
 *                      cp_ThreadData(cp, CP_NAVIGATOR) to acquire
 *                      this.
 *  name        char *  The name of the source to be looked up.
 *  resolve      int    If true resolve all aliases to return the
 *                      true source. Otherwise return what may be an
 *                      alias to another source.
 * Input/Output:
 *  id      SourceId *  If the source is found, its identification header
 *                      will be copied into *id.
 * Output:
 *  return       int    0 - Source found ok.
 *                      1 - Source not found (no error message).
 */
std::vector<std::pair<SourceId, AntNum::Id> > 
navLookupSourceExtended(Navigator *nav, char *name, 
			sza::util::Tracking::Type type, 
			unsigned antennas, int resolve)
{
  sza::array::Source *src=NULL; /* The located source */
  int status;       /* The return status of pthread_mutex_lock() */
  std::vector<std::pair<SourceId, AntNum::Id> > sourceList;
  sza::util::LogStream errStr;

  // Check the arguments.

  if(!nav || !name) {
    errStr.appendMessage(true, "NULL argument(s)");
    throw Error(errStr);
  };
  
  // Acquire the resource-protection mutex before proceeding.

  if((status=pthread_mutex_lock(&nav->guard))) {
    errStr.appendSysError(true, "pthread_mutex_lock");
    throw Error(errStr);
  };

  // Enclose the following in a try clause so that exceptions don't
  // cause us to exit without unlocking the guard mutex.

  try {

    // If the current source was requested, this can be different for
    // different antennas; in this case, we retrieve the list of
    // distinct sources for all requested antennas.

    if(strcasecmp(name, CURRENT_SOURCE)==0) 
      sourceList = 
	type==Tracking::TRACK_PHASE ? 
	nav->phaseManager_->getCenterAssociations((AntNum::Id)antennas) :
	nav->pointingManager_->getCenterAssociations((AntNum::Id)antennas);

    // Else a source was explicitly named

    else { 

      SourceId id;

      // Lookup the source.
      
      if((src = find_SourceByName(nav->sc, name)) == NULL) 
	errStr.appendMessage(true, "Error in find_SourceByName()");
      
      // Get a copy of its identification header.
      
      if(get_SourceId(src, resolve, &id))
	errStr.appendMessage(true, "Error in getSourceId()");
      
      // Insert it as the only element of the output vector

      std::pair<SourceId, AntNum::Id> tmpPair;
      
      tmpPair.first = id; // Copy constructor
      tmpPair.second = AntNum::ANTALL; // ignored
      
      sourceList.push_back(tmpPair);
    }
  } catch (...) {
    errStr.appendMessage(true, "Caught an exception");
  }

  // Relinquish exclusive access to the thread resources.
    
  pthread_mutex_unlock(&nav->guard);
  
  // If an error occurred, throw it now.

  if(errStr.isError())
    throw Error(errStr);

  return sourceList;
}

/*.......................................................................
 * An inter-thread accessor function to look up a scan in the
 * navigator scan catalog.
 *
 * Input:
 *  nav    Navigator *  The resource object of the navigator thread.
 *                      External threads should call
 *                      cp_ThreadData(cp, CP_NAVIGATOR) to acquire
 *                      this.
 *  name        char *  The name of the scan to be looked up.
 *  resolve      int    If true resolve all aliases to return the
 *                      true scan. Otherwise return what may be an
 *                      alias to another scan.
 * Input/Output:
 *  id      ScanId *  If the scan is found, its identification header
 *                      will be copied into *id.
 * Output:
 *  return       int    0 - Scan found ok.
 *                      1 - Scan not found (no error message).
 */
int nav_lookup_scan(Navigator *nav, char *name, int resolve, ScanId *id)
{
  sza::array::Scan *scan=NULL; /* The located scan */
  int status;       /* The return status of pthread_mutex_lock() */
  int waserr=0;     /* True if an error occurs while the mutex is held */
  /*
   * Check the arguments.
   */
  if(!nav || !name || !id) {
    lprintf(stderr, "nav_lookup_scan: NULL argument(s).\n");
    return 1;
  };
  /*
   * Acquire the rescan-protection mutex before proceding.
   */
  if((status=pthread_mutex_lock(&nav->guard))) {
    lprintf(stderr, "nav_lookup_scan (mutex_lock): %s.\n", strerror(status));
    return 1;
  };
  /*
   * Lookup the scan.
   */
  waserr = waserr || (scan = find_ScanByName(nav->scanc, name)) == NULL;
  /*
   * Get a copy of its identification header.
   */
  waserr = waserr || get_ScanId(scan, resolve, id);
  /*
   * Relinquish exclusive access to the thread resources.
   */
  pthread_mutex_unlock(&nav->guard);
  return waserr;
}

/*.......................................................................
 * An inter-thread accessor function to look up a source in the
 * navigator source catalog.
 *
 * Input:
 *  nav    Navigator *  The resource object of the navigator thread.
 *                      External threads should call
 *                      cp_ThreadData(cp, CP_NAVIGATOR) to acquire
 *                      this.
 *  name        char *  The name of the source to be looked up.
 *  resolve      int    If true resolve all aliases to return the
 *                      true source. Otherwise return what may be an
 *                      alias to another source.
 * Input/Output:
 *  id      SourceId *  If the source is found, its identification header
 *                      will be copied into *id.
 * Output:
 *  return       int    0 - Source found ok.
 *                      1 - Source not found (no error message).
 */
int nav_print_scan_info(Navigator *nav, char *name, int resolve, ScanId *id)
{
  Scan* scan = NULL; /* The located source */
  int status;       /* The return status of pthread_mutex_lock() */
  int waserr=0;     /* True if an error occurs while the mutex is held */
  /*
   * Check the arguments.
   */
  if(!nav || !name || !id) {
    lprintf(stderr, "nav_lookup_source: NULL argument(s).\n");
    return 1;
  };
  /*
   * Acquire the resource-protection mutex before proceding.
   */
  if((status=pthread_mutex_lock(&nav->guard))) {
    lprintf(stderr, "nav_lookup_source (mutex_lock): %s.\n", strerror(status));
    return 1;
  };
  /*
   * Lookup the source.
   */
  waserr = waserr || (scan = find_ScanByName(nav->scanc, name)) == NULL;
  /*
   * Get a copy of its identification header.
   */
  waserr = waserr || get_ScanId(scan, resolve, id);
  /*
   * Relinquish exclusive access to the thread resources.
   */
  pthread_mutex_unlock(&nav->guard);

  lprintf(stdout, "Scan: %s\n",id->name);
  lprintf(stdout, "Npt:  %d\n",scan->normal.offsets.size);
  lprintf(stdout, "File: %s\n",scan->normal.file);

  return waserr;
}

/*.......................................................................
 * An inter-thread accessor function to look up contemporary statistics
 * of a given cataloged source, by name.
 *
 * Input:
 *  nav    Navigator *  The resource object of the navigator thread.
 *                      External threads should call
 *                      cp_ThreadData(cp, CP_NAVIGATOR) to acquire
 *                      this.
 *  name        char *  The name of the source to be looked up.
 *  utc       double    The UTC for which the information is needed,
 *                      expressed as a Modified Julian Date. Alternatively
 *                      the current UTC will be substituted if you pass -1.
 *  horizon   double    The elevation of the horizon to use for computation
 *                      or rise and set times (radians).
 *  options unsigned    A bitwise union of SrcInfoOpt enumerators. (see
 *                      source.h for a list). This is used to specify what
 *                      statistics you want to have returned in *info.
 * Input/Output:
 *  info  SourceInfo *  The requested statistics will be recorded in
 *                      *info. See source.h for details.
 * Output:
 *  return       int    0 - Source found ok.
 *                      1 - Source not found (no error message).
 */
int nav_source_info(Navigator *nav, char *name, double utc,
		double horizon, unsigned options, SourceInfo *info)
{
  sza::array::Source *src=NULL; /* The located source */
  int status;       /* The return status of pthread_mutex_lock() */
  int waserr=0;     /* True if an error occurs while the mutex is held */
/*
 * Check the arguments.
 */
  if(!nav || !name || !info) {
    lprintf(stderr, "nav_source_info: NULL argument(s).\n");
    return 1;
  };
/*
 * Acquire the resource-protection mutex before proceding.
 */
  if((status=pthread_mutex_lock(&nav->guard))) {
    lprintf(stderr, "nav_source_info (mutex_lock): %s.\n", strerror(status));
    return 1;
  };
/*
 * Lookup the source.
 */
  waserr = waserr || (src = find_SourceByName(nav->sc, name)) == NULL;
/*
 * Look up the requested information.
 */
  waserr = waserr || source_info(nav->sc, &nav->site, src, utc, horizon,
				 options, info);
/*
 * Relinquish exclusive access to the thread resources.
 */
  pthread_mutex_unlock(&nav->guard);
  return waserr;
}

/*.......................................................................
 * An inter-thread accessor function to look up contemporary statistics
 * of a given cataloged source by catalog number
 *
 * Input:
 *  nav    Navigator *  The resource object of the navigator thread.
 *                      External threads should call
 *                      cp_ThreadData(cp, CP_NAVIGATOR) to acquire
 *                      this.
 *  name        char *  The name of the source to be looked up.
 *  utc       double    The UTC for which the information is needed,
 *                      expressed as a Modified Julian Date. Alternatively
 *                      the current UTC will be substituted if you pass -1.
 *  horizon   double    The elevation of the horizon to use for computation
 *                      or rise and set times (radians).
 *  options unsigned    A bitwise union of SrcInfoOpt enumerators. (see
 *                      source.h for a list). This is used to specify what
 *                      statistics you want to have returned in *info.
 * Input/Output:
 *  info  SourceInfo *  The requested statistics will be recorded in
 *                      *info. See source.h for details.
 * Output:
 *  return       int    0 - Source found ok.
 *                      1 - Source not found (no error message).
 */
int nav_source_info(Navigator *nav, unsigned number, double utc,
		    double horizon, unsigned options, SourceInfo *info)
{
  sza::array::Source *src=NULL; /* The located source */
  int status;       /* The return status of pthread_mutex_lock() */
  int waserr=0;     /* True if an error occurs while the mutex is held */
  
  // Check the arguments.

  if(!nav || !info) {
    lprintf(stderr, "nav_source_info: NULL argument(s).\n");
    return 1;
  };
  
  // Acquire the resource-protection mutex before proceding.

  if((status=pthread_mutex_lock(&nav->guard))) {
    lprintf(stderr, "nav_source_info (mutex_lock): %s.\n", strerror(status));
    return 1;
  };
  
  // Lookup the source.

  waserr = waserr || (src = find_SourceByNumber(nav->sc, number)) == NULL;
  
  // Look up the requested information.

  waserr = waserr || source_info(nav->sc, &nav->site, src, utc, horizon,
				 options, info);
  
  // Relinquish exclusive access to the thread resources.

  pthread_mutex_unlock(&nav->guard);
  return waserr;
}

/**.......................................................................
 * Return true if the requested source is the current source.
 */
bool navIsCurrent(std::string name)
{
  if(strcasecmp(name.c_str(), CURRENT_SOURCE)==0)
    return true;
  return false;
}
