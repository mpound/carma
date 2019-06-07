#ifndef CACHE_H
#define CACHE_H

namespace sza {
  namespace array {

    /*
     * The following type records the window of times over which a
     * cache is valid. The cache is valid for time t, where tmin <= t
     * <= tmax.  Both tmin and tmax are Modified Julian Date
     * representations of times.  The time system being represented in
     * this manner depends on the cache.
     */
    struct CacheWindow {
      double tmin;    /* The earliest time at which the cached values are ok */
      double tmax;    /* The latest time at which the cached values are ok */
    };
  };
};
void init_CacheWindow(sza::array::CacheWindow *win);  /* Set tmin=tmax=0.0 */

#endif
