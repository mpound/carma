#include "carma/szaarrayutils/cache.h"

/*.......................................................................
 * A public function that initializes the contents of a CacheWindow
 * container to tmin=tmax=0.0.
 *
 * Input:
 *  win    CacheWindow *  The window to initialize.
 */
void init_CacheWindow(sza::array::CacheWindow *win)
{
  if(win)
    win->tmin = win->tmax = 0.0;
}

