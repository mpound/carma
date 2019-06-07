#include "carma/szaarrayutils/scancache.h"
#include "carma/szaarrayutils/lprintf.h"

/**.......................................................................
 * Empty a cache.
 */
int empty_ScanCache(ScanCache* cache)
{
  cache->size = 0;
  cache->currentIndex = 0;

  return 0;
}

/**.......................................................................
 * Add an element to the current cache.
 */
int extend_ScanCache(ScanCache* offsets, unsigned long index, double azoff, 
		     double eloff, double dkoff)
{
  if(offsets->size == SCAN_CACHE_SIZE) {
    lprintf(stderr, "Unable to add more points to the current cache.\n");
    return 1;
  }

  if(offsets->size > 0 && index != offsets->cache[offsets->size-1].index + 1) {
    lprintf(stderr, "Attempt to add a non-consecutive entry to the cache.\n");
    return 1;
  }
    
  /**
   * Else add the requested element to the top of the cache.
   */
  offsets->cache[offsets->size].index = index;
  offsets->cache[offsets->size].azoff = azoff;
  offsets->cache[offsets->size].eloff = eloff;
  offsets->cache[offsets->size].dkoff = dkoff;

  fprintf(stdout,"Cache offsets: %lf %lf %lf\n",azoff,eloff,dkoff);

  offsets->size++;

  return 0;
}

/**.......................................................................
 * Get the next set of offsets from the cache.
 */
int get_ScanCache(ScanCache* offsets, double* azoff, double* eloff, 
		  double* dkoff, bool* active)
{
  if(offsets->currentIndex == offsets->size) {
    *azoff = 0.0;
    *eloff = 0.0;
    *dkoff = 0.0;
    *active = false;
  } else {
    *azoff = offsets->cache[offsets->currentIndex].azoff;
    *eloff = offsets->cache[offsets->currentIndex].eloff;
    *dkoff = offsets->cache[offsets->currentIndex].dkoff;
    offsets->currentIndex++;
    *active = true;
  }
  return 0;
}
