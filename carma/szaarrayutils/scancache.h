#ifndef SCANCACHE_H
#define SCANCACHE_H

/*
 * Set the number of scan entries per cache.  This should be large
 * enough to read all points for any scan.
 */
#define SCAN_CACHE_SIZE 100

/**.......................................................................
 * A single entry from a file off scan offsets.
 */
typedef struct {
  unsigned long index; /* The integer counter of this point in the scan */
  double azoff;        /* The AZ offset (degrees) */
  double eloff;        /* The EL offset (degrees) */
  double dkoff;        /* The DK offset (degrees) */
} ScanCacheEntry;

/**.......................................................................
 * A struct for managing offsets read in from a scan offset file.
 */
typedef struct {
  ScanCacheEntry cache[SCAN_CACHE_SIZE];
  unsigned int size;
  unsigned currentIndex;
} ScanCache;

/**.......................................................................
 * Empty a cache.
 */
int empty_ScanCache(ScanCache* cache);

/**.......................................................................
 * Add an element to the current cache.
 */
int extend_ScanCache(ScanCache* cache, unsigned long index, double azoff, 
		     double eloff, double dkoff);

/**.......................................................................
 * Get the next set of offsets from the cache.
 */
int get_ScanCache(ScanCache* cache, double* azoff, double* eloff, 
		  double* dkoff, bool* active);

#endif
