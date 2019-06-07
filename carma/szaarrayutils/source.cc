#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <limits.h>

#include "carma/szaarrayutils/lprintf.h"

#include "carma/szaarrayutils/source.h"
#include "carma/szaarrayutils/freelist.h"
#include "carma/szaarrayutils/hash.h"
#include "carma/szaarrayutils/szaconst.h"
#include "carma/szaarrayutils/slalib.h"
#include "carma/szaarrayutils/astrom.h"
#include "carma/szaarrayutils/pathname.h"

#include "carma/szautil/Astrometry.h"
#include "carma/szautil/Axis.h"
#include "carma/szautil/HourAngle.h"
#include "carma/szautil/DecAngle.h"

#include "carma/szautil/Exception.h"

using namespace sza::array;
using namespace sza::util;

/*
 * Set the time between precession updates (decimal days).
 */
#define EPOCH_UPDATE_INTERVAL (1.0/24.0)     /* 1 hour */

/*
 * Set the number of UT1-UTC ephemeris entries per Ut1Utc ephemeris
 * free-list.
 */
#define UT1UTC_BLOCK_SIZE 100

/*
 * Set the number of planetary ephemeris entries per EphemSource
 * cache.
 */
#define EPHEM_CACHE_SIZE 100

/*
 * Set an upper limit on the number of symbolic links in a given alias
 * source path.
 */
#define ALIAS_MAX_LINKS 10

/*
 * Set the maximum number of iterations used to refine horizon
 * crossing times.
 */
#define MAX_HORIZON_ITER 5

static Source *new_J2000Source(SourceCatalog *sc, char *name,
			       double ra, double dec,
			       float pmra, float pmdec, float mag);
static Source *read_J2000Source(SourceCatalog *sc, InputStream *stream,
				char *name);
static Source *bad_J2000Source(InputStream *stream, char *name, char *column);

static int write_J2000Source(SourceCatalog *sc, OutputStream *stream,
			     Source *src);
static int J2000Source_info(SourceCatalog *sc, Site *site, J2000Source *j2000,
			    double horizon, unsigned options, SourceInfo *info);


static Source *read_EphemSource(SourceCatalog *sc, InputStream *stream,
				char *name);
static int write_EphemSource(SourceCatalog *sc, OutputStream *stream,
			     Source *src);
static Source *new_EphemSource(SourceCatalog *sc, char *name, char *file);
static int read_ephemeris(SourceCatalog *sc, EphemSource *ephem, double tt);
static int bad_ephemeris(InputStream *input, EphemSource *ephem);
static int read_ephem_entry(InputStream *input, double last_tt, EphemEntry *ee);
static int bad_ephem_entry(InputStream *stream, char *column);
static int ephem_range_cmp(EphemSource *ephem, int midpt, double tt);
static int find_ephem_entry(EphemSource *ephem, double tt);
static int set_ephem_quad(EphemSource *ephem, int midpt);
static int update_EphemSource(SourceCatalog *sc, EphemSource *ephem, double tt);
static int sc_get_ephem(SourceCatalog *sc, EphemSource *ephem, double tt,
			double *ra, double *dec, double *dist,
			double *pmra, double *pmdec);
static int EphemSource_info(SourceCatalog *sc, Site *site, EphemSource *ephem,
			    double horizon, unsigned options, SourceInfo *info);


static Source *new_FixedSource(SourceCatalog *sc, char *name,
			       unsigned axis_mask,
			       double az, double el, double dk);
static Source *read_FixedSource(SourceCatalog *sc, InputStream *stream,
				char *name);
static Source *bad_FixedSource(InputStream *stream, char *name, char *column);
static int write_FixedSource(SourceCatalog *sc, OutputStream *stream,
			    Source *src);
static int FixedSource_info(SourceCatalog *sc, Site *site, FixedSource *fixed,
			    double horizon, unsigned options, SourceInfo *info);


static Source *resolve_Aliases(Source *src);

static Source *new_AliasSource(SourceCatalog *sc, char *name, char *source);
static Source *read_AliasSource(SourceCatalog *sc, InputStream *stream,
				char *name);
static int write_AliasSource(SourceCatalog *sc, OutputStream *stream,
			     Source *src);

static Source *add_Source(SourceCatalog *sc, Source *src);
static Source *del_Source(SourceCatalog *sc, Source *src);
static void ini_SourceId(Source *src, SourceType type);
static int set_SourceName(Source *src, char *name);

/*
 * Associate enumerated source-types with the names that they are
 * known by in source-catalog files. SourceType enumerators index
 * into this array.
 */
static char *src_type_name[] = {
  "j2000",    /* SRC_J2000 */
  "ephem",    /* SRC_EPHEM */
  "fixed",    /* SRC_FIXED */
  "alias",    /* SRC_ALIAS */
};
static const int num_src_type = sizeof(src_type_name)/sizeof(src_type_name[0]);

/*
 * Set the number of hash-buckets in the source catalog hash table.
 * For best results this should be a prime number.
 */
#define SOURCE_HASH_SIZE  1531

/*
 * The array of catalog entries is split into lists of sub-arrays.
 * This allows for efficient expansion of the catalog by adding
 * segments as needed. Set the number of sources per sub-array.
 */
#define CAT_SEG_SIZE 256

typedef struct  CatalogSegment CatalogSegment;
struct CatalogSegment {
  CatalogSegment *next;      /* The next array segment */
  Source *s[CAT_SEG_SIZE];   /* The sub-array of sources */
  int nsource;               /* The number of sources in this segment */
};

static Source **get_source_slot(SourceCatalog *sc, int number);

typedef struct Ut1UtcEntry Ut1UtcEntry;

struct Ut1UtcEntry {
  double utc;         /* The UTC of this entry, expressed as a MJD */
  double ut1utc;      /* The value of UT1-UTC (seconds) */
};

/*
 * Encapsulate the UT1-UTC interpolator.
 */
typedef struct {
  char *file;         /* The pathname of the ephemeris file */
  Ut1UtcEntry *cache; /* A cache of size<=EPHEM_CACHE_SIZE entries */
  int size;           /* The number of elements in cache[] */
  int midpt;          /* The middle element of the interpolation triple */
  QuadPath *qp;       /* The quadratic interpolator of UT1-UTC */
} Ut1Utc;

static Ut1Utc *new_Ut1Utc(void);
static Ut1Utc *del_Ut1Utc(Ut1Utc *uu);
static int bad_ut1utc_file(Ut1Utc *uu);
static int read_ut1utc(SourceCatalog *sc, Ut1Utc *uu, double utc);
static int bad_ut1utc_ephemeris(InputStream *input, Ut1Utc *uu);
static int read_ut1utc_entry(InputStream *input, double last_utc,
			     Ut1UtcEntry *uue);
static int bad_ut1utc_entry(InputStream *input, char *column);
static int ut1utc_range_cmp(Ut1Utc *uu, int midpt, double utc);
static int find_ut1utc_entry(Ut1Utc *uu, double utc);
static int set_ut1utc_quad(Ut1Utc *uu, int midpt);
static int update_ut1utc(SourceCatalog *sc, Ut1Utc *uu, double utc);
static int sc_get_ut1(SourceCatalog *sc, double utc, double *ut1);
static int sc_get_tt(SourceCatalog *sc, double utc, double *tt);

/*
 * Encapsulate the equation-of-the-equinoxes interpolator.
 */
typedef struct {
  double dt;          /* The update interval */
  QuadData data;      /* The current three samples being interpolated */
  QuadPath *qp;       /* The quadratic interpolator */
} EqnEqx;

static EqnEqx *new_EqnEqx(double tt, double dt);
static EqnEqx *del_EqnEqx(EqnEqx *ee);
static EqnEqx *del_EqnEqx(EqnEqx *ee);
static int update_eqneqx(EqnEqx *ee, double tt);
static int sc_get_eqneqx(SourceCatalog *sc, double tt, double *eqneqx);

static int sc_get_lst(SourceCatalog *sc, Site *site, double utc, double *lst);

/*
 * Encapsulate the cache of position-independent precession+nutation
 * parameters.
 */
typedef struct {
  double epoch;           /* The Julian epoch of the mean equinox of the */
                          /*  objects to be precessed */
  double dt;              /* The update interval */
  double tmin,tmax;       /* The time window (TT as a MJD) for which */
                          /*  mappa[] is a good approximation */
  double mappa[21];       /* Parameters returned by slaMappa() */
} MapqkCache;

static MapqkCache *new_MapqkCache(double epoch, double tt, double dt);
static MapqkCache *del_MapqkCache(MapqkCache *mc);
static int update_mapqk_cache(MapqkCache *mc, double tt);

/*
 * Define a source catalog container. This is typedef'd to
 * SourceCatalog in source.h.
 */
struct SourceCatalog {
  InputStream *ephem_input;  /* This is used for reading ephemeris files */
  FreeList *j2000_mem;       /* Memory for ra/dec sources */
  FreeList *ephem_mem;       /* Memory for ephemeris sources */
  FreeList *fixed_mem;       /* Memory for az/el sources */
  FreeList *alias_mem;       /* Memory for alias sources */
  CatalogSegment *head;      /* The head of a list of catalog segments */
  HashTable *hash;           /* A symbol-table of the catalog sources */
  int nsource;               /* The total number of sources in the catalog */
  Ut1Utc *ut1utc;            /* The UT1-UTC ephemeris */
  EqnEqx *eqneqx;            /* The equation-of-the-equinoxes interpolator */
  MapqkCache *mapqk;         /* The cache of precession+nutation parameters */
};

static void horizon_to_equatorial(double lst, Site *site,
		  double sin_el, double cos_el, double sin_az, double cos_az,
		  double *ra, double *dec);
static void equatorial_to_horizon(Site *site, double dist,
				  double pmra, double pmdec,
				  double sin_ha, double cos_ha,
				  double sin_dec, double cos_dec,
				  SourceAzElPa *coord, SourceAzElPa *rates);
static int ra_dec_visibility(SourceCatalog *sc, Site *site, Source *src,
			     double horizon, SourceInfo *info);
static int sc_refine_crossing_time(SourceCatalog *sc, Site *site, Source *src,
				   int do_rise, double horizon,
				   SourceInfo *info);

/*.......................................................................
 * Add a J2000 source to a source catalog.
 *
 * Input:
 *  sc   SourceCatalog *   The source catalog to which to add the source.
 *  name          char *   The name of the source.
 *  ra          double     The right-ascension of the source in radians
 *                         wrt epoch J2000.0.
 *  dec         double     The declination of the source in radians
 *                         wrt epoch J2000.0.
 *  pmra         float     The proper motion of the Right Ascension (radians).
 *  pmdec        float     The proper motion of the Declination (radians).
 * Output:
 *  return      Source *   The new source, or NULL on error.
 */
Source *add_J2000Source(SourceCatalog *sc, char *name, double ra, double dec,
			       float pmra, float pmdec, float mag)
{
/*
 * Allocate the source.
 */
  Source *src = new_J2000Source(sc, name, ra, dec, pmra, pmdec, mag);
  if(!src)
    return NULL;
/*
 * Add the source to the catalog.
 */
  return add_Source(sc, src);
}

/*.......................................................................
 * Add an ephemeris source to a source catalog.
 *
 * Input:
 *  sc   SourceCatalog *   The source catalog to which to add the source.
 *  name          char *   The name of the source.
 *  file          char *   The name of the ephemeris file.
 * Output:
 *  return      Source *   The new source, or NULL on error.
 */
Source *add_EphemSource(SourceCatalog *sc, char *name, char *file)
{
  Source *src;  /* The new source */
/*
 * Create the source and initialize it from the ephemeris file.
 */
  src = new_EphemSource(sc, name, file);
/*
 * Add the source to the catalog.
 */
  return src ? add_Source(sc, src) : NULL;
}

/*.......................................................................
 * Add a fixed-position source to a source catalog.
 *
 * Input:
 *  sc   SourceCatalog *   The source catalog to which to add the source.
 *  name          char *   The name of the source.
 *  axis_mask unsigned     A bitwise union of source.h::SourceAxes enumerators
 *                         specifying which of the following axes are
 *                         pertinent to this source.
 *  az          double     The desired azimuth (radians).
 *  el          double     The desired elevation (radians).
 *  dk          double     The desired deck angle (radians).
 * Output:
 *  return      Source *   The new source, or NULL on error.
 */
Source *add_FixedSource(SourceCatalog *sc, char *name, unsigned axis_mask,
			double az, double el, double dk)
{
/*
 * Create the source.
 */
  Source *src = new_FixedSource(sc, name, axis_mask, az, el, dk);
  if(!src)
    return NULL;
/*
 * Add the source to the catalog.
 */
  return add_Source(sc, src);
}

/*.......................................................................
 * Add an alias source to a source catalog.
 *
 * Input:
 *  sc   SourceCatalog *   The source catalog to which to add the source.
 *  name          char *   The name of the source.
 *  source        char *   The name of the source that is to be used
 *                         whenever this source is referred to.
 * Output:
 *  return      Source *   The new source, or NULL on error.
 */
Source *add_AliasSource(SourceCatalog *sc, char *name, char *source)
{
/*
 * Create the source.
 */
  Source *src = new_AliasSource(sc, name, source);
  if(!src)
    return NULL;
/*
 * Add the source to the catalog.
 */
  return add_Source(sc, src);
}

/*.......................................................................
 * Add a source to a source catalog. If a source of the same name already
 * exists, replace it at the same catalog number.
 *
 * Input:
 *  sc   SourceCatalog *  The catalog to add the source to.
 *  src         Source *  The source to be added.
 * Output:
 *  return      Source *  The successfully added source or NULL on
 *                        error.
 */
static Source *add_Source(SourceCatalog *sc, Source *src)
{
  CatalogSegment *seg;  /* The catalog segment to add the source to */
  Symbol *sym;          /* A source-catalog hash table entry */
  int i;
/*
 * Check the header of the source.
 */
  if(src->id.number != -1) {
    lprintf(stderr, "add_Source(%s): Source already in catalog.\n",
	    src->id.name);
    return NULL;
  };
/*
 * If a source of the same name already exists simply replace it.
 */
  sym = find_HashSymbol(sc->hash, src->id.name);
  if(sym) {
    src->id.number = ((Source *)sym->data)->id.number;
    (void) del_Source(sc, (Source* )sym->data);
    sym->data = src;
    *get_source_slot(sc, src->id.number) = src;
    return src;
  };
/*
 * Find the last block of sources in the source catalog.
 */
  if(sc->head) {
    for(seg=sc->head; seg->next; seg=seg->next)
      ;
  } else {
    seg = NULL;
  };
/*
 * If the last block is full, allocate a new one.
 */
  if(!seg || seg->nsource >= CAT_SEG_SIZE) {
    CatalogSegment *nseg;  /* The new segment */
/*
 * Attempt to allocate a new catalog segment.
 */
    nseg = (CatalogSegment* )malloc(sizeof(CatalogSegment));
    if(!nseg) {
      lprintf(stderr, "add_Source: Insufficient memory.\n");
      return del_Source(sc, src);
    };
/*
 * Initialize the new segment.
 */
    nseg->next = NULL;
    nseg->nsource = 0;
    for(i=0; i<CAT_SEG_SIZE; i++)
      nseg->s[i] = NULL;
/*
 * Append the new catalog segment to the list.
 */
    if(!seg)
      sc->head = nseg;
    else
      seg->next = nseg;
/*
 * Record the new end-segment for use below.
 */
    seg = nseg;
  };
/*
 * Add the source-list entry to the hash-table of the catalog.
 */
  if(!new_HashSymbol(sc->hash, src->id.name, 0, 0, src, 0))
    return del_Source(sc, src);
/*
 * Assign the source to the next free element of the array of sources.
 */
  seg->s[seg->nsource++] = src;
  src->id.number = sc->nsource++;
  return src;
}

/*.......................................................................
 * Allocate a new J2000 source.
 *
 * Input:
 *  sc   SourceCatalog *   The source catalog to which to add the source.
 *  name          char *   The name of the source.
 *  ra          double     The right-ascension of the source in radians
 *                         wrt epoch J2000.0.
 *  dec         double     The declination of the source in radians
 *                         wrt epoch J2000.0.
 *  pmra         float     The proper motion of the Right Ascension (radians).
 *  pmdec        float     The proper motion of the Declination (radians).
 * Output:
 *  return      Source *   The new source, or NULL on error.
 */
static Source *new_J2000Source(SourceCatalog *sc, char *name,
			       double ra, double dec,
			       float pmra, float pmdec, float mag)
{
  Source *src;   /* The object to be returned */
  
  // Check the arguments.

  if(ra < 0.0 || ra > twopi) {
    lprintf(stderr, "The Right Ascension of source %s is invalid.\n", name);
    return NULL;
  };
  if(dec < -halfpi || dec > halfpi) {
    lprintf(stderr, "The Declination of source %s is invalid.\n", name);
    return NULL;
  };
  
  // Allocate the container.

  src = (Source* )new_FreeListNode("new_J2000Source", sc->j2000_mem);
  if(!src)
    return NULL;
  
  // Before attempting any opertion that might fail, initialize the
  // source at least up to the point at which it can safely be passed
  // to del_Source().

  ini_SourceId(src, SRC_J2000);
  src->j2000.ra = ra;
  src->j2000.dec = dec;
  src->j2000.pmra = pmra;
  src->j2000.pmdec = pmdec;

  src->j2000.mag = mag;
  
  // Domain check and record the name of the source.

  if(set_SourceName(src, name))
    return del_Source(sc, src);

  return src;
}

/*.......................................................................
 * Allocate a new az,el,dk source.
 *
 * Input:
 *  sc   SourceCatalog *   The source catalog to which to add the source.
 *  name          char *   The name of the source.
 *  axis_mask unsigned     A bitwise union of source.h::SourceAxes enumerators
 *                         specifying which of the following axes are
 *                         pertinent to this source.
 *  az          double     The desired azimuth (radians 0..2*pi).
 *  el          double     The desired elevation (radians 0..pi/2).
 *  dk          double     The desired deck angle (radians -pi..pi).
 * Output:
 *  return      Source *   The new source, or NULL on error.
 */
static Source *new_FixedSource(SourceCatalog *sc, char *name,
			       unsigned axis_mask,
			       double az, double el, double dk)

{
  Source *src;   /* The object to be returned */
  
  // Check the arguments.

  if(axis_mask & Axis::AZ && (az < -twopi || az > twopi)) {
    lprintf(stderr, "The azimuth of source \"%s\" is invalid.\n", name);
    return NULL;
  };
  if(axis_mask & Axis::EL && (el < -halfpi || el > halfpi)) {
    lprintf(stderr, "The elevation of source \"%s\" is invalid.\n", name);
    return NULL;
  };
  if(axis_mask & Axis::PA && (dk < -twopi || dk > twopi)) {
    lprintf(stderr, "The deck angle of source \"%s\" is invalid.\n",name);
    return NULL;
  };
/*
 * Allocate the container.
 */
  src = (Source* )new_FreeListNode("new_FixedSource", sc->fixed_mem);
/*
 * Before attempting any opertion that might fail, initialize the
 * source at least up to the point at which it can safely be passed
 * to del_Source().
 */
  ini_SourceId(src, SRC_FIXED);
  src->fixed.axis_mask = axis_mask;
  src->fixed.az = axis_mask & Axis::AZ ? az : 0.0;
  src->fixed.el = axis_mask & Axis::EL ? el : 0.0;
  src->fixed.dk = axis_mask & Axis::PA ? dk : 0.0;
/*
 * Domain check and record the name of the source.
 */
  if(set_SourceName(src, name))
    return del_Source(sc, src);
  return src;
}

/*.......................................................................
 * Allocate a new alias source.
 *
 * Input:
 *  sc   SourceCatalog *   The source catalog to which to add the source.
 *  name          char *   The name of the source.
 *  source        char *   The name of the source that is to be used
 *                         whenever this source is referred to.
 * Output:
 *  return      Source *   The new source, or NULL on error.
 */
static Source *new_AliasSource(SourceCatalog *sc, char *name, char *source)

{
  Source *src;          /* The object to be returned */
  Source *old;          /* The source to be aliased by src */
/*
 * Check the arguments.
 */
  if(!sc || !name || !source) {
    lprintf(stderr, "new_AliasSource: NULL argument(s).\n");
    return NULL;
  };
/*
 * Lookup the existing source.
 */
  old = find_SourceByName(sc, source);
  if(!old) {
    lprintf(stderr, "new_AliasSource: Source \"%s\" not in catalog.\n", source);
    return NULL;
  };
/*
 * Allocate the container.
 */
  src = (Source* )new_FreeListNode("new_AliasSource", sc->alias_mem);
/*
 * Before attempting any opertion that might fail, initialize the
 * source at least up to the point at which it can safely be passed
 * to del_Source().
 */
  ini_SourceId(src, SRC_ALIAS);
  src->alias.sptr = get_source_slot(sc, old->id.number);
/*
 * Domain check and record the name of the source.
 */
  if(set_SourceName(src, name))
    return del_Source(sc, src);
  return src;
}


/*.......................................................................
 * Allocate a new ephemeris source.
 *
 * Input:
 *  sc   SourceCatalog *   The source catalog to which to add the source.
 *  name          char *   The name of the source.
 *  file          char *   The path name of the ephemeris file.
 * Output:
 *  return      Source *   The new source, or NULL on error.
 */
static Source *new_EphemSource(SourceCatalog *sc, char *name, char *file)
{
  Source *src;        /* The object to be returned */
  EphemSource *ephem; /* A pointer to src->ephem */
  double tt;          /* The current Terrestrial Time */
  int i;
/*
 * Check the arguments.
 */
  if(!name || !file) {
    lprintf(stderr, "new_EphemSource: NULL argument(s).\n");
    return NULL;
  };
/*
 * Allocate the container.
 */
  src = (Source* )new_FreeListNode("new_EphemSource", sc->ephem_mem);
  if(!src)
    return NULL;
/*
 * Before attempting any opertion that might fail, initialize the
 * source at least up to the point at which it can safely be passed
 * to del_Source().
 */
  ini_SourceId(src, SRC_EPHEM);
  src->ephem.file = NULL;
  src->ephem.cache = NULL;
  src->ephem.size = 0;
  src->ephem.midpt = -1;
  src->ephem.ra = NULL;
  src->ephem.dec = NULL;
  src->ephem.dist = NULL;
/*
 * Domain check and record the name of the source.
 */
  if(set_SourceName(src, name))
    return del_Source(sc, src);
/*
 * Get a pointer to the pertinent source member.
 */
  ephem = &src->ephem;
/*
 * Allocate a copy of the file name.
 */
  ephem->file = (char *)malloc(strlen(file) + 1);
  if(!ephem->file) {
    lprintf(stderr,
	    "new_EphemSource(%s): Insufficient memory for file name.\n", name);
    return del_Source(sc, src);
  };
  strcpy(ephem->file, file);
  /*
   * Allocate the cache array of ephemeris entries.
   */
  ephem->cache = (EphemEntry* )malloc(sizeof(EphemEntry) * EPHEM_CACHE_SIZE);
  if(!ephem->cache)
    return del_Source(sc, src);
/*
 * Clear the cache entries.
 */
  for(i=0; i<EPHEM_CACHE_SIZE; i++) {
    EphemEntry *ee = ephem->cache + i;
    ee->ra = ee->dec = ee->dist = ee->tt = 0.0;
  };
/*
 * Allocate interpolators for ra,dec and source distance.
 */
  if(!(ephem->ra = new_QuadPath(0.0, QP_POSITIVE_ANGLE)) ||
     !(ephem->dec = new_QuadPath(0.0, QP_SIGNED_ANGLE)) ||
     !(ephem->dist = new_QuadPath(0.0, QP_NORMAL)))
    return del_Source(sc, src);
/*
 * Get the current Terrestrial Time.
 */
  tt = current_mjd_tt();
  if(tt < 0.0)
    return NULL;
/*
 * Read the ephemeris of the source.
 */
  if(read_ephemeris(sc, ephem, tt))
    return del_Source(sc, src);
  return src;
}

/*.......................................................................
 * Read up to EPHEM_CACHE_SIZE entries of a planetary ephemeris file
 * into memory.
 *
 * Each non-blank line of the ephemeris must contain the following
 * fields.
 *
 *   tt ra dec dist
 *
 * Where tt is the Terrestrial Time at which the entry takes effect,
 * ra and is the geocentric Right Ascension, dec is the geocentric
 * Declination, and dist is the geocentric distance of the source.
 *
 * The Right Ascension is specified either in sexagesimal, as
 * hh:mm:ss.sss, or as a floating point number of hours.
 *
 * The Declination is specified either in sexagesimal, as
 * +/-ddd:mm:ss.sss, or as a floating point number of degrees.
 *
 * Comments can appear on any line, starting with a # character and
 * extending to the end of the line.
 *
 * Input:
 *  sc  SourceCatalog *  The parent catalog of the source.
 *  ephem EphemSource *  The source to update.
 *  input InputStream *  The stream from which to read the ephemeris.
 *  tt         double    The time for which the ephemeris is needed.
 *                       This is a Terestrial Time, expressed as a MJD.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int read_ephemeris(SourceCatalog *sc, EphemSource *ephem, double tt)

{
  InputStream *input;       /* A local alias of sc->ephem_input */
  int nrec;                 /* The number of entries read so far */
  EphemEntry *cache;        /* A local alias of sc->cache */
  int update;               /* True if this is an update */
/*
 * Are we updating an existing cache?
 */
  update = ephem->size != 0;
/*
 * Clear the current ephemeris cache.
 */
  ephem->size = 0;
  ephem->midpt = -1;
/*
 * Open the ephemeris file.
 */
  input = sc->ephem_input;
  if(open_FileInputStream(input, "", ephem->file))
    return bad_ephemeris(NULL, ephem);
  lprintf(stdout, "%s %s ephemeris from %s\n",
	  update ? "Updating":"Initializing",
	  ephem->id.name, ephem->file);
/*
 * Get a pointer to the start of the cache buffer.
 */
  cache = ephem->cache;
/*
 * Find the ephemeris entry of the file.
 */
  if(input_skip_white(input, 1, 0))
    return bad_ephemeris(input, ephem);
/*
 * Read the first two entries into the cache. The reason for
 * this will become apparent shortly.
 */
  if(read_ephem_entry(input, 0.0, &cache[0]) ||
     read_ephem_entry(input, cache[0].tt, &cache[1]))
    return bad_ephemeris(input, ephem);
/*
 * The ephemeris now contains 2 entries.
 */
  ephem->size = 2;
/*
 * Read in sequential blocks of EPHEM_CACHE_SIZE ephemeris entries
 * until a block that contains three records that can be used to
 * interpolate for time tt is found. Since the three records being
 * sought may straddle the boundary of one of these blocks, carry over
 * the last two entries of each block to the start of the next block.
 */
  do {
/*
 * Copy the oldest two entries of the last block to the start of the
 * next block (this is why two entries are read before entering this
 * loop).
 */
    if(ephem->size > 2) {
      cache[0] = cache[ephem->size-2];
      cache[1] = cache[ephem->size-1];
    };
/*
 * Read further entries into the cache until either EPHEM_CACHE_SIZE
 * entries have been read or the end of the file is reached.
 */
    for(nrec=2; nrec<EPHEM_CACHE_SIZE && input->nextc!=EOF; nrec++) {
      EphemEntry *ee = cache + nrec;
      if(read_ephem_entry(input, ee[-1].tt, ee))
	return bad_ephemeris(input, ephem);
    };
/*
 * Record the number of entries now in the cache.
 */
    ephem->size = nrec;
/*
 * If we reached the end of the file, or we passed three entries
 * that can be used to interpolate for tt, stop reading.
 */
  } while(input->nextc != EOF && ephem_range_cmp(ephem, nrec-2, tt) < 0);
/*
 * Close the ephemeris file.
 */
  close_InputStream(input);
/*
 * If there are three or more entries in the newly read cache,
 * search for three neighboring entries in the cache that are suitable
 * for quadratic interpolation for tt. If found, install the three
 * entries in the quadratic interpolators for ra, dec and distance.
 */
  if(nrec >= 3 && (ephem->midpt=find_ephem_entry(ephem, tt)) >= 0) {
    set_ephem_quad(ephem, ephem->midpt);
  } else {
    lprintf(stderr, "read_ephemeris(%s): TT=%g not covered by ephemeris.\n",
	    ephem->id.name, tt);
    return bad_ephemeris(NULL, ephem);
  };
  return 0;
}

/*.......................................................................
 * This is the private error return function of read_ephemeris().
 *
 * Input:
 *  input  InputStream *  The stream that is connected to the ephemeris
 *                        file (NULL if not yet opened).
 *  ephem  EphemSource *  The incomplete ephemeris.
 * Output:
 *  return         int    1 (the error return code of read_ephemeris()).
 */
static int bad_ephemeris(InputStream *input, EphemSource *ephem)
{
/*
 * Close the ephemeris file.
 */
  if(input)
    close_InputStream(input);
/*
 * Discard the incomplete ephemeris.
 */
  ephem->size = 0;
  ephem->midpt = -1;
/*
 * Return the error-return code of read_ephemeris().
 */
  return 1;
}

/*.......................................................................
 * Read one entry from a planetary ephemeris. Note that the stream pointer
 * must already point at the first non-white-space character of the record.
 * On successful returns the stream pointer will be left pointing at the
 * first non-white-space character of the next record.
 *
 * Input:
 *  input  InputStream *  The stream from which to read the entry.
 *  last_tt     double    The TT of the last entry read from the stream.
 * Input/Output:
 *  ee      EphemEntry *  The place to record the contents of the entry.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int read_ephem_entry(InputStream *input, double last_tt, EphemEntry *ee)
{
/*
 * Read the next record.
 */
  if(input_double(input, 0, &ee->tt))
    return bad_ephem_entry(input, "MJD Terrestrial Time");
  if(input_skip_space(input, 1, 0))
    return 1;
  if(input_sexagesimal(input, 0, &ee->ra))
    return bad_ephem_entry(input, "sexagesimal Right Ascension");
  if(input_skip_space(input, 1, 0))
    return 1;
  if(input_sexagesimal(input, 0, &ee->dec))
    return bad_ephem_entry(input, "sexagesimal Declination");
  if(input_skip_space(input, 1, 0))
    return 1;
  if(input_double(input, 0, &ee->dist))
    return bad_ephem_entry(input, "Distance");
  if(input_skip_white(input, 1, 0))
    return 1;
/*
 * The ephemeris is supposed to be in ascending order of time.
 */
  if(ee->tt < last_tt)
    return input_error(input, 1, "The ephemeris is not in time order.\n");
/*
 * Convert the Right Ascension and Declination values to radians.
 */
  ee->ra *= htor;
  ee->dec *= dtor;
  return 0;
}

/*.......................................................................
 * This is a private syntax-error return function for read_ephem_entry().
 *
 * Input:
 *  input   InputStream *  The stream in which the syntax error was found.
 *  column         char *  The description of the bad element.
 * Output:
 *  return          int    Always 1.
 */
static int bad_ephem_entry(InputStream *input, char *column)
{
  input_error(input, 1, "Bad %s ephemeris field.\n", column);
  input_error(input, 1, "Use: mjd_tt hh:mm:ss.s +dd:mm:ss.s distance\n");
  return 1;
}

/*.......................................................................
 * Compare the interpolation coverage of three neighboring ephemeris
 * entries to a given time.
 *
 * Input:
 *  ephem  EphemSource *  The source being checked.
 *  midpt          int    The cache index of the middle entry of
 *                        the three entries to compare. Note that it
 *                        is ok for this to be 0 or ephem->size-1, but
 *                        only -1 or 1 will ever be returned in this
 *                        case because the entry doesn't have two neighbors.
 *  tt          double    The time that an interpolation triple is
 *                        needed for (Terestrial Time expressed as a MJD).
 * Output:
 *  return         int    -1 - The ephemeris entries are suitable for
 *                             earlier times than tt.
 *                         0 - The ephemeris entries are suitable for
 *                             interpolation of tt.
 *  return         int     1 - The ephemeris entries are suitable for
 *                             later times than tt.
 */
static int ephem_range_cmp(EphemSource *ephem, int midpt, double tt)
{
/*
 * If the entry or the neigbors of the entry being queried are just beyond
 * the lower limit of the ephemeris, extrapolate whether the entry is
 * too early or two late by looking at the first entry in the ephemeris
 * that does have two neigbors.
 */
  if(midpt <= 0) {
    return ephem_range_cmp(ephem, 1, tt) <= 0 ? -1 : 1;
/*
 * If the entry or the neigbors of the entry being queried are just beyond
 * the upper limit of the ephemeris, extrapolate whether the entry is
 * too early or two late by looking at the last entry in the ephemeris
 * that does have two neigbors. 
 */
  } else if(midpt >= ephem->size-1) {
    return ephem_range_cmp(ephem, ephem->size-2, tt) < 0 ? -1 : 1;
/*
 * The entry has two neigbors in the bounds of the ephemeris.
 */
  } else {
    EphemEntry *ee = ephem->cache + midpt;  /* The entry being queried */
/*
 * The entry is suitable for interpolation of time tt if tt is within
 * half the interval between the entry's timestamp and that of each of
 * its neigbors. Compute the limits of this window.
 */
    double tmin = (ee[-1].tt + ee->tt)/2.0;
    double tmax = (ee->tt + ee[1].tt)/2.0;
/*
 * Compare the window to the requested time.
 */
    if(tmax < tt)
      return -1;
    if(tmin > tt)
      return 1;
    return 0;
  };
}

/*.......................................................................
 * Perform a binary search of the ephemeris cache of a given source to
 * locate three neighboring ephemeris entries that are suitable for
 * interpolating for a given time.
 *
 * Input:
 *  ephem   EphemSource *  The source who's ephemeris is to be searched.
 *  tt           double    The Terestrial Time to search for (MJD).
 * Output:
 *  return          int    The cache index of the middle of the three
 *                         ephemeris entries to search, or -1 if not found.
 */
static int find_ephem_entry(EphemSource *ephem, double tt)
{
  int mid = -1;                     /* The index at which to bisect the cache */
/*
 * Note that if the ephemeris is active, there are guaranteed to be at least
 * three entries in the cache. Also note that the first and last entries aren't
 * suitable mid points for interpolation because they only have one neighbor.
 */
  int below = 1;                    /* The lowest index below mid to check */
  int above = ephem->size - 2;      /* The highest index above mid to check */
/*
 * If the ephemeris is not active, return failure.
 */
  if(ephem->size < 3)
    return -1;
/*
 * Perform a binary search for the middle entry that supports interpolation
 * of time tt.
 */
  while(above >= below) {
    mid = (above + below)/2;
    switch(ephem_range_cmp(ephem, mid, tt)) {
    case -1:
      below = mid+1;
      break;
    case 0:
      return mid;
      break;
    case 1:
      above = mid-1;
      break;
    };
  };
  return -1;
}

/*.......................................................................
 * Set the current triple of a planetary ephemeris interpolator.
 *
 * Input:
 *  ephem EphemSource *  The container of the ephemeris.
 *  midpt         int    The cache index of the middle entry of
 *                       the three neighboring entries.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int set_ephem_quad(EphemSource *ephem, int midpt)
{
  EphemEntry *ee;  /* The ephemeris entry corresponding to 'midpt' */
  QuadData data;   /* The QuadPath initialization object */
/*
 * Make sure that the specified entry and its neighbors lie within the
 * bounds of the cache.
 */
  if(midpt <= 0 || midpt >= ephem->size-1) {
    lprintf(stderr, "set_ephem_quad: Index out of range.\n");
    return 1;
  };
/*
 * Record the new mid point.
 */
  ephem->midpt = midpt;
/*
 * Get a pointer to the cache element that corresponds to midpt.
 */
  ee = ephem->cache + midpt;
/*
 * Fill aspects of the interpolator initialization object that are
 * common to all of the ra,dec,dist interpolators.
 */
  data.npt = 3;
  data.s[0].x = ee[-1].tt;
  data.s[1].x = ee->tt;
  data.s[2].x = ee[+1].tt;
/*
 * Record the Right Ascension values.
 */
  data.s[0].y = ee[-1].ra;
  data.s[1].y = ee->ra;
  data.s[2].y = ee[+1].ra;
  if(set_QuadPath(ephem->ra, &data))
    return 1;
/*
 * Record the Declination values.
 */
  data.s[0].y = ee[-1].dec;
  data.s[1].y = ee->dec;
  data.s[2].y = ee[+1].dec;
  if(set_QuadPath(ephem->dec, &data))
    return 1;
/*
 * Record the ra values.
 */
  data.s[0].y = ee[-1].dist;
  data.s[1].y = ee->dist;
  data.s[2].y = ee[+1].dist;
  if(set_QuadPath(ephem->dist, &data))
    return 1;
  return 0;
}

/*.......................................................................
 * Given an ephemeris source, return the time window over which
 * its 3 ephemeris interpolation entries for the time 'tt' are usable.
 *
 * Input:
 *  sc     SourceCatalog *   The parent source catalog.
 *  src           Source *   The ephemeris source to check.
 *  tt            double     The Terrestrial Time whose enclosing
 *                           window is to be returned (expressed as a MJD).
 *                           If you send -1, the current time will be used.
 * Input/Output:
 *  win      CacheWindow *   The window of times over which the cache
 *                           is valid. The times are in TT.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Bad source or unable to update ephemeris.
 */
int get_ephem_window(SourceCatalog *sc, Source *src, double tt,
		     CacheWindow *win)
{
  EphemSource *ephem;/* The ephemeris source */
  EphemEntry *ee;    /* The middle ephemeris entry of the interpolator triple */
  
  // Check arguments.

  if(!sc || !src || !win) {
    lprintf(stderr, "get_ephem_window: NULL argument(s).\n");
    return 1;
  };
  
  // Resolve source aliases.

  src = resolve_Aliases(src);
  if(!src)
    return 1;
  
  // Check that the source is an ephemeris source.

  if(src->id.type != SRC_EPHEM) {
    lprintf(stderr, "get_ephem_window: \"%s\" is not an ephemeris source.\n",
	    src->id.name);
    return 1;
  };
  
  // Substitute the current time?

  if(tt < 0.0 && (tt = current_mjd_tt()) < 0.0)
    return 1;
  
  // Get the type-specific container of the source.

  ephem = &src->ephem;
  
  // Update the source ephemeris caches if necessary.

  if(update_EphemSource(sc, ephem, tt))
    return 1;
  
  // Get the middle entry of the three UT1-UTC cache entries that has
  // been chosen to interpolate for time 'tt'.

  ee = ephem->cache + ephem->midpt;
  
  // Calculate the time limits of the interpolation entries.

  win->tmin = (ee[-1].tt + ee->tt)/2.0;
  win->tmax = (ee->tt + ee[1].tt)/2.0;

  return 0;
}

/*.......................................................................
 * Return the contents of the quadratic interpolator for a specified
 * ephemeris source, valid for the time 'tt'.
 *
 * Input:
 *  sc     SourceCatalog *   The parent source catalog.
 *  src           Source *   The ephemeris source to query.
 *  tt            double     The Terrestrial Time for which the
 *                           interpolator is to be valid (expressed as a MJD).
 *                           The current time will be substituted if tt<0.0.
 * Input/Output:
 *  ra          QuadData *   The contents of the Right Ascension quadratic
 *                           interpolator contents will be assigned to *ra.
 *                           The x axis of the interpolator is tt as a MJD,
 *                           The y axis is the Right Ascension in radians.
 *                           (QuadData is defined in quad.h).
 *  dec         QuadData *   The contents of the Declination quadratic
 *                           interpolator contents will be assigned to *dec.
 *                           The x axis of the interpolator is tt as a MJD,
 *                           The y axis is the Declination in radians.
 *  dist        QuadData *   The contents of the distance quadratic
 *                           interpolator contents will be assigned to *dist.
 *                           The x axis of the interpolator is tt as a MJD,
 *                           The y axis is the distance in AU.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Failed to update cache, bad arguments,
 *                               or the source isn't an ephemeris source.
 */
int get_ephem_QuadData(SourceCatalog *sc, Source *src, double tt,
		       QuadData *ra, QuadData *dec, QuadData *dist)
{
  EphemSource *ephem;  /* The type-specific derivation of 'src' */
/*
 * Check arguments.
 */
  if(!sc || !src || !ra || !dec || !dist) {
    lprintf(stderr, "get_ephem_QuadData: NULL argument(s).\n");
    return 1;
  };
/*
 * Resolve source aliases.
 */
  src = resolve_Aliases(src);
  if(!src)
    return 1;
/*
 * Check that the source is an ephemeris source.
 */
  if(src->id.type != SRC_EPHEM) {
    lprintf(stderr, "get_ephem_QuadData: \"%s\" is not an ephemeris source.\n",
	    src->id.name);
    return 1;
  };
/*
 * Substitute the current time?
 */
  if(tt < 0.0 && (tt = current_mjd_tt()) < 0.0)
    return 1;
/*
 * Get the type-specific container of the source.
 */
  ephem = &src->ephem;
/*
 * Update the interpolation cache if necessary.
 */
  if(update_EphemSource(sc, ephem, tt))
    return 1;
/*
 * Return the interpolator contents.
 */
  return get_QuadPath(ephem->ra, ra) ||
         get_QuadPath(ephem->dec, dec) ||
         get_QuadPath(ephem->dist, dist);
}

/*.......................................................................
 * If needed, update a planetary ephemeris interpolator for a given time.
 *
 * Input:
 *  sc  SourceCatalog *  The parent catalog of the source.
 *  ephem EphemSource *  The interpolator to update.
 *  tt         double    The time at which the interpolation is needed.
 *                       (Terestrial Time as a MJD).
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int update_EphemSource(SourceCatalog *sc, EphemSource *ephem, double tt)
{
/*
 * Check the arguments.
 */
  if(!sc || !ephem) {
    lprintf(stderr, "update_EphemSource: Invalid argument(s).\n");
    return 1;
  };
/*
 * Substitute the current terrestrial time?
 */
  if(tt < 0.0 && (tt = current_mjd_tt()) < 0.0)
    return 1;
/*
 * Does the interpolator need updating?
 */
  if(ephem_range_cmp(ephem, ephem->midpt, tt) != 0) {
/*
 * Attempt to find cached ephemeris entries that can be used to
 * interpolate for tt.
 */
    int midpt = find_ephem_entry(ephem, tt);
/*
 * If suitable entries couldn't be found then refresh the cache from
 * the source's ephemeris file.
 */
    if(midpt < 0)
      return read_ephemeris(sc, ephem, tt);
/*
 * Initialize the interpolator to use the new midpt entry as its central
 * node.
 */
    if(set_ephem_quad(ephem, midpt))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Return the geocentric apparent Right Ascension, Declination and
 * distance of an ephemeris source.
 *
 * Input:
 *  sc   SourceCatalog * The parent source catalog of the UT1-UTC ephemeris.
 *  ephem  EphemSource * The source to query.
 *  tt          double   The target terrestrial time, expressed as a Modified
 *                       Julian Date.
 * Input/Output:
 *  ra,dec,dist double * On output *ra,*dec,*dist will be assigned with
 *                       the interpolated Right Ascension, Declination
 *                       and the distance of the source.
 *  pmra,pmdec double  * On output the proper motion in R.A. and Dec
 *                       (radians/sec) will be assigned to *pmra and
 *                       *pmdec.
 * Output:
 *  return        int   0 - OK.
 *                      1 - Error.
 */
static int sc_get_ephem(SourceCatalog *sc, EphemSource *ephem, double tt,
			double *ra, double *dec, double *dist,
			double *pmra, double *pmdec)
{
  if(!sc || !ra || !dec || !dist) {
    lprintf(stderr, "sc_get_ephem: NULL argument(s).\n");
    return 1;
  };
/*
 * Ensure that the interpolator is correctly positioned.
 */
  if(update_EphemSource(sc, ephem, tt))
    return 1;
  *ra = eval_QuadPath(ephem->ra, tt);
  *dec = eval_QuadPath(ephem->dec, tt);
  *dist = eval_QuadPath(ephem->dist, tt);
/*
 * Estimate the proper motion of the source.
 */
  *pmra = grad_QuadPath(ephem->ra, tt) / daysec;
  *pmdec = grad_QuadPath(ephem->dec, tt) / daysec;
  return 0;
}

/*.......................................................................
 * Initialize the generic source header of a source. This is intended
 * for use by source constructor functions.
 *
 * Input:
 *  src     Source *  The source whose header is to be initialized.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
static void ini_SourceId(Source *src, SourceType type)
{
  src->id.type = type;
  src->id.name[0] = '\0';
  src->id.number = -1;
}

/*.......................................................................
 * Check and record the name of a source.
 *
 * Input:
 *  src       Source *  The source whose name is to be set.
 *  name        char *  The name of the source. This must have <=
 *                      SRC_NAME_MAX characters (including the '\0').
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */
static int set_SourceName(Source *src, char *name)
{
  char *cptr;  /* A pointer into name[] */
/*
 * Check the arguments.
 */
  if(!src || !name) {
    lprintf(stderr, "set_SourceName: NULL argument(s).\n");
    return 1;
  };
/*
 * Check the validity of the source name.
 */
  if(name[0]=='\0') {
    lprintf(stderr, "set_SourceName: No source name provided.\n");
    return 1;
  };
/*
 * Only allow printable characters other than spaces and comment characters.
 */
  for(cptr=name; *cptr; cptr++) {
    if(!valid_source_char(*cptr)) {
      lprintf(stderr,
	      "set_SourceName: Illegal character '%c' found in source name.\n",
	      *cptr);
      return 1;
    };
  };
/*
 * Make sure that the source name doesn't excede the size accomodated
 * by SourceId::name[].
 */
  if(strlen(name) >= SRC_NAME_MAX) {
    lprintf(stderr, "set_SourceName(%s): Source name too long (>%d).\n", name,
	    SRC_NAME_MAX - 1);
    return 1;
  };
/*
 * Give the source its name.
 */
  strcpy(src->id.name, name);
  return 0;
}

/*.......................................................................
 * Delete a source.
 *
 * Input:
 *  sc    SourceCatalog *  The source catalog that contained the source.
 *  src          Source *  The source to be deleted.
 * Output:
 *  return       Source *  Allways NULL.
 */
static Source *del_Source(SourceCatalog *sc, Source *src)
{
  if(src) {
    switch(src->id.type) {
    case SRC_J2000:
      src = (Source *)del_FreeListNode("del_Source", sc->j2000_mem, src);
      break;
    case SRC_EPHEM:
      if(src->ephem.file)
	free(src->ephem.file);
      if(src->ephem.cache)
	free(src->ephem.cache);
      src->ephem.ra = del_QuadPath(src->ephem.ra);
      src->ephem.dec = del_QuadPath(src->ephem.dec);
      src->ephem.dist = del_QuadPath(src->ephem.dist);
      src = (Source* )del_FreeListNode("del_Source", sc->ephem_mem, src);
      break;
    case SRC_FIXED:
      src = (Source* )del_FreeListNode("del_Source", sc->fixed_mem, src);
      break;
    case SRC_ALIAS:
      src = (Source* )del_FreeListNode("del_Source", sc->alias_mem, src);
      break;
    default:
      break;
    };
  };
  return NULL;
}

/*.......................................................................
 * Get a public copy of the source identification header of a source.
 *
 * Input:
 *  src     Source *   The source to identify.
 *  resolve    int     If this is true and the source is an alias to
 *                     another source, return the id of the aliased
 *                     source in its place.
 * Input/Output:
 *  id    SourceId *   The output container for the ID.
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */
int get_SourceId(Source *src, int resolve, SourceId *id)
{
  if(!src || !id) {
    lprintf(stderr, "get_SourceId: NULL argument(s).\n");
    return 1;
  };
  if(resolve && (src=resolve_Aliases(src)) == NULL)
    return 1;
  *id = src->id;
  return 0;
}

/*.......................................................................
 * Return the catalog index of the given source.
 *
 * Input:
 *  src     Source *   The source to identify.
 *  resolve    int     If this is true and the source is an alias to
 *                     another source, return the id of the aliased
 *                     source in its place.
 * Output:
 *  return     int     The catalog index, or -1 on error.
 */
int get_Source_number(Source *src, int resolve)
{
  if(!src) {
    lprintf(stderr, "get_Source_number: NULL argument(s).\n");
    return -1;
  };
  if(resolve && (src=resolve_Aliases(src)) == NULL)
    return -1;
  return src->id.number;
}

/*.......................................................................
 * Create an empty source catalog.
 *
 * Output:
 *  return  SourceCatalog *  The new catalog, or NULL on error.
 */
SourceCatalog *new_SourceCatalog(void)
{
  SourceCatalog *sc;  /* The return container */
  double tt;          /* The current Terrestrial Time (MJD) */
/*
 * Allocate the container.
 */
  sc = (SourceCatalog *) malloc(sizeof(SourceCatalog));
  if(!sc) {
    lprintf(stderr, "new_SourceCatalog: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can safely be passed
 * to del_SourceCatalog().
 */
  sc->ephem_input = NULL;
  sc->j2000_mem = NULL;
  sc->ephem_mem = NULL;
  sc->fixed_mem = NULL;
  sc->alias_mem = NULL;
  sc->head = NULL;
  sc->hash = NULL;
  sc->nsource = 0;
  sc->ut1utc = NULL;
  sc->eqneqx = NULL;
  sc->mapqk = NULL;
/*
 * Allocate an input stream to be used when reading ephemerides.
 */
  sc->ephem_input = new_InputStream();
  if(!sc->ephem_input)
    return del_SourceCatalog(sc);
/*
 * Allocate free-lists for each of the supported types of source.
 */
  sc->j2000_mem = (FreeList* )new_FreeList("new_SourceCatalog", 
					   sizeof(J2000Source), 100);
  if(!sc->j2000_mem)
    return del_SourceCatalog(sc);

  sc->ephem_mem = (FreeList* )new_FreeList("new_SourceCatalog", 
					   sizeof(EphemSource), 20);
  if(!sc->ephem_mem)
    return del_SourceCatalog(sc);

  sc->fixed_mem = (FreeList* )new_FreeList("new_SourceCatalog", 
			       sizeof(FixedSource), 20);
  if(!sc->fixed_mem)
    return del_SourceCatalog(sc);

  sc->alias_mem = (FreeList* )new_FreeList("new_SourceCatalog", 
					   sizeof(AliasSource), 32);
  if(!sc->alias_mem)
    return del_SourceCatalog(sc);
/*
 * Create the source hash-table.
 */
  sc->hash = new_HashTable(NULL, SOURCE_HASH_SIZE, IGNORE_CASE, NULL, 0);
  if(!sc->hash)
    return del_SourceCatalog(sc);
/*
 * Get the current Terrestrial Time.
 */
  tt = current_mjd_tt();
  if(tt < 0)
    return del_SourceCatalog(sc);
/*
 * Allocate the object that will interpolate a UT1-UTC ephemeris.
 */
  sc->ut1utc = new_Ut1Utc();
  if(!sc->ut1utc)
    return del_SourceCatalog(sc);
/*
 * Allocate the object that will supply the value of the equation of
 * the equinoxes.
 */
  sc->eqneqx = new_EqnEqx(tt, EPOCH_UPDATE_INTERVAL);
  if(!sc->eqneqx)
    return del_SourceCatalog(sc);
/*
 * Allocate the object that will cache precession and nutation parameters.
 */
  sc->mapqk = new_MapqkCache(2000.0, tt, EPOCH_UPDATE_INTERVAL);
  if(!sc->mapqk)
    return del_SourceCatalog(sc);
/*
 * Return the empty catalog.
 */
  return sc;
}

/*.......................................................................
 * Delete a source catalog.
 *
 * Input:
 *  sc     SourceCatalog *  A source catalog to be deleted.
 * Output:
 *  return SourceCatalog *  Allways NULL.
 */
SourceCatalog *del_SourceCatalog(SourceCatalog *sc)
{
  int i;
  if(sc) {
    sc->ephem_input = del_InputStream(sc->ephem_input);
    sc->ut1utc = del_Ut1Utc(sc->ut1utc);
    sc->eqneqx = del_EqnEqx(sc->eqneqx);
    sc->mapqk = del_MapqkCache(sc->mapqk);
/*
 * Delete the list of catalog segments.
 */
    while(sc->head) {
/*
 * Remove the head of the list.
 */
      CatalogSegment *seg = sc->head;
      sc->head = seg->next;
/*
 * Delete the sources of the current array segment.
 */
      for(i=0; i<seg->nsource; i++)
	del_Source(sc, seg->s[i]);
/*
 * Discard the emptied segment.
 */
      free(seg);
    };
/*
 * Discard the symbol table.
 */
    sc->hash = del_HashTable(sc->hash);
/*
 * Discard the source freelists.
 */
    sc->j2000_mem = del_FreeList("del_SourceCatalog", sc->j2000_mem, 1);
    sc->ephem_mem = del_FreeList("del_SourceCatalog", sc->ephem_mem, 1);
    sc->fixed_mem = del_FreeList("del_SourceCatalog", sc->fixed_mem, 1);
    sc->alias_mem = del_FreeList("del_SourceCatalog", sc->alias_mem, 1);
/*
 * Discard the catalog container.
 */
    free(sc);
  };
  return NULL;
}

/*.......................................................................
 * Return the current number of sources in a source catalog.
 *
 * Input:
 *  sc      SourceCatalog *  The source catalog.
 * Output:
 *  return            int    The number of sources.
 */
int size_SourceCatalog(SourceCatalog *sc)
{
  return sc ? sc->nsource : 0;
}

/*.......................................................................
 * Create an object to provide an interface to a UT1-UTC ephemeris.
 *
 * Note that the object will not be ready for use until read_ut1utc()
 * has been sucessfully called to install a UT1-UTC ephemeris.
 *
 * Output:
 *  return       Ut1Utc *  The new object, or NULL on error.
 */
static Ut1Utc *new_Ut1Utc(void)
{
  Ut1Utc *uu;      /* The object to be returned */
  int i;
/*
 * Allocate the container.
 */
  uu = (Ut1Utc* )malloc(sizeof(Ut1Utc));
  if(!uu) {
    lprintf(stderr, "new_Ut1Utc: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can safely be passed
 * to del_Ut1Utc().
 */
  uu->file = NULL;
  uu->cache = NULL;
  uu->size = 0;
  uu->midpt = -1;
  uu->qp = NULL;
/*
 * Allocate the cache array of ephemeris entries.
 */
  uu->cache = (Ut1UtcEntry* )malloc(sizeof(Ut1UtcEntry) * EPHEM_CACHE_SIZE);
  if(!uu->cache)
    return del_Ut1Utc(uu);
/*
 * Clear the cache entries.
 */
  for(i=0; i<EPHEM_CACHE_SIZE; i++) {
    Ut1UtcEntry *uue = uu->cache + i;
    uue->utc = uue->ut1utc = 0.0;
  };
/*
 * Allocate a quadratic interpolation object.
 */
  uu->qp = new_QuadPath(0.0, QP_NORMAL);
  if(!uu->qp)
    return del_Ut1Utc(uu);
  return uu;
}

/*.......................................................................
 * Delete a Ut1Utc object.
 *
 * Input:
 *  uu       Ut1Utc *  The object to be deleted.
 * Output:
 *  return   Ut1Utc *  The deleted object (always NULL).
 */
static Ut1Utc *del_Ut1Utc(Ut1Utc *uu)
{
  if(uu) {
    if(uu->file)
      free(uu->file);
    if(uu->cache)
      free(uu->cache);
    uu->qp = del_QuadPath(uu->qp);
    free(uu);
  };
  return NULL;
}

/*.......................................................................
 * Install a new UT1-UTC ephemeris file.
 *
 * Input:
 *  sc     SourceCatalog *  The parent source catalog of the ephemeris.
 *  dir            char *   The directory if separate from the file name,
 *                          else "".
 *  file           char *   The path name of the text file to read from.
 *                          This will be appended to dir[] unless dir[]
 *                          is "".
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
int sc_set_ut1utc_ephemeris(SourceCatalog *sc, char *dir, char *file)
{
  double utc;   /* The current date and time (UTC as a MJD) */
  Ut1Utc *uu;   /* The ephemeris container */
/*
 * Check arguments.
 */
  if(!sc || !dir || !file) {
    lprintf(stderr, "sc_set_ut1utc_ephemeris: NULL argument(s).\n");
    return 1;
  };
/*
 * Get the ephemeris container.
 */
  uu = sc->ut1utc;
/*
 * Discard the previous ephemeris file, if there is one.
 */
  if(uu->file) {
    free(uu->file);
    uu->file = NULL;
  };
/*
 * Allocate a copy of the new file name.
 */
  uu->file = new_pathname(dir, file);
  if(!uu->file)
    return bad_ut1utc_file(uu);
/*
 * Get the current UTC.
 */
  utc = current_mjd_utc();
  if(utc < 0.0)
    return bad_ut1utc_file(uu);
/*
 * To validate the ephemeris file, attempt to fill the ephemeris cache
 * with entries for the current time.
 */
  if(read_ut1utc(sc, uu, utc))
    return bad_ut1utc_file(uu);
  return 0;
}

/*.......................................................................
 * This the private error return function of sc_set_ut1utc_ephemeris().
 *
 * Input:
 *  uu     Ut1Utc *  The ephemeris container.
 * Output:
 *  return    int    1 (the error return code of sc_set_ut1utc_ephemeris()).
 */
static int bad_ut1utc_file(Ut1Utc *uu)
{
/*
 * Discard the new file name.
 */
  if(uu->file)
    free(uu->file);
  uu->file = NULL;
  return 1;
}

/*.......................................................................
 * Read an ephemeris of UT1-UTC samples from a given input stream.
 *
 * Each non-blank line of the ephemeris must contain the following
 * fields.
 *
 *   utc   ut1-utc
 *
 * The first field is the UTC at which the entry takes effect. This is
 * expressed as a Modified Julian Date. The second entry is the offset
 * between UT1 and UTC. This must be >= 0 and <= 1.0.
 *
 * Comments can appear on any line, starting with a # character and
 * extending to the end of the line.
 *
 * Input:
 *  sc  SourceCatalog *  The parent source catalog.
 *  uu         Ut1Utc *  The object into which to read the ephemeris.
 *  utc        double    The earliest time for which the ephemeris is
 *                       needed. This is a UTC, expressed as a MJD.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int read_ut1utc(SourceCatalog *sc, Ut1Utc *uu, double utc)
{
  InputStream *input;       /* A local alias of sc->ephem_input */
  int nrec;                 /* The number of entries read so far */
  Ut1UtcEntry *cache;       /* A local alias of sc->cache */
  int update;               /* True if this is an update */
/*
 * Are we updating an existing cache?
 */
  update = uu->size != 0;
/*
 * Clear the current ut1-utc cache.
 */
  uu->size = 0;
  uu->midpt = -1;
/*
 * Get the cache array.
 */
  cache = uu->cache;
/*
 * Do we have a file to read from yet?
 */
  if(!uu->file) {
    lprintf(stderr,
	    "read_ut1utc: No UT1-UTC ephemeris file has been specified.\n");
    return 1;
  };
  lprintf(stdout, "%s UT1-UTC ephemeris from %s\n",
	  update ? "Updating":"Initializing", uu->file);
/*
 * Open the ephemeris file.
 */
  input = sc->ephem_input;
  if(open_FileInputStream(input, "", uu->file))
    return bad_ut1utc_ephemeris(NULL, uu);
/*
 * Find the first record of the file.
 */
  if(input_skip_white(input, 1, 0))
    return bad_ut1utc_ephemeris(input, uu);
/*
 * Read the first two entries into the cache. The reason for
 * this will become apparent shortly.
 */
  if(read_ut1utc_entry(input, 0.0, &cache[0]) ||
     read_ut1utc_entry(input, cache[0].utc, &cache[1]))
    return bad_ut1utc_ephemeris(input, uu);
/*
 * The ephemeris now contains 2 entries.
 */
  uu->size = 2;
/*
 * Read in sequential blocks of EPHEM_CACHE_SIZE ephemeris entries
 * until a block that contains three records that can be used to
 * interpolate for time tt is found. Since the three records being
 * sought may straddle the boundary of one of these blocks, carry over
 * the last two entries of each block to the start of the next block.
 */
  do {
/*
 * Copy the oldest two entries of the last block to the start of the
 * next block (this is why two entries are read before entering this
 * loop).
 */
    if(uu->size > 2) {
      cache[0] = cache[uu->size-2];
      cache[1] = cache[uu->size-1];
    };
/*
 * Read further entries into the cache until either EPHEM_CACHE_SIZE
 * entries have been read or the end of the file is reached.
 */
    for(nrec=2; nrec<EPHEM_CACHE_SIZE && input->nextc!=EOF; nrec++) {
      Ut1UtcEntry *uue = cache + nrec;
      if(read_ut1utc_entry(input, uue[-1].utc, uue))
	return bad_ut1utc_ephemeris(input, uu);
    };
/*
 * Record the number of entries now in the cache.
 */
    uu->size = nrec;
/*
 * If we reached the end of the file, or we passed three entries
 * that can be used to interpolate for utc, stop reading.
 */
  } while(input->nextc != EOF && ut1utc_range_cmp(uu, nrec-2, utc) < 0);
/*
 * Close the ephemeris file.
 */
  close_InputStream(input);
/*
 * If there are three or more entries in the newly read cache,
 * search for three neighboring entries in the cache that are suitable
 * for quadratic interpolation for utc. If found, install the three
 * entries in the quadratic interpolators for ra, dec and distance.
 */
  if(nrec >= 3 && (uu->midpt=find_ut1utc_entry(uu, utc)) >= 0) {
    set_ut1utc_quad(uu, uu->midpt);
  } else {
    lprintf(stderr, "Failed to find interpolation entries for UTC %g.\n", utc);
    return bad_ut1utc_ephemeris(NULL, uu);
  };
  return 0;
}

/*.......................................................................
 * This is the private error cleanup file of read_ut1utc().
 *
 * Input:
 *  input  InputStream *  The stream that is connected to the ephemeris
 *                        file (NULL if not yet opened).
 *  uu          Ut1Utc *  The incomplete UT1-UTC ephemeris.
 * Output:
 *  return         int    1 (the error return code of read_ut1utc()).
 */
static int bad_ut1utc_ephemeris(InputStream *input, Ut1Utc *uu)
{
/*
 * Close the ephemeris file.
 */
  if(input)
    close_InputStream(input);
/*
 * Discard the incomplete ephemeris.
 */
  uu->size = 0;
  uu->midpt = -1;
/*
 * Return the error-return code of read_ut1utc().
 */
  return 1;
}

/*.......................................................................
 * Read one entry from a UT1-UTC ephemeris. Note that the stream pointer
 * must already point at the first non-white-space character of the record.
 * On successful returns the stream pointer will be left pointing at the
 * first non-white-space character of the next record.
 *
 * Input:
 *  input  InputStream *  The stream from which to read the entry.
 *  last_utc    double    The UTC of the last entry read from the stream.
 * Input/Output:
 *  uue    Ut1UtcEntry *  The place to record the contents of the entry.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int read_ut1utc_entry(InputStream *input, double last_utc,
			     Ut1UtcEntry *uue)
{
/*
 * Read the next record.
 */
  if(input_double(input, 0, &uue->utc))
    return bad_ut1utc_entry(input, "MJD UTC");
  if(input_skip_space(input, 1, 0))
    return 1;
  if(input_double(input, 0, &uue->ut1utc))
    return bad_ut1utc_entry(input, "ut1-utc");
  if(input_skip_white(input, 1, 0))
    return 1;
/*
 * The ephemeris is supposed to be in ascending order of time.
 */
  if(uue->utc < last_utc)
    return input_error(input, 1,
		       "The ut1-utc ephemeris is not in time order.\n");
/*
 * The definition of UTC and UT1 are such that the two numbers are never
 * more than a second different.
 */
  if(fabs(uue->ut1utc) > 1.0)
    return input_error(input, 1, "Illegal |UT1-UTC| > 1.\n");
  return 0;
}

/*.......................................................................
 * This is a private syntax-error return function for read_ut1utc_entry().
 *
 * Input:
 *  input   InputStream *  The stream in which the syntax error was found.
 *  column         char *  The description of the bad element.
 * Output:
 *  return          int    Always 1.
 */
static int bad_ut1utc_entry(InputStream *input, char *column)
{
  input_error(input, 1, "Bad %s field.\n", column);
  input_error(input, 1, "Use: mjd_utc seconds\n");
  return 1;
}

/*.......................................................................
 * Compare the interpolation coverage of three neighboring UT1-UTC
 * ephemeris entries to a given time.
 *
 * Input:
 *  uu          Ut1Utc *  The parent UT1-UTC ephemeris.
 *  midpt          int    The cache index of the middle entry of
 *                        the three entries to compare. Note that it
 *                        is ok for this to be 0 or uu->size-1, but
 *                        only -1 or 1 will ever be returned in this
 *                        case because the entry doesn't have two neighbors.
 *  utc         double    The time that an interpolation triple is
 *                        needed for (UTC expressed as a MJD).
 * Output:
 *  return         int    -1 - The ephemeris entries are suitable for
 *                             earlier times than utc.
 *                         0 - The ephemeris entries are suitable for
 *                             interpolation of utc.
 *  return         int     1 - The ephemeris entries are suitable for
 *                             later times than utc.
 */
static int ut1utc_range_cmp(Ut1Utc *uu, int midpt, double utc)
{
/*
 * If the entry or the neigbors of the entry being queried are just beyond
 * the lower limit of the ephemeris, extrapolate whether the entry is
 * too early or two late by looking at the first entry in the ephemeris
 * that does have two neigbors.
 */
  if(midpt <= 0) {
    return ut1utc_range_cmp(uu, 1, utc) <= 0 ? -1 : 1;
/*
 * If the entry or the neigbors of the entry being queried are just beyond
 * the upper limit of the ephemeris, extrapolate whether the entry is
 * too early or two late by looking at the last entry in the ephemeris
 * that does have two neigbors. 
 */
  } else if(midpt >= uu->size-1) {
    return ut1utc_range_cmp(uu, uu->size-2, utc) < 0 ? -1 : 1;
/*
 * The entry has two neigbors in the bounds of the ephemeris.
 */
  } else {
    Ut1UtcEntry *uue = uu->cache + midpt;  /* The entry being queried */
/*
 * The entry is suitable for interpolation of time utc if utc is within
 * half the interval between the entry's timestamp and that of each of
 * its neigbors. Compute the limits of this window.
 */
    double tmin = (uue[-1].utc + uue->utc)/2.0;
    double tmax = (uue->utc + uue[1].utc)/2.0;
/*
 * Compare the window to the requested time.
 */
    if(tmax < utc)
      return -1;
    if(tmin > utc)
      return 1;
    return 0;
  };
}

/*.......................................................................
 * Perform a binary search of the ephemeris cache of UT1-UTC ephemeris to
 * locate three neighboring ephemeris entries that are suitable for
 * interpolating for a given time.
 *
 * Input:
 *  uu        Ut1Utc *  The parent UT1-UTC ephemeris to search.
 *  utc       double    The UTC to search for (MJD).
 * Output:
 *  return       int    The cache index of the middle of the three
 *                      ephemeris entries to search, or -1 if not found.
 */
static int find_ut1utc_entry(Ut1Utc *uu, double utc)
{
  int mid = -1;                     /* The index at which to bisect the cache */
/*
 * Note that if the ephemeris is active, there are guaranteed to be at least
 * three entries in the cache. Also note that the first and last entries aren't
 * suitable mid points for interpolation because they only have one neighbor.
 */
  int below = 1;                    /* The lowest index below mid to check */
  int above = uu->size - 2;         /* The highest index above mid to check */
/*
 * If the ephemeris is not active, return failure.
 */
  if(uu->size < 3)
    return -1;
/*
 * Perform a binary search for the middle entry that supports interpolation
 * of time utc.
 */
  while(above >= below) {
    mid = (above + below)/2;
    switch(ut1utc_range_cmp(uu, mid, utc)) {
    case -1:
      below = mid+1;
      break;
    case 0:
      return mid;
      break;
    case 1:
      above = mid-1;
      break;
    };
  };
/*
 * If the search succeded, above and below will both equal mid.
 */
  return -1;
}

/*.......................................................................
 * Set the current triple of the ephemeris entries to be interpolated.
 *
 * Input:
 *  uu        Ut1Utc *  The container of the ephemeris.
 *  midpt        int    The cache index of the middle entry of the three
 *                      neighboring entries.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
static int set_ut1utc_quad(Ut1Utc *uu, int midpt)
{
  Ut1UtcEntry *uue; /* The ephemeris entry corresponding to 'midpt' */
  QuadData data;    /* The QuadPath initialization object */
/*
 * Make sure that the specified entry and its neighbors lie within the
 * bounds of the cache.
 */
  if(midpt <= 0 || midpt >= uu->size-1) {
    lprintf(stderr, "set_ut1utc_quad: Index out of range.\n");
    return 1;
  };
/*
 * Record the new mid point.
 */
  uu->midpt = midpt;
/*
 * Get a pointer to the cache element that corresponds to midpt.
 */
  uue = uu->cache + midpt;
/*
 * Fill the interpolator initialization object.
 */
  data.npt = 3;
  data.s[0].x = uue[-1].utc;
  data.s[0].y = uue[-1].ut1utc;
  data.s[1].x = uue->utc;
  data.s[1].y = uue->ut1utc;
  data.s[2].x = uue[+1].utc;
  data.s[2].y = uue[+1].ut1utc;
/*
 * Set up the interpolator to interpolate the new triple.
 */
  if(set_QuadPath(uu->qp, &data))
    return 1;
  return 0;
}

/*.......................................................................
 * Return the time window over which the 3 UT1-UTC interpolation
 * entries for the UTC MJD time 'utc' are usable.
 *
 * Input:
 *  sc     SourceCatalog *   The parent source catalog.
 *  utc           double     The UTC time whose enclosing window is to
 *                           be returned, expressed as a Modified Julian
 *                           Date. The current time will be substituted
 *                           if utc < 0.
 * Input/Output:
 *  win      CacheWindow *   The window of times over which the cache
 *                           is valid, or {0,0} if no UT1-UTC file
 *                           has been specified yet. The times are in UTC.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Failed to update cache.
 */
int get_ut1utc_window(SourceCatalog *sc, double utc, CacheWindow *win)
{
  Ut1UtcEntry *uue;   /* The middle ephemeris interpolation cache entry */
/*
 * Check arguments.
 */
  if(!sc || !win) {
    lprintf(stderr, "get_ut1utc_window: NULL argument(s).\n");
    return 1;
  };
/*
 * Substitute the current time?
 */
  if(utc < 0.0 && (utc = current_mjd_utc()) < 0.0)
    return 1;
/*
 * Do we have an ut1-utc ephemeris yet?
 */
  if(sc->ut1utc->file) {
/*
 * Update the UT1-UTC interpolation cache.
 */
    if(update_ut1utc(sc, sc->ut1utc, utc))
      return 1;
/*
 * Get the middle entry of the three UT1-UTC cache entries that has
 * been chosen to interpolate for time 'utc'.
 */
    uue = sc->ut1utc->cache + sc->ut1utc->midpt;
/*
 * Return the time limits of the interpolation entries.
 */
    win->tmin = (uue[-1].utc + uue->utc)/2.0;
    win->tmax = (uue->utc + uue[1].utc)/2.0;
  } else {
    win->tmin = win->tmax = 0.0;
  };
  return 0;
}

/*.......................................................................
 * Return the contents of the quadratic interpolator of UT1-UTC
 * at time 'utc'.
 *
 * Input:
 *  sc     SourceCatalog *   The parent source catalog.
 *  utc           double     The UTC time for which the interpolator is
 *                           be valid, expressed as a Modified Julian
 *                           Date. The current date will be substituted
 *                           if utc < 0.
 * Input/Output:
 *  data        QuadData *   The quadratic interpolator contents will
 *                           be assigned to *data. See quad.h for details.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Failed to update cache.
 */
int get_ut1utc_QuadData(SourceCatalog *sc, double utc, QuadData *data)
{
/*
 * Check arguments.
 */
  if(!sc || !data) {
    lprintf(stderr, "get_ut1utc_QuadData: NULL argument(s).\n");
    return 1;
  };
/*
 * Substitute the current time?
 */
  if(utc < 0.0 && (utc = current_mjd_utc()) < 0.0)
    return 1;
/*
 * Update the interpolation cache if necessary.
 */
  if(update_ut1utc(sc, sc->ut1utc, utc))
    return 1;
/*
 * Return the interpolator contents.
 */
  return get_QuadPath(sc->ut1utc->qp, data);
}

/*.......................................................................
 * If needed, update the interpolator of UT1-UTC for a given time.
 *
 * Input:
 *  sc  SourceCatalog *  The parent source catalog of the UT1-UTC ephemeris.
 *  uu         Ut1Utc *  The interpolator to update.
 *  utc        double    The UTC at which the interpolation is needed (MJD).
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
static int update_ut1utc(SourceCatalog *sc, Ut1Utc *uu, double utc)
{
/*
 * Check the arguments.
 */
  if(!sc || !uu) {
    lprintf(stderr, "update_ut1utc: Invalid argument(s).\n");
    return 1;
  };
/*
 * If the cache is empty read ephemeris file to refill the cache for
 * time 'utc'.
 */
  if(uu->size<3)
    return read_ut1utc(sc, uu, utc);
/*
 * Does the interpolator need updating?
 */
  if(ut1utc_range_cmp(uu, uu->midpt, utc) != 0) {
/*
 * Attempt to find cached ephemeris entries that can be used to
 * interpolate for utc.
 */
    int midpt = find_ut1utc_entry(uu, utc);
/*
 * If suitable entries couldn't be found then refresh the cache from
 * the source's ephemeris file.
 */
    if(midpt < 0)
      return read_ut1utc(sc, uu, utc);
/*
 * Initialize the interpolator to use the new midpt entry as its central
 * node.
 */
    if(set_ut1utc_quad(uu, midpt))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Return the UT1 time that corresponds to a given UTC. Note that this
 * will be ambiguous during a leap second. Also note that until the
 * sc_set_ut1utc_ephemeris() has been called successfully, this function
 * will actually return UTC.
 *
 * Input:
 *  sc  SourceCatalog * The parent source catalog of the UT1-UTC ephemeris.
 *  uu         Ut1Utc * The UT1-UTC ephemeris.
 *  utc        double   The Universal Coordinated Time to convert to UT1,
 *                      expressed as a Modified Julian Date.
 * Input/Output:
 *  ut1        double * The value of UT1 corresponding to the specified
 *                      UTC.
 * Output:
 *  return        int   0 - OK.
 *                      1 - Error.
 */
static int sc_get_ut1(SourceCatalog *sc, double utc, double *ut1)
{
  double ut1utc;   /* The value of UT1-UTC */
/*
 * Check the arguments.
 */
  if(!sc || !ut1) {
    lprintf(stderr, "sc_get_ut1: NULL argument(s).\n");
    return 1;
  };
/*
 * If we don't yet have a UT1-UTC ephemeris, return utc.
 */
  if(!sc->ut1utc->file) {
    ut1utc = 0.0;
  } else {
/*
 * Ensure that the UT1-UTC interpolator is correctly positioned.
 */
    if(update_ut1utc(sc, sc->ut1utc, utc))
      return 1;
    ut1utc = eval_QuadPath(sc->ut1utc->qp, utc);
  };
/*
 * Return UT1.
 */
  *ut1 = utc + ut1utc / daysec;
  return 0;
}

/*.......................................................................
 * Return the Terrestrial Time that corresponds to a given UTC.
 *
 * Input:
 *  sc     SourceCatalog *   A source catalog containing a UT1-UTC
 *                           interpolator.
 *  utc           double     The UTC for which TT is required (MJD).
 * Input/Output:
 *  tt            double *   On output the Terrestrial Time will be
 *                           assigned to *tt.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Error.
 */
static int sc_get_tt(SourceCatalog *sc, double utc, double *tt)
{
  return (*tt = mjd_utc_to_mjd_tt(utc)) < 0.0;
}

/*.......................................................................
 * Create an object which supplies interpolated values of the equation
 * of the equinoxes.
 *
 * Input:
 *  tt      double    The time for which to initialize the interpolator.
 *                    The time should be a Terrestrial Time, expressed
 *                    as a MJD.
 *  dt      double    The time interval (days) at which to calculate new
 *                    values of the equation of the equinoxes. Three
 *                    consecutive values of the equation of the equinoxes,
 *                    separated by this interval, will be quadratically
 *                    interpolated.
 * Output:
 *  return  EqnEqx *  The new object, or NULL on error.
 */
static EqnEqx *new_EqnEqx(double tt, double dt)
{
  EqnEqx *ee;  /* The object to be returned */
/*
 * Check that the arguments make sense.
 */
  if(tt < 0.0) {
    lprintf(stderr, "new_EqnEqx: Illegal -ve TT rejected.\n");
    return NULL;
  };
  if(dt < 1.0/daysec) {
    lprintf(stderr, "new_EqnEqx: Update interval < minimum of 1 second.\n");
    return NULL;
  };
/*
 * Allocate the container.
 */
  ee = (EqnEqx* )malloc(sizeof(EqnEqx));
  if(!ee) {
    lprintf(stderr, "new_EqnEqx: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can safely be passed
 * to del_EqnEqx().
 */
  ee->dt = dt;
  ini_QuadData(&ee->data);
  ee->qp = NULL;
/*
 * Allocate a quadratic interpolation object.
 */
  ee->qp = new_QuadPath(0.0, QP_NORMAL);
  if(!ee->qp)
    return del_EqnEqx(ee);
/*
 * Initialize the interpolator for the specified time.
 */
  if(update_eqneqx(ee, tt))
    return del_EqnEqx(ee);
  return ee;
}

/*.......................................................................
 * Delete an EqnEqx object.
 *
 * Input:
 *  ee      EqnEqx *  The object to be deleted.
 * Output:
 *  return  EqnEqx *  The deleted object (always NULL).
 */
static EqnEqx *del_EqnEqx(EqnEqx *ee)
{
  if(ee) {
    ee->qp = del_QuadPath(ee->qp);
    free(ee);
  };
  return NULL;
}

/*.......................................................................
 * Return the time window over which the 3 equation-of-the-equinoxes
 * interpolation entries for the time 'tt' are usable.
 *
 * Input:
 *  sc     SourceCatalog *   The parent source catalog.
 *  tt            double     The Terrestrial Time whose enclosing window
 *                           is to be returned, expressed as a Modified
 *                           Julian Date. If tt < 0, the current
 *                           Terrestrial time will be substituted.
 * Input/Output:
 *  win      CacheWindow *   The window of times over which the cache
 *                           is valid. The times are in TT.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Failed to update cache.
 */
int get_eqneqx_window(SourceCatalog *sc, double tt, CacheWindow *win)
{
/*
 * Check arguments.
 */
  if(!sc || !win) {
    lprintf(stderr, "get_eqneqx_window: NULL argument(s).\n");
    return 1;
  };
/*
 * Substitute the current terrestrial time?
 */
  if(tt < 0.0 && (tt = current_mjd_tt()) < 0.0)
    return 1;
/*
 * Update the interpolation cache if necessary.
 */
  if(update_eqneqx(sc->eqneqx, tt))
    return 1;
/*
 * Return the time limits of the current interpolation entries.
 */
  {
    QuadSample *s = sc->eqneqx->data.s;
    win->tmin = (s[0].x + s[1].x)/2.0;
    win->tmax = (s[1].x + s[2].x)/2.0;
  };
  return 0;
}

/*.......................................................................
 * Return the contents of the quadratic interpolator for the equation of
 * the equinoxes at time 'tt'.
 *
 * Input:
 *  sc     SourceCatalog *   The parent source catalog.
 *  tt            double     The Terrestrial Time for which the
 *                           interpolator is to be valid, expressed as a
 *                           Modified Julian Date. If tt < 0, the current
 *                           Terrestrial Time will be substituted.
 * Input/Output:
 *  data        QuadData *   The quadratic interpolator contents will
 *                           be assigned to *data. See quad.h for details.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Failed to update cache.
 */
int get_eqneqx_QuadData(SourceCatalog *sc, double tt, QuadData *data)
{
/*
 * Check arguments.
 */
  if(!sc || !data) {
    lprintf(stderr, "get_eqneqx_QuadPath: NULL argument(s).\n");
    return 1;
  };
/*
 * Substitute the current terrestrial time?
 */
  if(tt < 0.0 && (tt = current_mjd_tt()) < 0.0)
    return 1;
/*
 * Update the interpolation cache if necessary.
 */
  if(update_eqneqx(sc->eqneqx, tt))
    return 1;
/*
 * Return the interpolator contents.
 */
  *data = sc->eqneqx->data;
  return 0;
}

/*.......................................................................
 * If needed, update the interpolator of the equation of the equinoxes
 * for a given time.
 *
 * Input:
 *  ee      EqnEqx *  The interpolator to update.
 *  tt      double    The Terrestrial Time (MJD) at which the
 *                    interpolation is needed.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
static int update_eqneqx(EqnEqx *ee, double tt)
{
  double dt;               /* The interval between samples (ee->dt) */
  double tmid;             /* The required mid time of the interpolation */
  int offset;              /* The number of intervals between tmid and the */
                           /*  current middle of the interpolation window */
  int abs_offset;          /* The absolute value of offset */
  int i;
/*
 * Check the arguments.
 */
  if(!ee || tt < 0.0) {
    lprintf(stderr, "update_eqneqx: Invalid argument(s).\n");
    return 1;
  };
/*
 * Get local copies of repeatedly-used parameters.
 */
  dt = ee->dt;
/*
 * The equation of the equinox is interpolated from values that lie
 * on a grid of origin 0 and interval dt. Find the time on this grid
 * that is nearest to the requested time. This is the grid sample that
 * should be the middle time of the interpolator.
 * In other words, if tt is more than half a sample interval from
 * the middle point of the interpolator, then the interpolator needs
 * to be updated.
 */
  tmid = dt * floor(tt/dt+0.5);
/*
 * Determine the number of intervals between the required mid point and
 * the current mid point. Note that this could overflow an int, so it
 * is limited to between -3 and 3 before assignment to an int. Offsets
 * outside this range are equivalent to the nearest limit.
 */
  if(ee->data.npt) {
    double tmp = floor((tmid - ee->data.s[1].x)/dt + 0.5);
    offset = tmp > 3 ? 3 : (tmp < -3 ? -3 : (int)tmp);
  } else {
    offset = 3;  /* Make sure that the interpolator gets initialized */
  };
/*
 * Get the absolute value of the offset.
 */
  abs_offset = abs(offset);
/*
 * Is an update needed?
 */
  if(abs_offset) {
/*
 * There are always three points in the interpolator.
 */
    ee->data.npt = 3;
/*
 * If the required mid time is later than the current mid time then
 * append new values after shifting down the remaining values to
 * make way for them. Note that slaEqeqx() is an expensive function,
 * so we try to evaluate it as few times as possible.
 */
    if(offset > 0) {
      for(i=0; i<3-abs_offset; i++)
	ee->data.s[i] = ee->data.s[i+abs_offset];
      for(i=3-abs_offset; i<3; i++) {
	double t = tmid + (i-1) * dt;
	ee->data.s[i].x = t;
	ee->data.s[i].y = slaEqeqx(t);
      };
/*
 * If the required mid time is before than the current mid time then
 * prepend as many new values as are needed after shifting
 * up the remaining values to make way for them.
 */
    } else {
      for(i=2; i >= abs_offset; i--)
	ee->data.s[i] = ee->data.s[i-abs_offset];
      for(i=0; i<abs_offset; i++) {
	double t = tmid + (i-1) * dt;
	ee->data.s[i].x = t;
	ee->data.s[i].y = slaEqeqx(t);
      };
    };
/*
 * Install the new values in the interpolator.
 */
    if(set_QuadPath(ee->qp, &ee->data))
      return 1;
  };
  return 0;
}

/*.......................................................................
 * Return the equation of the equinoxes corresponding to a given time.
 * Note that this will be ambiguous during a leap second.
 *
 * Input:
 *  sc  SourceCatalog * The parent source catalog of the UT1-UTC ephemeris.
 *  uu         Ut1Utc * The UT1-UTC ephemeris.
 *  tt         double   The target Terrestrial Time expressed as a Modified
 *                      Julian Date.
 * Input/Output:
 *  eqneqx     double * The value of equation of the equinoxes corresponding
 *                      to tt.
 * Output:
 *  return        int   0 - OK.
 *                      1 - Error.
 */
static int sc_get_eqneqx(SourceCatalog *sc, double tt, double *eqneqx)
{
  if(!sc || !eqneqx) {
    lprintf(stderr, "sc_get_eqneqx: NULL argument(s).\n");
    return 1;
  };
/*
 * Ensure that the interpolator is correctly positioned.
 */
  if(update_eqneqx(sc->eqneqx, tt))
    return 1;
  *eqneqx = eval_QuadPath(sc->eqneqx->qp, tt);
  return 0;
}

/*.......................................................................
 * Return the local apparent sidereal time of a given utc.
 *
 * Input:
 *  sc   SourceCatalog *  The catalog to use to acquire ut1 and eqneqx.
 *  site          Site *  The location of the observer.
 *  utc         double    The UTC to return times for.
 * Input/Output:
 *  lst         double *  The local apparent sidereal time that corresponds
 *                        to UTC.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int sc_get_lst(SourceCatalog *sc, Site *site, double utc, double *lst)
{
  double eqneqx;   /* The equation of the equinoxes at 'utc' */
  double ut1;      /* The UT1 equivalent of 'utc' */
  double tt;       /* The Terrestrial Time equivalent of 'utc' */
/*
 * Get the UT1 and Terrestrial Time equivalents of 'utc'.
 */
  if(sc_get_ut1(sc, utc, &ut1) ||
     sc_get_tt(sc, utc, &tt))
    return 1;
/*
 * Get the equation of the equinoxes for time, tt.
 */
  if(sc_get_eqneqx(sc, tt, &eqneqx))
    return 1;
/*
 * Get the local apparent sidereal time.
 */
  *lst = slaRanorm(slaGmst(ut1) + eqneqx + site->longitude);
  return 0;
}

/*.......................................................................
 * Create a new MapqkCache object. This caches the position-independent
 * parts of the precesion and nutation calculations for a given time
 * range.
 *
 * Input:
 *  epoch   double    The Julian epoch of the mean equinox of the
 *                    objects to be precessed (eg. 2000.0 for J2000).
 *  tt      double    The Terrestrial Time (MJD) for which to initialize
 *                    the cache.
 *  dt      double    The time interval (days) at which to update the cache.
 * Output:
 *  return  MapqkCache *  The new object, or NULL on error.
 */
static MapqkCache *new_MapqkCache(double epoch, double tt, double dt)
{
  MapqkCache *mc;  /* The object to be returned */
  int i;
/*
 * Check that the arguments make sense.
 */
  if(tt < 0.0) {
    lprintf(stderr, "new_MapqkCache: Illegal -ve TT rejected.\n");
    return NULL;
  };
  if(dt < 1.0/daysec) {
    lprintf(stderr, "new_MapqkCache: Update interval < minimum of 1 second.\n");
    return NULL;
  };
  if(epoch < 0.0) {
    lprintf(stderr, "new_MapqkCache: Illegal negative epoch rejected.\n");
    return NULL;
  };
/*
 * Allocate the container.
 */
  mc = (MapqkCache* )malloc(sizeof(MapqkCache));
  if(!mc) {
    lprintf(stderr, "new_MapqkCache: Insufficient memory.\n");
    return NULL;
  };
/*
 * Before attempting any operation that might fail, initialize the
 * container at least up to the point at which it can safely be passed
 * to del_MapqkCache().
 */
  mc->epoch = epoch;
  mc->dt = dt;
  mc->tmin = 0.0;
  mc->tmax = 0.0;
  for(i=0; i<(signed)(sizeof(mc->mappa)/sizeof(mc->mappa[0])); i++)
    mc->mappa[i] = 0.0;
/*
 * Initialize the cache for the specified time.
 */
  if(update_mapqk_cache(mc, tt))
    return del_MapqkCache(mc);
  return mc;
}

/*.......................................................................
 * Delete a MapqkCache object.
 *
 * Input:
 *  mc     MapqkCache *  The object to be deleted.
 * Output:
 *  return MapqkCache *  The deleted object (always NULL).
 */
static MapqkCache *del_MapqkCache(MapqkCache *mc)
{
  if(mc) {
    free(mc);
  };
  return NULL;
}

/*.......................................................................
 * Return the time window over which the cached precession and nutation
 * parameters for Terrestrial Time (tt) are usable for J2000 sources.
 *
 * Input:
 *  sc     SourceCatalog *   The parent source catalog.
 *  tt            double     The Terrestrial Time whose
 *                           enclosing window is to be returned,
 *                           expressed as a Modified Julian Date. If
 *                           tt < 0, the current Terrestrial time will
 *                           be substituted.
 * Input/Output:
 *  win      CacheWindow *   The window of times over which the cache
 *                           is valid. The times are in TT.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Failed to update cache.
 */
int get_mapqk_window(SourceCatalog *sc, double tt, CacheWindow *win)
{
  MapqkCache *mc;   /* The cache being queried */
  
  // Check arguments.

  if(!sc || !win) {
    lprintf(stderr, "get_mapqk_window: NULL argument(s).\n");
    return 1;
  };
  
  // Substitute the current terrestrial time?

  if(tt < 0.0 && (tt = current_mjd_tt()) < 0.0)
    return 1;
  
  // Get the cache.

  mc = sc->mapqk;
  
  // Update the interpolation cache if necessary.

  if(update_mapqk_cache(mc, tt))
    return 1;
  
  // Return time limits for the more stringent of the various possible
  // current caches.

  win->tmin = mc->tmin;
  win->tmax = mc->tmax;

  return 0;
}

/*.......................................................................
 * Return the contents of the precession/nutation parameter cache for
 * Terrestrial Time 'tt'. See slaMapqk() and slaMapqkz() for details of
 * how to use the parameters in the cache.
 *
 * Input:
 *  sc     SourceCatalog *   The parent source catalog.
 *  tt            double     The Terrestrial Time for which the
 *                           cache is to be valid, expressed as a
 *                           Modified Julian Date. If tt < 0, the
 *                           current Terrestrial Time will be
 *                           substituted.
 * Input/Output:
 *  data       MapqkData *   The array of parameters will be placed in
 *                           data->mappa[0..20].
 * Output:
 *  return           int     0 - OK.
 *                           1 - Failed to update cache.
 */
int get_mapqk_data(SourceCatalog *sc, double tt, MapqkData *data)
{
  MapqkCache *mc;   /* The cache being queried */
/*
 * Check arguments.
 */
  if(!sc || !data) {
    lprintf(stderr, "get_mapqk_QuadPath: NULL argument(s).\n");
    return 1;
  };
/*
 * Substitute the current terrestrial time?
 */
  if(tt < 0.0 && (tt = current_mjd_tt()) < 0.0)
    return 1;
/*
 * Get the cache.
 */
  mc = sc->mapqk;
/*
 * Update the interpolation cache if necessary.
 */
  if(update_mapqk_cache(mc, tt))
    return 1;
/*
 * Copy the cache array into the return container.
 */
  memcpy(data->mappa, mc->mappa, sizeof(mc->mappa));
  return 0;
}

/*.......................................................................
 * Update a cache of the position-independent parts of the precession and
 * nutation calculations for a given time. This does nothing if tt is
 * within the tollerance window of the cache wrt the last update.
 *
 * Input:
 *  mc   MapqkCache *   The cache to update.
 *  tt       double     The Terrestrial Time at which the cache is
 *                      needed.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
static int update_mapqk_cache(MapqkCache *mc, double tt)
{
  if(!mc || tt < 0.0) {
    lprintf(stderr, "update_mapqk_cache: Invalid argument(s).\n");
    return 1;
  };
/*
 * If the cache is out of date, update it.
 */
  if(tt <= mc->tmin || tt >= mc->tmax) {
    slaMappa(mc->epoch, tt, mc->mappa);
    mc->tmin = tt - mc->dt;
    mc->tmax = tt + mc->dt;
  };
  return 0;
}

/*.......................................................................
 * A public function that finds a source in a source-catalog by its name.
 *
 * Input:
 *  sc    SourceCatalog *  The source-catalog to be searched.
 *  name           char *  The name of the source to be looked up.
 * Output:
 *  return       Source *  A copy of the catalog entry, or NULL if not
 *                         found.
 */
Source *find_SourceByName(SourceCatalog *sc, char *name)
{
  Symbol *sym;  /* The hash-symbol that contains the source */
/*
 * Check arguments.
 */
  if(!sc || !name) {
    lprintf(stderr, "find_SourceByName: NULL argument.\n");
    return NULL;
  };
/*
 * Find the hash-table entry of the source.
 */
  sym = find_HashSymbol(sc->hash, name);
  if(!sym)
    return NULL;
/*
 * Return the source.
 */
  return (Source* )sym->data;
}

/*.......................................................................
 * A public function that finds a source in a source-catalog by its
 * catalog index.
 *
 * Input:
 *  sc    SourceCatalog *  The source-catalog to be searched.
 *  number         char *  The catalog-index of the source to be looked up.
 * Output:
 *  return       Source *  A copy of the catalog entry, or NULL if not
 *                         found.
 */
Source *find_SourceByNumber(SourceCatalog *sc, int number)
{
/*
 * Check arguments.
 */
  if(!sc) {
    lprintf(stderr, "find_SourceByNumber: NULL argument.\n");
    return NULL;
  };
/*
 * Determine the position of the segment that contains the source.
 */
  if(number < 0 || number >= sc->nsource) {
    lprintf(stderr, "find_SourceByNumber: Source number %d not in catalog.\n",
	    number);
    return NULL;
  };
/*
 * Return the source.
 */
  return *get_source_slot(sc, number);
}

/*.......................................................................
 * Lookup the catalog slot of a given source.
 *
 * Input:
 *  sc    SourceCatalog *   The parent source catalog.
 *  number          int     The source number in the catalog. If this
 *                          wasn't taken from the header of a source
 *                          in the catalog, then the caller should
 *                          verify that it lies between 0 and sc->nsource-1.
 * Output:
 *  return       Source **  The catalog slot numbered 'number'.
 */
static Source **get_source_slot(SourceCatalog *sc, int number)
{
  int sn;               /* The segment number in which the source lies */
  CatalogSegment *seg;  /* The sn'th catalog segment */
/*
 * Find the array segment that contains the slot.
 */
  sn = number / CAT_SEG_SIZE;
  for(seg=sc->head; sn; seg=seg->next, sn--)
    ;
  return seg->s + number % CAT_SEG_SIZE;
}

/*.......................................................................
 * A public function that reads source catalog entries from a given input
 * stream.
 *
 * Input:
 *  sc      SourceCatalog *   The catalog in which to record the sources.
 *  stream    InputStream *   The stream to read from.
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
int input_SourceCatalog(SourceCatalog *sc, InputStream *stream)
{
  SourceId id;     /* A temporary source identification header */
  int i;
/*
 * Check the arguments.
 */
  if(!sc || !stream) {
    lprintf(stderr, "inputSourceCatalog: NULL argument(s).\n");
    return 1;
  };
/*
 * Locate the first entry.
 */
  if(input_skip_white(stream, 1, 0))
    return 1;
/*
 * Read to the end of the stream or error.
 */
  while(stream->nextc != EOF) {
/*
 * Read the source type.
 */
    if(input_word(stream, 0, 1)) {
      return input_error(stream, 1,
	    "Missing source type. Should be J2000, EPHEM, FIXED, or ALIAS.\n");
    };
/*
 * Identify the source-type.
 */
    for(i=0; i<num_src_type; i++) {
      if(strcmp(stream->work, src_type_name[i]) == 0) {
	id.type = (SourceType) i;
	break;
      };
    };
    if(i >= num_src_type) {
      return input_error(stream, 1,
			 "Unknown source type '%s' in source catalog.\n",
			 stream->work);
    };
/*
 * Read the source name.
 */
    if(input_skip_space(stream, 1, 0))
      return 1;
    if(input_word(stream, 0, 0))
      return input_error(stream, 1, "Missing source name.\n");
    if(strlen(stream->work) >= SRC_NAME_MAX) {
      return input_error(stream, 1, "Source name '%s' too long.\n",
			 stream->work);
    };
    strcpy(id.name, stream->work);
/*
 * Allocate a source of the required type, read its parameters and
 * add it to the catalog.
 */
    switch(id.type) {
    case SRC_J2000:
      if(!read_J2000Source(sc, stream, id.name))
	return 1;
      break;
    case SRC_EPHEM:
      if(!read_EphemSource(sc, stream, id.name))
	return 1;
      break;
    case SRC_FIXED:
      if(!read_FixedSource(sc, stream, id.name))
	return 1;
      break;
    case SRC_ALIAS:
      if(!read_AliasSource(sc, stream, id.name))
	return 1;
      break;
    default:
      break;
    };
/*
 * Consume any trailing spaces.
 */
    if(input_skip_space(stream, 1, 0))
      return 1;
/*
 * Make sure that nothing else remains on the source line.
 */
    if(stream->nextc != '\n' && stream->nextc != EOF) {
      return input_error(stream, 1,
	       "Unexpected characters follow a valid source specification.\n");
    };
/*
 * Locate the start of the next source.
 */
    if(input_skip_white(stream, 1, 0))
      return 1;
  };
/*
 * Did we reach the end of the file as expected?
 */
  return stream->nextc != EOF;
}

/*.......................................................................
 * A public function that reads source catalog entries from a given text file.
 *
 * Input:
 *  sc      SourceCatalog *   The catalog in which to record the sources.
 *  dir              char *   The directory if separate from the file name,
 *                            else "".
 *  file             char *   The path name of the text file to read from.
 *                            This will be appended to dir[] unless dir[]
 *                            is "".
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
int read_SourceCatalog(SourceCatalog *sc, char *dir, char *file)
{
  InputStream *input;   /* An input stream connected to the file */
/*
 * Check arguments.
 */
  if(!sc || !file) {
    lprintf(stderr, "read_SourceCatalog: NULL argument(s).\n");
    return 1;
  };
/*
 * Create an input stream.
 */
  input = new_InputStream();
  if(!input)
    return 1;
/*
 * Connect the catalog file to an input stream and read sources from
 * it into the catalog.
 */
  if(open_FileInputStream(input, dir, file) ||
     input_SourceCatalog(sc, input)) {
    del_InputStream(input);
    return 1;
  };
/*
 * Close the file.
 */
  del_InputStream(input);
  return 0;
}

/*.......................................................................
 * A public function that writes the current source catalog entries to an
 * output stream, using a format that is compatible with
 * input_SourceCatalog().
 *
 * Input:
 *  sc      SourceCatalog *   The catalog to be written.
 *  stream   OutputStream *   The stream to write to.
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
int output_SourceCatalog(SourceCatalog *sc, OutputStream *stream)
{
  CatalogSegment *node;  /* A node of the list of catalog-segments */
  int type_width;        /* The width of the source-type field */
  int source_width;      /* The width of the source-name field */
  int i;
/*
 * Check arguments.
 */
  if(!sc || !stream) {
    lprintf(stderr, "write_SourceCatalog: NULL argument(s).\n");
    return 1;
  };
/*
 * Determine the length of the longest source-type keyword.
 */
  type_width = 0;
  for(i=0; i<num_src_type; i++) {
    int tmp_width = strlen(src_type_name[i]);
    if(tmp_width > type_width)
      type_width = tmp_width;
  };
/*
 * Determine the length of the longest source name.
 */
  source_width = 0;
  for(node=sc->head; node; node=node->next) {
    for(i=0; i<node->nsource; i++) {
      Source *src = node->s[i];
      int name_length = strlen(src->id.name);
      if(name_length > source_width)
	source_width = name_length;
    };
  };
/*
 * Write catalog entries from the source array.
 */
  for(node=sc->head; node; node=node->next) {
    for(i=0; i<node->nsource; i++) {
      Source *src = node->s[i];
      int name_length = strlen(src->id.name);
      int type_length = strlen(src_type_name[src->id.type]);
/*
 * Write the source type and source name, padded with spaces up to the
 * associated field widths (plus one space to separate each field from
 * the next).
 */
      if(write_OutputStream(stream, src_type_name[src->id.type]) ||
	 output_spaces(stream, type_width - type_length + 2) ||
	 write_OutputStream(stream, src->id.name) ||
	 output_spaces(stream, source_width - name_length + 2))
	return 1;
/*
 * Now write the type-specific parameters of the source.
 */
      switch(src->id.type) {
      case SRC_J2000:
	if(write_J2000Source(sc, stream, src))
	  return 1;
	break;
      case SRC_EPHEM:
	if(write_EphemSource(sc, stream, src))
	  return 1;
	break;
      case SRC_FIXED:
	if(write_FixedSource(sc, stream, src))
	  return 1;
	break;
      case SRC_ALIAS:
	if(write_AliasSource(sc, stream, src))
	  return 1;
	break;
      default:
	break;
      };
/*
 * Terminate the source line.
 */
      if(write_OutputStream(stream, " \n"))
	return 1;
    };
  };
  return 0;
}

/*.......................................................................
 * A public function that writes a source catalog to a given text
 * file, using the format expected by read_SourceCatalog().
 *
 * Input:
 *  sc      SourceCatalog *   The catalog in which to record the sources.
 *  dir              char *   The directory if separate from the file name,
 *                            else "".
 *  file             char *   The path name of the text file to write to.
 *                            This will be appended to dir[] unless dir[]
 *                            is "".
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
int write_SourceCatalog(SourceCatalog *sc, char *dir, char *file)
{
  OutputStream *output;   /* An output stream connected to the file */
/*
 * Check arguments.
 */
  if(!sc || !file) {
    lprintf(stderr, "write_SourceCatalog: NULL argument(s).\n");
    return 1;
  };
/*
 * Create an output stream.
 */
  output = new_OutputStream();
  if(!output)
    return 1;
/*
 * Connect the catalog file to the output stream and write sources from
 * it into the catalog.
 */
  if(open_FileOutputStream(output, dir, file) ||
     output_SourceCatalog(sc, output)) {
    del_OutputStream(output);
    return 1;
  };
/*
 * Close the file.
 */
  del_OutputStream(output);
  return 0;
}

/*.......................................................................
 * Read the parameters of a J2000 source from an input stream, and add it
 * to the source catalog.
 *
 * Input:
 *  sc      SourceCatalog *   The catalog to add the source to.
 *  stream    InputStream *   The stream to read from.
 *  name             char *   The name of the source.
 * Output:
 *  return         Source *   The new catalog entry of the source, or
 *                            NULL on error.
 */
static Source *read_J2000Source(SourceCatalog *sc, InputStream *stream,
				char *name)
{
  Source *src;       /* The object to be returned */
  double ra,dec;     /* The Right Ascension and declination of the source */
  double pmra,pmdec; /* The proper motion in Right Ascension and Declination */
  double mag;
  
  // Read the Right-ascension and declination of the source.

  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(input_sexagesimal(stream, 0, &ra))
    return bad_J2000Source(stream, name, "Right Ascension");
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(input_sexagesimal(stream, 0, &dec))
    return bad_J2000Source(stream, name, "Declination");
  if(input_skip_space(stream, 1, 0))
    return NULL;
  
  // Check for the optional proper motion parameters.

  if(stream->nextc != '\n') {
    if(input_sexagesimal(stream, 0, &pmra))
      return bad_J2000Source(stream, name, "RA proper motion");
    if(input_skip_space(stream, 1, 0))
      return NULL;
    if(input_sexagesimal(stream, 0, &pmdec))
      return bad_J2000Source(stream, name, "Dec proper motion");
    if(input_skip_space(stream, 1, 0))
      return NULL;
  } else {
    pmra = 0.0;
    pmdec = 0.0;
  };
  
  // Check for the optional magnitude/diam parameters

  if(stream->nextc != '\n') {
    if(input_double(stream, 0, &mag))
      return bad_J2000Source(stream, name, "Magnitude");
    if(input_skip_space(stream, 1, 0))
      return NULL;
  } else {
    mag = 10.0;
  };

  // Convert the Right Ascension and Declination values to radians.

  ra *= htor;
  dec *= dtor;
  pmra *= htor;
  pmdec *= dtor;
  
  // Encapsulate the source details.

  src = new_J2000Source(sc, name, ra, dec, pmra, pmdec, mag);
  if(!src)
    return NULL;
  
  // Add the source to the catalog.

  return add_Source(sc, src);
}

/*.......................................................................
 * This is a private syntax-error return function for read_J2000Source().
 *
 * Input:
 *  stream  InputStream *  The stream in which the syntax error was found.
 *  name           char *  The name of the source that couldn't be read.
 *  column         char *  The description of the bad element.
 * Output:
 *  return       Source *  Always NULL.
 */
static Source *bad_J2000Source(InputStream *stream, char *name, char *column)
{
  input_error(stream, 1, "The %s of J2000 source '%s' is invalid.\n",
	      column, name);
  input_error(stream, 1,
      "Use: J2000 name hh:mm:ss.s +dd:mm:ss.s [+hh:mm:ss.s +dd:mm:ss.s]\n");
  return NULL;
}

/*.......................................................................
 * Write the type-specific parameters of a J2000 source to an output stream.
 *
 * Input:
 *  sc      SourceCatalog *   The catalog to add the source to.
 *  stream   OutputStream *   The stream to write to.
 *  src            Source *   The J2000 source to be output.
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
static int write_J2000Source(SourceCatalog *sc, OutputStream *stream,
			     Source *src)
{
  if(output_sexagesimal(stream, "", 12, 0, 3, src->j2000.ra * rtoh) ||
     write_OutputStream(stream, "  ") ||
     output_sexagesimal(stream, "", 12, 0, 2, src->j2000.dec * rtod))
    return 1;
  return 0;
}

/*.......................................................................
 * Read the parameters of an ephemeris source from an input stream,
 * and add it to the source catalog.
 *
 * Input:
 *  sc      SourceCatalog *   The catalog to add the source to.
 *  stream    InputStream *   The stream to read from.
 *  name             char *   The name of the source.
 * Output:
 *  return         Source *   The new catalog entry of the source, or
 *                            NULL on error.
 */
static Source *read_EphemSource(SourceCatalog *sc, InputStream *stream,
				char *name)
{
  Source *src;   /* The object to be returned */
/*
 * Read the name of the ephemeris file into stream->work.
 */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(input_word(stream, 0, 0)) {
    input_error(stream, 1, "Missing file name for ephemeris source %s.\n",
		name);
    input_error(stream, 1, "Use: EPHEM source_name file_name\n");
    return NULL;
  };
/*
 * Allocate the source container.
 */
  src = new_EphemSource(sc, name, stream->work);
  if(!src)
    return NULL;
/*
 * Skip to what should be the end of the line.
 */
  if(input_skip_space(stream, 1, 0))
    return del_Source(sc, src);
/*
 * Add the source to the catalog.
 */
  return add_Source(sc, src);
}

/*.......................................................................
 * Write the type-specific parameters of an ephemeris source to
 * an output stream.
 *
 * Input:
 *  sc      SourceCatalog *   The catalog to add the source to.
 *  stream   OutputStream *   The stream to write to.
 *  src            Source *   The ephemeris source to be output.
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
static int write_EphemSource(SourceCatalog *sc, OutputStream *stream,
			     Source *src)
{
  if(write_OutputStream(stream, src->ephem.file))
    return 1;
  return 0;
}

/*.......................................................................
 * Read the parameters of an Azimuth/Elevation source from an input stream,
 * and add it to the source catalog.
 *
 * Input:
 *  sc      SourceCatalog *   The catalog to add the source to.
 *  stream    InputStream *   The stream to read from.
 *  name             char *   The name of the source.
 * Output:
 *  return         Source *   The new catalog entry of the source, or
 *                            NULL on error.
 */
static Source *read_FixedSource(SourceCatalog *sc, InputStream *stream,
				char *name)
{
  unsigned axis_mask; /* A bitwise union of source.h::SourceAxes enumerators */
                      /*  specifying which of the following axes are */
                      /*  significant to this source. */
  double az,el,dk;    /* The azimuth, elevation and deck angle of the source */
  Source *src;        /* The object to be returned */
  
  // No axes have been specified yet.

  axis_mask = 0;
  
  // See if there is an azimuth axis position.

  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(stream->nextc=='*') {
    az = 0.0;
    if(input_skip_space(stream, 1, 1))
      return bad_FixedSource(stream, name, "Azimuth");
  } else {
    if(input_sexagesimal(stream, 0, &az))
      return bad_FixedSource(stream, name, "Azimuth");
    axis_mask |= Axis::AZ;
  };
  
  // See if there is an elevation axis position.

  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(stream->nextc=='*') {
    el = 0.0;
    if(input_skip_space(stream, 1, 1))
      return bad_FixedSource(stream, name, "Elevation");
  } else {
    if(input_sexagesimal(stream, 0, &el))
      return bad_FixedSource(stream, name, "Elevation");
    axis_mask |= Axis::EL;
  };
  
  //------------------------------------------------------------
  // See if there is a deck axis position.

  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(stream->nextc == '*') {
    dk = 0.0;
    if(input_skip_space(stream, 1, 1))
      return bad_FixedSource(stream, name, "Deck-angle");

    // End of line means no further axes were specified

  } else if (stream->nextc == '\n') {
    dk = 0.0;
  } else {
    if(input_sexagesimal(stream, 0, &dk))
      return bad_FixedSource(stream, name, "Deck-angle");
    axis_mask |= Axis::PA;
  };
  if(input_skip_space(stream, 1, 0))
    return NULL;
/*
 * Convert the angles to radians.
 */
  az *= dtor;
  el *= dtor;
  dk *= dtor;
/*
 * There must be at least one specified axis.
 */
  if(axis_mask == 0) {
    input_error(stream, 1,
		"Fixed source %s doesn't specify any axis positions.\n",
		name);
    return NULL;
  };
/*
 * Instantiate the source.
 */
  src = new_FixedSource(sc, name, axis_mask, az, el, dk);
  if(!src)
    return NULL;
/*
 * Add the source to the catalog.
 */
  return add_Source(sc, src);
}

/*.......................................................................
 * This is a private syntax-error return function for read_FixedSource().
 *
 * Input:
 *  stream  InputStream *  The stream in which the syntax error was found.
 *  name           char *  The name of the source that couldn't be read.
 *  column         char *  The description of the bad element.
 * Output:
 *  return       Source *  Always NULL.
 */
static Source *bad_FixedSource(InputStream *stream, char *name, char *column)
{
  input_error(stream, 1, "The %s of fixed source '%s' is invalid.\n",
	      column, name);
  input_error(stream, 1, "Use: FIXED name hh:mm:ss.s dd:mm:ss.s +dd:mm:ss.s\n");
  return NULL;
}

/*.......................................................................
 * Write the type-specific parameters of a Azimuth/Elevation source to
 * an output stream.
 *
 * Input:
 *  sc      SourceCatalog *   The catalog to add the source to.
 *  stream   OutputStream *   The stream to write to.
 *  src            Source *   The Fixed source to be output.
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
static int write_FixedSource(SourceCatalog *sc, OutputStream *stream,
			    Source *src)
{
/*
 * Write the azimuth field.
 */
  if(src->fixed.axis_mask & Axis::AZ ?
     output_sexagesimal(stream, "", 12, 0, 3, src->fixed.az * rtoh) :
     write_OutputStream(stream, "*"))
    return 1;
/*
 * Write a field separator.
 */
  if(write_OutputStream(stream, "  "))
    return 1;
/*
 * Write the elevation field.
 */
  if(src->fixed.axis_mask & Axis::EL ?
     output_sexagesimal(stream, "", 12, 0, 2, src->fixed.el * rtod) :
     write_OutputStream(stream, "*"))
    return 1;
/*
 * Write a field separator.
 */
  if(write_OutputStream(stream, "  "))
    return 1;
/*
 * Write the azimuth field.
 */
  if(src->fixed.axis_mask & Axis::PA ?
     output_sexagesimal(stream, "", 12, 0, 2, src->fixed.dk * rtod) :
     write_OutputStream(stream, "*"))
    return 1;
  return 0;
}

/*.......................................................................
 * Read the parameters of an Azimuth/Elevation source from an input stream,
 * and add it to the source catalog.
 *
 * Input:
 *  sc      SourceCatalog *   The catalog to add the source to.
 *  stream    InputStream *   The stream to read from.
 *  name             char *   The name of the source.
 * Output:
 *  return         Source *   The new catalog entry of the source, or
 *                            NULL on error.
 */
static Source *read_AliasSource(SourceCatalog *sc, InputStream *stream,
				char *name)
{
  Source *src;     /* The object to be returned */
/*
 * Read the name of the source to be aliased.
 */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(input_word(stream, 0, 0)) {
    input_error(stream, 1, "Missing source name for alias source %s.\n",
		name);
    input_error(stream, 1, "Use: ALIAS alias_name source_name\n");
    return NULL;
  };
/*
 * Instantiate the source.
 */
  src = new_AliasSource(sc, name, stream->work);
  if(!src)
    return NULL;
/*
 * Add the source to the catalog.
 */
  return add_Source(sc, src);
}

/*.......................................................................
 * Write the type-specific parameters of a Azimuth/Elevation source to
 * an output stream.
 *
 * Input:
 *  sc      SourceCatalog *   The catalog to add the source to.
 *  stream   OutputStream *   The stream to write to.
 *  src            Source *   The Alias source to be output.
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
static int write_AliasSource(SourceCatalog *sc, OutputStream *stream,
			    Source *src)
{
  if(write_OutputStream(stream, (*src->alias.sptr)->id.name))
    return 1;
  return 0;
}

/*.......................................................................
 * Return the source that is aliased by a specified source.
 *
 * Input:
 *  src      Source *  The aliasing source.
 * Output:
 *  return   Source *  The aliased source (not itself an alias), or
 *                     NULL if there were to many links to traverse.
 */
static Source *resolve_Aliases(Source *src)
{
  Source *s;   /* The source to be returned */
  int n;       /* The number of links traversed */
/*
 * Follow up to ALIAS_MAX_LINKS links.
 */
  for(s=src, n=0;
      s->id.type == SRC_ALIAS && n < ALIAS_MAX_LINKS;
      s = *s->alias.sptr, n++)
    ;
/*
 * Was the maximum number of links exceeded?
 */
  if(s->id.type == SRC_ALIAS) {
    lprintf(stderr, "Too many links while resolving alias source \"%s\".\n",
	    src->id.name);
    return NULL;
  };
/*
 * Return the aliased source.
 */
  return s;
}

/*.......................................................................
 * A public function that returns the topocentric Right Ascension,
 * Declination, Distance, Azimuth, Elevation, Parallactic Angle,
 * Rise and set times, and the rate of change of azimuth,elevation,
 * and parralactic angle.
 *
 * Input:
 *  sc   SourceCatalog *  The parent source catalog of the source.
 *  site          Site *  The location of the observer (see astrom.h).
 *  src         Source *  The source to examine.
 *  utc         double    The UTC for which the information is needed (MJD),
 *                        or -1 for the current time.
 *  horizon     double    The elevation of the horizon to use for
 *                        rise/set calculations (radians).
 *  options   unsigned    A bitwise union of SrcInfoOpt inquiry options,
 *                        chosen from:
 *                         SIO_EQUAT   - Topocentric equatorial coordinates.
 *                                       These will be recorded in info->ra
 *                                       and info->dec.
 *                         SIO_HORIZ   - Topocentric horizon coordinates
 *                                       and rates, recorded in info->horiz.
 *                         SIO_ALMANAC - Source rise and set times, recorded
 *                                       in info->{sched,rise,set}.
 * Input/Output:
 *  info    SourceInfo *  On output *info will contain the requested
 *                       information about the source.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
int source_info(SourceCatalog *sc, Site *site, Source *src, double utc,
		double horizon, unsigned options, SourceInfo *info)
{
  double eqneqx;   /* The equation of the equinoxes at 'utc' */
/*
 * Check arguments.
 */
  if(!sc || !site || !src || !info) {
    lprintf(stderr, "source_info: NULL argument(s).\n");
    return 1;
  };
/*
 * Resolve any symbolic links.
 */
  src = resolve_Aliases(src);
  if(!src)
    return 1;
/*
 * Substitute the current time?
 */
  if(utc < 0.0 && (utc = current_mjd_utc()) < 0.0)
    return 1;
/*
 * Initialiaze the information information structure.
 */
  info->utc = utc;
/*
 * Get the UT1 and Terrestrial Time equivalents of 'utc'.
 */
  if(sc_get_ut1(sc, utc, &info->ut1) ||
     sc_get_tt(sc, utc, &info->tt))
    return 1;
/*
 * Get the equation of the equinoxes for time, tt.
 */
  if(sc_get_eqneqx(sc, info->tt, &eqneqx))
    return 1;
/*
 * Get the local apparent sidereal time.
 */
  info->lst = slaRanorm(slaGmst(info->ut1) + eqneqx + site->longitude);
/*
 * Set defaults for optional parameters.
 */
  info->ra = 0.0;
  info->dec = 0.0;
  info->raMean = 0.0;
  info->decMean = 0.0;
  info->axis_mask = 0;
  info->coord.az = info->coord.el = info->coord.pa = 0.0;
  info->rates.az = info->rates.el = info->rates.pa = 0.0;
  info->sched = SRC_IRREGULAR;
  info->rise = info->set = 0.0;
/*
 * Get the horizon coordinates of the source.
 */
  switch(src->id.type) {
  case SRC_FIXED:
    if(FixedSource_info(sc, site, &src->fixed, horizon, options, info))
      return 1;
    break;
  case SRC_J2000:
    if(J2000Source_info(sc, site, &src->j2000, horizon, options, info))
      return 1;
    break;
  case SRC_EPHEM:
    if(EphemSource_info(sc, site, &src->ephem, horizon, options, info))
      return 1;
    break;
  default:
    lprintf(stderr, "source_info: Unknown source type.\n");
    return 1;
    break;
  };
  return 0;
}

/*.......................................................................
 * Return topocentric information about a fixed source.
 *
 * Input:
 *  sc   SourceCatalog *  The parent source catalog of the source.
 *  site          Site *  The location of the observer.
 *  fixed  FixedSource *  The source to examine.
 *  horizon     double    The elevation of the horizon to use for
 *                        rise/set calculations (radians).
 *  options   unsigned    A bitwise union of SrcInfoOpt inquiry options.
 * Input/Output:
 *  info    SourceInfo *  On output *info will contain the requested
 *                        information about the source.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int FixedSource_info(SourceCatalog *sc, Site *site, FixedSource *fixed,
			    double horizon, unsigned options, SourceInfo *info)
{
/*
 * Fill in as much of the information as possible.
 */
  info->axis_mask = fixed->axis_mask;
  info->coord.az = fixed->az;
  info->coord.el = fixed->el;
  info->coord.pa = fixed->dk;
  info->rates.az = 0.0;
  info->rates.el = 0.0;
  info->rates.pa = 0.0;
  info->sched = fixed->axis_mask & Axis::EL && fixed->el < horizon ?
    SRC_NEVER_RISES : SRC_NEVER_SETS;
  info->rise = 0.0;
  info->set = 0.0;
/*
 * Optionally compute the topocentric Right Ascension and Declination that
 * correspond to the fixed azimuth and elevation.
 */
  if(options & SIO_EQUAT) {
    horizon_to_equatorial(info->lst, site, sin(fixed->el), cos(fixed->el),
			  sin(fixed->az), cos(fixed->az),
			  &info->ra, &info->dec);
  };
  return 0;
}

/*.......................................................................
 * Return topocentric information about a j2000 source.
 *
 * Input:
 *  sc   SourceCatalog *  The parent source catalog of the source.
 *  site          Site *  The location of the observer.
 *  j2000  J2000Source *  The source to examine.
 *  horizon     double    The elevation of the horizon to use for
 *                        rise/set calculations (radians).
 *  options   unsigned    A bitwise union of SrcInfoOpt inquiry options.
 * Input/Output:
 *  info    SourceInfo *  On output *info will contain the requested
 *                        information about the source.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int J2000Source_info(SourceCatalog *sc, Site *site, J2000Source *j2000,
			    double horizon, unsigned options, SourceInfo *info)
{
  double ha;               /* The hour angle of the source */
/*
 * Update the cached source-independent precession and nutation parameters
 * if necessary.
 */
  if(update_mapqk_cache(sc->mapqk, info->tt))
    return 1;
/*
 * Precess the J2000 coordinates to the current epoch.
 */
  if(j2000->pmra==0.0 && j2000->pmdec==0.0)
    slaMapqkz(j2000->ra, j2000->dec, sc->mapqk->mappa, &info->ra, &info->dec);
  else
    slaMapqk(j2000->ra, j2000->dec, j2000->pmra, j2000->pmdec,
	     0.0, 0.0, sc->mapqk->mappa, &info->ra, &info->dec);

  //  Store the mean positions too

  info->raMean  = j2000->ra;
  info->decMean = j2000->dec;
  
  // Compute the hour angle of the source.

  ha = info->lst - info->ra;
  
  // Optionally compute the horizon coordinates and rates that
  // correspond to the specified Right Ascension and Declination. Note
  // that we need horizontal coordinates to generate the almanac.

  if(options & SIO_HORIZ || options & SIO_ALMANAC) {
    equatorial_to_horizon(site, 0.0, 0.0, 0.0, sin(ha), cos(ha), sin(info->dec),
			  cos(info->dec), &info->coord, &info->rates);
  };
  
  // Optionally get the rise and set times of the source.

  if(options & SIO_ALMANAC) {
    if(ra_dec_visibility(sc, site, (Source *) j2000, horizon, info))
      return 1;
  };
  
  // All axes are relevant.

  info->axis_mask = SRC_ALL_AXES;
  return 0;
}

/*.......................................................................
 * Return topocentric information about an ephemeris source.
 *
 * Input:
 *  sc   SourceCatalog *  The parent source catalog of the source.
 *  site          Site *  The location of the observer.
 *  ephem  EphemSource *  The source to examine.
 *  horizon     double    The elevation of the horizon to use for
 *                        rise/set calculations (radians).
 *  options   unsigned    A bitwise union of SrcInfoOpt inquiry options.
 * Input/Output:
 *  info    SourceInfo *  On output *info will contain the requested
 *                        information about the source.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int EphemSource_info(SourceCatalog *sc, Site *site, EphemSource *ephem,
			    double horizon, unsigned options, SourceInfo *info)
{
  double dist;             /* The geocentric distance of the source */
  double ha;               /* The hour angle of the source */
  double pmra, pmdec;      /* The proper motion in RA and Dec (radians/sec) */
/*
 * Interpolate the ephemeris of the source, for time info->tt.
 */
  if(sc_get_ephem(sc, ephem, info->tt, &info->ra, &info->dec, &dist,
		  &pmra, &pmdec))
    return 1;
/*
 * Compute the hour angle of the source.
 */
  ha = info->lst - info->ra;
/*
 * Optionally compute the horizon coordinates and rates that correspond to the
 * specified Right Ascension and Declination. Note that we need
 * horizontal coordinates to generate the almanac. If the distance is none
 * zero then it is necessary to compute horizon coordinates in order to
 * correct the equatorial coordinates for parallax.
 */
  if((options & SIO_HORIZ || options & SIO_ALMANAC) ||
     (options & SIO_EQUAT && dist>0.0)) {
    equatorial_to_horizon(site, dist, pmra, pmdec, sin(ha), cos(ha),
			  sin(info->dec), cos(info->dec),
			  &info->coord, &info->rates);
/*
 * If the distance is non-zero then the Right Ascension and Declination
 * need to be adjusted to account for horizontal parallax. Note that
 * ra_dec_visibility requires topocentric ra,dec.
 */
    if(dist > 0.0 && (options & SIO_EQUAT || options & SIO_ALMANAC)) {
      horizon_to_equatorial(info->lst, site, sin(info->coord.el),
			    cos(info->coord.el), sin(info->coord.az),
			    cos(info->coord.az), &info->ra, &info->dec);
    };
  };
  
  // Optionally get the rise and set times of the source.

  if(options & SIO_ALMANAC) {
    if(ra_dec_visibility(sc, site, (Source *) ephem, horizon, info))
      return 1;
  };
  
  // All axes are relevant.

  info->axis_mask = SRC_ALL_AXES;

  // Get the J2000 coordinates corresponding to these apparent coords

  sza::util::HourAngle raApp, raMean;
  sza::util::DecAngle decApp, decMean;
  sza::util::TimeVal date(info->tt);

  raApp.setRadians(info->ra);
  decApp.setRadians(info->dec);

  sza::util::Astrometry::apparentToJ2000Place(raApp, decApp, date, raMean, decMean);

  info->raMean = raMean.radians();
  info->decMean = decMean.radians();

  return 0;
}

/*.......................................................................
 * Determine the visibility of a J2000 or ephemeris source.
 *
 * Input:
 *  sc   SourceCatalog *  The parent catalog of the source.
 *  site          Site *  The location of the observer.
 *  src         Source *  The source to examine.
 *  horizon     double    The elevation of the horizon above which the
 *                        source would be visible.
 * Input/Output:
 *  info    SourceInfo *  The source information. On input all but the
 *                        sched, rise and set members must be set. On
 *                        output these members will be initialized.
 * Output:
 *  return         int    0 - OK.
 *                        1 - Error.
 */
static int ra_dec_visibility(SourceCatalog *sc, Site *site, Source *src,
			     double horizon, SourceInfo *info)
{
  double set_ha;       /* The hour angle at which the source sets */
  double rise_ha;      /* The hour angle at which the source rises */
  double ha;           /* The hour angle of the source at time info->utc */
/*
 * Compute the hour angle of the source.
 */
  ha = slaRanorm(info->lst - info->ra);
/*
 * Predict the visibility of the source using the topocentric
 * equatorial coordinates for time info->utc.
 */
  switch(dec_visibility(site, horizon, info->dec, &set_ha)) {
  case DEC_ABOVE_HORIZON:
    info->sched = SRC_NEVER_SETS;
    info->rise = info->set = 0.0;
    break;
  case DEC_BELOW_HORIZON:
    info->sched = SRC_NEVER_RISES;
    info->rise = info->set = 0.0;
    break;
  case DEC_SPANS_HORIZON:
    info->sched = SRC_ALTERNATES;
/*
 * Get the earliest rise time conducive with the returned hour angle.
 */
    rise_ha = -set_ha;
    if(rise_ha < ha)
      rise_ha += twopi;
/*
 * Find the hour angle at which the source next sets after 'utc'.
 */
    if(set_ha < ha)
      set_ha += twopi;
/*
 * Convert the hour angles to UTC times.
 */
    info->rise = info->utc + (rise_ha - ha) * mst_to_ut / twopi;
    info->set = info->utc + (set_ha - ha) * mst_to_ut / twopi;
/*
 * Refine the rise time to account for precession and proper motion.
 * The result will be left in info->rise.
 */
    if(sc_refine_crossing_time(sc, site, src, 1, horizon, info))
	return 1;
/*
 * If the above refinement didn't mark the source as irregular
 * refine the source's set time.
 */
    if(info->sched == SRC_ALTERNATES &&
       sc_refine_crossing_time(sc, site, src, 0, horizon, info))
      return 1;
    return 0;
    break;
  default:
    lprintf(stderr,
	    "ra_dec_visibility: Bad return value from dec_visibility().\n");
    return 1;
    break;
  };
  return 0;
}

/*.......................................................................
 * Home in on the rise or set time of an ephemeris source using the
 * estimated times estimated by ra_dec_visibility() in *info.
 *
 * Input:
 *  sc     SourceCatalog *  The parent catalog of the source.
 *  site            Site *  The location of the observer.
 *  src           Source *  The source being examined.
 *  do_rise          int    If true refine the rise time of the source.
 *                          If false refine its set time.
 *  horizon       double    The elevation of the horizon.
 * Input/Output:
 *  info      SourceInfo *  The source information to use and modify.
 * Output:
 *  return           int    0 - OK.
 *                          1 - Error.
 */
static int sc_refine_crossing_time(SourceCatalog *sc, Site *site, Source *src,
				   int do_rise, double horizon,
				   SourceInfo *info)
{
  SourceInfo trial;        /* Source info at the estimated rise/set time */
  double offset;           /* The error in the elevation (el-horizon) */
  double last_offset = halfpi; /* The residual error of the last iteration */
  int n;                   /* The number of iterations so far */
/*
 * Perform up to MAX_HORIZON_ITER iterations before giving up.
 */
  for(n=0; n < MAX_HORIZON_ITER; n++) {
    double next_utc = do_rise ? info->rise : info->set;
/*
 * Get the position of the source at the estimated rise time.
 * Be careful not to ask for the almanac, because that might
 * produce an infinite loop.
 */
    if(source_info(sc, site, src, next_utc, horizon, SIO_EQUAT | SIO_HORIZ,
		   &trial))
      return 1;
/*
 * Compute the error in the elevation.
 */
    offset = trial.coord.el - horizon;
/*
 * Use the elevation tracking rate to make an improved estimate of
 * the crossing time. If the predicted rise/set time is before the
 * date for which the user asked for subsequent rise/set times, then
 * restart the search at the same sidereal time, but a day in the future.
 */
    if(do_rise) {
      info->rise -= offset / (trial.rates.el * daysec);
      if(info->rise < info->utc) {
	info->rise = info->rise + mst_to_ut;
	last_offset = halfpi;
	continue;
      };
    } else {
      info->set -= offset / (trial.rates.el * daysec);
      if(info->set < info->utc) {
	info->set = info->set + mst_to_ut;
	last_offset = halfpi;
	continue;
      };
    };
/*
 * If the elevation estimate is within 1 degree of the horizon then
 * the rise time is sufficiently accurate, so stop iterating.
 */
    if(fabs(offset) <= dtor)
      return 0;
/*
 * If the elevation of the new estimate is the same or worse than that
 * of the previous estimate then mark the source as irregular.
 */
    if(fabs(offset) >= fabs(last_offset) ||
       trial.rates.el == 0.0) {
      info->sched = SRC_IRREGULAR;
      info->rise = info->set = 0.0;
      return 0;
    };
/*
 * Record the new elevation as the comparison elevation for the next
 * iteration.
 */
    last_offset = offset;
  };
/*
 * The maximum number of iterations was exceeded, so mark the source
 * as irregular.
 */
  info->sched = SRC_IRREGULAR;
  info->rise = info->set = 0.0;
  return 0;
}

/*.......................................................................
 * This is a private intermediate function for computing the topocentric
 * Right Ascension and declination that corresponds to a given azimuth
 * and elevation.
 *
 * Input:
 *  lst     double   The target local apparent sidereal time.
 *  sin_lat double   sin(latitude).
 *  cos_lat double   cos(latitude).
 *  sin_el  double   sin(elevation)
 *  cos_el  double   cos(elevation)
 *  sin_az  double   sin(azimuth)
 *  cos_az  double   cos(azimuth)
 * Input/Output:
 *  ra,dec  double * The topocentric Right Ascension and Declination
 *                   will be assigned to *ra and *dec.
 */
static void horizon_to_equatorial(double lst, Site *site,
		  double sin_el, double cos_el, double sin_az, double cos_az,
		  double *ra, double *dec)
{
/*
 * Get local copies of multiple use site data.
 */
  double sin_lat = site->sin_lat;
  double cos_lat = site->cos_lat;
/*
 * Compute pertinent spherical trig equations.
 */
  double sin_dec = sin_lat * sin_el + cos_lat * cos_el * cos_az;
  double sin_az_cos_el = sin_az * cos_el;
  double cos_dec_cos_ha = sin_el * cos_lat - sin_lat * cos_el * cos_az;
/*
 * Compute the hour angle of the source.
 */
  double ha = (sin_az_cos_el != 0.0 || cos_dec_cos_ha != 0.0) ?
    atan2(-sin_az_cos_el, cos_dec_cos_ha) : 0.0;
/*
 * Convert the hour angle to Right Ascension.
 */
  *ra = slaRanorm(lst - ha);
  *dec = asin(sin_dec);
  return;
}

/*.......................................................................
 * This is a private intermediate function for computing the
 * azimuth and elevation that corresponds to a given topocentric
 * Right Ascension and declination.
 *
 * Input:
 *  site          Site *  The observer's location.
 *  dist        double    The distance of the source (AU) or 0 if
 *                        outside the solar system.
 *  pmra        double    Proper motion in Right Ascension (radians/sec).
 *  pmdec       double    Proper motion in Declination (radians/sec).
 *  sin_ha      double    sin(hour angle)
 *  cos_ha      double    cos(hour angle)
 *  sin_dec     double    sin(declination)
 *  cos_dec     double    cos(declination)
 * Input/Output:
 *  coord SourceAzElPa *  The container into which to place the
 *                        azimuth, elevation and parallactic angle
 *                        coordinates.
 *  rates SourceAzElPa *  The container into which to place the
 *                        azimuth, elevation and parallactic angle
 *                        rates.
 */
static void equatorial_to_horizon(Site *site, double dist,
				  double pmra, double pmdec,
				  double sin_ha, double cos_ha,
				  double sin_dec, double cos_dec,
				  SourceAzElPa *coord, SourceAzElPa *rates)
{
/*
 * Get local copies of multiple use site data.
 */
  double sin_lat = site->sin_lat;
  double cos_lat = site->cos_lat;
/*
 * Compute the rate of change of hour angle wrt sidereal time in
 * radians of sidereal time per second of UT1.
 */
  double dhdt = rot_to_ut * twopi / daysec - pmra;
/*
 * Compute pertinent spherical trig equations.
 */
  double cos_el_cos_az = sin_dec * cos_lat - cos_dec * sin_lat * cos_ha;
  double cos_el_sin_az = -cos_dec * sin_ha;
  double cos2_el = cos_el_cos_az*cos_el_cos_az + cos_el_sin_az*cos_el_sin_az;
/*
 * Get the geocentric elevation and its trig forms from the above equations.
 */
  double cos_el = sqrt(cos2_el);
  double sin_el = sin_dec * sin_lat + cos_dec * cos_lat * cos_ha;
  coord->el = asin(sin_el);
/*
 * Compute the geocentric azimuth.
 */
  coord->az = (cos_el_cos_az==0.0 && cos_el_sin_az==0.0) ?
    0.0 : slaRanorm(atan2(cos_el_sin_az, cos_el_cos_az));
/*
 * Adjust the elevation to account for horizontal parallax.
 */
  if(dist > 0.0 && cos_el != 0.0) {
    coord->el -= atan2(cos_el, dist/site->rcent - sin_el);
    cos_el = cos(coord->el);
    sin_el = sin(coord->el);
  };
/*
 * Compute the parallactic angle.
 */
  {
    double cos_az = cos(coord->az);
    double sin_az_cos_lat = sin(coord->az) * cos_lat;
    double cos_dec_cos_pa = sin_lat * cos_el - cos_lat * sin_el*cos_az;
    coord->pa = (sin_az_cos_lat == 0.0 && cos_dec_cos_pa == 0.0) ?
      0.0 : atan2(-sin_az_cos_lat, cos_dec_cos_pa); 
/*
 * Compute the move rates of each axis.
 */
    if(cos_el != 0.0) {
      double cos_pa = cos(coord->pa);
      double sin_pa = sin(coord->pa);
      rates->el = dhdt * sin_az_cos_lat + pmdec * cos_pa;
      rates->az = (dhdt * cos_dec_cos_pa + pmdec * sin_pa) / cos_el;
      rates->pa = (dhdt * -cos_lat * cos_az + pmdec * sin_pa * sin_el)/cos_el -
	pmdec * sin_dec * sin_pa * sin_pa;
    } else {
      rates->el = rates->az = rates->pa = 0.0;
    };
  };
  return;
}

/*.......................................................................
 * A public function that returns the geocentric Right Ascension and
 * Declination of a J2000 source precessed to Terrestrial Dynamical Time
 * 'tt'. An error will be returned if 'src' is not a J2000 source,
 * so be sure to use get_SourceId() to determine the type of the
 * source before making this call.
 *
 * Input:
 *  sc    SourceCatalog *   The parent catalog of the source.
 *  src          Source *   The J2000 source to precess.
 *  tt           double     The TerrestrUTC for which to compute the Right
 *                          Ascension and Declination, or -1 to use
 *                          the current time.
 * Input/Output:
 *  ra,dec       double *   The precessed Right Ascension and
 *                          Declination (radians).
 * Output:
 *  return          int     0 - OK.
 *                          1 - Error.                  
 */
int precess_J2000_source(SourceCatalog *sc, Source *src, double tt,
			 double *ra, double *dec)
{
  J2000Source *j2000;   /* The type-specific member of 'src' */
/*
 * Check the arguments.
 */
  if(!sc || !src || !ra || !dec) {
    lprintf(stderr, "precess_J2000_source: NULL argument(s).\n");
    return 1;
  };
/*
 * Resolve source aliases.
 */
  src = resolve_Aliases(src);
  if(!src)
    return 1;
/*
 * Make sure that the source is a J2000 source.
 */
  if(src->id.type != SRC_J2000) {
    lprintf(stderr, "precess_J2000_source: Not a J2000 source.\n");
    return 1;
  };
/*
 * Substitute the current terrestrial time?
 */
  if(tt < 0.0 && (tt = current_mjd_tt()) < 0.0)
    return 1;
/*
 * Get the type-specific version of 'src'.
 */
  j2000 = &src->j2000;
/*
 * Update the cached source-independent precession and nutation parameters
 * if necessary.
 */
  if(update_mapqk_cache(sc->mapqk, tt))
    return 1;
/*
 * Precess the J2000 coordinates to the current epoch.
 */
  if(j2000->pmra==0.0 && j2000->pmdec==0.0)
    slaMapqkz(j2000->ra, j2000->dec, sc->mapqk->mappa, ra, dec);
  else
    slaMapqk(j2000->ra, j2000->dec, j2000->pmra, j2000->pmdec,
	     0.0, 0.0, sc->mapqk->mappa, ra, dec);
  return 0;
}

/*.......................................................................
 * Get the coordinates of a fixed source. Note that an error will be
 * returned if the source is not a fixed source.
 *
 * Input:
 *  src          Source *  The fixed source to describe.
 * Input/Output:
 *  coord  SourceAzElPa *  The horizon coordinates of the source. 
 *  axis_mask  unsigned *  A bitwise union of source.h::SourceAxes enumerators
 *                         specifying which of the following axes are
 *                         pertinent to this source.
 * Output:
 *  return          int    0 - OK.
 *                         1 - Error.                  
 */
int describe_fixed_source(Source *src, SourceAzElPa *coord, unsigned *axis_mask)
{
  FixedSource *fixed;   /* The type-specific member of 'src' */
/*
 * Check the arguments.
 */
  if(!src || !coord) {
    lprintf(stderr, "describe_fixed_source: NULL argument(s).\n");
    return 1;
  };
/*
 * Resolve source aliases.
 */
  src = resolve_Aliases(src);
  if(!src)
    return 1;
/*
 * Make sure that the right source type has been provided.
 */
  if(src->id.type != SRC_FIXED) {
    lprintf(stderr, "describe_fixed_source: Not a fixed source.\n");
    return 1;
  };
/*
 * Get the type-specific version of 'src'.
 */
  fixed = &src->fixed;
/*
 * Initialize the return container.
 */
  coord->az = fixed->az;
  coord->el = fixed->el;
  coord->pa = fixed->dk;
  *axis_mask = fixed->axis_mask;
  return 0;
}

/*.......................................................................
 * Return non-zero if a given character is valid within a source name.
 */
int valid_source_char(int c)
{
  return isalnum(c) || c=='+' || c=='-' || c=='.';
}

unsigned nSource(SourceCatalog* sc)
{
  return sc->nsource;
}
