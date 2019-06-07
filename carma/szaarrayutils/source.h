#ifndef source_h
#define source_h

#include "carma/szaarrayutils/input.h"
#include "carma/szaarrayutils/output.h"
#include "carma/szaarrayutils/quad.h"
#include "carma/szaarrayutils/astrom.h"
#include "carma/szaarrayutils/cache.h"

/*
 * In all of the following functions time is specified in the form
 * of Modified Julian Dates (Julian Date - 2400000.5). If the time
 * is specified as -1.0, then the current time will automatically
 * be substituted. See astrom.h for time generation and manipulation
 * functions. Also note that in some places where one should strictly
 * use Barycentric Dynamical Time, Terrestrial Time is used instead.
 * The two time systems differ by at most a couple of milli-seconds.
 *
 * Thus arguments named utc denote Universal Coordinated Time's, whereas
 * arguments named tt denote Terrestrial Time.
 */

/*
 * The opaque SourceCatalog type contains a catalog of sources, indexed
 * by name and number. Its definition and implementation is private
 * to source.c.
 */
typedef struct SourceCatalog SourceCatalog;

/*
 * Create an empty source catalog.
 */
SourceCatalog *new_SourceCatalog(void);

/*
 * Delete a source catalog.
 */
SourceCatalog *del_SourceCatalog(SourceCatalog *sc);

/*
 * Read source catalog entries from an input stream. Note that
 * the new sources are added to the existing catalog. If a new source
 * has the same name as an existing catalog entry, the existing catalog
 * entry is overwritten with the new one.
 */
int input_SourceCatalog(SourceCatalog *sc, InputStream *stream);

/*
 * Read source catalog entries from a file. This is a wrapper around
 * input_SourceCatalog().
 */
int read_SourceCatalog(SourceCatalog *sc, char *dir, char *file);

/*
 * Write source catalog entries to an output stream, using the format
 * expected by input_SourceCatalog().
 */
int output_SourceCatalog(SourceCatalog *sc, OutputStream *stream);

/*
 * Write source catalog entries to a file. This is a wrapper around
 * output_SourceCatalog().
 */
int write_SourceCatalog(SourceCatalog *sc, char *dir, char *file);

/*
 * Specify a new UT1-UTC ephemeris file.
 */
int sc_set_ut1utc_ephemeris(SourceCatalog *sc, char *dir, char *file);

namespace sza {
  namespace array {
    
    typedef union SourceUnion Source;

    /*
     * Enumerate the supported types of sources.
     */
    enum SourceType {
      SRC_J2000,         /* A source at fixed J2000 RA and Dec */
      SRC_EPHEM,         /* An ephemeris source (eg. a planet) */
      SRC_FIXED,         /* A source at fixed azimuth and elevation */
      SRC_ALIAS,         /* An alternative name for another source */
      SRC_UNKNOWN        /* A type we can assign to unititalized
			    SourceType variables */
    };
    
    /*
     * Set the max length of a source name (including '\0').  This
     * should be set equal to SRC_LEN in szaregs.h.
     */
    enum {SRC_NAME_MAX=12};
    
    
    /*
     * All source types share an initial member of the following type.
     *
     * When a given source name is enterred into a SourceCatalog for the
     * first time, it is assigned a catalog index. This index can thereafter
     * be used as a unique synonym for the source name, since it doesn't change
     * even if the definition of the source associated with that name is
     * modified. 
     */
    struct SourceId {
      SourceType type;         /* The enumerated type of the source */
      char name[SRC_NAME_MAX]; /* The name of the source */
      int number;              /* The catalog index tied to name[] */
    };
    
    /*
     * The following type of source is specified at a fixed J2000.0 Right
     * Ascension and Declination.
     */
    struct J2000Source {
      SourceId id;  /* The generic source identification header */
      double ra;    /* The mean right-ascension at epoch J2000.0 (radians) */
      double dec;   /* The mean declination at epoch J2000.0 (radians) */
      float pmra;   /* The proper motion in Right Ascension (radians) */
      float pmdec;  /* The proper motion in Declination (radians) */
      float mag;    /* The magnitude of the star */ 
    };
    
    /*
     * The following type of source is located at a fixed azimuth and
     * elevation.
     */
    struct FixedSource {
      SourceId id;       /* The generic source identification header */
      unsigned axis_mask;/* A bitwise union of source.h::SourceAxes enumerators */
      /*  specifying which of the following axes are */
      /*  significant to this source. */
      double az;         /* The target azimuth (radians) */
      double el;         /* The target elevation (radians) */
      double dk;         /* The target deck-angle (radians) */
    };
    
    struct EphemEntry {
      double ra;         /* The geocentric apparent Right Ascension (radians) */
      double dec;        /* The geocentric apparent Declination (radians) */
      double dist;       /* The geocentric distance of the source (AU) */
      double tt;         /* The terrestrial time of the entry (MJD) */
    };
    
    /*
     * The following source type is for non-sidereal sources whos positions
     * are recorded in ephemeris files. The ephemeris file is not kept open
     * at all times, instead up to EPHEM_CACHE_SIZE contemporary entries are
     * kept in a cache. At any given time three points from this cache are
     * loaded into quadratic interpolators, to provide more precise positions.
     * The middle of these three entries is at element 'midpt' of the cache.
     * When the cache has been exhausted, it will be refreshed from the
     * ephemeris file.
     */
    struct EphemSource {
      SourceId id;        /* The generic source identification header */
      char *file;         /* The pathname of the ephemeris file */
      EphemEntry *cache;  /* A cache of size<=EPHEM_CACHE_SIZE entries */
      int size;           /* The number of elements in cache[] */
      int midpt;          /* The middle element of the interpolation triple */
      QuadPath *ra;       /* The Right Ascension interpolator */
      QuadPath *dec;      /* The Declination interpolator */
      QuadPath *dist;     /* The Distance interpolator */
    };
    
    /*
     * The following type of source is a symbolic link to another source.
     */
    struct AliasSource {
      SourceId id;       /* The generic source identification header */
      Source **sptr;     // The catalog entry of the source. Using
			 // this relies on the fact the only
			 // destructive operation that is allowed on
			 // an existing catalog entry is to replace
			 // its contents with a new source of the same
			 // name. We can't actually store the (Source
			 // *) pointer because that can change.
    };
    
    /*
     * The opaque Source datatype is a union of all source types.
     * Its definition is private to source.c. Be careful about
     * cacheing (Source *) pointers. If add_J2000Source() et al. is called
     * to change the definition of an existing source (identified by name),
     * then the original (Source *) pointer associated with that name will
     * become invalid and should not be used. It is safer to record catalog
     * numbers or source names (see get_SourceId).
     */
    union SourceUnion {
      SourceId id;        /* The first members of all source types */
      J2000Source j2000;  /* A source at fixed Ra and Dec (J2000) */
      EphemSource ephem;  /* An ephemeris source */
      FixedSource fixed;  /* A source at fixed azimuth and elevation */
      AliasSource alias;  /* A link to another source */
    };
    
    /*
     * Fixed sources directly set the position of one or more of the
     * azimuth, elevation and deck axes. The following enumerators
     * are used in a bitwise union to specify which of the axes are
     * specified by the source.
     */
    enum SourceAxes {
      SRC_AZ_AXIS = 1,          /* The source specifies the azimuth axis */
      SRC_EL_AXIS = 2,          /* The source specifies the elevation axis */
      SRC_DK_AXIS = 4,          /* The source specifies the deck axis */
      /* The source specifies all of the axes */
      SRC_ALL_AXES = SRC_AZ_AXIS | SRC_EL_AXIS | SRC_DK_AXIS
    };
  };
};

/*
 * The following function returns non-zero if a given character is
 * valid within a source name.
 */
int valid_source_char(int c);

/*
 * Get a copy of the identification header of a given Source.
 * On error it returns non-zero. If resolve is true and the
 * source is an alias to another source, the id of the aliased
 * source will be returned in its place (this rule is applied
 * recursively).
 */
int get_SourceId(sza::array::Source* src, int resolve, sza::array::SourceId *id);

/*
 * Return the source-catalog index of a given source (-1 on error).
 * See the documentation of SourceId for the usage of such indexes.
 * The resolve member is used as described for get_SourceId().
 */
int get_Source_number(sza::array::Source* src, int resolve);

/*
 * Use the following functions to add sources to the catalog.
 * Each function returns NULL on failure.
 */
sza::array::Source* add_J2000Source(SourceCatalog *sc, char *name, double ra, 
				    double dec, float pmra, float pmdec);
sza::array::Source* add_EphemSource(SourceCatalog *sc, char *name, char *file);
sza::array::Source* add_FixedSource(SourceCatalog *sc, char *name, 
				    unsigned axis_mask, double az, double el, 
				    double dk);
sza::array::Source* add_AliasSource(SourceCatalog *sc, char *name, char *source);

/*
 * Use the following functions to look up a source. The first is
 * useful for finding a source named by a user. It uses a hash table
 * so it should be relatively fast. The second uses the ordinal
 * position of the source in the catalog. This should be more
 * efficient for repeat lookups of the same source and can also be
 * used to step through the catalog. Note that if you have a SourceId
 * object, the ordinal number of the source is given by the 'number'
 * field.
 */
sza::array::Source* find_SourceByName(SourceCatalog *sc, char *name);
sza::array::Source* find_SourceByNumber(SourceCatalog *sc, int number);

/*
 * Return the number of sources that are currently in a source catalog.
 */
int size_SourceCatalog(SourceCatalog *sc);

/*
 * The following types are used with the source_visibility() function
 * to return the projected visibility of a given source with respect
 * to a specified horizon elevation.
 */
typedef enum {
  SRC_NEVER_RISES,  /* The source is always below the horizon */
  SRC_NEVER_SETS,   /* The source is always above the horizon */
  SRC_ALTERNATES,   /* The source periodically crosses the horizon */
  SRC_IRREGULAR     /* A source who's path is too different from */
  /*  sidereal for rise and set times to be determined. */
} SourceSched;

/*
 * The source_info() function takes a bit-wise union of the following
 * options for its "options" argument. This argument tells the
 * source_info() function which parameters you want it to compute.
 */
typedef enum {
  SIO_EQUAT = 1,   /* Topocentric equatorial coordinates */
  SIO_HORIZ = 2,   /* Topocentric horizon coordinates and rates */
  SIO_ALMANAC = 4, /* Rise and set times */
  /*
   * The following option selects all options.
   */
  SIO_ALL = SIO_EQUAT | SIO_HORIZ | SIO_ALMANAC
} SrcInfoOpt;

/*
 * Describe the structure that source_info() uses to return contemporary
 * information about a given source. All angles are in radians.
 */
typedef struct {
  double az;       /* The azimuth of a source (radians) */
  double el;       /* The elevation of a source (radians) */
  double pa;       /* The parallactic angle of a source (radians) */
} SourceAzElPa;

typedef struct {
  double utc;         /* The UTC for which this information was computed (MJD)*/
  double ut1;         /* The UT1 equivalent of 'utc' (MJD) */
  double tt;          /* The corresponding Terrestrial Time (MJD) */
  double lst;         /* The corresponding Local Apparent Sidereal Time, */
                      /*  measured in radians. */
  double ra,dec;      /* The topocentric apparent Right Ascension and Declination */
  double raMean,decMean;      /* The J2000 Right Ascension and Declination (where applicable) */
  unsigned axis_mask; /* For fixed sources this mask contains a bitwise */
                      /*  union of sza::util::Axis::Type enumerators
			  to specify which of the telescope axes are
			  specified by this source. */
  SourceAzElPa coord; /* The horizon coordinates of the source */
  SourceAzElPa rates; /* The az,el,pa motion of the source (radians/sec)*/
  SourceSched sched;  /* The visibility type of the source */
  double rise,set;    /* The rise and set times (UTC) of the source if */
                      /*  sched==SRC_ALTERNATES */
} SourceInfo;

/*
 * Fill in a provided SourceInfo structure with details about a given
 * source, as seen from a given site at a given time. The (Site *) datatype
 * needed by the "site" argument is defined in astrom.h and initialized with
 * set_Site(). The utc argument must be given either as a Modified Julian Date,
 * or as the value -1 which selects the current time. The horizon argument is
 * the local elevation of the horizon in radians. The options argument is a
 * bit-wise union of the SrcInfoOpt options defined above.
 */
int source_info(SourceCatalog *sc, sza::array::Site *site, 
		sza::array::Source* src, 
		double utc, double horizon, unsigned options, SourceInfo *info);

/*
 * Return the time window over which the 3 UT1-UTC interpolation
 * entries for the time 'utc' are usable. The returned time
 * limits are specified in UTC. If no UT1-UTC ephemeris has been
 * specified via sc_set_ut1utc_ephemeris() then win->tmin and win->tmax
 * will be set to 0.0.
 */
int get_ut1utc_window(SourceCatalog *sc, double utc, 
		      sza::array::CacheWindow *win);

/*
 * Return the contents of the quadratic interpolator of UT1-UTC at
 * Universal Coordinated Time, 'utc'. The QuadData x values are UTC
 * expressed as MJDs, and the y values are the values of UT1-UTC in
 * seconds.
 */
int get_ut1utc_QuadData(SourceCatalog *sc, double utc, 
			sza::array::QuadData *data);

/*
 * Return the time window over which the 3 equation-of-the-equinoxes
 * interpolation entries for the time 'tt' are usable.
 * The returned time limits are specified in TT.
 */
int get_eqneqx_window(SourceCatalog *sc, double tt, 
		      sza::array::CacheWindow *win);

/*
 * Return the contents of the quadratic interpolator of the equation of
 * the equinoxes at time 'tt'. The QuadData x values are TT times
 * expressed as MJDs, and the y values are the values of the equation
 * of the equinoxes, measured in radians.
 */
int get_eqneqx_QuadData(SourceCatalog *sc, double tt, 
			sza::array::QuadData *data);

/*
 * Given an ephemeris source, return the time window over which
 * the 3 ephemeris in its quadratic interpolators, initialized for
 * the Terrestrial Time 'tt', are usable. Since src must be an ephemeris
 * source, you should call get_SourceId() to check its type before calling
 * this function.
 *
 * The returned time limits are specified in TT.
 */
int get_ephem_window(SourceCatalog *sc, sza::array::Source* src, double tt,
		     sza::array::CacheWindow *win);

/*
 * Return the contents of the quadratic interpolators of a specified
 * ephemeris source, valid for the time 'tt'. Since src must be an
 * ephemeris source, you should call get_SourceId() to check its type
 * before calling this function. The QuadData x axis values are all
 * Terrestrial Times, expressed as MJDs. The ra interpolator y values
 * are geocentric Right Ascensions in radians, and the dec interpolator
 * y values are geocentric Declinations in radians. The dist interpolator
 * y values are geocentric source distances in AU. For sources who's
 * distances are unknown, 0 is substituted.
 */
int get_ephem_QuadData(SourceCatalog *sc, sza::array::Source* src, double tt,
		       sza::array::QuadData *ra, 
		       sza::array::QuadData *dec, 
		       sza::array::QuadData *dist);

/*
 * Return the time window over which the cached precession and nutation
 * parameters for time (tt) are usable for J2000 sources. These are
 * parameters used with the slalib slaMapqk/slaMapqkz functions to
 * precess the geocentric Right Ascension and declination of a source
 * from J2000 to the current epoch. The Right Ascensions of J2000 sources
 * are updated whenever this cache is updated.
 *
 * The returned time limits are specified in TT.
 */
int get_mapqk_window(SourceCatalog *sc, double tt, 
		     sza::array::CacheWindow *win);

/*
 * Return the contents of the precession/nutation parameter cache for
 * time 'tt'. See slaMapqk() and slaMapqkz() for details of how to use
 * the parameters in the cache.
 */
typedef struct {
  double mappa[21];  /* The array of cached parameters. */
                     /* This is the array of parameters returned by */
                     /*  the slalib slaMappa() function. */
} MapqkData;

int get_mapqk_data(SourceCatalog *sc, double tt, MapqkData *data);

/*
 * Return the geocentric Right Ascension and Declination of a 
 * J2000 source precessed to time 'tt'. An error will be returned
 * if 'src' is not a J2000 source, so be sure to use get_SourceId()
 * to determine the type of the source before making this call.
 */
int precess_J2000_source(SourceCatalog *sc, sza::array::Source* src, double tt,
			 double *ra, double *dec);

/*
 * Get the coordinates of a fixed source.
 */
int describe_fixed_source(sza::array::Source* src, SourceAzElPa *coord,
			  unsigned *axis_mask);

unsigned nSource(SourceCatalog* sc);

#endif
