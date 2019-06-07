#ifndef SCAN_H
#define SCAN_H

#include "carma/szaarrayutils/input.h"
#include "carma/szaarrayutils/output.h"
#include "carma/szaarrayutils/cache.h"
#include "carma/szaarrayutils/scancache.h"

/*
 * The opaque ScanCatalog type contains a catalog of scans, indexed
 * by name and number. Its definition and implementation is private
 * to scan.c.
 */
typedef struct ScanCatalog ScanCatalog;

/*
 * Create an empty scan catalog.
 */
ScanCatalog *new_ScanCatalog(void);

/*
 * Delete a scan catalog.
 */
ScanCatalog *del_ScanCatalog(ScanCatalog *sc);

/*
 * Read scan catalog entries from an input stream. Note that
 * the new scans are added to the existing catalog. If a new scan
 * has the same name as an existing catalog entry, the existing catalog
 * entry is overwritten with the new one.
 */
int input_ScanCatalog(ScanCatalog *sc, InputStream *stream);

/*
 * Read scan catalog entries from a file. This is a wrapper around
 * input_ScanCatalog().
 */
int read_ScanCatalog(ScanCatalog *sc, char *dir, char *file);

/*
 * Write scan catalog entries to an output stream, using the format
 * expected by input_ScanCatalog().
 */
int output_ScanCatalog(ScanCatalog *sc, OutputStream *stream);

/*
 * Write scan catalog entries to a file. This is a wrapper around
 * output_ScanCatalog().
 */
int write_ScanCatalog(ScanCatalog *sc, char *dir, char *file);

/*
 * The opaque Scan datatype is a union of all scan types.
 * Its definition is private to scan.c. Be careful about
 * cacheing (Scan *) pointers. If add_Scan() et al. is called
 * to change the definition of an existing scan (identified by name),
 * then the original (Scan *) pointer associated with that name will
 * become invalid and should not be used. It is safer to record catalog
 * numbers or scan names (see get_ScanId).
 */
namespace sza {
  namespace array {

    typedef union ScanUnion Scan;

    /*
     * Enumerate the supported types of scans
     */
    enum ScanType {
      SCAN_NORMAL,  /* All scans */
      SCAN_BOGUS,   /* A fake scan */
      SCAN_ALIAS    /* An alternative name for another scan */
    };

    /*
     * Set the max length of a scan name (including '\0').
     * This should be set equal to SCAN_LEN in cbiregs.h.
     */
    enum {SCAN_NAME_MAX=12};

    /*
     * All scans share an initial member of the following type.
     *
     * When a given scan name is enterred into a ScanCatalog for the
     * first time, it is assigned a catalog index. This index can thereafter
     * be used as a unique synonym for the scan name, since it doesn't change
     * even if the definition of the scan associated with that name is
     * modified. 
     */
    struct ScanId {
      ScanType type;            /* The enumerated type of the scan */
      char name[SCAN_NAME_MAX]; /* The name of the scan */
      int number;               /* The catalog index tied to name[] */
    };

    /*
     * The following scan type is for non-sidereal scans whos positions
     * are recorded in ephemeris files. The ephemeris file is not kept
     * open at all times, instead up to EPHEM_CACHE_SIZE contemporary
     * entries are kept in a cache. At any given time three points from
     * this cache are loaded into quadratic interpolators, to provide more
     * precise positions.  The middle of these three entries is at element
     * 'midpt' of the cache.  When the cache has been exhausted, it will
     * be refreshed from the ephemeris file.
     */
    struct  NormalScan {
      ScanId id;          /* The generic scan identification header */
      char *file;         /* The pathname of the scan file */
      ScanCache offsets;  /* A cache of size <= SCAN_CACHE_SIZE entries */
    };
    
    /**
     * The following type of scan is a symbolic link to another scan.
     */
    struct AliasScan {
      ScanId id;       /* The generic scan identification header */
      Scan **sptr;     /* The catalog entry of the scan. Using this relies */
      /*  on the fact the only destructive operation that */
      /*  is allowed on an existing catalog entry is */
      /*  to replace its contents with a new scan of the */
      /*  same name. We can't actually store the (Scan *) */
      /*  pointer because that can change. */
    };

    /*
     * This is just a placeholder. So that the scan catalog can contain
     * entries like "none"
     */
    struct BogusScan {
      ScanId id;          /* The generic scan identification header */
    };

    /*
     * Define a generic scan container. This is typedef'd to Scan
     * in scan.h.
     */
    union ScanUnion {
      ScanId id;          /* The first members of all scan types */
      NormalScan normal;  /* An ordinary scan */
      BogusScan  bogus;   /* A fake scan */
      AliasScan  alias;   /* A link to another scan */
    };
  };
};

/*
 * The following function returns non-zero if a given character is
 * valid within a scan name.
 */
int valid_scan_char(int c);

/*
 * Get a copy of the identification header of a given Scan.
 * On error it returns non-zero. If resolve is true and the
 * scan is an alias to another scan, the id of the aliased
 * scan will be returned in its place (this rule is applied
 * recursively).
 */
int get_ScanId(sza::array::Scan *scan, int resolve, sza::array::ScanId *id);

/*
 * Return the scan-catalog index of a given scan (-1 on error).
 * See the documentation of ScanId for the usage of such indexes.
 * The resolve member is used as described for get_ScanId().
 */
int get_Scan_number(sza::array::Scan *scan, int resolve);

/*
 * Use the following functions to add scans to the catalog.
 * Each function returns NULL on failure.
 */
sza::array::Scan *add_NormalScan(ScanCatalog *sc, char *name, char* file);
sza::array::Scan *add_BogusScan(ScanCatalog *sc, char *name);
sza::array::Scan *add_AliasScan(ScanCatalog *sc, char *name, char *scan);

/*
 * Use the following functions to look up a scan. The first is
 * useful for finding a scan named by a user. It uses a hash table
 * so it should be relatively fast. The second uses the ordinal
 * position of the scan in the catalog. This should be more
 * efficient for repeat lookups of the same scan and can also be
 * used to step through the catalog. Note that if you have a ScanId
 * object, the ordinal number of the scan is given by the 'number'
 * field.
 */
sza::array::Scan* find_ScanByName(ScanCatalog* sc, char* name);
sza::array::Scan* find_ScanByNumber(ScanCatalog* sc, int number);

/*
 * Return the number of scans that are currently in a scan catalog.
 */
int size_ScanCatalog(ScanCatalog* sc);

int get_scan_window(ScanCatalog* scanc, sza::array::Scan* scan, double tt,
		    sza::array::CacheWindow* win);

sza::array::Scan* resolve_ScanAliases(sza::array::Scan* scan);

#endif
