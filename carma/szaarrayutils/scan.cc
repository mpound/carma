#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <limits.h>

#include "carma/szaarrayutils/lprintf.h"

#include "carma/szaarrayutils/scan.h"
#include "carma/szaarrayutils/freelist.h"
#include "carma/szaarrayutils/hash.h"
#include "carma/szaarrayutils/szaconst.h"
#include "carma/szaarrayutils/slalib.h"
#include "carma/szaarrayutils/astrom.h"
#include "carma/szaarrayutils/pathname.h"

#include "carma/szaarrayutils/rtcnetcoms.h"
#include "carma/szaarrayutils/scancache.h"

using namespace sza::array;

/*
 * Set an upper limit on the number of symbolic links in a given alias
 * source path.
 */
#define ALIAS_MAX_LINKS 10

static Scan *read_NormalScan(ScanCatalog *sc, InputStream *stream,
			     char *name);
static int write_NormalScan(ScanCatalog *sc, OutputStream *stream,
			    Scan *scan);
static Scan *new_NormalScan(ScanCatalog *sc, char *name, char *file);

static int read_ephemeris(ScanCatalog *sc, NormalScan *normal);
static int bad_ephemeris(InputStream *input, NormalScan *ephem);
static int read_ephem_entry(InputStream *input, int last_index, ScanCacheEntry *ee);
static int bad_ephem_entry(InputStream *stream, char *column);


static Scan *new_BogusScan(ScanCatalog *sc, char *name);
static Scan *read_BogusScan(ScanCatalog *sc, InputStream *stream,
			    char *name);
static int write_BogusScan(ScanCatalog *sc, OutputStream *stream,
			   Scan *scan);

static Scan *resolve_Aliases(Scan *scan);
static Scan *new_AliasScan(ScanCatalog *sc, char *name, char *scan);
static Scan *read_AliasScan(ScanCatalog *sc, InputStream *stream,
			    char *name);
static int write_AliasScan(ScanCatalog *sc, OutputStream *stream,
			   Scan *scan);

static Scan *add_Scan(ScanCatalog *sc, Scan *scan);
static Scan *del_Scan(ScanCatalog *sc, Scan *scan);
static void ini_ScanId(Scan *scan, ScanType type);
static int set_ScanName(Scan *scan, char *name);

/*
 * Associate enumerated scan-types with the names that they are
 * known by in scan-catalog files. ScanType enumerators index
 * into this array.
 */
static char *scan_type_name[] = {
  "normal",   /* SCAN_NORMAL */
  "alias",    /* SCAN_ALIAS */
  "bogus",    /* SCAN_BOGUS */
};

static const int num_scan_type = sizeof(scan_type_name)/sizeof(scan_type_name[0]);

/*
 * Set the number of hash-buckets in the scan catalog hash table.
 * For best results this should be a prime number.
 */
#define SCAN_HASH_SIZE  1531

/*
 * The array of catalog entries is split into lists of sub-arrays.
 * This allows for efficient expansion of the catalog by adding
 * segments as needed. Set the number of scans per sub-array.
 */
#define CAT_SEG_SIZE 256

typedef struct  ScanCatalogSegment ScanCatalogSegment;
struct ScanCatalogSegment {
  ScanCatalogSegment *next; /* The next array segment */
  Scan *s[CAT_SEG_SIZE];    /* The sub-array of scans */
  int nscan;                /* The number of scans in this segment */
};

static Scan **get_scan_slot(ScanCatalog *sc, int number);

/*
 * Define a scan catalog container. This is typedef'd to
 * ScanCatalog in scan.h.
 */
struct ScanCatalog {
  InputStream *scan_input;   /* This is used for reading scan files */
  FreeList *normal_mem;      /* Memory for normal scans */
  FreeList *alias_mem;       /* Memory for alias scans */
  FreeList *bogus_mem;       /* Memory for bogus scans */
  ScanCatalogSegment *head;  /* The head of a list of catalog segments */
  HashTable *hash;           /* A symbol-table of the catalog scans */
  int nscan;                 /* The total number of scans in the catalog */
};

/**.......................................................................
 * Add an ephemeris scan to a scan catalog.
 *
 * Input:
 *  sc   ScanCatalog *   The scan catalog to which to add the scan.
 *  name          char *   The name of the scan.
 *  file          char *   The name of the ephemeris file.
 * Output:
 *  return      Scan *   The new scan, or NULL on error.
 */
Scan *add_NormalScan(ScanCatalog *sc, char *name, char *file)
{
  Scan *scan;  /* The new scan */
  /*
   * Create the scan and initialize it from the scan file.
   */
  scan = new_NormalScan(sc, name, file);
  /*
   * Add the scan to the catalog.
   */
  return scan ? add_Scan(sc, scan) : NULL;
}

/**.......................................................................
 * Add a bogus scan to a scan catalog.
 *
 * Input:
 *  sc   ScanCatalog *   The scan catalog to which to add the scan.
 *  name          char *   The name of the scan.
 *  file          char *   The name of the ephemeris file.
 * Output:
 *  return      Scan *   The new scan, or NULL on error.
 */
Scan *add_BogusScan(ScanCatalog *sc, char *name)
{
  Scan *scan;  /* The new scan */
  /*
   * Create the scan and initialize it from the scan file.
   */
  scan = new_BogusScan(sc, name);

  /*
   * Add the scan to the catalog.
   */
  return scan ? add_Scan(sc, scan) : NULL;
}

/**.......................................................................
 * Add an alias scan to a scan catalog.
 *
 * Input:
 *  sc   ScanCatalog *   The scan catalog to which to add the scan.
 *  name          char *   The name of the scan.
 *  scan        char *   The name of the scan that is to be used
 *                         whenever this scan is referred to.
 * Output:
 *  return      Scan *   The new scan, or NULL on error.
 */
Scan *add_AliasScan(ScanCatalog *sc, char *name, char *scan)
{
  /*
   * Create the scan.
   */
  Scan *scn = new_AliasScan(sc, name, scan);
  if(!scan)
    return NULL;
  /*
   * Add the scan to the catalog.
   */
  return add_Scan(sc, scn);
}

/*.......................................................................
 * Add a scan to a scan catalog. If a scan of the same name already
 * exists, replace it at the same catalog number.
 *
 * Input:
 *  sc   ScanCatalog *  The catalog to add the scan to.
 *  scan         Scan *  The scan to be added.
 * Output:
 *  return      Scan *  The successfully added scan or NULL on
 *                        error.
 */
static Scan *add_Scan(ScanCatalog *sc, Scan *scan)
{
  ScanCatalogSegment *seg;  /* The catalog segment to add the scan to */
  Symbol *sym;          /* A scan-catalog hash table entry */
  int i;
  /*
   * Check the header of the scan.
   */
  if(scan->id.number != -1) {
    lprintf(stderr, "add_Scan(%s): Scan already in catalog.\n",
	    scan->id.name);
    return NULL;
  };
  /*
   * If a scan of the same name already exists simply replace it.
   */
  sym = find_HashSymbol(sc->hash, scan->id.name);
  if(sym) {
    scan->id.number = ((Scan* )sym->data)->id.number;
    (void) del_Scan(sc, (Scan* )sym->data);
    sym->data = scan;
    *get_scan_slot(sc, scan->id.number) = scan;
    return scan;
  };
  /*
   * Find the last block of scans in the scan catalog.
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
  if(!seg || seg->nscan >= CAT_SEG_SIZE) {
    ScanCatalogSegment *nseg;  /* The new segment */
    /*
     * Attempt to allocate a new catalog segment.
     */
    nseg = (ScanCatalogSegment* )malloc(sizeof(ScanCatalogSegment));
    if(!nseg) {
      lprintf(stderr, "add_Scan: Insufficient memory.\n");
      return del_Scan(sc, scan);
    };
    /*
     * Initialize the new segment.
     */
    nseg->next = NULL;
    nseg->nscan = 0;
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
   * Add the scan-list entry to the hash-table of the catalog.
   */
  if(!new_HashSymbol(sc->hash, scan->id.name, 0, 0, scan, 0))
    return del_Scan(sc, scan);
  /*
   * Assign the scan to the next free element of the array of scans.
   */
  seg->s[seg->nscan++] = scan;
  scan->id.number = sc->nscan++;
  return scan;
}

/*.......................................................................
 * Allocate a new alias scan.
 *
 * Input:
 *  sc   ScanCatalog *   The scan catalog to which to add the scan.
 *  name          char *   The name of the scan.
 *  scan        char *   The name of the scan that is to be used
 *                         whenever this scan is referred to.
 * Output:
 *  return      Scan *   The new scan, or NULL on error.
 */
static Scan *new_AliasScan(ScanCatalog *sc, char *name, char *scan)
     
{
  Scan *scn;          /* The object to be returned */
  Scan *old;          /* The scan to be aliased by scan */
  /*
   * Check the arguments.
   */
  if(!sc || !name || !scan) {
    lprintf(stderr, "new_AliasScan: NULL argument(s).\n");
    return NULL;
  };
  /*
   * Lookup the existing scan.
   */
  old = find_ScanByName(sc, scan);
  if(!old) {
    lprintf(stderr, "new_AliasScan: Scan \"%s\" not in catalog.\n", scan);
    return NULL;
  };
  /*
   * Allocate the container.
   */
  scn = (Scan* )new_FreeListNode("new_AliasScan", sc->alias_mem);
  /*
   * Before attempting any opertion that might fail, initialize the
   * scan at least up to the point at which it can safely be passed to
   * del_Scan().
   */
  ini_ScanId(scn, SCAN_ALIAS);
  scn->alias.sptr = get_scan_slot(sc, old->id.number);
  /*
   * Domain check and record the name of the scan.
   */
  if(set_ScanName(scn, name))
    return del_Scan(sc, scn);
  return scn;
}

/*.......................................................................
 * Allocate a new bogus scan.
 *
 * Input:
 *  sc   ScanCatalog *   The scan catalog to which to add the scan.
 *  name          char *   The name of the scan.
 *  scan        char *   The name of the scan that is to be used
 *                         whenever this scan is referred to.
 * Output:
 *  return      Scan *   The new scan, or NULL on error.
 */
static Scan *new_BogusScan(ScanCatalog *sc, char *name)
     
{
  Scan *scn;          /* The object to be returned */
  /*
   * Check the arguments.
   */
  if(!sc || !name) {
    lprintf(stderr, "new_BogusScan: NULL argument(s).\n");
    return NULL;
  };
  /*
   * Allocate the container.
   */
  scn = (Scan* )new_FreeListNode("new_BogusScan", sc->bogus_mem);
  /*
   * Before attempting any opertion that might fail, initialize the
   * scan at least up to the point at which it can safely be passed to
   * del_Scan().
   */
  ini_ScanId(scn, SCAN_BOGUS);
  /*
   * Domain check and record the name of the scan.
   */
  if(set_ScanName(scn, name))
    return del_Scan(sc, scn);
  return scn;
}

/*.......................................................................
 * Allocate a new normal scan.
 *
 * Input:
 *  sc   ScanCatalog *   The scan catalog to which to add the scan.
 *  name          char *   The name of the scan.
 *  file          char *   The path name of the ephemeris file.
 * Output:
 *  return      Scan *   The new scan, or NULL on error.
 */
static Scan *new_NormalScan(ScanCatalog *sc, char *name, char *file)
{
  Scan *scan;        /* The object to be returned */
  NormalScan *normal; /* A pointer to scan->normal */
  int i;

  /*
   * Check the arguments.
   */
  if(!name || !file) {
    lprintf(stderr, "new_NormalScan: NULL argument(s).\n");
    return NULL;
  };
  /*
   * Allocate the container.
   */
  scan = (Scan* )new_FreeListNode("new_NormalScan", sc->normal_mem);
  if(!scan)
    return NULL;
  /*
   * Before attempting any opertion that might fail, initialize the
   * scan at least up to the point at which it can safely be passed to
   * del_Scan().
   */
  ini_ScanId(scan, SCAN_NORMAL);
  scan->normal.file         = NULL;
  scan->normal.offsets.size = 0;

  /*
   * Domain check and record the name of the scan.
   */
  if(set_ScanName(scan, name))
    return del_Scan(sc, scan);

  /*
   * Get a pointer to the pertinent scan member.
   */
  normal = &scan->normal;

  /*
   * Allocate a copy of the file name.
   */
  normal->file = (char* )malloc(strlen(file) + 1);
  if(!normal->file) {
    lprintf(stderr,
	    "new_NormalScan(%s): Insufficient memory for file name.\n", name);
    return del_Scan(sc, scan);
  };

  strcpy(normal->file, file);

  /*
   * Clear the cache entries.
   */
  normal->offsets.size = 0;
  for(i=0; i<SCAN_CACHE_SIZE; i++) {
    ScanCacheEntry* ee = normal->offsets.cache + i;
    ee->index = 0;
    ee->azoff = ee->eloff = ee->dkoff = 0.0;
  };

  /*
   * Read the ephemeris of the scan.
   */
  if(read_ephemeris(sc, normal))
    return del_Scan(sc, scan);
  return scan;
}

/*.......................................................................
 * Read up to EPHEM_CACHE_SIZE entries of a scan ephemeris file
 * into memory.
 *
 * Each non-blank line of the ephemeris must contain the following
 * fields.
 *
 *   index azoff eloff dkoff
 *
 * Where index is a monotonically inreasing index, and azoff, eloff,
 * dkoff are all sexagesiimal offsets in degrees.  (In fact, we don't
 * even need the index, bcause I could just generate it as I read the
 * file in, but it might make it easier to find errors in the file.)
 *
 * Comments can appear on any line, starting with a # character and
 * extending to the end of the line.
 *
 * Input:
 *  sc     ScanCatalog *  The parent catalog of the scan.
 *  scan   NormalScan  *  The scan to update.
 *  input  InputStream *  The stream from which to read the ephemeris.
 *
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error.
 */
static int read_ephemeris(ScanCatalog *sc, NormalScan *normal)
     
{
  InputStream *input;    /* A local alias of sc->scan_input */
  int nrec;              /* The number of entries read so far */
  ScanCacheEntry* cache; /* A local alias of s->cache */
  int update;            /* True if this is an update */

  /*
   * Are we updating an existing cache?
   */
  update = normal->offsets.size != 0;

  /*
   * Open the ephemeris file.
   */
  input = sc->scan_input;
  if(open_FileInputStream(input, "", normal->file))
    return bad_ephemeris(NULL, normal);
  lprintf(stdout, "%s %s pattern from %s\n",
	  update ? "Updating":"Initializing",
	  normal->id.name, normal->file);

  /*
   * Get a pointer to the start of the cache buffer.
   */
  cache = normal->offsets.cache;
  
  /*
   * Find the ephemeris entry of the file.
   */
  if(input_skip_white(input, 1, 0))
    return bad_ephemeris(input, normal);
  
  /*
   * Read further entries into the cache until either NORMAL_CACHE_SIZE
   * entries have been read or the end of the file is reached.
   */

  if(read_ephem_entry(input, -1, &cache[0]))
    return bad_ephemeris(input, normal);
  else
    normal->offsets.size++;;  
  
  for(nrec=1; nrec < SCAN_CACHE_SIZE && input->nextc!=EOF; nrec++) {
    ScanCacheEntry *ne = cache + nrec;
    if(read_ephem_entry(input, ne[-1].index, ne))
      return bad_ephemeris(input, normal);
    else
      normal->offsets.size++;;  
  }
  
  /*
   * Close the ephemeris file.
   */
  close_InputStream(input);

  /*
   * Emit an error if no valid entries were found.
   */
  if(nrec == 0) 
    return bad_ephemeris(NULL, normal);

  return 0;
}

/*.......................................................................
 * This is the private error return function of read_ephemeris().
 *
 * Input:
 *  input  InputStream *  The stream that is connected to the ephemeris
 *                        file (NULL if not yet opened).
 *  ephem  EphemScan *  The incomplete ephemeris.
 * Output:
 *  return         int    1 (the error return code of read_ephemeris()).
 */
static int bad_ephemeris(InputStream *input, NormalScan *normal)
{
  /*
   * Close the ephemeris file.
   */
  if(input)
    close_InputStream(input);

  /*
   * Discard the incomplete ephemeris.
   */
  normal->offsets.size = 0;

  /*
   * Return the error-return code of read_ephemeris().
   */
  return 1;
}

/*.......................................................................
 * Read one entry from a scan ephemeris. Note that the stream pointer
 * must already point at the first non-white-space character of the
 * record.  On successful returns the stream pointer will be left
 * pointing at the first non-white-space character of the next record.
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
static int read_ephem_entry(InputStream *input, int last_index, 
			    ScanCacheEntry *ee)
{
  /**
   * Read the next record.
   */
  if(input_ulong(input, 0, 0, &ee->index))
    return bad_ephem_entry(input, "Index");

  if(input_skip_space(input, 1, 0))
    return 1;

  if(input_sexagesimal(input, 0, &ee->azoff))
    return bad_ephem_entry(input, "sexagesimal AZ offset");

  if(input_skip_space(input, 1, 0))
    return 1;

  if(input_sexagesimal(input, 0, &ee->eloff))
    return bad_ephem_entry(input, "sexagesimal EL offset");

  if(input_skip_space(input, 1, 0))
    return 1;

  if(input_sexagesimal(input, 0, &ee->dkoff))
    return bad_ephem_entry(input, "sexagesimal DK offset");

  if(input_skip_white(input, 1, 0))
    return 1;

  /**
   * The ephemeris is supposed to be in ascending order of index.
   */
  if((signed)ee->index != last_index+1)
    return input_error(input, 1, "The ephemeris is not in index order.\n");
  
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
  input_error(input, 1, "Use: index dd:mm:ss.s dd:mm:ss.s\n");
  return 1;
}


/*.......................................................................
 * Initialize the generic scan header of a scan. This is intended
 * for use by scan constructor functions.
 *
 * Input:
 *  scan     Scan *  The scan whose header is to be initialized.
 * Output:
 *  return     int    0 - OK.
 *                    1 - Error.
 */
static void ini_ScanId(Scan *scan, ScanType type)
{
  scan->id.type = type;
  scan->id.name[0] = '\0';
  scan->id.number = -1;
}

/*.......................................................................
 * Check and record the name of a scan.
 *
 * Input:
 *  scan       Scan *  The scan whose name is to be set.
 *  name        char *  The name of the scan. This must have <=
 *                      SCAN_NAME_MAX characters (including the '\0').
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */
static int set_ScanName(Scan *scan, char *name)
{
  char *cptr;  /* A pointer into name[] */
  /*
   * Check the arguments.
   */
  if(!scan || !name) {
    lprintf(stderr, "set_ScanName: NULL argument(s).\n");
    return 1;
  };
  /*
   * Check the validity of the scan name.
   */
  if(name[0]=='\0') {
    lprintf(stderr, "set_ScanName: No scan name provided.\n");
    return 1;
  };
  /*
   * Only allow printable characters other than spaces and comment characters.
   */
  for(cptr=name; *cptr; cptr++) {
    if(!valid_scan_char(*cptr)) {
      lprintf(stderr,
	      "set_ScanName: Illegal character '%c' found in scan name.\n",
	      *cptr);
      return 1;
    };
  };
  /*
   * Make sure that the scan name doesn't excede the size accomodated
   * by ScanId::name[].
   */
  if(strlen(name) >= SCAN_NAME_MAX) {
    lprintf(stderr, "set_ScanName(%s): Scan name too long (>%d).\n", name,
	    SCAN_NAME_MAX - 1);
    return 1;
  };
  /*
   * Give the scan its name.
   */
  strcpy(scan->id.name, name);
  return 0;
}

/*.......................................................................
 * Delete a scan.
 *
 * Input:
 *  sc    ScanCatalog *  The scan catalog that contained the scan.
 *  scan          Scan *  The scan to be deleted.
 * Output:
 *  return       Scan *  Allways NULL.
 */
static Scan *del_Scan(ScanCatalog *sc, Scan *scan)
{
  if(scan) {
    switch(scan->id.type) {
    case SCAN_NORMAL:
      if(scan->normal.file)
	free(scan->normal.file);
      scan = (Scan* )del_FreeListNode("del_Scan", sc->normal_mem, scan);
      break;
    case SCAN_ALIAS:
      scan = (Scan* )del_FreeListNode("del_Scan", sc->alias_mem, scan);
      break;
    case SCAN_BOGUS:
      scan = (Scan* )del_FreeListNode("del_Scan", sc->bogus_mem, scan);
      break;
    };
  };
  return NULL;
}

/*.......................................................................
 * Get a public copy of the scan identification header of a scan.
 *
 * Input:
 *  scan     Scan *   The scan to identify.
 *  resolve    int     If this is true and the scan is an alias to
 *                     another scan, return the id of the aliased
 *                     scan in its place.
 * Input/Output:
 *  id    ScanId *   The output container for the ID.
 * Output:
 *  return     int     0 - OK.
 *                     1 - Error.
 */
int get_ScanId(Scan *scan, int resolve, ScanId *id)
{
  if(!scan || !id) {
    lprintf(stderr, "get_ScanId: NULL argument(s).\n");
    return 1;
  };
  if(resolve && (scan=resolve_Aliases(scan)) == NULL)
    return 1;
  *id = scan->id;
  return 0;
}

/*.......................................................................
 * Return the catalog index of the given scan.
 *
 * Input:
 *  scan     Scan *   The scan to identify.
 *  resolve    int     If this is true and the scan is an alias to
 *                     another scan, return the id of the aliased
 *                     scan in its place.
 * Output:
 *  return     int     The catalog index, or -1 on error.
 */
int get_Scan_number(Scan *scan, int resolve)
{
  if(!scan) {
    lprintf(stderr, "get_Scan_number: NULL argument(s).\n");
    return -1;
  };
  if(resolve && (scan=resolve_Aliases(scan)) == NULL)
    return -1;
  return scan->id.number;
}

/*.......................................................................
 * Create an empty scan catalog.
 *
 * Output:
 *  return  ScanCatalog *  The new catalog, or NULL on error.
 */
ScanCatalog *new_ScanCatalog(void)
{
  ScanCatalog *sc;  /* The return container */

  /*
   * Allocate the container.
   */
  sc = (ScanCatalog *) malloc(sizeof(ScanCatalog));
  if(!sc) {
    lprintf(stderr, "new_ScanCatalog: Insufficient memory.\n");
    return NULL;
  };
  
  /*
   * Before attempting any operation that might fail, initialize the
   * container at least up to the point at which it can safely be
   * passed to del_ScanCatalog().
   */
  sc->scan_input = NULL;
  sc->normal_mem = NULL;
  sc->alias_mem = NULL;
  sc->bogus_mem = NULL;
  sc->head = NULL;
  sc->hash = NULL;
  sc->nscan = 0;
  
  /*
   * Allocate an input stream to be used when reading ephemerides.
   */
  sc->scan_input = new_InputStream();
  if(!sc->scan_input)
    return del_ScanCatalog(sc);
  
  /*
   * Allocate free-lists for each of the supported types of scan.
   */
  sc->normal_mem = new_FreeList("new_ScanCatalog", sizeof(NormalScan), 20);
  if(!sc->normal_mem)
    return del_ScanCatalog(sc);
  sc->alias_mem = new_FreeList("new_ScanCatalog", sizeof(AliasScan), 32);
  if(!sc->alias_mem)
    return del_ScanCatalog(sc);
  sc->bogus_mem = new_FreeList("new_ScanCatalog", sizeof(BogusScan), 1);
  if(!sc->bogus_mem)
    return del_ScanCatalog(sc);
  
  /*
   * Create the scan hash-table.
   */
  sc->hash = new_HashTable(NULL, SCAN_HASH_SIZE, IGNORE_CASE, NULL, 0);
  if(!sc->hash)
    return del_ScanCatalog(sc);
  
  /*
   * Return the empty catalog.
   */
  return sc;
}

/*.......................................................................
 * Delete a scan catalog.
 *
 * Input:
 *  sc     ScanCatalog *  A scan catalog to be deleted.
 * Output:
 *  return ScanCatalog *  Allways NULL.
 */
ScanCatalog *del_ScanCatalog(ScanCatalog *sc)
{
  int i;
  if(sc) {
    sc->scan_input = del_InputStream(sc->scan_input);
    
    /*
     * Delete the list of catalog segments.
     */
    while(sc->head) {
      
      /*
       * Remove the head of the list.
       */
      ScanCatalogSegment *seg = sc->head;
      sc->head = seg->next;
      
      /*
       * Delete the scans of the current array segment.
       */
      for(i=0; i<seg->nscan; i++)
	del_Scan(sc, seg->s[i]);
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
     * Discard the scan freelists.
     */
    sc->normal_mem = del_FreeList("del_ScanCatalog", sc->normal_mem, 1);
    sc->alias_mem = del_FreeList("del_ScanCatalog", sc->alias_mem, 1);
    sc->bogus_mem = del_FreeList("del_ScanCatalog", sc->bogus_mem, 1);
    
    /*
     * Discard the catalog container.
     */
    free(sc);
  };
  
  return NULL;
}

/*.......................................................................
 * Return the current number of scans in a scan catalog.
 *
 * Input:
 *  sc      ScanCatalog *  The scan catalog.
 * Output:
 *  return            int    The number of scans.
 */
int size_ScanCatalog(ScanCatalog *sc)
{
  return sc ? sc->nscan : 0;
}

/*.......................................................................
 * A public function that finds a scan in a scan-catalog by its name.
 *
 * Input:
 *  sc    ScanCatalog *  The scan-catalog to be searched.
 *  name           char *  The name of the scan to be looked up.
 * Output:
 *  return       Scan *  A copy of the catalog entry, or NULL if not
 *                         found.
 */
Scan *find_ScanByName(ScanCatalog *sc, char *name)
{
  Symbol *sym;  /* The hash-symbol that contains the scan */
  
  /*
   * Check arguments.
   */
  if(!sc || !name) {
    lprintf(stderr, "find_ScanByName: NULL argument.\n");
    return NULL;
  };
  
  /*
   * Find the hash-table entry of the scan.
   */
  sym = find_HashSymbol(sc->hash, name);
  if(!sym)
    return NULL;
  
  /*
   * Return the scan.
   */
  return (Scan* )sym->data;
}

/*.......................................................................
 * A public function that finds a scan in a scan-catalog by its
 * catalog index.
 *
 * Input:
 *  sc    ScanCatalog *  The scan-catalog to be searched.
 *  number         char *  The catalog-index of the scan to be looked up.
 * Output:
 *  return       Scan *  A copy of the catalog entry, or NULL if not
 *                         found.
 */
Scan *find_ScanByNumber(ScanCatalog *sc, int number)
{
  
  /*
   * Check arguments.
   */
  if(!sc) {
    lprintf(stderr, "find_ScanByNumber: NULL argument.\n");
    return NULL;
  };
  
  /*
   * Determine the position of the segment that contains the scan.
   */
  if(number < 0 || number >= sc->nscan) {
    lprintf(stderr, "find_ScanByNumber: Scan number %d not in catalog.\n",
	    number);
    return NULL;
  };
  
  /*
   * Return the scan.
   */
  return *get_scan_slot(sc, number);
}

/*.......................................................................
 * Lookup the catalog slot of a given scan.
 *
 * Input:
 *  sc    ScanCatalog *   The parent scan catalog.
 *  number          int     The scan number in the catalog. If this
 *                          wasn't taken from the header of a scan
 *                          in the catalog, then the caller should
 *                          verify that it lies between 0 and sc->nscan-1.
 * Output:
 *  return       Scan **  The catalog slot numbered 'number'.
 */
static Scan **get_scan_slot(ScanCatalog *sc, int number)
{
  int sn;               /* The segment number in which the scan lies */
  ScanCatalogSegment *seg;  /* The sn'th catalog segment */
  
  /*
   * Find the array segment that contains the slot.
   */
  sn = number / CAT_SEG_SIZE;
  for(seg=sc->head; sn; seg=seg->next, sn--)
    ;
  return seg->s + number % CAT_SEG_SIZE;
}

/*.......................................................................
 * A public function that reads scan catalog entries from a given input
 * stream.
 *
 * Input:
 *  sc      ScanCatalog *   The catalog in which to record the scans.
 *  stream    InputStream *   The stream to read from.
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
int input_ScanCatalog(ScanCatalog *sc, InputStream *stream)
{
  ScanId id;     /* A temporary scan identification header */
  int i;
  
  /*
   * Check the arguments.
   */
  if(!sc || !stream) {
    lprintf(stderr, "inputScanCatalog: NULL argument(s).\n");
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
     * Read the scan type.
     */
    if(input_word(stream, 0, 1)) {
      return input_error(stream, 1,
			 "Missing scan type. Should be J2000, EPHEM, FIXED, or ALIAS.\n");
    };
    
    /*
     * Identify the scan-type.
     */
    for(i=0; i<num_scan_type; i++) {
      if(strcmp(stream->work, scan_type_name[i]) == 0) {
	id.type = (ScanType) i;
	break;
      };
    };
    if(i >= num_scan_type) {
      return input_error(stream, 1,
			 "Unknown scan type '%s' in scan catalog.\n",
			 stream->work);
    };
    
    /*
     * Read the scan name.
     */
    if(input_skip_space(stream, 1, 0))
      return 1;
    if(input_word(stream, 0, 0))
      return input_error(stream, 1, "Missing scan name.\n");
    if(strlen(stream->work) >= SCAN_NAME_MAX) {
      return input_error(stream, 1, "Scan name '%s' too long.\n",
			 stream->work);
    };
    strcpy(id.name, stream->work);
    
    /*
     * Allocate a scan of the required type, read its parameters and
     * add it to the catalog.
     */
    switch(id.type) {
    case SCAN_NORMAL:
      if(!read_NormalScan(sc, stream, id.name))
	return 1;
      break;
    case SCAN_ALIAS:
      if(!read_AliasScan(sc, stream, id.name))
	return 1;
      break;
    case SCAN_BOGUS:
      if(!read_BogusScan(sc, stream, id.name))
	return 1;
      break;
    };
    
    /*
     * Consume any trailing spaces.
     */
    if(input_skip_space(stream, 1, 0))
      return 1;
    
    /*
     * Make sure that nothing else remains on the scan line.
     */
    if(stream->nextc != '\n' && stream->nextc != EOF) {
      return input_error(stream, 1,
			 "Unexpected characters follow a valid scan specification.\n");
    };
    
    /*
     * Locate the start of the next scan.
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
 * A public function that reads scan catalog entries from a given text file.
 *
 * Input:
 *  sc      ScanCatalog *   The catalog in which to record the scans.
 *  dir              char *   The directory if separate from the file name,
 *                            else "".
 *  file             char *   The path name of the text file to read from.
 *                            This will be appended to dir[] unless dir[]
 *                            is "".
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
int read_ScanCatalog(ScanCatalog *sc, char *dir, char *file)
{
  InputStream *input;   /* An input stream connected to the file */
  
  /*
   * Check arguments.
   */
  if(!sc || !file) {
    lprintf(stderr, "read_ScanCatalog: NULL argument(s).\n");
    return 1;
  };
  
  /*
   * Create an input stream.
   */
  input = new_InputStream();
  if(!input)
    return 1;
  
  /*
   * Connect the catalog file to an input stream and read scans from
   * it into the catalog.
   */
  if(open_FileInputStream(input, dir, file) ||
     input_ScanCatalog(sc, input)) {
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
 * A public function that writes the current scan catalog entries to an
 * output stream, using a format that is compatible with
 * input_ScanCatalog().
 *
 * Input:
 *  sc      ScanCatalog *   The catalog to be written.
 *  stream   OutputStream *   The stream to write to.
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
int output_ScanCatalog(ScanCatalog *sc, OutputStream *stream)
{
  ScanCatalogSegment *node; /* A node of the list of catalog-segments */
  int type_width;           /* The width of the scan-type field */
  int scan_width;           /* The width of the scan-name field */
  int i;
  
  /*
   * Check arguments.
   */
  if(!sc || !stream) {
    lprintf(stderr, "write_ScanCatalog: NULL argument(s).\n");
    return 1;
  };
  
  /*
   * Determine the length of the longest scan-type keyword.
   */
  type_width = 0;
  for(i=0; i<num_scan_type; i++) {
    int tmp_width = strlen(scan_type_name[i]);
    if(tmp_width > type_width)
      type_width = tmp_width;
  };
  
  /*
   * Determine the length of the longest scan name.
   */
  scan_width = 0;
  for(node=sc->head; node; node=node->next) {
    for(i=0; i<node->nscan; i++) {
      Scan *scan = node->s[i];
      int name_length = strlen(scan->id.name);
      if(name_length > scan_width)
	scan_width = name_length;
    };
  };
  
  /*
   * Write catalog entries from the scan array.
   */
  for(node=sc->head; node; node=node->next) {
    for(i=0; i<node->nscan; i++) {
      Scan *scan = node->s[i];
      int name_length = strlen(scan->id.name);
      int type_length = strlen(scan_type_name[scan->id.type]);
      /*
       * Write the scan type and scan name, padded with spaces up to the
       * associated field widths (plus one space to separate each field from
       * the next).
       */
      if(write_OutputStream(stream, scan_type_name[scan->id.type]) ||
	 output_spaces(stream, type_width - type_length + 2) ||
	 write_OutputStream(stream, scan->id.name) ||
	 output_spaces(stream, scan_width - name_length + 2))
	return 1;
      /*
       * Now write the type-specific parameters of the scan.
       */
      switch(scan->id.type) {
      case SCAN_NORMAL:
	if(write_NormalScan(sc, stream, scan))
	  return 1;
	break;
      case SCAN_ALIAS:
	if(write_AliasScan(sc, stream, scan))
	  return 1;
	break;
      case SCAN_BOGUS:
	if(write_BogusScan(sc, stream, scan))
	  return 1;
	break;
      };
      
      /*
       * Terminate the scan line.
       */
      if(write_OutputStream(stream, " \n"))
	return 1;
    };
  };
  
  return 0;
}

/*.......................................................................
 * A public function that writes a scan catalog to a given text
 * file, using the format expected by read_ScanCatalog().
 *
 * Input:
 *  sc      ScanCatalog *   The catalog in which to record the scans.
 *  dir              char *   The directory if separate from the file name,
 *                            else "".
 *  file             char *   The path name of the text file to write to.
 *                            This will be appended to dir[] unless dir[]
 *                            is "".
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
int write_ScanCatalog(ScanCatalog *sc, char *dir, char *file)
{
  OutputStream *output;   /* An output stream connected to the file */
  
  /*
   * Check arguments.
   */
  if(!sc || !file) {
    lprintf(stderr, "write_ScanCatalog: NULL argument(s).\n");
    return 1;
  };
  
  /*
   * Create an output stream.
   */
  output = new_OutputStream();
  if(!output)
    return 1;
  
  /*
   * Connect the catalog file to the output stream and write scans from
   * it into the catalog.
   */
  if(open_FileOutputStream(output, dir, file) ||
     output_ScanCatalog(sc, output)) {
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
 * Read the parameters of an normal scan from an input stream,
 * and add it to the scan catalog.
 *
 * Input:
 *  sc      ScanCatalog *   The catalog to add the scan to.
 *  stream    InputStream *   The stream to read from.
 *  name             char *   The name of the scan.
 * Output:
 *  return         Scan *   The new catalog entry of the scan, or
 *                            NULL on error.
 */
static Scan *read_NormalScan(ScanCatalog *sc, InputStream *stream,
			     char *name)
{
  Scan *scan;   /* The object to be returned */
  
  /*
   * Read the name of the normal file into stream->work.
   */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(input_word(stream, 0, 0)) {
    input_error(stream, 1, "Missing file name for normal scan %s.\n",
		name);
    input_error(stream, 1, "Use: NORMAL scan_name file_name\n");
    return NULL;
  };
  
  /*
   * Allocate the scan container.
   */
  scan = new_NormalScan(sc, name, stream->work);
  if(!scan)
    return NULL;
  
  /*
   * Skip to what should be the end of the line.
   */
  if(input_skip_space(stream, 1, 0))
    return del_Scan(sc, scan);
  
  /*
   * Add the scan to the catalog.
   */
  return add_Scan(sc, scan);
}

/*.......................................................................
 * Write the type-specific parameters of an normal scan to
 * an output stream.
 *
 * Input:
 *  sc       ScanCatalog  *   The catalog to add the scan to.
 *  stream   OutputStream *   The stream to write to.
 *  scan     Scan         *   The ephemeris scan to be output.
 *
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
static int write_NormalScan(ScanCatalog *sc, OutputStream *stream,
			    Scan *scan)
{
  if(write_OutputStream(stream, scan->normal.file))
    return 1;
  return 0;
}

/*.......................................................................
 * Write the type-specific parameters of an Bogus scan to
 * an output stream.
 *
 * Input:
 *  sc      ScanCatalog  * The catalog to add the scan to.
 *  stream  OutputStream * The stream to write to.
 *  scan    Scan         * The Bogus scan to be output.
 *
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
static int write_BogusScan(ScanCatalog *sc, OutputStream *stream,
			   Scan *scan)
{
  return 0;
}


/*.......................................................................
 * Read the parameters of an Azimuth/Elevation scan from an input stream,
 * and add it to the scan catalog.
 *
 * Input:
 *  sc      ScanCatalog *   The catalog to add the scan to.
 *  stream    InputStream *   The stream to read from.
 *  name             char *   The name of the scan.
 * Output:
 *  return         Scan *   The new catalog entry of the scan, or
 *                            NULL on error.
 */
static Scan *read_BogusScan(ScanCatalog *sc, InputStream *stream,
			    char *name)
{
  Scan *scan;     /* The object to be returned */
  
  /*
   * Read the name of the scan to be bogused.
   */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(input_word(stream, 0, 0)) {
    input_error(stream, 1, "Missing scan name for bogus scan %s.\n",
		name);
    input_error(stream, 1, "Use: BOGUS bogus_name\n");
    return NULL;
  };
  
  /*
   * Instantiate the scan.
   */
  scan = new_BogusScan(sc, name);
  if(!scan)
    return NULL;
  
  /*
   * Add the scan to the catalog.
   */
  return add_Scan(sc, scan);
}

/*.......................................................................
 * Read the parameters of an Azimuth/Elevation scan from an input stream,
 * and add it to the scan catalog.
 *
 * Input:
 *  sc      ScanCatalog *   The catalog to add the scan to.
 *  stream    InputStream *   The stream to read from.
 *  name             char *   The name of the scan.
 * Output:
 *  return         Scan *   The new catalog entry of the scan, or
 *                            NULL on error.
 */
static Scan *read_AliasScan(ScanCatalog *sc, InputStream *stream,
			    char *name)
{
  Scan *scan;     /* The object to be returned */
  
  /*
   * Read the name of the scan to be aliased.
   */
  if(input_skip_space(stream, 1, 0))
    return NULL;
  if(input_word(stream, 0, 0)) {
    input_error(stream, 1, "Missing scan name for alias scan %s.\n",
		name);
    input_error(stream, 1, "Use: ALIAS alias_name scan_name\n");
    return NULL;
  };
  
  /*
   * Instantiate the scan.
   */
  scan = new_AliasScan(sc, name, stream->work);
  if(!scan)
    return NULL;
  
  /*
   * Add the scan to the catalog.
   */
  return add_Scan(sc, scan);
}

/*.......................................................................
 * Write the type-specific parameters of an Alias scan to
 * an output stream.
 *
 * Input:
 *  sc      ScanCatalog  * The catalog to add the scan to.
 *  stream  OutputStream * The stream to write to.
 *  scan    Scan         * The Alias scan to be output.
 *
 * Output:
 *  return            int     0 - OK.
 *                            1 - Error.
 */
static int write_AliasScan(ScanCatalog *sc, OutputStream *stream,
			   Scan *scan)
{
  if(write_OutputStream(stream, (*scan->alias.sptr)->id.name))
    return 1;
  return 0;
}

/*.......................................................................
 * Return the scan that is aliased by a specified scan.
 *
 * Input:
 *  scan      Scan *  The aliasing scan.
 * Output:
 *  return   Scan *  The aliased scan (not itself an alias), or
 *                     NULL if there were to many links to traverse.
 */
static Scan *resolve_Aliases(Scan *scan)
{
  Scan *s;   /* The scan to be returned */
  int n;       /* The number of links traversed */
  
  /*
   * Follow up to ALIAS_MAX_LINKS links.
   */
  for(s=scan, n=0;
      s->id.type == SCAN_ALIAS && n < ALIAS_MAX_LINKS;
      s = *s->alias.sptr, n++)
    ;
  /*
   * Was the maximum number of links exceeded?
   */
  if(s->id.type == SCAN_ALIAS) {
    lprintf(stderr, "Too many links while resolving alias scan \"%s\".\n",
	    scan->id.name);
    return NULL;
  };
  
  /*
   * Return the aliased scan.
   */
  return s;
}

/*.......................................................................
 * Return the scan that is aliased by a specified scan.
 *
 * Input:
 *  scan      Scan *  The aliasing scan.
 * Output:
 *  return   Scan *  The aliased scan (not itself an alias), or
 *                     NULL if there were to many links to traverse.
 */
Scan *resolve_ScanAliases(Scan *scan)
{
  return resolve_Aliases(scan);
}

/*.......................................................................
 * Return non-zero if a given character is valid within a scan name.
 */
int valid_scan_char(int c)
{
  return isalnum(c) || c=='+' || c=='-' || c=='.';
}

/*.......................................................................
 * Return the time window over which the last scan offsets sent to the
 * real-time controller are valid.
 *
 * Input:
 *  scanc  SourceCatalog *  The parent source catalog.
 *  scan   Scan *           The scan to check.
 *  tt     double           The Terrestrial Time whose enclosing
 *                          window is to be returned (expressed as a MJD).
 *                          If you send -1, the current time will be used.
 * Input/Output:
 *
 *  win      CacheWindow *   The window of times over which the cache
 *                           is valid. The times are in TT.
 * Output:
 *  return           int     0 - OK.
 *                           1 - Bad source or unable to update ephemeris.
 */
int get_scan_window(ScanCatalog *scanc, Scan *scan, double tt,
		     CacheWindow *win)
{
  /*
   * Substitute the current time?
   */
  if(tt < 0.0 && (tt = current_mjd_tt()) < 0.0)
    return 1;

  /*
   * Return the time limits of the interpolation entries.
   */
  win->tmin = tt;
  win->tmax = tt + ((double)(SCAN_NET_NPT/2)) / daysec; /* tmax is in MJD */

  return 0;
}
