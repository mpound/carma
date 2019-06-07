#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "carma/szaarrayutils/lprintf.h"
#include "carma/szaarrayutils/astrom.h"
#include "carma/szaarrayutils/pathname.h"
#include "carma/szaarrayutils/arcfile.h"

/*
 * Parameterize the length of an archive-file extension name, the
 * number of digits used to encode the year, the number of digits
 * used to encode the day number, and the number used to encode the
 * number of seconds into the day.
 */
#define ARC_EXT_LEN 3   /* log or dat etc.. */
#define ARC_DATE_LEN 8  /* YYYYMMDD */
#define ARC_TIME_LEN 6  /* HHMMSS */

/*
 * Compute the length of an archive file name (YYYYMMDD_HHMMSS.EXT).
 */
#define ARC_FILE_LEN (ARC_DATE_LEN + 1 + ARC_TIME_LEN + 1 + ARC_EXT_LEN)

#ifdef _GPP // If compiling with the C++ compiler
using namespace sza::array;
#endif

/*
 * List the suffixes for each of the known file types.
 * All suffixes must be ARC_EXT_LEN letters + '\0'.
 */
typedef struct {
  ArcFileType type;         /* The enumerated type of the file */
  char ext[ARC_EXT_LEN+1];  /* The extension to use with this file */
} ArcFileExt;

/*
 * List the file types in the order that they are enumerated in
 * arcfile.h.
 */
static const ArcFileExt extensions[ARC_NUM_TYPE] = {
  {ARC_LOG_FILE, "log"},
  {ARC_DAT_FILE, "dat"},
  {ARC_GRAB_FILE,"fit"},
};

static int bad_arc_file_name(char *path, int complain);

/*.......................................................................
 * Construct a time-stamped archive-file name.
 *
 * Input:
 *  dir         char *   The directory containing the file, or either
 *                       "" or NULL for the current directory.
 *  utc ArcTimeStamp *   The UTC time-stamp to give the file, expressed
 *                       as a Modified Julian Date, split into day-number
 *                       and time-of-day components. If utc==NULL, the
 *                       current UTC will be substituted.
 *  type ArcFileType     The type of archive file. This is used solely to
 *                       decide what extension name to give the file.
 * Output:
 *  return      char *   The full path name of the file (returned in
 *                       dynamically allocated memory), or NULL on error.
 *                       It is the responsibility of the caller to pass
 *                       this to free() when it is no longer required.
 */
char *arc_path_name(char *dir, ArcTimeStamp *utc, ArcFileType type)
{
  char name[ARC_FILE_LEN+1]; /* The name of the file + '\0' */
/*
 * Compose the file name.
 */
  if(arc_file_name(utc, type, name))
    return NULL;
/*
 * Translate and prepend the directory name, returning the pathname
 * in dynamically allocated memory.
 */
  return new_pathname(dir, name);
}

/*.......................................................................
 * Compute an archive file in a specified buffer. The buffer must have
 * room for at least arc_file_name_length()+1 bytes.
 *
 * Input:
 *  utc ArcTimeStamp *  The UTC time-stamp to give the file, expressed
 *                      as a Modified Julian Date, split into day-number
 *                      and time-of-day components. If utc==NULL, the
 *                      current UTC will be substituted.
 *  type ArcFileType    The type of archive file. This is used solely to
 *                      decide what extension name to give the file.
 * Input/Output:
 *  buffer     char *   A buffer of at least arc_file_name_length()+1
 *                      bytes. A '\0' terminated file name will be composed
 *                      in this buffer.
 * Output:
 *  return      int     0 - OK.
 *                      1 - Error.
 */
int arc_file_name(ArcTimeStamp *utc, ArcFileType type, char *buffer)
{
  const char *ext = NULL;    /* The file-name extension */
  Date date;                 /* The calendar date corresponding to 'utc' */
  int i;
/*
 * Check the arguments.
 */
  if(!buffer) {
    lprintf(stderr, "arc_file_name: NULL argument(s).\n");
    return 1;
  };
/*
 * Decompose the specified UTC into calendar components.
 */
  if(utc) {
    if(mjd_utc_to_date(utc->mjd, &date))
      return 1;
    date.hour = utc->sec / 3600;
    date.min = utc->sec / 60 - date.hour * 60;
    date.sec = utc->sec - date.hour * 3600 - date.min * 60;
  } else {
    if(current_date(&date))
      return 1;
  };
    
/*
 * Look up the filename extension.
 */
  for(i=0; i<ARC_NUM_TYPE && !ext; i++) {
    if(extensions[i].type == type)
      ext = extensions[i].ext;
  };
  if(!ext) {
    lprintf(stderr, "arc_file_name: Unknown file type.\n");
    return 1;
  };
/*
 * Compose the file name.
 */
  sprintf(buffer, "%.4d%.2d%.2d_%.2d%.2d%.2d.%.*s", date.year, date.month,
	  date.day, date.hour, date.min, date.sec,
	  ARC_EXT_LEN, ext);
  return 0;
}

/*.......................................................................
 * Decode the time-stamp and file type from a given archive file.
 *
 * Input:
 *  path        char *  The full path-name of the file.
 *  complain     int    If the file-name is invalid, emit a complaint to
 *                      stderr.
 * Input/Output:
 *  utc ArcTimeStamp *  The UTC timestamp encoded in the filename.
 *  type ArcFileType *  The type of archive file.
 * Output:
 *  return       int    0 - OK.
 *                      1 - Error.
 */
int arc_file_id(char *path, int complain, ArcTimeStamp *utc, ArcFileType *type)
{
  size_t length;   /* The length of the file name */
  char *name;      /* A pointer to the name part of the path name */
  char *endp;      /* The pointer to the next unprocessed component in name[] */
  Date date;       /* The calendar UTC encoded in the file name */
  long hhmmss;     /* The time packed as hours*10000 + minutes * 100 + secs */
  long yyyymmdd;   /* The date packed as year*10000 + month * 100 + day */
  int hour,min,sec;/* The time of day */
  int i,j;
/*
 * Check the arguments.
 */
  if(!path || !utc || !type) {
    lprintf(stderr, "arc_file_id: NULL argument(s).\n");
    return 1;
  };
/*
 * Get the length of the path name.
 */
  length = strlen(path);
/*
 * The path name should be at least as long as ARC_FILE_LEN.
 */
  if(length < ARC_FILE_LEN)
    return bad_arc_file_name(path, complain);
/*
 * Get a pointer to where the name part of the path should lie.
 */
  name = path + (length - ARC_FILE_LEN);
/*
 * Make sure that the name is on its own or preceded by a directory
 * separator.
 */
  if(length > ARC_FILE_LEN && name[-1] != '/')
    return bad_arc_file_name(path, complain);
/*
 * Read the date as a single integer from the file name.
 */
  yyyymmdd = strtol(name, &endp, 10);
  if(endp - name != ARC_DATE_LEN)
    return bad_arc_file_name(path, complain);
/*
 * Skip over the separator to the time-of-day component.
 */
  name = endp + 1;
/*
 * Read the time of day as a single integer from the file name.
 */
  hhmmss = strtol(name, &endp, 10);
  if(endp - name != ARC_TIME_LEN)
    return bad_arc_file_name(path, complain);
/*
 * Skip the . separator character and look up the remaining file-name
 * extension.
 */
  name = endp + 1;
  for(i=0; i<ARC_NUM_TYPE; i++) {
/*
 * Perform a case insensitive comparison.
 */
    char *e = (char* )extensions[i].ext;
    char *n = (char* )name;
    for(j=0; j<ARC_EXT_LEN; j++,e++,n++) {
      if(*e != (isupper((int)*n) ? tolower((int)*n) : *n))
	break;
    };
/*
 * Found?
 */
    if(j == ARC_EXT_LEN)
      break;
  };
/*
 * Not found?
 */
  if(i == ARC_NUM_TYPE)
    return bad_arc_file_name(path, complain);
  else
    *type = extensions[i].type;
/*
 * Unpack the yyyymmdd date component into its component parts.
 */
  date.year = yyyymmdd / 10000;
  date.month = (yyyymmdd - date.year * 10000) / 100;
  date.day = yyyymmdd % 100;
  date.hour = 0;
  date.min = 0;
  date.sec = 0;
  date.nsec = 0;
/*
 * Check the date.
 */
  if(date.year < 0 || date.month < 1 || date.month > 12 || date.day < 1 ||
     date.day > days_in_month(is_leap_year(date.year), date.month))
    return bad_arc_file_name(path, complain);
/*
 * Unpack the hhmmss time component into its component parts.
 */
  hour = hhmmss / 10000;
  min = (hhmmss - hour * 10000) / 100;
  sec = hhmmss % 100;
/*
 * Check the time of day.
 */
  if(hour < 0 || hour > 23 || min < 0 || min > 59 || sec < 0 || sec > 59)
    return bad_arc_file_name(path, complain);
/*
 * Convert the calendar date into a Modified Julian Day number.
 */
  if((utc->mjd = date_to_mjd_utc(&date)) < 0)
    return bad_arc_file_name(path, complain);
/*
 * Record the time of day in seconds.
 */
  utc->sec = (hour * 60 + min) * 60 + sec;
  return 0;
}

/*.......................................................................
 * This is a private error-return function of arc_file_id(). It is called
 * when an invalid file name is encountered.
 *
 * Input:
 *  path    char *   The full path-name of the file.
 *  complain int     If true, emit an error message to stderr. 
 * Output:
 *  return   int     1 - The error return value of arc_file_id().
 */
static int bad_arc_file_name(char *path, int complain)
{
  if(complain) {
    lprintf(stderr,"arc_file_id: \"%s\" isn't the name of an archive file.\n",
	    path);
  };
  return 1;
}

/*.......................................................................
 * Return the length of an archive file name (excluding any directory
 * prefix).
 *
 * Output:
 *  return   int   The requested length.
 */
int arc_file_name_length(void)
{
  return ARC_FILE_LEN;
}

