#include <string.h>
/* Header file for use with these routines */
#include "carma/szaarrayutils/date.h"


#include "carma/szaarrayutils/output.h"
#include "carma/szaarrayutils/astrom.h"

using namespace sza::array;

/* Convert MJD to printable date string, using MCS routines */

void mjd_to_date(double mjd, char* date_string, int size)
{
  OutputStream *output;
  output = new_OutputStream();
  if(!output)
    goto error;
  if(open_StringOutputStream(output, 1, date_string, size)) {
    del_OutputStream(output);
    goto error;
  };
  if(output_utc(output, "", 0, 0, mjd)) {
    del_OutputStream(output);
    goto error;
  };
  del_OutputStream(output);
  return;

error:
  strncpy(date_string, "[error]", size);
  return;
}

/*.......................................................................
 * Convert a UTC date string to the equivalent Modified Julian Date.
 *
 * Input:
 *  date_string  char *  The string to be decoded, eg:
 *                        "12-may-1998 12:34:50" or "12-May-1998 3:45".
 * Input/Output:
 *  mjd        double *  The decoded Modified Julian Date.
 * Output:
 *  return        int    0 - OK.
 *                       1 - Error (an error message will have been
 *                                  sent to stderr).
 */

int date_to_mjd(char *date_string, double *mjd)
{
  int status = 0;
  InputStream *input = new_InputStream();
  if (!input ||
      open_StringInputStream(input, 0, date_string) ||
      input_utc(input, 1, 0, mjd)) {
    status = 1;
  };
  del_InputStream(input);
  return status;
}

/* Parse a pair of date:time strings to get a UT range as two modified
 * Julian dates.
 * Input: arg1, arg2 (may be null)
 * Output: start, end
 * Returns: 0 if valid, otherwise invalid input strings
 */

int parse_timerange(char *arg1, char *arg2,
		    double *start, double *end)
{
  int i;
  char tmp[128];

  /* arg1 is start time */

  if (!arg1) {
    *start = 0.0;
  } else if (date_to_mjd(arg1, start) || *start < 50000.) {
    return 1; /* bad start time */
  }

  /* arg2 is end time */

  if (!arg2) {
    *end = 0.0;
    return 0;
  }

  /* if arg2 doesn't contain a date part, add one from start; if
     necessary, increment the date to make the end time later than the
     start */

  if (strchr(arg2, '-') != 0) {
    if (date_to_mjd(arg2, end) || *end < 50000. || *start > *end)
      return 2; /* bad end time */
  } else {
    mjd_to_date(*start, tmp, sizeof(tmp)); /* convert to string */
    for (i=0; i<2; i++) {
      *strchr(tmp, ' ') = '\0'; /* truncate string at end of date */
      strcat(tmp, ":");
      strncat(tmp, arg2, sizeof(tmp)-strlen(tmp)-1); /* append time */
      if (date_to_mjd(tmp, end))
	return 2; 
      if (*start < *end)
	return 0;
      mjd_to_date(*start+1.0, tmp, sizeof(tmp));
    }
  }
  return 0;
}
