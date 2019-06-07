#include "carma/szautil/Exception.h"

#include "carma/antenna/sza/antenna/control/Date.h"

using namespace sza::antenna::control;
using namespace sza::util;

/**.......................................................................
 * Constructor function just intializes the date fields by calling
 * reset(), below.
 */
Date::Date()
{
  reset();
}

/**.......................................................................
 * Intialize the date fields to zero
 */
void Date::reset() 
{
  sza::array::init_Date(&date_, 0, 0, 0, 0, 0, 0, 0);
}

/**.......................................................................
 * Convert from MJD to broken down calendar date
 */
void Date::convertMjdUtcToDate(double utc)
{
  if(sza::array::mjd_utc_to_date(utc, &date_))
    throw Error("Date::convertMjdToDate: "
		"Error in sza::array::mjd_utc_to_date().\n");
}

/**.......................................................................
 * Return the year.
 */
int Date::getYear()
{
  return date_.year;
}
