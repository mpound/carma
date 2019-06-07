#include "carma/szautil/RegDate.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include "carma/szaarrayutils/output.h"
#include "carma/szaarrayutils/astrom.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RegDate::RegDate(unsigned dayNo, unsigned mSec) 
{ 
  data_.dayNo_ = dayNo;
  data_.mSec_  = mSec;
  timeVal_.setMjd(dayNo, mSec);
}


RegDate::RegDate(RegDate::Data& data)
{
  data_.dayNo_ = data.dayNo_;
  data_.mSec_  = data.mSec_;
  timeVal_.setMjd(data.dayNo_, data.mSec_);
}

RegDate::RegDate(TimeVal& timeVal)
{
  setTo(timeVal);
}

void RegDate::operator=(TimeVal& timeVal)
{
  setTo(timeVal);
}

void RegDate::setTo(TimeVal& timeVal)
{
  timeVal_ = timeVal;
  data_.dayNo_ = timeVal_.getMjdDays();
  data_.mSec_  = timeVal_.getMjdMilliSeconds();
}

RegDate::RegDate() 
{
  timeVal_.setToCurrentTime();
  data_.dayNo_ = timeVal_.getMjdDays();
  data_.mSec_  = timeVal_.getMjdMilliSeconds();
}

void RegDate::initialize()
{
  data_.dayNo_ = 0;
  data_.mSec_  = 0;
}

void RegDate::setMjd(double mjd)
{
  unsigned dayNo = (unsigned)mjd;
  unsigned mSec = (unsigned)((mjd-dayNo)*milliSecondsPerDay_);

  setDayNumber(dayNo);
  setMilliSeconds(mSec);
}

void RegDate::setDayNumber(unsigned dayNo)
{
  data_.dayNo_ = dayNo;
  timeVal_.setMjd(data_.dayNo_, data_.mSec_);
}

void RegDate::setMilliSeconds(unsigned mSec)
{
  data_.mSec_ = mSec;
  timeVal_.setMjd(data_.dayNo_, data_.mSec_);
}

/**.......................................................................
 * Destructor.
 */
RegDate::~RegDate() {}

/**.......................................................................
 * An output operator
 */
ostream& sza::util::operator<<(ostream& os, RegDate& date)
{
  OutputStream* outputStream = 0;
  char fmtString[100];

  // Attempt to allocate the new output stream

  outputStream = new_OutputStream();

  if(!outputStream ||
     open_StringOutputStream(outputStream, 1, fmtString, 
			     sizeof(fmtString))) {
    del_OutputStream(outputStream);

    ThrowError("Unable to allocate a new stream");
  };

  if(sza::array::output_utc(outputStream, "", 24, 3, date.mjd())==1) {
    del_OutputStream(outputStream);
    ThrowError("Error outputting date");
  }

  os << fmtString;

  del_OutputStream(outputStream);

  return os;
}

/**.......................................................................
 * An output operator
 */
std::string RegDate::formatCarmaString()
{
  OutputStream* outputStream = 0;
  char fmtString[100];

  // Attempt to allocate the new output stream

  outputStream = new_OutputStream();

  if(!outputStream ||
     open_StringOutputStream(outputStream, 1, fmtString, 
			     sizeof(fmtString))) {
    del_OutputStream(outputStream);

    ThrowError("Unable to allocate a new stream");
  };

  if(sza::array::outputCarmaUtc(outputStream, "", 24, 1, mjd())==1) {
    del_OutputStream(outputStream);
    ThrowError("Error outputting date");
  }

  del_OutputStream(outputStream);

  return fmtString;
}

/**.......................................................................
 * An output operator
 */
std::string RegDate::str()
{
  OutputStream* outputStream = 0;
  char fmtString[100];

  // Attempt to allocate the new output stream

  outputStream = new_OutputStream();

  if(!outputStream ||
     open_StringOutputStream(outputStream, 1, fmtString, 
			     sizeof(fmtString))) {
    del_OutputStream(outputStream);

    ThrowError("Unable to allocate a new stream");
  };

  if(sza::array::output_utc(outputStream, "", 24, 3, mjd())==1) {
    del_OutputStream(outputStream);
    ThrowError("Error outputting date");
  }

  std::string outputStr(fmtString);

  del_OutputStream(outputStream);

  return outputStr;
}

/**.......................................................................
 * Return the date, in MJD days
 */
double RegDate::mjd()
{
  return data_.dayNo_ + (double)(data_.mSec_)/(1000 * 86400);
}

/**.......................................................................
 * Return the time, in hours
 */
double RegDate::timeInHours()
{
  return (double)data_.mSec_ / (1000 * 3600);
}

bool RegDate::operator==(RegDate& date)
{
  return data_.dayNo_ == date.data_.dayNo_ &&
    data_.mSec_ == date.data_.mSec_;
}

bool RegDate::operator>(RegDate& date)
{
  return mjd() > date.mjd();
}

bool RegDate::operator>=(RegDate& date)
{
    return mjd() >= date.mjd();
}

bool RegDate::operator<(RegDate& date)
{
  return mjd() < date.mjd();  
}

bool RegDate::operator<=(RegDate& date)
{
  return mjd() <= date.mjd();
}

RegDate RegDate::operator+(const RegDate& date)
{
  RegDate sum(date);
  sum.data_.dayNo_ += date.data_.dayNo_;
  sum.data_.mSec_  += date.data_.mSec_;

  if(sum.data_.mSec_ > milliSecondsPerDay_) {
    sum.data_.dayNo_++;
    sum.data_.mSec_ = sum.data_.mSec_ % milliSecondsPerDay_;
  }
  return sum;
}

RegDate RegDate::operator-(const RegDate& date)
{
  long int dayDiff  = data_.dayNo_ -= date.data_.dayNo_;
  long int mSecDiff = data_.mSec_  -= date.data_.mSec_;

  if(mSecDiff < 0) {
    --dayDiff;
    mSecDiff += milliSecondsPerDay_;
  }
    
  RegDate diff((dayDiff < 0 ? 0 : dayDiff), mSecDiff);
  return diff;
}

void RegDate::operator+=(const RegDate& date)
{
  data_.dayNo_ += date.data_.dayNo_;
  data_.mSec_  += date.data_.mSec_;

  if(data_.mSec_ > milliSecondsPerDay_) {
    data_.dayNo_++;
    data_.mSec_ = data_.mSec_ % milliSecondsPerDay_;
  }
}

void RegDate::operator-=(const RegDate& date)
{
  long int dayDiff  = data_.dayNo_ -= date.data_.dayNo_;
  long int mSecDiff = data_.mSec_  -= date.data_.mSec_;

  if(mSecDiff < 0) {
    --dayDiff;
    mSecDiff += milliSecondsPerDay_;
  }
    
  data_.dayNo_ = (dayDiff < 0 ? 0 : dayDiff);
  data_.mSec_  = mSecDiff;
}

RegDate RegDate::operator/(unsigned int divisor)
{
  double fracDay = data_.dayNo_ + ((double)data_.mSec_)/milliSecondsPerDay_;
  fracDay /= divisor;

  RegDate div((unsigned)fracDay, 
	      (unsigned)((fracDay-(unsigned)fracDay) * milliSecondsPerDay_));
  return div;
}

void RegDate::operator=(RegDate::Data& data)
{
  data_.dayNo_ = data.dayNo_;
  data_.mSec_  = data.mSec_;
}
