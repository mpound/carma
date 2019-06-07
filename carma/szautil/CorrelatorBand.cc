#include <cmath>

#include "carma/szautil/CorrelatorBand.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace sza::util;
using namespace std;

// Correlator bandwidth

const Frequency CorrelatorBand::bandWidth_(500e6); 

// Bandwidth per channel

const Frequency CorrelatorBand::bandWidthPerChannel_(500e6/(CorrelatorBand::NCHAN_TOTAL+1)); 

/**.......................................................................
 * Return the bandwidth
 */
Frequency CorrelatorBand::bandWidth()
{
  return bandWidth_;
};

/**.......................................................................
 * Return the bandwidth per channel
 */
Frequency CorrelatorBand::bandWidthPerChannel()
{
  return bandWidthPerChannel_;
};

/**.......................................................................
 * Constructor with enumerator
 */
CorrelatorBand::CorrelatorBand(Id id) : id_(id) 
{
  checkMaxBand();
};

/**.......................................................................
 * Constructor with integer band
 */
CorrelatorBand::CorrelatorBand(unsigned int iband)
{
  if(isValid(iband)) {
    id_ = intToId(iband);
  }
  else {
    ostringstream os;
    os << "CorrelatorBand::CorrelatorBand::Invalid band number requested: "
       << iband
       << ends;
    throw Error(os.str());
  }
  checkMaxBand();
}

/**.......................................................................
 * Constructor with no arguments
 */
CorrelatorBand::CorrelatorBand()
{
  id_ = BANDNONE;
  checkMaxBand();
}

/**.......................................................................
 * Copy constructor.
 */
CorrelatorBand::CorrelatorBand(const CorrelatorBand& band)
{
  id_ = band.id_;
  checkMaxBand();
}

/**.......................................................................
 * Return true if this is a valid single band
 */
bool CorrelatorBand::isValidSingleBand()
{
  // Iterate through known bands, looking for a match

  for(CorrelatorBand banditer(BAND0); banditer <= CorrelatorBand(BANDMAX); banditer++) 
    if(banditer.id_ == id_) 
      return true;
  return false;
}

/**.......................................................................
 * Addition operator for CorrelatorBand::Id.  Allows expressions like
 * band1 + band2
 */
const CorrelatorBand CorrelatorBand::operator+(const CorrelatorBand& band)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(band.id_);

  return CorrelatorBand(static_cast<CorrelatorBand::Id>(i1 | i2));
}

/**.......................................................................
 * Less than operator for CorrelatorBand.  Allows expressions like band0 < band1
 */
bool CorrelatorBand::operator<(const CorrelatorBand band)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(band.id_);

  return i1 < i2;
}

/**.......................................................................
 * Less than equals operator for CorrelatorBand.  Allows expressions like 
 * band0 <= band1
 */
bool CorrelatorBand::operator<=(const CorrelatorBand band)
{
  unsigned int i1, i2;

  DBPRINT(false, Debug::DEBUG31, "operator<=: " << band.id_ << ", " << id_);

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(band.id_);

  return i1 <= i2;
}

/**.......................................................................
 * Greater than operator for CorrelatorBand.  Allows expressions like 
 * band0 > band1
 */
bool CorrelatorBand::operator>(const CorrelatorBand band)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(band.id_);

  return i1 > i2;
}

/**.......................................................................
 * Greater than equals operator for CorrelatorBand.  Allows expressions like 
 * band0 >= band1
 */
bool CorrelatorBand::operator>=(const CorrelatorBand band)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(band.id_);

  return i1 >= i2;
}

/**.......................................................................
 * Equals operator for CorrelatorBand.  Allows expressions like BAND0 == BAND1
 */
bool CorrelatorBand::operator==(const CorrelatorBand band)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(band.id_);

  DBPRINT(false, Debug::DEBUG31, "operator==: " << band.id_ << ", " << id_);

  return i1 == i2;
}

/**.......................................................................
 * Prefix increment operator
 */
const CorrelatorBand& CorrelatorBand::operator++()
{
  switch (id_) {
  case BANDALL:  // Do nothing with these
  case BANDNONE: // Special case -- so we can initialize to BANDNONE,
	        // and increment through all receivers
    id_ = BAND0;
    break; 
  default:     // Increment to the next CorrelatorBand
    {
      unsigned int iband = static_cast<unsigned int>(id_);
      id_ = static_cast<Id>(iband*2);
    }
    break;
  }

  DBPRINT(false, Debug::DEBUG31, "operator++: " << id_);

  return *this;
}

/**.......................................................................
 * Post-fix increment operator
 */
const CorrelatorBand CorrelatorBand::operator++(int)
{
  CorrelatorBand before(id_);

  switch (id_) {
  case BANDALL:  // Do nothing with these
  case BANDNONE: // Special case -- so we can initialize to BANDNONE,
		// and increment through all receivers
    id_ = BAND0;
    break; 
  default:      // Increment to the next CorrelatorBand
    {
      unsigned int iband = static_cast<unsigned int>(id_);
      id_ = static_cast<Id>(iband*2);
    }
    break;
  }
  return before;
}

/**.......................................................................
 * Construct a register map name out of this Band enumerator.
 */
string CorrelatorBand::bandName()
{
  CorrelatorBand banditer(CorrelatorBand::BAND0), bandmax(CorrelatorBand::BANDMAX);
  int iband;
  ostringstream os;
  string bandname;

  for(iband=0; banditer <= bandmax; banditer++,iband++) {
    if(banditer.id_ == id_) {
      os << "band" << iband << ends;
      bandname = os.str();
      return bandname;
    }
  }
  bandname = "Not a valid single band";
  return bandname;
}

/**.......................................................................
 * Construct a register map name out of this Band enumerator.
 */
string CorrelatorBand::bandName(unsigned iBand)
{
  ostringstream os;
  os << "band" << iBand;
  return os.str();
}

/**.......................................................................
 * Return the integer Band Id associated with this enumerator.
 *
 * @throws Exception if this enumerator does not represent
 * a single valid band.
 */
unsigned int CorrelatorBand::getIntId()
{
  CorrelatorBand banditer(CorrelatorBand::BAND0), bandmax(CorrelatorBand::BANDMAX);
  unsigned int iBand;

  for(iBand=0; banditer <= bandmax; banditer++,iBand++) {
    if(banditer.id_ == id_)
      return iBand;
  }

  // If no valid band was found, throw an error.

  throw Error("CorrelatorBand::getIntId: Band enumerator does not specify"
  	      "a valid single band id.\n");
}

/**.......................................................................
 * Return an integer Band index suitable for passing to the
 * downconverter control API.
 *
 * @throws Exception if this enumerator does not represent
 * a single valid band.
 */
unsigned short CorrelatorBand::getDcBandIndex()
{
  // The downconverter API expects bands to be numbered starting at 0.

  return (unsigned short)(getIntId() + 1);
}

/**.......................................................................
 * Return an integer Band index suitable for passing to the
 * downconverter control API.
 *
 * @throws Exception if this enumerator does not represent
 * a single valid band.
 */
unsigned short CorrelatorBand::getDcNodeIndex()
{
  // The downconverter API expects bands to be numbered starting at 0.

  return (unsigned short)getIntId();
}

/**.......................................................................
 * Return the maximum number of bands
 */
unsigned int CorrelatorBand::getBandMax()
{
  CorrelatorBand banditer(CorrelatorBand::BAND0), bandmax(CorrelatorBand::BANDMAX);
  unsigned int iBand;

  for(iBand=0; banditer <= bandmax; banditer++,iBand++);

  return iBand;
}

/**.......................................................................
 * Write the contents to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, const CorrelatorBand& band)
{
  CorrelatorBand banditer(CorrelatorBand::BAND0),bandmax(CorrelatorBand::BANDMAX);
  int iBand;
  bool first=true;

  for(iBand=0;banditer <= bandmax;banditer++,iBand++) {
    if(band.id_ & banditer.id_) {
      if(!first)
	os << "+";
      os << "band" << iBand << ends;
      first=false;
    }
  }
  if(first)
    os << "Not a valid band";
  return os;
}

/**.......................................................................
 * Write the contents to an sstream
 */
ostringstream& 
sza::util::operator<<(ostringstream& os, const CorrelatorBand& band)
{
  CorrelatorBand banditer(CorrelatorBand::BAND0),bandmax(CorrelatorBand::BANDMAX);
  int iBand;

  for(iBand=0;banditer <= bandmax;banditer++,iBand++) {
    if(banditer == band) {
      os << "band" << iBand << ends;
      return os;
    }
  }
  os << "Not a valid band";
  return os;
}

/**.......................................................................
 * Addition operator for CorrelatorBand::Id.  Allows expressions like BAND0 +
 * BAND1
 */
CorrelatorBand::Id 
sza::util::operator+(const CorrelatorBand::Id id1, const CorrelatorBand::Id id2)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id1);
  i2 = static_cast<unsigned int>(id2);

  return static_cast<CorrelatorBand::Id>(i1 | i2);
}

//-----------------------------------------------------------------------
// Private methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Return true, if this is a valid single Band
 */
bool CorrelatorBand::isValid(unsigned int band)
{
  CorrelatorBand banditer(BAND0), bandmax(BANDMAX);

  // See if this index specifies a valid single receiver

  for(unsigned iBand=0;banditer <= bandmax;banditer++,iBand++)
    if(iBand==band)
      return true;
  return false;
}

/**.......................................................................
 * Convert from receiver index to enumerator
 */
CorrelatorBand::Id CorrelatorBand::intToId(unsigned int iBand)
{
  // If this is 0, return the first band

  if(iBand==0)
    return BAND0;

  return (Id)pow((double)2, (int)iBand);
}

/**.......................................................................
 * Return true if the passed Id is part of this object's band
 * set.  
 */
bool CorrelatorBand::isSet(Id id)
{
  return (id & id_);
}

/**
 * Set the id of this antenna enumerator.
 */
void CorrelatorBand::setId(CorrelatorBand::Id id)
{
  id_ = id;
}

/**.......................................................................
 * Return true if the passed band is part of this object's band
 * set.  
 */
bool CorrelatorBand::isSet(CorrelatorBand& band)
{
  return (band.id_ & id_);
}

/**.......................................................................
 * Private method to check for consistency between the enumerated
 * BANDMAX definition and the const CorrelatorBand::NBAND
 */
void CorrelatorBand::checkMaxBand() 
{
  LogStream errStr;
  unsigned int iBand = 1U << (CorrelatorBand::NBAND-1);

  DBPRINT(true, Debug::DEBUG31, "Inside checkMaxBand");

  if(iBand != BANDMAX) {
    std::cout << "Bandmax = " << BANDMAX << " iBand = " << iBand << std::cout;
    errStr.appendMessage(true, "Mismatch between CorrelatorBand::BANDMAX "
		       "and CorrelatorBand::NBAND");
    throw Error(errStr);
  }
}
