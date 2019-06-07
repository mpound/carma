#include <cmath>
#include <unistd.h>

#include "carma/szautil/AntNum.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Constructor with enumerator
 */
AntNum::AntNum(Id id) : id_(id) 
{
  checkMaxAnt();
};

/**.......................................................................
 * Constructor with integer ant
 */
AntNum::AntNum(unsigned int iant)
{
  if(sza::util::isValidAnt(iant)) {
    id_ = intToId(iant);
  }
  else {
    ostringstream os;
    os << "AntNum::AntNum::Invalid antenna number requested: "
       << iant ;
    throw Error(os.str());
  }
  checkMaxAnt();
}

/**.......................................................................
 * Constructor with no arguments
 */
AntNum::AntNum()
{
  id_ = ANTNONE;
  checkMaxAnt();
}

/**.......................................................................
 * Copy constructor.
 */
AntNum::AntNum(const AntNum& antNum)
{
  id_ = antNum.id_;
  checkMaxAnt();
}

/**.......................................................................
 * Copy constructor.
 */
AntNum::AntNum(AntNum* antNum)
{
  id_ = antNum->id_;
  checkMaxAnt();
}

/**.......................................................................
 * Return true if this is a valid single ant
 */
bool AntNum::isValidSingleAnt()
{
  // Iterate through known antennas, looking for a match

  for(AntNum antiter(ANT0); antiter <= AntNum(ANTMAX); antiter++) 
    if(antiter.id_ == id_) 
      return true;
  return false;
}

/**.......................................................................
 * Return true if this is a valid single ant
 */
bool AntNum::isValidSingleAnt(Id antId)
{
  // Iterate through known antennas, looking for a match

  for(AntNum antiter(ANT0); antiter <= AntNum(ANTMAX); antiter++) 
    if(antiter.id_ == antId) 
      return true;
  return false;
}

/**.......................................................................
 * Addition operator for AntNum.  Allows expressions like
 * ant1 + ant2
 */
const AntNum AntNum::operator+(const AntNum& ant)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(ant.id_);

  return AntNum(static_cast<AntNum::Id>(i1 | i2));
}

/**.......................................................................
 * Less than operator for AntNum.  Allows expressions like ant0 < ant1
 */
bool AntNum::operator<(const AntNum ant)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(ant.id_);

  return i1 < i2;
}

/**.......................................................................
 * Less than equals operator for AntNum.  Allows expressions like 
 * ant0 <= ant1
 */
bool AntNum::operator<=(const AntNum ant)
{
  unsigned int i1, i2;

  DBPRINT(false, Debug::DEBUG31, "operator<=: " << ant.id_ << ", " << id_);

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(ant.id_);

  return i1 <= i2;
}

/**.......................................................................
 * Greater than operator for AntNum.  Allows expressions like 
 * ant0 > ant1
 */
bool AntNum::operator>(const AntNum ant)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(ant.id_);

  return i1 > i2;
}

/**.......................................................................
 * Greater than equals operator for AntNum.  Allows expressions like 
 * ant0 >= ant1
 */
bool AntNum::operator>=(const AntNum ant)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(ant.id_);

  return i1 >= i2;
}

/**.......................................................................
 * Equals operator for AntNum.  Allows expressions like ANT0 == ANT1
 */
bool AntNum::operator==(const AntNum ant)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(ant.id_);

  DBPRINT(false, Debug::DEBUG31, "operator==: " << ant.id_ << ", " << id_);

  return i1 == i2;
}

/**.......................................................................
 * Prefix increment operator
 */
const AntNum& AntNum::operator++()
{
  switch (id_) {
  case ANTALL:  // Do nothing with these
  case ANTNONE: // Special case -- so we can initialize to ANTNONE,
	        // and increment through all receivers
    id_ = ANT0;
    break; 
  default:     // Increment to the next AntNum
    {
      unsigned int iant = static_cast<unsigned int>(id_);
      id_ = static_cast<Id>(iant*2);
    }
    break;
  }

  DBPRINT(true, Debug::DEBUG31, "operator++: " << id_);

  return *this;
}

/**.......................................................................
 * Post-fix increment operator
 */
const AntNum AntNum::operator++(int)
{
  AntNum before(id_);

  switch (id_) {
  case ANTALL:  // Do nothing with these
  case ANTNONE: // Special case -- so we can initialize to ANTNONE,
		// and increment through all receivers
    id_ = ANT0;
    break; 
  default:      // Increment to the next AntNum
    {
      unsigned int iant = static_cast<unsigned int>(id_);
      id_ = static_cast<Id>(iant*2);
    }
    break;
  }
  return before;
}

/**.......................................................................
 * Construct a CORBA object name associated with this antenna
 * enumerator.
 */
string AntNum::getObjectName()
{
  AntNum antiter(AntNum::ANT0), antmax(AntNum::ANTMAX);
  int iant;
  ostringstream os;
  string objname;

  for(iant=0; antiter <= antmax; antiter++,iant++) {
    if(antiter.id_ == id_) {

      // Use different object DO names depending on which channel we are using

      os << "carma.sza" << (iant+1) ;

      objname = os.str();
      return objname;
    }
  }
  objname = "Not a valid single antenna";
  return objname;  
}

/**.......................................................................
 * Construct a CORBA object name associated with this antenna
 * enumerator.
 */
string AntNum::getEventChannelName()
{
  ostringstream os;

  // Use different CORBA event channel names depending on which
  // version of the software
  
#if DIR_IS_STABLE
  os << "eventChannel" ;
#else
  os << "eventChannelUnstable" ;
#endif
  
  return os.str();
}

/**.......................................................................
 * Construct a register map name out of this Antenna enumerator.
 */
string AntNum::getAntennaName()
{
  AntNum antiter(AntNum::ANT0), antmax(AntNum::ANTMAX);
  int iant;
  ostringstream os;
  string antname;

  for(iant=0; antiter <= antmax; antiter++,iant++) {
    if(antiter.id_ == id_) {
      os << "antenna" << iant << ends;
      antname = os.str();
      return antname;
    }
  }
  antname = "Not a valid single antenna";
  return antname;
}

/**.......................................................................
 * Construct a logger prefix
 */
string AntNum::getLoggerPrefix()
{
  AntNum antiter(AntNum::ANT0), antmax(AntNum::ANTMAX);
  int iant;
  ostringstream os;
  string antname;

  for(iant=0; antiter <= antmax; antiter++,iant++) {
    if(antiter.id_ == id_) {
      os << "Antenna" << iant << ": ";
      antname = os.str();
      return antname;
    }
  }
  antname = "Not a valid single antenna";
  return antname;
}

/**.......................................................................
 * Return the integer Antenna Id associated with this enumerator.
 *
 * @throws Exception if this enumerator does not represent
 * a single valid antenna.
 */
unsigned int AntNum::getIntId()
{
  AntNum antiter(AntNum::ANT0), antmax(AntNum::ANTMAX);
  unsigned int iant;

  for(iant=0; antiter <= antmax; antiter++,iant++) {
    if(antiter.id_ == id_)
      return iant;
  }

  // If no valid antenna was found, throw an error.

  ThrowError("AntNum::getIntId: Antenna enumerator: " << id_);
}

/**.......................................................................
 * Return the integer Antenna Id associated with this enumerator
 * expected by the delay engine.
 *
 * @throws Exception if this enumerator does not represent
 * a single valid antenna.
 */
unsigned int AntNum::getDelayEngineIntId()
{
  return getIntId();
}

/**.......................................................................
 * Return a string representing an antenna set.
 */
std::string AntNum::printAntennaSet(AntNum::Id id)
{
  return sza::util::printAntennaSet(id);
}

/**.......................................................................
 * Return a string representing our antenna set.
 */
std::string AntNum::printAntennaSet()
{
  return sza::util::printAntennaSet(id_);
}

/**.......................................................................
 * Return the integer Antenna index suitable for passing to the
 * downconverter API.
 *
 * @throws Exception if this enumerator does not represent
 * a single valid antenna.
 */
unsigned short AntNum::getDcAntennaIndex()
{
  return (unsigned short)(getIntId() + 1);
}

/**.......................................................................
 * Return the integer Antenna index suitable for passing to the
 * downconverter API.
 *
 * @throws Exception if this enumerator does not represent
 * a single valid antenna.
 */
unsigned short AntNum::getDcNodeIndex()
{
  return (unsigned short)(getIntId() + 1);
}

/**.......................................................................
 * Return the integer Antenna index suitable for passing to the
 * CARMA control system
 *
 * @throws Exception if this enumerator does not represent
 * a single valid antenna.
 */
unsigned short AntNum::getCarmaAntennaIndex()
{
  return (unsigned short)(getIntId() + 1);
}

/**.......................................................................
 * Return the maximum number of antennas
 */
unsigned int AntNum::getAntMax()
{
  AntNum antiter(AntNum::ANT0), antmax(AntNum::ANTMAX);
  unsigned int iant;

  for(iant=0; antiter <= antmax; antiter++,iant++);

  return iant;
}

/**.......................................................................
 * Write the contents to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, const AntNum& ant)
{
  AntNum antiter(AntNum::ANT0),antmax(AntNum::ANTMAX);
  int iant;
  bool first=true;

  for(iant=0;antiter <= antmax;antiter++,iant++) {
    if(ant.id_ & antiter.id_) {
      if(!first)
        os << "+";
      os << "ant" << iant << ends;
      first=false;
    }
  }
  if(first)
    os << "Not a valid ant";
  return os;
}

/**.......................................................................
 * Generate a string representation of this antenna set
 */
std::string AntNum::getString()
{
  AntNum antiter(AntNum::ANT0),antmax(AntNum::ANTMAX);
  int iant;
  bool first=true;
  ostringstream os;

  for(iant=0;antiter <= antmax;antiter++,iant++) {
    if(id_ & antiter.id_) {
      if(!first)
	os << "+";
      os << "ant" << iant;
      first=false;
    }
  }
  if(first)
    os << "Not a valid ant";
  os << ends;
  return os.str();
}

/**.......................................................................
 * Addition operator for AntNum::Id.  Allows expressions like ANT0 +
 * ANT1
 */
AntNum::Id 
sza::util::operator+(const AntNum::Id id1, const AntNum::Id id2)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id1);
  i2 = static_cast<unsigned int>(id2);

  return static_cast<AntNum::Id>(i1 | i2);
}

/**.......................................................................
 * Subtraction operator for AntNum::Id.  Allows expressions like
 * ANTALL - ANT1
 */
AntNum::Id 
sza::util::operator-(const AntNum::Id id1, const AntNum::Id id2)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id1);
  i2 = static_cast<unsigned int>(id2);

  return static_cast<AntNum::Id>(i1 & ~i2);
}

//-----------------------------------------------------------------------
// Private methods
//-----------------------------------------------------------------------

/**.......................................................................
 * Return true, if this is a valid single antnena
 */
bool sza::util::isValidAnt(unsigned int ant)
{
  AntNum antiter(AntNum::ANT0), antmax(AntNum::ANTMAX);

  // See if this index specifies a valid single receiver

  for(unsigned iant=0;antiter <= antmax;antiter++,iant++)
    if(iant==ant)
      return true;
  return false;
}

/**.......................................................................
 * Return true, if this is a valid antenna set
 */
bool sza::util::isValidAntennaSet(AntNum::Id antennas)
{
  // Return true if any bits higher than ANTMAX are set

  return ((unsigned)antennas & ~((unsigned)AntNum::ANTALL)) ? false : true;
}

/**.......................................................................
 * Return a string representing an antenna set.
 */
std::string sza::util::printAntennaSet(AntNum::Id antennas)
{
  ostringstream os;

  // Only print this if it is a valid antenna set.

  if(!isValidAntennaSet(antennas)) 
    os << "Invalid antenna set" << ends;
  else {
    AntNum antSet(antennas);
    bool first=true;

    // Construct a string out of whichever antennas are set
    
    for(AntNum antiter(AntNum::ANT0); antiter <= AntNum(AntNum::ANTMAX); 
	antiter++) 
    {

      if(antSet.isSet(antiter.id_)) {
	if(first) 
	  first = false;
	else 
	  os << "+";
      
	os << "ant" << antiter.getIntId();
      }
    }
  }
  return os.str();
}

/**.......................................................................
 * Convert from receiver index to enumerator
 */
unsigned sza::util::idToInt(AntNum::Id id)
{
  AntNum antNum(id);

  return antNum.getIntId();
}

/**.......................................................................
 * Convert from receiver index to enumerator
 */
AntNum::Id sza::util::intToId(unsigned int iant)
{
  // If this is 0, return the first antenna

  if(iant==0)
    return AntNum::ANT0;

  return (AntNum::Id)pow((double)2, (int)iant);
}

/**.......................................................................
 * Convert from receiver index to enumerator
 */
unsigned AntNum::idToInt(Id id)
{
  return sza::util::idToInt(id);
}

/**.......................................................................
 * Convert from receiver index to enumerator
 */
AntNum::Id AntNum::intToId(unsigned int iant)
{
  return  sza::util::intToId(iant);
}

/**.......................................................................
 * Set the antennas represented by this object
 */
void AntNum::set(AntNum::Id id)
{
  id_ = id;
}

/**.......................................................................
 * Return true if the passed Id is part of this object's antenna
 * set.  
 */
bool AntNum::isSet(unsigned id)
{
  return (intToId(id) & id_);
}

/**.......................................................................
 * Return true if the passed Id is part of this object's antenna
 * set.  
 */
bool AntNum::isSet(Id id)
{
  return (id & id_);
}

/**.......................................................................
 * Return true if the passed antenna is part of this object's antenna
 * set.  
 */
bool AntNum::isSet(AntNum& antNum)
{
  return (antNum.id_ & id_);
}

/**.......................................................................
 * Return true if the passed antenna is part of this object's antenna
 * set.  
 */
bool AntNum::isSet(AntNum* antNum)
{
  return (antNum->id_ & id_);
}

/**.......................................................................
 * Set the id of this antenna enumerator.
 */
void AntNum::setId(AntNum::Id id)
{
  id_ = id;
}

/**.......................................................................
 * Set the id of this antenna enumerator.
 */
void AntNum::setId(unsigned int id)
{
  id_ = intToId(id);
}

/**.......................................................................
 * Set the id of this antenna enumerator.
 */
void AntNum::setId(const AntNum& antNum)
{
  id_ = antNum.id_;
}

/**.......................................................................
 * Initialize this antenna enumerator from the host name.
 */
void AntNum::setIdFromHost()
{
  LogStream errStr;
  char hostName[100];

  // Get the host name

  gethostname(hostName, sizeof(hostName));

  // Check that the returned string is a valid antenna host

  string hostString(hostName);

  // Definitions of "standardized" C++ methods appear to be in flux.
  // In particular, the string class compare method changed the order
  // of its arguments somewhere between gcc 2.96 and gcc 3.2.2

#if (__GNUC__ > 2)
  if(hostString.compare(0,3,"ant")==0) {
#else
  if(hostString.compare("ant", 0, 3)==0) {
#endif
    setId(atoi(&hostString[3]));
  } else {
    ThrowError("Cannot initialize antenna number from hostname: "
	       << hostName);
  }
}

/**.......................................................................
 * Return the antenna id associated with this
 * enumerator.
 */
AntNum::Id AntNum::getId()
{
  return id_;
}

/**.......................................................................
 * Private method to check for consistency between the enumerated
 * ANTMAX definition and the global #define MAX_ANT
 */
void AntNum::checkMaxAnt() 
{
  LogStream errStr;
  unsigned int iant = 1U << (AntNum::NANT-1);

  if(iant != ANTMAX) {
    errStr.appendMessage(true, "Mismatch between AntNum::ANTMAX and AntNum::NANT");
    throw Error(errStr);
  }
}
