#include <cmath>
#include <unistd.h>

#include "carma/szautil/DDSChannel.h"
#include "carma/szautil/Debug.h"
#include "carma/szautil/Directives.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace sza::util;
using namespace std;

/**.......................................................................
 * Convert from DDS Id to integer index expected by the lobe rotator
 */
unsigned DDSChannel::DDSToLRInt(Id id)
{
  if(id == DDSALL)
    return 0;
  else
    return idToInt(id) + 1;
}

unsigned DDSChannel::LRInt()
{
  return DDSToLRInt(id_);
}

/**.......................................................................
 * Convert from integer index expected by the lobe rotatorDDS Id to 
 */
DDSChannel::Id DDSChannel::LRIntToDDSId(unsigned intId)
{
  if(intId == 0)
    return DDSALL;
  else
    return intToId(intId-1);
}

/**.......................................................................
 * Return true if this is a valid single ant
 */
bool DDSChannel::isValidSingleChannel(Id ddsId)
{
  // Iterate through known channels, looking for a match

  for(DDSChannel ddsiter(DDS0); ddsiter <= DDSChannel(DDSMAX); ddsiter++) 
    if(ddsiter.id_ == ddsId) 
      return true;
  return false;
}

/**.......................................................................
 * Return true if this is a valid single ant
 */
bool DDSChannel::isValidSingleChannel()
{
  return isValidSingleChannel(id_);
}

/**.......................................................................
 * Addition operator for DDSChannel.  Allows expressions like
 * dds1 + dds2
 */
const DDSChannel DDSChannel::operator+(const DDSChannel& dds)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(dds.id_);

  return DDSChannel(static_cast<DDSChannel::Id>(i1 | i2));
}

/**.......................................................................
 * Less than operator for DDSChannel.  Allows expressions like dds0 < dds1
 */
bool DDSChannel::operator<(const DDSChannel dds)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(dds.id_);

  return i1 < i2;
}

/**.......................................................................
 * Less than equals operator for DDSChannel.  Allows expressions like 
 * dds0 <= dds1
 */
bool DDSChannel::operator<=(const DDSChannel dds)
{
  unsigned int i1, i2;

  DBPRINT(false, Debug::DEBUG31, "operator<=: " << dds.id_ << ", " << id_);

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(dds.id_);

  return i1 <= i2;
}

/**.......................................................................
 * Greater than operator for DDSChannel.  Allows expressions like 
 * dds0 > dds1
 */
bool DDSChannel::operator>(const DDSChannel dds)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(dds.id_);

  return i1 > i2;
}

/**.......................................................................
 * Greater than equals operator for DDSChannel.  Allows expressions like 
 * dds0 >= dds1
 */
bool DDSChannel::operator>=(const DDSChannel dds)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(dds.id_);

  return i1 >= i2;
}

/**.......................................................................
 * Equals operator for DDSChannel.  Allows expressions like DDS0 == DDS1
 */
bool DDSChannel::operator==(const DDSChannel dds)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id_);
  i2 = static_cast<unsigned int>(dds.id_);

  DBPRINT(false, Debug::DEBUG31, "operator==: " << dds.id_ << ", " << id_);

  return i1 == i2;
}

/**.......................................................................
 * Prefix increment operator
 */
const DDSChannel& DDSChannel::operator++()
{
  switch (id_) {
  case DDSALL:  // Do nothing with these
  case DDSNONE: // Special case -- so we can initialize to DDSNONE,
	        // and increment through all receivers
    id_ = DDS0;
    break; 
  default:     // Increment to the next DDSChannel
    {
      unsigned int idds = static_cast<unsigned int>(id_);
      id_ = static_cast<Id>(idds*2);
    }
    break;
  }

  DBPRINT(true, Debug::DEBUG31, "operator++: " << id_);

  return *this;
}

/**.......................................................................
 * Post-fix increment operator
 */
const DDSChannel DDSChannel::operator++(int)
{
  DDSChannel before(id_);

  switch (id_) {
  case DDSALL:  // Do nothing with these
  case DDSNONE: // Special case -- so we can initialize to DDSNONE,
		// and increment through all receivers
    id_ = DDS0;
    break; 
  default:      // Increment to the next DDSChannel
    {
      unsigned int idds = static_cast<unsigned int>(id_);
      id_ = static_cast<Id>(idds*2);
    }
    break;
  }
  return before;
}

/**.......................................................................
 * Return a string representing our channel .
 */
std::string DDSChannel::printChannels()
{
  return printChannels(id_);
}

/**.......................................................................
 * Return a string representing an antenna set.
 */
std::string DDSChannel::printChannels(DDSChannel::Id channels)
{
  ostringstream os;

  // Only print this if it is a valid ddsenna set.

  if(!isValidChannelSet(channels)) 
    os << "Invalid channel set" << ends;
  else {
    DDSChannel ddsSet(channels);
    bool first=true;

    // Construct a string out of whichever channels are set
    
    for(DDSChannel ddsiter(DDSChannel::DDS0); ddsiter <= DDSChannel(DDSChannel::DDSMAX); 
	ddsiter++) 
    {

      if(ddsSet.isSet(ddsiter.id_)) {
	if(first) 
	  first = false;
	else 
	  os << "+";
      
	os << "dds" << ddsiter.getIntId();
      }
    }
  }
  return os.str();
}

/**.......................................................................
 * Return true, if this is a valid channel set
 */
bool DDSChannel::isValidChannelSet(DDSChannel::Id channels)
{
  // Return false if any bits higher than DDSMAX are set

  return ((unsigned)channels & ~((unsigned)DDSChannel::DDSALL)) ? false : true;
}


/**.......................................................................
 * Return the integer DDS Channel Id associated with this enumerator.
 *
 * @throws Exception if this enumerator does not represent
 * a single valid DDS Channel.
 */
unsigned int DDSChannel::getIntId(Id id)
{
  DDSChannel ddsiter(DDSChannel::DDS0), ddsmax(DDSChannel::DDSMAX);
  unsigned int idds;

  for(idds=0; ddsiter <= ddsmax; ddsiter++,idds++) {
    if(ddsiter.id_ == id)
      return idds;
  }

  // If no valid DDS Channel was found, throw an error.

  throw Error("DDSChannel::getIntId: DDS Channel enumerator does not specify "
  	      "a valid single channel id.\n");
}

unsigned int DDSChannel::getIntId()
{
  return getIntId(id_);
}

/**.......................................................................
 * Return true if the passed Id is part of this object's dds channel
 * set.  
 */
bool DDSChannel::isSet(DDSChannel& dds)
{
  return isSet(dds.id_);
}

/**.......................................................................
 * Return true if the passed Id is part of this object's dds channel
 * set.  
 */
bool DDSChannel::isSet(unsigned id)
{
  return (intToId(id) & id_);
}

/**.......................................................................
 * Return true if the passed Id is part of this object's dds channel
 * set.  
 */
bool DDSChannel::isSet(Id id)
{
  return (id & id_);
}

/**.......................................................................
 * Convert from DDS channel index to enumerator
 */
unsigned DDSChannel::idToInt(DDSChannel::Id id)
{
  return getIntId(id);
}

/**.......................................................................
 * Convert from DDS channel index to enumerator
 */
DDSChannel::Id DDSChannel::intToId(unsigned int idds)
{
  // If this is 0, return the first dds channel

  if(idds==0)
    return DDSChannel::DDS0;

  return (DDSChannel::Id)pow((double)2, (int)idds);
}

/**.......................................................................
 * Return the antenna id associated with this
 * enumerator.
 */
DDSChannel::Id DDSChannel::getId()
{
  return id_;
}

/**.......................................................................
 * Write the contents to an ostream
 */
ostream& 
sza::util::operator<<(ostream& os, const DDSChannel& dds)
{
  DDSChannel ddsiter(DDSChannel::DDS0), ddsmax(DDSChannel::DDSMAX);
  int idds;
  bool first=true;

  for(idds=0; ddsiter <= ddsmax; ddsiter++, idds++) {
    if(dds.id_ & ddsiter.id_) {
      if(!first)
	os << "+";
      os << "dds" << idds << ends;
      first=false;
    }
  }
  if(first)
    os << "Not a valid dds";
  return os;
}

/**.......................................................................
 * Addition operator for DDSChannel::Id.  Allows expressions like DDS0 +
 * DDS1
 */
DDSChannel::Id 
sza::util::operator+(const DDSChannel::Id id1, const DDSChannel::Id id2)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id1);
  i2 = static_cast<unsigned int>(id2);

  return static_cast<DDSChannel::Id>(i1 | i2);
}

/**.......................................................................
 * Subtraction operator for DDSChannel::Id.  Allows expressions like
 * DDSALL - DDS1
 */
DDSChannel::Id 
sza::util::operator-(const DDSChannel::Id id1, const DDSChannel::Id id2)
{
  unsigned int i1, i2;

  i1 = static_cast<unsigned int>(id1);
  i2 = static_cast<unsigned int>(id2);

  return static_cast<DDSChannel::Id>(i1 & ~i2);
}

/**.......................................................................
 * Set the id of this DDS Channel enumerator.
 */
void DDSChannel::setId(DDSChannel::Id id)
{
  id_ = id;
}
