#include "carma/szautil/NetDat.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetDat::NetDat() {}

/**.......................................................................
 * Copy constructors
 */
NetDat::NetDat(const NetDat& netDat)
{
  bytes_   = netDat.bytes_;
}

NetDat::NetDat(NetDat& netDat)
{
  bytes_   = netDat.bytes_;
}

/**.......................................................................
 * Copy constructors
 */
NetDat& NetDat::operator=(const NetDat& netDat)
{
  bytes_   = netDat.bytes_;
}

NetDat& NetDat::operator=(NetDat& netDat)
{
  bytes_   = netDat.bytes_;
}

/**.......................................................................
 * Destructor.
 */
NetDat::~NetDat() {}

/**.......................................................................
 * Total size of the serialized version of this variable, in bytes.
 */
unsigned NetDat::size()
{
  COUT("Inside NetDat size with size = " << bytes_.size());
  return bytes_.size();
}

/**.......................................................................
 * Resize this object
 */
void NetDat::resize(unsigned size)
{
  if(size != bytes_.size()) {
    bytes_.resize(size);
  }
}

void NetDat::resize() 
{
}

/**.......................................................................
 * Return a reference to serialized data
 */
std::vector<unsigned char>& NetDat::getSerializedData()
{
  // Recalculate and resize the underlying byte array

  resize();

  // Serialize the data into it

  serialize();

  // And return a pointer to the result

  return bytes_;
}

/**.......................................................................
 * Return a reference to serialized data
 */
std::vector<unsigned char>& NetDat::getSerializedDataNoResize()
{
  // Serialize the data into it

  serialize();

  // And return a pointer to the result

  return bytes_;
}

/**.......................................................................
 * Return a pointer to serialized data
 */
unsigned char* const NetDat::getSerializedDataPtr()
{
  std::vector<unsigned char>& bytes = getSerializedData();

  if(bytes.size() > 0)
    return &bytes_[0];

  return 0;
}

/**.......................................................................
 * Return a pointer to serialized data
 */
void NetDat::packSerializedData(unsigned char* dest)
{
  unsigned char* src = getSerializedDataPtr();

  for(unsigned i=0; i < size(); i++)
    *dest++ = *src++;
}


/**.......................................................................
 * Check the size of an array against our size
 */
void NetDat::checkSize(const std::vector<unsigned char>& bytes)
{
  if(bytes.size() != size())
    ThrowError("NetDat: Data array has the wrong size for this object"
	       << bytes.size() << " size = " << size());

  if(bytes.size() == 0)
    ThrowError("Encountered zero-sized object");
}

/**.......................................................................
 * Deserialize data into this object
 */
void NetDat::deserialize(const std::vector<unsigned char>& bytes) {}

/**.......................................................................
 * Serialize the data
 */
void NetDat::serialize() {};

/**.......................................................................
 * Private deserialization method
 */
void NetDat::deserialize(const unsigned char* bytes) {}
