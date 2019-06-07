#include "carma/szautil/NetUnion.h"
#include "carma/szautil/NetVar.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

#include <arpa/inet.h>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetUnion::NetUnion() 
{ 
  id_ = NETUNION_UNKNOWN;
}

/**.......................................................................
 * Copy constructors - we don't want to copy members_, as this
 * contains pointers to private memory locations in each object
 */
NetUnion::NetUnion(const NetUnion& netUnion) 
{
  *this = (NetUnion&) netUnion;
}

NetUnion::NetUnion(NetUnion& netUnion) 
{
  *this = netUnion;
}

/**.......................................................................
 * Assignment operators
 */
void NetUnion::operator=(const NetUnion& netUnion) 
{
  *this = (NetUnion&) netUnion;
}

void NetUnion::operator=(NetUnion& netUnion) 
{
  id_    = netUnion.id_;
  bytes_ = netUnion.bytes_;
}

/**.......................................................................
 * Destructor.
 */
NetUnion::~NetUnion() 
{
  // Delete any objects which were allocated by this one

  for(std::map<unsigned, NetDat::Info>::iterator i=members_.begin();
      i != members_.end(); i++) {
    if(i->second.alloc_) {
      delete i->second.datPtr_;
      i->second.datPtr_ = 0;
    }
  }
}

/**.......................................................................
 * Add a member
 */
void NetUnion::addMember(unsigned id, NetDat* netDat, bool alloc)
{
  // Don't let the user create an id corresponding to UNKNOWN

  if(id == NETUNION_UNKNOWN)
    ThrowError("Invalid union id: " << id);

  // And don't let the user install multiple members with the same id

  if(findMember(id) != 0)
    ThrowError("A member with id: " << id << " already exists");

  // Add the new member to the list

  members_[id] = NetDat::Info(netDat, alloc);
}

/**.......................................................................
 * Find a member by id
 */
NetDat* const NetUnion::findMember(unsigned id)
{
  std::map<unsigned int, NetDat::Info>::iterator slot = members_.find(id);

  if(slot == members_.end())
    return 0;

  return slot->second.datPtr_;
}

/**.......................................................................
 * Find a member by id
 */
bool NetUnion::memberIsValid(unsigned id)
{
  std::map<unsigned int, NetDat::Info>::iterator slot = members_.find(id);

  if(slot == members_.end())
    return false;

  return true;
}

/**.......................................................................
 * Find a member by id
 */
NetDat* const NetUnion::getMember(unsigned id)
{
  NetDat* netDat = findMember(id);

  if(netDat == 0 && !memberIsValid(id)) {
    ThrowError("Invalid member requested");
  }
    
  return netDat;
}

/**.......................................................................
 * Set this union to the requested member
 */
void NetUnion::setTo(unsigned id)
{
  // Get the requested member to make sure it exists

  NetDat* netDat = getMember(id);

  // And set the id

  id_ = id;
}

/**.......................................................................
 * Recalculate and resize
 */
void NetUnion::resize()
{
  for(std::map<unsigned, NetDat::Info>::iterator i=members_.begin();
      i != members_.end(); i++) {
    NetDat* dat = i->second.datPtr_;
    if(dat != 0) {
      dat->resize();
    }
  }  

  // Resize to the maximum needed to serialize any member, plus the
  // size of the id identifying which member is being serialized

  NetDat::resize(size());
}

/**.......................................................................
 * Return the maximum size of any member of this union
 */
unsigned NetUnion::maxSize()
{
  unsigned maxSizeInBytes=0;
  unsigned memberSize;

  // Iterate over members of this union, resizing each one

  for(std::map<unsigned, NetDat::Info>::iterator i=members_.begin();
      i != members_.end(); i++) {
    NetDat* dat = i->second.datPtr_;

    if(dat != 0) {
      memberSize = dat->size();
      maxSizeInBytes = (maxSizeInBytes > memberSize) ? maxSizeInBytes : memberSize;
    }
  }

  // Resize to the maximum needed to serialize any member, plus the member id tag

  return maxSizeInBytes + sizeof(id_);
}

/**.......................................................................
 * Return the number of bytes needed to serialize the
 * currently-selected member of this union
 */
unsigned NetUnion::size()
{
  return sizeOf(id_) + sizeof(id_);
}

/**.......................................................................
 * Serialize the data in this struct
 */
void NetUnion::serialize()
{
  NetDat* netDat = getMember(id_);

  if(bytes_.size() > 0) {

    unsigned char* dest = &bytes_[0];

    // Write the id in network byte order

    unsigned int idNetworkOrder = htonl(id_);
    const unsigned char* src = (const unsigned char*) &idNetworkOrder;
 
    for(unsigned i=0; i < sizeof(id_); i++) {
      *dest++ = *src++;
    }

    // Now serialize the appropriate member
    
    if(netDat != 0) {
      
      std::vector<unsigned char>& bytes = netDat->getSerializedData();
      unsigned dataSize = bytes.size();

      src = &bytes[0];
      for(unsigned i=0; i < dataSize; i++) {
	*dest++ = *src++;
      }

    }

  }
}

/**.......................................................................
 * De-serialize data into this struct
 */
void NetUnion::deserializeNativeOrder(const std::vector<unsigned char>& bytes)
{
  checkSize(bytes);
  deserializeNativeOrder(&bytes[0], bytes.size());
}

/**.......................................................................
 * De-serialize data into this struct
 */
void NetUnion::deserialize(const std::vector<unsigned char>& bytes)
{
  checkSize(bytes);
  deserialize(&bytes[0], bytes.size());
}

void NetUnion::deserialize(const unsigned char* src, unsigned size)
{
  unsigned int idNetworkOrder;
  unsigned char* dest  = (unsigned char*) &idNetworkOrder;

  for(unsigned i=0; i < sizeof(id_); i++)
    *dest++ = *src++;

  // EML have to comment this out for now, since the server I'm
  // testing against is not yet sending on network byte order

  id_ = ntohl(idNetworkOrder);

  // Get the requested member
   
  NetDat* netDat = getMember(id_);

  // And de-serialize the appropriate member

  if(netDat != 0) {

    NetVar* var = (NetVar*)netDat;
    if(var->isResizeable()) {

      unsigned remain = size - sizeof(id_);
      if(netDat->size() != remain) {
	netDat->resize(remain);
      }

    }

    netDat->deserialize(src);
  }
}

void NetUnion::deserializeNativeOrder(const unsigned char* src, unsigned size)
{
  unsigned int idNetworkOrder;
  unsigned char* dest  = (unsigned char*) &idNetworkOrder;

  for(unsigned i=0; i < sizeof(id_); i++)
    *dest++ = *src++;

  // EML have to comment this out for now, since the server I'm
  // testing against is not yet sending on network byte order

  id_ = idNetworkOrder;

  // Get the requested member
   
  NetDat* netDat = getMember(id_);

  // And de-serialize the appropriate member

  if(netDat != 0) {

    NetVar* var = (NetVar*)netDat;
    if(var->isResizeable()) {
      unsigned remain = size - sizeof(id_);
      if(netDat->size() != remain) {
	netDat->resize(remain);
      }
    }

    netDat->deserialize(src);
  }
}

void NetUnion::deserialize(const unsigned char* src)
{
  ThrowError("Deprecated function deserialize(const unsigned char* src) called");
}

/**.......................................................................
 * Check the size of an array against our size
 */
void NetUnion::checkSize(const std::vector<unsigned char>& bytes)
{
  if(bytes.size() < sizeof(id_))
    ThrowError("Data array is too small for a NetUnion");

  if(bytes.size() == 0)
    ThrowError("Encountered zero-sized object");
}

/**.......................................................................
 * Check the size of an array against our size
 */
void NetUnion::checkSize(const std::vector<unsigned char>& bytes, unsigned id)
{
  if(bytes.size() != getMember(id)->size())
    ThrowError("NetUnion: Data array has the wrong size for this object: bytes = " 
	       << bytes.size() << " id size = " << getMember(id)->size());
}

/**.......................................................................
 * Return the size of the requested member
 */
unsigned NetUnion::sizeOf(unsigned id)
{
  NetDat* dat = getMember(id);
  return (dat ? dat->size() : 0);
}

/**.......................................................................
 * Add a variable to the internal vector of members
 */
void NetUnion::addVar(unsigned id, 
		      sza::util::DataType::Type type, void* vPtr, unsigned nEl, bool convert)
{
  NetVar* netVar = new NetVar(type, vPtr, nEl, convert);
  addMember(id, netVar, true);
}

void NetUnion::addVar(unsigned id, sza::util::DataType::Type type, 
		      void* vPtr, bool convert)
{
  NetVar* netVar = new NetVar(type, vPtr, convert);
  addMember(id, netVar, true);
}

/**.......................................................................
 * Add just an id
 */
void NetUnion::addCase(unsigned id)
{
  addMember(id, 0, false);
}

/**.......................................................................
 * Return the message type
 */
unsigned NetUnion::getType()
{
  return id_;
}

void NetUnion::addVar(unsigned id, bool& val, bool convert)
{
  addVar(id, sza::util::DataType::BOOL, &val, 1, convert);
}

void NetUnion::addVar(unsigned id, unsigned char& val, bool convert)
{		     
  addVar(id, sza::util::DataType::UCHAR, &val, 1, convert);
}		     
		      
void NetUnion::addVar(unsigned id, char& val, bool convert)
{		     
  addVar(id, sza::util::DataType::CHAR, &val, 1, convert);
}		     
		      
void NetUnion::addVar(unsigned id, short& val, bool convert)
{		     
  addVar(id, sza::util::DataType::SHORT, &val, 1, convert);
}		     
		      
void NetUnion::addVar(unsigned id, unsigned short& val, bool convert)
{		     
  addVar(id, sza::util::DataType::USHORT, &val, 1, convert);
}		     
		      
void NetUnion::addVar(unsigned id, int& val, bool convert)
{		     
  addVar(id, sza::util::DataType::INT, &val, 1, convert);
}		     
		      
void NetUnion::addVar(unsigned id, unsigned int& val, bool convert)
{		     
  addVar(id, sza::util::DataType::UINT, &val, 1, convert);
}		     
		      
void NetUnion::addVar(unsigned id, float& val, bool convert)
{		     
  addVar(id, sza::util::DataType::FLOAT, &val, 1, convert);
}		     
		      
void NetUnion::addVar(unsigned id, double& val, bool convert)
{		     
  addVar(id, sza::util::DataType::DOUBLE, &val, 1, convert);
}		     
		      
void NetUnion::addVar(unsigned id, bool* val, unsigned nEl, bool convert)
{		     
  addVar(id, sza::util::DataType::BOOL, val, nEl, convert);
}		     

void NetUnion::addVar(unsigned id, unsigned char* val, unsigned nEl, bool convert)
{		     
  addVar(id, sza::util::DataType::UCHAR, val, nEl, convert);
}		     
		      
void NetUnion::addVar(unsigned id, char* val, unsigned nEl, bool convert)
{		     
  addVar(id, sza::util::DataType::CHAR, val, nEl, convert);
}		     
		      
void NetUnion::addVar(unsigned id, short* val, unsigned nEl, bool convert)
{		     
  addVar(id, sza::util::DataType::SHORT, val, nEl, convert);
}		     
		      
void NetUnion::addVar(unsigned id, unsigned short* val, unsigned nEl, bool convert)
{		     
  addVar(id, sza::util::DataType::USHORT, val, nEl, convert);
}		     
		      
void NetUnion::addVar(unsigned id, int* val, unsigned nEl, bool convert)
{		     
  addVar(id, sza::util::DataType::INT, val, nEl, convert);
}		     
		      
void NetUnion::addVar(unsigned id, unsigned int* val, unsigned nEl, bool convert)
{		     
  addVar(id, sza::util::DataType::UINT, val, nEl, convert);
}		     
		      
void NetUnion::addVar(unsigned id, float* val, unsigned nEl, bool convert)
{		     
  addVar(id, sza::util::DataType::FLOAT, val, nEl, convert);
}		     
		      
void NetUnion::addVar(unsigned id, double* val, unsigned nEl, bool convert)
{		     
  addVar(id, sza::util::DataType::DOUBLE, (void*)val, nEl, convert);
}		     
		      
void NetUnion::addVar(unsigned id, std::string& val, bool convert)
{
  addVar(id, sza::util::DataType::STRING, (void*)&val, convert);
}		     
		      
void NetUnion::addVar(unsigned id, std::vector<unsigned char>& val, bool convert)
{
  addVar(id, sza::util::DataType::UCHAR, (void*)&val, convert);
}



