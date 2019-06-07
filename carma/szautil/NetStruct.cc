#include "carma/szautil/NetStruct.h"
#include "carma/szautil/NetVar.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetStruct::NetStruct() {}

/**.......................................................................
 * Copy constructors - we don't want to copy members_, as this
 * contains pointers to private memory locations in each object
 */
NetStruct::NetStruct(const NetStruct& netStruct) 
{
  bytes_   = netStruct.bytes_;
}

NetStruct::NetStruct(NetStruct& netStruct) 
{
  bytes_   = netStruct.bytes_;
}

void NetStruct::operator=(const NetStruct& netStruct) 
{
  bytes_   = netStruct.bytes_;
}

void NetStruct::operator=(NetStruct& netStruct) 
{
  bytes_   = netStruct.bytes_;
}

/**.......................................................................
 * Destructor.
 */
NetStruct::~NetStruct() 
{
  // Delete any objects which were allocated by this object

  for(unsigned i=0; i < members_.size(); i++) {
    if(members_[i].alloc_) {
      delete members_[i].datPtr_;
      members_[i].datPtr_ = 0;
    }
  }
}

/**.......................................................................
 * Add a member
 */
void NetStruct::addMember(NetDat* netDat, bool alloc)
{
  members_.push_back(NetDat::Info(netDat, alloc));
}

/**.......................................................................
 * Recalculate size and resize the underlying byte array
 */
void NetStruct::resize()
{
  // Iterate over members, resizing each one

  for(unsigned i=0; i < members_.size(); i++)
    members_[i].datPtr_->resize();

  // Now resize the underlying byte array to the new size

  NetDat::resize(size());
}

/**.......................................................................
 * Serialize the data in this struct
 */
void NetStruct::serialize()
{
  unsigned iByte=0;

  if(size() > 0) {
    unsigned char* dest = &bytes_[0];

    // Iterate over members, serializing each one

    for(unsigned i=0; i < members_.size(); i++, iByte++) {

      const unsigned char* src = members_[i].datPtr_->getSerializedDataPtr();

      // Iterate over the size of this member

      for(unsigned iEl=0; iEl < members_[i].datPtr_->size(); iEl++) {
	*dest++ = *src++;
      }
    }
  }
}

/**.......................................................................
 * De-serialize data into this struct
 */
void NetStruct::deserialize(const std::vector<unsigned char>& bytes)
{
  checkSize(bytes);

  deserialize(&bytes[0]);
}

void NetStruct::deserialize(const unsigned char* bytes)
{
  // Iterate over members, de-serializing into each one

  const unsigned char* src = &bytes[0];
  
  for(unsigned iMem=0; iMem < members_.size(); iMem++) {
    members_[iMem].datPtr_->deserialize(src);
    src += members_[iMem].datPtr_->size();
  }
}

/**.......................................................................
 * Check the size of an array against our size
 */
void NetStruct::checkSize(const std::vector<unsigned char>& bytes)
{
  if(bytes.size() == 0)
    ThrowError("Encountered zero-sized object");
}

/**.......................................................................
 * Add a variable to the internal vector of members
 */
void NetStruct::addVar(sza::util::DataType::Type type, void* vPtr, unsigned nEl)
{
  NetVar* netVar = new NetVar(type, vPtr, nEl);
  addMember(netVar, true);
}

/**.......................................................................
 * Add a variable to the internal vector of members
 */
void NetStruct::addVar(sza::util::DataType::Type type, void* vPtr)
{
  NetVar* netVar = new NetVar(type, vPtr);
  addMember(netVar, true);
}

/**.......................................................................
 * Return the size of this object.
 */
unsigned NetStruct::size()
{
  unsigned size=0;

  for(unsigned iMem=0; iMem < members_.size(); iMem++) {
    size += members_[iMem].datPtr_->size();
  }

  return size;
}

void NetStruct::addVar(bool& val)
{
  addVar(sza::util::DataType::BOOL, &val, 1);
}

void NetStruct::addVar(unsigned char& val)
{		     
  addVar(sza::util::DataType::UCHAR, &val, 1);
}		     
		      
void NetStruct::addVar(char& val)
{		     
  addVar(sza::util::DataType::CHAR, &val, 1);
}		     
		      
void NetStruct::addVar(short& val)
{		     
  addVar(sza::util::DataType::SHORT, &val, 1);
}		     
		      
void NetStruct::addVar(unsigned short& val)
{		     
  addVar(sza::util::DataType::USHORT, &val, 1);
}		     
		      
void NetStruct::addVar(int& val)
{		     
  addVar(sza::util::DataType::INT, &val, 1);
}		     
		      
void NetStruct::addVar(unsigned int& val)
{		     
  addVar(sza::util::DataType::UINT, &val, 1);
}		     
		      
void NetStruct::addVar(float& val)
{		     
  addVar(sza::util::DataType::FLOAT, &val, 1);
}		     
		      
void NetStruct::addVar(double& val)
{		     
  addVar(sza::util::DataType::DOUBLE, &val, 1);
}		     
		      
void NetStruct::addVar(bool* val, unsigned nEl)
{		     
  addVar(sza::util::DataType::BOOL, val, nEl);
}		     

void NetStruct::addVar(unsigned char* val, unsigned nEl)
{		     
  addVar(sza::util::DataType::UCHAR, val, nEl);
}		     
		      
void NetStruct::addVar(char* val, unsigned nEl)
{		     
  addVar(sza::util::DataType::CHAR, val, nEl);
}		     
		      
void NetStruct::addVar(short* val, unsigned nEl)
{		     
  addVar(sza::util::DataType::SHORT, val, nEl);
}		     
		      
void NetStruct::addVar(unsigned short* val, unsigned nEl)
{		     
  addVar(sza::util::DataType::USHORT, val, nEl);
}		     
		      
void NetStruct::addVar(int* val, unsigned nEl)
{		     
  addVar(sza::util::DataType::INT, val, nEl);
}		     
		      
void NetStruct::addVar(unsigned int* val, unsigned nEl)
{		     
  addVar(sza::util::DataType::UINT, val, nEl);
}		     
		      
void NetStruct::addVar(float* val, unsigned nEl)
{		     
  addVar(sza::util::DataType::FLOAT, val, nEl);
}		     
		      
void NetStruct::addVar(double* val, unsigned nEl)
{		     
  addVar(sza::util::DataType::DOUBLE, (void*)val, nEl);
}		     
		      
void NetStruct::addVar(std::string& val)
{
  addVar(sza::util::DataType::STRING, (void*)&val);
}		     
		      
void NetStruct::addVar(std::vector<unsigned char>& val)
{
  addVar(sza::util::DataType::UCHAR, (void*)&val);
}


