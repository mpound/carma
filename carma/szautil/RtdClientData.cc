#include "carma/szautil/RtdClientData.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RtdClientData::RtdClientData() 
{
  // Add the template as a member of this union

  addMember(MEM_TEMPLATE, &template_, false);

  // Add the addReg_ int as a member, in network byte order

  addVar(MEM_ADDREG,   addReg_, true);

  // Add the remReg_ int as a member, in network byte order

  addVar(MEM_REMREG,   remReg_, true);

  // Add the byte array as a member, in network byte order

  addVar(MEM_DATAREGS, dataBytes_,  true);

  // Add the ACK messages too

  addMember(MEM_ADDREGACK);  
  addMember(MEM_REMREGACK);
}

/**.......................................................................
 * Destructor.
 */
RtdClientData::~RtdClientData() {}

std::ostream& sza::util::operator<<(std::ostream& os, RtdClientData& data)
{
  switch(data.id_) {
  case RtdClientData::MEM_TEMPLATE:
    os << "Template";
    break;
  case RtdClientData::MEM_ADDREG:
    os << "Addreg command id = " << data.addReg_;
    break;
  case RtdClientData::MEM_REMREG:
    os << "Addreg command id = " << data.addReg_;
    break;
  default:
    os << "Data regs";
    break;
  }
  return os;
}
