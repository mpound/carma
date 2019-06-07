#include "carma/szautil/Exception.h"
#include "carma/szautil/RtdClient.h"

#include <arpa/inet.h>

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RtdClient::RtdClient(bool spawn, std::string host, unsigned connectPort) :
  Client(spawn, host,connectPort)
{
  unsigned sizeInBytes = 8;
  bytes_.resize(sizeInBytes);
  setReadBufSize(sizeInBytes + 8);
  setSendBufSize(sizeInBytes + 8);
}

/**.......................................................................
 * Destructor.
 */
RtdClient::~RtdClient() {}

void RtdClient::processServerData()
{
  data_.deserialize(bytes_);
  COUT("Got a message from the server: type = " << data_.getType());

  switch(data_.id_) {
  case RtdClientData::MEM_TEMPLATE:
    data_.template_.writeToFile();
    sendAddRegMsg(124780);
    break;
  case RtdClientData::MEM_DATAREGS:
    {
      unsigned int record;
      unsigned int* iptr = (unsigned int*)&data_.dataBytes_[0];
      COUT("Got data regs: size = " << data_.dataBytes_.size() << " " << iptr[0] 
	   << " hl = " << ntohl(iptr[0]));
    }
    break;
  case RtdClientData::MEM_ADDREGACK:
    COUT("Got an ADD ACK");
    break;
  case RtdClientData::MEM_REMREGACK:
    COUT("Got a REM ACK");
    break;
  default:
    CERR("Got an unrecognized message from the server");
    break;
  }
}

void RtdClient::sendAddRegMsg(unsigned id)
{
  COUT("Sending add reg msg");
  data_.addReg_ = id;
  data_.setTo(RtdClientData::MEM_ADDREG);
  sendServerData(data_);
}

void RtdClient::sendRemRegMsg(unsigned id)
{
  COUT("Sending rem reg msg");
  data_.remReg_ = id;
  data_.setTo(RtdClientData::MEM_REMREG);
  sendServerData(data_);
}
