#include "carma/szautil/FastPdbClient.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
FastPdbClient::FastPdbClient(bool spawn, std::string host, unsigned connectPort) :
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
FastPdbClient::~FastPdbClient() {}

void FastPdbClient::processServerData()
{
  data_.deserialize(bytes_);
  COUT("Got a message from the server: type = " << data_.getType());

  switch(data_.id_) {
  case FastPdbData::MEM_RESPONSE:
    COUT(data_.response_);
    break;
  default:
    CERR("Got an unrecognized message from the server");
    break;
  }
}

void FastPdbClient::sendListSourceMsg(std::string src)
{
  COUT("Sending list source msg");
  data_.setTo(FastPdbData::MEM_SRCLIST);
  data_.source_ = src;
  sendServerData(data_);
}
