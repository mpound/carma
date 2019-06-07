#include "carma/szautil/NetMonitorFrame.h"

#include "carma/szautil/NetMonitorFrameClient.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetMonitorFrameClient::NetMonitorFrameClient(bool spawnThread, std::string host, unsigned port, NetMonitorFrame* nmf, int fdWrite, bool spawnSignalHandler) :
  Client(spawnThread, host, port, 0, 0, spawnSignalHandler)
{
  nmf_     = nmf;
  fdWrite_ = fdWrite;

  unsigned sizeInBytes = 8;
  bytes_.resize(sizeInBytes);
  setReadBufSize(sizeInBytes + 8);
  setSendBufSize(sizeInBytes + 8);
}

/**.......................................................................
 * Destructor.
 */
NetMonitorFrameClient::~NetMonitorFrameClient() {};

void NetMonitorFrameClient::processServerData()
{
  nmf_->nadfm_.deserialize(bytes_);

  COUT("Type = " << nmf_->nadfm_.getType());

#if 0
  unsigned char srcname[100];
  nmf_->nadfm_.readReg("Control", "Subarray1", "source", srcname);
  COUT("Just read " << srcname);
#endif

  notify();
}

/**.......................................................................
 * Notify anyone registered that data have arrived.
 */
void NetMonitorFrameClient::notify()
{
  if(fdWrite_ != -1) {
    unsigned byte = 0x0;
    ::write(fdWrite_, &byte, 1);
  } 
}
