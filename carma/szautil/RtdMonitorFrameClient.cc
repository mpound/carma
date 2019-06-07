#include "carma/szautil/NetMonitorFrame.h"
#include "carma/szautil/RtdMonitorFrameClient.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RtdMonitorFrameClient::
RtdMonitorFrameClient(bool spawnThread, std::string host, unsigned port, NetMonitorFrame* nmf) :
  NetMonitorFrameClient(spawnThread, host, port, nmf, -1, false)
{
}

RtdMonitorFrameClient::
RtdMonitorFrameClient(bool spawnThread, std::string host, unsigned port, NetMonitorFrame* nmf, int fdWrite) :
  NetMonitorFrameClient(spawnThread, host, port, nmf, fdWrite, false)
{
}

/**.......................................................................
 * Destructor.
 */
RtdMonitorFrameClient::~RtdMonitorFrameClient() {};

/**.......................................................................
 * Respond to data from the server
 */
void RtdMonitorFrameClient::processServerData()
{
  nmf_->nadfm_.deserialize(bytes_);
  notify();
}
