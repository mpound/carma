#include "carma/szautil/Exception.h"
#include "carma/szautil/RtdClientTask.h"

#include <arpa/inet.h>

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RtdClientTask::RtdClientTask(bool spawn, std::string host, unsigned connectPort) :
  ClientTask<RtdClientMsg>(spawn, host, connectPort)
{
  unsigned sizeInBytes = 8;
  bytes_.resize(sizeInBytes);
  setReadBufSize(sizeInBytes + 8);
  setSendBufSize(sizeInBytes + 8);
}

/**.......................................................................
 * Destructor.
 */
RtdClientTask::~RtdClientTask() {}

void RtdClientTask::processMsg(RtdClientMsg* msg)
{
  COUTCOLOR("Inside processmsg with type = " << msg->type, "red");

  switch(msg->type) {
  case RtdClientMsg::ADD_REG:
    COUTCOLOR("Inside processMsg with ADD_REG", "red");
    data_.addReg_ = msg->body.regId_;
    data_.setTo(RtdClientData::MEM_ADDREG);
    sendServerData(data_);
    break;
  case RtdClientMsg::REM_REG:
    data_.remReg_ = msg->body.regId_;
    data_.setTo(RtdClientData::MEM_REMREG);
    sendServerData(data_);
    break;
  default:
    break;
  }
}

void RtdClientTask::processServerData()
{
  data_.deserialize(bytes_);
  CTOUT("Got a message from the server: type = " << data_.getType());

  switch(data_.id_) {
  case RtdClientData::MEM_TEMPLATE:
    {
      CTOUT("Got a template message");
    }
    break;
  case RtdClientData::MEM_DATAREGS:
    {
      CTOUT("Data size = " << data_.dataBytes_.size());
#if 0
      if(data_.dataBytes_.size() >= 4) {
	unsigned int* iptr = (unsigned int*)&data_.dataBytes_[0];
	unsigned int nl = *iptr;
	unsigned int hl = ntohl(nl);
	float* fptr = (float*) &hl;
	COUT("Float val = " << fptr[0]);
      }

      if(data_.dataBytes_.size() > 4) {
	unsigned int* iptr = (unsigned int*)&data_.dataBytes_[4];
	COUT("Int val  = " << ntohl(iptr[0]));
      }

      if(data_.dataBytes_.size() > 8) {
	unsigned char* cptr = (unsigned char*)&data_.dataBytes_[8];
	COUT("string val = " << cptr);
      }
#endif

    }
    break;
  case RtdClientData::MEM_ADDREGACK:
    CTOUT("Got an ADDREG acknowledgement");
    break;
  case RtdClientData::MEM_REMREGACK:
    CTOUT("Got a REMREG acknowledgement");
    break;
  default:
    CERR("Got an unrecognized message from the server");
    break;
  }
}

void RtdClientTask::sendAddRegMsg(unsigned id)
{
  COUTCOLOR("Sending add reg msg with id = " << id, "red");
  RtdClientMsg msg;
  msg.type = RtdClientMsg::ADD_REG;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  msg.body.regId_ = id;
  sendTaskMsg(&msg);
}

void RtdClientTask::sendRemRegMsg(unsigned id)
{
  COUTCOLOR("Sending remove reg msg with id = " << id, "red");
  RtdClientMsg msg;
  msg.type = RtdClientMsg::REM_REG;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  msg.body.regId_ = id;
  sendTaskMsg(&msg);
}
