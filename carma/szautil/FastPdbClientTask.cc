#include "carma/szautil/CondVar.h"
#include "carma/szautil/FastPdbClientTask.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
FastPdbClientTask::FastPdbClientTask(bool spawn, std::string host, unsigned connectPort, CondVar* condVar) :
  ClientTask<FastPdbClientMsg>(spawn, host, connectPort)
{
  unsigned sizeInBytes = 8;
  bytes_.resize(sizeInBytes);
  setReadBufSize(sizeInBytes + 8);
  setSendBufSize(sizeInBytes + 8);

  callback_ = 0;
  condVar_  = condVar;
}

/**.......................................................................
 * Destructor.
 */
FastPdbClientTask::~FastPdbClientTask() {}

void FastPdbClientTask::processMsg(FastPdbClientMsg* msg)
{
  switch(msg->type) {
  case FastPdbClientMsg::SRCLIST:
    data_.setTo(FastPdbData::MEM_SRCLIST);
    data_.source_ = msg->body.source;
    callback_ = msg->callback;
    sendServerData(data_);
    break;
  case FastPdbClientMsg::PROJLIST:
    data_.setTo(FastPdbData::MEM_PROJLIST);
    data_.project_ = msg->body.project;
    callback_ = msg->callback;
    sendServerData(data_);
    break;
  default:
    break;
  }
}

void FastPdbClientTask::processServerData()
{
  data_.deserialize(bytes_);

  switch(data_.id_) {
  case FastPdbData::MEM_RESPONSE:
    if(callback_) {
      (*callback_)(data_.response_);
    } else {
      COUT(data_.response_);
    }
    stop_ = true;
    break;
  default:
    CERR("Got an unrecognized message from the server");
    break;
  }
}

void FastPdbClientTask::sendListSourceMsg(std::string src, FASTPDB_CALLBACK_FN(*callback))
{
  FastPdbClientMsg msg;
  strncpy(msg.body.source, &src[0], src.size());
  msg.body.source[src.size()] = '\0';
  msg.callback = callback;
  msg.type = FastPdbClientMsg::SRCLIST;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  sendTaskMsg(&msg);
}

void FastPdbClientTask::sendListProjectMsg(std::string project, FASTPDB_CALLBACK_FN(*callback))
{
  FastPdbClientMsg msg;
  strncpy(msg.body.project, &project[0], project.size());
  msg.body.project[project.size()] = '\0';
  msg.callback = callback;
  msg.type = FastPdbClientMsg::PROJLIST;
  msg.genericMsgType_ = GenericTaskMsg::TASK_SPECIFIC;
  sendTaskMsg(&msg);
}

void FastPdbClientTask::serviceMsgQ()
{
  int nready=0;
	  
  do {
	    
    // Block in select() until one or more file descriptors are readable
	    
    if((nready=select(fdSet_.size(), 
		      fdSet_.readFdSet(), 
		      fdSet_.writeFdSet(), 
		      NULL, timeOutPtr_)) < 0) 
    {
      ThrowSysError("select()");
    }
	    
    if(nready > 0) {
	      
      // read data received over the socket connection
	      
      if(fdSet_.isSetInRead(handler_.getReadFd()))
	handler_.read();
	      
      // send data over the socket connection
	      
      if(fdSet_.isSetInWrite(handler_.getSendFd()))
	handler_.send();
	      
      // Else check our message queue for task messages

      if(fdSet_.isSetInRead(msgq_.fd())) {
	processTaskMsg(&stop_);
      }

    } else {
      if(connect()) {
	if(condVar_) {
	  condVar_->broadcast();
	}
      }
    }
	    
  } while(!stop_);
}
