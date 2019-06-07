#include "carma/antenna/sza/antenna/corba/CarmaSeqNum.h"

using namespace std;

using namespace sza::antenna::corba;

/**.......................................................................
 * Constructor.
 */
CarmaSeqNum::CarmaSeqNum() 
{
  seq_     = 0;
  success_ = true;
}

/**.......................................................................
 * Destructor.
 */
CarmaSeqNum::~CarmaSeqNum() {}

void CarmaSeqNum::setSeq(unsigned seq, bool success)
{
  guard_.lock();
  seq_ = seq;
  guard_.unlock();
}

unsigned CarmaSeqNum::getSeq()
{
  unsigned seq;
  guard_.lock();
  seq = seq_;
  guard_.unlock();

  return seq;
}

bool CarmaSeqNum::getSuccess()
{
  bool success;
  guard_.lock();
  success = success_;
  guard_.unlock();

  return success;
}
