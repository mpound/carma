#include "carma/szautil/Exception.h"
#include "carma/szautil/OffsetMsg.h"

/**.......................................................................
 * Write the contents of a OffsetMsg to an ostream
 */
std::ostream& sza::util::operator<<(std::ostream& os, OffsetMsg* msg)
{
  switch (msg->type) {

  case OffsetMsg::MOUNT:
    {
      COUT("Got a MOUNT command: " << std::endl 
	   << "  az = " << msg->body.mount.az << std::endl
	   << "  el = " << msg->body.mount.el << std::endl
	   << "  pa = " << msg->body.mount.pa);
    }
    break;

	
  case OffsetMsg::EQUAT:
    {
      COUT("Got a EQUAT command: " << std::endl 
	   << "  ra = " << msg->body.equat.ra << std::endl
	   << "  dec = " << msg->body.equat.dec);
    }
    break;

	
  case OffsetMsg::TV:
    {
      COUT("Got a TV command: " << std::endl 
	   << "  up = " << msg->body.tv.up << std::endl
	   << "  right = " << msg->body.tv.right);
    }
    break;

	
  case OffsetMsg::SKY:
    {
      COUT("Got a SKY command: " << std::endl 
	   << "  x = " << msg->body.sky.x << std::endl
	   << "  y = " << msg->body.sky.y);
    }
    break;

  default:
    break;

  }
  return os;
}

