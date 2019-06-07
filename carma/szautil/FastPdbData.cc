#include "carma/szautil/FastPdbData.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
FastPdbData::FastPdbData() 
{
  addVar(MEM_SRCLIST,  source_,   false);
  addVar(MEM_PROJLIST, project_,  false);
  addVar(MEM_RESPONSE, response_, false);
}

/**.......................................................................
 * Destructor.
 */
FastPdbData::~FastPdbData() {}

std::ostream& sza::util::operator<<(std::ostream& os, FastPdbData& data)
{
  switch(data.id_) {
  case FastPdbData::MEM_SRCLIST:
    os << "List source: '" << data.source_ << "'";
    break;
  case FastPdbData::MEM_PROJLIST:
    os << "Project list";
    break;
  default:
    os << "Default";
    break;
  }
  return os;
}
