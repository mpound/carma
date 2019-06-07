#include "carma/szautil/Unit.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
Unit::Unit() {}

/**.......................................................................
 * Destructor.
 */
Unit::~Unit() {}

/**.......................................................................
 * Return true if the passed name is a recognized name for this
 * unit
 */
bool Unit::isThisUnit(std::string unitName)
{
  addNames();

  for(unsigned iName=0; iName < names_.size(); iName++) {
    if(unitName == names_[iName])
      return true;
  }
  return false;
}

/**.......................................................................
 * Add a name to this list
 */
void Unit::addName(std::string name)
{
  names_.push_back(name);
}

void Unit::addNames()
{
  COUT("Calling Unit::addNames() stub");
}
