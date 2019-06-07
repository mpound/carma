#include "carma/szautil/RegisterSet.h"

using namespace std;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
RegisterSet::RegisterSet(ArrayMap* arrayMap, bool archivedOnly) :
archivedOnly_(archivedOnly) 
{
  regSet_ = 0;
  regSet_ = new_RegSet(arrayMap == 0 ? arrayMap_.arrayMap() : arrayMap);
}

/**.......................................................................
 * Destructor.
 */
RegisterSet::~RegisterSet() 
{
  if(regSet_ != 0) 
    regSet_ = del_RegSet(regSet_);
}

/**.......................................................................
 * Add a register description to a register set
 */
void RegisterSet::addRegister(RegDescription& desc)
{
  if(addRegSetRange(regSet_, desc)==1)
    ThrowError("Error adding register");
}

/**.......................................................................
 * Add a register description to a register set
 */
void RegisterSet::addRegister(RegDescription* desc)
{
  if(desc==0)
    ThrowError("NULL register description");

  addRegister(*desc);
}

/**.......................................................................
 * Add a vector of register descriptions to a register set
 */
void RegisterSet::addRegisters(std::vector<RegDescription>& regs)
{
  for(unsigned iReg=0; iReg < regs.size(); iReg++)
    if(addRegSetRange(regSet_, regs[iReg])==1)
      ThrowError("Error adding register");
}

/**.......................................................................
 * Add a vector of register descriptions to a register set
 */
void RegisterSet::reset()
{
  if(clr_RegSet(regSet_))
    ThrowError("Error clearing register set");
}

/**.......................................................................
 * Add a vector of register descriptions to a register set
 */
bool RegisterSet::operator==(RegisterSet& regSet)
{
  if(equiv_RegSet(regSet.regSet(), regSet_) == 1)
    return true;
  return false;
}

/**.......................................................................
 * Add a vector of register descriptions to a register set
 */
void RegisterSet::operator=(RegisterSet const& registerSet)
{
  if(dup_RegSet(regSet_, registerSet.regSet_))
    ThrowError("Error duplicating register sets");
}

