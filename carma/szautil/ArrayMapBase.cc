#include "carma/szautil/ArrayMapBase.h"

using namespace std;
using namespace sza::util;

ArrayMap* ArrayMapBase::arrayMap_ = 0;

unsigned ArrayMapBase::refCount_ = 0;

/**.......................................................................
 * Constructor.
 */
ArrayMapBase::ArrayMapBase() 
{
  // If the array map hasn't been allocated yet, do it now
  
  if(arrayMap_ == 0) {
    if((arrayMap_ = new_SzaArrayMap())==0) {
      LogStream errStr;
      errStr.appendMessage(true, "Unable to allocate array map\n");
      throw Error(errStr);
    }
  }
  
  // And increment the reference counter.  This will be used to tell
  // when arrayMap_ should be deleted
  
  refCount_++;
}

/**.......................................................................
 * Destructor.
 */
ArrayMapBase::~ArrayMapBase() 
{
  // Decrement the ref counter
  
  refCount_--;
  
  // Once the ref count drops to zero, it is safe to delete the array
  // map
  
  if(refCount_ == 0) {
    if(arrayMap_ != 0) {
      arrayMap_ = del_SzaArrayMap(arrayMap_);
      arrayMap_ = 0;
    }
  }
}

/**.......................................................................
 * Copy constructor
 */
ArrayMapBase::ArrayMapBase(ArrayMapBase& arrayMapBase)
{
  // We must increment the global reference counter.  Otherwise when
  // the destructor is called on this copy, arrayMap_ could be
  // erroneously deleted.
  
  refCount_++;
}
