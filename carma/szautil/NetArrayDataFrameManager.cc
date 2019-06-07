#include "carma/szautil/NetArrayDataFrameManager.h"
#include "carma/szautil/DataFrameNormal.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetArrayDataFrameManager::NetArrayDataFrameManager() 
{
  // Add the template as a member of this net union

  addMember(MEM_TEMPLATE, &nat_, false);

  // Add the frame data as a member of this union

  addVar(MEM_FRAME, ((DataFrameNormal*)frame_)->lvals_);

  // add the ucvec too

  addVar(MEM_UCVEC, ucvec_);
}

/**.......................................................................
 * Destructor.
 */
NetArrayDataFrameManager::~NetArrayDataFrameManager() {}

void NetArrayDataFrameManager::deserialize(const std::vector<unsigned char>& bytes)
{
  //  COUT("(2) Array map size before deserialize is now: " << arrayMap_->nByte_);
  //  COUT("(2) data vec size before deserialize is now:  " << frame()->dataVec().size());

  NetUnion::deserialize(bytes);

  //  COUT("(2) Array map size after deserialize is now: " << arrayMap_->nByte_);
  //  COUT("(2) data vec size after deserialize is now:  " << frame()->dataVec().size());

  // Now if a template was received, re-set our template based on it.

  if(getType() == NetArrayDataFrameManager::MEM_TEMPLATE) {
    resetArrayMap();
  }

  //  COUT("(2) Array map size is now: " << arrayMap_->nByte_);
  //  COUT("(2) data vec size is now:  " << frame()->dataVec().size());
}

void NetArrayDataFrameManager::deserializeNativeOrder(const std::vector<unsigned char>& bytes)
{
  //  COUT("(2) Array map size before deserialize is now: " << arrayMap_->nByte_);
  //  COUT("(2) data vec size before deserialize is now:  " << frame()->dataVec().size());

  NetUnion::deserializeNativeOrder(bytes);

  //  COUT("(2) Array map size after deserialize is now: " << arrayMap_->nByte_);
  //  COUT("(2) data vec size after deserialize is now:  " << frame()->dataVec().size());

  // Now if a template was received, re-set our template based on it.

  if(getType() == NetArrayDataFrameManager::MEM_TEMPLATE) {
    resetArrayMap();
  }

  //  COUT("(2) Array map size is now: " << arrayMap_->nByte_);
  //  COUT("(2) data vec size is now:  " << frame()->dataVec().size());
}

void NetArrayDataFrameManager::setTo(ArrayTemplate* arrayTemplate)
{
  nat_.setTo(arrayTemplate);
  resetArrayMap();

  ((DataFrameNormal*)frame_)->lvals_[0] = 'q';
  ((DataFrameNormal*)frame_)->lvals_[99] = 's';
}

void NetArrayDataFrameManager::setTo(unsigned id)
{
  NetUnion::setTo(id);
}

void NetArrayDataFrameManager::resetArrayMap()
{
  ArrayMap* arrayMap = new ArrayMap(nat_.arrayTemplate_, false, false);
  initialize(arrayMap, false);
  //  delete arrayMap;
}

ArrayTemplate* NetArrayDataFrameManager::getArrayTemplate()
{
  return nat_.getArrayTemplate();
}

void NetArrayDataFrameManager::resize()
{
  NetUnion::resize();
}




