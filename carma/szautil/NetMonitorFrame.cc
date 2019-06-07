#include "carma/szautil/NetMonitorFrame.h"

using namespace std;

using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
NetMonitorFrame::NetMonitorFrame() {}

/**.......................................................................
 * Destructor.
 */
NetMonitorFrame::~NetMonitorFrame() {}

void NetMonitorFrame::print()
{
  COUT("PRINTING***********************************************************************");

  switch(nadfm_.getType()) {
  case NetArrayDataFrameManager::MEM_TEMPLATE:
    printArrayTemplate(nadfm_.nat_.arrayTemplate_);
    break;
  case NetArrayDataFrameManager::MEM_FRAME:
    {
      std::vector<unsigned char>& dataVec = nadfm_.frame()->dataVec();
      COUT("Frame data size = " << dataVec.size() << " " << dataVec[0] << " " << dataVec[99]);
#if 0
      float fVal;
      nadfm_.readReg("WbPipeline", "TsysStageContainer.StageStats", "procTime", &fVal);
      COUT("Read float: " << fVal);

      int fCount,  rCount;
      nadfm_.readReg("WbPipeline", "IntegratorStageContainer.IntegratorStage", "frameCount", &fCount);
      nadfm_.readReg("WbPipeline", "IntegratorStageContainer.IntegratorStage", "frameCount", &rCount);
      COUT("Read int: " << fCount << " " << rCount);
#endif

    }
    break;
  case NetArrayDataFrameManager::MEM_UCVEC:
    {
      std::vector<unsigned char>& dataVec = nadfm_.ucvec_;
      COUT("Frame data size = " << dataVec.size() << " " << dataVec[0] << " " << dataVec[99]);
    }
    break;
  default:
    break;
  }

  COUT("PRINTING***********************************************************************DONE");
}

void NetMonitorFrame::printArrayTemplate(ArrayTemplate* arrayTemplate)
{
  COUT(arrayTemplate->ntemplate);
  COUT(arrayTemplate->templates[0].name);
  COUT(arrayTemplate->templates[0].regtemplate->nboard);
  COUT(arrayTemplate->templates[0].regtemplate->boards[0].name_);
  COUT(arrayTemplate->templates[0].regtemplate->boards[0].nblock_);
  COUT(arrayTemplate->templates[0].regtemplate->boards[0].blocks_[0].name_);
  COUT(arrayTemplate->templates[0].regtemplate->boards[0].blocks_[0].axes_->nEl(0));
  COUT(arrayTemplate->templates[0].regtemplate->boards[0].blocks_[0].axes_->nEl(1));
}

ArrayTemplate* NetMonitorFrame::getArrayTemplate()
{
  return nadfm_.getArrayTemplate();
}

ArrayMap* NetMonitorFrame::getArrayMap()
{
  return nadfm_.arrayMap();
}


