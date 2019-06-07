#include "carma/szautil/MonitorPoint.h"
#include "carma/szautil/PagerMonitor.h"
#include "carma/szautil/RegParser.h"

using namespace std;
using namespace sza::util;

unsigned PagerMonitor::defaultNframe_ = 1;

/**.......................................................................
 * Constructor.
 */
PagerMonitor::PagerMonitor(ArrayDataFrameManager* source) :
  source_(source), dest_(*source), mpManager_(&dest_), handler_(0) {}

/**.......................................................................
 * Constructor.
 */
PagerMonitor::PagerMonitor() {}

/**.......................................................................
 * Destructor.
 */
PagerMonitor::~PagerMonitor() {}

void PagerMonitor::setHandler(MONITOR_CONDITION_HANDLER(*handler), void* arg)
{
  handler_ = handler;
  arg_     = arg;
}

/**.......................................................................
 * A method to call to check registers
 */
void PagerMonitor::checkRegisters()
{
  // Iterate over the vector of register specifications, copying the
  // relevant data for each one

  for(unsigned iReg=0; iReg < registers_.size(); iReg++)
    registers_[iReg].copyRegister();
}

/**.......................................................................
 * Copy registers from source to destination.
 */
void PagerMonitor::RegSpec::copyRegister()
{
  switch(DataType::typeOf(block_)) {
  case DataType::BOOL:
    mp_->writeReg(false, (bool*)sourcePtr_, &range_);
    break;
  case DataType::UCHAR:
    mp_->writeReg(false, (unsigned char*)sourcePtr_, &range_);
    break;
  case DataType::CHAR:
    mp_->writeReg(false, (char*)sourcePtr_, &range_);
    break;
  case DataType::USHORT:
    mp_->writeReg(false, (unsigned short*)sourcePtr_, &range_);
    break;
  case DataType::SHORT:
    mp_->writeReg(false, (short*)sourcePtr_, &range_);
    break;
  case DataType::UINT:
    mp_->writeReg(false, (unsigned int*)sourcePtr_, &range_);
    break;
  case DataType::INT:
    mp_->writeReg(false, (int*)sourcePtr_, &range_);
    break;
  case DataType::ULONG:
    mp_->writeReg(false, (unsigned long*)sourcePtr_, &range_);
    break;
  case DataType::LONG:
    mp_->writeReg(false, (long*)sourcePtr_, &range_);
    break;
  case DataType::FLOAT:
    mp_->writeReg(false, (float*)sourcePtr_, &range_);
    break;
  case DataType::DOUBLE:
    mp_->writeReg(false, (double*)sourcePtr_, &range_);
    break;
  default:
    break;
  }
}
  
/**.......................................................................
 * Add a monitor point to activate the pager when a register
 * goes out of the specified range
 */
std::vector<PagerMonitor::RegSpec>
PagerMonitor::addOutOfRangeMonitorPoint(std::string regSpec, 
					double min, double max, 
					bool delta,
					unsigned nFrame,
					std::string comment)
  
{
  // And add monitor points associated with the passed register
  // specification

  DataType dMin = min;
  DataType dMax = max;
  
  return addMonitorPoint(regSpec, 
			 DataTypeTruthFn::lessThanOrGreaterThan, 
			 dMin, dMax, delta, nFrame, true, comment);
}

/**.......................................................................
 * Add a monitor point to activate the pager when a register
 * goes into the specified range
 */
std::vector<PagerMonitor::RegSpec>
PagerMonitor::addInRangeMonitorPoint(std::string regSpec, 
				     double min, double max, 
				     bool delta,
				     unsigned nFrame,
				     std::string comment)
{
  // And add monitor points associated with the passed register
  // specification

  DataType dMin = min;
  DataType dMax = max;

  return addMonitorPoint(regSpec, 
			 DataTypeTruthFn::greaterThanEqAndLessThanEq, 
			 dMin, dMax, delta, nFrame, false, comment);
}

/**.......................................................................
 * And add monitor points associated with a register specification
 */
std::vector<PagerMonitor::RegSpec>
PagerMonitor::addMonitorPoint(std::string regSpec, 
			      DataTypeTruthFn fn,
			      DataType& min,
			      DataType& max,
			      bool delta,				   
			      unsigned nFrame,
			      bool outOfRange,
			      std::string comment)
{
  // First remove the register, just in case it already exists

  remMonitorPoint(regSpec);

  // Parse the input register description into discrete coordinate
  // ranges

  RegParser parser;

  std::vector<RegDescription> regDescs = parser.inputRegs(regSpec, REG_INPUT_RANGE);
  std::vector<RegSpec> regSpecs;

  std::ostringstream os;

  // Now iterate over all discrete index ranges, adding a monitor
  // point for each one.
 
  for(unsigned iDesc=0; iDesc < regDescs.size(); iDesc++) {

    RegDescription& desc = regDescs[iDesc];

    try {
    os.str("");
    os << desc;

    MonitorPoint* mp = 0;

    // Only add the monitor point if one doesn't already exist.

    mp = mpManager_.addMonitorPoint(desc.regMapName(), desc.boardName(), 
				    desc.blockName(), desc.getRangePtr());

    // Ensure that the min/max are converted to the right data type
    // for this register.  

    min.convertToTypeOf(desc.block());
    max.convertToTypeOf(desc.block());

    // And create the condition for this register

    MonitorCondition condition(fn, min, max, delta, 0, nFrame, 0);

    // Install the condition, and a handler, into the monitor
    // condition

    mp->registerConditionHandler(handler_, arg_, os.str(), comment, 
				 condition, 
				 true, desc.getRangePtr());
    
    RegSpec regSpec(source_, desc, mp, outOfRange);

    // And add it to our vector of monitor points

    registers_.push_back(regSpec);

    // And add it to the temporary list of register specifications
    // added by this call

    regSpecs.push_back(regSpec);

    } catch(Exception& err) {
      COUT(err.what());
    }
  }

  // Return the vector of register specifications added by this call

  return regSpecs;
}

/**.......................................................................
 * And remove monitor points associated with a register specification
 */
void PagerMonitor::remMonitorPoint(std::string regSpec)
{
  // Parse the input register description into discrete coordinate
  // ranges

  RegParser parser;

  std::vector<RegDescription> regDescs = parser.inputRegs(regSpec);

  std::ostringstream os;

  // Now iterate over all discrete index ranges, adding a monitor
  // point for each one.
 
  for(unsigned iDesc=0; iDesc < regDescs.size(); iDesc++) {

    RegDescription& desc = regDescs[iDesc];

    os.str("");
    os << desc;

    MonitorPoint* mp = 0;

    // If this monitor point exists in the list of registers we are
    // managing, delete it
 
    if((mp = mpManager_.findMonitorPoint(desc.regMapName(), desc.boardName(), 
					 desc.blockName(), desc.getRangePtr(), false)) != 0) {

      // Find the RegSpec object that corresponds to this register
      
      std::vector<PagerMonitor::RegSpec>::iterator regIter;

      for(regIter = registers_.begin(); regIter != registers_.end(); regIter++) {
	if(regIter->mp_ == mp)
	  break;
      }

      if(regIter != registers_.end()) {
	registers_.erase(regIter);
      }

      // And delete the monitor point from the manager too

      mpManager_.remMonitorPoint(desc.regMapName(), desc.boardName(), 
				 desc.blockName(), desc.getRangePtr());
    }
  }
}

/**.......................................................................
 * Constructor for RegSpec
 */
PagerMonitor::RegSpec::RegSpec(std::string name, double min, double max, 
			       bool isDelta, unsigned nFrame, bool isOutOfRange)
{
  block_        = 0;
  mp_           = 0;
  sourcePtr_    = 0;

  name_         = name;
  isDelta_      = isDelta;
  nFrame_       = nFrame;
  min_          = min;
  max_          = max;
  isOutOfRange_ = isOutOfRange;
}

/**.......................................................................
 * Constructors for RegSpec
 */
PagerMonitor::RegSpec::RegSpec(ArrayDataFrameManager* sourceFm, 
			       RegDescription& desc, MonitorPoint* mp,
			       bool outOfRange)
{
  block_        = desc.block();
  range_        = desc.range();
  mp_           = mp;
  sourcePtr_    = sourceFm->getPtr(desc);

  ostringstream os;
  os << desc;

  name_         = os.str(); 
  isDelta_      =  mp_->isDelta();
  nFrame_       =  mp_->nFrame();
  min_          =  mp_->min();
  max_          =  mp_->max();

  isOutOfRange_ =  outOfRange;
}

PagerMonitor::RegSpec::RegSpec(RegSpec& regSpec)
{
  block_        = regSpec.block_;
  range_        = regSpec.range_;
  mp_           = regSpec.mp_;
  sourcePtr_    = regSpec.sourcePtr_;

  name_         = regSpec.name_;
  isDelta_      = regSpec.isDelta_;
  nFrame_       = regSpec.nFrame_;
  min_          = regSpec.min_;
  max_          = regSpec.max_;
  isOutOfRange_ = regSpec.isOutOfRange_;
}

PagerMonitor::RegSpec::RegSpec(const RegSpec& regSpec)
{
  block_        = regSpec.block_;
  range_        = regSpec.range_;
  mp_           = regSpec.mp_;
  sourcePtr_    = regSpec.sourcePtr_;

  name_         = regSpec.name_;
  isDelta_      = regSpec.isDelta_;
  nFrame_       = regSpec.nFrame_;
  min_          = regSpec.min_;
  max_          = regSpec.max_;
  isOutOfRange_ = regSpec.isOutOfRange_;
}

/**.......................................................................
 * Reset all monitor points
 */
void PagerMonitor::reset()
{
  mpManager_.reset();
}

/**.......................................................................
 * Clear this manager of all monitor points
 */
void PagerMonitor::clear()
{
  mpManager_.clear();
  registers_.clear();
}

/**.......................................................................
 * List all monitor points
 */
void PagerMonitor::list()
{
  mpManager_.list();
}

/**.......................................................................
 * List all monitor points
 */
std::vector<std::string> PagerMonitor::getList(bool sort)
{
  return mpManager_.getList(sort);
}

std::ostream& sza::util::operator<<(std::ostream& os, PagerMonitor& pm)
{
  os << pm.mpManager_;
  return os;
}

/**.......................................................................
 * List all monitor points
 */
std::vector<std::string> PagerMonitor::format(std::string regSpec)
{
  std::vector<std::string> regs;
  
  // Parse the input register description into discrete coordinate
  // ranges

  RegParser parser;
  std::vector<RegDescription> regDescs = parser.inputRegs(regSpec);
  std::ostringstream os;

  // Now iterate over all discrete index ranges, formatting the
  // monitor point for each one.
 
  for(unsigned iDesc=0; iDesc < regDescs.size(); iDesc++) {

    RegDescription& desc = regDescs[iDesc];

    os.str("");
    os << desc;

    MonitorPoint* mp = 0;

    // Only add the monitor point if one doesn't already exist.

    regs.push_back(mpManager_.formatReg(desc.regMapName(), desc.boardName(), 
					desc.blockName(), desc.getRangePtr()));
  }

  return regs;
}

/**.......................................................................
 * Return a list of all monitor points
 */
std::vector<PagerMonitor::RegSpec> PagerMonitor::getRegs()
{
  return registers_;
}

