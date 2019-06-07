#include "carma/ui/rtd/common/MonitorCellScaled.h"

#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/util/ErrorException.h"
#include "carma/util/StringUtils.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;
using namespace carma::util;
using namespace sza::util;

const double MonitorCellScaled::eps_ = 1e-12;

double MonitorCellScaled::getNativeValAsDouble(MonitorPoint* mp, int sampleNo) 
{
  double dval;
  float fval;
  short sval;
  int ival;
  std::string str;

  switch (mp->getValuetype()) {
  case MONITOR_VALUE_TYPE_SHORT:
    sval = mp->getMonitorPointSample(sampleNo).getMonitorValue().sh;
    dval = (double)(sval);
    break;
  case MONITOR_VALUE_TYPE_INTEGER:		
    ival = (int)mp->getMonitorPointSample(sampleNo).getMonitorValue().lo;
    dval = (double)(ival);
    break;
  case MONITOR_VALUE_TYPE_FLOAT:		
    fval = (float)mp->getMonitorPointSample(sampleNo).getMonitorValue().fl;
    dval = (double)(fval);
    break;					
  case MONITOR_VALUE_TYPE_DOUBLE:		
    dval = (double)mp->getMonitorPointSample(sampleNo).getMonitorValue().db;
    break;					
  default:						
    throw CARMA_ERROR("Unhandled CARMA data type");  
    break;	     
  }

  return dval;
}

double MonitorCellScaled::getNativeValAsDouble(MonitorPoint* mp1, int sampleNo1, Operator oper, MonitorPoint* mp2, int sampleNo2) 
{
  double dval  = getNativeValAsDouble(mp1, sampleNo1);
  double dval2 = 0.0;

  if(oper != OPER_NONE) {
    dval2 = getNativeValAsDouble(mp2, sampleNo2);

    switch(oper) {
    case OPER_ADD:
      dval += dval2;
      break;
    case OPER_SUB:
      dval -= dval2;
      break;
    case OPER_MULT:
      dval *= dval2;
      break;
    case OPER_DIV:
      dval /= dval2;
      break;
    default:
      ThrowCarmaError("Unsupported operator: " << oper);
      break;
    }
  }

  return dval;
}

void MonitorCellScaled::scaleVal(double& dval, double multiplier, double offset) 
{
  // Scale by the multiplier and offset

  dval = dval * multiplier + offset;

  // Scale this value by the frequency too

  dval = getFrequencyScaledValue(dval, antenna_, freqOperator_, freqNormalization_);
}

double MonitorCellScaled::getScaledValAsDouble(MonitorPoint* mp, double multiplier, double offset, int sampleNo) 
{
  double dval = getNativeValAsDouble(mp, sampleNo);
  scaleVal(dval, multiplier, offset);
  return dval;
}
    
double MonitorCellScaled::getScaledValAsDouble(MonitorPoint* mp1, int sampleNo1, Operator oper, MonitorPoint* mp2, int sampleNo2,
					       double multiplier, double offset)
{
  double dval = getNativeValAsDouble(mp1, sampleNo1, oper, mp2, sampleNo2);
  scaleVal(dval, multiplier, offset);
  return dval;
}

/**.......................................................................
 * Constructor.
 */
MonitorCellScaled::MonitorCellScaled( const int      cellWidth,
				      const bool     setMpWidth,
				      MonitorPoint & mp,
				      const int      sampleNo,
				      double multiplier,
				      double offset,
				      short int precision) : 
  MonitorCell(cellWidth, setMpWidth, mp, sampleNo),
  mp2_(mp),
  sampleNo2_(sampleNo)
{
  privateConstructor(mp, multiplier, offset, precision);
}

MonitorCellScaled::MonitorCellScaled( const int      cellWidth,
				      const bool     setMpWidth,
				      MonitorPoint & mp1,
				      const int      sampleNo1,
				      Operator oper,
				      MonitorPoint & mp2,
				      const int      sampleNo2,
				      double multiplier,
				      double offset,
				      short int precision) : 
  MonitorCell(cellWidth, setMpWidth, mp1, sampleNo1),
  mp2_(mp2),
  sampleNo2_(sampleNo2)
{
  privateConstructor(oper, multiplier, offset, precision);
}

void MonitorCellScaled::privateConstructor(MonitorPoint& mp, 
					   double multiplier, double offset, short int precision)
{
  multiplier_   = multiplier;
  offset_       = offset;
  precision_    = precision;
  color_        = WHITE_CELL_COLOR;
  checkOutlier_ = false;
  outlierDelta_ = 0.0;
  checkDeviationFromVal_ = false;
  valDelta_     = 0.0;
  freqOperator_ = OPER_NONE;
  mpOperator_   = OPER_NONE;

  switch (mp_.getValuetype()) {
  case MONITOR_VALUE_TYPE_SHORT:
    packFn_ = packShort;
    break;
  case MONITOR_VALUE_TYPE_INTEGER:
    packFn_ = packInt;
    break;
  case MONITOR_VALUE_TYPE_FLOAT:
    packFn_ = packFloat;
    break;
  case MONITOR_VALUE_TYPE_DOUBLE:
    packFn_ = packDouble;
    break;
  default:
    std::cout << " Unurecognized type " << mp_.getValuetype() << std::endl;
    throw CARMA_ERROR("Unhandled CARMA data type");
    break;
  }
}

void MonitorCellScaled::privateConstructor(Operator oper, double multiplier, double offset, short int precision)
{
  multiplier_   = multiplier;
  offset_       = offset;
  precision_    = precision;
  color_        = WHITE_CELL_COLOR;
  checkOutlier_ = false;
  outlierDelta_ = 0.0;
  checkDeviationFromVal_ = false;
  valDelta_     = 0.0;
  freqOperator_ = OPER_NONE;
  mpOperator_   = oper;

  packFn_ = packExpr;
}

void MonitorCellScaled::setColor(CellColor color)
{
  color_ = color;
}

/**.......................................................................
 * Constructor.
 */
MonitorCellScaled::MonitorCellScaled( const int cellWidth,
				      MonitorPoint& mp,
				      double multiplier,
				      double offset,
				      short int precision) :
  MonitorCell(cellWidth, true, mp, 0),
  mp2_(mp),
  sampleNo2_(0)
{
  privateConstructor(mp, multiplier, offset, precision);  
}

MonitorCellPtr
MonitorCellScaled::makeCell(const int      cellWidth,
			    MonitorPoint & mp,
			    double multiplier,
			    double offset,
			    short int precision) 
{
  // Don't want to default to MonitorCell::makeCell even if scale
  // factors are 1.0 and 0.0, since precision is then ignored in
  // formatting

  switch (mp.getValuetype()) {

    // The following types can't be scaled

  case MONITOR_VALUE_TYPE_BOOLEAN:
  case MONITOR_VALUE_TYPE_COMPLEX:
  case MONITOR_VALUE_TYPE_STRING:
  case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
    return MonitorCell::makeCell(cellWidth, mp);
  default:
    return MonitorCellPtr(new MonitorCellScaled(cellWidth, mp, multiplier, offset, precision));
    break;
  }
}

MonitorCellPtr
MonitorCellScaled::makeCell(const int      cellWidth,
			    MonitorPoint & mp1,
			    Operator oper,
			    MonitorPoint & mp2,
			    double multiplier,
			    double offset,
			    short int precision)
{
  // Don't want to default to MonitorCell::makeCell even if scale
  // factors are 1.0 and 0.0, since precision is then ignored in
  // formatting

  switch (mp1.getValuetype()) {

    // The following types can't be scaled

  case MONITOR_VALUE_TYPE_BOOLEAN:
  case MONITOR_VALUE_TYPE_COMPLEX:
  case MONITOR_VALUE_TYPE_STRING:
  case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
    return MonitorCell::makeCell(cellWidth, mp1);
  default:
    return MonitorCellPtr(new MonitorCellScaled(cellWidth, true, mp1, 0, oper, mp2, 0, multiplier, offset, precision));
    break;
  }
}

/**.......................................................................
 * Destructor.
 */
MonitorCellScaled::~MonitorCellScaled() {}

std::string MonitorCellScaled::computeText()
{
  return packFn_(this);
}

MP_PACK_FN(MonitorCellScaled::packShort)
{
  double dval;
  short sval;
  sval = cell->mp_.getMonitorPointSample(cell->sampleNo_).getMonitorValue().sh;
  dval = (double)(sval) * cell->multiplier_ + cell->offset_;

  return cell->formatValue(dval);
}

MP_PACK_FN(MonitorCellScaled::packUint)
{
  
  double dval;
  unsigned int uival;
  uival = (unsigned int)cell->mp_.getMonitorPointSample(cell->sampleNo_).getMonitorValue().lo;
  dval = (double)(uival) * cell->multiplier_ + cell->offset_;

  return cell->formatValue(dval);
}

MP_PACK_FN(MonitorCellScaled::packInt)
{
  double dval;
  int ival;
  ival = (int)cell->mp_.getMonitorPointSample(cell->sampleNo_).getMonitorValue().lo;

  dval = (double)(ival) * cell->multiplier_ + cell->offset_;

  return cell->formatValue(dval);
}

MP_PACK_FN(MonitorCellScaled::packFloat)
{
  double dval;
  float fval;
  fval = (float)cell->mp_.getMonitorPointSample(cell->sampleNo_).getMonitorValue().fl;
  dval = (double)(fval) * cell->multiplier_ + cell->offset_;

  return cell->formatValue(dval);
}

MP_PACK_FN(MonitorCellScaled::packDouble)
{
  double dval;
  dval = (double)cell->mp_.getMonitorPointSample(cell->sampleNo_).getMonitorValue().db;
  dval = dval * cell->multiplier_ + cell->offset_;

  return cell->formatValue(dval);
}

MP_PACK_FN(MonitorCellScaled::packExpr)
{
  double dval = cell->getNativeValAsDouble(&cell->mp_, cell->sampleNo_, cell->mpOperator_, &cell->mp2_, cell->sampleNo2_);
  dval = dval * cell->multiplier_ + cell->offset_;

  return cell->formatValue(dval);
}

std::string MonitorCellScaled::formatValue(double dval)
{
  dval = getFrequencyScaledValue(dval, antenna_, freqOperator_, freqNormalization_);

  ostringstream s;
  s.setf(ios::fixed);
  s << setprecision(precision_) << dval;

  return s.str();
}

void MonitorCellScaled::recast()
{
  if(mp_.getValuetype() == MONITOR_VALUE_TYPE_INTEGER) {

    packFn_ = packUint;
  }
}

/**.......................................................................
 * Overloaded method to compute the current color of this cell.
 *
 * If requested to color the cell depending on deviations, check if
 * all conditions are met.  Otherwise give it the default color
 */
CellColor MonitorCellScaled::computeColor()
{
  ostringstream os;
  os << "Inside computeColor for " << getName() << " color_ = " << color_ << std::endl;
  RtDisplay::appendToFile(os.str());

  if(checkOutlier_ && isOutlier())
    return conditionsAreMet() ? RED_CELL_COLOR : color_;
  else if(checkDeviationFromVal_ && deviatesFromVal())
    return conditionsAreMet() ? RED_CELL_COLOR : color_;
  else
    return color_;
}

/**.......................................................................
 * Use this method to create a monitor point whose color
 * represents whether or not:
 * 
 * fabs(mpval - mean(mp)) > permittedDelta
 */
void MonitorCellScaled::errorOnAbsDeviationFromMean(std::vector<ScaledMp>& mpVec, 
						    Delta permittedDelta,
						    Grouping group)
{
  if(group != GROUP_EXACT && mpVec.size() != AntennaMapper::nAntenna_) {
    ThrowCarmaError("Grouping by antenna type of subarray requires mpVec.size() = " << AntennaMapper::nAntenna_);
  }

  grouping_     = group;
  checkOutlier_ = true;
  outlierDelta_ = permittedDelta;
  outlierVec_   = mpVec;
}

/**.......................................................................
 * Return true if the current value of this monitor point is an
 * outlier, for the grouping that was requested
 */
bool MonitorCellScaled::isOutlier()
{
  double meanVal=0.0;
  double dVal;

  if(grouping_ != GROUP_EXACT && !hasAntenna()) {
    ThrowCarmaError("You have requsted grouping by antenna type or subarray, "
		    "but no antenna has been specified for this monitor point");
  }

  switch(grouping_) {

    //------------------------------------------------------------
    // If exact grouping was requested, just iterate over the vector
    // that was passed
    //------------------------------------------------------------

  case GROUP_EXACT:
    {
  ostringstream os;
  os << "Checking EXACT for " << getName() << std::endl;
  RtDisplay::appendToFile(os.str());

      for(unsigned i=0; i < outlierVec_.size(); i++) {
	ScaledMp& smp = outlierVec_[i];
	dVal = smp.getNativeValAsDouble();
	scaleVal(dVal, smp.multiplier_, smp.offset_);
	meanVal += (dVal - meanVal) / (i + 1);
      }
    }
    break;

    //------------------------------------------------------------
    // If grouping by antenna type was requested, iterate over the
    // vector of like antennas
    //------------------------------------------------------------

  case GROUP_ANTTYPE:
    {
  ostringstream os;
  os << "Checking ANT for " << getName() << std::endl;
  RtDisplay::appendToFile(os.str());

      std::vector<AntennaMapper::Antenna* >* antVec = antenna_->antennasByType_;
      for(unsigned iAnt=0; iAnt < antVec->size(); iAnt++) {
	AntennaMapper::Antenna* ant = antVec->at(iAnt);
	unsigned antIndex = ant->carmaAntNo_ - 1;

	ScaledMp& smp = outlierVec_[antIndex];
	dVal = smp.getNativeValAsDouble();
	scaleVal(dVal, smp.multiplier_, smp.offset_);
	meanVal += (dVal - meanVal) / (iAnt + 1);
      }
    }
    break;

    //------------------------------------------------------------
    // Else iterate over all antennas in the same subarray
    //------------------------------------------------------------

  default:
    {
  ostringstream os;
  os << "Checking SUBARRAY for " << getName() << std::endl;
  RtDisplay::appendToFile(os.str());

      std::vector<AntennaMapper::Antenna* >* antVec = antenna_->antennasBySubarray_;

      if(antVec == 0) {
	return false;
      }

      for(unsigned iAnt=0; iAnt < antVec->size(); iAnt++) {
	AntennaMapper::Antenna* ant = antVec->at(iAnt);
	unsigned antIndex = ant->carmaAntNo_ - 1;

	ScaledMp& smp = outlierVec_[antIndex];
	dVal = smp.getNativeValAsDouble();
	scaleVal(dVal, smp.multiplier_, smp.offset_);
	meanVal += (dVal - meanVal) / (iAnt + 1);
      }
    }
    break;
  }

  ostringstream os;

  if(mpOperator_ == OPER_NONE)
    dVal = getScaledValAsDouble(&mp_, multiplier_, offset_, sampleNo_);
  else
    dVal = getScaledValAsDouble(&mp_, sampleNo_, mpOperator_, &mp2_, sampleNo2_, multiplier_, offset_);

  bool res = (fabs(dVal - meanVal) > outlierDelta_.getValue(antenna_));

  if(res) {
    os.str("");
    os << getName() << " is outlier (" << res << ") because mean = " << meanVal << " and dVal = " << dVal << " and delta  = " << outlierDelta_.getValue(antenna_) << std::endl;
    RtDisplay::appendToFile(os.str());
  }

  return res;
}

/**.......................................................................
 * Use this method to create a monitor point whose color
 * represents whether or not 
 * 
 * fabs(mpval - mean(mp)) > permittedDelta
 */
void MonitorCellScaled::errorOnAbsDeviationFromVal(double val, Delta permittedDelta)
{
  checkDeviationFromVal_ = true;
  valDelta_ = permittedDelta;
  val_ = val;
}

/**.......................................................................
 * Return true if the current value of this monitor point deviates 
 * from the specified value
 */
bool MonitorCellScaled::deviatesFromVal()
{
  double dVal;
  dVal = getScaledValAsDouble(&mp_, multiplier_, offset_, sampleNo_);

#if 0
  if(antenna_)
    std::cout << "ant: " << antenna_->carmaAntNo_ << " Comparing val for " << getName() << " to " << valDelta_.getValue(antenna_) << std::endl;
  else
    std::cout << " Comparing val for " << getName() << " to " << valDelta_.getValue(antenna_) << std::endl;
#endif

  return (fabs(dVal - val_) > valDelta_.getValue(antenna_));
}

/**.......................................................................
 * Set up this monitor point value to be scaled by the current LO frequency
 */
void MonitorCellScaled::scaleByFrequency(Operator oper, sza::util::Frequency normalization)
{
  freqOperator_ = oper;
  freqNormalization_ = normalization;
}

/**.......................................................................
 * Return a version of this monitor point, scaled by the current
 * frequency
 */
double MonitorCellScaled::getFrequencyScaledValue(double dval, AntennaMapper::Antenna* antenna, Operator oper, Frequency& freqNorm)
{
  if(oper != OPER_NONE) {

    if(antenna == 0) 
      ThrowCarmaError("A frequency scaled delta was specified, but no valid antenna descriptor is available");
    
    double ratio = antenna->frequency_.GHz() / freqNorm.GHz();

    switch(oper) {
    case OPER_MULT:
      dval *= ratio;
      break;
    case OPER_DIV:
      dval /= ratio;
      break;
    default:
      ThrowCarmaError("Unsupported frequency scaling");
      break;
    }
  }
  return dval;
}

