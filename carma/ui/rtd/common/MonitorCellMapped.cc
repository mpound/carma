#include "carma/ui/rtd/common/MonitorCellMapped.h"

#include "carma/ui/rtd/common/RtDisplay.h"

#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/util/ErrorException.h"
#include "carma/util/StringUtils.h"
#include "carma/util/programLogging.h"

#include <iostream>

using namespace ::std;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;
using namespace carma::util;

/**.......................................................................
 * Constructor.
 */
MonitorCellMapped::MonitorCellMapped( const int      cellWidth,
				      const bool     setMpWidth,
				      MonitorPoint & mp,
				      const int      sampleNo,
				      std::map<std::string, std::string> labelMap) :
  MonitorCell(cellWidth, setMpWidth, mp, sampleNo) 
{
  privateConstructor(mp, labelMap);
}

void MonitorCellMapped::privateConstructor(MonitorPoint& mp, 
					   std::map<std::string, std::string> labelMap)
{
  labelMap_ =  labelMap;
  iBit_     = -1;
  formatFn_ =  0;
  defaultLabel_ = "Unknown";
  defaultColor_ = EMPTY_CELL_COLOR;
}

/**.......................................................................
 * Constructor.
 */
MonitorCellMapped::MonitorCellMapped( const int cellWidth,
				      MonitorPoint& mp,
				      std::map<std::string, std::string> labelMap) :
  MonitorCell(cellWidth, true, mp, 0) 
{
  privateConstructor(mp, labelMap);  
}

MonitorCellMappedPtr
MonitorCellMapped::makeCell(const int      cellWidth,
			    MonitorPoint & mp,
			    std::map<std::string, std::string> labelMap)
{
  return MonitorCellMappedPtr(new MonitorCellMapped(cellWidth, mp, labelMap));
}

MonitorCellMappedPtr
MonitorCellMapped::makeCell(const int      cellWidth,
			    MonitorPoint & mp,
			    int iBit,
			    std::map<std::string, std::string> labelMap)
{
  boost::shared_ptr<MonitorCellMapped> cell(new MonitorCellMapped(cellWidth, mp, labelMap));

  switch (mp.getValuetype()) {
  case MONITOR_VALUE_TYPE_BYTE:
    cell->formatFn_ = formatByte;
    cell->setBit(iBit);
    break;
  case MONITOR_VALUE_TYPE_BOOLEAN:
    cell->formatFn_ = formatBool;
    cell->setBit(iBit);
    break;
  case MONITOR_VALUE_TYPE_SHORT:
    cell->formatFn_ = formatShort;
    cell->setBit(iBit);
    break;
  case MONITOR_VALUE_TYPE_INTEGER:
    cell->formatFn_ = formatInt;
    cell->setBit(iBit);
    break;
  default:
    break;
  }

  return cell;
}

MonitorCellMappedPtr
MonitorCellMapped::makeCell(const int cellWidth,
			    MonitorPoint& mp)
{
  std::map<std::string, std::string> labelMap;

  labelMap["0"] = "False";
  labelMap["1"] = "True";

  return MonitorCellMappedPtr(new MonitorCellMapped(cellWidth, mp, labelMap));
}

MonitorCellMappedPtr MonitorCellMapped::makeCell(const int     cellWidth,
					 MonitorPoint& mp,
					 std::string state0)
{
  std::map<std::string, std::string> labelMap;

  labelMap["0"] = state0;

  return MonitorCellMappedPtr(new MonitorCellMapped(cellWidth, mp, labelMap));
}

MonitorCellMappedPtr MonitorCellMapped::makeCell(const int     cellWidth,
					 MonitorPoint& mp,
					 std::string state0,
					 std::string state1)
{
  std::map<std::string, std::string> labelMap;

  labelMap["0"] = state0;
  labelMap["1"] = state1;

  return MonitorCellMappedPtr(new MonitorCellMapped(cellWidth, mp, labelMap));
}

MonitorCellMappedPtr
MonitorCellMapped::makeCell(const int     cellWidth,
			    MonitorPoint& mp,
			    int iBit,
			    std::string state0,
			    std::string state1)
{
  std::map<std::string, std::string> labelMap;

  labelMap["0"] = state0;
  labelMap["1"] = state1;

  return makeCell(cellWidth, mp, iBit, labelMap);
}

MonitorCellMappedPtr
MonitorCellMapped::makeCell(const int     cellWidth,
			    MonitorPoint& mp,
			    std::string state0,
			    std::string state1,
			    std::string state2)
{
  std::map<std::string, std::string> labelMap;
  
  labelMap["0"] = state0;
  labelMap["1"] = state1;
  labelMap["2"] = state2;

  return MonitorCellMappedPtr(new MonitorCellMapped(cellWidth, mp, labelMap));
}

MonitorCellMappedPtr
MonitorCellMapped::makeCell(const int     cellWidth,
			    MonitorPoint& mp,
			    std::string state0,
			    std::string state1,
			    std::string state2,
			    std::string state3)
{
  std::map<std::string, std::string> labelMap;
  
  labelMap["0"] = state0;
  labelMap["1"] = state1;
  labelMap["2"] = state2;
  labelMap["3"] = state3;

  return MonitorCellMappedPtr(new MonitorCellMapped(cellWidth, mp, labelMap));
}

MonitorCellMappedPtr
MonitorCellMapped::makeCell(const int     cellWidth,
			    MonitorPoint& mp,
			    std::string state0,
			    std::string state1,
			    std::string state2,
			    std::string state3,
			    std::string state4)
{
  std::map<std::string, std::string> labelMap;
  
  labelMap["0"] = state0;
  labelMap["1"] = state1;
  labelMap["2"] = state2;
  labelMap["3"] = state3;
  labelMap["4"] = state4;

  return MonitorCellMappedPtr(new MonitorCellMapped(cellWidth, mp, labelMap));
}

MonitorCellMappedPtr
MonitorCellMapped::makeCell(const int     cellWidth,
			    MonitorPoint& mp,
			    std::string state0,
			    std::string state1,
			    std::string state2,
			    std::string state3,
			    std::string state4,
			    std::string state5)
{
  std::map<std::string, std::string> labelMap;
  
  labelMap["0"] = state0;
  labelMap["1"] = state1;
  labelMap["2"] = state2;
  labelMap["3"] = state3;
  labelMap["4"] = state4;
  labelMap["5"] = state5;

  return MonitorCellMappedPtr(new MonitorCellMapped(cellWidth, mp, labelMap));
}

MonitorCellMappedPtr
MonitorCellMapped::makeCell(const int     cellWidth,
			    MonitorPoint& mp,
			    std::string state0,
			    std::string state1,
			    std::string state2,
			    std::string state3,
			    std::string state4,
			    std::string state5,
			    std::string state6)
{
  std::map<std::string, std::string> labelMap;
  
  labelMap["0"] = state0;
  labelMap["1"] = state1;
  labelMap["2"] = state2;
  labelMap["3"] = state3;
  labelMap["4"] = state4;
  labelMap["5"] = state5;
  labelMap["6"] = state6;

  return MonitorCellMappedPtr(new MonitorCellMapped(cellWidth, mp, labelMap));
}

MonitorCellMappedPtr
MonitorCellMapped::makeCell(const int     cellWidth,
			    MonitorPoint& mp,
			    std::string state0,
			    std::string state1,
			    std::string state2,
			    std::string state3,
			    std::string state4,
			    std::string state5,
			    std::string state6,
			    std::string state7)
{
  std::map<std::string, std::string> labelMap;
  
  labelMap["0"] = state0;
  labelMap["1"] = state1;
  labelMap["2"] = state2;
  labelMap["3"] = state3;
  labelMap["4"] = state4;
  labelMap["5"] = state5;
  labelMap["6"] = state6;
  labelMap["7"] = state7;

  return MonitorCellMappedPtr(new MonitorCellMapped(cellWidth, mp, labelMap));
}
  
/**.......................................................................
 * Destructor.
 */
MonitorCellMapped::~MonitorCellMapped() {}

/**.......................................................................
 * Format text for this monitor point
 */
std::string MonitorCellMapped::computeText()
{
  RtDisplay::appendToFile("Inside computeText 1");

  std::string text;
  std::string label;

  // If a bit number was specified, extract the value of that bit.
  // Else call the base-class computeText() method

  if(iBit_ < 0) {
    text = MonitorCell::computeText();
  } else {
    std::ostringstream os;
    os << (*formatFn_)(mp_, sampleNo_, iBit_);
    text = os.str();
  }

  ostringstream os;
  os << "Inside computeText 2 text = '" << text << "'";
  RtDisplay::appendToFile(os.str());
  
  // If no label was found corresponding to this mp value, color the
  // cell whatever the default cell color is.

  if(labelMap_.find(text) == labelMap_.end()) {

    currentColor_ = defaultColor_;
    RtDisplay::appendToFile("Inside computeText 2a");
    return defaultLabel_;

    // Else return the label corresponding to this value.

  } else {

    label = labelMap_[text];

    // If a colorMap entry was specified corresponding to this value,
    // set the color accordingly, else tell computeColor() to default
    // to MonitorCell::computeColor() by setting the currentColor to
    // EMPTY_CELL_COLOR

    if(colorMap_.find(label) == colorMap_.end()) {
      currentColor_ = EMPTY_CELL_COLOR;
    } else {
      currentColor_ = colorMap_[label];
    }

    RtDisplay::appendToFile("Inside computeText 2b");

    return label;
  }
}

void MonitorCellMapped::setBit(int iBit)
{
  if(iBit < 32) {
    iBit_ = iBit;
  }
}

MP_BITMASK_FORMAT_FN(MonitorCellMapped::formatByte)
{
  unsigned char cval = mp.getMonitorPointSample(iSamp).getMonitorValue().byte;
  unsigned int bitVal = (cval >> iBit) & 0x1;
  return bitVal;
}

MP_BITMASK_FORMAT_FN(MonitorCellMapped::formatShort)
{
  short sval = mp.getMonitorPointSample(iSamp).getMonitorValue().sh;
  unsigned int bitVal = (sval >> iBit) & 0x1;
  return bitVal;
}

MP_BITMASK_FORMAT_FN(MonitorCellMapped::formatBool)
{
  int ival = (int)mp.getMonitorPointSample(iSamp).getMonitorValue().bo;
  unsigned int bitVal = (ival >> iBit) & 0x1;
  return bitVal;
}

MP_BITMASK_FORMAT_FN(MonitorCellMapped::formatInt)
{
  int ival = mp.getMonitorPointSample(iSamp).getMonitorValue().lo;
  unsigned int bitVal = (ival >> iBit) & 0x1;
  return bitVal;
}

void MonitorCellMapped::setDefaultLabel(std::string defaultLabel)
{
  defaultLabel_ = defaultLabel;
}

void MonitorCellMapped::setDefaultColor(CellColor defaultColor)
{
  defaultColor_ = defaultColor;
}

/**.......................................................................
 * Return the color for this cell.  If we are mapping this color, only
 * use the mapped color if all conditions are currently met
 */
CellColor MonitorCellMapped::computeColor()
{
  if(currentColor_ != EMPTY_CELL_COLOR) {
    CellColor color = conditionsAreMet() ? currentColor_ : WHITE_CELL_COLOR;
    return color;
  } else {
    return MonitorCell::computeColor();
  }

}

void MonitorCellMapped::setColorMap(std::map<std::string, CellColor>& colorMap)
{
  colorMap_ = colorMap;
}
