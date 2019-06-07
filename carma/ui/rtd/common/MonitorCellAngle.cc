#include "carma/ui/rtd/common/AngleCell.h"
#include "carma/ui/rtd/common/MonitorCellAngle.h"

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

/**.......................................................................
 * Constructor.
 */
MonitorCellAngle::MonitorCellAngle( const int      cellWidth,
				    const bool     setMpWidth,
				    MonitorPoint & mp,
				    const int      sampleNo,
				    double multiplier,
				    double offset,
				    short int precision,
				    angleFormatType format) : 
  MonitorCellScaled(cellWidth, setMpWidth, mp, sampleNo, multiplier, offset, precision) 
{
  angleFormat_ = format;
}

/**.......................................................................
 * Constructor.
 */
MonitorCellAngle::MonitorCellAngle( const int cellWidth,
				    MonitorPoint& mp,
				    double multiplier,
				    double offset,
				    short int precision,
				    angleFormatType format) :
  MonitorCellScaled(cellWidth, true, mp, 0, multiplier, offset, precision) 
{
  angleFormat_ = format;
}

MonitorCellPtr MonitorCellAngle::makeCell(const int      cellWidth,
					MonitorPoint & mp,
					double multiplier,
					double offset,
					short int precision,
					angleFormatType format)
{
  if(fabs(offset) < eps_ && fabs(multiplier-1.0) < eps_) {
    return MonitorCell::makeCell(cellWidth, mp);
  }
  
  switch (mp.getValuetype()) {

    // The following types can't be scaled

  case MONITOR_VALUE_TYPE_BOOLEAN:
  case MONITOR_VALUE_TYPE_COMPLEX:
  case MONITOR_VALUE_TYPE_STRING:
  case MONITOR_VALUE_TYPE_SERIAL_NUMBER:
    return MonitorCell::makeCell(cellWidth, mp);
  default:
    {
#if 0
      std::ostringstream os;
      os << "EML formatting " << mp.getCanonicalName() << " with precision = " << precision << " format  = " << format << std::endl;
      programLogInfo(os.str());
#endif
      
      return MonitorCellPtr(new MonitorCellAngle(cellWidth, mp, multiplier, offset, precision, format));
    }
    break;
  }
}
  
/**.......................................................................
 * Destructor.
 */
MonitorCellAngle::~MonitorCellAngle() {}

std::string MonitorCellAngle::formatValue(double dval)
{
#if 0
  std::ostringstream os;
  os << "EML formatting with format  = " << angleFormat_;
  programLogInfo(os.str());
#endif

  return AngleCell::formatValue(dval, "radians", angleFormat_, setMpWidthToFormatLength(), MOD_NONE);
}

