// $Id: MonitorCellAngle.h,v 1.2 2013/11/19 03:41:12 iws Exp $

#ifndef CARMA_UI_RTD_MONITORCELLANGLE_H
#define CARMA_UI_RTD_MONITORCELLANGLE_H

/**
 * @file MonitorCellAngle.h
 * 
 * Tagged: Thu Aug 26 17:19:18 PDT 2010
 * 
 * @version: $Revision: 1.2 $, $Date: 2013/11/19 03:41:12 $
 * 
 * @author Erik Leitch
 */
#include "carma/ui/rtd/common/AngleCell.h"
#include "carma/ui/rtd/common/MonitorCellScaled.h"

namespace carma {
  namespace ui {
    namespace rtd {

      class MonitorCellAngle : public MonitorCellScaled {
      public:

	/**
	 * Constructor.
	 */
	MonitorCellAngle( int                     cellWidth,
			  bool                    setMpWidth,
			  monitor::MonitorPoint&  monitorPoint,
			  int                     sampleNo,
			  double                  multiplier=1.0,
			  double                  offset=0.0,
			  short int               precision=2,
			  angleFormatType         format=FORMAT_DMS);

	MonitorCellAngle( int                     cellWidth,
			  monitor::MonitorPoint&  monitorPoint,
			  double                  multiplier=1.0,
			  double                  offset=0.0,
			  short int               precision=2,
			  angleFormatType         format=FORMAT_DMS);

	static MonitorCellPtr makeCell(const int              cellWidth,
				     monitor::MonitorPoint& mp,
				     double                 multiplier=1.0,
				     double                 offset=0.0,
				     short int              precision=2,
				     angleFormatType        format=FORMAT_DMS);
	
	/**
	 * Destructor.
	 */
	virtual ~MonitorCellAngle();

	std::string formatValue(double dval);

      private:

	angleFormatType angleFormat_;

      }; // End class MonitorCellAngle

    } // End namespace rtd
  } // End namespace ui
} // End namespace carma



#endif // End #ifndef CARMA_MONITOR_UI_RTD_MONITORCELLANGLE_H
