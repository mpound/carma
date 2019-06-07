// $Id: MonitorCellMapped.h,v 1.5 2013/11/19 03:41:12 iws Exp $

#ifndef CARMA_UI_RTD_MONITORCELLMAPPED_H
#define CARMA_UI_RTD_MONITORCELLMAPPED_H

/**
 * @file MonitorCellMapped.h
 * 
 * Tagged: Thu Aug 26 17:19:18 PDT 2010
 * 
 * @version: $Revision: 1.5 $, $Date: 2013/11/19 03:41:12 $
 * 
 * @author Erik Leitch
 */
#include "carma/ui/rtd/common/MonitorCell.h"
#include <boost/shared_ptr.hpp>

#define MP_BITMASK_FORMAT_FN(fn) unsigned (fn)(carma::monitor::MonitorPoint& mp, int iSamp, int iBit)

namespace carma {
  namespace ui {
    namespace rtd {

      class MonitorCellMapped;
      typedef boost::shared_ptr<MonitorCellMapped> MonitorCellMappedPtr;

      class MonitorCellMapped : public MonitorCell {
      public:

	/**
	 * Constructor.
	 */
	MonitorCellMapped( int                     cellWidth,
			   bool                    setMpWidth,
			   monitor::MonitorPoint&  monitorPoint,
			   int                     sampleNo,
			   std::map<std::string, std::string> labelMap);

	MonitorCellMapped( int                     cellWidth,
			   monitor::MonitorPoint&  monitorPoint,
			   std::map<std::string, std::string> labelMap);

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp,
				     std::map<std::string, std::string> labelMap);

	// Constructor for one bit in a bitmask

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp,
				     int iBit,
				     std::map<std::string, std::string> labelMap);

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp);

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp,
				     std::string state0);

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp,
				     std::string state0,
				     std::string state1);

	// Constructor for one bit in a bitmask

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp,
				     int iBit,
				     std::string state0,
				     std::string state1);

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp,
				     std::string state0,
				     std::string state1,
				     std::string state2);

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp,
				     std::string state0,
				     std::string state1,
				     std::string state2,
				     std::string state3);

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp,
				     std::string state0,
				     std::string state1,
				     std::string state2,
				     std::string state3,
				     std::string state4);

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp,
				     std::string state0,
				     std::string state1,
				     std::string state2,
				     std::string state3,
				     std::string state4,
				     std::string state5);

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp,
				     std::string state0,
				     std::string state1,
				     std::string state2,
				     std::string state3,
				     std::string state4,
				     std::string state5,
				     std::string state6);

	static MonitorCellMappedPtr makeCell(const int     cellWidth,
				     monitor::MonitorPoint& mp,
				     std::string state0,
				     std::string state1,
				     std::string state2,
				     std::string state3,
				     std::string state4,
				     std::string state5,
				     std::string state6,
				     std::string state7);

	/**
	 * Destructor.
	 */
	virtual ~MonitorCellMapped();

	virtual ::std::string computeText();

	void setBit(int iBit);

	void setDefaultLabel(std::string defaultLabel);
	void setDefaultColor(CellColor);
	void setColorMap(std::map<std::string, CellColor>& colorMap);

	CellColor computeColor();

      private:

	MP_BITMASK_FORMAT_FN(*formatFn_);

	int iBit_;

	std::map<std::string, std::string> labelMap_;
	std::map<std::string, CellColor>   colorMap_;

	std::string defaultLabel_;
	CellColor defaultColor_;
	CellColor currentColor_;
	
	void privateConstructor(monitor::MonitorPoint& mp, 
				std::map<std::string, std::string> labelMap);
	
	static MP_BITMASK_FORMAT_FN(formatByte);
	static MP_BITMASK_FORMAT_FN(formatShort);
	static MP_BITMASK_FORMAT_FN(formatBool);
	static MP_BITMASK_FORMAT_FN(formatInt);

      }; // End class MonitorCellMapped

    } // End namespace rtd
  } // End namespace ui
} // End namespace carma



#endif // End #ifndef CARMA_MONITOR_UI_RTD_MONITORCELLMAPPED_H
