// $Id: SzaRtdUtils.h,v 1.8 2013/11/19 03:41:14 iws Exp $

#ifndef CARMA_UI_RTD_SZARTDUTILS_H
#define CARMA_UI_RTD_SZARTDUTILS_H

/**
 * @file SzaRtdUtils.h
 * 
 * Tagged: Fri Aug 27 14:34:06 PDT 2010
 * 
 * @version: $Revision: 1.8 $, $Date: 2013/11/19 03:41:14 $
 * 
 * @author Erik Leitch
 */
#include "carma/ui/rtd/common/MonitorDisplay.h"
#include "carma/ui/rtd/common/MonitorCell.h"
#include "carma/monitor/MonitorSystem.h"
#include <boost/shared_ptr.hpp>
#include <iostream>

#define SZA_COL_ADD_FN(fn)       std::vector<MonitorCellPtr> (fn)(carma::monitor::MonitorContainer* container, unsigned colWidth)
#define SZA_ROW_ADD_FN(fn)       std::vector<MonitorCellPtr> (fn)(carma::monitor::MonitorContainer* container, unsigned colWidth)
#define SZA_LABEL_FN(fn)         std::vector<std::string> (fn)()
#define SZA_CONTAINER_FN(fn)     carma::monitor::MonitorContainer* (fn)(carma::monitor::SzaSubsystem& subsystem)

#define GENERIC_COLUMN_FN(fn)        std::vector<SzaRtdUtils::RtdColumn*> (fn)(MonitorDisplay& display)
#define GENERIC_ROW_FN(fn)           std::vector<SzaRtdUtils::RtdRow*>    (fn)(MonitorDisplay& display)

#define CAN_RECEIVED_MP(container) \
  SzaRtdUtils::cell_ = MonitorCellMapped::makeCell(colWidth, container->received(), "OFFLINE");\
  SzaRtdUtils::colorMap_.clear();\
  SzaRtdUtils::colorMap_["OFFLINE"] = RED_CELL_COLOR;\
  SzaRtdUtils::cell_->setColorMap(SzaRtdUtils::colorMap_);\
  SzaRtdUtils::cell_->setDefaultLabel("ONLINE");\
  SzaRtdUtils::cell_->setDefaultColor(GREEN_CELL_COLOR);\
  col.push_back(SzaRtdUtils::cell_);

#define BINARY_MAPPED_MP(mp, state1Label, state2Label, state1Color, state2Color) \
  SzaRtdUtils::cell_ = MonitorCellMapped::makeCell(colWidth, mp, state1Label, state2Label);\
  SzaRtdUtils::colorMap_.clear();\
  SzaRtdUtils::colorMap_[state1Label] = state1Color;\
  SzaRtdUtils::colorMap_[state2Label] = state2Color;\
  SzaRtdUtils::cell_->setColorMap(SzaRtdUtils::colorMap_);\
  col.push_back(SzaRtdUtils::cell_);

#define BOOLEAN_MAPPED_MP(mp, state1Label, state2Label, state1Color, state2Color) \
  {\
   std::map<std::string, std::string> labelMap; \
   labelMap["false"] = state1Label;\
   labelMap["true"]  = state2Label;\
   SzaRtdUtils::cell_ = MonitorCellMapped::makeCell(colWidth, mp, labelMap);\
   SzaRtdUtils::colorMap_.clear();\
   SzaRtdUtils::colorMap_[state1Label] = state1Color;\
   SzaRtdUtils::colorMap_[state2Label] = state2Color;\
   SzaRtdUtils::cell_->setColorMap(SzaRtdUtils::colorMap_);\
   col.push_back(SzaRtdUtils::cell_);\
 }

#define BINARY_BIT_MAPPED_MP(mp, bit, state1Label, state2Label, state1Color, state2Color) \
  SzaRtdUtils::cell_ = MonitorCellMapped::makeCell(colWidth, mp, bit, state1Label, state2Label); \
  SzaRtdUtils::colorMap_.clear();\
  SzaRtdUtils::colorMap_[state1Label] = state1Color;\
  SzaRtdUtils::colorMap_[state2Label] = state2Color;\
  SzaRtdUtils::cell_->setColorMap(SzaRtdUtils::colorMap_);\
  col.push_back(SzaRtdUtils::cell_);

namespace carma {
  namespace ui {
    namespace rtd {

      class MonitorCellMapped;
      typedef boost::shared_ptr<MonitorCellMapped> MonitorCellMappedPtr;

      class SzaRtdUtils {
      public:

	struct RtdColumn {

	  RtdColumn(std::string name, carma::monitor::MonitorContainer& container) {
	    name_      = name;
	    container_ = &container;
	  }

	  std::string name_;
	  carma::monitor::MonitorContainer* container_;
	};

	struct RtdRow {

	  RtdRow(std::string name, carma::monitor::MonitorContainer& container) {
	    name_      = name;
	    container_ = &container;
	  }

	  std::string name_;
	  carma::monitor::MonitorContainer* container_;
	};

	/**
	 * Constructor.
	 */
	SzaRtdUtils();

	/**
	 * Destructor.
	 */
	virtual ~SzaRtdUtils();

	// Generic function to add a column to a table

	static void addColumn(carma::monitor::SzaSubsystem& subsystem, RtTablePtr table, 
			      SZA_CONTAINER_FN(*conFn), SZA_COL_ADD_FN(*fn), unsigned colWidth);
	
	static void addColumn(carma::monitor::MonitorContainer* container, RtTablePtr table, 
			      SZA_COL_ADD_FN(*fn), unsigned colWidth);

	// Generic function to add a table to a folder

	static void addTable(MonitorDisplay& display, RtFolderPtr folder, std::string tableName,
			     SZA_CONTAINER_FN(*conFn), SZA_COL_ADD_FN(*fn), SZA_LABEL_FN(*labFn), unsigned colWidth);
	
	static void addTable(MonitorDisplay& display, RtFolderPtr folder, std::string tableName,
			     GENERIC_COLUMN_FN(*columnFn), SZA_COL_ADD_FN(*columnAddFn), SZA_LABEL_FN(*rowLabelFn), unsigned colWidth);

	static void addTable(MonitorDisplay& display, RtFolderPtr folder, std::string tableName,
			     GENERIC_ROW_FN(*rowFn), SZA_ROW_ADD_FN(*rowAddFn), SZA_LABEL_FN(*colLabelFn), 
			     std::vector<unsigned>& colWidths);

	static void addTable(MonitorDisplay& display, RtFolderPtr folder, std::string tableName,
			     GENERIC_ROW_FN(*rowFn), SZA_ROW_ADD_FN(*rowAddFn), SZA_LABEL_FN(*colLabelFn), 
			     unsigned colWidth);

	// Generic function to add a folder to an RTD page

	static void addFolder(MonitorDisplay& display, std::string folderName, 
			      SZA_CONTAINER_FN(*conFn), SZA_COL_ADD_FN(*colFn), SZA_LABEL_FN(*labFn), unsigned colWidth);

	static void addFolder(MonitorDisplay& display, std::string folderName, 
			      GENERIC_COLUMN_FN(*columnFn), SZA_COL_ADD_FN(*columnAddFn), SZA_LABEL_FN(*rowLabelFn), unsigned colWidth);

	static void addFolder(MonitorDisplay& display, std::string folderName, 
			      GENERIC_ROW_FN(*rowFn), SZA_ROW_ADD_FN(*rowAddFn), SZA_LABEL_FN(*columnLabelFn), 
			      std::vector<unsigned>& colWidths);

	static void addFolder(MonitorDisplay& display, std::string folderName, 
			      GENERIC_ROW_FN(*rowFn), SZA_ROW_ADD_FN(*rowAddFn), SZA_LABEL_FN(*columnLabelFn), 
			      unsigned colWidth);

	// A wrapper around addFolder to add the XAC folder that's
	// common to every CAN module

	static void addXacFolder(MonitorDisplay& display, std::string moduleName,
				 SZA_CONTAINER_FN(*conFn));

	static void addCanModule(MonitorDisplay& display, std::string moduleName,
				 SZA_CONTAINER_FN(*conFn), SZA_COL_ADD_FN(*colFn), SZA_LABEL_FN(*labFn), unsigned colWidth);

	static SZA_LABEL_FN(getBiasLabels);
	static SZA_COL_ADD_FN(getBiasColumn);
	static SZA_CONTAINER_FN(getBiasContainer);

	static SZA_COL_ADD_FN(getIntmodColumn);
	static SZA_LABEL_FN(getIntmodLabels);
	static SZA_CONTAINER_FN(getIntmodContainer);
   
	static SZA_LABEL_FN(getVaractorLabels);
	static SZA_COL_ADD_FN(getVaractorColumn);
	static SZA_CONTAINER_FN(getVaractorContainer);

	static SZA_LABEL_FN(getYigLabels);
	static SZA_COL_ADD_FN(getYigColumn);
	static SZA_CONTAINER_FN(getYigContainer);

	static MonitorCellMappedPtr cell_;
	static std::map<std::string, CellColor> colorMap_;

      private:

	static const unsigned nSza_;
	static const unsigned szaOffsetInCarma_;

	static SZA_LABEL_FN(getXacLabels);
	static SZA_COL_ADD_FN(getXacColumn);

      }; // End class SzaRtdUtils

    } // End namespace rtd
  } // End namespace ui
} // End namespace carma



#endif // End #ifndef CARMA_UI_RTD_SZARTDUTILS_H
