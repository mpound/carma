#ifndef CARMA_UI_DBMS_MONITORDATAEXTRACTOR_H
#define CARMA_UI_DBMS_MONITORDATAEXTRACTOR_H

/**
 * @file
 * $Id: MonitorDataExtractor.h,v 1.9 2007/11/19 16:35:12 hravlin Exp $
 * DataExtractor class
 *
 * @author Chul Gwon
 * @version $Revision: 1.9 $
 *
 */

#include "carma/ui/dbms/DataExtractor.h"
#include "carma/dbms/filter/TimeFilter.h"
#include "carma/dbms/MonitorPointSelector.h"
#include "carma/dbms/Table.h"
#include <set>
#include <vector>
#include <map>


namespace carma {

namespace dbms {
  // forward declaration of classes
  class MonitorDataQueryManager;
  class TimeRangeFilter;
  class MonitorPointFilter;
  class MonitorConfigurationDatabase;
}

namespace ui {

  /**
   * User Interface for retrieving Monitor Point information from the DBMS
   */

namespace dbms {

  /**
   * Class for extracting monitor data from the DBMS
   */

  class MonitorDataExtractor : public carma::ui::dbms::DataExtractor {
  public:
    MonitorDataExtractor(const std::string &dbConfFile);
    virtual ~MonitorDataExtractor();

    /**
     * Adds a single monitor point to the search criteria for the DBMS
     * @param mpName canonical name for Monitor Point to add to search
     */
    void addMonitorPoint(const std::string &mpName);
    /**
     * Add multiple monitor points using wildcard characters
     * @param mpName canonical name of Monitor Point(s) using a
     * wildcards
     * @param multiWildcard wildcard character used to specify
     * multiple characters
     * @param singleWildcard wildcard character used to specify a
     * single character
     * @param selectionMask string containing selection values.
     */
    void addMonitorPoints(const std::string &mpName,
			  const std::string &multiWildcard,
			  const std::string &singleWildcard);

    void addMonitorPoints(const std::string &mpName,
			  const std::string &multiWildcard,
			  const std::string &singleWildcard,
			  const std::string &selectionMask);

    /**
     * Clears query that was set up for MonitorDataExtractor
     */
    void clearPreviousQuery();

    /**
     * set the time range for finding monitor points
     * @param beginTime begin time (in units of frame count) for search
     * @param endTime end time (in units of frame count) for search
     * @param averageType the type of average to consider
     * @param includeTime1 include time1 in the range (adds an "=" to the 
     *        test)?
     * @param includeTime1 include time2 in the range (adds an "=" to the 
     *        test)?
     * @param colType the time identifier column type
     * @throws TimeFilterException if 
     *         colType==carma::dbms::TimeFilter::INTEGRATIONID 
     *         and (averageType == FRAME_AVG or MINUTE_AVG)
     * @see carma::dbms::TimeRangeFilter
     */
    void setTimeRange(const int &beginTime, 
		      const int &endTime,
		      const carma::dbms::MonitorAverageType &averageType = 
		      carma::dbms::FRAME_AVG,
		      const bool &includeTime1 = false,
		      const bool &includeTime2 = false,
		      const carma::dbms::TimeFilter::TimeColumnType colType = 
		      carma::dbms::TimeFilter::FRAMECOUNT);

    /**
     * (note: not implemented yet) this member function should set up
     * filters used for monitor points (see
     * carma::dbms::MonitorDataQueryManager).  setFilters_ variable
     * will then be passed into the MDQM constructor.
     */
    void setFilter();

    /**
     * get monitor points from dbms, where each monitor point occupies
     * a single Table
     */
    std::map<int, carma::dbms::Table> getTablePerMp();

    /**
     * get monitor points from dbms, where all monitor points are put
     * into a single Table
     */
    carma::dbms::Table getTableOfAllMps();

    /**
     * Return the canonical name for a given tagId
     * @param tagId tag ID in string form for monitor point
     */
    std::string getCanonicalName(const std::string &tagId);

    /**
     * Return the canonical name for a given tagId
     * @param tagId tag ID in int form for monitor point
     */
    std::string getCanonicalName(const int tagId);

  private:
    void createMonitorDataQueryManager();

    carma::dbms::MonitorDataQueryManager *queryManager_;
    carma::dbms::MPSelectorSet mpSelectors_;
    carma::dbms::TimeRangeFilter *timeRange_;
    std::set<carma::dbms::MonitorPointFilter*> mpFilters_;
    carma::dbms::MonitorConfigurationDatabase *mpConfDb_;
  }; // end class MonitorDataExtractor
}; // end namespace dbms
}; // end namespace ui
}; // end namespace carma

#endif
