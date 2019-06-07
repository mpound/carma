/**
 * @file
 * $Id: MonitorDataExtractor.cc,v 1.10 2011/08/28 18:42:37 scott Exp $
 * DataExtractor class
 *
 * @author Chul Gwon
 * @version $Revision: 1.10 $
 *
 */

#include "carma/ui/dbms/MonitorDataExtractor.h"
#include "carma/dbms/MonitorConfigurationDatabase.h"
#include "carma/dbms/MonitorDataQueryManager.h"
#include "carma/dbms/MonitorPointSelector.h"

#include "carma/dbms/filter/TimeRangeFilter.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"
#include "carma/util/StringUtils.h"
#include "carma/util/Trace.h"
#include <sstream>

using namespace carma::ui::dbms;
using namespace carma::util;
using namespace std;

namespace {

  std::vector<std::string> 
  parseMonitorPointQueryString(const std::string &queryString) {
    // ripped and slightly modified from CorbaUtils.cc getHierarchyIds
    std::vector<std::string> mps;
    std::string::size_type nextMpBeginPos = 0;
    std::string::size_type nextMpEndPos = 0;

    while (nextMpEndPos != std::string::npos) {
      nextMpEndPos = queryString.find(',', nextMpBeginPos);

      const std::string::size_type nextMpLength =
	nextMpEndPos - nextMpBeginPos;
    
      mps.push_back(carma::util::StringUtils::trimWhiteSpace(queryString.substr(nextMpBeginPos, 
										nextMpLength)));
      nextMpBeginPos = nextMpEndPos + 1;
    }
    
    return mps;
  }
}

MonitorDataExtractor::MonitorDataExtractor(const std::string &dbConfFile) :
  DataExtractor(dbConfFile)
{
  mpConfDb_ = new carma::dbms::MonitorConfigurationDatabase(dbConnection_);
  queryManager_ = 0;
  timeRange_ = 0;
}

MonitorDataExtractor::~MonitorDataExtractor() {
  clearPreviousQuery();
}

void MonitorDataExtractor::addMonitorPoint(const std::string &mpName) {
  // get Id from canonical name

  int tagId = mpConfDb_->getTagID(mpName);

  if (tagId < 0) {
    CPTRACE(carma::util::Trace::TRACE5,
	    mpName << " does not exist");
    throw CARMA_ERROR("Invalid monitor point name");
  } 
  
  CPTRACE(carma::util::Trace::TRACE6, "tagId: " << tagId);

  mpSelectors_.insert(new carma::dbms::MonitorPointSelector(tagId, dbConnection_));
}

void
MonitorDataExtractor::addMonitorPoints(const std::string &mpName,
				       const std::string &multiWildcard,
				       const std::string &singleWildcard) {
  addMonitorPoints(mpName, multiWildcard, singleWildcard, "");
}

void
MonitorDataExtractor::addMonitorPoints(const std::string &mpName,
				       const std::string &multiWildcard,
				       const std::string &singleWildcard,
				       const std::string &selectionMask) {

  std::vector<std::string> mps = parseMonitorPointQueryString(mpName);

  if (mps.size() < 1) {
    CARMA_CPTRACE(carma::util::Trace::TRACE4,
		  "MonitorDataExtractor::addMonitorPoints - MP search string empty");
    throw CARMA_ERROR("MonitorDataExtractor::addMonitorPoints - no MPs specified");
  }

  for (std::vector<std::string>::iterator i = mps.begin();
       i != mps.end();
       i++) {
    std::map<int, std::string> tagIds =
      mpConfDb_->getTagIDs(*i, multiWildcard, singleWildcard);
    
    if (tagIds.size() < 1) {
      CARMA_CPTRACE(carma::util::Trace::TRACE4,
		    "MonitorDataExtractor::addMonitorPoints: "
		    << "No monitor points match the pattern " 
		    << *i);
      throw CARMA_ERROR("Invalid monitor point search string");
    }
    
    for (std::map<int, std::string>::iterator i = tagIds.begin(); i != tagIds.end(); i++) {
      CARMA_CPTRACE(carma::util::Trace::TRACE6,
		    "mpName: " << i->second << "; tagId: " << i->first);
      carma::dbms::MonitorPointSelector *selector =
	new carma::dbms::MonitorPointSelector(i->first, dbConnection_);
      if(selectionMask != "")
	selector->setSelectionMask(selectionMask);
      mpSelectors_.insert(selector);
      //      mpSelectors_.insert(new carma::dbms::MonitorPointSelector(i->first, dbConnection_));
    }
  }
}

void
MonitorDataExtractor::clearPreviousQuery() {
  if( mpSelectors_.empty() == false ) {
    //cout << "Erasing selectors" << endl;
    mpSelectors_.erase(mpSelectors_.begin(), mpSelectors_.end());
    //cout << "Erasure complete" << endl;
  }
  if( mpFilters_.empty() == false ) {
    //cout << "Erasing filters" << endl;
    mpFilters_.erase(mpFilters_.begin(), mpFilters_.end());
    //cout << "Erasure complete" << endl;
  }

  if (timeRange_ != 0) {
    delete timeRange_;
    timeRange_ = 0;
  }

  if (queryManager_ != 0) {
    //programLogInfo("Deleting query manager");
    delete queryManager_;
    queryManager_ = 0;
    //programLogInfo("Deletion complete");
  }
}

void 
MonitorDataExtractor::setTimeRange(const int &beginTime, 
				   const int &endTime,
				   const carma::dbms::MonitorAverageType &averageType,
				   const bool &includeTime1,
				   const bool &includeTime2,
				   const carma::dbms::TimeFilter::TimeColumnType colType) {

  int absoluteStartTime = queryManager_->getBeginningOfProduction();
  if (beginTime < absoluteStartTime) {
    std::ostringstream ex;
    ex << "Begin time of " << beginTime 
       << " is earlier than the start of production (" << absoluteStartTime << ")";
    throw CARMA_ERROR(ex);
  } else {
    timeRange_ = new carma::dbms::TimeRangeFilter(beginTime,
						  endTime,
						  averageType,
						  includeTime1,
						  includeTime2,
						  colType);
  }
}

void
MonitorDataExtractor::createMonitorDataQueryManager() {

  if (queryManager_ == 0) {
    try {
      queryManager_ = new carma::dbms::MonitorDataQueryManager(mpSelectors_,
							       timeRange_,
							       dbConnection_);
    } catch (const carma::util::ErrorException &ex) {
      CARMA_CPTRACE(carma::util::Trace::TRACE3,
		    "Problems creating MonitorDataQueryManager:" << ex);
      throw ex;
    }
  }
}

std::map<int,carma::dbms::Table>
MonitorDataExtractor::getTablePerMp() {

  createMonitorDataQueryManager();

  std::map<int, carma::dbms::Table> returnTable;
  /*
  std::vector<std::string> setupQueries = queryManager_->getSetUpQueries();
  for (std::vector<std::string>::iterator i = setupQueries.begin();
       i != setupQueries.end();
       i++) {
    CARMA_CPTRACE(carma::util::Trace::TRACE6,
		  "SETUP QUERY: " << *i);
  }
  */
  try {
    returnTable = queryManager_->execNarrowQueries();
    CARMA_CPTRACE(carma::util::Trace::TRACE6, "table size: " << returnTable.size());
  } catch (const carma::util::ErrorException &ex) {
    CARMA_CPTRACE(carma::util::Trace::TRACE2, ex 
		  << "\n This may be caused because you deleted a Table by hand in sql."
		  << "  Must clean out MonitorDataTableIndex in sql as well");
    throw ex;
  }
  return returnTable;
}

carma::dbms::Table
MonitorDataExtractor::getTableOfAllMps() {

  createMonitorDataQueryManager();

  carma::dbms::Table returnTable;
  try {
    returnTable = queryManager_->execWideQuery();
  } catch (const carma::util::ErrorException &ex) {
    CARMA_CPTRACE(carma::util::Trace::TRACE2, ex);
    throw ex;
  }
  
  return returnTable;
}

std::string 
MonitorDataExtractor::getCanonicalName(const std::string &tagId) {
  std::string name;
  try {
    name = mpConfDb_->getCanonicalName(tagId);
  } catch (const carma::util::ErrorException &ex) {
    CARMA_CPTRACE(carma::util::Trace::TRACE2, ex);
    throw ex;
  }

  return name;
}
 
std::string 
MonitorDataExtractor::getCanonicalName(const int tagId) {
  std::string name;
  try {
    name = mpConfDb_->getCanonicalName(tagId);
  } catch (const carma::util::ErrorException &ex) {
    CARMA_CPTRACE(carma::util::Trace::TRACE2, ex);
    throw ex;
  }

  return name;
}
