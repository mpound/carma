
/**
 * @file
 * $Id: DataExtractor.cc,v 1.1 2005/01/18 02:51:11 cgwon Exp $
 * DataExtractor class
 *
 * @author Chul Gwon
 * @version $Revision: 1.1 $
 *
 */

#include "carma/ui/dbms/DataExtractor.h"
#include "carma/dbms/DBConfigurator.h"
#include "carma/dbms/ResultsCache.h"
#include "carma/util/Trace.h"
#include "carma/util/NotFoundException.h"

using namespace carma::ui::dbms;

DataExtractor::DataExtractor(const std::string &dbConfFile) {
  dbConnection_ = 0;

  carma::dbms::DBConfigurator *dbConfigurator = 0;

  try {
    dbConfigurator = new carma::dbms::DBConfigurator(dbConfFile);
  } catch (const carma::util::NotFoundException &ex) {
    CPTRACE(carma::util::Trace::TRACE4,
	    "DB conf file " << dbConfFile << " not found");
    throw ex;
  }

  try {
    dbConnection_ = 
      carma::dbms::DBConnectionFactory::createConnection(dbConfigurator);
    carma::dbms::ResultsCache rc = 
      carma::dbms::ResultsCache::getCache(dbConnection_);

  } catch (const carma::dbms::DBConnectionException &ex) {
    CPTRACE(carma::util::Trace::TRACE4,
	    ex.what());
    delete dbConfigurator;
    throw ex;
  }
  delete dbConfigurator;
}

DataExtractor::~DataExtractor() {
  carma::dbms::ResultsCache::closeCache();
  delete dbConnection_;
}


