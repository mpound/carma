#ifndef CARMA_UI_DBMS_DATAEXTRACTOR_H
#define CARMA_UI_DBMS_DATAEXTRACTOR_H

/**
 * @file
 * $Id: DataExtractor.h,v 1.2 2005/02/17 15:59:09 cgwon Exp $
 * DataExtractor class
 *
 * @author Chul Gwon
 * @version $Revision: 1.2 $
 *
 */

#include "carma/dbms/DBConnection.h"

namespace carma {
namespace ui {

  /**
   * User Interface for retrieving Monitor Point information from the DBMS
   */

namespace dbms {
  /**
   * base class for creating UI objects that can access the DBMS
   */
  class DataExtractor {
  public:
    DataExtractor(const std::string &dbConfFile);
    virtual ~DataExtractor();

    
  protected:
    carma::dbms::DBConnection *dbConnection_;

  }; // end class DataExtractor
}; // end namespace dbms
}; // end namespace ui
}; // end namespace carma

#endif
