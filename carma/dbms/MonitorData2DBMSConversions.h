#ifndef CARMA_DBMS_MONITORDATA2DBMSCONVERSIONS_H
#define CARMA_DBMS_MONITORDATA2DBMSCONVERSIONS_H

/**
 * @file
 * methods to convert between monitor point parameters and db values
 * and vice versa
 *
 * @author: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#include "carma/monitor/MonitorPoint.h"

#include <string>

namespace carma {
namespace dbms {


    /**
     * convert the MonitorPoint::VALIDITY value to the value which is 
     * written in the db
     */
    unsigned short validity2DB(const carma::monitor::MonitorPoint::VALIDITY& validity);

    /**
     * convert a db value to its MonitorPoint::VALIDITY counterpart
     */
    carma::monitor::MonitorPoint::VALIDITY db2Validity
        (const unsigned short& dbValidity);

    /**
     * convert monitor validity to string.
     */
    std::string 
    validityToString( carma::monitor::MonitorPoint::VALIDITY validity );


    /**
     * convert the MonitorPoint::BLANKING_FLAGGING value to the value which is 
     * written in the db
     */
    unsigned short blankingFlagging2DB
        (const carma::monitor::MonitorPoint::BLANKING_FLAGGING& blanking);
    
    /**
     * convert a db value to its MonitorPoint::BLANKING_FLAGGING counterpart
     */
    carma::monitor::MonitorPoint::BLANKING_FLAGGING 
        db2BlankingFlagging(const unsigned short& dbBlanking);

    /**
     * convert a monitor blanking flagging enumerator to string.
     */
    std::string 
    blankingFlaggingToString( 
        carma::monitor::MonitorPoint::BLANKING_FLAGGING bf );

}}

#endif
