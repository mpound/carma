#ifndef CARMA_DBMS_SYSLOG2DBMSCONVERSIONS_H
#define CARMA_DBMS_SYSLOG2DBMSCONVERSIONS_H

/**
 * @file
 * @author: Marc Pound
 * $Id: Syslog2DBMSConversions.h,v 1.5 2014/04/02 23:11:03 iws Exp $
 * $CarmaCopyright$
 *
 */

#include "carma/dbms/Typedefs.h"
#include <string>

namespace carma {
namespace dbms {


      /**
       * Numeric values corresponding to syslog priority levels.
       * Values are staggered, so that we can insert values
       * in the future if necessary (not likely) and not
       * change the value of existing levels.  
       * (As I just did for DEBUG 4/12/2005!)
       * <b>It is imperative
       * that the constant values of existing levels do not
       * change with time or the database will lose integrity.
       * </b>
       */
      static const dbPriorityType UNKNOWN = 10;
      static const dbPriorityType NOTSET  = 20;
      static const dbPriorityType DEBUG   = 25;
      static const dbPriorityType INFO    = 30;
      static const dbPriorityType NOTICE  = 40;
      static const dbPriorityType WARN    = 50;
      static const dbPriorityType ERROR   = 60;
      static const dbPriorityType CRIT    = 70;
      static const dbPriorityType ALERT   = 80;
      static const dbPriorityType FATAL   = 90;
      /*
    typedef enum DB_PRIORITY {
      UNKNOWN_PRIORITY,
      NOTSET_PRIORITY,
      DEBUG__PRIORITY,
      INFO_PRIORITY,
      NOTICE_PRIORITY,
      WARN_PRIORITY,
      ERROR_PRIORITY,
      CRIT_PRIORITY,
      ALERT_PRIORITY,
      FATAL_PRIORITY 
    };
      */

    /**
     * convert the syslog priority string to the value which is 
     * written in the db
     */
    dbPriorityType priority2DB(const std::string& priority);

    /**
     * convert a db value to its syslog priority string
     */
    const std::string& db2Priority (const dbPriorityType dbPriority);

    const PriorityMap getDB2PriorityMap();


    /**
     * Corresponding to log4cpp string values, these
     * are allowed to change over the lifetime of CARMA
     */

    static const std::string UNKNOWN_STR ( "UNKNOWN" );
    static const std::string NOTSET_STR  ( "NOTSET" );
    static const std::string DEBUG_STR   ( "DEBUG" );
    static const std::string INFO_STR    ( "INFO"  );
    static const std::string NOTICE_STR  ( "NOTICE" );
    static const std::string WARN_STR    ( "WARN"  );
    static const std::string ERROR_STR   ( "ERROR" );
    static const std::string ALERT_STR   ( "ALERT" );
    static const std::string CRIT_STR    ( "CRIT"  );
    static const std::string FATAL_STR   ( "FATAL" );

}}

#endif
