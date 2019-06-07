#ifndef CARMA_DBMS_LOGPROCESSOR_H
#define CARMA_DBMS_LOGPROCESSOR_H
#include "carma/dbms/LogRecordSelector.h"
#include "carma/dbms/Syslog2DBMSConversions.h"
#include "carma/dbms/SyslogMMAPFile.h"
#include "carma/dbms/SyslogMessage.h"
#include "carma/util/Logger.h"
#include "carma/util/Time.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <sys/types.h>
#include <unistd.h>
/**
 * @file
 * $Id: LogProcessor.h,v 1.13 2011/12/21 22:56:43 mpound Exp $
 * @author Marc Pound
 */

namespace carma
{
  namespace dbms
  {

    /**
     */
    class LogProcessor
    {

      public:

	/**
	 * Constructor. Takes the
	 * @param fileName The log file that this object is
	 * keeping tabs on.
	 */
	LogProcessor();

	/** The destructor for Logger.  */
	virtual ~LogProcessor();

	static SyslogMessage *fromString( std::string *aMessage );
	static SyslogMessage *fromMMAP( SyslogMMAPFile &smapf, unsigned char *ptr );

	static void decomposeSyslogMessage( std::string *aMessage,
	    SyslogMessage *bMessage );
	static void getHostBounds(std::string *fullMessage, int &start, int &end );



      private:
	/**
	 * The structure of the rdbms table columns
	 */
	enum LOGTABLECOLUMNS { 
	  CALLER_COL,       // the caller of the log message as well as info 
	  // from syslog. 
	  FRAME_COL,        // input - current year in local timezone
	  // ouput - the frame count 
	  PRIORITY_COL,     // message priority
	  LOGNAME_COL,      // logname (i.e. program name or object)
	  NDC_COL,          // next diagnostic context
	  MESSAGE_COL,      // the log message
	  NUM_LOGTABLE_COLS // number of columns in the table always last
	} ;
    };
  }
}

#endif // CARMA_DBMS_LOGPROCESSOR_H
