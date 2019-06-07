#ifndef CARMA_DBMS_LOGINDEXER_H
#define CARMA_DBMS_LOGINDEXER_H
#include "carma/dbms/LogProcessor.h"
#include "carma/util/Time.h"
#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>
#include <vector>
#include <map>


/**
 * @file
 * $Id: LogIndexer.h,v 1.5 2007/11/21 12:01:59 colby Exp $
 * @author Marc Pound
 */

namespace carma {
  namespace dbms {

    /**
     *  This class is responsible for keeping the history
     *  of what syslog2Db has already processed and initializing
     *  LogProcessors so that they do not reprocessed previously
     *  processed log messages.  It
     *  reads and writes a file in /var/log/carma that contains
     *  file name, last frame count, last raw line processed
     *  for each log file in /var/log/carma.
     *  Thus if syslog2b dies and is restarted, old data are not
     *  reprocessed.
     *  The index format is
     *  <pre>
     *   filename =  number of lines processed so far
     *              : framecount of first line 
     *              : framcount of last line processed 
     *  </pre>
     */
    class LogIndexer
    {

    public:

	/**
	 * Constructor. 
	 * @param the index file to read from and write to.
	 */
	LogIndexer();

        /** The destructor.  */
        virtual ~LogIndexer();

    private:
    };
  }
}

#endif // CARMA_DBMS_LOGINDEXER_H
