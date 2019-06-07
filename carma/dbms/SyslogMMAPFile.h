#ifndef CARMA_DBMS_SYSLOGMMAPFILE_H
#define CARMA_DBMS_SYSLOGMMAPFILE_H

#include <string>

#include <iostream>

#include "carma/dbms/SyslogMessage.h"

#include "carma/util/SemaphoreOperator.h"

#include "log4cpp/Category.hh"

/**
 * @file
 * $Id: SyslogMMAPFile.h,v 1.4 2007/12/14 09:10:58 colby Exp $
 * @author Colby Gutierrez-Kraybill
 */

namespace carma
{
  namespace dbms
  {

    /**
     * This class is responsible for opening/closing
     * the syslog mmap file.  This file is how various
     * processes converge on the local ACC cache of syslog
     * messages that come through the syslog.fifo/ipq
     */
    class SyslogMMAPFile
    {

      public:

	/**
	 * Constructor. 
	 * @param mmapFileName the mmap file to read from and write to.
	 * @param writer is this instantiation a writer to the file default false.
	 */
	SyslogMMAPFile( const std::string& mmapFileName, bool writer = false );

	/** The destructor.  */
	~SyslogMMAPFile();

	int getSize() { return mmapSize_; };
	void *getPtr() { return mmapPtr_; };

	std::string computeVersionHash();
	long long computeStateID(const char *);
	long long getMapStateID();
	void setMapStateID( long long val ) {  *currentState_id_ = val; };

	bool isConsistent() { return ( myStateID_ == *currentState_id_ ); };

	static const size_t MMAP_SIZE = 50000300;
	static const size_t VERSIONID_SIZE = 100;
	static const size_t SYSLOGMEDIAN_SIZE = 165;
	static const long MAX_MESSAGES = 300000;
	static const char *MYID;

	std::string toString();
	std::string toVerboseString();
	std::string getMyID();
	std::string getVersionHash();
	size_t getHeaderSize();
	size_t getMapSize();
	size_t getVersionIDSize();
	size_t getSyslogMedianSize();

	void atomicUpdate( SyslogMessage *aSyslogMessage );
	long getCurrentPos();
	long getLastPos();
	long getPosLoc( long pos );

	long getNearestAgedPos( double mjd );
	double peekMJD( long pos );

	long getNumMessages();

	SyslogMessage *getCurrentMessage();
	SyslogMessage *getLastMessage();
	SyslogMessage *getMessageAt( long pos );
	SyslogMessage *getMessageAtAndUpdateNext( long &pos );

      private:
	std::string mmapFileName_;
	bool writer_;
	int mmapFD_;
	int mmapSize_;
	log4cpp::Category &log_;

	unsigned char *mmapPtr_;
	unsigned char *hashPtr_;
	unsigned char *IDPtr_;
	size_t *headerPtrStart_;
	size_t *headerPtrEnd_;
	size_t *headerCurrent_;
	size_t *header_size_;
	size_t *versionID_size_;
	size_t *syslogMedian_size_;
	size_t *lastBytePos_;
	long long *currentState_id_;
	long long myStateID_;
	int headerSize_;
	std::string versionHash_;
        long *currentAtomicPos_;	
        long *lastPos_;	
	unsigned char *ringBufferStart_;
	unsigned char *ringBufferEnd_;

	static carma::util::SemaphoreOperator *semOp_;

	// this is a little crazed...
	static ::std::string *SEM_NAME;
	static unsigned short num_;
    };
  }
}

#endif // CARMA_DBMS_SYSLOGMMAPFILE_H
