/**
 * @file TransferDB.h
 * Class definition for TransferDB class
 * @author Lisa Xu
 * 
 * $Revision: 1.3 $
 * $Date: 2009/03/24 00:17:34 $
 * $Id: TransferDB.h,v 1.3 2009/03/24 00:17:34 mpound Exp $
 */
 
#ifndef CARMA_ARCHIVE_TRANSFERDB_H_
#define CARMA_ARCHIVE_TRANSFERDB_H_

// C includes
#include <sys/types.h>
//#include <dirent.h>
#include <stdio.h>
//#include <zlib.h>
#include <errno.h>

// Std C++ includes
#include <iostream>
#include <fstream>
#include <exception>
#include <vector>
#include <string>

// Carma include
#include <carma/services/Table.h>

// Carma tools includes
#include "log4cpp/Category.hh"

namespace carma {
namespace archive {

class TransferDB
{
	public:

    typedef enum {
        FILE_NAME   = 0, 
        FILE_SIZE   = 1,
        CHECKSUM    = 2,
        TRANSF_DATE = 3,
        DELETE_DATE = 4,
        TIMESTAMP   = 5
    } ColumnNumber;

	/** 
	 * Constructor
	 */
    TransferDB();

	TransferDB(std::string mutex, std::string dbFilename);

	/** 
     * Desctructor
	 */
	~TransferDB();

    /**
     * Return a table reading from the db file
     */
    carma::services::Table getTable();

    /**
     * get filenames (the keys) from database table
     */
    std::vector<std::string> getKeys();

    /**
     * get delete dates from database table
     */
    std::vector<std::string> getDeleteDates();

    /**
     * get timestamps from database table
     */
    std::vector<std::string> getTimestamps();

    /**
     * get checksum of the file
     */
    std::string getChecksum();

    /**
     * update a cell at (row, col) with the value
     */
    void update(int row, int col, std::string value);

    /**
     * add a row to the end
     */
    void add(std::string aRow);

    /**
     * remove a row
     */
    void removeRow(int row);

    /**
     * Update the transfer database file with the table
     */
    void updateDBFile(std::vector<std::string>& dbRecords, bool append);

    /**
     * Check to see if any lock that did not get cleared due to a system crash
     * Clear it if the lock is older than 5 min.
     */
    void clearOldLock();

    private:

    /**
     * lock the file before writing to it.
     */
    int getLock();

    /**
     * release lock after done.
     */
    void releaseLock();

    /**
     * parse the line using the given delimiter
     */
    std::vector<std::string> tokenize(std::string& line);

    /**
     * the lock file (mutex)
     */
    std::string lock;

    /**
     * The database table
     */
    carma::services::Table dbTable;

    /**
     * The transfer database file
     */
    std::string dbFile;
};

}} // namespace carma::archive

#endif /*CARMA_ARCHIVE_TRANSFERDB_H_*/
