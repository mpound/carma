/**
 * @file DataTransfer.h
 * Class definition for Data Transfer class
 * @author Lisa Xu
 *
 * $Revision:
 * $Date:
 * $Id:
 */

#ifndef CARMA_ARCHIVE_DATATRANSFER_H_
#define CARMA_ARCHIVE_DATATRANSFER_H_

#include "carma/archive/TransferDB.h"
#include "carma/monitor/DataflowSubsystem.h"
#include "carma/sdp/MiriadUV.h"
#include "carma/util/types.h"

// C includes
#include <sys/types.h>
#include <dirent.h>
#include <stdio.h>
#include <zlib.h>
#include <errno.h>

// Std C++ includes
#include <iostream>
#include <fstream>
#include <map>
#include <exception>
#include <vector>
#include <string>

// Carma tools includes
#include "log4cpp/Category.hh"
#include "bgftp/ArchiveJni.h"

namespace carma {
namespace archive {

class DataTransfer
{
	public:
	/**
	 * Constructor.
	 */
	DataTransfer(std::map<std::string, std::string> arv, 
                 const carma::util::frameType startFrame,
                 const carma::util::frameType pollInterval,
                 const bool emulate = false );

	/**
     * Desctructor
	 */
	~DataTransfer();

    /**
     * Update input parameters, input directories, and file filter (for
     * identifying if is: visbrick, astro-header, or sza data).
     *
     * Return -1 if there is no files to be transferred; otherwise 0.
     */
    int update(const std::vector<std::string> & p,
               const std::vector<std::string> & dirs,
               const std::string & filter);

    /**
     * Transfer mpdat files
     * &p: input parameters;
     * &dirs: input directories;
     * &filter: file filter (.mpdat).
     * Return -1 if there is no files to be transferred; otherwise 0.
     */
    int move_mp(const std::vector<std::string> & p,
                const std::vector<std::string> & dirs,
                const std::string              & filter);

    /**
     * Transfer sza miriad files
     * &p: input parameters;
     * &dirs: input directories;
     * &filter: file filter (sza_).
     * Return -1 if there is no files to be transferred; otherwise 0.
     */
    int move_sza(const std::vector<std::string>  & p,
                 const std::vector<std::string>  & dirs,
                 const std::string               & filter);

    /**
     * Transfer quality report data
     * &p: input parameters;
     * &dir: input directory (/opt/sdp/quality/transfer)
     * &filter: file filter (qr_).
     * Return -1 if no quality-reports to transfer; otherwise 0.
     */
    int move_qr(const std::vector<std::string> & p,
                const std::string              & dir,
                const std::string              & filter);

    /**
     * Start file transfer by calling the update function
     */
    void start(const std::vector<std::string> & p,
               const std::vector<std::string> & dirs,
               const std::string              & filter);

    /**
     * Stop tranfer by calling the stop function of the jni object
     */
    void stop();

    void updateMonitorPoints();


    /**
     * Scan and filter all the files in a given directory;
     * Return a vector of input file-list (to transfer).
     */
    std::vector<std::string> getFiles(const std::string & dir, const std::string & filter);
    std::vector<std::string> getSZAFiles(const std::string & dir);
    std::vector<std::string> getQualityReports(const std::string & dir);
    /**
     * Print and set input parameters
     */
    void setParams(const std::vector<std::string> & params, const std::string & filter);

    /**
     * Compress a source file and save to dest file in gzip format.
     */
    void gzip(const std::string & source, const std::string & dest);

    /**
     * Decompress a .gz file
     */
    //void gunzip(std::string& source, std::string& dest);

    /**
     * Append transferred file names to transfer.log
     */
    void writeTransferLog(const std::vector<std::string> & list);

    /**
     * Write the transferred list to the transfer database, either append to the
     * end of the file, or rewrite it (when some astroheaders got retransferred)
     */
    void writeTransferDB(const std::vector<std::string> & list, const bool append);

    /**
     * Update the internal transferred filelist vector
     */
    void appendDoneList(const std::vector<std::string>& list);

    /**
     * Get the date from the .mpdat file name (return a date string
     * in yyyy-mm-dd format)
     */
    std::string getDate(const std::string & filename);

    /**
     * Get the year string from the startframe of an astroheader file
     */
    std::string getYear(const std::string & filename);

     /**
      * get the subdir name (frame-count range) for a visbrick file
      */
    std::string getRange(const std::string & name);

    /**
     * the name of transfer log
     */
    std::string logFile;

    /**
     * this contains the files that have been transferred.
     */
    std::vector<std::string> doneList;

    /**
     * the object that does jni stuff between the C++ and Java code.
     */
    ArchiveJni *jniObj;

    /**
     * the date that would be used for directory name of .mpdat files
     */
    std::string theDate;

    /**
     * the year string that will be used as subdir name of astroheader files
     */
    std::string theYear;

    /**
     * will be used as subdir name for sza files at the archive
     */
    std::string szaYear;

    /**
     * the subdir (frame-count range) that the visbrick file
     * whose frame-count falls in the range will be put in
     */
    std::string theRange;

    /**
     * indicating if the value of theYear has been set or not
     */
    bool yearIsSet;

    /**
     * indicating if the value of szaYear has been set or not
     */
    bool szaYearIsSet;

    /**
     * indicating if the value of theRange has been set or not
     */
    bool rangeIsSet;

    /**
     * indicating if we need only to append to the transfer db or rewrite
     * all the records (this needs to be done when astroheaders got
     * re-transferred and the old records deleted from the db file)
     */
    bool toAppend;

    /**
     * the transfer database object
     */
    TransferDB* db;
    typedef enum xferFileTypeEnum {
        FILE_ASTROHEADER,
        FILE_MONITOR,
        FILE_QUALITY,
        FILE_SZA,
        FILE_VISBRICK,
        FILE_NONE
    } xferFileType;

    /**
     * Squirrel away the names of the last files
     * transferred for later update of the monitor system. 
     * The MPs updated depends on the name/type of files,
     * visbrick, astroheader, mpdat, quality
     */
    void setLastTransferred( const size_t length, const xferFileType type );

    /**
     * @return The rate of file transfer (files per unit time) over
     * the most input interval
     * @param periodInMinutes  The interval over which to compute the file transfer rate
     */
    size_t fileTransferRate( const size_t intervalInMinutes );

    /**
     * Time (framecount) this application was started. Used to compute
     * "uptime" monitor point.
     */
    carma::util::frameType startFrame_;

    /** how often DataTransfer wakes up to scan directories */
    carma::util::frameType pollInterval_;

    /** if true, enable emulation mode for testing */
    bool emulate_;

    /** Time (framecount) at which data were last transferred */
    carma::util::frameType lastXferTime_;

    /* total number of files of all types transferred */
    size_t numFilesXferred_;
    /* number of astroheader files transferred */
    size_t numAHXferred_;
    /* number of monitor data files transferred */
    size_t numMPXferred_;
    /* number of visbrick files transferred */
    size_t numVisbrickXferred_;
    /* number of quality files transferred */
    size_t numQualityXferred_;

    /** 
     * This is a running list of files transferred at a given timestamp,
     * from whch we can compute the file transfer rates over different
     * periods
     */
    ::std::map<carma::util::frameType, size_t> fileXferRateMap_;

    /**
     * Monitor subsystem 
     */
    carma::monitor::DataflowSubsystem dataflow_;

};

}} // namespace carma::archive

#endif /*CARMA_ARCHIVE_DATATRANSFER_H_*/
