/**
 * @file TransferDB.cc
 * This class performs transfer database (table) functionalities 
 * @author Lisa Xu
 * $Revision: 1.3 $
 * $Date: 2012/01/12 23:27:12 $
 * $Id: TransferDB.cc,v 1.3 2012/01/12 23:27:12 iws Exp $
 */
 
#include <fcntl.h>  // for open()
#include <cerrno>   // for errno 
#include <cstdio>   // for perror()
#include <stdio.h>  // for remove()

// Carma includes
#include "carma/archive/TransferDB.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/FileUtils.h"
#include "carma/util/ErrorException.h"
#include "carma/services/Table.h"

#define DELIMITER  " "

// namespace directives
using carma::util::Program;
using carma::util::FileUtils;
//using carma::util::Time;

// Namespace using directives
using namespace std;
using namespace carma::util;

namespace carma {
  namespace archive {
    TransferDB::TransferDB(std::string lockfile, std::string dbFilename)
    {
        lock = lockfile;        //lockfile name (full path)
        dbFile = dbFilename;    //the db file name (full path)

        dbTable.open(dbFile);
    }

    TransferDB::~TransferDB() {
    }

    carma::services::Table TransferDB::getTable()
    {
        dbTable.open(dbFile);
        return dbTable;
    }

    /**
     * get the filenames (keys) from the table
     */
    vector<string> TransferDB::getKeys()
    {
        return dbTable.getColumn(TransferDB::FILE_NAME);
    }

    vector<string> TransferDB::getDeleteDates()
    {
        return dbTable.getColumn(TransferDB::DELETE_DATE);
    }

    vector<string> TransferDB::getTimestamps()
    {
        return dbTable.getColumn(TransferDB::TIMESTAMP);
    }

    void TransferDB::update(int row, int col, string value)
    {
        string line = dbTable.getRow(row);
        vector<string> v = tokenize(line);
        v[col] = value;
        line = v[0];
        for(int i=1; i<(int)v.size(); i++) {
            line += DELIMITER + v[i];
        }

        dbTable.putRow(row, line);
    }
    
    void TransferDB::add(string aRow)
    {
        dbTable.addRow(aRow);
    }

    void TransferDB::removeRow(int row)
    {
        dbTable.removeRow(row);
    }

    void TransferDB::updateDBFile(vector<string>& list, bool append)
    {
        log4cpp::Category& logger = Program::getLogger();

        //check if db file is locked or not
        while(getLock() == -1) {
            cout << "Sleeping..." << endl;
            sleep(5);  //sleep 5 sec
        }

        //append to the db table
        int size = static_cast<int>(list.size());
        for(int i=0; i<size; i++) dbTable.addRow(list[i]);

        ofstream out;
        if(append) {
            out.open(dbFile.c_str(), ios::app);
            for(int i=0; i<size; i++) out << list[i] << endl;
        }
        else {
            //re-name db file to dbfile.old;
            string old = dbFile + ".old";
            int ret = rename(dbFile.c_str(), old.c_str());
            ostringstream msg("");
            if(ret != 0) {
                msg << "Error rename file: " << dbFile << " to " << old << endl;
                logger << log4cpp::Priority::WARN << msg.str();
                CARMA_CPTRACE(Trace::TRACE4, msg.str());
                throw CARMA_ERROR(msg.str());
            }

            //now write to the db file
            size = dbTable.getNrows();
            out.open(dbFile.c_str());
            if(!out) {
                msg.str("");
                msg << "Error open file: " << dbFile << endl;
                logger << log4cpp::Priority::WARN << msg.str();
                CARMA_CPTRACE(Trace::TRACE4, msg.str());
                throw CARMA_ERROR(msg.str());
            }

            for(int i=0; i<size; i++) {
                string s = dbTable.getRow(i);
                //cout << s << endl;
                out << s << endl;
            }
        }

        out.close();
        releaseLock();
    }

    void TransferDB::clearOldLock()
    {
        if(FileUtils::exists(lock)) {
            //get the timestamp of the lock file
            time_t mtime = FileUtils::modificationTime(lock);

            time_t now = time(NULL);  // in seconds
            int diff = now - mtime;

            /*
            ostringstream s;
            s << mtime;
            cout << "Timestamp of lock: " << s.str() << endl;
            s.str(""); s << now;
            cout << "Now: " << s.str() << endl;
            */

            //release (remove) the lock if it's older than 5 min
            if(diff > 300) {
                releaseLock();
            }
        }
    }

    int TransferDB::getLock()
    {
        clearOldLock();

        int fd = ::open(lock.c_str(), O_WRONLY | O_CREAT | O_EXCL, 0666);

        if (fd<0 && errno==EEXIST) {
            // the lock file already exist; another process is 
            // holding the lock
            cout<< "Transfer DB is currently locked." << endl;
            return -1;
        }
        else if (fd < 0) {
            // perror() appends a verbal description of the current
            // errno value after the user-supplied string
            perror("locking failed for the following reason");
            return -1;
        }

        // if we got here, we own the lock
        return fd;
    }

    void TransferDB::releaseLock()
    {
        int fd = remove(lock.c_str());
        if(fd != 0)
            perror( "Error deleting lock file." );
        else
            cout<< "Lock successfully released." << endl;
    }

    vector<string> TransferDB::tokenize(string& line)
    {
        vector<string> tokens;
        string::size_type pos1 = line.find_first_not_of(DELIMITER, 0); // skip first del
        string::size_type pos2 = line.find_first_of(DELIMITER, pos1);  // find first non-del

        //loop while more token found
        while (string::npos != pos2 || string::npos != pos1) {
            tokens.push_back(carma::util::StringUtils::trimWhiteSpace(line.substr(pos1, pos2 - pos1)));
            pos1 = line.find_first_not_of(DELIMITER, pos2);  // skip del's
            pos2 = line.find_first_of(DELIMITER, pos1);      // find next non-del
        }

        return tokens;
    }

  } // namespace archive
} // namespace carma
