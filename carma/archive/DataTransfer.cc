/**
 * @file DataTransfer.cc
 * This is class that performs data transfer functionalities
 * @author Lisa Xu
 * $Revision: 1.28 $
 * $Date: 2012/03/19 15:14:21 $
 * $Id: DataTransfer.cc,v 1.28 2012/03/19 15:14:21 mpound Exp $
 */

// Carma includes
#include "carma/archive/DataTransfer.h"
#include "carma/archive/TransferDB.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/Trace.h"
#include "carma/util/FileUtils.h"
#include "carma/util/ErrorException.h"

#define CHUNK 131072   //128K - gzip buffer size

// namespace directives
using carma::util::Program;
using carma::util::FileUtils;
using carma::util::Time;

// Namespace using directives
using namespace std;
using namespace carma::util;
using namespace carma::archive;

    DataTransfer::DataTransfer(map<string, string> arv)
    {
        vector<string> initParams;
        initParams.push_back(arv["host"]);
        initParams.push_back(arv["proxyCert"]);

        logFile = arv["transferLog"];
        string lockFile = arv["transferLockFile"];
        string dbFile = arv["transferDBFile"];

        /* test
        dbFile = "/tmp/test.db";
        vector<string> v;
        v.push_back("/tmp/testfile-1");
        v.push_back("/tmp/testfile-2");
        writeTransferDB(v, false);

        db->update(0,TransferDB::TIMESTAMP,"123456789");
        db->add("THIS-IS-A-NEW-ROW size checksum tdate ddate tstamp");
        db->writeDBFile();
        exit(0);
        */

        //Initializing
        jniObj = new ArchiveJni(initParams);

        //read transferred file list into doneList vector
        db = new TransferDB(lockFile, dbFile);
        vector<string> v1 = db->getKeys();
        vector<string> v2 = db->getTimestamps();
        int size = (int) v1.size();
        for(int i=0; i<size; i++) {
            string s = v1[i] + " " + v2[i];
            doneList.push_back(s);
        }

        Program::getLogger() << log4cpp::Priority::INFO
                             << "Entering DataTransfer...";
        CARMA_CPTRACE(Trace::TRACE6, "Entering DataTransfer...");
    }

    DataTransfer::~DataTransfer() {
        delete jniObj;
        delete db;
    }

    void DataTransfer::start(const vector<string> & p,
                             const vector<string> & dirs,
                             const string & filter) {
        update(p, dirs, filter);
    }

    void DataTransfer::stop() {
        jniObj->call_stop();
        delete jniObj;
        jniObj = NULL;
    }

    int DataTransfer::update(const vector<string> & p, 
                             const vector<string> & dirs, 
                             const string & filter)
    {
      log4cpp::Category& logger = Program::getLogger();
      const size_t dirSize = dirs.size();

      //scanning directories for input files
      vector<string> all;
      yearIsSet = false;  //used for astroheader data to indicate the year subdir is set
      rangeIsSet = false; //used for visbrick data to indicate the subdir (frame-count range)
                          //is set so we only pick the files that belong to the same subdir

      for(size_t i=0; i<dirSize; i++) {
        vector<string> files = getFiles(dirs[i], filter);
        if(!files.empty()) {
          const size_t len = files.size();
          for(size_t j=0; j<len; j++)
            all.push_back(files[j]);
        }
      }
      ostringstream msg;
      size_t listSize = all.size();
      if(listSize == 0) {
        msg << "No " << filter << "* files to transfer.";

        logger << log4cpp::Priority::INFO << msg.str();
        CARMA_CPTRACE(Trace::TRACE6, msg.str());
        cout << msg.str() << endl;

        return -1;
      }

      //print and set parameters
      setParams(p, filter);

      size_t num = 3;  //the number of files in each batch
      size_t begin = 0;
      size_t end = num;
      if(end > listSize) end = listSize;

      vector<string> aBatch;
      while(begin < listSize)
      {
        //fill up the list and zip the files before transferring
        aBatch.clear();
        for(size_t i=begin; i<end; i++) {
          string name = all[i];
          if(FileUtils::exists(name)) {
            string gzName = name + ".gz";
            try {
              //compress the file
              gzip(name, gzName);
              aBatch.push_back(gzName);
            }
            catch(const ErrorException& exc) {
              logger << log4cpp::Priority::ERROR << exc.getErrorMessage();
              CARMA_CPTRACE(Trace::TRACE4, exc.getErrorMessage());
              cout << exc.getErrorMessage() << flush;
              if(end < listSize) end++;
            }
          }
          else if(end < listSize) {
            end++;
          }
        }

        //transfer the batch of files and perform post-transfer tasks
        size_t n = aBatch.size();
        if(n > 0) {
          vector<string> v = jniObj->call_update(aBatch);
          size_t length = v.size();
          if(length > 0)
          {
            //remove .gz suffix in filenames
            vector<string> tmp;
            for(size_t i=0; i<length; i++) {
              string s1 = v[i];
              string::size_type pos = s1.find(".gz");
              string s2 = s1.substr(0, pos);
              tmp.push_back(s2);
            }

            //write to transfer db and update done-list
            writeTransferDB(tmp, toAppend);

            for(size_t i=0; i<length; i++) {   //add timestamp to each line
              string s = tmp[i];
              ostringstream timestamp;
              time_t mtime = 0;
              try {
                mtime = FileUtils::modificationTime(s);
              }
              catch(const ErrorException& exc) {
                string err = "update() " +  exc.getErrorMessage();
                logger << log4cpp::Priority::ERROR << exc.getErrorMessage();
                CARMA_CPTRACE(Trace::TRACE4, err);
                cout << err << flush;
              }

              timestamp << mtime;
              tmp[i] = s + " " + string(timestamp.str());
            }
            appendDoneList(tmp);
          }

          //delete the compressed (*.gz) files
          for(size_t i=0; i<n; i++) {
            string file = aBatch[i];
            int ret = 0;
            if(FileUtils::exists(file)) ret = remove(file.c_str());

            if(ret != 0) {
              msg.str("");
              msg << "Error removing " + file + ": " + strerror(errno);
              logger << log4cpp::Priority::WARN << msg.str();
              CARMA_CPTRACE(Trace::TRACE4, msg.str());
              //printf(err.c_str(), strerror(errno), errno);
            }
          }
        }

        begin = end;
        end = end + num;
        if(end > listSize) end = listSize;
      }

      return 0;
    }

    /**
     * Transfer quality reports
     */
    int DataTransfer::move_qr(const vector<string> & p, const string & qrDir , const string & filter)
    {
      log4cpp::Category& logger = Program::getLogger();

      //scanning the directory (/opt/sdp/quality/transfer) for input files
      vector<string> all;
      vector<string> files = getQualityReports(qrDir);
      if(!files.empty()) {
          size_t len = files.size();
          for(size_t j=0; j<len; j++)
            all.push_back(files[j]);
      }
      ostringstream msg;
      size_t listSize = all.size();
      if(listSize == 0) {
        msg << "No Quality reports to transfer.";
        logger << log4cpp::Priority::INFO << msg.str();
        CARMA_CPTRACE(Trace::TRACE6, msg.str());
        cout << msg.str() << endl;

        return -1;
      }

      setParams(p, filter);  //print and set parameters

      size_t num = 3;
      size_t begin = 0;
      size_t end = num;
      if(end > listSize) end = listSize;

      vector<string> aBatch;
      while(begin < listSize)
      {
        aBatch.clear();
        for(size_t i=begin; i<end; i++) {
          string name = all[i];
          if(FileUtils::exists(name)) aBatch.push_back(name);
          else if(end < listSize) end++;
        }

        size_t n = aBatch.size();
        if(n > 0) {
          vector<string> v = jniObj->call_update(aBatch);
          size_t length = v.size();
          if(length > 0)
          {
            //write to transfer db and update done-list
            writeTransferDB(v, toAppend);
            for(size_t i=0; i<length; i++) {   //add time-stamp to each line
              string s = v[i];
              ostringstream timestamp;
              time_t mtime = 0;
              try {
                mtime = FileUtils::modificationTime(s);
              }
              catch(const ErrorException& exc) {
                string err = "Getting timestamp of " + s + ": " + exc.getErrorMessage();
                logger << log4cpp::Priority::WARN << err;
                CARMA_CPTRACE(Trace::TRACE4, err);
                cout << err << flush;
              }

              timestamp << mtime;
              v[i] = s + " " + string(timestamp.str());
            }

            appendDoneList(v);
          }
        }

        begin = end;
        end = end + num;
        if(end > listSize) end = listSize;
      }

      return 0;
    }

    int DataTransfer::move_sza(const vector<string> & p, const vector<string> & dirs, const string & filter)
    {
      log4cpp::Category& logger = Program::getLogger();
      size_t dirSize = dirs.size();

      //scanning directories for input files
      vector<string> all;
      szaYearIsSet = false;  //to indicate the year subdir is set
      for(size_t i=0; i<dirSize; i++) {
        string dir = dirs[i];
        vector<string> files = getSZAFiles(dir);
        if(!files.empty()) {
          size_t len = files.size();
          for(size_t j=0; j<len; j++)
            all.push_back(files[j]);
        }
      }
      ostringstream msg;
      size_t listSize = all.size();
      if(listSize == 0) {
        msg << "No SZA files to transfer.";
        logger << log4cpp::Priority::INFO << msg.str();
        CARMA_CPTRACE(Trace::TRACE6, msg.str());
        cout << msg.str() << endl;

        return -1;
      }

      setParams(p, filter);  //print and set parameters

      size_t num = 3;
      size_t begin = 0;
      size_t end = num;
      if(end > listSize) end = listSize;

      vector<string> aBatch;
      while(begin < listSize)
      {
        aBatch.clear();
        for(size_t i=begin; i<end; i++) {
          string name = all[i];
          if(FileUtils::exists(name)) aBatch.push_back(name);
          else if(end < listSize) end++;
        }

        size_t n = aBatch.size();
        if(n > 0) {
          vector<string> v = jniObj->call_update(aBatch);
          size_t length = v.size();
          if(length > 0)
          {
            //write to transfer db and update done-list
            writeTransferDB(v, toAppend);
            for(size_t i=0; i<length; i++) {   //add time-stamp to each line
              string s = v[i];
              ostringstream timestamp;
              time_t mtime = 0;
              try {
                mtime = FileUtils::modificationTime(s);
              }
              catch(const ErrorException& exc) {
                string err = "Getting timestamp of " + s + ": " + exc.getErrorMessage();
                logger << log4cpp::Priority::WARN << err;
                CARMA_CPTRACE(Trace::TRACE4, err);
                cout << err << flush;
              }

              timestamp << mtime;
              v[i] = s + " " + string(timestamp.str());
            }

            appendDoneList(v);
          }
        }

        begin = end;
        end = end + num;
        if(end > listSize) end = listSize;
      }

      return 0;
    }

    /**
     * Transfer monitor point data (mpdat) only
     */
    int DataTransfer::move_mp(const vector<string> & p, const vector<string> & dirs, const string & filter)
    {
      log4cpp::Category& logger = Program::getLogger();

      //we need to look for the oldest data in all the transfer
      //directories (pick the oldest date out of the frame-count)
      size_t dirSize = dirs.size();
      frameType theSmallest = 0;
      for(size_t i=0; i<dirSize; i++) {
          string dir = dirs[i];
          struct dirent** namelist = 0;
          int n = scandir(dir.c_str(), &namelist, 0, alphasort);
          // NOTE: error code n == -1 not handled.
          bool done = false;  //should be done after found the smallest frame-count
          for (int k=0; k<n; k++) {
            if(!done) {
              const string name(namelist[k]->d_name);
              bool isGood = FileUtils::exists(dir+"/"+name);
              //Only check the names that end with .mpdat but don't contain .done or .tar or .gz;
              //and just compare with one file (the first one) since the list is sorted
              if(isGood && (name.find(".mpdat") != string::npos)
                        && (name.find(".done") == string::npos)
                        && (name.find(".tar") == string::npos)
                        && (name.find(".gz") == string::npos)
                        && !(name.compare("..")==0)
                        && !(name.compare(".")==0)) {
                //find the location of "_" and "." (e.g. complex_392526840.mpdat)
                unsigned int loc1 = name.find("_", 0);
                unsigned int loc2 = name.find(".", loc1);

                //get the MJD frame string out of the file name
                string s = name.substr(loc1+1, loc2-loc1-1);

                //convert the string to frameType number (unsigned integer)
                frameType fnum = atoi(s.data());

                //save the smallest frame-count
                if(theSmallest == 0 || theSmallest > fnum) {
                  theSmallest = fnum;
                  done = true;
                }
              }
            }

            free(namelist[k]);
          }

          if(namelist) free(namelist);  //Free resources
      }

      ostringstream msg;
      if(theSmallest == 0) {
        msg << "No mpdat files to transfer.";
        logger << log4cpp::Priority::INFO << msg.str();
        CARMA_CPTRACE(Trace::TRACE6, msg.str());
        cout << msg.str() << endl;

        return -1;
      }

      //set the date (in yyyy-mm-dd (FITS) format)
      theDate = Time::getFITSdateString(theSmallest);

      //print and set parameters
      setParams(p, filter);

      for(size_t i=0; i<dirSize; i++) {
        string dir = dirs[i];
        vector<string> files = getFiles(dir, filter);
        if(!files.empty()) {
          size_t listSize = files.size();

          size_t num = 30;  //the default number of files in each tar file
          if(dir.find("short", 0) != string::npos) num = 50;
          else if(dir.find("string", 0) != string::npos ) num = 60;

          size_t begin = 0;
          size_t end = num;
          if(end > listSize) end = listSize;

          chdir(dir.c_str());   //change directory

          vector<string> vTar;
		  vector<string> tmp;
          while(begin < listSize)
          {
        	  //fill up the list and zip the files before transferring
        	  vTar.clear();
        	  tmp.clear();
        	  string name = "";
        	  for(size_t i=begin; i<end; i++) {
        		  name = files[i];
        		  if(FileUtils::exists(name)) tmp.push_back(name);
        		  else if(end < listSize) end++;
        	  }

        	  //transfer the files and perform post-transfer tasks
        	  size_t n = tmp.size();
        	  if(n > 0) {
        		  string tar = name + ".tar.gz";
        		  string cmd = "tar -czhf " + tar;
        		  for(size_t x=0; x<n; x++) {
        			  cmd = cmd + " " + tmp[x];
        		  }

        		  //make tar file
        		  int ret = system(cmd.c_str());
				  if (ret != 0) {
					  msg.str("");
					  msg << "Error making tar file: " << tar << endl;
					  logger << log4cpp::Priority::WARN << msg.str();
					  CARMA_CPTRACE(Trace::TRACE4, msg.str());
				  }

				  // transfer the tar file
				  if(FileUtils::exists(tar)) {
					  string fullname = dir + "/" + tar;
					  vTar.push_back(fullname);

					  vector<string> v = jniObj->call_update(vTar);
					  size_t length = v.size();
					  if(length > 0 && FileUtils::exists(tar))  // successfully transferred
					  {
						  ret = remove(tar.c_str());  // remove the tar file
						  if (ret != 0) {
							  msg.str("");
							  msg << "Error removing tar file: " << tar << endl;
							  logger << log4cpp::Priority::WARN << msg.str();
							  CARMA_CPTRACE(Trace::TRACE4, msg.str());
						  }

						  //mark mpdat files .done
						  for(size_t i=0; i<n; i++) {
							  string file = tmp[i];
							  string done = file + ".done";
							  ret = rename(file.c_str(), done.c_str());
							  if (ret != 0) {
								  msg.str("");
								  msg << "Error renaming file: " << file << endl;
								  logger << log4cpp::Priority::WARN << msg.str();
								  CARMA_CPTRACE(Trace::TRACE4, msg.str());
							  }
						  }
					  }
				  }
        	  }

        	  begin = end;
        	  end = end + num;
        	  if(end > listSize) end = listSize;

          } // end of while loop
        } //end of checking file list
      } //end of scanning dirs

      return 0;
    }

    /**
     * Print and set input parameters
     */
    void DataTransfer::setParams(const vector<string> & params, const string & filter) {
      size_t len = params.size();
      vector<string> p(len);
      for(size_t i=0; i<len; i++) {
        ostringstream msg;
        switch(i) {
          case 0: {
            p[i] = params[i];
            msg << "Source Root Dir = " << p[i];
            break;
          }
          case 1: {
            p[i] = params[i];

            //add date to desRoot for .mpdat files
            if(filter.compare(".mpdat")==0) {
                p[i] = params[i] + theDate;
            }
            //add year to desRoot dir for astrohdr files
            else if(filter.compare("astrohdr_")==0) {
                p[i] = params[i] + theYear;
            }
            else if(filter.compare("VisBrickData_")==0) {
                p[i] = params[i] + theRange;
            }
            else if(filter.compare("sza_")==0) {
                p[i] = params[i] + szaYear;
            }

            msg << "Destination Root Dir = " << p[i];
            break;
          }
          case 2: {
            string date = Time::getDateString();
            p[i] = params[i] + "_" + date;
            msg << "Report file = " << p[i];
            break;
          }
          case 3: {
            p[i] = params[i];
            msg << "Bandwidth Limit file = " << p[i];
            break;
          }
          case 4: {
            p[i] = params[i];
            msg << "Maximum Bandwidth = " << p[i];
            break;
          }
          default: {
            p[i] = params[i];
            msg << "Input Parameter = " << p[i];
            break;
          }
        }

        CARMA_CPTRACE(Trace::TRACE6, msg.str());
        cout << msg.str() << endl;
      }

      //set archive parameters
      jniObj->set_params(p);
    }

    /**
     * Compress file source and save as file dest (in gzip format).
     */
    void DataTransfer::gzip(const string & source, const string & dest)
    {
        /* check zlib version
        static const char* myVersion = ZLIB_VERSION;
        if (zlibVersion()[0] != myVersion[0]) {
            fprintf(stderr, "incompatible zlib version\n");
            exit(1);
        }
        else if (strcmp(zlibVersion(), ZLIB_VERSION) != 0) {
            fprintf(stderr, "warning: different zlib version\n");
        }

        printf("zlib version %s = 0x%04x, compile flags = 0x%lx\n",
               ZLIB_VERSION, ZLIB_VERNUM, zlibCompileFlags());
        */

        //allocation
        string error = "";
        Byte *comp;
        comp = (Byte*) calloc((uInt)CHUNK, 1);
        if (comp == Z_NULL) {
            error = "DataTransfer::gzip(): out of memory\n";
            Program::getLogger() << log4cpp::Priority::ERROR << error.c_str();
            CARMA_CPTRACE(Trace::TRACE4, error.c_str());
            cout << error << endl;
            exit(1);
        }

        FILE* src = fopen(source.data(), "r");
        if(src == NULL) {
            error = "DataTransfer::gzip(): Could not open file " + source;
            throw CARMA_EXCEPTION(ErrorException, error.c_str());
        }

        //add .gz extention if there isn't one
        //unsigned pos = dest.find(".gz", dest.length()-3);
        //if(pos == string::npos) dest = dest + ".gz";

        gzFile file = gzopen(dest.data(), "wb6");
        if (file == NULL) {
            error = "DataTransfer::gzip(): gzopen error: " + dest;
            throw CARMA_EXCEPTION(ErrorException, error.c_str());
        }

        int err, flush, read, write;
        do {
            read = fread(comp, 1, CHUNK, src);
            write = gzwrite(file, comp, read);
            if (write != read) {
                error = "gzwrite error: " + string(gzerror(file, &err));
                throw CARMA_EXCEPTION(ErrorException, error.c_str());
            }

            flush = feof(src) ? Z_FINISH : Z_NO_FLUSH;

        } while (flush != Z_FINISH);

        // close files and clean up
        gzclose(file);
        fclose(src);
        free(comp);
    }

    /** decompress a .gz file --- not fully tested yet.
    void DataTransfer::gunzip(string& gzfile, string& dest) {
        Byte *buff = (Byte*) calloc((uInt)CHUNK, 1);
        if (buff == Z_NULL) {
            fprintf(stderr, "gunzip: out of memory\n"); exit(1);
        }
        gzFile src = gzopen(gzfile.data(), "rb");
        if(src == NULL) {
            fprintf(stderr, "gzopen error\n"); exit(1);
        }
        FILE* file = fopen(dest.data(), "w");
        int flush, read, write;
        do {
            read = gzread(src, buff, CHUNK);
            write = fwrite(buff, 1, read, file);
            if (write != read) {
                fprintf(stderr, "gunzip can't write to file.\n"); exit(1);
            }
            flush = gzeof(src) ? Z_FINISH : Z_NO_FLUSH;
        } while (flush != Z_FINISH);
        gzclose(src); fclose(file); free(buff);
    } */

    vector<string> DataTransfer::getFiles(const string & dir, const string & filter)
    {
      // Initialization
      vector<string> retval;

      // Determine directory prefix for constructing the full file name
      string::size_type pidx = dir.find_last_of("/");
      string prefix;
      if ((pidx != string::npos) && ((pidx+1) == dir.length())) {
        prefix = dir;
      }
      else {
        prefix = dir + "/";
      }

      // Read the directory using scandir
      struct dirent** namelist = 0;
      int n = scandir(dir.c_str(), &namelist, 0, alphasort);
      toAppend = true;  //to append the donelist (transfer log) file or not
      for (int i=0; i<n; i++) {
        const string name(namelist[i]->d_name);
        free(namelist[i]);

    	if(name.compare(".")==0 || name.compare("..")==0) continue;

        string fullFileName = prefix + name;
        bool good = FileUtils::exists(fullFileName);
        if(good)
        {
          unsigned int loc = name.find(filter);
          unsigned int gz = name.find(".gz");
          unsigned int write = name.find(".write");
          if(loc != string::npos && (gz==string::npos) && (write==string::npos))
          {
            loc = name.find(".done");

            //monitor point data
            if (filter.compare(".mpdat")==0) {
              //First check if name has ".done" in it.
              if(loc != string::npos || name.find(".tar") != string::npos) {
                good = false;    //found ".done" or ".tar"
              }
              else {  //check the date
                string myDate = getDate(name);
                if(myDate.compare(theDate) != 0)
                  good = false;  //don't pick the file if my date is diff
                                 //from the date set for subdir name
              }
            }
            //astroheader files
            else if(filter.compare("astrohdr_")==0) {
              // the file is not ready (not marked .done)
              if(loc == string::npos) {
                good = false;
              }
              else {
                vector<string>::iterator doneListIterator;
                doneListIterator = doneList.begin();
                for(size_t i=0; i<doneList.size(); i++)
                {
                  string s = doneList[i];
                  unsigned int pos = s.find(" ");
                  string ah_file = s.substr(0, pos);

                  //if it has been transferred, we check to see if the
                  //timestamp is changed (modified).
                  if(fullFileName.compare(ah_file) == 0)
                  {
                    string timestamp = s.substr(pos+1, s.length()-1);
                    time_t value = atoi(timestamp.data());
                    time_t mtime = 0;
                    try {
                      mtime = FileUtils::modificationTime(fullFileName);
                    }
                    catch(const ErrorException& exc) {
                      string err = "getFiles(): getting timestamp of " + fullFileName + exc.getErrorMessage();
                      Program::getLogger() << log4cpp::Priority::ERROR << err;
                      CARMA_CPTRACE(Trace::TRACE4, err);
                      cout << err << flush;

                      good = false;
                      break;
                    }

                    if(mtime == value) {   //timestamp has not changed
                        good = false;
                    }
                    //timestamp changed - need to re-transfer
                    else {
                      doneList.erase(doneListIterator);  //remove it from the done list
                      db->removeRow(i);  //remove it from the transfer db
                      toAppend = false;
                    }

                    break;
                  }

                  doneListIterator++;
                }

                // check if it belongs to picked subdir (year)
                if(good) {
                  string year = getYear(fullFileName);
                  if(year.compare("") == 0) {  //couldn't read start-frame
                    good = false;
                  }
                  else if(yearIsSet) {
                    if(year.compare(theYear) != 0) good = false;
                  }
                  else {
                    theYear = year;
                    yearIsSet = true;
                  }
                }
              }
            }
            //visBrick data
            else if(filter.compare("VisBrickData_")==0) {
                //check if the file is in transfer.log
                for(size_t i=0; i<doneList.size(); i++)
                {
                  string s = doneList[i];
                  unsigned int pos = s.find(" ");
                  string vb_file = s.substr(0, pos);
                  if(fullFileName.compare(vb_file) == 0)
                  {
                    //found it in transfer.log
                    good = false;
                    break;
                  }
                }

                // set or check subdir (frame-count range) and only pick the ones belong to same subdir
                if(good) {
                  string range = getRange(name);
                  if(rangeIsSet) {
                    if(range.compare(theRange) != 0) good = false;
                  }
                  else {
                    theRange = range;
                    rangeIsSet = true;
                  }
                }
            }

            if(good) {
            	if (filter.compare(".mpdat")==0) retval.push_back(name);  //monitor point data
            	else retval.push_back(fullFileName);
            }
          }
        }
      }

      if(namelist) free(namelist);  //Free resources
      return retval;
    }

    vector<string> DataTransfer::getSZAFiles(const string & dir)
    {
      vector<string> retval;
      string::size_type pidx = dir.find_last_of("/");
      string prefix;
      if ((pidx != string::npos) && ((pidx+1) == dir.length())) prefix = dir;
      else prefix = dir + "/";

      struct dirent** namelist = 0;
      int n = scandir(dir.c_str(), &namelist, 0, alphasort);
      toAppend = true;  //to append the donelist (transfer log) file or not
      for (int i=0; i<n; i++) {
        const string name(namelist[i]->d_name);
        free(namelist[i]);

    	if(name.compare(".")==0 || name.compare("..")==0) continue;

        string fullFileName = prefix + name;
        string surffix = name.substr(name.length()-6, 6); // the last 6 chars of name
        if(FileUtils::exists(fullFileName) && surffix.find("tar.gz") != string::npos)
        {
          //check if the file is in transfer.db
          bool good = true;
          vector<string>::iterator doneListIterator;
          doneListIterator = doneList.begin();
          for(size_t i=0; i<doneList.size(); i++) {
            string s = doneList[i];
            unsigned int pos = s.find(" ");
            string szaFile = s.substr(0, pos);
            if(fullFileName.compare(szaFile) == 0)  //found in transfer.db
            {
              string timestamp = s.substr(pos+1, s.length()-1);
              time_t value = atoi(timestamp.data());
              time_t mtime = 0;
              try {
                mtime = FileUtils::modificationTime(fullFileName);
              }
              catch(const ErrorException& exc) {
                string err = "getSZAFiles() " + exc.getErrorMessage();
                CARMA_CPTRACE(Trace::TRACE4, err);
                cout << err << flush;

                good = false;
                break;
              }

              if(mtime == value) {   //timestamp has not changed
                good = false;
              }
              //timestamp changed - need to re-transfer
              else {
                doneList.erase(doneListIterator);  //remove it from the done list
                db->removeRow(i);  //remove it from the transfer db
                toAppend = false;
              }

              break;
            }

            doneListIterator++;
          }

          if(good) {
            chdir(dir.c_str());   //change to dir directory

            //untar the sza file (e.g. zrcx253.disk2.4.mir.tar.gz)
            unsigned int loc = name.find(".tar.gz", 0);  //find pos of ".tar.gz"
            string miriad = name.substr(0, loc);
            string cmd = "tar -zxf " + name;
            int ret = system(cmd.c_str());
            if(ret != 0) {
              string err = "Error untar " + name;
              Program::getLogger() << log4cpp::Priority::ERROR << err;
              CARMA_CPTRACE(Trace::TRACE4, err);
              cout << err << flush;
              good = false;  //something went wrong when untarring
            }

            if(FileUtils::exists(miriad) && good) {
              //check file sizes (inside of the miriad directory)
              string mlist[5] = {"flags", "header", "vartable", "visdata", "wflags"};
              chdir(miriad.c_str());
              for (int j=0; j<5; j++) {
                string mfile = mlist[j];

                int filesize = 0;
                try {
                  filesize = FileUtils::size(mfile);
                }
                catch(const ErrorException& exc) {
                  string err = "Getting size of " + mfile + " ... " +  exc.getErrorMessage();
                  CARMA_CPTRACE(Trace::TRACE4, err);
                  cout << err << flush;
                }

                if(filesize == 0) {
                  Program::getLogger() << log4cpp::Priority::INFO << "Found zero length file in " << name;
                  good = false;  //found a zero length file
                  break;
                }
              }

              // now get date from the header
              if(good) {
                chdir(dir.c_str());   //change back to dir directory
                carma::sdp::MiriadUV* muv_ = 0;
                try {
                  muv_ = new carma::sdp::MiriadUVBin();
                  muv_->uvopen(miriad.c_str(),"old");
                  double preamble[5];
                  int n = 1000;
                  float data[n];
                  int flags[n], nread;
                  muv_->uvread(preamble, data, flags, n, nread);
                  double time = preamble[2] - 2400000.5;
                  muv_->uvclose();
                  delete muv_;
                  string date = Time::getDateTimeString(time,0,"%C%y-%m-%dT");
                  string year = date.substr(0,4);
                  string cmd = "rm -rf " + miriad;  //remove untarred file (dir)
                  int ret = system(cmd.c_str());
                  if ( ret == -1 ) {
                      ostringstream os;
                      os << "Failed to remove directory " << miriad;
                      programLogWarnIfPossible( os.str() );
                  }

                  if(szaYearIsSet) {
                    if(year.compare(szaYear) != 0) good = false;
                  }
                  else {
                    szaYear = year;
                    szaYearIsSet = true;
                  }
                }
                catch(const ErrorException& exc) {
                  if ( muv_ != 0 )  {
                      muv_->uvclose();
                      delete muv_;
                  }
                  string err = "Checking sza file " + miriad + ": " +  exc.getErrorMessage();
                  Program::getLogger() << log4cpp::Priority::INFO << err;
                  CARMA_CPTRACE(Trace::TRACE4, err);
                  cout << err << flush;

                  good = false;  //skip this file
                }
              }
            }
            else { //empty tar.gz file
              good = false;
              Program::getLogger() << log4cpp::Priority::INFO << name << " is empty.";
            }
          }

          if(good) retval.push_back(fullFileName);
        }
      }

      if(namelist) free(namelist);  //Free resources
      return retval;
    }

    /**
     * Scan quality report directory (/opt/sdp/quality/transfer) for
     * the tar.gz files need to be transferred or re-transferred
     */
    vector<string> DataTransfer::getQualityReports(const string & dir)
    {
      vector<string> retval;
      string::size_type pidx = dir.find_last_of("/");
      string prefix;
      if ((pidx != string::npos) && ((pidx+1) == dir.length())) prefix = dir;
      else prefix = dir + "/";

      struct dirent** namelist = 0;
      int n = scandir(dir.c_str(), &namelist, 0, alphasort);
      toAppend = true;  //to append the donelist (transfer log) file or not
      for (int i=0; i<n; i++) {
        const string name(namelist[i]->d_name);
        free(namelist[i]);
        if(name.compare(".")==0 || name.compare("..")==0) continue;
        string fullFileName = prefix + name;
        string surffix = name.substr(name.length()-6, 6); // the last 6 chars of name
        if(FileUtils::exists(fullFileName) && surffix.find("tar.gz") != string::npos)
        {
          //check if the file is in transfer.db
          bool good = true;
          vector<string>::iterator doneListIterator;
          doneListIterator = doneList.begin();
          for(size_t i=0; i< doneList.size(); i++) {
            string s = doneList[i];
            unsigned int pos = s.find(" ");
            string qrFile = s.substr(0, pos);
            if(fullFileName.compare(qrFile) == 0)  //found in transfer.db
            {
              string timestamp = s.substr(pos+1, s.length()-1);
              time_t value = atoi(timestamp.data());
              time_t mtime = 0;
              try {
                mtime = FileUtils::modificationTime(fullFileName);
              }
              catch(const ErrorException& exc) {
                string err = "getQualityReports(): " + exc.getErrorMessage();
                Program::getLogger() << log4cpp::Priority::ERROR << err;
                CARMA_CPTRACE(Trace::TRACE4, err);
                cout << err << flush;

                good = false;
                break;
              }

              if(mtime == value) {   //timestamp has not changed
                good = false;
              }
              //timestamp changed - need to re-transfer
              else {
                doneList.erase(doneListIterator);  //remove it from the done list
                db->removeRow(i);  //remove it from the transfer db
                toAppend = false;
              }

              break;
            }

            doneListIterator++;
          }

          if(good) retval.push_back(fullFileName);
        }
      }

      if(namelist) free(namelist);  //Free resources
      return retval;
    }

    /**
     * retrive the year value out of the starframe string which
     * is in the second line of the astrohdr file.
     * name -- the astrohdr file name
     */
    string DataTransfer::getYear(const string & name) {
      string s = "";
      ifstream file(name.c_str());
      if(file.is_open()) {
        if(!file.eof()) getline(file, s);
        s = "";
        if(!file.eof()) getline(file, s);  //we want the 2nd line

        if(s.compare("") == 0) {
            cout << "Could not read startframe number of " << name << endl;
            return s;
        }

        //the 2nd line like this: <INTEGRATION startframe="385858998">
        string::size_type pos1 = s.find("startframe=");
        string::size_type pos2 = s.find(">");

        //the position of first digit
        pos1 = pos1 + 12;
        string frame = s.substr(pos1, pos2-pos1-1);
        //cout << "Line: " << s << "  Start Frame: " << frame<< endl;

        //first convert the string to frameType number (unsigned integer)
        frameType num = atoi(frame.data());

        //now get date in yyyy-mm-dd (FITS) format
        string date = Time::getFITSdateString(num);

        s = date.substr(0,4);
        file.close();
      }

      return s;
    }

    string DataTransfer::getDate(const string & name) {
        //find the location of "_" and "." in a mpdat file
        //such as complex_392526840.mpdat
        unsigned int loc1 = name.find("_");
        unsigned int loc2 = name.find_first_of(".", loc1);

        //get the MJD frame string out of the file name
        string s = name.substr(loc1+1, loc2-loc1-1);

        //first convert the string to frameType number (unsigned integer)
        frameType num = atoi(s.data());

        //now get date in yyyy-mm-dd (FITS) format
        string date = Time::getFITSdateString(num);
        return date;
    }

    string DataTransfer::getRange(const string & name) {
        //find the location of "_" in visbrick filename (visBrickData_564309003)
        unsigned int loc = name.find("_", 0);

        //get the first two numbers of the MJD frame-count, then find the range
        string s = name.substr(loc+1, 2);
        string range = s + "0000000_" + s + "9999999";

        return range;
    }

    void DataTransfer::writeTransferLog(const vector<string> & list) {
      size_t size = list.size();
      ofstream out;
      out.open(logFile.c_str(), ios::app);
      for(size_t i=0; i < size; ++i) {
          out << list[i] << endl;
      }
      out.close();
    }

    void DataTransfer::writeTransferDB(const vector<string> & list, const bool append) {
      //generate the db records from the file list
      size_t size = list.size();
      vector<string> records(size);
      for(size_t i=0; i<size; i++) {
        off_t filesize = 0;
        try {
          filesize = FileUtils::size(list[i]);
        }
        catch(const ErrorException& exc) {
          string err = "Getting size of " + list[i] + " ... " +  exc.getErrorMessage();
          CARMA_CPTRACE(Trace::TRACE4, err);
          cout << err << flush;
        }

        ostringstream nBytes, timestamp;
        nBytes << filesize;
        time_t mtime = 0;
        string checksum = "5b0bb8d75caa7aa05d52a75c15cf73d1";  //init random number
        try {
          mtime = FileUtils::modificationTime(list[i]);
          checksum = FileUtils::computeMessageDigest(list[i], MD5);
        }
        // an astroheader could be none existing (filename changed) when re-opened, so just
        // print the exception, write the record into db, and re-transfer when it's done again
        catch(const ErrorException& exc) {
          string err = "Getting timestamp of " + list[i] + " ... " +  exc.getErrorMessage();
          CARMA_CPTRACE(Trace::TRACE4, err);
          cout << err << flush;
        }

        timestamp << mtime;
        string transferDate = Time::getFITSdateString();

        //compose the db record for the file. Note the "NA" following the
        //transfer date means the delete date is not available yet
        records[i] = list[i] + " " + string(nBytes.str()) + " " + checksum + " " +
                     transferDate + " NA " + string(timestamp.str());

        //cout << records[i] << endl;
      }

      db->updateDBFile(records, append);
    }

    void DataTransfer::appendDoneList(const vector<string>& list) {
        size_t size = list.size();
        for(size_t i=0; i<size; i++) {
            doneList.push_back(list[i]);
        }
    }

