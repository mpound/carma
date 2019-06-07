/**
 * @file dataTransfer.cc
 *
 * @usage archiveExe dbconffile=dbms/dbms.conf
 *                  sdpconffile=sdp/sdp.conf
 *                  arvconffile=archive/archive.conf
 *
 * @description a main process of data transfer that tranfers data files from
 * carma high site to NCSA through GridFTP server
 *
 * @key dbconffile  dbms/dbms.conf       string RDBMS configuration file.
 * @key sdpconffile sdp/sdp.conf         string SDP configuration file.
 * @key arvconffile archive/archive.conf string Archive configuration file
 * @key emulate     false                bool   Emulation mode for testing.
 *
 * @logger DEFAULT_FACILITY carma.archive.dataTransfer
 *
 * $Revision: 1.16 $
 * $Date: 2012/03/07 22:16:07 $
 * $Id: dataTransfer.cc,v 1.16 2012/03/07 22:16:07 mpound Exp $
 */

// Carma includes
#include <carma/archive/DataTransfer.h>
#include <carma/util/Program.h>
#include <carma/util/programLogging.h>
#include <carma/util/Trace.h>
#include <carma/util/Time.h>
#include <carma/util/KeyValueConfigFile.h>
#include <carma/util/NotFoundException.h>
#include <carma/dbms/MonitorSystemAndDBMSRelationships.h>

// Std C++ includes
#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <exception>

// C includes
#include <stdlib.h>
#include <time.h>

// namespace directives
using namespace std;
using namespace carma::archive;
using namespace carma::util;
using carma::dbms::MonitorAverageType;

// Main
int Program::main() {
  map<string, string> dbconf, sdpconf, arvconf;
  string dbconffile, sdpconffile, arvconffile;
  bool emulate = getBoolParameter("emulate");
  try {
    // Get input db parameter values
    dbconffile = getConfFile(getStringParameter("dbconffile"));

    // Read db configuration file parameters
    dbconf = KeyValueConfigFile::load(dbconffile);
  }
  catch (const NotFoundException& exc) {
    ostringstream messg;
    messg << "Unable to read db configuration file " << dbconffile << endl;
    messg << " " << exc.getMessage();
    CARMA_CPTRACE(carma::util::Trace::TRACE1, messg);
    programLogCriticalIfPossible( messg.str() );
    exc.report();
    return EXIT_FAILURE;
  }

  try {
    // Get input sdp parameter values
    sdpconffile = getConfFile(getStringParameter("sdpconffile"));

    // Read sdp configuration file parameters
    sdpconf = KeyValueConfigFile::load(sdpconffile);
  }
  catch (const NotFoundException& exc) {
    ostringstream messg;
    messg << "Unable to read sdp configuration file " << sdpconffile << endl;
    messg << " " << exc.getMessage();
    CARMA_CPTRACE(carma::util::Trace::TRACE1, messg);
    programLogCriticalIfPossible( messg.str() );
    exc.report();
    return EXIT_FAILURE;
  }

  try {
    // Get input archive parameter values
    arvconffile = getConfFile(getStringParameter("arvconffile"));

    // Read archive configuration file parameters
    arvconf = KeyValueConfigFile::load(arvconffile);
  }
  catch (const NotFoundException& exc) {
    ostringstream messg;
    messg << "Unable to read archive configuration file "<< arvconffile << endl;
    messg << " " << exc.getMessage();
    programLogCriticalIfPossible( messg.str() );
    CARMA_CPTRACE(Trace::TRACE1, messg);
    exc.report();
    return EXIT_FAILURE;
  }

  //monitor point data params
  vector<string> mpParams = vector<string>();
  mpParams.push_back(dbconf["transferDir"]);
  mpParams.push_back(arvconf["mpDesRoot"]);
  string mpFilter = ".mpdat";

  vector<string> mpDirs = vector<string>();
  mpDirs.push_back(dbconf["transferMinuteComplexDir"]);
  mpDirs.push_back(dbconf["transferMinuteNumericDir"]);
  mpDirs.push_back(dbconf["transferMinuteShortDir"]);
  mpDirs.push_back(dbconf["transferMinuteStringDir"]);

  mpDirs.push_back(dbconf["transferWBCorrelComplexDir"]);
  mpDirs.push_back(dbconf["transferWBCorrelNumericDir"]);
  mpDirs.push_back(dbconf["transferWBCorrelShortDir"]);
  mpDirs.push_back(dbconf["transferWBCorrelStringDir"]);

  mpDirs.push_back(dbconf["transferSLCorrelComplexDir"]);
  mpDirs.push_back(dbconf["transferSLCorrelNumericDir"]);
  mpDirs.push_back(dbconf["transferSLCorrelShortDir"]);
  mpDirs.push_back(dbconf["transferSLCorrelStringDir"]);

  //astro header params
  vector<string> sdpParams = vector<string>();
  //sdpParams.push_back(sdpconf["top"]);
  sdpParams.push_back(sdpconf["astroHeaderDir"]);
  sdpParams.push_back(arvconf["sdpDesRoot"]);
  string sdpFilter = "astrohdr_";

  string astroHeaderDir = sdpconf["astroHeaderDir"];
  string::size_type pidx = astroHeaderDir.find_last_of("/");
  string prefix;
  if ((pidx != string::npos) && ((pidx+1) == astroHeaderDir.length()))
    prefix = astroHeaderDir;
  else
    prefix = astroHeaderDir + "/";

  vector<MonitorAverageType> mat;
  mat.push_back(dbms::FRAME_AVG);
  mat.push_back(dbms::MINUTE_AVG);
  mat.push_back(dbms::WBCORREL_AVG);
  mat.push_back(dbms::SLCORREL_AVG);

  vector<string> sdpDirs = vector<string>();
  //sdpDirs.push_back(sdpconf["astroHeaderDir"]);

  //we need to get sub-directories of astroheader
  for(size_t i = 0; i < mat.size(); i++) {
    string subdir = prefix + carma::dbms::toString(mat[i]) + "/";
    sdpDirs.push_back(subdir);
    //cout << "sdp subdir: " << subdir <<endl;
  }

  //quality report params
  vector<string> qrParams = vector<string>();
  string qrDir = sdpconf["qualityDir"] + "/transfer";
  qrParams.push_back(qrDir);
  qrParams.push_back(arvconf["qrDesRoot"]);
  string qrFilter = "qr_";

  //visbrick data params
  vector<string> vbParams = vector<string>();
  //vbParams.push_back(sdpconf["top"]);
  vbParams.push_back(sdpconf["visBrickDir"]);
  vbParams.push_back(arvconf["vbDesRoot"]);

  vector<string> vbDirs = vector<string>();
  vbDirs.push_back(sdpconf["visBrickDir"]);
  string vbFilter = "VisBrickData_";

  vector<string> tmp = vector<string>();
  tmp.push_back(arvconf["reportFile"]);
  tmp.push_back(arvconf["bwFile"]);
  tmp.push_back(arvconf["maxBW"]);

  for(size_t i = 0; i < tmp.size(); i++) {
    mpParams.push_back(tmp[i]);
    sdpParams.push_back(tmp[i]);
    vbParams.push_back(tmp[i]);
    qrParams.push_back(tmp[i]);
  }

  string poll = arvconf["pollingTime"];
  int pTime = atoi(poll.data()) * 60;  //seconds

  DataTransfer * transfer = new DataTransfer(arvconf, 
                     Time::computeClosestFrame(),
                     static_cast<util::frameType>(pTime*2), 
                     emulate );
  while(true) {
    //int vb = 
      transfer->update(vbParams, vbDirs, vbFilter);
      transfer->updateMonitorPoints();
    //int sdp = 
      transfer->update(sdpParams, sdpDirs, sdpFilter);
      transfer->updateMonitorPoints();
    //int qr = 
      transfer->move_qr(qrParams, qrDir, qrFilter);
      transfer->updateMonitorPoints();
    //int mp = 
      transfer->move_mp(mpParams, mpDirs, mpFilter);
      transfer->updateMonitorPoints();

    //if((vb<0) && (sdp<0) && (mp<0)) {
        ostringstream msg;
        msg << "Data transfer: sleep " << pTime << " sec. " << endl;
        const string msgstr = msg.str();
        CARMA_CPTRACE(carma::util::Trace::TRACE6, msgstr);
        //programLogInfoIfPossible( msgstr );
        cout << msgstr << flush;
        sleep(pTime);
    //}
  }

  // Exit
  transfer->stop();
  delete transfer;
  return EXIT_SUCCESS;
}
