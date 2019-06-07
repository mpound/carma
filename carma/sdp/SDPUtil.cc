/**
 *
 * @file SDPUtil.cc
 *
 * Utility class for the science data products subsystem.
 *
 * $Id: SDPUtil.cc,v 1.5 2012/02/06 21:23:13 friedel Exp $
 *
 * @author Athol Kemball
 */

// Carma includes
#include "carma/sdp/SDPUtil.h"

#include "carma/util/StringUtils.h"
#include "carma/util/FileUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma tools includes
#include "log4cpp/Category.hh"
#include "log4cpp/Priority.hh"
#include "boost/filesystem.hpp"

// C++ standard library or system includes
#include <fstream>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <unistd.h>
#include <limits.h>
#include <dirent.h>
#include <errno.h>
#include <cstring>
#include <stdlib.h>

// Namespace using directives
using carma::util::Program;

namespace carma {
  namespace sdp {

    //=========================================================================
    
    std::string SDPUtil::astroHdrFileName(const std::string& dir,
					  const std::string& obsBlockId)
    {
      // Compose the astro hdr file name for a given observing block id.
      
      // Initialization
      std::string retval;

      // Construct the asto header file name
      std::string trimDir = carma::util::StringUtils::trimWhiteSpace(dir);
      std::ostringstream ostr;
      if (!trimDir.empty()) {
	ostr << trimDir << "/";
      };
      ostr << "astrohdr_" << obsBlockId << ".xml";
      retval = ostr.str();
      return retval;
    };

    //=========================================================================

    std::string SDPUtil::sdpFileName(const std::string& dir,
				     const std::string& astroHdrFile)
    {
      // Compose the output SDP file name associated with an input astro header

      // Initialization
      std::string retval;

      // Construct the sdp output file name
      std::string trimDir = carma::util::StringUtils::trimWhiteSpace(dir);
      if (!trimDir.empty()) {
	retval = trimDir + "/";
      };
      retval += extractObsBlockId(astroHdrFile) + ".mir";
      return retval;
    };

    //=========================================================================

    void SDPUtil::deleteSDPFile(const std::string& outputSDPFile)
    {
      // Delete an existing sdp output file.

      // Delete all components of the file

      boost::filesystem::path mir_folder(outputSDPFile.c_str());
      boost::filesystem::remove_all( mir_folder );
 
      return;
    };

    //=========================================================================

    std::string SDPUtil::extractObsBlockId(const std::string& fileName)
    {
      // Extract the observing block id. from a file name

      // Initialization
      std::string retval;

      // Search for the obs block id. markers; need to deal with both
      // input monitor point flat files and output astro hdr files
      std::string::size_type aidx = fileName.find("astrohdr_");
      std::string::size_type xidx = fileName.find(".xml");
      std::string::size_type midx = fileName.find(".mpdat");
      std::string::size_type ridx = fileName.find(".read");
      std::string::size_type eidx = fileName.find(".error");

      // Case file type of:
      // a) Input monitor point flat file
      if ((midx != std::string::npos) && (ridx != std::string::npos) &&
	  ((ridx-1) > (midx+8))) {
	retval = fileName.substr(midx+7,ridx-midx-7);
      };
      if ((midx != std::string::npos) && (eidx != std::string::npos) &&
	  ((eidx-1) > (midx+8))) {
	retval = fileName.substr(midx+7,eidx-midx-7);
      };

      // b) Output astro hdr file
      if ((aidx != std::string::npos) && (xidx != std::string::npos) &&
	  (xidx > (aidx+1))) {
	retval = fileName.substr(aidx+9, xidx-aidx-9);
      };
      return retval;
    };

    //=========================================================================

    std::vector<std::string> SDPUtil::parseObsBlockId(const std::string& 
						      obsBlockId)
    {
      // Parse an obs. block id. descriptor into sub-components.
  
      // Initialization
      std::vector<std::string> retval;
      std::string::size_type indx = 0;
      std::string::size_type jndx;

      // Extract sub-components of the obs. block id. string
      jndx = obsBlockId.find('.',indx);
      while (jndx != std::string::npos) {
	retval.push_back(obsBlockId.substr(indx,jndx-indx));
	indx = jndx + 1;
	jndx = obsBlockId.find('.',indx);
      };
      retval.push_back(obsBlockId.substr(indx));
      return retval;
    };

    //=========================================================================
    void SDPUtil::mailRTS(const std::string& message){
	std::string fl = "echo \"";
	fl += message + "\" > /tmp/junk";
	system(fl.c_str());
	std::string mail = "/usr/bin/mail ";
	mail += "-s 'sdpFiller Failure' rts@mmarray.org < /tmp/junk";
	system(mail.c_str());
	return;
    };
    //=========================================================================

  } // namespace sdp
} // namespace carma
//============================================================================

