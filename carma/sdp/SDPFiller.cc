/**
 *
 * @file SDPFiller.cc
 *
 * A class to create output files in the science data product format.
 *
 * $Id: SDPFiller.cc,v 1.45 2013/04/16 15:38:57 friedel Exp $
 *
 * @author Harold Ravlin & Athol Kemball
 */

// Carma includes
#include "carma/sdp/SDPFiller.h"
#include "carma/sdp/XMLHandler.h"
#include "carma/sdp/SDPUtil.h"

#include "carma/monitor/DataflowSubsystem.h"

#include "carma/util/ErrorException.h"
#include "carma/util/NotFoundException.h"
#include "carma/util/StringUtils.h"
#include "carma/util/FileUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma tools includes
#include "log4cpp/Category.hh"
#include "log4cpp/Priority.hh"
#include "xercesc/util/PlatformUtils.hpp"
#include "xercesc/sax2/XMLReaderFactory.hpp"
#include "xercesc/sax2/SAX2XMLReader.hpp"
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

// Namespace using directives
using carma::util::Program;
using carma::sdp::XMLHandler;


namespace carma {
  namespace sdp {

std::vector<std::string> SDPFiller::badFiles;

//============================================================================

SDPFiller::SDPFiller(const std::string& astroHeaderDir,
		     const std::string& visBrickDir,
		     const std::string& scienceDataFormatDir, 
		     const std::string& recycleDir,
		     const int& sleep,
		     const std::string& stopFile,
		     const bool writeFloats,
		     const bool getScript,
		     const bool force) :
  astroHeaderDir_p(astroHeaderDir),
  visBrickDir_p(visBrickDir),
  scienceDataFormatDir_p(scienceDataFormatDir),
  recycleDir_p(recycleDir),
  sleep_p(sleep),
  stopFile_p(stopFile),
  lastFrameWritten_p(),
  writeFloats_( writeFloats ),
  getScript_( getScript ),
  force_( force )
                    
{
  // Construct from the configuration parameters used by the science
  // data products filler.
  dataflow_.startAutoWriter(0.05);
  dataflow_.sdpFiller().errorState().setValue(false);
  badFiles.clear();

};

//============================================================================

SDPFiller::~SDPFiller()
{
  // Destructor
};

//============================================================================

void SDPFiller::reset()
{
  // Reset the science data products filler.

  return;
};

//============================================================================

void SDPFiller::processFiles(const std::vector<std::string>& 
			     inputAstroHdrFiles,
			     const carma::util::frameType& startFrame, 
			     const carma::util::frameType& endFrame,
			     const std::string& corrType,
			     const bool& append, const bool& pdb)
{
  // Process a list of astronomical header files.

  return;
};

//===========================================================================

bool SDPFiller::in(std::string & file)
{
  for(uint i=0; i < badFiles.size(); i++){
    if(badFiles[i].find(file) != std::string::npos)
      return true;
  }
  return false;
}

//============================================================================

void SDPFiller::processFiles(const std::vector<std::string>& 
			     inputAstroHdrFiles,
			     const carma::util::frameType& startFrame, 
			     const carma::util::frameType& endFrame,
			     const std::string& corrType,
			     const std::string& outputSDPFile,
			     const bool& append, const bool& pdb)
{
  // Process a list of astronomical header files within a frame count
  // range to a specified output file.

  // Initialization:
  // Logger reference
  log4cpp::Category& logger = Program::getLogger();
  // Initialize the XML4C2 system
  XMLPlatformUtils::Initialize();

  // Create a SAX2 XML parser
  SAX2XMLReader* parser = XMLReaderFactory::createXMLReader();
  // Set auto schema validation
  parser->setFeature(XMLUni::fgSAX2CoreValidation, true);
  parser->setFeature(XMLUni::fgXercesDynamic, true);
  // Disable full schema checking
  parser->setFeature(XMLUni::fgXercesSchemaFullChecking, false);
  // Enable namespace processing
  parser->setFeature(XMLUni::fgSAX2CoreNameSpaces, true);
  
  // Create a handler for the astro header output XML, and register
  // it with the parser
  XMLHandler *handler = new XMLHandler();
  parser->setContentHandler(handler);
  // Disable non-fatal XML parse errors 
  parser->setErrorHandler(NULL);
  // Disable own parse error handler later
  // parser->setErrorHandler(handler);

  // Configure the XML handler
  handler->configure(visBrickDir_p, corrType);

  // Parse the input astro header files
  std::string currentInputFile;
  // Loop over input files
  for (uint i=0; i < inputAstroHdrFiles.size(); i++) {
    currentInputFile = inputAstroHdrFiles[i];
    if(in(currentInputFile))
      continue;
    handler->setCurrentFile(currentInputFile);
    // Determine if the current input astro header file is being updated
    bool writeStatus = 
      (currentInputFile.find(".xml.write") != std::string::npos);
    bool incremental = (lastFrameWritten_p.count(currentInputFile) > 0) ?
      lastFrameWritten_p[currentInputFile] > 0 : false;

    bool justGatherPdb = false;

    // Decide if the output file is to be appended to or overwritten
    std::string mode;
    carma::util::frameType sFrame = startFrame;
    carma::util::frameType eFrame = endFrame;
    if ((writeStatus && incremental) || append) {
      // Append mode
      mode = "append";
      // Set frame ranges
      if (writeStatus && incremental) {
	sFrame = std::max(startFrame, lastFrameWritten_p[currentInputFile]+1);
	// Write trace message
	std::ostringstream msg;
	msg << "Incremental fill [" << sFrame << "," << eFrame << "], "
	    << currentInputFile;
	logger << log4cpp::Priority::INFO << msg.str();
	CARMA_CPTRACE(carma::util::Trace::TRACE6, msg.str());
      } else if (append) {
	std::ostringstream msg;
	msg << "Append fill [" << sFrame << "," << eFrame << "], "
	    << currentInputFile;
	logger << log4cpp::Priority::INFO << msg.str();
	CARMA_CPTRACE(carma::util::Trace::TRACE6, msg.str());
      }
    } else {
      // Rewrite mode
      mode = "new";
      if (carma::util::FileUtils::exists(outputSDPFile)) {
	if(force_){
	  deleteSDPFile(outputSDPFile);
	}
	else{
	  justGatherPdb = true;
	}
      };
      std::ostringstream msg;
      msg << "Rewrite fill [" << sFrame << "," << eFrame << "], "
	  << currentInputFile;
      logger << log4cpp::Priority::INFO << msg.str();
      CARMA_CPTRACE(carma::util::Trace::TRACE6, msg.str());
    };

    // Select frame count range
    handler->selectFrameRange(sFrame, eFrame);

    // Open output file
    std::ostringstream msg;
    msg << "Opening miriad file for " << currentInputFile;
    logger << log4cpp::Priority::INFO << msg.str();

    bool found;
    // always write floats for WB data
    found = (currentInputFile.find("flux") != std::string::npos ||
	     currentInputFile.find("fringe") != std::string::npos ||
	     currentInputFile.find("base") != std::string::npos ||
	     currentInputFile.find("ct0") != std::string::npos ||
	     currentInputFile.find(".WB.")  != std::string::npos ||
	     currentInputFile.find("SL")  != std::string::npos ||
	     currentInputFile.find("SH")  != std::string::npos);

    bool writeFloats = found || writeFloats_;
    handler->openMiriad(currentInputFile, outputSDPFile.c_str(), 
			mode , writeFloats,justGatherPdb
                       );

    // Reset project data accumulation if required.
    if (pdb && !writeStatus) {
      handler->resetProjectData();
    };

    // Parse the current astro header file
    try {
      msg.str("");
      msg << "Parsing " << currentInputFile;
      logger << log4cpp::Priority::INFO << msg.str();

      parser->parse(currentInputFile.c_str());
      msg.str("");
      msg << "Done parse " << currentInputFile;
      logger << log4cpp::Priority::INFO << msg.str();
      

    } catch (...){
      dataflow_.sdpFiller().errorState().setValue(true);
      badFiles.push_back(currentInputFile);
      std::ostringstream emsg;
      
      emsg << "Error parsing astro hdr file: " << currentInputFile;
      logger << log4cpp::Priority::CRIT << emsg.str();
    };
      /*    } catch (const XMLException& exc) {
      if (!(writeStatus && incremental)) {
	std::ostringstream emsg;
	emsg << "Error parsing astro hdr file: " << currentInputFile
	     << ", SAX error: " << XMLString::transcode(exc.getMessage());
	logger << log4cpp::Priority::ERROR << emsg.str();
	CARMA_CPTRACE(carma::util::Trace::TRACE6, emsg.str());
      };
      };*/

    // Close output science data file
    msg.str("");
    msg << "Closing miriad file for " << currentInputFile;
    logger << log4cpp::Priority::INFO << msg.str();

    handler->closeMiriad();
    if( getScript_ )
      {
	CARMA_CPTRACE(carma::util::Trace::TRACE2, "add script");
	handler->addScript(outputSDPFile);
      }
    // Print visbrick warning messages as required
    // (Count is reset in openMiriad())
    handler->printVisBrickWarnings(incremental);

    // Update project database if required.
    if (pdb && !writeStatus) {
      handler->updateProjectDatabase();
    };

    // Update count of last integration start frame written 
    if (writeStatus) {
      lastFrameWritten_p[currentInputFile] = 
	std::max(handler->getLastFrame(), lastFrameWritten_p[currentInputFile]);
      std::ostringstream msg;
      msg << "Last frame written: " << lastFrameWritten_p[currentInputFile]
	  << " for: " << currentInputFile;
      logger << log4cpp::Priority::INFO << msg.str();
      CARMA_CPTRACE(carma::util::Trace::TRACE6, msg.str());
    };

  }; // loop over input files

  // Delete parser and terminate
  delete handler;
  delete parser;
  std::ostringstream msg;
  msg.str("");
  msg << "Cleaning up " << currentInputFile;
  logger << log4cpp::Priority::INFO << msg.str();

  XMLPlatformUtils::Terminate();

  return;
};

//============================================================================

void 
SDPFiller::processFrameCountRange(const carma::util::frameType& startFrame,
				  const carma::util::frameType& endFrame,
				  const std::string& corrType,
				  const std::string& outputSDPFile,
				  const bool& append, const bool& rt,
				  const bool& pdb)
{
  // Process input astronomical header files within a frame count range
  // to a specified output file.

  std::vector<std::string> fileList = refreshInputFileList(startFrame,
							   endFrame, true, rt);
  processFiles(fileList, startFrame, endFrame, corrType, outputSDPFile, 
	       append, pdb);
  return;
};

//============================================================================

void SDPFiller::processAll(const carma::util::frameType& startFrame, 
			   const carma::util::frameType& endFrame,
			   const std::string& corrType, const bool& rt,
			   const bool& pdb)
{
  // Process all available input astronomical header files to their
  // default output sdp files.

  // Initialization
  log4cpp::Category& logger = Program::getLogger();

  // Loop, processing all available input astro header files
  bool stopRequested = false;
  while (!stopRequested) {
    // Refresh list of available input astronomical header files;
    // include .write files if in real-time mode.
    std::vector<std::string> fileList = 
      refreshInputFileList(startFrame, endFrame, false, rt);

    // Loop over all available input astronomical header files
    int ndone = 0;
    int nwrite = 0;
    uint jfile = 0;
    bool loopExit = false;
    // scan through the files and see if there is a .write file in each
    bool WBwrite = false;
    bool SLwrite = false;
    for(uint i = 0; i < fileList.size(); i++){
      if(fileList[i].find(".xml.write") != std::string::npos){
	if(fileList[i].find("SLCorrelIntegrated") != std::string::npos){
	  SLwrite = true;
	} else if(fileList[i].find("WBCorrelIntegrated") != std::string::npos){
	  WBwrite = true;
	}
      }
    }

    while ((jfile < fileList.size()) && !loopExit) {
      // Process current astronomical header file
      std::string currentFile = fileList[jfile];
      try {
	std::vector<std::string> inputFiles;
	inputFiles.push_back(currentFile);
	if((currentFile.find("SLCorrelIntegrated") != std::string::npos &&
	    SLwrite) || (currentFile.find("WBCorrelIntegrated") != std::string::npos &&
			 WBwrite)){
	  // Always re-write if files are to be marked .done
	  bool append = false;
	  processFiles(inputFiles, startFrame, endFrame, corrType,
		       sdpFileName(currentFile), append, pdb);

	  // Rename processed input astronomical header file; do
	  // not rename .write files
	  try {
	    if (currentFile.find(".xml.write") == std::string::npos) {
	      updateReadState("done", currentFile);
	      ndone++;
	    } else {
	      nwrite++;
	    };
	    CARMA_CPTRACE(carma::util::Trace::TRACE6, 
			  "Processed " << currentFile);
	 
	  } catch (const carma::util::ErrorException& ex) {
	    // Astroheader file has been re-opened (.write) or (.busy)
	    // since last dirlist(). Defer processing of this obs block.
	    loopExit = true;
	  };
	}
      } catch (const carma::util::ErrorException& ex) {
	std::ostringstream emsg;
	emsg << "Error processing astro hdr file: " << currentFile;
	logger << log4cpp::Priority::CRIT << emsg.str();
	throw CARMA_EXCEPTION(carma::util::ErrorException, emsg.str());
	/*	emsg << "Error processing astro hdr file: " << currentFile
	     << " it has been moved to the corrupted directory";
	logger << log4cpp::Priority::CRIT << emsg.str();
	std::string::size_type loc;
	loc = currentFile.rfind("/");
	if(loc == std::string::npos){
	    loc = 0;
	}
	else{
	    loc++;
	}
	if(moveCorruptedFile(currentFile.substr(loc)) != 0){
	    std::ostringstream oss;
	    oss << "Could not move " << currentFile 
		<< " to the corrupted directory";
	    logger << log4cpp::Priority::CRIT << oss.str();
	    SDPUtil::mailRTS(oss.str());
	}
	else{
	    std::ostringstream oss;
	    oss << "The following astroheader has been moved to the corrupted directory: " << currentFile;
	    SDPUtil::mailRTS(oss.str());
	    }*/
      };
      jfile++;
    };

    // Sleep if current input file list only contains .write files
    // being filled incrementally
    if ((ndone == 0) && (nwrite > 0)) {
      CARMA_CPTRACE(carma::util::Trace::TRACE7, "Incremental sleep");
      logger << log4cpp::Priority::INFO << "Incremental sleep";
      sleep(sleep_p/2);
    };

    // Clear associative map of files being filled incrementally if
    // no .write files found in current pass
    //if (nwrite == 0) {
    //  lastFrameWritten_p.clear();
    //};

    // Sleep longer if no input files found at all
    if ((ndone + nwrite) == 0) {
      CARMA_CPTRACE(carma::util::Trace::TRACE7, "Sleep");
      logger << log4cpp::Priority::INFO << "Sleep";
      sleep(sleep_p);
    };
  };
  logger << log4cpp::Priority::INFO << "Returning from processAll";
  return;
};

//============================================================================

std::vector<std::string> SDPFiller::dirlist(const std::string& dir)
{
  // Return a directory listing for a specified directory

  // Initialization
  std::vector<std::string> retval;

  // Determine directory prefix used in constructing the full file name
  std::string::size_type pidx = dir.find_last_of("/");
  std::string prefix;
  if ((pidx != std::string::npos) && ((pidx+1) == dir.length())) {
    prefix = dir;
  } else {
    prefix = dir + "/";
  };

  // Read the directory list using scandir
  struct dirent** namelist = 0;
  int n = scandir(dir.c_str(), &namelist, 0, alphasort);
  for (int i=0; i < n; i++) {
    std::string fileName = static_cast<std::string>(namelist[i]->d_name);
    free(namelist[i]);
    std::string fullFileName = prefix + fileName;

    retval.push_back(fullFileName);
  };
  if (namelist) {
    free(namelist);
  };

  return retval;
};

//============================================================================

carma::util::frameType SDPFiller::extractFirstFrameCount(const std::string& 
							 fileName)
{
  // Extract first frame count in an astronomical header file

  // Initialization
  carma::util::frameType retval = 0;

  // Open file
  std::ifstream infile;
  infile.open(fileName.c_str(), std::ios::in);

  // Read first and second lines from the file
  std::string line1, line2;
  std::getline(infile, line1);
  std::getline(infile, line2);
  infile.close();

  // Apply validity checks and extract first frame count
  if (line1.find("<VISDATA") != std::string::npos) {
    if (line2.find("<INTEGRATION") != std::string::npos) {
      std::string::size_type indx = line2.find_first_of("\"");
      std::string::size_type jndx = line2.find_last_of("\"");
      if ((indx != std::string::npos) && (jndx != std::string::npos) &&
	  ((jndx-1) > (indx-1))) {
	long id = atol(line2.substr(indx+1,jndx-indx-1).c_str());
	if (id > 0) {
	  retval = static_cast<carma::util::frameType>(id);
	};
      };
    };
  };
  return retval;
};

//============================================================================

std::vector<std::string> 
SDPFiller::refreshInputFileList(const carma::util::frameType& startFrame, 
				const carma::util::frameType& endFrame,
				bool ignoreDone, bool ignoreWrite)
{
  // Refresh list of input astronomical header files

  // Initialization
  std::vector<std::string> retval;

  // Retrieve list of input files in astro header directories
  std::vector<std::string> dlist =
    dirlist(astroHeaderDir_p+"/SLCorrelIntegrated");
  std::vector<std::string> tempList = 
    dirlist(astroHeaderDir_p+"/WBCorrelIntegrated");
  dlist.insert(dlist.end(), tempList.begin(), tempList.end());

  // Extract start frame count in each file in preparation
  // for an indexed sort in order of increasing frame count.
  std::vector<std::string> fileList;
  std::vector<int> index;
  std::vector<carma::util::frameType> frameCount;
  for (int i=0; i < static_cast<int>(dlist.size()); i++) {
    // Skip unrecognized files
    if ((dlist[i].find("astrohdr_") != std::string::npos) &&
	(dlist[i].find(".xml") != std::string::npos) &&
	(dlist[i].find(".xml.busy") == std::string::npos) &&
	((dlist[i].find(".xml.write") == std::string::npos) || ignoreWrite) &&
	((dlist[i].find(".xml.done") == std::string::npos) || ignoreDone)) {

      // Extract start frame count
      carma::util::frameType fileFrameCount = extractFirstFrameCount(dlist[i]);
      if (fileFrameCount > 0) {
	// Add to list and index
	fileList.push_back(dlist[i]);
	index.push_back(fileList.size()-1);
	frameCount.push_back(fileFrameCount);
      };
    };
  };
  
  // Simple indexed sort in order of increasing start frame count
  for (int i=0; (i+1) < static_cast<int>(frameCount.size()); i++) {
    for (int j=i+1; j < static_cast<int>(frameCount.size()); j++) {
      if (frameCount[i] > frameCount[j]) {
	int swapIndex = index[i];
	index[i] = index[j];
	index[j] = swapIndex;
	carma::util::frameType swapCount = frameCount[i];
	frameCount[i] = frameCount[j];
	frameCount[j] = swapCount;
      };
    };
  };

  // Return astro header file list in frame count order
  int nindex = index.size();
  for (int i=0; i < nindex; i++) {
    // Exclude files with frame counts out of range
    bool exclude = (frameCount[index[i]] > endFrame);
    if (!exclude) {
      retval.push_back(fileList[index[i]]);
    };
  };

  return retval;
};

//============================================================================
std::string SDPFiller::sdpFileName(const std::string& astroHdrFile)
{
  // Compose the output SDP file name associated with an input astro header

  // Initialization
  std::string retval;

  // Construct the sdp output file name
  retval = scienceDataFormatDir_p + "/" + extractObsBlockId(astroHdrFile)
    + ".mir";
  return retval;
};

//============================================================================
std::string SDPFiller::extractObsBlockId(const std::string& astroHdrFile)
{
  // Extract the observing block id. from an astro header file name

  // Initialization
  std::string retval;

  // Search for the obs block id. markers
  std::string::size_type aidx = astroHdrFile.find("astrohdr_");
  std::string::size_type xidx = astroHdrFile.find(".xml");

  // Extract obs block id.
  if ((aidx != std::string::npos) && (xidx != std::string::npos) &&
      (xidx > (aidx+1))) {
    retval = astroHdrFile.substr(aidx+9, xidx-aidx-9);
  };
  return retval;
};

//============================================================================
void SDPFiller::deleteSDPFile(const std::string& outputSDPFile)
{
  // Delete an existing sdp output file.

  boost::filesystem::path mir_folder(outputSDPFile.c_str());
  boost::filesystem::remove_all( mir_folder );
  // Delete all components of the file
  
  return;
};
  
//============================================================================
void SDPFiller::updateReadState(const std::string& state, 
				std::string& astroHdrFile)
{
  // Update read status of an input astro header file

  // Initialization
  log4cpp::Category& logger = Program::getLogger();

  // Compose new name for output file
  std::string::size_type xidx = astroHdrFile.find(".xml");
  std::string baseName = astroHdrFile.substr(0, xidx+4);
  std::string newName = baseName + "." + state;

  // Rename file
  int result = rename(astroHdrFile.c_str(), newName.c_str());
  if (result != 0) {
    std::ostringstream emsg;
    emsg << "Error changing read status of astro header file: " 
	 << astroHdrFile;
    logger << log4cpp::Priority::CRIT << emsg.str();
    throw CARMA_EXCEPTION(carma::util::ErrorException, emsg.str());
  } else {
    astroHdrFile = newName;
  };
  return;
};

//============================================================================
int SDPFiller::moveCorruptedFile(const std::string& corruptedFile){
    std::string cor = astroHeaderDir_p + "/corrupted/" + corruptedFile;
    std::string hdr = astroHeaderDir_p + "/SLCorrelIntegrated/" + corruptedFile;
    return rename(hdr.c_str(),cor.c_str());
};
//============================================================================
} // namespace sdp
} // namespace carma
//============================================================================

