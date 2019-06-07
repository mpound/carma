#include "carma/antenna/sza/antenna/corba/CarmaFileHandler.h"

#include "carma/szautil/Exception.h"
#include "carma/szautil/Sort.h"
#include "carma/szautil/String.h"

#include <fstream>

using namespace std;
using namespace sza::antenna::corba;
using namespace sza::util;

/**.......................................................................
 * Constructor.
 */
CarmaFileHandler::CarmaFileHandler(std::string dir) 
{
  dir_ = dir;
}

void CarmaFileHandler::generateFileList()
{
  getFileList(dir_);
  loadCompletedFiles();
  mergeFileLists();
}

/**.......................................................................
 * Destructor.
 */
CarmaFileHandler::~CarmaFileHandler() {}

void CarmaFileHandler::markCurrentFileAsComplete()
{
  if(iFile_ >= 0 && iFile_ < nFile_) {
    completedFiles_.push_back(fileNames_[iFile_]);
    ++iFile_;
  }
}

std::string CarmaFileHandler::getNextFile()
{
  return fileNames_[iFile_];
}

bool CarmaFileHandler::atEnd()
{
  // Don't convert the last (most recent file) because it is still
  // being written by the control system

  return iFile_ == nFile_- 1;
}

void CarmaFileHandler::loadCompletedFiles()
{
  ifstream inFile(completedFileList_.c_str());
  
  // If the file doesn't exist, just return

  if (!inFile) {
    return;
  }

  String str;
  std::string s;
  while(getline(inFile, s)) {
    completedFiles_.push_back(s);
  }

  inFile.close();
}

void CarmaFileHandler::writeCompletedFiles()
{
  ofstream outFile(completedFileList_.c_str());

  if (!outFile) {
    ThrowError("Can't open input file " << completedFileList_);
  }

  for(unsigned i=0; i < completedFiles_.size(); i++) {
    outFile << completedFiles_[i] << std::endl;
  }

  outFile.close();
}

void CarmaFileHandler::mergeFileLists()
{
  // Purge any files in the completed list that are older than the
  // first file in the new listing

  String parser(fileNames_[0]);
  unsigned iFrameOldestNew = parser.findNextInstanceOf("_", true, " ", false).toInt();
  
  //  First purge any files from the new file list that are older than
  //  the last file in the completed list

  std::vector<std::string> tmpFileList;

  if(completedFiles_.size() > 0) {
    parser = completedFiles_[completedFiles_.size()-1];
    COUT("parser = " << parser);
    unsigned iFrameNewestOld = parser.findNextInstanceOf("_", true, " ", false).toInt();

    COUT("Most recent completed frame is: " << iFrameNewestOld);

    for(unsigned i=0; i < fileNames_.size(); i++) {
      parser = fileNames_[i];
      unsigned iFrameNew = parser.findNextInstanceOf("_", true, " ", false).toInt();
      
      // Only keep files that are newer than the last file we already
      // converted

      if(iFrameNew > iFrameNewestOld) {
	tmpFileList.push_back(fileNames_[i]);
      }

    }
    
    fileNames_ = tmpFileList;
  }

  COUT("File list is now: " << fileNames_.size() << " long: first = " << fileNames_[0]);

  // Now purge files from the completed list that are older than the
  // oldest NEW file in our original list.  These have already been
  // deleted from disk and don't need to be kept track of.

  tmpFileList.resize(0);

  for(unsigned i=0; i < completedFiles_.size(); i++) {
    parser = completedFiles_[i];
    unsigned iFrameOld = parser.findNextInstanceOf("_", true, " ", false).toInt();

    // Only keep files that are as new as the first file that was in
    // our original list of new files to be converted

    if(iFrameOld >= iFrameOldestNew)
      tmpFileList.push_back(completedFiles_[i]);
  }

  completedFiles_ = tmpFileList;


  // And reset the current file index and number of files

  iFile_ = 0;
  nFile_ = fileNames_.size();

  COUT("File list is now: " << nFile_ << " long");
}

void  CarmaFileHandler::getFileList(std::string dir)
{
  COUT("Generating file list from directory: " << dir);
  
  sza::util::DirList dirList(dir, false);
  std::list<DirList::DirEnt> files = dirList.getFiles();

  for(std::list<DirList::DirEnt>::iterator iEnt = files.begin(); iEnt != files.end(); iEnt++) {
    String str(iEnt->fullName());
    if(isFileType(str)) {
      fileNames_.push_back(iEnt->fullName());
    }
  }

  fileNames_ = Sort::sortNumeric(fileNames_);

  iFile_ = 0;
  nFile_ = fileNames_.size();

  COUT("Found: " << nFile_ << " files: first = " << fileNames_[0]);
  COUT("Found: " << nFile_ << " files: next  = " << fileNames_[1]);
  COUT("Found: " << nFile_ << " files: mext  = " << fileNames_[2]);
}

void CarmaFileHandler::setCompletedFileList(std::string fileList)
{
  completedFileList_ = fileList;
}

/**.......................................................................
 * Write the contents of this object to an ostream
 */
ostream& 
sza::antenna::corba::operator<<(ostream& os, CarmaFileHandler& handler)
{
  for(unsigned i=0; i < handler.fileNames_.size(); i++) {
    os << handler.fileNames_[i] << std::endl;
  }

  return os;
}

 bool CarmaFileHandler::isFileType(sza::util::String& str) 
 {
   return true;
 }
