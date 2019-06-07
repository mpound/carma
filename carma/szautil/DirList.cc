#include "carma/szautil/DirList.h"
#include "carma/szautil/Exception.h"

#include <string>
#include <sstream>

using namespace std;
using namespace sza::util;

#include <sys/stat.h>
#include <sys/types.h>

#include <unistd.h>
#include <dirent.h>

/**.......................................................................
 *  Get a listing of the specified directory, and optionally descend
 *  into subdirectories
 */
DirList::DirList(std::string path, bool descend)
{
  findEntries(entries_, path, descend);
}

/**.......................................................................
 * Const Copy Constructor.
 */
DirList::DirList(const DirList& objToBeCopied)
{
  *this = (DirList&)objToBeCopied;
};

/**.......................................................................
 * Copy Constructor.
 */
DirList::DirList(DirList& objToBeCopied)
{
  *this = objToBeCopied;
};

/**.......................................................................
 * Const Assignment Operator.
 */
void DirList::operator=(const DirList& objToBeAssigned)
{
  *this = (DirList&)objToBeAssigned;
};

/**.......................................................................
 * Assignment Operator.
 */
void DirList::operator=(DirList& objToBeAssigned)
{
  std::cout << "Calling default assignment operator for class: DirList" << std::endl;
};

/**.......................................................................
 * Output Operator.
 */
std::ostream& sza::util::operator<<(std::ostream& os, DirList& obj)
{
  os << "Default output operator for class: DirList" << std::endl;
  return os;
};

/**.......................................................................
 * Destructor.
 */
DirList::~DirList() {}

void DirList::findEntries(std::list<DirEnt>& entries_, std::string path, 
			  bool descend)
{
  DIR* dir = 0;
  dir = opendir(path.c_str());

  if(!dir) {
    ThrowSysError("Unable to open: " << path);
    return;
  }

  struct dirent* dp=0;

  // Now iterate through entries

  std::ostringstream os;
  while((dp=readdir(dir)) != 0) {

    os.str("");
    os << path << "/" << dp->d_name;

    getRights(os.str());

    entries_.push_back(DirEnt(dp->d_name, path, 
			      getType(os.str()), getRights(os.str())));
    
    DirEnt& entry = entries_.back();
  }
  
  // Cleanup
  
  if(dir) {
    if(closedir(dir) < 0) {
      ThrowSysError("closedir()");
    }
  }

  // Now do a second pass to see if any of the entries were
  // directories themselves.  We do this after the call to closedir(),
  // or else the call will eventually fail with "too many open files"
  // error.

  if(descend) {
    
    std::list<DirEnt>::iterator entry;

    for(entry = entries_.begin(); entry != entries_.end(); entry++) {

      // If this was a directory, descend into it too
      
      if(entry->isDir() && entry->isReadable() && entry->name_ != "."
	 && entry->name_ != "..") {

	os.str("");
	os << path << "/" << entry->name_;
	findEntries(entry->entries_, os.str(), descend);
      }
    }
  }
}

/**.......................................................................
 * Check whether a pathname refers to a file with selected attributes.
 */
bool DirList::testPathname(std::string path, EntryType type, unsigned rights)
{
  int mode;   /* The access mode to be tested with access() */
  
  // Do we need to test the file type?

  if(type != TYPE_ANY) {
    
    // Look up the file attributes.

    struct stat statbuf;    /* The file-statistics return buffer */

    if(lstat(path.c_str(), &statbuf) < 0) {
      ThrowSysError("lstat()");
    }
    
    // See if the file attributes match the requested file type.

    switch(type) {
    case TYPE_PIPE:    /* Test whether the path refers to a named pipe */
      if(!S_ISFIFO(statbuf.st_mode))
	return false;
      break;
    case TYPE_DIR:     /* Test whether the path refers to a directory */
      if(!S_ISDIR(statbuf.st_mode))
	return false;
      break;
    case TYPE_FILE:     /* Test whether the path refers to an ordinary file */
      if(!S_ISREG(statbuf.st_mode))
	return false;
      break;
    default:
      ThrowError("Unknown entry type requested");
    };
  };
  
  // Convert the specified set of access rights into the equivalent
  // that is used by the access() system call.

  mode = 0;
  if(rights & ENTRY_READ)
    mode |= R_OK;
  if(rights & ENTRY_WRITE)
    mode |= W_OK;
  if(rights & ENTRY_EXE)
    mode |= X_OK;
  if(rights & ENTRY_OK)
    mode |= F_OK;
  
  // Test the accessibility of the file.

  if(access(path.c_str(), mode) < 0) {
    ReportSysError("access(), with: entry = " << path);
    return false;
  }

  return true;
}

/**.......................................................................
 * Check whether a pathname refers to a file with selected attributes.
 */
DirList::EntryType DirList::getType(std::string path, bool link)
{
  unsigned type = 0;

  // Look up the file attributes.
  
  struct stat statbuf;    /* The file-statistics return buffer */
  
  if(link) {
    if(stat(path.c_str(), &statbuf) < 0) {
      ThrowSysError("lstat()");
    }
  } else {
    if(lstat(path.c_str(), &statbuf) < 0) {
      ThrowSysError("lstat()");
    }
  }
  
  // See if the file attributes match the requested file type.
  
  if(S_ISFIFO(statbuf.st_mode)) {
    type = TYPE_PIPE;
  } else if(S_ISDIR(statbuf.st_mode)) {
    type = TYPE_DIR;
  } else if(S_ISREG(statbuf.st_mode)) {
    type = TYPE_FILE;
  } else if(S_ISLNK(statbuf.st_mode)) {
    type = TYPE_LINK | getType(path, true);
  } else {
    ThrowError("Unknown entry type");
  }

  return (EntryType)type;
}

/**.......................................................................
 * Get the access rights to this entry
 */
DirList::EntryRights DirList::getRights(std::string path)
{
  unsigned rights = 0;
  int status=0;

  status=0;
  if(access(path.c_str(), R_OK)==0) {
    rights |= ENTRY_READ;
  }

  status=0;
  if(access(path.c_str(), W_OK)==0) {
    rights |= ENTRY_WRITE;
  }

  status=0;
  if(access(path.c_str(), X_OK)==0) {
    rights |= ENTRY_EXE;
  }

  status=0;
  if(access(path.c_str(), F_OK)==0) {
    rights |= ENTRY_OK;
  }

  return (EntryRights) rights;
}

/**.......................................................................
 *  Get a listing of the specified directory, and optionally descend
 *  into subdirectories
 */
void DirList::listEntries() 
{
  listEntries(entries_);
}

/**.......................................................................
 *  Get a listing of the specified directory, and optionally descend
 *  into subdirectories
 */
void DirList::listEntries(std::list<DirEnt>& entries)
{
  std::list<DirEnt>::iterator entry;

  for(entry = entries.begin(); entry != entries.end(); entry++) {

    COUT(*entry);

    if(entry->type_ == TYPE_DIR) {
      listEntries(entry->entries_);
    }
  }
}

std::ostream& sza::util::operator<<(std::ostream& os, DirList::DirEnt& entry)
{
  os << entry.path_ << "/" << entry.name_ << " ";

  unsigned type = (unsigned)entry.type_;

  if(type & DirList::TYPE_PIPE) {
    os << "(pipe)";
  } else if(type & DirList::TYPE_DIR) {
    os << "(dir)";
  } else if(type & DirList::TYPE_FILE) {
    os << "(file)";
  } else {
    os << "(unknown)";
  }

  if(type & DirList::TYPE_LINK) 
    os << "(link)";

  os << " ";

  unsigned rights = (unsigned)entry.rights_;

  if(rights & DirList::ENTRY_READ)
    os << "R";
  if(rights & DirList::ENTRY_WRITE)
    os << "W";
  if(rights & DirList::ENTRY_EXE)
    os << "X";
  if(rights & DirList::ENTRY_OK)
    os << "OK";
  
  return os;
}

std::list<DirList::DirEnt> DirList::getFiles(bool includeSymlinks)
{
  std::list<DirEnt> files;

  getFiles(files, entries_, includeSymlinks);

  return files;
}

void DirList::getFiles(std::list<DirEnt>& files, 
		       std::list<DirEnt>& entries, bool includeSymlinks)
{
  std::list<DirEnt>::iterator entry;

  for(entry=entries.begin(); entry != entries.end(); entry++) {

    if(entry->isFile()) {
      if(!entry->isLink() || includeSymlinks) {
	files.push_back(*entry);

	// We only want to copy the entry itself here, not any
	// descendants

	files.back().entries_.clear();
      }
    }

    getFiles(files, entry->entries_, includeSymlinks);

  }
}

std::list<DirList::DirEnt> DirList::getDirs(bool includeSymlinks)
{
  std::list<DirEnt> dirs;

  getDirs(dirs, entries_, includeSymlinks);

  return dirs;
}

void DirList::getDirs(std::list<DirEnt>& dirs, 
		      std::list<DirEnt>& entries, bool includeSymlinks)
{
  std::list<DirEnt>::iterator entry;

  for(entry=entries.begin(); entry != entries.end(); entry++) {

    if(entry->isDir()) {

      if(!entry->isLink() || includeSymlinks) {
	dirs.push_back(*entry);

	// We only want to copy the entry itself here, not any
	// descendants

	dirs.back().entries_.clear();
      }
    }

    getDirs(dirs, entry->entries_, includeSymlinks);

  }
}

