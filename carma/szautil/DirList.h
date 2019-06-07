// $Id: DirList.h,v 1.1 2010/12/13 21:06:30 eml Exp $

#ifndef SZA_UTIL_DIRLIST_H
#define SZA_UTIL_DIRLIST_H

/**
 * @file DirList.h
 * 
 * Tagged: Thu Feb  7 01:42:41 NZDT 2008
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:30 $
 * 
 * @author username: Command not found.
 */

#include <iostream>
#include <sstream>
#include <list>

namespace sza {
  namespace util {

    class DirList {
    public:

      enum EntryType {
	TYPE_FILE  = 1,
	TYPE_DIR   = 2,
	TYPE_PIPE  = 4,
	TYPE_LINK  = 8,
	TYPE_ANY  = TYPE_FILE | TYPE_DIR | TYPE_PIPE,
      };

      // Enumerate an orthogonal set of file permissions that can be
      // tested for by testPathname()
      
      enum EntryRights {
	ENTRY_NONE  = 0,
	ENTRY_READ  = 1,  /* Test if the file is readable */
	ENTRY_WRITE = 2,  /* Test if the file is writable */
	ENTRY_EXE   = 4,  /* Test if the file is executable */
	ENTRY_OK    = 8   /* Test if the file exists, regardless of permissions */
      };

      struct DirEnt {
	volatile EntryType   type_;   // Type of this entry
	volatile EntryRights rights_; // Access of this entry
	std::string name_;   // Name of this entry
	std::string path_;   // Path to this entry, relative to the top directory

	// If this entry is a directory, this list will contain all
	// nodes below it

	std::list<DirEnt> entries_; 

	DirEnt(std::string name, std::string path, EntryType type, 
	       EntryRights rights) {
	  name_     = name;
	  path_     = path;
	  type_     = type;
	  rights_   = rights;
	}

	bool isDir() {
	  return (unsigned)(type_) & TYPE_DIR;
	}

	bool isLink() {
	  return (unsigned)(type_) & TYPE_LINK;
	}

	bool isFile() {
	  return (unsigned)(type_) & TYPE_FILE;
	}

	bool isReadable() {
	  return (unsigned)(rights_) & ENTRY_READ;
	}

	std::string pathName() {
	  std::ostringstream os;
	  os << path_ << "/";
	  return os.str();
	}

	std::string fullName() {
	  std::ostringstream os;
	  os << path_ << "/" << name_;
	  return os.str();
	}
      };

      friend std::ostream& operator<<(std::ostream& os, DirEnt& entry);

      /**
       * Constructor.
       */
      DirList(std::string path, bool descend);

      /**
       * Copy Constructor.
       */
      DirList(const DirList& objToBeCopied);

      /**
       * Copy Constructor.
       */
      DirList(DirList& objToBeCopied);

      /**
       * Const Assignment Operator.
       */
      void operator=(const DirList& objToBeAssigned);

      /**
       * Assignment Operator.
       */
      void operator=(DirList& objToBeAssigned);

      /**
       * Output Operator.
       */
      friend std::ostream& operator<<(std::ostream& os, DirList& obj);

      /**
       * Destructor.
       */
      virtual ~DirList();

      //  Get a listing of the specified directory, and optionally
      //  descend into subdirectories

      void listEntries();

      std::list<DirEnt> getFiles(bool includeSymlinks=false);
      std::list<DirEnt> getDirs(bool includeSymlinks=false);

      void listEntries(std::list<DirEnt>& entries);

    private:

      // The list of entries in this directory

      std::list<DirEnt> entries_; 

      void findEntries(std::list<DirEnt>& entries_, std::string path,
				bool descend);

      // Check whether a pathname refers to a file with selected
      // attributes.

      bool testPathname(std::string path, EntryType type, unsigned rights);

      // Check whether a pathname refers to a file with selected
      // attributes.

      EntryType getType(std::string path, bool link=false);

      // Get the access rights to this entry

      DirList::EntryRights getRights(std::string path);

      void getFiles(std::list<DirEnt>& files, 
		    std::list<DirEnt>& entries, bool includeSymlinks);
      
      void getDirs(std::list<DirEnt>& dirs, 
		   std::list<DirEnt>& entries, bool includeSymlinks);

    }; // End class DirList

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_DIRLIST_H
