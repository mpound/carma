// $Id: CarmaFileHandler.h,v 1.2 2010/12/13 20:52:26 eml Exp $

#ifndef SZA_ANTENNA_CORBA_CARMAFILEHANDLER_H
#define SZA_ANTENNA_CORBA_CARMAFILEHANDLER_H

/**
 * @file CarmaFileHandler.h
 * 
 * Tagged: Tue May  4 12:02:55 PDT 2010
 * 
 * @version: $Revision: 1.2 $, $Date: 2010/12/13 20:52:26 $
 * 
 * @author username: Command not found.
 */
#include "carma/szautil/DirList.h"
#include "carma/szautil/String.h"

#include <string>
#include <vector>

namespace sza {
  namespace antenna {
    namespace corba {

      class CarmaFileHandler {
      public:

	/**
	 * Constructor.
	 */
	CarmaFileHandler(std::string dir);

	/**
	 * Destructor.
	 */
	virtual ~CarmaFileHandler();

	// Return true if we are at the end of the file list

	bool atEnd();

	std::string getNextFile();
	void markCurrentFileAsComplete();

	void getFileList(std::string dir);
	void generateFileList();

	// Load the list of completed files from disk

	void loadCompletedFiles();

	// Write the list of completed files back to disk

	void writeCompletedFiles();

	// Purge files in the "completed" list that are older than the
	// first file in our list

	void mergeFileLists();

	void setCompletedFileList(std::string fileList);

	virtual bool isFileType(sza::util::String& fileName);

	friend std::ostream& operator<<(std::ostream& os, CarmaFileHandler& handler);

      protected:

	std::string dir_;
	unsigned nFile_;
	int iFile_;

	std::vector<std::string> fileNames_;

	// A list of completed files

	std::vector<std::string> completedFiles_;
	std::vector<std::string> lastCompletedFiles_;

	// The name of a file containing the list of completed files

	std::string completedFileList_;

      }; // End class CarmaFileHandler

      std::ostream& operator<<(std::ostream& os, CarmaFileHandler& handler);

    } // End namespace corba
  } // End namespace antenna
} // End namespace sza



#endif // End #ifndef SZA_ANTENNA_CORBA_CARMAFILEHANDLER_H
