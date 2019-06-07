// $Id: ArchiveFileHandler.h,v 1.2 2011/11/01 23:04:24 eml Exp $

#ifndef SZA_UTIL_ARCHIVEFILEHANDLER_H
#define SZA_UTIL_ARCHIVEFILEHANDLER_H

/**
 * @file ArchiveFileHandler.h
 * 
 * Tagged: Thu Jan 29 23:10:35 NZDT 2009
 * 
 * @version: $Revision: 1.2 $, $Date: 2011/11/01 23:04:24 $
 * 
 * @author username: Command not found.
 */

#include "carma/szautil/FileHandler.h"

#include "carma/szaarrayutils/archive.h"
#include "carma/szaarrayutils/arraymap.h"
#include "carma/szaarrayutils/monitor_stream.h"
#include "carma/szaarrayutils/netbuf.h"
#include "carma/szaarrayutils/netobj.h"

#include <iostream>

namespace sza {
  namespace util {

    class ArchiveFileHandler : public FileHandler {
    public:

      /**
       * Constructor.
       */
      ArchiveFileHandler();

      /**
       * Copy Constructor.
       */
      ArchiveFileHandler(const ArchiveFileHandler& objToBeCopied);

      /**
       * Copy Constructor.
       */
      ArchiveFileHandler(ArchiveFileHandler& objToBeCopied);

      /**
       * Const Assignment Operator.
       */
      void operator=(const ArchiveFileHandler& objToBeAssigned);

      /**
       * Assignment Operator.
       */
      void operator=(ArchiveFileHandler& objToBeAssigned);

      /**
       * Output Operator.
       */
      friend std::ostream& operator<<(std::ostream& os, ArchiveFileHandler& obj);

      /**
       * Destructor.
       */
      virtual ~ArchiveFileHandler();

      // Open the file for reading.  If memMap = true, the file will
      // be memory-mapped

      void openForRead(bool memMap=false);

      ArrayMap* getArrayMap() {
	return arrayMap_;
      }

    private:

      ArrayMap* arrayMap_;            // The register map of the archive file
      sza::array::NetReadStr* nrs_;   // The file input stream 
      int nrsSize_;                   // The size of the input stream buffer
      int lastArrayMapSize_;          // The last size read
      
      unsigned nFramesInFile_;

      // The number of header bytes in each msg in the file

      unsigned nBytesInMsgHeader_;

      // The total number of bytes in the head of the file (before the
      // register frames begin)

      unsigned nBytesInFileHeader_;

      // The total number of bytes in each frame in the file.  Note
      // that this is larger than the number of bytes in the array map
      // by nBytesInMsgHeader_

      unsigned nBytesInFrameMsg_;

      // The offset in bytes of the register array.frame.utc from the
      // head of the array map

      unsigned offsetInBytesOfFrameUtcInArrayMap_;

      // Initialize the net read stream

      void initializeNetReadStr();

      // Get the next message from the file

      sza::array::MsReadState readNextMsg();

      // Read the arraymap from the file

      void readArrayMap();

      // Get the maximum buffer size from the file

      bool readSize();

    public:

      void readTimestamps();

      void advanceToFrame(unsigned iFrame, off_t offset=0);

      // Return the MJD of the specified frame

      double getMjd(unsigned iFrame);

      // Binary searches for the first frame in the file before/after the
      // specified date

      unsigned findFirstFrameBefore(std::string date);
      unsigned findFirstFrameAfter(std::string date);

      // Return the total number of frames in this file

      unsigned nFrame();

    }; // End class ArchiveFileHandler

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_ARCHIVEFILEHANDLER_H
