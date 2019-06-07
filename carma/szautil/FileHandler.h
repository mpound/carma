// $Id: FileHandler.h,v 1.1 2010/12/13 21:06:30 eml Exp $

#ifndef SZA_UTIL_FILEHANDLER_H
#define SZA_UTIL_FILEHANDLER_H

/**
 * @file FileHandler.h
 * 
 * Tagged: Thu Jan 29 22:24:18 NZDT 2009
 * 
 * @version: $Revision: 1.1 $, $Date: 2010/12/13 21:06:30 $
 * 
 * @author username: Command not found.
 */

#include <iostream>
#include <unistd.h>
#include <vector>

namespace sza {
  namespace util {

    class FileHandler {
    public:

      /**
       * Constructor.
       */
      FileHandler();
      FileHandler(std::string path);

      /**
       * Copy Constructor.
       */
      FileHandler(const FileHandler& objToBeCopied);

      /**
       * Copy Constructor.
       */
      FileHandler(FileHandler& objToBeCopied);

      /**
       * Const Assignment Operator.
       */
      void operator=(const FileHandler& objToBeAssigned);

      /**
       * Assignment Operator.
       */
      void operator=(FileHandler& objToBeAssigned);

      /**
       * Output Operator.
       */
      friend std::ostream& operator<<(std::ostream& os, FileHandler& obj);

      /**
       * Destructor.
       */
      virtual ~FileHandler();
      
      void setTo(std::string path);
      virtual void openForRead(bool memMap=false);
      void close();
      void advanceByNbytes(off_t bytes);
      void setToBeginning();
      off_t setToEnd();
      unsigned getFileSizeInBytes();

      int getFd();

      off_t getCurrentOffset();
      void read(void* buf, size_t nByte);

      void memoryMap();
      void loadFile();

    protected:

      std::string path_;
      bool pathIsSet_;

      int fd_;

      off_t currentOffset_;
      size_t sizeInBytes_;

      //------------------------------------------------------------
      // If the file is memory-mapped, memMap_ will be set to true,
      // and mptrHead_ will point to the head of the memory mapped space
      //------------------------------------------------------------

      bool memMap_;
      unsigned char* mptrHead_;

      bool loadFile_;
      std::vector<unsigned char> fbuf_;

      void checkFd();

    }; // End class FileHandler

  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_FILEHANDLER_H
