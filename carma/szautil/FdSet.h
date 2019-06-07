#ifndef FDSET_H
#define FDSET_H

/**
 * @file FdSet.h
 * 
 * Started: Thu Feb 26 22:08:23 UTC 2004
 * 
 * @author Erik Leitch
 */
#include <sys/select.h>

namespace sza {
  namespace util {
    
    class FdSet {
    public:
      
      /**
       * Constructor.
       */
      FdSet();
      
      /**
       * Destructor.
       */
      virtual ~FdSet();
      
      /**
       * Private method to zero the set of descriptors to be watched
       * for readability.
       */
      void zeroReadFdSet();
      
      /**
       * Private method to zero the set of descriptors to be watched
       * for writability.
       */
      void zeroWriteFdSet();
      
      /**
       * Private method to register a file descriptor to be
       * watched for input.
       */
      void registerReadFd(int fd);
      
      /**
       * Private method to register a file descriptor to be
       * watched for output availability.
       */
      void registerWriteFd(int fd);
      
      /**
       * Remove a file descriptor from the mask of descriptors to
       * be watched for readability.
       */
      void clearFromReadFdSet(int fd);
      
      /**
       * Remove a file descriptor from the mask of descriptors to
       * be watched for writeability.
       */
      void clearFromWriteFdSet(int fd);	  
      
      /**
       * Return a pointer to the set of read file descriptors
       */
      fd_set* readFdSet();
      
      /**
       * Return a pointer to the set of write file descriptors
       */
      fd_set* writeFdSet();
      
      /**
       * Return the size of the largest file descriptor in the set.
       */
      int size();
      
      /**
       * Return true if the file descriptor is set in the read set.
       */
      bool isSetInRead(int fd);
      
      /**
       * Return true if the file descriptor is set in the write set.
       */
      bool isSetInWrite(int fd);
      
      /**
       * Clear the read and write fd sets
       */
      void clear();

      /**
       * Clear an fd from both the read and write fd sets
       */
      void clear(int fd);

      /**
       * Debugging method
       */
      void print();

    private:
      
      /**
       * The set of fds from which to listen for input
       */
      fd_set readFdSet_;    

      /**
       * We will store a copy of the fds to be watched for
       * readability, since select() will modify the above set.
       */
      fd_set readFdSetSave_;    
      
      /**
       * The set of fds to watch for writability
       */
      fd_set writeFdSet_;   

      /**
       * We will store a copy of the fds to be watched for
       * writability, since select() will modify the above set.
       */
      fd_set writeFdSetSave_;   
      
      /**
       * The max fd + 1 in readFdSet
       */
      int fdSetSize_;    
      
    }; // End class Fd
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 


