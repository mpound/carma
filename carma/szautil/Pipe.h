#ifndef SZA_UTIL_PIPE_H
#define SZA_UTIL_PIPE_H

/**
 * @file Pipe.h
 * 
 * Tagged: Fri Nov 14 12:39:34 UTC 2003
 * 
 * @author Erik Leitch
 */
#ifdef PIPE_WAIT
#undef PIPE_WAIT
#endif

#ifdef PIPE_NOWAIT
#undef PIPE_NOWAIT
#endif

/*
 * Enumerate the two special I/O timeout values.
 */

/* Wait to complete the transaction */

#define PIPE_WAIT  -1 

/* Return immediately if the transaction would block */

#define PIPE_NOWAIT 0

/*
 * Enumerate the return statuses of read_pipe() and write_pipe().
 */
enum  PipeState {
  PIPE_OK,      /* The I/O completed successfully */
  PIPE_BUSY,    /* The I/O couldn't be performed without blocking */
  PIPE_ERROR    /* An error occurred */
};

#include <pthread.h> // Needed for POSIX thread function calls
#include <limits.h>  // PIPE_BUF 

namespace sza {
  namespace util {
    
    /**
     * A class to encapsulate a pipe
     */
    class Pipe {
      
    public:
    
      /**
       * Constructor.
       *
       * @throws Exception
       */
      Pipe();
      
      /**
       * Destructor.
       *
       * @throws Exception
       */
      virtual ~Pipe();
      
      /**
       * Read from the pipe.
       *
       * @throws Exception
       */
      virtual void readPipe(void *buffer, size_t nbyte, long timeout);
      
      /**
       * Write to the pipe.
       *
       * @throws Exception
       */
      virtual void writePipe(void *buffer, size_t nbyte, long timeout);

      /**
       * Read from the pipe.
       *
       * @throws Exception
       */
      virtual PipeState read(void *buffer, size_t nbyte, long timeout=PIPE_NOWAIT);
      
      /**
       * Write to the pipe.
       *
       * @throws Exception
       */
      virtual PipeState write(void *buffer, size_t nbyte, long timeout=PIPE_NOWAIT);
      
      /**
       * Return the file descriptor associated with this pipe.
       */
      int fd();
      
      /**
       * Return an intialized set of readable file descriptors
       * associated with this queue.
       */
      fd_set rfds();
      
      int readFd() {return readfd_.fd_;};

      int writeFd() {return writefd_.fd_;};

    protected:
      
      /**
       * A mutex guard for the pipe.
       */
      pthread_mutex_t guard_;
      
      /**
       * True when the guard mutex has been initialized.
       */
      bool guardIsReady_;
      
      /**
       * Define a struct to encapsulate a file descriptor associated
       * with either end of a pipe
       */
      struct PipeFd {
	
	/**
	 * Constructor.
	 */
	PipeFd();
	
	/**
	 * Destructor.
	 */
	~PipeFd();
	
	/**
	 * A condition variable which can be used for other threads
	 * to signal when the fd is readable or writable.
	 */
	pthread_cond_t retry_; 
	
	/**
	 * True when retry has been initialized.
	 */
	bool retryIsReady_;
	
	/**
	 * The file descriptor
	 */
	int fd_; 
	
	/**
	 * Initialize the pipe fds.
	 *
	 * @throws Exception
	 */
	void fillPipeFd();
      };
      
      /**
       * File descriptor corresponding to the read end of the pipe.
       */
      PipeFd readfd_;
      
      /**
       * File descriptor corresponding to the write end of the pipe.
       */
      PipeFd writefd_;
      
      /*
       * One can write up to PIPE_BUF bytes atomically, but there is no such
       * guarantee for read(). The following buffer allows this guarantee to
       * be extended to reads. It accumulates data sequentially from one
       * or more incomplete reads, and hands out request-sized chunks to
       * any readers that request <= nread bytes, on a first come first
       * served basis.
       */
      char unread_[PIPE_BUF];  
      
      /**
       * The number of bytes in buffer[]
       */
      size_t nread_;            
      
      /**
       * Get the current time of day.  Used by read/writePipe() for
       * timeouts.
       *
       * @throws Exception
       */
      void getTimeOfDay(struct timespec* ts);
      
    }; // End class Pipe
    
  }; // End namespace util
}; // End namespace sza

#endif // PIPE_H







