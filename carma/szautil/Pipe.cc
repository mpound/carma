#include <iostream>
#include <sstream>

#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <time.h>
#include <sys/time.h> // gettimeofday() 
#include <signal.h>

#include "carma/szautil/Debug.h"
#include "carma/szautil/Exception.h"
#include "carma/szautil/LogStream.h"
#include "carma/szautil/Pipe.h"
#include "carma/szautil/Thread.h"

using namespace sza::util;
using namespace std;

/**.......................................................................
 * PipeFd constructor
 */
Pipe::PipeFd::PipeFd()
{
  retryIsReady_ = false;
  fd_           = -1;
}

/**.......................................................................
 * PipeFd destructor function
 */
Pipe::PipeFd::~PipeFd()
{

 if(retryIsReady_) {
    pthread_cond_destroy(&retry_);
    retryIsReady_ = false;
  };

  if(fd_ >= 0) {
    close(fd_);
    fd_ = -1;
  };
}

/**.......................................................................
 * Pipe constructor function
 */
Pipe::Pipe()
{
  bool waserr=false;
  int fds[2];

  guardIsReady_ = false;
  unread_[0]    = '\0';
  nread_        = 0;

  // Allocate the guard mutex.

  int status = pthread_mutex_init(&guard_, NULL);

  DBPRINT(true, Debug::DEBUG10, "Thread (" << pthread_self() 
	  << ") is owner of pipe guard: " << &guard_);

  if(status) {
    ostringstream os;
    os << "Pipe::Pipe(): Error in pthread_mutex_init: " 
       << strerror(status) << ends;
    throw Error(os.str());
  };

  guardIsReady_ = true;

  // Create the pipe.

  if(pipe(fds)) {

    ostringstream os;
    os << "Pipe::Pipe: Error in pipe(): " 
       << strerror(errno) << ends;
    throw Error(os.str());
  };

  // Record the file descriptors of the two ends, so that henceforth
  // calls to del_PipeFd() will cause them to be closed.

  readfd_.fd_  = fds[0];
  writefd_.fd_ = fds[1];

  readfd_.fillPipeFd();
  writefd_.fillPipeFd();
}

/**.......................................................................
 * Set up the contents of a PipeFd
 */
void Pipe::PipeFd::fillPipeFd()
{
  // Allocate the retry-I/O condition variable.

  int status = pthread_cond_init(&retry_, NULL);

  if(status) {
    ostringstream os;
    os << "Pipe::PipeFd::fillPipeFd(): Error in pthread_cond_init: " 
       << strerror(status) << ends;
    throw Error(os.str());
  };

  retryIsReady_ = true;

  // Prepare the fd for non-blocking I/O. First get the current fcntl
  // flags.

  int flags = fcntl(fd_, F_GETFL, 0);
  if(flags < 0) {
    ostringstream os;
    os << "Pipe::PipeFd::fillPipeFd(): Error in fcntl(): " 
       << strerror(flags) << ends;
    throw Error(os.str());
  };

  // Now add in the non-blocking I/O bit.

#ifdef O_NONBLOCK
  flags |= O_NONBLOCK;  /* POSIX.1 */
#elif defined(O_NDELAY)
  flags |= O_NDELAY;    /* System V */
#elif defined(FNDELAY)
  flags |= FNDELAY;     /* BSD */
#else
#error "Pipe::PipeFd::fillPipeFd(): Non-blocking I/O doesn't appear to be supported."
#endif

  // Install the new flags.

  if(fcntl(fd_, F_SETFL, flags) < 0) {
    throw Error("Pipe::PipeFd::fillPipeFd(): "
		"Unable to select non-blocking I/O.\n");
  }
}

/**.......................................................................
 * Pipe destructor function
 */
Pipe::~Pipe()
{
  if(guardIsReady_) {
    if(pthread_mutex_destroy(&guard_) != 0) {
      LogStream errStr;

      errStr.appendSysError(true, "pthread_mutex_destroy()");
      errStr.report();
    }
    guardIsReady_ = false;
  };
}

/**.......................................................................
 * Read from the Pipe
 */
void Pipe::readPipe(void *buffer, size_t nbyte, long timeout)
{
  int status;             // The return status of pthreads functions
  PipeState pipe_state;   // The status of the I/O operation 
  struct timespec endtime;// The absolute time at which to give up waiting 
  bool wasError=false;
  int oldtype;
  LogStream err;

  // Check arguments.

  if(buffer == 0) {
    err.appendMessage(true, "buffer is NULL.");
    throw Error(err);
  }

  // We can't guarantee atomic transactions for > PIPE_BUF bytes.

  if(nbyte > PIPE_BUF) {
    err.appendMessage(true, "Cannot perform an atomic read for nbyte > PIPE_BUF.");
    throw Error(err);
  }

  // Work out the timeout, if relevant.

  if(timeout > 0) {
    unsigned long nsec;       // Temporary variable in nanosecond calculation 

    getTimeOfDay(&endtime);

    // Add the timeout duration, being careful to avoid overflowing
    // the nanosecond member (Note that timespec::tv_nsec is signed
    // and nsec isn't).

    endtime.tv_sec += timeout / 1000UL;
    nsec = (unsigned long) endtime.tv_nsec + 1000000UL * (timeout%1000UL);
    if(nsec < 1000000000UL) {
      endtime.tv_nsec = nsec;
    } else {
      endtime.tv_sec += nsec / 1000000000UL;
      endtime.tv_nsec = nsec % 1000000000UL;
    };
  };

  // Acquire exclusive access to the pipe.

  INSTALL_MUTEX_CLEANUP(guard_, err);

  if(pthread_mutex_lock(&guard_)) {
    err.appendSysError(true, "pthread_mutex_lock");

    // Unlock the mutex if it was successfully locked

    if(pthread_mutex_trylock(&guard_) && errno==EBUSY) {
      if(pthread_mutex_unlock(&guard_))
	err.appendSysError(true, "pthread_mutex_unlock");
    }

    throw Error(err);
  }

  // Attempt to read the requested number of bytes, using non-blocking
  // I/O.  If the data can't be read, and blocking I/O has been
  // requested, wait on a retry condition variable for the pipe to
  // become readable. This variable is posted whenever data is written
  // to the other end of the pipe.

  do {

    // Work out the number of bytes that still need to be read.  Note
    // that if we had to wait in pthread_cond_wait() during the
    // previous iteration of this loop, another thread may have
    // appended some data to unread_[], or taken some out of it.

    int request = nbyte - nread_;

    // If there is sufficient data in unread_[], copy nbyte bytes
    // to the output buffer, then shift any remaining bytes to the
    // beginning of pipe->unread[].

    if(request <= 0) {
      memcpy(buffer, unread_, nbyte);
      nread_ -= nbyte;
      memmove(unread_, unread_ + nbyte, nread_);
      pipe_state = PIPE_OK;
    } else {
      status = ::read(readfd_.fd_, ((char *)buffer) + nread_, request);

      // If the read completed, move buffered data in unread into the
      // output buffer, and signal blocked writers that the pipe may
      // now have some space available for another message. Note that
      // we use broadcast rather than signal because different threads
      // may be trying to write different amounts of data, and what
      // may not satisfy one thread might satisfy another.

      DBPRINT(false, Debug::DEBUG7, "Just read from the pipe: status = " 
	      << status << " pthread_id is: "
	      << "(" << pthread_self() << ")");

      if(status == request) {
	memcpy(buffer, unread_, nread_);
	nread_ = 0;

	DBPRINT(false, Debug::DEBUG7, "Signalling writefd_.retry "
		<< "(" << pthread_self() << ")");

	pthread_cond_broadcast(&writefd_.retry_);
	pipe_state = PIPE_OK;

	// The return value that signifies that the pipe couldn't be
	// read without blocking, depends on which convention of
	// non-blocking I/O is being used.

      } else if(status == 
#ifdef O_NONBLOCK            /* POSIX.1 */
		-1 && errno == EAGAIN
#elif defined(O_NDELAY)      /* System V */
		0
#elif defined(FNDELAY)       /* BSD */
		-1 && errno == EWOULDBLOCK
#endif
		) {

	// The read would have blocked.

	pipe_state = PIPE_BUSY;

	// Unless non-blocking I/O has been requested, relinquish
	// exclusive access to the pipe while waiting for a message to
	// be written to the other end of the pipe.

	if(timeout < 0) {

	  if(Debug::debugging(Debug::DEBUG2))
	    DBPRINT(false, Debug::DEBUG7, "About to wait on readfd_.retry"
		    << " pthread_id is: " << pthread_self());

	  if(pthread_cond_wait(&readfd_.retry_, &guard_) != 0) {
	    err.appendSysError(true, "pthread_cond_wait");
	    pipe_state = PIPE_ERROR;

	  if(Debug::debugging(Debug::DEBUG2))
	    DBPRINT(false, Debug::DEBUG7, "Error waiting"
		    << " timeout is: " << timeout);

	  }

	  if(Debug::debugging(Debug::DEBUG2))
	    DBPRINT(false, Debug::DEBUG7, "Done waiting"
		    << " timeout is: " << timeout);

	} else if(timeout > 0) {
	  status = pthread_cond_timedwait(&readfd_.retry_, &guard_, &endtime);
	  if(status == ETIMEDOUT) {
	    timeout = 0;
	    pipe_state = PIPE_BUSY;
	  } else if(status) {
	    err.appendSysError(true, "pthread_cond_timedwait");
	    pipe_state = PIPE_ERROR;
	  };
	};

	// I/O error.

      } else if(status < 0) {

	err.appendSysError(true, "read");
	pipe_state = PIPE_ERROR;

	// If fewer bytes were returned than were needed to complete the read,
	// append them to pipe->unread[] then signal other threads that the
	// pipe may now have space for another write(). Also signal other
	// waiting readers that more data has been added to the read buffer.
	// Note that we use broadcast rather than signal because different
	// threads may be attempting different sized transactions,
	// and what may not satisfy one thread might satisfy another.

      } else if(status < request) {
	memcpy(unread_ + nread_, ((char *)buffer) + nread_, status);
	nread_ += status;

	DBPRINT(false, Debug::DEBUG7, "Signalling read/writefd_.retry "
		<< "(" << pthread_self() << ")");

	pthread_cond_broadcast(&writefd_.retry_);
	pthread_cond_broadcast(&readfd_.retry_);
	pipe_state = PIPE_BUSY;

	// Unexpected return code.

      } else {
	err.initMessage(true);
	err << "Unexpected code " 
	   << status
	   << " returned by read().";
	pipe_state = PIPE_ERROR;
      };
    };
  } while(timeout != 0 && pipe_state==PIPE_BUSY);

  // Relinquish exclusive access to the pipe and return the completion
  // status.

  if(pthread_mutex_unlock(&guard_) != 0) 
    err.appendSysError(true, "pthread_mutex_unlock");

  // Now remove the handler, but don't execute it.
  
  UNINSTALL_MUTEX_CLEANUP(err);

  // If an error occurred, throw an exception now.

  if(err.isError()) 
    throw Error(err);
}

/**.......................................................................
 * Read from the Pipe
 */
PipeState Pipe::read(void *buffer, size_t nbyte, long timeout)
{
  int status;             // The return status of pthreads functions
  PipeState pipe_state;   // The status of the I/O operation 
  struct timespec endtime;// The absolute time at which to give up waiting 
  bool wasError=false;
  int oldtype;
  LogStream err;

  // Check arguments.

  if(buffer == 0) {
    err.appendMessage(true, "buffer is NULL.");
    return PIPE_ERROR;
  }

  // We can't guarantee atomic transactions for > PIPE_BUF bytes.

  if(nbyte > PIPE_BUF) {
    err.appendMessage(true, "Cannot perform an atomic read for nbyte > PIPE_BUF.");
    return PIPE_ERROR;
  }

  // Work out the timeout, if relevant.

  if(timeout > 0) {
    unsigned long nsec;       // Temporary variable in nanosecond calculation 

    try {
      getTimeOfDay(&endtime);
    } catch(...) {
      return PIPE_ERROR;
    }

    // Add the timeout duration, being careful to avoid overflowing
    // the nanosecond member (Note that timespec::tv_nsec is signed
    // and nsec isn't).

    endtime.tv_sec += timeout / 1000UL;
    nsec = (unsigned long) endtime.tv_nsec + 1000000UL * (timeout%1000UL);
    if(nsec < 1000000000UL) {
      endtime.tv_nsec = nsec;
    } else {
      endtime.tv_sec += nsec / 1000000000UL;
      endtime.tv_nsec = nsec % 1000000000UL;
    };
  };

  // Acquire exclusive access to the pipe.

  INSTALL_MUTEX_CLEANUP(guard_, err);

  if(pthread_mutex_lock(&guard_)) {
    err.appendSysError(true, "pthread_mutex_lock");

    // Unlock the mutex if it was successfully locked

    if(pthread_mutex_trylock(&guard_) && errno==EBUSY) {
      if(pthread_mutex_unlock(&guard_))
	err.appendSysError(true, "pthread_mutex_unlock");
    }

    return PIPE_ERROR;
  }

  // Attempt to read the requested number of bytes, using non-blocking
  // I/O.  If the data can't be read, and blocking I/O has been
  // requested, wait on a retry condition variable for the pipe to
  // become readable. This variable is posted whenever data is written
  // to the other end of the pipe.

  do {

    // Work out the number of bytes that still need to be read.  Note
    // that if we had to wait in pthread_cond_wait() during the
    // previous iteration of this loop, another thread may have
    // appended some data to unread_[], or taken some out of it.

    int request = nbyte - nread_;

    // If there is sufficient data in unread_[], copy nbyte bytes
    // to the output buffer, then shift any remaining bytes to the
    // beginning of pipe->unread[].

    if(request <= 0) {
      memcpy(buffer, unread_, nbyte);
      nread_ -= nbyte;
      memmove(unread_, unread_ + nbyte, nread_);
      pipe_state = PIPE_OK;
    } else {

      COUT("Reading: " << request << " bytes from fd: " << readfd_.fd_ << pthread_self());
 
      status = ::read(readfd_.fd_, ((char *)buffer) + nread_, request);

      COUT("Read: " << request << " bytes from fd: " << readfd_.fd_ << " status = " << status << " " << pthread_self());

      // If the read completed, move buffered data in unread into the
      // output buffer, and signal blocked writers that the pipe may
      // now have some space available for another message. Note that
      // we use broadcast rather than signal because different threads
      // may be trying to write different amounts of data, and what
      // may not satisfy one thread might satisfy another.

      if(status == request) {

	COUT("(read) Here 0 " << pthread_self());

	memcpy(buffer, unread_, nread_);
	nread_ = 0;

	DBPRINT(false, Debug::DEBUG7, "Signalling writefd_.retry "
		<< "(" << pthread_self() << ")");

	pthread_cond_broadcast(&writefd_.retry_);
	pipe_state = PIPE_OK;

	// The return value that signifies that the pipe couldn't be
	// read without blocking, depends on which convention of
	// non-blocking I/O is being used.

      } else if(status == 
#ifdef O_NONBLOCK            /* POSIX.1 */
		-1 && errno == EAGAIN
#elif defined(O_NDELAY)      /* System V */
		0
#elif defined(FNDELAY)       /* BSD */
		-1 && errno == EWOULDBLOCK
#endif
		) {

	COUT("(read) Here 1 " << pthread_self());

	// The read would have blocked.

	pipe_state = PIPE_BUSY;

	// Unless non-blocking I/O has been requested, relinquish
	// exclusive access to the pipe while waiting for a message to
	// be written to the other end of the pipe.

	if(timeout < 0) {

	  COUT("(read) Here 2: timeout = " << timeout << pthread_self());

	  if(pthread_cond_wait(&readfd_.retry_, &guard_) != 0) {
	    err.appendSysError(true, "pthread_cond_wait");
	    pipe_state = PIPE_ERROR;
	  }

	} else if(timeout > 0) {

	  COUT("(read) Here 3 " << pthread_self());

	  status = pthread_cond_timedwait(&readfd_.retry_, &guard_, &endtime);

	  if(status == ETIMEDOUT) {
	    timeout = 0;
	    pipe_state = PIPE_BUSY;
	  } else if(status) {
	    err.appendSysError(true, "pthread_cond_timedwait");
	    pipe_state = PIPE_ERROR;
	  };

	};

	// I/O error.

      } else if(status < 0) {

	COUT("(read) Here 4 " << pthread_self());

	err.appendSysError(true, "read");
	pipe_state = PIPE_ERROR;

	// If fewer bytes were returned than were needed to complete the read,
	// append them to pipe->unread[] then signal other threads that the
	// pipe may now have space for another write(). Also signal other
	// waiting readers that more data has been added to the read buffer.
	// Note that we use broadcast rather than signal because different
	// threads may be attempting different sized transactions,
	// and what may not satisfy one thread might satisfy another.

      } else if(status < request) {

	COUT("(read) Here 5 " << pthread_self());

	memcpy(unread_ + nread_, ((char *)buffer) + nread_, status);
	nread_ += status;

	DBPRINT(false, Debug::DEBUG7, "Signalling read/writefd_.retry "
		<< "(" << pthread_self() << ")");

	pthread_cond_broadcast(&writefd_.retry_);
	pthread_cond_broadcast(&readfd_.retry_);
	pipe_state = PIPE_BUSY;

	// Unexpected return code.

      } else {

	COUT("(read) Here 6 " << pthread_self());

	err.initMessage(true);
	err << "Unexpected code " 
	   << status
	   << " returned by read().";
	pipe_state = PIPE_ERROR;
      };
    };
    
    COUT("(read) pipe_state = " << timeout << " " << (pipe_state==PIPE_BUSY));

  } while(timeout != 0 && pipe_state==PIPE_BUSY);
  
  // Relinquish exclusive access to the pipe and return the completion
  // status.
  
  COUT("(read) Relinquishing access " << pthread_self());
  
  if(pthread_mutex_unlock(&guard_) != 0) {
    err.appendSysError(true, "pthread_mutex_unlock");
    pipe_state = PIPE_ERROR;
  }
  
  // Now remove the handler, but don't execute it.
  
  UNINSTALL_MUTEX_CLEANUP(err);
  
  // If an error occurred, throw an exception now.

  return pipe_state;
}

/**.......................................................................
 * Write to a Pipe
 */
void Pipe::writePipe(void *buffer, size_t nbyte, long timeout)
{
  int status;             /* The return status of pthreads functions. */
  PipeState pipe_state;   /* The status of the I/O operation */
  struct timespec endtime;/* The absolute time at which to give up waiting */
  LogStream err;

  sigset_t allSignals;
  sigfillset(&allSignals);
  pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

  // Check arguments.

  if(buffer == 0) {
    err.appendMessage(true, "Received NULL buffer.");
    throw Error(err);
  }

  // We can't guarantee atomic transactions for > PIPE_BUF bytes.

  if(nbyte > PIPE_BUF) {
    err.initMessage(true);
    err << "Rejected nbyte = " << (unsigned long) nbyte 
       << " > PIPE_BUF = " << (unsigned long) PIPE_BUF;
    throw Error(err);
  };

  // Work out the timeout, if relevant.

  if(timeout > 0) {
    unsigned long nsec;       /* Temporary variable in nanosecond calculation */
    getTimeOfDay(&endtime);

    // Add the timeout duration, being careful to avoid overflowing
    // the nanosecond member (Note that timespec::tv_nsec is signed
    // and nsec isn't).

    endtime.tv_sec += timeout / 1000UL;
    nsec = (unsigned long) endtime.tv_nsec + 1000000UL * (timeout%1000UL);
    if(nsec < 1000000000UL) {
      endtime.tv_nsec = nsec;
    } else {
      endtime.tv_sec += nsec / 1000000000UL;
      endtime.tv_nsec = nsec % 1000000000UL;
    };
  };

  // Acquire exclusive access to the pipe.

  INSTALL_MUTEX_CLEANUP(guard_, err);
  
  DBPRINT(true, Debug::DEBUG10, "About to lock pipe guard: " << &guard_ 
	  << "(" << pthread_self() << ")");

  if(pthread_mutex_lock(&guard_)) {
    err.appendSysError(true, "pthread_mutex_lock");
    
    // Unlock the mutex if it was successfully locked
    
    if(pthread_mutex_trylock(&guard_) && errno==EBUSY) {
      
      if(pthread_mutex_unlock(&guard_))
	err.appendSysError(true, "pthread_mutex_unlock");
    }
    
    throw Error(err);
  }

  // Attempt to write the requested number of bytes, using
  // non-blocking I/O.  If the data can't be written, and blocking
  // I/O has been requested, wait on a retry condition variable for
  // the pipe to become writable. This variable is posted whenever
  // data is read from the other end of the pipe.
  
  do {
    
    status = ::write(writefd_.fd_, buffer, nbyte);
    
    // If the write succeeded, signal blocked readers that the pipe
    // now contains some data to be read.  Note that we use
    // broadcast rather than signal because different threads may be
    // trying to read different amounts of data, and what may not
    // satisfy one thread might satisfy another.
    
    if(status == nbyte) {
      pipe_state = PIPE_OK;

      DBPRINT(false, Debug::DEBUG7, "Signalling readfd_.retry "
	      << "(" << pthread_self() << ")");

      pthread_cond_broadcast(&readfd_.retry_);
      
      // The return value that signifies that the pipe couldn't be written to
      // without blocking, depends on which convention of non-blocking I/O is
      // being used.
      
    } else if(status == 
#ifdef O_NONBLOCK            /* POSIX.1 */
	      -1 && errno == EAGAIN
#elif defined(O_NDELAY)      /* System V */
	      0
#elif defined(FNDELAY)       /* BSD */
	      -1 && errno == EWOULDBLOCK
#endif
	      ) {
      // The write would have blocked.
      
      pipe_state = PIPE_BUSY;

      if(Debug::debugging(Debug::DEBUG2))
	DBPRINT(false, Debug::DEBUG7, "write would have blocked"
		<< " timeout is: " << timeout);
      
      // Unless non-blocking I/O has been requested, relinquish exclusive
      // access to the pipe while waiting for a message to be read from the
      // other end of the pipe.
      
      if(timeout < 0) {

	  if(Debug::debugging(Debug::DEBUG2))
	    DBPRINT(false, Debug::DEBUG7, "About to wait on writefd_.retry_"
		    << " pthread_id is: " << pthread_self());

	if(pthread_cond_wait(&writefd_.retry_, &guard_) != 0) {
	  err.appendSysError(true, "pthread_cond_wait");
	  pipe_state = PIPE_ERROR;

	  if(Debug::debugging(Debug::DEBUG2))
	    DBPRINT(false, Debug::DEBUG7, "Error waiting"
		    << " timeout is: " << timeout);

	}

	  if(Debug::debugging(Debug::DEBUG2))
	    DBPRINT(false, Debug::DEBUG7, "Done waiting"
		    << " timeout is: " << timeout);

      } else if(timeout > 0) {
	status = pthread_cond_timedwait(&writefd_.retry_, &guard_, &endtime);
	if(status == ETIMEDOUT) {
	  timeout = 0;
	  pipe_state = PIPE_BUSY;
	} else if(status) {
	    err.appendSysError(true, "pthread_cond_timedwait");
	    pipe_state = PIPE_ERROR;
	};
      };

      // I/O error.
      
    } else if(status < 0) {

      err.appendSysError(true, "write");
      pipe_state = PIPE_ERROR;

      // Unexpected return code.

    } else {
      err.initMessage(true);
      err << "Unexpected code " 
	  << status
	  << " returned by write().";
      pipe_state = PIPE_ERROR;
    };
  } while(timeout != 0 && pipe_state==PIPE_BUSY);

  // Relinquish exclusive access to the pipe and return the completion
  // status.

  pthread_mutex_unlock(&guard_);

  // Now remove the handler, but don't execute it.
  
  UNINSTALL_MUTEX_CLEANUP(err);

  // If an error occurred, throw an exception now.

  if(pipe_state==PIPE_ERROR)
    throw Error(err);
}

/**.......................................................................
 * Write to a Pipe
 */
PipeState Pipe::write(void *buffer, size_t nbyte, long timeout)
{
  int status;             /* The return status of pthreads functions. */
  PipeState pipe_state;   /* The status of the I/O operation */
  struct timespec endtime;/* The absolute time at which to give up waiting */
  LogStream err;

  sigset_t allSignals;
  sigfillset(&allSignals);
  pthread_sigmask(SIG_BLOCK, &allSignals, NULL);

  // Check arguments.

  if(buffer == 0) {
    err.appendMessage(true, "Received NULL buffer.");
    return PIPE_ERROR;
  }

  // We can't guarantee atomic transactions for > PIPE_BUF bytes.

  if(nbyte > PIPE_BUF) {
    err.initMessage(true);
    err << "Rejected nbyte = " << (unsigned long) nbyte 
       << " > PIPE_BUF = " << (unsigned long) PIPE_BUF;
    throw Error(err);
  };

  // Work out the timeout, if relevant.

  if(timeout > 0) {
    unsigned long nsec;       /* Temporary variable in nanosecond calculation */

    try {
      getTimeOfDay(&endtime);
    } catch(...) {
      return PIPE_ERROR;
    }

    // Add the timeout duration, being careful to avoid overflowing
    // the nanosecond member (Note that timespec::tv_nsec is signed
    // and nsec isn't).

    endtime.tv_sec += timeout / 1000UL;
    nsec = (unsigned long) endtime.tv_nsec + 1000000UL * (timeout%1000UL);
    if(nsec < 1000000000UL) {
      endtime.tv_nsec = nsec;
    } else {
      endtime.tv_sec += nsec / 1000000000UL;
      endtime.tv_nsec = nsec % 1000000000UL;
    };
  };

  // Acquire exclusive access to the pipe.

  INSTALL_MUTEX_CLEANUP(guard_, err);
  
  DBPRINT(true, Debug::DEBUG10, "About to lock pipe guard: " << &guard_ 
	  << "(" << pthread_self() << ")");

  if(pthread_mutex_lock(&guard_)) {
    err.appendSysError(true, "pthread_mutex_lock");
    
    // Unlock the mutex if it was successfully locked
    
    if(pthread_mutex_trylock(&guard_) && errno==EBUSY) {
      
      if(pthread_mutex_unlock(&guard_))
	err.appendSysError(true, "pthread_mutex_unlock");
    }
    
    return PIPE_ERROR;
  }

  // Attempt to write the requested number of bytes, using
  // non-blocking I/O.  If the data can't be written, and blocking
  // I/O has been requested, wait on a retry condition variable for
  // the pipe to become writable. This variable is posted whenever
  // data is read from the other end of the pipe.
  
  do {
    
    COUT("writing " << nbyte << " bytes to fd: " << writefd_.fd_);

    status = ::write(writefd_.fd_, buffer, nbyte);
    
    // If the write succeeded, signal blocked readers that the pipe
    // now contains some data to be read.  Note that we use
    // broadcast rather than signal because different threads may be
    // trying to read different amounts of data, and what may not
    // satisfy one thread might satisfy another.
    
    if(status == nbyte) {
      pipe_state = PIPE_OK;

      DBPRINT(false, Debug::DEBUG7, "Signalling readfd_.retry "
	      << "(" << pthread_self() << ")");

      pthread_cond_broadcast(&readfd_.retry_);
      
      // The return value that signifies that the pipe couldn't be written to
      // without blocking, depends on which convention of non-blocking I/O is
      // being used.
      
    } else if(status == 
#ifdef O_NONBLOCK            /* POSIX.1 */
	      -1 && errno == EAGAIN
#elif defined(O_NDELAY)      /* System V */
	      0
#elif defined(FNDELAY)       /* BSD */
	      -1 && errno == EWOULDBLOCK
#endif
	      ) {
      // The write would have blocked.
      
      pipe_state = PIPE_BUSY;

      if(Debug::debugging(Debug::DEBUG2))
	DBPRINT(false, Debug::DEBUG7, "write would have blocked"
		<< " timeout is: " << timeout);
      
      // Unless non-blocking I/O has been requested, relinquish exclusive
      // access to the pipe while waiting for a message to be read from the
      // other end of the pipe.
      
      if(timeout < 0) {

	  if(Debug::debugging(Debug::DEBUG2))
	    DBPRINT(false, Debug::DEBUG7, "About to wait on writefd_.retry_"
		    << " pthread_id is: " << pthread_self());

	if(pthread_cond_wait(&writefd_.retry_, &guard_) != 0) {
	  err.appendSysError(true, "pthread_cond_wait");
	  pipe_state = PIPE_ERROR;

	  if(Debug::debugging(Debug::DEBUG2))
	    DBPRINT(false, Debug::DEBUG7, "Error waiting"
		    << " timeout is: " << timeout);

	}

	  if(Debug::debugging(Debug::DEBUG2))
	    DBPRINT(false, Debug::DEBUG7, "Done waiting"
		    << " timeout is: " << timeout);

      } else if(timeout > 0) {
	status = pthread_cond_timedwait(&writefd_.retry_, &guard_, &endtime);
	if(status == ETIMEDOUT) {
	  timeout = 0;
	  pipe_state = PIPE_BUSY;
	} else if(status) {
	    err.appendSysError(true, "pthread_cond_timedwait");
	    pipe_state = PIPE_ERROR;
	};
      };

      // I/O error.
      
    } else if(status < 0) {

      err.appendSysError(true, "write");
      pipe_state = PIPE_ERROR;

      // Unexpected return code.

    } else {
      err.initMessage(true);
      err << "Unexpected code " 
	  << status
	  << " returned by write().";
      pipe_state = PIPE_ERROR;
    };
  } while(timeout != 0 && pipe_state==PIPE_BUSY);

  // Relinquish exclusive access to the pipe and return the completion
  // status.

  pthread_mutex_unlock(&guard_);

  // Now remove the handler, but don't execute it.
  
  UNINSTALL_MUTEX_CLEANUP(err);

  // If an error occurred, throw an exception now.

  return pipe_state;
}

/**.......................................................................
 * Get the current time of day.  Used by read/writePipe() for timeouts
 */
void Pipe::getTimeOfDay(struct timespec* ts)
{
  struct timeval tp;

  // The BSD gettimeofday() function seems to be much more widely
  // available than the posix clock_gettime() function.

  if(gettimeofday(&tp, NULL)) {
    ostringstream os;
    os << "Pipe:getTimeOfDay: " << strerror(errno) << ends;
    throw Error(os.str());
  };

  ts->tv_sec  = tp.tv_sec;
  ts->tv_nsec = tp.tv_usec * 1000;
}

/**.......................................................................
 * Return the readable fd associated with this pipe.  For use in select()
 */
int Pipe::fd()
{
  return readfd_.fd_;
}

/**.......................................................................
 * Return an intialized set of readable file descriptors
 * associated with this pipe.  
 *
 * The history of this is that macros like FD_ZERO and FD_SET cannot
 * be used in template class definitions (for instance GenericTask.h,
 * where this method is used), because gcc 2.95 has a bug that will
 * cause it to choke on the 'volatile' keyword in bits/select.h where
 * these macros are defined.  Not clear that this bug has been
 * resolved, but there's one note I found indicating that it was fixed
 * in gcc 3.3.
 */
fd_set Pipe::rfds()
{
  fd_set readable_fds;

  if(readfd_.fd_ < 0)
    throw Error("File descriptor is NULL.\n");

  FD_ZERO(&readable_fds);
  FD_SET(readfd_.fd_, &readable_fds);

  return readable_fds;
}
