#ifndef SZA_UTIL_PIPEQUEUE_H
#define SZA_UTIL_PIPEQUEUE_H

/**
 * @file PipeQueue.h
 * 
 * Tagged: Fri Nov 14 12:39:34 UTC 2003
 * 
 * @author Erik Leitch
 */

#include "carma/szautil/Pipe.h"

#include <queue>

namespace sza {
  namespace util {
    
    /**
     * A class to encapsulate a pipe
     */
    class PipeQueue : public Pipe {
      
    public:
    
      // A class for handling a single message

      class QueueNode {
      public:
	
	QueueNode();
	QueueNode(void* buffer, size_t nbyte);

	QueueNode(const QueueNode& node);
	QueueNode(QueueNode& node);
	void operator=(const QueueNode& node);
	void operator=(QueueNode& node);

	~QueueNode();

	unsigned nbyte_;
	unsigned char* buffer_;

      };

	// A class for handling a queue of messages

      class MsgQueue {
      public:

	void push(void* buffer, size_t nbyte);
	void pop(void* buffer, size_t nbyte);
	bool empty();

	// Direct access to the queue

	QueueNode& front();
	void pop();

      private:

	std::queue<QueueNode> queue_;

      };

      /**
       * Constructor.
       *
       * @throws Exception
       */
      PipeQueue();
      
      /**
       * Destructor.
       *
       * @throws Exception
       */
      ~PipeQueue();
      
      /**
       * Write to the pipe.
       *
       * @throws Exception
       */
      PipeState write(void *buffer, size_t nbyte, long timeout=PIPE_NOWAIT);
      
      /**
       * Read from the pipe.
       *
       * @throws Exception
       */
      PipeState read(void *buffer, size_t nbyte, long timeout=PIPE_NOWAIT);
      
    public:

      Mutex queueGuard_;
      MsgQueue messages_;

    }; // End class Pipe
    
  }; // End namespace util
}; // End namespace sza

#endif // PIPE_H







