#ifndef PIPEQ_h
#define PIPEQ_h

/**
 * @file PipeQ.h
 * 
 * Tagged: Fri Nov 14 12:39:35 UTC 2003
 * 
 * @author Erik Leitch
 */
#include "Pipe.h"

namespace sza {
  namespace util {
    
    /**
     * Template class for a message queue implemented using pipes
     */
    template<class Msg>
      class PipeQ {
      
      public:
      
      /**
       * Constructor.
       */
      PipeQ();
      
      /**
       * Destructor.
       */
      ~PipeQ();
      
      /**
       * Method to send a message to the message queue.
       *
       * @throws Exception
       */
      void sendMsg(Msg* msg);
      
      /**
       * Read message method simply gets the next message off the pipe.
       *
       * @throws Exception
       */
      void readMsg(Msg* msg);
      
      /**
       * Return the selectable file descriptor associated with this
       * message queue.
       */
      int fd();
      
      /**
       * Return an intialized set of readable file descriptors
       * associated with this queue.
       */
      fd_set rfds();
      
      private:
      
      /**
       * Every message queue will have a pipe for messages
       */
      Pipe pipe_;
    };
    
    /**
     * Write the next message to the pipe.
     *
     * @throws Exception
     */
    template<class Msg>
      void PipeQ<Msg>::sendMsg(Msg* msg) 
      {
	pipe_.writePipe((void*) msg, sizeof(Msg), -1);
      };
    
    /**
     * Read the next message off the pipe.
     *
     * @throws Exception
     */
    template<class Msg>
      void PipeQ<Msg>::readMsg(Msg* msg) 
      {
	pipe_.readPipe((void*) msg, sizeof(Msg), -1);
      };
    
    /**
     * Get the readable file descriptor associated with this queue
     */
    template<class Msg>
      int PipeQ<Msg>::fd() 
      {
	return pipe_.fd();
      };
    
    /**
     * Get the readable file descriptor associated with this queue.
     */
    template<class Msg>
      fd_set PipeQ<Msg>::rfds() 
      {
	return pipe_.rfds();
      };
    
    /**
     * Constructor function.
     *
     * @throws Exception (via Pipe::Pipe)
     */
    template<class Msg>
      PipeQ<Msg>::PipeQ() {};
    
    /**
     * Destructor function.
     * 
     * @throws Exception (via Pipe::~Pipe)
     */
    template<class Msg>
      PipeQ<Msg>::~PipeQ() {};
    
  }; // End namespace util
}; // End namespace sza

#endif
