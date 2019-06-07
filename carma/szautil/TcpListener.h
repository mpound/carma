#ifndef TCPLISTENER_H
#define TCPLISTENER_H

/**
 * @file TcpListener.h
 * 
 * Started: Tue Mar  2 13:43:07 UTC 2004
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class TcpListener {
    public:
      
      /**
       * Constructor.
       */
      TcpListener(unsigned port, unsigned queueSize);
      
      /**
       * Destructor.
       */
      virtual ~TcpListener();
      
      /**
       * Accept a connection on our server port.
       *
       * @param bool blocking If false, set up the returned
       * descriptor for non-blocking I/O
       */
      int acceptConnection(bool blocking=false);
      
      /**
       * Return our file descriptor
       */
      inline int getFd() { return fd_;};
      
    private:
      
      /**
       * The file descriptor associated with our server socket.
       */
      int fd_;
      
    }; // End class TcpListener
    
  }; // End namespace util
}; // End namespace sza

#endif // End #ifndef 


