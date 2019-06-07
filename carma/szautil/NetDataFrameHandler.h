#ifndef SZA_UTIL_NETDATAFRAMEHANDLER_H
#define SZA_UTIL_NETDATAFRAMEHANDLER_H

/**
 * @file NetDataFrameHandler.h
 * 
 * Tagged: Sun Apr  4 22:34:52 UTC 2004
 * 
 * @author Erik Leitch
 */
namespace sza {
  namespace util {
    
    class NetDataFrameHandler : public NetHandler {
    public:
      
      /**
       * Constructor.
       */
      NetDataFrameHandler();
      
      /**
       * Destructor.
       */
      virtual ~NetDataFrameHandler();
      
    private:
    }; // End class NetDataFrameHandler
    
  } // End namespace util
} // End namespace sza



#endif // End #ifndef SZA_UTIL_NETDATAFRAMEHANDLER_H
