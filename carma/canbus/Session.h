/** @file
 * Declaration of carma::canbus::canoverip::Session class.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.2 $
 * $Date: 2010/06/18 22:47:09 $
 * $Id: Session.h,v 1.2 2010/06/18 22:47:09 abeard Exp $
 */

#ifndef SESSION_H
#define SESSION_H

// System includes
#include <netinet/in.h>
#include <pthread.h>
#include <sys/socket.h>

#include <vector>

namespace log4cpp {
    class Category;
}

namespace carma {
namespace canbus {

// Forward declaration
class Message;

namespace canoverip {


/**
 * Class to encompass a CAN over IP session.
 *
 * Not intended or safe for use by anybody except the canOverIpServer!
 */
class Session {
public:

    /**
     * Constructor
     * @param readSock Socket descriptor for this Connection/Session
     * @param writeSock Socket descriptor for this Connection/Session
     */
    Session(int readSock, int writeSock);
    
    /**
     * Destructor.
     */
    ~Session();
   
    /**
     * CAN Message filter type.
     */
    typedef struct {
        unsigned int aCode;           // CAN id filter acceptance codes
        unsigned int aMask;           // CAN id filter acceptance masks
        unsigned short busFilter;     // Bus Id Filter
    } FilterType;

    /**
     * Add a filter to this Session.
     */
    void addFilter(FilterType filter);

    /**
     * Clear existing filters.
     */
    void clearFilters( );

    /**
     * Check to see if a CAN Message Id matches a filter for this Session.
     */
    bool matchesFilter(const carma::canbus::Message & msg);

    /**
     * Get read sock.
     * @return readsock
     */
    inline int getReadSock( ) { return rsd_; };

    /**
     * Get write sock.
     * @return write sock.
     */
    inline int getWriteSock( ) { return wsd_; };

protected:
    

private:
    
    typedef union {
        struct sockaddr_in sa_in; // Server addresses
        struct sockaddr sa;
    } SockAddressUnion;
    
    int rsd_;                          // Read Socket Descriptor.
    int wsd_;                          // Write Socket Descriptor.
    SockAddressUnion ra_;              // Read socket address
    SockAddressUnion wa_;              // Write socket address
    pthread_mutex_t mutex_;            // Mutex for data structure access. 
    std::vector<FilterType> filters_;  // Vector to hold filters.
    log4cpp::Category &log_;

    Session &operator=(const Session &);
    Session(const Session &);
    
}; 
}}} // namespace carma::canbus::canoverip
#endif
