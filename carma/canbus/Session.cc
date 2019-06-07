/**@file
 * Definition of carma::canbus::canoverip::Session class.
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.5 $
 * $Date: 2012/08/03 23:10:10 $
 * $Id: Session.cc,v 1.5 2012/08/03 23:10:10 abeard Exp $
 */

// Std lib includes
#include <algorithm>
#include <climits>


// System includes
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include <pthread.h>

// Carma includes
#include "carma/canbus/Session.h"
#ifdef HAVE_SOCKETCAN
#include "carma/canbus/SocketCan.h"
#else
#include "carma/canbus/DirectCan.h"
#endif
#include "carma/canbus/exceptions.h"
#include "carma/util/Program.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedPthreadMutexLock.h"
#include "carma/util/ScopedPthreadMutexLockManager.h"
#include "carma/util/StartPthread.h"
#include "carma/util/Trace.h"

// Carma tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::canbus;
using namespace carma::canbus::canoverip;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace {

    // Constants
    const size_t MAX_FILTERS                = 100;
    const idType GLOBAL_RESET_API           = 0x00000000;
    const idType GLOBAL_RESET_ENG           = 0x08000000;
    const idType TIME_SYNC_API              = 0x08020000;
    const idType TIME_SYNC_ENG              = 0x00020000;
    const Trace::TraceLevel TRACE_CTOR_DTOR = Trace::TRACE4; 
    const Trace::TraceLevel TRACE_SESSIONS  = Trace::TRACE5;
    const Trace::TraceLevel TRACE_RX_TX     = Trace::TRACE6;
    const Trace::TraceLevel TRACE_MUTEX     = Trace::TRACE7;

    typedef std::vector<Session *> SessionVector;
    const SessionVector::size_type MAX_SESSIONS = 100;
   
    // Class to encompass threading and shared activities of a 
    // CanOverIp Session.  As a Singleton ( a crappy one ).
    class SessionCore {
    public:

        static SessionCore & instance( );  // Retrieve an instance
        void addSession(Session* recruit); // Add a session
    
    private:

        enum mode {
            SET_READ_FILTER    = 0x00,
            CLEAR_READ_FILTERS = 0x01,
            WRITE_CAN_MSG      = 0x02,
            WAKE_UP            = 0xee
        };

        // CAN message for sending over the network.  This is just a mnemonic
        // it isn't actually used currently as the actual bytes need to be
        // placed into a byte array for transport.
        struct netCanMsg_t {
            unsigned int id;
            unsigned short busId;
            unsigned char size;
            unsigned char data[8];
        }; // 15 Bytes

        // Request/CAN message for posting to CANbus or setting filter.
        struct requestMsg_t {
            unsigned char mode;
            /**
             * @union id_or_ac
             * CAN id or Acceptance code
             */
            union {
                unsigned int id;
                unsigned int ac;
            };
            /**
             * @union
             * busId or busFilter
             * 0xff = All busses.
             */
            union {
                unsigned int busId;
                unsigned int busFilter;
            };
            unsigned char size;
            /**
             * @union
             * CAN message data or 4 byte acceptance mask
             */
            union {
                unsigned char data[8];   // Data
                unsigned int am;         // Acceptance mask
            };
        }; // 16 bytes

        static SessionCore* instance_;
        SessionVector sessions_;          // Active sessions. 
        PthreadMutex sessionMutex_;       // Mutex for session mods.
#ifdef HAVE_SOCKETCAN
        carma::canbus::SocketCan can_;
#else
        carma::canbus::DirectCan can_;
#endif
        int maxFd_;
        
        pthread_t requestThreadId_;       // Read thread identifier
        pthread_t writeThreadId_;         // Write thread identifier

        Category &log_;                   // Log4cpp logger.

        // Thread entry points
        static void requestThreadEntry( SessionCore & This );
        static void writeThreadEntry( SessionCore & This );

        // Threads for processing requests and sending messages 
        // from/to the client
        void processRequestsThread();
        void canMsgWriteThread();

        bool blocked(carma::canbus::idType id);
        void packNetCanMsg(
                        const carma::canbus::Message &msg, 
                        unsigned char *buff
                        );
        requestMsg_t unpackRequestMsg(unsigned char *buff);
        void processRequest(const requestMsg_t &requestMsg, Session* session);

        SessionVector::iterator removeSession(Session * goner);

        SessionCore( );
        ~SessionCore( );
        SessionCore &operator=(const SessionCore &);
        SessionCore(const SessionCore &);

    }; // End class SessionCore

    SessionCore* SessionCore::instance_ = 0;

    // -------------------------------------
    SessionCore::SessionCore( ) 
        : 
        maxFd_(0), 
        log_( Program::getLogger() )
    {
        CPTRACE(TRACE_CTOR_DTOR, "SessionCore::SessionCore() - Begin.");

        ScopedPthreadMutexLock scopelock(sessionMutex_);

        // Avoid vector reallocation
        sessions_.reserve(MAX_SESSIONS);

        // Start read thread.
        requestThreadId_ = StartPthreadWithRef<SessionCore>(
            requestThreadEntry,
            *this,
            "SessionCore::requestThread",
            0, 
            true);

        // Start write thread.
        writeThreadId_ = StartPthreadWithRef<SessionCore>(
            writeThreadEntry,
            *this,
            "SessionCore::writeThread",
            0,
            true);
        
        CPTRACE(TRACE_CTOR_DTOR, "SessionCore::SessionCore() - End.");

    } // End SessionCore::SessionCore

    // -------------------------------------
    SessionCore::~SessionCore( )
    {
        int status;
        void *result;

        CPTRACE(TRACE_CTOR_DTOR, "SessionCore::~SessionCore() - Begin.");

        // First terminate threads as we don't want any of them accidently 
        // running with a dangling 'this' pointer.

        // Find out which thread called this...
        if (pthread_self() == requestThreadId_) {
            status = pthread_cancel(writeThreadId_);
            if (status == ESRCH) {
                // Thread has already exited
            } else if (status != 0) {
                log_ << Priority::ERROR << "~SessionCore - Error cancelling "
                    << "writeThread: " << strerror(status);
            } else {
                // Block on it and make sure it exits (they aren't detached)
                status = pthread_join(writeThreadId_, &result);
                if (status != 0) {
                    log_ << Priority::ERROR << "~SessionCore - Error joining on"
                    << " writeThread: " << strerror(status);
                } 
            }
        } else {
            // Do the same for the read thread...
            status = pthread_cancel(requestThreadId_);
            if (status == ESRCH) {
                // Thread has already exited
                // Not much we can do here...
            } else if (status != 0) {
                log_ << Priority::ERROR << "~SessionCore - Error cancelling "
                    << "read thread." << strerror(status);
            } else {
                // Block on it and make sure it exits (they aren't detached)
                status = pthread_join(requestThreadId_, &result);
                if (status != 0) {
                    log_ << Priority::ERROR << "~SessionCore - Error joining on"
                    << " readThread: " << strerror(status);
                } 
            }
        }
        instance_ = 0;
        CPTRACE(TRACE_CTOR_DTOR, "SessionCore::~SessionCore() - End.");
    } // End SessionCore::~SessionCore

    // -------------------------------------
    SessionCore & SessionCore::instance( ) 
    {
        // Might as well make it thread safe...
        static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

        // ...with double checked locking...
        if (!instance_) {
            pthread_mutex_lock(&lock);
            if (!instance_) {
                // Use a function static instance so that it deletes itself.
                static SessionCore session;
                instance_ = &session;
            }
            pthread_mutex_unlock(&lock);
        }

        return *instance_;
    }

    // -------------------------------------
    void SessionCore::addSession(Session * recruit)
    {
        ScopedPthreadMutexLock scopelock(sessionMutex_);
        CPTRACE(TRACE_SESSIONS, "SessionCore::addSession() - " 
            "Adding session to SessionCore.");
        requestMsg_t wakeUpMsg;
        wakeUpMsg.mode = WAKE_UP;

        if (sessions_.size() + 1 > MAX_SESSIONS) {
            log_ << Priority::WARN << "SessionCore::addSession() - "
                "Max sessions reached, session not added.";
            return;
        }

        sessions_.insert(sessions_.end(), recruit);
        
        if (recruit->getWriteSock( ) > maxFd_) 
            maxFd_ = recruit->getWriteSock( );

        CPTRACE(TRACE_SESSIONS, "SessionCore::addSession() - "
            "Session added to SessionCore.");
    }

    // -------------------------------------
    SessionVector::iterator SessionCore::removeSession(Session * goner)
    {
        bool recalculateMaxFd = false;
        SessionVector::iterator pos, next = sessions_.end();
        
        CPTRACE(TRACE_SESSIONS, "SessionCore::removeSession() - "
            "Removing session from SessionCore.");

        // Find session and remove it from sessions_ vector.
        pos = find( sessions_.begin(), sessions_.end(), goner );
        if ( pos != sessions_.end() )
            next = sessions_.erase(pos);

        if ( goner ) {
            recalculateMaxFd = (goner->getWriteSock() == maxFd_);
            delete goner;
        } else {
            log_ << Priority::ERROR << "SessionCore::removeSession() - "
                "Input Session pointer is 0.";
        }

        if ( recalculateMaxFd ) {
            int tmp, newMax = 0;
            for (pos = sessions_.begin(); pos != sessions_.end(); ++pos) {
                tmp = (*pos)->getWriteSock( );
                newMax = (tmp > newMax ? tmp : newMax);
            }
            maxFd_ = newMax;
        }
        
        CPTRACE(TRACE_SESSIONS, "SessionCore::removeSession() - "
            "Session removed from SessionCore.");

        return next;

    } // End removeSession
        

    // -------------------------------------
    bool SessionCore::blocked(idType id) 
    {
        if (id == TIME_SYNC_API || id == TIME_SYNC_ENG 
                || id == GLOBAL_RESET_API || id == GLOBAL_RESET_ENG) {
            return true;
        } else {
            return false;
        }
    }; // End SessionCore::blocked

    // -------------------------------------
    void SessionCore::packNetCanMsg(
                           const carma::canbus::Message &msg, 
                           unsigned char *buff
                          )
    {
        idType id = htonl(msg.getId());
        busIdType busId = htons(msg.getBusId());
        vector<byteType> data = msg.getData();
        unsigned short index = 0;

        memcpy(buff, &id, sizeof(idType));
        index = sizeof(idType);

        memcpy(&buff[index], &busId, sizeof(busIdType));

        index += sizeof(busIdType);
        buff[index] = static_cast<unsigned char> (data.size());
        index += 1;

        // Copy the data
        memcpy(static_cast<void *>(&buff[index]), 
               static_cast<const void *>(&data[0]),
               data.size());
    } // End SessionCore::packNetCanMsg

    // -------------------------------------
    SessionCore::requestMsg_t SessionCore::unpackRequestMsg(unsigned char *buff)
    {
        requestMsg_t reqMsg;

        reqMsg.mode = buff[0];

        // Only unpack what is needed
        switch (reqMsg.mode) {
            case SET_READ_FILTER:
                memcpy(&reqMsg.ac, &buff[1], 4);
                memcpy(&reqMsg.busFilter, &buff[5], 2);
                memcpy(&reqMsg.am, &buff[8], 4);

                // Convert to host byte order
                reqMsg.ac = ntohl(reqMsg.ac);
                reqMsg.busFilter = ntohs(reqMsg.busFilter);
                reqMsg.am = ntohl(reqMsg.am);
                break;
            case WRITE_CAN_MSG:
                memcpy(&reqMsg.id, &buff[1], 4);
                memcpy(&reqMsg.busId, &buff[5], 2);
                memcpy(&reqMsg.size, &buff[7], 1);
                memcpy(reqMsg.data, &buff[8], 8);

                // Convert to host byte order
                reqMsg.id = ntohl(reqMsg.id);
                // Mask off high three bits..
                reqMsg.id &= 0x1FFFFFFF;
                reqMsg.busId = ntohs(reqMsg.busId);
                if (reqMsg.size > 8) reqMsg.size = 8;
                break;
            default:
                break;
        }
        return reqMsg;
    } // End SessionCore::unpackRequestMessage
    
    // -------------------------------------
    void SessionCore::processRequest(
        const requestMsg_t &requestMsg,
        Session* session) 
    {
        carma::canbus::Message msg;
        vector<byteType> data;
        Session::FilterType filter;

        try {
            switch (requestMsg.mode) {
                case SET_READ_FILTER:
                    filter.aCode     = requestMsg.ac;
                    filter.aMask     = requestMsg.am;
                    filter.busFilter = requestMsg.busFilter;
                    session->addFilter(filter);
                    break;
                case CLEAR_READ_FILTERS:
                    session->clearFilters();
                    break;
                case WRITE_CAN_MSG:
                    // Extract the message and place into a 
                    // carma::canbus::Message
                    if (!blocked(requestMsg.id)) {
                        msg.setId(requestMsg.id);
                        msg.setBusId(requestMsg.busId);
                        for (unsigned int i = 0; i < requestMsg.size; i++) {
                            data.insert(data.end(), requestMsg.data[i]);
                        }
                        msg.setData(data);

                        // Post the message to the canbus.
                        can_.postMessage(msg);
                    }
                    break;
                case WAKE_UP:
                    // Exists only to bust out of select.
                    break;
                default:
                    // Don't do anything - spurious mode...
                    break;
            }
        } catch (carma::util::ErrorException &ex) {
            log_ << Priority::WARN << "Session::processRequest() - "
                << "ErrorException caught, message dropped: " << ex.what();
        }
    } // End SessionCore::processRequest
    
    // -------------------------------------
    void SessionCore::requestThreadEntry(SessionCore & This) 
    {
        try {
            This.processRequestsThread();
        } catch (const std::exception & ex) {
            This.log_ << Priority::ERROR << "std::exception caught: "
                << ex.what();
        } catch (...) {
            This.log_ << Priority::ERROR << "Unknown exception caught.";
        }
    } // End SessionCore::requestThreadEntry

    // --------------------------------------
    void SessionCore::writeThreadEntry(SessionCore & This) 
    {
        try {
            This.canMsgWriteThread();
        } catch (const std::exception & ex) {
            This.log_ << Priority::ERROR << "SessionCore::writeThreadEntry() - "
                "std::exception caught: "
                << ex.what();
        } catch (...) {
            This.log_ << Priority::ERROR << "SessionCore::writeThreadEntry() - "
                "Unknown exception caught.";
        }
        // Hmmm. Should I abort here?
    } // End SessionCore::writeThreadEntry

    // --------------------------------------
    void SessionCore::canMsgWriteThread()
    {
        int status;
        unsigned char buff[15];
        size_t buffSize = sizeof(buff);
        SessionVector::iterator pos;

        carma::canbus::Message msg; // CAN message from DirectCan...

        // Begin the read process...
        while (true) {
            try {
                // Block on DirectCan for a message.
                msg = can_.getMessage();

                ScopedPthreadMutexLockManager managedScopelock(sessionMutex_);
                managedScopelock.LockMutex();

                // Loop over all sessions...
                for (pos = sessions_.begin(); pos != sessions_.end(); ) {
                    if ( (*pos)->matchesFilter( msg ) ) {
                        // Msg matches - send it off to this guy.
                        packNetCanMsg(msg, buff); // Pack it into byte array.
                        status = send(
                                (*pos)->getReadSock( ),
                                static_cast<void *>(buff), 
                                buffSize, 
                                MSG_NOSIGNAL);

                        if (status == -1) {
                            pos = removeSession(*pos);
                            log_ << Priority::INFO << "SessionCore::"
                                << "writeThreadEntry() - send failed due to: " 
                                << strerror(errno);
                        } else {
                            ++pos;
                        }
                    } else {
                        ++pos;
                    } // End if session matches filter.
                } // End loop over all sessions.
            } catch (const carma::canbus::BufferOverflowException & ex) {
                // Log but keep going...
                log_ << Priority::WARN << ex.what();
            } 
        } // End loop forever
    } // End SessionCore::canMsgWriteThread

    // --------------------------------------
    void SessionCore::processRequestsThread()
    {
        int sresult;
        int nBytesRx;   // Number of bytes received.
        unsigned char buff[16];
        size_t buffSize = sizeof(buff);
        struct requestMsg_t rmsg;
        struct timeval timeout;
        fd_set readfds;
        SessionVector::iterator pos;
        
        CARMA_CPTRACE(TRACE_RX_TX, "SessionCore::processRequestsThread() - "
            "Entering.");

        while (true) {

            CARMA_CPTRACE(TRACE_MUTEX, "processRequestThread() - "
                "Scopelocking sessionMutex_.");

            // Prevent anybody from adding a session under our nose.
            ScopedPthreadMutexLockManager managedScopelock(sessionMutex_);
            managedScopelock.LockMutex();

            // Reset the timeout - the timeout let's us do a couple things
            // 1) Allows new sessions to get their read fd added to readfds.
            // 2) Prevents us from using 100% cpu when readfds is empty.
            // Select can modify timeval so we must reset it.
            timeout.tv_sec = 0;
            timeout.tv_usec = 200000; // 5hz 

            // Prepare the read file descriptor set.
            FD_ZERO(&readfds);

            for (unsigned int i = 0; i < sessions_.size(); i++)     
                FD_SET(sessions_[i]->getWriteSock(), &readfds);

            managedScopelock.UnlockMutex();

            // blocks until sockets become readable.
            sresult = select(maxFd_ + 1, &readfds, NULL, NULL, &timeout);

            managedScopelock.LockMutex();

            pos = sessions_.begin();
            while ( pos != sessions_.end() && sresult > 0) {
                if ( FD_ISSET( (*pos)->getWriteSock(), &readfds) ) {
                    int status;
                    int wsd = (*pos)->getWriteSock();

                    // Clear this file descriptor in case of retry
                    FD_CLR(wsd, &readfds);
                    sresult--;

                    // Read a message from the client until we get it all
                    nBytesRx = 0;
                    while ( static_cast<size_t>(nBytesRx) < buffSize ) {
                        status = recv(
                                wsd, 
                                &buff[nBytesRx], 
                                buffSize-nBytesRx, 
                                MSG_NOSIGNAL);
                        if ( status > 0 )
                            nBytesRx += status;
                        else 
                            break;
                    }

                    // Make sure read went smoothly...  If not, delete session
                    if ( status == 0 ) {       // Client terminated
                        pos = removeSession( *pos );
                    } else if ( status < 0 ) { // Other Badness
                        pos = removeSession( *pos );
                        log_ << Priority::INFO << "SessionCore::"
                            << "requestThreadEntry() - recv failed: "
                            << strerror(errno);
                    } else {                   // Success
                        rmsg = unpackRequestMsg(buff);
                        processRequest(rmsg, *pos );
                        ++pos;
                    }
                } else {
                    ++pos;
                } // End if FD_ISSSET
            } // End loop over sessions_
        } // End loop forever
    } // End SessionCore::processRequestsThread
    
} // End anonymous namespace

// -----------------------------------------------------------------------------
Session::Session(int readSock, int writeSock) 
    : 
    rsd_(readSock), 
    wsd_(writeSock),
    log_(Program::getLogger())
{

    CPTRACE(TRACE_CTOR_DTOR, "Session() - Begin.");

    int status;
    socklen_t len = sizeof(ra_.sa);
    
    // Set the addresses for the sockets
    status = getpeername(rsd_, &ra_.sa, &len);
    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::SystemException,
            "Session::Session() - Error retrieving socket name " 
            + (string)strerror(status));
    }

    status = getpeername(wsd_, &wa_.sa, &len);
    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::SystemException, 
            "Session::Session() - Error retrieving socket name " 
            + (string)strerror(status));
    }
    
    // Initialize mutex...
    status = pthread_mutex_init(&mutex_, NULL); 
    if (status != 0) {
        throw CARMA_EXCEPTION(carma::canbus::PthreadFailException,
            "Session::Session() - Couldn't initialize mutex." 
            + (string)strerror(status));
    }

    SessionCore::instance().addSession(this);

    log_ << Priority::NOTICE << "Session initiated with client " 
         << inet_ntoa(wa_.sa_in.sin_addr);
    
    CPTRACE(TRACE_CTOR_DTOR, "Session() - End.");
}

// -----------------------------------------------------------------------------
Session::~Session()
{
    CPTRACE(TRACE_CTOR_DTOR, "~Session() - Begin.");

    // Shutdown the rsd_ and wsd_ sockets.
    shutdown(rsd_, SHUT_RDWR);
    shutdown(wsd_, SHUT_RDWR);
    close(rsd_);
    close(wsd_);

    // TODO: Check the return value of these guys to make sure that close was
    // successful.  If not, try again....
    
    log_ << Priority::NOTICE << "Session terminated with client " 
         << inet_ntoa(wa_.sa_in.sin_addr);
    CPTRACE(TRACE_CTOR_DTOR, "~Session() - End.");

}

// -----------------------------------------------------------------------------
void Session::addFilter(FilterType filter)
{
    ostringstream os;
    os << "Setting filter to: ac 0x" << hex << filter.aCode << ", am 0x" 
       << filter.aMask << ", bus 0x" << filter.busFilter << dec;
    
    log_ << Priority::NOTICE << os.str();

    pthread_mutex_lock(&mutex_);

    if (filters_.size() < MAX_FILTERS) {
        filters_.insert(filters_.end(), filter);
    } else {
        // Nothing - Don't add more filters though. 
        // If there are more than MAX_FILTERS, I'm assuming that
        // either the client has lost track of them, doesn't know 
        // what he's doing, or is trying to crash my server - 
        // regardless, don't let it.
    }

    pthread_mutex_unlock(&mutex_);
}

// -----------------------------------------------------------------------------
void Session::clearFilters( )
{
    pthread_mutex_lock(&mutex_);
    filters_.clear();
    pthread_mutex_unlock(&mutex_);
    log_ << Priority::INFO << "Filters cleared.";
}

// -----------------------------------------------------------------------------
bool Session::matchesFilter(const carma::canbus::Message & msg)
{
    pthread_mutex_lock(&mutex_);

    // Loop over all filters for this Session.
    for (unsigned int i = 0; i < filters_.size(); i++) {

        // Test to see if the msg passes the filters. 
        if ( (   (msg.getId() & ~(filters_[i].aMask)) == 
                 (filters_[i].aCode & ~(filters_[i].aMask))
              && (filters_[i].busFilter == msg.getBusId() || 
                  filters_[i].busFilter == 0xFFFF))) 
        {
            pthread_mutex_unlock(&mutex_);
            return true;
        }
    }
    pthread_mutex_unlock(&mutex_);
    return false;
}

