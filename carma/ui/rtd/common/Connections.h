
#ifndef CARMA_UI_RTD_CONNECTIONS_H
#define CARMA_UI_RTD_CONNECTIONS_H

#include <string>
#include "MemoryMappedFile.h"



/*
 * Class definition for the Connections classes. This class keeps track
 * of all connections to the monitoring window package.
 * The memory mapped filename is hardwired to be "connections.shmem"
 * in the mmap directory.
 * 
 * @author Steve Scott
 *
 * $id: $
  *
 * $CarmaCopyright$
 *
 */



namespace carma {
    namespace ui {
        namespace rtd { 




/*
** Define the size of strings and arrays
*/
const int SIZEOF_USERNAME      = 14;
const int SIZEOF_LONGUSERNAME  = 60;
const int SIZEOF_FULLNAME      = 40;
const int SIZEOF_COMPACTNAME   = 40;
const int SIZEOF_LOCATION      = 10;
const int SIZEOF_FULLLOCATION  = 50;
const int SIZEOF_WINDOWNAME    = 20;
const int SIZEOF_CONNECTSTART  = 20;
const int SIZEOF_WINDOWUPDATE  = 10;
const int NUMBEROF_CONNECTIONS = 150;


/**
* Structure definition for a single connection to the system.
*/
struct Cnx {
    /// Server process (unique for each window)
    int     processId; 
    /// Window name 
    char    windowName[SIZEOF_WINDOWNAME];
    /// Username, e.g. jsmith
    char    userName[SIZEOF_USERNAME];
    /// Full username, e.g. "Joe Smith"
    char    fullName[SIZEOF_FULLNAME];
    /// Concatenation of username(fullname)
    char    compactName[SIZEOF_COMPACTNAME];
    /// Nodename w/o domain
    char    location[SIZEOF_LOCATION];  
    /// Nodename w/ domain  
    char    fullLocation[SIZEOF_FULLLOCATION];
    /// UT time of start of connection
    char    connectStartString[SIZEOF_CONNECTSTART];
    /// MJD time of start of connection
    double  connectStartMJD;
    /// Window update rate in hertz
    char    windowUpdateRate[SIZEOF_WINDOWUPDATE];
    /// Flag for guest access mode
    bool    guest;
    /// The user for this window is in control (has usurped)
    bool    hasControl;  
    /// This entry has a valid connection in it
    bool    valid;       
};

/**
* Structure definition for keeping track of all connections, windows, 
* and user control of the system.
*/
struct ConnectionData {
    /// Increment everytime control or cnx change
    int     serialNumber;  
    /// Logical: 1 if someone has control    
    bool    haveControlUser;   
    /// Logical: 1 if a reload is in progress
    bool    reloadInProgress;  
    /// Control user short form, null string if nobody in control
    char    userName[SIZEOF_USERNAME]; 
    /// Control user, includes location, gecos
    char    longUserName[SIZEOF_LONGUSERNAME];  
    /// Array of all of the individual connections
    Cnx     cnx[NUMBEROF_CONNECTIONS];  
};

/**
 * Base class for the connection objects.
 * Implementation is on a memory mapped file with full locking capabilities.
 *
 * @param pid      Program id
 * @param filename Full filename for the memory mapped file
 * @param p        File protection (readonly or read/write)
 */ 
class ConnectionBase {
public:
    /** 
     * Constructor
     */
    ConnectionBase(int pid, const std::string& filename, MemoryMappedFile::Protection_t p);
    /// Destructor
    virtual ~ConnectionBase();
    /// Dump status of all connections to a string
    virtual std::string   toString();
    /// Lock connection database shared memory for read access
    void          lockRead();
    /// Lock connection database shared memory for read access
    void          lockWrite();
    /// Release lock on connection database shared memory
    void          unlock();
    /// Get the serial number of this data set (debugging)
    int getSerialNumber();
    /// Return the maximum number of connections possible
    int getMaxConnections();
protected:
    /// Process id for this connection
    int               pid_;
    /// Pointer to a copy of connect database
    ConnectionData*   connection;
    /// Pointer to the memory mapped file that contains connection database
    MemoryMappedFile* memMap; 
    /// Filename of connection database
    std::string            memoryMappedFilename;
private:
};

/**
 * Class to access connection shared memory object with read/write mode
 */
class ConnectionRW:public ConnectionBase {
public:
    /// Constructor
    ConnectionRW(int pid, const std::string& filename);

    /// Destructor
    virtual ~ConnectionRW() {};
    
    /// Register the window in the connection table
    void registerWindow( const char * windowName,
                         const char * userName,
                         const char * fullName,
                         const char * location,
                         const char * fullLocation,
                         int          guest );
    /// Unregister the window in the connection table
    void unregisterWindow(int pid);
    /// Remove dead processes from the connection table
    void unregisterDeadWindows();
    /// Usurp or abdicates control; returns a message suitable for logging
    char* setControl(bool usurp);
    /// Set window update rate in seconds 
    void setUpdateRate(double updateRate);
    /// Set or clear the reloadInProgress flag 
    void setReloadInProgress(int state);
    /// Get the value of the reloadInProgress flag 
    int getReloadInProgress();
    /// For backdoor debugging only 
    void setPID(int _pid);
private:
    /// Update the control state of individual connections
    void updateCnxControlState();
    /// Control change message
    char controlChangeMessage[200];
};


/**
 * Class to access connection shared memory object with readonly mode
 */
class ConnectionRO:public ConnectionBase {
public:
    /// Constructor
    ConnectionRO(int pid, const std::string& filename);
    /// Destructor
    virtual ~ConnectionRO() {};
     
    /// This method gets the current cnx from cache (refreshing if necessary) 
    Cnx* getThisCnx();
    /// This method gets the connectionData from cache (refreshing if necessary) 
    ConnectionData* getConnectionData(); 
    /// Get the serial number for the cached data (debugging)
    int getCacheSerialNumber();
    /// Return state of this object as a string
    std::string toString();
    
private:
    ConnectionData cachedData;          // The cached data for ro operations
    int            cnxIndex;            // Index for the cnx for this process
};

 
}}} // End namespace carma::ui::rtd


#endif  // CARMA_UI_RTD_CONNECTIONS_H 
