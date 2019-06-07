

#include "carma/ui/rtd/common/Connections.h"
#include <fstream>
#include <iomanip>
#include <stdio.h>
#include <sstream>

#include <string>
#include <cstring>
#include <cstdlib>

using namespace std;
using namespace carma::ui::rtd;


// Constructor
ConnectionBase::ConnectionBase(int pid, 
        const string& filename, MemoryMappedFile::Protection_t prot): 
        pid_(pid), memoryMappedFilename(filename) 
{

    
    //cout<<"Memory mapped filename:"<<memoryMappedFilename<<endl;
    try {
        memMap = new MemoryMappedFile(memoryMappedFilename.c_str(), 
    	     sizeof(ConnectionData), true, prot); 
        connection =  (ConnectionData*) memMap->getClientArea();
    } catch(const MemoryMappedFileException &) {
        cout << "ConnectionBase Constructor: Memory mapped file exception";
        cout << "(" << memoryMappedFilename << ") " << endl;
        perror("mmap");
        exit(EXIT_FAILURE);
    }
}
// Destructor
ConnectionBase::~ConnectionBase() 
{
    delete memMap;
}

int ConnectionBase::getMaxConnections()
{
    return NUMBEROF_CONNECTIONS;
}

void ConnectionBase::lockRead() 
{
    try {
        memMap->lockRead();
    } catch(const MemoryMappedFileException &) {
        cout<<"ConnectionBase::lockRead: Memory mapped file ";
        cout << "("<<memoryMappedFilename<<") " << endl;
        exit(EXIT_FAILURE);
    }
}
void ConnectionBase::lockWrite() 
{
    try {
        memMap->lockWrite();
    } catch(const MemoryMappedFileException &) {
        cout<<"ConnectionBase::lockWrite: Memory mapped file ";
        cout<<"("<<memoryMappedFilename<<") "<<endl;
        perror("mmap");
        exit(EXIT_FAILURE);
    }
}

void ConnectionBase::unlock() 
{
    try {
        memMap->unlock();
    } catch(const MemoryMappedFileException &) {
        cout<<"ConnectionBase::unlock: Memory mapped file exception\n";
        exit(EXIT_FAILURE);
    }
}

string ConnectionBase::toString() 
{
    ostringstream o;
    o.setf(ios::fixed);
    
    lockRead();
    o << "SN:" << setw(6) << connection->serialNumber 
      << "   " << "Connection:";
    if (strlen(connection->userName) <= 0)o <<"nobody";
    else o << connection->userName;
    if (strlen(connection->longUserName) > 0) {
        o << "  " << connection->longUserName;
    }
    o << "\n";
    
    for (int i=0; i<NUMBEROF_CONNECTIONS; i++) {
        Cnx c = connection->cnx[i];
    	o << setw(2) << i << ":";
    	if (c.valid == false) {
    	    o << "Empty\n";
    	}
    	else {
    	    o << setw(5) << c.processId << ": ";
    	    o << setw(12) << c.windowName << ":";
    	    o << c.userName << "/" << c.fullName << "/" << c.compactName 
              << "/" << c.location << "/" << c.fullLocation << "\n";
    	    o << "    Guest:" << (c.guest?"Y":"N");
    	    o << " Control:" << (c.hasControl?"Y":"N");
    	    o << setw(16) << setprecision(5) << c.connectStartMJD
    	      << "  " << c.connectStartString << "\n";
    	}
    }
    unlock();
    return o.str();
}


// Constructor
ConnectionRW::ConnectionRW(int pid, const string& fname):
        ConnectionBase(pid, fname,
            (MemoryMappedFile::Protection_t) 
            (MemoryMappedFile::Read|MemoryMappedFile::Write)) 
{

}

    
// Register a new window 
void
ConnectionRW::registerWindow( const char * const inWindowName, 
                              const char * const inUserName,
                              const char * const inFullName,
                              const char * const inLocation,
                              const char * const inFullLocation,
                              const int          inGuest ) 
{	
    const string PDT("PST8PDT");
    // Fill up a connection object before we go and store it away!
    Cnx c;
    
    c.processId = pid_;

    connection->serialNumber++;

    if ( inWindowName == 0 )
        c.windowName[0] = '\0';
    else {
        strncpy(c.windowName, inWindowName, SIZEOF_WINDOWNAME);
        c.windowName[SIZEOF_WINDOWNAME - 1] = '\0';
    }
    
    if ( inUserName == 0 )
        c.userName[0] = '\0';
    else {
        strncpy(c.userName, inUserName, SIZEOF_USERNAME);
        c.userName[SIZEOF_USERNAME - 1] = '\0';
    }
    
    if ( inFullName == 0 )
        c.fullName[0] = '\0';
    else {
        strncpy(c.fullName, inFullName, SIZEOF_FULLNAME);
        c.fullName[SIZEOF_FULLNAME - 1] = '\0';
    }
    
    ostringstream o;
    o << c.userName;
    if ((strlen(c.userName) > 0) && (strlen(c.fullName) > 0))o << "(";
    if ( strlen(c.fullName) > 0)o<<c.fullName;
    if ((strlen(c.userName) > 0) && (strlen(c.fullName) > 0))o << ")";
    strncpy(c.compactName, o.str().c_str(), SIZEOF_COMPACTNAME);
    c.compactName[SIZEOF_COMPACTNAME - 1] = '\0';
   // delete o.str();
        
    if ( inLocation == 0 )
        c.location[0] = '\0';
    else {
        strncpy(c.location, inLocation, SIZEOF_LOCATION);
        c.location[SIZEOF_LOCATION - 1] = '\0';
    }
    
    if ( inFullLocation == 0 )
        c.fullLocation[0] = '\0';
    else {
        strncpy(c.fullLocation, inFullLocation, SIZEOF_FULLLOCATION);
        c.fullLocation[SIZEOF_FULLLOCATION - 1] = '\0';
    }

    // Enter start time of the connection (in UT)    
    time_t t;
    time(&t);
    c.connectStartMJD = t/86400.0 + 40587;
    //cout<<"ConnectStart:"<<c.connectStartMJD<<endl;
    struct tm* tmstruct;
    // The PST default is only used if there are problems
    const char* tz; 
    // Save current timezone so we can switch back later
    char* tzp = getenv("TZ"); 
    if (tzp) { 
        tz = tzp;
    }
    else tz = PDT.c_str();
    setenv("TZ", "GMT", 1);       // Use UT 
    tmstruct = localtime(&t);
    char timeCode[] = "%d%h/%R"; 
    strftime(c.connectStartString, SIZEOF_CONNECTSTART, timeCode, tmstruct);
    c.connectStartString[SIZEOF_CONNECTSTART - 1] = '\0';
    setenv("TZ", tz, 1);  // Return to local time
    tzset();
    
    c.windowUpdateRate[0] = '\0';
    c.guest      = inGuest;
    c.hasControl = 0;
    c.valid      = true;
    
    // Now store it in first available spot
    lockWrite();
    // But first, remove any dead cnx from the table...
    unregisterDeadWindows();
    // Now back to putting this one in the table
    for (int i=0; i< NUMBEROF_CONNECTIONS; i++) {
        if (connection->cnx[i].valid == false) {
            connection->cnx[i] = c;
            break;
        }
    }
    unlock();
    updateCnxControlState();
}
    
// This method does a /map control shmem/write/unmap 
void ConnectionRW::unregisterWindow(int pid) {
    bool found = false;
    bool debug = false;
    ofstream log;
    int slot = 0;
    
    if (debug) {
        log.open("/tmp/unregWindowDebug.txt");
        log  << "unregisterWindow(): entering" << endl;
    }
    lockWrite();
    connection->serialNumber++;

    for (int i=0; i< NUMBEROF_CONNECTIONS; i++) {
        if (!found) {
            if (debug) {
                //log  << "unregister cnx " << i << " not found: " << endl;
            }
            bool pidMatch = connection->cnx[i].processId == pid;
            if (connection->cnx[i].valid && pidMatch) {
                memset(&(connection->cnx[i]), 0, sizeof(Cnx));
                found = true;
                slot = i;
            }
        }
        if (found) {
            if (debug) {
                //log << "unregister cnx " << i << " found: " << endl;
            }
            // Copy this array down to fill in the hole
            if (i < (NUMBEROF_CONNECTIONS - 1)) {
            	connection->cnx[i] = connection->cnx[i+1];
            }
            else {
                // Make sure the top slot is clear
                memset(&(connection->cnx[i]), 0, sizeof(Cnx));
            }
        }
    }
    if (debug) {
        if (found) log << "Unregistering slot# " << slot << endl;
        else       log << "Window not found to unregister" << endl;
    }
    // If we don't have any connections from the control user, make it nobody
    for (int i=0; i< NUMBEROF_CONNECTIONS; i++) {
        if (connection->cnx[i].valid  &&
            !connection->cnx[i].guest &&
            (strcmp(connection->userName, connection->cnx[i].userName) == 0)) {
                unlock();
                return;  // Found one, we are done
        }
    }
    // No luck, we have no control user, so set accordingly
    connection->haveControlUser = 0;
    strcpy(connection->userName,     "");
    strcpy(connection->longUserName, "");
    unlock();
    if (debug) {
        log << "unregister() exiting" << endl;
        log.close();
    }
}
    
// Removes all dead windows in the connection table (those with an invalid pid)
// No locking is done, so place this inside something else that is locking
// the connection table.
void ConnectionRW::unregisterDeadWindows() {
    bool debug = false;
    ofstream log;
    
    if (debug) {
        log.open("/tmp/unregDeadWindowsDebug.txt");
        log  << "unregisterWindow(): entering" << endl;
    }
    for (int i=0; i < NUMBEROF_CONNECTIONS; i++) {
        if (connection->cnx[i].valid) { 
            int pid = connection->cnx[i].processId;
            // We use an attempt to get the process group as a way to tell
            // whether or not we have a valid process ID
            int pgid = getpgid(pid);
            if (false && debug) {
                 log << "unregDeadWindows(): pid=" << pid 
                     << "  pgid=" << pgid << endl;
            }
            // If the group ID is -1 we have an invalid pid
            if (pgid == -1) {
                // Clear out the connection
                memset(&(connection->cnx[i]), 0, sizeof(Cnx));                
                if (debug) {
                    log << "unregDeadWindows(): unregistering dead pid=" 
                        << pid << endl;
                }
            }
        }
    }
    // Now compress table for any windows that have been removed
    for (int i=0; i < NUMBEROF_CONNECTIONS; i++) {
        if (!connection->cnx[i].valid) {
            // We found an empty slot 
            for (int j=i+1; j < NUMBEROF_CONNECTIONS; j++) {
                // Find the first valid data and copy it down
                if (connection->cnx[j].valid) {
                    if (debug) {
                        log << "moving cnx " << j << " down to cnx " << i
                            << endl;
                    }
                    connection->cnx[i] = connection->cnx[j];
                    // Clear out the connection
                    memset(&(connection->cnx[j]), 0, sizeof(Cnx));   
                    // We only copy down one             
                    break;
                }               
            }
        }
    }
    if (debug) {
        log.close();
    }
}
    
/// Changes control state for this connection 
/// Returns a static string about the control change
char* ConnectionRW::setControl(bool usurp) 
{
    int i;
    //int wantsControl = !!usurp;  // Limit to 1 or 0
    //static char str[200]; 

    lockWrite();
    // If don't want control and nobody has it, exit quietly
    if (!usurp && !connection->haveControlUser) {
        controlChangeMessage[0] = '\0';
        unlock();
        return controlChangeMessage;
    }
            
    // Increment the serial number
    connection->serialNumber++;
    
    // Find the connection for this pid
    Cnx* c;
    // First find the username for this pid
    for (i=0; i< NUMBEROF_CONNECTIONS; i++) {
        c = &(connection->cnx[i]);
        if (c->valid && (c->processId == pid_)) {
            break;
        }
    }

    if (usurp) {
        // Create the usurp message
        std::string s("Control usurped by ");
        s.append(c->userName);
        if (connection->haveControlUser) {
            s += " from ";
            s += connection->userName;
        }
        strcpy(controlChangeMessage, s.c_str());
        
        // Indicate that we have a control user
        connection->haveControlUser = true;
        
        // Set the control userName
        strcpy(connection->userName, c->userName);
        
        // Set the longUserName
        s = c->fullName; // Reuse the string...
        s += "("; 
        s += c->userName;
        s += "@";
        s += c->location;
        s += ")";
        strncpy(connection->longUserName, s.c_str(), SIZEOF_LONGUSERNAME);
        connection->longUserName[SIZEOF_LONGUSERNAME - 1] = '\0';
    }
    else {  
        // This connection is abdicating
        strcpy(controlChangeMessage, "Control abdicated by ");
        strcat(controlChangeMessage,  c->userName);
        
        connection->haveControlUser = false;  
        strcpy(connection->userName, "");
        strcpy(connection->longUserName, "");
    }
    unlock();
    
    updateCnxControlState();
    return controlChangeMessage;
}
// Update the control state of each connection, based on the control userName
void ConnectionRW::updateCnxControlState() 
{
    lockWrite();
    for (int i=0; i< NUMBEROF_CONNECTIONS; i++) {
        Cnx* c = &(connection->cnx[i]);
        if (c->valid) {
             if ( !c->guest && 
                   connection->haveControlUser &&
                   (strcmp(connection->userName, c->userName) == 0)) {
                 c->hasControl = true;
             }
             else {
                 c->hasControl = false;
             }
        }
    }
    unlock();
}

    
// Not currently implemented 
void ConnectionRW::setUpdateRate(double _updateRate) 
{
    connection->serialNumber++;
}

// Set the reloadInProgress flag 
void ConnectionRW::setReloadInProgress(int state) 
{
    lockWrite();
    connection->reloadInProgress = state;
    connection->serialNumber++;
    unlock();
}

// Get the reloadInProgress flag 
int ConnectionRW::getReloadInProgress() 
{
    return connection->reloadInProgress;
}

// For testing use only
void ConnectionRW::setPID(int pid) 
{
    pid_ = pid;
}



// ----------------------------------------------------------------------------
// Readonly data

    
// Constructor
ConnectionRO::ConnectionRO(int pid, const string& fname):
        ConnectionBase(pid, fname, MemoryMappedFile::Read) 
{
    cachedData.serialNumber = -1;
}

// This method gets the current cnx from cache (refreshing if necessary) 
Cnx*          ConnectionRO::getThisCnx() 
{
    return &(getConnectionData()->cnx[cnxIndex]);	
}
    
// This method gets the controlData from cache (refreshing if necessary).
// We use the cache method to minimize locking. We can read and compare
// the serial number before deciding if we need to lock and copy to cache.
// It also stores away the cnxIndex for this cnx. 
ConnectionData*  ConnectionRO::getConnectionData() 
{
    if (connection->serialNumber != cachedData.serialNumber) {
    	lockRead();
    	cachedData = *connection;
    	//memMap.unlock();
    	for (int i=0; i< NUMBEROF_CONNECTIONS; i++) {
    	    if (cachedData.cnx[i].processId == pid_) {
    	        cnxIndex = i;
    	        break;
    	     }
    	 }
    	 unlock();
    }
    return &cachedData;
}

int ConnectionBase::getSerialNumber()
{
    return connection->serialNumber;
}

int ConnectionRO::getCacheSerialNumber()
{
    return cachedData.serialNumber;
}

string ConnectionRO::toString()
{
    ostringstream o;
    o << "CacheSN:" << setw(6) << getCacheSerialNumber() << "    "
      << ConnectionBase::toString();
    return o.str();
}

     
