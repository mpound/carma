
// when this include is after the define, then ftruncate is not defined
// on Linux using gcc 2.96
#include <unistd.h>

#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <sstream>
#include <sys/mman.h>
#include <sys/uio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <cerrno>
#include <errno.h>

#include "carma/ui/rtd/common/MemoryMappedFile.h"

using namespace std;
using namespace carma::ui::rtd;


const int MEMORYMAPPEDFILE_MAGIC = 0x00000100;

struct carma::ui::rtd::MapfileHeader {
    int magic;
    unsigned int page_size;
    unsigned int client_size;
};


MemoryMappedFile::MemoryMappedFile(const char* map_file_name, 
				    size_t client_size, 
				    bool reuse,
				    Protection_t p_mask,
				    bool _realFile) : debug(false) {
    init(map_file_name, client_size, reuse, p_mask, _realFile);
}
MemoryMappedFile::MemoryMappedFile(const string& map_file_name, 
				    size_t client_size, 
				    bool reuse,
				    Protection_t p_mask,
				    bool _realFile) : debug(false) {
    if(debug)cerr << "map_file_name= " << map_file_name << endl;
    if(debug)cerr << "client_size= " << client_size << endl;
    init(map_file_name.c_str(), client_size, reuse, p_mask, _realFile);
}

void MemoryMappedFile::init(const char* map_file_name, 
			     size_t client_size, 
			     bool reuse,
			     Protection_t p_mask,
			     bool _realFile) {

    if(debug)cerr << "init: client_size= " << client_size << endl;
    MapfileHeader h;
    struct stat statbuf;
    bool try_old_file = false;
    realFile = _realFile;

    ostringstream errPhraseStream;
    errPhraseStream<<(realFile?"real":"shared memory")<<" file ("
             <<map_file_name<<") for "
             <<(((p_mask&PROT_WRITE)==PROT_WRITE)?"write":"read");
    string errPhrase = errPhraseStream.str();	

    
    int openMask = O_RDONLY;                      // Default to read only
    if ((p_mask & PROT_WRITE) == PROT_WRITE) {
        openMask = O_RDWR | O_CREAT; // Read/write
    }

    h.page_size = sysconf (_SC_PAGESIZE);
    h.client_size = client_size;

    if(debug) cout<<"MemoryMappedFile.init(): filename="<<map_file_name;
    if(debug) cout<<"  pmask="<<p_mask<<"  openMask="<<openMask<<endl;
    
    if (realFile) {
        if (reuse && stat(map_file_name, &statbuf) != -1) {
            try_old_file = true;
        }
        fd = open(map_file_name, openMask, 0666);
    }
    else {
        fd = shm_open(map_file_name, openMask, 0666);
        if (fd < 0) {
            fd = shm_open(map_file_name, O_RDWR | O_CREAT, 0666);
	    if (fd < 0) {
            if (errno == ENOENT) {
				perror("Filename may contain multiple / characters which is illegal.");
			} else {
				perror("File create with shm_open()");
			}
		} else {
                // truncate the file to the correct length
                if( ftruncate(fd, (off_t)(h.page_size + h.client_size)) < 0) {
                    perror("ftruncate");
                }
             }
          }
        else {
            if (reuse)try_old_file = true;
            else {
                if(ftruncate(fd, h.page_size + h.client_size)) {
                    perror("ftruncate");
                }
            }
        }
    }

    // expect to be able to open the named file.
    if (fd < 0) {
        cerr<<"MemoryMappedFile:Couldn't open "<<errPhrase<<endl;	
        throw MemoryMappedFileException(); // file cannot be opened
    }

    /*
     * Reuse a previously mmapped real file.
     *
     * Any of the following are errors:
     * 
     *   The file does not exist in the file system or is not statable by this
     *   process.
     *
     *   The file exists and is statable but the size of the previous value for
     *   MapfileHeader->clientSize differs from the clientSize value passed to
     *   this function.
     *
     */
    if (realFile) {
        if (try_old_file) {

            // the file must be readable by this process
            if (read (fd, &h, sizeof (MapfileHeader)) != sizeof(MapfileHeader)) {
                cerr<<"MemoryMappedFile:Couldn't read from"<<map_file_name<<endl;	
                perror("mmapfile error");
                close (fd);
                throw MemoryMappedFileException ();
            }
            // the file must be recognized as a previously mapped file
            if (h.magic != MEMORYMAPPEDFILE_MAGIC) {
                cerr<<"MemoryMappedFile:Magic number error for "<<map_file_name<<endl;	
                cerr<<"  read "<<h.magic<<", should have been "
                    <<MEMORYMAPPEDFILE_MAGIC<<endl;
                close (fd);
                throw MemoryMappedFileException ();
            }                
 
            // the previously mapped client_size must be the same as the requested size
            if (h.client_size != client_size) {
                cerr<<"MemoryMappedFile:Wrong client area size for "
                    <<map_file_name<<endl;	
                cerr<<" constructor requested "<<client_size
                    <<" bytes but header says file contains "
                    <<h.client_size<<"."<<endl;
                cerr<<" maybe element (structure) size has changed?!?"<<endl;
                close (fd);
                throw MemoryMappedFileException ();
            }

            // the actual file must contain sysconf(_SC_PAGESIZE) + clientSize bytes
            int filesize = client_size + h.page_size;
            if (statbuf.st_size != filesize) {     
                cerr<<"MemoryMappedFile:File size incorrect for "<<map_file_name<<endl;	
                cerr<<"  file size is "<<statbuf.st_size
                    <<" but should be "<<filesize<<endl;
                cerr<<" maybe element (structure) size has changed?!?"<<endl;
                close (fd);
                throw MemoryMappedFileException ();
            }   // End of readable, Real file, old file
        }       //  End of Real file, old file

        /*
         * Using a new file.
         */  
        else {      
            h.magic = MEMORYMAPPEDFILE_MAGIC;
    
            if (debug) cout << "writing new header to file" << endl;
            // write the new header to the file
            if ((write (fd,&h,sizeof(MapfileHeader)) != sizeof(MapfileHeader)) ||
	  
	            // truncate the file to the correct length
	            (ftruncate (fd, h.page_size + h.client_size) < 0)) {
      
                close (fd);
                cout<<"MemoryMappedFile:Couldn't open real file for write"<<endl;	
                perror("mmapfile:");
                throw MemoryMappedFileException ();
            } // End of writable, Real file, new file
        }     // End of Real file, new file
	
        if ((header = reinterpret_cast<MapfileHeader*>
                (mmap (0,	                    // arbitrary placement
	               h.page_size + h.client_size,  // total length to map
	               PROT_READ,                    // read only at first
	               MAP_SHARED,                   // shared between processes  
	               fd,                           // file descriptor
	               0)))                          // map file from the start      
                == MAP_FAILED) {    
            if (debug) cout<<"MemoryMappedFile:Couldn't map real file"<<endl;	
            perror("mmapfile:");
            throw MemoryMappedFileException ();    
        }
        //cout << "finished mmap" << endl;
    }
    // Not a real file (it's shared memory)
    else {
        h.page_size = sysconf(_SC_PAGESIZE);
	
        if (try_old_file) {
            if ((header = reinterpret_cast<MapfileHeader*>
                    (mmap (0,	                     // arbitrary placement
	                   h.page_size + client_size,    // total length to map
	                   PROT_READ,                    // read only at first
	                   MAP_SHARED,                   // shared between processes  
	                   fd,                           // file descriptor
	                   0)))                          // map file from the start      
                    == MAP_FAILED) {    
                cout<<"MemoryMappedFile:Couldn't map shared memory file for read"<<endl;	
                perror("mmapfile:");
                throw MemoryMappedFileException ();    
            }
            // Check magic number
            if (header->magic != MEMORYMAPPEDFILE_MAGIC) {               
                cout<<"MemoryMappedFile:Wrong magic number in shared memory file"<<endl;	
                close (fd);
                throw MemoryMappedFileException ();
            }
            // Check client size
            if (header->client_size != client_size) {               
                cout<<"MemoryMappedFile:Wrong client size in shared memory file"<<endl;	
                close (fd);
                throw MemoryMappedFileException ();
            }
        }       //  End of Shared memory file, old file

        /*
         * Using a new shared memory file.
         */  
        else {      
            h.magic = MEMORYMAPPEDFILE_MAGIC;
            h.page_size = sysconf(_SC_PAGESIZE);
            h.client_size = client_size;
    
            if ((header = reinterpret_cast<MapfileHeader*>
                    (mmap (0,	                       // arbitrary placement
	                   h.page_size + client_size,  // total length to map
	                   PROT_READ|PROT_WRITE,       // must write header
	                   MAP_SHARED,                 // shared between processes  
	                   fd,                         // file descriptor
	                   0)))                        // map file from the start      
                    == MAP_FAILED) {    
                cout<<"MemoryMappedFile:Couldn't map shared memory file for write"<<endl;	
                perror("mmapfile:");
                throw MemoryMappedFileException ();    
            }
           
            memcpy(header, &h, sizeof(h));
        }     // End of Real file, new file
    } 
     
    if (debug) cout << "sizeof(h)= " << sizeof(h) << endl;
    client_data = reinterpret_cast<caddr_t> (header) + h.page_size; 
    if (debug) cout << "client_data= " << client_data << endl;

    // modify the page protections to allow write to the client area
    // rh
    if (mprotect (reinterpret_cast<caddr_t> (client_data), h.client_size, p_mask) < 0) {    
        throw MemoryMappedFileException ();			 
    }      

    // If a new file is being used, clear the client area
    if (try_old_file == false) {
        //cout << "starting bzero " << endl;
        //cout << "client_size= " << client_size << endl;
        bzero (client_data, client_size);
        //cout << "bzero finished" << endl;
    }

    file_name = new char[strlen (map_file_name) + 1];
    strcpy (file_name, map_file_name);
    //fprintf(stderr, "file_name= %s\n", file_name);

}

MemoryMappedFile::~MemoryMappedFile() {

    if (header) {
        size_t size = header->page_size + header->client_size;
        // rh
        if (munmap (reinterpret_cast<caddr_t> (header), size) < 0) {      
        //if (munmap ((header), size) < 0) {      
            throw MemoryMappedFileException();
        }
        header = 0;
        close(fd);
    }
}

// ***This will fail if the memory mapped file is NFS mounted***
bool MemoryMappedFile::lockRead () {

    struct flock lock_command = {F_RDLCK, 0, 0, 0, 0};

    while (fcntl (fd, F_SETLKW, &lock_command) == -1) {
        if (errno != EINTR) {
            throw MemoryMappedFileException();
        }
    }
    return true;
}

bool MemoryMappedFile::lockWrite () {

  struct flock lock_command = {F_WRLCK, 0, 0, 0, 0};

  while (fcntl (fd, F_SETLKW, &lock_command) == -1) {
      if (errno != EINTR) {
          throw MemoryMappedFileException();
      }
  }

  return true;

}

bool MemoryMappedFile::unlock() {

    struct flock lock_command = {F_UNLCK, 0, 0, 0, 0};

    while (fcntl (fd, F_SETLKW, &lock_command) == -1) {
        if (errno != EINTR) {
            throw MemoryMappedFileException();
        }
    }

    return true;
}

void* MemoryMappedFile::getClientArea() {
    return client_data;
}

const char* MemoryMappedFile::getFileName() {
    return file_name;
}

int MemoryMappedFile::getFileDescriptor() const {
    return fd;
}















