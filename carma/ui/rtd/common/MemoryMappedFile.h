
#ifndef CARMA_UI_RTD_MEMORYMAPPEDFILE_H_
#define CARMA_UI_RTD_MEMORYMAPPEDFILE_H_

/**
 * @file
 *
 * MemoryMappedFile is a class that provides a C++ interface to the
 * mmap(2) system call.
 *
 * @author Scott Brumbaugh
 * @date February 1999
 *
 * $id: $
 *
 */

#include <sys/mman.h>
#include <iostream>
#include <string>




namespace carma {
    namespace ui {
        namespace rtd { 

 
struct MapfileHeader;

/**
 * Exceptions for MemoryMappedFile's
 */
class MemoryMappedFileException {};

/** 
 *
 * Memory Mapped file is a class that provides a C++ interface to the
 * mmap(2) system call.  Usefulness of the class is increased with the
 * inclusion of interprocess synchronization primatives (locks) based
 * on the descriptor of the mmaped file.  The mmaped region and thus
 * the mmaped file itself is divided into two parts called the header
 * and client regions.  The client region is intended to be read and
 * written by users of the class and is of arbitrary length.  The
 * header portion consists of the first page of memory in the mmaped
 * region, it is intended to provide a reserved space for private and
 * protected object data.
 *  
 * The name of the file to be used as backing store is passed as the
 * first argument to the constructor.  If a file of that name cannot
 * be opened then MemoryMappedFileException is thrown.  A pointer to
 * the start of the client region which extends for 'client_size'
 * bytes should be retrieved using the 'getClientArea' method.
 * 
 * A value of B_TRUE for the 'reuse' argument in the constructor will
 * allow a previously mapped file to be reread if a file of the name
 * 'map_file_name' is found to exist.  In this case the 'client_size'
 * argument must correspond to the 'client_size' of the previous use,
 * otherwise MemoryMappedFileException is thrown.  If 'reuse' is
 * B_FALSE the previous contents and size are discarded.
 *
 * A MemoryMappedFile object provides for mutually exlusive access
 * through the 'lockRead', 'lockWrite', and 'unlock' methods.  All of
 * these methods provide wrappers around the fcntl(2) file descriptor
 * locking services F_SETLKW.  The descriptor operated on in these
 * locking calls is that for the backing store file specified in the
 * constructor.
 *
 *======================================================================
 *
 */
class MemoryMappedFile {
public:
    /**
     * File protection flags
     */
    enum Protection_t {
        NoAccess = PROT_NONE, 
        Read     = PROT_READ, 
        Write    = PROT_WRITE, 
        Exec     = PROT_EXEC
    };
  
    /**
     * @deprecated Trying to eliminate constructor with char* type name
     *
     * @param map_file_name File name for the memory mapped file
     * @param client_size Size of the data area in bytes
     * @param reuse Flag for treating file as empty and overwriting the header
     * @param p_mask File protection mask for reading and or writing
     * @param persistent Flag to use a persistent disk file as opposed to pure shared memory
     *
     */ 
    MemoryMappedFile(const char* map_file_name, 
		      size_t client_size, 
		      bool reuse,
		      Protection_t p_mask = Protection_t (Read | Write),
		      bool persistent = true);
    /**
     * @param map_file_name File name for the memory mapped file
     * @param client_size Size of the data area in bytes
     * @param reuse Flag for treating file as empty and overwriting the header
     * @param p_mask File protection mask for reading and or writing
     * @param persistent Flag to use a persistent disk file as opposed to pure shared memory
     *
     */ 
    MemoryMappedFile(const std::string& map_file_name, 
		      size_t client_size, 
		      bool reuse,
		      Protection_t p_mask = Protection_t (Read | Write),
		      bool persistent = true);

    /// Destructor
    ~MemoryMappedFile();
    /**
    * Lock the file for read access
    */
    bool        lockRead();
    /**
    * Lock the file for write access
    */
    bool        lockWrite();
    /**
    * Unlock the file
    */
    bool        unlock();
    /**
    * Get a pointer to the start of the data area
    */
    void*       getClientArea();
    /**
    * Get the filename
    */
    const char* getFileName();
    /**
    * Get the file descriptor
    */
    int getFileDescriptor() const;

private:
    void init(const char* map_file_name, 
		      size_t client_size, 
		      bool reuse,
		      Protection_t p_mask = Protection_t (Read | Write),
		      bool persistent = true);
    MapfileHeader* header;
    void* client_data;
    char* file_name;
    int   fd;
    bool  realFile;
    bool  debug;
};



}}} // End namespace carma::ui::rtd


#endif // _MEMORYMAPPEDFILE_H_



