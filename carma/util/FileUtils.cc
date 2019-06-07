/**
 * @file
 * Implementation for some common file functions.
 *
 * @author Original: Dave Mehringer
 *
 * $CarmaCopyright$
 *
 */

#include "carma/util/FileUtils.h"

#include "carma/util/CommonExceptions.h"
#include "carma/util/posixErrors.h"
#include "carma/util/programLogging.h"

#include <cerrno>
#include <dirent.h>
#include <fcntl.h>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sys/stat.h>
#include <sys/types.h>
#include <sstream>
#include <openssl/evp.h>
#include <unistd.h>
#include <vector>

using namespace ::std;
using namespace carma;
using namespace carma::util;


string FileUtils::computeMessageDigest(const string& fileName, 
                                       const DigestType& digestType) {
    EVP_MD_CTX mdctx;
    const EVP_MD *md;
    unsigned char md_value[EVP_MAX_MD_SIZE];
    unsigned md_len, i;
    OpenSSL_add_all_digests();
    string digest;
    switch (digestType) {
    case MD5:
        digest = "md5";
        break;
    case SHA1:
        digest = "sha1";
        break;
    default:
        ostringstream emsg;
        emsg << "Unhandled DigestType " << digestType;
        throw CARMA_ERROR(emsg.str());
    }
    md = EVP_get_digestbyname(digest.c_str());
    if(!md) {
        ostringstream emsg;
        emsg << "Unknown message digest " << digest;
        throw CARMA_ERROR(emsg.str());
    }
    EVP_MD_CTX_init(&mdctx);
    EVP_DigestInit_ex(&mdctx, md, NULL);
    ifstream file (fileName.c_str());
    if (! file.is_open()) {
        ostringstream emsg;
        emsg << "Failed to open " << fileName << " for reading";
        throw CARMA_EXCEPTION(NotFoundException,emsg.str());
    }
    file.seekg (0, ios::beg);
    int bytesStart = file.tellg();
    file.seekg (0, ios::end);
    int bytesEnd = file.tellg();
    int bytesLeft = bytesEnd - bytesStart;
    file.seekg (0, ios::beg);
    int nbytes;
    while ( bytesLeft > 0 ) {
        nbytes = min(bytesLeft,256);
        
        vector< char > buffer( nbytes );

        file.read (&(buffer[ 0 ]),nbytes);
        EVP_DigestUpdate(&mdctx, &(buffer[ 0 ]), nbytes);
        bytesLeft -= nbytes;
    }
    file.close();
    EVP_DigestFinal_ex(&mdctx, md_value, &md_len);
    EVP_MD_CTX_cleanup(&mdctx);
    ostringstream messageDigest;
    for(i = 0; i < md_len; i++) {
        messageDigest << hex << setw(2) << setfill('0')
                      << static_cast< int >( md_value[i] );
    }
    return messageDigest.str();
}

string FileUtils::computeSha1Sum(const string& fileName) {
    return computeMessageDigest(fileName, SHA1);
}


bool
FileUtils::exists( const string & fileName )
{
    struct stat64 statBuf;
    
    // all this really means is that stat failed, which
    // could be for any number of reasons, including
    // the file does not exist.
    
    if ( stat64( fileName.c_str(), &statBuf ) != 0 ) {
        const int savedErrno = errno;
        
        if ( savedErrno != ENOENT ) {
            ostringstream oss;
        
            oss << "FileUtils::exists(" << fileName << "):"
                << " stat gave an unexpected error of " << savedErrno
                << " - " << strerror( savedErrno );
                
            programLogErrorIfPossible( oss.str() );
        }
        
        return false;
    } else
        return true;
}

void 
FileUtils::rename( const std::string & from, const std::string & to )
{
    errno = 0;
    const int result = ::rename( from.c_str( ), to.c_str( ) );
    const int localErrno = errno;

    if ( result == -1 ) throwPosixError( localErrno );
}
    
void 
FileUtils::removeFile( const std::string & target )
{
    errno = 0;
    const int result = unlink( target.c_str( ) );
    const int localErrno = errno;

    if ( result == -1 ) throwPosixError( localErrno );
}

void 
FileUtils::makeDirectory( const std::string & path, const mode_t mode )
{
    errno = 0;
    const int result = ::mkdir( path.c_str(), mode ); 
    const int localErrno = errno;
    if ( result == -1 )
        throwPosixError( localErrno );
}

void
FileUtils::hardLink( const std::string & target, const std::string & link )
{
    errno = 0;
    const int result = ::link( target.c_str(), link.c_str() );
    const int localErrno = errno;
    
    if ( result == -1 ) 
        throwPosixError( localErrno );
}

unsigned FileUtils::lineCount(const std::string& fileName) {
    const int count = 100000;
    
    vector< char > lineBuffer( count );
    
    int fd = open(fileName.c_str(),O_RDONLY);
    if(fd < 0) {
        ostringstream emsg;
        emsg << "Error opening file " << fileName << ": " 
             << getError(fileName);
        throw CARMA_EXCEPTION(ErrorException, emsg.str());
    }   
    int k,lineCount=0;
    while((k = read(fd, &(lineBuffer[ 0 ]), count)) > 0) {
        for(int i=0 ; i < k; i++) {
            if(lineBuffer[ i ] == '\n') {
                lineCount++;
            }
        }
    }
    close(fd);
    return lineCount;
}

off_t FileUtils::size(const string& fileName) {
    struct stat statBuf;
    
    const int status = stat( fileName.c_str(), &statBuf );
    if ( status != 0 ) {
        ostringstream emsg;
        emsg << "Error opening file " << fileName << ": " 
             << getError(fileName);
        throw CARMA_EXCEPTION(ErrorException, emsg.str());
    }

    return statBuf.st_size;
}

off64_t FileUtils::size64(const string& fileName) {
    struct stat64 statBuf;
    
    const int status = stat64( fileName.c_str(), &statBuf );
    if ( status != 0 ) {
        ostringstream emsg;
        emsg << "Error opening file " << fileName << ": " 
             << getError(fileName);
        throw CARMA_EXCEPTION(ErrorException, emsg.str());
    }

    return statBuf.st_size;
}

time_t FileUtils::modificationTime(const std::string& fileName)
{
    struct stat statBuf;
    
    const int status = stat( fileName.c_str(), &statBuf );
    if ( status != 0 ) {
        ostringstream emsg;
        emsg << "Error opening file " << fileName << ": " 
             << getError(fileName);
        throw CARMA_EXCEPTION(ErrorException, emsg.str());
    }

    return statBuf.st_mtime;
}

time_t FileUtils::changeTime(const std::string& fileName)
{
    struct stat statBuf;
    
    const int status = stat( fileName.c_str(), &statBuf );
    if ( status != 0 ) {
        ostringstream emsg;
        emsg << "Error opening file " << fileName << ": " 
             << getError(fileName);
        throw CARMA_EXCEPTION(ErrorException, emsg.str());
    }

    return statBuf.st_ctime;
}


time_t FileUtils::accessTime(const std::string& fileName)
{
    struct stat statBuf;
    
    const int status = stat( fileName.c_str(), &statBuf );
    if ( status != 0 ) {
        ostringstream emsg;
        emsg << "Error opening file " << fileName << ": " 
             << getError(fileName);
        throw CARMA_EXCEPTION(ErrorException, emsg.str());
    }
    
    return statBuf.st_atime;
}


string FileUtils::getError(const string& fileName) {
    string emsg;
    switch(errno) {
    case EEXIST:
        emsg = fileName + " already exists and O_CREAT and O_EXCL were used.";
        return emsg;
    case EISDIR:
        emsg = fileName + " refers to a directory and the access requested "
            + "involved writing (that is, O_WRONLY or O_RDWR is set).";
        return emsg;
    case EACCES:
        emsg = "The requested access to the file is not allowed, or one of ";
        emsg += "the directories in pathname did not allow search (execute) ";
        emsg += "permission, or the file did not exist yet and write access ";
        emsg += "to the parent directory is not allowed.";
        return emsg;
    case ENAMETOOLONG:
        emsg = fileName + " was too long.";
        return emsg;
    case ENOENT:
        emsg = "O_CREAT is not set and the named file does not exist. Or, a ";
        emsg += "directory component in pathname does not exist or is a ";
        emsg += "dangling symbolic link.";
        return emsg;
    case ENOTDIR:
        emsg = "A component used as a directory in pathname is not, in fact, ";
        emsg += "a directory, or O_DIRECTORY was specified and pathname was ";
        emsg += "not a directory.";
        return emsg;
    case ENXIO:
        emsg = "O_NONBLOCK | O_WRONLY is set, the named file is a FIFO and ";
        emsg += "no process has the file open for reading.  Or, the file is ";
        emsg += "a device special file and no corresponding device exists.";
        return emsg;
    case ENODEV:
        emsg = fileName + " refers to a device special file and no ";
        emsg += "corresponding device exists. (This is a Linux kernel bug - ";
        emsg += "in this situation ENXIO must be returned.)";
        return emsg;
    case EROFS:
        emsg = fileName + " refers to a file on a read-only filesystem ";
        emsg += "and write access was requested.";
        return emsg;
    case  ETXTBSY:
        emsg = fileName + " refers to an executable image which is currently ";
        emsg += "being executed and write access was requested.";
        return emsg;
    case EFAULT:
        emsg = fileName + " points outside your accessible address space.";
        return emsg;
    case ELOOP:
        emsg = "Too many symbolic links were encountered in resolving ";
        emsg += fileName + ", or O_NOFOLLOW was specified but " + fileName;
        emsg += "was a symbolic link.";
        return emsg;
    case ENOSPC:
        emsg = fileName + " was to be created but the device containing ";
        emsg += fileName + " has no room for the new file.";
        return emsg;
    case ENOMEM:
        emsg = "Insufficient kernel memory was available.";
        return emsg;
    case EMFILE:
        emsg = "The process already has the maximum number of files open.";
        return emsg;
    case ENFILE:
        emsg = "The limit on the total number of files open on the system ";
        emsg += "has been reached.";
        return emsg;
    default:
        emsg = "Unhandled error code";
        return emsg;
    }
}

void FileUtils::writeMonitorDataFlatFileHeader
    (FILE * & file, const frameType& frameCount, const string& signature) {
    ostringstream os;
    os << "%5d\t%11d\t%" << signature.length() << "s" << ::std::endl;
    // includes newline character
    fprintf(file,os.str().c_str(),
            getMonitorDataFlatFileHeaderLength(signature.length()),
            frameCount,signature.c_str());
}

void FileUtils::readMonitorDataFlatFileHeader 
    (const string& fileName, frameType& frameCount, string& signature) {
    ifstream file (fileName.c_str());
    if (! file.is_open()) {
        ostringstream emsg;
        emsg << "Failed to open " << fileName << " for reading";
        throw CARMA_EXCEPTION(NotFoundException,emsg.str());
    }
    int bufLen = 200;
    char lineBuffer[bufLen];
    file.getline(lineBuffer, bufLen);
    file.close();
    const vector< string > parts =
        StringUtils::tokenize(string(lineBuffer),"\t");
    if(parts.size() != 3) {
        signature = "bad";
        frameCount = 0;
        return;
    }
    frameCount = atoi(parts[1].c_str());
    if(frameCount <= 3e8) {
        //corrupt
        signature = "bad";
        frameCount = 0;
        return;
    }        
    signature = parts[2];
}


unsigned FileUtils::getMonitorDataFlatFileHeaderLength(const unsigned& sigLength) {
    return (19 + sigLength);
}    
    
std::vector<string> FileUtils::scandir( const std::string & dir )
{
    vector<string> dircontents;
    struct dirent **namelist; // Pointer to an array of dirent 

    const int n = ::scandir( dir.c_str( ), &namelist, 0, alphasort );

    if ( n < 0 ) {
        const int savedErrno = errno;
        ostringstream msg;
        msg << "FileUtils::scandir( dir=" << dir << " ) - "
            << strerror( savedErrno ) << ".";
        throw CARMA_EXCEPTION( ErrorException, msg.str( ) );
    } 
        
    for ( int i = 0; i < n; ++i ) {
        dircontents.push_back( string( namelist[i]->d_name ) );
        free( namelist[i] );
    }

    free( namelist );

    return dircontents;
}
