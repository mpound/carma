/**
 *
 * @file   
 * Common file functions
 *
 * @author Dave Mehringer
 * @version $Id: FileUtils.h,v 1.11 2011/12/21 22:57:06 mpound Exp $
 *
 * $CarmaCopyright$
 *
 */

#ifndef CARMA_UTIL_FILEUTILS_H
#define CARMA_UTIL_FILEUTILS_H

#include "carma/util/StringUtils.h"
#include "carma/util/Time.h"

#include <string>
#include <sys/stat.h>
#include <vector>

namespace carma {
    namespace util {

/**
 * Common file functions
 * Additional file-related functions can be added here.
 * This class contains no state.
 *
 */
class FileUtils {
public:

    /**
     * compute the specified message digest for the specified file
     * @param fileName the file for which to compute the digest
     * @param digestType the type of digest to compute
     * @return the message digest
     */
    static std::string computeMessageDigest(const std::string& fileName, 
                                            const DigestType& digestType);

    /**
     * compute the SHA1 sum of the file
     */
    static std::string computeSha1Sum(const std::string& fileName);

    /**
     * does the specified file exist?
     * @param fileName the file for which to check existence
     * @return true if the file exists, false if not
     */
    static bool exists(const std::string& fileName);
    
    /**
     * Rename a file or directory.
     * @param from Current location of file.
     * @param to Location to move file to.
     * @throw ErrorException on failure.
     */
    static void rename( const std::string & from, const std::string & to );

    /**
     * Delete a file.
     * @param target File or directory to remove.
     * @throw ErrorException on failure.
     */
    static void removeFile( const std::string & target );

    /**
     * Make a directory.
     * @param path Pathname of directory.
     * @param mode Access permission mode defaults to rwx for user and group
     * and world readable.
     * @throw ErrorException on failure.
     */
    static void makeDirectory( const std::string & path, 
                               mode_t mode = S_IRWXU | S_IRWXG | S_IROTH );

    /**
     * Create a hard link to a file.
     * @param target Path to target file.
     * @param link Path to link.
     * @throw ErrorException on failure.
     */
    static void hardLink( const std::string & target, 
                          const std::string & link );

    /**
     * get the number of lines in the specified file
     * @param fileName the file name
     * @return the number of lines in the file
     * @throws carma::util::FileNotFoundException if the file doesn't exist or
     *         cannot be read
     */
    static unsigned lineCount(const std::string& fileName);


    /**
     * @Return the total size of the file in bytes
     */
    static off_t size(const std::string& fileName);
    static off64_t size64(const std::string& fileName);

    /**
     * get file error diagnostic message
     * @return the diagnostic message associated with the global variable errno
     */
    static std::string getError(const std::string& fileName);

    /**
     * @return The last time the file was modified by 
     *  e.g. by mknod(2), truncate(2), utime(2) and write(2) 
     *  (of more than  zero  bytes).  
     *  An ErrorException is thrown if the file cannot be examined.
     *
     * @see stat(2)
     */
    static time_t modificationTime(const std::string& fileName);

    /**
     * @return The last time the file was changed by writing or
     * setting inode information (i.e., owner, group, link count, mode).
     * An ErrorException is thrown if the file cannot be examined.
     *
     * @see stat(2)
     */
    static time_t changeTime(const std::string& fileName);

    /**
     * @return The last time the file was accessed,  e.g. by execve(2),
     * mknod(2), pipe(2), utime(2) and read(2)  (of  more  than  zero  bytes).
     * An ErrorException is thrown if the file cannot be examined.
     *
     * @see stat(2)
     */
    static time_t accessTime(const std::string& fileName);

    /**
     * write the header of a monitor data flat file
     * @param file file descriptor
     * @param frameCount the frameCount to write to the header
     * @param signature the tagID/canonical name signature to write to the
     *        header
     */
    static void writeMonitorDataFlatFileHeader
        (FILE * & file, const carma::util::frameType& frameCount,
         const std::string& signature);

    /**
     * read and parse the header of a monitor data flat file
     * @param fileName [in] flat file name
     * @param frameCount [out] the frameCount from the header
     * @param signature [out] the tagID/canonical name signature from the 
     *        header
     */
    static void readMonitorDataFlatFileHeader
        (const std::string& fileName, carma::util::frameType& frameCount,
         std::string& signature);

    /**
     * get the length in bytes, including the new line character, of a
     * monitor data flat file having a signature of the specified length
     * @param sigLength length in bytes of the tagID/canonical name signature
     * @return the length in bytes of the header including the new line
     *         character
     */
    static unsigned getMonitorDataFlatFileHeaderLength(const unsigned& sigLength);
        
    /**
     * Return contents of a directory as a vector of strings.
     * This is a ::scandir wrapper.
     */
    static std::vector<std::string> scandir( const std::string & dir );

};
}}
#endif  // CARMA_UTIL_FILEUTILS_H
