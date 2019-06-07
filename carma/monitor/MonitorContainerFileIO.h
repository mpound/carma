#ifndef CARMA_MONITOR_MONITORSUBSYSTEMFILEIO_H
#define CARMA_MONITOR_MONITORSUBSYSTEMFILEIO_H

#include "carma/monitor/types.h"

#include <iosfwd>
#include <set>
#include <vector>

namespace carma {
namespace monitor {

class MonitorContainer;

/** 
 * Write the contents of a MonitorContainer to the specified file as text.
 * If the file exists, it's contents are overwritten, if not, 
 * this function attempts to create it throwing if not successful. 
 * For each valid monitor point a line is output containing the tagId, 
 * canonical name, type information and the value of the monitor point.
 * Only valid monitor points from the input container are written.
 * @param container Monitor container to write from.
 * @param filename File name of file to write.
 * @param skip Canonical name of subcontainer(s) to skip.
 * @throw ErrorException on failure to write file or other error.
 * @see setContainerFromFile
 */
void writeContainerToFile( 
    const MonitorContainer & container,
    const std::string & filename, 
    const std::vector< std::string > & skip = std::vector< std::string >() );

/**
 * Read the contents of the specified file and populate the input monitor
 * system with data from the file.  
 * @param container Monitor container to restore using data from file.
 * @param filename Name of file containing restore data.
 * @throw ErrorException on failure to open file or other IO error.
 * @throw ErrorException on mismatched container and file contents. 
 * @see writeContainerToFile
 */
void setContainerFromFile( MonitorContainer & container,
                           const std::string & filename );

/**
 * Compare an input monitor container to a file containing a saved version of
 * given monitor container.  A vector of tag ids containing monitor points which
 * HAVE changed is returned. Note that the tagIds may not be persistent if
 * they are generated on the fly and are thus unsuitable for storage.
 * @param container Monitor container to compare file data to.
 * @param filename Name of file containing monitor data to compare container to.
 * @throw ErrorException on failure to open file or other IO error.
 */
std::set< carma::monitor::tagIDType >
compareContainerToFile( MonitorContainer & container,
                        const std::string & filename );

}} // namespace carma::monitor
#endif
