#ifndef CARMA_UTIL_VMSIZE_H
#define CARMA_UTIL_VMSIZE_H

/*!
 * @file
 * Header file for a simple class to get the virtual memory size of a process.
 *
 * @author Steve Scott
 * @version $Revision: 1.3 $ $Date: 2008/02/14 17:51:09 $
 *
 * $CarmaCopyright$
 */

#include <fstream>
#include <sstream>

#include <unistd.h>


namespace carma {
    namespace util {


/**
 * Class to get virtual memory size of a process.
 * The information is gotten from the /proc filesystem, in the same
 * way that ps works. Specifically, we use /proc/pid/status.
 * This class could easily be expanded to get other memory quantities
 * that are present in this same file.
 */
class VmSize {
public:
    /**
     * Default constructor; vmsize for current process.
     * This gets the VM size at the time of construction.
     * @see refresh
     */
    VmSize();  
    /**
     * Constructor for a specified process
     * This gets the VM size at the time of construction.
     * @param pid process ID for the VM size.
     * @see refresh
     */
    VmSize(pid_t pid);
    /**
     * Get the current VM size for the process specified in the constructor
     */
    void   refresh();
    /**
     * Returns the last measured size, in KB.
     */
    int    getSizeKB();
    /**
     * Returns last measured size, in human readable units
     * @see getUnitsHuman, getStringHuman
     */
    double getSizeHuman();
    /**
     * Returns the human readable units for last measured size
     * @see getSizeHuman, getStringHuman
     * 
     */
    std::string getUnitsHuman();
    /**
     * Returns a string with size and units in human readable units
     * for last measured vm size.
     * @param precision number of digits to the right of the decimal
     *  (default = 2)
     * @see getSizeHuman, getUnitsHuman
     */
    std::string getStringHuman(int precision = 2);
private:
    pid_t pid_;
    int   vmsize_; //KB  
};    

} }  // End namespace carma::util


#endif    // CARMA_UTIL_VMSIZE_H
