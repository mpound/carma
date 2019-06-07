

/*!
 * @file
 * Implementation for a simple class to get the virtual memory size of a process.
 *
 * @author Steve Scott
 * @version $Revision: 1.2 $ $Date: 2008/02/12 22:23:06 $
 *
 * $CarmaCopyright$
 */

#include <fstream>
#include <iomanip>
#include <ios>
#include <iostream>
#include <ostream>
#include <sstream>

//#include <unistd.h>

#include "carma/util/VmSize.h"


using namespace ::std;
using namespace carma::util;

VmSize::VmSize()
{
    pid_ = getpid();
    refresh();
}

VmSize::VmSize(pid_t pid)
{
    pid_ = pid;
    refresh();
}

void VmSize::refresh()
{
    ostringstream o;
    o << "/proc/" << pid_ << "/status";
    ifstream f(o.str().c_str());
    string s, header, units;
    // Go through the file, looking for the right line
    while (getline(f, s)) {
        istringstream istr(s);
        istr >> header; 
        // Is this it?
        if (header == "VmSize:") {
            // yes, get the size
            istr >> vmsize_ >> units;
        }     
    } 
    f.close();
}

int VmSize::getSizeKB()
{
    return vmsize_;
}

double VmSize::getSizeHuman()
{
    if (vmsize_ >= 1000000) return 1e-6*vmsize_;
    if (vmsize_ >= 1000)    return 1e-3*vmsize_;
    return vmsize_;
}

string VmSize::getUnitsHuman()
{
    if (vmsize_ >= 1000000) return "GB";
    if (vmsize_ >= 1000)    return "MB";
    return "KB";
}

string VmSize::getStringHuman(int precision)
{
    ostringstream o;
    if (vmsize_ < 1000) precision -= 2;
    if (precision < 0)precision = 0;
    o  << setiosflags(ios::fixed)
       << setprecision(precision) << getSizeHuman() 
       << " " << getUnitsHuman() ;
    return o.str();
}


