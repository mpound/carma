/* *
 * @file
 * MonitorPointHeader.cc - Contains method definitions for monitor point 
 * header class.
 *
 * @author: N. S. Amarnath
 *
 * $Id: MonitorPointHeader.cc,v 1.9 2010/07/12 21:43:02 abeard Exp $
 *
 * @CarmaCopyright@
 */

#include <iosfwd>

#include "carma/corba/corba.h"
#include "carma/monitor/MonitorPointSet.h"
#include "carma/monitor/MonitorPointSample.h"
#include "carma/monitor/MonitorPointHeader.h"
#include "carma/util/compileTimeCheck.h"

using namespace carma;
using namespace carma::monitor;
using namespace carma::util;


namespace {


void
compileTimeChecks( )
{
#if __WORDSIZE == 64
    compileTimeCheck< sizeof( MonitorHeader ) == 16 >();
#else
    compileTimeCheck< sizeof( MonitorHeader ) == 8 >();
#endif
}


}  // namespace < anonymous >


//
// MonitorPointHeader methods
//


void
MonitorPointHeader::setNumSamplesPerCycle (ushort nSamples)
{
    if (header_.nSamples == nSamples) return;
    if (set_ != NULL)   {
        set_->setNumSamplesPerCycle (index_, nSamples);
        set_->markMpAtIndexModified (index_);
    }  else
	this->setSamplesPerCycle(nSamples);
}


void
MonitorPointHeader::clearSamples (bool deallocate)
{
    ushort nSamples = this->getNumSamplesPerCycle();

    if (nSamples == 0)  return;
    if (nSamples > 1) nSamples++;

    for (int i = 0;  i < nSamples;  i++)
        this->getMonitorPointSample (i).clear (deallocate);
}


//
// End MonitorPointHeader methods
//

