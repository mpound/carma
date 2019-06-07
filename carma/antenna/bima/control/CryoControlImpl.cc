/**
 * @file
 * Class definition for CryoControlImpl.
 *
 * @author Colby Gutierrez-Kraybill
 *
 * $Revision: 1.8 $
 * $Date: 2012/02/21 21:06:58 $
 * $Id: CryoControlImpl.cc,v 1.8 2012/02/21 21:06:58 abeard Exp $
 */

// Carma includes
#include "carma/antenna/bima/control/CryoControlImpl.h"
#include "carma/util/Trace.h"
#include "carma/util/Program.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>


using namespace std;
using namespace log4cpp;
using namespace carma::util;
using namespace carma::antenna::bima;

// -----------------------------------------------------------------------------
CryoControlImpl::CryoControlImpl( )
 : 
    log_(Program::getLogger())
{
    CPTRACE(Trace::TRACE6, "CryoControlImpl() - Creating cryo control object.");
}

// -----------------------------------------------------------------------------
CryoControlImpl::~CryoControlImpl ()
{
    CPTRACE(Trace::TRACE6, "CryoControlImpl() - Destroying cryo "
        "control object.");
}

// -----------------------------------------------------------------------------
void CryoControlImpl::turnCompressor ( carma::antenna::common::SwitchState state )
{

  log_ << Priority::INFO << "turnCompressor( state=" << state
    << " ) - not implemented on bima antennas";
}

// -----------------------------------------------------------------------------
void CryoControlImpl::resetCompressor ()
{
  log_ << Priority::INFO << "Commanded to reset compressor"
    << " - not implmented on bima antennas";
}

// -----------------------------------------------------------------------------
void CryoControlImpl::fillCompressor ()
{
  log_ << Priority::INFO << "Commanded to fill compressor"
    << " - not implmented on bima antennas";
}

// -----------------------------------------------------------------------------
void CryoControlImpl::purgeCompressor ()
{
  log_ << Priority::INFO << "Commanded to purge compressor"
    << " - not implmented on bima antennas";
}

// -----------------------------------------------------------------------------
void CryoControlImpl::reset ()
{
  log_ << Priority::INFO << "Reseting Cyro control"
    << " - not implmented on bima antennas";
}
