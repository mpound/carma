/**
 * @file
 * Class definition of CalibratorControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.19 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: CalibratorControlImpl.cc,v 1.19 2012/02/15 21:05:00 abeard Exp $
 */
// STL
#include <iostream>

// Carma includes
#include "carma/antenna/ovro/control/CalibratorControlImpl.h"
#include "carma/antenna/ovro/canbus/Optics.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna;
using namespace carma::antenna::common;
using namespace carma::antenna::ovro;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace { // Anonymous namespace for local riffraff

    ::std::string
    calPosToString( CalibratorControl::Position position ) {
        switch ( position ) {
            case CalibratorControl::SKY: return "SKY";
            case CalibratorControl::AMBIENT: return "AMBIENT";
            case CalibratorControl::FIXEDTEMP: return "FIXEDTEMP";
            case CalibratorControl::PARTIAL: return "PARTIAL";
            default: return "< UNKNOWN >";
        }
    }

    Optics::CAL_POS
    calPosToOpticsCalPos( CalibratorControl::Position position ) {
        switch ( position ) {
            case CalibratorControl::SKY: return Optics::SKY;
            case CalibratorControl::AMBIENT: return Optics::AMBIENT;
            case CalibratorControl::FIXEDTEMP: return Optics::HOT;
            case CalibratorControl::PARTIAL: return Optics::PARTIAL;
        }

        throw CARMA_EXCEPTION(UserException, "Unknown position.");
    }

} // End namespace < unnamed >


// -----------------------------------------------------------------------------
CalibratorControlImpl::CalibratorControlImpl(
        carma::antenna::ovro::Optics& optics ) :
    optics_(optics),
    log_(Program::getLogger())
{
    CARMA_CPTRACE(Trace::TRACE6, "CalibratorControlImpl() - Creating ovro "
        "calibrator control object.");
}

// -----------------------------------------------------------------------------
CalibratorControlImpl::~CalibratorControlImpl()
{
    CARMA_CPTRACE(Trace::TRACE6, "~CalibratorControlImpl() - Destroying ovro "
        "calibrator control object.");
}

// -----------------------------------------------------------------------------
void CalibratorControlImpl::setPos(
    common::CalibratorControl::Position position )
{
    const string strPos = calPosToString( position );

    log_ << Priority::INFO << "CalibratorControlImpl::setPos( "
        << " position=" << strPos << " )";

    const Optics::CAL_POS canPos = calPosToOpticsCalPos( position );

    optics_.setCalPosition( canPos );
}

// -----------------------------------------------------------------------------
void CalibratorControlImpl::setPos(
    common::CalibratorControl::Position position,
    ::CORBA::ULong seqNo )
try {
    const bool seqNoFromRx = false; // This sequence number is a cal seq no
    this->setPos( position, seqNo, seqNoFromRx ); 

} catch ( ... ) {
    logCaughtAndRethrowAsUser( Priority::ERROR );
}

// -----------------------------------------------------------------------------
void CalibratorControlImpl::setPos(
    const common::CalibratorControl::Position position,
    const unsigned long seqNo,
    const bool withRxSeqNo )
try {
    const string strPos = calPosToString( position );

    log_ << Priority::INFO << "CalibratorControlImpl::setPos( "
        " position=" << strPos << ", seqNo=" << seqNo << 
        ", withRxSeqNo=" << withRxSeqNo << " )";

    const Optics::CAL_POS canPos = calPosToOpticsCalPos( position );

    optics_.setCalPosition(canPos, seqNo, withRxSeqNo );

} catch (...) {
    logCaughtAndRethrowAsUser( Priority::ERROR );
}
    
