/**
 * @file
 * FocusControlImpl Corba control implementation - class definition.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.17 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: FocusControlImpl.cc,v 1.17 2012/02/15 21:05:00 abeard Exp $
 */

// Carma includes
#include "carma/antenna/ovro/canbus/SecondaryMirror.h"
#include "carma/antenna/ovro/control/FocusControlImpl.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::ovro;
using namespace carma::util;
using namespace log4cpp;

// -----------------------------------------------------------------------------
FocusControlImpl::FocusControlImpl(SecondaryMirror& secondary ) :
    secondary_(secondary),
    log_(Program::getLogger())
{
    // Nothing
}

// -----------------------------------------------------------------------------
FocusControlImpl::~FocusControlImpl()
{
    // Nothing
}

// -----------------------------------------------------------------------------
void FocusControlImpl::setX( const ::CORBA::Float position,
                             const ::CORBA::ULong seqNo )
try {
        log_ << Priority::INFO
            << "FocusControlImpl::setX() - Setting X focus to "
            << position << "mm with seqNo " << seqNo << ".";
        secondary_.setXPosition(position, seqNo);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void FocusControlImpl::setY( const ::CORBA::Float position,
                             const ::CORBA::ULong seqNo )
try {
    log_ << Priority::INFO
      << "FocusControlImpl::setY() - Setting Y focus to "
      << position << "mm with seqNo " << seqNo << ".";
    secondary_.setYPosition(position, seqNo);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void FocusControlImpl::setZ( const ::CORBA::Float position,
                             const ::CORBA::ULong seqNo )
try {
    log_ << Priority::INFO
      << "FocusControlImpl::setZ() - Setting Z focus to "
      << position << "mm with seqNo " << seqNo << ".";
    secondary_.setZPosition(position, seqNo);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void FocusControlImpl::doZTracking( const ::CORBA::Boolean tracking,
                                    const ::CORBA::ULong seqNo )
try {
    log_ << Priority::INFO
      << "FocusControlImpl::doZTracking() - doZTracking invoked with "
      << "tracking " << tracking << " and seqNo " << seqNo << ".";
    log_ << Priority::WARN << "FocusControlImpl::doZTracking() - ZTracking "
      << "is not yet implemented!";
    secondary_.doZTracking(tracking, seqNo);
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void FocusControlImpl::cycleLvdtPower()
try {
    log_ << Priority::INFO
      << "FocusControlImpl::cycleLvdtPower() - cycleLvdtPower invoked.";
    secondary_.cycleLvdtPower();
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void FocusControlImpl::stopMotion()
try {
    log_ << Priority::INFO
      << "FocusControlImpl::stopMotion() - Stop motion invoked.";
    secondary_.stopMotion();
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}

// -----------------------------------------------------------------------------
void FocusControlImpl::reset()
try {
    log_ << Priority::INFO
      << "FocusControlImpl::reset() - Reset invoked.";
    secondary_.reset();
} catch (...) {
    logCaughtAsErrorAndRethrowAsUser();
}
