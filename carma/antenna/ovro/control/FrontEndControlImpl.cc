/**
 * @file
 * Class definitions for the FrontEndControlImpl Corba control implementation.
 *
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * Version: $Revision: 1.28 $
 * $Date: 2012/02/15 21:05:00 $
 * $Id: FrontEndControlImpl.cc,v 1.28 2012/02/15 21:05:00 abeard Exp $
 */

// Carma includes
#include "carma/antenna/common/SisReceiver.h"
#include "carma/antenna/common/loggingUtils.h"
#include "carma/antenna/ovro/canbus/AntennaIF.h"
#include "carma/antenna/ovro/control/FrontEndControlImpl.h"
#include "carma/util/BaseException.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/IllegalArgumentException.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"

#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace carma::antenna::common;
using namespace carma::antenna::ovro;
using namespace carma::util;
using namespace log4cpp;
using namespace std;

namespace { // Anonymous for local constants, typedefs and other riff raff.

    short stageToShort(carma::antenna::common::FrontEndControl::Stage stage)
    {
        switch ( stage ) {
            case carma::antenna::common::FrontEndControl::FIRST:
                return 1;
                break;
            case carma::antenna::common::FrontEndControl::SECOND:
                return 2;
                break;
            case carma::antenna::common::FrontEndControl::THIRD:
                return 3;
                break;
            case carma::antenna::common::FrontEndControl::FOURTH:
                return 4;
                break;
            default:
                throw CARMA_EXCEPTION(IllegalArgumentException,
                    "Invalid stage.");
                break;
        }
    }

    string ampToString(carma::antenna::common::FrontEndControl::Amp amp)
    {
        string sAmp; // This can't throw
        switch (amp) {
            case carma::antenna::common::FrontEndControl::RF:
                sAmp = "RF";
                break;
            case carma::antenna::common::FrontEndControl::IF:
                sAmp = "IF";
                break;
            default:
                sAmp = "INVALID";
                break;
        }
        return sAmp; // Copy constructor can't throw either.
    }

    string stageToString(carma::antenna::common::FrontEndControl::Stage stage)
    {
        string sStage;
        switch (stage) {
            case carma::antenna::common::FrontEndControl::FIRST:
                sStage = "FIRST";
                break;
            case carma::antenna::common::FrontEndControl::SECOND:
                sStage = "SECOND";
                break;
            case carma::antenna::common::FrontEndControl::THIRD:
                sStage = "THIRD";
                break;
            case carma::antenna::common::FrontEndControl::FOURTH:
                sStage = "FOURTH";
                break;
            default:
                sStage = "INVALID";
                break;
        }
        return sStage;
    }
} // End namespace <unnamed>

// -----------------------------------------------------------------------------
FrontEndControlImpl::FrontEndControlImpl(
    carma::antenna::common::SisReceiver &sisRx,
    carma::antenna::common::RxControl::Type type,
    carma::antenna::common::RxControl::Pol_Type polType,
    carma::antenna::ovro::AntennaIF & antIF ) :
        sisRx_(sisRx),
        antIF_( antIF ),
        log_(Program::getLogger()),
        rxType_(type)
{
    string polString;
    if ( polType == carma::antenna::common::RxControl::SINGLE )
        polString = "SinglePol";
    else if ( polType == carma::antenna::common::RxControl::LEFTCIRCULAR )
        polString = "LeftCircularPol";
    else if ( polType == carma::antenna::common::RxControl::RIGHTCIRCULAR )
        polString = "RightCircularPol";
    else
        polString = "<unknown>";

    typeString_ = rxType_.rxAsString() + "-" + polString;

    CARMA_CPTRACE(carma::util::Trace::TRACE6, "FrontEndControlImpl() - "
        "Creating LO control object for receiver of type "
        + typeString_ + ".");
}

// -----------------------------------------------------------------------------
FrontEndControlImpl::~FrontEndControlImpl()
{
    // Nothing
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setFrequency(::CORBA::Double freq)
{
    try {
        log_ << Priority::INFO << "FrontEndControlImpl::setFrequency() - "
            << "Called with freq=" << freq << ", for " << typeString_ << ".";
        sisRx_.tuneMixer( static_cast<float>( freq ) );
    } catch (...) {
        logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
    }
}

// -----------------------------------------------------------------------------
carma::antenna::common::IVCurve *
FrontEndControlImpl::getIVCurve( )
{
    auto_ptr< common::IVCurve > answer( new common::IVCurve );

    try {

        common::SisReceiver::IVCurve ivCurve = sisRx_.getIVCurve( );

        ostringstream msg;
        msg << "RxControlImpl::getIVCurve() - Retrieving IV curve with "
            << ivCurve.data.size() << " points ";

        if ( ivCurve.doPowerRequested )
            msg << "with ";
        else
            msg << "without total power.";

        typedef vector< common::SisReceiver::IVPoint >::iterator IvIterator;
        for (IvIterator i = ivCurve.data.begin(); i != ivCurve.data.end(); ++i)
        {
            antenna::common::IVPoint point;
            point.fjd = (*i).fjd;
            point.Ij = (*i).Ij;
            point.Vj = (*i).Vj;

            const CORBA::ULong newIdx = answer->ivSequence.length( );
            answer->ivSequence.length( newIdx + 1 );
            answer->ivSequence[ newIdx ] = point;
        }

        if ( ivCurve.doPowerRequested ) {

            ovro::IFTotalPowerVec totalPower = antIF_.getIFTotalPower( );

            typedef ovro::IFTotalPowerVec::const_iterator TpIterator;

            const TpIterator tpEnd = totalPower.end();
            for ( TpIterator tpi = totalPower.begin( ); tpi != tpEnd; ++tpi ) {

                const CORBA::ULong newTpIdx = answer->totPower.length( );
                answer->totPower.length( newTpIdx + 1 );
                answer->totPower[newTpIdx] = *tpi;
            }


            msg << totalPower.size() << " total power points.";
        }

        logInfoWithRxNdc( rxType_, msg.str( ) );

    } catch (...) {
        logCaughtAsErrorAndRethrowAsUser( log_ );
    }

    return answer.release();
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setSISVj(::CORBA::Float voltage)
{
    try {
        log_ << Priority::INFO << "FrontEndControlImpl::setSISVj() - "
            << "Setting Vj to " << voltage << " mV for " << typeString_ << ".";
        sisRx_.setVj( static_cast<float>( voltage ) );
    } catch (...) {
        logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
    }
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setSISIj(::CORBA::Float current)
{
    try {
        log_ << Priority::INFO << "FrontEndControlImpl::setSISIj() - "
            << "Setting Ij to " << current << " uA for " << typeString_ << ".";
        sisRx_.setIj( static_cast<float>( current ) );
    } catch (...) {
        logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
    }
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::doIVcurve(
    const ::CORBA::Float startVjInMv,
    const ::CORBA::Float stopVjInMv,
    const ::CORBA::Float stepVjInMv,
    const ::CORBA::UShort deltaInMs,
    const ::CORBA::Boolean doPower,
    const ::CORBA::ULong seqNo )
try {
    log_ << Priority::INFO
        << "doIVcurveWithSeqNo( start=" << startVjInMv
        << " stop=" << stopVjInMv
        << " step=" << stepVjInMv
        << " delta=" << deltaInMs
        << " doPower=" << doPower
        << " seqNo=" << seqNo << " );";

    sisRx_.doIVCurve( static_cast<float>(startVjInMv),
            static_cast<float>(stopVjInMv),
            static_cast<float>(stepVjInMv),
            static_cast<int>(deltaInMs),
            seqNo,
            doPower );

} catch ( ... ) {
    logCaughtAndRethrowAsUser( Priority::ERROR );
}



// -----------------------------------------------------------------------------
void FrontEndControlImpl::setVG(
    carma::antenna::common::FrontEndControl::Amp amp,
    carma::antenna::common::FrontEndControl::Stage stage,
    ::CORBA::Float voltage)
{
  try {
    string sAmp(ampToString(amp));
    string sStage(stageToString(stage));
    log_ << Priority::INFO << "FrontEndControlImpl::setVG() - "
      << sAmp << " amplifier " << sStage << " stage "
      << voltage << " V for " << typeString_ << ".";
    sisRx_.setVg( stageToShort( stage ), static_cast<float>( voltage ) );
  } catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setVD(
    carma::antenna::common::FrontEndControl::Amp amp,
    carma::antenna::common::FrontEndControl::Stage stage,
    ::CORBA::Float voltage)
{
  try {
    string sAmp(ampToString(amp));
    string sStage(stageToString(stage));
    log_ << Priority::INFO << "FrontEndControlImpl::setVD() - "
      << sAmp << " amplifier " << sStage << " stage "
      << voltage << " V for " << typeString_ << ".";
    sisRx_.setVd( stageToShort( stage ), static_cast<float>( voltage ) );
  } catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setID(
    carma::antenna::common::FrontEndControl::Amp amp,
    carma::antenna::common::FrontEndControl::Stage stage,
    ::CORBA::Float current)
{
  try {
    string sAmp(ampToString(amp));
    string sStage(stageToString(stage));
    log_ << Priority::INFO << "FrontEndControlImpl::setID() - "
      << sAmp << " amplifier " << sStage << " stage "
      << current << " mA for " << typeString_ << ".";
    sisRx_.setId( stageToShort( stage ), static_cast<float>( current ) );
  } catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
  void FrontEndControlImpl::setMixer(::CORBA::Float voltage)
{
  try {
    log_ << Priority::INFO << "FrontEndControlImpl::setMixer() - "
      << "Called, but not implemented on OVRO receivers!.";
  } catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
  void FrontEndControlImpl::setLoAttenuation(::CORBA::Float atten)
{
  try {
    log_ << Priority::INFO << "FrontEndControlImpl::setLoAttenuation() - "
      << "Setting to " << atten << "% for " << typeString_ << ".";
    sisRx_.setLoAttenuation( static_cast<float>( atten ) );
  } catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
void carma::antenna::ovro::FrontEndControlImpl::getVgap(
    carma::antenna::ovro::FrontEndControl::CurrentMode mode,
    ::CORBA::Float current)
{
  try {
    log_ << Priority::INFO << "FrontEndControlImpl::getVgap() - "
      << "Mode " << mode << " current " << current << " uA for "
      <<  typeString_ << ".";
    SisReceiver::CurrentModeType cmode =
      static_cast<SisReceiver::CurrentModeType>( mode );
    sisRx_.getVgap( cmode, current );
  } catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
  void FrontEndControlImpl::setIgap(::CORBA::Float current)
{
  try {
    log_ << Priority::INFO << "FrontEndControlImpl::setIgap() - "
      << "Setting Igap to " << " current " << current << " uA for "
      <<  typeString_ << ".";
    sisRx_.setIgap( static_cast<float>( current ) );
  } catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setVjLoopMode(
    carma::antenna::ovro::FrontEndControl::VjLoopMode mode)
{
  try {
    log_ << Priority::INFO << "FrontEndControlImpl::setVjLoopMode() - "
      << "Setting vJLoopMode for " <<  typeString_ << ".";
    sisRx_.setVjLoopMode(static_cast<SisReceiver::VjLoopModeType>( mode ));
  } catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
  }
}

// -----------------------------------------------------------------------------
void FrontEndControlImpl::setIjLoopMode(
    carma::antenna::ovro::FrontEndControl::IjLoopMode mode)
{
  try {
    log_ << Priority::INFO << "FrontEndControlImpl::setIjLoopMode() - "
      << "Setting IjLoopMode for " <<  typeString_ << ".";
    sisRx_.setIjLoopMode(static_cast<SisReceiver::IjLoopModeType>( mode ));
  } catch (...) {
    logAndRethrowCaughtExceptionAsUserException( log_, Priority::ERROR );
  }
}

