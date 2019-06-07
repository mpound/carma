
/**
 * @file
 * Control method definitions for Loberotator class
 *
 * @author Steve Scott
 * $Revision: 1.108 $
 * $Date: 2013/04/04 15:11:14 $
 * $Id: Loberotator.cc,v 1.108 2013/04/04 15:11:14 scott Exp $
 *
 * $CarmaCopyright$
 */

// System includes
#include <iomanip>

// CARMA includes
#include "carma/loberotator/Loberotator.h"
#include "carma/loberotator/LoberotatorMaster.h"
#include "carma/canbus/exceptions.h"
#include "carma/canbus/Utilities.h"
#include "carma/monitor/LoberotatorSubsystem.h"
#include "carma/util/ExceptionUtils.h"
#include "carma/util/Program.h"
#include "carma/util/Time.h"
#include "carma/util/Trace.h"
#include "carma/util/SystemException.h"

// Carma Tools includes
#include <log4cpp/Category.hh>
#include <log4cpp/Priority.hh>

using namespace log4cpp;
using namespace std;
using namespace carma;
using namespace carma::canbus;
using namespace carma::util;
using namespace carma::monitor;
using namespace carma::loberotator;

// Static initialization
bool Loberotator::updateEnabled_ = true;

// Shorthand
typedef unsigned int uint;
typedef LoberotatorSubsystem::Channel CHAN;
typedef loberotator::LoberotatorControl LRC;
typedef LRC::WalshAssignmentSeq WALSHSEQ;

// ***************************************************************************
Loberotator::Loberotator( nodeType node,
                          LoberotatorMaster* master,
                          LoberotatorSubsystem* mon) :
        XacDevice(API_ID, node, *master),
        master_(*master),
        mon_(mon),
        logger_(Program::getLogger())
{
    CARMA_CPTRACE(Trace::TRACE7, "Creating Loberotator Object for node: "
       << node );

    traceEnabled_ = false;

    setBoardType(0);
    setSerialNumber(API_ID + node_);

    // Initialize a few persistent monitor points
    if (node > 0) {  // Skip global node (==0)
        int chanIdx = node - 1;
        int boardIdx = getBoardIndex();
        mon->channel(chanIdx).channelNum().setValue(node);
        mon->channel(chanIdx).boardNum().setValue(boardIdx+1);
        mon->board(boardIdx).boardNum().setValue(boardIdx+1);
    }

    // Initialize simulators
    int ni = node - 1;
    phaseSim_         = new SimData(   0, 360, 2*ni, 4.25);
    rateSim_          = new SimData(-250, 250,   ni, 1.25);
    tempSim_          = new SimData(36,    44,   ni, 0.02);
    ps5vdSim_         = new SimData(3.8,  6.2,   ni, 0.05);
    ps5vaSim_         = new SimData(3.6,  6.0, 5+ni, 0.05);
    psNeg5vSim_       = new SimData(-7,  -3.6,   ni, 0.05);
    ps24vSim_         = new SimData(20.2, 29.0,  ni, 0.05);
    // Dwell must be > 0, so fix
    int dwell = node;
    if (dwell <= 0)dwell = 1;
    psColumnStateSim_ = new SimIntegerData(0, 5, 0, dwell);
    controlStateSim_  = new SimIntegerData(0, 5, 0, dwell);
    ppsStateSim_      = new SimIntegerData(0, 2, 0, dwell);
    hbStateSim_       = new SimIntegerData(0, 2, 0, dwell);
    psStateSim_       = new SimIntegerData(0, 2, 0, dwell);
    dataValidSim_     = new SimIntegerData(0, 2, 0, dwell);
    timeOffsetSim_    = new SimIntegerData(-120, 140, 0, dwell);

    // Initialize
    if (node > 0) {
        enableFringeTracking(true);
        setOffsetControl(false);
        setOffsetPhase(0);
        setOffsetRate(0);
    }


    // Initialize delay and frequency variables
    delayInterp_.empty();
    LO1Freq_    = 100.0;
    multiplier_ = 1;
    divisor_    = 1;
    sign_       = 1;
}

// ******************************************************************************
Loberotator::~Loberotator()
{
    delete phaseSim_;
    delete rateSim_;
    delete tempSim_;
    delete ps5vdSim_;
    delete ps5vaSim_;
    delete psNeg5vSim_;
    delete ps24vSim_;
    delete psColumnStateSim_;
    delete controlStateSim_;
    delete ppsStateSim_;
    delete hbStateSim_;
    delete psStateSim_;
    delete dataValidSim_;
    delete timeOffsetSim_;
}


// **************************************************************************
// ********************* Begin External Interface ***************************
// **************************************************************************



// ******************************************************************************
void Loberotator::updateDelayAndFreq(
    const LRC::DelayFreqPacket& update)
try {

    // Bounds check channel numbers, and throw if any are incorrect
    for (uint i = 0; i < update.delaySeq.length(); i++) {
        checkChanRange( update.delaySeq[i].channelID );
    }

    // Bounds check all parameters
    // An exception is throw containing all detected errors
    // The exception prevents the use of *any* of the parameters, on any chan
    ostringstream o;
    o << setiosflags(ios::fixed);
    double f = update.frequency;
    int    m = update.multiplier;
    int    d = update.divisor;
    int    s = update.sign;
    const double fLow  = 10.0;  // GHz
    const double fHigh = 350.0;  // GHz
    if ((f < fLow) ||(f > fHigh)) {
        if (o.tellp() > 0)o << "\n";
        o << "Frequency("
          << setprecision(1) << f << ") is out of range ["
          << fLow << "-" << fHigh << "]";
    }
    int mHigh = 30;
    if ((m < 1) ||(m > mHigh)) {
        if (o.tellp() > 0)o << "\n";
        o << "Multiplier(" << m
          << ") is out of range [1-" << mHigh << "]";
    }
    int dHigh = 30;
    if ((d < 1) ||(d > mHigh)) {
        if (o.tellp() > 0)o << "\n";
        o << "Divisor(" << d
          << ") is out of range [1-" << dHigh << "]";
    }
    if ((s != 1) && (s != -1)) {
        if (o.tellp() > 0)o << "\n";
        o << "Sign(" << s << ") is not equal to +1 or -1";
    }
    if (o.tellp() > 0) {
        throw CARMA_EXCEPTION(UserException, o.str().c_str());
    }

    if (false && cmdLoggingEnabled()) {
        // Create log msg
        ostringstream o;
        o << setiosflags(ios::fixed) << boolalpha
          << "updateDelayAndFreq<chan/walshCol(delay,mjd)[3]>(";
        for (uint i = 0; i < update.delaySeq.length(); i++) {
            if (i != 0) o << ", ";
            o << update.delaySeq[i].channelID << "/"
              << update.delaySeq[i].walshColumn << "/";
            for (int t=0; t<3; t++) {
                o << "("
                  << setprecision(5) << update.delaySeq[i].triplet[t].delay
                  << ","
                  << setprecision(5) << update.delaySeq[i].triplet[t].mjd
                  << ")";
                if (t < 2) o << "/" ;
            }
        }
        o << ")";
        o << fixed << boolalpha
          << "<freq/mul/div/sign>(";
        o << setprecision(6) << update.frequency << "/"
          << update.multiplier << "/"
          << update.divisor << "/"
          << update.sign
          << ")";
        cmdlog() << o.str();
    }

    // Set delays
    for (uint i = 0; i < update.delaySeq.length(); i++) {
        setDelay(update.delaySeq[i]);
        // Send the walsh column
        int chan        = update.delaySeq[i].channelID;
        Loberotator& l  = lr(chan-1);
        short col       = update.delaySeq[i].walshColumn;
        l.sendWalshColumn(static_cast<unsigned char>(col));
     }

    // stash LO Freq, multiplier, divisor and sign
    for (uint i = 0; i < update.delaySeq.length(); i++) {
        int chan        = update.delaySeq[i].channelID;
        Loberotator& l  = lr(chan-1);
        l.LO1Freq_      = update.frequency;
        l.multiplier_   = update.multiplier;
        l.divisor_      = update.divisor;
        l.sign_         = update.sign;
        // Monitor system values
        CHAN& monChan = getMonCommandChannel(chan);
        monChan.loFreq().setValue(l.LO1Freq_);
        monChan.multiplier().setValue(l.multiplier_);
        monChan.divisor().setValue(l.divisor_);
        monChan.sign().setValue(l.sign_);
    }
} catch (...) {
    rethrowCaughtAsUser();
}

// ******************************************************************************
void Loberotator::assignWalshColumn(const WALSHSEQ& walshAssignments)
try {
    // Log the command
    ostringstream o;
    o << "assignWalshColumn<chan/col>(";
    std::string sep = "";
    for (uint i = 0; i < walshAssignments.length(); i++) {
        int chan  = walshAssignments[i].channel;
        short col = walshAssignments[i].walshColumn;
        o << sep << chan << "/" << col ;
        sep = ",";
    }
    o << ")";
    cmdlog() << o.str();

    // Go through each element of sequence and set walsh column
    for (uint i = 0; i < walshAssignments.length(); i++) {
        int chan        = walshAssignments[i].channel;
        Loberotator& l  = lr(chan-1);
        short col       = walshAssignments[i].walshColumn;
        l.sendWalshColumn(static_cast<unsigned char>(col));
    }
} catch ( ... ) {
    rethrowCaughtAsUser();
    // Just in case rethrowCaughtAsUser() is broken
    throw util::UserException("rethrowCaughtAsUser() is broken", __FILE__,
            __LINE__ );
}

// **************************************************************************
// Fringe tracking in this software layer, as opposed to the CANnode
void Loberotator::enableFringeTracking(CORBA::Long chan, bool enable)
{
    checkFullChanRange(chan);
    if (cmdLoggingEnabled()) {
        cmdlog() << "enableFringeTracking(chan=" << chan << ", enable="
                 << boolalpha << enable << ")" ;
    }

    if (chan != 0) {
        return lr(chan-1).enableFringeTracking(enable);
    }
    // Do all channels
    for (int c = 1; c <= Chassis::N_CHAN; c++) {
        lr(c-1).enableFringeTracking(enable);
    }
}

// Should only be called by above method
void Loberotator::enableFringeTracking(bool enable)
{

    fringeTracking_ = enable;

    // Monitor system value
    typedef LoberotatorSubsystem::FringeTrackingMonitorPointEnum TRK;
    CHAN& chan = getMonCommandChannel();
    if (enable) {
        chan.fringeTracking().setValue(TRK::ON);
    }
    else {
        chan.fringeTracking().setValue(TRK::OFF);
    }
}

// ************************Offset stuff**********************************
void Loberotator::setOffsetControl(CORBA::Long chan, bool enable)
{
    checkFullChanRange(chan);
    if (cmdLoggingEnabled()) {
        cmdlog() << "setOffsetControl(chan=" << chan << ", enable="
                 << boolalpha << enable << ")" ;
    }

    // Single channel
    if (chan > 0) {
        return lr(chan-1).setOffsetControl(enable);
    }

    // All channels
    for (int c = 1; c <= Chassis::N_CHAN; c++) {
        lr(c-1).setOffsetControl(enable);
    }
}
// Should only be called by above method
void Loberotator::setOffsetControl(bool enable)
{
    offsetControl_ = enable;
    // Monitor system value
    typedef LoberotatorSubsystem::OffsetPhaseStateMonitorPointEnum STATE;
    CHAN& chan = getMonCommandChannel();
    if (enable) {
        chan.offsetPhaseState().setValue(STATE::ON);
    }
    else {
        chan.offsetPhaseState().setValue(STATE::OFF);
    }
}

void Loberotator::setOffsetPhase(CORBA::Long chan, double phaseOffset)
{
    checkChanRange(chan);
    if (cmdLoggingEnabled()) {
        cmdlog() << "setOffsetPhase(chan=" << chan << ", phaseOffset="
                 << setprecision(2) << phaseOffset << ")" ;
    }

    // Call implementation for this channel
    lr(chan-1).setOffsetPhase(phaseOffset);
}

void Loberotator::setOffsetPhase(double phaseOffset)
{
    // Convert to turns and store
    phaseOffset_ = phaseOffset/360;

    // Monitor system value
    CHAN& chan = getMonCommandChannel();
    chan.offsetPhase().setValue(phaseOffset);
}

void Loberotator::setOffsetRate(CORBA::Long chan, double rateOffset)
{
    checkChanRange(chan);
    if (cmdLoggingEnabled()) {
        cmdlog() << "setOffsetRate(chan=" << chan << ", rateOffset="
                 << setprecision(3) << rateOffset << ")" ;
    }
    lr(chan-1).setOffsetRate(rateOffset);
}
void Loberotator::setOffsetRate(double rateOffset)
{
    rateOffset_ = rateOffset;

    // Monitor system value
    CHAN& chan = getMonCommandChannel();
    chan.offsetPhaseRate().setValue(rateOffset);
}


// **************************************************************************
// Can be to all chans (0) or single chan
void Loberotator::enableDDSfringeTracking(CORBA::Long chan, bool enable)
{
    unsigned char disable = static_cast<unsigned char>(!enable);
    checkFullChanRange(chan);
    if (cmdLoggingEnabled()) {
        cmdlog() << "enableDDSfringeTracking(chan=" << chan << ", enable="
                 << boolalpha << enable << ")" ;
    }

    try {
        sendGuardMessage();
        canbus::Message msg = createMsgToChan(chan, DISABLE_LOBEROTATION);
        msg << disable;
        io_.postMessage(msg);
    }
    catch ( carma::util::ErrorException &e ) {
        throw CARMA_EXCEPTION(UserException, e.what());
    }
}

// **************************************************************************
// Can be to all chans (0) or single chan
void Loberotator::enable90PhaseSwitching(CORBA::Long chan, bool enable)
{
    unsigned char disable = static_cast<unsigned char>(!enable);
    checkFullChanRange(chan);
    if (cmdLoggingEnabled()) {
        cmdlog() << "enablePhaseSwitching90(chan=" << chan << ", enable="
                 << boolalpha << enable << ")" ;
    }
    try {
        // Yes, five guard messages
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        canbus::Message msg = createMsgToChan(chan, DISABLE_PHASESWITCH90);
        msg << disable;
        io_.postMessage(msg);
    }
    catch ( carma::util::ErrorException &e ) {
        throw CARMA_EXCEPTION(UserException, e.what());
    }
}
void Loberotator::enable180PhaseSwitching(CORBA::Long chan, bool enable)
{
    unsigned char disable = static_cast<unsigned char>(!enable);
    checkFullChanRange(chan);
    if (cmdLoggingEnabled()) {
        cmdlog() << "enablePhaseSwitching180(chan=" << chan << ", enable="
                 << boolalpha << enable << ")" ;
    }
    try {
        // Yes, five guard messages
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        canbus::Message msg = createMsgToChan(chan, DISABLE_PHASESWITCH180);
        msg << disable;
        io_.postMessage(msg);
    }
    catch ( carma::util::ErrorException &e ) {
        throw CARMA_EXCEPTION(UserException, e.what());
    }
}
void Loberotator::enablePhaseSwitching(CORBA::Long chan, bool enable)
{
    unsigned char disable = static_cast<unsigned char>(!enable);
    checkFullChanRange(chan);
    if (cmdLoggingEnabled()) {
        cmdlog() << "enablePhaseSwitching(chan=" << chan << ", enable="
                 << boolalpha << enable << ")" ;
    }
    try {
        // Yes, five guard messages
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        canbus::Message msg = createMsgToChan(chan, DISABLE_PHASESWITCH90);
        msg << disable;
        io_.postMessage(msg);
        // Yes, three guard messages
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        sendGuardMessage();
        canbus::Message msg2 = createMsgToChan(chan, DISABLE_PHASESWITCH180);
        msg2 << disable;
        io_.postMessage(msg2);
    }
    catch ( carma::util::ErrorException &e ) {
        throw CARMA_EXCEPTION(UserException, e.what());
    }
}

// **************************************************************************
void Loberotator::resetBoard(CORBA::Long board)
{
    if ((board < 0) || (board > Chassis::N_CHAN/4)) {
        ostringstream o;
        o << "Board number (" << board << ") must be in range [0"
          << "-" << Chassis::N_CHAN/4 << "]" ;
        throw CARMA_EXCEPTION(UserException, o.str().c_str());
    }
    if (cmdLoggingEnabled()) {
        cmdlog() << "resetBoard(board=" << board  << ")" ;
    }
    try {
        sendGuardMessage();
        canbus::Message msg = createMsgToChan(4*(board-1)+1, NODE_RESET);
        // Message contents avoids accidental resets
        msg << static_cast<unsigned char>(0xE1);
        msg << static_cast<unsigned char>(0x1E);
        msg << static_cast<unsigned char>(0xA5);
        msg << static_cast<unsigned char>(0x5A);
        msg << static_cast<unsigned char>(0xC3);
        msg << static_cast<unsigned char>(0x3C);
        msg << static_cast<unsigned char>(0x96);
        msg << static_cast<unsigned char>(0x69);
        io_.postMessage(msg);
    }
    catch ( carma::util::ErrorException &e ) {
        throw CARMA_EXCEPTION(UserException, e.what());
    }
}

// **************************************************************************
void Loberotator::hardReset()
{
    if (cmdLoggingEnabled()) {
        cmdlog() << "hardReset()" ;
    }
    master().reset();
}

// ******************************************************************************
void Loberotator::enableTrace(CORBA::Long chan, bool enable)
{
   checkFullChanRange(chan);
   // Don't log this command, it is not pertinent to the control state

    // Single channel
    if (chan > 0) {
        lr(chan-1).traceEnabled_ = enable;
        return;
    }

    // All channels
    for (int c = 0; c <= Chassis::N_CHAN-1; c++) {
        lr(c).traceEnabled_ = enable;;
    }
}
// ******************************************************************************
void Loberotator::enableUpdate(bool enable)
{
    if (cmdLoggingEnabled()) {
        cmdlog() << "enableUpdate(" << enable << ")" ;
    }
    Loberotator::updateEnabled_ = enable;
}

// ******************************************************************************
void Loberotator::loadPhaseSwitchColumn(CORBA::Long chan, short columnId)
{

    checkChanRange(chan);
    nodeType inputId = chan;

    const int nStates = 1024;
    const int cols180 = getPhaseTable180()->getNumColumns();
    const int cols90  = getPhaseTable90()->getNumColumns();
    const int COLUMN_MAX = min(cols90, cols180);
    if ((columnId < 1) || (columnId > COLUMN_MAX)) {
        ostringstream o;
        o << "ColumnId number (" << columnId << ") must be in range [1"
          << "-" << COLUMN_MAX << "]" ;
        throw CARMA_EXCEPTION(UserException, o.str().c_str());
    }

    if (cmdLoggingEnabled()) {
        cmdlog() << "loadPhaseSwitchColumn("
                 << chan << ", " << columnId << ")" ;
    }

    // Create an array with all of the data for download
    // Assumes 1024 2-bit phase states, 4 2-bit states per byte,
    // and 7 data bytes per message.
    // NOTE: James Lamb and Steve Scott have agreed to not use the
    // checksum (but it still part of the API). The checksum
    // is not computed correctly here.
    int nMessages = static_cast<int>(ceil(static_cast<double>(nStates)/(4*7)));
    int psTick  = 0;
    int nDataBytes = nMessages*7;
    unsigned char psdata[nDataBytes];
    unsigned char crc = 0;
    const int len180 = getPhaseTable180()->getNumRows();
    const int len90  = getPhaseTable90()->getNumRows();
    const int cIndex = columnId -1;
    for (int d=0; d<nDataBytes; d++) {
        unsigned char temp = 0;
        for (int i=0; i<4; i++) {
            temp = temp << 2;
            int i180 = psTick%len180;
            int i90  = (psTick/len180)%len90; // 180 nested inside 90
            temp |= 2*getPhaseTable90()->getState(cIndex, i90) +
                      getPhaseTable180()->getState(cIndex, i180);
            psTick++;
        }
        psdata[d] = temp;
        crc += temp;
    }


    try {
        idType id = createId(false, getApi(), inputId, BEGIN_LOAD_COLUMN );
        vector<byteType> data;

        carma::canbus::Message msg( id, getBusId() );
        uShortToData( data, nStates);
        uByteToData( data, crc );
        uByteToData( data, 0x00 );
        uLongToData( data, 0xDEADBEEF );
        msg.setData( data );
        io_.postMessage( msg );

        // Give xac time to process and remove message from queue
        sendGuardMessage();

        idType payloadId = createId(false, getApi(), inputId, LOAD_COLUMN);
        carma::canbus::Message payload( payloadId, getBusId() );

        for ( int i = 0, index = 0; i < nMessages; i+=7, index++ ) {
            data.resize(0);
            uByteToData( data, index );
            uByteToData( data, psdata[i+6] );
            uByteToData( data, psdata[i+5] );
            uByteToData( data, psdata[i+4] );
            uByteToData( data, psdata[i+3] );
            uByteToData( data, psdata[i+2] );
            uByteToData( data, psdata[i+1] );
            uByteToData( data, psdata[i] );
            payload.setData( data );
            io_.postMessage( payload );
            // Give xac time to process and remove message from queue
            sendGuardMessage();
        }

    }
    catch ( carma::util::ErrorException &eex ) {
        throw CARMA_EXCEPTION(UserException, eex.what());
    }
}

// ********************* End of external interface *********************

// **************************************************************************
LoberotatorSubsystem& Loberotator::mon()
{
    return *mon_;
}

// **************************************************************************
// Single channel version
void Loberotator::setDelay(
    const loberotator::LoberotatorControl::DelayChan & delayChan )
{
    Loberotator& l = lr(delayChan.channelID - 1);
    l.delayInterp_.lock();
    l.delayInterp_.empty();
    ostringstream os;
    for (unsigned short i = 0; i < 3; i++) {
        l.delayInterp_.extend(
            delayChan.triplet[i].mjd, delayChan.triplet[i].delay
        );
    }
    l.delayInterp_.unlock();
    delayUpdateTimestamp_ = Time::MJD();
}


// ***************************************************************************
// Computes new phase and rates for this channel and sets local variables.
// The phase & rate are sent to the hw with the independent sendPhaseAndRate()
// Phase is for start of next Frame
// Rate is diff between nextFrame+1 & nextFrame, using nomenclature
// next and nextPlus
void Loberotator::updatePhaseAndRate()
{
    // We want time of the next frame and the one after that
    int    currentFrame     = Time::computeCurrentFrame();  // frames
    double mjdNextFrame     = Time::MJD(currentFrame + 1);  // mjd
    double mjdNextFramePlus = Time::MJD(currentFrame + 2);  // mjd

    // compute phase and phase rates (freq)s
    double delayNext;
    double delayNextPlus;
    double phiNext     = 0;
    double phiNextPlus = 0;

    delayInterp_.lock();
    delayNext     = delayInterp_.evaluate(mjdNextFrame);
    delayNextPlus = delayInterp_.evaluate(mjdNextFramePlus);
    delayInterp_.unlock();

    if (fringeTracking_) {
        // convert delay to phase in turns
        phiNext     = -1.0 * LO1Freq_ * delayNext ;
        phiNextPlus = -1.0 * LO1Freq_ * delayNextPlus;
    }

    // If offset phases and rates are enabled, add an offset phase
    if (offsetControl_) {
        double offsetNext;
        double offsetNextPlus;

        offsetNext  =
            phaseOffset_ + Time::SECONDS_PER_DAY*mjdNextFrame*rateOffset_;
        offsetNextPlus =
            phaseOffset_ + Time::SECONDS_PER_DAY*mjdNextFramePlus*rateOffset_;
        phiNext     += offsetNext;
        phiNextPlus += offsetNextPlus;
    }

    // Get only the fractional part of the phase
    double dummy;
    double phiNormalized = modf(phiNext, &dummy);
    // Make the range = [0.0 - 1.0]
    if (phiNormalized < 0) phiNormalized += 1;
    double phase = phiNormalized*360; // Convert to degrees
    // compute phase rate in hertz
    // The * 2 is equivalent to / 0.5, but computationally faster..
    double rate = 2 * (phiNextPlus - phiNext);

    // Monitor system update
    CHAN& chan = getMonCommandChannel();
    chan.delay().setValue(delayNext);
    chan.delayRate().setValue(2*(delayNextPlus-delayNext));

    // Set the rate and phase, dev and mul
    // convert phase to millidegrees
    outputPhase_ = static_cast<unsigned long>(phase*1000+0.5);
    // compute phase rate in millihertz
    // The * 2 is equivilent to / 0.5, yet faster..
    outputRate_ = static_cast<long>(round(rate * 1000));
    outputMul_ = multiplier_ * sign_;
    outputDiv_ = static_cast<short>(divisor_);

    if (traceEnabled_) {
        static int count = 0;
        if (count++%traceInterval_ == 0) {
            CARMA_CPTRACE(Trace::TRACE5,
                "LRout(" << node_ << "): "
                << "  " << fringeTracking_ << "/"
                        << delayNext       << "/" << delayNextPlus
                << "  " << offsetControl_  << "/"
                        << phaseOffset_    << "/" << rateOffset_
                << "  " << phiNext         << "/" << phiNextPlus
                << "  " << updateEnabled_  << "/"
                        << outputRate_     << "/" << outputPhase_ << "/"
                        << outputMul_      << "/" << outputDiv_);
        }
    }
}

// *************************************************************************
// No guard messages as timing may be sensitive to extra delay
void Loberotator::sendMulDiv()
{
    if(!updateEnabled_) return;
    // If there is an error this may throw
    canbus::Message msg = createMsgToChan(node_, SET_MULTIPLIER);
    msg << outputMul_ << outputDiv_;
    io_.postMessage(msg);
}

// *************************************************************************
// Assumes phase and rate are already set in member variables
// No guard messages as timing may be sensitive to extra delay
void Loberotator::sendPhaseAndRate()
{
    if(!updateEnabled_) return;
    // If there is an error this may throw
    canbus::Message msg = createMsgToChan(node_, SET_PHASE_AND_RATE);
    msg << outputRate_ << outputPhase_;
    io_.postMessage(msg);
}

// *************************************************************************
void Loberotator::sendWalshColumn(const unsigned char walshColumn)
{
    sendGuardMessage();
    // If there is an error this may throw
    canbus::Message msg = createMsgToChan(node_, SELECT_WALSH_COLUMN);
    msg << walshColumn;
    io_.postMessage(msg);
}

// *************************************************************************
// This message is intentionally sent to a bogus nodeID so that its
// only effect is to create a temporal buffer between messages that
// are going out of the output queue. This prevents over-writing the
// input message in the CAN nodes as they do not have a hardware queue.
// A time delay of one message on the CANbus (100 usec) is enough for the
// nodes to grab the message and stuff it into a memory queue.
void Loberotator::sendGuardMessage()
{
    const int BOGUS_NODE = 125;
    canbus::Message msg = createMsgToChan(BOGUS_NODE, SET_PHASE_AND_RATE);
    const unsigned long bogus1 = 0XDeadBeef;
    const unsigned long bogus2 = 0XFeedCafe;
    msg << bogus1 << bogus2;
    io_.postMessage(msg);
}

// *************************************************************************
int Loberotator::getBoardIndex()
{
    // Node numbers for boards are 1,5,9...
    return (getNode()-1)/4;
}

Loberotator& Loberotator::lr(int chanIndex)
{
    return master().loberotator(chanIndex);
}

void Loberotator::checkChanRange(int chanNo)
{
    if ((chanNo < 1) || (chanNo > Chassis::N_CHAN)) {
        ostringstream o;
        o << "Channel number (" << chanNo << ") must be in range [1"
          << "-" << Chassis::N_CHAN << "]" ;
        throw CARMA_EXCEPTION(UserException, o.str().c_str());
    }
}

void Loberotator::checkFullChanRange(int chanNo)
{
    if ((chanNo < 0) || (chanNo > Chassis::N_CHAN)) {
        ostringstream o;
        o << "Channel number (" << chanNo << ") must be in range [0"
          << "-" << Chassis::N_CHAN << "]" ;
        throw CARMA_EXCEPTION(UserException, o.str().c_str());
    }
}

canbus::Message Loberotator::createMsgToChan(int chanNo, canbus::msgType msgID)
{
    canbus::idType id =
        createId(false, getApi(), chanNo, msgID);
    canbus::Message msg(id, getBusId());
    return msg;
}

CHAN& Loberotator::getMonCommandChannel(int c)
{
    return mon().channel(c-1);
}
CHAN& Loberotator::getMonCommandChannel()
{
    return getMonCommandChannel(node_);
}

/**
 * See carma::canbus::getApiId
 */
apiType Loberotator::getApiId()
{
    return API_ID;
}

log4cpp::Category&  Loberotator::log()
{
    return logger_;
}

log4cpp::CategoryStream  Loberotator::cmdlog()
{
    CategoryStream cmdlogger = logger_ << CMD_LOG_PRIORITY ;
    cmdlogger << setiosflags(ios::fixed);
    return cmdlogger;
}

bool Loberotator::cmdLoggingEnabled()
{
    return true;
}

// *************************************************************************
LoberotatorMaster& Loberotator::master()
{
    return master_;
}

// *************************************************************************
SimData& Loberotator::phaseSim()
{
    return *phaseSim_;
}
// *************************************************************************
SimData& Loberotator::rateSim()
{
    return *rateSim_;
}
// *************************************************************************
SimData& Loberotator::tempSim()
{
    return *tempSim_;
}
SimData& Loberotator::ps5vaSim()
{
    return *ps5vaSim_;
}
SimData& Loberotator::ps5vdSim()
{
    return *ps5vdSim_;
}
SimData& Loberotator::psNeg5vSim()
{
    return *psNeg5vSim_;
}
SimData& Loberotator::ps24vSim()
{
    return *ps24vSim_;
}
SimIntegerData& Loberotator::psColumnStateSim()
{
    return *psColumnStateSim_;
}
SimIntegerData& Loberotator:: controlStateSim()
{
    return *controlStateSim_;
}
SimIntegerData& Loberotator::ppsStateSim()
{
    return *ppsStateSim_;
}
SimIntegerData& Loberotator::hbStateSim()
{
    return *hbStateSim_;
}
SimIntegerData& Loberotator::psStateSim()
{
    return *psStateSim_;
}
SimIntegerData& Loberotator::dataValidSim()
{
    return *dataValidSim_;
}
SimIntegerData& Loberotator::timeOffsetSim()
{
    return *timeOffsetSim_;
}


