/**
 *
 * Handles all parameters and calculations for the LO chain.
 *
 * @author: Steve Scott
 *
 * $Id: LOchain.cc,v 1.17 2011/11/09 00:39:36 scott Exp $
 *
 * $CarmaCopyright$
 *
 */


#include <iomanip>
#include <iostream>
#include <sstream>
#include <math.h>

#include "carma/control/LOchain.h"
#include "carma/util/ErrorException.h"

using namespace ::std;
using namespace carma;
using namespace carma::control;
using namespace carma::util;


// ------------------------ Freq class ---------------------------

Freq::Freq( ) :
value_(0.0)
{
}


Freq::Freq( const double val,
            const UNITS  units ) :
value_( 0.0 )
{
    switch( units ) {
        case uHz: value_ = 1e-6 * val;
                break;
        case mHz: value_ = 1e-3 * val;
                break;
        case  Hz: value_ =  val;
                break;
        case KHz: value_ = 1e3 * val;
                break;
        case MHz: value_ = 1e6 * val;
                break;
        case GHz: value_ = 1e9 * val; 
                break;
        default:  // CARMA_ERROR(o);
                break;
    }
}


Freq Freq::operator+(const Freq& rhs) const
{
    return Freq(value_ + rhs.value_, Hz);
}

Freq Freq::operator-(const Freq& rhs) const
{
    return Freq(value_ - rhs.value_, Hz);
}

Freq Freq::operator*(const Freq& rhs) const
{
    return Freq(value_ * rhs.value_, Hz);
}

Freq Freq::operator*(const double& rhs) const
{
    return Freq(value_ * rhs, Hz);
}

Freq Freq::operator/(double div) const
{
    return Freq(value_ / div, Hz);
}

double Freq::div(const Freq& div) const
{
    return value_ / div.value_;
}

double Freq::microhertz() const
{
    return 1e6*value_;
}
double Freq::millihertz() const
{
    return 1e3*value_;
}
double Freq:: hertz() const
{
    return value_;
}
double Freq::kilohertz() const
{
    return 1e-3*value_;
}
double Freq::megahertz() const
{
    return 1e-6*value_;
}
double Freq::gigahertz() const
{
    return 1e-9*value_;
}


// ------------------------ LOchain class ---------------------------
LOchain::LOchain(int mode) :
    oscfreq_(),
    oscOffsetLock_(),
    yigOffsetLock_(),
    refLow_(),
    refHigh_(),
    yigLow_(),
    yigHigh_(),
    preferredHarmonicIndex_(0),
    harmonics_(),
    mode_(mode)
{
}


LOchain::LOchain(const Freq& freq, int mode) :
    oscfreq_(),
    oscOffsetLock_(),
    yigOffsetLock_(),
    refLow_(),
    refHigh_(),
    yigLow_(),
    yigHigh_(),
    preferredHarmonicIndex_(0),
    harmonics_(),
    multiplier_(1),
    mode_(mode)
{
    setOscillatorFreq(freq);
}


LOchain::~LOchain()
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


void
LOchain::setOscillatorFreq(const Freq& freq)
{
    
    Freq newOscFreq = freq;
    
    Freq newOscOffsetLock( 50.00, Freq::MHz ); // Include lock sign
    Freq newYigOffsetLock(-10.00, Freq::MHz ); // Include lock sign
    Freq newRefLow(         1.10, Freq::GHz );
    Freq newRefHigh(        1.26, Freq::GHz );
    Freq newYigLow(         8.0,  Freq::GHz );
    Freq newYigHigh(       12.5,  Freq::GHz );
    bool allowEven = false;
    bool is1cm = (freq.gigahertz() < 50);

    if (is1cm) {
        newRefLow        = Freq( 997, Freq::MHz); // Critical value
        newRefHigh       = Freq( 999, Freq::MHz); // Critical value
    }
    // KLUDGE: Leaving this in until HW decides long term plan (2011/01/19)
    if (is1cm) {
        allowEven = true;
    }

    HarmonicVec newHarmonics =
        computeHarmonics( newOscFreq,
                          newOscOffsetLock,
                          newYigOffsetLock,
                          newRefLow,
                          newRefHigh,
                          newYigLow,
                          newYigHigh );
 
    size_t newPreferredHarmonicIndex =
        findPreferredHarmonic(newHarmonics, newOscFreq, allowEven, mode_);

    std::swap( oscfreq_,       newOscFreq );
    std::swap( oscOffsetLock_, newOscOffsetLock );
    std::swap( yigOffsetLock_, newYigOffsetLock );
    std::swap( refLow_,        newRefLow );
    std::swap( refHigh_,       newRefHigh );
    std::swap( yigLow_,        newYigLow );
    std::swap( yigHigh_,       newYigHigh );

    std::swap(preferredHarmonicIndex_, newPreferredHarmonicIndex);
    harmonics_.swap(newHarmonics);
}

void
LOchain::setOscillatorMultiplier(const int multiplier)
{
    multiplier_ = multiplier;
}    

int
LOchain::getOscillatorMultiplier()
{
    return multiplier_ ;
}    

Freq
LOchain::getOscillatorFreq() const
{
    return oscfreq_;
}

Freq
LOchain::getLOfreq() const
{
    return oscfreq_ * multiplier_ ;
}


Harmonic
LOchain::getPreferredHarmonic() const
{
    return harmonics_.at( preferredHarmonicIndex_ );
}


LOchain::HarmonicVec
LOchain::getAllHarmonics() const
{
    return harmonics_;
}


// Only odd harms
LOchain::HarmonicVec
LOchain::getHarmonics() const
{
    HarmonicVec hvec;
    
    HarmonicVec::const_iterator i = harmonics_.begin();
    const HarmonicVec::const_iterator iEnd = harmonics_.end();
    
    for ( ; i != iEnd; ++i ) {
        if (i->getYigHarmonicNumber() & 1 ) hvec.push_back( *i );
    }

    return hvec;
}


LOchain::HarmonicVec
LOchain::computeHarmonics( const Freq& oscFreq,
                           const Freq& oscOffsetLock,
                           const Freq& yigOffsetLock,
                           const Freq& refLow,
                           const Freq& refHigh,
                           const Freq& yigLow,
                           const Freq& yigHigh )
{
    HarmonicVec result;

    const Freq refMiddle = (refHigh + refLow)/2;
    const Freq yigMiddle = (yigHigh + yigLow)/2;
    const Freq yigHalfSpan = (yigHigh - yigLow)/2;
    const Freq oscMinusOffset = oscFreq - oscOffsetLock;
    const bool is1cm = (oscFreq.gigahertz() < 50);

    // Find max/min yig harmonic numbers
    // Truncate
    const int mMax = static_cast<int>(oscMinusOffset.div(yigLow)); 
    // This needs to be rounded up...
    const int mMin = static_cast<int>(ceil(oscMinusOffset.div(yigHigh)));
    
    result.reserve(mMax - mMin);
    
    for ( int m = mMin; m <= mMax; ++m ) {
        // Compute and store harmonic info
        const Freq yig = oscMinusOffset/m;
        // There may be more than one harmonic of the ref that will work,
        // but we choose only the one closest to the middle.
        const int n =
            static_cast<int>(round((yig - yigOffsetLock).div(refMiddle)));
        const Freq ref = (yig - yigOffsetLock)/n;

        {
            Harmonic h;            
            h.setYigHarmonicNumber(m);
            h.setYigFreq(yig);
            h.setRefHarmonicNumber(n);
            h.setRefFreq(ref);    
            result.push_back(h);
        }
        
        // Find the max tuning range of this harmonic.
        // This is the minimum of the fractional range of the yig or ref.
        double range = 1.00;
        range = min((yigHigh-yig).div(yig), range);
        range = min((yig-yigLow).div(yig),  range);
        range = min((refHigh-ref).div(ref), range);
        range = min((ref-refLow).div(ref),  range);
        
        result.rbegin()->setTuningRange(range);

        if (false) {
            cout << "m=" << m << endl;
            cout << 100*(yigHigh-yig).div(yig) << endl;
            cout << 100*(yig-yigLow).div(yig)  << endl;
            cout << 100*(refHigh-ref).div(ref) << endl;
            cout << 100*(ref-refLow).div(ref)  << endl;
            cout << endl;
        }
        double score = 0.0;
        if (is1cm) {
            score = range*10000;
        }
        else {
            if (range < 1e-5) {
                score = 0.0;
            }
            else {
                double distFromCenter = (yig - yigMiddle).gigahertz();
                double s = 1.0;
                if (distFromCenter < 0) s = 1+distFromCenter/2.25;
                else                    s = 1-(1.25/2.25)*distFromCenter/2.25;                    
                score = 10*s;
            }
        }
        result.rbegin()->setTuningScore(score);

    }
 
    return result;
}


size_t
LOchain::findPreferredHarmonic(const HarmonicVec& harmonics,
                               const Freq&        oscFreq,
                               bool  allowEven,
                               int   mode)
{
    // Find the preferred harmonic; odd (usually), with highest score

    int winnerIndex = 0;
    {
        HarmonicVec::const_iterator i = harmonics.begin();
        const HarmonicVec::const_iterator iEnd = harmonics.end();
    
        HarmonicVec::const_iterator winner = iEnd;
        size_t index = 0;
        
        for ( ; i != iEnd; ++i, ++index ) {
            bool isOdd = (i->getYigHarmonicNumber() & 1);
            if (isOdd || allowEven) {
                if (winner == iEnd) {
                    winner = i;
                    winnerIndex = index;
                }
                else {
                    if (mode == 1) {
                        if (i->getTuningScore() > winner->getTuningScore())  {
                            winner = i;
                            winnerIndex = index;
                        }
                    }
                    else {
                        if (i->getTuningRange() > winner->getTuningRange())  {
                            winner = i;
                            winnerIndex = index;
                        }
                    }
                }
            }
        }
        
        if ( winner == iEnd ) {
            ostringstream oss;
            string msg  = "an odd";
            if (allowEven) msg = "a";
            oss << "LOchain could not find " << msg << " YIG harmonic for " 
                << oscFreq.gigahertz() << "GHz";                
            throw CARMA_ERROR(oss);
        }
    }
    
    return winnerIndex;
}

int LOchain::getNumberHarmonics() const
{
    return static_cast<int>(harmonics_.size());
}



void
LOchain::updateFreq( const Freq & f)
{
    oscfreq_ = f/multiplier_;

    HarmonicVec::iterator i = harmonics_.begin();
    const HarmonicVec::iterator iEnd = harmonics_.end();
    
    for ( ; i != iEnd; ++i ) {
        i->setYigFreq( (getOscillatorFreq() - oscOffsetLock_) /
                        i->getYigHarmonicNumber() );
        i->setRefFreq( (i->getYigFreq() - yigOffsetLock_) /
                        i->getRefHarmonicNumber() );
    } 
}


string
LOchain::toString( const Harmonic & h ) const
{
    ostringstream oss;
    
    oss << setiosflags(ios::fixed)
        << setw(9)<< setprecision(5) << getOscillatorFreq().gigahertz() 
        << " " << setw(2) << h.getYigHarmonicNumber()
        << " " << setw(8) << setprecision(5) << h.getYigFreq().gigahertz() 
        << " " << setw(2) << h.getRefHarmonicNumber()
        << " " << setw(7) << setprecision(5) << h.getRefFreq().gigahertz() 
        << " " << setw(5) << setprecision(2) << 100*h.getTuningRange() << "%"
        << " " << setw(5) << setprecision(2) << h.getTuningScore()
        << " " << setw(2) << h.getRank();

    return oss.str();
}


string
LOchain::toString( const HarmonicVec & harms ) const
{
    ostringstream oss;

    HarmonicVec::const_iterator i = harms.begin();
    const HarmonicVec::const_iterator iEnd = harms.end();\

    for ( ; i != iEnd; ++i )
        oss << toString( *i ) << "\n";

    return oss.str();
}


string LOchain::toStringPreferred() const 
{
    return toString(getPreferredHarmonic());
}
    
string LOchain::toString() const 
{
    return toString(getHarmonics());
}
    
string LOchain::toStringAll() const 
{
    return toString(harmonics_);
}

// ------------------------ Harmonic class ---------------------------

Harmonic::Harmonic() :
yig_(),
ref_(),
yigHarmonicNo_(0),
refHarmonicNo_(0),
tuningRange_(0),
tuningScore_(0),
rank_(0)
{
}


Harmonic::~Harmonic()
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}


Freq Harmonic::getYigFreq() const
{
    return yig_;
}

void Harmonic::setYigFreq(const Freq& yig)
{
    yig_ = yig;
}

Freq Harmonic::getRefFreq() const
{
    return ref_;
}

void Harmonic::setRefFreq(const Freq& ref)
{
    ref_ = ref;
}

int Harmonic::getYigHarmonicNumber() const
{
    return yigHarmonicNo_;
}

void Harmonic::setYigHarmonicNumber(const int harm)
{
    yigHarmonicNo_ = harm;
}

int Harmonic::getRefHarmonicNumber() const
{
    return refHarmonicNo_;
}

void Harmonic::setRefHarmonicNumber(const int harm)
{
    refHarmonicNo_ = harm;
}


double Harmonic::getTuningRange() const
{
    return tuningRange_;
}

void Harmonic::setTuningRange(const double range)
{
    tuningRange_ = range;
}

double Harmonic::getTuningScore() const
{
    return tuningScore_;
}

void Harmonic::setTuningScore(const double score)
{
    tuningScore_ = score;
}

int Harmonic::getRank() const
{
    return rank_;
}

void Harmonic::setRank(const int rank)
{
    rank_ = rank;
}
