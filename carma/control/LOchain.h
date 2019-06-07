#ifndef CARMA_CONTROL_LOCHAIN_H
#define CARMA_CONTROL_LOCHAIN_H

/**
 * @file
 *
 * Handles all parameters and calculations for the LO chain.
 *
 * @author: Steve Scott
 *
 * $Id: LOchain.h,v 1.14 2011/11/09 00:39:36 scott Exp $
 *
 * $CarmaCopyright$
 *
 */

#include <vector> 
#include <string> 


namespace carma {
namespace control {


/**
 * Frequency class. Developed because there was no default constructor
 * for the services::Frequency class. But also added some handy division
 * methods. And uses enumerations rather than strings for units for
 * additional type safety.
 */
class Freq
{
public:
    /**
     * Enumeration for units
     */
    enum UNITS {
        uHz,  // microHz
        mHz,  // milliHz
         Hz,
        KHz,
        MHz,
        GHz 
    } ;
    /**
     *  Default constructor
     */
    Freq();
    /**
     * Constructor, specifying freq and units
     * @param val frequency value
     * @param units units of the frequency
     */   
    Freq(double val, UNITS units);
    
    Freq operator+(const Freq& rhs) const;
    Freq operator-(const Freq& rhs) const;
    Freq operator*(const Freq& rhs) const;
    Freq operator*(const double& rhs) const;
    Freq operator/(double div) const;
    
    /**
     * Division operation. One Freq divided by another, returning double.
     * @param div divisor frequency
     * @return ratio of the two frequencies
     */   
    double div(const Freq& div) const;
    
    /**
     * Accessor, microHertz
     */
    double microhertz() const;
    
    /**
     * Accessor, milliHertz
     */
    double millihertz() const;
     
    /**
     * Accessor,  Hertz
     */
    double hertz() const;
     
    /**
     * Accessor,  kiloHertz
     */
    double kilohertz() const;
     
    /**
     * Accessor,  megaHertz
     */
    double megahertz() const;
     
    /**
     * Accessor,  gigaHertz
     */
    double gigahertz() const;
private:
    double value_; // in Hz
};


/**
 * An LOchain harmonic; has self consistent ref and yig frequencies 
 * and harmonics.
 */
class Harmonic 
{
public:
    /**
     * Constructor
     */
    Harmonic();
    
    virtual ~Harmonic();

    /**
     * Get the yig frequency.
     */
    Freq getYigFreq() const;

    /**
     * Set the yig frequency.
     * @param yig frequency 
     */
    void setYigFreq(const Freq& yig);

    /**
     * Get the ref frequency.
     */
    Freq getRefFreq() const;

    /**
     * Set the ref frequency.
     * @param ref frequency 
     */
    void setRefFreq(const Freq& ref);

    /**
     * Get the yig harmonic number.
     */
    int getYigHarmonicNumber() const;

    /**
     * Set the yig harmonic number.
     * @param harm harmonic number 
     */
    void setYigHarmonicNumber(int harm);

     /**
     * Get the ref harmonic number.
     */
    int getRefHarmonicNumber() const;

    /**
     * Set the harmonic number.
     * @param harm harmonic number 
     */
    void setRefHarmonicNumber(int harm);

    /**
     * Get the fractional tuning range
     */
    double getTuningRange() const;

    /**
     * Set the tuning range
     * @param range absolute fractional tuning range
     */
    void setTuningRange(double range);

    /**
     * Get the score for this harmonic (range is [0,10)
     */
    double getTuningScore() const;

    /**
     * Set the tuning score
     * @param score of this tuning
     */
    void setTuningScore(double score);
    
    /**
     * Get the rank for this harmonic (1 is best, 2 is second best, etc)
     */
    int getRank() const;

    /**
     * Set the rank
     * @param rank of this tuning
     */
    void setRank(int rank);
    
    
private:
    Freq   yig_;
    Freq   ref_;
    int    yigHarmonicNo_;    // Yig * harm
    int    refHarmonicNo_;    // ref * harm
    // Minimum absolute fractional tuning range of yig or ref
    double tuningRange_;  
    double tuningScore_;      // Score of this tuning; range=[0,10]
    int    rank_;             // Rank among odd harms (even harms rank=0)
};


/**
 * Handles all parameters and calculations for the CARMA LO chain.
 * All harmonics are calculated when the frequency is set, either
 * with the explicit setOscillatorFreq() method or with the constructor
 * that takes a Freq as input. There is also a method that updates the
 * the yig and ref frequencies while leaving the harmonics fixed. This
 * minimizes any phase effects by avoiding large freq changes in the ref.
 * The preferred harmonic is selected based on the highest score, which
 * is measured by the absolute distance of the YIG from 10.5GHz.
 * Relevant equations and ranges:
 * <pre>
 * Fosc = m*Fyig + 50MHz
 *   with m odd
 * Fyig = n*Fref - 10MHz
 *
 * Ranges (GHz):
 *   yig    = [8.0 - 12.5]
 *   ref    = [1.10 - 1.26] or [0.99 - 1.00] depending on mode
 *   osc3mm = [67 - 117]
 *   osc1mm = [67 - 90]
 * </pre>   
 *
 */
class LOchain 
{

public:
    /**
     * Constructor
     *
     * @param mode - specifies a computation mode
     *        mode=0: normal
     *        mode=1: new way of selecting preferred harmonics
     */
    LOchain(int mode);
  
    /**
     * Constructor, with oscillator frequency specified
     * and compute LO chain.
     * Does not include tripling for 1mm.
     *
     * @param freq Oscillator frequency
     * @param mode - specifies a computation mode; see above
     */
    explicit LOchain(const Freq& freq, int mode=0);
  
    virtual ~LOchain();
    
    /**
     * Set the oscillator (Gunn) frequency and computes LO chain.
     * Does not include tripling for 1mm.
     *
     * @param freq Oscillator frequency
     */
     void  setOscillatorFreq(const Freq& freq);
    
    /**
     * Set the oscillator multiplier to get the LOfreq.
     * This is usually one, except for 1mm, where it is 3.
     *
     * @param multiplier
     */
     void  setOscillatorMultiplier(int multiplier);
    
    /**
     * Get the oscillator multiplier.
    *
     * @see setOscillatorMultiplier
     */
    int  getOscillatorMultiplier();

    /**
     * Get the oscillator (Gunn) frequency.
     */
    Freq getOscillatorFreq() const;

    /**
     * Get the LO frequency.
     */
    Freq getLOfreq() const;

    /**
     * Update the LO frequency. 
     * This in turn updates the oscillator (Gunn) frequency 
     * and computes ref & yig freqs.
     * Does not change the harmonics numbers, to give continuous phase.
     *
     * @param freq LO frequency
     */
     void  updateFreq(const Freq& freq);

    /**
     * Gets the preferred harmonic. <br>
     * In general there are many harmonics for a given frequency
     * and this one chooses the one with the highest score.
     * @see updateFreq
     */
    Harmonic getPreferredHarmonic() const;

    /**
     * Gets the specified harmonic. <br>
     * In general there are many harmonics for a given frequency
     * and this one returns the one specified.
     * @param whichOne Selects the harmonic, where 1 if the highest ranked,
     * 2 is second ranked, etc. 
     * @throws if rank is out of range
     * @see updateFreq, getNumberHarmonics
     */
    Harmonic getHarmonic(int rank) const;

    /**
     * Gets the number of available harmonics. <br>
     * In general there are many harmonics for a given frequency
     * and this one returns the one specified.
     * @param whichOne Selects the harmonic, where 0 if the highest ranked,
     * 1 is next, etc. Defaults to 0. 
     * @throws if whichOne is out of range
     * @see updateFreq, getHarmonic
     */
    int getNumberHarmonics() const;

    /**
     * Dump LOchain parameters for a single harmonic to a string. Diagnostic.
     * @param h harmonic to convert to string
     */
    ::std::string toString( const Harmonic& h ) const;

    /**
     * Dump LOchain parameters for a vector of harmonics to a string. Diagnostic.
     * @param h vector of harmonic pointers  to convert to string
     */
    ::std::string toString( const ::std::vector< Harmonic >& h ) const;

    /**
     * Dump LOchain parameters for the odd YIG harmonics to a string. Diagnostic.
     */
    ::std::string toString() const;

    /**
     * Dump the LOchain parameters for all harmonics to a string. Diagnostic.
     */
    ::std::string toStringAll() const;

    /**
     * Dump the LOchain parameters for the preferred harmonics to a string. 
     * Diagnostic.
     */
    ::std::string toStringPreferred() const;
    
private:
    LOchain(); // Default private constructor
    typedef ::std::vector<Harmonic> HarmonicVec;

    /**
     * Get all odd YIG harmonics and return in vector.
     * The carma hardware is designed to work with odd YIG harmonics.
     */
    HarmonicVec getHarmonics() const;

    /**
     * Get all possible harmonics (even and odd YIG) and return in vector.
     */
    HarmonicVec getAllHarmonics() const;

    static HarmonicVec
    computeHarmonics( const Freq& oscFreq,
                      const Freq& oscOffsetLock,
                      const Freq& yigOffsetLock,
                      const Freq& refLow,
                      const Freq& refHigh,
                      const Freq& yigLow,
                      const Freq& yigHigh);

    static size_t
    findPreferredHarmonic(const HarmonicVec& harmonics,
                          const Freq&        oscFreq,
                          bool allowEven,
                          int mode);

    Freq   oscfreq_;
    Freq   oscOffsetLock_;
    Freq   yigOffsetLock_;
    Freq   refLow_;
    Freq   refHigh_;
    Freq   yigLow_;
    Freq   yigHigh_;
    
    size_t      preferredHarmonicIndex_;  // index to the preferred harmonic
    HarmonicVec harmonics_;
    int         multiplier_;
    int         mode_;
    int         nOdd_; // Number of odd yig harmonics
};


}  // namespace carma::control
}  // namespace carma


#endif // End of conditional include guard
