/**
 * @file
 * Implementation of signal paths for CARMA array.
 *
 * @author: N. S. Amarnath
 *
 * $Id: SignalPath.h,v 1.5 2006/02/03 19:26:57 tcosta Exp $
 *
 * $CarmaCopyright$
 *
 */

#ifndef CARMA_MONITOR_SIGNAL_PATH_H
#define CARMA_MONITOR_SIGNAL_PATH_H

#include <string>
#include <iosfwd>

#include "carma/util/ErrorException.h"
#include "carma/util/NotFoundException.h"

namespace carma  {
  namespace monitor  {

/**
 * @class ::carma::monitor::SignalPath
 * @brief Class to manage information about mapping of signal paths
 * from antennas to correlator/downconverter inputs.
 *
 * Antname as used here can be legal/standard name - of the form 
 * <antenna-dish-size-in-meters>"m#"<antenna-number>. So the legal
 * name for Ovro1 will be ovro1. Ovro1 is the historical name.
 */
class SignalPath {

public:

    /**
     * Constructor
     *
     * Initializes certain constants common to all class instances 
     * (MAX_SYNTH_ID, MAX_IF_CHANNEL_ID, MAX_LR_CHANNEL_ID, 
     * MAX_LO_CHANNEL_ID and MAX_DC_INPUT_ID) if
     * they haven't already been initialized.
     */
    SignalPath ();

    /**
     * Destructor
     */
    ~SignalPath();

    /**
     * Gets absolute CARMA antenna number - this is an absolute antenna number
     * in the range [1,23] which is immutable. Runs through ovro antennas
     * first, then bima and sza. So OVRO antenna 3 (ovro3) gets the CARMA
     * antenna number 3, and BIMA antenna 3 (bima3) becomes # 9.
     *
     * @param antName std::string& antenna name as legal or historical name
     * @return unsigned short CARMA antenna number - identifies antenna uniquely.
     * @see SignalPath.cc
     */
    unsigned short getCarmaAntennaNo (const std::string& antName) const ;

    /**
     * Gets standard Carma antenna name given the Carma antenna number - 
     * this is an absolute antenna number  in the range [1,23] which is 
     * immutable. 
     *
     * @param carmaAntennaNo - antenna number identifies antenna uniquely.
     * @return std::string standard Carma antenna name - identifies antenna 
     *         uniquely.
     * @see SignalPath.cc
     */
    std::string 
	getCarmaAntennaName(const unsigned short carmaAntennaNo) const ;

    /**
     * Gets input downconverter number for a specified antenna.
     *
     * @param antName std::string& antenna name as legal or historical name
     * @return downconverter input # (same as correlator input #)
     */
    unsigned int getInputNo (const std::string& antName) const ;

    /**
     * Gets synthesizer number for a specified antenna.
     *
     * @param antName std::string& antenna name as legal or historical name
     * @return synthesizer # 
     */
    unsigned int getSynthesizerNo (const std::string& antName) const ;

    /**
     * Gets IF channel number for a specified antenna.
     *
     * @param antName std::string& antenna name as legal or historical name
     * @return IF channel # 
     */
    unsigned int getIFchannelNo (const std::string& antName) const ;

    /**
     * Gets LO channel number for a specified antenna.
     *
     * @param antName std::string& antenna name as legal or historical name
     * @return LO channel # 
     */
    unsigned int getLOchannelNo (const std::string& antName) const ;

    /**
     * Gets LR channel number for a specified antenna.
     *
     * @param antName std::string& antenna name as legal or historical name
     * @return LR channel # 
     */
    unsigned int getLRchannelNo (const std::string& antName) const ;

    /**
     * Gets IF channel number for a specified downconverter input.
     *
     * @param dcInputID const int, downconverter input # 
     *        (same as correlator input #)
     * @return IFchannelID int IF channel number 
     */
    unsigned int getIFchannelID (const unsigned int dcInputID) const ;

    /**
     * Modifies signal path mapping. Associates an antenna
     * with a station number.
     *
     * @param antName std::string& antenna name in short/long form
     * @param stationID int station number to associate with antenna
     */
    void mapAntToStation (std::string& antName, unsigned int stationID) ;

    /**
     * Modifies signal path mapping. Associates an antenna
     * with a synthesizer.
     *
     * @param antName std::string& antenna name in short/long form
     * @param synthID int synthesizer number (1-3) to associate with antenna
     */
    void mapAntToSynth (std::string& antName, unsigned int synthID) ;

    /**
     * Modifies signal path mapping. Associates an antenna
     * with an IF channel.
     *
     * @param antName std::string& antenna name in short/long form
     * @param IFchannelID int IF channel number to associate with antenna
     */
    void mapAntToIFchannel (std::string& antName, unsigned int IFchannelID) ;

    /**
     * Modifies signal path mapping. Associates an antenna
     * with a loberotator channel.
     *
     * @param antName std::string& antenna name in short/long form
     * @param LRchannelID int loberotator channel number to associate 
     *        with antenna
     */
    void mapAntToLRchannel (std::string& antName, unsigned int LRchannelID) ;

    /**
     * Modifies signal path mapping. Associates an antenna
     * with a local oscillator channel.
     *
     * @param antName std::string& antenna name in short/long form
     * @param LOchannelID int LO channel number to associate with antenna
     */
    void mapAntToLOchannel (std::string& antName, unsigned int LOchannelID) ;

    /**
     * Modifies signal path mapping. Associates an IF channel
     * with a downconverter input..
     *
     * @param IFchannelID int IF channel number 
     * @param dcInputID int downconverter input number to associate 
     *        with specified channel
     */
    void mapIFchannelToDCinput 
	(const unsigned int IFchannelID, unsigned int dcInputID) ;

  protected:

    /**
     * Returns an index to a mapping entry given an antenna
     * name in long or short form.
     *
     * @param antName std::string& antenna name in short/long form
     * @return int index to mapping entry
     */
    int findAntEntry (const std::string& antName) const ;

    /**
     * Returns an index to a mapping entry given an IF channel
     * identifier (channel number).
     *
     * @param IFchannelID int IF channel number 
     * @return int index to mapping entry
     */
    unsigned int findIFchannelEntry (const unsigned int IFchannelID) const ;

    /**
     * Initializes certain constants common to all class instances 
     * (MAX_SYNTH_ID, MAX_IF_CHANNEL_ID, MAX_LR_CHANNEL_ID, 
     * MAX_LO_CHANNEL_ID and MAX_DC_INPUT_ID) if
     * they haven't already been initialized.
     */
    static void initializeMaxConstants () ;

  private:

};  // class SignalPath


/**
 * @class 
 * @brief Exception class used when entry for antenna or IF channel
 * not found in signal map table.
 */
class EntryNotFoundException : public ::carma::util::NotFoundException {
  public:
    /**
     * Constructor
     * @param msg      The message for this exception.  
     * @param fileName The source file containing the code throwing the
     *                 exception. Can be set using the 
     *                 cpp macro '__FILE__'.  
     * @param lineNum  The line number in the source file where the 
     *                 exception is created. Can be set using the 
     *                 cpp macro '__LINE__'.
     */
    EntryNotFoundException (const std::ostringstream& oss, 
                                                const char* fileName, 
                                                const int lineNum) ;
    /**
     * Destructor.
     * @brief Only throws because ancestor throws.
     */
    virtual ~EntryNotFoundException () throw ();
};


/**
 * @class 
 * @brief Exception class used when entry for antenna name is
 * not found in signal map table.
 */
class AntennaNotFoundException : public EntryNotFoundException  {
  public:
    /**
     * Constructor
     * @param msg      The message for this exception.  
     * @param fileName The source file containing the code throwing the
     *                 exception. Can be set using the 
     *                 cpp macro '__FILE__'.  
     * @param lineNum  The line number in the source file where the 
     *                 exception is created. Can be set using the 
     *                 cpp macro '__LINE__'.
     */
    AntennaNotFoundException (const std::ostringstream& oss, 
                                                const char* fileName, 
                                                const int lineNum) ;
    /**
     * Destructor.
     * @brief Only throws because ancestor throws.
     */
    virtual ~AntennaNotFoundException () throw ();
};


/**
 * @class 
 * @brief Exception class used when entry for IF channel ID is
 * not found in signal map table.
 */
class IFchannelEntryNotFoundException : public EntryNotFoundException  {
  public:
    /**
     * Constructor
     * @param msg      The message for this exception.  
     * @param fileName The source file containing the code throwing the
     *                 exception. Can be set using the 
     *                 cpp macro '__FILE__'.  
     * @param lineNum  The line number in the source file where the 
     *                 exception is created. Can be set using the 
     *                 cpp macro '__LINE__'.
     */
    IFchannelEntryNotFoundException (const std::ostringstream& oss, 
                                                const char* fileName, 
                                                const int lineNum) ;
    /**
     * Destructor.
     * @brief Only throws because ancestor throws.
     */
    virtual ~IFchannelEntryNotFoundException () throw ();
};


/**
 * @class 
 * @brief Exception class used when input parameter synthesizer ID is
 * out of range.
 */
class SynthIDOutOfRangeException : public ::carma::util::ErrorException  {
  public:
    /**
     * Constructor
     * @param msg      The message for this exception.  
     * @param fileName The source file containing the code throwing the
     *                 exception. Can be set using the 
     *                 cpp macro '__FILE__'.  
     * @param lineNum  The line number in the source file where the 
     *                 exception is created. Can be set using the 
     *                 cpp macro '__LINE__'.
     */
    SynthIDOutOfRangeException (const std::ostringstream& oss, 
                                                const char* fileName, 
                                                const int lineNum) ;
    /**
     * Destructor.
     * @brief Only throws because ancestor throws.
     */
    virtual ~SynthIDOutOfRangeException () throw ();
};


/**
 * @class 
 * @brief Exception class used when input parameter IF channel ID is
 * out of range.
 */
class IFchannelIDOutOfRangeException : public ::carma::util::ErrorException  {
  public:
    /**
     * Constructor
     * @param msg      The message for this exception.  
     * @param fileName The source file containing the code throwing the
     *                 exception. Can be set using the 
     *                 cpp macro '__FILE__'.  
     * @param lineNum  The line number in the source file where the 
     *                 exception is created. Can be set using the 
     *                 cpp macro '__LINE__'.
     */
    IFchannelIDOutOfRangeException (const std::ostringstream& oss, 
                                                const char* fileName, 
                                                const int lineNum) ;
    /**
     * Destructor.
     * @brief Only throws because ancestor throws.
     */
    virtual ~IFchannelIDOutOfRangeException () throw ();
};


/**
 * @class 
 * @brief Exception class used when input parameter LR channel ID is
 * out of range.
 */
class LRchannelIDOutOfRangeException : public ::carma::util::ErrorException  {
  public:
    /**
     * Constructor
     * @param msg      The message for this exception.  
     * @param fileName The source file containing the code throwing the
     *                 exception. Can be set using the 
     *                 cpp macro '__FILE__'.  
     * @param lineNum  The line number in the source file where the 
     *                 exception is created. Can be set using the 
     *                 cpp macro '__LINE__'.
     */
    LRchannelIDOutOfRangeException (const std::ostringstream& oss, 
                                                const char* fileName, 
                                                const int lineNum) ;
    /**
     * Destructor.
     * @brief Only throws because ancestor throws.
     */
    virtual ~LRchannelIDOutOfRangeException () throw ();
};


/**
 * @class 
 * @brief Exception class used when input parameter LO channel ID is
 * out of range.
 */
class LOchannelIDOutOfRangeException : public ::carma::util::ErrorException  {
  public:
    /**
     * Constructor
     * @param msg      The message for this exception.  
     * @param fileName The source file containing the code throwing the
     *                 exception. Can be set using the 
     *                 cpp macro '__FILE__'.  
     * @param lineNum  The line number in the source file where the 
     *                 exception is created. Can be set using the 
     *                 cpp macro '__LINE__'.
     */
    LOchannelIDOutOfRangeException (const std::ostringstream& oss, 
                                                const char* fileName, 
                                                const int lineNum) ;

    /**
     * Destructor.
     * @brief Only throws because ancestor throws.
     */
    virtual ~LOchannelIDOutOfRangeException () throw ();
};

} } // namespace ::carma::monitor


#endif //  CARMA_MONITOR_SIGNAL_PATH_H
