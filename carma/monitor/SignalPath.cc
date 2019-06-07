#include <iosfwd>
#include <sstream>
#include <string>

#include "carma/util/ErrorException.h"
#include "carma/util/NotFoundException.h"
#include "carma/monitor/SignalPath.h"

using namespace carma ;
using namespace carma::monitor ;



namespace {

/**
 * @struct SignalPathEntry
 *
 * @brief Structure to hold signal path mappings.
 * An entry in a table of mappings representing signal path from
 * station (associated with an antenna) to downconverter inputs,
 * IF channels, LO channels and LR channels.
 */
typedef struct SignalPathStruct {
    std::string     antAlias;
    std::string     antStandardName;
    unsigned int    stationNum;
    unsigned int    LRchannel;
    unsigned int    IFchannel;
    unsigned int    LOchannel;
    unsigned int    dcInput;    // downconverter input number
    unsigned int    synthesizer;
} SignalPathEntry;


/**
 * For now, a hard-coded table. The table may be modified 
 * by hand to change mappings. Methods will be added to modify this table
 * on the fly.
 */
static SignalPathEntry signalPathTab[] = 
   //      Antname   Ant legal   Station   LR       IF    LO    Down    Synth
   //               or standard           chan     chan  chan converter 
   //                  name                                     input
       {
         { "Ovro1",   "ovro1",    0,       1,       1,    1,     1,       1  },
         { "Ovro2",   "ovro2",    0,       2,       2,    2,     2,       1  },
         { "Ovro3",   "ovro3",    0,       3,       3,    3,     3,       1  },
         { "Ovro4",   "ovro4",    0,       4,       4,    4,     4,       1  },
         { "Ovro5",   "ovro5",    0,       5,       5,    5,     5,       1  },
         { "Ovro6",   "ovro6",    0,       6,       6,    6,     6,       1  },
         { "Bima1",   "bima1",    0,       7,       7,    7,     7,       1  },
         { "Bima2",   "bima2",    0,       8,       8,    8,     8,       1  },
         { "Bima3",   "bima3",    0,       9,       9,    9,     9,       1  },
         { "Bima4",   "bima4",    0,      10,      10,   10,    10,       1  },
         { "Bima5",   "bima5",    0,      11,      11,   11,    11,       1  },
         { "Bima6",   "bima6",    0,      12,      12,   12,    12,       1  },
         { "Bima7",   "bima7",    0,      13,      13,   13,    13,       1  },
         { "Bima8",   "bima8",    0,      14,      14,   14,    14,       1  },
         { "Bima9",   "bima9",    0,      15,      15,   15,    15,       1  },
         { "Sza1",    "sza1",     0,      16,      16,   16,    16,       2  },
         { "Sza2",    "sza2",     0,      17,      17,   17,    17,       2  },
         { "Sza3",    "sza3",     0,      18,      18,   18,    18,       2  },
         { "Sza4",    "sza4",     0,      19,      19,   19,    19,       2  },
         { "Sza5",    "sza5",     0,      20,      20,   20,    20,       2  },
         { "Sza6",    "sza6",     0,      21,      21,   21,    21,       2  },
         { "Sza7",    "sza7",     0,      22,      22,   22,    22,       2  },
         { "Sza8",    "sza8",     0,      23,      23,   23,    23,       2  },
         { "Spare",   "Sp",       0,      24,      24,   24,    24,       3  }
     };


const unsigned int numSignalPathEntries = 
                   sizeof (signalPathTab)/sizeof(SignalPathEntry);

bool allMaxConstantsInitialized = false;

unsigned int  MAX_SYNTH_ID      = 0;
unsigned int  MAX_IF_CHANNEL_ID = 0;
unsigned int  MAX_LR_CHANNEL_ID = 0;
unsigned int  MAX_LO_CHANNEL_ID = 0;
unsigned int  MAX_DC_INPUT_ID   = 0;

const int ENTRY_NOT_FOUND = -1;

};



SignalPath::SignalPath ()
{
    initializeMaxConstants ();
}


SignalPath::~SignalPath ()
{
}


void
SignalPath::initializeMaxConstants ()
{
    if (allMaxConstantsInitialized)
        return;

    unsigned int maxSynthID = 0;
    unsigned int maxIFchannelID = 0;
    unsigned int maxLOchannelID = 0;
    unsigned int maxLRchannelID = 0;
    unsigned int maxDCinputID = 0;
    for (unsigned int i = 0;  i < numSignalPathEntries;  i++)  {
        maxSynthID     = std::max (maxSynthID, signalPathTab[i].synthesizer);
        maxIFchannelID = std::max (maxIFchannelID, signalPathTab[i].IFchannel);
        maxLRchannelID = std::max (maxLRchannelID, signalPathTab[i].LRchannel);
        maxLOchannelID = std::max (maxLOchannelID, signalPathTab[i].LOchannel);
        maxDCinputID   = std::max (maxDCinputID, signalPathTab[i].dcInput);
    }

    MAX_SYNTH_ID      = maxSynthID;
    MAX_IF_CHANNEL_ID = maxIFchannelID;
    MAX_LR_CHANNEL_ID = maxLOchannelID;
    MAX_LO_CHANNEL_ID = maxLRchannelID;
    MAX_DC_INPUT_ID   = maxDCinputID;
    allMaxConstantsInitialized = true;
}



int
SignalPath::findAntEntry (const std::string& antName) const
{
    unsigned int    i;

    for (i = 0;   i < numSignalPathEntries  
		        &&  signalPathTab[i].antAlias != antName  
                        &&  signalPathTab[i].antStandardName != antName;  i++)
        ;

    if ( i == numSignalPathEntries )  {
	return ENTRY_NOT_FOUND;
    }

    return i;
}



unsigned int
SignalPath::findIFchannelEntry (const unsigned int IFchannelID) const
{
    unsigned int    i;

    for (i = 0;   i < numSignalPathEntries  
		        &&  signalPathTab[i].IFchannel != IFchannelID;  i++)
        ;

    if ( i == numSignalPathEntries )  {
        std::ostringstream os;
        os << "SignalPath::findIFchannelEntry - unknown IF channel ID "
           << IFchannelID;
        throw CARMA_ERROR (os.str());
    }

    return i;
}



unsigned short
SignalPath::getCarmaAntennaNo (const std::string& antName) const 
{
    int    entry = findAntEntry (antName);
    CARMA_TEST ( (entry != ENTRY_NOT_FOUND), AntennaNotFoundException, 
                  ("SignalPath::getCarmaAntennaNo - Antenna " 
                    + antName + " not found." )
               );

    unsigned short antNo = entry+1;
    return antNo;
}


std::string 
SignalPath::getCarmaAntennaName (const unsigned short carmaAntennaNo) const 
{
    return signalPathTab[carmaAntennaNo-1].antStandardName;
}


unsigned int
SignalPath::getInputNo (const std::string& antName) const
{
    int    entry = findAntEntry (antName);
    CARMA_TEST ( (entry != ENTRY_NOT_FOUND), AntennaNotFoundException, 
                  ("SignalPath::getCarmaAntennaNo - Antenna " 
                    + antName + " not found." )
               );

    return signalPathTab[entry].dcInput;
}


unsigned int 
SignalPath::getSynthesizerNo (const std::string& antName) const 
{
    int    entry = findAntEntry (antName);
    CARMA_TEST ( (entry != ENTRY_NOT_FOUND), AntennaNotFoundException,
                  ("SignalPath::getSynthesizerNo - Antenna " 
                    + antName + " not found." )
               );

    return signalPathTab[entry].synthesizer;
}


unsigned int 
SignalPath::getIFchannelNo (const std::string& antName) const 
{
    int    entry = findAntEntry (antName);
    CARMA_TEST ( (entry != ENTRY_NOT_FOUND), AntennaNotFoundException,
                  ("SignalPath::getIfchannel- Antenna " 
                    + antName + " not found." )
               );

    return signalPathTab[entry].IFchannel;
}


unsigned int 
SignalPath::getLOchannelNo (const std::string& antName) const 
{
    int    entry = findAntEntry (antName);
    CARMA_TEST ( (entry != ENTRY_NOT_FOUND), AntennaNotFoundException,
                  ("SignalPath::getLOchannel- Antenna " 
                    + antName + " not found." )
               );

    return signalPathTab[entry].LOchannel;
}


unsigned int 
SignalPath::getLRchannelNo (const std::string& antName) const 
{
    int    entry = findAntEntry (antName);
    CARMA_TEST ( (entry != ENTRY_NOT_FOUND), AntennaNotFoundException,
                  ("SignalPath::getLRchannel- Antenna " 
                    + antName + " not found." )
               );

    return signalPathTab[entry].LRchannel;
}


unsigned int 
SignalPath::getIFchannelID (const unsigned int dcInputID) const 
{
    unsigned int    i;

    for (i = 0;   i < numSignalPathEntries  
		        &&  signalPathTab[i].dcInput != dcInputID;  i++)
        ;

    if ( i == numSignalPathEntries )  {
        std::ostringstream os;
        os << "SignalPath::getIFchannelID - unknown downconverter input ID "
           << dcInputID;
        throw CARMA_ERROR (os.str());
    }

    return signalPathTab[i].IFchannel;
}



void 
SignalPath::mapAntToStation (std::string& antName, unsigned int stationID)
{
    int entry = findAntEntry (antName);
    CARMA_TEST ( (entry != ENTRY_NOT_FOUND), AntennaNotFoundException, 
                  ("SignalPath::mapAntToStation - Antenna " 
                    + antName + " not found." )
               );
    signalPathTab[entry].stationNum = stationID;
}


void 
SignalPath::mapAntToSynth (std::string& antName, unsigned int synthID)
{
    std::ostringstream synthIDstream;
    synthIDstream << " SignalMap::mapAntToSynth - SynthID = " << synthID
                  << " out of bounds";
    CARMA_TEST ( ((0 < synthID)  &&  (synthID < MAX_SYNTH_ID)),
                 SynthIDOutOfRangeException,
                 synthIDstream.str()
               );
    
    int entry = findAntEntry (antName);
    CARMA_TEST ( (entry != ENTRY_NOT_FOUND), AntennaNotFoundException, 
                  ("SignalPath::mapAntToSynth - Antenna " 
                    + antName + " not found." )
               );
    signalPathTab[entry].synthesizer = synthID;
}



void 
SignalPath::mapAntToIFchannel (std::string& antName, unsigned int IFchannelID)
{
    std::ostringstream ifIDstream;
    ifIDstream << " IFchannelID = " << IFchannelID;
    CARMA_TEST ( ((0 < IFchannelID)  &&  (IFchannelID < MAX_IF_CHANNEL_ID)),
                 IFchannelIDOutOfRangeException,
                 ("SignalPath::mapAntToIFchannel - " 
                   + ifIDstream.str() + " out of bounds." )
               );
    
    int entry = findAntEntry (antName);
    CARMA_TEST ( (entry != ENTRY_NOT_FOUND), AntennaNotFoundException, 
                  ("SignalPath::mapAntToIFchannel - Antenna " 
                    + antName + " not found." )
               );
    signalPathTab[entry].IFchannel = IFchannelID;
}


void 
SignalPath::mapAntToLRchannel (std::string& antName, unsigned int LRchannelID)
{
    std::ostringstream lrIDstream;
    lrIDstream << " LRchannelID = " << LRchannelID;
    CARMA_TEST ( ((0 < LRchannelID)  &&  (LRchannelID < MAX_LR_CHANNEL_ID)),
                 LRchannelIDOutOfRangeException,
                 ("SignalPath::mapAntToLRchannel - " 
                   + lrIDstream.str() + " out of bounds." )
               );
    
    int entry = findAntEntry (antName);
    CARMA_TEST ( (entry != ENTRY_NOT_FOUND), AntennaNotFoundException, 
                  ("SignalPath::mapAntToIFchannel - Antenna " 
                    + antName + " not found." )
               );
    signalPathTab[entry].LRchannel = LRchannelID;
}



void 
SignalPath::mapAntToLOchannel (std::string& antName, unsigned int LOchannelID)
{
    std::ostringstream loIDstream;
    loIDstream << " LOchannelID = " << LOchannelID;
    CARMA_TEST ( ((0 < LOchannelID)  &&  (LOchannelID < MAX_LO_CHANNEL_ID)),
                 LOchannelIDOutOfRangeException,
                 ("SignalPath::mapAntToLOchannel - " 
                   + loIDstream.str() + " out of bounds." )
               );
    
    int entry = findAntEntry (antName);
    CARMA_TEST ( (entry != ENTRY_NOT_FOUND), AntennaNotFoundException, 
                  ("SignalPath::mapAntToIFchannel - Antenna " 
                    + antName + " not found." )
               );
    signalPathTab[entry].LOchannel = LOchannelID;
}


void 
SignalPath::mapIFchannelToDCinput (const unsigned int IFchannelID, 
	                           unsigned int dcInputID)
{
    std::ostringstream ifIDstream;
    ifIDstream << " IFchannelID = " << IFchannelID;
    CARMA_TEST ( ((0 < IFchannelID)  &&  (IFchannelID < MAX_IF_CHANNEL_ID)),
                 IFchannelIDOutOfRangeException,
                 ("SignalPath::mapIFchannelToDCinput - " 
                   + ifIDstream.str() + " out of bounds." )
               );
    
    std::ostringstream dcInputStream;
    dcInputStream << " IFchannelID = " << dcInputID;
    CARMA_TEST ( ((0 < dcInputID)  &&  (dcInputID < MAX_DC_INPUT_ID)),
                 IFchannelIDOutOfRangeException,
                 ("SignalPath::mapIFchannelToDCinput - " 
                   + dcInputStream.str() + " out of bounds." )
               );
    
    int entry = findIFchannelEntry (IFchannelID);
    CARMA_TEST ( (( entry != ENTRY_NOT_FOUND )),
                 IFchannelEntryNotFoundException,
                 ("SignalPath::mapIFchannelToDCinput - " 
                   + ifIDstream.str() + " not found." )
               );
    
    signalPathTab[entry].dcInput = dcInputID;
}


EntryNotFoundException::EntryNotFoundException (const std::ostringstream& os, 
                                                const char* fileName, 
                                                const int lineNum)
                       : ::carma::util::NotFoundException (os, fileName, lineNum)
{
}


EntryNotFoundException::~EntryNotFoundException () throw () { }

AntennaNotFoundException::AntennaNotFoundException 
                                          (const std::ostringstream& os,
                                               const char* fileName, 
                                               const int lineNum)
                      : EntryNotFoundException (os, fileName, lineNum)
{
}


AntennaNotFoundException::~AntennaNotFoundException () throw () { }

IFchannelEntryNotFoundException::IFchannelEntryNotFoundException 
                                          (const std::ostringstream& os,
                                               const char* fileName, 
                                               const int lineNum)
                      : EntryNotFoundException (os, fileName, lineNum)
{
}

IFchannelEntryNotFoundException::~IFchannelEntryNotFoundException () throw () { }

SynthIDOutOfRangeException::SynthIDOutOfRangeException 
                                (const std::ostringstream& os,
                                               const char* fileName,
                                               const int lineNum)
                      : ::carma::util::ErrorException (os, fileName, lineNum)
{
}


SynthIDOutOfRangeException::~SynthIDOutOfRangeException () throw () { }

IFchannelIDOutOfRangeException::IFchannelIDOutOfRangeException
                                (const std::ostringstream& os,
                                               const char* fileName,
                                               const int lineNum)
                      : ::carma::util::ErrorException (os, fileName, lineNum)
{
}

IFchannelIDOutOfRangeException::~IFchannelIDOutOfRangeException () throw () { }

LRchannelIDOutOfRangeException::LRchannelIDOutOfRangeException
                                (const std::ostringstream& os,
                                               const char* fileName,
                                               const int lineNum)
                      : ::carma::util::ErrorException (os, fileName, lineNum)
{
}

LRchannelIDOutOfRangeException::~LRchannelIDOutOfRangeException () throw () { }

LOchannelIDOutOfRangeException::LOchannelIDOutOfRangeException
                                (const std::ostringstream& os,
                                               const char* fileName,
                                               const int lineNum)
                      : ::carma::util::ErrorException (os, fileName, lineNum)
{
}

LOchannelIDOutOfRangeException::~LOchannelIDOutOfRangeException () throw () { }

